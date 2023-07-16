#![feature(box_patterns)]

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use semantic_tokens::{SemanticTokenGenerator, STOKEN_TYPES};
use tl_core::ast::AstNode;
use tl_core::token::Span;
use tl_core::Module;
use tl_evaluator::error::ErrorLevel;
use tl_evaluator::evaluator::Evaluator;
use tl_evaluator::pass::{MemberPass, TypeFirst};
use tl_evaluator::scope::scope::{Scope, ScopeValue};
use tl_evaluator::scope::scope_manager::ScopeManager;
use tl_util::Rf;
use tl_vm::stdlib::fill_module;
use tl_vm::vm_type::Type;
use tl_vm::vm_value::VmValue;
use tokio::net::TcpListener;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::Request;
use tower_lsp::{lsp_types::*, LanguageServer};
use tower_lsp::{Client, LspService, Server};

mod completion;
mod semantic_tokens;

struct ReadDirectoryRequest {}

impl Request for ReadDirectoryRequest {
    type Params = String;

    type Result = Vec<(String, u32)>;

    const METHOD: &'static str = "lsif/readDirectory";
}

pub struct Backend {
    element_names: HashSet<String>,
    style_enum: HashMap<String, CompletionType>,

    documents: RwLock<HashMap<Url, (Arc<Module>, ScopeManager<Type, VmValue>)>>,
    client: Arc<Client>,

    symbol_tree: Rf<Scope<Type, VmValue>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _p: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: SemanticTokensLegend {
                                token_types: STOKEN_TYPES.into(),
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
                    ..Default::default()
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: None,
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let toks = {
            let map = &*self.documents.read().unwrap();

            let Some(mods) = map.get(&params.text_document.uri) else {
                return Ok(None);
            };

            let index = if let Some(id) = mods.1.index_of_mod(&mods.0.name) {
                id
            } else {
                0
            };

            let mut scope = mods.1.clone();
            scope.reset_current_scope();

            let mut generator = SemanticTokenGenerator::new(&mut scope);
            // generator.scope_index.push(index);
            // generator.scope_index.push(0);

            for (i, stmt) in mods.0.stmts.iter().enumerate() {
                // generator.scope_index[1] = i;

                generator.recurse(stmt, i);
            }

            generator.build()
        };

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data: toks,
            result_id: None,
        })))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let res = {
            let map = &*self.documents.read().unwrap();
            let Some(mods) = map.get(&params.text_document_position.text_document.uri) else {
                return Ok(None);
            };
            let sp = Span {
                line_num: params.text_document_position.position.line,
                position: params.text_document_position.position.character,
                ..Default::default()
            };

            let items = mods
                .0
                .stmts
                .iter()
                .find_map(|f| self.bsearch_statement(&mods.0, f, &sp));

            if items.is_none() {
                if !mods.0.stmts.iter().any(|f| f.get_range().contains(&sp)) {
                    Some(
                        self.element_names
                            .iter()
                            .map(|name| CompletionItem {
                                label: name.into(),
                                kind: Some(CompletionItemKind::PROPERTY),
                                ..Default::default()
                            })
                            .collect(),
                    )
                } else {
                    items
                }
            } else {
                items
            }
        };

        if let Some(items) = res {
            return Ok(Some(CompletionResponse::Array(items)));
        } else {
            return Ok(None);
        }
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn initialized(&self, _p: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let out = tl_core::Module::parse_str(&params.text_document.text, "mymod");

        for err in out.1 {
            self.client.log_message(MessageType::ERROR, err).await;
        }

        let module = Arc::new(out.0);

        let code_pass =
            Evaluator::<Type, VmValue, TypeFirst>::new(self.symbol_tree.clone(), module.clone(), 1);
        code_pass.evaluate();
        let code_pass_state = code_pass.finish();

        let code_pass =
            Evaluator::<Type, VmValue, MemberPass>::new_with_state(code_pass_state, module.clone());
        code_pass.evaluate();
        let code_pass_state = code_pass.finish();

        // let code_pass = CodePass::new(self.symbol_tree.clone(), module.clone(), 1);
        // let code_pass_state = code_pass.run();

        let diags: Vec<_> = code_pass_state
            .errors
            .iter()
            .map(|err| Diagnostic {
                range: to_rng(&err.range),
                severity: match err.kind.get_level() {
                    ErrorLevel::Error => Some(DiagnosticSeverity::ERROR),
                    ErrorLevel::Warning => Some(DiagnosticSeverity::WARNING),
                    ErrorLevel::Info => Some(DiagnosticSeverity::INFORMATION),
                    ErrorLevel::Hint => Some(DiagnosticSeverity::HINT),
                },
                message: err.kind.to_string(),
                ..Default::default()
            })
            .collect();

        self.client
            .publish_diagnostics(params.text_document.uri.clone(), diags, None)
            .await;

        (*(self.documents.write().unwrap()))
            .insert(params.text_document.uri, (module, code_pass_state.scope));
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let doc = params.text_document;
        for change in params.content_changes {
            let text = change.text;

            let out = tl_core::Module::parse_str(&text, "mymod");

            for err in out.1 {
                self.client.log_message(MessageType::ERROR, err).await;
            }

            let module = Arc::new(out.0);

            let code_pass = Evaluator::<Type, VmValue, TypeFirst>::new(
                self.symbol_tree.clone(),
                module.clone(),
                1,
            );
            code_pass.evaluate();
            let code_pass_state = code_pass.finish();

            let code_pass = Evaluator::<Type, VmValue, MemberPass>::new_with_state(
                code_pass_state,
                module.clone(),
            );
            code_pass.evaluate();
            let code_pass_state = code_pass.finish();

            let diags: Vec<_> = code_pass_state
                .errors
                .iter()
                .map(|err| Diagnostic {
                    range: to_rng(&err.range),
                    severity: match err.kind.get_level() {
                        ErrorLevel::Error => Some(DiagnosticSeverity::ERROR),
                        ErrorLevel::Warning => Some(DiagnosticSeverity::WARNING),
                        ErrorLevel::Info => Some(DiagnosticSeverity::INFORMATION),
                        ErrorLevel::Hint => Some(DiagnosticSeverity::HINT),
                    },
                    message: err.kind.to_string(),
                    ..Default::default()
                })
                .collect();

            self.client
                .publish_diagnostics(doc.uri.clone(), diags, None)
                .await;

            (*(self.documents.write().unwrap()))
                .insert(doc.uri.clone(), (module, code_pass_state.scope));

            self.client.semantic_tokens_refresh().await.unwrap();
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub enum CompletionType {
    Enum(Vec<String>),
    Boolean,
    Symbol(Box<CompletionType>),
    Style,
    Color,
    Rect,
    Unknown,
}

#[tokio::main]
async fn main() {
    let _read = tokio::io::stdin();
    let _write = tokio::io::stdout();

    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let _args = std::env::args();

    let listener = TcpListener::bind("127.0.0.1:5007").await.unwrap();
    println!("Listening");
    let (stream, _) = listener.accept().await.unwrap();
    println!("Connection");

    let (read, write) = tokio::io::split(stream);
    #[cfg(feature = "runtime-agnostic")]
    let (read, write) = (read.compat(), write.compat_write());

    let (service, socket) = LspService::new(|client| {
        let client = Arc::new(client);

        let symbol_tree = Rf::new(Scope::root());
        {
            let std_mod_scope = symbol_tree.borrow_mut().insert(
                symbol_tree.clone(),
                "std".to_string(),
                ScopeValue::Root,
                0,
            );

            fill_module(std_mod_scope);
        }

        Backend {
            element_names: HashSet::from_iter(["style".into(), "view".into(), "setup".into()]),
            style_enum: HashMap::from([]),
            documents: RwLock::new(HashMap::new()),
            client,
            symbol_tree,
        }
    });
    Server::new(read, write, socket).serve(service).await;
}

#[inline]
fn to_rng(range: &tl_core::token::Range) -> Range {
    if range.start == range.end {
        Range::new(
            Position {
                line: range.start.line_num,
                character: range.start.position,
            },
            Position {
                line: range.start.line_num,
                character: range.start.position + range.start.length,
            },
        )
    } else {
        Range::new(
            Position {
                line: range.start.line_num,
                character: range.start.position,
            },
            Position {
                line: range.end.line_num,
                character: range.end.position + range.end.length,
            },
        )
    }
}

#[inline]
fn range_contains(inner: &Range, outer: &Range) -> bool {
    inner.start.line >= outer.start.line
        && inner.end.line <= outer.end.line
        && inner.start.character >= outer.start.character
        && inner.end.character <= outer.end.character
}
