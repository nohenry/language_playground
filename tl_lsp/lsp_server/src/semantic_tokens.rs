use tl_core::{
    ast::{
        ArgList, EnclosedPunctuationList, Expression, GenericParameter, ParamaterList,
        ParsedTemplate, Statement, Type,
    },
    token::SpannedToken,
};
use tl_vm::{
    const_value::{ConstValue, ConstValueKind},
    scope::{ScopeManager, ScopeValue},
};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

pub const STOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::EVENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::MACRO,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
];

#[derive(Default)]
pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
    pub fn push(&mut self, line: u32, position: u32, length: u32, token: u32, modifier: u32) {
        if self.last_line == line {
            let delta_pos = position - self.last_pos;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: delta_pos,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        } else {
            let delta_line = line - self.last_line;
            self.last_line = line;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line,
                delta_start: position,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        }
    }

    pub fn push_token(&mut self, token: &SpannedToken, index: u32, modifier: u32) {
        self.push(
            token.span().line_num,
            token.span().position,
            token.span().length,
            index,
            modifier,
        )
    }

    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

fn get_stype_index(ty: SemanticTokenType) -> u32 {
    STOKEN_TYPES.iter().position(|f| *f == ty).unwrap_or(0) as u32
}

fn get_stype_index_from_str(ty: &str) -> u32 {
    STOKEN_TYPES
        .iter()
        .position(|f| f.as_str() == ty)
        .unwrap_or(0) as u32
}

pub struct SemanticTokenGenerator<'a> {
    scope: &'a ScopeManager,
    pub scope_index: Vec<usize>,
    pub builder: SemanticTokenBuilder,
}

impl<'a> SemanticTokenGenerator<'a> {
    pub fn new(scope: &'a ScopeManager) -> SemanticTokenGenerator<'a> {
        SemanticTokenGenerator {
            scope,
            scope_index: Vec::with_capacity(50),
            builder: SemanticTokenBuilder::default(),
        }
    }

    pub fn build(self) -> Vec<SemanticToken> {
        self.builder.build()
    }
}

impl<'a> SemanticTokenGenerator<'a> {
    pub fn recurse(&mut self, stmt: &Statement) {
        match stmt {
            Statement::List(list) => {
                for l in list.iter_items() {
                    self.recurse(l);
                }
            }
            Statement::Decleration {
                ty, ident, expr, ..
            } => {
                self.recurse_type(ty);
                let func = match expr {
                    Some(Expression::Record { .. }) => get_stype_index_from_str("struct"),
                    _ => get_stype_index_from_str("variable"),
                };

                self.builder.push_token(ident, func, 0);

                if let Some(expr) = expr {
                    self.recurse_expression(expr);
                }
            }
            Statement::Expression(e) => self.recurse_expression(e),
            Statement::UseStatement { token, args } => {
                if let Some(token) = token {
                    self.builder
                        .push_token(token, get_stype_index_from_str("keyword"), 0)
                }

                self.scope
                    .iter_use(args.iter_items().map(|f| (f.as_str(), f)), |_sym, ud| {
                        self.builder
                            .push_token(ud, get_stype_index_from_str("namespace"), 0);
                    });
            }
            Statement::Function {
                fn_tok,
                ident,
                parameters,
                return_parameters,
                body,
                ..
            } => {
                self.builder
                    .push_token(fn_tok, get_stype_index_from_str("keyword"), 0);
                self.builder
                    .push_token(ident, get_stype_index_from_str("function"), 0);

                self.recurse_params(parameters);
                self.recurse_params(return_parameters);

                if let Some(body) = body {
                    self.recurse(body);
                }
            }
            Statement::TypeAlias {
                ty_tok,
                ident,
                generic,
                ty,
                ..
            } => {
                self.builder
                    .push_token(ty_tok, get_stype_index_from_str("keyword"), 0);
                self.builder
                    .push_token(ident, get_stype_index_from_str("type"), 0);

                if let Some(generic) = generic {
                    self.recurse_generic(generic)
                }

                self.recurse_type(ty);
            }
            Statement::Block(b) => {
                for statement in b.items.iter() {
                    self.recurse(statement);
                }
            }
        }
    }

    pub fn recurse_expression(&mut self, value: &Expression) {
        match value {
            Expression::Boolean(_, tok) => {
                self.builder
                    .push_token(tok, get_stype_index_from_str("keyword"), 0)
            }
            Expression::String(template_string, _tok) => {
                for templ in &template_string.0 {
                    match templ {
                        ParsedTemplate::String(s) => {
                            self.builder.push_token(
                                s,
                                get_stype_index(SemanticTokenType::STRING),
                                0,
                            );
                        }
                        ParsedTemplate::Template(st, _o, _c) => {
                            self.recurse_expression(st);
                        }
                    }
                }
            }
            Expression::Ident(tok) => {
                let mut current_scope = Vec::new();
                self.scope
                    .push_scope_chain(&mut current_scope, self.scope_index.iter());

                if let Some(sym) = self
                    .scope
                    .find_symbol_in_scope(tok.as_str(), &current_scope)
                {
                    let sym = sym.borrow();
                    match &sym.value {
                        ScopeValue::ConstValue(ConstValue {
                            kind:
                                ConstValueKind::Function { .. } | ConstValueKind::NativeFunction { .. },
                            ..
                        }) => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::FUNCTION),
                                0,
                            );
                        }
                        ScopeValue::Struct { .. } => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        ScopeValue::ConstValue(ConstValue {
                            kind: ConstValueKind::StructInstance { .. },
                            ..
                        }) => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        _ => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::VARIABLE),
                                0,
                            );
                        }
                    }
                }
            }
            Expression::Float(_, _, tok) => {
                self.builder
                    .push_token(tok, get_stype_index(SemanticTokenType::NUMBER), 0);
            }
            Expression::Integer(_, _, tok) => {
                self.builder
                    .push_token(tok, get_stype_index(SemanticTokenType::NUMBER), 0);
            }

            Expression::FunctionCall { expr, args } => {
                self.recurse_expression(expr);
                self.recurse_args(args);
            }
            Expression::Tuple(_) => (),
            Expression::Array { values, .. } => values
                .iter_items()
                .for_each(|item| self.recurse_expression(item)),
            Expression::BinaryExpression { left, right, .. } => {
                if let Some(left) = left {
                    self.recurse_expression(left);
                }
                if let Some(right) = right {
                    self.recurse_expression(right);
                }
            }
            Expression::Record(parameters) => {
                for kv in parameters.items.iter() {
                    if let Some(name) = &kv.name {
                        self.builder
                            .push_token(name, get_stype_index_from_str("property"), 0)
                    }

                    self.recurse_expression(&kv.expr);
                }
            }
        }
    }

    pub fn recurse_type(&mut self, ty: &Type) {
        match ty {
            Type::Integer { token, .. } => {
                self.builder
                    .push_token(token, get_stype_index_from_str("type"), 0);
            }
            Type::Float { token, .. } => {
                self.builder
                    .push_token(token, get_stype_index_from_str("type"), 0);
            }
            Type::Boolean(tok) => self
                .builder
                .push_token(tok, get_stype_index_from_str("type"), 0),
            Type::Expression(e) => self.recurse_expression(e),
            Type::Ref {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Option {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Result {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Ident(ident) => {
                self.builder
                    .push_token(ident, get_stype_index_from_str("type"), 0);
            }
            Type::Struct(values) => {
                for item in values.iter_items() {
                    if let Some(ty) = &item.ty {
                        self.recurse_type(ty);
                    }

                    if let Some(name) = &item.name {
                        self.builder.push_token(
                            name,
                            get_stype_index(SemanticTokenType::VARIABLE),
                            0,
                        );
                    }
                }
            }
            Type::Generic { base_type, list } => {
                if let Some(base_type) = base_type {
                    self.recurse_type(base_type);
                }

                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            Type::Union(list) => {
                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            Type::Array(list) => {
                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            _ => (),
        }
    }

    pub fn recurse_generic(&mut self, stmt: &EnclosedPunctuationList<GenericParameter>) {
        for stmt in stmt.iter_items() {
            match stmt {
                GenericParameter::Unbounded(ident) => {
                    self.builder
                        .push_token(ident, get_stype_index_from_str("typeParameter"), 0)
                }
                GenericParameter::Bounded { bounds, ident, .. } => {
                    self.builder
                        .push_token(ident, get_stype_index_from_str("typeParameter"), 0);

                    for bound in bounds.iter_items() {
                        self.builder
                            .push_token(bound, get_stype_index_from_str("interface"), 0);
                    }
                }
            }
        }
    }

    pub fn recurse_args(&mut self, args: &ArgList) {
        for item in args.iter_items() {
            self.recurse_expression(item)
        }
    }

    pub fn recurse_params(&mut self, args: &ParamaterList) {
        for item in args.iter_items() {
            if let Some(ty) = &item.ty {
                self.recurse_type(ty);
            }

            if let Some(name) = &item.name {
                self.builder
                    .push_token(name, get_stype_index(SemanticTokenType::VARIABLE), 0);
            }
        }
    }
}
