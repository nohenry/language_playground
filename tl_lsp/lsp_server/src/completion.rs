use tl_core::{
    ast::Statement,
    token::{Operator, Span, SpannedToken, Token},
    Module,
};
use tower_lsp::lsp_types::CompletionItem;

use crate::Backend;

impl Backend {
    pub fn bsearch_value_with_key(
        &self,
        _key: &SpannedToken,
        _span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        None
    }

    pub fn bsearch_statement(
        &self,
        _module: &Module,
        item: &Statement,
        _span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        match item {
            Statement::UseStatement { args, .. } => {
                if let Some((_, Some(SpannedToken(_, Token::Operator(Operator::Dot))))) =
                    args.iter().last()
                {
                    // if let Some(sym) = module.resolve_symbol_chain(args.iter_items()) {
                    //     println!("Use {}", sym.borrow().name);
                    //     let comp = Vec::new();

                    //     return Some(comp);
                    // }
                }
            }
            _ => (),
        }
        None
    }
}
