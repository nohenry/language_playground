use tl_core::{Module, ast::{Statement, Type}};

use crate::resolve::Resolve;


pub struct Lowerer {
    resolve: Resolve,
}

impl Lowerer {
    pub fn lower(module: Module, resolve: Resolve) -> Module {
        let mut lowerer = Lowerer {
            resolve,  
        };

        let stmts = module.stmts.into_iter().map(|stmt| lowerer.lower_statement(&stmt)).collect();

        Module {
            name: module.name,
            content: module.content,
            stmts
        }
    }

    fn lower_statement(&self, statement: &Statement) -> Statement {
        match statement {
            statement => statement.clone(),
        }        
    }

    fn lower_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Integer { width, signed, token } => Type::None()
        }
    }
}
