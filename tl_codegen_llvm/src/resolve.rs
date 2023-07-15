use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
    rc::Rc,
};

use tl_core::ast::{ClassBody, Expression, InterfaceBody, Statement};

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct Path(Vec<String>);

impl Path {
    pub fn with(&self, path_segment: &str) -> Path {
        let mut new_path = self.clone();
        new_path.0.push(path_segment.to_string());
        new_path
    }

    pub fn std(path_segment: &str) -> Path {
        Path(vec!["std".to_string(), path_segment.to_string()])
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.join("."))
    }
}

// #[derive(Debug)]
// pub struct ResolveNode {
//     children: HashMap<Path, Rc<ResolveNode>>,
//     leaf: bool,
// }

// impl ResolveNode {
//     fn new_leaf() -> ResolveNode {
//         ResolveNode {
//             children: HashMap::default(),
//             leaf: true,
//         }
//     }

//     fn new() -> ResolveNode {
//         ResolveNode {
//             children: HashMap::default(),
//             leaf: false,
//         }
//     }
// }

#[derive(Default, Debug)]
pub struct Resolve {
    types: HashSet<Path>,
    modules: HashSet<Path>,
    functions: HashSet<Path>,
    variables: HashSet<Path>,
}

impl Resolve {
    pub fn resolve(module: tl_core::Module) -> Resolve {
        let mut resolve = Resolve::default();

        let mut path = Path(vec![module.name.clone()]);

        for stmt in &module.stmts {
            resolve.resolve_statement(&mut path, stmt);
        }

        for ty in [
            "int", "int8", "int16", "int32", "int64", "iptr", "uint8", "uint16", "uint32",
            "uint64", "uptr", "float", "float32", "float64", "bool", "char", "char8", "str", "str8", "fixed", "optional", "array", "pointer"
        ] {
            resolve.types.insert(Path::std(ty));
        }

        resolve
    }

    fn resolve_statement(&mut self, current_path: &Path, statement: &Statement) {
        match statement {
            Statement::Class {
                ident: Some(ident),
                body,
                ..
            } => {
                let path = current_path.with(ident.as_str());

                for item in body.iter_items() {
                    self.resolve_class_body(&path, item);
                }

                self.types.insert(path.clone());
            }
            Statement::Interface {
                ident: Some(ident),
                body,
                ..
            } => {
                let path = current_path.with(ident.as_str());

                for item in body.iter_items() {
                    self.resolve_interface_body(&path, item);
                }

                self.types.insert(path.clone());
            }
            Statement::Function { ident, body, .. } => {
                let path = current_path.with(ident.as_str());

                if let Some(body) = body {
                    self.resolve_statement(&path, body);
                }

                self.functions.insert(path);
            }
            Statement::VariableDeclaration { ident, .. } => {
                let path = current_path.with(ident.as_str());
                self.variables.insert(path);
            }
            Statement::Modifer(modifier) => {
                if let Some(stmt) = &modifier.statement {
                    self.resolve_statement(current_path, stmt);
                }
            }
            Statement::TypeAlias { ident, .. } => {
                let path = current_path.with(ident.as_str());
                self.types.insert(path);
            }
            Statement::Expression(expression) => self.resolve_expression(current_path, expression),
            _ => (),
        }
    }
}

impl Resolve {
    fn resolve_class_body(&mut self, current_path: &Path, class_body: &ClassBody) {
        match class_body {
            ClassBody::ComputedField { name, .. } => {
                let path = current_path.with(name.as_str());
                self.variables.insert(path);
            }
            ClassBody::Field { name, .. } => {
                let path = current_path.with(name.as_str());
                self.variables.insert(path);
            }
            ClassBody::Function { ident, body, .. } => {
                let path = current_path.with(ident.as_str());

                if let Some(body) = body {
                    self.resolve_statement(&path, body);
                }

                self.functions.insert(path);
            }
            ClassBody::GetterSetters { name, .. } => {
                let path = current_path.with(name.as_str());
                self.variables.insert(path);
            }
            ClassBody::Modifier(modifier) => {
                if let Some(stmt) = &modifier.statement {
                    self.resolve_class_body(current_path, stmt);
                }
            }
        }
    }
}

impl Resolve {
    fn resolve_interface_body(&mut self, current_path: &Path, interface_body: &InterfaceBody) {
        match interface_body {
            InterfaceBody::Function { ident, .. } => {
                let path = current_path.with(ident.as_str());
                self.functions.insert(path);
            }
            InterfaceBody::ComputedField { name, .. } => {
                let path = current_path.with(name.as_str());
                self.variables.insert(path);
            }
            InterfaceBody::Modifier(modifier) => {
                if let Some(stmt) = &modifier.statement {
                    self.resolve_interface_body(current_path, stmt);
                }
            }
        }
    }
}

impl Resolve {
    fn resolve_expression(&mut self, current_path: &Path, expression: &Expression) {
        match expression {
            Expression::Block(block) => {
                for item in block.iter_items() {
                    self.resolve_statement(current_path, item);
                }
            }
            _ => (),
        }
    }
}

impl Display for Resolve {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("types:\n")?;

        for ty in &self.types {
            f.write_str("    ")?;
            ty.fmt(f)?;
            f.write_char('\n')?;
        }

        f.write_str("modules:\n")?;

        for module in &self.modules {
            f.write_str("    ")?;
            module.fmt(f)?;
            f.write_char('\n')?;
        }

        f.write_str("functions:\n")?;

        for func in &self.functions {
            f.write_str("    ")?;
            func.fmt(f)?;
            f.write_char('\n')?;
        }

        f.write_str("variables:\n")?;

        for var in &self.variables {
            f.write_str("    ")?;
            var.fmt(f)?;
            f.write_char('\n')?;
        }

        Ok(())
    }
}
