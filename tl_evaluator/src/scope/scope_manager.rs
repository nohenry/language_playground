use tl_core::{
    ast::Expression,
    token::{Operator, SpannedToken, Token},
};
use tl_util::{
    format::{BoxedGrouperIter, Config, NodeDisplay, TreeDisplay},
    Rf,
};

use crate::{evaluation_type::EvaluationType, evaluation_value::EvaluationValue};

use super::scope::{Scope, ScopeValue};

#[derive(Clone)]
pub struct ScopeRef(usize, String);

#[derive(Clone)]
pub struct ScopeManager<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    root: Rf<Scope<T, V>>,
    pub module: Rf<Scope<T, V>>,
    current_scope: Vec<Rf<Scope<T, V>>>,
}

impl<'a, T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> ScopeManager<T, V> {
    pub fn new(root: Rf<Scope<T, V>>, module: Rf<Scope<T, V>>) -> ScopeManager<T, V> {
        let name = if let ScopeValue::Module(modu) = &module.borrow().value {
            modu.name.clone()
        } else {
            "".to_string()
        };

        root.borrow_mut().children.insert(name, module.clone());

        let mut vec = Vec::with_capacity(20);
        vec.push(module.clone());

        ScopeManager {
            root,
            module,
            current_scope: vec,
        }
    }

    pub fn reset_current_scope(&mut self) {
        self.current_scope.truncate(1);
    }

    pub fn add_use(&mut self, path: Vec<String>) {
        if let Some(sym) = self.current_scope.last() {
            let mut sym = sym.borrow_mut();
            sym.uses.push(path)
        }
    }

    pub fn push_scope_chain<'b>(
        &self,
        buf: &mut Vec<Rf<Scope<T, V>>>,
        chain: impl Iterator<Item = &'a usize>,
    ) {
        self.push_scope_chain_impl(&self.root, buf, chain)
    }

    pub fn push_scope_chain_impl<'b>(
        &self,
        node: &Rf<Scope<T, V>>,
        buf: &mut Vec<Rf<Scope<T, V>>>,
        mut chain: impl Iterator<Item = &'b usize>,
    ) {
        if let Some(next) = chain.next() {
            if let Some(child) = node
                .borrow()
                .children
                .iter()
                .find(|f| f.1.borrow().index == *next)
            {
                buf.push(child.1.clone());

                self.push_scope_chain_impl(child.1, buf, chain);
            }
        }
    }

    pub fn push_scope(&mut self, rf: Rf<Scope<T, V>>) {
        self.current_scope.push(rf);
    }

    pub fn pop_scope(&mut self) -> Rf<Scope<T, V>> {
        self.current_scope.remove(self.current_scope.len() - 1)
    }

    fn follow_member_access_leaf(
        &'a mut self,
        left: &Expression,
        right: &Expression,
        mut cb: impl FnMut(&mut V),
    ) -> bool {
        match (left, right) {
            (Expression::Ident(left), Expression::Ident(right)) => {
                let Some(sym) = self.find_symbol(left.as_str()) else {
                    return false;
                };
                let mut sym = sym.borrow_mut();
                // let ScopeValue::EvaluationValue(
                //         value
                //     ) = &mut sym.value else {
                //         if
                //         return false
                //     };

                let value = match &mut sym.value {
                    ScopeValue::EvaluationValue(value) if value.is_struct_instance() => value,
                    _ => return false,
                };

                if let Some(m) = value.get_struct_member_mut(right.as_str()) {
                    cb(m);
                }
                return true;
            }
            _ => (),
        }

        false
    }

    pub fn follow_member_access_mut(
        &'a mut self,
        left: &Expression,
        right: &Expression,
        mut cb: impl FnMut(&mut V),
    ) -> bool {
        match (left, right) {
            (Expression::Ident(left), Expression::Ident(right)) => {
                let Some(sym) = self.find_symbol(left.as_str()) else {
                    return false;
                };
                let mut sym = sym.borrow_mut();

                let value = match &mut sym.value {
                    ScopeValue::EvaluationValue(value) if value.is_struct_instance() => value,
                    _ => return false,
                };

                if let Some(m) = value.get_struct_member_mut(right.as_str()) {
                    cb(m);
                }
                return true;
            }
            (
                Expression::BinaryExpression {
                    op_token: SpannedToken(_, Token::Operator(Operator::Dot)),
                    left: Some(left),
                    right: Some(right),
                },
                Expression::Ident(member_right),
            ) => {
                return self.follow_member_access_leaf(left, right, |cv| {
                    if !cv.is_struct_instance() {
                        return;
                    }

                    if let Some(m) = cv.get_struct_member_mut(member_right.as_str()) {
                        cb(m);
                    }
                });
            }
            _ => (),
        }

        false
    }

    pub fn find_symbol_local(&'a self, name: &str) -> Option<Rf<Scope<T, V>>> {
        self.current_scope
            .last()
            .and_then(|scope| scope.borrow().children.get(name).cloned())
    }

    pub fn find_symbol_in_mod(&'a self, name: &str) -> Option<Rf<Scope<T, V>>> {
        self.current_scope
            .iter()
            .rev()
            .find_map(|scope| scope.borrow().children.get(name).cloned())
    }

    pub fn find_symbol(&'a self, name: &str) -> Option<Rf<Scope<T, V>>> {
        if let Some(sym) = self.find_symbol_in_mod(name) {
            return Some(sym);
        }

        let use_found = self.current_scope.iter().rev().find_map(|scope| {
            scope.borrow().uses.iter().find_map(|us| {
                let node = self.resolve_use(us, |_| {})?;
                let node = node.borrow();
                node.children.get(name).cloned()
            })
        });
        if let Some(fnd) = use_found {
            return Some(fnd);
        }

        None
    }

    pub fn index_of_mod(&self, name: &str) -> Option<usize> {
        self.root.borrow().children.iter().position(|f| f.0 == name)
    }

    // pub fn find_symbol_local_in_scope(&'a self, name: &str, scope: &Vec<usize>) -> Option<Rf<Scope<T, V>>> {
    //    scope
    //         .last()
    //         .and_then(|scope| scope.borrow().children.get(name).cloned())
    // }

    pub fn find_symbol_in_mod_in_scope(
        &'a self,
        name: &str,
        scope: &Vec<Rf<Scope<T, V>>>,
    ) -> Option<Rf<Scope<T, V>>> {
        scope
            .iter()
            .rev()
            .find_map(|scope| scope.borrow().children.get(name).cloned())

        // let mut curr = self.root.clone();
        // scope
        //     .iter()
        //     .map(|f| {
        //         let p = if let Some(sym) = curr.borrow().children.iter().nth(*f) {
        //             sym.1.clone()
        //         } else {
        //             return None
        //         };
        //         curr = p.clone();
        //         Some(p)
        //     })
        //     // .rev()
        //     .find_map(|scope| scope?.borrow().children.get(name).cloned())
    }

    pub fn find_symbol_in_scope(
        &'a self,
        name: &str,
        scope: &Vec<Rf<Scope<T, V>>>,
    ) -> Option<Rf<Scope<T, V>>> {
        if let Some(sym) = self.find_symbol_in_mod_in_scope(name, scope) {
            return Some(sym);
        }

        let use_found = scope.iter().rev().find_map(|scope| {
            scope.borrow().uses.iter().find_map(|us| {
                let node = self.resolve_use(us, |_| {})?;
                let node = node.borrow();
                node.children.get(name).cloned()
            })
        });
        if let Some(fnd) = use_found {
            return Some(fnd);
        }

        None
    }

    pub fn resolve_symbol_indicies<'b>(
        &self,
        name: &str,
        indicies: impl Iterator<Item = &'b usize>,
    ) -> Option<Rf<Scope<T, V>>> {
        self.resolve_symbol_indicies_impl(&self.root, name, indicies)
    }

    pub fn resolve_symbol_indicies_impl<'b>(
        &self,
        node: &Rf<Scope<T, V>>,
        name: &str,
        mut indicies: impl Iterator<Item = &'b usize>,
    ) -> Option<Rf<Scope<T, V>>> {
        if let Some(next) = indicies.next() {
            let node = node.borrow();
            let node = node.children.iter().find(|f| f.1.borrow().index == *next)?;

            self.resolve_symbol_indicies_impl(node.1, name, indicies)
            // if let Some(sym) = self.resolve_symbol_indicies_impl(&node.1, name, indicies) {
            //     return Some(sym)
            // } else {
            //     node.1.borrow().children.get(name).cloned()
            // }
        } else {
            // if let Some(sym) = node.borrow().children.get(name) {
            //     return Some(sym.clone())
            // }
            // None
            Some(node.clone())
        }
    }

    pub fn resolve_use(
        &self,
        use_path: &[String],
        cb: impl Fn(&Rf<Scope<T, V>>),
    ) -> Option<Rf<Scope<T, V>>> {
        if let Some(start) = use_path.first() {
            if let Some(sym) = self.find_symbol_in_mod(start) {
                cb(&sym);
                return self.resolve_use_impl(&sym, &use_path[1..], cb);
            }

            if let Some(sym) = self.root.borrow().children.get(start) {
                cb(sym);
                return self.resolve_use_impl(sym, &use_path[1..], cb);
            }
        }
        None
    }

    pub fn resolve_use_impl(
        &self,
        node: &Rf<Scope<T, V>>,
        use_path: &[String],
        cb: impl Fn(&Rf<Scope<T, V>>),
    ) -> Option<Rf<Scope<T, V>>> {
        if use_path.is_empty() {
            match &node.borrow().value {
                ScopeValue::Use(u) => return self.resolve_use(u, cb),
                _ => (),
            }
            return Some(node.clone());
        } else if let Some(first) = use_path.first() {
            if let Some(child) = node.borrow().children.get(first) {
                cb(child);
                return self.resolve_use_impl(child, &use_path[1..], cb);
            }
        }
        None
    }

    pub fn iter_use<'b, U>(
        &self,
        mut path: impl Iterator<Item = (&'b str, U)>,
        mut cb: impl FnMut(&Rf<Scope<T, V>>, U),
    ) -> Option<Rf<Scope<T, V>>> {
        if let Some(start) = path.next() {
            if let Some(sym) = self.find_symbol_in_mod(start.0) {
                cb(&sym, start.1);
                return self.iter_use_impl(&sym, path, cb);
            }

            if let Some(sym) = self.root.borrow().children.get(start.0) {
                cb(sym, start.1);
                return self.iter_use_impl(sym, path, cb);
            }
        }
        None
    }

    pub fn iter_use_impl<'b, U>(
        &self,
        node: &Rf<Scope<T, V>>,
        mut path: impl Iterator<Item = (&'b str, U)>,
        mut cb: impl FnMut(&Rf<Scope<T, V>>, U),
    ) -> Option<Rf<Scope<T, V>>> {
        if let Some(nd) = path.next() {
            if let Some(child) = node.borrow().children.get(nd.0) {
                cb(child, nd.1);
                return self.iter_use_impl(child, path, cb);
            }
        } else {
            match &node.borrow().value {
                ScopeValue::Use(u) => return self.resolve_use(u, |_| {}),
                _ => (),
            }
            return Some(node.clone());
        }

        None
    }

    pub fn update_value(
        &mut self,
        name: &str,
        value: ScopeValue<T, V>,
        index: usize,
    ) -> Option<ScopeValue<T, V>> {
        println!("Update value");
        // tl_util::set_backtrace(false);
        // std::enV::set_var("RUST_LIB_BACKTRACE", "0", self.type_provider.as_ref(), self.type_provider.as_ref());
        {
            if let Some(sym) = { self.find_symbol(name) } {
                let old_value = std::mem::replace(&mut sym.borrow_mut().value, value);
                return Some(old_value);
            }
        }

        if let Some(scp) = self.current_scope.last() {
            scp.borrow_mut().children.insert(
                name.to_string(),
                Rf::new(Scope::new(scp.clone(), name.to_string(), value, index)),
            );
        }
        // tl_util::set_backtrace(true);
        // std::enV::set_var("RUST_LIB_BACKTRACE", "1", self.type_provider.as_ref(), self.type_provider.as_ref());

        None
    }

    pub fn insert_value(
        &mut self,
        name: &str,
        value: ScopeValue<T, V>,
        index: usize,
    ) -> Rf<Scope<T, V>> {
        if let Some(scp) = self.current_scope.last() {
            let rf = Rf::new(Scope::new(scp.clone(), name.to_string(), value, index));
            scp.borrow_mut()
                .children
                .insert(name.to_string(), rf.clone());
            return rf;
        }
        panic!()
    }

    pub fn remove_value(&mut self, name: &str) {
        if let Some(scp) = self.current_scope.last() {
            let mut scope = scp.borrow_mut();
            scope.children.remove(name);
        }
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> NodeDisplay
    for ScopeManager<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Scope Manager")
    }
}

impl<T: EvaluationType<Value = V> + TreeDisplay, V: EvaluationValue<Type = T> + TreeDisplay>
    TreeDisplay for ScopeManager<T, V>
{
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        None
    }

    fn child_at_bx<'a>(&'a self, _index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<()> + 'a> {
        match _index {
            0 => Box::new(self.module.borrow()),
            1 => Box::new(BoxedGrouperIter(
                "Curent Scope".to_string(),
                self.current_scope.len(),
                self.current_scope
                    .iter()
                    .map(|f| Box::new(f.borrow()) as Box<dyn TreeDisplay>),
            )),
            _ => panic!(),
        }
    }
}
