use std::{collections::HashMap, hash::Hasher, io::Read, sync::Arc};

use linked_hash_map::LinkedHashMap;
use tl_core::{
    ast::{EnclosedList, EnclosedPunctuationList, Expression, GenericParameter, Param},
    token::{Operator, SpannedToken, Token},
    Module,
};
use tl_util::{
    format::{BoxedGrouper, BoxedGrouperIter, GrouperIter, NodeDisplay, TreeDisplay},
    Rf,
};

use crate::const_value::{ConstValue, ConstValueKind, Type};

#[derive(Clone)]
pub enum ScopeValue {
    ConstValue(ConstValue),
    Struct {
        ident: String,
        members: LinkedHashMap<String, Type>,
    },
    StructTemplate {
        ident: String,
        raw_members: EnclosedList<Param>,
        members: LinkedHashMap<String, Type>,
        generics: Vec<GenericParameter>,
        constructions: HashMap<Vec<Type>, String>,
        construction_start_index: usize,
    },
    TypeAlias {
        ident: String,
        ty: Box<Type>,
    },
    Use(Vec<String>),
    Module(Arc<Module>),
    Root,
}

impl NodeDisplay for ScopeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScopeValue::ConstValue(ConstValue {
                ty: Type::Function { .. },
                ..
            }) => f.write_str("Function"),
            ScopeValue::ConstValue(_) => f.write_str("Constant Value"),
            ScopeValue::Struct { .. } => f.write_str("Record"),
            ScopeValue::StructTemplate { .. } => f.write_str("Struct Template"),
            ScopeValue::TypeAlias { .. } => f.write_str("Type Alias"),
            ScopeValue::Use(_) => f.write_str("Use"),
            ScopeValue::Module(_) => f.write_str("Module"),
            ScopeValue::Root => f.write_str("Root"),
        }
    }
}

impl TreeDisplay for ScopeValue {
    fn num_children(&self) -> usize {
        match self {
            ScopeValue::ConstValue(c) => c.num_children(),
            ScopeValue::Struct { .. } => 1,
            ScopeValue::StructTemplate { .. } => 2,
            ScopeValue::TypeAlias { .. } => 2,
            ScopeValue::Use(s) => s.len(),
            ScopeValue::Module(_) => 0,
            ScopeValue::Root => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ScopeValue::ConstValue(c) => c.child_at(index),
            ScopeValue::Struct { members, .. } => Some(members),
            ScopeValue::StructTemplate {
                members,
                constructions,
                ..
            } => match index {
                0 => Some(members),
                1 => Some(constructions),
                _ => None,
            },
            ScopeValue::TypeAlias { ident, ty } => match index {
                0 => Some(ident),
                1 => Some(&**ty),
                _ => None,
            },
            ScopeValue::Use(s) => s.child_at(index),
            ScopeValue::Module(_) => None,
            ScopeValue::Root => None,
        }
    }
}

pub struct Scope {
    pub name: String,
    pub value: ScopeValue,
    pub parent: Option<Rf<Scope>>,
    pub children: LinkedHashMap<String, Rf<Scope>>,
    pub uses: Vec<Vec<String>>,
    pub index: usize,
}

impl Scope {
    pub fn root() -> Scope {
        Scope {
            name: "".to_string(),
            value: ScopeValue::Root,
            parent: None,
            children: LinkedHashMap::new(),
            uses: Vec::new(),
            index: 0,
        }
    }

    pub fn new(parent: Rf<Scope>, name: String, value: ScopeValue, index: usize) -> Scope {
        Scope {
            name: name.to_string(),
            value,
            parent: Some(parent),
            children: LinkedHashMap::new(),
            uses: Vec::new(),
            index,
        }
    }

    pub fn with_children(mut self, children: LinkedHashMap<String, Rf<Scope>>) -> Self {
        self.children = children;
        self
    }

    pub fn hash<H: Hasher>(self_rf: &Rf<Scope>, state: &mut H) {
        Self::iter(self_rf, &mut |sym| {
            let item = sym.borrow();
            state.write_str(&item.name);
        });
    }

    pub fn iter(self_rf: &Rf<Scope>, cb: &mut impl FnMut(&Rf<Scope>)) {
        let slf = self_rf.borrow();
        if let Some(prnt) = &slf.parent {
            // let parent = prnt.borrow();
            Self::iter(self_rf, cb);
            cb(prnt)
        }
    }

    pub fn insert(
        &mut self,
        self_rf: Rf<Scope>,
        name: String,
        val: ScopeValue,
        index: usize,
    ) -> Rf<Scope> {
        let rf = Rf::new(Scope::new(self_rf, name.to_string(), val, index));

        self.children.insert(name, rf.clone());

        rf
    }

    pub fn insert_node(&mut self, scope: Scope) -> Rf<Scope> {
        let name = scope.name.clone();
        let rf = Rf::new(scope);

        self.children.insert(name, rf.clone());

        rf
    }

    pub fn update(&mut self, name: &str, val: ScopeValue) -> Option<Rf<Scope>> {
        if let Some(vs) = self.children.get_mut(name) {
            vs.borrow_mut().value = val;
            return Some(vs.clone());
        }
        None
    }
}

impl NodeDisplay for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Scope - {} {}", self.index, self.children.len())
    }
}

impl TreeDisplay for Scope {
    fn num_children(&self) -> usize {
        1 + (if !self.children.is_empty() { 1 } else { 0 })
            + (if !self.uses.is_empty() { 1 } else { 0 })
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay<()>> {
        match _index {
            0 => Some(&self.value),
            // 1 => Some(&self.index),
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match _index {
            1 if !self.uses.is_empty() => Box::new(GrouperIter(
                "Use".to_string(),
                self.uses.len(),
                self.uses.iter().map(|f| f as &'a dyn TreeDisplay),
            )),
            _ => Box::new(BoxedGrouperIter(
                "Children".to_string(),
                self.children.len(),
                self.children.iter().map(|f| {
                    Box::new(BoxedGrouper(f.0.clone(), Box::new(f.1.borrow())))
                        as Box<dyn TreeDisplay>
                }),
            )),
        }
    }
}

#[derive(Clone)]
pub struct ScopeRef(usize, String);

pub struct ScopeManager {
    root: Rf<Scope>,
    pub module: Rf<Scope>,
    current_scope: Vec<Rf<Scope>>,
}

impl<'a> ScopeManager {
    pub fn new(root: Rf<Scope>, module: Rf<Scope>) -> ScopeManager {
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

    pub fn add_use(&mut self, path: Vec<String>) {
        if let Some(sym) = self.current_scope.last() {
            let mut sym = sym.borrow_mut();
            sym.uses.push(path)
        }
    }

    pub fn push_scope_chain<'b>(
        &self,
        buf: &mut Vec<Rf<Scope>>,
        chain: impl Iterator<Item = &'a usize>,
    ) {
        self.push_scope_chain_impl(&self.root, buf, chain)
    }

    pub fn push_scope_chain_impl<'b>(
        &self,
        node: &Rf<Scope>,
        buf: &mut Vec<Rf<Scope>>,
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

    pub fn push_scope(&mut self, rf: Rf<Scope>) {
        self.current_scope.push(rf);
    }

    pub fn pop_scope(&mut self) -> Rf<Scope> {
        self.current_scope.remove(self.current_scope.len() - 1)
    }

    fn follow_member_access_leaf(
        &'a mut self,
        left: &Expression,
        right: &Expression,
        mut cb: impl FnMut(&mut ConstValue),
    ) -> bool {
        match (left, right) {
            (Expression::Ident(left), Expression::Ident(right)) => {
                let Some(sym) = self.find_symbol(left.as_str()) else {
                    return false;
                };
                let mut sym = sym.borrow_mut();
                let ScopeValue::ConstValue(
                        ConstValue {
                            ty: Type::StructInstance { .. },
                            kind: ConstValueKind::StructInstance { members, .. }
                        }
                    ) = &mut sym.value else {
                        return false
                    };

                if let Some(m) = members.get_mut(right.as_str()) {
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
        mut cb: impl FnMut(&mut ConstValue),
    ) -> bool {
        match (left, right) {
            (Expression::Ident(left), Expression::Ident(right)) => {
                let Some(sym) = self.find_symbol(left.as_str()) else {
                    return false;
                };
                let mut sym = sym.borrow_mut();
                let ScopeValue::ConstValue(
                        ConstValue {
                            ty: Type::StructInstance { .. },
                            kind: ConstValueKind::StructInstance { members, .. }
                        }
                    ) = &mut sym.value else {
                        return false
                    };

                if let Some(m) = members.get_mut(right.as_str()) {
                    cb(m);
                }
                return true;
            }
            (
                Expression::BinaryExpression {
                    op_token: Some(SpannedToken(_, Token::Operator(Operator::Dot))),
                    left: Some(left),
                    right: Some(right),
                },
                Expression::Ident(member_right),
            ) => {
                return self.follow_member_access_leaf(left, right, |cv| {
                    let ConstValue {
                        ty: Type::StructInstance { .. },
                        kind: ConstValueKind::StructInstance { members, .. }
                    } = cv else {
                        return;
                    };

                    if let Some(m) = members.get_mut(member_right.as_str()) {
                        cb(m);
                    }
                });
            }
            _ => (),
        }

        false
    }

    pub fn find_symbol_local(&'a self, name: &str) -> Option<Rf<Scope>> {
        self.current_scope
            .last()
            .and_then(|scope| scope.borrow().children.get(name).cloned())
    }

    pub fn find_symbol_in_mod(&'a self, name: &str) -> Option<Rf<Scope>> {
        self.current_scope
            .iter()
            .rev()
            .find_map(|scope| scope.borrow().children.get(name).cloned())
    }

    pub fn find_symbol(&'a self, name: &str) -> Option<Rf<Scope>> {
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

    // pub fn find_symbol_local_in_scope(&'a self, name: &str, scope: &Vec<usize>) -> Option<Rf<Scope>> {
    //    scope
    //         .last()
    //         .and_then(|scope| scope.borrow().children.get(name).cloned())
    // }

    pub fn find_symbol_in_mod_in_scope(
        &'a self,
        name: &str,
        scope: &Vec<Rf<Scope>>,
    ) -> Option<Rf<Scope>> {
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

    pub fn find_symbol_in_scope(&'a self, name: &str, scope: &Vec<Rf<Scope>>) -> Option<Rf<Scope>> {
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
    ) -> Option<Rf<Scope>> {
        self.resolve_symbol_indicies_impl(&self.root, name, indicies)
    }

    pub fn resolve_symbol_indicies_impl<'b>(
        &self,
        node: &Rf<Scope>,
        name: &str,
        mut indicies: impl Iterator<Item = &'b usize>,
    ) -> Option<Rf<Scope>> {
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

    pub fn resolve_use(&self, use_path: &[String], cb: impl Fn(&Rf<Scope>)) -> Option<Rf<Scope>> {
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
        node: &Rf<Scope>,
        use_path: &[String],
        cb: impl Fn(&Rf<Scope>),
    ) -> Option<Rf<Scope>> {
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
        mut cb: impl FnMut(&Rf<Scope>, U),
    ) -> Option<Rf<Scope>> {
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
        node: &Rf<Scope>,
        mut path: impl Iterator<Item = (&'b str, U)>,
        mut cb: impl FnMut(&Rf<Scope>, U),
    ) -> Option<Rf<Scope>> {
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
        value: ScopeValue,
        index: usize,
    ) -> Option<ScopeValue> {
        println!("Update value");
        // tl_util::set_backtrace(false);
        // std::env::set_var("RUST_LIB_BACKTRACE", "0");
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
        // std::env::set_var("RUST_LIB_BACKTRACE", "1");

        None
    }

    pub fn insert_value(&mut self, name: &str, value: ScopeValue, index: usize) -> Rf<Scope> {
        if let Some(scp) = self.current_scope.last() {
            let rf = Rf::new(Scope::new(scp.clone(), name.to_string(), value, index));
            scp.borrow_mut()
                .children
                .insert(name.to_string(), rf.clone());
            return rf;
        }
        panic!()
    }
}

impl NodeDisplay for ScopeManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Scope Manager")
    }
}

impl TreeDisplay for ScopeManager {
    fn num_children(&self) -> usize {
        1
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay<()>> {
        None
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
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
