use std::{collections::HashMap, sync::Arc, hash::Hasher};

use crate::evaluation_type::EvaluationType;
use crate::evaluation_value::EvaluationValue;

use linked_hash_map::LinkedHashMap;
use tl_core::{ast::{EnclosedList, Param, GenericParameter}, Module};
use tl_util::{Rf, format::{NodeDisplay, TreeDisplay, GrouperIter, BoxedGrouperIter, BoxedGrouper}};

use super::intrinsics::IntrinsicType;

pub enum ScopeValue<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    EvaluationValue(V),
    Struct {
        ident: String,
        members: LinkedHashMap<String, T>,
    },
    StructTemplate {
        ident: String,
        raw_members: EnclosedList<Param>,
        members: LinkedHashMap<String, T>,
        generics: Vec<GenericParameter>,
        constructions: HashMap<Vec<T>, String>,
        construction_start_index: usize,
    },
    IntrinsicStruct {
        initial_value: Rf<dyn IntrinsicType + Send + Sync>,
    },
    IntrinsicStructTemplate {
        initial_value: Rf<dyn IntrinsicType + Send + Sync>,
        generics: Vec<GenericParameter>,
    },
    TypeAlias {
        ident: String,
        ty: Box<T>,
    },
    Use(Vec<String>),
    Module(Arc<Module>),
    Root,
}

impl <T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> NodeDisplay for ScopeValue<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // ScopeValue::ConstValue(ConstValue {
            //     ty: Type::Function { .. },
            //     ..
            // }) => f.write_str("Function"),
            ScopeValue::EvaluationValue(_) => f.write_str("Value"),
            ScopeValue::Struct { .. } => f.write_str("Record"),
            ScopeValue::StructTemplate { .. } => f.write_str("Struct Template"),
            ScopeValue::IntrinsicStruct { .. } => f.write_str("Intrinsic Struct"),
            ScopeValue::IntrinsicStructTemplate { .. } => f.write_str("Intrinsic Struct Template"),
            ScopeValue::TypeAlias { .. } => f.write_str("Type Alias"),
            ScopeValue::Use(_) => f.write_str("Use"),
            ScopeValue::Module(_) => f.write_str("Module"),
            ScopeValue::Root => f.write_str("Root"),
        }
    }
}

impl <T: EvaluationType<Value = V> + TreeDisplay, V: EvaluationValue<Type = T> + TreeDisplay> TreeDisplay for ScopeValue<T, V> {
    fn num_children(&self) -> usize {
        match self {
            ScopeValue::EvaluationValue(c) => c.num_children(),
            ScopeValue::Struct { .. } => 1,
            ScopeValue::StructTemplate { .. } => 2,
            ScopeValue::IntrinsicStruct { .. } => 0,
            ScopeValue::IntrinsicStructTemplate { .. } => 1,
            ScopeValue::TypeAlias { .. } => 2,
            ScopeValue::Use(s) => s.len(),
            ScopeValue::Module(_) => 0,
            ScopeValue::Root => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ScopeValue::EvaluationValue(c) => c.child_at(index),
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
            ScopeValue::IntrinsicStruct { .. } => None,
            ScopeValue::IntrinsicStructTemplate { generics, .. } => Some(generics),
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

pub struct Scope<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    pub name: String,
    pub value: ScopeValue<T, V>,
    pub parent: Option<Rf<Scope<T, V>>>,
    pub children: LinkedHashMap<String, Rf<Scope<T, V>>>,
    pub uses: Vec<Vec<String>>,
    pub index: usize,
}

impl <T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> Scope<T, V> {
    pub fn root() -> Scope<T, V> {
        Scope {
            name: "".to_string(),
            value: ScopeValue::Root,
            parent: None,
            children: LinkedHashMap::new(),
            uses: Vec::new(),
            index: 0,
        }
    }

    pub fn new(parent: Rf<Scope<T, V>>, name: String, value: ScopeValue<T, V>, index: usize) -> Scope<T, V> {
        Scope {
            name: name.to_string(),
            value,
            parent: Some(parent),
            children: LinkedHashMap::new(),
            uses: Vec::new(),
            index,
        }
    }

    pub fn with_children(mut self, children: LinkedHashMap<String, Rf<Scope<T, V>>>) -> Self {
        self.children = children;
        self
    }

    pub fn hash<H: Hasher>(self_rf: &Rf<Scope<T, V>>, state: &mut H) {
        Self::iter(self_rf, &mut |sym| {
            let item = sym.borrow();
            state.write_str(&item.name);
        });
    }

    pub fn iter(self_rf: &Rf<Scope<T, V>>, cb: &mut impl FnMut(&Rf<Scope<T, V>>)) {
        let slf = self_rf.borrow();
        if let Some(prnt) = &slf.parent {
            // let parent = prnt.borrow();
            Self::iter(self_rf, cb);
            cb(prnt)
        }
    }

    pub fn insert(
        &mut self,
        self_rf: Rf<Scope<T, V>>,
        name: String,
        val: ScopeValue<T, V>,
        index: usize,
    ) -> Rf<Scope<T, V>> {
        let rf = Rf::new(Scope::new(self_rf, name.to_string(), val, index));

        self.children.insert(name, rf.clone());

        rf
    }

    pub fn insert_node(&mut self, scope: Scope<T, V>) -> Rf<Scope<T, V>> {
        let name = scope.name.clone();
        let rf = Rf::new(scope);

        self.children.insert(name, rf.clone());

        rf
    }

    pub fn update(&mut self, name: &str, val: ScopeValue<T, V>) -> Option<Rf<Scope<T, V>>> {
        if let Some(vs) = self.children.get_mut(name) {
            vs.borrow_mut().value = val;
            return Some(vs.clone());
        }
        None
    }
}

impl <T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> NodeDisplay for Scope<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Scope - {} {}", self.index, self.children.len())
    }
}

impl <T: EvaluationType<Value = V> + TreeDisplay, V: EvaluationValue<Type = T> + TreeDisplay> TreeDisplay for Scope<T, V> {
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