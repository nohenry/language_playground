use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use linked_hash_map::LinkedHashMap;
use tl_core::{
    ast::{AstNode, EnclosedList, Expression, GenericParameter, Param, ParamaterList, Statement},
    token::{Range, SpannedToken, Token},
    Module,
};
use tl_util::{format::TreeDisplay, Rf};

use crate::{
    const_value::{ConstValue, Type},
    error::EvaluationError,
    scope::{Scope, ScopeManager, ScopeValue},
};

pub enum PassType {
    TypeOnly,
    SecondType,
    Members,
    // Variables,
}

pub struct CodePassState {
    pub scope: ScopeManager,
    pub errors: Vec<EvaluationError>,
}

pub struct CodePass {
    module: Arc<Module>,
    state: RwLock<CodePassState>,
    pass: PassType,
}

impl CodePass {
    pub fn new(root: Rf<Scope>, module: Arc<Module>, index: usize) -> CodePass {
        let scope = Rf::new(Scope::new(
            root.clone(),
            module.name.to_string(),
            ScopeValue::Module(module.clone()),
            index,
        ));
        CodePass {
            module,
            state: RwLock::new(CodePassState {
                scope: ScopeManager::new(root, scope),
                errors: Vec::new(),
            }),
            pass: PassType::TypeOnly,
        }
    }

    fn rstate(&self) -> RwLockReadGuard<'_, CodePassState> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, CodePassState> {
        self.state.write().unwrap()
    }
}

impl CodePass {
    pub fn run(mut self) -> CodePassState {
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index);
        }

        self.pass = PassType::Members;
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index);
        }

        self.state.into_inner().unwrap()
    }

    pub fn evaluate_statement(&self, statement: &Statement, index: usize) {
        match statement {
            // Struct decleration
            Statement::TypeAlias {
                ident,
                generic,
                ty: box tl_core::ast::Type::Struct(members),
                ..
            } => {
                match self.pass {
                    PassType::TypeOnly => {
                        let scope = if let Some(generics) = generic {
                            self.wstate().scope.insert_value(
                                ident.as_str(),
                                ScopeValue::StructTemplate {
                                    ident: ident.as_str().to_string(),
                                    raw_members: members.clone(),
                                    members: LinkedHashMap::default(),
                                    generics: generics.iter_items().cloned().collect(),
                                    constructions: HashMap::new(),
                                    construction_start_index: 0,
                                },
                                index,
                            )
                        } else {
                            self.wstate().scope.insert_value(
                                ident.as_str(),
                                ScopeValue::Struct {
                                    ident: ident.as_str().to_string(),
                                    members: LinkedHashMap::default(),
                                },
                                index,
                            )
                        };

                        if let Some(generic) = generic {
                            {}

                            self.wstate().scope.push_scope(scope);

                            for param in generic.iter_items() {
                                match param {
                                    GenericParameter::Unbounded(b) => {
                                        self.wstate().scope.insert_value(
                                            b.as_str(),
                                            ScopeValue::TypeAlias {
                                                ident: b.as_str().to_string(),
                                                ty: Box::new(Type::Integer {
                                                    width: 8,
                                                    signed: false,
                                                }),
                                            },
                                            index,
                                        );
                                        // self.wstate().scope.insert_value(b.as_str(), ScopeValue::ConstValue(ConstValue::empty()), index);
                                    }
                                    _ => todo!(),
                                }
                            }

                            self.wstate().scope.pop_scope();
                        }
                    }
                    PassType::Members => {
                        let Some(sym) = ({ self.wstate().scope.find_symbol(ident.as_str()) }) else {
                            return;
                        };
                        self.wstate().scope.push_scope(sym.clone());

                        let emembers = self.evaluate_struct_members(members);

                        let mut sym = sym.borrow_mut();
                        if let ScopeValue::Struct { members, .. }
                        | ScopeValue::StructTemplate { members, .. } = &mut sym.value
                        {
                            *members = emembers
                        }

                        self.wstate().scope.pop_scope();
                        // }
                    }
                    _ => (),
                };
            }
            Statement::TypeAlias {
                ident,
                generic: None,
                ty,
                ..
            } => match self.pass {
                PassType::TypeOnly => {
                    self.wstate().scope.insert_value(
                        ident.as_str(),
                        ScopeValue::TypeAlias {
                            ident: ident.as_str().to_string(),
                            ty: Box::new(Type::Empty),
                        },
                        index,
                    );
                }
                PassType::Members => {
                    let expr_ty = self.evaluate_type(ty);
                    if let Some(sym) = self.wstate().scope.find_symbol(ident.as_str()) {
                        let mut sym = sym.borrow_mut();
                        if let ScopeValue::TypeAlias { ty, .. } = &mut sym.value {
                            *ty.as_mut() = expr_ty
                        }
                    }
                }
                _ => (),
            },
            Statement::Impl {
                generics,
                ty: Some(ty),
                body: Some(body),
                ..
            } => match self.pass {
                PassType::Members => {
                    let ty = if let Some(generics) = generics {
                        let tmp_scope = self.wstate().scope.insert_value(
                            "tmp_generics",
                            ScopeValue::Root,
                            index,
                        );

                        self.wstate().scope.push_scope(tmp_scope.clone());

                        for param in generics.iter_items() {
                            match param {
                                GenericParameter::Unbounded(b) => {
                                    self.wstate().scope.insert_value(
                                        b.as_str(),
                                        ScopeValue::TypeAlias {
                                            ident: b.as_str().to_string(),
                                            ty: Box::new(Type::Integer {
                                                width: 8,
                                                signed: false,
                                            }),
                                        },
                                        index,
                                    );
                                }
                                _ => todo!(),
                            }
                        }

                        let ty = self.evaluate_type(ty);
                        println!("Type: {}", ty.format());

                        self.wstate().scope.pop_scope();

                        self.wstate().scope.remove_value("tmp_generics");

                        ty
                    } else {
                        self.evaluate_type(ty)
                    };

                    match ty {
                        Type::Symbol(sym) => {
                            self.wstate().scope.push_scope(sym.clone());

                            for (i, stmt) in body.iter_items().enumerate() {
                                self.evaluate_member_impl_stmt(&sym, stmt, i);
                            }

                            self.wstate().scope.pop_scope();
                        }
                        _ => (),
                    }
                }
                _ => (),
            },
            Statement::Function {
                ident,
                parameters,
                return_type,
                body: Some(body),
                ..
            } => match self.pass {
                PassType::TypeOnly => {
                    // return;
                    let sym = self.wstate().scope.insert_value(
                        ident.as_str(),
                        ScopeValue::ConstValue(ConstValue::empty()),
                        index,
                    );

                    let eparameters = self.evaluate_params(parameters);
                    let ereturn = return_type
                        .as_ref()
                        .map(|ty| self.evaluate_type(ty))
                        .unwrap_or(Type::Empty);

                    self.wstate().scope.update_value(
                        ident.as_str(),
                        ScopeValue::ConstValue(ConstValue::func(
                            Statement::clone(body),
                            eparameters,
                            ereturn,
                            sym,
                        )),
                        index,
                    );
                }
                PassType::Members => {
                    // return;
                    let Some(rf) = self.rstate().scope.find_symbol(ident.as_str()) else {
                        return;
                    };
                    let (pvals, _) = {
                        let ScopeValue::ConstValue(ConstValue {
                        ty: Type::Function { parameters, return_type},
                        ..
                    }) = &rf.borrow().value else {
                        return;
                    };

                        let pvals: Vec<_> = parameters
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    ScopeValue::ConstValue(ConstValue::default_for(ty)),
                                )
                            })
                            .collect();

                        (pvals, return_type.clone())
                    };

                    self.wstate().scope.push_scope(rf);

                    for (name, ty) in pvals {
                        self.wstate().scope.update_value(&name, ty, index);
                    }

                    self.wstate().scope.pop_scope();
                }
                _ => (),
            },
            Statement::Decleration { ident, .. } => match self.pass {
                PassType::TypeOnly => {
                    self.wstate().scope.insert_value(
                        ident.as_str(),
                        ScopeValue::ConstValue(ConstValue::empty()),
                        index,
                    );
                }
                _ => (),
            },
            Statement::UseStatement { args, .. } => match self.pass {
                PassType::TypeOnly => {
                    let path = args
                        .iter_items()
                        .map(|sym| sym.as_str().to_string())
                        .collect();
                    self.wstate().scope.add_use(path)
                }
                _ => (),
            },
            _ => (),
        }
    }

    pub fn evaluate_member_impl_stmt(
        &self,
        member_ty: &Rf<Scope>,
        statement: &Statement,
        index: usize,
    ) {
        match statement {
            Statement::Function {
                ident,
                parameters,
                return_type,
                body: Some(body),
                ..
            } => {
                let sym = self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::ConstValue(ConstValue::empty()),
                    index,
                );

                let mut eparameters = self.evaluate_params(parameters);
                eparameters.insert(
                    "self".to_string(),
                    Type::Ref {
                        base_type: Box::new(Type::Symbol(member_ty.clone())),
                        // base_type: Box::new(Type::Boolean),
                    },
                );

                let ereturn = return_type
                    .as_ref()
                    .map(|ty| self.evaluate_type(ty))
                    .unwrap_or(Type::Empty);

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::ConstValue(ConstValue::func(
                        Statement::clone(body),
                        eparameters,
                        ereturn,
                        sym.clone(),
                    )),
                    index,
                );

                // let (pvals, _) = {
                //     let ScopeValue::ConstValue(ConstValue {
                //         ty: Type::Function { parameters, return_type},
                //         ..
                //     }) = &sym.borrow().value else {
                //         return;
                //     };

                //     let pvals: Vec<_> = parameters
                //         .iter()
                //         .map(|(name, ty)| {
                //             (
                //                 name.clone(),
                //                 ScopeValue::ConstValue(ConstValue::default_for(ty)),
                //             )
                //         })
                //         .collect();

                //     (pvals, return_type.clone())
                // };

                // self.wstate().scope.push_scope(sym);

                // for (name, ty) in pvals {
                //     self.wstate().scope.update_value(&name, ty, index);
                // }

                // self.wstate().scope.pop_scope();
            }
            _ => (),
        }
    }

    pub fn evaluate_params(&self, params: &ParamaterList) -> LinkedHashMap<String, Type> {
        let iter = params.items.iter_items().filter_map(|f| {
            if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
                Some((ident.as_str().to_string(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        LinkedHashMap::from_iter(iter)
    }

    pub fn evaluate_struct_members(
        &self,
        members: &EnclosedList<Param>,
    ) -> LinkedHashMap<String, Type> {
        let iter = members.iter_items().filter_map(|f| {
            if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
                Some((ident.as_str().to_string(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        LinkedHashMap::from_iter(iter)
    }

    // fn evaluate_type(&self, ty: &tl_core::ast::Type) -> Type {
    //     match ty {
    //         tl_core::ast::Type::Integer { width, signed, .. } => Type::Integer {
    //             width: *width,
    //             signed: *signed,
    //         },
    //         tl_core::ast::Type::Float { width, .. } => Type::Float { width: *width },
    //         tl_core::ast::Type::Ident(id) => {
    //             if let Some(sym) = { self.rstate().scope.find_symbol(id.as_str()) } {
    //                 return Type::Symbol(sym);
    //             }
    //             // self.add_error(EvaluationError {
    //             //     kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
    //             //     range: id.get_range(),
    //             // });
    //             Type::Empty
    //         }
    //         tl_core::ast::Type::Boolean(_) => Type::Boolean,
    //         tl_core::ast::Type::Ref {
    //             base_type: Some(ty),
    //             ..
    //         } => Type::Ref {
    //             base_type: Box::new(self.evaluate_type(ty)),
    //         },
    //         _ => Type::Empty,
    //     }
    // }

    fn evaluate_type(&self, ty: &tl_core::ast::Type) -> Type {
        match ty {
            tl_core::ast::Type::Integer { width, signed, .. } => Type::Integer {
                width: *width,
                signed: *signed,
            },
            tl_core::ast::Type::Float { width, .. } => Type::Float { width: *width },
            tl_core::ast::Type::Ident(id) => {
                if let Some(sym) = self.rstate().scope.find_symbol(id.as_str()) {
                    return Type::Symbol(sym);
                }
                // self.add_error(EvaluationError {
                //     kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
                //     range: id.get_range(),
                // });
                Type::Empty
            }
            tl_core::ast::Type::Boolean(_) => Type::Boolean,
            tl_core::ast::Type::Ref {
                base_type: Some(ty),
                ..
            } => Type::Ref {
                base_type: Box::new(self.evaluate_type(ty)),
            },
            tl_core::ast::Type::Generic {
                base_type: Some(box tl_core::ast::Type::Ident(tok)),
                list,
            } => {
                let types: Vec<_> = list.iter_items().map(|ty| self.evaluate_type(ty)).collect();

                let Some(symrf) = self.rstate().scope.find_symbol(tok.as_str()) else {
                    // self.add_error(EvaluationError {
                    //     kind: EvaluationErrorKind::SymbolNotFound(tok.as_str().to_string()),
                    //     range: tok.get_range(),
                    // });

                    return Type::Empty
                };

                let csi = {
                    let sym = symrf.borrow();
                    match &sym.value {
                        ScopeValue::StructTemplate {
                            generics,
                            constructions,
                            construction_start_index,
                            ..
                        } => {
                            if !self.verify_generics_match(generics, &types, list.get_range()) {
                                return Type::Empty;
                            }

                            // If we have already constructed this struct with the same type arguments, reuse this construction
                            if let Some(child_construction_name) = constructions.get(&types) {
                                let construction = sym
                                    .children
                                    .get(child_construction_name)
                                    .expect("Compiler Bug!");
                                return Type::Symbol(construction.clone());
                            }

                            *construction_start_index
                        }
                        ScopeValue::IntrinsicStructTemplate {
                            initial_value,
                            generics,
                        } => {
                            if !self.verify_generics_match(generics, &types, list.get_range()) {
                                return Type::Empty;
                            }

                            return Type::Intrinsic(symrf.clone());
                        }
                        _ => {
                            // self.add_error(EvaluationError {
                            //     kind: EvaluationErrorKind::TypeMismatch(
                            //         Type::Empty,
                            //         Type::Empty,
                            //         TypeHint::Struct,
                            //     ),
                            //     range: tok.get_range(),
                            // });
                            return Type::Empty;
                        }
                    }
                };

                let mut sym = symrf.borrow_mut();

                let mut hash = DefaultHasher::new();
                types.hash(&mut hash);
                let hash = hash.finish().to_string();

                let raw_members = {
                    let ScopeValue::StructTemplate { constructions, raw_members, .. } = &mut sym.value else {
                        // self.add_error(EvaluationError {
                        //     kind: EvaluationErrorKind::TypeMismatch(Type::Empty, Type::Empty, TypeHint::Record),
                        //     range: tok.get_range(),
                        // });
                        return Type::Empty
                    };

                    constructions.insert(types.clone(), hash.clone());
                    raw_members.clone()
                };

                // Build generic parameter symbols
                let children: Vec<_> = {
                    sym.children
                        .iter()
                        .take(csi)
                        .zip(types.into_iter())
                        .map(|((k, _), ty)| {
                            Scope::new(
                                symrf.clone(),
                                k.to_string(),
                                ScopeValue::TypeAlias {
                                    ident: k.to_string(),
                                    ty: Box::new(ty),
                                },
                                0,
                            )
                        })
                        .collect()
                };

                let child = sym.insert(
                    symrf.clone(),
                    hash.clone(),
                    ScopeValue::Struct {
                        ident: hash,
                        members: LinkedHashMap::new(),
                    },
                    0,
                );

                // insert generic parameter symbols
                {
                    let mut child = child.borrow_mut();
                    for c in children {
                        child.insert_node(c);
                    }
                }

                {
                    self.wstate().scope.push_scope(child.clone());

                    // We need to regenerate types using generic parameters
                    let emembers = self.evaluate_struct_members(&raw_members);

                    let mut child_sym = child.borrow_mut();
                    let ScopeValue::Struct { members, .. } = &mut child_sym.value else {
                        panic!("Expected struct!")
                    };

                    *members = emembers;

                    self.wstate().scope.pop_scope();
                }

                Type::Symbol(child)
            }
            _ => Type::Empty,
        }
    }

    fn verify_generics_match(
        &self,
        params: &Vec<GenericParameter>,
        args: &Vec<Type>,
        errored_range: Range,
    ) -> bool {
        if args.len() != params.len() {
            // self.add_error(EvaluationError {
            //     kind: EvaluationErrorKind::ArgCountMismatch(args.len() as _, params.len() as _),
            //     range: errored_range,
            // });
            return false;
        }

        // TODO: Verify bindings match

        true
    }

    // fn add_error(&self, error: EvaluationError) {
    //     self.wstate().errors.push(error)
    // }
}
