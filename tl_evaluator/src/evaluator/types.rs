use std::{collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

use linked_hash_map::LinkedHashMap;
use tl_core::{ast::{AstNode, GenericParameter}, token::Range};

use crate::{evaluation_type::{EvaluationType, EvaluationTypeProvider}, evaluation_value::EvaluationValue, error::{EvaluationError, EvaluationErrorKind, TypeHint}, scope::scope::{ScopeValue, Scope}, pass::Pass};

use super::Evaluator;

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>, TP: EvaluationTypeProvider<Type = T>, P: Pass> Evaluator<T, V, TP, P> {
    pub fn evaluate_type(&self, ty: &tl_core::ast::Type) -> T {
        match ty {
            tl_core::ast::Type::Integer { width, signed, .. } => self.type_provider.integer(*width, *signed),
            tl_core::ast::Type::Float { width, .. } => self.type_provider.float(*width),
            tl_core::ast::Type::Ident(id) => {
                if let Some(sym) = self.rstate().scope.find_symbol(id.as_str()) {
                    return self.type_provider.symbol(sym);
                }
                self.add_error(EvaluationError {
                    kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
                    range: id.get_range(),
                });
                self.type_provider.empty()
            }
            tl_core::ast::Type::Boolean(_) => self.type_provider.bool(),
            tl_core::ast::Type::Ref {
                base_type: Some(ty),
                ..
            } => self.type_provider.rf(self.evaluate_type(ty)),
            tl_core::ast::Type::Generic {
                base_type: Some(box tl_core::ast::Type::Ident(tok)),
                list,
            } => {
                let types: Vec<_> = list.iter_items().map(|ty| self.evaluate_type(ty)).collect();

                let Some(symrf) = self.rstate().scope.find_symbol(tok.as_str()) else {
                    self.add_error(EvaluationError {
                        kind: EvaluationErrorKind::SymbolNotFound(tok.as_str().to_string()),
                        range: tok.get_range(),
                    });

                    return self.type_provider.empty()
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
                                return self.type_provider.empty()
                            }

                            // If we have already constructed this struct with the same type arguments, reuse this construction
                            if let Some(child_construction_name) = constructions.get(&types) {
                                let construction = sym
                                    .children
                                    .get(child_construction_name)
                                    .expect("Compiler Bug!");
                                return self.type_provider.symbol(construction.clone());
                            }

                            *construction_start_index
                        }
                        ScopeValue::IntrinsicStructTemplate {
                            // initial_value,
                            generics,
                            ..
                        } => {
                            if !self.verify_generics_match(generics, &types, list.get_range()) {
                                return self.type_provider.empty()
                            }

                            return self.type_provider.intrinsic(symrf.clone());
                        }
                        _ => {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::TypeMismatch(
                                    self.type_provider.empty(),
                                    self.type_provider.empty(),
                                    TypeHint::Struct,
                                ),
                                range: tok.get_range(),
                            });
                            return self.type_provider.empty();
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
                        return self.type_provider.empty()
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

                self.type_provider.symbol(child)
            }
            _ => self.type_provider.empty(),
        }
    }

    pub fn verify_generics_match(
        &self,
        params: &Vec<GenericParameter>,
        args: &Vec<T>,
        errored_range: Range,
    ) -> bool {
        if args.len() != params.len() {
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::ArgCountMismatch(args.len() as _, params.len() as _),
                range: errored_range,
            });
            return false;
        }

        // TODO: Verify bindings match

        true
    }
}