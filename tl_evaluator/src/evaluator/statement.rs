use std::{collections::HashMap, fmt::Display};

use linked_hash_map::LinkedHashMap;
use tl_core::ast::{AstNode, Expression, GenericParameter, Statement};
use tl_util::{format::TreeDisplay, Rf};

use crate::{
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    evaluation_type::EvaluationType,
    evaluation_value::EvaluationValue,
    pass::{EvaluationPass, MemberPass, TypeFirst},
    scope::scope::{Scope, ScopeValue},
};

use super::Evaluator;

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T> + Display>
    Evaluator<T, V, EvaluationPass>
{
    pub fn evaluate(&self) -> Vec<V> {
        let vals = self
            .module
            .stmts
            .iter()
            .enumerate()
            .map(|(index, stmt)| self.evaluate_statement(stmt, index))
            .collect();

        vals
    }

    pub fn evaluate_statement(&self, statement: &Statement, index: usize) -> V {
        match statement {
            Statement::TypeAlias {
                ident,
                generic: None,
                ty: box tl_core::ast::Type::Struct(members),
                ..
            } => {
                let Some(sym) = ({ self.wstate().scope.find_symbol(ident.as_str()) }) else {
                    return V::empty();
                };
                self.wstate().scope.push_scope(sym.clone());

                let members = self.evaluate_struct_members(members);
                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::Struct {
                        members,
                        ident: ident.as_str().to_string(),
                    },
                    index,
                );

                self.wstate().scope.pop_scope();
            }
            Statement::TypeAlias {
                ident,
                generic: Some(gen),
                ty: box tl_core::ast::Type::Struct(raw_members),
                ..
            } => {
                let Some(sym) = ({ self.wstate().scope.find_symbol(ident.as_str()) }) else {
                    return V::empty();
                };
                self.wstate().scope.push_scope(sym.clone());

                let members = self.evaluate_struct_members(raw_members);
                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::StructTemplate {
                        raw_members: raw_members.clone(),
                        members,
                        ident: ident.as_str().to_string(),
                        generics: gen.iter_items().cloned().collect(),
                        constructions: HashMap::new(),
                        construction_start_index: gen.items.len(),
                    },
                    index,
                );

                self.wstate().scope.pop_scope();
            }
            Statement::Function {
                ident,
                parameters,
                return_type,
                body: Some(_),
                ..
            } => {
                let params = self.evaluate_params(parameters);
                let ty = return_type.as_ref().map(|ty| self.evaluate_type(ty));

                let sym = self.wstate().scope.find_symbol(ident.as_str()).unwrap();

                let mut mut_sym = sym.borrow_mut();
                if let ScopeValue::EvaluationValue(value) = &mut mut_sym.value {
                    if value.is_function() {
                        value
                            .get_type_mut()
                            .set_function_parameters(params.into_iter());
                        value
                            .get_type_mut()
                            .set_function_return(ty.map(|ty| ty).unwrap_or_else(|| T::empty()));
                    }
                }
            }
            Statement::Decleration {
                ty,
                ident,
                expr: Some(raw_expr),
                ..
            } => {
                let ty = self.evaluate_type(ty);
                let expr = self.evaluate_expression(raw_expr, index);

                if ty.is_symbol() {
                    let sym = ty.symbol_rf();
                    let args = expr.get_struct_members();

                    if let ScopeValue::Struct { members, .. } = &sym.borrow().value {
                        let (value, args) = self.evaluate_struct_init(
                            &sym,
                            members,
                            &LinkedHashMap::from_iter(args),
                            |i| {
                                if let Expression::Record(r) = raw_expr {
                                    if let Some(item) = r.iter_items().nth(i) {
                                        return Some(item.expr.get_range());
                                    }
                                }
                                None
                            },
                            raw_expr.get_range(),
                        );

                        self.wstate().scope.update_value(
                            ident.as_str(),
                            ScopeValue::EvaluationValue(value),
                            index,
                        );

                        if let Some(sym) = self.wstate().scope.find_symbol(ident.as_str()) {
                            let mut scope = sym.borrow_mut();
                            scope.children = args
                                .into_iter()
                                .map(|arg| {
                                    (
                                        arg.0.clone(),
                                        Rf::new(Scope::new(
                                            sym.clone(),
                                            arg.0.clone(),
                                            ScopeValue::EvaluationValue(arg.1.clone()),
                                            index,
                                        )),
                                    )
                                })
                                .collect();
                        }
                    }
                } else {
                    let expr = expr.resolve_ref_value().unwrap_or_else(|| expr);
                    let expr = expr.try_implicit_cast(&ty).unwrap_or(expr);

                    if expr.get_type() != &ty {
                        self.add_error(EvaluationError {
                            kind: EvaluationErrorKind::TypeMismatch(
                                expr.into(),
                                ty,
                                TypeHint::Variable,
                            ),
                            range: raw_expr.get_range(),
                        });
                        return V::empty();
                    }
                    self.wstate().scope.insert_value(
                        ident.as_str(),
                        ScopeValue::EvaluationValue(expr),
                        index,
                    );
                }
            }
            Statement::Expression(expr) => return self.evaluate_expression(expr, index),
            Statement::List(list) => {
                if list.num_children() == 1 {
                    let item = list
                        .iter_items()
                        .next()
                        .expect("Value should have been present. This is probably a rustc bug");
                    return self.evaluate_statement(item, 0);
                } else {
                    let values: Vec<_> = list
                        .iter_items()
                        .enumerate()
                        .map(|(index, stmt)| self.evaluate_statement(stmt, index))
                        .collect();
                    return V::tuple(values);
                }
            }
            Statement::Block(list) => {
                if list.num_children() == 1 {
                    let item = list
                        .iter_items()
                        .next()
                        .expect("Value should have been present. This is probably a rustc bug");
                    return self.evaluate_statement(item, 0);
                } else {
                    let values: Vec<_> = list
                        .iter_items()
                        .enumerate()
                        .map(|(index, stmt)| self.evaluate_statement(stmt, index))
                        .collect();
                    return V::tuple(values);
                }
            }
            _ => (),
        }
        V::empty()
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T> + Display>
    Evaluator<T, V, TypeFirst>
{
    pub fn evaluate(&self) {
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index)
        }
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
                                        ty: Box::new(T::empty()),
                                    },
                                    index,
                                );
                            }
                            _ => todo!(),
                        }
                    }

                    self.wstate().scope.pop_scope();
                }
            }
            Statement::TypeAlias {
                ident,
                generic: None,
                ..
            } => {
                self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::TypeAlias {
                        ident: ident.as_str().to_string(),
                        ty: Box::new(T::empty()),
                    },
                    index,
                );
            }

            Statement::Function {
                ident,
                parameters,
                return_type,
                body: Some(body),
                ..
            } => {
                let sym = self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(V::empty()),
                    index,
                );

                let eparameters = self.evaluate_params(parameters);
                let ereturn = return_type
                    .as_ref()
                    .map(|ty| self.evaluate_type(ty))
                    .unwrap_or(T::empty());

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(V::function(
                        Statement::clone(body),
                        eparameters,
                        ereturn,
                        sym,
                    )),
                    index,
                );
            }
            Statement::Decleration { ident, .. } => {
                self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(V::empty()),
                    index,
                );
            }
            Statement::UseStatement { args, .. } => {
                let path = args
                    .iter_items()
                    .map(|sym| sym.as_str().to_string())
                    .collect();
                self.wstate().scope.add_use(path)
            }
            _ => (),
        }
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T> + Display>
    Evaluator<T, V, MemberPass>
{
    pub fn evaluate(&self) {
        for (index, stmt) in self.module.stmts.iter().enumerate() {
            self.evaluate_statement(stmt, index)
        }
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
            Statement::TypeAlias {
                ident,
                generic: None,
                ty,
                ..
            } => {
                let expr_ty = self.evaluate_type(ty);
                if let Some(sym) = self.wstate().scope.find_symbol(ident.as_str()) {
                    let mut sym = sym.borrow_mut();
                    if let ScopeValue::TypeAlias { ty, .. } = &mut sym.value {
                        *ty.as_mut() = expr_ty
                    }
                }
            }
            Statement::Impl {
                generics,
                ty: Some(ty),
                body: Some(body),
                ..
            } => {
                let ty = if let Some(generics) = generics {
                    let tmp_scope =
                        self.wstate()
                            .scope
                            .insert_value("tmp_generics", ScopeValue::Root, index);

                    self.wstate().scope.push_scope(tmp_scope.clone());

                    for param in generics.iter_items() {
                        match param {
                            GenericParameter::Unbounded(b) => {
                                self.wstate().scope.insert_value(
                                    b.as_str(),
                                    ScopeValue::TypeAlias {
                                        ident: b.as_str().to_string(),
                                        ty: Box::new(T::empty()),
                                    },
                                    index,
                                );
                            }
                            _ => todo!(),
                        }
                    }

                    let ty = self.evaluate_type(ty);
                    // println!("Type: {}", ty.format());

                    self.wstate().scope.pop_scope();

                    self.wstate().scope.remove_value("tmp_generics");

                    ty
                } else {
                    self.evaluate_type(ty)
                };

                if ty.is_symbol() {
                    let sym = ty.symbol_rf();
                    self.wstate().scope.push_scope(sym.clone());

                    for (i, stmt) in body.iter_items().enumerate() {
                        self.evaluate_member_impl_stmt(&sym, stmt, i);
                    }

                    self.wstate().scope.pop_scope();
                }
            }
            Statement::Function {
                ident,
                parameters,
                return_type,
                body: Some(body),
                ..
            } => {
                // return;
                let Some(rf) = self.rstate().scope.find_symbol(ident.as_str()) else {
                        return;
                    };
                let (pvals, _) = {
                    let ScopeValue::EvaluationValue(value) = &rf.borrow().value else {
                        return;
                    };

                    if !value.is_function() {
                        return;
                    }

                    let parameters = value.get_type().function_parameters_rf();
                    let pvals: Vec<_> = parameters
                        .map(|(name, ty)| {
                            (
                                name.clone(),
                                ScopeValue::EvaluationValue(V::default_for(&ty)),
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
        }
    }

    pub fn evaluate_member_impl_stmt(
        &self,
        member_ty: &Rf<Scope<T, V>>,
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
                    ScopeValue::EvaluationValue(V::empty()),
                    index,
                );

                let mut eparameters = self.evaluate_params(parameters);
                eparameters.insert("self".to_string(), T::rf(T::symbol(member_ty.clone())));

                let ereturn = return_type
                    .as_ref()
                    .map(|ty| self.evaluate_type(ty))
                    .unwrap_or(T::empty());

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(V::function(
                        Statement::clone(body),
                        eparameters,
                        ereturn,
                        sym.clone(),
                    )),
                    index,
                );
            }
            _ => (),
        }
    }
}
