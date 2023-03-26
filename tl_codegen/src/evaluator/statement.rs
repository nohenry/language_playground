use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use linked_hash_map::LinkedHashMap;
use tl_core::ast::{AstNode, Expression, GenericParameter, Statement};
use tl_evaluator::{
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    evaluation_type::{EvaluationType, EvaluationTypeProvider},
    evaluation_value::EvaluationValue,
    pass::{EvaluationPass, MemberPass, TypeFirst},
    scope::scope::{Scope, ScopeValue},
};
use tl_util::{format::TreeDisplay, Rf};

use crate::{
    llvm_type::LlvmType,
    llvm_value::{LlvmValue, LlvmValueKind},
};

use super::LlvmEvaluator;

impl<'a> LlvmEvaluator<'a, EvaluationPass> {
    pub fn evaluate(&self) -> Vec<LlvmValue<'a>> {
        let vals = self
            .module
            .stmts
            .iter()
            .enumerate()
            .map(|(index, stmt)| self.evaluate_statement(stmt, index))
            .collect();

        vals
    }

    pub fn evaluate_statement(&self, statement: &Statement, index: usize) -> LlvmValue<'a> {
        match statement {
            // Non-generic struct definition
            Statement::TypeAlias {
                ident,
                generic: None,
                ty: box tl_core::ast::Type::Struct(members),
                ..
            } => {
                let Some(sym) = ({ self.wstate().scope.find_symbol(ident.as_str()) }) else {
                    return LlvmValue::empty(self.context.as_ref());
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
            // Generic struct definition
            Statement::TypeAlias {
                ident,
                generic: Some(gen),
                ty: box tl_core::ast::Type::Struct(raw_members),
                ..
            } => {
                let Some(sym) = ({ self.wstate().scope.find_symbol(ident.as_str()) }) else {
                    return LlvmValue::empty(self.context.as_ref());
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
                        value.get_type_mut().set_function_return(
                            ty.map(|ty| ty).unwrap_or_else(|| self.context.empty()),
                        );
                    }
                }
            }
            // Variable decleration
            Statement::Decleration {
                ty,
                ident,
                expr: Some(raw_expr),
                ..
            } => {
                let ty = self.evaluate_type(ty);
                let expr = self.evaluate_expression(raw_expr, index);

                if ty.is_symbol() && expr.is_struct_initializer() {
                    let sym = ty.symbol_rf();
                    println!("{}", expr.format());
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

                    let expr = match (&expr.ty, &ty) {
                        (
                            LlvmType::Ref { base_type: lty, .. },
                            LlvmType::Ref { base_type: rty, .. },
                        ) if lty == rty => {
                            expr.clone()
                        }
                        _ => expr
                            .resolve_ref_value(self.context.as_ref())
                            .unwrap_or_else(|| expr),
                    };
                    println!("{}", expr.format());
                    // let expr =
                    let mut expr = expr
                        .try_implicit_cast(&ty, self.context.as_ref())
                        .unwrap_or(expr);

                    if expr.get_type() != &ty {
                        self.add_error(EvaluationError {
                            kind: EvaluationErrorKind::TypeMismatch(
                                expr.into(),
                                ty,
                                TypeHint::Variable,
                            ),
                            range: raw_expr.get_range(),
                        });
                        return LlvmValue::empty(self.context.as_ref());
                    }

                    let mut scope = self.wstate();

                    if let Some(sym) = scope.scope.find_symbol(ident.as_str()) {
                        let sym = sym.borrow();
                        if let ScopeValue::EvaluationValue(value) = &sym.value {
                            if let Some(alloc) = value.inst {
                                self.context.builder.build_store(
                                    alloc,
                                    expr.llvm_basc_value().expect("Not a basic value"),
                                );
                                expr.inst = Some(alloc);
                            }
                        }
                    }

                    scope.scope.insert_value(
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
                    return LlvmValue::tuple(values, self.context.as_ref());
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
                    return LlvmValue::tuple(values, self.context.as_ref());
                }
            }
            _ => (),
        }
        LlvmValue::empty(self.context.as_ref())
    }
}

impl<'a> LlvmEvaluator<'a, TypeFirst> {
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
                                        ty: Box::new(self.context.empty()),
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
                        ty: Box::new(self.context.empty()),
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
                    ScopeValue::EvaluationValue(LlvmValue::empty(self.context.as_ref())),
                    index,
                );

                let eparameters = self.evaluate_params(parameters);
                let ereturn = return_type
                    .as_ref()
                    .map(|ty| self.evaluate_type(ty))
                    .unwrap_or(self.context.empty());

                // self.type_provider.inform_function_decleration();
                let function = LlvmValue::gen_function(
                    ident.as_str(),
                    Statement::clone(body),
                    eparameters,
                    ereturn,
                    sym,
                    self.context.as_ref(),
                );

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(function.0),
                    index,
                );
            }
            Statement::Decleration { ident, .. } => {}
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

impl<'a> LlvmEvaluator<'a, MemberPass> {
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
                                        ty: Box::new(self.context.empty()),
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
                                ScopeValue::EvaluationValue(LlvmValue::default_for(&ty)),
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
            Statement::Decleration {
                ty,
                ident,
                ..
                // eq,
                // expr,
            } => {
                let ty = self.evaluate_type(ty);
                let ty = ty.llvm_basic_type().unwrap();

                let alloc = self.context.builder.build_alloca(ty, ident.as_str());
                self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(LlvmValue {
                        kind: LlvmValueKind::Empty,
                        ty: self.context.empty(),
                        inst: Some(alloc),
                    }),
                    index,
                );
            }
            _ => (),
        }
    }

    pub fn evaluate_member_impl_stmt(
        &self,
        member_ty: &Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
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
                    ScopeValue::EvaluationValue(LlvmValue::empty(self.context.as_ref())),
                    index,
                );

                let mut eparameters = self.evaluate_params(parameters);
                eparameters.insert(
                    "self".to_string(),
                    self.context.rf(self.context.symbol(member_ty.clone())),
                );

                let ereturn = return_type
                    .as_ref()
                    .map(|ty| self.evaluate_type(ty))
                    .unwrap_or(self.context.empty());

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(LlvmValue::function(
                        ident.as_str(),
                        Statement::clone(body),
                        eparameters,
                        ereturn,
                        sym.clone(),
                        self.context.as_ref(),
                    )),
                    index,
                );
            }
            _ => (),
        }
    }
}
