use std::collections::HashMap;

use inkwell::{
    types::BasicTypeEnum,
    values::{AnyValue, AnyValueEnum, BasicValue},
};
use linked_hash_map::LinkedHashMap;
use tl_core::ast::{AstNode, Expression, GenericParameter, Statement, Type};
use tl_evaluator::{
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    evaluation_type::{EvaluationType, EvaluationTypeProvider},
    evaluation_value::EvaluationValue,
    pass::{EvaluationPass, MemberPass, TypeFirst},
    scope::scope::{Scope, ScopeValue},
};
use tl_util::{
    format::{Config, TreeDisplay},
    Rf,
};

use crate::{
    llvm_type::LlvmType,
    llvm_value::{LlvmValue, LlvmValueKind},
    LlvmContextState,
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
                body: Some(body),
                ..
            } => {
                let params = self.evaluate_params(parameters);
                let ty = self.evaluate_type(&return_type);

                // Cloning references for later
                let sym = self.wstate().scope.find_symbol(ident.as_str()).unwrap();
                let new_sym = sym.clone();
                let new_state_sym = sym.clone();

                let (ret_ty, func, llvm_params) = {
                    let sym = sym.borrow();

                    let ScopeValue::EvaluationValue(value) = &sym.value else {
                        return LlvmValue::empty(self.context.as_ref());
                    };

                    let llvm_params: Vec<_> = sym
                        .children
                        .iter()
                        .filter_map(|(_, c)| {
                            if let ScopeValue::EvaluationValue(ref pval) = c.borrow().value {
                                Some(pval.llvm_value.into_pointer_value())
                            } else {
                                None
                            }
                        })
                        .collect();

                    (
                        value.ty.function_return().clone(),
                        value.llvm_value.into_function_value(),
                        llvm_params,
                    )
                };

                // Make function's scope accessible when evluating body
                self.wstate().scope.push_scope(new_sym);

                let current_block = func
                    .get_first_basic_block()
                    .expect("Unable to get function's first basic block! (compiler bug)");
                self.context.builder.position_at_end(current_block);

                let return_block = Some(self.context.context.append_basic_block(func, "ret_block"));

                // If return is non void, create a variable to store return value
                let return_storage = match ret_ty {
                    LlvmType::Empty(_) => None,
                    _ => Some(
                        self.context
                            .builder
                            .build_alloca(ret_ty.llvm_basic_type().unwrap(), "_ret_val"),
                    ),
                };

                for (param, value) in llvm_params.iter().zip(func.get_param_iter()) {
                    self.context.builder.build_store(*param, value);
                }

                // Build the new context for the function
                let new_state = LlvmContextState {
                    current_block,
                    current_function: new_state_sym,
                    return_storage,
                    return_dirtied: false,
                    return_block,
                };
                let state = self.context.wstate().replace(new_state);

                let return_value = self.evaluate_statement(body, 0);

                match (return_value, ret_ty) {
                    // Case for void or, no return type
                    (_, LlvmType::Empty(_)) => {
                        let state = self.context.rstate();
                        if state.return_dirtied {
                            // If return statements are used, jump to return block and return

                            self.context
                                .builder
                                .build_unconditional_branch(state.return_block.unwrap());
                            self.context
                                .builder
                                .position_at_end(state.return_block.unwrap());
                            self.context.builder.build_return(None);
                        } else {
                            // If no return statements are used, remove the return block and return

                            state
                                .return_block
                                .unwrap()
                                .remove_from_function()
                                .expect("Unable to remove return block from function");
                            self.context.builder.build_return(None);
                        }
                    }
                    // Case for return is last statement in body
                    (_, return_type) if body.get_last().is_return() => {
                        let state = self.context.rstate();

                        self.context
                            .builder
                            .position_at_end(state.return_block.unwrap());

                        self.context
                            .builder
                            .build_return(Some(&state.return_storage.unwrap()));
                    }
                    // Case for return type
                    (value, return_type) => {
                        let value = value
                            .resolve_ref_value(self.context.as_ref())
                            .unwrap_or_else(|| value);
                        let value = value
                            .try_implicit_cast(&return_type, self.context.as_ref())
                            .unwrap_or_else(|| value);

                        if value.get_type() == &return_type {
                            let state = self.context.rstate();

                            if state.return_dirtied {
                                // If return statements are used, jump to return block and return the return storage

                                self.context
                                    .builder
                                    .build_unconditional_branch(state.return_block.unwrap());

                                self.context
                                    .builder
                                    .position_at_end(state.return_block.unwrap());

                                self.context
                                    .builder
                                    .build_return(Some(&state.return_storage.unwrap()));
                            } else {
                                // If no return statements are used, remove the return block, remove the return storage, and try to return the statements value

                                state
                                    .return_block
                                    .unwrap()
                                    .remove_from_function()
                                    .expect("Unable to remove return block from function");

                                state
                                    .return_storage
                                    .unwrap()
                                    .as_instruction()
                                    .expect("Unable to get return storage as instruction")
                                    .remove_from_basic_block();

                                self.context
                                    .builder
                                    .position_at_end(state.return_block.unwrap());

                                let val = value.llvm_basc_value().unwrap();
                                self.context.builder.build_return(Some(&val));
                            }
                        } else {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::TypeMismatch(
                                    value.into(),
                                    return_type,
                                    TypeHint::ReturnParameter,
                                ),
                                range: body.get_last().get_range(),
                            });
                        }
                    }
                }

                self.context.builder.position_at_end(state.current_block);
                let _ = self.context.wstate().replace(state);
                self.wstate().scope.pop_scope();
            }
            Statement::VariableDeclaration {
                ty: Type::Let(_),
                ident,

                default: Some((_, raw_expr)),
            } => {
                let mut expr = self.evaluate_expression(raw_expr, index);

                let mut scope = self.wstate();

                if let Some(sym) = scope.scope.find_symbol(ident.as_str()) {
                    let mut sym = sym.borrow_mut();

                    if let ScopeValue::EvaluationValue(value) = &mut sym.value {
                        let current_block = self
                            .context
                            .builder
                            .get_insert_block()
                            .expect("Should have block");

                        let instruction = value
                            .llvm_value
                            .into_pointer_value()
                            .as_instruction()
                            .unwrap();
                        self.context.builder.position_before(&instruction);

                        let reduced_type = expr.ty.reduce(self.context.as_ref());

                        let alloc = self.context.builder.build_alloca(
                            reduced_type
                                .llvm_basic_type()
                                .expect("Couldn't get llvm type"),
                            ident.as_str(),
                        );
                        instruction.remove_from_basic_block();
                        self.context.builder.position_at_end(current_block);

                        value.llvm_value = alloc.into();
                        value.ty = reduced_type;

                        self.context.builder.build_store(
                            value.llvm_value.into_pointer_value(),
                            expr.llvm_basc_value().expect("Not a basic value"),
                        );

                        expr.llvm_value = value.llvm_value;
                        expr.ty = value.ty.clone();
                    } else {
                        panic!("Should be EvaluationValue")
                    }
                } else {
                    panic!("Should have been evaluated in previous pass")
                }

                scope
                    .scope
                    .insert_value(ident.as_str(), ScopeValue::EvaluationValue(expr), index);
            }
            // Variable declaration
            Statement::VariableDeclaration {
                ty,
                ident,
                default: Some((_, raw_expr)),
            } => {
                let ty = self.evaluate_type(ty);
                let expr = self.evaluate_expression(raw_expr, index);

                if ty.is_symbol() && expr.is_struct_initializer() {
                    // Initialize class

                    let sym = ty.symbol_rf();

                    let args = expr.get_struct_members();

                    if let ScopeValue::Struct { members, .. } = &sym.borrow().value {
                        let (value, args) = self.evaluate_struct_init(
                            &sym,
                            members,
                            &LinkedHashMap::from_iter(args),
                            |i| {
                                // if let Expression::ClassInitializer(r) = raw_expr.as_ref() {
                                //     if let Some(item) = r.iter_items().nth(i) {
                                //         return Some(item.expr.get_range());
                                //     }
                                // }
                                todo!();
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
                        ) if lty == rty => expr.clone(),
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
                            self.context.builder.build_store(
                                value.llvm_value.into_pointer_value(),
                                expr.llvm_basc_value().expect("Not a basic value"),
                            );
                            expr.llvm_value = value.llvm_value;
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
                if list.num_children(&Config::default()) == 1 {
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
            Statement::Return { expr, .. } => {
                let mut state = self.context.wstate();
                state.return_dirtied = true;
                let func = state.current_function.borrow();

                if let ScopeValue::EvaluationValue(LlvmValue {
                    ty:
                        LlvmType::Function {
                            parameters,
                            return_type,
                            llvm_type,
                        },
                    ..
                }) = &func.value
                {
                    match (return_type.as_ref(), expr) {
                        // In the case of only return (no expression)
                        (LlvmType::Empty(_), None) => {
                            // Only jump to return branch
                            if let Some(return_block) = state.return_block {
                                self.context
                                    .builder
                                    .build_unconditional_branch(return_block);
                            }
                        }
                        // In the case of return with an expression
                        (ty, Some(expr)) => {
                            // Evaluate and cast expression;
                            let expr = self.evaluate_expression(expr, index);
                            let expr = expr
                                .resolve_ref_value(self.context.as_ref())
                                .unwrap_or_else(|| expr);
                            let expr = expr
                                .try_implicit_cast(ty, self.context.as_ref())
                                .unwrap_or_else(|| expr);

                            // store the expression in the return storage
                            if let Some(return_storage) = state.return_storage {
                                self.context.builder.build_store(
                                    return_storage,
                                    expr.llvm_basc_value()
                                        .expect("Unable to get LlvmValue as BasicValue"),
                                );
                            }

                            // finally jump to return block
                            if let Some(return_block) = state.return_block {
                                self.context
                                    .builder
                                    .build_unconditional_branch(return_block);
                            }
                        }
                        _ => todo!("Throw error"),
                    }
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
            // Struct declaration
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
                // Insert empty function stub. the function needs to reference itself later
                let sym = self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(LlvmValue::empty(self.context.as_ref())),
                    index,
                );
            }
            Statement::VariableDeclaration { ident, .. } => {}
            Statement::ImportStatement { args, .. } => {
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
            // Struct declaration
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
                let Some(rf) = self.rstate().scope.find_symbol(ident.as_str()) else {
                    return;
                };

                let eparameters = self.evaluate_params(parameters);
                let ereturn = self.evaluate_type(return_type);

                let function = LlvmValue::gen_function(
                    Statement::clone(body),
                    eparameters,
                    ereturn,
                    rf.clone(),
                    self.context.as_ref(),
                );

                let llvm_func = function.0.llvm_value.into_function_value();
                let function_params = function.0.ty.function_parameters_rf();

                // let (pvals, ret_ty, func) = {
                //     let ScopeValue::EvaluationValue(value) = &rf.borrow().value else {
                //         return;
                //     };

                //     match &value.ty {
                //         LlvmType::Function { .. } => (),
                //         _ => return
                //     }

                //     let parameters = value.get_type().function_parameters_rf();
                //     let pvals: Vec<_> = parameters
                //         .map(|(name, ty)| {
                //             (
                //                 name.clone(),
                //                 ScopeValue::EvaluationValue(LlvmValue::default_for(&ty, self.context.as_ref())),
                //             )
                //         })
                //         .collect();
                //     let return_type = value.get_type().function_return();

                //     (pvals, return_type.clone(), value.llvm_value.into_function_value())
                // };

                // Build the new context for the function
                let current_block = llvm_func
                    .get_first_basic_block()
                    .expect("Unable to get function's first basic block! (compiler bug)");
                let new_state = LlvmContextState {
                    current_block,
                    current_function: rf.clone(),
                    return_storage: None,
                    return_dirtied: false,
                    return_block: None,
                };

                // Push the function scope so we have access to locals when evaluating
                self.wstate().scope.push_scope(rf);

                self.context.builder.position_at_end(current_block);

                for (llvm_value, (name, ty)) in llvm_func.get_param_iter().zip(function_params) {
                    let alloc = self.context.builder.build_alloca(
                        ty.llvm_basic_type()
                            .expect("Unable to convert to basic type"),
                        "_param",
                    );

                    self.wstate().scope.insert_value(
                        name,
                        ScopeValue::EvaluationValue(LlvmValue {
                            kind: LlvmValueKind::Empty,
                            ty: ty.clone(),
                            llvm_value: alloc.into(),
                        }),
                        index,
                    );
                }

                let state = self.context.wstate().replace(new_state);

                self.evaluate_statement(body, 0);

                self.context.builder.position_at_end(state.current_block);
                let _ = self.context.wstate().replace(state);

                self.wstate().scope.pop_scope();

                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(function.0),
                    index,
                );
            }
            Statement::VariableDeclaration {
                ty: Type::Let(_),
                ident,
                ..
            } => {
                let alloc = self
                    .context
                    .builder
                    .build_alloca(self.context.context.i8_type(), ident.as_str());

                self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(LlvmValue {
                        kind: LlvmValueKind::Empty,
                        ty: self.context.empty(),
                        llvm_value: alloc.into(),
                    }),
                    index,
                );
            }
            Statement::VariableDeclaration { ty, ident, .. } => {
                let llvm_ty = self.evaluate_type(ty);
                let ty = llvm_ty.llvm_basic_type().unwrap();

                let alloc = self.context.builder.build_alloca(ty, ident.as_str());
                self.wstate().scope.insert_value(
                    ident.as_str(),
                    ScopeValue::EvaluationValue(LlvmValue {
                        kind: LlvmValueKind::Empty,
                        ty: llvm_ty,
                        llvm_value: alloc.into(),
                    }),
                    index,
                );
            }
            Statement::Expression(Expression::Block(list)) => {
                if list.num_children(&Config::default()) == 0 {
                    // return LlvmValue::empty(self.context.as_ref());
                } else if list.num_children(&Config::default()) == 1 {
                    let item = list
                        .iter_items()
                        .next()
                        .expect("Value should have been present. This is probably a rustc bug");
                    let _value = self.evaluate_statement(item, 0);
                } else {
                    let _values: Vec<_> = list
                        .iter_items()
                        .enumerate()
                        .map(|(index, stmt)| self.evaluate_statement(stmt, index))
                        .collect();
                    // return values.into_iter().rev().next().unwrap();
                }
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

                let ereturn = self.evaluate_type(return_type);

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
