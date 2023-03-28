use std::fmt::Display;

use inkwell::intrinsics::Intrinsic;
use linked_hash_map::LinkedHashMap;
use tl_core::{
    ast::{AstNode, Expression, ParsedTemplate, ParsedTemplateString},
    token::{Operator, Range, SpannedToken, Token},
};

use tl_evaluator::{
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    evaluation_type::{EvaluationType, EvaluationTypeProvider},
    evaluation_value::EvaluationValue,
    pass::EvaluationPass,
    scope::scope::ScopeValue,
};
use tl_util::format::TreeDisplay;

use crate::{
    context,
    llvm_type::LlvmType,
    llvm_value::{LlvmValue, LlvmValueKind},
};

use super::LlvmEvaluator;

impl<'a> LlvmEvaluator<'a, EvaluationPass> {
    pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> LlvmValue<'a> {
        match expression {
            Expression::Integer(val, _, _) => LlvmValue::cinteger(*val, self.context.as_ref()),
            Expression::Float(val, _, _) => LlvmValue::cfloat(*val, self.context.as_ref()),
            Expression::Boolean(b, _) => LlvmValue::bool(*b, self.context.as_ref()),
            Expression::String(ParsedTemplateString(vs), _) => {
                let str = vs
                    .iter()
                    .map(|f| match f {
                        ParsedTemplate::String(s) => s.as_str().to_string(),
                        ParsedTemplate::Template(t, _, _) => {
                            let expr = self.evaluate_expression(t, index);

                            if let Some(data) = expr.resolve_ref(self.context.as_ref()) {
                                if let ScopeValue::EvaluationValue(cv) = &data.borrow().value {
                                    return format!("{}", cv);
                                }
                            }

                            format!("{expr}")
                        }
                    })
                    .intersperse("".to_string())
                    .collect::<String>();
                LlvmValue::string(str, self.context.as_ref())
            }
            Expression::Ident(tok @ SpannedToken(_, Token::Ident(id))) => {
                let sym = self.rstate().scope.find_symbol(id);
                if let Some(sym) = sym {
                    let symv = sym.borrow();
                    match &symv.value {
                        ScopeValue::EvaluationValue(cv) => {
                            let mut rf = LlvmValue::sym_reference(
                                &sym,
                                cv.get_type().clone(),
                                self.context.as_ref(),
                            );
                            rf
                        }
                        ScopeValue::Struct { .. } => LlvmValue::sym_reference(
                            &sym,
                            self.context.empty(),
                            self.context.as_ref(),
                        ),
                        _ => LlvmValue::empty(self.context.as_ref()),
                    }
                } else {
                    self.add_error(EvaluationError {
                        kind: EvaluationErrorKind::SymbolNotFound(id.to_string()),
                        range: tok.get_range(),
                    });
                    LlvmValue::empty(self.context.as_ref())
                }
            }
            Expression::BinaryExpression {
                left: Some(left),
                right: Some(right),
                op_token: Some(SpannedToken(_, Token::Operator(o))),
            } => self.evaluate_binary_expression(left, o, right, index),
            Expression::Record(r) => {
                let hmp = LinkedHashMap::from_iter(
                    r.iter_items()
                        .map(|f| (f.name().clone(), self.evaluate_expression(&f.expr, index))),
                );

                LlvmValue::create_struct_initializer(hmp, self.context.as_ref())
            }
            Expression::FunctionCall {
                expr,
                args: raw_args,
            } => {
                let expr = self.evaluate_expression(expr, index);
                let args = self.evaluate_args(raw_args, index);

                let expr = {
                    let expr = expr.resolve_ref(self.context.as_ref()).unwrap();
                    let expr = expr.borrow();
                    let expr = if let ScopeValue::EvaluationValue(cv) = &expr.value {
                        cv.clone()
                    } else {
                        return LlvmValue::empty(self.context.as_ref());
                    };
                    expr
                };

                match (&expr.ty, &expr.kind) {
                    // Normal function
                    (
                        LlvmType::Function {
                            parameters,
                            return_type,
                            llvm_type,
                        },
                        LlvmValueKind::Function {
                            rf,
                            body,
                            entry_block,
                        },
                    ) => {
                        self.wstate().scope.push_scope(rf.clone());

                        let has_args: Option<Vec<_>> = args
                            .into_iter()
                            .zip(parameters.iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg
                                    .try_implicit_cast(&ty, self.context.as_ref())
                                    .unwrap_or(arg);

                                // let arg = if let Some(value) = arg.resolve_ref_value() {
                                //     LlvmValue {
                                //         ty: value.ty,
                                //         kind: arg.kind,
                                //     }
                                // } else {
                                //     arg
                                // };

                                if arg.get_type() != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.into(),
                                            ty.clone(),
                                            TypeHint::Parameter,
                                        ),
                                        range: raw_args
                                            .items
                                            .iter_items()
                                            .nth(i)
                                            .unwrap()
                                            .get_range(),
                                    });
                                    return None;
                                }
                                self.wstate().scope.insert_value(
                                    &name,
                                    ScopeValue::EvaluationValue(arg),
                                    index,
                                );

                                Some(())
                            })
                            .collect();

                        if has_args.is_none() {
                            return LlvmValue::empty(self.context.as_ref());
                        }

                        let return_value = self.evaluate_statement(&body, index);

                        self.wstate().scope.pop_scope();

                        // TODO: verify types here as well
                        match return_value.kind {
                            LlvmValueKind::Tuple(vals) => vals
                                .into_iter()
                                .last()
                                .unwrap_or_else(|| LlvmValue::empty(self.context.as_ref())),
                            _ => return_value,
                        }
                    }

                    // Native function
                    (
                        LlvmType::Function {
                            parameters,
                            return_type,
                            llvm_type,
                        },
                        LlvmValueKind::NativeFunction { rf, callback },
                    ) => {
                        // let callback = expr.native_function_callback();

                        let arglen = args.len();
                        let plen = parameters.len();

                        let has_args: Option<LinkedHashMap<_, _>> = args
                            .into_iter()
                            .zip(parameters.iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg
                                    .try_implicit_cast(&ty, self.context.as_ref())
                                    .unwrap_or(arg);

                                if arg.get_type() != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.into(),
                                            ty.clone(),
                                            TypeHint::Parameter,
                                        ),
                                        range: raw_args
                                            .items
                                            .iter_items()
                                            .nth(i)
                                            .unwrap()
                                            .get_range(),
                                    });
                                    return None;
                                }

                                Some((name.clone(), arg))
                            })
                            .collect();

                        if has_args.is_none() {
                            return LlvmValue::empty(self.context.as_ref());
                        } else if arglen != plen {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::ArgCountMismatch(arglen as _, plen as _),
                                range: raw_args.get_range(),
                            });
                            return LlvmValue::empty(self.context.as_ref());
                        }

                        let return_val = callback(has_args.as_ref().unwrap());

                        // LlvmValue::record_instance(rf.clone());

                        return_val
                    }
                    _ => LlvmValue::empty(self.context.as_ref()),
                }
            }
            _ => LlvmValue::empty(self.context.as_ref()),
        }
    }

    pub fn evaluate_binary_expression(
        &self,
        raw_left: &Expression,
        op: &Operator,
        raw_right: &Expression,
        index: usize,
    ) -> LlvmValue<'a> {
        match (op, raw_left) {
            (Operator::Equals, Expression::Ident(name)) => {
                let right = self.evaluate_expression(raw_right, index);
                let right = right.resolve_ref_value(self.context.as_ref()).unwrap();

                let Some(sym) = self.wstate().scope.find_symbol(name.as_str()) else {
                    self.add_error(EvaluationError { kind: EvaluationErrorKind::SymbolNotFound(name.as_str().to_string()), range: name.get_range() }

                    );
                    return LlvmValue::empty(self.context.as_ref())
                };

                let mut value = sym.borrow_mut();

                let ScopeValue::EvaluationValue(cv)  = &mut value.value else {
                    // TODO: throw error
                    return LlvmValue::empty(self.context.as_ref())
                };

                let right = right
                    .try_implicit_cast(cv.get_type(), self.context.as_ref())
                    .unwrap_or_else(|| right);
                *cv = right.clone();

                return right;
            }
            (
                Operator::Equals,
                Expression::BinaryExpression {
                    op_token: Some(SpannedToken(_, Token::Operator(Operator::Dot))),
                    left: Some(dleft),
                    right: Some(dright),
                },
            ) => {
                let right = self.evaluate_expression(raw_right, index);
                let right = right.resolve_ref_value(self.context.as_ref()).unwrap();
                // let right =
                let scope = &mut self.wstate().scope;
                let updated_value = scope.follow_member_access_mut(dleft, dright, |cv| {
                    *cv = right
                        .try_implicit_cast(cv.get_type(), self.context.as_ref())
                        .unwrap_or_else(|| right.clone());
                });
                if !updated_value {
                    return LlvmValue::empty(self.context.as_ref());
                };
                return right;
            }
            (Operator::Dot, _) => {
                let left = self.evaluate_expression(raw_left, index);

                match (raw_right, left.get_type().is_ref()) {
                    (Expression::Ident(member), true) => {
                        let child = left.resolve_ref(self.context.as_ref()).unwrap();
                        let child_scope = child.borrow();

                        let rtype = if let Some(val) = child_scope.children.get(member.as_str()) {
                            let value = val.borrow();

                            let ScopeValue::EvaluationValue(cv) = &value.value else {
                                return LlvmValue::empty(self.context.as_ref())
                            };

                            cv.get_type().clone()
                        } else {
                            return LlvmValue::empty(self.context.as_ref());
                        };

                        return LlvmValue::reference(
                            left,
                            member.as_str().to_string(),
                            rtype,
                            self.context.as_ref(),
                        );
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        let left = self.evaluate_expression(raw_left, index);
        let right = self.evaluate_expression(raw_right, index);

        println!("{}", left.format());
        let left = left
            .resolve_ref_value(self.context.as_ref())
            .unwrap_or(left);
        let right = right
            .resolve_ref_value(self.context.as_ref())
            .unwrap_or(right);

        let (left, right) = match (left, right) {
            (
                left @ LlvmValue {
                    ty: LlvmType::Integer { .. },
                    ..
                },
                right @ LlvmValue {
                    ty: LlvmType::CoercibleInteger(_),
                    ..
                },
            ) => {
                let right = right
                    .try_implicit_cast(&left.ty, self.context.as_ref())
                    .unwrap_or_else(|| right);
                (left, right)
            }
            (
                left @ LlvmValue {
                    ty: LlvmType::CoercibleInteger(_),
                    ..
                },
                right @ LlvmValue {
                    ty: LlvmType::Integer { .. },
                    ..
                },
            ) => {
                let left = left
                    .try_implicit_cast(&right.ty, self.context.as_ref())
                    .unwrap_or_else(|| left);
                (left, right)
            }
            (
                left @ LlvmValue {
                    ty: LlvmType::Float { .. },
                    ..
                },
                right @ LlvmValue {
                    ty: LlvmType::CoercibleFloat(_),
                    ..
                },
            ) => {
                let right = right
                    .try_implicit_cast(&left.ty, self.context.as_ref())
                    .unwrap_or_else(|| right);
                (left, right)
            }
            (
                left @ LlvmValue {
                    ty: LlvmType::CoercibleFloat(_),
                    ..
                },
                right @ LlvmValue {
                    ty: LlvmType::Float { .. },
                    ..
                },
            ) => {
                let left = left
                    .try_implicit_cast(&right.ty, self.context.as_ref())
                    .unwrap_or_else(|| left);
                (left, right)
            }
            (left, right) => {
                let right = right
                    .try_implicit_cast(&left.ty, self.context.as_ref())
                    .unwrap_or_else(|| right);
                (left, right)
            }
        };

        let res = match (&left.ty, &right.ty) {
            (LlvmType::Integer { .. }, LlvmType::Integer { .. })
            | (LlvmType::CoercibleInteger(_), LlvmType::CoercibleInteger(_)) => {
                let signed = left.ty.integer_signed();

                let val = match op {
                    Operator::Plus => Some(self.context.builder.build_int_add(
                        left.llvm_value.into_int_value(),
                        right.llvm_value.into_int_value(),
                        "",
                    )),
                    Operator::Minus => Some(self.context.builder.build_int_sub(
                        left.llvm_value.into_int_value(),
                        right.llvm_value.into_int_value(),
                        "",
                    )),
                    Operator::Multiply => Some(self.context.builder.build_int_mul(
                        left.llvm_value.into_int_value(),
                        right.llvm_value.into_int_value(),
                        "",
                    )),
                    Operator::Divide if signed => Some(self.context.builder.build_int_signed_div(
                        left.llvm_value.into_int_value(),
                        right.llvm_value.into_int_value(),
                        "",
                    )),
                    Operator::Divide => Some(self.context.builder.build_int_unsigned_div(
                        left.llvm_value.into_int_value(),
                        right.llvm_value.into_int_value(),
                        "",
                    )),
                    Operator::Exponent => {
                        let powi = Intrinsic::find("llvm.powi");
                        // if let Some(powi) = powi {
                        //     powi.get_declaration(&self.context.module, )
                        // }
                        // self.context.builder.build_call(function, args, name)
                        None
                    }
                    _ => None,
                };

                val.map(|val| LlvmValue {
                    ty: left.ty.clone(),
                    kind: LlvmValueKind::Integer {},
                    llvm_value: val.into(),
                })
                .unwrap_or_else(|| LlvmValue::empty(self.context.as_ref()))
            }
            (LlvmType::Float { .. }, LlvmType::Float { .. })
            | (LlvmType::CoercibleFloat(_), LlvmType::CoercibleFloat(_)) => {
                // let signed = left.ty.integer_signed();

                let val = match op {
                    Operator::Plus => Some(self.context.builder.build_float_add(
                        left.llvm_value.into_float_value(),
                        right.llvm_value.into_float_value(),
                        "",
                    )),
                    Operator::Minus => Some(self.context.builder.build_float_sub(
                        left.llvm_value.into_float_value(),
                        right.llvm_value.into_float_value(),
                        "",
                    )),
                    Operator::Multiply => Some(self.context.builder.build_float_mul(
                        left.llvm_value.into_float_value(),
                        right.llvm_value.into_float_value(),
                        "",
                    )),
                    Operator::Divide => Some(self.context.builder.build_float_div(
                        left.llvm_value.into_float_value(),
                        right.llvm_value.into_float_value(),
                        "",
                    )),
                    Operator::Exponent => {
                        let powi = Intrinsic::find("llvm.powi");
                        // if let Some(powi) = powi {
                        //     powi.get_declaration(&self.context.module, )
                        // }
                        // self.context.builder.build_call(function, args, name)
                        None
                    }
                    _ => None,
                };

                val.map(|val| LlvmValue {
                    ty: left.ty.clone(),
                    kind: LlvmValueKind::Float {},
                    llvm_value: val.into(),
                })
                .unwrap_or_else(|| LlvmValue::empty(self.context.as_ref()))
            }
            _ => LlvmValue::empty(self.context.as_ref()),
        };

        // let res = match (&left.ty, &right.ty) {
        //     (LlvmType::CoercibleInteger(_), LlvmType::CoercibleInteger(_)) => match op {
        //         Operator::Plus => LlvmValue::cinteger(
        //             left.kind.as_integer() + right.kind.as_integer(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Minus => LlvmValue::cinteger(
        //             left.kind.as_integer() - right.kind.as_integer(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Multiply => LlvmValue::cinteger(
        //             left.kind.as_integer() * right.kind.as_integer(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Divide => LlvmValue::cinteger(
        //             left.kind.as_integer() / right.kind.as_integer(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Exponent => LlvmValue::cinteger(
        //             left.kind.as_integer().pow(right.kind.as_integer() as _),
        //             self.context.as_ref(),
        //         ),
        //         _ => LlvmValue::empty(self.context.as_ref()),
        //     },
        //     (
        //         LlvmType::Integer {
        //             // width,
        //             signed,
        //             llvm_type: ty,
        //         },
        //         LlvmType::CoercibleInteger(_),
        //     )
        //     | (
        //         LlvmType::CoercibleInteger(_),
        //         LlvmType::Integer {
        //             // width,
        //             signed,
        //             llvm_type: ty,
        //         },
        //     ) => {
        //         self.context.builder.build_int_add(
        //             left.llvm_basc_value().unwrap().into_int_value(),
        //             right.llvm_basc_value().unwrap().into_int_value(),
        //             "",
        //         );
        //         match op {
        //             Operator::Plus => LlvmValue::integer(
        //                 left.kind.as_integer() + right.kind.as_integer(),
        //                 ty.get_bit_width() as _,
        //                 *signed,
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Minus => LlvmValue::integer(
        //                 left.kind.as_integer() - right.kind.as_integer(),
        //                 ty.get_bit_width() as _,
        //                 *signed,
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Multiply => LlvmValue::integer(
        //                 left.kind.as_integer() * right.kind.as_integer(),
        //                 ty.get_bit_width() as _,
        //                 *signed,
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Divide => LlvmValue::integer(
        //                 left.kind.as_integer() / right.kind.as_integer(),
        //                 ty.get_bit_width() as _,
        //                 *signed,
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Exponent => LlvmValue::integer(
        //                 left.kind.as_integer().pow(right.kind.as_integer() as _),
        //                 ty.get_bit_width() as _,
        //                 *signed,
        //                 self.context.as_ref(),
        //             ),
        //             _ => LlvmValue::empty(self.context.as_ref()),
        //         }
        //     }
        //     (
        //         LlvmType::Integer {
        //             signed,
        //             llvm_type: ty,
        //         },
        //         LlvmType::Integer {
        //             signed: rs,
        //             llvm_type: rty,
        //         },
        //     ) if ty.get_bit_width() == rty.get_bit_width() && signed == rs => match op {
        //         Operator::Plus => LlvmValue::integer(
        //             left.kind.as_integer() + right.kind.as_integer(),
        //             ty.get_bit_width() as _,
        //             *signed,
        //             self.context.as_ref(),
        //         ),
        //         Operator::Minus => LlvmValue::integer(
        //             left.kind.as_integer() - right.kind.as_integer(),
        //             ty.get_bit_width() as _,
        //             *signed,
        //             self.context.as_ref(),
        //         ),
        //         Operator::Multiply => LlvmValue::integer(
        //             left.kind.as_integer() * right.kind.as_integer(),
        //             ty.get_bit_width() as _,
        //             *signed,
        //             self.context.as_ref(),
        //         ),
        //         Operator::Divide => LlvmValue::integer(
        //             left.kind.as_integer() / right.kind.as_integer(),
        //             ty.get_bit_width() as _,
        //             *signed,
        //             self.context.as_ref(),
        //         ),
        //         Operator::Exponent => LlvmValue::integer(
        //             left.kind.as_integer().pow(right.kind.as_integer() as _),
        //             ty.get_bit_width() as _,
        //             *signed,
        //             self.context.as_ref(),
        //         ),
        //         _ => LlvmValue::empty(self.context.as_ref()),
        //     },
        //     (LlvmType::CoercibleFloat(_), LlvmType::CoercibleFloat(_)) => match op {
        //         Operator::Plus => LlvmValue::cfloat(
        //             left.kind.as_float() + right.kind.as_float(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Minus => LlvmValue::cfloat(
        //             left.kind.as_float() - right.kind.as_float(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Multiply => LlvmValue::cfloat(
        //             left.kind.as_float() * right.kind.as_float(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Divide => LlvmValue::cfloat(
        //             left.kind.as_float() / right.kind.as_float(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Exponent => LlvmValue::cfloat(
        //             left.kind.as_float().powf(right.kind.as_float()),
        //             self.context.as_ref(),
        //         ),
        //         _ => LlvmValue::empty(self.context.as_ref()),
        //     },
        //     (ty @ LlvmType::Float(_), LlvmType::CoercibleFloat(_))
        //     | (LlvmType::CoercibleFloat(_), ty @ LlvmType::Float(_)) => match op {
        //         Operator::Plus => LlvmValue::float(
        //             left.kind.as_float() + right.kind.as_float(),
        //             ty.get_float_width(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Minus => LlvmValue::float(
        //             left.kind.as_float() - right.kind.as_float(),
        //             ty.get_float_width(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Multiply => LlvmValue::float(
        //             left.kind.as_float() * right.kind.as_float(),
        //             ty.get_float_width(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Divide => LlvmValue::float(
        //             left.kind.as_float() / right.kind.as_float(),
        //             ty.get_float_width(),
        //             self.context.as_ref(),
        //         ),
        //         Operator::Exponent => LlvmValue::float(
        //             left.kind.as_float().powf(right.kind.as_float()),
        //             ty.get_float_width(),
        //             self.context.as_ref(),
        //         ),
        //         _ => LlvmValue::empty(self.context.as_ref()),
        //     },
        //     (lty @ LlvmType::Float(_), rty @ LlvmType::Float(_))
        //         if lty.get_float_width() == rty.get_float_width() =>
        //     {
        //         match op {
        //             Operator::Plus => LlvmValue::float(
        //                 left.kind.as_float() + right.kind.as_float(),
        //                 lty.get_float_width(),
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Minus => LlvmValue::float(
        //                 left.kind.as_float() - right.kind.as_float(),
        //                 lty.get_float_width(),
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Multiply => LlvmValue::float(
        //                 left.kind.as_float() * right.kind.as_float(),
        //                 lty.get_float_width(),
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Divide => LlvmValue::float(
        //                 left.kind.as_float() / right.kind.as_float(),
        //                 lty.get_float_width(),
        //                 self.context.as_ref(),
        //             ),
        //             Operator::Exponent => LlvmValue::float(
        //                 left.kind.as_float().powf(right.kind.as_float()),
        //                 lty.get_float_width(),
        //                 self.context.as_ref(),
        //             ),
        //             _ => LlvmValue::empty(self.context.as_ref()),
        //         }
        //     }
        //     _ => LlvmValue::empty(self.context.as_ref()),
        // };

        match res.get_type() {
            LlvmType::Empty(_) => {
                self.add_error(EvaluationError {
                    kind: EvaluationErrorKind::BinExpMismatch(
                        op.clone(),
                        left.into(),
                        right.into(),
                    ),
                    range: Range::from((&raw_left.get_range(), &raw_right.get_range())),
                });
                LlvmValue::empty(self.context.as_ref())
            }
            _ => res.into(),
        }
        // LlvmValue::empty(self.context.as_ref())
    }
}

// impl<T: EvaluationType<Value = LlvmValue>, LlvmValue: EvaluationValue<Type = T> + Display, P: Pass> Evaluator<T, LlvmValue, P> {
//     pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> LlvmValue {
//         LlvmValue::empty(self.type_provider.as_ref())
//     }
// }
