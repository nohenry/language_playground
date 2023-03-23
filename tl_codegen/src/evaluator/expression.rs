use std::fmt::Display;

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

use crate::{llvm_value::LlvmValue, llvm_type::LlvmType};

use super::LlvmEvaluator;

impl<'a> LlvmEvaluator<'a, EvaluationPass> {
    pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> LlvmValue<'a> {
        match expression {
            Expression::Integer(val, _, _) => {
                LlvmValue::cinteger(*val, self.context.as_ref())
            }
            Expression::Float(val, _, _) => LlvmValue::cfloat(*val, self.context.as_ref()),
            Expression::Boolean(b, _) => LlvmValue::bool(*b, self.context.as_ref()),
            Expression::String(ParsedTemplateString(vs), _) => {
                let str = vs
                    .iter()
                    .map(|f| match f {
                        ParsedTemplate::String(s) => s.as_str().to_string(),
                        ParsedTemplate::Template(t, _, _) => {
                            let expr = self.evaluate_expression(t, index);

                            if let Some(data) = expr.resolve_ref() {
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
                        ScopeValue::EvaluationValue(cv) => LlvmValue::sym_reference(
                            &sym,
                            cv.get_type().clone(),
                            self.context.as_ref(),
                        ),
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
                    let expr = expr.resolve_ref().unwrap();
                    let expr = expr.borrow();
                    let expr = if let ScopeValue::EvaluationValue(cv) = &expr.value {
                        cv.clone()
                    } else {
                        return LlvmValue::empty(self.context.as_ref());
                    };
                    expr
                };

                if expr.is_function() {
                    let ptypes = expr.get_type().function_parameters_rf();
                    let body = expr.function_body();
                    let rf = expr.function_rf();

                    self.wstate().scope.push_scope(rf.clone());

                    let has_args: Option<Vec<_>> = args
                        .into_iter()
                        .zip(ptypes.into_iter())
                        .enumerate()
                        .map(|(i, (arg, (name, ty)))| {
                            let arg = arg
                                .try_implicit_cast(&ty, self.context.as_ref())
                                .unwrap_or(arg);

                            // let arg = if let Some(value) = arg.resolve_ref_value() {
                            //     ConstValue {
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
                                    range: raw_args.items.iter_items().nth(i).unwrap().get_range(),
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
                    if return_value.is_tuple() {
                        return_value
                            .tuple_value()
                            .into_iter()
                            .last()
                            .unwrap_or_else(|| LlvmValue::empty(self.context.as_ref()))
                    } else {
                        return_value
                    }
                } else if expr.is_native_function() {
                    let ptypes: LinkedHashMap<String, LlvmType<'a>> = expr
                        .get_type()
                        .function_parameters_rf()
                        .map(|(s, t)| (s.clone(), t.clone()))
                        .collect();
                    let callback = expr.native_function_callback();

                    let arglen = args.len();
                    let plen = ptypes.len();

                    let has_args: Option<LinkedHashMap<_, _>> = args
                        .into_iter()
                        .zip(ptypes.into_iter())
                        .enumerate()
                        .map(|(i, (arg, (name, ty)))| {
                            let arg = arg
                                .try_implicit_cast(&ty, self.context.as_ref())
                                .unwrap_or(arg);

                            if arg.get_type() != &ty {
                                self.add_error(EvaluationError {
                                    kind: EvaluationErrorKind::TypeMismatch(
                                        arg.into(),
                                        ty.clone(),
                                        TypeHint::Parameter,
                                    ),
                                    range: raw_args.items.iter_items().nth(i).unwrap().get_range(),
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

                    // ConstValue::record_instance(rf.clone());

                    return_val
                } else {
                    LlvmValue::empty(self.context.as_ref())
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
                let right = right.resolve_ref_value().unwrap();

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
                let right = right.resolve_ref_value().unwrap();
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
                        let child = left.resolve_ref().unwrap();
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

        let res = if left.is_cinteger() && right.is_cinteger() {
            match op {
                Operator::Plus => LlvmValue::cinteger(
                    left.integer_value() + right.integer_value(),
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::cinteger(
                    left.integer_value() - right.integer_value(),
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::cinteger(
                    left.integer_value() * right.integer_value(),
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::cinteger(
                    left.integer_value() / right.integer_value(),
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::cinteger(
                    left.integer_value().pow(right.integer_value() as _),
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else if left.is_integer() && right.is_cinteger()
            || left.is_cinteger() && right.is_integer()
        {
            let (width, signed) = if left.is_integer() {
                (left.integer_width(), left.is_signed())
            } else {
                (right.integer_width(), right.is_signed())
            };
            match op {
                Operator::Plus => LlvmValue::integer(
                    left.integer_value() + right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::integer(
                    left.integer_value() - right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::integer(
                    left.integer_value() * right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::integer(
                    left.integer_value() / right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::integer(
                    left.integer_value().pow(right.integer_value() as _),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else if left.is_integer()
            && right.is_integer()
            && left.integer_width() == right.integer_width()
            && left.is_signed() == right.is_signed()
        {
            let width = left.integer_width();
            let signed = left.is_signed();

            match op {
                Operator::Plus => LlvmValue::integer(
                    left.integer_value() + right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::integer(
                    left.integer_value() - right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::integer(
                    left.integer_value() * right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::integer(
                    left.integer_value() / right.integer_value(),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::integer(
                    left.integer_value().pow(right.integer_value() as _),
                    width,
                    signed,
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else if left.is_cfloat() && right.is_cfloat() {
            match op {
                Operator::Plus => LlvmValue::cfloat(
                    left.float_value() + right.float_value(),
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::cfloat(
                    left.float_value() - right.float_value(),
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::cfloat(
                    left.float_value() * right.float_value(),
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::cfloat(
                    left.float_value() / right.float_value(),
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::cfloat(
                    left.float_value().powf(right.float_value()),
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else if left.is_float() && right.is_cfloat() || left.is_cfloat() && right.is_float() {
            let width = if left.is_float() {
                left.float_width()
            } else {
                right.float_width()
            };
            match op {
                Operator::Plus => LlvmValue::float(
                    left.float_value() + right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::float(
                    left.float_value() - right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::float(
                    left.float_value() * right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::float(
                    left.float_value() / right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::float(
                    left.float_value().powf(right.float_value()),
                    width,
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else if left.is_float() && right.is_float() && left.float_width() == right.float_width() {
            let width = left.float_width();
            match op {
                Operator::Plus => LlvmValue::float(
                    left.float_value() + right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Minus => LlvmValue::float(
                    left.float_value() - right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Multiply => LlvmValue::float(
                    left.float_value() * right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Divide => LlvmValue::float(
                    left.float_value() / right.float_value(),
                    width,
                    self.context.as_ref(),
                ),
                Operator::Exponent => LlvmValue::float(
                    left.float_value().powf(right.float_value()),
                    width,
                    self.context.as_ref(),
                ),
                _ => LlvmValue::empty(self.context.as_ref()),
            }
        } else {
            LlvmValue::empty(self.context.as_ref())
        };

        if res.get_type().is_empty() {
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::BinExpMismatch(op.clone(), left.into(), right.into()),
                range: Range::from((&raw_left.get_range(), &raw_right.get_range())),
            });
            LlvmValue::empty(self.context.as_ref())
        } else {
            res
        }
    }
}

// impl<T: EvaluationType<Value = LlvmValue>, LlvmValue: EvaluationValue<Type = T> + Display, P: Pass> Evaluator<T, LlvmValue, P> {
//     pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> LlvmValue {
//         LlvmValue::empty(self.type_provider.as_ref())
//     }
// }
