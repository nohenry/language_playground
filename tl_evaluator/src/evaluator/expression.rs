use std::fmt::Display;

use linked_hash_map::LinkedHashMap;
use tl_core::{
    ast::{AstNode, Expression, ParsedTemplate, ParsedTemplateString},
    token::{Operator, Range, SpannedToken, Token},
};

use crate::{
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    evaluation_type::EvaluationType,
    evaluation_value::EvaluationValue,
    scope::scope::ScopeValue, pass::{EvaluationPass, TypeFirst, Pass},
};

use super::Evaluator;

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T> + Display> Evaluator<T, V, EvaluationPass> {
    pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> V {
        match expression {
            Expression::Integer(val, _, _) => V::cinteger(*val),
            Expression::Float(val, _, _) => V::cfloat(*val),
            Expression::Boolean(b, _) => V::bool(*b),
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
                V::string(str)
            }
            Expression::Ident(tok @ SpannedToken(_, Token::Ident(id))) => {
                let sym = self.rstate().scope.find_symbol(id);
                if let Some(sym) = sym {
                    let symv = sym.borrow();
                    match &symv.value {
                        ScopeValue::EvaluationValue(cv) => {
                            V::sym_reference(&sym, cv.get_type().clone())
                        }
                        ScopeValue::Struct { .. } => V::sym_reference(&sym, T::empty()),
                        _ => V::empty(),
                    }
                } else {
                    self.add_error(EvaluationError {
                        kind: EvaluationErrorKind::SymbolNotFound(id.to_string()),
                        range: tok.get_range(),
                    });
                    V::empty()
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

                V::create_struct_initializer(hmp)
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
                        return V::empty();
                    };
                    expr
                };

                if expr.is_function() {
                    let ptypes = expr.get_type().function_parameters();
                    let rptypes = expr.get_type().function_return();
                    let body = expr.function_body();
                    let rf = expr.function_rf();

                    self.wstate().scope.push_scope(rf.clone());

                    let has_args: Option<Vec<_>> = args
                        .into_iter()
                        .zip(ptypes.into_iter())
                        .enumerate()
                        .map(|(i, (arg, (name, ty)))| {
                            let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

                            // let arg = if let Some(value) = arg.resolve_ref_value() {
                            //     ConstValue {
                            //         ty: value.ty,
                            //         kind: arg.kind,
                            //     }
                            // } else {
                            //     arg
                            // };

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
                            self.wstate().scope.insert_value(
                                &name,
                                ScopeValue::EvaluationValue(arg),
                                index,
                            );

                            Some(())
                        })
                        .collect();

                    if has_args.is_none() {
                        return V::empty();
                    }

                    let return_value = self.evaluate_statement(&body, index);

                    self.wstate().scope.pop_scope();

                    // TODO: verify types here as well
                    if return_value.is_tuple() {
                        return_value
                            .tuple_value()
                            .into_iter()
                            .last()
                            .unwrap_or_else(|| V::empty())
                    } else {
                        return_value
                    }
                } else if expr.is_native_function() {
                    let ptypes: LinkedHashMap<String, T> =
                        expr.get_type().function_parameters().collect();
                    let rptypes = expr.get_type().function_return();
                    // let body = expr.function_body();
                    let callback = expr.native_function_callback();
                    let rf = expr.native_function_rf();

                    let arglen = args.len();
                    let plen = ptypes.len();

                    let has_args: Option<LinkedHashMap<_, _>> = args
                        .into_iter()
                        .zip(ptypes.into_iter())
                        .enumerate()
                        .map(|(i, (arg, (name, ty)))| {
                            let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

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
                        return V::empty();
                    } else if arglen != plen {
                        self.add_error(EvaluationError {
                            kind: EvaluationErrorKind::ArgCountMismatch(arglen as _, plen as _),
                            range: raw_args.get_range(),
                        });
                        return V::empty();
                    }

                    let return_val = callback(has_args.as_ref().unwrap());

                    // ConstValue::record_instance(rf.clone());

                    return_val
                } else {
                    V::empty()
                }
            }
            _ => V::empty(),
        }
    }

    pub fn evaluate_binary_expression(
        &self,
        raw_left: &Expression,
        op: &Operator,
        raw_right: &Expression,
        index: usize,
    ) -> V {
        match (op, raw_left) {
            (Operator::Equals, Expression::Ident(name)) => {
                let right = self.evaluate_expression(raw_right, index);
                let right = right.resolve_ref_value().unwrap();

                let Some(sym) = self.wstate().scope.find_symbol(name.as_str()) else {
                    self.add_error(EvaluationError { kind: EvaluationErrorKind::SymbolNotFound(name.as_str().to_string()), range: name.get_range() }

                    );
                    return V::empty()
                };

                let mut value = sym.borrow_mut();

                let ScopeValue::EvaluationValue(cv)  = &mut value.value else {
                    // TODO: throw error
                    return V::empty()
                };

                let right = right
                    .try_implicit_cast(cv.get_type())
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
                        .try_implicit_cast(cv.get_type())
                        .unwrap_or_else(|| right.clone());
                });
                if !updated_value {
                    return V::empty();
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
                                return V::empty()
                            };

                            cv.get_type().clone()
                        } else {
                            return V::empty();
                        };

                        return V::reference(left, member.as_str().to_string(), rtype);
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
                Operator::Plus => V::cinteger(left.integer_value() + right.integer_value()),
                Operator::Minus => V::cinteger(left.integer_value() - right.integer_value()),
                Operator::Multiply => V::cinteger(left.integer_value() * right.integer_value()),
                Operator::Divide => V::cinteger(left.integer_value() / right.integer_value()),
                Operator::Exponent => {
                    V::cinteger(left.integer_value().pow(right.integer_value() as _))
                }
                _ => V::empty(),
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
                Operator::Plus => {
                    V::integer(left.integer_value() + right.integer_value(), width, signed)
                }
                Operator::Minus => {
                    V::integer(left.integer_value() - right.integer_value(), width, signed)
                }
                Operator::Multiply => {
                    V::integer(left.integer_value() * right.integer_value(), width, signed)
                }
                Operator::Divide => {
                    V::integer(left.integer_value() / right.integer_value(), width, signed)
                }
                Operator::Exponent => V::integer(
                    left.integer_value().pow(right.integer_value() as _),
                    width,
                    signed,
                ),
                _ => V::empty(),
            }
        } else if left.is_integer()
            && right.is_integer()
            && left.integer_width() == right.integer_width()
            && left.is_signed() == right.is_signed()
        {
            let width = left.integer_width();
            let signed = left.is_signed();

            match op {
                Operator::Plus => {
                    V::integer(left.integer_value() + right.integer_value(), width, signed)
                }
                Operator::Minus => {
                    V::integer(left.integer_value() - right.integer_value(), width, signed)
                }
                Operator::Multiply => {
                    V::integer(left.integer_value() * right.integer_value(), width, signed)
                }
                Operator::Divide => {
                    V::integer(left.integer_value() / right.integer_value(), width, signed)
                }
                Operator::Exponent => V::integer(
                    left.integer_value().pow(right.integer_value() as _),
                    width,
                    signed,
                ),
                _ => V::empty(),
            }
        } else if left.is_cfloat() && right.is_cfloat() {
            match op {
                Operator::Plus => V::cfloat(left.float_value() + right.float_value()),
                Operator::Minus => V::cfloat(left.float_value() - right.float_value()),
                Operator::Multiply => V::cfloat(left.float_value() * right.float_value()),
                Operator::Divide => V::cfloat(left.float_value() / right.float_value()),
                Operator::Exponent => V::cfloat(left.float_value().powf(right.float_value())),
                _ => V::empty(),
            }
        } else if left.is_float() && right.is_cfloat() || left.is_cfloat() && right.is_float() {
            let width = if left.is_float() {
                left.float_width()
            } else {
                right.float_width()
            };
            match op {
                Operator::Plus => V::float(left.float_value() + right.float_value(), width),
                Operator::Minus => V::float(left.float_value() - right.float_value(), width),
                Operator::Multiply => V::float(left.float_value() * right.float_value(), width),
                Operator::Divide => V::float(left.float_value() / right.float_value(), width),
                Operator::Exponent => V::float(left.float_value().powf(right.float_value()), width),
                _ => V::empty(),
            }
        } else if left.is_float() && right.is_float() && left.float_width() == right.float_width() {
            let width = left.float_width();
            match op {
                Operator::Plus => V::float(left.float_value() + right.float_value(), width),
                Operator::Minus => V::float(left.float_value() - right.float_value(), width),
                Operator::Multiply => V::float(left.float_value() * right.float_value(), width),
                Operator::Divide => V::float(left.float_value() / right.float_value(), width),
                Operator::Exponent => V::float(left.float_value().powf(right.float_value()), width),
                _ => V::empty(),
            }
        } else {
            V::empty()
        };

        if res.get_type().is_empty() {
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::BinExpMismatch(op.clone(), left.into(), right.into()),
                range: Range::from((&raw_left.get_range(), &raw_right.get_range())),
            });
            V::empty()
        } else {
            res
        }
    }
}


// impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T> + Display, P: Pass> Evaluator<T, V, P> {
//     pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> V {
//         V::empty()
//     }
// }