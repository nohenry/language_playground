use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use linked_hash_map::LinkedHashMap;
use tl_core::{
    ast::{
        ArgList, AstNode, EnclosedList, Expression, Param, ParamaterList, ParsedTemplate,
        ParsedTemplateString, Statement,
    },
    token::{Operator, Range, SpannedToken, Token},
    Module,
};
use tl_util::{format::TreeDisplay, Rf};

use crate::{
    const_value::{ConstValue, ConstValueKind, Type},
    error::{EvaluationError, EvaluationErrorKind, TypeHint},
    scope::{Scope, ScopeManager, ScopeValue},
};

pub struct EvaluatorState {
    pub scope: ScopeManager,
    pub errors: Vec<EvaluationError>,
}

pub struct Evaluator {
    module: Arc<Module>,
    pub state: RwLock<EvaluatorState>,
}

impl Evaluator {
    pub fn new(module: Arc<Module>, scope_manager: ScopeManager) -> Evaluator {
        Evaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: scope_manager,
                errors: Vec::new(),
            }),
        }
    }

    fn rstate(&self) -> RwLockReadGuard<'_, EvaluatorState> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, EvaluatorState> {
        self.state.write().unwrap()
    }
}

impl Evaluator {
    pub fn evaluate(&self) -> Vec<ConstValue> {
        let vals = self
            .module
            .stmts
            .iter()
            .enumerate()
            .map(|(index, stmt)| self.evaluate_statement(stmt, index))
            .collect();

        vals
    }

    pub fn evaluate_statement(&self, statement: &Statement, index: usize) -> ConstValue {
        match statement {
            Statement::TypeAlias {
                ident,
                generic,
                ty: box tl_core::ast::Type::Struct(members),
                ..
            } => {
                let members = self.evaluate_struct_members(members);
                self.wstate().scope.update_value(
                    ident.as_str(),
                    ScopeValue::Struct {
                        members,
                        ident: ident.as_str().to_string(),
                    },
                    index,
                );
            }
            // Statement::Decleration {
            //     ident: SpannedToken(_, Token::Ident(id)),
            //     expr: Some(Expression::Record(parameters)),
            //     ..
            // } => {
            //     let members = self.evaluate_struct_members(parameters);
            //     self.wstate().scope.update_value(
            //         id,
            //         ScopeValue::Struct {
            //             members,
            //             ident: id.to_string(),
            //         },
            //         index,
            //     );
            // }
            Statement::Function {
                ident,
                parameters,
                return_parameters,
                body: Some(body),
                ..
            } => {
                let params = self.evaluate_params(parameters);
                let return_params = self.evaluate_params(return_parameters);

                let sym = self.wstate().scope.find_symbol(ident.as_str()).unwrap();

                let mut mut_sym = sym.borrow_mut();
                if let ScopeValue::ConstValue(ConstValue {
                    ty:
                        Type::Function {
                            parameters,
                            return_parameters,
                        },
                    ..
                }) = &mut mut_sym.value
                {
                    *parameters = params;
                    *return_parameters = return_params
                }
                // sym.borrow_mut().update(
                //     ident.as_str(),
                //     ScopeValue::ConstValue(ConstValue::func(
                //         Statement::clone(body),
                //         params,
                //         return_params,
                //         sym.clone(),
                //     )),
                // );
                // self.wstate().scope.update_value(
                //     ident.as_str(),
                //     ScopeValue::ConstValue(ConstValue::func(
                //         Statement::clone(body),
                //         parameters,
                //         return_parameters,
                //         sym,
                //     )),
                //     index,
                // );
            }
            Statement::Decleration {
                ty,
                ident,
                expr: Some(raw_expr),
                ..
            } => {
                let ty = self.evaluate_type(ty);
                let expr = self.evaluate_expression(raw_expr, index);

                match (ty, expr) {
                    (
                        Type::Symbol(sym),
                        ConstValue {
                            kind: ConstValueKind::StructInitializer { members: args },
                            ..
                        },
                    ) => {
                        if let ScopeValue::Struct { members, .. } = &sym.borrow().value {
                            let value = self.evaluate_struct_init(
                                &sym,
                                members,
                                &args,
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
                                ScopeValue::ConstValue(value),
                                index,
                            );
                        }
                    }
                    (ty, expr) => {
                        let expr = expr.try_implicit_cast(&ty).unwrap_or(expr);
                        if expr.ty != ty {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::TypeMismatch(
                                    expr.ty,
                                    ty,
                                    TypeHint::Variable,
                                ),
                                range: raw_expr.get_range(),
                            });
                            return ConstValue::empty();
                        }
                        self.wstate().scope.insert_value(
                            ident.as_str(),
                            ScopeValue::ConstValue(expr),
                            index,
                        );
                    }
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
                    return ConstValue::tuple(values);
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
                    return ConstValue::tuple(values);
                }
            }

            // Statement::UseStatement { args, .. } => {
            //     let path = args
            //         .iter_items()
            //         .map(|sym| sym.as_str().to_string())
            //         .collect();
            //     self.wstate().scope.add_use(path)
            // }
            _ => (),
        }
        ConstValue::empty()
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

    pub fn evaluate_expression(&self, expression: &Expression, index: usize) -> ConstValue {
        match expression {
            Expression::Integer(val, _, _) => ConstValue::cinteger(*val),
            Expression::Float(val, _, _) => ConstValue::cfloat(*val),
            Expression::Boolean(b, _) => ConstValue::bool(*b),
            Expression::String(ParsedTemplateString(vs), _) => {
                let str = vs
                    .iter()
                    .map(|f| match f {
                        ParsedTemplate::String(s) => s.as_str().to_string(),
                        ParsedTemplate::Template(t, _, _) => {
                            let expr = self.evaluate_expression(t, index);

                            if let Some(data) = expr.resolve_ref() {
                                if let ScopeValue::ConstValue(cv) = &data.borrow().value {
                                    return format!("{}", cv);
                                }
                            }

                            format!("{expr}")
                        }
                    })
                    .intersperse("".to_string())
                    .collect::<String>();
                ConstValue::string(str)
            }
            Expression::Ident(tok @ SpannedToken(_, Token::Ident(id))) => {
                let sym = self.rstate().scope.find_symbol(id);
                if let Some(sym) = sym {
                    let symv = sym.borrow();
                    // println!("{}", self.rstate().scope.module.format());
                    match &symv.value {
                        ScopeValue::ConstValue(cv) => ConstValue::reference(&sym, cv.ty.clone()),
                        ScopeValue::Struct { .. } => ConstValue {
                            ty: Type::Symbol(sym.clone()),
                            kind: ConstValueKind::Empty,
                        },
                        _ => ConstValue::empty(),
                    }
                } else {
                    self.add_error(EvaluationError {
                        kind: EvaluationErrorKind::SymbolNotFound(id.to_string()),
                        range: tok.get_range(),
                    });
                    ConstValue::empty()
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

                ConstValue::record_initializer(hmp)
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
                    let expr = if let ScopeValue::ConstValue(cv) = &expr.value {
                        cv.clone()
                    } else {
                        return ConstValue::empty();
                    };
                    expr
                };

                match (&expr.ty, &expr.kind) {
                    // Function is called
                    (
                        Type::Function {
                            parameters: ptypes,
                            return_parameters: rptypes,
                        },
                        ConstValueKind::Function { body, rf },
                    ) => {
                        self.wstate().scope.push_scope(rf.clone());

                        let has_args: Option<Vec<_>> = args
                            .into_iter()
                            .zip(ptypes.into_iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

                                if &arg.ty != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.ty,
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
                                    ScopeValue::ConstValue(arg),
                                    index,
                                );

                                Some(())
                            })
                            .collect();

                        if has_args.is_none() {
                            return ConstValue::empty();
                        }

                        let _ = self.evaluate_statement(&body, index);

                        // TODO: verify types here as well

                        let return_values: LinkedHashMap<_, _> = rptypes
                            .into_iter()
                            .map(|(name, ty)| {
                                let sym = self.rstate().scope.find_symbol_local(&name);
                                let vl = if let Some(sym) = sym {
                                    let sym = sym.borrow();
                                    if let ScopeValue::ConstValue(cv) = &sym.value {
                                        if &cv.ty == ty {
                                            cv.clone()
                                        } else {
                                            // TODO: error handling
                                            ConstValue::empty()
                                        }
                                    } else {
                                        // TODO: error handling
                                        ConstValue::empty()
                                    }
                                } else {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::NotInitialized {
                                            hint: TypeHint::ReturnParameter,
                                        },
                                        range: expression.get_range(),
                                    });
                                    ConstValue::default_for(ty)
                                };
                                (name.clone(), vl)
                            })
                            .collect();

                        let value = ConstValue::record_instance(rf.clone(), return_values);

                        self.wstate().scope.pop_scope();

                        value
                    }
                    (
                        Type::Function {
                            parameters: ptypes, ..
                        },
                        ConstValueKind::NativeFunction { rf, callback },
                    ) => {
                        let arglen = args.len();
                        let plen = ptypes.len();

                        let has_args: Option<LinkedHashMap<_, _>> = args
                            .into_iter()
                            .zip(ptypes.into_iter())
                            .enumerate()
                            .map(|(i, (arg, (name, ty)))| {
                                let arg = arg.try_implicit_cast(&ty).unwrap_or(arg);

                                if &arg.ty != ty {
                                    self.add_error(EvaluationError {
                                        kind: EvaluationErrorKind::TypeMismatch(
                                            arg.ty,
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
                            return ConstValue::empty();
                        } else if arglen != plen {
                            self.add_error(EvaluationError {
                                kind: EvaluationErrorKind::ArgCountMismatch(arglen as _, plen as _),
                                range: raw_args.get_range(),
                            });
                            return ConstValue::empty();
                        }

                        let return_vals = callback(has_args.as_ref().unwrap());

                        ConstValue::record_instance(rf.clone(), return_vals)
                    }
                    // TODO: throw error
                    _ => ConstValue::empty(),
                }
            }
            _ => ConstValue::empty(),
        }
    }

    pub fn evaluate_struct_init(
        &self,
        symbol: &Rf<Scope>,
        members: &LinkedHashMap<String, Type>,
        args: &LinkedHashMap<String, ConstValue>,
        member_range_provider: impl Fn(usize) -> Option<Range>,
        full_range: Range,
    ) -> ConstValue {
        let arg_len = args.len();
        let len_off = members.len() != args.len();

        let arg_vals: LinkedHashMap<_, _> = members
            .iter()
            .enumerate()
            .filter_map(|(i, (name, ty))| {
                if let Some(arg) = args.get(name) {
                    let arg = arg.try_implicit_cast(ty).unwrap_or_else(|| arg.clone());

                    if &arg.ty == ty {
                        return Some((name.clone(), arg));
                    } else {
                        self.add_error(EvaluationError {
                            kind: EvaluationErrorKind::TypeMismatch(
                                arg.ty,
                                ty.clone(),
                                TypeHint::Parameter,
                            ),
                            range: member_range_provider(i).unwrap_or(full_range),
                        });
                        return None;
                    }
                }
                None
            })
            .collect();

        if len_off {
            // If the number of arguments doesn't match the record
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::ArgCountMismatch(arg_len as _, members.len() as _),
                range: full_range,
            });
        } else if arg_vals.len() == members.len() {
            // Everything good!
            return ConstValue::record_instance(symbol.clone(), arg_vals);
        }
        ConstValue::empty()
    }

    pub fn evaluate_binary_expression(
        &self,
        raw_left: &Expression,
        op: &Operator,
        raw_right: &Expression,
        index: usize,
    ) -> ConstValue {
        match (op, raw_left) {
            (Operator::Equals, Expression::Ident(SpannedToken(_, Token::Ident(name)))) => {
                let right = self.evaluate_expression(raw_right, index);
                self.wstate().scope.update_value(
                    name,
                    ScopeValue::ConstValue(right.clone()),
                    index,
                );
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
                let scope = &mut self.wstate().scope;
                let updated_value = scope.follow_member_access_mut(dleft, dright, |cv| {
                    *cv = right.clone();
                });
                if !updated_value {
                    return ConstValue::empty();
                };
                return right;
            }
            (Operator::Dot, _) => {
                let left = self.evaluate_expression(raw_left, index);
                // println!("potato: s{}", left.format());
                let left = left.resolve_ref();
                let left = left.as_ref().unwrap().borrow();
                let left = if let ScopeValue::ConstValue(cv) = &left.value {
                    cv
                } else {
                    return ConstValue::empty();
                };

                match (&left.kind, raw_right) {
                    (
                        ConstValueKind::StructInstance { members, .. },
                        Expression::Ident(SpannedToken(_, Token::Ident(member))),
                    ) => {
                        if let Some(val) = members.get(member) {
                            return val.clone();
                        }
                    }
                    (
                        ConstValueKind::Ref { symbol, offset },
                        Expression::Ident(SpannedToken(_, Token::Ident(member))),
                    ) => {
                        if let ScopeValue::ConstValue(cv) = &symbol.borrow().value {
                            if let ConstValueKind::StructInstance { rf, members } = &cv.kind {
                                if let Some(val) = members.get(member) {
                                    return val.clone();
                                }
                            }
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        let left = self.evaluate_expression(raw_left, index);
        let right = self.evaluate_expression(raw_right, index);

        let res = match (&left.ty, &right.ty) {
            (Type::CoercibleInteger, Type::CoercibleInteger) => match op {
                Operator::Plus => {
                    ConstValue::cinteger(left.kind.as_integer() + right.kind.as_integer())
                }
                Operator::Minus => {
                    ConstValue::cinteger(left.kind.as_integer() - right.kind.as_integer())
                }
                Operator::Multiply => {
                    ConstValue::cinteger(left.kind.as_integer() * right.kind.as_integer())
                }
                Operator::Divide => {
                    ConstValue::cinteger(left.kind.as_integer() / right.kind.as_integer())
                }
                Operator::Exponent => {
                    ConstValue::cinteger(left.kind.as_integer().pow(right.kind.as_integer() as _))
                }
                _ => ConstValue::empty(),
            },
            (Type::Integer { width, signed }, Type::CoercibleInteger)
            | (Type::CoercibleInteger, Type::Integer { width, signed }) => match op {
                Operator::Plus => ConstValue::integer(
                    left.kind.as_integer() + right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Minus => ConstValue::integer(
                    left.kind.as_integer() - right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Multiply => ConstValue::integer(
                    left.kind.as_integer() * right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Divide => ConstValue::integer(
                    left.kind.as_integer() / right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Exponent => ConstValue::integer(
                    left.kind.as_integer().pow(right.kind.as_integer() as _),
                    *width,
                    *signed,
                ),
                _ => ConstValue::empty(),
            },
            (
                Type::Integer { width, signed },
                Type::Integer {
                    width: rw,
                    signed: rs,
                },
            ) if width == rw && signed == rs => match op {
                Operator::Plus => ConstValue::integer(
                    left.kind.as_integer() + right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Minus => ConstValue::integer(
                    left.kind.as_integer() - right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Multiply => ConstValue::integer(
                    left.kind.as_integer() * right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Divide => ConstValue::integer(
                    left.kind.as_integer() / right.kind.as_integer(),
                    *width,
                    *signed,
                ),
                Operator::Exponent => ConstValue::integer(
                    left.kind.as_integer().pow(right.kind.as_integer() as _),
                    *width,
                    *signed,
                ),
                _ => ConstValue::empty(),
            },
            (Type::CoercibleFloat, Type::CoercibleFloat) => match op {
                Operator::Plus => ConstValue::cfloat(left.kind.as_float() + right.kind.as_float()),
                Operator::Minus => ConstValue::cfloat(left.kind.as_float() - right.kind.as_float()),
                Operator::Multiply => {
                    ConstValue::cfloat(left.kind.as_float() * right.kind.as_float())
                }
                Operator::Divide => {
                    ConstValue::cfloat(left.kind.as_float() / right.kind.as_float())
                }
                Operator::Exponent => {
                    ConstValue::cfloat(left.kind.as_float().powf(right.kind.as_float()))
                }
                _ => ConstValue::empty(),
            },
            (Type::Float { width }, Type::CoercibleFloat)
            | (Type::CoercibleFloat, Type::Float { width }) => match op {
                Operator::Plus => {
                    ConstValue::float(left.kind.as_float() + right.kind.as_float(), *width)
                }
                Operator::Minus => {
                    ConstValue::float(left.kind.as_float() - right.kind.as_float(), *width)
                }
                Operator::Multiply => {
                    ConstValue::float(left.kind.as_float() * right.kind.as_float(), *width)
                }
                Operator::Divide => {
                    ConstValue::float(left.kind.as_float() / right.kind.as_float(), *width)
                }
                Operator::Exponent => {
                    ConstValue::float(left.kind.as_float().powf(right.kind.as_float()), *width)
                }
                _ => ConstValue::empty(),
            },
            (Type::Float { width }, Type::Float { width: rw }) if width == rw => match op {
                Operator::Plus => {
                    ConstValue::float(left.kind.as_float() + right.kind.as_float(), *width)
                }
                Operator::Minus => {
                    ConstValue::float(left.kind.as_float() - right.kind.as_float(), *width)
                }
                Operator::Multiply => {
                    ConstValue::float(left.kind.as_float() * right.kind.as_float(), *width)
                }
                Operator::Divide => {
                    ConstValue::float(left.kind.as_float() / right.kind.as_float(), *width)
                }
                Operator::Exponent => {
                    ConstValue::float(left.kind.as_float().powf(right.kind.as_float()), *width)
                }
                _ => ConstValue::empty(),
            },
            _ => ConstValue::empty(),
        };

        if let Type::Empty = &res.ty {
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::BinExpMismatch(op.clone(), left.ty, right.ty),
                range: Range::from((&raw_left.get_range(), &raw_right.get_range())),
            });
            ConstValue::empty()
        } else {
            res
        }
    }

    fn evaluate_args(&self, args: &ArgList, index: usize) -> Vec<ConstValue> {
        args.iter_items()
            .map(|expr| self.evaluate_expression(expr, index))
            .collect()
    }

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
                self.add_error(EvaluationError {
                    kind: EvaluationErrorKind::SymbolNotFound(id.as_str().to_string()),
                    range: id.get_range(),
                });
                Type::Empty
            }
            tl_core::ast::Type::Boolean(_) => Type::Boolean,
            tl_core::ast::Type::Ref {
                base_type: Some(ty),
                ..
            } => Type::Ref {
                base_type: Box::new(self.evaluate_type(ty)),
            },
            _ => Type::Empty,
        }
    }

    fn add_error(&self, error: EvaluationError) {
        self.wstate().errors.push(error)
    }
}
