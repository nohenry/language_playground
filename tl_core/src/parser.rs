use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::{
    ast::{
        ArgList, AstNode, EnclosedList, EnclosedPunctuationList, Param, ParamaterList,
        PunctuationList, Statement,
    },
    error::{ParseError, ParseErrorKind},
    token::{Operator, Range, SpannedToken, Token, TokenIndex, TokenStream},
};

pub struct Parser {
    pub(crate) tokens: TokenStream,
    pub(crate) errors: RwLock<Vec<ParseError>>,
}

#[macro_export]
macro_rules! restore {
    ($self:expr, $e:expr) => {{
        let index = $self.tokens.get_index();
        let res = $e;
        if res.is_none() {
            index.restore(&$self.tokens);
        }
        res
    }};
}

#[macro_export]
macro_rules! fallback {
    ($self:expr, $e:expr) => {{
        let index = $self.tokens.get_index();
        let res = $e;
        if res.is_none() {
            index.restore(&$self.tokens);
            return None;
        }
        res
    }};
}

impl Parser {
    pub fn new(token_stream: impl Into<TokenStream>) -> Self {
        Self {
            tokens: token_stream.into(),
            errors: RwLock::new(Vec::new()),
        }
    }

    pub fn get_errors(&self) -> RwLockReadGuard<'_, Vec<ParseError>> {
        self.errors.read().unwrap()
    }

    pub fn get_errors_mut(&self) -> RwLockWriteGuard<'_, Vec<ParseError>> {
        self.errors.write().unwrap()
    }

    pub fn add_error(&self, error: ParseError) {
        let p = &mut *self.errors.write().unwrap();
        p.push(error);
    }

    pub fn parse(&self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();
        self.ignore_ws();
        while let Some(stmt) = self.parse_statement() {
            statements.push(stmt);

            if let Some(Token::Newline) = self.tokens.peek() {
                self.tokens.next();
            }
            self.ignore_ws();
        }

        Some(statements)
    }

    pub fn parse_statement(&self) -> Option<Statement> {
        match self.tokens.peek() {
            Some(Token::Ident(s)) if s == "use" => {
                if let Some(us) = self.parse_use() {
                    return Some(us);
                }
            }
            Some(Token::Ident(s)) if s == "impl" => {
                if let Some(us) = self.parse_impl() {
                    return Some(us);
                }
            }
            Some(Token::Ident(s)) if s == "return" => {
                let tok = self.tokens.next().unwrap();
                let expr = self.parse_expression();

                return Some(Statement::Return {
                    ret_token: tok.clone(),
                    expr,
                });
            }
            Some(Token::Ident(s)) if s == "type" => {
                let ty_tok = self.tokens.next().unwrap();
                let symb = self.expect(Token::Ident(String::new())).unwrap();

                let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek()
                {
                    self.parse_generic_parameters()
                } else {
                    None
                };

                if let Some(Token::Operator(Operator::OpenBrace)) = self.tokens.peek() {
                    if let Some(strct) = self.parse_struct() {
                        return Some(Statement::TypeAlias {
                            ty_tok: ty_tok.clone(),
                            ident: symb.clone(),
                            generic,
                            eq: None,
                            ty: Box::new(strct),
                        });
                    }
                } else {
                    if let Some(us) = self.parse_type() {
                        let eq = self.expect_operator(Operator::Equals).unwrap();
                        return Some(Statement::TypeAlias {
                            ty_tok: ty_tok.clone(),
                            ident: symb.clone(),
                            generic,
                            eq: Some(eq.clone()),
                            ty: Box::new(us),
                        });
                    }
                }
            }
            Some(Token::Ident(id)) if id.as_str() == "fn" => {
                return self.parse_function();
            }
            Some(Token::Ident(_)) => {
                if let Some(decl) = restore!(self, self.parse_variable_decleration()) {
                    return Some(decl);
                }
            }
            Some(Token::Operator(Operator::OpenBrace)) => {
                if let Some(stmt) = self
                    .parse_enclosed_list(Operator::OpenBrace, Operator::CloseBrace, || {
                        self.parse_statement().map(|stmt| (stmt, true))
                    })
                    .map(|list| Statement::Block(list))
                {
                    return Some(stmt);
                }
            }
            _ => (),
        };

        let expression = self.parse_operator_expression(0);
        if expression.is_some() {
            return expression.map(Statement::Expression);
        }

        None
    }

    pub fn parse_variable_decleration(&self) -> Option<Statement> {
        let ty = self.parse_type();
        let ident = match self.tokens.peek() {
            Some(Token::Ident(_)) => self.tokens.next(),
            _ => return None,
        };

        if let (Some(ty), Some(ident)) = (ty, ident) {
            let Some(eq) = self.expect_operator(Operator::Equals).cloned() else {
                self.tokens.back();
                return None;
            };

            let expr = self.parse_expression();

            return Some(Statement::Decleration {
                ty,
                ident: ident.clone(),
                eq,
                expr,
            });
        }
        None
    }

    // pub fn parse_decleration(&self) -> Option<Statement> {
    //     let ident = match self.tokens.next() {
    //         Some(tok @ SpannedToken(_, Token::Ident(_))) => tok.clone(),
    //         _ => {
    //             self.tokens.back();
    //             return None;
    //         }
    //     };
    //     let Some(colon) = self.expect_operator(Operator::Colon).cloned() else {
    //         self.tokens.back();
    //         return None;
    //     };
    //     let expr = self.parse_expression(0);

    //     Some(Statement::Decleration { ident, colon, expr })
    // }

    pub fn parse_impl(&self) -> Option<Statement> {
        let token = match self.tokens.peek() {
            Some(Token::Ident(id)) if id == "impl" => self.tokens.next().cloned().unwrap(),
            _ => return None,
        };

        let generics = match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenAngle)) => self.parse_generic_parameters(),
            _ => None,
        };

        let ty = self.parse_type();

        // let open_brace = self.expect(Token::Operator(Operator::OpenBrace));

        let body = self.parse_enclosed_list(Operator::OpenBrace, Operator::CloseBrace, || {
            self.parse_statement().map(|f| (f, true))
        });

        Some(Statement::Impl {
            impl_tok: token,
            generics,
            ty,
            body,
        })
    }

    pub fn parse_use(&self) -> Option<Statement> {
        let token = self.tokens.next();
        let mut args = PunctuationList::default();
        let mut last_line = token.map(|l| l.span().line_num);
        while let Some(Token::Ident(_)) = self.tokens.peek() {
            let tok = self.tokens.next();

            match (self.tokens.peek(), tok) {
                (Some(Token::Operator(Operator::Dot)), Some(id)) => {
                    let dot = self.tokens.next();
                    args.push(id.clone(), dot.cloned());
                }
                (_, Some(id)) => {
                    let lline = *last_line.get_or_insert(id.span().line_num);
                    if lline == id.span().line_num {
                        args.push(id.clone(), None);
                    } else {
                        self.tokens.back();
                    }
                    break;
                }
                _ => break,
            }
        }
        Some(Statement::UseStatement {
            token: token.cloned(),
            args,
        })
    }

    pub fn parse_parameters(&self) -> Option<ParamaterList> {
        let open = self.expect_operator(Operator::OpenParen);

        let args = match self.tokens.peek() {
            Some(Token::Operator(Operator::CloseParen)) => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some(arg) = self.parse_parameter() {
                    if arg.name.is_none() && arg.ty.is_none() {
                        return None;
                    }
                    let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                        self.tokens.next().cloned()
                    } else {
                        None
                    };
                    if let Some(Token::Operator(Operator::CloseParen)) = self.tokens.peek() {
                        args.push(arg, comma);
                        break;
                    }
                    if comma.is_none() {
                        self.add_error(ParseError {
                            kind: ParseErrorKind::InvalidSyntax(
                                "Expected comma in arguments!".to_string(),
                            ),
                            range: Range::default(),
                        });
                    }
                    args.push(arg, comma);
                }
                args
            }
        };

        let close = self.expect_operator(Operator::CloseParen);

        if let (Some(open), Some(close)) = (open, close) {
            Some(ParamaterList {
                items: args,
                range: Range {
                    start: open.0,
                    end: close.0,
                },
            })
        } else {
            self.add_error(ParseError {
                kind: ParseErrorKind::InvalidSyntax(
                    "Unable to parse parameters brackets!".to_string(),
                ),
                range: Range::default(),
            });
            Some(ParamaterList {
                items: args,
                range: Range::default(),
            })
        }
    }

    pub fn parse_parameter(&self) -> Option<Param> {
        let ty = fallback!(self, self.parse_type());
        let ident = fallback!(self, self.expect(Token::Ident("".into())));

        match (ident, ty) {
            (Some(ident), Some(ty)) => Some(Param {
                ty: Some(ty),
                name: Some(ident.clone()),
            }),
            (ident, ty) => {
                self.add_error(ParseError {
                    kind: ParseErrorKind::InvalidSyntax("Unable to parse arg fields!".to_string()),
                    range: Range::default(),
                });
                Some(Param {
                    ty,
                    name: ident.cloned(),
                })
            }
        }
    }

    pub fn parse_arguments(&self) -> Option<ArgList> {
        let open = self.expect_operator(Operator::OpenParen);

        let args = match self.tokens.peek() {
            Some(Token::Operator(Operator::CloseParen)) => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some(arg) = self.parse_operator_expression(0) {
                    let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                        self.tokens.next().cloned()
                    } else {
                        None
                    };
                    if let Some(Token::Operator(Operator::CloseParen)) = self.tokens.peek() {
                        args.push(arg, comma);
                        break;
                    }
                    if comma.is_none() {
                        self.add_error(ParseError {
                            kind: ParseErrorKind::InvalidSyntax(
                                "Expected comma in arguments!".to_string(),
                            ),
                            range: Range::default(),
                        });
                    }
                    args.push_sep(arg, comma.unwrap());
                }
                args
            }
        };

        let close = self.expect_operator(Operator::CloseParen);

        if let (Some(open), Some(close)) = (open, close) {
            Some(ArgList {
                items: args,
                range: Range {
                    start: open.0,
                    end: close.0,
                },
            })
        } else {
            self.add_error(ParseError {
                kind: ParseErrorKind::InvalidSyntax("Unable to parse arg brackets!".to_string()),
                range: Range::default(),
            });
            Some(ArgList {
                items: args,
                range: Range::default(),
            })
        }
    }

    pub fn parse_list<T: AstNode>(
        &self,
        mut cb: impl FnMut() -> Option<(T, bool)>,
    ) -> Option<Vec<T>> {
        let mut items = Vec::new();

        while let Some((arg, valid)) = cb() {
            if !valid {
                return None;
            }

            items.push(arg);
        }

        Some(items)
    }

    pub fn parse_punctutation_list<T: AstNode>(
        &self,
        first: Option<T>,
        punc: Operator,
        mut cb: impl FnMut() -> Option<(T, bool)>,
    ) -> Option<PunctuationList<T>> {
        let mut args = PunctuationList::default();

        if let Some(first) = first {
            match self.tokens.peek() {
                Some(Token::Operator(op)) if op == &punc => {
                    args.push(first, self.tokens.next().cloned())
                }
                _ => return None,
            }
        }

        while let Some((arg, valid)) = cb() {
            if !valid {
                return None;
            }
            let punctuation = if let Some(Token::Operator(op)) = self.tokens.peek() {
                if op == &punc {
                    self.tokens.next().cloned()
                } else if args.len() == 0 {
                    return None;
                } else {
                    args.push_term(arg);
                    break;
                }
            } else if args.len() == 0 {
                return None;
            } else {
                args.push_term(arg);
                break;
            };

            args.push(arg, punctuation);
        }
        Some(args)
    }

    pub fn parse_enclosed_list<T: AstNode>(
        &self,
        open: Operator,
        close: Operator,
        cb: impl FnMut() -> Option<(T, bool)>,
    ) -> Option<EnclosedList<T>> {
        let open = self.expect_operator(open);

        let list = self.parse_list(cb);

        let close = self.expect_operator(close);

        if let (Some(open), Some(items), Some(close)) = (open, list, close) {
            Some(EnclosedList {
                open: open.clone(),
                items,
                close: close.clone(),
            })
        } else {
            None
        }
    }

    pub fn parse_enclosed_punctuation_list<T: AstNode>(
        &self,
        open: Operator,
        punc: Operator,
        close: Operator,
        mut cb: impl FnMut() -> Option<(T, bool)>,
    ) -> Option<EnclosedPunctuationList<T>> {
        let open = self.expect_operator(open);

        let args = match self.tokens.peek() {
            Some(Token::Operator(op)) if op == &close => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some((arg, valid)) = cb() {
                    if !valid {
                        return None;
                    }
                    let comma = if let Some(Token::Operator(op)) = self.tokens.peek() {
                        if op == &punc {
                            self.tokens.next().cloned()
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    if let Some(Token::Operator(cl)) = self.tokens.peek() {
                        if cl == &close {
                            args.push(arg, comma);
                            break;
                        }
                    }
                    if comma.is_none() {
                        self.add_error(ParseError {
                            kind: ParseErrorKind::InvalidSyntax(
                                "Expected comma in arguments!".to_string(),
                            ),
                            range: Range::default(),
                        });
                    }
                    args.push(arg, comma);
                }
                args
            }
        };

        let close = self.expect_operator(close);

        if let (Some(open), Some(close)) = (open, close) {
            Some(EnclosedPunctuationList {
                open: open.clone(),
                items: args,
                close: close.clone(),
            })
        } else {
            None
            // self.add_error(ParseError {
            //     kind: ParseErrorKind::InvalidSyntax("Unable to parse enclosed list!".to_string()),
            //     range: Range::default(),
            // });
            // panic!()
            // Some(Enclo{
            //     items: args,
            //     range: Range::default(),
            // })
        }
    }

    // fn parse_expression(&self) -> Option<Expression> {
    //     self.parse_literal()
    // }

    // fn parse_literal(&self) -> Option<Expression> {
    //     match self.tokens.peek() {
    //         Some(Token::Ident(_)) => Some(Expression::Ident(self.tokens.next().cloned().unwrap())),
    //         _ => None,
    //     }
    // }

    pub(crate) fn expect_operator(&self, operator: Operator) -> Option<&SpannedToken> {
        self.ignore_ws();
        let Some(Token::Operator(o)) = self.tokens.peek() else {
            return None;
        };

        if o == &operator {
            return self.tokens.next();
        }

        None
    }

    pub fn ignore_ws(&self) {
        while let Some(Token::Newline) = self.tokens.peek() {
            self.tokens.next();
        }
    }

    pub fn save_state(&self) -> TokenIndex {
        self.tokens.get_index()
    }

    pub(crate) fn expect(&self, token_type: Token) -> Option<&SpannedToken> {
        self.ignore_ws();
        let Some(tok) = self.tokens.peek() else {
            return None;
        };
        if std::mem::discriminant(tok) == std::mem::discriminant(&token_type) {
            return self.tokens.next();
        }

        None
    }
}
