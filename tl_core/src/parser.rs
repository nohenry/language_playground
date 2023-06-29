use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use tl_util::format::{NodeDisplay, TreeDisplay};

use crate::{
    ast::{
        ArgList, AstNode, Binding, ClassBody, EnclosedList, EnclosedPunctuationList, Expression,
        GetterSetter, InterfaceBody, MatchBinding, Modifer, ModiferStatement, Param, ParamaterList,
        PunctuationList, Statement, Type,
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

            self.ignore_ws();
        }

        Some(statements)
    }

    pub fn parse_statement(&self) -> Option<Statement> {
        if let Some(modifer) = self.parse_modifier(Self::parse_statement) {
            return Some(Statement::Modifer(modifer));
        }

        match self.tokens.peek() {
            Some(Token::Ident(s)) if s == "import" => {
                if let Some(us) = self.parse_import() {
                    return Some(us);
                }
            }
            Some(Token::Ident(s)) if s == "return" => {
                let tok = self.tokens.next().unwrap();
                let expr = self.parse_expression(None);

                return Some(Statement::Return {
                    ret_token: tok.clone(),
                    expr,
                });
            }
            Some(Token::Ident(s)) if s == "class" => {
                if let Some(strct) = self.parse_class_declaration() {
                    return Some(strct);
                }
            }
            Some(Token::Ident(s)) if s == "interface" => {
                if let Some(strct) = self.parse_interface_declaration() {
                    return Some(strct);
                }
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
            Some(Token::Ident(_)) => {
                if let Some(decl) = restore!(self, self.parse_variable_or_function_declaration()) {
                    return Some(decl);
                }
            }
            Some(Token::Operator(Operator::OpenBrace)) => {
                if let Some(stmt) = self.parse_block() {
                    return Some(stmt);
                }
            }
            _ => (),
        };

        let expression = self.parse_expression(None);
        if expression.is_some() {
            return expression.map(Statement::Expression);
        }

        None
    }

    pub fn parse_block(&self) -> Option<Statement> {
        self.parse_enclosed_punctuation_list(
            Operator::OpenBrace,
            Operator::Newline,
            Operator::CloseBrace,
            || self.parse_statement().map(|stmt| (stmt, true)),
        )
        .map(|list| Statement::Expression(Expression::Block(list)))
    }

    pub fn parse_block_expression(&self) -> Option<Expression> {
        self.parse_enclosed_punctuation_list(
            Operator::OpenBrace,
            Operator::Newline,
            Operator::CloseBrace,
            || self.parse_statement().map(|stmt| (stmt, true)),
        )
        .map(|list| Expression::Block(list))
    }

    pub fn parse_modifier<T: AstNode + NodeDisplay + TreeDisplay>(
        &self,
        f: impl Fn(&Parser) -> Option<T>,
    ) -> Option<ModiferStatement<T>> {
        match self.tokens.peek() {
            Some(Token::Ident(modifier)) => {
                let modifier = match modifier.as_str() {
                    "public" => Modifer::Public,
                    "protected" => Modifer::Protected,
                    "const" => Modifer::Const,
                    "unique" => Modifer::Unique,
                    "closed" => Modifer::Closed,
                    _ => return None,
                };
                let token = self.tokens.next().unwrap();

                let statement = f(self);

                Some(ModiferStatement {
                    modifier,
                    modifier_token: token.clone(),
                    statement: statement.map(|f| Box::new(f)),
                })
            }
            _ => None,
        }
    }

    pub fn parse_variable_or_function_declaration(&self) -> Option<Statement> {
        let ty = self.parse_type();
        let ident = match self.tokens.peek() {
            Some(Token::Ident(_)) => self.tokens.next(),
            _ => return None,
        };

        if let (Some(ty), Some(ident)) = (ty, ident) {
            match self.tokens.peek() {
                Some(Token::Operator(Operator::Equals)) => {
                    let eq = self.tokens.next().cloned().unwrap();
                    let expr = Box::new(self.parse_expression(None)?);

                    Some(Statement::VariableDeclaration {
                        ty,
                        ident: ident.clone(),
                        default: Some((eq, expr)),
                    })
                }
                Some(Token::Operator(Operator::EqArrow)) => {
                    let arrow = self.tokens.next().cloned().unwrap();

                    let expr = self.parse_expression(None);

                    Some(Statement::ComputedVariableDeclaration {
                        ty,
                        ident: ident.clone(),
                        arrow,
                        expr,
                    })
                }
                _ => restore!(
                    self,
                    self.parse_function_declaration(ty.clone(), ident.clone())
                )
                .or_else(|| {
                    Some(Statement::VariableDeclaration {
                        ty,
                        ident: ident.clone(),
                        default: None,
                    })
                }),
            }
        } else {
            None
        }
    }

    pub fn parse_class_field_or_function_declaration(&self) -> Option<ClassBody> {
        let ty = self.parse_type();
        let ident = match self.tokens.peek() {
            Some(Token::Ident(_)) => self.tokens.next(),
            _ => return None,
        };

        if let (Some(ty), Some(ident)) = (ty, ident) {
            match self.tokens.peek() {
                Some(Token::Operator(Operator::Equals)) => {
                    let eq = self.tokens.next().cloned().unwrap();
                    let expr = Box::new(self.parse_expression(None)?);

                    Some(ClassBody::Field {
                        field_type: Box::new(ty),
                        name: ident.clone(),
                        default: Some((eq, expr)),
                    })
                }
                Some(Token::Operator(Operator::EqArrow)) => {
                    let arrow = self.tokens.next().cloned().unwrap();

                    if let Some(Token::Operator(Operator::OpenBrace)) = self.tokens.peek() {
                        if let Some(list) = restore!(
                            self,
                            self.parse_enclosed_list(
                                Operator::OpenBrace,
                                Operator::CloseBrace,
                                || self.parse_getter_setter().map(|f| (f, true)),
                            )
                        ) {
                            return Some(ClassBody::GetterSetters {
                                field_type: Box::new(ty),
                                name: ident.clone(),
                                arrow,
                                entries: list,
                            });
                        } else {
                            let expression = Box::new(self.parse_expression(None)?);

                            Some(ClassBody::ComputedField {
                                field_type: Box::new(ty),
                                name: ident.clone(),
                                arrow,
                                expression,
                            })
                        }
                    } else {
                        let expression = Box::new(self.parse_expression(None)?);

                        Some(ClassBody::ComputedField {
                            field_type: Box::new(ty),
                            name: ident.clone(),
                            arrow,
                            expression,
                        })
                    }
                }
                _ => restore!(
                    self,
                    self.parse_function_declaration(ty.clone(), ident.clone())
                )
                .map(|func| match func {
                    Statement::Function {
                        return_type,
                        ident,
                        generic,
                        parameters,
                        arrow,
                        body,
                    } => ClassBody::Function {
                        return_type,
                        ident,
                        generic,
                        parameters,
                        arrow,
                        body,
                    },
                    _ => panic!(),
                })
                .or_else(|| {
                    Some(ClassBody::Field {
                        field_type: Box::new(ty),
                        name: ident.clone(),
                        default: None,
                    })
                }),
            }
        } else {
            None
        }
    }

    pub fn parse_function_declaration(&self, ty: Type, ident: SpannedToken) -> Option<Statement> {
        let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
            self.parse_generic_parameters()
        } else {
            None
        };

        let parameters = self.parse_parameters()?;

        let (arrow, body) = if let Some(Token::Operator(Operator::EqArrow)) = self.tokens.peek() {
            let arrow = self.tokens.next().unwrap().clone();

            (
                Some(arrow),
                self.parse_expression(None)
                    .map(|expr| Statement::Expression(expr)),
            )
        } else {
            (None, self.parse_block())
        };

        Some(Statement::Function {
            return_type: ty,
            ident,
            generic,
            parameters,
            arrow,
            body: body.map(Box::new),
        })
    }

    pub fn parse_class_declaration(&self) -> Option<Statement> {
        let token = self.tokens.next()?;
        let ident = self.expect_ident()?;

        let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
            self.parse_generic_parameters()
        } else {
            None
        };

        let inherits = if let Some(Token::Operator(Operator::Colon)) = self.tokens.peek() {
            let colon = self.tokens.next().unwrap().clone();

            let list = self.parse_punctutation_list(None, Operator::Comma, || {
                self.parse_type().map(|f| (f, true))
            })?;

            Some((colon, list))
        } else {
            None
        };

        let body = self.parse_enclosed_punctuation_list(
            Operator::OpenBrace,
            Operator::Newline,
            Operator::CloseBrace,
            || self.parse_class_body().map(|f| (f, true)),
        );

        if let Some(body) = body {
            Some(Statement::Class {
                token: token.clone(),
                ident: Some(ident.clone()),
                generic,
                body,
                inherits,
            })
        } else {
            None
        }
    }

    pub fn parse_class_body(&self) -> Option<ClassBody> {
        if let Some(modifier) = self.parse_modifier(Self::parse_class_body) {
            return Some(ClassBody::Modifier(modifier));
        }

        let op = self.parse_class_field_or_function_declaration();

        op
    }

    pub fn parse_getter_setter(&self) -> Option<GetterSetter> {
        println!("{:?}", self.tokens.current());
        match self.tokens.peek_ignore_ws() {
            Some(Token::Ident(ident)) if ident == "get" => {
                let get_token = self.tokens.next_ignore_ws()?.clone();

                let (colon, expression) =
                    if let Some(Token::Operator(Operator::Colon)) = self.tokens.peek() {
                        let colon = self.tokens.next()?.clone();
                        let expression = self.parse_expression(None)?;

                        (Some(colon), expression)
                    } else {
                        (None, self.parse_block_expression()?)
                    };

                Some(GetterSetter::Get {
                    get_token,
                    colon,
                    body: Box::new(expression),
                })
            }
            Some(Token::Ident(ident)) if ident == "set" => {
                let set_token = self.tokens.next_ignore_ws()?.clone();

                let (colon, expression) =
                    if let Some(Token::Operator(Operator::Colon)) = self.tokens.peek() {
                        let colon = self.tokens.next()?.clone();
                        let expression = self.parse_expression(None)?;

                        (Some(colon), expression)
                    } else {
                        (None, self.parse_block_expression()?)
                    };

                Some(GetterSetter::Set {
                    set_token,
                    colon,
                    body: Box::new(expression),
                })
            }
            _ => None,
        }
    }

    pub fn parse_interface_declaration(&self) -> Option<Statement> {
        let token = self.tokens.next()?.clone();
        let ident = self.expect_ident()?.clone();

        let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
            self.parse_generic_parameters()
        } else {
            None
        };

        let inherits = if let Some(Token::Operator(Operator::Colon)) = self.tokens.peek() {
            let colon = self.tokens.next().unwrap().clone();

            let list = self.parse_punctutation_list(None, Operator::Comma, || {
                self.parse_type().map(|f| (f, true))
            })?;

            Some((colon, list))
        } else {
            None
        };

        let body = self.parse_enclosed_punctuation_list(
            Operator::OpenBrace,
            Operator::Newline,
            Operator::CloseBrace,
            || self.parse_interface_body().map(|f| (f, true)),
        )?;

        Some(Statement::Interface {
            token,
            ident: Some(ident),
            generic,
            inherits,
            body,
        })
    }

    pub fn parse_interface_body(&self) -> Option<InterfaceBody> {
        if let Some(modifier) = self.parse_modifier(Self::parse_interface_body) {
            return Some(InterfaceBody::Modifier(modifier));
        }

        self.parse_interface_computed_or_function()
    }

    pub fn parse_interface_computed_or_function(&self) -> Option<InterfaceBody> {
        let ty = self.parse_type();
        let ident = self.expect_ident()?.clone();

        match self.tokens.peek() {
            Some(Token::Operator(Operator::EqArrow)) => self.parse_interface_computed(ty?, ident),
            _ => self.parse_interface_function(ty?, ident),
        }
    }

    pub fn parse_interface_computed(&self, ty: Type, name: SpannedToken) -> Option<InterfaceBody> {
        let arrow = self.expect_operator(Operator::EqArrow)?.clone();

        if let Some(Token::Operator(Operator::OpenBrace)) = self.tokens.peek() {
            let list = self.parse_enclosed_punctuation_list(
                Operator::OpenBrace,
                Operator::Comma,
                Operator::CloseBrace,
                || match self.tokens.peek() {
                    Some(Token::Ident(ident)) if ident == "get" || ident == "set" => {
                        Some((self.tokens.next().unwrap().clone(), true))
                    }
                    _ => None,
                },
            )?;

            Some(InterfaceBody::ComputedField {
                field_type: Box::new(ty),
                name,
                arrow,
                getters_setters: Some(list),
            })
        } else {
            Some(InterfaceBody::ComputedField {
                field_type: Box::new(ty),
                name,
                arrow,
                getters_setters: None,
            })
        }
    }

    pub fn parse_interface_function(&self, ty: Type, name: SpannedToken) -> Option<InterfaceBody> {
        let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
            self.parse_generic_parameters()
        } else {
            None
        };

        let parameters = self.parse_parameters().unwrap();

        Some(InterfaceBody::Function {
            return_type: ty,
            ident: name,
            generic,
            parameters,
        })
    }

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

    pub fn parse_import(&self) -> Option<Statement> {
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
        Some(Statement::ImportStatement {
            token: token.cloned(),
            args,
        })
    }

    pub fn parse_parameters(&self) -> Option<ParamaterList> {
        self.parse_enclosed_punctuation_list(
            Operator::OpenParen,
            Operator::Comma,
            Operator::CloseParen,
            || self.parse_parameter().map(|f| (f, true)),
        )
    }

    pub fn parse_parameter(&self) -> Option<Param> {
        let ty = Box::new(fallback!(self, self.parse_type())?);
        let ident = fallback!(self, self.expect_ident());

        let default = if let Some(Token::Operator(Operator::Equals)) = self.tokens.peek() {
            let equals = self.tokens.next().unwrap().clone();
            let expression = Box::new(self.parse_expression(None)?);

            Some((equals, expression))
        } else {
            None
        };

        Some(Param {
            ty,
            name: ident.cloned(),
            default,
        })
    }

    pub fn parse_arguments(&self) -> Option<ArgList> {
        self.parse_enclosed_punctuation_list(
            Operator::OpenParen,
            Operator::Comma,
            Operator::CloseParen,
            || self.parse_expression(None).map(|f| (f, true)),
        )
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

    pub fn parse_punctutation_list<T: AstNode + TreeDisplay>(
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

    pub fn parse_enclosed_list<T: AstNode + TreeDisplay>(
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

    pub fn parse_enclosed_punctuation_list<T: AstNode + TreeDisplay>(
        &self,
        open: Operator,
        punc: Operator,
        close: Operator,
        mut cb: impl FnMut() -> Option<(T, bool)>,
    ) -> Option<EnclosedPunctuationList<T>> {
        let open = self.expect_operator(open);

        let args = match self.tokens.peek_ignore_ws() {
            Some(Token::Operator(op)) if op == &close => PunctuationList::default(),
            _ => {
                let mut args = PunctuationList::default();

                while let Some((arg, valid)) = cb() {
                    if !valid {
                        return None;
                    }

                    let comma = if let (Operator::Newline, Some(Token::Newline)) =
                        (&punc, self.tokens.peek())
                    {
                        Some(self.tokens.next().unwrap().clone())
                    } else if let Some(Token::Operator(op)) = self.tokens.peek_ignore_ws() {
                        if op == &punc {
                            self.tokens.next().cloned()
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(Token::Operator(cl)) = self.tokens.peek_ignore_ws() {
                        if cl == &close {
                            args.push(arg, comma);
                            break;
                        }
                    }

                    if comma.is_none() {
                        println!("{:?}", self.tokens.peek());
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

        self.tokens.ignore_ws();
        let close = self.expect_operator(close);

        if let (Some(open), Some(close)) = (open, close) {
            Some(EnclosedPunctuationList {
                open: open.clone(),
                items: args,
                close: close.clone(),
            })
        } else {
            None
        }
    }

    pub fn parse_binding(&self) -> Option<Binding> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenParen)) => {
                Some(Binding::Tuple(self.parse_enclosed_punctuation_list(
                    Operator::OpenParen,
                    Operator::Comma,
                    Operator::CloseParen,
                    || self.parse_binding().map(|f| (f, true)),
                )?))
            }
            Some(Token::Ident(_)) => Some(Binding::Variable(self.tokens.next()?.clone())),
            Some(Token::Operator(Operator::Underscore)) => {
                Some(Binding::Ignore(self.tokens.next()?.clone()))
            }
            _ => None,
        }
    }

    pub fn parse_match_binding(&self) -> Option<MatchBinding> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenParen)) => {
                Some(MatchBinding::Tuple(self.parse_enclosed_punctuation_list(
                    Operator::OpenParen,
                    Operator::Comma,
                    Operator::CloseParen,
                    || self.parse_match_binding().map(|f| (f, true)),
                )?))
            }
            Some(Token::Ident(_)) => Some(MatchBinding::Variable(self.tokens.next()?.clone())),
            Some(Token::Operator(Operator::Underscore)) => {
                Some(MatchBinding::Ignore(self.tokens.next()?.clone()))
            }
            _ => self
                .parse_expression(None)
                .map(|expr| MatchBinding::Expression(Box::new(expr))),
        }
    }

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

    pub(crate) fn expect_ident(&self) -> Option<&SpannedToken> {
        self.ignore_ws();
        let Some(Token::Ident(_)) = self.tokens.peek() else {
            return None;
        };

        self.tokens.next()
    }
}
