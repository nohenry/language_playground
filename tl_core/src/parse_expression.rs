use crate::{
    ast::{
        ElseClause, EnclosedList, Expression, KeyValue, MatchBody, MatchEntry, ParsedTemplate,
        ParsedTemplateString, PunctuationList, Statement,
    },
    error::{ParseError, ParseErrorKind},
    lexer::Template,
    parser::Parser,
    restore,
    token::{Operator, Range, SpannedToken, Token},
};

impl Parser {
    pub fn parse_expression(&self, prec: Option<u32>) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenBrace)) => self.parse_block_expression(),
            Some(Token::Ident(ident)) if ident == "if" => self.parse_if_expression(),
            Some(Token::Ident(ident)) if ident == "for" => self.parse_for_loop(),
            Some(Token::Ident(ident)) if ident == "while" => self.parse_while_loop(),
            Some(Token::Ident(ident)) if ident == "match" => self.parse_match(),
            Some(Token::Operator(Operator::OpenParen)) => {
                restore!(self, self.parse_anon_function())
                    .or_else(|| self.parse_operator_expression(prec.unwrap_or_default()))
            }
            _ => self.parse_operator_expression(prec.unwrap_or_default()),
        }
    }

    pub fn parse_operator_expression(&self, last_prec: u32) -> Option<Expression> {
        let mut left = if let Some(Token::Operator(op)) = self.tokens.peek() {
            let prec = self.precedence_of_unary_operator(op);

            if prec > last_prec && prec != 0 {
                let op_token = self.tokens.next().cloned().unwrap();
                let expr = self.parse_expression(Some(prec))?;

                Some(Expression::UnaryExpression {
                    op_token,
                    expr: Some(Box::new(expr)),
                })
            } else {
                self.parse_primary_expression()
            }
        } else {
            self.parse_primary_expression()
        };

        while let Some(t) = self.tokens.peek() {
            left = match t {
                Token::Operator(o) => {
                    let prec = self.precedence_of_operator(o);
                    if prec <= last_prec || prec == 0 {
                        break;
                    }

                    let op_token = self.tokens.next().unwrap().clone();

                    /* Calling parse_operator_expression instead of parse_expression for cases like:
                     * if b { 6 } else { 9 }
                     *    ^^^^^^^ is this a class init or expression then block?
                     *
                     * if (Data { data: [0: 50] }) { 6 } else { 9 }
                     *    ^----------------------^ parens are needed for this case
                     *
                     * 2 + Data { data: [0: 50] }
                     *
                     * This will be an error and should be written as
                     *
                     * 2 + (Data {data: [0: 50]})
                     *
                     * An exception is assignment because you should be able to say:
                     *
                     * data = Data { ...data }
                     *
                     */
                    let right = match o {
                        Operator::Equals => self.parse_expression(None),
                        _ => self.parse_operator_expression(prec),
                    };

                    Some(Expression::BinaryExpression {
                        left: left.map(Box::new),
                        right: right.map(Box::new),
                        op_token,
                    })
                }
                // Token::Ident(ident) if ident != "else" && ident != "as" => {
                //     let function_name = self.tokens.next()?.clone();
                //     let right = self.parse_expression(None)?;

                //     Some(Expression::FunctionCall {
                //         expr: Box::new(Expression::Ident(function_name)),
                //         args: crate::ast::OneOf::B(Box::new(right)),
                //     })
                // }
                _ => break,
            }
        }

        // Postfix
        let left = match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenParen)) => self.parse_function_call(left?),
            Some(Token::Operator(Operator::OpenSquare)) => {
                let open = self.tokens.next().unwrap().clone();
                let expr = self.parse_operator_expression(0);
                let close = self.expect_operator(Operator::CloseSquare)?.clone();

                Some(Expression::Index {
                    expression: Box::new(left?),
                    open,
                    indexer: expr.map(Box::new),
                    close,
                })
            }
            _ => left,
        };

        left
    }

    pub fn parse_primary_expression(&self) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenParen)) => {
                let _open = self.tokens.next().unwrap();
                let expr = self.parse_expression(None);
                let _close = self.tokens.next().unwrap(); // TODO: error

                expr
            }
            Some(Token::Operator(Operator::OpenBrace)) => self.parse_struct_initializer(),
            Some(Token::Operator(Operator::OpenSquare)) => {
                let list = self.parse_enclosed_punctuation_list(
                    Operator::OpenSquare,
                    Operator::Comma,
                    Operator::CloseSquare,
                    || self.parse_expression(None).map(|f| (f, true)),
                )?;

                Some(Expression::Array(list))
            }
            _ => self.parse_literal(),
        }
    }

    pub fn parse_struct_initializer(&self) -> Option<Expression> {
        let struct_type = Box::new(self.parse_type()?);
        let list = self.parse_enclosed_punctuation_list(
            Operator::OpenBrace,
            Operator::Comma,
            Operator::CloseBrace,
            || {
                let name = self.expect_ident()?.clone();
                let colon = self.expect_operator(Operator::Colon)?.clone();
                let expr = Box::new(self.parse_expression(None)?);

                Some((KeyValue { name, colon, expr }, true))
            },
        )?;

        Some(Expression::ClassInitializer {
            struct_type,
            values: list,
        })
    }

    pub fn parse_anon_function(&self) -> Option<Expression> {
        let parameters = self.parse_parameters()?;
        let arrow = self.expect_operator(Operator::EqArrow)?.clone();
        let expression = Box::new(self.parse_expression(None)?);

        Some(Expression::AnonFunction {
            parameters,
            arrow,
            expression,
        })
    }

    pub fn parse_if_expression(&self) -> Option<Expression> {
        let if_token = self.tokens.next().unwrap().clone();
        let expression = Box::new(self.parse_operator_expression(0)?);

        let stmt_expression = self.parse_if_body(false)?;

        let else_clause = match self.tokens.peek_ignore_ws() {
            Some(Token::Ident(ident)) if ident == "elseif" => {
                let else_token = self.tokens.current_ignore_ws().unwrap().clone();
                let expression = self.parse_if_body(true)?;

                Some(ElseClause {
                    else_token,
                    body: Box::new(expression),
                })
            }
            Some(Token::Ident(ident)) if ident == "else" => {
                let else_token = self.tokens.next_ignore_ws().unwrap().clone();
                let expression = self.parse_if_body(false)?;

                Some(ElseClause {
                    else_token,
                    body: Box::new(expression),
                })
            }
            _ => None,
        };

        Some(Expression::If {
            if_token,
            expression,
            body: Box::new(stmt_expression),
            else_clause,
        })
    }

    pub fn parse_if_body(&self, elseif: bool) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenBrace)) => self.parse_block_expression(),
            _ => {
                self.add_error(ParseError {
                    kind: ParseErrorKind::InvalidSyntax("Expected {".into()),
                    range: self.tokens.current().unwrap().span().into(),
                });
                return None;
            }
        }
    }

    pub fn parse_for_loop(&self) -> Option<Expression> {
        let for_token = self.tokens.next().unwrap().clone();
        let binding = self.parse_binding()?;
        let in_token = self.expect_ident()?.clone();

        if in_token.as_str() != "in" {
            return None;
        }

        let expression = Box::new(self.parse_expression(None)?);
        let body = Box::new(self.parse_block()?);

        Some(Expression::ForLoop {
            for_token,
            binding,
            in_token,
            expression,
            body,
        })
    }

    pub fn parse_while_loop(&self) -> Option<Expression> {
        let while_token = self.tokens.next().unwrap().clone();

        let expression = Box::new(self.parse_operator_expression(0)?);
        let body = Box::new(self.parse_statement()?);

        Some(Expression::WhileLoop {
            while_token,
            expression,
            body,
        })
    }

    pub fn parse_match(&self) -> Option<Expression> {
        let match_token = self.tokens.next().unwrap().clone();
        let expression = Box::new(self.parse_operator_expression(0)?);
        let body = self.parse_match_body()?;

        Some(Expression::Match {
            match_token,
            expression,
            body,
        })
    }

    pub fn parse_match_body(&self) -> Option<MatchBody> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenBrace)) => {
                let body = self.parse_enclosed_punctuation_list(
                    Operator::OpenBrace,
                    Operator::Comma,
                    Operator::CloseBrace,
                    || self.parse_match_entry().map(|f| (f, true)),
                )?;

                Some(MatchBody::Patterns(body))
            }
            Some(Token::Ident(ident)) if ident == "as" => {
                let as_token = self.tokens.next().unwrap().clone();
                let binding = self.parse_match_binding()?;

                Some(MatchBody::AsBinding { as_token, binding })
            }
            _ => None,
        }
    }

    pub fn parse_match_entry(&self) -> Option<MatchEntry> {
        let binding = self.parse_match_binding()?;
        let arrow = self.expect_operator(Operator::EqArrow)?.clone();
        let expression = Box::new(self.parse_expression(None)?);

        Some(MatchEntry {
            binding,
            arrow,
            expression,
        })
    }

    pub fn parse_function_call(&self, expression: Expression) -> Option<Expression> {
        let args = self.parse_arguments()?;

        Some(Expression::FunctionCall {
            expr: Box::new(expression),
            args: crate::ast::OneOf::A(args),
        })
    }

    pub fn parse_function_body(
        &self,
    ) -> Option<(Option<SpannedToken>, PunctuationList<Statement>)> {
        let first_comma = self.expect_operator(Operator::Comma).cloned();

        let mut stmts = PunctuationList::default();

        while let Some(stmt) = self.parse_statement() {
            let comma = if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
                self.tokens.next().cloned()
            } else {
                None
            };
            if let Some(Token::Newline) = self.tokens.peek() {
                stmts.push(stmt, comma);
                break;
            }
            if comma.is_none() {
                self.add_error(ParseError {
                    kind: ParseErrorKind::InvalidSyntax(
                        "Expected comma in function body!".to_string(),
                    ),
                    range: Range::default(),
                });
                stmts.push(stmt, comma);
                return Some((first_comma, stmts));
            }
            stmts.push(stmt, comma);
        }

        Some((first_comma, stmts))
    }

    pub fn parse_literal(&self) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Integer(i)) => Some(Expression::Integer(
                *i,
                None,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Float(f)) => Some(Expression::Float(
                *f,
                None,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Ident(i)) if i == "true" => Some(Expression::Boolean(
                true,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Ident(i)) if i == "false" => Some(Expression::Boolean(
                false,
                self.tokens.next().unwrap().clone(),
            )),
            Some(Token::Ident(_)) => Some(Expression::Ident(self.tokens.next().unwrap().clone())),
            Some(Token::TemplateString(ts)) => {
                let tok = self.tokens.next().unwrap();
                let v: Vec<_> = ts
                    .iter()
                    .filter_map(|t| match t {
                        Template::String(s) => Some(ParsedTemplate::String(s.clone())),
                        Template::Template(t, o, c) => Some(ParsedTemplate::Template(
                            Box::new({
                                let parser = Parser::new(t.clone());
                                let expr = parser.parse_expression(None)?;

                                let mut errors = self.errors.write().unwrap();
                                errors.append(&mut parser.get_errors_mut());

                                expr
                            }),
                            o.clone(),
                            c.clone(),
                        )),
                    })
                    .collect();
                Some(Expression::String(ParsedTemplateString(v), tok.clone()))
            }
            _ => None,
        }
    }

    pub fn precedence_of_operator(&self, operator: &Operator) -> u32 {
        use Operator::*;
        match operator {
            Equals => 1, // Assignement
            PlusEqual | MinusEqual | MultiplyEqual | DivideEqual | AndEqual | OrEqual
            | AmpersandEqual | PipeEqual | CarotEqual | ExponentEqual | PercentEqual => 2,

            OpenAngle | CloseAngle | OpenAngleEqual | CloseAngleEqual => 5,

            Or => 10,
            And => 14,

            DoubleDot | DoubleDotEqual | Colon => 16,

            Plus | Minus => 20,

            Multiply | Divide | Pipe | Ampersand | Percent => 30,

            Exponent => 40,

            // OpenParen => 50,
            Dot => 60,

            _ => 0, // TODO: error
        }
    }

    pub fn precedence_of_unary_operator(&self, operator: &Operator) -> u32 {
        match operator {
            Operator::Plus | Operator::Minus => 10,
            Operator::Ampersand | Operator::Multiply => 45,
            Operator::TripleDot => 45,
            _ => 0, // TODO: error
        }
    }

    pub fn precedence_of_type_operator(&self, operator: &Operator) -> u32 {
        match operator {
            Operator::Ampersand => 20,
            _ => 0, // TODO: error
        }
    }
}
