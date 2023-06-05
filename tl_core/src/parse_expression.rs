use crate::{
    ast::{
        EnclosedList, Expression, KeyValue, ParsedTemplate, ParsedTemplateString, PunctuationList,
        Statement,
    },
    error::{ParseError, ParseErrorKind},
    lexer::Template,
    parser::Parser,
    token::{Operator, Range, SpannedToken, Token},
};

impl Parser {
    pub fn parse_expression(&self) -> Option<Expression> {
        match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenBrace)) => self.parse_struct_initializer(),
            _ => self.parse_operator_expression(0),
        }
    }

    pub fn parse_operator_expression(&self, last_prec: u32) -> Option<Expression> {
        let mut left = self.parse_primary_expression();

        while let Some(t) = self.tokens.peek() {
            left = match t {
                Token::Operator(o) => {
                    let prec = self.precedence_of_operator(o);
                    if prec <= last_prec || prec == 0 {
                        break;
                    }

                    match (o, left) {
                        (Operator::OpenParen, Some(expr)) => {
                            let (cl, skip) = self.parse_function_call(expr);
                            left = Some(cl);
                            if skip {
                                continue;
                            }
                        }
                        (_, l) => left = l,
                    };

                    let op_token = self.tokens.next().unwrap().clone();

                    let right = self.parse_operator_expression(prec);

                    Some(Expression::BinaryExpression {
                        left: left.map(Box::new),
                        right: right.map(Box::new),
                        op_token,
                    })
                }
                _ => break,
            }
        }

        left
    }

    pub fn parse_primary_expression(&self) -> Option<Expression> {
        if let Some(Token::Operator(Operator::OpenParen)) = self.tokens.peek() {
            // let state = self.save_state();
            // if let Some(func) = self.parse_function() {
            //     return Some(func);
            // }
            // state.restore(&self.tokens);

            let _open = self.tokens.next().unwrap();
            let expr = self.parse_operator_expression(0);
            let _close = self.tokens.next().unwrap(); // TODO: error

            expr
        } else {
            self.parse_literal()
        }
    }

    pub fn parse_struct_initializer(&self) -> Option<Expression> {
        let open = self.expect_operator(Operator::OpenBrace)?;
        let list = self.parse_list(|| {
            let name = match self.tokens.peek() {
                Some(Token::Ident(_)) => self.tokens.next(),
                _ => None,
            };

            let colon = self.expect_operator(Operator::Colon);

            let expr = self.parse_expression();

            if let (Some(name), Some(colon), Some(expr)) = (name, colon, expr) {
                return Some((
                    KeyValue {
                        name: Some(name.clone()),
                        colon: colon.clone(),
                        expr: Box::new(expr),
                    },
                    true,
                ));
            }

            None
        })?;
        let close = self.expect_operator(Operator::CloseBrace)?;

        Some(Expression::Record(EnclosedList {
            open: open.clone(),
            items: list,
            close: close.clone(),
        }))
    }

    pub fn parse_function_call(&self, expression: Expression) -> (Expression, bool) {
        let Some(args) = self.parse_arguments() else {
            return (expression, false);
        };

        (
            Expression::FunctionCall {
                expr: Box::new(expression),
                args,
            },
            true,
        )
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
                                let expr = parser.parse_operator_expression(0)?;

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
        match operator {
            Operator::Equals => 1, // Assignement

            Operator::Or => 10,
            Operator::And => 14,

            Operator::Plus | Operator::Minus => 20,

            Operator::Multiply
            | Operator::Divide
            | Operator::Pipe
            | Operator::Ampersand
            | Operator::Percent => 30,

            Operator::Exponent => 40,

            Operator::OpenParen => 50,

            Operator::Dot => 60,

            _ => 0, // TODO: error
        }
    }

    pub fn precedence_of_operator_for_ty(&self, operator: &Operator) -> u32 {
        match operator {
            Operator::Ampersand => 20,
            _ => 0, // TODO: error
        }
    }
}
