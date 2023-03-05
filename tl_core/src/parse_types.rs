use crate::{
    ast::{EnclosedList, EnclosedPunctuationList, GenericParameter, Type},
    parser::Parser,
    restore,
    token::{Operator, Token},
};

impl Parser {
    pub fn parse_struct(&self) -> Option<Type> {
        let open = self.expect_operator(Operator::OpenBrace);

        let list = self.parse_list(|| self.parse_parameter().map(|f| (f, true)));

        let close = self.expect_operator(Operator::CloseBrace);

        if let (Some(open), Some(list), Some(close)) = (open, list, close) {
            Some(Type::Struct(EnclosedList {
                open: open.clone(),
                items: list,
                close: close.clone(),
            }))
        } else {
            None
        }
    }

    pub fn parse_type(&self) -> Option<Type> {
        let mut ty = self.parse_type_primary();

        if let Some(Token::Operator(Operator::Pipe)) = self.tokens.peek() {
            let p = self.parse_punctutation_list(ty, Operator::Pipe, || {
                restore!(self, self.parse_type_primary().map(|ty| (ty, true)))
            });

            if let Some(p) = p {
                return Some(Type::Union(p));
            } else {
                return None;
            }
        }

        let ty = loop {
            ty = match self.tokens.peek() {
                Some(Token::Operator(Operator::At)) => Some(Type::Ref {
                    ref_token: self.tokens.next().unwrap().clone(),
                    base_type: ty.map(Box::new),
                }),
                Some(Token::Operator(Operator::Question)) => Some(Type::Option {
                    question: self.tokens.next().unwrap().clone(),
                    base_type: ty.map(Box::new),
                }),
                Some(Token::Operator(Operator::OpenAngle)) => {
                    let enclosed_list = self.parse_enclosed_punctuation_list(
                        Operator::OpenAngle,
                        Operator::Comma,
                        Operator::CloseAngle,
                        || self.parse_type().map(|ty| (ty, true)),
                    );
                    enclosed_list.map(|el| Type::Generic {
                        base_type: ty.map(Box::new),
                        list: el,
                    })
                }
                _ => break ty,
            };
        };

        ty
    }

    pub fn parse_type_primary(&self) -> Option<Type> {
        if let Some(Token::Operator(Operator::OpenParen)) = self.tokens.peek() {
            let _open = self.tokens.next().unwrap();
            let expr = self.parse_type();
            let _close = self.tokens.next().unwrap(); // TODO: error

            expr
        } else {
            self.parse_type_lit()
        }
    }

    pub fn parse_type_lit(&self) -> Option<Type> {
        let ty_first = match self.tokens.peek() {
            Some(Token::Operator(Operator::OpenSquare)) => {
                let enclosed_list = self.parse_enclosed_punctuation_list(
                    Operator::OpenSquare,
                    Operator::Comma,
                    Operator::CloseSquare,
                    || self.parse_type().map(|ty| (ty, true)),
                );
                enclosed_list.map(Type::Array)
            }
            Some(Token::Operator(Operator::OpenBrace)) => {
                let enclosed_list = self.parse_enclosed_punctuation_list(
                    Operator::OpenBrace,
                    Operator::Comma,
                    Operator::CloseBrace,
                    || self.parse_type().map(|ty| (ty, true)),
                );
                enclosed_list.map(Type::Tuple)
            }

            Some(Token::Ident(id)) => match id.as_str() {
                "i8" => Some(Type::Integer {
                    width: 8,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i16" => Some(Type::Integer {
                    width: 16,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i32" => Some(Type::Integer {
                    width: 32,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "i64" => Some(Type::Integer {
                    width: 64,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u8" => Some(Type::Integer {
                    width: 8,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u16" => Some(Type::Integer {
                    width: 16,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u32" => Some(Type::Integer {
                    width: 32,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "u64" => Some(Type::Integer {
                    width: 64,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "f32" => Some(Type::Float {
                    width: 32,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "f64" => Some(Type::Float {
                    width: 64,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "bool" => Some(Type::Boolean(self.tokens.next().unwrap().clone())),
                "char" => Some(Type::Char(self.tokens.next().unwrap().clone())),
                _ => Some(Type::Ident(self.tokens.next().unwrap().clone())),
            },
            Some(_) => {
                let expr = restore!(self, self.parse_literal());
                expr.map(|expr| Type::Expression(Box::new(expr)))
            }
            _ => None,
        };

        ty_first
    }

    pub fn parse_generic_parameters(&self) -> Option<EnclosedPunctuationList<GenericParameter>> {
        self.parse_enclosed_punctuation_list(
            Operator::OpenAngle,
            Operator::Comma,
            Operator::CloseAngle,
            || self.parse_generic_parameter().map(|gn| (gn, true)),
        )
    }

    pub fn parse_generic_parameter(&self) -> Option<GenericParameter> {
        let ident = match self.tokens.peek() {
            Some(Token::Ident(_)) => self.tokens.next().unwrap().clone(),
            _ => return None,
        };

        match self.tokens.peek() {
            Some(Token::Operator(Operator::Colon)) => {
                let colon = self.tokens.next().unwrap().clone();

                let first = if let Some(Token::Ident(_)) = self.tokens.peek() {
                    self.tokens.next().cloned()
                } else {
                    None
                };

                if let Some(Token::Operator(Operator::Ampersand)) = self.tokens.peek() {
                    let list = restore!(
                        self,
                        self.parse_punctutation_list(first, Operator::Ampersand, || {
                            if let Some(Token::Ident(_)) = self.tokens.peek() {
                                Some((self.tokens.next().unwrap().clone(), true))
                            } else {
                                None
                            }
                        })
                    );

                    if let Some(list) = list {
                        return Some(GenericParameter::Bounded {
                            ident,
                            colon,
                            bounds: list,
                        });
                    } else {
                        return None;
                    }
                }

                return first.map(GenericParameter::Unbounded);
            }
            _ => (),
        };

        Some(GenericParameter::Unbounded(ident))
    }
}
