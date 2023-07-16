use crate::{
    ast::{EnclosedPunctuationList, GenericParameter, Type},
    parser::Parser,
    restore,
    token::{Operator, Token},
};

impl Parser {
    pub fn parse_type(&self) -> Option<Type> {
        let mut ty = self.parse_type_primary();

        if let (Some(Token::Operator(Operator::Dot)), Some(Type::Ident(ident))) =
            (self.tokens.peek(), ty.clone())
        {
            let p = self.parse_punctutation_list(Some(ident), Operator::Dot, || {
                restore!(self, self.tokens.next().map(|ty| (ty.clone(), true)))
            });

            ty = if let Some(p) = p {
                Some(Type::Path(p))
            } else {
                None
            };
        }

        if let Some(Token::Operator(Operator::Pipe)) = self.tokens.peek() {
            let p = self.parse_punctutation_list(ty, Operator::Pipe, || {
                restore!(self, self.parse_type_primary().map(|ty| (ty, true)))
            });

            ty = if let Some(p) = p {
                Some(Type::Union(p))
            } else {
                None
            };
        }

        let ty = loop {
            ty = match self.tokens.peek() {
                Some(Token::Operator(Operator::Ampersand)) => Some(Type::Ref {
                    ref_token: self.tokens.next().unwrap().clone(),
                    mutable: false,
                    base_type: ty.map(Box::new),
                }),
                Some(Token::Operator(Operator::Multiply)) => Some(Type::Ref {
                    ref_token: self.tokens.next().unwrap().clone(),
                    mutable: true,
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
                Some(Token::Operator(Operator::OpenSquare)) => {
                    let open = self.tokens.next()?.clone();
                    let size = match self.tokens.peek() {
                        Some(Token::Operator(Operator::CloseSquare)) => None,
                        _ => self.parse_expression(None),
                    };
                    let close = self.expect_operator(Operator::CloseSquare)?.clone();

                    Some(Type::Array {
                        base_type: Box::new(ty?),
                        open,
                        size: size.map(Box::new),
                        close,
                    })
                }
                Some(Token::Operator(Operator::OpenParen)) => {
                    let parameters = self.parse_parameters()?;

                    Some(Type::Function {
                        parameters,
                        return_type: Box::new(ty?),
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
                "let" => Some(Type::Let(self.tokens.next()?.clone())),
                "none" => Some(Type::None(self.tokens.next()?.clone())),
                "int" => Some(Type::Integer {
                    width: 32,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "int8" => Some(Type::Integer {
                    width: 8,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "int16" => Some(Type::Integer {
                    width: 16,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "int32" => Some(Type::Integer {
                    width: 32,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "int64" => Some(Type::Integer {
                    width: 64,
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "iptr" => Some(Type::IntegerPointer {
                    signed: true,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uint" => Some(Type::Integer {
                    width: 32,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uint8" => Some(Type::Integer {
                    width: 8,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uint16" => Some(Type::Integer {
                    width: 16,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uint32" => Some(Type::Integer {
                    width: 32,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uint64" => Some(Type::Integer {
                    width: 64,
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "uptr" => Some(Type::IntegerPointer {
                    signed: false,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "float32" | "float" => Some(Type::Float {
                    width: 32,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "float64" => Some(Type::Float {
                    width: 64,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "float128" => Some(Type::Float {
                    width: 128,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "fixed32" | "fixed" => Some(Type::Fixed {
                    width: 32,
                    decimals: 16,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "fixed64" => Some(Type::Fixed {
                    width: 64,
                    decimals: 32,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "fixed128" => Some(Type::Fixed {
                    width: 64,
                    decimals: 64,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "bool" => Some(Type::Boolean(self.tokens.next().unwrap().clone())),
                "char" => Some(Type::Char {
                    width: 32,
                    token: self.tokens.next().unwrap().clone(),
                }),
                "char8" => Some(Type::Char {
                    width: 8,
                    token: self.tokens.next().unwrap().clone(),
                }),
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
