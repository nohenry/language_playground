use crate::{parser::Parser, ast::Type, restore, token::{Token, Operator}};

impl Parser {
    pub fn parse_type(&self) -> Option<Type> {
        let ty = self.parse_type_primary();

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
                let enclosed_list = self.parse_enclosed_list(
                    Operator::OpenSquare,
                    Operator::Comma,
                    Operator::CloseSquare,
                    || self.parse_type().map(|ty| (ty, true)),
                );
                enclosed_list.map(|el| Type::Array(el))
            }
            Some(Token::Operator(Operator::OpenBrace)) => {
                let enclosed_list = self.parse_enclosed_list(
                    Operator::OpenBrace,
                    Operator::Comma,
                    Operator::CloseBrace,
                    || self.parse_type().map(|ty| (ty, true)),
                );
                enclosed_list.map(|el| Type::Tuple(el))
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

        let ty_first = match self.tokens.peek() {
            Some(Token::Operator(Operator::Ampersand)) => Some(Type::Ref {
                ref_token: self.tokens.next().unwrap().clone(),
                base_type: ty_first.map(|ty| Box::new(ty)),
            }),
            Some(Token::Operator(Operator::Question)) => Some(Type::Option {
                question: self.tokens.next().unwrap().clone(),
                ty: ty_first.map(|ty| Box::new(ty)),
            }),
            Some(Token::Operator(Operator::OpenAngle)) => {
                let enclosed_list = self.parse_enclosed_list(
                    Operator::OpenAngle,
                    Operator::Comma,
                    Operator::CloseAngle,
                    || self.parse_type().map(|ty| (ty, true)),
                );
                enclosed_list.map(|el| Type::Generic {
                    base_type: ty_first.map(|ty| Box::new(ty)),
                    list: el,
                })
            }
            _ => ty_first,
        };

        ty_first
    }
}
