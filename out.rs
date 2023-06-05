{} {}
0 1 "arrow, expression : Some(expression),"
0 0 "arrow,"
{} {Type::Path { qself: None, path: Path { leading_colon: None, segments: [PathSegment { ident: Ident { ident: "SpannedToken", span: #0 bytes(48159..48171) }, arguments: PathArguments::None }] } }}
0 3 "ty, expr : Some(expr),"
0 2 "ty, eq,"
0 3 "token, body,"
0 1 "token : Some(token), args,"
1 1 "args,"
0 4 "ty_tok, ty,"
0 5 "return_type, body : Some(body),"
0 4 "return_type, arrow : Some(arrow),"
0 3 "return_type, parameters,"
0 3 "impl_tok, body : Some(body),"
0 2 "impl_tok, ty : Some(ty),"
0 1 "impl_tok, generics : Some(generics),"
0 0 "impl_tok,"
0 1 "ret_token, expr : Some(expr),"
0 0 "ret_token,"
#![feature(prelude_import)]
#![feature(trait_upcasting)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(round_char_boundary)]
#![feature(trace_macros)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use ast::{EnclosedList, Expression, Param, ParamaterList, Statement, Type};
use lexer::Lexer;
use linked_hash_map::LinkedHashMap;
use log::{Log, SetLoggerError};
use parser::Parser;
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};
pub mod ast {
    use core::fmt;
    use std::fmt::Debug;
    use tl_util::{
        format::{AsTree, NodeDisplay, TreeDisplay},
        macros::{AstNode, FormatNode},
    };
    use crate::token::{Range, SpannedToken, Token, Unit};
    pub trait AstNode: TreeDisplay {
        fn get_range(&self) -> Range;
    }
    impl<T: AstNode> AstNode for Vec<T> {
        fn get_range(&self) -> Range {
            if let (Some(first), Some(last)) = (self.first(), self.last()) {
                Range::from((&first.get_range(), &last.get_range()))
            } else {
                Range::default()
            }
        }
    }
    impl AstNode for SpannedToken {
        fn get_range(&self) -> Range {
            self.0.into()
        }
    }
    pub struct PunctuationList<T: AstNode> {
        tokens: Vec<(T, Option<SpannedToken>)>,
    }
    #[automatically_derived]
    impl<T: ::core::clone::Clone + AstNode> ::core::clone::Clone for PunctuationList<T> {
        #[inline]
        fn clone(&self) -> PunctuationList<T> {
            PunctuationList {
                tokens: ::core::clone::Clone::clone(&self.tokens),
            }
        }
    }
    impl<T: AstNode> Default for PunctuationList<T> {
        fn default() -> Self {
            Self {
                tokens: Vec::default(),
            }
        }
    }
    impl<T: AstNode> PunctuationList<T> {
        pub fn push(&mut self, val: T, separator: Option<SpannedToken>) {
            self.tokens.push((val, separator))
        }
        pub fn push_sep(&mut self, val: T, separator: SpannedToken) {
            self.tokens.push((val, Some(separator)))
        }
        pub fn push_term(&mut self, val: T) {
            self.tokens.push((val, None))
        }
        pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
            self.tokens.iter().map(|(v, _)| v)
        }
        pub fn iter(&self) -> impl Iterator<Item = &(T, Option<SpannedToken>)> + '_ {
            self.tokens.iter()
        }
        pub fn take(self) -> Vec<(T, Option<SpannedToken>)> {
            self.tokens
        }
        pub fn len(&self) -> usize {
            self.tokens.len()
        }
    }
    impl<T> AstNode for PunctuationList<T>
    where
        T: AstNode,
    {
        fn get_range(&self) -> Range {
            match (self.iter().next(), self.iter().last()) {
                (Some((_, Some(f))), Some((_, Some(l)))) => Range::from((*f.span(), *l.span())),
                (Some((_, Some(f))), Some((l, _))) => Range::from((*f.span(), &l.get_range())),
                (Some((f, _)), Some((_, Some(l)))) => Range::from((&f.get_range(), *l.span())),
                (Some((f, _)), Some((l, _))) => Range::from((&f.get_range(), &l.get_range())),
                _ => Range::default(),
            }
        }
    }
    impl<T> NodeDisplay for PunctuationList<T>
    where
        T: NodeDisplay + AstNode,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Punctuation List")?;
            f.write_fmt(::core::fmt::Arguments::new_v1(
                &[" "],
                &[::core::fmt::ArgumentV1::new_display(&self.tokens.len())],
            ))
        }
    }
    impl<T> TreeDisplay for PunctuationList<T>
    where
        T: TreeDisplay + AstNode,
    {
        fn num_children(&self) -> usize {
            if let Some((_, Some(_))) = self.tokens.last() {
                self.tokens.len() * 2
            } else if !self.tokens.is_empty() {
                self.tokens.len() * 2 - 1
            } else {
                0
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            let p = &self.tokens[index / 2];
            if index % 2 == 0 {
                Some(&p.0)
            } else {
                Some(p.1.as_ref().unwrap())
            }
        }
    }
    impl<T: PartialEq + AstNode> PartialEq for PunctuationList<T> {
        fn eq(&self, other: &Self) -> bool {
            for (a, b) in self.iter_items().zip(other.iter_items()) {
                if a != b {
                    return false;
                }
            }
            true
        }
    }
    pub struct ParamaterList {
        pub range: Range,
        pub items: PunctuationList<Param>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParamaterList {
        #[inline]
        fn clone(&self) -> ParamaterList {
            ParamaterList {
                range: ::core::clone::Clone::clone(&self.range),
                items: ::core::clone::Clone::clone(&self.items),
            }
        }
    }
    impl AstNode for ParamaterList {
        fn get_range(&self) -> Range {
            self.range
        }
    }
    impl ParamaterList {
        pub fn iter_items(&self) -> impl Iterator<Item = &Param> + '_ {
            self.items.iter_items()
        }
    }
    impl NodeDisplay for ParamaterList {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Element Parameters")
        }
    }
    impl TreeDisplay for ParamaterList {
        fn num_children(&self) -> usize {
            2
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match index {
                0 => Some(&self.range),
                1 => Some(&self.items),
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
    impl PartialEq for ParamaterList {
        fn eq(&self, other: &Self) -> bool {
            for (a, b) in self.iter_items().zip(other.iter_items()) {
                if a != b {
                    return false;
                }
            }
            true
        }
    }
    pub struct KeyValue {
        pub name: Option<SpannedToken>,
        pub colon: Option<SpannedToken>,
        pub expr: Box<Expression>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for KeyValue {
        #[inline]
        fn clone(&self) -> KeyValue {
            KeyValue {
                name: ::core::clone::Clone::clone(&self.name),
                colon: ::core::clone::Clone::clone(&self.colon),
                expr: ::core::clone::Clone::clone(&self.expr),
            }
        }
    }
    impl AstNode for KeyValue {
        fn get_range(&self) -> Range {
            match (&self.name, &self.expr) {
                (Some(name), expr) => Range::from((*name.span(), &expr.get_range())),
                (None, expr) => expr.get_range(),
            }
        }
    }
    impl KeyValue {
        pub fn name(&self) -> &String {
            match &self.name {
                Some(SpannedToken(_, Token::Ident(s))) => s,
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
    impl NodeDisplay for KeyValue {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("KeyValue")
        }
    }
    impl TreeDisplay for KeyValue {
        fn num_children(&self) -> usize {
            {
                (if let Some(_) = self.name { 1 } else { 0 })
                    + (if let Some(_) = Some(true) { 1 } else { 0 })
                    + 0
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            {
                let mut ind = 0;
                if let Some(v) = &self.name {
                    if index == ind {
                        return Some(v);
                    }
                    ind += 1;
                }
                if let Some(v) = Some(&*self.expr) {
                    if index == ind {
                        return Some(v);
                    }
                    ind += 1;
                }
                ind
            };
            None
        }
    }
    pub struct Param {
        pub ty: Option<Type>,
        pub name: Option<SpannedToken>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Param {
        #[inline]
        fn clone(&self) -> Param {
            Param {
                ty: ::core::clone::Clone::clone(&self.ty),
                name: ::core::clone::Clone::clone(&self.name),
            }
        }
    }
    impl PartialEq for Param {
        fn eq(&self, other: &Self) -> bool {
            self.ty == other.ty
        }
    }
    impl AstNode for Param {
        fn get_range(&self) -> Range {
            match (&self.name, &self.ty) {
                (Some(name), None) => Range::from(*name.span()),
                (Some(name), Some(value)) => Range::from((name, &value.get_range())),
                _ => Range::default(),
            }
        }
    }
    impl Param {
        pub fn name(&self) -> &String {
            match &self.name {
                Some(SpannedToken(_, Token::Ident(s))) => s,
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
    impl NodeDisplay for Param {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Parameter")
        }
    }
    impl TreeDisplay for Param {
        fn num_children(&self) -> usize {
            {
                (if let Some(_) = self.ty { 1 } else { 0 })
                    + (if let Some(_) = self.name { 1 } else { 0 })
                    + 0
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            {
                let mut ind = 0;
                if let Some(v) = &self.ty {
                    if index == ind {
                        return Some(v);
                    }
                    ind += 1;
                }
                if let Some(v) = &self.name {
                    if index == ind {
                        return Some(v);
                    }
                    ind += 1;
                }
                ind
            };
            None
        }
    }
    pub struct ArgList {
        pub range: Range,
        pub items: PunctuationList<Expression>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ArgList {
        #[inline]
        fn clone(&self) -> ArgList {
            ArgList {
                range: ::core::clone::Clone::clone(&self.range),
                items: ::core::clone::Clone::clone(&self.items),
            }
        }
    }
    impl AstNode for ArgList {
        fn get_range(&self) -> Range {
            self.range
        }
    }
    impl ArgList {
        pub fn iter_items(&self) -> impl Iterator<Item = &Expression> + '_ {
            self.items.iter_items()
        }
        pub fn len(&self) -> usize {
            self.items.len()
        }
    }
    impl NodeDisplay for ArgList {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Arg Parameters")
        }
    }
    impl TreeDisplay for ArgList {
        fn num_children(&self) -> usize {
            2
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match index {
                0 => Some(&self.range),
                1 => Some(&self.items),
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
    pub struct EnclosedPunctuationList<T: AstNode> {
        pub open: SpannedToken,
        pub items: PunctuationList<T>,
        pub close: SpannedToken,
    }
    #[automatically_derived]
    impl<T: ::core::clone::Clone + AstNode> ::core::clone::Clone for EnclosedPunctuationList<T> {
        #[inline]
        fn clone(&self) -> EnclosedPunctuationList<T> {
            EnclosedPunctuationList {
                open: ::core::clone::Clone::clone(&self.open),
                items: ::core::clone::Clone::clone(&self.items),
                close: ::core::clone::Clone::clone(&self.close),
            }
        }
    }
    impl<T: AstNode> AstNode for EnclosedPunctuationList<T> {
        fn get_range(&self) -> Range {
            Range::from((&self.open, &self.close))
        }
    }
    impl<T: AstNode> EnclosedPunctuationList<T> {
        pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
            self.items.iter_items()
        }
    }
    impl<T: AstNode> NodeDisplay for EnclosedPunctuationList<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Enclosed Punctuation List")
        }
    }
    impl<T: AstNode> TreeDisplay for EnclosedPunctuationList<T> {
        fn num_children(&self) -> usize {
            1
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match index {
                0 => Some(&self.items),
                _ => None,
            }
        }
    }
    impl<T: PartialEq + AstNode> PartialEq for EnclosedPunctuationList<T> {
        fn eq(&self, other: &Self) -> bool {
            for (a, b) in self.iter_items().zip(other.iter_items()) {
                if a != b {
                    return false;
                }
            }
            true
        }
    }
    pub struct EnclosedList<T: AstNode> {
        pub open: SpannedToken,
        pub items: Vec<T>,
        pub close: SpannedToken,
    }
    #[automatically_derived]
    impl<T: ::core::clone::Clone + AstNode> ::core::clone::Clone for EnclosedList<T> {
        #[inline]
        fn clone(&self) -> EnclosedList<T> {
            EnclosedList {
                open: ::core::clone::Clone::clone(&self.open),
                items: ::core::clone::Clone::clone(&self.items),
                close: ::core::clone::Clone::clone(&self.close),
            }
        }
    }
    impl<T: AstNode> AstNode for EnclosedList<T> {
        fn get_range(&self) -> Range {
            Range::from((&self.open, &self.close))
        }
    }
    impl<T: AstNode> EnclosedList<T> {
        pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
            self.items.iter()
        }
    }
    impl<T: AstNode> NodeDisplay for EnclosedList<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Enclosed List")
        }
    }
    impl<T: AstNode> TreeDisplay for EnclosedList<T> {
        fn num_children(&self) -> usize {
            self.items.num_children()
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            self.items.child_at(index)
        }
    }
    pub enum Type {
        Integer {
            width: u8,
            signed: bool,
            token: SpannedToken,
        },
        Float {
            width: u8,
            token: SpannedToken,
        },
        Fixed {
            width: u8,
            decimals: u8,
            token: SpannedToken,
        },
        Boolean(SpannedToken),
        Char {
            width: u8,
            token: SpannedToken,
        },
        Ident(SpannedToken),
        Ref {
            ref_token: SpannedToken,
            mutable: bool,
            base_type: Option<Box<Type>>,
        },
        Array(EnclosedPunctuationList<Type>),
        Union(PunctuationList<Type>),
        Tuple(EnclosedPunctuationList<Type>),
        Generic {
            base_type: Option<Box<Type>>,
            list: EnclosedPunctuationList<Type>,
        },
        Expression(Box<Expression>),
        Function {
            parameters: ParamaterList,
            return_type: Option<(SpannedToken, Box<Type>)>,
        },
        Option {
            base_type: Option<Box<Type>>,
            question: SpannedToken,
        },
        Result {
            error: SpannedToken,
            base_type: Option<Box<Type>>,
        },
        Struct(EnclosedList<Param>),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Type {
        #[inline]
        fn clone(&self) -> Type {
            match self {
                Type::Integer {
                    width: __self_0,
                    signed: __self_1,
                    token: __self_2,
                } => Type::Integer {
                    width: ::core::clone::Clone::clone(__self_0),
                    signed: ::core::clone::Clone::clone(__self_1),
                    token: ::core::clone::Clone::clone(__self_2),
                },
                Type::Float {
                    width: __self_0,
                    token: __self_1,
                } => Type::Float {
                    width: ::core::clone::Clone::clone(__self_0),
                    token: ::core::clone::Clone::clone(__self_1),
                },
                Type::Fixed {
                    width: __self_0,
                    decimals: __self_1,
                    token: __self_2,
                } => Type::Fixed {
                    width: ::core::clone::Clone::clone(__self_0),
                    decimals: ::core::clone::Clone::clone(__self_1),
                    token: ::core::clone::Clone::clone(__self_2),
                },
                Type::Boolean(__self_0) => Type::Boolean(::core::clone::Clone::clone(__self_0)),
                Type::Char {
                    width: __self_0,
                    token: __self_1,
                } => Type::Char {
                    width: ::core::clone::Clone::clone(__self_0),
                    token: ::core::clone::Clone::clone(__self_1),
                },
                Type::Ident(__self_0) => Type::Ident(::core::clone::Clone::clone(__self_0)),
                Type::Ref {
                    ref_token: __self_0,
                    mutable: __self_1,
                    base_type: __self_2,
                } => Type::Ref {
                    ref_token: ::core::clone::Clone::clone(__self_0),
                    mutable: ::core::clone::Clone::clone(__self_1),
                    base_type: ::core::clone::Clone::clone(__self_2),
                },
                Type::Array(__self_0) => Type::Array(::core::clone::Clone::clone(__self_0)),
                Type::Union(__self_0) => Type::Union(::core::clone::Clone::clone(__self_0)),
                Type::Tuple(__self_0) => Type::Tuple(::core::clone::Clone::clone(__self_0)),
                Type::Generic {
                    base_type: __self_0,
                    list: __self_1,
                } => Type::Generic {
                    base_type: ::core::clone::Clone::clone(__self_0),
                    list: ::core::clone::Clone::clone(__self_1),
                },
                Type::Expression(__self_0) => {
                    Type::Expression(::core::clone::Clone::clone(__self_0))
                }
                Type::Function {
                    parameters: __self_0,
                    return_type: __self_1,
                } => Type::Function {
                    parameters: ::core::clone::Clone::clone(__self_0),
                    return_type: ::core::clone::Clone::clone(__self_1),
                },
                Type::Option {
                    base_type: __self_0,
                    question: __self_1,
                } => Type::Option {
                    base_type: ::core::clone::Clone::clone(__self_0),
                    question: ::core::clone::Clone::clone(__self_1),
                },
                Type::Result {
                    error: __self_0,
                    base_type: __self_1,
                } => Type::Result {
                    error: ::core::clone::Clone::clone(__self_0),
                    base_type: ::core::clone::Clone::clone(__self_1),
                },
                Type::Struct(__self_0) => Type::Struct(::core::clone::Clone::clone(__self_0)),
            }
        }
    }
    impl PartialEq for Type {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (
                    Self::Integer {
                        width: l_width,
                        signed: l_signed,
                        token: _l_token,
                    },
                    Self::Integer {
                        width: r_width,
                        signed: r_signed,
                        token: _r_token,
                    },
                ) => l_width == r_width && l_signed == r_signed,
                (
                    Self::Float {
                        width: l_width,
                        token: _l_token,
                    },
                    Self::Float {
                        width: r_width,
                        token: _r_token,
                    },
                ) => l_width == r_width,
                (Self::Boolean(_l0), Self::Boolean(_r0)) => true,
                (
                    Self::Char {
                        width: _l_width,
                        token: _l_token,
                    },
                    Self::Char {
                        width: _r_width,
                        token: _r_token,
                    },
                ) => _r_width == _l_width,
                (Self::Ident(l0), Self::Ident(r0)) => l0.as_str() == r0.as_str(),
                (
                    Self::Ref {
                        ref_token: _l_ref_token,
                        mutable: _l_mutable,
                        base_type: Some(l_base_type),
                    },
                    Self::Ref {
                        ref_token: _r_ref_token,
                        mutable: _r_mutable,
                        base_type: Some(r_base_type),
                    },
                ) => _l_mutable == _r_mutable && l_base_type == r_base_type,
                (Self::Array(l0), Self::Array(r0)) => l0 == r0,
                (Self::Union(l0), Self::Union(r0)) => l0 == r0,
                (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
                (
                    Self::Generic {
                        base_type: Some(btl0),
                        list: l0,
                    },
                    Self::Generic {
                        base_type: Some(btr0),
                        list: r0,
                    },
                ) => l0 == r0 && btl0 == btr0,
                (
                    Self::Function {
                        parameters: l_parameters,
                        return_type: Some(l_return_type),
                    },
                    Self::Function {
                        parameters: r_parameters,
                        return_type: Some(r_return_type),
                    },
                ) => l_parameters == r_parameters && l_return_type.1 == r_return_type.1,
                (
                    Self::Function {
                        parameters: l_parameters,
                        return_type: None,
                    },
                    Self::Function {
                        parameters: r_parameters,
                        return_type: None,
                    },
                ) => l_parameters == r_parameters,
                (
                    Self::Option {
                        base_type: Some(l_ty),
                        question: _l_question,
                    },
                    Self::Option {
                        base_type: Some(r_ty),
                        question: _r_question,
                    },
                ) => l_ty == r_ty,
                (
                    Self::Result {
                        error: _l_error,
                        base_type: Some(l_ty),
                    },
                    Self::Result {
                        error: _r_error,
                        base_type: Some(r_ty),
                    },
                ) => l_ty == r_ty,
                _ => false,
            }
        }
    }
    impl AstNode for Type {
        fn get_range(&self) -> Range {
            match self {
                Self::Integer { token, .. } => token.span().into(),
                Self::Float { token, .. } => token.span().into(),
                Self::Fixed { token, .. } => token.span().into(),
                Self::Boolean(tok) => tok.span().into(),
                Self::Char { token, .. } => token.span().into(),
                Self::Ident(ident) => ident.span().into(),
                Self::Array(a) => a.get_range(),
                Self::Union(a) => a.get_range(),
                Self::Tuple(a) => a.get_range(),
                Self::Generic {
                    base_type: Some(base_type),
                    list,
                } => Range::from((&base_type.get_range(), &list.get_range())),
                Self::Generic { list, .. } => list.get_range(),
                Self::Expression(a) => a.get_range(),
                Self::Function {
                    parameters,
                    return_type: None,
                } => parameters.get_range(),
                Self::Function {
                    parameters,
                    return_type: Some((_, ty)),
                } => Range::from((&parameters.get_range(), &ty.get_range())),
                Self::Option {
                    base_type: Some(ty),
                    question,
                } => Range::from((&ty.get_range(), &question.get_range())),
                Self::Option { question, .. } => question.get_range(),
                Self::Result {
                    base_type: Some(ty),
                    error,
                } => Range::from((&error.get_range(), &ty.get_range())),
                Self::Result { error, .. } => error.get_range(),
                Self::Ref {
                    ref_token,
                    base_type: Some(base_type),
                    ..
                } => Range::from((&base_type.get_range(), &ref_token.get_range())),
                Self::Ref { ref_token, .. } => ref_token.get_range(),
                Self::Struct(s) => s.get_range(),
            }
        }
    }
    impl NodeDisplay for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                Self::Float { width, .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["float"],
                    &[::core::fmt::ArgumentV1::new_display(&width)],
                )),
                Self::Fixed {
                    width, decimals, ..
                } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["fixed", "<", ">"],
                    &[
                        ::core::fmt::ArgumentV1::new_display(&width),
                        ::core::fmt::ArgumentV1::new_display(&decimals),
                    ],
                )),
                Self::Integer {
                    width,
                    signed: true,
                    ..
                } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["int"],
                    &[::core::fmt::ArgumentV1::new_display(&width)],
                )),
                Self::Integer {
                    width,
                    signed: false,
                    ..
                } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["uint"],
                    &[::core::fmt::ArgumentV1::new_display(&width)],
                )),
                Self::Boolean(_) => f.write_str("bool"),
                Self::Char { width, .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["char"],
                    &[::core::fmt::ArgumentV1::new_display(&width)],
                )),
                Self::Ident(ident) => f.write_str(ident.as_str()),
                Self::Array(_a) => f.write_str("Array"),
                Self::Union(_u) => f.write_str("Union"),
                Self::Tuple(_a) => f.write_str("Tuple"),
                Self::Generic { .. } => f.write_str("Generic"),
                Self::Expression(_e) => f.write_str("Expression"),
                Self::Function { .. } => f.write_str("Function"),
                Self::Option {
                    base_type: _ty,
                    question: _,
                } => f.write_str("Optional"),
                Self::Result {
                    base_type: _ty,
                    error: _,
                } => f.write_str("Result"),
                Self::Ref {
                    ref_token: _,
                    mutable,
                    base_type: _,
                } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["Reference "],
                    &[::core::fmt::ArgumentV1::new_display(&if *mutable {
                        "mut"
                    } else {
                        ""
                    })],
                )),
                Self::Struct(_) => f.write_str("Struct"),
            }
        }
    }
    impl fmt::Debug for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            <Type as NodeDisplay>::fmt(self, f)
        }
    }
    impl TreeDisplay for Type {
        fn num_children(&self) -> usize {
            match self {
                Type::Array(a) => a.items.num_children(),
                Type::Union(a) => a.num_children(),
                Type::Tuple(a) => a.items.num_children(),
                Type::Generic {
                    base_type: Some(_), ..
                } => 2,
                Type::Generic { .. } => 1,
                Type::Expression(_e) => 1,
                Type::Option { .. } => 1,
                Type::Result { .. } => 1,
                Type::Ref { .. } => 1,
                Type::Struct(s) => s.num_children(),
                _ => 0,
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match self {
                Type::Array(a) => a.items.child_at(index),
                Type::Union(a) => a.child_at(index),
                Type::Tuple(a) => a.items.child_at(index),
                Type::Generic {
                    base_type: Some(base_type),
                    list,
                } => match index {
                    0 => Some(&**base_type),
                    1 => Some(list),
                    _ => None,
                },
                Type::Generic { list, .. } => Some(list),
                Type::Expression(e) => Some(&**e),
                Type::Option { base_type: ty, .. } => {
                    ty.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f)
                }
                Type::Result { base_type: ty, .. } => {
                    ty.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f)
                }
                Type::Ref { base_type, .. } => {
                    base_type.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f)
                }
                Type::Struct(s) => s.child_at(index),
                _ => None,
            }
        }
    }
    pub enum GenericParameter {
        Unbounded(SpannedToken),
        Bounded {
            ident: SpannedToken,
            colon: SpannedToken,
            bounds: PunctuationList<SpannedToken>,
        },
    }
    #[automatically_derived]
    impl ::core::clone::Clone for GenericParameter {
        #[inline]
        fn clone(&self) -> GenericParameter {
            match self {
                GenericParameter::Unbounded(__self_0) => {
                    GenericParameter::Unbounded(::core::clone::Clone::clone(__self_0))
                }
                GenericParameter::Bounded {
                    ident: __self_0,
                    colon: __self_1,
                    bounds: __self_2,
                } => GenericParameter::Bounded {
                    ident: ::core::clone::Clone::clone(__self_0),
                    colon: ::core::clone::Clone::clone(__self_1),
                    bounds: ::core::clone::Clone::clone(__self_2),
                },
            }
        }
    }
    impl AstNode for GenericParameter {
        fn get_range(&self) -> Range {
            match self {
                GenericParameter::Unbounded(u) => u.get_range(),
                GenericParameter::Bounded { ident, bounds, .. } => {
                    Range::from((*ident.span(), &bounds.get_range()))
                }
            }
        }
    }
    impl NodeDisplay for GenericParameter {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                GenericParameter::Unbounded(u) => f.write_str(u.as_str()),
                GenericParameter::Bounded { ident, .. } => f.write_str(ident.as_str()),
            }
        }
    }
    impl TreeDisplay for GenericParameter {
        fn num_children(&self) -> usize {
            match self {
                GenericParameter::Bounded { bounds, .. } => bounds.num_children(),
                _ => 0,
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
            match self {
                GenericParameter::Bounded { bounds, .. } => bounds.child_at(index),
                _ => None,
            }
        }
    }
    pub enum ParsedTemplate {
        String(SpannedToken),
        Template(Box<Expression>, SpannedToken, SpannedToken),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParsedTemplate {
        #[inline]
        fn clone(&self) -> ParsedTemplate {
            match self {
                ParsedTemplate::String(__self_0) => {
                    ParsedTemplate::String(::core::clone::Clone::clone(__self_0))
                }
                ParsedTemplate::Template(__self_0, __self_1, __self_2) => ParsedTemplate::Template(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                    ::core::clone::Clone::clone(__self_2),
                ),
            }
        }
    }
    impl NodeDisplay for ParsedTemplate {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                ParsedTemplate::String(l) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["Literal: `", "`"],
                    &[::core::fmt::ArgumentV1::new_display(&l.as_str())],
                )),
                ParsedTemplate::Template(_t, _, _) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Value"], &[]))
                }
            }
        }
    }
    impl TreeDisplay for ParsedTemplate {
        fn num_children(&self) -> usize {
            match self {
                ParsedTemplate::Template(_, _, _) => 1,
                _ => 0,
            }
        }
        fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay<()>> {
            match self {
                ParsedTemplate::Template(e, _, _) => Some(&**e),
                _ => None,
            }
        }
    }
    pub struct ParsedTemplateString(pub Vec<ParsedTemplate>);
    #[automatically_derived]
    impl ::core::clone::Clone for ParsedTemplateString {
        #[inline]
        fn clone(&self) -> ParsedTemplateString {
            ParsedTemplateString(::core::clone::Clone::clone(&self.0))
        }
    }
    impl NodeDisplay for ParsedTemplateString {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Parsed Template String")
        }
    }
    impl TreeDisplay for ParsedTemplateString {
        fn num_children(&self) -> usize {
            self.0.len()
        }
        fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay<()>> {
            self.0.get(_index).map::<&dyn TreeDisplay<()>, _>(|t| t)
        }
    }
    pub enum Expression {
        BinaryExpression {
            left: Option<Box<Expression>>,
            right: Option<Box<Expression>>,
            op_token: Option<SpannedToken>,
        },
        Boolean(bool, SpannedToken),
        Integer(u64, Option<Unit>, SpannedToken),
        Float(f64, Option<Unit>, SpannedToken),
        Ident(SpannedToken),
        String(ParsedTemplateString, SpannedToken),
        FunctionCall {
            expr: Box<Expression>,
            args: ArgList,
        },
        Tuple(Vec<Expression>),
        Array {
            values: PunctuationList<Expression>,
            range: Range,
        },
        Record(EnclosedList<KeyValue>),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Expression {
        #[inline]
        fn clone(&self) -> Expression {
            match self {
                Expression::BinaryExpression {
                    left: __self_0,
                    right: __self_1,
                    op_token: __self_2,
                } => Expression::BinaryExpression {
                    left: ::core::clone::Clone::clone(__self_0),
                    right: ::core::clone::Clone::clone(__self_1),
                    op_token: ::core::clone::Clone::clone(__self_2),
                },
                Expression::Boolean(__self_0, __self_1) => Expression::Boolean(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                ),
                Expression::Integer(__self_0, __self_1, __self_2) => Expression::Integer(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                    ::core::clone::Clone::clone(__self_2),
                ),
                Expression::Float(__self_0, __self_1, __self_2) => Expression::Float(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                    ::core::clone::Clone::clone(__self_2),
                ),
                Expression::Ident(__self_0) => {
                    Expression::Ident(::core::clone::Clone::clone(__self_0))
                }
                Expression::String(__self_0, __self_1) => Expression::String(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                ),
                Expression::FunctionCall {
                    expr: __self_0,
                    args: __self_1,
                } => Expression::FunctionCall {
                    expr: ::core::clone::Clone::clone(__self_0),
                    args: ::core::clone::Clone::clone(__self_1),
                },
                Expression::Tuple(__self_0) => {
                    Expression::Tuple(::core::clone::Clone::clone(__self_0))
                }
                Expression::Array {
                    values: __self_0,
                    range: __self_1,
                } => Expression::Array {
                    values: ::core::clone::Clone::clone(__self_0),
                    range: ::core::clone::Clone::clone(__self_1),
                },
                Expression::Record(__self_0) => {
                    Expression::Record(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl Expression {
        pub fn as_function(&self) -> Option<(&Expression, &ArgList)> {
            match self {
                Expression::FunctionCall { expr, args } => Some((expr, args)),
                _ => None,
            }
        }
    }
    impl AstNode for Expression {
        fn get_range(&self) -> Range {
            match self {
                Self::Record(parameters) => parameters.get_range(),
                Self::Boolean(_, b) => b.get_range(),
                Self::BinaryExpression {
                    left: Some(left),
                    right: Some(right),
                    ..
                } => Range::from((&left.get_range(), &right.get_range())),
                Self::BinaryExpression {
                    left: Some(left),
                    op_token: Some(op),
                    ..
                } => Range::from((&left.get_range(), *op.span())),
                Self::BinaryExpression {
                    op_token: Some(op),
                    right: Some(right),
                    ..
                } => Range::from((*op.span(), &right.get_range())),
                Self::BinaryExpression {
                    op_token: Some(op), ..
                } => Range::from(*op.span()),
                Self::Tuple(s) => match (s.first(), s.last()) {
                    (Some(s), Some(e)) => Range::from((&s.get_range(), &e.get_range())),
                    _ => Range::default(),
                },
                Self::Array { range, .. } => *range,
                Self::Integer(_, _, s) => s.0.into(),
                Self::Float(_, _, s) => s.0.into(),
                Self::Ident(s) => s.0.into(),
                Self::FunctionCall { expr, args } => {
                    Range::from((&expr.get_range(), &args.get_range()))
                }
                _ => Range::default(),
            }
        }
    }
    impl NodeDisplay for Expression {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                Self::Record { .. } => f.write_str("Record"),
                Self::Boolean(true, _) => f.write_str("Boolean - True"),
                Self::Boolean(false, _) => f.write_str("Boolean - False"),
                Self::BinaryExpression {
                    op_token: Some(SpannedToken(_, Token::Operator(op))),
                    ..
                } => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["BinExp "],
                    &[::core::fmt::ArgumentV1::new_display(&op.as_str())],
                )),
                Self::BinaryExpression { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["BinExp"], &[]))
                }
                Self::Integer(i, Some(u), _) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["", ""],
                    &[
                        ::core::fmt::ArgumentV1::new_display(&i),
                        ::core::fmt::ArgumentV1::new_display(&u),
                    ],
                )),
                Self::Float(i, Some(u), _) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["", ""],
                    &[
                        ::core::fmt::ArgumentV1::new_display(&i),
                        ::core::fmt::ArgumentV1::new_display(&u),
                    ],
                )),
                Self::Integer(i, None, _) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&i)],
                )),
                Self::Float(i, None, _) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&i)],
                )),
                Self::Ident(SpannedToken(_, Token::Ident(i))) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(
                        &[""],
                        &[::core::fmt::ArgumentV1::new_display(&i)],
                    ))
                }
                Self::String(_, _) => f.write_str("TemplateString"),
                Self::FunctionCall { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["FunctionCall"], &[]))
                }
                Self::Array { .. } => f.write_str("Array"),
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
    impl fmt::Debug for Expression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            <Expression as NodeDisplay>::fmt(self, f)
        }
    }
    impl TreeDisplay for Expression {
        fn num_children(&self) -> usize {
            match self {
                Self::Record { .. } => 1,
                Self::FunctionCall { .. } => 2,
                Self::Array { values, .. } => values.num_children(),
                Self::BinaryExpression {
                    left: Some(_),
                    right: Some(_),
                    ..
                } => 2,
                Self::BinaryExpression { left: Some(_), .. } => 1,
                Self::BinaryExpression { right: Some(_), .. } => 1,
                Self::String(p, _) => p.num_children(),
                _ => 0,
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match self {
                Self::Record(parameters) => Some(parameters),
                Self::FunctionCall { expr, args, .. } => match index {
                    0 => Some(&**expr),
                    1 => Some(args),
                    _ => None,
                },
                Self::Array { values, .. } => values.child_at(index),
                Self::BinaryExpression {
                    left: Some(l),
                    right: Some(r),
                    ..
                } => match index {
                    0 => Some(&**l),
                    1 => Some(&**r),
                    _ => None,
                },
                Self::BinaryExpression { left: Some(l), .. } => Some(&**l),
                Self::BinaryExpression { right: Some(r), .. } => Some(&**r),
                Self::String(t, _) => t.child_at(index),
                _ => None,
            }
        }
    }
    pub enum FunctionBody {
        Block(SpannedToken, SpannedToken, Option<SpannedToken>),
        Expression {
            arrow: SpannedToken,
            expression: Option<Box<Expression>>,
        },
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FunctionBody {
        #[inline]
        fn clone(&self) -> FunctionBody {
            match self {
                FunctionBody::Block(__self_0, __self_1, __self_2) => FunctionBody::Block(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                    ::core::clone::Clone::clone(__self_2),
                ),
                FunctionBody::Expression {
                    arrow: __self_0,
                    expression: __self_1,
                } => FunctionBody::Expression {
                    arrow: ::core::clone::Clone::clone(__self_0),
                    expression: ::core::clone::Clone::clone(__self_1),
                },
            }
        }
    }
    impl NodeDisplay for FunctionBody {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                FunctionBody::Block(..) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Block"], &[]))
                }
                FunctionBody::Expression { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Expression"], &[]))
                }
                _ => Ok(()),
            }
        }
    }
    impl TreeDisplay for FunctionBody {
        fn num_children(&self) -> usize {
            match self {
                FunctionBody::Block(_a0, _a1, _a2) => {
                    0 + 1 + 1 + { (if let Some(_) = _a2 { 1 } else { 0 }) + 0 }
                }
                FunctionBody::Expression { arrow, expression } => {
                    0 + 1 + { (if let Some(_) = expression { 1 } else { 0 }) + 0 }
                }
                _ => 0,
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match self {
                FunctionBody::Block(_a0, _a1, _a2) => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(_a0) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(_a1) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = _a2 {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                FunctionBody::Expression { arrow, expression } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(arrow) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = expression.map_tree() {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                _ => None,
            }
        }
        fn child_at_bx<'b>(&'b self, index: usize) -> Box<dyn TreeDisplay + 'b> {
            match self {
                _ => ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(
                    &["Unexpected index for enum!"],
                    &[],
                )),
            }
        }
    }
    impl AstNode for FunctionBody {
        fn get_range(&self) -> Range {
            match self {
                FunctionBody::Block(_a0, .., Some(_a2)) => {
                    Range::from((&_a0.get_range(), &_a2.get_range()))
                }
                FunctionBody::Block(_a0, .., _a1, _) => {
                    Range::from((&_a0.get_range(), &_a1.get_range()))
                }
                FunctionBody::Expression {
                    arrow,
                    expression: Some(expression),
                    ..
                } => Range::from((&arrow.get_range(), &expression.get_range())),
                FunctionBody::Expression { arrow, .. } => {
                    Range::from((&arrow.get_range(), &arrow.get_range()))
                }
            }
        }
    }
    # [ignore_all (type = SpannedToken)]
    pub enum Statement {
        Expression(Expression),
        Declaration {
            ty: Type,
            #[keep_item]
            ident: SpannedToken,
            eq: SpannedToken,
            expr: Option<Expression>,
        },
        Class {
            token: SpannedToken,
            ident: SpannedToken,
            generic: Option<EnclosedPunctuationList<GenericParameter>>,
            body: EnclosedList<Statement>,
        },
        ImportStatement {
            token: Option<SpannedToken>,
            args: PunctuationList<SpannedToken>,
        },
        List(PunctuationList<Statement>),
        Block(EnclosedList<Statement>),
        TypeAlias {
            ty_tok: SpannedToken,
            ident: SpannedToken,
            generic: Option<EnclosedPunctuationList<GenericParameter>>,
            eq: Option<SpannedToken>,
            ty: Box<Type>,
        },
        Function {
            return_type: Type,
            ident: SpannedToken,
            generic: Option<EnclosedPunctuationList<GenericParameter>>,
            parameters: ParamaterList,
            arrow: Option<SpannedToken>,
            body: Option<Box<Statement>>,
        },
        Impl {
            impl_tok: SpannedToken,
            generics: Option<EnclosedPunctuationList<GenericParameter>>,
            ty: Option<Type>,
            body: Option<EnclosedList<Statement>>,
        },
        Return {
            ret_token: SpannedToken,
            expr: Option<Expression>,
        },
        Modifer(ModiferStatement),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Statement {
        #[inline]
        fn clone(&self) -> Statement {
            match self {
                Statement::Expression(__self_0) => {
                    Statement::Expression(::core::clone::Clone::clone(__self_0))
                }
                Statement::Declaration {
                    ty: __self_0,
                    ident: __self_1,
                    eq: __self_2,
                    expr: __self_3,
                } => Statement::Declaration {
                    ty: ::core::clone::Clone::clone(__self_0),
                    ident: ::core::clone::Clone::clone(__self_1),
                    eq: ::core::clone::Clone::clone(__self_2),
                    expr: ::core::clone::Clone::clone(__self_3),
                },
                Statement::Class {
                    token: __self_0,
                    ident: __self_1,
                    generic: __self_2,
                    body: __self_3,
                } => Statement::Class {
                    token: ::core::clone::Clone::clone(__self_0),
                    ident: ::core::clone::Clone::clone(__self_1),
                    generic: ::core::clone::Clone::clone(__self_2),
                    body: ::core::clone::Clone::clone(__self_3),
                },
                Statement::ImportStatement {
                    token: __self_0,
                    args: __self_1,
                } => Statement::ImportStatement {
                    token: ::core::clone::Clone::clone(__self_0),
                    args: ::core::clone::Clone::clone(__self_1),
                },
                Statement::List(__self_0) => Statement::List(::core::clone::Clone::clone(__self_0)),
                Statement::Block(__self_0) => {
                    Statement::Block(::core::clone::Clone::clone(__self_0))
                }
                Statement::TypeAlias {
                    ty_tok: __self_0,
                    ident: __self_1,
                    generic: __self_2,
                    eq: __self_3,
                    ty: __self_4,
                } => Statement::TypeAlias {
                    ty_tok: ::core::clone::Clone::clone(__self_0),
                    ident: ::core::clone::Clone::clone(__self_1),
                    generic: ::core::clone::Clone::clone(__self_2),
                    eq: ::core::clone::Clone::clone(__self_3),
                    ty: ::core::clone::Clone::clone(__self_4),
                },
                Statement::Function {
                    return_type: __self_0,
                    ident: __self_1,
                    generic: __self_2,
                    parameters: __self_3,
                    arrow: __self_4,
                    body: __self_5,
                } => Statement::Function {
                    return_type: ::core::clone::Clone::clone(__self_0),
                    ident: ::core::clone::Clone::clone(__self_1),
                    generic: ::core::clone::Clone::clone(__self_2),
                    parameters: ::core::clone::Clone::clone(__self_3),
                    arrow: ::core::clone::Clone::clone(__self_4),
                    body: ::core::clone::Clone::clone(__self_5),
                },
                Statement::Impl {
                    impl_tok: __self_0,
                    generics: __self_1,
                    ty: __self_2,
                    body: __self_3,
                } => Statement::Impl {
                    impl_tok: ::core::clone::Clone::clone(__self_0),
                    generics: ::core::clone::Clone::clone(__self_1),
                    ty: ::core::clone::Clone::clone(__self_2),
                    body: ::core::clone::Clone::clone(__self_3),
                },
                Statement::Return {
                    ret_token: __self_0,
                    expr: __self_1,
                } => Statement::Return {
                    ret_token: ::core::clone::Clone::clone(__self_0),
                    expr: ::core::clone::Clone::clone(__self_1),
                },
                Statement::Modifer(__self_0) => {
                    Statement::Modifer(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl NodeDisplay for Statement {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                Statement::Expression(..) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Expression"], &[]))
                }
                Statement::Declaration { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Declaration"], &[]))
                }
                Statement::Class { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Class"], &[]))
                }
                Statement::ImportStatement { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["ImportStatement"], &[]))
                }
                Statement::List(..) => f.write_fmt(::core::fmt::Arguments::new_v1(&["List"], &[])),
                Statement::Block(..) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Block"], &[]))
                }
                Statement::TypeAlias { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["TypeAlias"], &[]))
                }
                Statement::Function { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Function"], &[]))
                }
                Statement::Impl { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Impl"], &[]))
                }
                Statement::Return { .. } => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Return"], &[]))
                }
                Statement::Modifer(..) => {
                    f.write_fmt(::core::fmt::Arguments::new_v1(&["Modifer"], &[]))
                }
                _ => Ok(()),
            }
        }
    }
    impl TreeDisplay for Statement {
        fn num_children(&self) -> usize {
            match self {
                Statement::Expression(_a0) => 0 + 1,
                Statement::Declaration {
                    ty,
                    ident,
                    eq,
                    expr,
                } => 0 + 1 + 1 + { (if let Some(_) = expr { 1 } else { 0 }) + 0 },
                Statement::Class {
                    token,
                    ident,
                    generic,
                    body,
                } => 0 + { (if let Some(_) = generic { 1 } else { 0 }) + 0 } + 1,
                Statement::ImportStatement { token, args } => {
                    0 + { (if let Some(_) = token { 1 } else { 0 }) + 0 } + 1
                }
                Statement::List(_a0) => 0 + 1,
                Statement::Block(_a0) => 0 + 1,
                Statement::TypeAlias {
                    ty_tok,
                    ident,
                    generic,
                    eq,
                    ty,
                } => {
                    0 + { (if let Some(_) = generic { 1 } else { 0 }) + 0 }
                        + { (if let Some(_) = eq { 1 } else { 0 }) + 0 }
                        + 1
                }
                Statement::Function {
                    return_type,
                    ident,
                    generic,
                    parameters,
                    arrow,
                    body,
                } => {
                    0 + 1
                        + { (if let Some(_) = generic { 1 } else { 0 }) + 0 }
                        + 1
                        + { (if let Some(_) = arrow { 1 } else { 0 }) + 0 }
                        + { (if let Some(_) = body { 1 } else { 0 }) + 0 }
                }
                Statement::Impl {
                    impl_tok,
                    generics,
                    ty,
                    body,
                } => {
                    0 + { (if let Some(_) = generics { 1 } else { 0 }) + 0 }
                        + { (if let Some(_) = ty { 1 } else { 0 }) + 0 }
                        + { (if let Some(_) = body { 1 } else { 0 }) + 0 }
                }
                Statement::Return { ret_token, expr } => {
                    0 + { (if let Some(_) = expr { 1 } else { 0 }) + 0 }
                }
                Statement::Modifer(_a0) => 0 + 1,
                _ => 0,
            }
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match self {
                Statement::Expression(_a0) => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(_a0) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Declaration {
                    ty,
                    ident,
                    eq,
                    expr,
                } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(ty) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(ident) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = expr {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Class {
                    token,
                    ident,
                    generic,
                    body,
                } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = generic {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(body) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::ImportStatement { token, args } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = token {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(args) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::List(_a0) => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(_a0) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Block(_a0) => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(_a0) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::TypeAlias {
                    ty_tok,
                    ident,
                    generic,
                    eq,
                    ty,
                } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = generic {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = eq {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(&**ty) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Function {
                    return_type,
                    ident,
                    generic,
                    parameters,
                    arrow,
                    body,
                } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(return_type) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = generic {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = Some(parameters) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = arrow {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = body.map_tree() {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Impl {
                    impl_tok,
                    generics,
                    ty,
                    body,
                } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = generics {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = ty {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        if let Some(v) = body {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Return { ret_token, expr } => {
                    {
                        let mut ind = 0;
                        if let Some(v) = expr {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                Statement::Modifer(_a0) => {
                    {
                        let mut ind = 0;
                        if let Some(v) = Some(_a0) {
                            if index == ind {
                                return Some(v);
                            }
                            ind += 1;
                        }
                        ind
                    };
                    None
                }
                _ => None,
            }
        }
        fn child_at_bx<'b>(&'b self, index: usize) -> Box<dyn TreeDisplay + 'b> {
            match self {
                _ => ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(
                    &["Unexpected index for enum!"],
                    &[],
                )),
            }
        }
    }
    impl AstNode for Statement {
        fn get_range(&self) -> Range {
            match self {
                Statement::Expression(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
                Statement::Declaration {
                    ty,
                    expr: Some(expr),
                    ..
                } => Range::from((&ty.get_range(), &expr.get_range())),
                Statement::Declaration { ty, eq, .. } => {
                    Range::from((&ty.get_range(), &eq.get_range()))
                }
                Statement::Class { token, body, .. } => {
                    Range::from((&token.get_range(), &body.get_range()))
                }
                Statement::ImportStatement {
                    token: Some(token),
                    args,
                    ..
                } => Range::from((&token.get_range(), &args.get_range())),
                Statement::ImportStatement { args, .. } => {
                    Range::from((&args.get_range(), &args.get_range()))
                }
                Statement::List(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
                Statement::Block(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
                Statement::TypeAlias { ty_tok, ty, .. } => {
                    Range::from((&ty_tok.get_range(), &ty.get_range()))
                }
                Statement::Function {
                    return_type,
                    body: Some(body),
                    ..
                } => Range::from((&return_type.get_range(), &body.get_range())),
                Statement::Function {
                    return_type,
                    arrow: Some(arrow),
                    ..
                } => Range::from((&return_type.get_range(), &arrow.get_range())),
                Statement::Function {
                    return_type,
                    parameters,
                    ..
                } => Range::from((&return_type.get_range(), &parameters.get_range())),
                Statement::Impl {
                    impl_tok,
                    body: Some(body),
                    ..
                } => Range::from((&impl_tok.get_range(), &body.get_range())),
                Statement::Impl {
                    impl_tok,
                    ty: Some(ty),
                    ..
                } => Range::from((&impl_tok.get_range(), &ty.get_range())),
                Statement::Impl {
                    impl_tok,
                    generics: Some(generics),
                    ..
                } => Range::from((&impl_tok.get_range(), &generics.get_range())),
                Statement::Impl { impl_tok, .. } => {
                    Range::from((&impl_tok.get_range(), &impl_tok.get_range()))
                }
                Statement::Return {
                    ret_token,
                    expr: Some(expr),
                    ..
                } => Range::from((&ret_token.get_range(), &expr.get_range())),
                Statement::Return { ret_token, .. } => {
                    Range::from((&ret_token.get_range(), &ret_token.get_range()))
                }
                Statement::Modifer(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
            }
        }
    }
    impl Statement {
        pub fn get_last(&self) -> &Statement {
            match self {
                Statement::Block(list) => list.items.last().unwrap(),
                stmt => stmt,
            }
        }
        pub fn is_return(&self) -> bool {
            match self {
                Statement::Return { .. } => true,
                _ => false,
            }
        }
    }
    pub enum Modifer {
        Public,
        Protected,
        Unique,
        Const,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Modifer {
        #[inline]
        fn clone(&self) -> Modifer {
            match self {
                Modifer::Public => Modifer::Public,
                Modifer::Protected => Modifer::Protected,
                Modifer::Unique => Modifer::Unique,
                Modifer::Const => Modifer::Const,
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Modifer {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Modifer::Public => ::core::fmt::Formatter::write_str(f, "Public"),
                Modifer::Protected => ::core::fmt::Formatter::write_str(f, "Protected"),
                Modifer::Unique => ::core::fmt::Formatter::write_str(f, "Unique"),
                Modifer::Const => ::core::fmt::Formatter::write_str(f, "Const"),
            }
        }
    }
    impl NodeDisplay for Modifer {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }
    impl TreeDisplay for Modifer {
        fn num_children(&self) -> usize {
            0
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
            None
        }
    }
    pub struct ModiferStatement {
        pub modifier: Modifer,
        pub modifier_token: SpannedToken,
        pub statement: Option<Box<Statement>>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ModiferStatement {
        #[inline]
        fn clone(&self) -> ModiferStatement {
            ModiferStatement {
                modifier: ::core::clone::Clone::clone(&self.modifier),
                modifier_token: ::core::clone::Clone::clone(&self.modifier_token),
                statement: ::core::clone::Clone::clone(&self.statement),
            }
        }
    }
    impl AstNode for ModiferStatement {
        fn get_range(&self) -> Range {
            if let Some(stmt) = &self.statement {
                Range::from((*self.modifier_token.span(), &stmt.get_range()))
            } else {
                self.modifier_token.get_range()
            }
        }
    }
    impl NodeDisplay for ModiferStatement {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Modifier")
        }
    }
    impl TreeDisplay for ModiferStatement {
        fn num_children(&self) -> usize {
            2
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
            match index {
                0 => Some(&self.modifier),
                1 => self.statement.map_tree(),
                _ => None,
            }
        }
    }
}
pub mod error {
    use std::{error::Error, fmt::Display};
    use crate::token::Range;
    pub struct ParseError {
        pub kind: ParseErrorKind,
        pub range: Range,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParseError {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ParseError",
                "kind",
                &&self.kind,
                "range",
                &&self.range,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParseError {
        #[inline]
        fn clone(&self) -> ParseError {
            ParseError {
                kind: ::core::clone::Clone::clone(&self.kind),
                range: ::core::clone::Clone::clone(&self.range),
            }
        }
    }
    impl Display for ParseError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.kind.fmt(f)
        }
    }
    impl Error for ParseError {}
    pub enum ParseErrorKind {
        InvalidSyntax(String),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParseErrorKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParseErrorKind::InvalidSyntax(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "InvalidSyntax", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParseErrorKind {
        #[inline]
        fn clone(&self) -> ParseErrorKind {
            match self {
                ParseErrorKind::InvalidSyntax(__self_0) => {
                    ParseErrorKind::InvalidSyntax(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl Display for ParseErrorKind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::InvalidSyntax(s) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["Invalid Syntax: "],
                    &[::core::fmt::ArgumentV1::new_display(&s)],
                )),
            }
        }
    }
}
pub mod lexer {
    use crate::token::{Operator, Span, SpannedToken, Token};
    pub struct Lexer {}
    impl Lexer {
        pub fn lex(&self, input: &str) -> Vec<SpannedToken> {
            let mut start_index = 0;
            let mut end_index = 1;
            let mut line_num = 0;
            let mut position = 0;
            let mut tokens: Vec<SpannedToken> = Vec::new();
            let mut string = false;
            let mut line_comment = false;
            let mut block_comment = false;
            let input_len = input.len();
            while start_index < input_len && end_index <= input_len {
                let sub_str = &input[start_index..(input.ceil_char_boundary(end_index))];
                let next = if string {
                    None
                } else {
                    if end_index + 1 <= input.len() {
                        let end = input.ceil_char_boundary(end_index + 1);
                        if end <= input.len() {
                            input[input.floor_char_boundary(end_index)..end]
                                .chars()
                                .next()
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                };
                if let (Some(token), lex_length) = self.try_lex(sub_str, next, string) {
                    match token {
                        Token::Operator(Operator::BlockCommentClose) if block_comment => {
                            block_comment = false
                        }
                        Token::Newline if line_comment => {
                            if end_index + 1 < input_len
                                && &input[start_index..end_index + 1] == "\r\n"
                            {
                                start_index += 1;
                            }
                            line_num += 1;
                            position = 0;
                            line_comment = false
                        }
                        _ if block_comment || line_comment => (),
                        Token::Operator(Operator::BlockCommentOpen) => block_comment = true,
                        Token::Operator(Operator::LineComment) => line_comment = true,
                        Token::Operator(Operator::Quote) => string = true,
                        Token::String => {
                            let tok = self.parse_template(
                                &sub_str[0..sub_str.len() - 1],
                                line_num,
                                position,
                                tokens.len() as _,
                            );
                            let token = SpannedToken::new(tok.1, tok.0);
                            position += token.0.length;
                            tokens.push(token);
                            string = false
                        }
                        Token::Whitespace => position += 1,
                        Token::Newline => {
                            if end_index + 1 < input_len
                                && &input[start_index..end_index + 1] == "\r\n"
                            {
                                start_index += 1;
                            }
                            line_num += 1;
                            position = 0;
                        }
                        Token::Ident(_) => {
                            let token = SpannedToken::new(
                                token,
                                Span {
                                    line_num,
                                    position,
                                    length: (end_index - start_index) as u32,
                                    token_index: tokens.len() as u32,
                                },
                            );
                            tokens.push(token);
                            position += (end_index - start_index) as u32;
                        }
                        token => {
                            let length = lex_length.unwrap_or(end_index - start_index) as u32;
                            let token = SpannedToken::new(
                                token,
                                Span {
                                    line_num,
                                    position,
                                    length,
                                    token_index: tokens.len() as u32,
                                },
                            );
                            tokens.push(token);
                            position += length;
                            start_index += length as usize;
                            end_index = start_index + 1;
                            continue;
                        }
                    }
                    start_index += sub_str.len();
                    end_index = start_index + 1;
                } else {
                    end_index += (sub_str.len() - sub_str.chars().count()) + 1;
                }
            }
            tokens.push(SpannedToken::new(
                Token::Newline,
                Span {
                    line_num,
                    position,
                    length: 1,
                    token_index: tokens.len() as u32,
                },
            ));
            tokens
        }
        pub fn try_lex(
            &self,
            input: &str,
            next: Option<char>,
            string: bool,
        ) -> (Option<Token>, Option<usize>) {
            if string {
                if let Some('\'') = input.chars().last() {
                    return (Some(Token::String), None);
                }
                return (None, None);
            }
            if input.len() == 1 {
                match input.chars().next() {
                    Some('[') => return (Some(Token::Operator(Operator::OpenSquare)), None),
                    Some(']') => return (Some(Token::Operator(Operator::CloseSquare)), None),
                    Some('(') => return (Some(Token::Operator(Operator::OpenParen)), None),
                    Some(')') => return (Some(Token::Operator(Operator::CloseParen)), None),
                    Some('{') => return (Some(Token::Operator(Operator::OpenBrace)), None),
                    Some('}') => return (Some(Token::Operator(Operator::CloseBrace)), None),
                    Some('<') => return (Some(Token::Operator(Operator::OpenAngle)), None),
                    Some('>') => return (Some(Token::Operator(Operator::CloseAngle)), None),
                    Some(':') => return (Some(Token::Operator(Operator::Colon)), None),
                    Some('.') => match next {
                        Some('.') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Dot)), None),
                    },
                    Some(',') => return (Some(Token::Operator(Operator::Comma)), None),
                    Some('+') => return (Some(Token::Operator(Operator::Plus)), None),
                    Some('-') => match next {
                        Some('>') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Minus)), None),
                    },
                    Some('*') => match next {
                        Some('*') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Multiply)), None),
                    },
                    Some('|') => match next {
                        Some('|') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Pipe)), None),
                    },
                    Some('&') => match next {
                        Some('&') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Ampersand)), None),
                    },
                    Some('/') => match next {
                        Some('/') => return (None, None),
                        _ => return (Some(Token::Operator(Operator::Divide)), None),
                    },
                    Some('!') => return (Some(Token::Operator(Operator::Exclamation)), None),
                    Some('@') => return (Some(Token::Operator(Operator::At)), None),
                    Some('#') => return (Some(Token::Operator(Operator::Pound)), None),
                    Some('$') => return (Some(Token::Operator(Operator::Dollar)), None),
                    Some('%') => return (Some(Token::Operator(Operator::Percent)), None),
                    Some('^') => return (Some(Token::Operator(Operator::Carot)), None),
                    Some(';') => return (Some(Token::Operator(Operator::SemiColon)), None),
                    Some('~') => return (Some(Token::Operator(Operator::Tilde)), None),
                    Some('`') => return (Some(Token::Operator(Operator::BackTick)), None),
                    Some('\'') => return (Some(Token::Operator(Operator::Quote)), None),
                    Some('?') => return (Some(Token::Operator(Operator::Question)), None),
                    Some('=') => return (Some(Token::Operator(Operator::Equals)), None),
                    Some('\r' | '\n') => return (Some(Token::Newline), None),
                    Some(c) if c.is_whitespace() => return (Some(Token::Whitespace), None),
                    _ => (),
                }
            }
            let mut chars = input.chars();
            match (chars.next(), chars.next(), chars.next()) {
                (Some('*'), Some('*'), _) => {
                    return (Some(Token::Operator(Operator::Exponent)), None)
                }
                (Some('|'), Some('|'), _) => return (Some(Token::Operator(Operator::Or)), None),
                (Some('&'), Some('&'), _) => return (Some(Token::Operator(Operator::And)), None),
                (Some('-'), Some('>'), _) => return (Some(Token::Operator(Operator::Arrow)), None),
                (Some('/'), Some('/'), _) => {
                    return (Some(Token::Operator(Operator::LineComment)), None)
                }
                (Some('/'), Some('*'), _) => {
                    return (Some(Token::Operator(Operator::BlockCommentOpen)), None)
                }
                (Some('*'), Some('/'), _) => {
                    return (Some(Token::Operator(Operator::BlockCommentClose)), None)
                }
                (Some('.'), Some('.'), Some('.')) => {
                    return (Some(Token::Operator(Operator::TripleDot)), None)
                }
                (Some('.'), Some('.'), Some('=')) => {
                    return (Some(Token::Operator(Operator::TripleDot)), None)
                }
                (Some('.'), Some('.'), _) => match next {
                    Some('.' | '=') => return (None, None),
                    _ => return (Some(Token::Operator(Operator::DoubleDot)), None),
                },
                _ => (),
            }
            let del = next.map(|c| !(c.is_numeric() || c == '.')).unwrap_or(true);
            let count = input
                .chars()
                .fold(0u8, |acc, c| if c == '.' { 1 + acc } else { acc });
            if count == 1 && next == Some('.') {
                let val = input[..input.len() - 1].parse().unwrap_or(0.0f64);
                return (Some(Token::Float(val)), None);
            }
            if !input.chars().any(|c| !(c.is_numeric() || c == '.')) && count <= 1 && del {
                if count == 1 {
                    let val = input.parse().unwrap_or(0.0f64);
                    return (Some(Token::Float(val)), None);
                } else {
                    let val = input.parse().unwrap_or(0u64);
                    return (Some(Token::Integer(val)), None);
                }
            }
            let del = next
                .map(|c| !(c.is_alphanumeric() || c == '_'))
                .unwrap_or(true);
            if input
                .chars()
                .next()
                .filter(|f| f.is_alphabetic() || *f == '_')
                .is_some()
                & !input.chars().any(|c| !(c.is_alphanumeric() || c == '_'))
                && del
            {
                return (Some(Token::Ident(input.to_string())), None);
            }
            (None, None)
        }
        pub fn parse_template(
            &self,
            input: &str,
            line: u32,
            mut position: u32,
            token_index: u32,
        ) -> (Span, Token) {
            let mut iindex = 0;
            let mut last_index = 0;
            let mut last_span = Span::default();
            let mut open = 0;
            let mut toks = Vec::new();
            let sindex = position + 1;
            for i in 0..input.chars().count() {
                let c = input.chars().nth(i).unwrap();
                if c == '}' {
                    open -= 1;
                }
                if c == '{' && open == 0 {
                    let st = input[last_index..i].to_string();
                    let len = st.len();
                    last_span = Span {
                        line_num: line,
                        position: position + len as u32,
                        length: 1,
                        token_index,
                    };
                    let token = SpannedToken::new(
                        Token::Ident(st),
                        Span {
                            line_num: line,
                            position: position + 1,
                            length: len as u32,
                            token_index,
                        },
                    );
                    position += len as u32 + 1;
                    toks.push(Template::String(token));
                    iindex = i;
                    open += 1
                } else if c == '}' && open == 0 {
                    let st = &input[iindex + 1..i];
                    let utoks = self.lex(st);
                    let tok_len = utoks.len();
                    let p = utoks
                        .into_iter()
                        .map(|f| {
                            SpannedToken::new(
                                f.1,
                                Span {
                                    line_num: f.0.line_num + line,
                                    position: f.0.position + position + 1,
                                    length: f.0.length,
                                    token_index,
                                },
                            )
                        })
                        .take(tok_len)
                        .collect::<Vec<_>>();
                    last_index = i + 1;
                    position += st.len() as u32 + 2;
                    toks.push(Template::Template(
                        p,
                        SpannedToken::new(Token::Operator(Operator::OpenBrace), last_span),
                        SpannedToken::new(
                            Token::Operator(Operator::CloseBrace),
                            Span {
                                line_num: line,
                                position: position - 1,
                                length: 1,
                                token_index,
                            },
                        ),
                    ));
                }
            }
            {
                let st = input[last_index..input.len()].to_string();
                let len = st.len();
                let token = SpannedToken::new(
                    Token::Ident(st),
                    Span {
                        line_num: line,
                        position,
                        length: len as u32,
                        token_index,
                    },
                );
                toks.push(Template::String(token));
            }
            (
                Span {
                    length: input.len() as u32 + 2,
                    line_num: line,
                    position: sindex - 1,
                    token_index,
                },
                Token::TemplateString(toks),
            )
        }
    }
    pub enum Template {
        String(SpannedToken),
        Template(Vec<SpannedToken>, SpannedToken, SpannedToken),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Template {
        #[inline]
        fn clone(&self) -> Template {
            match self {
                Template::String(__self_0) => {
                    Template::String(::core::clone::Clone::clone(__self_0))
                }
                Template::Template(__self_0, __self_1, __self_2) => Template::Template(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                    ::core::clone::Clone::clone(__self_2),
                ),
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Template {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Template::String(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "String", &__self_0)
                }
                Template::Template(__self_0, __self_1, __self_2) => {
                    ::core::fmt::Formatter::debug_tuple_field3_finish(
                        f, "Template", &__self_0, &__self_1, &__self_2,
                    )
                }
            }
        }
    }
    fn get_utf8_slice(string: &str, start: usize, end: usize) -> Option<&str> {
        if !(end >= start) {
            ::core::panicking::panic("assertion failed: end >= start")
        };
        string.char_indices().nth(start).and_then(|(start_pos, _)| {
            string[start_pos..]
                .char_indices()
                .nth(end - start - 1)
                .map(|(end_pos, _)| &string[start_pos..end_pos])
        })
    }
}
pub mod parse_expression {
    use crate::{
        ast::{
            EnclosedList, Expression, KeyValue, ParsedTemplate, ParsedTemplateString,
            PunctuationList, Statement,
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
                        let op_token = self.tokens.next().cloned();
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
                let _open = self.tokens.next().unwrap();
                let expr = self.parse_operator_expression(0);
                let _close = self.tokens.next().unwrap();
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
                            colon: Some(colon.clone()),
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
            let Some (args) = self . parse_arguments () else { return (expression , false) ; } ;
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
                Some(Token::Ident(_)) => {
                    Some(Expression::Ident(self.tokens.next().unwrap().clone()))
                }
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
                Operator::Equals => 1,
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
                _ => 0,
            }
        }
        pub fn precedence_of_operator_for_ty(&self, operator: &Operator) -> u32 {
            match operator {
                Operator::Ampersand => 20,
                _ => 0,
            }
        }
    }
}
pub mod parse_types {
    use crate::{
        ast::{EnclosedList, EnclosedPunctuationList, GenericParameter, Type},
        parser::Parser,
        restore,
        token::{Operator, Token},
    };
    impl Parser {
        pub fn parse_type(&self) -> Option<Type> {
            let mut ty = self.parse_type_primary();
            if let Some(Token::Operator(Operator::Pipe)) = self.tokens.peek() {
                let p = self.parse_punctutation_list(ty, Operator::Pipe, || {
                    let index = self.tokens.get_index();
                    let res = self.parse_type_primary().map(|ty| (ty, true));
                    if res.is_none() {
                        index.restore(&self.tokens);
                    }
                    res
                });
                if let Some(p) = p {
                    return Some(Type::Union(p));
                } else {
                    return None;
                }
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
                    _ => break ty,
                };
            };
            ty
        }
        pub fn parse_type_primary(&self) -> Option<Type> {
            if let Some(Token::Operator(Operator::OpenParen)) = self.tokens.peek() {
                let _open = self.tokens.next().unwrap();
                let expr = self.parse_type();
                let _close = self.tokens.next().unwrap();
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
                    "float32" | "float" => Some(Type::Float {
                        width: 32,
                        token: self.tokens.next().unwrap().clone(),
                    }),
                    "float64" => Some(Type::Float {
                        width: 64,
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
                    let expr = {
                        let index = self.tokens.get_index();
                        let res = self.parse_literal();
                        if res.is_none() {
                            index.restore(&self.tokens);
                        }
                        res
                    };
                    expr.map(|expr| Type::Expression(Box::new(expr)))
                }
                _ => None,
            };
            ty_first
        }
        pub fn parse_generic_parameters(
            &self,
        ) -> Option<EnclosedPunctuationList<GenericParameter>> {
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
                        let list = {
                            let index = self.tokens.get_index();
                            let res =
                                self.parse_punctutation_list(first, Operator::Ampersand, || {
                                    if let Some(Token::Ident(_)) = self.tokens.peek() {
                                        Some((self.tokens.next().unwrap().clone(), true))
                                    } else {
                                        None
                                    }
                                });
                            if res.is_none() {
                                index.restore(&self.tokens);
                            }
                            res
                        };
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
}
pub mod parser {
    use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
    use crate::{
        ast::{
            ArgList, AstNode, EnclosedList, EnclosedPunctuationList, Modifer, ModiferStatement,
            Param, ParamaterList, PunctuationList, Statement, Type,
        },
        error::{ParseError, ParseErrorKind},
        token::{Operator, Range, SpannedToken, Token, TokenIndex, TokenStream},
    };
    pub struct Parser {
        pub(crate) tokens: TokenStream,
        pub(crate) errors: RwLock<Vec<ParseError>>,
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
                    let expr = self.parse_expression();
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
                Some(Token::Ident(s)) if s == "type" => {
                    let ty_tok = self.tokens.next().unwrap();
                    let symb = self.expect(Token::Ident(String::new())).unwrap();
                    let generic =
                        if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
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
                    if let Some(decl) = {
                        let index = self.tokens.get_index();
                        let res = self.parse_variable_or_function_declaration();
                        if res.is_none() {
                            index.restore(&self.tokens);
                        }
                        res
                    } {
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
        pub fn parse_modifier(
            &self,
            f: impl Fn(&Parser) -> Option<Statement>,
        ) -> Option<ModiferStatement> {
            match self.tokens.peek() {
                Some(Token::Ident(modifier)) => {
                    let modifier = match modifier.as_str() {
                        "public" => Modifer::Public,
                        "protected" => Modifer::Protected,
                        "const" => Modifer::Const,
                        "unique" => Modifer::Unique,
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
                let eq = match self.tokens.peek() {
                    Some(Token::Operator(Operator::Equals)) => self.tokens.next().cloned().unwrap(),
                    _ => return self.parse_function_declaration(ty, ident.clone()),
                };
                let expr = self.parse_expression();
                return Some(Statement::Declaration {
                    ty,
                    ident: ident.clone(),
                    eq,
                    expr,
                });
            }
            None
        }
        pub fn parse_function_declaration(
            &self,
            ty: Type,
            ident: SpannedToken,
        ) -> Option<Statement> {
            let parameters = self.parse_parameters().unwrap();
            let arrow = if let Some(Token::Operator(Operator::Arrow)) = self.tokens.peek() {
                let arrow = self.tokens.next().unwrap().clone();
                Some(arrow)
            } else {
                None
            };
            let body = self.parse_statement().map(|bd| Box::new(bd));
            Some(Statement::Function {
                generic: None,
                ident,
                parameters,
                arrow,
                return_type: ty,
                body,
            })
        }
        pub fn parse_class_declaration(&self) -> Option<Statement> {
            let token = self.tokens.next()?;
            let ident = self.tokens.next()?;
            let generic = if let Some(Token::Operator(Operator::OpenAngle)) = self.tokens.peek() {
                self.parse_generic_parameters()
            } else {
                None
            };
            let body = self.parse_enclosed_list(Operator::OpenBrace, Operator::CloseBrace, || {
                self.parse_statement().map(|f| (f, true))
            });
            if let Some(body) = body {
                Some(Statement::Class {
                    token: token.clone(),
                    ident: ident.clone(),
                    generic,
                    body,
                })
            } else {
                None
            }
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
            let open = self.expect_operator(Operator::OpenParen);
            let args = match self.tokens.peek() {
                Some(Token::Operator(Operator::CloseParen)) => PunctuationList::default(),
                _ => {
                    let mut args = PunctuationList::default();
                    while let Some(arg) = self.parse_parameter() {
                        if arg.name.is_none() && arg.ty.is_none() {
                            return None;
                        }
                        let comma =
                            if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
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
            let ty = {
                let index = self.tokens.get_index();
                let res = self.parse_type();
                if res.is_none() {
                    index.restore(&self.tokens);
                    return None;
                }
                res
            };
            let ident = {
                let index = self.tokens.get_index();
                let res = self.expect(Token::Ident("".into()));
                if res.is_none() {
                    index.restore(&self.tokens);
                    return None;
                }
                res
            };
            match (ident, ty) {
                (Some(ident), Some(ty)) => Some(Param {
                    ty: Some(ty),
                    name: Some(ident.clone()),
                }),
                (ident, ty) => {
                    self.add_error(ParseError {
                        kind: ParseErrorKind::InvalidSyntax(
                            "Unable to parse arg fields!".to_string(),
                        ),
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
                        let comma =
                            if let Some(Token::Operator(Operator::Comma)) = self.tokens.peek() {
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
                    kind: ParseErrorKind::InvalidSyntax(
                        "Unable to parse arg brackets!".to_string(),
                    ),
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
            }
        }
        pub(crate) fn expect_operator(&self, operator: Operator) -> Option<&SpannedToken> {
            self.ignore_ws();
            let Some (Token :: Operator (o)) = self . tokens . peek () else { return None ; } ;
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
            let Some (tok) = self . tokens . peek () else { return None ; } ;
            if std::mem::discriminant(tok) == std::mem::discriminant(&token_type) {
                return self.tokens.next();
            }
            None
        }
        pub(crate) fn expect_ident(&self) -> Option<&SpannedToken> {
            self.ignore_ws();
            let Some (Token :: Ident (_)) = self . tokens . peek () else { return None ; } ;
            self.tokens.next()
        }
    }
}
pub mod token {
    use std::{fmt::Display, sync::RwLock};
    use tl_util::format::{NodeDisplay, TreeDisplay};
    use crate::lexer::Template;
    pub enum Operator {
        OpenSquare,
        CloseSquare,
        OpenParen,
        CloseParen,
        OpenBrace,
        CloseBrace,
        OpenAngle,
        CloseAngle,
        Dot,
        DoubleDot,
        DoubleDotEqual,
        TripleDot,
        Colon,
        Comma,
        Arrow,
        Plus,
        Minus,
        Multiply,
        Divide,
        Exponent,
        Or,
        And,
        Ampersand,
        Exclamation,
        At,
        Pound,
        Dollar,
        Percent,
        Carot,
        Pipe,
        SemiColon,
        Tilde,
        BackTick,
        Quote,
        SingleQuote,
        Question,
        LineComment,
        BlockCommentOpen,
        BlockCommentClose,
        Equals,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Operator {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Operator::OpenSquare => ::core::fmt::Formatter::write_str(f, "OpenSquare"),
                Operator::CloseSquare => ::core::fmt::Formatter::write_str(f, "CloseSquare"),
                Operator::OpenParen => ::core::fmt::Formatter::write_str(f, "OpenParen"),
                Operator::CloseParen => ::core::fmt::Formatter::write_str(f, "CloseParen"),
                Operator::OpenBrace => ::core::fmt::Formatter::write_str(f, "OpenBrace"),
                Operator::CloseBrace => ::core::fmt::Formatter::write_str(f, "CloseBrace"),
                Operator::OpenAngle => ::core::fmt::Formatter::write_str(f, "OpenAngle"),
                Operator::CloseAngle => ::core::fmt::Formatter::write_str(f, "CloseAngle"),
                Operator::Dot => ::core::fmt::Formatter::write_str(f, "Dot"),
                Operator::DoubleDot => ::core::fmt::Formatter::write_str(f, "DoubleDot"),
                Operator::DoubleDotEqual => ::core::fmt::Formatter::write_str(f, "DoubleDotEqual"),
                Operator::TripleDot => ::core::fmt::Formatter::write_str(f, "TripleDot"),
                Operator::Colon => ::core::fmt::Formatter::write_str(f, "Colon"),
                Operator::Comma => ::core::fmt::Formatter::write_str(f, "Comma"),
                Operator::Arrow => ::core::fmt::Formatter::write_str(f, "Arrow"),
                Operator::Plus => ::core::fmt::Formatter::write_str(f, "Plus"),
                Operator::Minus => ::core::fmt::Formatter::write_str(f, "Minus"),
                Operator::Multiply => ::core::fmt::Formatter::write_str(f, "Multiply"),
                Operator::Divide => ::core::fmt::Formatter::write_str(f, "Divide"),
                Operator::Exponent => ::core::fmt::Formatter::write_str(f, "Exponent"),
                Operator::Or => ::core::fmt::Formatter::write_str(f, "Or"),
                Operator::And => ::core::fmt::Formatter::write_str(f, "And"),
                Operator::Ampersand => ::core::fmt::Formatter::write_str(f, "Ampersand"),
                Operator::Exclamation => ::core::fmt::Formatter::write_str(f, "Exclamation"),
                Operator::At => ::core::fmt::Formatter::write_str(f, "At"),
                Operator::Pound => ::core::fmt::Formatter::write_str(f, "Pound"),
                Operator::Dollar => ::core::fmt::Formatter::write_str(f, "Dollar"),
                Operator::Percent => ::core::fmt::Formatter::write_str(f, "Percent"),
                Operator::Carot => ::core::fmt::Formatter::write_str(f, "Carot"),
                Operator::Pipe => ::core::fmt::Formatter::write_str(f, "Pipe"),
                Operator::SemiColon => ::core::fmt::Formatter::write_str(f, "SemiColon"),
                Operator::Tilde => ::core::fmt::Formatter::write_str(f, "Tilde"),
                Operator::BackTick => ::core::fmt::Formatter::write_str(f, "BackTick"),
                Operator::Quote => ::core::fmt::Formatter::write_str(f, "Quote"),
                Operator::SingleQuote => ::core::fmt::Formatter::write_str(f, "SingleQuote"),
                Operator::Question => ::core::fmt::Formatter::write_str(f, "Question"),
                Operator::LineComment => ::core::fmt::Formatter::write_str(f, "LineComment"),
                Operator::BlockCommentOpen => {
                    ::core::fmt::Formatter::write_str(f, "BlockCommentOpen")
                }
                Operator::BlockCommentClose => {
                    ::core::fmt::Formatter::write_str(f, "BlockCommentClose")
                }
                Operator::Equals => ::core::fmt::Formatter::write_str(f, "Equals"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Operator {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Operator {
        #[inline]
        fn eq(&self, other: &Operator) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Operator {
        #[inline]
        fn clone(&self) -> Operator {
            match self {
                Operator::OpenSquare => Operator::OpenSquare,
                Operator::CloseSquare => Operator::CloseSquare,
                Operator::OpenParen => Operator::OpenParen,
                Operator::CloseParen => Operator::CloseParen,
                Operator::OpenBrace => Operator::OpenBrace,
                Operator::CloseBrace => Operator::CloseBrace,
                Operator::OpenAngle => Operator::OpenAngle,
                Operator::CloseAngle => Operator::CloseAngle,
                Operator::Dot => Operator::Dot,
                Operator::DoubleDot => Operator::DoubleDot,
                Operator::DoubleDotEqual => Operator::DoubleDotEqual,
                Operator::TripleDot => Operator::TripleDot,
                Operator::Colon => Operator::Colon,
                Operator::Comma => Operator::Comma,
                Operator::Arrow => Operator::Arrow,
                Operator::Plus => Operator::Plus,
                Operator::Minus => Operator::Minus,
                Operator::Multiply => Operator::Multiply,
                Operator::Divide => Operator::Divide,
                Operator::Exponent => Operator::Exponent,
                Operator::Or => Operator::Or,
                Operator::And => Operator::And,
                Operator::Ampersand => Operator::Ampersand,
                Operator::Exclamation => Operator::Exclamation,
                Operator::At => Operator::At,
                Operator::Pound => Operator::Pound,
                Operator::Dollar => Operator::Dollar,
                Operator::Percent => Operator::Percent,
                Operator::Carot => Operator::Carot,
                Operator::Pipe => Operator::Pipe,
                Operator::SemiColon => Operator::SemiColon,
                Operator::Tilde => Operator::Tilde,
                Operator::BackTick => Operator::BackTick,
                Operator::Quote => Operator::Quote,
                Operator::SingleQuote => Operator::SingleQuote,
                Operator::Question => Operator::Question,
                Operator::LineComment => Operator::LineComment,
                Operator::BlockCommentOpen => Operator::BlockCommentOpen,
                Operator::BlockCommentClose => Operator::BlockCommentClose,
                Operator::Equals => Operator::Equals,
            }
        }
    }
    impl Operator {
        pub fn as_str(&self) -> &str {
            match self {
                Self::OpenSquare => "[",
                Self::CloseSquare => "]",
                Self::OpenParen => "(",
                Self::CloseParen => ")",
                Self::OpenBrace => "{",
                Self::CloseBrace => "}",
                Self::OpenAngle => "<",
                Self::CloseAngle => ">",
                Self::Dot => ".",
                Self::DoubleDot => "..",
                Self::DoubleDotEqual => "..=",
                Self::TripleDot => "...",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::Arrow => "->",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Multiply => "*",
                Self::Divide => "/",
                Self::Exponent => "**",
                Self::Or => "||",
                Self::And => "&&",
                Self::Ampersand => "&",
                Self::Exclamation => "!",
                Self::At => "@",
                Self::Pound => "#",
                Self::Dollar => "$",
                Self::Percent => "%",
                Self::Carot => "^",
                Self::Pipe => "|",
                Self::SemiColon => ";",
                Self::Tilde => "~",
                Self::BackTick => "`",
                Self::SingleQuote => "'",
                Self::Quote => "\"",
                Self::Question => "?",
                Self::LineComment => "//",
                Self::BlockCommentOpen => "/*",
                Self::BlockCommentClose => "*/",
                Self::Equals => "=",
            }
        }
    }
    pub enum Keyword {}
    #[automatically_derived]
    impl ::core::fmt::Debug for Keyword {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            unsafe { ::core::intrinsics::unreachable() }
        }
    }
    pub enum Unit {
        Pixel,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Unit {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "Pixel")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Unit {
        #[inline]
        fn clone(&self) -> Unit {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Unit {}
    impl Display for Unit {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Unit::Pixel => f.write_str("px"),
            }
        }
    }
    pub enum Token {
        Ident(String),
        Integer(u64),
        Float(f64),
        Operator(Operator),
        String,
        TemplateString(Vec<Template>),
        Newline,
        Whitespace,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Token {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Token::Ident(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Ident", &__self_0)
                }
                Token::Integer(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Integer", &__self_0)
                }
                Token::Float(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Float", &__self_0)
                }
                Token::Operator(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Operator", &__self_0)
                }
                Token::String => ::core::fmt::Formatter::write_str(f, "String"),
                Token::TemplateString(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TemplateString",
                        &__self_0,
                    )
                }
                Token::Newline => ::core::fmt::Formatter::write_str(f, "Newline"),
                Token::Whitespace => ::core::fmt::Formatter::write_str(f, "Whitespace"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Token {
        #[inline]
        fn clone(&self) -> Token {
            match self {
                Token::Ident(__self_0) => Token::Ident(::core::clone::Clone::clone(__self_0)),
                Token::Integer(__self_0) => Token::Integer(::core::clone::Clone::clone(__self_0)),
                Token::Float(__self_0) => Token::Float(::core::clone::Clone::clone(__self_0)),
                Token::Operator(__self_0) => Token::Operator(::core::clone::Clone::clone(__self_0)),
                Token::String => Token::String,
                Token::TemplateString(__self_0) => {
                    Token::TemplateString(::core::clone::Clone::clone(__self_0))
                }
                Token::Newline => Token::Newline,
                Token::Whitespace => Token::Whitespace,
            }
        }
    }
    impl NodeDisplay for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Ident(s) => f.write_str(s),
                Self::Operator(o) => f.write_str(o.as_str()),
                Self::Integer(i) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&i)],
                )),
                Self::Float(fl) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &[""],
                    &[::core::fmt::ArgumentV1::new_display(&fl)],
                )),
                Self::TemplateString(s) => f.write_fmt(::core::fmt::Arguments::new_v1(
                    &["`", "`"],
                    &[::core::fmt::ArgumentV1::new_debug(&s)],
                )),
                Self::Newline => f.write_str("Newline"),
                Self::String => f.write_str("String"),
                Self::Whitespace => f.write_str("Whitespace"),
            }
        }
    }
    pub struct TokenIndex(usize);
    impl TokenIndex {
        pub fn restore(self, ts: &TokenStream) {
            *ts.next_index.write().unwrap() = self.0;
        }
    }
    pub struct TokenStream {
        tokens: Vec<SpannedToken>,
        next_index: RwLock<usize>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenStream {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "TokenStream",
                "tokens",
                &&self.tokens,
                "next_index",
                &&self.next_index,
            )
        }
    }
    impl<'a> TokenStream {
        pub fn next(&self) -> Option<&SpannedToken> {
            let next_index = *self.next_index.read().unwrap();
            if next_index >= self.tokens.len() {
                return None;
            }
            let r = &self.tokens[next_index];
            let mut s = self.next_index.write().unwrap();
            *s += 1;
            Some(r)
        }
        pub fn peek(&'a self) -> Option<&'a Token> {
            let next_index = *self.next_index.read().unwrap();
            if next_index >= self.tokens.len() {
                return None;
            }
            Some(self.tokens[next_index].tok())
        }
        pub fn back(&'a self) {
            let mut s = self.next_index.write().unwrap();
            *s -= 1;
        }
        pub fn get_index(&'a self) -> TokenIndex {
            TokenIndex(*self.next_index.read().unwrap())
        }
    }
    impl From<Vec<SpannedToken>> for TokenStream {
        fn from(value: Vec<SpannedToken>) -> Self {
            TokenStream {
                tokens: value,
                next_index: RwLock::new(0),
            }
        }
    }
    pub struct SpannedToken(pub Span, pub Token);
    #[automatically_derived]
    impl ::core::fmt::Debug for SpannedToken {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field2_finish(f, "SpannedToken", &&self.0, &&self.1)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SpannedToken {
        #[inline]
        fn clone(&self) -> SpannedToken {
            SpannedToken(
                ::core::clone::Clone::clone(&self.0),
                ::core::clone::Clone::clone(&self.1),
            )
        }
    }
    impl SpannedToken {
        pub fn new(token: Token, span: Span) -> Self {
            Self(span, token)
        }
        #[inline]
        pub fn tok(&self) -> &Token {
            &self.1
        }
        #[inline]
        pub fn span(&self) -> &Span {
            &self.0
        }
        #[inline]
        pub fn as_op_str(&self) -> &str {
            match &self.1 {
                Token::Operator(op) => op.as_str(),
                _ => ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(
                    &["Expected to be identifier"],
                    &[],
                )),
            }
        }
        #[inline]
        pub fn as_str(&self) -> &str {
            match &self.1 {
                Token::Ident(id) => id,
                _ => ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(
                    &["Expected to be identifier"],
                    &[],
                )),
            }
        }
    }
    impl NodeDisplay for SpannedToken {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["Token: "], &[]))?;
            self.1.fmt(f)
        }
    }
    impl TreeDisplay for SpannedToken {
        fn num_children(&self) -> usize {
            1
        }
        fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
            Some(&self.0)
        }
    }
    pub struct Span {
        pub line_num: u32,
        pub position: u32,
        pub length: u32,
        pub token_index: u32,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Span {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "Span",
                "line_num",
                &&self.line_num,
                "position",
                &&self.position,
                "length",
                &&self.length,
                "token_index",
                &&self.token_index,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Span {
        #[inline]
        fn clone(&self) -> Span {
            let _: ::core::clone::AssertParamIsClone<u32>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Span {}
    #[automatically_derived]
    impl ::core::default::Default for Span {
        #[inline]
        fn default() -> Span {
            Span {
                line_num: ::core::default::Default::default(),
                position: ::core::default::Default::default(),
                length: ::core::default::Default::default(),
                token_index: ::core::default::Default::default(),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Span {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Span {
        #[inline]
        fn eq(&self, other: &Span) -> bool {
            self.line_num == other.line_num
                && self.position == other.position
                && self.length == other.length
                && self.token_index == other.token_index
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Span {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Span {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<u32>;
        }
    }
    impl Span {
        pub fn contains(&self, other: &Span) -> bool {
            if self.line_num == other.line_num && other.position < self.position + self.length {
                return true;
            }
            false
        }
        pub fn before(&self, other: &Span) -> bool {
            if self.line_num == other.line_num && other.position >= self.position + self.length {
                return true;
            }
            false
        }
        pub fn right_before(&self, other: &Span) -> bool {
            if self.line_num == other.line_num && other.position == self.position + self.length {
                return true;
            }
            false
        }
        pub fn after(&self, other: &Span) -> bool {
            if self.line_num == other.line_num && other.position + other.length < self.position {
                return true;
            }
            false
        }
        pub fn right_after(&self, other: &Span) -> bool {
            if self.line_num == other.line_num && other.position + other.length == self.position {
                return true;
            }
            false
        }
    }
    impl From<SpannedToken> for Span {
        fn from(value: SpannedToken) -> Self {
            value.0
        }
    }
    impl From<&SpannedToken> for Span {
        fn from(value: &SpannedToken) -> Self {
            value.0
        }
    }
    impl Ord for Span {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.partial_cmp(other).unwrap()
        }
    }
    impl PartialOrd for Span {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            match self.line_num.partial_cmp(&other.line_num) {
                Some(core::cmp::Ordering::Equal) => {}
                ord => return ord,
            }
            self.position.partial_cmp(&other.position)
        }
    }
    impl NodeDisplay for Span {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Span line: ", ", character: ", ", ", " long, token: "],
                &[
                    ::core::fmt::ArgumentV1::new_display(&self.line_num),
                    ::core::fmt::ArgumentV1::new_display(&self.position),
                    ::core::fmt::ArgumentV1::new_display(&self.length),
                    ::core::fmt::ArgumentV1::new_display(&self.token_index),
                ],
            ))
        }
    }
    impl TreeDisplay for Span {
        fn num_children(&self) -> usize {
            0
        }
        fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
            ::core::panicking::panic("explicit panic")
        }
    }
    pub struct Range {
        pub start: Span,
        pub end: Span,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Range {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Range",
                "start",
                &&self.start,
                "end",
                &&self.end,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Range {
        #[inline]
        fn clone(&self) -> Range {
            let _: ::core::clone::AssertParamIsClone<Span>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Range {}
    #[automatically_derived]
    impl ::core::default::Default for Range {
        #[inline]
        fn default() -> Range {
            Range {
                start: ::core::default::Default::default(),
                end: ::core::default::Default::default(),
            }
        }
    }
    impl Range {
        pub fn new(start: Span, end: Span) -> Range {
            Range { start, end }
        }
        pub fn contains(&self, span: &Span) -> bool {
            span >= &self.start && span <= &self.end
        }
    }
    impl From<(&Range, &Range)> for Range {
        fn from(value: (&Range, &Range)) -> Self {
            Range {
                start: value.0.start,
                end: value.1.end,
            }
        }
    }
    impl<T> From<(&Range, T)> for Range
    where
        T: Into<Span>,
    {
        fn from(value: (&Range, T)) -> Self {
            Range {
                start: value.0.start,
                end: value.1.into(),
            }
        }
    }
    impl<T> From<(T, &Range)> for Range
    where
        T: Into<Span>,
    {
        fn from(value: (T, &Range)) -> Self {
            Range {
                start: value.0.into(),
                end: value.1.end,
            }
        }
    }
    impl<T, U> From<(T, U)> for Range
    where
        T: Into<Span>,
        U: Into<Span>,
    {
        fn from(value: (T, U)) -> Self {
            Range {
                start: value.0.into(),
                end: value.1.into(),
            }
        }
    }
    impl From<&Span> for Range {
        fn from(value: &Span) -> Self {
            Range {
                start: *value,
                end: *value,
            }
        }
    }
    impl From<Span> for Range {
        fn from(value: Span) -> Self {
            Range {
                start: value,
                end: value,
            }
        }
    }
    impl NodeDisplay for Range {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(::core::fmt::Arguments::new_v1(&["Range"], &[]))
        }
    }
    impl TreeDisplay for Range {
        fn num_children(&self) -> usize {
            2
        }
        fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay> {
            match index {
                0 => Some(&self.start),
                1 => Some(&self.end),
                _ => ::core::panicking::panic("explicit panic"),
            }
        }
    }
}
use error::ParseError;
pub use pollster;
impl Module {
    pub fn parse_str(input: &str, mod_name: &str) -> (Module, Vec<ParseError>) {
        let lexer = Lexer {};
        let tokens = lexer.lex(input);
        for p in &tokens {
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1_formatted(
                    &["", "\n"],
                    &[::core::fmt::ArgumentV1::new_debug(&p)],
                    &[::core::fmt::rt::v1::Argument {
                        position: 0usize,
                        format: ::core::fmt::rt::v1::FormatSpec {
                            fill: ' ',
                            align: ::core::fmt::rt::v1::Alignment::Unknown,
                            flags: 4u32,
                            precision: ::core::fmt::rt::v1::Count::Implied,
                            width: ::core::fmt::rt::v1::Count::Implied,
                        },
                    }],
                    unsafe { ::core::fmt::UnsafeArg::new() },
                ));
            };
        }
        let parser = Parser::new(tokens);
        let parsed = parser.parse().unwrap();
        for p in &parsed {
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(
                    &["", "\n"],
                    &[::core::fmt::ArgumentV1::new_display(&p.format())],
                ));
            };
        }
        let er = parser.get_errors().clone();
        (
            Module {
                name: mod_name.to_string(),
                content: input.to_string(),
                stmts: parsed,
            },
            er,
        )
    }
}
pub fn set_logger(logger: Box<dyn Log>) -> Result<(), SetLoggerError> {
    log::set_boxed_logger(logger)
}
pub struct Module {
    pub name: String,
    pub content: String,
    pub stmts: Vec<Statement>,
}
impl Module {
    pub fn empty(name: &str) -> Module {
        Module {
            name: name.to_string(),
            content: "".to_string(),
            stmts: Vec::new(),
        }
    }
    pub fn format(&self) -> String {
        self.stmts
            .iter()
            .map(|f| {
                let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                    &["", "\n"],
                    &[::core::fmt::ArgumentV1::new_display(&f.format())],
                ));
                res
            })
            .collect()
    }
}
pub enum SymbolKind {
    Record,
    Function,
    Variable,
    Parameter { ty: Type },
    ReturnParameter { ty: Type },
    Use(Vec<String>),
    Root,
}
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub parent: Option<Rf<Symbol>>,
    pub children: LinkedHashMap<String, Rf<Symbol>>,
}
impl NodeDisplay for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            SymbolKind::Root => f.write_str("Root"),
            SymbolKind::Record { .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Record `", "`"],
                &[::core::fmt::ArgumentV1::new_display(&self.name)],
            )),
            SymbolKind::Function { .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Function `", "`"],
                &[::core::fmt::ArgumentV1::new_display(&self.name)],
            )),
            SymbolKind::Variable { .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Variable `", "`"],
                &[::core::fmt::ArgumentV1::new_display(&self.name)],
            )),
            SymbolKind::Parameter { .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Parameter `", "`"],
                &[::core::fmt::ArgumentV1::new_display(&self.name)],
            )),
            SymbolKind::ReturnParameter { .. } => f.write_fmt(::core::fmt::Arguments::new_v1(
                &["Return Parameter`", "`"],
                &[::core::fmt::ArgumentV1::new_display(&self.name)],
            )),
            SymbolKind::Use(_) => f.write_fmt(::core::fmt::Arguments::new_v1(&["Use"], &[])),
        }
    }
}
impl TreeDisplay for Symbol {
    fn num_children(&self) -> usize {
        match &self.kind {
            SymbolKind::Parameter { .. } => 1,
            SymbolKind::ReturnParameter { .. } => 1,
            _ => self.children.len(),
        }
    }
    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match &self.kind {
            SymbolKind::Parameter { ty } => Some(ty),
            SymbolKind::ReturnParameter { ty } => Some(ty),
            _ => None,
        }
    }
    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay + 'a> {
        let p = self.children.values().nth(index).unwrap().borrow();
        Box::new(p)
    }
}
impl Symbol {
    pub fn new_root() -> Rf<Symbol> {
        Rf::new(Symbol {
            name: "root".to_string(),
            kind: SymbolKind::Root,
            parent: None,
            children: LinkedHashMap::new(),
        })
    }
    pub fn insert_unnamed(symb: &Rf<Symbol>, name: &str, kind: SymbolKind) -> Option<Rf<Symbol>> {
        let insert_index = {
            let symb = symb.borrow();
            [0; 128]
                .into_iter()
                .enumerate()
                .map(|(i, _)| i)
                .find_map(|v| {
                    let val = {
                        let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                            &[""],
                            &[::core::fmt::ArgumentV1::new_display(&v)],
                        ));
                        res
                    };
                    if symb.children.get(&val).is_none() {
                        Some(val)
                    } else {
                        None
                    }
                })
        };
        if let Some(insert_index) = insert_index {
            let new = Rf::new(Symbol {
                name: name.into(),
                kind,
                parent: Some(symb.clone()),
                children: LinkedHashMap::new(),
            });
            symb.borrow_mut().children.insert(insert_index, new.clone());
            Some(new)
        } else {
            None
        }
    }
    pub fn insert(symb: &Rf<Symbol>, name: &str, kind: SymbolKind) -> Rf<Symbol> {
        let new = Rf::new(Symbol {
            name: name.to_string(),
            kind,
            parent: Some(symb.clone()),
            children: LinkedHashMap::new(),
        });
        symb.borrow_mut()
            .children
            .insert(name.to_string(), new.clone());
        new
    }
}
pub struct ModuleDescender<U: Clone> {
    user_data: U,
    on_statement: Option<Box<dyn FnMut(&Statement, U) -> (U, U)>>,
    on_expression: Option<Box<dyn FnMut(&Expression, U) -> U>>,
    on_parameters: Option<Box<dyn FnMut(&ParamaterList, U) -> U>>,
    on_struct_members: Option<Box<dyn FnMut(&EnclosedList<Param>, U) -> U>>,
    on_return_parameters: Option<Box<dyn FnMut(&ParamaterList, U) -> U>>,
}
#[automatically_derived]
impl<U: ::core::default::Default + Clone> ::core::default::Default for ModuleDescender<U> {
    #[inline]
    fn default() -> ModuleDescender<U> {
        ModuleDescender {
            user_data: ::core::default::Default::default(),
            on_statement: ::core::default::Default::default(),
            on_expression: ::core::default::Default::default(),
            on_parameters: ::core::default::Default::default(),
            on_struct_members: ::core::default::Default::default(),
            on_return_parameters: ::core::default::Default::default(),
        }
    }
}
impl<U: Clone> ModuleDescender<U> {
    pub fn new(user_data: U) -> ModuleDescender<U> {
        ModuleDescender {
            user_data,
            on_statement: None,
            on_expression: None,
            on_parameters: None,
            on_struct_members: None,
            on_return_parameters: None,
        }
    }
    pub fn with_on_statement(
        mut self,
        on_statement: impl FnMut(&Statement, U) -> (U, U) + 'static,
    ) -> ModuleDescender<U> {
        self.on_statement = Some(Box::new(on_statement));
        self
    }
    pub fn with_on_expression(
        mut self,
        on_value: impl FnMut(&Expression, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_expression = Some(Box::new(on_value));
        self
    }
    pub fn with_on_parameters(
        mut self,
        on_parameters: impl FnMut(&ParamaterList, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_parameters = Some(Box::new(on_parameters));
        self
    }
    pub fn with_on_struct_members(
        mut self,
        on_struct_memebers: impl FnMut(&EnclosedList<Param>, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_struct_members = Some(Box::new(on_struct_memebers));
        self
    }
    pub fn with_on_return_parameters(
        mut self,
        on_return_parameters: impl FnMut(&ParamaterList, U) -> U + 'static,
    ) -> ModuleDescender<U> {
        self.on_return_parameters = Some(Box::new(on_return_parameters));
        self
    }
    pub fn descend(mut self, node: &Vec<Statement>) -> U {
        for node in node {
            self.descend_statement(node)
        }
        self.user_data
    }
    pub fn descend_expression(&mut self, node: &Expression) {
        match node {
            Expression::Record(parameters) => {}
            _ => (),
        }
        if let Some(on_value) = &mut self.on_expression {
            self.user_data = on_value(node, self.user_data.clone())
        }
    }
    pub fn descend_statement(&mut self, node: &Statement) {
        let sets = if let Some(on_statement) = &mut self.on_statement {
            Some(on_statement(node, self.user_data.clone()))
        } else {
            None
        };
        let sets = if let Some(sets) = sets {
            self.user_data = sets.0;
            Some(sets.1)
        } else {
            None
        };
        match node {
            Statement::Declaration {
                expr: Some(expr), ..
            } => self.descend_expression(expr),
            Statement::Expression(e) => self.descend_expression(e),
            Statement::Function {
                parameters,
                return_type,
                ..
            } => {
                if let Some(on_prm) = &mut self.on_parameters {
                    on_prm(parameters, self.user_data.clone());
                }
            }
            _ => (),
        }
        if let Some(sets) = sets {
            self.user_data = sets;
        }
    }
}
pub struct MutModuleDescender<U: Clone> {
    callback_first: bool,
    user_data: U,
    on_statement: Option<Box<dyn FnMut(&mut Statement, U) -> (U, U)>>,
    on_expression: Option<Box<dyn FnMut(&mut Expression, U) -> U>>,
}
#[automatically_derived]
impl<U: ::core::default::Default + Clone> ::core::default::Default for MutModuleDescender<U> {
    #[inline]
    fn default() -> MutModuleDescender<U> {
        MutModuleDescender {
            callback_first: ::core::default::Default::default(),
            user_data: ::core::default::Default::default(),
            on_statement: ::core::default::Default::default(),
            on_expression: ::core::default::Default::default(),
        }
    }
}
impl<U: Clone> MutModuleDescender<U> {
    pub fn new(user_data: U) -> MutModuleDescender<U> {
        MutModuleDescender {
            callback_first: true,
            user_data,
            on_statement: None,
            on_expression: None,
        }
    }
    pub fn with_on_statement(
        mut self,
        on_statement: impl FnMut(&mut Statement, U) -> (U, U) + 'static,
    ) -> MutModuleDescender<U> {
        self.on_statement = Some(Box::new(on_statement));
        self
    }
    pub fn with_on_expression(
        mut self,
        on_value: impl FnMut(&mut Expression, U) -> U + 'static,
    ) -> MutModuleDescender<U> {
        self.on_expression = Some(Box::new(on_value));
        self
    }
    pub fn with_callback_first(mut self, callback_first: bool) -> MutModuleDescender<U> {
        self.callback_first = callback_first;
        self
    }
    pub fn descend(mut self, node: &mut Vec<Statement>) -> U {
        for node in node {
            self.descend_statement(node)
        }
        self.user_data
    }
    pub fn descend_expression(&mut self, node: &mut Expression) {
        if let Some(on_value) = &mut self.on_expression {
            self.user_data = on_value(node, self.user_data.clone())
        }
    }
    pub fn descend_statement(&mut self, node: &mut Statement) {
        if self.callback_first {
            let sets = if let Some(on_statement) = &mut self.on_statement {
                Some(on_statement(node, self.user_data.clone()))
            } else {
                None
            };
            let sets = if let Some(sets) = sets {
                self.user_data = sets.0;
                Some(sets.1)
            } else {
                None
            };
            match node {
                Statement::Declaration {
                    expr: Some(expr), ..
                } => self.descend_expression(expr),
                Statement::Expression(e) => self.descend_expression(e),
                _ => (),
            }
            if let Some(sets) = sets {
                self.user_data = sets;
            }
        } else {
            match node {
                Statement::Declaration {
                    expr: Some(expr), ..
                } => self.descend_expression(expr),
                Statement::Expression(e) => self.descend_expression(e),
                _ => (),
            }
            if let Some(on_statement) = &mut self.on_statement {
                self.user_data = on_statement(node, self.user_data.clone()).1
            }
        }
    }
}
