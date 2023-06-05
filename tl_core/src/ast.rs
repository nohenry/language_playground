use core::fmt;

use tl_util::{
    format::{AsTree, Config, FormatType, NodeDisplay, TreeDisplay},
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

macro_rules! addup {
    ($($e:expr),*) => {{
        $((if let Some(_) = $e { 1 } else { 0 })+)* 0
    }};
}

macro_rules! switchon {
    ($index:expr, $($e:expr),* $(,)?) => {{
        let mut ind = 0;
        switchon!{ @parse $index, ind, $($e),*,}
    }};
    (@parse $index:expr, $ind:expr, $e:expr, $($es:expr),* $(,)?) => {
        if let Some(v) = $e {
            if $index == $ind {
                return Some(v)
            }
            $ind += 1;
        }
        switchon!{@parse $index, $ind, $($es),*,}
    };
    (@parse $index:expr,$ind:expr,$(,)?) => {
        $ind
    };
}

impl AstNode for SpannedToken {
    fn get_range(&self) -> Range {
        self.0.into()
    }
}

#[derive(Clone)]
pub struct PunctuationList<T: AstNode> {
    tokens: Vec<(T, Option<SpannedToken>)>,
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
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Punctuation List")?;
        write!(f, " {}", self.tokens.len())
    }
}

impl<T> TreeDisplay for PunctuationList<T>
where
    T: TreeDisplay + AstNode,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        if let Some((_, Some(_))) = self.tokens.last() {
            self.tokens.len() * 2
        } else if !self.tokens.is_empty() {
            self.tokens.len() * 2 - 1
        } else {
            0
        }
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
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

#[derive(Clone)]
pub struct ParamaterList {
    pub range: Range,
    pub items: PunctuationList<Param>,
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
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Element Parameters")
    }
}

impl TreeDisplay for ParamaterList {
    fn num_children(&self, _cfg: &Config) -> usize {
        2
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        match index {
            0 => Some(&self.range),
            1 => Some(&self.items),
            _ => panic!(),
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

#[derive(Clone, AstNode)]
pub struct KeyValue {
    pub name: Option<SpannedToken>,
    pub colon: SpannedToken,
    pub expr: Box<Expression>,
}

impl KeyValue {
    pub fn name(&self) -> &String {
        match &self.name {
            Some(SpannedToken(_, Token::Ident(s))) => s,
            _ => panic!(),
        }
    }
}

impl NodeDisplay for KeyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("KeyValue")
    }
}

impl TreeDisplay for KeyValue {
    fn num_children(&self, _cfg: &Config) -> usize {
        addup!(self.name, Some(true))
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        switchon!(index, &self.name, Some(&*self.expr));
        None
    }
}

#[derive(Clone, AstNode)]
pub struct Param {
    pub ty: Option<Type>,
    pub name: SpannedToken,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Param {
    pub fn name(&self) -> &String {
        match &self.name {
            SpannedToken(_, Token::Ident(s)) => s,
            _ => panic!(),
        }
    }
}

impl NodeDisplay for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Parameter")
    }
}

impl TreeDisplay for Param {
    fn num_children(&self, _cfg: &Config) -> usize {
        addup!(self.ty) + 1
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        switchon!(index, &self.ty, Some(&self.name));
        None
    }
}

#[derive(Clone)]
pub struct ArgList {
    pub range: Range,
    pub items: PunctuationList<Expression>,
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
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Arg Parameters")
    }
}

impl TreeDisplay for ArgList {
    fn num_children(&self, _cfg: &Config) -> usize {
        2
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        match index {
            0 => Some(&self.range),
            1 => Some(&self.items),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct EnclosedPunctuationList<T: AstNode> {
    pub open: SpannedToken,
    pub items: PunctuationList<T>,
    pub close: SpannedToken,
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
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Enclosed Punctuation List")
    }
}

impl<T: AstNode> TreeDisplay for EnclosedPunctuationList<T> {
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
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

#[derive(Clone)]
pub struct EnclosedList<T: AstNode> {
    pub open: SpannedToken,
    pub items: Vec<T>,
    pub close: SpannedToken,
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
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Enclosed List")
    }
}

impl<T: AstNode> TreeDisplay for EnclosedList<T> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.items.num_children(_cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        self.items.child_at(index, _cfg)
    }
}

#[derive(Clone, AstNode)]
pub enum Type {
    Integer {
        #[skip_item]
        width: u8,
        #[skip_item]
        signed: bool,
        token: SpannedToken,
    },
    Float {
        #[skip_item]
        width: u8,
        token: SpannedToken,
    },
    Fixed {
        #[skip_item]
        width: u8,
        #[skip_item]
        decimals: u8,
        token: SpannedToken,
    },
    Boolean(SpannedToken),
    Char {
        #[skip_item]
        width: u8,
        token: SpannedToken,
    },
    Ident(SpannedToken),
    Ref {
        ref_token: SpannedToken,
        #[skip_item]
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
        #[skip_item]
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
            // (Self::Expression(l0), Self::Expression(r0)) => l0 == r0,
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

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        match self {
            Self::Float { width, .. } => write!(f, "float{width}"),
            Self::Fixed { width, decimals, .. } => write!(f, "fixed{width}<{decimals}>"),
            Self::Integer {
                width,
                signed: true,
                ..
            } => write!(f, "int{width}"),
            Self::Integer {
                width,
                signed: false,
                ..
            } => write!(f, "uint{width}"),
            Self::Boolean(_) => f.write_str("bool"),
            Self::Char{width, ..} => write!(f, "char{width}"),
            Self::Ident(ident) => f.write_str(ident.as_str()),
            Self::Array(_a) => {
                f.write_str("Array")
                // write!(f, "{}", a.open.as_op_str())?;
                // let p: String = a
                //     .items
                //     .iter_items()
                //     .map(|f| format!("{}", f.format()))
                //     .intersperse(", ".to_string())
                //     .collect();
                // write!(f, "{}{}", p, a.close.as_op_str())
            }
            Self::Union(_u) => {
                f.write_str("Union")
                // for item in u
                //     .iter_items()
                //     .map(|f| format!("{}", f.format()))
                //     .intersperse(" | ".to_string())
                // {
                //     f.write_str(item.as_str())?;
                // }
                // Ok(())
            }
            Self::Tuple(_a) => {
                f.write_str("Tuple")
                // write!(f, "{}", a.open.as_op_str())?;
                // let p: String = a
                //     .items
                //     .iter_items()
                //     .map(|f| format!("{}", f.format()))
                //     .intersperse(", ".to_string())
                //     .collect();
                // write!(f, "{}{}", p, a.close.as_op_str())
            }
            Self::Generic { .. } => {
                f.write_str("Generic")
                // write!(f, "{}", a.open.as_op_str())?;
                // let p: String = a
                //     .items
                //     .iter_items()
                //     .map(|f| format!("{}", f.format()))
                //     .intersperse(", ".to_string())
                //     .collect();
                // write!(f, "{}{}", p, a.close.as_op_str())
            }
            Self::Expression(_e) => f.write_str("Expression"),
            Self::Function {
                ..
                // return_type: None,
            } => {
                f.write_str("Function")
                // f.write_str("(")?;

                // let err: fmt::Result = parameters
                //     .iter_items()
                //     .map(|f| format!("{} {}", f.ty.as_ref().unwrap().format(), f.name()))
                //     .intersperse(", ".to_string())
                //     .map(|st| f.write_str(st.as_str()))
                //     .collect();
                // err?;

                // f.write_str(")")
            }
            // Self::Function {
            //     parameters,
            //     return_type: Some((_, ret)),
            // } => {
            //     f.write_str("(")?;

            //     let err: fmt::Result = parameters
            //         .iter_items()
            //         .map(|f| format!("{} {}", f.ty.as_ref().unwrap().format(), f.name()))
            //         .intersperse(", ".to_string())
            //         .map(|st| f.write_str(st.as_str()))
            //         .collect();
            //     err?;

            //     f.write_str(") =>");

            //     ret.fmt(f)
            // }
            Self::Option { base_type: _ty, question: _ } => {
                f.write_str("Optional")
                // if let Some(ty) = &ty {
                //     ty.fmt(f)?;
                // }
                // f.write_str(question.as_op_str())
            }
            Self::Result { base_type: _ty, error: _ } => {
                f.write_str("Result")
                // if let Some(ty) = &ty {
                //     ty.fmt(f)?;
                // }
                // f.write_str(error.as_op_str())
            }
            Self::Ref {
                ref_token: _,
                mutable,
                base_type: _,
            } => {
                write!(f, "Reference {}", if *mutable { "mut" } else { "" })

                // if let Some(base_type) = &base_type {
                //     base_type.fmt(f)?;
                // }
                // f.write_str(ref_token.as_op_str())
            }
            Self::Struct(_) => f.write_str("Struct"),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Type as NodeDisplay>::fmt(
            self,
            f,
            &Config {
                format_type: FormatType::Debug,
            },
        )
    }
}

impl TreeDisplay for Type {
    fn num_children(&self, _cfg: &Config) -> usize {
        match self {
            Type::Array(a) => a.items.num_children(_cfg),
            Type::Union(a) => a.num_children(_cfg),
            Type::Tuple(a) => a.items.num_children(_cfg),
            Type::Generic {
                base_type: Some(_), ..
            } => 2,
            Type::Generic { .. } => 1,
            Type::Expression(_e) => 1,
            Type::Option { .. } => 1,
            Type::Result { .. } => 1,
            Type::Ref { .. } => 1,
            Type::Struct(s) => s.num_children(_cfg),
            _ => 0,
        }
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        match self {
            Type::Array(a) => a.items.child_at(index, _cfg),
            Type::Union(a) => a.child_at(index, _cfg),
            Type::Tuple(a) => a.items.child_at(index, _cfg),
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
            Type::Option { base_type: ty, .. } => ty.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f),
            Type::Result { base_type: ty, .. } => ty.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f),
            Type::Ref { base_type, .. } => base_type.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f),
            Type::Struct(s) => s.child_at(index, _cfg),
            _ => None,
        }
    }
}

#[derive(Clone, FormatNode, AstNode)]
pub enum GenericParameter {
    Unbounded(SpannedToken),
    Bounded {
        ident: SpannedToken,
        colon: SpannedToken,
        bounds: PunctuationList<SpannedToken>,
    },
}

#[derive(Clone)]
pub enum ParsedTemplate {
    String(SpannedToken),
    Template(Box<Expression>, SpannedToken, SpannedToken),
}

impl NodeDisplay for ParsedTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        match self {
            ParsedTemplate::String(l) => write!(f, "Literal: `{}`", l.as_str()),
            ParsedTemplate::Template(_t, _, _) => write!(f, "Value"),
        }
    }
}

impl TreeDisplay for ParsedTemplate {
    fn num_children(&self, _cfg: &Config) -> usize {
        match self {
            ParsedTemplate::Template(_, _, _) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ParsedTemplate::Template(e, _, _) => Some(&**e),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct ParsedTemplateString(pub Vec<ParsedTemplate>);

impl NodeDisplay for ParsedTemplateString {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Parsed Template String")
    }
}

impl TreeDisplay for ParsedTemplateString {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.0.len()
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        self.0.get(_index).map::<&dyn TreeDisplay<()>, _>(|t| t)
    }
}

#[derive(Clone, FormatNode, AstNode)]
#[ignore_all(type = bool, type = u64, type = f64, type = Option<Unit>)]
pub enum Expression {
    BinaryExpression {
        left: Option<Box<Expression>>,
        op_token: SpannedToken,
        right: Option<Box<Expression>>,
    },
    Boolean(#[skip_item] bool, SpannedToken),
    Integer(#[skip_item] u64, #[skip_item] Option<Unit>, SpannedToken),
    Float(#[skip_item] f64, #[skip_item] Option<Unit>, SpannedToken),
    Ident(SpannedToken),
    String(#[skip_item] ParsedTemplateString, SpannedToken),
    FunctionCall {
        expr: Box<Expression>,
        args: ArgList,
    },
    Tuple(Vec<Expression>),
    Array(EnclosedPunctuationList<Expression>),
    Record(EnclosedList<KeyValue>),
}

impl Expression {
    pub fn as_function(&self) -> Option<(&Expression, &ArgList)> {
        match self {
            Expression::FunctionCall { expr, args } => Some((expr, args)),
            _ => None,
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Expression as NodeDisplay>::fmt(
            self,
            f,
            &Config {
                format_type: FormatType::Debug,
            },
        )
    }
}

#[derive(Clone, FormatNode, AstNode)]

pub enum FunctionBody {
    Block(SpannedToken, SpannedToken, Option<SpannedToken>),
    Expression {
        arrow: SpannedToken,
        expression: Option<Box<Expression>>,
    },
}

#[derive(Clone, FormatNode, AstNode)]
#[ignore_all(type = SpannedToken)]
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

#[derive(Clone, FormatNode)]
pub enum Modifer {
    Public,
    Protected,
    Unique,
    Const,
}

#[derive(Clone, AstNode)]
pub struct ModiferStatement {
    #[skip_item]
    pub modifier: Modifer,
    pub modifier_token: SpannedToken,
    pub statement: Option<Box<Statement>>,
}

impl NodeDisplay for ModiferStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("Modifier")
    }
}

impl TreeDisplay for ModiferStatement {
    fn num_children(&self, _cfg: &Config) -> usize {
        2
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        match index {
            0 => Some(&self.modifier),
            1 => self.statement.map_tree(),
            _ => None,
        }
    }
}
