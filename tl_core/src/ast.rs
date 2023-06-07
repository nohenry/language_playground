use tl_util::{
    format::{AsTree, Config, NodeDisplay, TreeDisplay},
    macros::{AstNode, NodeFormat, TreeFormat},
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

#[derive(Clone, NodeFormat)]
#[extra_format(" {}", self.tokens.len())]
pub struct PunctuationList<T: AstNode> {
    tokens: Vec<(T, Option<SpannedToken>)>,
}

impl<T: AstNode> Default for PunctuationList<T> {
    fn default() -> Self {
        Self {
            tokens: Default::default(),
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


#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
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

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
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

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
pub struct EnclosedPunctuationList<T: AstNode> {
    pub open: SpannedToken,
    pub items: PunctuationList<T>,
    pub close: SpannedToken,
}

impl<T: AstNode> EnclosedPunctuationList<T> {
    pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter_items()
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

pub type ParamaterList = EnclosedPunctuationList<Param>;
pub type ArgList = EnclosedPunctuationList<Expression>;

#[derive(Clone, AstNode, NodeFormat)]
pub struct EnclosedList<T: AstNode> {
    pub open: SpannedToken,
    pub items: Vec<T>,
    pub close: SpannedToken,
}

impl<T: AstNode> EnclosedList<T> {
    pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter()
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

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
#[ignore_all(type = u8, type = bool)]
#[skip_all(type = u8, type = bool)]
pub enum Type {
    Integer {
        width: u8,
        signed: bool,
        token: SpannedToken,
    },
    IntegerPointer {
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
        #[skip_item]
        #[ignore_item]
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

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub enum GenericParameter {
    Unbounded(SpannedToken),
    Bounded {
        ident: SpannedToken,
        colon: SpannedToken,
        bounds: PunctuationList<SpannedToken>,
    },
}

#[derive(Clone, TreeFormat)]
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

#[derive(Clone, NodeFormat)]
pub struct ParsedTemplateString(pub Vec<ParsedTemplate>);

impl TreeDisplay for ParsedTemplateString {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.0.len()
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        self.0.get(_index).map::<&dyn TreeDisplay<()>, _>(|t| t)
    }
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
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

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]

pub enum FunctionBody {
    Block(SpannedToken, SpannedToken, Option<SpannedToken>),
    Expression {
        arrow: SpannedToken,
        expression: Option<Box<Expression>>,
    },
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
#[ignore_all(type = SpannedToken)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration {
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

#[derive(Clone, NodeFormat, TreeFormat)]
pub enum Modifer {
    Public,
    Protected,
    Unique,
    Const,
}

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
pub struct ModiferStatement {
    #[skip_item]
    pub modifier: Modifer,
    pub modifier_token: SpannedToken,
    pub statement: Option<Box<Statement>>,
}