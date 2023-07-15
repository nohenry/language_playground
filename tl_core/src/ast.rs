use tl_util::{
    format::{AsTree, Config, NodeDisplay, SemanticType, TreeDisplay},
    macros::{AstNode, NodeFormat, TreeFormat},
};

use crate::token::{Range, SpannedToken, Token, Unit};

pub trait AstNode {
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

impl<A: AstNode, B: AstNode> AstNode for (A, B) {
    fn get_range(&self) -> Range {
        Range::from((&self.0.get_range(), &self.1.get_range()))
    }
}

impl<T: AstNode> AstNode for Box<T> {
    fn get_range(&self) -> Range {
        self.as_ref().get_range()
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

pub enum OneOf<A, B> {
    A(A),
    B(B),
}

impl<A: Clone, B: Clone> Clone for OneOf<A, B> {
    fn clone(&self) -> Self {
        match self {
            Self::A(arg0) => Self::A(arg0.clone()),
            Self::B(arg0) => Self::B(arg0.clone()),
        }
    }
}

impl<A: TreeDisplay + AstNode + Clone, B: TreeDisplay + AstNode + Clone> AstNode for OneOf<A, B> {
    fn get_range(&self) -> Range {
        match self {
            OneOf::A(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
            OneOf::B(_a0, ..) => Range::from((&_a0.get_range(), &_a0.get_range())),
        }
    }
}

impl<A: TreeDisplay + AstNode + Clone, B: TreeDisplay + AstNode + Clone> NodeDisplay
    for OneOf<A, B>
{
    #![allow(dead_code)]
    #![allow(unused_variables)]
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        match self {
            OneOf::A(a0) => {
                let this = (a0);
                write!(f, "A")?;
                Ok(())
            }
            OneOf::B(a0) => {
                let this = (a0);
                write!(f, "B")?;
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl<A: TreeDisplay + AstNode + Clone, B: TreeDisplay + AstNode + Clone> TreeDisplay
    for OneOf<A, B>
{
    fn semantic_type(&self) -> SemanticType {
        match self {
            _ => SemanticType::Default,
        }
    }

    fn num_children(&self, _cfg: &Config) -> usize {
        match self {
            OneOf::A(_a0) => 0 + 1,
            OneOf::B(_a0) => 0 + 1,
        }
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        match self {
            OneOf::A(_a0) => {
                switchon!(index, Some(_a0),);
                None
            }
            OneOf::B(_a0) => {
                switchon!(index, Some(_a0),);
                None
            }
        }
    }

    fn child_at_bx<'b>(&'b self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'b> {
        match self {
            _ => panic!("Unexpected index for enum!"),
        }
    }
}

impl<A, B> AsRef<OneOf<A, B>> for OneOf<A, B> {
    fn as_ref(&self) -> &OneOf<A, B> {
        self
    }
}

impl<A, B> OneOf<A, B> {
    pub fn map_a<T>(&self, f: impl Fn(&A) -> T) -> OneOf<T, &B> {
        match self {
            Self::A(a) => OneOf::A(f(a)),
            Self::B(b) => OneOf::B(b),
        }
    }

    pub fn map_b<T>(&self, f: impl Fn(&B) -> T) -> OneOf<&A, T> {
        match self {
            Self::A(a) => OneOf::A(a),
            Self::B(b) => OneOf::B(f(b)),
        }
    }

    pub fn a_or(self, f: impl Fn(B) -> A) -> A {
        match self {
            Self::A(a) => a,
            Self::B(b) => f(b),
        }
    }

    pub fn b_or(self, f: impl Fn(A) -> B) -> B {
        match self {
            Self::A(a) => f(a),
            Self::B(b) => b,
        }
    }
}

impl<
        A: TreeDisplay + AstNode + Clone + PartialEq,
        B: TreeDisplay + AstNode + Clone + PartialEq,
    > PartialEq for OneOf<A, B>
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::A(l0), Self::A(r0)) => l0 == r0,
            (Self::B(l0), Self::B(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Clone, NodeFormat)]
#[extra_format(" {}", self.tokens.len())]
pub struct PunctuationList<T: AstNode + TreeDisplay> {
    tokens: Vec<(T, Option<SpannedToken>)>,
}

impl<T: AstNode + TreeDisplay> Default for PunctuationList<T> {
    fn default() -> Self {
        Self {
            tokens: Default::default(),
        }
    }
}

impl<T: AstNode + TreeDisplay> PunctuationList<T> {
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
    T: AstNode + TreeDisplay,
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
        self.tokens.len()
        // if let Some((_, Some(_))) = self.tokens.last() {
        //     self.tokens.len() * 2
        // } else if !self.tokens.is_empty() {
        //     self.tokens.len() * 2 - 1
        // } else {
        //     0
        // }
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        self.tokens.get(index).map::<&dyn TreeDisplay, _>(|f| &f.0)
        // let p = &self.tokens[index / 2];
        // if index % 2 == 0 {
        //     Some(&p.0)
        // } else {
        //     Some(p.1.as_ref().unwrap())
        // }
    }
}

impl<T: PartialEq + AstNode + TreeDisplay> PartialEq for PunctuationList<T> {
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
    pub name: SpannedToken,
    pub colon: SpannedToken,
    pub expr: Box<Expression>,
}

impl PartialEq for KeyValue {
    fn eq(&self, other: &Self) -> bool {
        self.name.as_str() == other.name.as_str() && self.expr == other.expr
    }
}

impl KeyValue {
    pub fn name(&self) -> &String {
        match &self.name {
            SpannedToken(_, Token::Ident(s)) => s,
            _ => panic!(),
        }
    }
}

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
pub struct Param {
    pub ty: Box<Type>,
    pub name: Option<SpannedToken>,
    pub default: Option<(SpannedToken, Box<Expression>)>,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Param {
    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|name| name.as_str())
    }
}

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
pub struct EnclosedPunctuationList<T: AstNode + TreeDisplay> {
    pub open: SpannedToken,
    pub items: PunctuationList<T>,
    pub close: SpannedToken,
}

impl<T: AstNode + TreeDisplay> EnclosedPunctuationList<T> {
    pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter_items()
    }
}

impl<T: PartialEq + AstNode + TreeDisplay> PartialEq for EnclosedPunctuationList<T> {
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
pub struct EnclosedList<T: AstNode + TreeDisplay> {
    pub open: SpannedToken,
    pub items: Vec<T>,
    pub close: SpannedToken,
}

impl<T: AstNode + TreeDisplay> EnclosedList<T> {
    pub fn iter_items(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter()
    }
}

impl<T: AstNode + TreeDisplay> TreeDisplay for EnclosedList<T> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.items.num_children(_cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        self.items.child_at(index, _cfg)
    }
}

impl<T: PartialEq + AstNode + TreeDisplay> PartialEq for EnclosedList<T> {
    fn eq(&self, other: &Self) -> bool {
        for (a, b) in self.iter_items().zip(other.iter_items()) {
            if a != b {
                return false;
            }
        }
        true
    }
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
#[ignore_all(type = u8, type = bool)]
#[skip_all(type = u8, type = bool)]
#[semantic(SemanticType::Type)]
pub enum Type {
    #[extra_format("{}-{}", width, if *signed { "s" } else { "u" })]
    Integer {
        width: u8,
        signed: bool,
        token: SpannedToken,
    },
    #[extra_format("-{}", if *signed { "s" } else { "u" })]
    IntegerPointer {
        signed: bool,
        token: SpannedToken,
    },
    #[extra_format("{}", width)]
    Float {
        width: u8,
        token: SpannedToken,
    },
    #[extra_format("{}.{}", width, decimals)]
    Fixed {
        width: u8,
        decimals: u8,
        token: SpannedToken,
    },
    Boolean(SpannedToken),
    #[extra_format("{}", width)]
    Char {
        width: u8,
        token: SpannedToken,
    },
    Ident(SpannedToken),
    #[extra_format("{}", if *mutable { "mut" } else { "" })]
    Ref {
        #[ignore_item]
        ref_token: SpannedToken,
        mutable: bool,
        base_type: Option<Box<Type>>,
    },
    Array {
        open: SpannedToken,
        size: Option<Box<Expression>>,
        close: SpannedToken,
    },
    Union(PunctuationList<Type>),
    Tuple(EnclosedPunctuationList<Type>),
    Generic {
        base_type: Option<Box<Type>>,
        list: EnclosedPunctuationList<Type>,
    },
    Expression(Box<Expression>),
    Function {
        return_type: Box<Type>,
        parameters: ParamaterList,
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
    Path(PunctuationList<SpannedToken>),
    Let(SpannedToken),
    None(SpannedToken),
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
            (Self::Array { size: size_l, .. }, Self::Array { size: size_r, .. }) => {
                size_l == size_r
            }
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
                    return_type: l_return_type,
                    parameters: l_parameters,
                },
                Self::Function {
                    return_type: r_return_type,
                    parameters: r_parameters,
                },
            ) => l_parameters == r_parameters && l_return_type == r_return_type,
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
pub enum Binding {
    Variable(SpannedToken),
    Tuple(EnclosedPunctuationList<Binding>),
    Ignore(SpannedToken),
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub enum MatchBody {
    Patterns(EnclosedPunctuationList<MatchEntry>),
    AsBinding {
        as_token: SpannedToken,
        binding: MatchBinding,
    },
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub enum MatchBinding {
    Variable(SpannedToken),
    Tuple(EnclosedPunctuationList<MatchBinding>),
    Expression(Box<Expression>),
    Ignore(SpannedToken),
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub struct MatchEntry {
    pub binding: MatchBinding,
    pub arrow: SpannedToken,
    pub expression: Box<Expression>,
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub struct ElseClause {
    pub else_token: SpannedToken,
    pub body: Box<Expression>,
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
#[ignore_all(type = bool, type = u64, type = f64, type = Option<Unit>)]
pub enum Expression {
    BinaryExpression {
        left: Option<Box<Expression>>,
        op_token: SpannedToken,
        right: Option<Box<Expression>>,
    },
    UnaryExpression {
        op_token: SpannedToken,
        expr: Option<Box<Expression>>,
    },
    #[semantic(SemanticType::Literal)]
    Boolean(#[skip_item] bool, SpannedToken),
    #[semantic(SemanticType::Literal)]
    Integer(#[skip_item] u64, #[skip_item] Option<Unit>, SpannedToken),
    #[semantic(SemanticType::Literal)]
    Float(#[skip_item] f64, #[skip_item] Option<Unit>, SpannedToken),
    #[semantic(SemanticType::Variable)]
    Ident(SpannedToken),
    #[semantic(SemanticType::String)]
    String(#[skip_item] ParsedTemplateString, SpannedToken),
    FunctionCall {
        expr: Box<Expression>,
        args: OneOf<ArgList, Box<Expression>>,
    },
    Tuple(Vec<Expression>),
    Array(EnclosedPunctuationList<Expression>),
    ClassInitializer {
        struct_type: Box<Type>,
        values: EnclosedPunctuationList<KeyValue>,
    },
    Index {
        expression: Box<Expression>,
        open: SpannedToken,
        indexer: Option<Box<Expression>>,
        close: SpannedToken,
    },
    Block(EnclosedPunctuationList<Statement>),
    AnonFunction {
        parameters: ParamaterList,
        arrow: SpannedToken,
        expression: Box<Expression>,
    },
    If {
        if_token: SpannedToken,
        expression: Box<Expression>,
        body: Box<Expression>,
        else_clause: Option<ElseClause>,
    },
    ForLoop {
        for_token: SpannedToken,
        binding: Binding,
        in_token: SpannedToken,
        expression: Box<Expression>,
        body: Box<Statement>,
    },
    WhileLoop {
        while_token: SpannedToken,
        expression: Box<Expression>,
        body: Box<Statement>,
    },
    Match {
        match_token: SpannedToken,
        expression: Box<Expression>,
        body: MatchBody,
    },
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::BinaryExpression {
                    left: l_left,
                    op_token: l_op_token,
                    right: l_right,
                },
                Self::BinaryExpression {
                    left: r_left,
                    op_token: r_op_token,
                    right: r_right,
                },
            ) => l_left == r_left && l_right == r_right,
            (Self::Boolean(l0, l1), Self::Boolean(r0, r1)) => l0 == r0,
            (Self::Integer(l0, l1, l2), Self::Integer(r0, r1, r2)) => l0 == r0,
            (Self::Float(l0, l1, l2), Self::Float(r0, r1, r2)) => l0 == r0,
            (Self::Ident(l0), Self::Ident(r0)) => l0.as_str() == r0.as_str(),
            (Self::String(l0, l1), Self::String(r0, r1)) => l1.as_str() == r1.as_str(),
            (
                Self::FunctionCall {
                    expr: l_expr,
                    args: l_args,
                },
                Self::FunctionCall {
                    expr: r_expr,
                    args: r_args,
                },
            ) => l_expr == r_expr && l_args == r_args,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (
                Self::ClassInitializer {
                    struct_type: l_expr,
                    values: l_values,
                },
                Self::ClassInitializer {
                    struct_type: r_expr,
                    values: r_values,
                },
            ) => l_expr == r_expr || l_values == r_values,
            _ => false,
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
pub enum GetterSetter {
    Get {
        get_token: SpannedToken,
        colon: Option<SpannedToken>,
        body: Box<Expression>,
    },
    Set {
        set_token: SpannedToken,
        colon: Option<SpannedToken>,
        body: Box<Expression>,
    },
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub enum ClassBody {
    Field {
        field_type: Box<Type>,
        name: SpannedToken,
        default: Option<(SpannedToken, Box<Expression>)>,
    },
    ComputedField {
        field_type: Box<Type>,
        name: SpannedToken,
        arrow: SpannedToken,
        expression: Box<Expression>,
    },
    GetterSetters {
        field_type: Box<Type>,
        name: SpannedToken,
        arrow: SpannedToken,
        entries: EnclosedList<GetterSetter>,
    },
    Function {
        return_type: Type,
        ident: SpannedToken,
        generic: Option<EnclosedPunctuationList<GenericParameter>>,
        parameters: ParamaterList,
        arrow: Option<SpannedToken>,
        body: Option<Box<Statement>>,
    },
    Modifier(ModiferStatement<ClassBody>),
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
pub enum InterfaceBody {
    ComputedField {
        field_type: Box<Type>,
        name: SpannedToken,
        arrow: SpannedToken,
        getters_setters: Option<EnclosedPunctuationList<SpannedToken>>,
    },
    Function {
        return_type: Type,
        ident: SpannedToken,
        generic: Option<EnclosedPunctuationList<GenericParameter>>,
        parameters: ParamaterList,
    },
    Modifier(ModiferStatement<InterfaceBody>),
}

#[derive(Clone, NodeFormat, TreeFormat, AstNode)]
#[ignore_all(type = SpannedToken)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration {
        ty: Type,
        #[keep_item]
        ident: SpannedToken,
        default: Option<(SpannedToken, Box<Expression>)>,
    },
    ComputedVariableDeclaration {
        ty: Type,
        #[keep_item]
        ident: SpannedToken,
        arrow: SpannedToken,
        expr: Option<Expression>,
    },
    Class {
        token: SpannedToken,
        ident: Option<SpannedToken>,
        generic: Option<EnclosedPunctuationList<GenericParameter>>,
        inherits: Option<(SpannedToken, PunctuationList<Type>)>,
        body: EnclosedPunctuationList<ClassBody>,
    },
    Interface {
        token: SpannedToken,
        ident: Option<SpannedToken>,
        generic: Option<EnclosedPunctuationList<GenericParameter>>,
        inherits: Option<(SpannedToken, PunctuationList<Type>)>,
        body: EnclosedPunctuationList<InterfaceBody>,
    },
    ImportStatement {
        token: Option<SpannedToken>,
        args: PunctuationList<SpannedToken>,
    },
    List(PunctuationList<Statement>),
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
    Modifer(ModiferStatement<Statement>),
}

impl Statement {
    pub fn get_last(&self) -> &Statement {
        match self {
            Statement::Expression(Expression::Block(list)) => {
                list.items.iter_items().last().unwrap()
            }
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
#[semantic(SemanticType::Keyword)]
pub enum Modifer {
    Public,
    Protected,
    Unique,
    Const,
    Closed,
}

#[derive(Clone, AstNode, NodeFormat, TreeFormat)]
pub struct ModiferStatement<T: AstNode + NodeDisplay + TreeDisplay> {
    #[skip_item]
    pub modifier: Modifer,
    #[ignore_item]
    pub modifier_token: SpannedToken,
    pub statement: Option<Box<T>>,
}
