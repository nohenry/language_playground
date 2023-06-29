use std::{fmt::Display, sync::RwLock};

use colored::Colorize;
use tl_util::format::{Config, FormatType, NodeDisplay, TreeDisplay};

use crate::lexer::Template;

#[derive(Debug, Clone)]
pub enum Operator {
    OpenSquare,
    CloseSquare,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenAngle,
    OpenAngleEqual,
    CloseAngle,
    CloseAngleEqual,

    Dot,
    DoubleDot,
    DoubleDotEqual,
    TripleDot,
    Colon,
    Comma,
    Arrow,
    EqArrow,

    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Multiply,
    MultiplyEqual,
    Divide,
    DivideEqual,
    Exponent,
    ExponentEqual,
    Or,
    OrEqual,
    And,
    AndEqual,

    Ampersand,
    AmpersandEqual,
    Exclamation,
    At,
    Pound,
    Dollar,
    Percent,
    PercentEqual,
    Carot,
    CarotEqual,
    Pipe,
    PipeEqual,
    SemiColon,
    Tilde,
    BackTick,
    Quote,
    SingleQuote,
    Question,
    Underscore,

    LineComment,
    BlockCommentOpen,
    BlockCommentClose,

    Newline,

    Equals,
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
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
            Self::OpenAngleEqual => "<=",
            Self::CloseAngle => ">",
            Self::CloseAngleEqual => ">=",

            Self::Dot => ".",
            Self::DoubleDot => "..",
            Self::DoubleDotEqual => "..=",
            Self::TripleDot => "...",
            Self::Colon => ":",
            Self::Comma => ",",
            Self::Arrow => "->",
            Self::EqArrow => "=>",

            Self::Plus => "+",
            Self::PlusEqual => "+=",
            Self::Minus => "-",
            Self::MinusEqual => "-=",
            Self::Multiply => "*",
            Self::MultiplyEqual => "*=",
            Self::Divide => "/",
            Self::DivideEqual => "/=",
            Self::Exponent => "**",
            Self::ExponentEqual => "**=",
            Self::Or => "||",
            Self::OrEqual => "||=",
            Self::And => "&&",
            Self::AndEqual => "&&=",

            Self::Ampersand => "&",
            Self::AmpersandEqual => "&=",
            Self::Exclamation => "!",
            Self::At => "@",
            Self::Pound => "#",
            Self::Dollar => "$",
            Self::Percent => "%",
            Self::PercentEqual => "%=",
            Self::Carot => "^",
            Self::CarotEqual => "^=",
            Self::Pipe => "|",
            Self::PipeEqual => "|=",
            Self::SemiColon => ";",
            Self::Tilde => "~",
            Self::BackTick => "`",
            Self::SingleQuote => "'",
            Self::Quote => "\"",
            Self::Question => "?",
            Self::Underscore => "_",

            Self::LineComment => "//",
            Self::BlockCommentOpen => "/*",
            Self::BlockCommentClose => "*/",

            Self::Newline => "\\n",

            Self::Equals => "=",
        }
    }
}

#[derive(Debug)]
pub enum Keyword {
    // Output,
}

#[derive(Debug, Clone, Copy)]
pub enum Unit {
    Pixel,
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit::Pixel => f.write_str("px"),
        }
    }
}

#[derive(Debug, Clone)]
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

impl NodeDisplay for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        match self {
            Self::Ident(s) => f.write_str(s),
            Self::Operator(o) => f.write_str(o.as_str()),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::TemplateString(s) => write!(f, "`{s:?}`"),
            Self::Newline => f.write_str("Newline"),
            Self::String => f.write_str("String"),
            Self::Whitespace => f.write_str("Whitespace"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as NodeDisplay>::fmt(
            self,
            f,
            &Config {
                format_type: FormatType::Display,
            },
        )
    }
}

pub struct TokenIndex(usize);

impl TokenIndex {
    pub fn restore(self, ts: &TokenStream) {
        *ts.next_index.write().unwrap() = self.0;
    }
}

#[derive(Debug)]
pub struct TokenStream {
    tokens: Vec<SpannedToken>,
    next_index: RwLock<usize>,
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

    pub fn current(&self) -> Option<&SpannedToken> {
        let next_index = *self.next_index.read().unwrap();
        if next_index >= self.tokens.len() {
            return None;
        }
        let r = &self.tokens[next_index];
        Some(r)
    }

    pub fn peek(&'a self) -> Option<&'a Token> {
        let next_index = *self.next_index.read().unwrap();
        if next_index >= self.tokens.len() {
            return None;
        }
        Some(self.tokens[next_index].tok())
    }

    pub fn next_ignore_ws(&self) -> Option<&SpannedToken> {
        self.ignore_ws();
        let next_index = *self.next_index.read().unwrap();
        if next_index >= self.tokens.len() {
            return None;
        }
        let r = &self.tokens[next_index];
        let mut s = self.next_index.write().unwrap();
        *s += 1;
        Some(r)
    }

    pub fn current_ignore_ws(&self) -> Option<&SpannedToken> {
        self.ignore_ws();
        let next_index = *self.next_index.read().unwrap();
        if next_index >= self.tokens.len() {
            return None;
        }
        let r = &self.tokens[next_index];
        Some(r)
    }

    pub fn peek_ignore_ws(&'a self) -> Option<&'a Token> {
        self.ignore_ws();
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

    pub fn ignore_ws(&self) {
        while let Some(Token::Newline) = self.peek() {
            self.next();
        }
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

#[derive(Debug, Clone)]
pub struct SpannedToken(pub Span, pub Token);

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

    // pub fn as_unquoted_str(&self) -> &str {
    //     match &self.1 {
    //         Token::String(id) => &id[1..id.len() - 1],
    //         _ => panic!("Expected to be string"),
    //     }
    // }

    #[inline]
    pub fn as_op_str(&self) -> &str {
        match &self.1 {
            Token::Operator(op) => op.as_str(),
            _ => panic!("Expected to be identifier"),
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        match &self.1 {
            Token::Ident(id) => id,
            _ => panic!("Expected to be identifier"),
        }
    }
}

impl NodeDisplay for SpannedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        write!(f, "Token: ",)?;
        f.write_str(&format!("{}", self.1).blue().to_string())
    }
}

impl TreeDisplay for SpannedToken {
    fn num_children(&self, _cfg: &Config) -> usize {
        if let FormatType::Display = _cfg.format_type {
            0
        } else {
            1
        }
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        Some(&self.0)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub line_num: u32,
    pub position: u32,
    pub length: u32,
    pub token_index: u32,
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, cfg: &Config) -> std::fmt::Result {
        match &cfg.format_type {
            FormatType::Debug => {
                write!(
                    f,
                    "Span line: {}, character: {}, {} long, token: {}",
                    self.line_num, self.position, self.length, self.token_index
                )
            }
            FormatType::Display => {
                write!(
                    f,
                    "Span line: {}, character: {}, {} long, token: {}",
                    self.line_num, self.position, self.length, self.token_index
                )
            }
        }
    }
}

impl TreeDisplay for Span {
    fn num_children(&self, _cfg: &Config) -> usize {
        0
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        panic!()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Range {
    pub start: Span,
    pub end: Span,
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        write!(f, "Range")
    }
}

impl TreeDisplay for Range {
    fn num_children(&self, _cfg: &Config) -> usize {
        2
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        match index {
            0 => Some(&self.start),
            1 => Some(&self.end),
            _ => panic!(),
        }
    }
}
