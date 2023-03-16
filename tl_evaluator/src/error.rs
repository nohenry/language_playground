use std::{error::Error, fmt::{Display, Debug}};

use colored::Colorize;
use tl_core::token::{Operator, Range};

use crate::evaluation_type::EvaluationType;

// use crate::const_value::Type;

pub enum ErrorLevel {
    Info,
    Warning,
    Error,
    Hint,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLevel::Hint => f.write_str(&"hint".bright_green().bold()),
            ErrorLevel::Info => f.write_str(&"info".bright_cyan().bold()),
            ErrorLevel::Warning => f.write_str(&"warning".bright_yellow().bold()),
            ErrorLevel::Error => f.write_str(&"error".bright_red().bold()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvaluationError<Type: EvaluationType> {
    pub kind: EvaluationErrorKind<Type>,
    pub range: Range,
}

impl <Type: EvaluationType> Display for EvaluationError<Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl <Type: EvaluationType + Display> EvaluationError<Type> {
    pub fn print(&self, file_path: &str, lines: &[&str]) {
        let padding = (self.range.start.line_num + 1).to_string().len() + 2;
        println!("{}: {}", self.kind.get_level(), self.kind);
        // file path
        println!(
            "{:>padding$}{} {}:{}:{}",
            "--".bold().blue(),
            ">".bold().blue(),
            file_path,
            self.range.start.line_num + 1,
            self.range.start.position + 1
        );

        // Initial pipe
        println!("{:>padding$}", "|".bold().blue());
        // Line contents
        println!(
            "{} {} {}",
            (self.range.start.line_num + 1).to_string().bold().blue(),
            "|".bold().blue(),
            lines[self.range.start.line_num as usize]
        );
        let start = 1 + self.range.start.position as usize;
        let len = if self.range.end <= self.range.start {
            self.range.start.length as usize
        } else if self.range.start.line_num == self.range.end.line_num {
            (self.range.end.position - self.range.start.position + 1) as usize
        } else {
            0
        };
        // let len = (self.range.end.position - self.range.start.position + 1) as usize;
        let notes = self.kind.get_notes();
        if let Some(s) = notes.first() {
            println!(
                "{:>padding$}{:>start$}{:^>len$} {}",
                "|".bold().blue(),
                " ",
                "^".bold().bright_red(),
                s.bold().bright_red()
            );
        }

        println!("{:>padding$}", "|".bold().blue());
        for note in notes.iter() {
            println!(
                "{:>padding$} {} {}",
                "=".bold().blue(),
                "note:".bold().bright_white(),
                note
            )
        }
    }
}

impl <Type: EvaluationType + Debug> Error for EvaluationError<Type> {}

#[derive(Debug, Clone)]
pub enum TypeHint {
    Variable,
    Parameter,
    ReturnParameter,
    Function,
    Struct,
    StructMember,
}

#[derive(Debug, Clone)]
pub enum EvaluationErrorKind<Type: EvaluationType> {
    TypeMismatch(Type, Type, TypeHint),
    ArgCountMismatch(u8, u8),
    NotInitialized { hint: TypeHint },
    BinExpMismatch(Operator, Type, Type),
    SymbolNotFound(String),
}

impl <Type: EvaluationType + Display> EvaluationErrorKind<Type> {
    pub fn get_level(&self) -> ErrorLevel {
        match self {
            EvaluationErrorKind::TypeMismatch(_, _, _) => ErrorLevel::Error,
            EvaluationErrorKind::ArgCountMismatch(_, _) => ErrorLevel::Error,
            EvaluationErrorKind::BinExpMismatch(_, _, _) => ErrorLevel::Error,
            EvaluationErrorKind::SymbolNotFound(_) => ErrorLevel::Error,

            EvaluationErrorKind::NotInitialized { .. } => ErrorLevel::Warning,
        }
    }

    pub fn get_notes(&self) -> Vec<String> {
        match self {
            Self::TypeMismatch(found, expected, ..) => {
                vec![format!(
                    "expected: `{}` found: `{}`",
                    expected.to_string().bold(),
                    found.to_string().bold()
                )]
            }
            Self::ArgCountMismatch(found, expected) => {
                vec![format!(
                    "expected {} arguments but received {}",
                    expected.to_string().bold(),
                    found.to_string().bold()
                )]
            }
            Self::NotInitialized {
                hint: TypeHint::Variable,
            } => {
                vec![format!("variable never initialized",)]
            }
            Self::NotInitialized {
                hint: TypeHint::ReturnParameter,
            } => {
                vec![format!("return value never initialized",)]
            }
            Self::NotInitialized {
                hint: TypeHint::Parameter,
            } => {
                vec![format!("param never initialized",)]
            }
            Self::BinExpMismatch(o, l, r) => {
                vec![format!(
                    "cannot apply operator `{}` to types `{}` and `{}`",
                    o.as_str().to_string().bold(),
                    l.to_string().bold(),
                    r.to_string().bold()
                )]
            }
            Self::SymbolNotFound(sym) => {
                vec![format!("symbol `{}` not found in scope", sym.bold(),)]
            }
            _ => vec![],
        }
    }
}

impl <Type: EvaluationType> Display for EvaluationErrorKind<Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(_, _, TypeHint::ReturnParameter) => {
                f.write_str(&"return type mismatch".bold().bright_white())
            }
            Self::TypeMismatch(_, _, TypeHint::Parameter) => {
                f.write_str(&"parameter type mismatch".bold().bright_white())
            }
            Self::TypeMismatch(_, _, TypeHint::Variable) => {
                f.write_str(&"variable type mismatch".bold().bright_white())
            }
            Self::TypeMismatch(_, _, TypeHint::Function) => {
                f.write_str(&"function type mismatch".bold().bright_white())
            }
            Self::TypeMismatch(_, _, TypeHint::Struct) => {
                f.write_str(&"struct type mismatch".bold().bright_white())
            }
            Self::TypeMismatch(_, _, TypeHint::StructMember) => {
                f.write_str(&"struct member type mismatch".bold().bright_white())
            }
            // Self::TypeMismatch(_, _, _) => f.write_str(&"type mismatch".bold().bright_white()),
            Self::ArgCountMismatch(_, _) => f.write_str(&"type mismatch".bold().bright_white()),
            Self::NotInitialized { .. } => f.write_str(&"never initialized".bold().bright_white()),
            Self::BinExpMismatch { .. } => {
                f.write_str(&"operation cannot be evaluated".bold().bright_white())
            }
            Self::SymbolNotFound(_) => f.write_str("symbol not found"),
        }
    }
}
