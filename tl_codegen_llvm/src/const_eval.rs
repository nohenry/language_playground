use inkwell::{types::AnyTypeEnum, values::AnyValueEnum};
use tl_core::{
    ast::{Expression, ParsedTemplate},
    token::{Operator, SpannedToken, Token},
};

use crate::{module::Module, AsValue, TypeBuilder, Value};

#[macro_export]
macro_rules! op {
    ($expr:expr; $self: expr, $left:expr, $right:expr; $($enumv:tt, $op:tt, $func:tt);*) => {
        match $expr {
            $(
                (AnyValueEnum::$enumv(a), AnyValueEnum::$enumv(b), Operator::$op) if a.get_type() == b.get_type() => Some(
                    $self.codegen_module.builder.$func(a, b, "").as_value($left.ty.clone()),
                ),
            )*
            _ => None
        }
    };
}

/// Evaluates constant expressions
pub struct ConstEval<'module, 'ctx> {
    pub codegen_module: &'module Module<'ctx>,
}

impl<'module, 'ctx> ConstEval<'module, 'ctx> {
    /// Creates a new `ConstEval`
    pub fn new(codegen_module: &'module Module<'ctx>) -> ConstEval<'module, 'ctx> {
        ConstEval { codegen_module }
    }

    /// Evaluates the given expression. `Some` is returned with the folded value. `None` is returned if the expression isn't constant.
    pub fn evaluate_expression(&self, expression: &Expression) -> Option<Value<'ctx>> {
        match expression {
            Expression::Integer(i, _, _) => {
                let ty = self.codegen_module.context.i64_type().as_type();
                Some(ty.llvm.into_int_type().const_int(*i, false).as_value(ty))
            }
            Expression::Float(f, _, _) => {
                let ty = self.codegen_module.context.f64_type().as_type();
                Some(ty.llvm.into_float_type().const_float(*f).as_value(ty))
            }
            Expression::Boolean(b, _) => {
                let ty = self.codegen_module.context.bool_type().as_type();
                if *b {
                    Some(ty.llvm.into_int_type().const_all_ones().as_value(ty))
                } else {
                    Some(ty.llvm.into_int_type().const_zero().as_value(ty))
                }
            }
            Expression::String(tmp, _) => {
                // Constant strings can't have templates (for now);
                // TODO: try and resolve constant templates
                if tmp.0.len() > 1 {
                    return None;
                }

                if let ParsedTemplate::String(s) = &tmp.0[0] {
                    let ty = self
                        .codegen_module
                        .context
                        .i8_type()
                        .array_type(s.as_str().as_bytes().len() as _)
                        .as_type();

                    Some(
                        self.codegen_module
                            .context
                            .const_string(s.as_str().as_bytes(), false)
                            .as_value(ty),
                    )
                } else {
                    None
                }
            }
            Expression::Array(list) => {
                let items: Option<Vec<_>> = list
                    .iter_items()
                    .map(|expr| self.evaluate_expression(expr))
                    .collect();

                let Some(items) = items else {
                    return None;
                };

                let base = if items.len() > 0 {
                    items[0].ty.clone()
                } else {
                    unimplemented!("Unable to get array base type!")
                };

                let ty = base.array_type(items.len() as _);

                match &base.llvm {
                    AnyTypeEnum::ArrayType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_array_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    AnyTypeEnum::IntType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_int_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    AnyTypeEnum::FloatType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_float_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    AnyTypeEnum::PointerType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_pointer_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    AnyTypeEnum::StructType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_struct_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    AnyTypeEnum::VectorType(arr) => {
                        let llvm_items: Vec<_> = items
                            .iter()
                            .map(|item| item.llvm.into_vector_value())
                            .collect();

                        Some(arr.const_array(&llvm_items).as_value(ty))
                    }
                    _ => None,
                }
            }
            Expression::BinaryExpression {
                left: Some(left),
                right: Some(right),
                op_token: SpannedToken(_, Token::Operator(op)),
                ..
            } => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;

                println!("{:#?} {:#?}", left.llvm, right.llvm);

                let value = op! {
                    (left.llvm, right.llvm, op);
                    self, left, right;

                    IntValue, Plus, build_int_add;
                    IntValue, Minus, build_int_sub;
                    IntValue, Multiply, build_int_mul;
                    IntValue, Ampersand, build_and;
                    IntValue, Pipe, build_or;
                    IntValue, Caret, build_xor;

                    FloatValue, Plus, build_float_add;
                    FloatValue, Minus, build_float_sub;
                    FloatValue, Multiply, build_float_mul;
                    FloatValue, Divide, build_float_div;
                    FloatValue, Percent, build_float_rem
                };

                if value.is_some() {
                    return value;
                }

                match (left.llvm, right.llvm, op) {
                    (AnyValueEnum::IntValue(a), AnyValueEnum::IntValue(b), Operator::Divide)
                        if a.get_type() == b.get_type() && left.ty.signed == right.ty.signed =>
                    {
                        if left.ty.signed {
                            Some(
                                self.codegen_module
                                    .builder
                                    .build_int_signed_div(a, b, "")
                                    .as_value(left.ty.clone()),
                            )
                        } else {
                            Some(
                                self.codegen_module
                                    .builder
                                    .build_int_unsigned_div(a, b, "")
                                    .as_value(right.ty.clone()),
                            )
                        }
                    }
                    (AnyValueEnum::ArrayValue(a), AnyValueEnum::ArrayValue(b), Operator::Plus)
                        if a.get_type().get_element_type() == b.get_type().get_element_type() =>
                    {
                        if a.is_const_string() {
                            let a = a.get_string_constant().unwrap().to_str().unwrap();
                            let b = b.get_string_constant().unwrap().to_str().unwrap();

                            let items = format!("{}{}", a, b);

                            let ty = self
                                .codegen_module
                                .context
                                .i8_type()
                                .array_type(items.as_bytes().len() as _)
                                .as_type();

                            Some(
                                self.codegen_module
                                    .context
                                    .const_string(items.as_bytes(), false)
                                    .as_value(ty),
                            )
                        } else {
                            use inkwell::types::AnyType;

                            let mut items: Vec<_> = a.iter().collect();
                            items.extend(b.iter());

                            let ty = a.get_type().get_element_type().as_any_type_enum().as_type();

                            match &a.get_type().get_element_type().as_any_type_enum() {
                                AnyTypeEnum::ArrayType(arr) => {
                                    let llvm_items: Vec<_> =
                                        items.iter().map(|item| item.into_array_value()).collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                AnyTypeEnum::IntType(arr) => {
                                    let llvm_items: Vec<_> =
                                        items.iter().map(|item| item.into_int_value()).collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                AnyTypeEnum::FloatType(arr) => {
                                    let llvm_items: Vec<_> =
                                        items.iter().map(|item| item.into_float_value()).collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                AnyTypeEnum::PointerType(arr) => {
                                    let llvm_items: Vec<_> = items
                                        .iter()
                                        .map(|item| item.into_pointer_value())
                                        .collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                AnyTypeEnum::StructType(arr) => {
                                    let llvm_items: Vec<_> =
                                        items.iter().map(|item| item.into_struct_value()).collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                AnyTypeEnum::VectorType(arr) => {
                                    let llvm_items: Vec<_> =
                                        items.iter().map(|item| item.into_vector_value()).collect();

                                    Some(arr.const_array(&llvm_items).as_value(ty))
                                }
                                _ => None,
                            }
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
