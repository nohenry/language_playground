use tl_core::{
    ast::{
        ArgList, AstNode, EnclosedPunctuationList, Expression, GenericParameter, ParamaterList,
        ParsedTemplate, Statement, Type,
    },
    token::{Range, SpannedToken},
};
use tl_evaluator::{
    evaluation_type::EvaluationType,
    evaluation_value::EvaluationValue,
    scope::{scope::ScopeValue, scope_manager::ScopeManager},
};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

pub const STOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::EVENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::MACRO,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
];

#[derive(Default)]
pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
    pub fn push(&mut self, line: u32, position: u32, length: u32, token: u32, modifier: u32) {
        if self.last_line == line {
            let delta_pos = position - self.last_pos;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: delta_pos,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        } else {
            let delta_line = line - self.last_line;
            self.last_line = line;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line,
                delta_start: position,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        }
    }

    pub fn push_token(&mut self, token: &SpannedToken, index: u32, modifier: u32) {
        self.push(
            token.span().line_num,
            token.span().position,
            token.span().length,
            index,
            modifier,
        )
    }

    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

fn get_stype_index(ty: SemanticTokenType) -> u32 {
    STOKEN_TYPES.iter().position(|f| *f == ty).unwrap_or(0) as u32
}

fn get_stype_index_from_str(ty: &str) -> u32 {
    STOKEN_TYPES
        .iter()
        .position(|f| f.as_str() == ty)
        .unwrap_or(0) as u32
}

pub struct SemanticTokenGenerator<'a, T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    scope: &'a mut ScopeManager<T, V>,
    pub scope_index: Vec<usize>,
    pub builder: SemanticTokenBuilder,
    // pub current_scope:
}

impl<'a, T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>
    SemanticTokenGenerator<'a, T, V>
{
    pub fn new(scope: &'a mut ScopeManager<T, V>) -> SemanticTokenGenerator<'a, T, V> {
        SemanticTokenGenerator {
            scope,
            scope_index: Vec::with_capacity(50),
            builder: SemanticTokenBuilder::default(),
        }
    }

    pub fn build(self) -> Vec<SemanticToken> {
        self.builder.build()
    }
}

impl<'a, T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>
    SemanticTokenGenerator<'a, T, V>
{
    pub fn recurse(&mut self, stmt: &Statement, index: usize) {
        match stmt {
            Statement::List(list) => {
                for (i, l) in list.iter_items().enumerate() {
                    self.recurse(l, i);
                }
            }
            Statement::Decleration {
                ty, ident, expr, ..
            } => {
                self.recurse_type(ty);
                // let func = match expr {
                //     Some(Expression::Record { .. }) => get_stype_index_from_str("struct"),
                //     _ => get_stype_index_from_str("variable"),
                // };

                self.builder
                    .push_token(ident, get_stype_index_from_str("variable"), 0);

                if let Some(expr) = expr {
                    self.recurse_expression(expr);
                }
            }
            Statement::Expression(e) => self.recurse_expression(e),
            Statement::UseStatement { token, args } => {
                if let Some(token) = token {
                    self.builder
                        .push_token(token, get_stype_index_from_str("keyword"), 0)
                }

                self.scope
                    .iter_use(args.iter_items().map(|f| (f.as_str(), f)), |_sym, ud| {
                        self.builder
                            .push_token(ud, get_stype_index_from_str("namespace"), 0);
                    });
            }
            Statement::Function {
                fn_tok,
                ident,
                parameters,
                return_type,
                body,
                ..
            } => {
                self.builder
                    .push_token(fn_tok, get_stype_index_from_str("keyword"), 0);
                self.builder
                    .push_token(ident, get_stype_index_from_str("function"), 0);

                self.recurse_params(parameters);
                if let Some(ty) = &return_type {
                    self.recurse_type(ty);
                }

                let sm = self.scope.find_symbol_local(ident.as_str()).map(|f| {
                    self.scope.push_scope(f);
                });

                if let Some(body) = body {
                    self.recurse(body, 0);
                }

                if sm.is_some() {
                    self.scope.pop_scope();
                }
            }
            Statement::TypeAlias {
                ty_tok,
                ident,
                generic,
                ty,
                ..
            } => {
                self.builder
                    .push_token(ty_tok, get_stype_index_from_str("keyword"), 0);
                self.builder
                    .push_token(ident, get_stype_index_from_str("type"), 0);

                if let Some(generic) = generic {
                    self.recurse_generic(generic)
                }

                self.recurse_type(ty);
            }
            Statement::Block(b) => {
                for (i, statement) in b.items.iter().enumerate() {
                    self.recurse(statement, i);
                }
            }
            Statement::Impl {
                impl_tok,
                generics,
                ty,
                body,
            } => {
                self.builder
                    .push_token(impl_tok, get_stype_index_from_str("keyword"), 0);

                if let Some(generics) = &generics {
                    self.recurse_generic(generics);
                }

                let evaluated_type = if let Some(ty) = ty {
                    self.recurse_type(ty);
                    self.evaluate_type(ty)
                } else {
                    T::empty()
                };

                if let Some(body) = body {
                    let pop = if evaluated_type.is_symbol() {
                        let sym = evaluated_type.symbol_rf();
                        self.scope.push_scope(sym.clone());
                        true
                    } else {
                        false
                    };

                    for (i, stmt) in body.iter_items().enumerate() {
                        self.recurse(stmt, i);
                    }

                    if pop {
                        self.scope.pop_scope();
                    }
                }
            }
        }
    }

    pub fn recurse_expression(&mut self, value: &Expression) {
        match value {
            Expression::Boolean(_, tok) => {
                self.builder
                    .push_token(tok, get_stype_index_from_str("keyword"), 0)
            }
            Expression::String(template_string, _tok) => {
                for templ in &template_string.0 {
                    match templ {
                        ParsedTemplate::String(s) => {
                            self.builder.push_token(
                                s,
                                get_stype_index(SemanticTokenType::STRING),
                                0,
                            );
                        }
                        ParsedTemplate::Template(st, _o, _c) => {
                            self.recurse_expression(st);
                        }
                    }
                }
            }
            Expression::Ident(tok) => {
                let mut current_scope = Vec::new();
                self.scope
                    .push_scope_chain(&mut current_scope, self.scope_index.iter());

                if let Some(sym) = self.scope.find_symbol(tok.as_str()) {
                    let sym = sym.borrow();
                    match &sym.value {
                        ScopeValue::EvaluationValue(value) if value.is_function() || value.is_native_function() => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::FUNCTION),
                                0,
                            );
                        }
                        ScopeValue::Struct { .. } => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        ScopeValue::EvaluationValue(value) if value.is_struct_instance() => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        _ => {
                            self.builder.push_token(
                                tok,
                                get_stype_index(SemanticTokenType::VARIABLE),
                                0,
                            );
                        }
                    }
                }
            }
            Expression::Float(_, _, tok) => {
                self.builder
                    .push_token(tok, get_stype_index(SemanticTokenType::NUMBER), 0);
            }
            Expression::Integer(_, _, tok) => {
                self.builder
                    .push_token(tok, get_stype_index(SemanticTokenType::NUMBER), 0);
            }

            Expression::FunctionCall { expr, args } => {
                self.recurse_expression(expr);
                self.recurse_args(args);
            }
            Expression::Tuple(_) => (),
            Expression::Array { values, .. } => values
                .iter_items()
                .for_each(|item| self.recurse_expression(item)),
            Expression::BinaryExpression { left, right, .. } => {
                if let Some(left) = left {
                    self.recurse_expression(left);
                }
                if let Some(right) = right {
                    self.recurse_expression(right);
                }
            }
            Expression::Record(parameters) => {
                for kv in parameters.items.iter() {
                    if let Some(name) = &kv.name {
                        self.builder
                            .push_token(name, get_stype_index_from_str("property"), 0)
                    }

                    self.recurse_expression(&kv.expr);
                }
            }
        }
    }

    pub fn recurse_type(&mut self, ty: &Type) {
        match ty {
            Type::Integer { token, .. } => {
                self.builder
                    .push_token(token, get_stype_index_from_str("type"), 0);
            }
            Type::Float { token, .. } => {
                self.builder
                    .push_token(token, get_stype_index_from_str("type"), 0);
            }
            Type::Boolean(tok) => self
                .builder
                .push_token(tok, get_stype_index_from_str("type"), 0),
            Type::Expression(e) => self.recurse_expression(e),
            Type::Ref {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Option {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Result {
                base_type: Some(base_type),
                ..
            } => self.recurse_type(base_type),
            Type::Ident(ident) => {
                self.builder
                    .push_token(ident, get_stype_index_from_str("type"), 0);
            }
            Type::Struct(values) => {
                for item in values.iter_items() {
                    if let Some(ty) = &item.ty {
                        self.recurse_type(ty);
                    }

                    if let Some(name) = &item.name {
                        self.builder.push_token(
                            name,
                            get_stype_index(SemanticTokenType::VARIABLE),
                            0,
                        );
                    }
                }
            }
            Type::Generic { base_type, list } => {
                if let Some(base_type) = base_type {
                    self.recurse_type(base_type);
                }

                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            Type::Union(list) => {
                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            Type::Array(list) => {
                for ty in list.iter_items() {
                    self.recurse_type(ty)
                }
            }
            _ => (),
        }
    }

    pub fn recurse_generic(&mut self, stmt: &EnclosedPunctuationList<GenericParameter>) {
        for stmt in stmt.iter_items() {
            match stmt {
                GenericParameter::Unbounded(ident) => {
                    self.builder
                        .push_token(ident, get_stype_index_from_str("typeParameter"), 0)
                }
                GenericParameter::Bounded { bounds, ident, .. } => {
                    self.builder
                        .push_token(ident, get_stype_index_from_str("typeParameter"), 0);

                    for bound in bounds.iter_items() {
                        self.builder
                            .push_token(bound, get_stype_index_from_str("interface"), 0);
                    }
                }
            }
        }
    }

    pub fn recurse_args(&mut self, args: &ArgList) {
        for item in args.iter_items() {
            self.recurse_expression(item)
        }
    }

    pub fn recurse_params(&mut self, args: &ParamaterList) {
        for item in args.iter_items() {
            if let Some(ty) = &item.ty {
                self.recurse_type(ty);
            }

            if let Some(name) = &item.name {
                self.builder
                    .push_token(name, get_stype_index(SemanticTokenType::VARIABLE), 0);
            }
        }
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> SemanticTokenGenerator<'_, T, V> {
    fn evaluate_type(&self, ty: &tl_core::ast::Type) -> T {
        match ty {
            tl_core::ast::Type::Integer { width, signed, .. } => T::integer(*width, *signed),
            tl_core::ast::Type::Float { width, .. } => T::float(*width),
            tl_core::ast::Type::Ident(id) => {
                if let Some(sym) = self.scope.find_symbol(id.as_str()) {
                    return T::symbol(sym);
                }

                T::empty()
            }
            tl_core::ast::Type::Boolean(_) => T::bool(),
            tl_core::ast::Type::Ref {
                base_type: Some(ty),
                ..
            } => T::rf(self.evaluate_type(ty)),
            tl_core::ast::Type::Generic {
                base_type: Some(box tl_core::ast::Type::Ident(tok)),
                list,
            } => {
                let types: Vec<_> = list.iter_items().map(|ty| self.evaluate_type(ty)).collect();

                let Some(symrf) = self.scope.find_symbol(tok.as_str()) else {
                    return T::empty()
                };

                let csi = {
                    let sym = symrf.borrow();
                    match &sym.value {
                        ScopeValue::StructTemplate {
                            generics,
                            constructions,
                            construction_start_index,
                            ..
                        } => {
                            if !self.verify_generics_match(generics, &types, list.get_range()) {
                                return T::empty()
                            }

                            // If we have already constructed this struct with the same type arguments, reuse this construction
                            if let Some(child_construction_name) = constructions.get(&types) {
                                let construction = sym
                                    .children
                                    .get(child_construction_name)
                                    .expect("Compiler Bug!");
                                return T::symbol(construction.clone())
                            }
                            panic!("Should Prboably have returned?");

                            *construction_start_index
                        }
                        ScopeValue::IntrinsicStructTemplate {
                            initial_value,
                            generics,
                        } => {
                            if !self.verify_generics_match(generics, &types, list.get_range()) {
                                return T::empty()
                            }

                            return T::intrinsic(symrf.clone());
                        }
                        _ => {
                            return T::empty()
                        }
                    }
                };

                // let mut sym = symrf.borrow_mut();

                // let mut hash = DefaultHasher::new();
                // types.hash(&mut hash);
                // let hash = hash.finish().to_string();

                // let raw_members = {
                //     let ScopeValue::StructTemplate { constructions, raw_members, .. } = &mut sym.value else {
                //         // self.add_error(EvaluationError {
                //         //     kind: EvaluationErrorKind::TypeMismatch(Type::Empty, Type::Empty, TypeHint::Record),
                //         //     range: tok.get_range(),
                //         // });
                //         return tl_vm::const_value::Type::Empty
                //     };

                //     constructions.insert(types.clone(), hash.clone());
                //     raw_members.clone()
                // };

                // // Build generic parameter symbols
                // let children: Vec<_> = {
                //     sym.children
                //         .iter()
                //         .take(csi)
                //         .zip(types.into_iter())
                //         .map(|((k, _), ty)| {
                //             Scope::new(
                //                 symrf.clone(),
                //                 k.to_string(),
                //                 ScopeValue::TypeAlias {
                //                     ident: k.to_string(),
                //                     ty: Box::new(ty),
                //                 },
                //                 0,
                //             )
                //         })
                //         .collect()
                // };

                // let child = sym.insert(
                //     symrf.clone(),
                //     hash.clone(),
                //     ScopeValue::Struct {
                //         ident: hash,
                //         members: LinkedHashMap::new(),
                //     },
                //     0,
                // );

                // // insert generic parameter symbols
                // {
                //     let mut child = child.borrow_mut();
                //     for c in children {
                //         child.insert_node(c);
                //     }
                // }

                // {
                //     self.scope.push_scope(child.clone());

                //     // We need to regenerate types using generic parameters
                //     let emembers = self.evaluate_struct_members(&raw_members);

                //     let mut child_sym = child.borrow_mut();
                //     let ScopeValue::Struct { members, .. } = &mut child_sym.value else {
                //         panic!("Expected struct!")
                //     };

                //     *members = emembers;

                //     self.scope.pop_scope();
                // }

                // tl_vm::const_value::Type::Symbol(child)
            }
            _ => T::empty(),
        }
    }

    fn verify_generics_match(
        &self,
        params: &Vec<GenericParameter>,
        args: &Vec<T>,
        errored_range: Range,
    ) -> bool {
        if args.len() != params.len() {
            return false;
        }

        // TODO: Verify bindings match

        true
    }
}
