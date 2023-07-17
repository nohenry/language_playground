use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    fmt::Display,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use tl_core::ast::{
    AstNode, ClassBody, Expression, InterfaceBody, MatchBinding, MatchBody, ModiferStatement,
    Statement,
};
use tl_util::format::{NodeDisplay, TreeDisplay};

use crate::{const_eval, module::Module, Type, TypeBuilder};

use colored::Colorize;

/// Represents a generic argument in a path.
/// std.List<std.uint32>
/// std.array<uint8, 45>
#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub enum GenericArgument {
    /// A path to another type
    Path(Path),
    /// A constant value
    Constant(u64),
}

impl Display for GenericArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Path(path) => path.fmt(f),
            Self::Constant(constant) => constant.fmt(f),
        }
    }
}

impl From<Path> for GenericArgument {
    fn from(value: Path) -> Self {
        Self::Path(value)
    }
}

impl From<u64> for GenericArgument {
    fn from(value: u64) -> Self {
        Self::Constant(value)
    }
}

/// A path points to somewhere into the module map. This can be either absolute or relative
#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct Path {
    /// Path part: std.array
    pub path: Vec<String>,
    /// Generic part: <uint8, 34>
    pub generic: Vec<GenericArgument>,
}

impl Path {
    /// Returns true if `self` starts with the `other` path. Generics are ignored.
    pub fn starts_with(&self, other: &Path) -> bool {
        for (a, b) in self.path.iter().zip(other.path.iter()) {
            if a != b {
                return false;
            }
        }

        true
    }

    /// Creates a new path with `self` cloned and `path_segemnt` appended onto the end.
    ///
    /// # Example
    /// ```no_run,ignore
    /// use tl_codegen_llvm::Path;
    ///
    /// let mymod = Path::empty().with("mymod").with("inner");
    /// assert_eq!(mymod, Path::from(["mymod".into(), "inner".into()].into_iter()));
    /// ```
    pub fn with(&self, path_segment: &str) -> Path {
        let mut new_path = self.clone();
        new_path.path.push(path_segment.to_string());
        new_path
    }

    /// Creates a new path with `self` cloned and `generics` extended onto the generic arguments.
    ///
    /// # Example
    /// ```no_run,ignore
    /// use tl_codegen_llvm::Path;
    ///
    /// let mymod = Path::empty().with("mymod").with("Type").with_generics([Path::std("uint32").into()]);
    /// assert_eq!(mymod.generic, vec![GenericArgument::Path(Path::std("uint32"))]);
    /// ```
    pub fn with_generic(&self, generics: impl Iterator<Item = GenericArgument>) -> Path {
        let mut new_path = self.clone();
        new_path.generic.extend(generics);
        new_path
    }

    /// Creates a new path that starts with `std` and `path_segment` appended on.
    pub fn std(path_segment: &str) -> Path {
        Path {
            path: vec!["std".to_string(), path_segment.to_string()],
            generic: Vec::new(),
        }
    }

    /// Creates a new path that starts with `std`
    pub fn std_mod() -> Path {
        Path {
            path: vec!["std".to_string()],
            generic: Vec::new(),
        }
    }

    /// Creates an empty path
    pub fn empty() -> Path {
        Path {
            path: Vec::new(),
            generic: Vec::new(),
        }
    }

    /// Mangles a `self` into a string for use in llvm
    ///
    /// Symbols are separated with an '_'. If there are any generic parameters, the hash of the generics are appended on after a '$'.
    ///
    /// # Example
    ///
    /// ```no_run,ignore
    /// use tl_codegen_llvm::resolve::Path;
    ///
    /// assert_eq!(Path::std("io").with("Data").mangle(), "std_io_Data");
    /// ```
    pub fn mangle(&self) -> String {
        if self.generic.len() > 0 {
            let mut hasher = DefaultHasher::new();
            Hash::hash(&self.generic, &mut hasher);

            format!("{}${}", self.path.join("_"), hasher.finish())
        } else {
            self.path.join("_")
        }
    }
}

impl<I: Iterator<Item = String>> From<I> for Path {
    fn from(value: I) -> Self {
        Self {
            path: value.collect(),
            generic: Vec::new(),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.generic.len() > 0 {
            write!(
                f,
                "{}<{}>",
                self.path.join("."),
                self.generic
                    .iter()
                    .map(|gen| gen.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        } else {
            f.write_str(&self.path.join("."))
        }
    }
}

/// A pass of symbol resolving
pub trait ResolvePass {
    /// The next pass to run
    type Next: ResolvePass;
}

/// Resolves classes and interfaces
pub struct TypePass;
/// Resolves all instances of generic types
pub struct GenericPass;
/// Resolves type aliases
pub struct TypeAliasPass;
/// Resolves all functions
pub struct FunctionPass;

impl ResolvePass for TypePass {
    type Next = GenericPass;
}

impl ResolvePass for GenericPass {
    type Next = TypeAliasPass;
}

impl ResolvePass for TypeAliasPass {
    type Next = FunctionPass;
}

impl ResolvePass for FunctionPass {
    type Next = ();
}

impl ResolvePass for () {
    type Next = ();
}

/// Functions for resolving in a pass
trait Resolver {
    /// Resolve a statement. `current_path` is the path of the parent symbol
    fn resolve_statement(&mut self, _current_path: &Path, _statement: &Statement) {
        unimplemented!()
    }
    /// Resolve a class body. `current_path` is the path of the parent symbol
    fn resolve_class_body(&mut self, _current_path: &Path, _class_body: &ClassBody) {
        unimplemented!()
    }
    /// Resolve an interface body. `current_path` is the path of the parent symbol
    fn resolve_interface_body(&mut self, _current_path: &Path, _interface_body: &InterfaceBody) {
        unimplemented!()
    }
    /// Resolve an expression. `current_path` is the path of the parent symbol
    fn resolve_expression(&mut self, _current_path: &Path, _expression: &Expression) {
        unimplemented!()
    }
}

/// An entry in the symbol map.
#[derive(Clone)]
pub struct SymbolEntry<T> {
    /// If set, the symbol is exported from the module
    public: bool,
    /// If set, the symbol is unique in the translation unit (static)
    unique: bool,
    /// If set, the symbol constant
    constant: bool,

    /// The symbol data. T can be anything.
    symbol: T,
}

impl<T> std::fmt::Display for SymbolEntry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.public {
            write!(f, "{}", "public ".blue())?;
        }

        if self.constant {
            write!(f, "{}", "const ".blue())?;
        }

        if self.unique {
            write!(f, "{}", "unique ".blue())?;
        }

        Ok(())
    }
}

/// Used for Resolving symbols by analyzing the provided AST.
///
/// # Example
/// ```no_run,ignore
/// let resolve = Resolve::new(&ast_module, codegen_module);
/// let resolve = resolve.resolve(); // Resolves types
/// let resolve = resolve.resolve(); // Resolves generics
/// let resolve = resolve.resolve(); // Resolves type aliases
/// let resolve: Resolve<_, ()> = resolve.resolve(); // Resolves functions
/// ```
pub struct Resolve<'ctx, P: ResolvePass> {
    /// Code generation module. Used for llvm.
    pub codegen_module: Module<'ctx>,

    /// Symbol namespace for types
    types: HashMap<Path, SymbolEntry<Type<'ctx>>>,
    modules: HashMap<Path, ()>,
    functions: HashMap<Path, SymbolEntry<inkwell::values::FunctionValue<'ctx>>>,
    variables: HashMap<Path, SymbolEntry<Type<'ctx>>>,
    imports: HashSet<Path>,

    make_public: bool,
    make_constant: bool,
    make_unique: bool,

    pass: PhantomData<P>,
}

impl<'ctx, P: ResolvePass> Resolve<'ctx, P> {
    /// Creates a new `Resolve` instance with the first pass (`TypePass`).
    pub fn new(module: &tl_core::Module, codegen_module: Module<'ctx>) -> Resolve<'ctx, TypePass> {
        let path = Path::empty().with(&module.name);

        let mut resolve = Resolve {
            codegen_module,

            types: HashMap::default(),
            modules: HashMap::default(),
            functions: HashMap::default(),
            variables: HashMap::default(),
            imports: HashSet::default(),

            make_public: false,
            make_constant: false,
            make_unique: false,

            pass: PhantomData,
        };

        resolve.modules.insert(Path::std_mod(), ());
        resolve.imports.insert(Path::std_mod());

        resolve.modules.insert(path.clone(), ());

        resolve
    }

    /// Resolves a `path` within the `current_path`. Returns `Some` with the absolute path if exists, otherwise returns `None`.
    /// * Only searches for a symbol between the current module, and the `current_path`
    /// * Any imported modules will be searched as well
    pub fn resolve_path(&self, current_path: &Path, path: &Path) -> Option<Path> {
        let mut path_iter = path.path.iter();
        let first = path_iter
            .next()
            .expect("Expected at least one path segment");

        let path_index = current_path
            .path
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, seg)| if seg == first { Some(i) } else { None });

        if let Some(index) = path_index {
            // path is some subset of current_path

            let mut new_path = current_path.path[..index].to_vec();
            new_path.extend(path.path.iter().cloned());

            let path = Path {
                path: new_path,
                generic: path.generic.clone(),
            };

            let sym = self
                .modules
                .get(&path)
                .or_else(|| self.types.get(&path).map(|_| &()))
                .or_else(|| self.functions.get(&path).map(|_| &()))
                .or_else(|| self.variables.get(&path).map(|_| &()));

            sym.map(|_| path)
        } else {
            if let Some(_) = self.modules.get(current_path) {
                let mut new_path = current_path.path[0..].to_vec();
                new_path.extend(path.path.iter().cloned());

                let path = Path {
                    path: new_path,
                    generic: path.generic.clone(),
                };

                let sym = self
                    .modules
                    .get(&path)
                    .or_else(|| self.types.get(&path).map(|_| &()))
                    .or_else(|| self.functions.get(&path).map(|_| &()))
                    .or_else(|| self.variables.get(&path).map(|_| &()));

                return sym.map(|_| path);
            }

            let mut new_path = Path {
                path: current_path.path.clone(),
                generic: Vec::new(),
            };

            // Resolve in current module
            while self.modules.get(&new_path).is_none() && new_path.path.len() > 0 {
                new_path.path.truncate(new_path.path.len() - 1);

                let mut next_path = new_path.path.to_vec();
                next_path.extend(path.path.iter().cloned());

                let path = Path {
                    path: next_path,
                    generic: path.generic.clone(),
                };

                let sym = self
                    .modules
                    .get(&path)
                    .or_else(|| self.types.get(&path).map(|_| &()))
                    .or_else(|| self.functions.get(&path).map(|_| &()))
                    .or_else(|| self.variables.get(&path).map(|_| &()));

                if sym.is_some() {
                    return Some(path);
                }
            }

            for import in &self.imports {
                println!("{} - {}", import, path);

                if let Some(path) = self.resolve_path(import, path) {
                    println!("path {path}");
                    let sym = self
                        .modules
                        .get(&path)
                        .map(|_| true)
                        .or_else(|| self.types.get(&path).map(|p| p.public))
                        .or_else(|| self.functions.get(&path).map(|p| p.public))
                        .or_else(|| self.variables.get(&path).map(|p| p.public));

                    if let Some(true) = sym {
                        return Some(path);
                    }
                }
            }

            None
        }
    }

    /// Formats a pretty path.
    /// * Modules are bright green
    /// * Types are yellow
    /// * Functions are cyan
    /// * Variables are purple
    pub fn format_path(&self, path: &Path) -> String {
        let mut new_path = Path {
            path: Vec::new(),
            generic: Vec::new(),
        };

        path.path
            .iter()
            .map(|p| {
                new_path.path.push(p.to_string());

                if self.modules.get(&new_path).is_some() {
                    p.bright_green().to_string()
                } else if self.types.get(&new_path).is_some() {
                    p.yellow().to_string()
                } else if self.functions.get(&new_path).is_some() {
                    p.cyan().to_string()
                } else if self.variables.get(&new_path).is_some() {
                    p.purple().to_string()
                } else {
                    p.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Helper function that sets up modifiers and resets them. If `modifer.statement` is `Some`, `f` will be called with the value and `self` passed in.
    /// See [`tl_core::ast::Modifier`]
    pub fn resolve_modifier<T: TreeDisplay + AstNode + NodeDisplay>(
        &mut self,
        modifier: &ModiferStatement<T>,
        f: impl Fn(&mut Self, &T),
    ) {
        let old;
        match modifier.modifier {
            tl_core::ast::Modifer::Public => {
                old = self.make_public;
                self.make_public = true;
            }
            tl_core::ast::Modifer::Unique => {
                old = self.make_unique;
                self.make_unique = true;
            }
            tl_core::ast::Modifer::Const => {
                old = self.make_constant;
                self.make_constant = true;
            }
            _ => old = false,
        }

        if let Some(stmt) = &modifier.statement {
            f(self, stmt)
        }

        match modifier.modifier {
            tl_core::ast::Modifer::Public => self.make_public = old,
            tl_core::ast::Modifer::Unique => self.make_unique = old,
            tl_core::ast::Modifer::Const => self.make_constant = old,
            _ => (),
        }
    }

    /// Make a [`SymbolEntry`] with the specified symbol. Uses `make_*` fields in `self`. This should be used in most cases when inserting a symbol into the map.
    fn make_entry<T>(&self, symbol: T) -> SymbolEntry<T> {
        SymbolEntry {
            public: self.make_public,
            unique: self.make_unique,
            constant: self.make_constant,
            symbol,
        }
    }
}

impl<'ctx> Resolve<'ctx, TypePass> {
    pub fn resolve(
        mut self,
        module: &tl_core::Module,
    ) -> Resolve<'ctx, <TypePass as ResolvePass>::Next> {
        let mut path = Path::empty().with(&module.name);

        for stmt in &module.stmts {
            self.resolve_statement(&mut path, stmt);
        }

        Resolve {
            pass: PhantomData,
            ..self
        }
    }
}

impl<'ctx> Resolve<'ctx, GenericPass> {
    pub fn resolve(
        mut self,
        module: &tl_core::Module,
    ) -> Resolve<'ctx, <GenericPass as ResolvePass>::Next> {
        let mut path = Path::empty().with(&module.name);

        for stmt in &module.stmts {
            self.resolve_statement(&mut path, stmt);
        }

        Resolve {
            pass: PhantomData,
            ..self
        }
    }
}

impl<'ctx> Resolve<'ctx, TypeAliasPass> {
    pub fn resolve(
        mut self,
        module: &tl_core::Module,
    ) -> Resolve<'ctx, <TypeAliasPass as ResolvePass>::Next> {
        let mut path = Path::empty().with(&module.name);

        for stmt in &module.stmts {
            self.resolve_statement(&mut path, stmt);
        }

        Resolve {
            pass: PhantomData,
            ..self
        }
    }
}

impl<'ctx> Resolve<'ctx, FunctionPass> {
    pub fn resolve(
        mut self,
        module: &tl_core::Module,
    ) -> Resolve<'ctx, <FunctionPass as ResolvePass>::Next> {
        let mut path = Path::empty().with(&module.name);

        for stmt in &module.stmts {
            self.resolve_statement(&mut path, stmt);
        }

        Resolve {
            pass: PhantomData,
            ..self
        }
    }
}

impl<'ctx> Resolve<'ctx, ()> {
    pub fn _resolve(self) -> Resolve<'ctx, ()> {
        Resolve {
            pass: PhantomData,
            ..self
        }
    }
}

impl<'ctx> Resolver for Resolve<'ctx, TypePass> {
    fn resolve_statement(&mut self, current_path: &Path, statement: &Statement) {
        match statement {
            Statement::Class {
                ident: Some(ident), ..
            } => {
                let path = current_path.with(ident.as_str());

                self.types.insert(
                    path.clone(),
                    self.make_entry(
                        self.codegen_module
                            .context
                            .opaque_struct_type(ident.as_str())
                            .path(path.clone()),
                    ),
                );
            }
            Statement::Interface {
                ident: Some(ident), ..
            } => {
                let path = current_path.with(ident.as_str());

                self.types.insert(
                    path.clone(),
                    self.make_entry(
                        self.codegen_module
                            .context
                            .opaque_struct_type(ident.as_str())
                            .path(path.clone()),
                    ),
                );
            }
            Statement::ImportStatement { args, .. } => {
                let path = args.iter_items().map(|arg| arg.as_str().to_string()).into();

                let path = self
                    .resolve_path(current_path, &path)
                    .map_or_else(
                        || self.modules.get(&path).map(|_| path.clone()),
                        |p| Some(p),
                    )
                    .expect("Unable to resolve import path");
                println!("import path {}", path);

                self.imports.insert(path);
            }
            Statement::Modifer(modifier) => {
                let old;
                match modifier.modifier {
                    tl_core::ast::Modifer::Public => {
                        old = self.make_public;
                        self.make_public = true;
                    }
                    tl_core::ast::Modifer::Unique => {
                        old = self.make_unique;
                        self.make_unique = true;
                    }
                    tl_core::ast::Modifer::Const => {
                        old = self.make_constant;
                        self.make_constant = true;
                    }
                    _ => old = false,
                }

                if let Some(stmt) = &modifier.statement {
                    self.resolve_statement(current_path, stmt);
                }

                match modifier.modifier {
                    tl_core::ast::Modifer::Public => self.make_public = old,
                    tl_core::ast::Modifer::Unique => self.make_unique = old,
                    tl_core::ast::Modifer::Const => self.make_constant = old,
                    _ => (),
                }
            }
            _ => (),
        }
    }
}

impl<'ctx> Resolver for Resolve<'ctx, GenericPass> {
    fn resolve_statement(&mut self, current_path: &Path, statement: &Statement) {
        match statement {
            Statement::Class {
                ident: Some(ident),
                body,
                ..
            } => {
                let path = current_path.with(ident.as_str());

                for item in body.iter_items() {
                    self.resolve_class_body(&path, item);
                }
            }
            Statement::Interface {
                ident: Some(ident),
                body,
                ..
            } => {
                let path = current_path.with(ident.as_str());

                for item in body.iter_items() {
                    self.resolve_interface_body(&path, item);
                }
            }
            Statement::Function {
                return_type,
                ident,
                body,
                ..
            } => {
                let path = current_path.with(ident.as_str());

                if let Some(body) = body {
                    self.resolve_statement(&path, body);
                }

                self.resolve_types(current_path, return_type);
            }
            Statement::Modifer(modifier) => {
                self.resolve_modifier(modifier, |resolver, stmt| {
                    resolver.resolve_statement(current_path, stmt);
                });
            }
            Statement::VariableDeclaration { ty, .. } => {
                self.resolve_types(current_path, ty);
            }
            Statement::Expression(expression) => self.resolve_expression(current_path, expression),
            _ => (),
        }
    }

    fn resolve_expression(&mut self, current_path: &Path, expression: &Expression) {
        match expression {
            Expression::Block(block) => block
                .iter_items()
                .for_each(|stmt| self.resolve_statement(current_path, stmt)),
            Expression::AnonFunction {
                parameters,
                expression,
                ..
            } => {
                for param in parameters.iter_items() {
                    println!();
                    self.resolve_types(current_path, &param.ty);
                }

                self.resolve_expression(current_path, expression);
            }
            Expression::ClassInitializer { struct_type, .. } => {
                self.resolve_types(current_path, &struct_type);
            }
            Expression::BinaryExpression { left, right, .. } => {
                if let Some(left) = left {
                    self.resolve_expression(current_path, &left);
                }

                if let Some(right) = right {
                    self.resolve_expression(current_path, &right);
                }
            }
            Expression::UnaryExpression {
                expr: Some(expr), ..
            } => self.resolve_expression(current_path, &expr),
            Expression::Tuple(exprs) => exprs.iter().for_each(|expr| {
                self.resolve_expression(current_path, expr);
            }),
            Expression::Array(exprs) => exprs.iter_items().for_each(|expr| {
                self.resolve_expression(current_path, expr);
            }),
            Expression::Index {
                expression,
                indexer,
                ..
            } => {
                self.resolve_expression(current_path, expression);
                if let Some(indexer) = indexer {
                    self.resolve_expression(current_path, indexer);
                }
            }
            Expression::If {
                expression,
                body,
                else_clause,
                ..
            } => {
                self.resolve_expression(current_path, expression);
                self.resolve_expression(current_path, body);
                if let Some(else_clause) = else_clause {
                    self.resolve_expression(current_path, &else_clause.body);
                }
            }
            Expression::ForLoop {
                expression, body, ..
            } => {
                self.resolve_expression(current_path, expression);
                self.resolve_statement(current_path, body);
            }
            Expression::WhileLoop {
                expression, body, ..
            } => {
                self.resolve_expression(current_path, expression);
                self.resolve_statement(current_path, body);
            }
            Expression::Match {
                expression, body, ..
            } => {
                self.resolve_expression(current_path, expression);

                fn resolve_match_binding(
                    resolve: &mut Resolve<'_, GenericPass>,
                    current_path: &Path,
                    binding: &MatchBinding,
                ) {
                    match binding {
                        MatchBinding::Expression(expression) => {
                            resolve.resolve_expression(current_path, expression)
                        }
                        MatchBinding::Tuple(vals) => vals
                            .iter_items()
                            .for_each(|expr| resolve_match_binding(resolve, current_path, expr)),
                        _ => (),
                    }
                }

                match body {
                    MatchBody::Patterns(entries) => {
                        for entry in entries.items.iter_items() {
                            resolve_match_binding(self, current_path, &entry.binding);
                            self.resolve_expression(current_path, &entry.expression);
                        }
                    }
                    MatchBody::AsBinding { binding, .. } => {
                        resolve_match_binding(self, current_path, &binding);
                    }
                }
            }
            _ => (),
        }
    }

    fn resolve_class_body(&mut self, current_path: &Path, class_body: &ClassBody) {
        match class_body {
            ClassBody::ComputedField {
                field_type,
                expression,
                ..
            } => {
                self.resolve_types(current_path, field_type);
                self.resolve_expression(current_path, &expression);
            }
            ClassBody::Field {
                field_type,
                default,
                ..
            } => {
                self.resolve_types(current_path, field_type);
                if let Some((_, expr)) = default {
                    self.resolve_expression(current_path, expr);
                }
            }
            ClassBody::Function { ident, body, .. } => {
                let path = current_path.with(ident.as_str());

                if let Some(body) = body {
                    self.resolve_statement(&path, body);
                }
            }
            ClassBody::GetterSetters {
                field_type,
                entries,
                ..
            } => {
                self.resolve_types(current_path, field_type);

                for entry in entries.iter_items() {
                    match entry {
                        tl_core::ast::GetterSetter::Get { body, .. } => {
                            self.resolve_expression(current_path, body)
                        }
                        tl_core::ast::GetterSetter::Set { body, .. } => {
                            self.resolve_expression(current_path, body)
                        }
                    }
                }
            }
            ClassBody::Modifier(modifier) => {
                self.resolve_modifier(modifier, |resolver, stmt| {
                    resolver.resolve_class_body(current_path, stmt);
                });
            }
        }
    }

    fn resolve_interface_body(&mut self, current_path: &Path, interface_body: &InterfaceBody) {
        match interface_body {
            InterfaceBody::Function {
                return_type,
                parameters,
                ..
            } => {
                self.resolve_types(current_path, return_type);
                for param in parameters.iter_items() {
                    self.resolve_types(current_path, &param.ty);
                }
            }
            InterfaceBody::ComputedField { field_type, .. } => {
                self.resolve_types(current_path, field_type);
            }
            InterfaceBody::Modifier(modifier) => {
                self.resolve_modifier(modifier, |resolver, stmt| {
                    resolver.resolve_interface_body(current_path, stmt);
                });
            }
        }
    }
}

impl<'ctx, P: ResolvePass> Resolve<'ctx, P> {
    fn resolve_types(&mut self, current_path: &Path, ty: &tl_core::ast::Type) -> Type<'ctx> {
        match ty {
            tl_core::ast::Type::None(_) => self
                .codegen_module
                .context
                .void_type()
                .path(Path::std("none")),
            tl_core::ast::Type::Integer { width, signed, .. } => self
                .codegen_module
                .context
                .custom_width_int_type(*width as _)
                .signed(*signed)
                .path(if *signed {
                    Path::std(&format!("int{}", width))
                } else {
                    Path::std(&format!("uint{}", width))
                }),
            tl_core::ast::Type::IntegerPointer { signed, .. } => self
                .codegen_module
                .context
                .ptr_sized_int_type(&self.codegen_module.target_machine.get_target_data(), None)
                .signed(*signed)
                .path(Path::std(if *signed { "iptr" } else { "uptr" })),
            tl_core::ast::Type::Float { width, .. } => {
                let llvm_ty = match *width {
                    16 => self.codegen_module.context.f16_type(),
                    32 => self.codegen_module.context.f32_type(),
                    64 => self.codegen_module.context.f64_type(),
                    128 => self.codegen_module.context.f128_type(),
                    _ => panic!("Unknown float type"),
                };
                llvm_ty
                    .as_type()
                    .path(Path::std(&format!("float{}", width)))
            }
            tl_core::ast::Type::Boolean(_) => self
                .codegen_module
                .context
                .i8_type()
                .as_type()
                .path(Path::std("bool")),
            tl_core::ast::Type::Char { width, .. } => self
                .codegen_module
                .context
                .custom_width_int_type(*width as _)
                .signed(false)
                .path(Path::std(&format!("char{}", width))),
            tl_core::ast::Type::Ref {
                mutable,
                base_type: Some(ty),
                ..
            } => {
                let base = self.resolve_types(current_path, ty);

                base.ptr_type(Default::default()).mutable(*mutable).path(
                    Path::std(if *mutable {
                        "mut_reference"
                    } else {
                        "reference"
                    })
                    .with_generic([base.path.clone().into()].into_iter()),
                )
            }
            tl_core::ast::Type::Array {
                size, base_type, ..
            } => {
                if let Some(size) = size {
                    let ce = const_eval::ConstEval::new(&self.codegen_module);
                    let expr = ce
                        .evaluate_expression(size)
                        .expect("Unable to evaluate constant expression");

                    println!("{}", expr.llvm);

                    if !expr.llvm.is_int_value() {
                        panic!("Expected array size to be an integer!")
                    }

                    let int = expr
                        .llvm
                        .into_int_value()
                        .get_zero_extended_constant()
                        .expect("Expected a constant int!");

                    self.resolve_types(current_path, &base_type)
                        .array_type(int as _)
                } else {
                    self.resolve_types(current_path, &base_type)
                        .ptr_type(Default::default())
                }
            }
            tl_core::ast::Type::Ident(ident) => {
                let path = Path::empty().with(ident.as_str());

                println!("{self}");

                let path = self
                    .resolve_path(current_path, &path)
                    .expect("Unable to resolve path while resolving generic type!");

                let stored_type = self.types.get(&path).unwrap().clone();

                stored_type.symbol.clone()
            }
            tl_core::ast::Type::Path(path) => {
                let path = path
                    .iter_items()
                    .map(|item| item.as_str().to_string())
                    .into();

                let path = self
                    .resolve_path(current_path, &path)
                    .expect("Unable to resolve path while resolving generic type!");

                let stored_type = self.types.get(&path).unwrap().clone();

                stored_type.symbol.clone()
            }
            tl_core::ast::Type::Generic {
                base_type: Some(ty),
                list,
            } => {
                let path = match &**ty {
                    tl_core::ast::Type::Ident(ident) => Path::empty().with(ident.as_str()),
                    tl_core::ast::Type::Path(params) => params
                        .iter_items()
                        .map(|item| item.as_str().to_string())
                        .into(),
                    tl_core::ast::Type::Fixed { width, .. } => {
                        Path::std(&format!("fixed{}", width))
                    }
                    ty => panic!("Expected path or name! Found {:?}", ty.format()),
                };

                let path = self
                    .resolve_path(current_path, &path)
                    .expect("Unable to resolve path while resolving generic type!");

                let stored_type = self.types.get(&path).unwrap().clone();

                let path = path.with_generic(list.iter_items().map(|param| {
                    let mapped_type = self.resolve_types(current_path, &param);
                    GenericArgument::Path(mapped_type.path.clone())
                }));

                // This generic type was already constructed
                if let Some(entry) = self.types.get(&path) {
                    return entry.symbol.clone();
                }

                let ty = self
                    .codegen_module
                    .context
                    .opaque_struct_type(&path.mangle())
                    .path(path.clone());

                self.types.insert(
                    path.clone(),
                    SymbolEntry {
                        symbol: ty.clone(),
                        ..stored_type
                    },
                );

                ty
            }
            c => todo!("Not implemented for {}", c.format()),
        }
    }
}

impl<'ctx> Resolver for Resolve<'ctx, TypeAliasPass> {
    fn resolve_statement(&mut self, current_path: &Path, statement: &Statement) {
        match statement {
            Statement::TypeAlias { ident, ty, .. } => {
                let path = current_path.with(ident.as_str());

                let ty = self.resolve_types(current_path, ty);

                self.types.insert(path, self.make_entry(ty));
            }
            _ => (),
        }
    }
}

impl<'ctx> Resolver for Resolve<'ctx, FunctionPass> {
    fn resolve_statement(&mut self, current_path: &Path, statement: &Statement) {
        match statement {
            Statement::Function {
                return_type,
                ident,
                parameters,
                generic,
                ..
            } => {
                use inkwell::types::BasicType;

                if generic.is_some() {
                    todo!("Generic functions not implemented yet");
                }

                let path = current_path.with(ident.as_str());

                let parameters: Vec<_> = parameters
                    .iter_items()
                    .map(|param| self.resolve_types(current_path, &param.ty))
                    .collect();

                let return_ty = self.resolve_types(current_path, return_type);

                let llvm_params: Vec<_> = parameters
                    .iter()
                    .map(|ty| {
                        inkwell::types::BasicMetadataTypeEnum::try_from(ty.llvm)
                            .expect("Unable to convert to basic metadata type!")
                    })
                    .collect();

                // Void type requires explicit fn_type call
                let ty = match return_ty.llvm {
                    inkwell::types::AnyTypeEnum::VoidType(ty) => ty.fn_type(&llvm_params, false),
                    ty => inkwell::types::BasicTypeEnum::try_from(ty)
                        .expect("Unable to convert to basic type!")
                        .fn_type(&llvm_params, false),
                };

                // We add to the module with a mangled name and insert into the module map with the path
                let func = self
                    .codegen_module
                    .module
                    .add_function(&path.mangle(), ty, None);

                self.functions.insert(path, self.make_entry(func));
            }
            _ => (),
        }
    }
}

impl<P: ResolvePass> Display for Resolve<'_, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("types:\n")?;

        for ty in &self.types {
            writeln!(
                f,
                "    {}{}: {:?}",
                ty.1,
                self.format_path(&ty.0),
                ty.1.symbol
            )?;
        }

        f.write_str("modules:\n")?;

        for module in &self.modules {
            writeln!(f, "    {}", self.format_path(&module.0))?;
        }

        f.write_str("functions:\n")?;

        for func in &self.functions {
            writeln!(
                f,
                "    {}{}: {:?}",
                func.1,
                self.format_path(&func.0),
                func.1.symbol
            )?;
        }

        f.write_str("variables:\n")?;

        for var in &self.variables {
            writeln!(
                f,
                "    {}{}: {:?}",
                var.1,
                self.format_path(&var.0),
                var.1.symbol
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_path() {}
}
