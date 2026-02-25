/// Aver static type checker.
///
/// Two-phase analysis:
///   Phase 1 — build a signature table from all FnDef nodes and builtins.
///   Phase 2 — check top-level statements, then each FnDef for call-site
///              argument types, return type, BinOp compatibility, and effects.
///
/// The checker is deliberately lenient: `Type::Any` is compatible with
/// everything, so partially-typed programs still pass.  Full strictness
/// requires concrete annotations on every function.
use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::ast::{BinOp, Expr, FnBody, FnDef, Module, Pattern, Stmt, TopLevel, TypeDef};
use crate::source::{canonicalize_path, find_module_file, parse_source};
use crate::types::{parse_type_str_strict, Type};

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
}

pub fn run_type_check(items: &[TopLevel]) -> Vec<TypeError> {
    run_type_check_with_base(items, None)
}

pub fn run_type_check_with_base(items: &[TopLevel], base_dir: Option<&str>) -> Vec<TypeError> {
    let mut checker = TypeChecker::new();
    checker.check(items, base_dir);
    checker.errors
}

// ---------------------------------------------------------------------------
// Internal structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Type>,
    ret: Type,
    effects: Vec<String>,
}

struct TypeChecker {
    fn_sigs: HashMap<String, FnSig>,
    module_sig_cache: HashMap<String, Vec<(String, FnSig)>>,
    value_members: HashMap<String, Type>,
    /// Top-level bindings visible from function bodies.
    globals: HashMap<String, (Type, bool)>,
    /// name → (type, is_mutable)
    locals: HashMap<String, (Type, bool)>,
    errors: Vec<TypeError>,
    /// Return type of the function currently being checked; None at top level.
    current_fn_ret: Option<Type>,
}

impl TypeChecker {
    fn new() -> Self {
        let mut tc = TypeChecker {
            fn_sigs: HashMap::new(),
            module_sig_cache: HashMap::new(),
            value_members: HashMap::new(),
            globals: HashMap::new(),
            locals: HashMap::new(),
            errors: Vec::new(),
            current_fn_ret: None,
        };
        tc.register_builtins();
        tc
    }

    fn error(&mut self, msg: impl Into<String>) {
        self.errors.push(TypeError {
            message: msg.into(),
        });
    }

    fn insert_sig(&mut self, name: &str, params: &[Type], ret: Type, effects: &[&str]) {
        self.fn_sigs.insert(
            name.to_string(),
            FnSig {
                params: params.to_vec(),
                ret,
                effects: effects.iter().map(|s| s.to_string()).collect(),
            },
        );
    }

    fn fn_type_from_sig(sig: &FnSig) -> Type {
        Type::Fn(
            sig.params.clone(),
            Box::new(sig.ret.clone()),
            sig.effects.clone(),
        )
    }

    fn sig_from_callable_type(ty: &Type) -> Option<FnSig> {
        match ty {
            Type::Fn(params, ret, effects) => Some(FnSig {
                params: params.clone(),
                ret: (*ret.clone()),
                effects: effects.clone(),
            }),
            _ => None,
        }
    }

    fn binding_type(&self, name: &str) -> Option<Type> {
        self.locals
            .get(name)
            .or_else(|| self.globals.get(name))
            .map(|(ty, _)| ty.clone())
    }

    // -----------------------------------------------------------------------
    // Builtin signatures
    // -----------------------------------------------------------------------
    fn register_builtins(&mut self) {
        let any = || Type::Any;
        let sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("print", &[Type::Any], Type::Unit, &["Console"]),
            ("str", &[Type::Any], Type::Str, &[]),
            ("int", &[Type::Any], Type::Int, &[]),
            ("abs", &[Type::Any], Type::Any, &[]),
            ("len", &[Type::Any], Type::Int, &[]),
            (
                "map",
                &[Type::Any, Type::Any],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "filter",
                &[Type::Any, Type::Any],
                Type::List(Box::new(any())),
                &[],
            ),
            ("fold", &[Type::Any, Type::Any, Type::Any], any(), &[]),
            (
                "get",
                &[Type::Any, Type::Int],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
            (
                "push",
                &[Type::Any, Type::Any],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "head",
                &[Type::Any],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
            (
                "tail",
                &[Type::Any],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
        ];
        for (name, params, ret, effects) in sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        let service_sigs: &[(&str, &[Type], Type, &[&str])] =
            &[("Console.print", &[Type::Any], Type::Unit, &["Console"])];
        for (name, params, ret, effects) in service_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }
    }

    // -----------------------------------------------------------------------
    // Phase 1 — build signature table from program FnDefs
    // -----------------------------------------------------------------------
    fn build_signatures(&mut self, items: &[TopLevel]) {
        for item in items {
            match item {
                TopLevel::FnDef(f) => {
                    let mut params = Vec::new();
                    for (param_name, ty_str) in &f.params {
                        match parse_type_str_strict(ty_str) {
                            Ok(ty) => params.push(ty),
                            Err(unknown) => {
                                self.error(format!(
                                    "Function '{}': unknown type '{}' for parameter '{}'",
                                    f.name, unknown, param_name
                                ));
                                params.push(Type::Any);
                            }
                        }
                    }
                    let ret = match parse_type_str_strict(&f.return_type) {
                        Ok(ty) => ty,
                        Err(unknown) => {
                            self.error(format!(
                                "Function '{}': unknown return type '{}'",
                                f.name, unknown
                            ));
                            Type::Any
                        }
                    };
                    self.fn_sigs.insert(
                        f.name.clone(),
                        FnSig {
                            params,
                            ret,
                            effects: f.effects.clone(),
                        },
                    );
                }
                TopLevel::TypeDef(td) => {
                    self.register_type_def_sigs(td);
                }
                _ => {}
            }
        }
    }

    /// Register constructor signatures for user-defined types.
    fn register_type_def_sigs(&mut self, td: &TypeDef) {
        match td {
            TypeDef::Sum {
                name: type_name,
                variants,
            } => {
                // Register the type name in fn_sigs so `Ident("Shape")` resolves
                // to Named("Shape") without error (checked after locals in infer_type).
                self.fn_sigs.insert(
                    type_name.clone(),
                    FnSig {
                        params: vec![],
                        ret: Type::Named(type_name.clone()),
                        effects: vec![],
                    },
                );
                // Register each constructor with a qualified key: "Shape.Circle"
                for variant in variants {
                    let params: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Any))
                        .collect();
                    let key = format!("{}.{}", type_name, variant.name);
                    if params.is_empty() {
                        // Zero-arg constructors are values in Aver (`Shape.Point`), not functions.
                        self.value_members
                            .insert(key, Type::Named(type_name.clone()));
                    } else {
                        self.fn_sigs.insert(
                            key,
                            FnSig {
                                params,
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                    }
                }
            }
            TypeDef::Product {
                name: type_name,
                fields,
            } => {
                // Record constructors are handled via Expr::RecordCreate, not FnCall.
                // Register a dummy sig so Ident("TypeName") resolves to Named(type_name).
                let params: Vec<Type> = fields
                    .iter()
                    .map(|(_, ty_str)| parse_type_str_strict(ty_str).unwrap_or(Type::Any))
                    .collect();
                self.fn_sigs.insert(
                    type_name.clone(),
                    FnSig {
                        params,
                        ret: Type::Named(type_name.clone()),
                        effects: vec![],
                    },
                );
            }
        }
    }

    fn module_decl(items: &[TopLevel]) -> Option<&Module> {
        items.iter().find_map(|item| {
            if let TopLevel::Module(m) = item {
                Some(m)
            } else {
                None
            }
        })
    }

    fn exposed_set(items: &[TopLevel]) -> Option<HashSet<String>> {
        Self::module_decl(items).and_then(|m| {
            if m.exposes.is_empty() {
                None
            } else {
                Some(m.exposes.iter().cloned().collect())
            }
        })
    }

    fn module_cache_key(path: &Path) -> String {
        canonicalize_path(path).to_string_lossy().to_string()
    }

    fn attr_path(expr: &Expr) -> Option<Vec<String>> {
        match expr {
            Expr::Ident(name) => Some(vec![name.clone()]),
            Expr::Attr(inner, field) => {
                let mut parts = Self::attr_path(inner)?;
                parts.push(field.clone());
                Some(parts)
            }
            _ => None,
        }
    }

    fn attr_key(expr: &Expr) -> Option<String> {
        Self::attr_path(expr).map(|parts| parts.join("."))
    }

    fn has_namespace_prefix(&self, key: &str) -> bool {
        let prefix = format!("{}.", key);
        self.fn_sigs.keys().any(|k| k.starts_with(&prefix))
    }

    fn cycle_display(loading: &[String], next: &str) -> String {
        let mut chain = loading
            .iter()
            .map(|key| {
                Path::new(key)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(key)
                    .to_string()
            })
            .collect::<Vec<_>>();
        chain.push(
            Path::new(next)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(next)
                .to_string(),
        );
        chain.join(" -> ")
    }

    fn load_module_sigs(
        &mut self,
        name: &str,
        base_dir: &str,
        loading: &mut Vec<String>,
    ) -> Result<(), String> {
        let path = find_module_file(name, base_dir)
            .ok_or_else(|| format!("Module '{}' not found in '{}'", name, base_dir))?;
        let cache_key = Self::module_cache_key(&path);

        if let Some(entries) = self.module_sig_cache.get(&cache_key).cloned() {
            for (key, sig) in entries {
                self.fn_sigs.insert(key, sig);
            }
            return Ok(());
        }

        if loading.contains(&cache_key) {
            return Err(format!(
                "Circular import: {}",
                Self::cycle_display(loading, &cache_key)
            ));
        }

        loading.push(cache_key.clone());
        let result = (|| -> Result<Vec<(String, FnSig)>, String> {
            let src = std::fs::read_to_string(&path)
                .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
            let items = parse_source(&src)
                .map_err(|e| format!("Parse error in '{}': {}", path.display(), e))?;

            if let Some(module) = Self::module_decl(&items) {
                let expected = name.rsplit('.').next().unwrap_or(name);
                if module.name != expected {
                    return Err(format!(
                        "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                        expected,
                        name,
                        module.name,
                        path.display()
                    ));
                }
                for dep_name in &module.depends {
                    self.load_module_sigs(dep_name, base_dir, loading)?;
                }
            }

            let exposed = Self::exposed_set(&items);
            let mut entries = Vec::new();
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    let include = match &exposed {
                        Some(set) => set.contains(&fd.name),
                        None => !fd.name.starts_with('_'),
                    };
                    if !include {
                        continue;
                    }

                    let mut params = Vec::new();
                    for (param_name, ty_str) in &fd.params {
                        let ty = parse_type_str_strict(ty_str).map_err(|unknown| {
                            format!(
                                "Module '{}', function '{}': unknown type '{}' for parameter '{}'",
                                name, fd.name, unknown, param_name
                            )
                        })?;
                        params.push(ty);
                    }

                    let ret = parse_type_str_strict(&fd.return_type).map_err(|unknown| {
                        format!(
                            "Module '{}', function '{}': unknown return type '{}'",
                            name, fd.name, unknown
                        )
                    })?;

                    entries.push((
                        format!("{}.{}", name, fd.name),
                        FnSig {
                            params,
                            ret,
                            effects: fd.effects.clone(),
                        },
                    ));
                }
            }

            Ok(entries)
        })();
        loading.pop();

        let entries = result?;
        for (key, sig) in &entries {
            self.fn_sigs.insert(key.clone(), sig.clone());
        }
        self.module_sig_cache.insert(cache_key, entries);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Phase 2 — check each function
    // -----------------------------------------------------------------------
    fn check_fn(&mut self, f: &FnDef) {
        // Start with globals and overlay parameter bindings.
        self.locals = self.globals.clone();
        if let Some(sig) = self.fn_sigs.get(&f.name).cloned() {
            for ((param_name, _), param_type) in f.params.iter().zip(sig.params.iter()) {
                // params are immutable (like val)
                self.locals
                    .insert(param_name.clone(), (param_type.clone(), false));
            }

            let declared_ret = sig.ret.clone();
            let declared_effects = sig.effects.clone();

            self.current_fn_ret = Some(declared_ret.clone());

            match &f.body {
                FnBody::Expr(expr) => {
                    let inferred = self.infer_type(expr);
                    if !inferred.compatible(&declared_ret) {
                        self.error(format!(
                            "Function '{}': body returns {} but declared return type is {}",
                            f.name,
                            inferred.display(),
                            declared_ret.display()
                        ));
                    }
                    // Check effect propagation in expression
                    self.check_effects_in_expr(expr, &f.name, &declared_effects);
                }
                FnBody::Block(stmts) => {
                    let last_type = self.check_stmts(stmts, &f.name, &declared_effects);
                    if !last_type.compatible(&declared_ret) {
                        self.error(format!(
                            "Function '{}': body returns {} but declared return type is {}",
                            f.name,
                            last_type.display(),
                            declared_ret.display()
                        ));
                    }
                }
            }

            self.current_fn_ret = None;
        }
    }

    fn check_top_level_stmts(&mut self, items: &[TopLevel]) {
        self.locals.clear();
        for item in items {
            if let TopLevel::Stmt(stmt) = item {
                match stmt {
                    Stmt::Val(name, expr) => {
                        let ty = self.infer_type(expr);
                        self.locals.insert(name.clone(), (ty, false));
                    }
                    Stmt::Var(name, expr, _) => {
                        let ty = self.infer_type(expr);
                        self.locals.insert(name.clone(), (ty, true));
                    }
                    Stmt::Assign(name, expr) => {
                        let rhs_ty = self.infer_type(expr);
                        match self.locals.get(name).cloned() {
                            None => {
                                self.error(format!(
                                    "Assignment to undeclared variable '{}' in <top-level>",
                                    name
                                ));
                            }
                            Some((_, false)) => {
                                self.error(format!(
                                    "Cannot assign to '{}' in <top-level>: declared with val (immutable)",
                                    name
                                ));
                            }
                            Some((var_ty, true)) => {
                                if !rhs_ty.compatible(&var_ty) {
                                    self.error(format!(
                                        "Assignment to '{}' in <top-level>: expected {}, got {}",
                                        name,
                                        var_ty.display(),
                                        rhs_ty.display()
                                    ));
                                }
                            }
                        }
                    }
                    Stmt::Expr(expr) => {
                        let _ = self.infer_type(expr);
                    }
                }
            }
        }
        self.globals = self.locals.clone();
    }

    fn check_stmts(&mut self, stmts: &[Stmt], fn_name: &str, caller_effects: &[String]) -> Type {
        let mut last = Type::Unit;
        for stmt in stmts {
            match stmt {
                Stmt::Val(name, expr) => {
                    let ty = self.infer_type(expr);
                    self.check_effects_in_expr(expr, fn_name, caller_effects);
                    self.locals.insert(name.clone(), (ty, false));
                    last = Type::Unit;
                }
                Stmt::Var(name, expr, _) => {
                    let ty = self.infer_type(expr);
                    self.check_effects_in_expr(expr, fn_name, caller_effects);
                    self.locals.insert(name.clone(), (ty, true));
                    last = Type::Unit;
                }
                Stmt::Assign(name, expr) => {
                    let rhs_ty = self.infer_type(expr);
                    self.check_effects_in_expr(expr, fn_name, caller_effects);
                    match self.locals.get(name).cloned() {
                        None => {
                            self.error(format!(
                                "Assignment to undeclared variable '{}' in '{}'",
                                name, fn_name
                            ));
                        }
                        Some((_, false)) => {
                            self.error(format!(
                                "Cannot assign to '{}' in '{}': declared with val (immutable)",
                                name, fn_name
                            ));
                        }
                        Some((var_ty, true)) => {
                            if !rhs_ty.compatible(&var_ty) {
                                self.error(format!(
                                    "Assignment to '{}' in '{}': expected {}, got {}",
                                    name,
                                    fn_name,
                                    var_ty.display(),
                                    rhs_ty.display()
                                ));
                            }
                        }
                    }
                    last = Type::Unit;
                }
                Stmt::Expr(expr) => {
                    last = self.infer_type(expr);
                    self.check_effects_in_expr(expr, fn_name, caller_effects);
                }
            }
        }
        last
    }

    // -----------------------------------------------------------------------
    // Effect propagation: ERROR (not warning) if callee has effect caller lacks
    // -----------------------------------------------------------------------
    fn callee_key(fn_expr: &Expr) -> Option<String> {
        Self::attr_key(fn_expr)
    }

    fn callable_effects(&self, fn_expr: &Expr) -> Option<(String, Vec<String>)> {
        if let Some(callee_name) = Self::callee_key(fn_expr) {
            if let Some(callee_sig) = self.fn_sigs.get(&callee_name) {
                return Some((callee_name, callee_sig.effects.clone()));
            }
        }
        if let Expr::Ident(name) = fn_expr {
            if let Some(ty) = self.binding_type(name) {
                if let Type::Fn(_, _, effects) = ty {
                    return Some((name.clone(), effects));
                }
            }
        }
        None
    }

    fn check_effects_in_expr(&mut self, expr: &Expr, caller_name: &str, caller_effects: &[String]) {
        match expr {
            Expr::FnCall(fn_expr, args) => {
                if let Some((callee_name, effects)) = self.callable_effects(fn_expr) {
                    for effect in &effects {
                        if !caller_effects.contains(effect) {
                            self.error(format!(
                                "Function '{}' calls '{}' which has effect '{}', but '{}' does not declare it",
                                caller_name, callee_name, effect, caller_name
                            ));
                        }
                    }
                }
                self.check_effects_in_expr(fn_expr, caller_name, caller_effects);
                for arg in args {
                    self.check_effects_in_expr(arg, caller_name, caller_effects);
                }
            }
            Expr::BinOp(_, left, right) => {
                self.check_effects_in_expr(left, caller_name, caller_effects);
                self.check_effects_in_expr(right, caller_name, caller_effects);
            }
            Expr::Pipe(left, right) => {
                self.check_effects_in_expr(left, caller_name, caller_effects);
                // x |> f counts as calling f — check f's effects
                if let Some((callee_name, effects)) = self.callable_effects(right) {
                    for effect in &effects {
                        if !caller_effects.contains(effect) {
                            self.error(format!(
                                "Function '{}' pipes into '{}' which has effect '{}', but '{}' does not declare it",
                                caller_name, callee_name, effect, caller_name
                            ));
                        }
                    }
                }
                self.check_effects_in_expr(right, caller_name, caller_effects);
            }
            Expr::Match(subject, arms) => {
                self.check_effects_in_expr(subject, caller_name, caller_effects);
                for arm in arms {
                    self.check_effects_in_expr(&arm.body, caller_name, caller_effects);
                }
            }
            Expr::Constructor(_, Some(inner)) => {
                self.check_effects_in_expr(inner, caller_name, caller_effects);
            }
            Expr::ErrorProp(inner) => {
                self.check_effects_in_expr(inner, caller_name, caller_effects);
            }
            Expr::List(elems) => {
                for elem in elems {
                    self.check_effects_in_expr(elem, caller_name, caller_effects);
                }
            }
            Expr::Attr(obj, _) => {
                self.check_effects_in_expr(obj, caller_name, caller_effects);
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, expr) in fields {
                    self.check_effects_in_expr(expr, caller_name, caller_effects);
                }
            }
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // Type inference for expressions
    // -----------------------------------------------------------------------
    fn infer_type(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(_) => Type::Int,
                crate::ast::Literal::Float(_) => Type::Float,
                crate::ast::Literal::Str(_) => Type::Str,
                crate::ast::Literal::Bool(_) => Type::Bool,
            },

            Expr::InterpolatedStr(_) => Type::Str,

            Expr::Ident(name) => {
                if let Some((ty, _)) = self.locals.get(name) {
                    ty.clone()
                } else if let Some(sig) = self.fn_sigs.get(name) {
                    Self::fn_type_from_sig(sig)
                } else {
                    self.error(format!("Unknown identifier '{}'", name));
                    Type::Any
                }
            }

            Expr::FnCall(fn_expr, args) => {
                // Infer arg types
                let arg_types: Vec<Type> = args.iter().map(|a| self.infer_type(a)).collect();

                // Helper: check arity + arg types against a sig, return sig.ret
                let check_call = |tc: &mut Self, display_name: &str, sig: FnSig| -> Type {
                    if arg_types.len() != sig.params.len() {
                        tc.error(format!(
                            "Function '{}' expects {} argument(s), got {}",
                            display_name,
                            sig.params.len(),
                            arg_types.len()
                        ));
                    } else {
                        for (i, (arg_ty, param_ty)) in
                            arg_types.iter().zip(sig.params.iter()).enumerate()
                        {
                            if !arg_ty.compatible(param_ty) {
                                tc.error(format!(
                                    "Argument {} of '{}': expected {}, got {}",
                                    i + 1,
                                    display_name,
                                    param_ty.display(),
                                    arg_ty.display()
                                ));
                            }
                        }
                    }
                    sig.ret
                };

                if let Expr::Ident(name) = fn_expr.as_ref() {
                    if let Some(sig) = self.fn_sigs.get(name).cloned() {
                        return check_call(self, name, sig);
                    }
                    if let Some(binding_ty) = self.binding_type(name) {
                        if let Some(sig) = Self::sig_from_callable_type(&binding_ty) {
                            return check_call(self, name, sig);
                        }
                        self.error(format!(
                            "Cannot call '{}': expected function, got {}",
                            name,
                            binding_ty.display()
                        ));
                        return Type::Any;
                    }
                    self.error(format!("Call to unknown function '{}'", name));
                    return Type::Any;
                }

                if let Some(display_name) = Self::callee_key(fn_expr) {
                    if let Some(sig) = self.fn_sigs.get(&display_name).cloned() {
                        return check_call(self, &display_name, sig);
                    }
                }

                let callee_ty = self.infer_type(fn_expr);
                if let Some(sig) = Self::sig_from_callable_type(&callee_ty) {
                    return check_call(self, "<fn value>", sig);
                }

                if !matches!(callee_ty, Type::Any) {
                    self.error(format!("Cannot call value of type {}", callee_ty.display()));
                }
                Type::Any
            }

            Expr::BinOp(op, left, right) => {
                let lt = self.infer_type(left);
                let rt = self.infer_type(right);
                self.check_binop(op, &lt, &rt);
                match op {
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                        Type::Bool
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        // Promote to Float if either side is Float
                        if matches!(lt, Type::Float) || matches!(rt, Type::Float) {
                            Type::Float
                        } else if matches!(lt, Type::Int) && matches!(rt, Type::Int) {
                            Type::Int
                        } else if matches!(lt, Type::Str)
                            && matches!(rt, Type::Str)
                            && matches!(op, BinOp::Add)
                        {
                            Type::Str
                        } else {
                            Type::Any
                        }
                    }
                }
            }

            Expr::Constructor(name, arg) => match name.as_str() {
                "Ok" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(inner), Box::new(Type::Any))
                }
                "Err" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(Type::Any), Box::new(inner))
                }
                "Some" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Option(Box::new(inner))
                }
                "None" => Type::Option(Box::new(Type::Any)),
                _ => Type::Any,
            },

            Expr::List(elems) => {
                let inner = if let Some(first) = elems.first() {
                    self.infer_type(first)
                } else {
                    Type::Any
                };
                Type::List(Box::new(inner))
            }

            Expr::Match(subject, arms) => {
                let _ = self.infer_type(subject);
                // Infer from first arm; check remaining arms for consistency
                if let Some(first_arm) = arms.first() {
                    let first_ty =
                        self.infer_type_with_pattern_bindings(&first_arm.pattern, &first_arm.body);
                    for arm in arms.iter().skip(1) {
                        let arm_ty = self.infer_type_with_pattern_bindings(&arm.pattern, &arm.body);
                        // Only report mismatch when both types are concrete
                        if !first_ty.compatible(&arm_ty)
                            && !matches!(first_ty, Type::Any)
                            && !matches!(arm_ty, Type::Any)
                        {
                            self.error(format!(
                                "Match arms return incompatible types: {} vs {}",
                                first_ty.display(),
                                arm_ty.display()
                            ));
                        }
                    }
                    first_ty
                } else {
                    Type::Any
                }
            }

            Expr::Pipe(left, right) => {
                // x |> f is equivalent to f(x)
                let call = Expr::FnCall(Box::new((**right).clone()), vec![(**left).clone()]);
                self.infer_type(&call)
            }

            Expr::ErrorProp(inner) => {
                // expr? unwraps Result<T,E> → T, propagating E as early return.
                let ty = self.infer_type(inner);
                match ty {
                    Type::Result(ok_ty, err_ty) => {
                        match self.current_fn_ret.clone() {
                            Some(Type::Result(_, fn_err_ty)) => {
                                if !err_ty.compatible(&fn_err_ty) {
                                    self.error(format!(
                                        "Operator '?': Err type {} is incompatible with function's Err type {}",
                                        err_ty.display(),
                                        fn_err_ty.display()
                                    ));
                                }
                            }
                            Some(Type::Any) => {} // gradual typing — skip check
                            Some(other) => {
                                self.error(format!(
                                    "Operator '?' used in function returning {}, which is not Result",
                                    other.display()
                                ));
                            }
                            None => {
                                self.error("Operator '?' used outside of a function".to_string());
                            }
                        }
                        *ok_ty
                    }
                    Type::Any => Type::Any,
                    other => {
                        self.error(format!(
                            "Operator '?' can only be applied to Result, got {}",
                            other.display()
                        ));
                        Type::Any
                    }
                }
            }

            Expr::Attr(obj, field) => {
                if let Some(mut parts) = Self::attr_path(obj) {
                    let obj_key = parts.join(".");
                    parts.push(field.clone());
                    let key = parts.join(".");
                    if let Some(ty) = self.value_members.get(&key) {
                        return ty.clone();
                    }
                    if let Some(sig) = self.fn_sigs.get(&key) {
                        return Self::fn_type_from_sig(sig);
                    }
                    if self.has_namespace_prefix(&key) {
                        // Intermediate namespace (e.g. Models.User in Models.User.findById)
                        return Type::Any;
                    }
                    if self.has_namespace_prefix(&obj_key) {
                        self.error(format!(
                            "Unknown member '{}.{}' (not exposed or missing)",
                            obj_key, field
                        ));
                        return Type::Any;
                    }
                }
                let obj_ty = self.infer_type(obj);
                match obj_ty {
                    Type::Named(_) => Type::Any,
                    Type::Any => Type::Any,
                    other => {
                        self.error(format!(
                            "Field access on non-record type {}",
                            other.display()
                        ));
                        Type::Any
                    }
                }
            }

            Expr::RecordCreate { type_name, fields } => {
                for (_, expr) in fields {
                    let _ = self.infer_type(expr);
                }
                Type::Named(type_name.clone())
            }
        }
    }

    fn infer_type_with_pattern_bindings(&mut self, pattern: &Pattern, body: &Expr) -> Type {
        let mut bindings = Vec::new();
        Self::collect_pattern_bindings(pattern, &mut bindings);

        let mut prev = Vec::new();
        for bind_name in bindings {
            let old = self.locals.get(&bind_name).cloned();
            prev.push((bind_name.clone(), old));
            self.locals.insert(bind_name, (Type::Any, false));
        }

        let out_ty = self.infer_type(body);

        for (name, old) in prev {
            if let Some(old_val) = old {
                self.locals.insert(name, old_val);
            } else {
                self.locals.remove(&name);
            }
        }

        out_ty
    }

    fn collect_pattern_bindings(pattern: &Pattern, out: &mut Vec<String>) {
        match pattern {
            Pattern::Ident(name) if name != "_" => out.push(name.clone()),
            Pattern::Cons(head, tail) => {
                if head != "_" {
                    out.push(head.clone());
                }
                if tail != "_" {
                    out.push(tail.clone());
                }
            }
            Pattern::Constructor(_, bindings) => {
                for name in bindings {
                    if name != "_" {
                        out.push(name.clone());
                    }
                }
            }
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // BinOp type rules
    // -----------------------------------------------------------------------
    fn check_binop(&mut self, op: &BinOp, lt: &Type, rt: &Type) {
        if matches!(lt, Type::Any) || matches!(rt, Type::Any) {
            return; // gradual — skip
        }
        match op {
            BinOp::Add => {
                let ok = (matches!(lt, Type::Int | Type::Float)
                    && matches!(rt, Type::Int | Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error(format!(
                        "Operator '+' requires Int/Float or String on both sides, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let ok =
                    matches!(lt, Type::Int | Type::Float) && matches!(rt, Type::Int | Type::Float);
                if !ok {
                    self.error(format!(
                        "Arithmetic operator requires numeric types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Eq | BinOp::Neq => {
                if !lt.compatible(rt) {
                    self.error(format!(
                        "Equality operator requires same types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                let ok = (matches!(lt, Type::Int | Type::Float)
                    && matches!(rt, Type::Int | Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error(format!(
                        "Comparison operator requires numeric or String types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Entry point
    // -----------------------------------------------------------------------
    fn check(&mut self, items: &[TopLevel], base_dir: Option<&str>) {
        self.build_signatures(items);

        if let Some(base) = base_dir {
            if let Some(module) = Self::module_decl(items) {
                let mut loading = Vec::new();
                for dep_name in &module.depends {
                    if let Err(e) = self.load_module_sigs(dep_name, base, &mut loading) {
                        self.error(e);
                    }
                }
            }
        }

        self.check_top_level_stmts(items);

        for item in items {
            if let TopLevel::FnDef(f) = item {
                self.check_fn(f);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{run_type_check, TypeChecker};
    use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Stmt, TopLevel};

    fn errors(items: Vec<TopLevel>) -> Vec<String> {
        run_type_check(&items)
            .into_iter()
            .map(|e| e.message)
            .collect()
    }

    #[test]
    fn top_level_statements_are_typechecked() {
        let items = vec![TopLevel::Stmt(Stmt::Val(
            "x".to_string(),
            Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Literal(Literal::Str("a".to_string()))),
            ),
        ))];
        let errs = errors(items);
        assert!(
            errs.iter().any(|e| e.contains("Operator '+' requires")),
            "expected top-level BinOp type error, got: {:?}",
            errs
        );
    }

    #[test]
    fn unknown_function_calls_are_errors() {
        let main_fn = FnDef {
            name: "main".to_string(),
            params: vec![],
            return_type: "Unit".to_string(),
            effects: vec![],
            desc: None,
            body: FnBody::Block(vec![Stmt::Expr(Expr::FnCall(
                Box::new(Expr::Ident("nosuch".to_string())),
                vec![Expr::Literal(Literal::Int(1))],
            ))]),
        };

        let errs = errors(vec![TopLevel::FnDef(main_fn)]);
        assert!(
            errs.iter()
                .any(|e| e.contains("Call to unknown function 'nosuch'")),
            "expected unknown function error, got: {:?}",
            errs
        );
    }

    #[test]
    fn functions_can_assign_to_top_level_var_bindings() {
        let top_level_var = TopLevel::Stmt(Stmt::Var(
            "x".to_string(),
            Expr::Literal(Literal::Int(1)),
            None,
        ));
        let main_fn = TopLevel::FnDef(FnDef {
            name: "main".to_string(),
            params: vec![],
            return_type: "Unit".to_string(),
            effects: vec![],
            desc: None,
            body: FnBody::Block(vec![Stmt::Assign(
                "x".to_string(),
                Expr::Literal(Literal::Int(2)),
            )]),
        });

        let errs = errors(vec![top_level_var, main_fn]);
        assert!(
            errs.is_empty(),
            "expected no errors for assignment to top-level var, got: {:?}",
            errs
        );
    }

    #[test]
    fn nested_attr_callee_key() {
        let expr = Expr::Attr(
            Box::new(Expr::Attr(
                Box::new(Expr::Ident("Models".to_string())),
                "User".to_string(),
            )),
            "findById".to_string(),
        );
        assert_eq!(
            TypeChecker::callee_key(&expr),
            Some("Models.User.findById".to_string())
        );
    }
}
