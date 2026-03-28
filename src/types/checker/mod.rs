/// Aver static type checker.
///
/// Two-phase analysis:
///   Phase 1 — build a signature table from all FnDef nodes and builtins.
///   Phase 2 — check top-level statements, then each FnDef for call-site
///              argument types, return type, BinOp compatibility, and effects.
///
/// The checker keeps gradual typing for nested placeholders, but applies
/// stricter rules for checker constraints: a bare `Unknown` does not satisfy
/// a concrete expected type in argument/return/ascription checks.
use std::collections::{HashMap, HashSet};
use std::path::Path;

use super::{Type, parse_type_str_strict};
use crate::ast::{BinOp, Expr, FnDef, Literal, Module, Pattern, Stmt, TopLevel, TypeDef};
use crate::source::{
    canonicalize_path, find_module_file, parse_source, require_module_declaration,
};

mod builtins;
mod exhaustiveness;
mod flow;
mod infer;
mod memo;
mod modules;

#[cfg(test)]
mod tests;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

/// Result of type-checking that also carries memo-safety metadata.
#[derive(Debug)]
pub struct TypeCheckResult {
    pub errors: Vec<TypeError>,
    /// For each user-defined fn: (param_types, return_type, effects).
    /// Used by the memo system to decide which fns qualify.
    pub fn_sigs: HashMap<String, (Vec<Type>, Type, Vec<String>)>,
    /// Set of type names whose values are memo-safe (hashable scalars / records of scalars).
    pub memo_safe_types: HashSet<String>,
    /// Unused binding warnings: (binding_name, fn_name, line).
    pub unused_bindings: Vec<(String, String, usize)>,
}

pub fn run_type_check(items: &[TopLevel]) -> Vec<TypeError> {
    run_type_check_with_base(items, None)
}

pub fn run_type_check_with_base(items: &[TopLevel], base_dir: Option<&str>) -> Vec<TypeError> {
    run_type_check_full(items, base_dir).errors
}

pub fn run_type_check_full(items: &[TopLevel], base_dir: Option<&str>) -> TypeCheckResult {
    let mut checker = TypeChecker::new();
    checker.check(items, base_dir);

    // Export fn_sigs for memo analysis
    let fn_sigs: HashMap<String, (Vec<Type>, Type, Vec<String>)> = checker
        .fn_sigs
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                (v.params.clone(), v.ret.clone(), v.effects.clone()),
            )
        })
        .collect();

    // Compute memo-safe named types
    let memo_safe_types = checker.compute_memo_safe_types(items);

    TypeCheckResult {
        errors: checker.errors,
        fn_sigs,
        memo_safe_types,
        unused_bindings: checker.unused_warnings,
    }
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

#[derive(Debug, Clone)]
struct ModuleSigCache {
    fn_entries: Vec<(String, FnSig)>,
    value_entries: Vec<(String, Type)>,
    record_field_entries: Vec<(String, Type)>,
    type_variants: Vec<(String, Vec<String>)>,
    opaque_types: Vec<String>,
}

struct TypeChecker {
    fn_sigs: HashMap<String, FnSig>,
    module_sig_cache: HashMap<String, ModuleSigCache>,
    value_members: HashMap<String, Type>,
    /// Field types for record types: "TypeName.fieldName" → Type.
    /// Populated for both user-defined `record` types and built-in records
    /// (HttpResponse, Header). Enables checked dot-access on Named types.
    record_field_types: HashMap<String, Type>,
    /// Variant names for sum types: "Shape" → ["Circle", "Rect", "Point"].
    /// Pre-populated for Result and Option; extended by user-defined sum types.
    type_variants: HashMap<String, Vec<String>>,
    /// Top-level bindings visible from function bodies.
    globals: HashMap<String, Type>,
    /// Local bindings in the current function/scope.
    locals: HashMap<String, Type>,
    errors: Vec<TypeError>,
    /// Return type of the function currently being checked; None at top level.
    current_fn_ret: Option<Type>,
    /// Line number of the function currently being checked; None at top level.
    current_fn_line: Option<usize>,
    /// Type names that are opaque in this module's context (imported via `exposes opaque`).
    opaque_types: HashSet<String>,
    /// Names referenced during type checking of current function body (for unused detection).
    used_names: HashSet<String>,
    /// Bindings defined in the current function body: (name, line).
    fn_bindings: Vec<(String, usize)>,
    /// Unused binding warnings collected during checking: (binding_name, fn_name, line).
    unused_warnings: Vec<(String, String, usize)>,
}

impl TypeChecker {
    fn new() -> Self {
        let mut type_variants = HashMap::new();
        type_variants.insert(
            "Result".to_string(),
            vec!["Ok".to_string(), "Err".to_string()],
        );
        type_variants.insert(
            "Option".to_string(),
            vec!["Some".to_string(), "None".to_string()],
        );

        let mut tc = TypeChecker {
            fn_sigs: HashMap::new(),
            module_sig_cache: HashMap::new(),
            value_members: HashMap::new(),
            record_field_types: HashMap::new(),
            type_variants,
            globals: HashMap::new(),
            locals: HashMap::new(),
            errors: Vec::new(),
            current_fn_ret: None,
            current_fn_line: None,
            opaque_types: HashSet::new(),
            used_names: HashSet::new(),
            fn_bindings: Vec::new(),
            unused_warnings: Vec::new(),
        };
        tc.register_builtins();
        tc
    }

    /// Check whether `required_effect` is satisfied by `caller_effects`.
    fn caller_has_effect(&self, caller_effects: &[String], required_effect: &str) -> bool {
        caller_effects
            .iter()
            .any(|declared| crate::effects::effect_satisfies(declared, required_effect))
    }

    fn error(&mut self, msg: impl Into<String>) {
        let line = self.current_fn_line.unwrap_or(1);
        self.errors.push(TypeError {
            message: msg.into(),
            line,
            col: 0,
        });
    }

    fn error_at_line(&mut self, line: usize, msg: impl Into<String>) {
        self.errors.push(TypeError {
            message: msg.into(),
            line,
            col: 0,
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
                ret: *ret.clone(),
                effects: effects.clone(),
            }),
            _ => None,
        }
    }

    fn binding_type(&self, name: &str) -> Option<Type> {
        self.locals
            .get(name)
            .or_else(|| self.globals.get(name))
            .cloned()
    }

    /// Compatibility used for checker constraints (call args, returns, ascriptions).
    ///
    /// We keep gradual typing for nested placeholders (`Result<Int, Unknown>` can
    /// still fit `Result<Int, String>`), but reject *bare* `Unknown` when a
    /// concrete type is required. This closes common false negatives where an
    /// unresolved expression silently passes a concrete signature.
    pub(super) fn constraint_compatible(actual: &Type, expected: &Type) -> bool {
        if matches!(actual, Type::Unknown) && !matches!(expected, Type::Unknown) {
            return false;
        }
        actual.compatible(expected)
    }
}
