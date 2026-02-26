/// Aver static type checker.
///
/// Two-phase analysis:
///   Phase 1 — build a signature table from all FnDef nodes and builtins.
///   Phase 2 — check top-level statements, then each FnDef for call-site
///              argument types, return type, BinOp compatibility, and effects.
///
/// The checker is deliberately lenient: `Type::Unknown` is compatible with
/// everything, so partially-typed programs still pass. Full strictness
/// requires concrete annotations on every function.
use std::collections::{HashMap, HashSet};
use std::path::Path;

use super::{parse_type_str_strict, Type};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Module, Pattern, Stmt, TopLevel, TypeDef};
use crate::source::{canonicalize_path, find_module_file, parse_source};

mod builtins;
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
    pub line: Option<usize>,
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

struct TypeChecker {
    fn_sigs: HashMap<String, FnSig>,
    module_sig_cache: HashMap<String, Vec<(String, FnSig)>>,
    value_members: HashMap<String, Type>,
    /// Field types for record types: "TypeName.fieldName" → Type.
    /// Populated for both user-defined `record` types and built-in records
    /// (HttpResponse, Header). Enables checked dot-access on Named types.
    record_field_types: HashMap<String, Type>,
    /// Named effect aliases: `effects AppIO = [Console, Disk]`
    effect_aliases: HashMap<String, Vec<String>>,
    /// Top-level bindings visible from function bodies.
    globals: HashMap<String, Type>,
    /// Local bindings in the current function/scope.
    locals: HashMap<String, Type>,
    errors: Vec<TypeError>,
    /// Return type of the function currently being checked; None at top level.
    current_fn_ret: Option<Type>,
    /// Line number of the function currently being checked; None at top level.
    current_fn_line: Option<usize>,
}

impl TypeChecker {
    fn new() -> Self {
        let mut tc = TypeChecker {
            fn_sigs: HashMap::new(),
            module_sig_cache: HashMap::new(),
            value_members: HashMap::new(),
            record_field_types: HashMap::new(),
            effect_aliases: HashMap::new(),
            globals: HashMap::new(),
            locals: HashMap::new(),
            errors: Vec::new(),
            current_fn_ret: None,
            current_fn_line: None,
        };
        tc.register_builtins();
        tc
    }

    /// Expand a list of effect names, resolving any aliases one level deep.
    fn expand_effects(&self, effects: &[String]) -> Vec<String> {
        let mut result = Vec::new();
        for e in effects {
            if let Some(expanded) = self.effect_aliases.get(e) {
                result.extend(expanded.iter().cloned());
            } else {
                result.push(e.clone());
            }
        }
        result
    }

    /// Check whether `required_effect` is satisfied by `caller_effects` (with alias expansion).
    fn caller_has_effect(&self, caller_effects: &[String], required_effect: &str) -> bool {
        let expanded_caller = self.expand_effects(caller_effects);
        // Also expand the required effect (in case it's itself an alias)
        let expanded_required = self.expand_effects(&[required_effect.to_string()]);
        expanded_required
            .iter()
            .all(|e| expanded_caller.contains(e))
    }

    fn error(&mut self, msg: impl Into<String>) {
        self.errors.push(TypeError {
            message: msg.into(),
            line: self.current_fn_line,
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
}
