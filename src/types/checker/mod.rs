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

use super::{Type, parse_type_str_strict};
use crate::ast::{
    BinOp, Expr, FnDef, Literal, Module, Pattern, Spanned, Stmt, TailCallData, TopLevel, TypeDef,
};

mod builtins;
pub mod effect_classification;
pub mod effect_lifting;
mod exhaustiveness;
mod flow;
pub mod hostile_values;
mod infer;
mod memo;
mod modules;
pub mod proof_trust_header;

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
    /// Optional secondary span for multi-region diagnostics (e.g. declared type vs actual return).
    pub secondary: Option<TypeErrorSpan>,
}

#[derive(Debug, Clone)]
pub struct TypeErrorSpan {
    pub line: usize,
    pub col: usize,
    pub label: String,
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
    finalize_check_result(checker, items)
}

/// Variant of [`run_type_check_full`] that uses pre-loaded dependency
/// modules instead of resolving them from disk. The playground feeds
/// this from its in-memory virtual fs so multi-file projects type-
/// check without any filesystem access.
pub fn run_type_check_with_loaded(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> TypeCheckResult {
    let mut checker = TypeChecker::new();
    checker.check_with_loaded(items, loaded);
    finalize_check_result(checker, items)
}

fn finalize_check_result(mut checker: TypeChecker, items: &[TopLevel]) -> TypeCheckResult {
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

    let memo_safe_types = checker.compute_memo_safe_types(items);

    check_module_effect_boundary(items, &mut checker.errors);

    TypeCheckResult {
        errors: checker.errors,
        fn_sigs,
        memo_safe_types,
        unused_bindings: checker.unused_warnings,
    }
}

/// Enforce module-level `effects [...]` declaration against per-fn effect
/// usage. The rule:
///
/// - Module without `effects [...]` → legacy/mixed, no enforcement (0.13
///   migration shim; 0.14+ may upgrade to soft warning).
/// - Module with `effects [...]` (including `effects []` for explicit pure)
///   → every function's `! [...]` must be covered by the module's declared
///   surface. A namespace-level entry like `Disk` admits any `Disk.*`
///   method; a method-level entry like `Time.now` admits only that one.
fn check_module_effect_boundary(items: &[TopLevel], errors: &mut Vec<TypeError>) {
    let Some(allowed) = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => m.effects.as_ref().map(|e| (e, m)),
        _ => None,
    }) else {
        return;
    };
    let (allowed_list, module) = allowed;

    let allowed_namespaces: HashSet<&str> = allowed_list
        .iter()
        .filter(|e| !e.contains('.'))
        .map(|e| e.as_str())
        .collect();
    let allowed_methods: HashSet<&str> = allowed_list.iter().map(|e| e.as_str()).collect();

    for item in items {
        let TopLevel::FnDef(fd) = item else { continue };
        for eff in &fd.effects {
            let method = eff.node.as_str();
            if allowed_methods.contains(method) {
                continue;
            }
            if let Some((ns, _)) = method.split_once('.')
                && allowed_namespaces.contains(ns)
            {
                continue;
            }
            errors.push(TypeError {
                message: format!(
                    "module '{}' declared `effects [{}]` but '{}' uses '{}' which is not in the declared boundary",
                    module.name,
                    allowed_list.join(", "),
                    fd.name,
                    method
                ),
                line: eff.line,
                col: 1,
                secondary: module.effects_line.map(|l| TypeErrorSpan {
                    line: l,
                    col: 1,
                    label: "module effects declared here".to_string(),
                }),
            });
        }
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
    value_members: HashMap<String, Type>,
    /// Field types for record types: "TypeName.fieldName" → Type.
    /// Populated for both user-defined `record` types and built-in records
    /// (HttpResponse, Header). Enables checked dot-access on Named types.
    record_field_types: HashMap<String, Type>,
    /// Unqualified → qualified aliases for cross-module lookups.
    /// E.g. "Shape.Circle" → "Data.Shape.Circle".
    sig_aliases: HashMap<String, String>,
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
    /// Oracle v1: `.result` / `.trace` / `.trace.*` projections are
    /// only meaningful inside `verify <fn> trace` cases. This flag is
    /// set true while checking such a case's LHS / RHS, false
    /// otherwise. Outside verify-trace the projections are rejected at
    /// check time — otherwise user code would type-check then crash
    /// at runtime with "namespace has no member 'trace'".
    in_verify_trace_context: bool,
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
            value_members: HashMap::new(),
            record_field_types: HashMap::new(),
            sig_aliases: HashMap::new(),
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
            in_verify_trace_context: false,
        };
        tc.register_builtins();
        tc
    }

    // -- Alias-aware lookups ------------------------------------------------

    fn find_fn_sig(&self, key: &str) -> Option<&FnSig> {
        self.fn_sigs
            .get(key)
            .or_else(|| self.sig_aliases.get(key).and_then(|c| self.fn_sigs.get(c)))
    }

    fn find_value_member(&self, key: &str) -> Option<&Type> {
        self.value_members.get(key).or_else(|| {
            self.sig_aliases
                .get(key)
                .and_then(|c| self.value_members.get(c))
        })
    }

    fn find_record_field_type(&self, key: &str) -> Option<&Type> {
        self.record_field_types.get(key).or_else(|| {
            self.sig_aliases
                .get(key)
                .and_then(|c| self.record_field_types.get(c))
        })
    }

    // -- Helpers -----------------------------------------------------------

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
            secondary: None,
        });
    }

    fn error_at_line(&mut self, line: usize, msg: impl Into<String>) {
        self.errors.push(TypeError {
            message: msg.into(),
            line,
            col: 0,
            secondary: None,
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
