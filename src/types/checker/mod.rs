/// Aver static type checker.
///
/// Two-phase analysis:
///   Phase 1 — build a signature table from all FnDef nodes and builtins.
///   Phase 2 — check top-level statements, then each FnDef for call-site
///              argument types, return type, BinOp compatibility, and effects.
///
/// The checker resolves named generic variables at call sites. Error recovery
/// uses `Type::Invalid`, which matches anything to suppress cascading diagnostics
/// (Iron — A4).
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
pub mod hostile_effects;
pub mod hostile_values;
mod infer;
mod memo;
mod modules;
pub mod oracle_subtypes;
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

/// Self-host variant of [`run_type_check_full`]: bypasses the
/// opaque-type checks (construction, field access, pattern match).
/// Used exclusively by `aver compile --with-self-host-support` so
/// `self_hosted/domain/builtins.av` can round-trip opaque host
/// types (e.g. `Tcp.Connection`) through the replay JSON contract.
/// User code outside the self-host always goes through the regular
/// [`run_type_check_full`] and stays bound by the opaque rules.
pub fn run_type_check_full_self_host(
    items: &[TopLevel],
    base_dir: Option<&str>,
) -> TypeCheckResult {
    let mut checker = TypeChecker::new();
    checker.self_host_mode = true;
    checker.check(items, base_dir);
    finalize_check_result(checker, items)
}

/// Self-host variant of [`run_type_check_with_loaded`]. See
/// [`run_type_check_full_self_host`] for the opaque-bypass rationale.
pub fn run_type_check_with_loaded_self_host(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> TypeCheckResult {
    let mut checker = TypeChecker::new();
    checker.self_host_mode = true;
    checker.check_with_loaded(items, loaded);
    finalize_check_result(checker, items)
}

fn finalize_check_result(mut checker: TypeChecker, items: &[TopLevel]) -> TypeCheckResult {
    // Internal `fn_sigs` is keyed by canonical "Module.name" (Iron — A3).
    // The exported map preserves both forms so external consumers
    // (`verify_effects`, Lean / Dafny codegen, the CLI summary) can
    // continue to look entries up by the bare name the user wrote.
    let mut fn_sigs: HashMap<String, (Vec<Type>, Type, Vec<String>)> = checker
        .fn_sigs
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                (v.params.clone(), v.ret.clone(), v.effects.clone()),
            )
        })
        .collect();
    for (alias, canonical) in &checker.sig_aliases {
        if !fn_sigs.contains_key(alias)
            && let Some(sig) = checker.fn_sigs.get(canonical)
        {
            fn_sigs.insert(
                alias.clone(),
                (sig.params.clone(), sig.ret.clone(), sig.effects.clone()),
            );
        }
    }

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

/// Iron — A5: typed key for `record_field_types`. Pre-A5 the map
/// was keyed by `"TypeName.fieldName"` stringifications, which
/// forced every reader to `strip_prefix(format!("{type}."))` and
/// then re-check that the remainder didn't itself contain a dot
/// (because the post-A3 dual-keying mirrored each entry under both
/// the canonical `"Module.Type.field"` form and the bare alias
/// `"Type.field"` — and the canonical form spuriously matched the
/// `"Module."` prefix-strip when the read came from a module
/// looking up its own fields). The struct key separates the two
/// dimensions, so the canonical resolution happens once at
/// insert/lookup (via `sig_aliases`) and iteration filters on
/// `key.type_name == canonical` with no string-shape gymnastics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RecordFieldKey {
    pub(crate) type_name: String,
    pub(crate) field_name: String,
}

impl RecordFieldKey {
    pub(crate) fn new(type_name: impl Into<String>, field_name: impl Into<String>) -> Self {
        Self {
            type_name: type_name.into(),
            field_name: field_name.into(),
        }
    }
}

struct TypeChecker {
    fn_sigs: HashMap<String, FnSig>,
    value_members: HashMap<String, Type>,
    /// Field types for record types, keyed by `(type_name, field_name)`.
    /// Populated for both user-defined `record` types and built-in records
    /// (HttpResponse, Header). Single entry per (canonical type name, field);
    /// lookup canonicalises `type_name` through `sig_aliases` at read time.
    /// Enables checked dot-access on Named types.
    record_field_types: HashMap<RecordFieldKey, Type>,
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
    /// When `true`, opaque-type construction + field-access + pattern-match
    /// checks are bypassed. Used only by the self-host compile path
    /// (`aver compile --with-self-host-support`) where
    /// `self_hosted/domain/builtins.av` round-trips opaque host types
    /// (e.g. `Tcp.Connection`) through the replay `Val` representation:
    /// it serialises by reading `.id` / `.host` / `.port`, and
    /// reconstructs by `Tcp.Connection(id = …, host = …, port = …)` on
    /// replay deserialise. Both operations are forbidden in user code by
    /// design (Phase 4.7+ fix #11), but the self-host has to read +
    /// write the underlying record shape because that's the contract
    /// with the replay JSON format. The flag is set by
    /// [`run_type_check_full_self_host`] / [`run_type_check_with_loaded_self_host`]
    /// and never user-toggleable from source.
    self_host_mode: bool,
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
            self_host_mode: false,
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

    fn find_record_field_type(&self, type_name: &str, field_name: &str) -> Option<&Type> {
        // Iron — A5: lookup canonicalises the type-name dimension via
        // `sig_aliases` before hashing. We only need to do that for
        // the type-name part — field names are intra-type and stay
        // verbatim.
        let direct = RecordFieldKey::new(type_name, field_name);
        if let Some(ty) = self.record_field_types.get(&direct) {
            return Some(ty);
        }
        if let Some(canonical_type) = self.sig_aliases.get(type_name) {
            let canonical = RecordFieldKey::new(canonical_type, field_name);
            return self.record_field_types.get(&canonical);
        }
        None
    }

    /// Iron — A5: list every `(field_name, field_type)` pair declared
    /// for `type_name`. Resolves `type_name` through `sig_aliases`
    /// so a bare reference in source matches a canonical entry; the
    /// reverse direction (canonical reference hitting a bare entry)
    /// is a no-op because A3 normalises stored keys to canonical
    /// form whenever a module alias exists.
    fn fields_for_type(&self, type_name: &str) -> Vec<(String, Type)> {
        let canonical = self
            .sig_aliases
            .get(type_name)
            .map(String::as_str)
            .unwrap_or(type_name);
        self.record_field_types
            .iter()
            .filter(|(k, _)| k.type_name == canonical || k.type_name == type_name)
            .map(|(k, v)| (k.field_name.clone(), v.clone()))
            .collect()
    }

    /// Iron — A5: `true` if any field has been registered for
    /// `type_name`. Drops the pre-A5 `record_field_types.keys().any(|k|
    /// k.starts_with(&format!("{}.", type_name)))` substring probe.
    fn has_record_schema(&self, type_name: &str) -> bool {
        let canonical = self
            .sig_aliases
            .get(type_name)
            .map(String::as_str)
            .unwrap_or(type_name);
        self.record_field_types
            .keys()
            .any(|k| k.type_name == canonical || k.type_name == type_name)
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

    /// Iron — A3: `&self`-bearing constraint check. Resolves bare
    /// Named types through `sig_aliases` so source-faithful Spanned
    /// stamps (often bare inside a module) match against
    /// canonicalised fn signatures (always "Module.Type").
    pub(super) fn compatible(&self, actual: &Type, expected: &Type) -> bool {
        let mut subst = HashMap::new();
        Self::match_expected_type_inner(actual, expected, &mut subst, &self.sig_aliases)
    }

    /// Static-form matcher (no alias resolution). Tests use this
    /// directly; production code should reach for `compatible`
    /// instead.
    pub(super) fn match_expected_type(
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        Self::match_expected_type_inner(actual, expected, subst, &HashMap::new())
    }

    /// Iron — A3: `&self` matcher that lets the caller carry a
    /// substitution (poly fn arg inference). The pure `compatible`
    /// helper above hides `subst` for the common "no Type::Var
    /// involved" callers; this method exposes it for the FnCall arg
    /// loop in `infer/expr.rs`.
    pub(super) fn match_with(
        &self,
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        Self::match_expected_type_inner(actual, expected, subst, &self.sig_aliases)
    }

    fn match_expected_type_inner(
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
        aliases: &HashMap<String, String>,
    ) -> bool {
        // Iron — A4: `Type::Invalid` is the checker's "we already
        // reported an error here, don't compound it" sentinel.
        // Returning `false` for it turned every downstream use site
        // into a fresh `expected X, got Invalid` diagnostic — a single
        // unknown-fn call could fan out to N + 1 errors (the unknown
        // fn plus one per downstream consumer). Treat Invalid as a
        // wildcard on either side so the original error stands alone.
        // Per-callsite guards like `!matches!(ty, Type::Invalid)`
        // around `self.compatible(...)` are now redundant but harmless;
        // sweeping them is deliberately out of scope here.
        if matches!(actual, Type::Invalid) || matches!(expected, Type::Invalid) {
            return true;
        }
        match expected {
            Type::Var(name) => Self::bind_expected_var(name, actual, subst),
            Type::Invalid => unreachable!("Type::Invalid handled by the early guard above"),
            Type::Int => matches!(actual, Type::Int),
            Type::Float => matches!(actual, Type::Float),
            Type::Str => matches!(actual, Type::Str),
            Type::Bool => matches!(actual, Type::Bool),
            Type::Unit => matches!(actual, Type::Unit),
            Type::Named(expected_name) => match actual {
                // Iron — A3: bare ↔ canonical resolves through
                // `sig_aliases`. After A3, fn / record / variant
                // signatures live under their "Module.Type" key and
                // mirror a bare-name alias in `sig_aliases`; source
                // expressions stamp Spanned.ty in whatever form the
                // user wrote. Resolve both sides to the canonical
                // form first, then compare strictly. Two distinct
                // modules both exposing "Shape" still produce
                // ambiguous aliases at registration time — that's
                // surfaced upfront elsewhere; here we only need to
                // know that whatever bare form survives in
                // `sig_aliases` IS the unique canonical.
                Type::Named(actual_name) => {
                    let exp_canon = aliases
                        .get(expected_name)
                        .map(String::as_str)
                        .unwrap_or(expected_name);
                    let act_canon = aliases
                        .get(actual_name)
                        .map(String::as_str)
                        .unwrap_or(actual_name);
                    act_canon == exp_canon
                }
                _ => false,
            },
            Type::Option(expected_inner) => match actual {
                Type::Option(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, aliases)
                }
                _ => false,
            },
            Type::List(expected_inner) => match actual {
                Type::List(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, aliases)
                }
                _ => false,
            },
            Type::Vector(expected_inner) => match actual {
                Type::Vector(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, aliases)
                }
                _ => false,
            },
            Type::Result(expected_ok, expected_err) => match actual {
                Type::Result(actual_ok, actual_err) => {
                    Self::match_expected_type_inner(actual_ok, expected_ok, subst, aliases)
                        && Self::match_expected_type_inner(actual_err, expected_err, subst, aliases)
                }
                _ => false,
            },
            Type::Map(expected_k, expected_v) => match actual {
                Type::Map(actual_k, actual_v) => {
                    Self::match_expected_type_inner(actual_k, expected_k, subst, aliases)
                        && Self::match_expected_type_inner(actual_v, expected_v, subst, aliases)
                }
                _ => false,
            },
            Type::Tuple(expected_items) => match actual {
                Type::Tuple(actual_items) if actual_items.len() == expected_items.len() => {
                    actual_items.iter().zip(expected_items.iter()).all(
                        |(actual_item, expected_item)| {
                            Self::match_expected_type_inner(
                                actual_item,
                                expected_item,
                                subst,
                                aliases,
                            )
                        },
                    )
                }
                _ => false,
            },
            Type::Fn(expected_params, expected_ret, expected_effects) => match actual {
                Type::Fn(actual_params, actual_ret, actual_effects)
                    if actual_params.len() == expected_params.len() =>
                {
                    actual_params.iter().zip(expected_params.iter()).all(
                        |(actual_param, expected_param)| {
                            Self::match_expected_type_inner(
                                actual_param,
                                expected_param,
                                subst,
                                aliases,
                            )
                        },
                    ) && Self::match_expected_type_inner(actual_ret, expected_ret, subst, aliases)
                        && actual_effects.iter().all(|actual| {
                            expected_effects
                                .iter()
                                .any(|expected| crate::effects::effect_satisfies(expected, actual))
                        })
                }
                _ => false,
            },
        }
    }

    fn bind_expected_var(name: &str, actual: &Type, subst: &mut HashMap<String, Type>) -> bool {
        match actual {
            Type::Var(actual_name) => return actual_name == name,
            // Iron — A4: matches the wildcard in `match_expected_type_inner`.
            // An already-errored actual binds vacuously instead of
            // refusing the unification and triggering a cascade.
            Type::Invalid => return true,
            _ => {}
        }
        if let Some(bound) = subst.get(name).cloned() {
            return Self::match_expected_type(actual, &bound, subst)
                && Self::match_expected_type(&bound, actual, subst);
            // bind_expected_var is alias-agnostic — Var bindings
            // never compare Named types against `sig_aliases` since
            // the binding rule already accepts whatever concrete
            // type the caller hands in.
        }
        // Occurs check — refuse `T := F<…T…>` style circular bindings.
        // Without this, polymorphic recursion patterns like `fn nest(v:
        // A) -> Unit; nest([v])` would insert `A → List<A>` into `subst`
        // and rely on downstream structural mismatch to terminate
        // matching. The HashMap entry itself is still a cycle that
        // later `instantiate_type` walks would have to skip; rejecting
        // the bind at source keeps the substitution map well-formed
        // and surfaces the constraint failure to the caller as a
        // normal type-incompatibility error.
        if Self::type_contains_var(actual, name) {
            return false;
        }
        subst.insert(name.to_string(), actual.clone());
        true
    }

    /// Structural recursion over `ty` looking for any `Type::Var(name)`.
    /// Used by the occurs check in [`bind_expected_var`]; not exposed
    /// elsewhere because it's a one-step deep walk over a finite Type
    /// AST (no shared subterms, no cycles in the AST itself — the cycle
    /// would only exist in the substitution map, which the bind path
    /// is what guards).
    fn type_contains_var(ty: &Type, name: &str) -> bool {
        match ty {
            Type::Var(other) => other == name,
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Invalid
            | Type::Named(_) => false,
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                Self::type_contains_var(inner, name)
            }
            Type::Result(ok, err) => {
                Self::type_contains_var(ok, name) || Self::type_contains_var(err, name)
            }
            Type::Map(k, v) => Self::type_contains_var(k, name) || Self::type_contains_var(v, name),
            Type::Tuple(items) => items.iter().any(|t| Self::type_contains_var(t, name)),
            Type::Fn(params, ret, _effects) => {
                params.iter().any(|p| Self::type_contains_var(p, name))
                    || Self::type_contains_var(ret, name)
            }
        }
    }

    pub(super) fn instantiate_type(ty: &Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Result(ok, err) => Type::Result(
                Box::new(Self::instantiate_type(ok, subst)),
                Box::new(Self::instantiate_type(err, subst)),
            ),
            Type::Option(inner) => Type::Option(Box::new(Self::instantiate_type(inner, subst))),
            Type::List(inner) => Type::List(Box::new(Self::instantiate_type(inner, subst))),
            Type::Vector(inner) => Type::Vector(Box::new(Self::instantiate_type(inner, subst))),
            Type::Map(k, v) => Type::Map(
                Box::new(Self::instantiate_type(k, subst)),
                Box::new(Self::instantiate_type(v, subst)),
            ),
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|item| Self::instantiate_type(item, subst))
                    .collect(),
            ),
            Type::Fn(params, ret, effects) => Type::Fn(
                params
                    .iter()
                    .map(|param| Self::instantiate_type(param, subst))
                    .collect(),
                Box::new(Self::instantiate_type(ret, subst)),
                effects.clone(),
            ),
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Invalid
            | Type::Named(_) => ty.clone(),
        }
    }
}
