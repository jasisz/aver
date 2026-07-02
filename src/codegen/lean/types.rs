/// Aver type → Lean 4 type string mapping.
use crate::types::Type;
use std::cell::RefCell;
use std::collections::HashSet;

thread_local! {
    /// Type names of the canonical Peano ADTs in the program currently being
    /// transpiled to Lean. A canonical Peano type (`T { Zero; Succ(T) }`,
    /// shape-detected by `detect_canonical_peano` — any name, not just `Nat`)
    /// is lifted to Lean's builtin `Nat`: its VALUES and PATTERNS already lift
    /// (`Zero`→`0`, `Succ e`→`e + 1`), so its TYPE annotations must lift to
    /// `Nat` too — otherwise a `T`-typed binder is matched against `Nat`
    /// literals and the proof is ill-typed. Populated once per
    /// `transpile_unified`; empty outside it, so a stray `type_to_lean` keeps
    /// the prior (no-lift) behavior.
    static CANONICAL_PEANO: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Mark `names` as the canonical-Peano types to lift to `Nat` for the lifetime
/// of the returned guard. The guard clears the set on drop, so a `type_to_lean`
/// call outside the transpile scope never sees stale lift state.
pub(crate) fn scope_canonical_peano(names: HashSet<String>) -> CanonicalPeanoGuard {
    CANONICAL_PEANO.with(|s| *s.borrow_mut() = names);
    CanonicalPeanoGuard
}

pub(crate) struct CanonicalPeanoGuard;

impl Drop for CanonicalPeanoGuard {
    fn drop(&mut self) {
        CANONICAL_PEANO.with(|s| s.borrow_mut().clear());
    }
}

fn is_canonical_peano(name: &str) -> bool {
    let bare = name.rsplit('.').next().unwrap_or(name);
    CANONICAL_PEANO.with(|s| s.borrow().contains(bare))
}

/// Convert an Aver `Type` to a Lean 4 type string.
pub fn type_to_lean(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(ok, err) => {
            // Lean's Except has reversed order: Except Error Ok
            format!(
                "Except {} {}",
                type_to_lean_atom(err),
                type_to_lean_atom(ok)
            )
        }
        Type::Option(inner) => format!("Option {}", type_to_lean_atom(inner)),
        Type::List(inner) => format!("List {}", type_to_lean_atom(inner)),
        Type::Vector(inner) => format!("Array {}", type_to_lean_atom(inner)),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(type_to_lean).collect();
            format!("({})", parts.join(" × "))
        }
        Type::Map(key, value) if crate::codegen::common::is_set_type(ty) => {
            format!("Finset {}", type_to_lean_atom(key))
        }
        Type::Map(key, value) => {
            // No direct Map in Lean core; use a list of pairs as approximation
            format!("List ({} × {})", type_to_lean(key), type_to_lean(value))
        }
        Type::Fn(params, ret, _effects) => {
            let mut parts: Vec<String> = params.iter().map(type_to_lean_atom).collect();
            parts.push(type_to_lean(ret));
            parts.join(" → ")
        }
        Type::Var(_) | Type::Invalid => {
            panic!(
                "Lean codegen: encountered Type::Invalid or Type::Var. \
                 This indicates unresolved typing leaked into codegen."
            )
        }
        // display-only: rendering the Lean type identifier string
        // — `name` IS the right surface, `id` carries no display
        // information. Identity-sensitive routing happens at the
        // call layer (see `backend_named_type_key`).
        Type::Named { name, .. } => {
            if is_canonical_peano(name) {
                // Lifted to builtin `Nat` (consistent with the value/pattern
                // lift in `expr.rs`/`pattern.rs`), so any-named Peano ADT — not
                // just one literally called `Nat` — gets the builtin-`Nat`
                // proof machinery (`omega`, `Nat.*`).
                "Nat".to_string()
            } else {
                lean_named_type_name(name)
            }
        }
    }
}

/// Lean structure name for an Aver named record/type.
///
/// A builtin HOST carrier record (`Tcp.Connection`, `HttpResponse`,
/// `Terminal.Size`, …) is emitted into `AverCommon` under a dot →
/// underscore mangled name (see [`crate::codegen::builtin_records`],
/// the single source of truth for these), so every reference to one
/// must mangle the same way. A USER record is emitted inside its
/// owning module's `namespace M`, so its Lean name is the dotted
/// namespaced path (`Domain.Rational.Fraction`) — the dots must be
/// preserved or the type ascription fails to resolve. Bare names (no
/// dot) are unaffected either way.
pub(crate) fn lean_named_type_name(name: &str) -> String {
    if crate::codegen::builtin_records::find(name).is_some() {
        name.replace('.', "_")
    } else {
        name.to_string()
    }
}

/// Like type_to_lean but wraps compound types in parens for use as type arguments.
fn type_to_lean_atom(ty: &Type) -> String {
    match ty {
        Type::Result(..)
        | Type::Option(_)
        | Type::List(_)
        | Type::Vector(_)
        | Type::Fn(..)
        | Type::Map(..) => {
            format!("({})", type_to_lean(ty))
        }
        _ => type_to_lean(ty),
    }
}

/// Convert an Aver type annotation string to a Lean 4 type string.
pub fn type_annotation_to_lean(ann: &str) -> String {
    let ty = crate::types::parse_type_str(ann);
    type_to_lean(&ty)
}

#[cfg(test)]
mod tests {
    use super::type_annotation_to_lean;

    #[test]
    fn nested_result_type_arguments_are_parenthesized() {
        assert_eq!(
            type_annotation_to_lean("Result<List<Cmd>, String>"),
            "Except String (List Cmd)"
        );
    }
}
