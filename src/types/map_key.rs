//! What a `Map` may be keyed on.
//!
//! A map iterates its entries sorted by key — on the VM, in compiled Rust,
//! and in the exported proof model — so the key type needs a total order
//! that every one of those can state. `Int`, `String` and `Bool` have one.
//! Nothing else does: `Float` has no place for NaN in a finite range, and
//! records, variants, tuples, lists and opaque handles were being ordered
//! by their printed form, which is not a property of the value.
//!
//! This list is the single source of truth. The proof exporter's own
//! backstop reads it rather than carrying a second copy, and the Lean
//! prelude carries exactly these three `AverKeyOrder` instances.

use super::Type;

/// The key types a `Map` may have, spelled as the user writes them.
pub const ORDERED_MAP_KEY_TYPES: &[&str] = &["Int", "String", "Bool"];

/// Whether `ty` can key a map.
///
/// An unresolved type variable passes: it is the shape an empty map literal
/// carries before anything decides its key, and a map with no entries has
/// nothing to order. Every door that later decides that key checks it.
/// `Invalid` passes so a program that already failed to typecheck is not
/// reported twice.
pub fn map_key_has_ordering(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Int | Type::Str | Type::Bool | Type::Var(_) | Type::Invalid
    )
}

/// The one wording for the refusal. Every door emits this tail, so a reader
/// who meets it twice meets the same sentence.
///
/// The phrasing is keyed on by `diagnostics::classify` — keep the leading
/// `a Map key type must have an ordering` shape.
pub fn unordered_map_key_message(key: &Type) -> String {
    format!(
        "a Map key type must have an ordering, and `{}` has none; a map iterates its entries sorted by key on every backend and in the proof model — use Int, String or Bool as the key",
        key.display()
    )
}

/// The first key inside `ty` that cannot key a map, if there is one.
///
/// Doors that report through a plain `Result<_, String>` — a signature
/// registered from a dependency — use this instead of the checker's
/// diagnostic list.
pub fn first_unordered_map_key(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Map(k, v) => {
            if !map_key_has_ordering(k) {
                return Some(k);
            }
            first_unordered_map_key(k).or_else(|| first_unordered_map_key(v))
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            first_unordered_map_key(inner)
        }
        Type::Result(ok, err) => {
            first_unordered_map_key(ok).or_else(|| first_unordered_map_key(err))
        }
        Type::Tuple(items) => items.iter().find_map(first_unordered_map_key),
        Type::Fn(params, ret, _) => params
            .iter()
            .find_map(first_unordered_map_key)
            .or_else(|| first_unordered_map_key(ret)),
        Type::Named { .. }
        | Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => None,
    }
}
