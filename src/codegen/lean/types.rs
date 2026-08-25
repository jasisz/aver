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
    /// Canonical provider-owned resource names for the program currently being
    /// emitted. A resource can share a historical builtin carrier name (today
    /// `Tcp.Connection`); in that case it must keep its capability namespace
    /// instead of being flattened to the legacy `Tcp_Connection` record.
    static CAPABILITY_RESOURCES: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    /// The program's type table and the module whose file is being emitted,
    /// so a user type is spelled by its owner: a type declared in the module
    /// being emitted keeps the name the source wrote, a type declared in any
    /// other module is spelled with that module's path (`A.Fraction`). The
    /// path resolves on its own, so a type a direct dependency only re-exposes
    /// and a type two opened modules both declare need no `open`. Populated
    /// once per `transpile_unified`; empty outside it, so a stray
    /// `type_to_lean` keeps the bare spelling.
    static TYPE_OWNERS: RefCell<Option<TypeOwners>> = const { RefCell::new(None) };
}

struct TypeOwners {
    symbols: crate::ir::SymbolTable,
    /// The entry's `module` name: a bare name written in the entry resolves
    /// against it, the way the resolver does.
    entry_module: Option<String>,
    /// Prefix of the dependency module being emitted; `None` for the entry.
    emitting: Option<String>,
}

/// Make `symbols` the type table user types are spelled against for the
/// lifetime of the returned guard, with the entry as the module being emitted.
pub(crate) fn scope_type_owners(
    symbols: crate::ir::SymbolTable,
    entry_module: Option<String>,
) -> TypeOwnersGuard {
    TYPE_OWNERS.with(|s| {
        *s.borrow_mut() = Some(TypeOwners {
            symbols,
            entry_module,
            emitting: None,
        })
    });
    TypeOwnersGuard
}

pub(crate) struct TypeOwnersGuard;

impl Drop for TypeOwnersGuard {
    fn drop(&mut self) {
        TYPE_OWNERS.with(|s| *s.borrow_mut() = None);
    }
}

/// Mark `prefix` as the dependency module being emitted for the lifetime of
/// the returned guard; the entry is being emitted again once it drops.
pub(crate) fn scope_emitting_module(prefix: &str) -> EmittingModuleGuard {
    TYPE_OWNERS.with(|s| {
        if let Some(owners) = s.borrow_mut().as_mut() {
            owners.emitting = Some(prefix.to_string());
        }
    });
    EmittingModuleGuard
}

pub(crate) struct EmittingModuleGuard;

impl Drop for EmittingModuleGuard {
    fn drop(&mut self) {
        TYPE_OWNERS.with(|s| {
            if let Some(owners) = s.borrow_mut().as_mut() {
                owners.emitting = None;
            }
        });
    }
}

/// The owner-qualified Lean spelling of a user type declared in a module
/// other than the one being emitted; `None` keeps the spelling the source
/// wrote (a type of the module being emitted, a builtin carrier, or a name
/// the resolver does not know). A capability resource is a user type like
/// any other: `Kv.Handle` outside the capability module, bare inside it. The
/// entry reaches a resource without opening its module whenever it threads a
/// transitive capability's operation (`! [Kv.count]` with `depends [Box]`),
/// and a bare `Handle` there is not an error under Lean's default
/// `autoImplicit` but a silently bound type variable.
fn owner_qualified_type_name(name: &str) -> Option<String> {
    TYPE_OWNERS.with(|s| {
        let owners = s.borrow();
        let owners = owners.as_ref()?;
        let resolver = crate::ir::hir::ResolveCtx {
            symbols: &owners.symbols,
            current_module: owners
                .emitting
                .clone()
                .or_else(|| owners.entry_module.clone()),
        };
        let entry = owners.symbols.type_entry(resolver.resolve_type_id(name)?);
        if entry.key.scope_str() == owners.emitting.as_deref() {
            return None;
        }
        Some(super::syntax::aver_path_to_lean(&entry.key.canonical()))
    })
}

pub(crate) fn scope_capability_resources(names: HashSet<String>) -> CapabilityResourceGuard {
    CAPABILITY_RESOURCES.with(|s| *s.borrow_mut() = names);
    CapabilityResourceGuard
}

pub(crate) struct CapabilityResourceGuard;

impl Drop for CapabilityResourceGuard {
    fn drop(&mut self) {
        CAPABILITY_RESOURCES.with(|s| s.borrow_mut().clear());
    }
}

fn is_capability_resource(name: &str) -> bool {
    CAPABILITY_RESOURCES.with(|s| s.borrow().contains(name))
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
        // A set-shaped map (`Map<T, Unit>`) renders as an ordinary map. It
        // used to render as `Finset T`, which is Mathlib — not a dependency of
        // the generated project — so the export did not build.
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
                owner_qualified_type_name(name).unwrap_or_else(|| lean_named_type_name(name))
            }
        }
    }
}

/// Lean structure name for an Aver named record/type.
///
/// A compiler-owned host carrier record (`Tcp.Connection`, `Tcp.Dial`,
/// `Tcp.Listener`, …) is emitted into `AverCommon` under a dot →
/// underscore mangled name (see [`crate::codegen::builtin_records`],
/// the single source of truth for these), so every reference to one
/// must mangle the same way. A source-owned record is emitted inside its
/// owning module's `namespace M`, so its Lean name is the dotted
/// namespaced path (`Domain.Rational.Fraction`) — the dots must be
/// preserved or the type ascription fails to resolve, with each
/// segment routed through the reserved-token guard (an Aver type or
/// module named `Type`/`Prop` is legal but collides with Lean syntax).
pub(crate) fn lean_named_type_name(name: &str) -> String {
    if is_capability_resource(name) {
        super::syntax::aver_path_to_lean(name)
    } else if crate::codegen::builtin_records::find(name).is_some() {
        name.replace('.', "_")
    } else {
        super::syntax::aver_path_to_lean(name)
    }
}

/// Like type_to_lean but wraps compound types in parens for use as type arguments.
pub(super) fn type_to_lean_atom(ty: &Type) -> String {
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
    use super::{scope_capability_resources, type_annotation_to_lean};
    use std::collections::HashSet;

    #[test]
    fn nested_result_type_arguments_are_parenthesized() {
        assert_eq!(
            type_annotation_to_lean("Result<List<Cmd>, String>"),
            "Except String (List Cmd)"
        );
    }

    #[test]
    fn capability_resource_wins_over_legacy_builtin_record_name() {
        let _guard = scope_capability_resources(HashSet::from(["Tcp.Connection".to_string()]));
        assert_eq!(type_annotation_to_lean("Tcp.Connection"), "Tcp.Connection");
    }
}
