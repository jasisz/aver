//! Identity keys for named declarations.
//!
//! Aver source can declare the same bare name in two modules
//! (`A.foo` and `B.foo` are distinct fns; `A.Natural` and
//! `B.Natural` are distinct types). The typechecker keeps these
//! straight at its boundary (`fn_sigs` is keyed by canonical
//! `Module.name`), but downstream consumers — proof IR, backends,
//! the VM bytecode lowerer — historically threaded bare `String`s
//! around and re-derived identity from `fd.name` at every lookup.
//! That produced a steady drip of "two same-bare-name fns get the
//! wrong contract / refinement / strategy" bugs (review rounds 1-6
//! of the proof-export audit).
//!
//! `FnKey` / `TypeKey` / `LawKey` are the typed replacements.
//! Identity is resolved **once** at the IR boundary (today: the
//! `proof_lower` stage); from there backends consume the key
//! directly. The compiler refuses to compare a key to a bare
//! `String`, so the bug class can no longer be expressed.
//!
//! These types live in `crate::ir` rather than `crate::ir::proof_ir`
//! because identity-across-module-boundary is a general IR concern,
//! not a proof-specific one. The proof flow is the first consumer;
//! future cross-module identity sites (VM call-site resolution,
//! Rust codegen multi-module module-fn lookups) will use the same
//! types.

// ---------------------------------------------------------------------------
// Opaque IDs
// ---------------------------------------------------------------------------
//
// These live here, not in `symbol_table`, so that `crate::ast::Type` can
// carry a `TypeId` field without `ast` having to depend on `crate::ir`'s
// AST-bearing modules. `identity` has no upward deps (only `std`), which
// keeps the dependency layering clean: `ast` → `ir::identity` →
// `ir::symbol_table` (consumes ast) → everything else.

/// Opaque, stable identity for a function declaration. Indices are
/// assigned by [`crate::ir::SymbolTable::build`] in deterministic order
/// (modules in dep order, then entry; fns in source order within each
/// scope). Two builds against the same input produce the same IDs.
///
/// `FnId` is `Copy` and 4 bytes wide — cheap to thread through
/// resolved AST and IR nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FnId(pub u32);

/// Opaque, stable identity for a type declaration (record or sum).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TypeId(pub u32);

/// Opaque, stable identity for a constructor (a single variant of a
/// sum type, or a single record's nominal constructor — record types
/// still have exactly one "constructor" in the symbol table sense so
/// pattern emit + value construction route through one shape).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct CtorId(pub u32);

/// Opaque, stable identity for a module. `ModuleId(0)` is reserved
/// for the entry scope (matching `FnKey::scope == None`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ModuleId(pub u32);

impl ModuleId {
    /// The entry scope — top-level items not declared inside any dep
    /// module. Always assigned `ModuleId(0)`.
    pub const ENTRY: ModuleId = ModuleId(0);

    pub fn is_entry(self) -> bool {
        self == Self::ENTRY
    }
}

/// Canonical identity for a user-defined function across the IR
/// boundary. `scope = None` is the entry file; `scope = Some(prefix)`
/// names a dep module (`"ModuleA"`, `"Models.User"`, …). `name`
/// is the bare source name. Two modules each declaring a `foo`
/// produce distinct `FnKey { scope: Some("A"), name: "foo" }` and
/// `FnKey { scope: Some("B"), name: "foo" }` keys.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnKey {
    pub scope: Option<String>,
    pub name: String,
}

impl FnKey {
    /// Entry-scope (None) constructor.
    pub fn entry(name: impl Into<String>) -> Self {
        Self {
            scope: None,
            name: name.into(),
        }
    }

    /// Module-scope constructor.
    pub fn in_module(prefix: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            scope: Some(prefix.into()),
            name: name.into(),
        }
    }

    /// Canonical text form: `Module.name` for module-scoped fns,
    /// bare `name` for entry. Use this only when crossing an
    /// interop boundary (Lean qualified imports, Dafny
    /// `Aver_Module.fn` references) — internal IR / backend logic
    /// should keep operating on the typed key.
    pub fn canonical(&self) -> String {
        match &self.scope {
            Some(prefix) => format!("{}.{}", prefix, self.name),
            None => self.name.clone(),
        }
    }

    /// Scope as a borrowed `&str` for callers that just need to
    /// branch on entry vs module without taking ownership.
    pub fn scope_str(&self) -> Option<&str> {
        self.scope.as_deref()
    }
}

/// Canonical identity for a user-defined type — same shape and
/// reasoning as [`FnKey`]. Refined records, sum types, and opaque
/// records all live in the IR keyed by `TypeKey` so a bare AST
/// reference (`Expr::RecordCreate { type_name: "Natural" }`) is
/// resolved once at the IR boundary against the caller's owning
/// scope, never re-resolved per emit site.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypeKey {
    pub scope: Option<String>,
    pub name: String,
}

impl TypeKey {
    pub fn entry(name: impl Into<String>) -> Self {
        Self {
            scope: None,
            name: name.into(),
        }
    }

    pub fn in_module(prefix: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            scope: Some(prefix.into()),
            name: name.into(),
        }
    }

    pub fn canonical(&self) -> String {
        match &self.scope {
            Some(prefix) => format!("{}.{}", prefix, self.name),
            None => self.name.clone(),
        }
    }

    pub fn scope_str(&self) -> Option<&str> {
        self.scope.as_deref()
    }
}

/// Canonical identity for a verify-law theorem. Targets a fn by
/// [`FnKey`]; the `law_name` is local to that fn (Aver's
/// `verify <fn> law <law_name>` syntax — names only collide within
/// one fn's verify block).
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LawKey {
    pub fn_key: FnKey,
    pub law_name: String,
}

impl LawKey {
    pub fn new(fn_key: FnKey, law_name: impl Into<String>) -> Self {
        Self {
            fn_key,
            law_name: law_name.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fn_key_canonical_distinguishes_entry_from_module() {
        let entry = FnKey::entry("foo");
        let in_a = FnKey::in_module("A", "foo");
        let in_b = FnKey::in_module("B", "foo");
        assert_eq!(entry.canonical(), "foo");
        assert_eq!(in_a.canonical(), "A.foo");
        assert_eq!(in_b.canonical(), "B.foo");
        // The Hash + Eq contract distinguishes them — that's the
        // entire point of the type.
        assert_ne!(entry, in_a);
        assert_ne!(in_a, in_b);
    }

    #[test]
    fn nested_module_prefix_round_trips_through_canonical() {
        let key = FnKey::in_module("Models.User", "freshId");
        assert_eq!(key.canonical(), "Models.User.freshId");
        assert_eq!(key.scope_str(), Some("Models.User"));
    }
}
