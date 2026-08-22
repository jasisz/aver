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
//! `SymbolTable` resolves identity once for typechecking and HIR/MIR;
//! the VM, proof lowering, and ordinary backend paths then consume the
//! opaque IDs directly. A few source-shape proof/codegen adapters remain
//! as explicitly marked migration bridges tracked outside this module.
//!
//! These types live in `crate::ir` rather than `crate::ir::proof_ir`
//! because identity-across-module-boundary is a general IR concern,
//! not a proof-specific one. Typechecking, VM lowering, and every
//! backend share these identities rather than defining local notions
//! of which declaration a name denotes.

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
///
/// Unlike the source-order IDs used for functions and constructors, a
/// `TypeId` is derived from its canonical [`TypeKey`]. Dependency modules are
/// typechecked in their own tables before whole-program lowering; using a
/// vector position here let an ID for `A.Policy` silently name `Bytes` in the
/// next table. The canonical fingerprint is table-independent, and every
/// [`crate::ir::SymbolTable`] checks collisions when registering it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TypeId(pub u64);

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

/// Opaque, stable identity for a built-in function (e.g.
/// `Console.print`, `List.prepend`, `String.fromInt`). Phase 6
/// wave 11: replaces the previous `MirCallee::Builtin(String)`
/// shape with a typed id so MIR substrate uses typed identity
/// everywhere (matching `FnId` / `CtorId` / `TypeId`'s
/// "consumers never re-parse names" rule). The string name
/// stays available via `SymbolTable::builtin_entry(id).name`
/// for diagnostics + the registries that still key off strings
/// (the VM builtin table, the Rust codegen's
/// `emit_builtin_call`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct BuiltinId(pub u32);

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

impl TypeId {
    /// Derive the same opaque identity from the same canonical key in every
    /// symbol table. FNV-1a is used only as a compact deterministic
    /// fingerprint; `SymbolTable::build` detects a collision and fails closed
    /// instead of allowing two declarations to share an identity.
    pub(crate) fn for_key(key: &TypeKey) -> Self {
        const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
        const PRIME: u64 = 0x0000_0100_0000_01b3;

        fn add(hash: &mut u64, bytes: &[u8]) {
            for byte in bytes {
                *hash ^= u64::from(*byte);
                *hash = hash.wrapping_mul(PRIME);
            }
        }

        let mut hash = OFFSET;
        match key.scope.as_deref() {
            Some(scope) => {
                add(&mut hash, &[1]);
                add(&mut hash, scope.as_bytes());
            }
            None => add(&mut hash, &[0]),
        }
        add(&mut hash, &[0xff]);
        add(&mut hash, key.name.as_bytes());
        TypeId(hash)
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
