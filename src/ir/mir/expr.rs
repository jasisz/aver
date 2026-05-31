//! `MirExpr` + supporting node types.
//!
//! Phase 2a of #252. Expression-based (every `MirExpr` is a value),
//! structured (no terminator-style basic blocks), and identity-typed
//! (declaration refs go through `FnId` / `TypeId` / `CtorId`).
//!
//! The shape decisions pinned in the Phase 1 RFC live here as code:
//!
//! - `Try` is its own variant — *not* desugared to `Match`.
//! - `Match` is structured with `Vec<MirMatchArm>` — no flat
//!   switch / jump representation.
//! - Constructors carry `CtorId`, record types carry `TypeId`,
//!   tail-call targets carry `FnId`.
//! - Each node carries a `Span` for diagnostics + future
//!   correlation with `ProofIR`.

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::hir::{BuiltinCtor, BuiltinIntrinsic};
use crate::ir::{BuiltinId, CtorId, FnId, TypeId};

use super::program::LocalId;

/// Local-read with last-use annotation. Phase 6 wave 4. Slot is
/// the binding identity; `last_use = true` when this is the
/// final read of that slot in the enclosing fn body, mirroring
/// HIR's `AnnotBool` last-use stamp.
/// Phase 5 prep: `name` carries the source-level binding name
/// (param name, let binding name, pattern binding name). VM
/// backends ignore it (dispatch by slot). Rust / wasm-gc
/// backends use it as the emitted Rust ident / export name.
/// Empty for synthetic locals.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirLocal {
    pub slot: LocalId,
    pub last_use: bool,
    pub name: String,
}

impl MirLocal {
    /// Construct a `MirLocal` with `last_use = false` and empty
    /// name. Convenience for hand-built test fixtures.
    pub fn at(slot: LocalId) -> Self {
        Self {
            slot,
            last_use: false,
            name: String::new(),
        }
    }
}

impl std::fmt::Display for MirLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.last_use {
            write!(f, "{}*", self.slot)
        } else {
            write!(f, "{}", self.slot)
        }
    }
}

/// Typed identity for a constructor reference inside MIR. Two
/// flavors: user-declared variants identified by stable `CtorId`,
/// and language-level built-in constructors (`Result.Ok` /
/// `Result.Err` / `Option.Some` / `Option.None`) that don't get
/// user-program ids because they're not user-declared.
///
/// Wave 3c-i pin: built-in ctors travel through the same
/// `MirConstruct` / `MirPattern::Ctor` shape as user ctors, so
/// every consumer reading constructor identity goes through one
/// matchable enum — no separate "is this Result.Ok" string check
/// scattered across backends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirCtor {
    /// User-declared sum-type variant or record constructor.
    User(CtorId),
    /// Language-level built-in (`Result.Ok` / `Result.Err` /
    /// `Option.Some` / `Option.None`). Same `BuiltinCtor` enum
    /// the resolver pass uses, re-exported by `super::*`.
    Builtin(BuiltinCtor),
}

/// One MIR expression. Every variant is a value — there's no
/// separate statement form at this phase. Sequencing happens via
/// `Let`.
#[derive(Debug, Clone)]
pub enum MirExpr {
    /// A literal value (`Int`, `Float`, `Bool`, `String`, `Unit`).
    /// Same vocabulary the existing typed AST already uses.
    Literal(Spanned<Literal>),
    /// Read a previously-bound local. The `LocalId` was introduced
    /// either as a function parameter (`MirParam::local`) or via a
    /// `Let` in this body's lexical scope. Phase 6 wave 4: carries
    /// a `last_use` flag the lowerer propagates from HIR's
    /// `ResolvedExpr::Resolved { last_use, .. }` so backends can
    /// emit `MOVE_LOCAL` (skipping ref-count bumps) on the final
    /// read of a slot.
    Local(Spanned<MirLocal>),
    /// `let binding = value; body` — sequence two expressions and
    /// surface the second's value. Phase 2a's only sequencing
    /// primitive; everything else inside a function body composes
    /// from this + the value-form variants below.
    Let(Spanned<MirLet>),
    /// Apply a callee (user fn / builtin) to arguments. The callee
    /// kind discriminates so backends know whether to look up via
    /// `FnId` (typed identity) or via the named-builtin registry.
    Call(Spanned<MirCall>),
    /// Tail call to a user fn — same SCC as the surrounding fn.
    /// Backends decide the final shape (wasm-gc tail-call insn,
    /// VM tail dispatch, Rust loop rewrite).
    TailCall(Spanned<MirTailCall>),
    /// Binary operator over numeric / boolean operands. Same set as
    /// `ast::BinOp` — MIR doesn't normalize arithmetic here; that's
    /// a Phase 6 optimizer concern.
    BinOp(Spanned<MirBinOp>),
    /// Unary numeric negation. Distinct from `BinOp(Sub, 0, x)` so
    /// IEEE-754 `-0.0` semantics are preserved on `Float`.
    Neg(Box<Spanned<MirExpr>>),
    /// `match <subject> { arm₁ ; arm₂ ; … }` — structured. Phase 4
    /// VM walks arms in order, picks the first matching pattern.
    Match(Spanned<MirMatch>),
    /// Construct a sum-type variant by `CtorId`. The variant's
    /// declared fields are filled in argument order.
    Construct(Spanned<MirConstruct>),
    /// Build a fresh record of a named product type. Fields are
    /// (`field_name`, value) pairs to keep the dump readable; the
    /// declared field order is determined by `TypeId` and
    /// validated at lowering time.
    RecordCreate(Spanned<MirRecordCreate>),
    /// `T.update(base, field = v, …)` — produce a new record that
    /// matches `base` except for the named field overrides.
    RecordUpdate(Spanned<MirRecordUpdate>),
    /// Field access (`base.field`) on a record value.
    Project(Spanned<MirProject>),
    /// `if cond { then } else { else }` — direct conditional
    /// shape introduced by Phase 6 wave 9's `bool_match_to_if`
    /// pass. Lowering never produces this node directly; the
    /// optimizer pass rewrites qualifying two-arm `Bool` match
    /// expressions into it so every backend gets a uniform
    /// if/else node instead of re-implementing the recognition.
    IfThenElse(Spanned<MirIfThenElse>),
    /// `value?` — the canonical `?` propagation. Phase 1's
    /// most-important pin: this stays a node. Lowering to nested
    /// `Match` is a per-backend choice, not a pipeline-wide
    /// transform. Rust will eventually emit `?` native, VM emits
    /// tag-check + early return.
    ///
    /// The bound form `let x = step()?; body` is expressed as
    /// `MirExpr::Let { binding: x, value: MirExpr::Try(step()),
    /// body }` — no dedicated `TryBind` variant. The original
    /// design had one, but it duplicated the semantics of
    /// `Let { value: Try(_), ... }` exactly. Consumers that need
    /// to recognize the `?-bind` pattern do so by walking
    /// `Let` and inspecting `value.node` for `MirExpr::Try`.
    Try(Box<Spanned<MirExpr>>),
    /// `[a, b, c]` — list literal. Elements lower to MIR
    /// expressions; the resulting value is `List<T>` with `T`
    /// inferred at type-check time.
    List(Vec<Spanned<MirExpr>>),
    /// `(a, b, c)` — tuple literal.
    Tuple(Vec<Spanned<MirExpr>>),
    /// `{"k" => v, …}` — map literal. Keys + values lower as MIR
    /// expressions; the resulting value is `Map<K, V>`.
    MapLiteral(Vec<(Spanned<MirExpr>, Spanned<MirExpr>)>),
    /// `"…{expr}…"` — interpolated string. Each part is either a
    /// literal text segment or an embedded MIR expression whose
    /// value gets stringified at runtime.
    InterpolatedStr(Vec<MirStrPart>),
    /// Independent product: `(a, b, c)!` or `(a, b, c)?!`. The
    /// `unwrap_results` flag captures the `?` form (every element
    /// must be `Result<…>`; `Err` short-circuits with the first
    /// error). Schedule (`complete` / `cancel` / `sequential`) is
    /// an aver.toml runtime policy and is NOT carried in MIR.
    IndependentProduct(Spanned<MirIndependentProduct>),
    /// Early `return value;` — used in lowered bodies that have a
    /// natural early-return shape (the `?` propagation lowering
    /// inside a backend is the canonical example). Functions that
    /// don't return early end their body with the final expression
    /// itself; `Return` is only for the explicit early-exit case.
    Return(Box<Spanned<MirExpr>>),
}

/// `let binding = value; body`.
///
/// Phase 5 wave-Let foundation: `binding_name` carries the
/// source-level binder when the let came from `Stmt::Binding`,
/// or stays empty for synthetic locals introduced by stmt-chain
/// lowering (`Stmt::Expr` at non-tail position). Same propagation
/// shape `MirLocal { name }` uses on the read side, so Rust /
/// wasm-gc backends can emit `let x = …` for source-named
/// bindings and fall back to HIR for the unnamed synthetics.
#[derive(Debug, Clone)]
pub struct MirLet {
    pub binding: LocalId,
    pub binding_name: String,
    pub value: Box<Spanned<MirExpr>>,
    pub body: Box<Spanned<MirExpr>>,
}

/// Apply `callee` to `args`.
#[derive(Debug, Clone)]
pub struct MirCall {
    pub callee: MirCallee,
    pub args: Vec<Spanned<MirExpr>>,
}

/// What we're calling. Two flavors during Phase 2–3; richer
/// Built-in callees lift to `BuiltinId` since Phase 6 wave 11
/// — backends look up the canonical name via
/// `SymbolTable::builtin_entry(id)` when they need to dispatch
/// against the existing string-keyed runtime registries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirCallee {
    /// User-defined function (any module, including the current
    /// one). Resolved at HIR → MIR lowering — never a string.
    Fn(FnId),
    /// Built-in registered in the runtime's builtin table
    /// (`Console.print`, `List.prepend`, …). Interned via
    /// `SymbolTable::intern_builtin` at lowering time; consumers
    /// resolve the canonical name through
    /// `SymbolTable::builtin_entry(id).name`.
    Builtin(BuiltinId),
    /// Synthesis-only intrinsic — the buffer-build / stringify ops the
    /// deforestation pass (`interp_lower`) emits for `String.join` and
    /// interpolation chains (`__buf_new` / `__buf_append` /
    /// `__buf_append_sep_unless_first` / `__buf_finalize` / `__to_str`).
    /// Never user-visible; carried so the MIR walker emits the same
    /// `BUFFER_*` / CONCAT opcodes the HIR walker does instead of
    /// dropping the whole fn to the HIR fallback.
    Intrinsic(BuiltinIntrinsic),
    /// First-class fn value held in a local slot — calling a `Fn(..)`
    /// parameter or a let-bound fn value (`f(x)` where `f` is a slot).
    /// The backend pushes the slot value (the callee) then the args and
    /// dispatches dynamically (the VM's `CALL_VALUE`). `last_use` lets
    /// the read use `MOVE_LOCAL` over `LOAD_LOCAL`.
    LocalSlot { slot: u16, last_use: bool },
}

/// `target(args…)` in tail position — same SCC as the surrounding fn.
#[derive(Debug, Clone)]
pub struct MirTailCall {
    pub target: FnId,
    pub args: Vec<Spanned<MirExpr>>,
}

/// `lhs <op> rhs`.
#[derive(Debug, Clone)]
pub struct MirBinOp {
    pub op: BinOp,
    pub lhs: Box<Spanned<MirExpr>>,
    pub rhs: Box<Spanned<MirExpr>>,
}

/// Structured match expression.
#[derive(Debug, Clone)]
pub struct MirMatch {
    pub subject: Box<Spanned<MirExpr>>,
    pub arms: Vec<MirMatchArm>,
}

/// `if cond { then_branch } else { else_branch }` — direct
/// conditional, no pattern dispatch. Phase 6 wave 9 introduces
/// this so every backend gets it for free instead of
/// re-implementing the "two-arm bool match → if/else"
/// recognition (HIR's `try_emit_bool_if_else` etc). The
/// optimizer pass `bool_match_to_if` rewrites qualifying
/// `Match` nodes into `IfThenElse`; backends consume only the
/// rewritten form.
#[derive(Debug, Clone)]
pub struct MirIfThenElse {
    pub cond: Box<Spanned<MirExpr>>,
    pub then_branch: Box<Spanned<MirExpr>>,
    pub else_branch: Box<Spanned<MirExpr>>,
}

/// One arm of a `match`. Pattern picks the variant; `body` is the
/// value produced when this arm fires.
#[derive(Debug, Clone)]
pub struct MirMatchArm {
    pub pattern: MirPattern,
    pub body: Spanned<MirExpr>,
}

/// Pattern shape for `match` arms. Identity-typed where applicable
/// (constructor patterns reference `CtorId`); `LocalId` is the
/// fresh local introduced by the binding form.
#[derive(Debug, Clone)]
pub enum MirPattern {
    /// `_` — catch-all, binds nothing.
    Wildcard,
    /// Literal arm: `0`, `true`, `"foo"`.
    Literal(Literal),
    /// Identifier binding — captures the matched value into a
    /// fresh local accessible in the arm body. The `String`
    /// carries the source-level binder name (Phase 5 prep) so
    /// Rust / wasm-gc walkers can emit the binding ident.
    Bind(LocalId, String),
    /// `[]` — empty-list pattern.
    EmptyList,
    /// `[head, ..tail]` — cons pattern; both bindings fresh.
    /// `head_name` / `tail_name` carry the source idents (Phase 5
    /// prep) for backends that emit named locals.
    Cons {
        head: LocalId,
        head_name: String,
        tail: LocalId,
        tail_name: String,
    },
    /// `(a, b, c)` — tuple pattern; each component is a sub-pattern.
    Tuple(Vec<MirPattern>),
    /// `Module.Variant(b1, b2, …)` / `Result.Ok(b)` / `Option.None`
    /// — constructor pattern. `ctor` discriminates user vs built-in
    /// variant via `MirCtor`; `bindings` are the fresh locals for
    /// the variant's fields in declaration order (empty for
    /// nullary variants like `Option.None`). `binding_names` is a
    /// parallel array of source idents (Phase 5 prep) — same
    /// length as `bindings`.
    Ctor {
        ctor: MirCtor,
        bindings: Vec<LocalId>,
        binding_names: Vec<String>,
    },
}

/// Construct a sum-type variant. `ctor` discriminates user vs
/// built-in via `MirCtor` (wave 3c-i — built-in ctors now ride
/// the same node as user ctors instead of being dropped from the
/// MIR program).
#[derive(Debug, Clone)]
pub struct MirConstruct {
    pub ctor: MirCtor,
    pub args: Vec<Spanned<MirExpr>>,
}

/// Build a fresh record.
#[derive(Debug, Clone)]
pub struct MirRecordCreate {
    pub type_id: TypeId,
    pub fields: Vec<MirRecordField>,
}

/// `T.update(base, …)` — produce a record matching `base` except
/// for the named field overrides.
#[derive(Debug, Clone)]
pub struct MirRecordUpdate {
    pub base: Box<Spanned<MirExpr>>,
    pub type_id: TypeId,
    pub updates: Vec<MirRecordField>,
}

/// One `field = value` pair inside a record create or update.
#[derive(Debug, Clone)]
pub struct MirRecordField {
    pub name: String,
    pub value: Spanned<MirExpr>,
}

/// `base.field` projection.
#[derive(Debug, Clone)]
pub struct MirProject {
    pub base: Box<Spanned<MirExpr>>,
    pub field: String,
}

/// `(a, b, c)!` or `(a, b, c)?!`.
#[derive(Debug, Clone)]
pub struct MirIndependentProduct {
    pub items: Vec<Spanned<MirExpr>>,
    /// `true` for `?!` (unwrap `Ok`, propagate first `Err`);
    /// `false` for `!` (raw tuple of `Result`s).
    pub unwrap_results: bool,
}

/// Part of an interpolated string.
#[derive(Debug, Clone)]
pub enum MirStrPart {
    /// Literal text between interpolation slots.
    Literal(String),
    /// `{expr}` — value gets stringified at runtime.
    Expr(Spanned<MirExpr>),
}

/// Declared effect on a `MirFn`. Carries the source name
/// (`"Console.print"`, `"Disk.readText"`) for now; a later phase
/// may swap in `EffectId` for typed identity once the effect
/// registry grows ids.
#[derive(Debug, Clone)]
pub struct MirEffectAnnotation {
    pub name: String,
}

// `Spanned<T>` is re-used from `crate::ast::Spanned` — it already
// carries a `SourceLine` plus a `OnceLock<Type>` type stamp, which
// matches MIR's need to surface type-check results into the
// executable substrate for later optimization passes. Defining a
// second MIR-private `Spanned` would mean either dropping the type
// stamp (losing information) or duplicating the OnceLock wiring;
// reusing the AST helper keeps lowering trivial and the dump
// output uniform across IR layers.
