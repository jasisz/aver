//! Resolved AST / High-level IR — typed-identity layer between
//! [`crate::ast`] and the backends.
//!
//! ## Why this layer exists
//!
//! Pre-Phase-E backends consumed `crate::ast::Expr` directly. That
//! shape carries source-level names everywhere: `Expr::FnCall(Ident
//! "foo")` for a call, `Expr::Constructor("Result.Ok", arg)` for a
//! variant, `Expr::RecordCreate { type_name: "Shape", … }` for a
//! record. Every backend (VM compiler, Rust codegen, wasm-gc, Lean,
//! Dafny, self-host) re-derived identity from those strings — six
//! independent resolvers each guessing the same answer, drifting
//! whenever a corner case touched only one of them. The proof IR
//! layer (PRs #141–#146) moved the proof flow onto opaque `FnId` /
//! `TypeId` / `CtorId`; Phase B (#148) did the same for the
//! typechecker's internal `fn_sigs` + matcher. Phase E generalises
//! the move to a shared resolved AST every backend reads.
//!
//! ## Contract
//!
//! [`ResolvedExpr`] is a mechanical translation of `Expr` — same
//! shape, same operator precedence, same evaluation order. The only
//! difference is that every name that referred to a declared symbol
//! is replaced by its opaque ID:
//!
//! - `FnCall(Box<Spanned<Expr>>, …)` → [`ResolvedExpr::Call`] with
//!   a [`ResolvedCallee`] discriminating user fn / builtin namespace
//!   method / local lambda / ambient operator.
//! - `Constructor(String, …)` → [`ResolvedExpr::Ctor`] with a
//!   [`ResolvedCtor`] discriminating user `CtorId` / built-in
//!   variant (`Result.Ok` / `Result.Err` / `Option.Some` /
//!   `Option.None`).
//! - `RecordCreate { type_name, fields }` → [`ResolvedExpr::RecordCreate`]
//!   with a `TypeId` (always `Some` for user records — built-in
//!   record types stay `Type::Named { id: None, … }` everywhere,
//!   including here).
//! - `RecordUpdate` → [`ResolvedExpr::RecordUpdate`] same.
//! - `TailCall(TailCallData)` → [`ResolvedExpr::TailCall`] with a
//!   `FnId` for the target.
//! - `Pattern::Constructor(name, bindings)` → [`ResolvedPattern::Ctor`]
//!   with a [`ResolvedCtor`].
//! - `Stmt::Binding(name, Option<String>, expr)` →
//!   [`ResolvedStmt::Binding`] with a resolved
//!   [`crate::ast::Type`] annotation instead of a string.
//!
//! Anything that doesn't reference a declared symbol passes through
//! structurally — literals, binops, neg, list / tuple / map
//! literals, interpolation, independent products, error-prop, slot
//! resolutions (`Expr::Resolved`), match arms (recursively
//! resolved).
//!
//! ## What this layer does NOT do
//!
//! - **No semantic information added** beyond identity. Refinement
//!   subtype decisions, recursion contracts, law theorems — all of
//!   that stays in [`crate::ir::ProofIR`]. Resolved HIR is the
//!   universal substrate, not a proof IR.
//! - **No optimization**. The lowering is one-to-one; the same
//!   number of expressions land on the other side.
//! - **No effect propagation rewriting**. Effects stay on the
//!   resolved fn signature exactly as the typechecker classified
//!   them.
//!
//! ## What's NOT in this PR
//!
//! This module ships the shape only — definitions + smoke tests.
//! Future PRs in the Phase E stack:
//!
//! - **PR 2** (`name_resolve` pass): builds [`ResolvedExpr`] from
//!   `Expr` by walking the AST against a `SymbolTable` +
//!   typechecker result.
//! - **PR 3+** (one per backend): each backend migrates from
//!   consuming `Expr` to [`ResolvedExpr`]. Old paths stay as
//!   fallback during migration.
//! - **PR N** (cleanup): drop pre-resolve consumers, remove the
//!   bare-string lookups that the resolved layer subsumes (Phase D
//!   in #147 terminology).

use crate::ast::{AnnotBool, BinOp, FnResolution, Literal, Module, Spanned, Type};
use crate::ir::identity::{CtorId, FnId, TypeId};

pub mod classify;
pub mod dump;
pub mod resolve;
pub use classify::{
    ForwardSlot, ResolvedBodyBindingPlan, ResolvedBodyExprPlan, ResolvedBodyPlan,
    ResolvedBoolSubjectPlan, ResolvedForwardCallPlan, ResolvedLeafOp, ResolvedThinBodyPlan,
    ThinKind, call_plan_from_resolved_callee, classify_body_expr_plan_resolved,
    classify_body_plan_resolved, classify_bool_match_shape_resolved,
    classify_bool_subject_plan_resolved, classify_dispatch_pattern_resolved,
    classify_dispatch_table_shape_resolved, classify_forward_call_resolved,
    classify_leaf_op_resolved, classify_list_match_shape_resolved,
    classify_match_dispatch_plan_resolved, classify_thin_fn_def_resolved, resolved_to_dotted,
    semantic_constructor_from_resolved_ctor,
};
pub use dump::{dump_resolved_expr, dump_resolved_program};
pub use resolve::{ResolveCtx, resolve_fn_def_external, resolve_program, resolve_top_level};

/// Count every `ResolvedCallee::Unresolved` and
/// `ResolvedCtor::Unresolved` reachable from `fd`'s body. The Phase E
/// contract for `resolved_items` is **zero unresolved for well-typed
/// programs** — non-zero means either the typechecker already
/// rejected the program (resolver bailed to recovery) or there's a
/// resolver gap. CI invariants compare this count against typecheck
/// errors to catch silent regressions.
pub fn count_unresolved_in_fn(fd: &ResolvedFnDef) -> usize {
    let mut count = 0;
    count_unresolved_in_body(&fd.body, &mut count);
    count
}

fn count_unresolved_in_body(body: &ResolvedFnBody, out: &mut usize) {
    match body {
        ResolvedFnBody::Block(stmts) => {
            for stmt in stmts {
                count_unresolved_in_stmt(stmt, out);
            }
        }
    }
}

fn count_unresolved_in_stmt(stmt: &ResolvedStmt, out: &mut usize) {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            count_unresolved_in_expr(&value.node, out)
        }
    }
}

fn count_unresolved_in_expr(expr: &ResolvedExpr, out: &mut usize) {
    match expr {
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
        ResolvedExpr::Attr(obj, _) => count_unresolved_in_expr(&obj.node, out),
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Unresolved { callee } = callee {
                *out += 1;
                count_unresolved_in_expr(&callee.node, out);
            }
            for a in args {
                count_unresolved_in_expr(&a.node, out);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            count_unresolved_in_expr(&l.node, out);
            count_unresolved_in_expr(&r.node, out);
        }
        ResolvedExpr::Neg(inner) | ResolvedExpr::ErrorProp(inner) => {
            count_unresolved_in_expr(&inner.node, out)
        }
        ResolvedExpr::Match { subject, arms } => {
            count_unresolved_in_expr(&subject.node, out);
            for arm in arms {
                count_unresolved_in_pattern(&arm.pattern, out);
                count_unresolved_in_expr(&arm.body.node, out);
            }
        }
        ResolvedExpr::Ctor(ctor, args) => {
            if matches!(ctor, ResolvedCtor::Unresolved { .. }) {
                *out += 1;
            }
            for a in args {
                count_unresolved_in_expr(&a.node, out);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let ResolvedStrPart::Parsed(inner) = p {
                    count_unresolved_in_expr(&inner.node, out);
                }
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for i in items {
                count_unresolved_in_expr(&i.node, out);
            }
        }
        ResolvedExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                count_unresolved_in_expr(&k.node, out);
                count_unresolved_in_expr(&v.node, out);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                count_unresolved_in_expr(&e.node, out);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            count_unresolved_in_expr(&base.node, out);
            for (_, e) in updates {
                count_unresolved_in_expr(&e.node, out);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                count_unresolved_in_expr(&a.node, out);
            }
        }
    }
}

fn count_unresolved_in_pattern(pat: &ResolvedPattern, out: &mut usize) {
    match pat {
        ResolvedPattern::Wildcard
        | ResolvedPattern::Literal(_)
        | ResolvedPattern::Ident(_)
        | ResolvedPattern::EmptyList
        | ResolvedPattern::Cons(_, _) => {}
        ResolvedPattern::Tuple(items) => {
            for p in items {
                count_unresolved_in_pattern(p, out);
            }
        }
        ResolvedPattern::Ctor(ctor, _) => {
            if matches!(ctor, ResolvedCtor::Unresolved { .. }) {
                *out += 1;
            }
        }
    }
}

/// Resolved expression — the mechanical mirror of [`crate::ast::Expr`].
///
/// Every variant that referenced a declared symbol by string in
/// `Expr` now carries its opaque ID. Variants that didn't reference
/// any declaration pass through unchanged in shape.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedExpr {
    /// Literal — untouched.
    Literal(Literal),
    /// Source-level identifier that the resolver pass could NOT
    /// classify into a `Resolved` slot, a fn ref, or a constructor.
    /// Typically a top-level / global binding name; backends look it
    /// up in the post-resolve global table.
    ///
    /// After full Phase E + D, this variant should rarely survive —
    /// every name has either a slot, an `FnId`, a `CtorId`, or
    /// fails to type-check. Kept here as the safety hatch during
    /// migration.
    Ident(String),
    /// Compiled local-slot reference. Identical to `Expr::Resolved`
    /// — the slot resolver runs before name-resolve and its output
    /// is already typed identity.
    Resolved {
        slot: u16,
        name: String,
        last_use: AnnotBool,
    },
    /// Field / namespace projection. The object is recursively
    /// resolved; the field name stays as source text because record
    /// fields are scoped under their owning `TypeId`, not in a
    /// global namespace.
    Attr(Box<Spanned<ResolvedExpr>>, String),
    /// Function call — `Expr::FnCall(callee, args)` lifted into a
    /// [`ResolvedCallee`].
    Call(ResolvedCallee, Vec<Spanned<ResolvedExpr>>),
    /// Binary operator.
    BinOp(
        BinOp,
        Box<Spanned<ResolvedExpr>>,
        Box<Spanned<ResolvedExpr>>,
    ),
    /// Unary minus.
    Neg(Box<Spanned<ResolvedExpr>>),
    /// `match subject { arm, … }`.
    Match {
        subject: Box<Spanned<ResolvedExpr>>,
        arms: Vec<ResolvedMatchArm>,
    },
    /// Constructor call — `Result.Ok(v)`, `Shape.Circle(r)`, etc.
    Ctor(ResolvedCtor, Vec<Spanned<ResolvedExpr>>),
    /// Result-error propagation: `expr?`.
    ErrorProp(Box<Spanned<ResolvedExpr>>),
    /// Interpolated string `"a${x}b"`.
    InterpolatedStr(Vec<ResolvedStrPart>),
    /// `[a, b, c]` list literal.
    List(Vec<Spanned<ResolvedExpr>>),
    /// `(a, b, c)` tuple literal.
    Tuple(Vec<Spanned<ResolvedExpr>>),
    /// `{"a" => 1, "b" => 2}` map literal.
    MapLiteral(Vec<(Spanned<ResolvedExpr>, Spanned<ResolvedExpr>)>),
    /// `Shape(name = "...", count = 0)` record-create form. The
    /// `type_id` is `Some` for user records resolved through the
    /// symbol table, `None` for built-in record types (`HttpResponse`,
    /// `Header`, `Tcp.Connection`, `Buffer`, …) which don't carry
    /// `TypeId`s by design.
    RecordCreate {
        type_id: Option<TypeId>,
        /// Source-level type name kept verbatim for diagnostics +
        /// backend codegen mangling. Mirrors `Type::Named { name }`.
        type_name: String,
        fields: Vec<(String, Spanned<ResolvedExpr>)>,
    },
    /// `Shape.update(base, field = newVal, …)` record-update.
    RecordUpdate {
        type_id: Option<TypeId>,
        type_name: String,
        base: Box<Spanned<ResolvedExpr>>,
        updates: Vec<(String, Spanned<ResolvedExpr>)>,
    },
    /// Tail-position call (SCC peer). `target` is the `FnId` the
    /// TCO transform pass committed to.
    TailCall {
        target: FnId,
        args: Vec<Spanned<ResolvedExpr>>,
    },
    /// Independent product `(a, b)!` or `(a, b)?!`. `unwrap` toggles
    /// the `?!` form.
    IndependentProduct(Vec<Spanned<ResolvedExpr>>, bool),
}

/// Callee classification for [`ResolvedExpr::Call`]. Replaces the
/// pre-Phase-E `FnCall(Box<Spanned<Expr>>, …)` shape where the
/// callee was an arbitrary `Expr` subtree that every backend re-
/// parsed.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedCallee {
    /// User-defined fn (entry module or any dep). Resolved by name-
    /// resolve against the program's `SymbolTable`.
    Fn(FnId),
    /// Built-in namespace method — `Int.add`, `Console.print`,
    /// `Vector.fromList`, etc. These don't have `FnId`s in the
    /// program symbol table; carrier is the canonical
    /// `"Namespace.method"` string. The lookup table for these is
    /// flat and global, so a string key is enough — and stable
    /// across compiler versions.
    Builtin(String),
    /// Compiler-synthesised intrinsic emitted by the
    /// `interp_lower` / `buffer_build` deforestation passes.
    /// Source-illegal names (`__buf_*`, `__to_str`) that only ever
    /// appear after lowering. Carrying them as a typed variant
    /// instead of `Unresolved { Ident("__buf_*") }` keeps the
    /// "well-typed → zero unresolved" invariant honest — see
    /// [`BuiltinIntrinsic`] for the enumerated set and the
    /// `name_resolve_invariant_zero_unresolved_*` tests in
    /// [`crate::ir::pipeline`].
    Intrinsic(BuiltinIntrinsic),
    /// First-class fn value bound to a local slot (lambda / fn ref
    /// passed as an argument). The slot resolver already mapped
    /// the binding; we just thread the slot through.
    LocalSlot {
        slot: u16,
        name: String,
        last_use: AnnotBool,
    },
    /// Callee shape the resolver couldn't classify — typically a
    /// type error the typechecker already reported. Backends bail
    /// out cleanly when they see this; pass exists so resolve can
    /// emit a placeholder instead of panicking.
    Unresolved {
        /// The source-faithful expression the resolver gave up on,
        /// kept for diagnostics. Resolved recursively into the same
        /// IR so the surrounding tree still walks cleanly.
        callee: Box<Spanned<ResolvedExpr>>,
    },
}

/// The enumerated set of compiler-synthesised call intrinsics. New
/// variants are added only when a lowering pass introduces a new
/// `Expr::Ident("__…")` shape; none has a `__…` user-source spelling.
/// (`IntDivEuclid` / `IntModEuclid` are additionally produced by the
/// resolver's literal-divisor discharge of source `Int.div` / `Int.mod`
/// calls with a syntactic nonzero literal divisor.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinIntrinsic {
    /// `__buf_new(<cap_hint>)` — allocate a fresh buffer slot.
    BufNew,
    /// `__buf_append(<buf>, <str>)` — concatenate a string fragment
    /// onto the host-side buffer pool entry.
    BufAppend,
    /// `__buf_append_sep_unless_first(<buf>, <sep>)` — emit the
    /// separator before every fragment except the first.
    BufAppendSepUnlessFirst,
    /// `__buf_finalize(<buf>)` — materialise the buffer pool entry
    /// into an Aver `String` value and free the slot.
    BufFinalize,
    /// `__to_str(<value>)` — coerce any value to its display string
    /// (used by interpolation lowering before `__buf_append`).
    ToStr,
    /// `__int_div_euclid(<a>, <k>)` — unchecked Euclidean (flooring)
    /// `(Int, Int) -> Int` division, total for every `k != 0` (over ℤ the
    /// old `i64::MIN / -1` overflow is just a valid large quotient).
    /// Produced by the HIR resolver for the literal-divisor discharge —
    /// `Int.div(a, K)` with a syntactic nonzero integer literal `K`
    /// (`crate::ast::is_literal_nonzero_int_divisor`, the same predicate
    /// the typechecker's discharge rule uses) — and by the MIR
    /// `const_fold` pass as a belt-and-suspenders rewrite for any literal
    /// nonzero divisor that reaches MIR as a `Builtin` call. There is
    /// still no `from_name` mapping: the source spelling is `Int.div`.
    IntDivEuclid,
    /// `__int_mod_euclid(<a>, <k>)` — unchecked Euclidean modulo
    /// `(Int, Int) -> Int`, partner of `IntDivEuclid` so that
    /// `div(a,k)*k + mod(a,k) == a` for every sign. Produced by the HIR
    /// resolver (literal-divisor discharge of `Int.mod(a, K)`) and by the
    /// MIR `const_fold` pass for a non-zero literal divisor `k`.
    IntModEuclid,
}

impl BuiltinIntrinsic {
    /// Canonical source-level name. Used by diagnostic dumps and as
    /// the bridge into the resolver / VM dispatch tables.
    pub const fn name(self) -> &'static str {
        match self {
            Self::BufNew => "__buf_new",
            Self::BufAppend => "__buf_append",
            Self::BufAppendSepUnlessFirst => "__buf_append_sep_unless_first",
            Self::BufFinalize => "__buf_finalize",
            Self::ToStr => "__to_str",
            Self::IntDivEuclid => "__int_div_euclid",
            Self::IntModEuclid => "__int_mod_euclid",
        }
    }

    /// Recognise a bare identifier as one of the known intrinsics.
    /// Returns `None` for anything else — the resolver then falls
    /// through to its regular fn / Unresolved classification.
    ///
    /// `IntDivEuclid` / `IntModEuclid` are deliberately absent: their
    /// source spelling is `Int.div` / `Int.mod` (the resolver's
    /// literal-divisor discharge produces them from the call shape, and
    /// `const_fold` synthesises them at MIR level), never a `__…`
    /// identifier.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "__buf_new" => Some(Self::BufNew),
            "__buf_append" => Some(Self::BufAppend),
            "__buf_append_sep_unless_first" => Some(Self::BufAppendSepUnlessFirst),
            "__buf_finalize" => Some(Self::BufFinalize),
            "__to_str" => Some(Self::ToStr),
            _ => None,
        }
    }
}

/// Constructor classification for [`ResolvedExpr::Ctor`] and
/// [`ResolvedPattern::Ctor`].
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedCtor {
    /// User-defined sum-type variant or record constructor.
    User {
        ctor_id: CtorId,
        /// Owning type identity — kept on the callsite so backends
        /// that pattern-match on (type, variant) don't need to walk
        /// the symbol table on every emit.
        type_id: TypeId,
        /// Source name of the variant (`"Circle"`, `"Triangle"`,
        /// the record name itself for product types). Kept for
        /// diagnostic display + backend codegen mangling.
        name: String,
    },
    /// `Result.Ok` / `Result.Err` / `Option.Some` / `Option.None`.
    /// These are language-level rather than user-defined so they
    /// don't carry `CtorId`s — discriminated by the variant.
    Builtin(BuiltinCtor),
    /// Constructor expression the resolver couldn't classify —
    /// typechecker already reported the error. Kept as a
    /// passthrough so the surrounding tree walks cleanly.
    Unresolved {
        /// Source name (`"Foo.Bar"`) preserved for diagnostics.
        name: String,
    },
}

/// Identity of the four built-in algebraic constructors. Mirror of
/// the `Result` / `Option` types that the language exposes
/// implicitly — these never get a user-program `CtorId` because
/// they're not user-declared.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinCtor {
    ResultOk,
    ResultErr,
    OptionSome,
    OptionNone,
}

/// Interpolated string piece — mirror of [`crate::ast::StrPart`].
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedStrPart {
    Literal(String),
    Parsed(Box<Spanned<ResolvedExpr>>),
}

/// One arm of a `match` — mirror of [`crate::ast::MatchArm`] with
/// the pattern lifted to [`ResolvedPattern`].
#[derive(Debug)]
pub struct ResolvedMatchArm {
    pub pattern: ResolvedPattern,
    pub body: Box<Spanned<ResolvedExpr>>,
    /// Per-arm slot table — same role as
    /// [`crate::ast::MatchArm::binding_slots`]. Populated by the
    /// resolver pass; backends read it instead of re-doing the
    /// name → slot lookup.
    pub binding_slots: std::sync::OnceLock<Vec<u16>>,
}

impl Clone for ResolvedMatchArm {
    fn clone(&self) -> Self {
        let binding_slots = std::sync::OnceLock::new();
        if let Some(v) = self.binding_slots.get() {
            let _ = binding_slots.set(v.clone());
        }
        Self {
            pattern: self.pattern.clone(),
            body: self.body.clone(),
            binding_slots,
        }
    }
}

impl PartialEq for ResolvedMatchArm {
    fn eq(&self, other: &Self) -> bool {
        // `binding_slots` is filled by a later pass and isn't part
        // of structural identity — same rule [`MatchArm`] uses.
        self.pattern == other.pattern && self.body == other.body
    }
}

/// Pattern shape — mirror of [`crate::ast::Pattern`].
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedPattern {
    Wildcard,
    Literal(Literal),
    Ident(String),
    EmptyList,
    Cons(String, String),
    Tuple(Vec<ResolvedPattern>),
    /// Constructor pattern — `Result.Ok(x)`, `Shape.Circle(r)`,
    /// `Shape.Point`. Resolved to a [`ResolvedCtor`] +
    /// pattern-binding names.
    Ctor(ResolvedCtor, Vec<String>),
}

/// Statement form — mirror of [`crate::ast::Stmt`] with the
/// optional type annotation lifted to [`Type`] (already canonicalised
/// against the resolver's symbol table) instead of a source string.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedStmt {
    Binding {
        name: String,
        ty_ann: Option<Type>,
        value: Spanned<ResolvedExpr>,
    },
    Expr(Spanned<ResolvedExpr>),
}

/// Function body — mirror of [`crate::ast::FnBody`].
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedFnBody {
    Block(Vec<ResolvedStmt>),
}

impl ResolvedFnBody {
    pub fn stmts(&self) -> &[ResolvedStmt] {
        match self {
            Self::Block(stmts) => stmts,
        }
    }
}

/// Resolved fn definition. Mirrors [`crate::ast::FnDef`] but with
/// signature types parsed to [`Type`] (and resolved through the
/// owner module's resolver context, see #148 round 6) instead of
/// source strings, plus the body lifted to [`ResolvedFnBody`].
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedFnDef {
    /// Stable opaque identity of this fn. The pre-resolve `FnDef`
    /// only carried a source name; here we promote identity to a
    /// first-class field.
    pub fn_id: FnId,
    /// Source-level fn name. Kept for diagnostics + backend
    /// codegen mangling.
    pub name: String,
    pub line: usize,
    /// Parameters: `(binding_name, resolved_param_type)`. The
    /// resolver canonicalises each annotation through the
    /// declaring module's own resolver context.
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub effects: Vec<Spanned<String>>,
    pub desc: Option<String>,
    pub body: std::sync::Arc<ResolvedFnBody>,
    /// Slot-resolver output — see [`FnResolution`]. Carried through
    /// unchanged.
    pub resolution: Option<FnResolution>,
}

/// Resolved top-level item — mirror of [`crate::ast::TopLevel`].
/// `Verify`, `Decision`, `TypeDef` items pass through with their
/// original AST representation: they aren't on the runtime hot
/// path, and their internal expressions get resolved lazily by
/// proof-export passes that already consume `Expr`. Future PRs may
/// promote them.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTopLevel {
    Module(Module),
    FnDef(ResolvedFnDef),
    /// Verify / Decision / TypeDef items: passthrough for now.
    /// Each carries its original AST node — the resolver lifts only
    /// what runtime backends consume. See module doc for the
    /// rationale + the future PR that promotes these.
    Passthrough(crate::ast::TopLevel),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::identity::TypeId;

    #[test]
    fn resolved_callee_user_fn_carries_typed_identity() {
        let callee = ResolvedCallee::Fn(FnId(7));
        let call = ResolvedExpr::Call(
            callee,
            vec![Spanned::new(ResolvedExpr::Literal(Literal::Int(42)), 3)],
        );
        // Smoke check: shape constructs and the FnId survives clone.
        let clone = call.clone();
        assert_eq!(clone, call);
        match clone {
            ResolvedExpr::Call(ResolvedCallee::Fn(id), args) => {
                assert_eq!(id, FnId(7));
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected ResolvedExpr::Call(Fn, _)"),
        }
    }

    #[test]
    fn resolved_ctor_user_carries_ctor_and_type_id() {
        let ctor = ResolvedCtor::User {
            ctor_id: CtorId(2),
            type_id: TypeId(5),
            name: "Circle".to_string(),
        };
        let expr = ResolvedExpr::Ctor(
            ctor,
            vec![Spanned::new(ResolvedExpr::Literal(Literal::Float(1.0)), 1)],
        );
        let ResolvedExpr::Ctor(
            ResolvedCtor::User {
                ctor_id,
                type_id,
                name,
            },
            ..,
        ) = expr
        else {
            panic!("expected User ctor variant")
        };
        assert_eq!(ctor_id, CtorId(2));
        assert_eq!(type_id, TypeId(5));
        assert_eq!(name, "Circle");
    }

    #[test]
    fn resolved_ctor_builtin_variants_distinguish() {
        // Round-trip every builtin variant — guard against an
        // accidental rename / collapse.
        let variants = [
            BuiltinCtor::ResultOk,
            BuiltinCtor::ResultErr,
            BuiltinCtor::OptionSome,
            BuiltinCtor::OptionNone,
        ];
        for v in variants {
            let c = ResolvedCtor::Builtin(v);
            match c {
                ResolvedCtor::Builtin(got) => assert_eq!(got, v),
                _ => panic!("builtin ctor lost variant kind"),
            }
        }
    }

    #[test]
    fn resolved_pattern_ctor_round_trips_through_clone() {
        let pat = ResolvedPattern::Ctor(
            ResolvedCtor::User {
                ctor_id: CtorId(11),
                type_id: TypeId(3),
                name: "Square".to_string(),
            },
            vec!["side".to_string()],
        );
        assert_eq!(pat.clone(), pat);
    }

    #[test]
    fn resolved_match_arm_equality_ignores_binding_slots() {
        // Mirror of `MatchArm`'s structural equality rule — two
        // arms with the same pattern + body must compare equal
        // regardless of whether the slot table has been filled in.
        let body = Box::new(Spanned::new(ResolvedExpr::Literal(Literal::Int(0)), 1));
        let a = ResolvedMatchArm {
            pattern: ResolvedPattern::Wildcard,
            body: body.clone(),
            binding_slots: std::sync::OnceLock::new(),
        };
        let b = ResolvedMatchArm {
            pattern: ResolvedPattern::Wildcard,
            body,
            binding_slots: {
                let lock = std::sync::OnceLock::new();
                let _ = lock.set(vec![3, 4]);
                lock
            },
        };
        assert_eq!(a, b);
    }

    #[test]
    fn resolved_fn_def_carries_fn_id_and_resolved_param_types() {
        let body = ResolvedFnBody::Block(vec![ResolvedStmt::Expr(Spanned::new(
            ResolvedExpr::Literal(Literal::Int(1)),
            1,
        ))]);
        let def = ResolvedFnDef {
            fn_id: FnId(0),
            name: "id".to_string(),
            line: 1,
            params: vec![("x".to_string(), Type::Int)],
            return_type: Type::Int,
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(body),
            resolution: None,
        };
        assert_eq!(def.fn_id, FnId(0));
        assert_eq!(def.params[0].1, Type::Int);
        assert_eq!(def.return_type, Type::Int);
        assert_eq!(def.body.stmts().len(), 1);
    }

    #[test]
    fn resolved_stmt_binding_threads_optional_resolved_type_ann() {
        // `Stmt::Binding(name, Option<String>, …)` in the pre-Phase-E
        // AST stored the annotation as a source string. Resolved
        // form lifts to `Option<Type>`.
        let stmt_with = ResolvedStmt::Binding {
            name: "n".to_string(),
            ty_ann: Some(Type::Int),
            value: Spanned::new(ResolvedExpr::Literal(Literal::Int(0)), 1),
        };
        let stmt_without = ResolvedStmt::Binding {
            name: "n".to_string(),
            ty_ann: None,
            value: Spanned::new(ResolvedExpr::Literal(Literal::Int(0)), 1),
        };
        assert_ne!(stmt_with, stmt_without);
    }

    #[test]
    fn resolved_callee_unresolved_passes_through_inner_expr() {
        // The Unresolved escape hatch lets the resolver emit a
        // placeholder when it can't classify the callee — every
        // backend uses the same bail-out path. This test just
        // verifies it constructs and clones.
        let inner = Spanned::new(ResolvedExpr::Ident("dynamic".to_string()), 1);
        let call = ResolvedExpr::Call(
            ResolvedCallee::Unresolved {
                callee: Box::new(inner.clone()),
            },
            vec![],
        );
        let clone = call.clone();
        assert_eq!(clone, call);
    }

    // Compile-time documentation: the structural mapping
    // `Expr → ResolvedExpr` is exhaustive — every variant `Expr`
    // can produce maps to a counterpart here. Asserting this in
    // code keeps reviewers honest: when a new `Expr` variant lands,
    // either this list gains a row or the compiler stops building.
    #[test]
    fn variant_coverage_matches_expr() {
        use crate::ast::Expr;
        fn cover(expr: &Expr) -> &'static str {
            match expr {
                Expr::Literal(_) => "Literal → Literal",
                Expr::Ident(_) => "Ident → Ident",
                Expr::Resolved { .. } => "Resolved → Resolved",
                Expr::Attr(_, _) => "Attr → Attr",
                Expr::FnCall(_, _) => "FnCall → Call",
                Expr::BinOp(_, _, _) => "BinOp → BinOp",
                Expr::Neg(_) => "Neg → Neg",
                Expr::Match { .. } => "Match → Match",
                Expr::Constructor(_, _) => "Constructor → Ctor",
                Expr::ErrorProp(_) => "ErrorProp → ErrorProp",
                Expr::InterpolatedStr(_) => "InterpolatedStr → InterpolatedStr",
                Expr::List(_) => "List → List",
                Expr::Tuple(_) => "Tuple → Tuple",
                Expr::MapLiteral(_) => "MapLiteral → MapLiteral",
                Expr::RecordCreate { .. } => "RecordCreate → RecordCreate",
                Expr::RecordUpdate { .. } => "RecordUpdate → RecordUpdate",
                Expr::TailCall(_) => "TailCall → TailCall",
                Expr::IndependentProduct(_, _) => "IndependentProduct → IndependentProduct",
            }
        }
        // Trivial probe — the real assertion is the `match`'s
        // exhaustiveness check at compile time.
        let probe = Expr::Literal(Literal::Int(0));
        assert!(cover(&probe).contains("Literal"));
        // Mirror sentinel — touching every `ResolvedExpr` variant
        // here keeps the symmetry honest from the other side too.
        let _: ResolvedExpr = ResolvedExpr::Literal(Literal::Int(0));
        let _: ResolvedExpr = ResolvedExpr::Ident(String::new());
        let _: ResolvedExpr = ResolvedExpr::Resolved {
            slot: 0,
            name: String::new(),
            last_use: AnnotBool(false),
        };
        let dummy = || Box::new(Spanned::new(ResolvedExpr::Literal(Literal::Int(0)), 1));
        let _: ResolvedExpr = ResolvedExpr::Attr(dummy(), String::new());
        let _: ResolvedExpr = ResolvedExpr::Call(ResolvedCallee::Fn(FnId(0)), vec![]);
        let _: ResolvedExpr = ResolvedExpr::BinOp(BinOp::Add, dummy(), dummy());
        let _: ResolvedExpr = ResolvedExpr::Neg(dummy());
        let _: ResolvedExpr = ResolvedExpr::Match {
            subject: dummy(),
            arms: vec![],
        };
        let _: ResolvedExpr =
            ResolvedExpr::Ctor(ResolvedCtor::Builtin(BuiltinCtor::OptionNone), vec![]);
        let _: ResolvedExpr = ResolvedExpr::ErrorProp(dummy());
        let _: ResolvedExpr = ResolvedExpr::InterpolatedStr(vec![]);
        let _: ResolvedExpr = ResolvedExpr::List(vec![]);
        let _: ResolvedExpr = ResolvedExpr::Tuple(vec![]);
        let _: ResolvedExpr = ResolvedExpr::MapLiteral(vec![]);
        let _: ResolvedExpr = ResolvedExpr::RecordCreate {
            type_id: None,
            type_name: String::new(),
            fields: vec![],
        };
        let _: ResolvedExpr = ResolvedExpr::RecordUpdate {
            type_id: None,
            type_name: String::new(),
            base: dummy(),
            updates: vec![],
        };
        let _: ResolvedExpr = ResolvedExpr::TailCall {
            target: FnId(0),
            args: vec![],
        };
        let _: ResolvedExpr = ResolvedExpr::IndependentProduct(vec![], false);
    }
}
