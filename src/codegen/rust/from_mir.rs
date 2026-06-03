//! Rust backend: emit expressions from Core MIR.
//!
//! Mirror of [`super::expr::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and emits the
//! same Rust source string — the same deduplication MIR brought to the
//! VM: one semantic walker per construct lives in MIR, and every backend
//! reads from it instead of forking `ResolvedExpr`.
//!
//! [`emit_mir_expr`] is the dispatcher; [`coverage_report`] measures how
//! much of a program it can render standalone. [`emit_mir_fn_body`] wraps
//! it into the full single-expr-plan body format the HIR walker emits,
//! and [`parity_gated_body`] is the Wave-1 production wire-up: for each
//! fn it compares the MIR-walker body against the HIR-walker body and
//! emits the MIR one **only when byte-identical** (counting it
//! "graduated"), else falls back to HIR. The byte-exact gate makes the
//! production output identical to HIR by construction — it cannot
//! regress — while the MIR path is exercised + verified for the
//! graduated subset on every compile. A construct the walker returns
//! `None` for (or any borrow decision that doesn't match HIR) blocks
//! graduation and the fn falls back to the HIR walker.
//!
//! ## Covered constructs
//!
//! `Literal`, `Local`, `Neg`, `BinOp` (numeric ops, plus `Str` `+`
//! concat — the right side borrowed for `AverStr`'s `Add<&AverStr>` —
//! disambiguated from numeric add by the operands' type stamps),
//! `Call` (`Fn` / `Builtin` / `Intrinsic` / `LocalSlot` — the last a
//! first-class fn-pointer call `name(args…)`, post-#379 always a plain
//! fn-pointer since `Type::Fn` is param-only), `Return`, `TailCall`
//! (emitted as a plain call; the HIR self-TCO `continue` rewrite needs
//! `ectx`, so the wire-up's parity check is the safety net), `Try` (`?`),
//! `Tuple`, `List`, `MapLiteral`, `Let` (block-expr `{ let x = …; … }`),
//! `Project`, `RecordCreate` / `RecordUpdate`, `Construct` (built-in and
//! user ctors, including dep-module records resolved through
//! `module_prefixes`), `IfThenElse`, `IndependentProduct`, and `FnValue`
//! (a fn referenced as a value — the `StaticRef` shape).
//!
//! `Match` (Wave 2) — `MirExpr::Match` emits through [`emit_mir_match`],
//! mirroring HIR's `emit_match` / `emit_dispatch_table_match` /
//! `emit_list_match` selection byte-for-byte. The shared classifiers
//! (`classify_match_dispatch_plan_resolved` etc.) + `emit_pattern` +
//! the dispatch/list emitters are reused directly by translating each
//! `MirPattern` → `ResolvedPattern` and feeding a `body_for_arm`
//! closure that renders the matching arm's MIR body. Bool two-arm
//! matches never reach this arm — the MIR optimizer's `bool_match_to_if`
//! pass already rewrote them to `IfThenElse` (handled above).
//!
//! `InterpolatedStr` never reaches the walker — `interp_lower` lowers it
//! away before codegen runs. With `FnValue` + `LocalSlot` graduated
//! (W6/Stage-0), every reachable MIR construct now has a walker arm; the
//! remaining HIR fallbacks are per-fn byte-parity misses (e.g. the
//! TCO-loop `continue` rewrite), not construct gaps.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Spanned, Type};
use crate::codegen::CodegenContext;
use crate::codegen::common::module_prefix_to_rust_path;
use crate::ir::hir::{
    BuiltinCtor, BuiltinIntrinsic, ResolvedCtor, ResolvedMatchArm, ResolvedPattern,
    classify_match_dispatch_plan_resolved,
};
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirLocal, MirMatch, MirPattern, MirProgram};
use crate::ir::{MatchDispatchPlan, SymbolTable};

use super::emit_ctx::{is_copy_type, should_borrow_param};
use super::expr::{
    callee_borrow_mask, constructor_boxed_positions, emit_dispatch_table_match, emit_list_match,
    emit_literal, emit_parallel_result_tuple_unwrap, emit_pattern_rebindings,
    emit_ref_match_rebindings, emit_result_tuple_unwrap, emit_tuple_from_vars, has_list_patterns,
    has_string_literal_patterns,
};
use super::pattern::emit_pattern;
use super::syntax::aver_name_to_rust;

/// Walker-side emit context. Holds the slice of the
/// `CodegenContext` the MIR-to-Rust walker reads — kept explicit
/// so future `CodegenContext` refactors don't ripple through the
/// walker, and so other backends (wasm-gc, wasip2) can introduce
/// their own emit-ctx structs without inheriting Rust-specific
/// fields.
///
/// Two distinct shapes share this struct:
///
/// - **coverage / test** (`for_test`): only `symbol_table` +
///   `module_prefixes` are populated; `codegen` is `None` and the
///   borrow fields are empty. The coverage walk only asks "does
///   this fn emit `Some`", so it never needs the borrow machinery
///   or the full `CodegenContext`.
/// - **production parity gate** (`for_fn`): carries the full
///   `&CodegenContext` plus the per-fn borrow policy
///   (`local_types` / `rc_wrapped` / `borrowed_params` /
///   `current_module_scope`), recomputed from the `ResolvedFnDef`
///   the HIR walker uses. This is the slice of
///   [`super::emit_ctx::EmitCtx`] the covered arms need so their
///   clone / borrow / `Arc::new` decisions match HIR byte-for-byte.
#[derive(Clone, Copy)]
pub struct MirEmitCtx<'a> {
    pub symbol_table: &'a SymbolTable,
    pub module_prefixes: &'a HashSet<String>,
    /// Full codegen context — `Some` only on the production parity
    /// gate path. `constructor_boxed_positions` /
    /// `callee_borrow_mask` need it; the coverage walk leaves it
    /// `None` (no borrow decisions, just structural reach).
    pub codegen: Option<&'a CodegenContext>,
    /// Local variable types (fn params + let bindings) for
    /// copy-type elision. Empty on the coverage path.
    pub local_types: &'a HashMap<String, Type>,
    /// Params passed as `Rc<T>` (self-TCO) / `&T` (mutual-TCO).
    pub rc_wrapped: &'a HashSet<String>,
    /// Params emitted as `&T` (borrow-by-default for non-Copy,
    /// non-Str params).
    pub borrowed_params: &'a HashSet<String>,
    /// Owning module prefix for the fn whose body this ctx emits.
    pub current_module_scope: Option<&'a str>,
    /// Interned built-in fn names, indexed by `BuiltinId`
    /// (`MirProgram.builtins`). The `Call(Builtin(id))` arm resolves
    /// `id` → dotted name through this slice, mirroring wasm-gc's
    /// `ctx.mir_builtins`. Empty on the coverage / test path — a
    /// `BuiltinId` then resolves to nothing (`None` → HIR fallback),
    /// which is fine because that path only inspects `Some` vs `None`.
    pub mir_builtins: &'a [String],
}

impl<'a> MirEmitCtx<'a> {
    /// Construct a minimal walker ctx for the coverage walk /
    /// tests. Caller supplies a hand-built symbol table;
    /// `module_prefixes` defaults to the caller's owned empty set
    /// (or a populated one when the test needs to exercise
    /// module-scoped name resolution). No `CodegenContext`, no
    /// borrow policy — the covered arms emit conservative output
    /// (no clone / borrow / `Arc::new`), which is fine because the
    /// coverage walk only inspects `Some` vs `None`.
    pub fn for_test(symbol_table: &'a SymbolTable, module_prefixes: &'a HashSet<String>) -> Self {
        static EMPTY_TYPES: std::sync::OnceLock<HashMap<String, Type>> = std::sync::OnceLock::new();
        static EMPTY_SET: std::sync::OnceLock<HashSet<String>> = std::sync::OnceLock::new();
        Self {
            symbol_table,
            module_prefixes,
            codegen: None,
            local_types: EMPTY_TYPES.get_or_init(HashMap::new),
            rc_wrapped: EMPTY_SET.get_or_init(HashSet::new),
            borrowed_params: EMPTY_SET.get_or_init(HashSet::new),
            current_module_scope: None,
            // No builtin table on the coverage path: `Call(Builtin)`
            // resolves to `None` and the fn reports as HIR-fallback,
            // matching the pre-Wave-3a coverage walk's reach.
            mir_builtins: &[],
        }
    }

    /// Construct a **program-level** walker ctx for free-standing
    /// expressions that belong to no `ResolvedFnDef` — verify cases
    /// (this wave) and, next wave, `main` / top-level statements. The
    /// MIR mirror of `EmitCtx::empty()`: carries the full
    /// `&CodegenContext` (so ctor boxing / `callee_borrow_mask` / match
    /// emission work, unlike the coverage `for_test` path which leaves
    /// `codegen` `None`), but with an **empty per-fn policy** — no
    /// params, no locals, nothing borrowed-by-default. Every name a
    /// covered arm sees is treated owned / non-Copy, exactly as
    /// `EmitCtx::empty()` does for the HIR walker on these same
    /// free-standing exprs.
    ///
    /// Shared infra: both the verify wire-up and the next-wave
    /// main/top-stmt wire-up build their `MirEmitCtx` from here, so the
    /// "no-anchor" emit policy lives in one place.
    ///
    /// `mir_builtins` is passed explicitly rather than read off
    /// `ctx.mir_program`: free-standing exprs are lowered against a
    /// *clone* of the entry program (so builtin / instantiation table
    /// growth stays local), and `Call(Builtin(id))` must resolve `id`
    /// through that grown clone's table — not the entry program's,
    /// which may lack a builtin the lowering just interned. The caller
    /// owns the clone and lends its `builtins` slice here.
    pub(super) fn program_level(
        ctx: &'a CodegenContext,
        policy: &'a MirFnEmitPolicy,
        mir_builtins: &'a [String],
    ) -> Self {
        Self {
            symbol_table: &ctx.symbol_table,
            module_prefixes: &ctx.module_prefixes,
            codegen: Some(ctx),
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            borrowed_params: &policy.borrowed_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            mir_builtins,
        }
    }

    /// Construct a borrow-aware walker ctx for the production
    /// parity gate. `policy` is the [`MirFnEmitPolicy`] recomputed
    /// per-fn from the `ResolvedFnDef` (the same inputs
    /// `build_fn_ectx_from_resolved` feeds the HIR walker), and
    /// `ctx` is the full codegen context the borrow helpers query.
    pub(super) fn for_fn(ctx: &'a CodegenContext, policy: &'a MirFnEmitPolicy) -> Self {
        Self {
            symbol_table: &ctx.symbol_table,
            module_prefixes: &ctx.module_prefixes,
            codegen: Some(ctx),
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            borrowed_params: &policy.borrowed_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            // The builtin table the parity gate already built into the
            // `CodegenContext`. `Call(Builtin(id))` resolves `id`
            // through it; if the ctx carries no MIR program (it always
            // does on the gate path, but be defensive) builtins just
            // won't resolve → HIR fallback.
            mir_builtins: ctx
                .mir_program
                .as_ref()
                .map(|p| p.builtins.as_slice())
                .unwrap_or(&[]),
        }
    }

    /// Is this local a Copy type in Rust (i64 / f64 / bool / ())?
    fn is_copy(&self, name: &str) -> bool {
        self.local_types.get(name).is_some_and(is_copy_type)
    }

    fn is_rc_wrapped(&self, name: &str) -> bool {
        self.rc_wrapped.contains(name)
    }

    fn is_borrowed_param(&self, name: &str) -> bool {
        self.borrowed_params.contains(name)
    }
}

/// Per-fn borrow policy for the MIR walker — the slice of
/// [`super::emit_ctx::EmitCtx`] the covered arms read, owned so a
/// borrowing [`MirEmitCtx`] can be built from it. Recomputed per
/// fn from the `ResolvedFnDef`, mirroring `for_fn` /
/// `for_fn_no_borrow` on `EmitCtx`.
pub(super) struct MirFnEmitPolicy {
    pub local_types: HashMap<String, Type>,
    pub rc_wrapped: HashSet<String>,
    pub borrowed_params: HashSet<String>,
    pub current_module_scope: Option<String>,
}

impl MirFnEmitPolicy {
    /// The empty / no-anchor borrow policy — no params, no locals,
    /// nothing borrowed-by-default. Feeds [`MirEmitCtx::program_level`]
    /// for free-standing expressions (verify cases, main / top-level
    /// statements). The MIR mirror of `EmitCtx::empty()`.
    pub(super) fn empty() -> Self {
        Self {
            local_types: HashMap::new(),
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
            current_module_scope: None,
        }
    }

    /// Build the borrow policy from a `ResolvedFnDef`'s param
    /// types. `borrow_by_default` mirrors `EmitCtx::for_fn` (true)
    /// vs `EmitCtx::for_fn_no_borrow` (false, the TCO path):
    /// when false, no param is borrowed-by-default. `rc_wrapped`
    /// starts empty (set later for TCO pass-through, which the
    /// covered subset doesn't graduate).
    pub(super) fn from_resolved(
        resolved: &crate::ir::hir::ResolvedFnDef,
        scope: Option<&str>,
        borrow_by_default: bool,
    ) -> Self {
        let local_types: HashMap<String, Type> = resolved
            .params
            .iter()
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect();
        let borrowed_params = if borrow_by_default {
            local_types
                .iter()
                .filter(|(_, ty)| should_borrow_param(ty))
                .map(|(name, _)| name.clone())
                .collect()
        } else {
            HashSet::new()
        };
        Self {
            local_types,
            rc_wrapped: HashSet::new(),
            borrowed_params,
            current_module_scope: scope.map(String::from),
        }
    }
}

/// Mirror of `RustSourceCallCtx::resolve_module_call` in
/// `toplevel.rs`: find the longest registered module prefix
/// inside a dotted name. Returns `(prefix, suffix)` on hit,
/// `None` when no registered prefix matches.
fn resolve_module_call<'a>(
    dotted: &'a str,
    module_prefixes: &HashSet<String>,
) -> Option<(&'a str, &'a str)> {
    let mut best: Option<(&str, &str)> = None;
    for (dot_idx, _) in dotted.match_indices('.') {
        let prefix = &dotted[..dot_idx];
        let suffix = &dotted[dot_idx + 1..];
        if module_prefixes.contains(prefix)
            && best.is_none_or(|existing| prefix.len() > existing.0.len())
        {
            best = Some((prefix, suffix));
        }
    }
    best
}

/// How many fns the MIR walker can emit
/// standalone vs how many need HIR fallback. Pre-wire-up signal
/// so callers can track walker reach across the shipped corpus
/// without altering the codegen path.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CoverageReport {
    /// Total fn count in the lowered program.
    pub total: usize,
    /// Fns whose entire body the walker emits standalone
    /// (no `None` anywhere in the recursive walk).
    pub mir_covered: usize,
    /// Fns the walker can't emit — the recursive walk hit at
    /// least one variant that returned `None`. Caller would
    /// fall back to the HIR walker in a wire-up.
    pub hir_fallback: usize,
}

impl CoverageReport {
    /// Walker reach as a percentage of total fns. `0.0` when
    /// the program is empty (no fns lowered).
    pub fn ratio(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.mir_covered as f64 / self.total as f64
        }
    }
}

/// Walk every fn in `program` and report walker reach. For each
/// fn, calls [`emit_mir_expr`] on the body and counts
/// `Some` / `None`. Suitable for `--explain-mir-coverage`–style
/// diagnostics; the codegen path itself is untouched.
pub fn coverage_report(program: &MirProgram, emit_ctx: &MirEmitCtx<'_>) -> CoverageReport {
    coverage_report_with_blockers(program, emit_ctx).0
}

/// Same reach measurement as [`coverage_report`], plus a histogram
/// of the *first* construct that blocked each HIR-fallback fn.
///
/// For every fn the walker can't emit, `first_blocker` does the same
/// recursive `emit_mir_expr`-shaped walk but, instead of building a
/// string, returns a stable label for the first `MirExpr` variant /
/// `MirCallee` kind that would have returned `None`. Counting those
/// labels gives a per-wave roadmap: "lower `Match` next" reads
/// straight off the dominant bucket. The returned map is keyed by
/// label and ordered (BTreeMap) for deterministic report output.
///
/// This is diagnostic-only — it does not touch the production emit
/// path, and the walk is the exact mirror of [`emit_mir_expr`] so the
/// blocker it names is the one the wired-up backend would hit.
pub fn coverage_report_with_blockers(
    program: &MirProgram,
    emit_ctx: &MirEmitCtx<'_>,
) -> (
    CoverageReport,
    std::collections::BTreeMap<&'static str, usize>,
) {
    let mut report = CoverageReport::default();
    let mut blockers: std::collections::BTreeMap<&'static str, usize> =
        std::collections::BTreeMap::new();
    for (_, mir_fn) in program.iter() {
        report.total += 1;
        if emit_mir_expr(&mir_fn.body, emit_ctx).is_some() {
            report.mir_covered += 1;
        } else {
            report.hir_fallback += 1;
            let label = first_blocker(&mir_fn.body, emit_ctx).unwrap_or("Unknown");
            *blockers.entry(label).or_insert(0) += 1;
        }
    }
    (report, blockers)
}

/// Recursively find the first construct that makes [`emit_mir_expr`]
/// return `None` for `expr`, and name it with a stable label. Returns
/// `None` only when the whole subtree emits cleanly (the caller treats
/// that as "no blocker"). The traversal order matches
/// `emit_mir_expr`'s argument-evaluation order exactly so the label
/// pins the *same* node the emit walk would have bailed on.
fn first_blocker(expr: &Spanned<MirExpr>, emit_ctx: &MirEmitCtx<'_>) -> Option<&'static str> {
    // Leaf check: if this node emits cleanly on its own, no blocker
    // lives at-or-below it.
    if emit_mir_expr(expr, emit_ctx).is_some() {
        return None;
    }
    // The node (or one of its children) blocks. Recurse into children
    // first so we report the deepest / leftmost actual blocker, not the
    // wrapper that merely propagated a child's `None`.
    match &expr.node {
        MirExpr::Neg(inner) | MirExpr::Return(inner) | MirExpr::Try(inner) => {
            first_blocker(inner, emit_ctx).or(Some(label_for(&expr.node)))
        }
        MirExpr::BinOp(b) => first_blocker(&b.node.lhs, emit_ctx)
            .or_else(|| first_blocker(&b.node.rhs, emit_ctx))
            .or(Some("BinOp")),
        MirExpr::Call(c) => {
            // `Fn`, `Builtin`, `Intrinsic` and `LocalSlot` callees can all
            // emit cleanly (Wave 3a graduated the pure builtins +
            // intrinsics; W6/Stage-0 graduated the first-class `LocalSlot`
            // fn-pointer call), so recurse into the args first and only
            // report the callee kind when every arg emits but the call as a
            // whole still returned `None` (an effectful / unresolved
            // builtin, or a shape the walker can't render).
            for a in &c.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            match &c.node.callee {
                MirCallee::Builtin(_) => Some("Call(Builtin)"),
                MirCallee::Intrinsic(_) => Some("Call(Intrinsic)"),
                MirCallee::Fn(_) => Some("Call(Fn)"),
                MirCallee::LocalSlot { .. } => Some("Call(LocalSlot)"),
            }
        }
        MirExpr::TailCall(tc) => {
            for a in &tc.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            Some("TailCall")
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for item in items {
                if let Some(b) = first_blocker(item, emit_ctx) {
                    return Some(b);
                }
            }
            Some(label_for(&expr.node))
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                if let Some(b) = first_blocker(k, emit_ctx) {
                    return Some(b);
                }
                if let Some(b) = first_blocker(v, emit_ctx) {
                    return Some(b);
                }
            }
            Some("MapLiteral")
        }
        MirExpr::Let(l) => first_blocker(&l.node.value, emit_ctx)
            .or_else(|| first_blocker(&l.node.body, emit_ctx))
            .or(Some("Let(synthetic)")),
        MirExpr::Project(p) => first_blocker(&p.node.base, emit_ctx).or(Some("Project")),
        MirExpr::RecordCreate(r) => {
            for f in &r.node.fields {
                if let Some(b) = first_blocker(&f.value, emit_ctx) {
                    return Some(b);
                }
            }
            Some("RecordCreate(builtin/Tcp)")
        }
        MirExpr::RecordUpdate(u) => {
            if let Some(b) = first_blocker(&u.node.base, emit_ctx) {
                return Some(b);
            }
            for f in &u.node.updates {
                if let Some(b) = first_blocker(&f.value, emit_ctx) {
                    return Some(b);
                }
            }
            Some("RecordUpdate(builtin/Tcp)")
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            Some("Construct")
        }
        MirExpr::IfThenElse(ite) => first_blocker(&ite.node.cond, emit_ctx)
            .or_else(|| first_blocker(&ite.node.then_branch, emit_ctx))
            .or_else(|| first_blocker(&ite.node.else_branch, emit_ctx))
            .or(Some("IfThenElse")),
        MirExpr::Match(m) => {
            if let Some(b) = first_blocker(&m.node.subject, emit_ctx) {
                return Some(b);
            }
            for arm in &m.node.arms {
                if let Some(b) = first_blocker(&arm.body, emit_ctx) {
                    return Some(b);
                }
            }
            // Subject + every arm body emit cleanly, yet the Match as a
            // whole returned `None` — the blocker is the match shape
            // itself (an untranslatable pattern, or a dispatch shape the
            // walker can't reproduce byte-identically yet).
            Some("Match")
        }
        // Variants `emit_mir_expr` never recurses into (it returns
        // `None` immediately): they are themselves the blocker.
        other => Some(label_for(other)),
    }
}

/// Stable histogram label for a `MirExpr` variant. Kept short and
/// variant-named so the report reads as a worklist.
fn label_for(expr: &MirExpr) -> &'static str {
    match expr {
        MirExpr::Literal(_) => "Literal",
        MirExpr::Local(_) => "Local(synthetic)",
        MirExpr::Let(_) => "Let(synthetic)",
        MirExpr::Call(_) => "Call",
        MirExpr::TailCall(_) => "TailCall",
        MirExpr::BinOp(_) => "BinOp",
        MirExpr::Neg(_) => "Neg",
        MirExpr::Match(_) => "Match",
        MirExpr::Construct(_) => "Construct",
        MirExpr::RecordCreate(_) => "RecordCreate",
        MirExpr::RecordUpdate(_) => "RecordUpdate",
        MirExpr::Project(_) => "Project",
        MirExpr::IfThenElse(_) => "IfThenElse",
        MirExpr::Try(_) => "Try",
        MirExpr::List(_) => "List",
        MirExpr::Tuple(_) => "Tuple",
        MirExpr::MapLiteral(_) => "MapLiteral",
        MirExpr::InterpolatedStr(_) => "InterpolatedStr",
        MirExpr::IndependentProduct(_) => "IndependentProduct",
        MirExpr::Return(_) => "Return",
        MirExpr::FnValue(_) => "FnValue",
    }
}

/// Try to emit Rust source for `expr` directly from MIR.
/// Returns `None` for any variant outside the covered subset —
/// the signal to fall back to the HIR walker.
///
/// Mirror of [`super::expr::emit_expr`] for the covered subset;
/// output strings are character-for-character identical to the HIR
/// walker's output on the same input when the per-fn borrow policy
/// (`local_types` / `rc_wrapped` / `borrowed_params`) is threaded
/// through the [`MirEmitCtx`] (the production parity-gate path).
/// The parity gate ([`parity_gated_body`]) is the safety net: a body
/// only graduates onto the production path when its MIR rendering is
/// byte-equal to the HIR rendering.
pub(super) fn emit_mir_expr(expr: &Spanned<MirExpr>, emit_ctx: &MirEmitCtx<'_>) -> Option<String> {
    match &expr.node {
        MirExpr::Literal(lit) => {
            // The MIR const-fold pass collapses `Neg(Literal(273.15))`
            // → `Literal(-273.15)`. HIR never folds — it keeps the
            // `Neg` node and emits `(-273.15f64)` (the `Neg` arm's
            // `(-{inner})` wrapper). Re-introduce that wrapper for a
            // negative numeric literal at expression position so the
            // folded form matches HIR byte-for-byte. (Literal *patterns*
            // don't reach here — they translate to `ResolvedPattern` and
            // emit through the shared `emit_pattern` / dispatch path.)
            match &lit.node {
                // `checked_neg` guards `i64::MIN` — that value can't have
                // come from a `Neg` fold (the fold itself uses
                // `checked_neg`), so leave it bare rather than panic.
                crate::ast::Literal::Int(i) if *i < 0 => match i.checked_neg() {
                    Some(pos) => Some(format!(
                        "(-{})",
                        emit_literal(&crate::ast::Literal::Int(pos))
                    )),
                    None => Some(emit_literal(&lit.node)),
                },
                crate::ast::Literal::Float(f) if f.is_sign_negative() => Some(format!(
                    "(-{})",
                    emit_literal(&crate::ast::Literal::Float(-f))
                )),
                _ => Some(emit_literal(&lit.node)),
            }
        }
        MirExpr::Local(spanned_local) => {
            let name = &spanned_local.node.name;
            if name.is_empty() {
                // Synthetic locals (intermediate stmt-chain
                // effectful expressions) carry no source name —
                // the Rust backend can't emit them as idents.
                // Caller falls back to HIR.
                return None;
            }
            Some(aver_name_to_rust(name))
        }
        MirExpr::Neg(inner) => Some(format!("(-{})", emit_mir_expr(inner, emit_ctx)?)),
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            let l = emit_mir_expr(&bop.lhs, emit_ctx)?;
            let r = emit_mir_expr(&bop.rhs, emit_ctx)?;
            let op_str = match bop.op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            // Read type stamps to disambiguate
            // numeric `+` from `AverStr` concat. Same shape HIR
            // walker takes via `ectx.expr_is_numeric`. HIR's
            // disambiguation is `expr_is_numeric(lhs) ||
            // expr_is_numeric(rhs)` → plain add; otherwise the
            // `AverStr` concat path, where the LHS is run through
            // `maybe_clone` (it's consumed by `Add`, the RHS is
            // borrowed via `&` for `Add<&AverStr>`). Mirror that
            // exactly so Str + Str matches byte-for-byte.
            //
            // GENUINE DIVERGENCE (Wave 4 boundary — left on HIR
            // fallback by design): the MIR walker reads the operand's
            // *type stamp* (correct for let-bound locals + match
            // bindings + user-fn-call returns), while HIR's
            // `expr_is_numeric` reads `ectx.local_types`, which only
            // carries *params*. So for `left + right` where `left` /
            // `right` are `Int`s bound by `let left = leftRes?` (not
            // params), HIR misclassifies them as non-numeric and emits
            // the concat-shaped `(left + &right)`; MIR correctly emits
            // `(left + right)`. Both COMPILE and produce identical
            // results (`i64: Add<&i64>` exists in std), so neither is
            // unsound — MIR is just cleaner. Matching HIR here would
            // mean deliberately ignoring MIR's correct stamps, so these
            // fns (`applyEvalOp`, `validateAndCombine[NoOp]`, `size`,
            // `sumDirect`, `countS`'s `&str` deref, …) stay on HIR
            // fallback. The eventual HIR retirement fixes HIR (give it
            // let-local types), not MIR.
            if matches!(bop.op, BinOp::Add)
                && !ty_is_numeric(bop.lhs.ty())
                && !ty_is_numeric(bop.rhs.ty())
            {
                let l = mir_maybe_clone(l, &bop.lhs.node, emit_ctx);
                Some(format!("({} + &{})", l, r))
            } else if matches!(bop.op, BinOp::Eq | BinOp::Neq) {
                // HIR derefs `AverStr` (Rc<str>) to `&str` when one
                // side is a string literal, since `Rc<str>` doesn't
                // impl `PartialEq<&str>`. Mirror that so string
                // equality matches.
                if let MirExpr::Literal(lit) = &bop.rhs.node
                    && let crate::ast::Literal::Str(s) = &lit.node
                {
                    return Some(format!("(&*{} {} {:?})", l, op_str, s));
                }
                if let MirExpr::Literal(lit) = &bop.lhs.node
                    && let crate::ast::Literal::Str(s) = &lit.node
                {
                    return Some(format!("({:?} {} &*{})", s, op_str, r));
                }
                Some(format!("({} {} {})", l, op_str, r))
            } else {
                Some(format!("({} {} {})", l, op_str, r))
            }
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match &call.callee {
                MirCallee::Fn(fn_id) => {
                    // Resolve canonical name through the same
                    // symbol table the HIR walker uses, then emit
                    // the call exactly as HIR's
                    // `emit_named_function_call` does: each arg goes
                    // through `borrow_arg` (when the callee's i-th
                    // param is borrowed-by-default `&T`) or
                    // `clone_arg` (owned), and the module-qualified
                    // head is path-mangled via `resolve_module_call`.
                    let name = emit_ctx.symbol_table.fn_entry(*fn_id).key.canonical();
                    emit_named_call(&name, &call.args, emit_ctx)
                }
                // Resolve the interned dotted name and dispatch:
                //   - EFFECTFUL builtins (Wave 3b) →
                //     `emit_mir_effectful_builtin_call`, which mirrors
                //     HIR's `emit_builtin_call` replay-reroute / policy-
                //     wrap / bare-frame machinery byte-for-byte.
                //   - PURE builtins (Wave 3a) → `emit_mir_builtin_call`.
                // An out-of-range id (a lowering-invariant violation we
                // tolerate defensively) returns `None` → HIR fallback.
                MirCallee::Builtin(id) => {
                    let name = emit_ctx.mir_builtins.get(id.0 as usize)?.as_str();
                    if super::builtins::builtin_is_effectful(name) {
                        emit_mir_effectful_builtin_call(name, &call.args, emit_ctx)
                    } else {
                        emit_mir_builtin_call(name, &call.args, emit_ctx)
                    }
                }
                // Wave 3a: the 5 deforestation intrinsics (buffer build
                // + `__to_str`). Args are by-value (no clone / borrow),
                // mirroring `emit_builtin_call_inner`'s intrinsic arms.
                // The Rust backend deforests differently, so a buffered
                // fn's MIR shape may not byte-match HIR — the parity
                // gate then falls back safely.
                MirCallee::Intrinsic(intrinsic) => {
                    emit_mir_intrinsic_call(*intrinsic, &call.args, emit_ctx)
                }
                // First-class fn value held in a slot — calling a `Fn(..)`
                // param. Post-#379 the slot holds a plain fn-pointer (no
                // closures / `dyn Fn` — `Type::Fn` is param-only), so this
                // emits the direct call-by-name `name(args…)`. Mirror of
                // HIR's `CallPlan::Dynamic` (`emit_fn_call_with_options`):
                // the head is `aver_name_to_rust(name)` and every arg goes
                // through `clone_arg`.
                MirCallee::LocalSlot { name, .. } => {
                    let func = aver_name_to_rust(name);
                    let mut arg_strs = Vec::with_capacity(call.args.len());
                    for a in &call.args {
                        arg_strs.push(mir_clone_arg(
                            emit_mir_expr(a, emit_ctx)?,
                            &a.node,
                            emit_ctx,
                        ));
                    }
                    Some(format!("{}({})", func, arg_strs.join(", ")))
                }
            }
        }
        MirExpr::Return(inner) => Some(format!("return {}", emit_mir_expr(inner, emit_ctx)?)),
        MirExpr::TailCall(spanned_tc) => {
            // Tail call outside a self-TCO loop
            // emits as a regular function call — mirror of HIR's
            // `ResolvedExpr::TailCall` outside-loop branch
            // (which the resolver leaves intact and the emitter
            // routes through `emit_named_function_call`). When
            // the surrounding fn IS in a TCO loop, HIR rewrites
            // it to `continue` + param assigns — the walker
            // can't see that without `ectx`, so the wire-up
            // layer's parity check is the safety net (mismatch
            // → fall back to HIR).
            let tc = &spanned_tc.node;
            let name = emit_ctx.symbol_table.fn_entry(tc.target).key.canonical();
            emit_named_call(&name, &tc.args, emit_ctx)
        }
        MirExpr::Try(inner) => {
            // `value?` propagation. Mirror of
            // HIR's `ResolvedExpr::ErrorProp` emit — append `?`
            // to the inner expression's Rust form.
            Some(format!("{}?", emit_mir_expr(inner, emit_ctx)?))
        }
        MirExpr::Tuple(items) => {
            // `(a, b, c)` tuple literal. Mirror
            // of HIR's `ResolvedExpr::Tuple` emit — each element
            // routed through `clone_arg` for ownership.
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(mir_clone_arg(
                    emit_mir_expr(item, emit_ctx)?,
                    &item.node,
                    emit_ctx,
                ));
            }
            Some(format!("({})", parts.join(", ")))
        }
        MirExpr::List(items) => {
            // `[a, b, c]` list literal. Mirror
            // of HIR's `ResolvedExpr::List` — empty case folds
            // to `aver_rt::AverList::empty()`, non-empty to
            // `from_vec(vec![...])` with `clone_arg` elements.
            if items.is_empty() {
                return Some("aver_rt::AverList::empty()".to_string());
            }
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(mir_clone_arg(
                    emit_mir_expr(item, emit_ctx)?,
                    &item.node,
                    emit_ctx,
                ));
            }
            Some(format!(
                "aver_rt::AverList::from_vec(vec![{}])",
                parts.join(", ")
            ))
        }
        MirExpr::MapLiteral(entries) => {
            // `{"k" => v, …}` map literal.
            // Mirror of HIR's `ResolvedExpr::MapLiteral` — empty
            // → `HashMap::new()`, non-empty →
            // `vec![(k, v), …].into_iter().collect::<HashMap<_, _>>()`,
            // keys + values routed through `clone_arg`.
            if entries.is_empty() {
                return Some("HashMap::new()".to_string());
            }
            let mut parts = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                let key_str = mir_clone_arg(emit_mir_expr(k, emit_ctx)?, &k.node, emit_ctx);
                let val_str = mir_clone_arg(emit_mir_expr(v, emit_ctx)?, &v.node, emit_ctx);
                parts.push(format!("({}, {})", key_str, val_str));
            }
            Some(format!(
                "vec![{}].into_iter().collect::<HashMap<_, _>>()",
                parts.join(", ")
            ))
        }
        MirExpr::Let(spanned_let) => {
            // `let binding = value; body` →
            // Rust block-expression `{ let x = value; body }`.
            // A discarded intermediate (an effectful `Stmt::Expr` at
            // non-tail position, or a `_ = effect()` discard) carries
            // `binding_name.is_empty()` — there's no source ident to
            // bind, so the value is emitted as a bare statement
            // (`{ value; body }`), evaluated for its effect with the
            // result dropped. Mirror of HIR's discarded-`Stmt::Expr`
            // shape.
            let let_node = &spanned_let.node;
            let value = emit_mir_expr(&let_node.value, emit_ctx)?;
            let body = emit_mir_expr(&let_node.body, emit_ctx)?;
            if let_node.binding_name.is_empty() {
                Some(format!("{{ {}; {} }}", value, body))
            } else {
                let name = aver_name_to_rust(&let_node.binding_name);
                Some(format!("{{ let {} = {}; {} }}", name, value, body))
            }
        }
        MirExpr::Project(spanned_proj) => {
            // `base.field` projection. Mirror of
            // HIR's `ResolvedLeafOp::FieldAccess` emit shape —
            // emit_expr(base) + "." + aver_name_to_rust(field).
            // No clone insertion here; the HIR walker handles
            // that via `maybe_clone` at outer call sites.
            let proj = &spanned_proj.node;
            let base = emit_mir_expr(&proj.base, emit_ctx)?;
            Some(format!("{}.{}", base, aver_name_to_rust(&proj.field)))
        }
        MirExpr::RecordCreate(spanned_rec) => {
            // `T { field = v, … }` record literal.
            // Mirror of HIR's `ResolvedExpr::RecordCreate` emit
            // shape exactly — HIR reads the source-level
            // `type_name` string (verbatim on `MirRecordCreate`)
            // and only special-cases `Tcp.Connection` → the
            // re-exported `Tcp_Connection` struct. Fields route
            // through `clone_arg`.
            let rec = &spanned_rec.node;
            let rust_type = if rec.type_name == "Tcp.Connection" {
                "Tcp_Connection"
            } else {
                rec.type_name.as_str()
            };
            let mut parts = Vec::with_capacity(rec.fields.len());
            for f in &rec.fields {
                let val =
                    mir_clone_arg(emit_mir_expr(&f.value, emit_ctx)?, &f.value.node, emit_ctx);
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!("{} {{ {} }}", rust_type, parts.join(", ")))
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            // `T.update(base, field = v, …)` →
            // `{type_name} { field: value, …, ..base }`. Same
            // verbatim-type-name + Tcp.Connection rename as
            // RecordCreate; base + updates route through
            // `clone_arg`.
            let upd = &spanned_upd.node;
            let rust_type = if upd.type_name == "Tcp.Connection" {
                "Tcp_Connection"
            } else {
                upd.type_name.as_str()
            };
            let base = mir_clone_arg(
                emit_mir_expr(&upd.base, emit_ctx)?,
                &upd.base.node,
                emit_ctx,
            );
            let mut parts = Vec::with_capacity(upd.updates.len());
            for f in &upd.updates {
                let val =
                    mir_clone_arg(emit_mir_expr(&f.value, emit_ctx)?, &f.value.node, emit_ctx);
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!(
                "{} {{ {}, ..{} }}",
                rust_type,
                parts.join(", "),
                base
            ))
        }
        MirExpr::Construct(spanned_ctor) => {
            // Built-in ctors emit Result / Option wrappers; user
            // ctors resolve through the symbol table for
            // module-qualified path mangling. Both mirror HIR's
            // `clone_arg` on every arg; the User-ctor path also
            // wraps recursive (self-typed) fields in
            // `std::sync::Arc::new(...)` via
            // `constructor_boxed_positions` so recursive types
            // (`Tree.Node(left: Tree, …)`) emit byte-identical to
            // HIR's `emit_type_constructor_call`.
            let con = &spanned_ctor.node;
            match con.ctor {
                MirCtor::Builtin(builtin) => {
                    let (name, takes_arg) = match builtin {
                        BuiltinCtor::ResultOk => ("Ok", true),
                        BuiltinCtor::ResultErr => ("Err", true),
                        BuiltinCtor::OptionSome => ("Some", true),
                        BuiltinCtor::OptionNone => ("None", false),
                    };
                    if !takes_arg {
                        // `Option.None` — no args, no parens.
                        return Some(name.to_string());
                    }
                    let mut args = Vec::with_capacity(con.args.len());
                    for a in &con.args {
                        args.push(mir_clone_arg(
                            emit_mir_expr(a, emit_ctx)?,
                            &a.node,
                            emit_ctx,
                        ));
                    }
                    Some(format!("{}({})", name, args.join(", ")))
                }
                MirCtor::User(ctor_id) => {
                    // Resolve `CtorId` → owning type → variant name
                    // via the symbol table, then route the
                    // qualified type name through
                    // `resolve_module_call` for module-path
                    // mangling. Mirror of HIR's
                    // `emit_type_constructor_call`, including the
                    // boxed-position `Arc::new` on recursive fields
                    // (queried via `constructor_boxed_positions`,
                    // keyed by the `Type.Variant` name).
                    let ctor_entry = emit_ctx.symbol_table.ctor_entry(ctor_id);
                    let variant_name = ctor_entry.name.clone();
                    let type_entry = emit_ctx.symbol_table.type_entry(ctor_entry.owning_type);
                    let qualified = type_entry.key.canonical();
                    let boxed_positions = match emit_ctx.codegen {
                        Some(cg) => {
                            let ctor_name = format!("{}.{}", qualified, variant_name);
                            constructor_boxed_positions(&ctor_name, cg)
                        }
                        // Coverage path: no ctx → no boxed-position
                        // info. The parity gate isn't active here
                        // (coverage only reads Some/None), so an
                        // empty set is fine.
                        None => HashSet::new(),
                    };
                    let mut args = Vec::with_capacity(con.args.len());
                    for (idx, a) in con.args.iter().enumerate() {
                        let arg = mir_clone_arg(emit_mir_expr(a, emit_ctx)?, &a.node, emit_ctx);
                        if boxed_positions.contains(&idx) {
                            args.push(format!("std::sync::Arc::new({})", arg));
                        } else {
                            args.push(arg);
                        }
                    }
                    let args_str = args.join(", ");
                    // HIR emits a nullary variant as a unit variant
                    // (`E::Point`, no parens). Mirror that so
                    // zero-arg ctors match.
                    let head = if let Some((prefix, suffix)) =
                        resolve_module_call(&qualified, emit_ctx.module_prefixes)
                    {
                        format!("{}::{}", module_prefix_to_rust_path(prefix), suffix)
                    } else {
                        qualified
                    };
                    if con.args.is_empty() {
                        Some(format!("{}::{}", head, variant_name))
                    } else {
                        Some(format!("{}::{}({})", head, variant_name, args_str))
                    }
                }
            }
        }
        MirExpr::IfThenElse(spanned_ite) => emit_mir_if_then_else(&spanned_ite.node, emit_ctx),
        MirExpr::Match(spanned_match) => emit_mir_match(&spanned_match.node, emit_ctx),
        MirExpr::IndependentProduct(spanned_ip) => {
            emit_mir_independent_product(&spanned_ip.node, emit_ctx)
        }
        // A fn referenced as a *value* (`callWith(dbl)` passes `dbl`).
        // Post-#379, a fn value only ever enters through a `Fn(..)` param,
        // so the name is always a plain fn name — but mirror HIR's
        // `ResolvedLeafOp::StaticRef` in full (incl. the variant-vs-fn
        // refinement + module-path mangling) so the emit is byte-identical.
        // The VM does the same (`compile_ident` → `symbol_ref`).
        MirExpr::FnValue(name) => Some(emit_mir_static_ref(name, emit_ctx)),
        _ => None,
    }
}

/// Mirror of HIR's `ResolvedLeafOp::StaticRef` emit
/// (`src/codegen/rust/expr.rs`): a fn / variant referenced as a value.
/// Refines a dotted name that resolves to a known user-defined variant to
/// the Rust enum-variant form (`Shape::Point`); otherwise emits the
/// module-mangled fn reference (`Fibonacci::fib`) or the bare
/// `aver_name_to_rust(name)`. `Option.None` / `None` collapse to `None`.
///
/// `is_user_type` needs the full `CodegenContext`; on the coverage /
/// test path (`codegen` is `None`) the variant refinement is skipped —
/// the parity gate isn't active there, so the conservative fn-reference
/// shape is fine (coverage only inspects `Some` vs `None`).
fn emit_mir_static_ref(name: &str, ctx: &MirEmitCtx<'_>) -> String {
    if name == "Option.None" || name == "None" {
        return "None".to_string();
    }
    if let Some((type_name, variant_name)) = name.rsplit_once('.')
        && let Some(cg) = ctx.codegen
    {
        let is_user = |n: &str| crate::codegen::common::is_user_type(n, cg);
        if is_user(type_name) {
            return if let Some((prefix, _)) = resolve_module_call(name, ctx.module_prefixes) {
                let module_path = module_prefix_to_rust_path(prefix);
                let bare_type = type_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(type_name);
                format!("{}::{}::{}", module_path, bare_type, variant_name)
            } else {
                format!("{}::{}", type_name, variant_name)
            };
        }
        if let Some((_, bare_type)) = type_name.rsplit_once('.')
            && is_user(bare_type)
        {
            return if let Some((prefix, _)) = resolve_module_call(name, ctx.module_prefixes) {
                let module_path = module_prefix_to_rust_path(prefix);
                format!("{}::{}::{}", module_path, bare_type, variant_name)
            } else {
                format!("{}::{}", bare_type, variant_name)
            };
        }
    }
    if let Some((prefix, bare)) = resolve_module_call(name, ctx.module_prefixes) {
        let module_path = module_prefix_to_rust_path(prefix);
        format!("{}::{}", module_path, aver_name_to_rust(bare))
    } else {
        aver_name_to_rust(name)
    }
}

/// rust-on-MIR W6/Stage-0: render one free-standing `verify`-case
/// expression through the MIR walker. `resolved` is the already-lifted
/// `ResolvedExpr` (the caller does the on-demand `ctx.resolve_expr` the
/// HIR `emit_expr_legacy` does). Lowers it via `lower_top_level_value`
/// against a clone of the entry `MirProgram` (the same isolation the VM
/// uses for top-level statements: builtin / instantiation table growth
/// stays local to the clone), then emits it with a **program-level**
/// [`MirEmitCtx`] (no params / locals — verify exprs have no fn anchor).
///
/// Returns `None` when the expr is outside the lowerable subset OR the
/// walker can't render it — the per-expr signal for the caller to fall
/// back to `emit_expr_legacy` for that one expression. The `#[test]` /
/// `assert_eq!` / Result-`?` scaffolding is unaffected; only the
/// expression string changes.
pub(super) fn emit_mir_verify_expr(
    resolved: &Spanned<crate::ir::hir::ResolvedExpr>,
    ctx: &CodegenContext,
) -> Option<String> {
    let base = ctx.mir_program.as_ref()?;
    // Clone so the lowerer's builtin / instantiation table growth stays
    // local to this expression (mirrors the VM top-level path #338).
    let mut prog = base.clone();
    let lowered = crate::ir::mir::lower_top_level_value(resolved, &mut prog).ok()?;
    let policy = MirFnEmitPolicy::empty();
    // Lend the grown clone's builtin table (it backs `Call(Builtin(id))`
    // resolution and may carry a builtin the lowering just interned) plus
    // the full `ctx` for the borrow / ctor helpers.
    let emit_ctx = MirEmitCtx::program_level(ctx, &policy, &prog.builtins);
    emit_mir_expr(&lowered, &emit_ctx)
}

/// rust-on-MIR W6/Stage-0: render the **`main` fn body** through the MIR
/// walker. `main` is the one entry-point that DOES carry a
/// `ResolvedFnDef` (reachable via `fn_id_for_decl` →
/// `resolved_program.fn_by_id` → `mir_program.fn_by_id`), so — unlike the
/// free-standing verify / top-stmt exprs — its body has a real fn anchor:
/// we build the borrow policy from the resolved main (`from_resolved`,
/// borrow-by-default like the non-TCO HIR path) and emit via the same
/// `for_fn` ctx + `emit_mir_fn_body` the production parity gate uses for
/// every other fn.
///
/// `fn_id` is the resolved-main FnId the caller already computed
/// (`entry_module_sections` runs `fn_id_for_decl` for every fn). Returns
/// `None` when there's no MIR program, the main FnId has no lowered
/// `MirFn` (body outside the lowerable subset — e.g. multi-statement
/// bodies whose intermediate `Stmt::Expr` lower to synthetic-named lets),
/// or the walker can't render the body — the signal for the caller to
/// fall back to the HIR per-statement main-body emit. The `fn main()` /
/// `-> Result<…>` signature and the guest/replay wrappers are unaffected;
/// only the body string moves onto MIR.
pub(super) fn emit_mir_main_body(fn_id: crate::ir::FnId, ctx: &CodegenContext) -> Option<String> {
    let mir_fn = ctx.mir_program.as_ref()?.fn_by_id(fn_id)?;
    let resolved = ctx.resolved_program.fn_by_id(fn_id)?;
    // Main lives in the entry module → no module scope. Borrow-by-default
    // matches the non-TCO HIR path the main body uses (`build_fn_ectx`).
    let policy = MirFnEmitPolicy::from_resolved(resolved, None, /* borrow_by_default */ true);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    emit_mir_fn_body(&mir_fn.body, &emit_ctx)
}

/// rust-on-MIR W6/Stage-2 (guest-entry): render a **guest-entry fn's
/// inner body** through the MIR walker. The guest-entry fn (the
/// self-host's `runGuestCliProgram`) is the last construct still pinned
/// to the HIR expr walker: its body is wrapped in the
/// `aver_replay::with_guest_scope[_args][_result]` (replay scope) and
/// `crate::self_host_support::with_program_fn_store` (self-host state)
/// templates — pure string wrappers the caller keeps unchanged — but the
/// INNER body string was still produced by `emit_fn_body` (HIR).
///
/// Unlike `main`, the caller already holds the `&ResolvedFnDef` (and its
/// `fn_id`), so this takes the resolved fn directly rather than looking it
/// up by `FnId`. The borrow policy is rebuilt exactly as the guest-entry
/// HIR path's `ectx` is (`build_fn_ectx_from_resolved` — borrow-by-default,
/// the non-TCO shape; guest-entry returns before the `has_tco` branch).
/// `scope` is the owning module prefix (`None` for the entry-module
/// guest-entry).
///
/// Returns `None` (→ HIR `emit_fn_body` fallback, so this stays
/// non-regressing while HIR is still compiled) when there's no MIR
/// program, the guest-entry FnId has no lowered `MirFn`, or the walker
/// can't render the body. The covered subset (a `Match` over a user-fn
/// call + `Str`-concat) renders cleanly, so under forced-MIR this is the
/// MIR path; only the body string moves onto MIR, the replay /
/// self-host-state wrappers stay template text.
pub(super) fn emit_mir_guest_entry_body(
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    let mir_fn = ctx.mir_program.as_ref()?.fn_by_id(resolved_fd.fn_id)?;
    let policy =
        MirFnEmitPolicy::from_resolved(resolved_fd, scope, /* borrow_by_default */ true);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    emit_mir_fn_body(&mir_fn.body, &emit_ctx)
}

/// rust-on-MIR W6/Stage-0: render every **top-level statement value**
/// through the MIR walker, all-or-nothing. Free-standing module-scope
/// statements (`x = expr` / a bare `expr`) belong to no `ResolvedFnDef`,
/// so this mirrors the VM top-level path (#338): clone the entry
/// `MirProgram` ONCE (so the lowerer's builtin / instantiation table
/// growth stays consistent across all the statements that share it),
/// lower each statement's already-resolved value via
/// `lower_top_level_value`, and **pre-check** that every value both
/// lowers AND the walker renders it — deciding before emitting anything
/// so a mid-walk reject never leaves a half-written main body (exactly
/// what the VM `compile_top_level` does with `mir_expr_compilable`).
///
/// Returns the rendered value strings in statement order on full success
/// (the caller wraps each in the `let {name} = …;` / bare-expr-discard
/// `…;` templating, identical to the HIR `emit_stmt` shapes), or `None`
/// if there's no MIR program or ANY statement falls outside the lowerable
/// / renderable subset — the signal for the caller to fall back to the
/// HIR per-statement `emit_stmt_legacy` path for the whole block.
pub(super) fn emit_mir_top_stmt_values(
    resolved_values: &[&Spanned<crate::ir::hir::ResolvedExpr>],
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    let base = ctx.mir_program.as_ref()?;
    // One clone shared across every statement: the lowerer grows its
    // builtin / instantiation tables in place, so all the `Call(Builtin)`
    // ids the walker resolves key off the same grown table (mirrors the
    // VM lowering one `prog` for the whole `__top_level__` chunk).
    let mut prog = base.clone();
    let lowered: Vec<Spanned<MirExpr>> = resolved_values
        .iter()
        .map(|value| crate::ir::mir::lower_top_level_value(value, &mut prog).ok())
        .collect::<Option<_>>()?;
    let policy = MirFnEmitPolicy::empty();
    let emit_ctx = MirEmitCtx::program_level(ctx, &policy, &prog.builtins);
    // All-or-nothing: render every value before returning any, so a
    // single un-renderable statement falls the WHOLE block back to HIR
    // rather than leaving a half-MIR / half-HIR main body.
    lowered
        .iter()
        .map(|low| emit_mir_expr(low, &emit_ctx))
        .collect::<Option<Vec<_>>>()
}

/// Emit `MirExpr::IndependentProduct` (`(a, b, c)!` / `(a, b, c)?!`)
/// byte-identical to HIR's `ResolvedExpr::IndependentProduct` arm
/// (`super::expr`). The Rust backend is the one target that truly
/// PARALLELIZES the product (the VM and wasm-gc lower it sequentially):
/// each element runs on its own `std::thread::scope` thread.
///
/// Mirror notes (the three behaviors this arm must preserve to stay
/// byte-equal under the parity gate):
///
/// 1. **`?!` (`unwrap_results == true`).** A shared `__cancel_flag`
///    (`Arc<AtomicBool>`) is threaded into every branch via
///    `run_cancelable_branch`; a branch that produces `Err` sets the
///    flag so siblings can short-circuit (the *cancel* independence
///    mode — `complete` ignores the flag, but the emitted shape is the
///    same; the runtime decides). Joined branches are folded by
///    `emit_parallel_result_tuple_unwrap` (which unwraps the
///    `ParallelBranch::Completed` wrapper, then propagates the first
///    `Err` with `?`).
/// 2. **`!` (`unwrap_results == false`).** Same `thread::scope`/`spawn`,
///    but no cancel flag and no unwrap — joined branch values fold
///    straight into a tuple via `emit_tuple_from_vars` (a bare product
///    of `Result`s, preserved positionally).
/// 3. **Replay sequential fallback.** When `emit_replay_runtime` is on,
///    the parallel body is wrapped in
///    `if is_effect_tracking_active() { <sequential replay groups> }
///    else { <parallel> }`. The sequential arm uses
///    `enter_effect_group` / `set_effect_branch(i)` / `exit_effect_group`
///    so per-branch effects record/replay deterministically on one
///    thread; the parallel arm additionally captures + re-installs the
///    parallel scope context per spawned branch.
///
/// Each element is rendered through `mir_clone_arg` (the byte-identical
/// mirror of HIR's `clone_arg`). The `run_cancelable_branch` /
/// `ParallelBranch` / parallel-scope runtime is emitted UNCONDITIONALLY
/// by `super::runtime`, so no new runtime is needed.
fn emit_mir_independent_product(
    ip: &crate::ir::mir::MirIndependentProduct,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut parts: Vec<String> = Vec::with_capacity(ip.items.len());
    for it in &ip.items {
        parts.push(mir_clone_arg(
            emit_mir_expr(it, emit_ctx)?,
            &it.node,
            emit_ctx,
        ));
    }

    let n = parts.len();
    // The replay flag lives on the full `CodegenContext`; the coverage /
    // test path has none → treat as no replay (mirror of HIR's
    // `ctx.emit_replay_runtime`, conservative on the coverage walk).
    let has_replay = emit_ctx.codegen.is_some_and(|c| c.emit_replay_runtime);
    let unwrap = ip.unwrap_results;

    let mut code = String::new();
    if has_replay {
        // Runtime branch: if recording/replaying, execute sequentially
        // with replay groups (thread_local state stays on one thread).
        code.push_str("if crate::aver_replay::is_effect_tracking_active() { ");
        code.push_str("crate::aver_replay::enter_effect_group(); ");
        for (i, part) in parts.iter().enumerate() {
            code.push_str(&format!(
                "crate::aver_replay::set_effect_branch({i}); let _r{i} = {part}; "
            ));
        }
        code.push_str("crate::aver_replay::exit_effect_group(); ");
        if unwrap {
            code.push_str(&emit_result_tuple_unwrap("_r", "__v", n));
            code.push('?');
        } else {
            code.push_str(&emit_tuple_from_vars("_r", n));
        }
        code.push_str(" } else { ");
    }

    if unwrap {
        code.push_str("{ ");
        if has_replay {
            code.push_str(
                "let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); ",
            );
        }
        code.push_str(
            "let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); ",
        );
        code.push_str("std::thread::scope(|_s| { ");
        for (i, part) in parts.iter().enumerate() {
            if has_replay {
                code.push_str(&format!(
                    "let __parallel_scope{i} = __parallel_scope.clone(); "
                ));
            }
            code.push_str(&format!("let __cancel_flag{i} = __cancel_flag.clone(); "));
            code.push_str(&format!("let _h{i} = _s.spawn(move || "));
            if has_replay {
                code.push_str(&format!(
                    "crate::aver_replay::with_parallel_scope_context(__parallel_scope{i}.clone(), move || "
                ));
            }
            code.push_str("{ crate::run_cancelable_branch(__cancel_flag");
            code.push_str(&i.to_string());
            code.push_str(".clone(), move || { let __result = ");
            code.push_str(part);
            code.push_str("; if let Err(_) = &__result { __cancel_flag");
            code.push_str(&i.to_string());
            code.push_str(".store(true, std::sync::atomic::Ordering::Relaxed); } __result }) }");
            if has_replay {
                code.push(')');
            }
            code.push_str("); ");
        }
        for i in 0..n {
            code.push_str(&format!("let _b{i} = _h{i}.join().unwrap(); "));
        }
        code.push_str(&emit_parallel_result_tuple_unwrap("_b", "_r", "__v", n));
        code.push_str(" })? }");
    } else {
        if has_replay {
            code.push_str(
                "let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); ",
            );
        }
        code.push_str("std::thread::scope(|_s| { ");
        for (i, part) in parts.iter().enumerate() {
            if has_replay {
                code.push_str(&format!(
                    "let __parallel_scope{i} = __parallel_scope.clone(); "
                ));
                code.push_str(&format!(
                    "let _h{i} = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope{i}.clone(), move || {part})); "
                ));
            } else {
                code.push_str(&format!("let _h{i} = _s.spawn(move || {part}); "));
            }
        }
        for i in 0..n {
            code.push_str(&format!("let _r{i} = _h{i}.join().unwrap(); "));
        }
        code.push_str(&emit_tuple_from_vars("_r", n));
        code.push_str(" }) ");
    }

    if has_replay {
        code.push('}');
    }
    Some(code)
}

/// Emit `MirExpr::IfThenElse` byte-identical to HIR's
/// `try_emit_bool_if_else` (the only producer of `IfThenElse` is the
/// MIR `bool_match_to_if` pass, which rewrites the exact two-arm bool
/// matches HIR routes through `try_emit_bool_if_else`).
///
/// Two HIR behaviors are mirrored here that the naive `if cond { then }
/// else { else }` emit misses:
///
/// 1. **Condition canonicalization.** HIR's
///    `classify_bool_subject_plan_resolved` never emits `>=` / `<=` /
///    `!=` in the condition: it rewrites `>=`→`<`, `<=`→`>`, `!=`→`==`
///    and *swaps* the then/else branches (`invert`). The MIR pass keeps
///    the source operator + branch order, so a `code >= 48` subject
///    renders as `if (code >= 48) { then } else { else }` where HIR
///    renders `if (code < 48) { else } else { then }`. Re-apply HIR's
///    rewrite so the two match.
/// 2. **Branch clone.** HIR runs each branch through `maybe_clone`
///    (owning position). Mirror with `mir_maybe_clone` (a no-op for the
///    already-graduated cases, exact for the rest).
fn emit_mir_if_then_else(
    ite: &crate::ir::mir::MirIfThenElse,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // HIR's `classify_bool_subject_plan_resolved` maps a comparison
    // subject to a canonical operator + an `invert` flag:
    //   ==  →  "==", keep ;  !=  →  "==", invert
    //   <   →  "<",  keep ;  >=  →  "<",  invert
    //   >   →  ">",  keep ;  <=  →  ">",  invert
    // `invert == true` swaps the then/else branches. Crucially, HIR's
    // `try_emit_bool_if_else` renders the condition operands with a
    // *plain* `emit_expr` — it does NOT apply the `BinOp` arm's
    // string-literal `&*x == "lit"` deref. So a `match name == "_"`
    // subject emits `name == AverStr::from("_")` in the condition, not
    // `&*name == "_"`. Mirror that by emitting the comparison cond
    // directly here from the raw operand renders, bypassing the
    // deref-applying `BinOp` arm.
    let (cond, then_src, else_src) = mir_if_cond_and_branches(ite, emit_ctx)?;

    let then_branch = mir_maybe_clone(emit_mir_expr(then_src, emit_ctx)?, &then_src.node, emit_ctx);
    let else_branch = mir_maybe_clone(emit_mir_expr(else_src, emit_ctx)?, &else_src.node, emit_ctx);
    Some(format!(
        "if {} {{ {} }} else {{ {} }}",
        cond, then_branch, else_branch
    ))
}

// ── Match (Wave 2) ──────────────────────────────────────────────────────
//
// `MirExpr::Match` → Rust source byte-identical to HIR's `emit_match`
// (`src/codegen/rust/expr.rs`). The strategy is to reuse the *shared*
// recognition + emit machinery the HIR walker already routes through:
//
//   1. Translate each `MirPattern` → `ResolvedPattern` (resolving ctor
//      identity through the symbol table, exactly as the resolver
//      stamped it). Build synthetic `ResolvedMatchArm`s carrying those
//      patterns + neutral bodies.
//   2. Pre-render every arm body via the MIR walker (`emit_mir_expr` +
//      `mir_maybe_clone`). If any arm body can't render, the whole
//      match falls back to HIR. The dispatch/list emitters take a
//      `body_for_arm` closure; we map each synthetic arm back to its
//      pre-rendered MIR body by pointer offset into the synthetic slice.
//   3. Drive the SAME selection ladder `emit_match` uses (single-arm
//      irrefutable → `let`; borrowed-param `match_on_ref`; list match;
//      dispatch table; generic `match`) using the SAME shared
//      classifier (`classify_match_dispatch_plan_resolved`) and the
//      SAME `emit_dispatch_table_match` / `emit_list_match` /
//      `emit_pattern` / `emit_pattern_rebindings` functions.
//
// Bool two-arm matches never reach here — the MIR optimizer's
// `bool_match_to_if` already rewrote them to `MirExpr::IfThenElse`
// (handled by the dedicated arm in `emit_mir_expr`). So this arm only
// ever sees list / dispatch-table / generic shapes, exactly the
// non-bool subset HIR's `emit_match` reaches after its own bool short
// circuit. Any shape the walker can't reproduce byte-identically
// returns `None` and the parity gate falls back safely.

/// Mirror of HIR's `is_irrefutable_pattern` over `ResolvedPattern`.
fn resolved_pattern_is_irrefutable(pat: &ResolvedPattern) -> bool {
    match pat {
        ResolvedPattern::Wildcard | ResolvedPattern::Ident(_) => true,
        ResolvedPattern::Tuple(pats) => pats.iter().all(resolved_pattern_is_irrefutable),
        _ => false,
    }
}

/// Translate a `MirPattern` → `ResolvedPattern`, resolving ctor
/// identity through the symbol table the same way the resolver pass
/// stamped it (so `emit_pattern` / `emit_pattern_rebindings` /
/// `classify_*` see the exact `ResolvedPattern` shape the HIR walker
/// would have). Returns `None` for any pattern shape the walker can't
/// translate yet (none currently — every `MirPattern` maps).
fn mir_pattern_to_resolved(pat: &MirPattern, ctx: &MirEmitCtx<'_>) -> Option<ResolvedPattern> {
    Some(match pat {
        MirPattern::Wildcard => ResolvedPattern::Wildcard,
        MirPattern::Literal(lit) => ResolvedPattern::Literal(lit.clone()),
        // A `Bind` is HIR's `Ident` binding (`x -> …`). The source
        // binder name is what HIR emits.
        MirPattern::Bind(_, name) => ResolvedPattern::Ident(name.clone()),
        MirPattern::EmptyList => ResolvedPattern::EmptyList,
        MirPattern::Cons {
            head_name,
            tail_name,
            ..
        } => ResolvedPattern::Cons(head_name.clone(), tail_name.clone()),
        MirPattern::Tuple(sub) => {
            let mut parts = Vec::with_capacity(sub.len());
            for p in sub {
                parts.push(mir_pattern_to_resolved(p, ctx)?);
            }
            ResolvedPattern::Tuple(parts)
        }
        MirPattern::Ctor {
            ctor,
            binding_names,
            ..
        } => {
            let resolved_ctor = match ctor {
                MirCtor::Builtin(b) => ResolvedCtor::Builtin(*b),
                MirCtor::User(ctor_id) => {
                    // Resolve `CtorId` → owning type + variant name,
                    // exactly as the resolver stamped a user
                    // `ResolvedCtor::User`. `semantic_constructor_from_resolved_ctor`
                    // (used downstream by `emit_pattern` /
                    // `emit_pattern_rebindings`) reads `type_id` + `name`.
                    let entry = ctx.symbol_table.ctor_entry(*ctor_id);
                    ResolvedCtor::User {
                        ctor_id: *ctor_id,
                        type_id: entry.owning_type,
                        name: entry.name.clone(),
                    }
                }
            };
            ResolvedPattern::Ctor(resolved_ctor, binding_names.clone())
        }
    })
}

/// Build a neutral-bodied [`ResolvedMatchArm`] carrying just `pattern`.
/// The dispatch/list emitters only read `arm.pattern` + call the
/// `body_for_arm` closure; they never touch `arm.body`, so a `Unit`
/// literal placeholder is safe and the real MIR-rendered body is
/// supplied through the closure.
fn synthetic_arm(pattern: ResolvedPattern) -> ResolvedMatchArm {
    ResolvedMatchArm {
        pattern,
        body: Box::new(Spanned {
            node: crate::ir::hir::ResolvedExpr::Literal(crate::ast::Literal::Unit),
            line: 0,
            ty: std::sync::OnceLock::new(),
        }),
        binding_slots: std::sync::OnceLock::new(),
    }
}

/// Emit Rust for a `MirExpr::Match`, byte-identical to HIR's
/// `emit_match`. Returns `None` (→ HIR fallback) when the subject or
/// any arm body can't render, when a pattern can't translate, or when
/// the match shape isn't one the walker reproduces yet.
fn emit_mir_match(m: &MirMatch, emit_ctx: &MirEmitCtx<'_>) -> Option<String> {
    // Default (non-TCO) arm-body renderer: emit the arm body through
    // the MIR walker, then `maybe_clone` for the owning position —
    // exactly HIR's per-arm
    // `maybe_clone(emit_expr(&arm.body.node, …), &arm.body.node, …)`.
    emit_mir_match_with(m, emit_ctx, &|arm_body, ctx| {
        let body = emit_mir_expr(arm_body, ctx)?;
        Some(mir_maybe_clone(body, &arm_body.node, ctx))
    })
}

/// Core of [`emit_mir_match`], parameterized over how each arm body is
/// rendered. `render_arm` turns one arm's `Spanned<MirExpr>` body into
/// Rust source (or `None` → fall back). The default path renders bodies
/// as values (`maybe_clone`); the Wave-5 self-TCO loop path renders them
/// in tail position (self-`TailCall` → rebind + `continue`, value arm →
/// `return <expr>;`), so the same dispatch/list/generic machinery is
/// reused for TCO matches instead of forking the recognition.
fn emit_mir_match_with(
    m: &MirMatch,
    emit_ctx: &MirEmitCtx<'_>,
    render_arm: &dyn Fn(&Spanned<MirExpr>, &MirEmitCtx<'_>) -> Option<String>,
) -> Option<String> {
    // Translate patterns up front — bail if any pattern can't map.
    let mut arms: Vec<ResolvedMatchArm> = Vec::with_capacity(m.arms.len());
    for arm in &m.arms {
        arms.push(synthetic_arm(mir_pattern_to_resolved(
            &arm.pattern,
            emit_ctx,
        )?));
    }

    // Pre-render every arm body, in arm order. `body_for_arm` (below)
    // maps a `&ResolvedMatchArm` back to its index by pointer offset
    // into `arms`, then reads the matching pre-rendered string.
    let mut arm_bodies: Vec<String> = Vec::with_capacity(m.arms.len());
    for arm in &m.arms {
        arm_bodies.push(render_arm(&arm.body, emit_ctx)?);
    }

    let body_for_arm = |arm: &ResolvedMatchArm| -> String {
        // The dispatch/list emitters always hand back a reference to an
        // element of `arms` (they index `&arms[i]`), so identity match
        // by address recovers the arm's position → its pre-rendered MIR
        // body. Falls back to an empty body only if an emitter ever
        // passed a foreign reference (it doesn't), which the parity
        // gate would then reject as a mismatch.
        arms.iter()
            .position(|candidate| std::ptr::eq(candidate, arm))
            .map(|idx| arm_bodies[idx].clone())
            .unwrap_or_default()
    };

    // ── 1. Single-arm irrefutable → `let` destructuring. ──
    // Mirror of `emit_match`'s first branch.
    if arms.len() == 1 && resolved_pattern_is_irrefutable(&arms[0].pattern) {
        let subj = mir_clone_arg(
            emit_mir_expr(&m.subject, emit_ctx)?,
            &m.subject.node,
            emit_ctx,
        );
        let codegen = emit_ctx.codegen?;
        let pat = emit_pattern(&arms[0].pattern, false, codegen);
        let body = arm_bodies[0].clone();
        return Some(match &arms[0].pattern {
            ResolvedPattern::Wildcard => body,
            ResolvedPattern::Ident(name) => {
                let name = aver_name_to_rust(name);
                format!("{{ let {} = {}; {} }}", name, subj, body)
            }
            _ => format!("{{ let {} = {}; {} }}", pat, subj, body),
        });
    }

    // The shared dispatch/list/pattern emitters all need a real
    // `CodegenContext` (boxed-field lookup, module-prefix mangling).
    // The coverage walk runs without one — there the match only needs
    // to report "would emit", so we still translate + recurse but bail
    // before the ctx-dependent emit. (Production parity always has a
    // ctx; coverage only reads Some/None and matches will fall into the
    // None bucket on the coverage path, which is conservative + fine.)
    let codegen = emit_ctx.codegen?;

    // ── 2. Borrowed-param subject → match on the reference. ──
    // Mirror of `emit_match`'s `match_on_ref` special case: only when
    // no arm has pattern bindings.
    let no_bindings = arms
        .iter()
        .all(|arm| crate::ir::vars::resolved_pattern_bindings(&arm.pattern).is_empty());
    let match_on_ref = no_bindings && mir_subject_is_borrowed_param(&m.subject.node, emit_ctx);
    let subj = if match_on_ref {
        emit_mir_expr(&m.subject, emit_ctx)?
    } else {
        mir_clone_arg(
            emit_mir_expr(&m.subject, emit_ctx)?,
            &m.subject.node,
            emit_ctx,
        )
    };

    let dispatch_plan = classify_match_dispatch_plan_resolved(&arms);

    // Bool match → if/else is unreachable here: the MIR optimizer
    // already rewrote two-arm bool matches into `IfThenElse`. If a
    // `Bool` plan somehow survived (hand-built MIR in a test), fall
    // back rather than re-implement `try_emit_bool_if_else` (which
    // needs the subject's `ResolvedExpr` form for the compare-invert
    // rewrite the MIR walker can't reproduce).
    if matches!(dispatch_plan.as_ref(), Some(MatchDispatchPlan::Bool(_))) {
        return None;
    }

    // ── 3. List match. ──
    if has_list_patterns(&arms) {
        let list_shape = match dispatch_plan.as_ref() {
            Some(MatchDispatchPlan::List(shape)) => Some(*shape),
            _ => None,
        };
        return Some(emit_list_match(
            subj,
            &arms,
            list_shape,
            true,
            codegen,
            body_for_arm,
        ));
    }

    // ── 4. Dispatch table (literals / wrapper tags). ──
    if let Some(MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
        return Some(emit_dispatch_table_match(subj, &arms, shape, body_for_arm));
    }

    // ── 5. Generic `match`. ──
    // Mirror of `emit_match`'s tail. `needs_as_str` is always `true`
    // in HIR (`subject_might_be_string` is a `true` stub), so the
    // string-literal-pattern case derefs the subject to `&str`.
    let needs_as_str = true;
    let match_expr = if needs_as_str && has_string_literal_patterns(&arms) {
        format!("&*{}", subj)
    } else {
        subj
    };

    let mut arm_strs = Vec::with_capacity(arms.len());
    for (idx, arm) in arms.iter().enumerate() {
        let pat = emit_pattern(&arm.pattern, needs_as_str, codegen);
        let body = arm_bodies[idx].clone();
        let mut rebindings = emit_pattern_rebindings(&arm.pattern, codegen);
        if match_on_ref {
            let ref_rebinds = emit_ref_match_rebindings(&arm.pattern);
            if !ref_rebinds.is_empty() {
                rebindings = format!("{}{}", ref_rebinds, rebindings);
            }
        }
        arm_strs.push(format!(
            "        {} => {{\n            {}{}\n        }}",
            pat, rebindings, body
        ));
    }

    Some(format!(
        "match {} {{\n{}\n    }}",
        match_expr,
        arm_strs.join(",\n")
    ))
}

/// Is the match subject a read of a borrowed-param local? Mirror of
/// `emit_match`'s `match_on_ref` subject check
/// (`ResolvedExpr::Ident | Resolved` whose name `is_borrowed_param`).
fn mir_subject_is_borrowed_param(subject: &MirExpr, emit_ctx: &MirEmitCtx<'_>) -> bool {
    local_of(subject).is_some_and(|local| emit_ctx.is_borrowed_param(&local.name))
}

/// Emit the FULL function body the MIR walker would produce for a
/// single-expression body, in the exact format
/// [`super::toplevel::emit_fn_body`]'s single-expr-plan path emits
/// — the leading `    crate::cancel_checkpoint();\n    ` then the
/// body expression. Returns `None` when the walker can't render the
/// body (any uncovered construct anywhere in the tree).
///
/// This is the unit the production parity gate compares against the
/// HIR walker's `emit_fn_body` output: byte-equal → emit MIR
/// (graduated), else fall back to HIR. Because the comparison is
/// exact, the production output can only ever be the HIR output OR a
/// byte-identical MIR rendering — it cannot regress.
///
/// One return-position detail mirrored from
/// `emit_body_expr_plan_with_options`: a field access (`Project`) on
/// a borrowed param in tail/return position needs `.clone()` to
/// produce an owned value (`emit_mir_expr` emits `obj.field`
/// without it).
///
/// Wave 4 closes the multi-statement boundary: a top-level `Let`
/// chain (the MIR shape a `Block` body with `let` bindings lowers
/// to) is emitted as flat statement lines —
/// `    let a = …;\n    let b = …;\n    <final-expr>` — exactly the
/// format [`super::toplevel::emit_fn_body`]'s `Block` arm produces,
/// instead of the nested block-expr `{ let a = …; { let b = …; … } }`
/// `emit_mir_expr` renders for an inline `Let`. See
/// [`emit_mir_let_chain_flat`].
pub(super) fn emit_mir_fn_body(
    body: &Spanned<MirExpr>,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // A top-level `Let` is a multi-statement body. HIR emits it as
    // flat statement lines (named binding → `let …;`, discarded
    // intermediate `Stmt::Expr` → bare `…;`) then the final expression
    // on its own line — never a nested block-expr. Mirror that line
    // shape so multi-statement bodies graduate. The chain handles both
    // named and empty-`binding_name` (discarded) bindings, so no
    // first-binding guard is needed.
    if let MirExpr::Let(spanned_let) = &body.node
        && let Some(lines) = emit_mir_let_chain_flat(&spanned_let.node, emit_ctx)
    {
        return Some(format!("    crate::cancel_checkpoint();\n    {}", lines));
    }

    let mut code = emit_mir_expr(body, emit_ctx)?;
    // Return-position field access on a borrowed param → clone for
    // an owned result. Mirror of HIR's
    // `emit_body_expr_plan_with_options` `Leaf`/`Expr` arms.
    if let MirExpr::Project(p) = &body.node
        && let Some(local) = local_of(&p.node.base.node)
        && emit_ctx.is_borrowed_param(&local.name)
    {
        code = format!("{}.clone()", code);
    }
    Some(format!("    crate::cancel_checkpoint();\n    {}", code))
}

/// Emit a top-level `Let` chain as flat Rust statement lines, mirroring
/// [`super::toplevel::emit_fn_body`]'s `Block` arm byte-for-byte: each
/// binding becomes `let {name} = {value};` (value rendered raw, no
/// clone wrapper — exactly as HIR's `emit_stmt` does), one per line,
/// 4-space indented and `\n`-joined, terminated by the chain's final
/// expression rendered raw on its own line.
///
/// The chain is the run of directly-nested `Let` nodes: each one emits
/// its statement line and continues into its body until a body that
/// isn't a `Let` becomes the final expression. A named binding emits
/// `let {name} = {value};`; an empty-`binding_name` binding (a
/// discarded intermediate `Stmt::Expr` or a `_ = effect()` discard)
/// emits a bare `{value};` statement (the value evaluated for its
/// effects, result dropped) — the exact mirror of HIR's `emit_fn_body`
/// non-last `ResolvedStmt::Expr` arm (`{expr};`). Returns `None` only
/// when a binding value or the final expression can't render.
fn emit_mir_let_chain_flat(
    let_node: &crate::ir::mir::MirLet,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines: Vec<String> = Vec::new();
    let mut current = let_node;
    loop {
        let value = emit_mir_expr(&current.value, ctx)?;
        if current.binding_name.is_empty() {
            // Discarded intermediate (`Stmt::Expr` at non-tail position,
            // or a `_ = effect()` discard binding). No source ident to
            // bind — emit the value as a bare statement and drop it, the
            // exact mirror of HIR's non-last `ResolvedStmt::Expr` arm
            // (`{expr};`). Typically an effectful builtin call
            // (`Console.print(…)`) evaluated for its effect.
            lines.push(format!("{};", value));
        } else {
            let name = aver_name_to_rust(&current.binding_name);
            lines.push(format!("let {} = {};", name, value));
        }

        // Continue the chain when the body is another `Let` (named or a
        // discarded intermediate); the first non-`Let` body is the final
        // expression. Both binder shapes lower to flat statement lines,
        // so the nested-block shape never needs to appear.
        match &current.body.node {
            MirExpr::Let(next) => {
                current = &next.node;
            }
            _ => {
                let final_expr = emit_mir_expr(&current.body, ctx)?;
                lines.push(final_expr);
                break;
            }
        }
    }
    Some(lines.join("\n    "))
}

// ── Production parity gate + graduated-fn counter ───────────────────────
//
// The parity gate is the safety net that lets the MIR walker into
// the production Rust emit path with ZERO regression risk: for a fn,
// compute the MIR-walker body and the HIR-walker body; if they're
// byte-identical, emit the MIR one (and count it "graduated");
// otherwise emit the HIR one (fallback). Production output is
// therefore always either the unchanged HIR output or a string
// equal to it, so it cannot change — yet the MIR path is exercised
// + verified for the graduated subset on every compile.

use std::sync::atomic::{AtomicUsize, Ordering};

/// How many fns the parity gate has graduated (MIR body byte-equal
/// to HIR) since process start, and how many it has considered.
/// Process-global counters so `aver compile --explain-mir-coverage
/// --target rust` and the differential harness can report the
/// graduated fraction after a transpile run without threading a
/// counter through the whole emit pipeline.
static GRADUATED: AtomicUsize = AtomicUsize::new(0);
static CONSIDERED: AtomicUsize = AtomicUsize::new(0);

/// Reset the parity-gate counters. Called at the start of a
/// transpile so a fresh compile reports its own numbers.
pub(super) fn reset_parity_counters() {
    GRADUATED.store(0, Ordering::Relaxed);
    CONSIDERED.store(0, Ordering::Relaxed);
}

/// Is the `AVER_RUST_MIR_ONLY=1` escape hatch active? When set, the
/// parity gate forces the MIR walker's body onto the production path
/// (even when it diverges from HIR) for any fn the walker can render,
/// so the security differential test can make the MIR path own effect
/// emission. Read from the env each call (cheap; only on the codegen
/// path) so a test can toggle it per-process without a rebuild.
fn mir_only_hatch_enabled() -> bool {
    // W6/Stage-2: default ON; set `AVER_RUST_MIR_ONLY=0` to restore the byte-exact HIR fallback.
    std::env::var_os("AVER_RUST_MIR_ONLY").is_none_or(|v| v != "0")
}

/// Is the rust-on-MIR Wave-5 TCO path active (`AVER_RUST_MIR_TCO=1`)?
///
/// When set, the self-TCO loop and the mutual-recursion trampoline are
/// synthesized from `MirExpr::TailCall` by the MIR walker
/// ([`emit_mir_tco_fn`] / [`emit_mir_mutual_tco_block`]) instead of from
/// the source-AST HIR emitter (`emit_tco_fn` / `emit_mutual_tco_block`).
/// Deliberately a SEPARATE flag from `AVER_RUST_MIR_ONLY` (the security
/// hatch the differential tests depend on): toggling TCO onto MIR must
/// not change effect-emission ownership, and vice versa.
///
/// Read from the env each call (cheap; only on the codegen path) so a
/// test can toggle it per-process without a rebuild. W6/Stage-2: default
/// ON; set `AVER_RUST_MIR_TCO=0` to restore the HIR TCO emitter (rollback).
pub(super) fn mir_tco_enabled() -> bool {
    std::env::var_os("AVER_RUST_MIR_TCO").is_none_or(|v| v != "0")
}

/// Is the rust-on-MIR W6/Stage-0 verify path active
/// (`AVER_RUST_MIR_VERIFY=1`)?
///
/// When set, each `verify` case's left/right expression is lowered to
/// MIR (via `lower_top_level_value`) and rendered by the MIR walker
/// (with a program-level [`MirEmitCtx`]) instead of the source-AST HIR
/// emitter (`emit_expr_legacy`). The `#[test]` / `assert_eq!` / Result
/// `?` scaffolding is unchanged template text; only the two per-case
/// expression strings move onto MIR. Per-expr fallback to the HIR
/// emitter when the expr doesn't lower / the walker returns `None`.
///
/// A SEPARATE flag from `AVER_RUST_MIR_TCO` / `AVER_RUST_MIR_ONLY`:
/// routing verify emission through MIR is independent of TCO synthesis
/// and of effect-emission ownership.
///
/// Read from the env each call (cheap; only on the codegen path) so a
/// test can toggle it per-process without a rebuild. W6/Stage-2: default
/// ON; set `AVER_RUST_MIR_VERIFY=0` to restore the HIR verify emitter.
pub(super) fn mir_verify_enabled() -> bool {
    std::env::var_os("AVER_RUST_MIR_VERIFY").is_none_or(|v| v != "0")
}

/// Is the rust-on-MIR W6/Stage-0 main / top-level-statement path active
/// (`AVER_RUST_MIR_MAIN=1`)?
///
/// When set, the `main` fn BODY is rendered by the MIR walker (via
/// [`emit_mir_main_body`] — main carries a real `ResolvedFnDef`, so it
/// uses the same `for_fn` borrow policy as the production parity gate)
/// and the TOP-LEVEL STATEMENT values are rendered all-or-nothing through
/// [`emit_mir_top_stmt_values`] (the VM #338 isolation: one cloned
/// program, pre-check every value renders before emitting any), instead
/// of the source-AST HIR emitters (`emit_stmt_legacy` / `emit_expr_legacy`).
/// The `fn main()` / `-> Result<…>` signature and the
/// `aver_replay::with_guest_scope[_result]` guest wrapper stay unchanged
/// template text; only the body / statement-value strings move onto MIR.
/// Per-surface fallback to the HIR emit when the MIR body / any top-stmt
/// value doesn't lower / the walker returns `None`.
///
/// A SEPARATE flag from `AVER_RUST_MIR_VERIFY` / `AVER_RUST_MIR_TCO` /
/// `AVER_RUST_MIR_ONLY`: routing main / top-stmt emission through MIR is
/// independent of verify routing, TCO synthesis, and effect-emission
/// ownership.
///
/// Read from the env each call (cheap; only on the codegen path) so a
/// test can toggle it per-process without a rebuild. W6/Stage-2: default
/// ON; set `AVER_RUST_MIR_MAIN=0` to restore the HIR main emitter.
pub(super) fn mir_main_enabled() -> bool {
    std::env::var_os("AVER_RUST_MIR_MAIN").is_none_or(|v| v != "0")
}

/// `(graduated, considered)` since the last [`reset_parity_counters`].
pub fn parity_counters() -> (usize, usize) {
    (
        GRADUATED.load(Ordering::Relaxed),
        CONSIDERED.load(Ordering::Relaxed),
    )
}

/// Production parity gate for one fn body. `hir_body` is the body
/// the HIR walker already produced (the format `emit_fn_body`
/// returns). When a `MirFn` is available, render its body via the
/// MIR walker and compare: byte-equal → return the MIR string and
/// bump the graduated counter; otherwise → return `hir_body`
/// unchanged (fallback). Either way `considered` bumps so the
/// reported fraction is graduated / considered.
///
/// `resolved` supplies the per-fn borrow policy (param types /
/// borrow-by-default), recomputed exactly as
/// `build_fn_ectx_from_resolved` does for the HIR walker.
/// `borrow_by_default` is `false` only on the TCO no-borrow
/// path (which never graduates — those bodies aren't single-expr
/// plans the MIR walker renders identically — but the flag keeps
/// the policy honest).
pub(super) fn parity_gated_body(
    hir_body: String,
    mir_fn: Option<&crate::ir::mir::MirFn>,
    resolved: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
    borrow_by_default: bool,
    ctx: &CodegenContext,
) -> String {
    CONSIDERED.fetch_add(1, Ordering::Relaxed);
    let Some(mir_fn) = mir_fn else {
        return hir_body;
    };
    let policy = MirFnEmitPolicy::from_resolved(resolved, scope, borrow_by_default);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    // SECURITY-TEST ESCAPE HATCH (`AVER_RUST_MIR_ONLY=1`): when set,
    // force the MIR body onto the production path for any fn whose MIR
    // walker renders successfully, even if it is NOT byte-identical to
    // HIR — disabling the byte-exact fallback. Production default (env
    // UNSET) is unchanged: the parity gate still governs, so normal
    // compiles never see this branch and cannot regress. The hatch
    // exists solely so the differential security test can force the MIR
    // walker to OWN effect emission (replay / policy / bare framing) —
    // otherwise an effectful fn always falls back to HIR and the test
    // would silently exercise the HIR path instead.
    if mir_only_hatch_enabled()
        && let Some(mir_body) = emit_mir_fn_body(&mir_fn.body, &emit_ctx)
    {
        GRADUATED.fetch_add(1, Ordering::Relaxed);
        return mir_body;
    }
    match emit_mir_fn_body(&mir_fn.body, &emit_ctx) {
        Some(mir_body) if mir_body == hir_body => {
            GRADUATED.fetch_add(1, Ordering::Relaxed);
            mir_body
        }
        other => {
            // `AVER_RUST_MIR_DIFF=1` dumps every covered-but-not-
            // graduated fn (MIR walker emitted `Some`, but it didn't
            // byte-match HIR) — the Wave-1 long-pole worklist.
            if std::env::var_os("AVER_RUST_MIR_DIFF").is_some()
                && let Some(mir_body) = other
            {
                eprintln!(
                    "[mir-diff] {}\n  HIR: {:?}\n  MIR: {:?}",
                    resolved.name, hir_body, mir_body
                );
            }
            hir_body
        }
    }
}

/// Is the type stamp a primitive numeric?
/// `Int` / `Float` / `Byte` count; everything else (incl. `Str`)
/// doesn't. Mirror of HIR's `EmitCtx::expr_is_numeric` for the
/// MIR walker's `+` dispatch.
fn ty_is_numeric(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int | Type::Float))
}

// ── Wave 5: TCO loop / trampoline synthesis from MIR ────────────────────
//
// Rust has no TCO primitive — the VM emits a `TAIL_CALL` opcode and
// wasm-gc a `return_call`, both flat instructions. In generated Rust the
// loop (self-recursive) and the trampoline (mutual-recursive) STRUCTURE
// is synthesized in source. Waves 1-4 put every NON-TCO construct on
// MIR behind a byte-parity gate; TCO is the last holdout because the
// rewrite is structural (a self-`TailCall` arm becomes `continue` after
// rebinding the loop's mutable params; a value arm becomes `return`).
//
// Approach B (full-lift): the MIR walker emits its OWN correct loop /
// trampoline, verified BEHAVIORALLY (build + run vs VM + self-host
// regen), not by byte-parity (TCO never byte-graduates). Two
// simplifications the behavioral net unlocks vs the HIR emitter:
//
//   * **Always-snapshot param rebind.** For every rebound param, emit
//     `let __tcoN = <arg>;` for ALL of them first, then
//     `param = __tcoN;` in order, then `continue;`. Strictly correct
//     (no read-after-write clobber), no substring heuristic. Identity
//     rebinds (`arg == param`) and pass-through (rc) params are skipped.
//   * **No loop-invariant hoisting.** That was a byte-parity
//     optimization, not correctness — deferred.
//
// The ownership / borrow facts (rc pass-through params Arc-wrapped on
// the self-loop / `&T` extra trampoline args; non-rc owned params `mut`
// with NO borrow-by-default) are re-derived from the AST `FnDef` via the
// same `compute_rc_params` / `compute_self_passthrough_params` the HIR
// emitter uses — those are name/structure based and SCC discovery reuses
// the existing `find_mutual_tco_groups`. Get the ownership wrong → rustc
// rejects, which the build gate catches.

/// Emit a self-TCO fn entirely from MIR: the public signature
/// (`mut`-owned params, rc params Arc-wrapped before the loop) + the
/// `loop { cancel_checkpoint(); <tco-body> }` wrapper, where the body
/// renders self-`TailCall` arms as `{ rebind; continue }` and value arms
/// as `return <expr>;`.
///
/// `fd` supplies param names/types + drives the AST-based rc /
/// pass-through computation (mirroring `emit_tco_fn`); `mir_fn.body` is
/// the MIR body walked in tail position. Returns `None` (→ HIR fallback)
/// when any sub-expression can't render.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_mir_tco_fn(
    fd: &crate::ast::FnDef,
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    mir_fn: &crate::ir::mir::MirFn,
    fn_name: &str,
    ret_type: &str,
    visibility: &str,
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    use super::toplevel::{compute_rc_params, compute_self_passthrough_params, rc_param_names};

    let passthrough_indices = compute_self_passthrough_params(fd);
    let rc_indices = compute_rc_params(std::slice::from_ref(&fd), ctx);
    let rc_names = rc_param_names(&fd.params, &rc_indices);

    // Borrow policy: no borrow-by-default (owned `mut` params), rc
    // params wrapped (`(*x).clone()` on read). Mirror of
    // `emit_tco_fn`'s `build_fn_ectx_no_borrow_from_resolved` +
    // `with_rc_wrapped`.
    let mut policy = MirFnEmitPolicy::from_resolved(resolved_fd, scope, /* borrow */ false);
    policy.rc_wrapped = rc_names.clone();
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);

    // Render the body in tail position FIRST — bail before emitting any
    // signature if the walker can't render it.
    let body_code = emit_mir_tco_body(
        &mir_fn.body,
        mir_fn.fn_id,
        &fd.params,
        &passthrough_indices,
        &emit_ctx,
    )?;

    let params = emit_tco_params_mir(&fd.params, &rc_indices);
    let mut lines = Vec::new();
    lines.push(format!(
        "{}fn {}({}) -> {} {{",
        visibility, fn_name, params, ret_type
    ));
    // Wrap pass-through params in Arc before the loop (shadowing the
    // original binding). Mirror of `emit_tco_fn`.
    for &i in &rc_indices {
        let rust_name = aver_name_to_rust(&fd.params[i].0);
        lines.push(format!(
            "    let {} = std::sync::Arc::new({});",
            rust_name, rust_name
        ));
    }
    lines.push("    loop {".to_string());
    lines.push(body_code);
    lines.push("    }".to_string());
    lines.push("}".to_string());
    Some(lines.join("\n"))
}

/// Self-TCO param signature: non-rc params are `mut T` (rebound in the
/// loop), rc params are plain `T` (shadowed by the Arc::new binding).
/// Mirror of `emit_fn_params_tco`.
fn emit_tco_params_mir(
    params: &[(String, String)],
    rc_indices: &std::collections::HashSet<usize>,
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            let rust_type = super::types::type_annotation_to_rust(type_ann);
            let rust_name = aver_name_to_rust(name);
            if rc_indices.contains(&i) {
                format!("{}: {}", rust_name, rust_type)
            } else {
                format!("mut {}: {}", rust_name, rust_type)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Emit the self-TCO loop body (inside `loop { … }`). Leads with
/// `cancel_checkpoint();`, then renders the MIR body in tail position. A
/// top-level `Let` chain (leading bindings) emits flat `let x = v;` lines
/// then recurses into the chain's final expression as a tail expr.
fn emit_mir_tco_body(
    body: &Spanned<MirExpr>,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines = Vec::new();
    lines.push("        crate::cancel_checkpoint();".to_string());

    // Walk the leading `Let` chain as plain statements, then the final
    // expression as a tail expr. A named binding emits `let x = v;`; an
    // empty-`binding_name` binding (a discarded intermediate `Stmt::Expr`
    // or a `_ = effect()` discard) emits a bare `v;` statement (the value
    // evaluated for its effect, result dropped) — the mirror of HIR's
    // non-last `Stmt::Expr` arm.
    let mut current = body;
    while let MirExpr::Let(spanned_let) = &current.node {
        let let_node = &spanned_let.node;
        let value = emit_mir_expr(&let_node.value, ctx)?;
        if let_node.binding_name.is_empty() {
            lines.push(format!("        {};", value));
        } else {
            let name = aver_name_to_rust(&let_node.binding_name);
            lines.push(format!("        let {} = {};", name, value));
        }
        current = &let_node.body;
    }

    let tail = emit_mir_tco_tail_expr(current, self_fn, params, passthrough, ctx)?;
    lines.push(format!("        {}", tail));
    Some(lines.join("\n"))
}

/// Emit a MIR expression in self-TCO tail position. Self-`TailCall` →
/// `{ rebind; continue }`; `Match` / `IfThenElse` recurse into arms
/// (still tail position); anything else is a base-case value → `return
/// <expr>;`.
fn emit_mir_tco_tail_expr(
    expr: &Spanned<MirExpr>,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match &expr.node {
        MirExpr::TailCall(spanned_tc) => {
            let tc = &spanned_tc.node;
            if tc.target == self_fn && tc.args.len() == params.len() {
                emit_mir_self_tco_continue(&tc.args, params, passthrough, ctx)
            } else {
                // Tail call to a DIFFERENT fn (out of this self-loop):
                // emit a plain call + return. The leverage note's
                // module-DAG invariant means a self-TCO body's tail
                // calls target itself; a foreign target here is rare but
                // handled correctly.
                let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                Some(format!(
                    "return {};",
                    emit_named_call(&name, &tc.args, ctx)?
                ))
            }
        }
        MirExpr::Match(spanned_match) => {
            emit_mir_match_with(&spanned_match.node, ctx, &|arm_body, ctx| {
                emit_mir_tco_tail_expr(arm_body, self_fn, params, passthrough, ctx)
            })
        }
        MirExpr::IfThenElse(spanned_ite) => {
            emit_mir_tco_if_then_else(&spanned_ite.node, self_fn, params, passthrough, ctx)
        }
        // Base-case value (or `?` / let-bound value): `return <expr>;`.
        _ => Some(format!("return {};", emit_mir_value_return(expr, ctx)?)),
    }
}

/// Render a MIR `IfThenElse` in TCO tail position — both branches stay
/// in tail position (recurse). Reuses the condition canonicalization
/// from [`emit_mir_if_then_else`] would be ideal, but that helper
/// renders branches as values; here branches are tail exprs, so we
/// re-derive the condition the same way (the MIR `bool_match_to_if` pass
/// is the only producer).
fn emit_mir_tco_if_then_else(
    ite: &crate::ir::mir::MirIfThenElse,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let (cond, then_src, else_src) = mir_if_cond_and_branches(ite, ctx)?;
    let then_branch = emit_mir_tco_tail_expr(then_src, self_fn, params, passthrough, ctx)?;
    let else_branch = emit_mir_tco_tail_expr(else_src, self_fn, params, passthrough, ctx)?;
    Some(format!(
        "if {} {{ {} }} else {{ {} }}",
        cond, then_branch, else_branch
    ))
}

/// Render a value expression for a `return` in a TCO / trampoline base
/// case. Mirror of `emit_mir_expr` + the owning-position `maybe_clone`,
/// plus the HIR `emit_tco_expr` `_` arm's bare-rc-ident deref-clone:
/// returning a pass-through param (Arc<T> / &T) needs `(*x).clone()` to
/// yield an owned `T`.
fn emit_mir_value_return(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let code = emit_mir_expr(expr, ctx)?;
    Some(mir_maybe_clone(code, &expr.node, ctx))
}

/// Emit the self-TCO `{ rebind; continue }` block from the tail-call
/// args, using the always-snapshot rule. Pass-through (rc) params and
/// identity rebinds (`arg == param`) are skipped; every other rebound
/// param gets a `let __tcoN = <arg>;` snapshot first (avoiding
/// read-after-write clobber), then `param = __tcoN;` in order, then
/// `continue;`.
fn emit_mir_self_tco_continue(
    args: &[Spanned<MirExpr>],
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut arg_strs = Vec::with_capacity(args.len());
    for a in args {
        arg_strs.push(mir_clone_arg(emit_mir_expr(a, ctx)?, &a.node, ctx));
    }

    // Which positions are actually rebound (non-passthrough, non-identity)?
    let mut rebind: Vec<bool> = vec![false; params.len()];
    for (i, (name, _)) in params.iter().enumerate() {
        if passthrough.contains(&i) {
            continue;
        }
        if arg_strs[i] == aver_name_to_rust(name) {
            continue; // identity — no-op
        }
        rebind[i] = true;
    }

    let mut lines = Vec::new();
    lines.push("{".to_string());
    // Phase 1: snapshot ALL rebound args into temps (always-snapshot).
    for (i, arg_str) in arg_strs.iter().enumerate() {
        if rebind[i] {
            lines.push(format!("            let __tco{} = {};", i, arg_str));
        }
    }
    // Phase 2: assign temps back to params, in order.
    for (i, (name, _)) in params.iter().enumerate() {
        if rebind[i] {
            lines.push(format!(
                "            {} = __tco{};",
                aver_name_to_rust(name),
                i
            ));
        }
    }
    lines.push("            continue;".to_string());
    lines.push("        }".to_string());
    Some(lines.join("\n"))
}

/// Recompute the canonicalized condition + the (possibly swapped) tail
/// branches for a MIR `IfThenElse`. Shared by the value emitter
/// ([`emit_mir_if_then_else`]) and the TCO emitter — extracted so the
/// condition-rewrite logic lives in one place.
fn mir_if_cond_and_branches<'a>(
    ite: &'a crate::ir::mir::MirIfThenElse,
    ctx: &MirEmitCtx<'_>,
) -> Option<(String, &'a Spanned<MirExpr>, &'a Spanned<MirExpr>)> {
    let canonical_compare = |op: BinOp| -> Option<(&'static str, bool)> {
        match op {
            BinOp::Eq => Some(("==", false)),
            BinOp::Neq => Some(("==", true)),
            BinOp::Lt => Some(("<", false)),
            BinOp::Gte => Some(("<", true)),
            BinOp::Gt => Some((">", false)),
            BinOp::Lte => Some((">", true)),
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => None,
        }
    };
    match &ite.cond.node {
        MirExpr::BinOp(spanned_binop) if canonical_compare(spanned_binop.node.op).is_some() => {
            let bop = &spanned_binop.node;
            let (op_str, invert) = canonical_compare(bop.op).expect("checked by guard");
            let l = emit_mir_expr(&bop.lhs, ctx)?;
            let r = emit_mir_expr(&bop.rhs, ctx)?;
            let cond = format!("({} {} {})", l, op_str, r);
            if invert {
                Some((cond, &ite.else_branch, &ite.then_branch))
            } else {
                Some((cond, &ite.then_branch, &ite.else_branch))
            }
        }
        _ => {
            let cond = emit_mir_expr(&ite.cond, ctx)?;
            Some((cond, &ite.then_branch, &ite.else_branch))
        }
    }
}

// ── Wave 5: mutual-recursion trampoline from MIR ────────────────────────

/// Emit a mutual-TCO block from MIR: a state enum (one variant per
/// member, payload = non-rc param values), a trampoline dispatch loop
/// (member-`TailCall` bounces to a new enum variant, a value `return`s),
/// and thin wrapper fns. Mirror of
/// [`super::toplevel::emit_mutual_tco_block`], but the member bodies are
/// walked from MIR (`MirFn.body`) instead of the source AST.
///
/// `group_fns` is the SCC (from the existing AST-based
/// `find_mutual_tco_groups`); `mir_fns` are the matching `MirFn`s in the
/// same order. Returns `None` (→ HIR fallback for the whole block) when
/// any member body can't render — the block is all-or-nothing because
/// the members share one trampoline.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_mir_mutual_tco_block(
    group_id: usize,
    group_fns: &[&crate::ast::FnDef],
    mir_fns: &[&crate::ir::mir::MirFn],
    resolved_fns: &[&crate::ir::hir::ResolvedFnDef],
    ctx: &CodegenContext,
    scope: Option<&str>,
    visibility: &str,
) -> Option<String> {
    use super::toplevel::{compute_rc_params, fn_name_to_variant, rc_param_names};

    if group_fns.is_empty() {
        return None;
    }
    let enum_name = format!("__MutualTco{}", group_id);
    let trampoline_name = format!("__mutual_tco_trampoline_{}", group_id);
    let ret_type = if group_fns[0].return_type.is_empty() {
        "()".to_string()
    } else {
        super::types::type_annotation_to_rust(&group_fns[0].return_type)
    };

    let member_fn_ids: HashSet<crate::ir::FnId> = mir_fns.iter().map(|m| m.fn_id).collect();
    let rc_indices = compute_rc_params(group_fns, ctx);
    let rc_names = rc_param_names(&group_fns[0].params, &rc_indices);

    // Render every member's trampoline-arm body FIRST — bail before
    // emitting anything if a member can't render (all-or-nothing block).
    let mut arm_bodies: Vec<String> = Vec::with_capacity(group_fns.len());
    for (i, mir_fn) in mir_fns.iter().enumerate() {
        // Trampoline arm policy: no borrow-by-default, rc params wrapped.
        let mut policy = MirFnEmitPolicy::from_resolved(resolved_fns[i], scope, false);
        policy.rc_wrapped = rc_names.clone();
        let arm_ctx = MirEmitCtx::for_fn(ctx, &policy);
        let body = emit_mir_trampoline_body(
            &mir_fn.body,
            &member_fn_ids,
            &enum_name,
            &rc_names,
            &arm_ctx,
        )?;
        arm_bodies.push(body);
    }

    let mut sections = Vec::new();

    // 1. Enum — one variant per member, payload = non-rc param types.
    let mut enum_lines = Vec::new();
    enum_lines.push("#[allow(non_camel_case_types)]".to_string());
    enum_lines.push(format!("enum {} {{", enum_name));
    for fd in group_fns {
        let variant = fn_name_to_variant(&fd.name);
        let param_types: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(_, ty)| super::types::type_annotation_to_rust(ty))
            .collect();
        if param_types.is_empty() {
            enum_lines.push(format!("    {},", variant));
        } else {
            enum_lines.push(format!("    {}({}),", variant, param_types.join(", ")));
        }
    }
    enum_lines.push("}".to_string());
    sections.push(enum_lines.join("\n"));

    // 2. Trampoline fn — rc params are extra `&T` args.
    let rc_extra_params: String = mutual_rc_param_sig(group_fns[0], &rc_names);
    let mut tramp_lines = Vec::new();
    tramp_lines.push(format!(
        "fn {}(mut __state: {}{}) -> {} {{",
        trampoline_name, enum_name, rc_extra_params, ret_type
    ));
    tramp_lines.push("    loop {".to_string());
    tramp_lines.push("        __state = match __state {".to_string());
    for (fd, arm_body) in group_fns.iter().zip(&arm_bodies) {
        let variant = fn_name_to_variant(&fd.name);
        let param_bindings: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, _)| format!("mut {}", aver_name_to_rust(name)))
            .collect();
        let binding = if param_bindings.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!("{}::{}({})", enum_name, variant, param_bindings.join(", "))
        };
        tramp_lines.push(format!("            {} => {{", binding));
        tramp_lines.push(arm_body.clone());
        tramp_lines.push("            }".to_string());
    }
    tramp_lines.push("        };".to_string());
    tramp_lines.push("    }".to_string());
    tramp_lines.push("}".to_string());
    sections.push(tramp_lines.join("\n"));

    // 3. Wrapper fns — borrow-by-default params, clone borrowed into the
    //    enum variant, pass rc params as `&T` extra trampoline args.
    for fd in group_fns {
        let fn_name = aver_name_to_rust(&fd.name);
        let variant = fn_name_to_variant(&fd.name);
        let params = super::toplevel::emit_fn_params_pub(&fd.params, false);
        let variant_arg_names: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, type_ann)| {
                let rust_name = aver_name_to_rust(name);
                let ty = crate::types::parse_type_str(type_ann);
                if should_borrow_param(&ty) {
                    format!("{}.clone()", rust_name)
                } else {
                    rust_name
                }
            })
            .collect();
        let variant_call = if variant_arg_names.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!(
                "{}::{}({})",
                enum_name,
                variant,
                variant_arg_names.join(", ")
            )
        };
        let rc_extra_args: String = {
            let parts: Vec<String> = fd
                .params
                .iter()
                .filter(|(name, _)| rc_names.contains(name))
                .map(|(name, _)| format!("&{}", aver_name_to_rust(name)))
                .collect();
            if parts.is_empty() {
                String::new()
            } else {
                format!(", {}", parts.join(", "))
            }
        };
        let mut wrapper = Vec::new();
        if let Some(desc) = &fd.desc {
            wrapper.push(format!("/// {}", desc));
        }
        wrapper.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        wrapper.push(format!(
            "    {}({}{})",
            trampoline_name, variant_call, rc_extra_args
        ));
        wrapper.push("}".to_string());
        sections.push(wrapper.join("\n"));
    }

    Some(sections.join("\n\n"))
}

/// Build the rc-param extra `&T` argument list for the mutual
/// trampoline signature (`, x: &T, y: &U`), or empty when no rc params.
fn mutual_rc_param_sig(fd: &crate::ast::FnDef, rc_names: &HashSet<String>) -> String {
    if rc_names.is_empty() {
        return String::new();
    }
    let parts: Vec<String> = fd
        .params
        .iter()
        .filter(|(name, _)| rc_names.contains(name))
        .map(|(name, ty)| {
            format!(
                "{}: &{}",
                aver_name_to_rust(name),
                super::types::type_annotation_to_rust(ty)
            )
        })
        .collect();
    if parts.is_empty() {
        String::new()
    } else {
        format!(", {}", parts.join(", "))
    }
}

/// Emit one trampoline arm body from MIR: leads with
/// `cancel_checkpoint();`, walks the leading `Let` chain as plain `let`
/// statements, then renders the final expression in trampoline tail
/// position (member-`TailCall` → enum variant bounce, value → `return`).
fn emit_mir_trampoline_body(
    body: &Spanned<MirExpr>,
    members: &HashSet<crate::ir::FnId>,
    enum_name: &str,
    rc_names: &HashSet<String>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines = Vec::new();
    lines.push("                crate::cancel_checkpoint();".to_string());

    let mut current = body;
    while let MirExpr::Let(spanned_let) = &current.node {
        let let_node = &spanned_let.node;
        let value = emit_mir_expr(&let_node.value, ctx)?;
        if let_node.binding_name.is_empty() {
            // Discarded intermediate (`Stmt::Expr` / `_ = effect()`)
            // — bare statement, result dropped.
            lines.push(format!("                {};", value));
        } else {
            let name = aver_name_to_rust(&let_node.binding_name);
            lines.push(format!("                let {} = {};", name, value));
        }
        current = &let_node.body;
    }

    let tail = emit_mir_trampoline_tail_expr(current, members, enum_name, rc_names, ctx)?;
    lines.push(format!("                {}", tail));
    Some(lines.join("\n"))
}

/// Render a MIR expression in trampoline tail position. A `TailCall` to
/// a group member becomes an enum-variant bounce (excluding rc args); a
/// `TailCall` to a non-member, or any base-case value, becomes a
/// `return`. `Match` / `IfThenElse` recurse (still tail position).
fn emit_mir_trampoline_tail_expr(
    expr: &Spanned<MirExpr>,
    members: &HashSet<crate::ir::FnId>,
    enum_name: &str,
    rc_names: &HashSet<String>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match &expr.node {
        MirExpr::TailCall(spanned_tc) => {
            let tc = &spanned_tc.node;
            if members.contains(&tc.target) {
                // Bounce → enum variant for the TARGET member, excluding
                // its rc (pass-through) args. The target's param names
                // drive which positional args are rc — read them off the
                // target fn entry's source-level signature so the rc
                // filter matches the target, not the caller.
                let target_name = ctx.symbol_table.fn_entry(tc.target).key.name.clone();
                let variant = super::toplevel::fn_name_to_variant(&target_name);
                let mut arg_strs = Vec::new();
                for a in &tc.args {
                    // Skip rc args by the arg's source-level name: a
                    // pass-through arg is a bare local read whose name is
                    // in `rc_names` (shared across the SCC by name+type).
                    if let Some(local) = local_of(&a.node)
                        && rc_names.contains(&local.name)
                    {
                        continue;
                    }
                    arg_strs.push(mir_clone_arg(emit_mir_expr(a, ctx)?, &a.node, ctx));
                }
                if arg_strs.is_empty() {
                    Some(format!("{}::{}", enum_name, variant))
                } else {
                    Some(format!(
                        "{}::{}({})",
                        enum_name,
                        variant,
                        arg_strs.join(", ")
                    ))
                }
            } else {
                let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                Some(format!("return {}", emit_named_call(&name, &tc.args, ctx)?))
            }
        }
        MirExpr::Match(spanned_match) => {
            emit_mir_match_with(&spanned_match.node, ctx, &|arm_body, ctx| {
                emit_mir_trampoline_tail_expr(arm_body, members, enum_name, rc_names, ctx)
            })
        }
        MirExpr::IfThenElse(spanned_ite) => {
            let (cond, then_src, else_src) = mir_if_cond_and_branches(&spanned_ite.node, ctx)?;
            let t = emit_mir_trampoline_tail_expr(then_src, members, enum_name, rc_names, ctx)?;
            let e = emit_mir_trampoline_tail_expr(else_src, members, enum_name, rc_names, ctx)?;
            Some(format!("if {} {{ {} }} else {{ {} }}", cond, t, e))
        }
        _ => Some(format!("return {}", emit_mir_value_return(expr, ctx)?)),
    }
}

// ── MIR-side borrow / clone machinery ───────────────────────────────────
//
// Mirror of the HIR walker's `expr_skip_clone` / `maybe_clone` /
// `clone_arg` / `borrow_arg` (emit_ctx.rs + expr.rs), keyed off
// `MirLocal` (slot + `last_use` + source `name`) instead of
// `ResolvedExpr::Resolved`. The covered arms route every arg /
// field / element / base through these so their output matches HIR
// byte-for-byte on the borrow decisions. When the walker has no
// `CodegenContext` (coverage path), the local-name lookups still
// work off the (empty) policy fields and degrade to the
// conservative `last_use ? move : clone` shape — which is fine
// because the coverage walk only inspects `Some` vs `None`.

/// `&MirExpr` reference to a source-named local, if any. Synthetic
/// locals (empty name) are excluded — the walker already bails on
/// them upstream.
fn local_of(expr: &MirExpr) -> Option<&MirLocal> {
    match expr {
        MirExpr::Local(l) if !l.node.name.is_empty() => Some(&l.node),
        _ => None,
    }
}

/// Should `.clone()` be skipped for this MIR expr? Mirror of HIR's
/// `expr_skip_clone`. A local read skips clone on its last use or
/// when Copy; `rc_wrapped` / `borrowed_params` never skip (they
/// need the special clone paths in `mir_maybe_clone`). A name that
/// isn't a known local is treated as a global / namespace and
/// always skips. Non-locals (literals, nested exprs) never need a
/// clone wrapper here.
fn mir_expr_skip_clone(expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> bool {
    match local_of(expr) {
        Some(local) => {
            let name = local.name.as_str();
            if ctx.is_rc_wrapped(name) || ctx.is_borrowed_param(name) {
                return false;
            }
            local.last_use || ctx.is_copy(name)
        }
        None => true,
    }
}

/// Mirror of HIR's `maybe_clone`: wrap a local read in the right
/// clone shape for an owning position (arg, return, ctor field,
/// tuple / list / map element). `code` is the already-emitted
/// expression text for `expr`.
fn mir_maybe_clone(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    if let Some(local) = local_of(expr) {
        let name = local.name.as_str();
        return if mir_expr_skip_clone(expr, ctx) {
            code
        } else if ctx.is_rc_wrapped(name) {
            // Pass-through param (Rc<T> / &T): deref then clone.
            format!("(*{}).clone()", code)
        } else {
            // Borrowed param or plain owned local: clone to own.
            format!("{}.clone()", code)
        };
    }
    // Field access (`Project`): emit_mir_expr produces `base.field`
    // without clone; clone here for ownership. Matches HIR's
    // `maybe_clone` `Attr` arm — builtin namespace access never
    // reaches the MIR walker (it lowers to a `Call`), so no
    // namespace special-case is needed.
    if matches!(expr, MirExpr::Project(_)) {
        return format!("{}.clone()", code);
    }
    code
}

/// Mirror of HIR's `clone_arg` (`clone_arg_with_options`): emit an
/// expression as an owning argument. HIR elides the `.clone()` on a
/// record field access whose field type is Copy
/// (`attr_result_is_copy`); Wave 4 ports that elision here via
/// [`mir_attr_result_is_copy`], reading the base local's stamped type.
/// For the common case (non-`Project` args) this delegates to
/// `mir_maybe_clone`, matching HIR exactly.
fn mir_clone_arg(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    if let MirExpr::Project(p) = expr
        && mir_attr_result_is_copy(&p.node, ctx)
    {
        // Copy-typed record field: HIR returns the bare field access
        // (no `.clone()`). Mirror that.
        return code;
    }
    mir_maybe_clone(code, expr, ctx)
}

/// Mirror of HIR's `attr_result_is_copy` over a `MirProject`: the
/// field access result is Copy iff the projection base is a
/// `Type::Named` local and the projected field's declared type is a
/// Copy type. Reads the base's type from `local_types` (params + let
/// bindings — the MIR walker has richer coverage than HIR here, but the
/// guard `obj is a Named local` is the same), then defers to the shared
/// `record_field_is_copy` for the field-type lookup. Returns `false`
/// (HIR's conservative "needs a clone") when there's no `CodegenContext`
/// (coverage path) or the base isn't a Named local.
fn mir_attr_result_is_copy(proj: &crate::ir::mir::MirProject, ctx: &MirEmitCtx<'_>) -> bool {
    let Some(cg) = ctx.codegen else {
        return false;
    };
    let Some(local) = local_of(&proj.base.node) else {
        return false;
    };
    let Some(named_ty) = ctx
        .local_types
        .get(&local.name)
        .filter(|t| matches!(t, Type::Named { .. }))
    else {
        return false;
    };
    super::expr::record_field_is_copy(named_ty, &proj.field, cg)
}

/// Emit a named user-function call (`Call(Fn)` /
/// outside-loop `TailCall`). Mirror of HIR's
/// `emit_named_function_call`: per-arg `borrow_arg` (when the
/// callee's i-th param is borrowed-by-default `&T`) or `clone_arg`
/// (owned), and `resolve_module_call` head path-mangling.
///
/// `callee_borrow_mask` needs the full `CodegenContext`; on the
/// coverage path (`codegen == None`) there's no mask, so every arg
/// rides `clone_arg` (conservative — coverage only reads Some/None,
/// and the production parity gate never runs without a ctx).
fn emit_named_call(name: &str, args: &[Spanned<MirExpr>], ctx: &MirEmitCtx<'_>) -> Option<String> {
    let borrow_mask = match ctx.codegen {
        Some(cg) => callee_borrow_mask(name, args.len(), cg),
        None => vec![false; args.len()],
    };
    let mut arg_strs = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        let code = emit_mir_expr(a, ctx)?;
        let s = if borrow_mask.get(i).copied().unwrap_or(false) {
            mir_borrow_arg(code, &a.node, ctx)
        } else {
            mir_clone_arg(code, &a.node, ctx)
        };
        arg_strs.push(s);
    }
    if let Some((prefix, suffix)) = resolve_module_call(name, ctx.module_prefixes) {
        Some(format!(
            "{}::{}({})",
            module_prefix_to_rust_path(prefix),
            aver_name_to_rust(suffix),
            arg_strs.join(", ")
        ))
    } else {
        Some(format!(
            "{}({})",
            aver_name_to_rust(name),
            arg_strs.join(", ")
        ))
    }
}

/// Mirror of HIR's `borrow_arg`: emit an expression for passing to
/// a user fn whose param is `&T`. `code` is the already-emitted
/// text for `expr`.
fn mir_borrow_arg(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    let Some(local) = local_of(expr) else {
        // Complex expression: borrow the temporary.
        return format!("&{}", code);
    };
    let name = local.name.as_str();
    if ctx.is_copy(name) {
        // Copy type: by value.
        code
    } else if matches!(ctx.local_types.get(name), Some(Type::Str)) {
        // AverStr (Rc<str>): by value; last-use moves, else clone.
        if local.last_use {
            code
        } else if ctx.is_rc_wrapped(name) {
            format!("(*{}).clone()", code)
        } else {
            format!("{}.clone()", code)
        }
    } else if ctx.is_borrowed_param(name) {
        // Already `&T` — pass directly.
        code
    } else if ctx.is_rc_wrapped(name) {
        // Pass-through TCO param: deref to `&T`.
        format!("&*{}", code)
    } else {
        // Owned local: borrow it (last-use and non-last-use both
        // emit `&code` in the HIR walker).
        format!("&{}", code)
    }
}

// ── Wave 3a: PURE builtin calls + deforestation intrinsics ──────────────
//
// Mirror of the HIR oracle `emit_builtin_call` / `emit_builtin_call_inner`
// (`builtins.rs`) for the ~88 PURE builtins (Result / Option / Int /
// Float / String / List / Map / Vector / Bool / Char / Byte). The
// EFFECTFUL families (Args / Console / Http / HttpServer / Disk / Env /
// Random / SelfHostRuntime / Tcp / Terminal / Time) are split off at the
// `Call(Builtin)` arm to `emit_mir_effectful_builtin_call` (Wave 3b,
// below) — they are NOT handled here.
//
// Each arm copies its HIR sibling's shape verbatim, substituting:
//   `emit_arg(i)`                  → `emit_mir_expr(&args[i], ctx)?`
//   `clone_arg(&args[i].node, …)`  → `mir_clone_arg(emit_mir_expr(…)?, …)`
//   `emit_str_arg_or_deref(…)`     → `mir_str_arg_or_deref(&args[i], ctx)?`
// then runs the `builtin_needs_str_conversion` `.into_aver()` post-step
// that `emit_builtin_call` applies (Int.mod, Int/Float.fromString,
// String.* returning String, Char.fromCode, Byte.*). The byte-parity
// gate is the safety net: any arm whose output diverges from HIR blocks
// graduation and the fn falls back to HIR.

/// Mirror of HIR's `emit_str_arg_or_deref`: emit a string-accepting
/// argument (`String.contains` / `startsWith` / `endsWith`) as a bare
/// `"foo"` literal (no allocation) or, for any other expression, the
/// deref form `&*code`. Returns `None` when the inner expr can't emit.
fn mir_str_arg_or_deref(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    if let MirExpr::Literal(lit) = &expr.node
        && let crate::ast::Literal::Str(s) = &lit.node
    {
        return Some(format!("{:?}", s));
    }
    let code = emit_mir_expr(expr, ctx)?;
    Some(format!("&*{}", code))
}

/// Resolve a nested expression that is itself a `Call(Builtin(id))` to
/// its canonical dotted name + arg slice. MIR lowering wipes the
/// syntactic shape the HIR `ResolvedLeafOp` classifiers key off
/// (`Option.withDefault` / `Result.withDefault` / `Vector.get` over a
/// nested builtin), so the fusion recognizers
/// ([`try_emit_mir_fusion`]) re-match the pattern over this resolved
/// `(name, args)` form instead. Returns `None` for any non-`Call`, a
/// non-`Builtin` callee, or an out-of-range / unresolved `BuiltinId`
/// (the same defensive fallthrough the `Call(Builtin)` arm takes).
fn mir_builtin_call_parts<'a, 'c>(
    expr: &'a MirExpr,
    ctx: &MirEmitCtx<'c>,
) -> Option<(&'c str, &'a [Spanned<MirExpr>])> {
    let MirExpr::Call(spanned_call) = expr else {
        return None;
    };
    let call = &spanned_call.node;
    let MirCallee::Builtin(id) = &call.callee else {
        return None;
    };
    let name = ctx.mir_builtins.get(id.0 as usize)?.as_str();
    Some((name, &call.args))
}

/// Two MIR exprs that name the SAME source local. Used by the
/// `VectorSetOrDefaultSameVector` fusion's same-vector guard (HIR's
/// `default_expr.node != inner_args[0].node` check). Compares by slot,
/// not the whole `MirLocal`, because the two reads can carry different
/// `last_use` flags (the outer default read is typically the last use
/// of the slot, the inner `Vector.set` read is not) yet still denote
/// the same vector. Synthetic / unnamed locals never match.
fn mir_same_local(a: &MirExpr, b: &MirExpr) -> bool {
    match (local_of(a), local_of(b)) {
        (Some(la), Some(lb)) => la.slot == lb.slot,
        _ => false,
    }
}

/// Re-recognize the three codegen FUSIONS the HIR walker performs over
/// pre-lowering `ResolvedLeafOp` shapes but MIR lowering flattens into
/// nested builtin `Call`s. The HIR classifiers
/// (`classify_vector_set_or_default` / `classify_int_mod_or_default` /
/// `classify_list_index_get` in `ir::hir::classify`) match the
/// syntactic AST; here we re-match the equivalent `MirExpr::Call`
/// nesting and emit the EXACT fused Rust form the HIR `ResolvedLeafOp`
/// emitter (`emit_leaf_op_with_options`, `expr.rs`) produces, so the
/// byte-parity gate graduates these fns instead of falling back to the
/// (un-fused, slower) generic builtin emit. Returns `None` when the
/// outer call isn't one of the three fusion heads or the nested shape
/// doesn't match — the caller then emits the generic builtin form.
fn try_emit_mir_fusion(
    name: &str,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match name {
        // Fusion #1: `Option.withDefault(Vector.set(v, i, x), v)` where
        // both `v` are the SAME local → in-place bounds-checked set.
        // HIR: `ResolvedLeafOp::VectorSetOrDefaultSameVector`.
        "Option.withDefault" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            if inner_name != "Vector.set" || inner_args.len() != 3 {
                return None;
            }
            // Same-vector guard: the default arm (`args[1]`) must be the
            // same local as the vector being set (`inner_args[0]`).
            if !mir_same_local(&args[1].node, &inner_args[0].node) {
                return None;
            }
            // HIR: vector + value via `clone_arg`, index via raw emit.
            let vector = mir_clone_arg(
                emit_mir_expr(&inner_args[0], ctx)?,
                &inner_args[0].node,
                ctx,
            );
            let index = emit_mir_expr(&inner_args[1], ctx)?;
            let value = mir_clone_arg(
                emit_mir_expr(&inner_args[2], ctx)?,
                &inner_args[2].node,
                ctx,
            );
            Some(format!(
                "{{ let __vec = {}; let __idx = {} as usize; if __idx < __vec.len() {{ __vec.set_unchecked(__idx, {}) }} else {{ __vec }} }}",
                vector, index, value
            ))
        }
        // Fusion #2: `Result.withDefault(Int.mod(a, b), default)` → skip
        // the `Result` allocation. HIR:
        // `ResolvedLeafOp::IntModOrDefaultLiteral`.
        "Result.withDefault" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            if inner_name != "Int.mod" || inner_args.len() != 2 {
                return None;
            }
            // The default arm must be a literal (HIR's
            // `classify_int_mod_or_default` requires a literal default).
            let MirExpr::Literal(default_lit) = &args[1].node else {
                return None;
            };
            let a = &inner_args[0];
            let b = &inner_args[1];
            // Non-zero literal divisor → skip the runtime zero check.
            if let MirExpr::Literal(b_lit) = &b.node
                && let crate::ast::Literal::Int(n) = &b_lit.node
                && *n != 0
            {
                let a_str = emit_mir_expr(a, ctx)?;
                let b_str = emit_literal(&crate::ast::Literal::Int(*n));
                Some(format!("({}).rem_euclid({})", a_str, b_str))
            } else {
                let a_str = emit_mir_expr(a, ctx)?;
                let b_str = emit_mir_expr(b, ctx)?;
                let default = emit_literal(&default_lit.node);
                Some(format!(
                    "{{ let __b = {}; if __b == 0i64 {{ {} }} else {{ ({}).rem_euclid(__b) }} }}",
                    b_str, default, a_str
                ))
            }
        }
        // Fusion #3: `Vector.get(Vector.fromList(list), index)` → index
        // the materialized `Vec` directly, skipping the intermediate
        // `AverVector::from_vec` (an extra `Rc::new`). HIR:
        // `ResolvedLeafOp::ListIndexGet`.
        "Vector.get" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            if inner_name != "Vector.fromList" || inner_args.len() != 1 {
                return None;
            }
            let list = emit_mir_expr(&inner_args[0], ctx)?;
            let index = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "{}.to_vec().get({} as usize).cloned()",
                list, index
            ))
        }
        _ => None,
    }
}

/// Emit a PURE builtin call from MIR, byte-identical to the HIR
/// oracle's `emit_builtin_call` (minus the effectful / replay / policy
/// branches, which never reach here). Returns `None` for any builtin
/// the oracle doesn't cover here (→ HIR fallback). `name` is already
/// known non-effectful (the `Call(Builtin)` arm gated it).
fn emit_mir_builtin_call(
    name: &str,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // FUSIONS first: the HIR walker recognizes these
    // `Option.withDefault` / `Result.withDefault` / `Vector.get` over a
    // nested builtin shapes PRE-lowering and emits a fused form. MIR
    // lowering flattens the shape, so re-recognize it here before the
    // generic per-builtin arms below produce the un-fused (slower)
    // output. Anything that doesn't match falls through to the generic
    // emit, byte-identical to HIR's non-fused path.
    if let Some(fused) = try_emit_mir_fusion(name, args, ctx) {
        return Some(fused);
    }

    // `emit_arg(i)`: raw emit (HIR's `emit_expr(&args[i].node, …)`).
    macro_rules! arg {
        ($i:expr) => {
            emit_mir_expr(&args[$i], ctx)?
        };
    }
    // `clone_arg(&args[i].node, …)`: owning clone.
    macro_rules! clone {
        ($i:expr) => {
            mir_clone_arg(emit_mir_expr(&args[$i], ctx)?, &args[$i].node, ctx)
        };
    }

    let result = match name {
        // ---- Result ----
        "Result.Ok" => format!("Ok({})", clone!(0)),
        "Result.Err" => format!("Err({})", clone!(0)),
        "Result.withDefault" => format!("{}.unwrap_or({})", clone!(0), clone!(1)),

        // ---- Option ----
        "Option.Some" => format!("Some({})", clone!(0)),
        "Option.withDefault" => format!("{}.unwrap_or({})", clone!(0), clone!(1)),
        "Option.toResult" => format!("{}.ok_or({})", clone!(0), clone!(1)),

        // ---- Int ----
        "Int.abs" => format!("{}.abs()", arg!(0)),
        "Int.fromFloat" => format!("({} as i64)", arg!(0)),
        "Int.fromString" => format!("{}.parse::<i64>().map_err(|e| e.to_string())", arg!(0)),
        "Int.min" => format!("{}.min({})", arg!(0), arg!(1)),
        "Int.max" => format!("{}.max({})", arg!(0), arg!(1)),
        "Int.mod" => {
            let a = arg!(0);
            let b = arg!(1);
            format!(
                "if ({b}) == 0i64 {{ Err(\"Int.mod: divisor must not be zero\".to_string()) }} else {{ Ok(({a}).rem_euclid({b})) }}"
            )
        }

        // ---- Float ----
        "Float.abs" => format!("{}.abs()", arg!(0)),
        "Float.round" => format!("{}.round() as i64", arg!(0)),
        "Float.floor" => format!("{}.floor() as i64", arg!(0)),
        "Float.ceil" => format!("{}.ceil() as i64", arg!(0)),
        "Float.fromString" => format!("{}.parse::<f64>().map_err(|e| e.to_string())", arg!(0)),
        "Float.sqrt" => format!("{}.sqrt()", arg!(0)),
        "Float.pow" => format!("{}.powf({})", arg!(0), arg!(1)),
        "Float.min" => format!("{}.min({})", arg!(0), arg!(1)),
        "Float.max" => format!("{}.max({})", arg!(0), arg!(1)),
        "Float.sin" => format!("{}.sin()", arg!(0)),
        "Float.cos" => format!("{}.cos()", arg!(0)),
        "Float.atan2" => format!("{}.atan2({})", arg!(0), arg!(1)),
        "Float.pi" => "std::f64::consts::PI".to_string(),
        "Float.fromInt" => format!("{} as f64", arg!(0)),

        // ---- String ----
        "String.fromInt" => format!("{}.to_string()", arg!(0)),
        "String.fromFloat" => format!("{}.to_string()", arg!(0)),
        "String.fromBool" => format!("{}.to_string()", arg!(0)),
        "String.charAt" => {
            let s = arg!(0);
            let idx = arg!(1);
            format!("{}.chars().nth({} as usize).map(|c| c.to_string())", s, idx)
        }
        "String.len" => format!("({}.chars().count() as i64)", arg!(0)),
        "String.slice" => {
            let s = arg!(0);
            let from = arg!(1);
            let to = arg!(2);
            format!("aver_rt::string_slice(&{}, {}, {})", s, from, to)
        }
        "String.contains" => {
            let s = arg!(0);
            let sub = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.contains({})", s, sub)
        }
        "String.startsWith" => {
            let s = arg!(0);
            let prefix = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.starts_with({})", s, prefix)
        }
        "String.endsWith" => {
            let s = arg!(0);
            let suffix = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.ends_with({})", s, suffix)
        }
        "String.trim" => format!("{}.trim().to_string()", arg!(0)),
        "String.toUpper" => format!("{}.to_uppercase()", arg!(0)),
        "String.toLower" => format!("{}.to_lowercase()", arg!(0)),
        "String.split" => {
            let s = arg!(0);
            let delim = arg!(1);
            format!(
                "aver_rt::AverList::from_vec({}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>())",
                s, delim
            )
        }
        "String.join" => {
            let parts = arg!(0);
            let delim = arg!(1);
            format!("aver_rt::string_join(&{}, &{})", parts, delim)
        }
        "String.replace" => {
            let s = arg!(0);
            let from = arg!(1);
            let to = arg!(2);
            format!("{}.replace(&*{}, &*{})", s, from, to)
        }
        "String.chars" => format!(
            "aver_rt::AverList::from_vec({}.chars().map(|c| c.to_string()).collect::<Vec<_>>())",
            arg!(0)
        ),
        "String.repeat" => {
            let s = arg!(0);
            let n = arg!(1);
            format!("{}.repeat({} as usize)", s, n)
        }
        "String.indexOf" => {
            let s = arg!(0);
            let sub = arg!(1);
            format!("{}.find(&*{}).map(|i| i as i64).unwrap_or(-1i64)", s, sub)
        }
        "String.byteLength" => format!("({}.len() as i64)", arg!(0)),

        // ---- List ----
        "List.len" => {
            if let MirExpr::List(items) = &args[0].node
                && items.is_empty()
            {
                "0i64".to_string()
            } else {
                format!("({}.len() as i64)", arg!(0))
            }
        }
        "List.prepend" => format!("aver_rt::AverList::prepend({}, &{})", clone!(0), clone!(1)),
        "List.take" => {
            let list = arg!(0);
            let count = arg!(1);
            format!(
                "{{ let __n = if ({count}) <= 0 {{ 0usize }} else {{ usize::try_from({count}).unwrap_or(usize::MAX) }}; aver_rt::AverList::from_vec(({list}).iter().take(__n).cloned().collect::<Vec<_>>()) }}"
            )
        }
        "List.drop" => {
            let list = arg!(0);
            let count = arg!(1);
            format!(
                "{{ let __n = if ({count}) <= 0 {{ 0usize }} else {{ usize::try_from({count}).unwrap_or(usize::MAX) }}; aver_rt::AverList::from_vec(({list}).iter().skip(__n).cloned().collect::<Vec<_>>()) }}"
            )
        }
        "List.concat" => format!("aver_rt::AverList::concat(&{}, &{})", clone!(0), clone!(1)),
        "List.reverse" => format!("{}.reverse()", arg!(0)),
        "List.contains" => {
            let list = arg!(0);
            let item = arg!(1);
            format!("{}.contains(&{})", list, item)
        }
        "List.zip" => {
            let a = arg!(0);
            let b = arg!(1);
            format!(
                "aver_rt::AverList::from_vec({}.iter().zip({}.iter()).map(|(a, b)| (a.clone(), b.clone())).collect::<Vec<_>>())",
                a, b
            )
        }
        "List.fromVector" => format!("{}.to_list()", arg!(0)),

        // ---- Map ----
        "Map.fromList" => format!(
            "{{ let mut m = HashMap::new(); for (k, v) in {}.iter().cloned() {{ m = m.insert_owned(k, v); }} m }}",
            clone!(0)
        ),
        "Map.entries" => format!(
            "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); aver_rt::AverList::from_vec(es) }}",
            arg!(0)
        ),
        "Map.get" => {
            let map = arg!(0);
            let key = arg!(1);
            format!("{}.get(&{}).cloned()", map, key)
        }
        "Map.set" => format!("{}.insert_owned({}, {})", clone!(0), clone!(1), clone!(2)),
        "Map.has" => {
            let map = arg!(0);
            let key = arg!(1);
            format!("{}.contains_key(&{})", map, key)
        }
        "Map.remove" => {
            let map = clone!(0);
            let key = arg!(1);
            format!("{}.remove_owned(&{})", map, key)
        }
        "Map.keys" => format!(
            "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); aver_rt::AverList::from_vec(ks) }}",
            arg!(0)
        ),
        "Map.values" => format!(
            "aver_rt::AverList::from_vec({}.values().cloned().collect::<Vec<_>>())",
            arg!(0)
        ),
        "Map.len" => format!("({}.len() as i64)", arg!(0)),

        // ---- Bool ----
        "Bool.or" => format!("({} || {})", arg!(0), arg!(1)),
        "Bool.and" => format!("({} && {})", arg!(0), arg!(1)),
        "Bool.not" => format!("(!{})", arg!(0)),

        // ---- Char ----
        "Char.toCode" => format!(
            "({}.chars().next().map(|c| c as i64).unwrap_or(0i64))",
            arg!(0)
        ),
        "Char.fromCode" => format!("char::from_u32({} as u32).map(|c| c.to_string())", arg!(0)),

        // ---- Byte ----
        "Byte.toHex" => format!(
            "{{ let __n = {}; if (0i64..=255i64).contains(&__n) {{ Ok(format!(\"{{:02x}}\", __n as u8)) }} else {{ Err(format!(\"Byte.toHex: {{}} is out of range 0–255\", __n)) }} }}",
            arg!(0)
        ),
        "Byte.fromHex" => format!(
            "{{ let __s = {}; if __s.len() != 2 {{ Err(format!(\"Byte.fromHex: expected exactly 2 hex chars, got '{{}}'\", __s)) }} else {{ u8::from_str_radix(&__s, 16).map(|n| n as i64).map_err(|_| format!(\"Byte.fromHex: invalid hex '{{}}'\", __s)) }} }}",
            arg!(0)
        ),

        // ---- Vector ----
        "Vector.new" => {
            let size = arg!(0);
            let default = clone!(1);
            format!("aver_rt::AverVector::new({} as usize, {})", size, default)
        }
        "Vector.get" => {
            let vec = arg!(0);
            let idx = arg!(1);
            format!("{}.get({} as usize).cloned()", vec, idx)
        }
        "Vector.set" => {
            let vec = clone!(0);
            let idx = arg!(1);
            let val = clone!(2);
            format!("{}.set_owned({} as usize, {})", vec, idx, val)
        }
        "Vector.len" => format!("({}.len() as i64)", arg!(0)),
        "Vector.fromList" => format!("aver_rt::AverVector::from_vec({}.to_vec())", arg!(0)),

        // Not a covered pure builtin (effectful builtins never reach
        // here — gated at the call arm). HIR fallback.
        _ => return None,
    };

    // Mirror of `emit_builtin_call`'s `.into_aver()` post-step for
    // String-returning pure builtins (and Int.mod / Int.fromString /
    // Float.fromString / Char.fromCode / Byte.*).
    if super::builtins::builtin_needs_str_conversion(name) {
        Some(format!("({}).into_aver()", result))
    } else {
        Some(result)
    }
}

// ── Wave 3b: EFFECTFUL builtin calls (replay / policy / bare framing) ───
//
// SECURITY-SENSITIVE. Mirror of the HIR oracle `emit_builtin_call`
// (`builtins.rs`) for the 11 EFFECTFUL families (Args / Console / Http /
// HttpServer / Disk / Env / Random / SelfHostRuntime / Tcp / Terminal /
// Time). Wave 3a gated these out (`builtin_is_effectful` → `None` → HIR
// fallback); Wave 3b emits them, threading `ctx.policy` +
// `ctx.emit_replay_runtime` (reachable through `ctx.codegen`).
//
// The three wrappers HIR applies are reproduced by the SAME shared
// composers `emit_builtin_call` calls — `compose_replay_effect_call`
// (replay reroute), `compose_effectful_builtin_raw` (the raw `aver_rt::*`
// body), and `compose_effect_wrap` (policy `check_*` + bare
// `cancel_checkpoint` framing) — so the MIR output is byte-identical to
// HIR by construction. The only walker-specific inputs are the per-arg
// renders: `mir_clone_arg` (the replay temps, HIR's `clone_arg`) and the
// raw `emit_mir_expr` (the non-replay args + the policy first arg, HIR's
// `emit_expr`).
//
// A dropped composer here silently disables aver.toml DENY enforcement
// or record/replay capture (invisible to rustc + coverage + happy-path
// stdout) — the differential security test under `AVER_RUST_MIR_ONLY=1`
// forces this path and is revert-proofed against exactly that drop.

/// Emit an EFFECTFUL builtin call from MIR, byte-identical to the HIR
/// oracle's `emit_builtin_call`. `name` is already known effectful (the
/// `Call(Builtin)` arm routed it here). Returns `None` (→ HIR fallback)
/// when an arg can't render, when the production `CodegenContext` is
/// absent (coverage path — no policy/replay info), or when the raw
/// effect body isn't one the oracle covers.
fn emit_mir_effectful_builtin_call(
    name: &str,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // The policy / replay flags live on the full `CodegenContext`. The
    // coverage / test path has none → fall back to HIR (which the
    // coverage walk reads as a `None`, conservative + fine). The
    // production parity gate always carries a ctx.
    let codegen = ctx.codegen?;

    // (1) Replay reroute — mirror of `emit_builtin_call`'s
    //     `if ctx.emit_replay_runtime && builtin_is_effectful(name)`.
    //     Each arg is bound to `__effect_argN` via the `clone_arg`
    //     mirror; the shared composer emits the
    //     `cancel_checkpoint` + `invoke_effect(<effect>, vec![json], || raw)`
    //     block from the temp names.
    if codegen.emit_replay_runtime {
        let mut arg_clones = Vec::with_capacity(args.len());
        for a in args {
            arg_clones.push(mir_clone_arg(emit_mir_expr(a, ctx)?, &a.node, ctx));
        }
        return super::builtins::compose_replay_effect_call(name, &arg_clones);
    }

    // (2) Raw effect body — mirror of `emit_builtin_call_inner`'s
    //     effectful arms, every arg by-value (raw `emit_mir_expr`, HIR's
    //     `emit_arg`). The shared composer renders the `aver_rt::*` call.
    let mut arg_strs = Vec::with_capacity(args.len());
    for a in args {
        arg_strs.push(emit_mir_expr(a, ctx)?);
    }
    let result = super::builtins::compose_effectful_builtin_raw(name, &arg_strs)?;

    // `.into_aver()` post-step for String-returning effectful builtins
    // (mirror of `emit_builtin_call`'s `builtin_needs_str_conversion`).
    let result = if super::builtins::builtin_needs_str_conversion(name) {
        format!("({}).into_aver()", result)
    } else {
        result
    };

    // (3) Policy wrap (Http/Disk/Env) + bare `cancel_checkpoint` framing
    //     — mirror of `emit_builtin_call`'s tail. The first arg for the
    //     `check_*` call is rendered raw (HIR's `emit_expr`).
    let policy_active = codegen.policy.is_some() && !codegen.emit_replay_runtime;
    let first_arg = if policy_active && !args.is_empty() {
        Some(emit_mir_expr(&args[0], ctx)?)
    } else {
        None
    };
    Some(super::builtins::compose_effect_wrap(
        name,
        result,
        policy_active,
        first_arg,
    ))
}

/// Emit one of the 5 deforestation intrinsics from MIR, byte-identical
/// to the HIR oracle's `emit_builtin_call_inner` intrinsic arms. Args
/// are by-value (raw `emit_mir_expr`, no clone / borrow), matching the
/// loop-rebind shape the deforestation synthesizer emits. The Rust
/// backend deforests differently, so a buffered fn's MIR shape may not
/// byte-match HIR — the parity gate then falls back safely.
fn emit_mir_intrinsic_call(
    intrinsic: BuiltinIntrinsic,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match intrinsic {
        BuiltinIntrinsic::BufNew => {
            let cap = emit_mir_expr(&args[0], ctx)?;
            Some(format!(
                "aver_rt::Buffer::with_capacity(({}) as usize)",
                cap
            ))
        }
        BuiltinIntrinsic::BufAppend => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            let s = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "{{ let mut __b = {}; __b.push_str(&{}); __b }}",
                buf, s
            ))
        }
        BuiltinIntrinsic::BufAppendSepUnlessFirst => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            let sep = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "{{ let mut __b = {}; if !__b.is_empty() {{ __b.push_str(&{}); }} __b }}",
                buf, sep
            ))
        }
        BuiltinIntrinsic::BufFinalize => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            Some(format!("aver_rt::AverStr::from({})", buf))
        }
        BuiltinIntrinsic::ToStr => {
            let arg = emit_mir_expr(&args[0], ctx)?;
            Some(format!(
                "aver_rt::AverStr::from(aver_rt::aver_display(&({})))",
                arg
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::SymbolTable;
    use crate::ir::mir::{LocalId, MirBinOp, MirCall, MirExpr, MirLocal};
    use std::sync::OnceLock;

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: OnceLock::new(),
        }
    }

    fn span_ty<T>(node: T, ty: Type) -> Spanned<T> {
        let stamp = OnceLock::new();
        let _ = stamp.set(ty);
        Spanned {
            node,
            line: 0,
            ty: stamp,
        }
    }

    /// Empty `MirEmitCtx` with statically-borrowed empty symbol
    /// table + empty module-prefix set. `OnceLock`s give us a
    /// `'static` lifetime so tests can pass `&empty_ctx()`
    /// inline without juggling local owners.
    fn empty_ctx() -> MirEmitCtx<'static> {
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        MirEmitCtx::for_test(
            SYMBOLS.get_or_init(SymbolTable::default),
            PREFIXES.get_or_init(HashSet::new),
        )
    }

    #[test]
    fn emits_int_literal_as_i64_suffix() {
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert_eq!(emit_mir_expr(&lit, &empty_ctx()).as_deref(), Some("42i64"));
    }

    #[test]
    fn emits_local_via_aver_name_to_rust() {
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let expr = span(MirExpr::Local(span(local)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("local should emit");
        assert!(
            emit.contains("x"),
            "local emit should reference `x`: {emit}"
        );
    }

    #[test]
    fn returns_none_for_synthetic_local() {
        let local = MirLocal {
            slot: LocalId(7),
            last_use: false,
            name: String::new(),
        };
        let expr = span(MirExpr::Local(span(local)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
    }

    #[test]
    fn empty_fn_policy_has_no_anchor() {
        // The shared no-anchor policy: no params/locals, nothing
        // borrowed-by-default — the MIR mirror of `EmitCtx::empty()`.
        let policy = MirFnEmitPolicy::empty();
        assert!(policy.local_types.is_empty());
        assert!(policy.rc_wrapped.is_empty());
        assert!(policy.borrowed_params.is_empty());
        assert!(policy.current_module_scope.is_none());
    }

    #[test]
    fn program_level_ctx_renders_free_expr() {
        // A program-level ctx (empty policy + a real symbol table /
        // codegen) renders a free-standing literal — the verify-case
        // shape (no fn anchor). We can't build a full `CodegenContext`
        // cheaply here, so assert the policy/ctx wiring via the
        // walker on a literal that needs no `codegen`.
        let policy = MirFnEmitPolicy::empty();
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        static BUILTINS: OnceLock<Vec<String>> = OnceLock::new();
        // `program_level` needs a `&CodegenContext`; the literal arm
        // never reads it, so exercise the borrow-field plumbing via
        // `for_test` + the empty policy's slices instead (same shapes).
        let ctx = MirEmitCtx {
            symbol_table: SYMBOLS.get_or_init(SymbolTable::default),
            module_prefixes: PREFIXES.get_or_init(HashSet::new),
            codegen: None,
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            borrowed_params: &policy.borrowed_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            mir_builtins: BUILTINS.get_or_init(Vec::new),
        };
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        assert_eq!(emit_mir_expr(&lit, &ctx).as_deref(), Some("7i64"));
    }

    #[test]
    fn main_body_policy_borrows_by_default_like_hir() {
        // `emit_mir_main_body` builds its policy from the resolved-main
        // via `from_resolved(.., borrow_by_default = true)` — the same
        // non-TCO borrow rules the HIR main body uses (`build_fn_ectx`).
        // A `List<Int>` param borrows; an `Int` param does not. (Main
        // usually has no params, but the policy must honour the same
        // rule so a `main(args: List<String>)`-style entry borrows
        // identically to every other fn.)
        let resolved = crate::ir::hir::ResolvedFnDef {
            fn_id: crate::ir::FnId(0),
            name: "main".to_string(),
            line: 1,
            params: vec![
                ("xs".to_string(), Type::List(Box::new(Type::Int))),
                ("n".to_string(), Type::Int),
            ],
            return_type: Type::Unit,
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ir::hir::ResolvedFnBody::Block(vec![])),
            resolution: None,
        };
        let policy = MirFnEmitPolicy::from_resolved(&resolved, None, true);
        assert!(policy.borrowed_params.contains("xs"));
        assert!(!policy.borrowed_params.contains("n"));
        assert!(policy.current_module_scope.is_none());
    }

    #[test]
    fn emits_int_binop_add_as_plus() {
        let x = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(x.clone())), Type::Int)),
            rhs: Box::new(span_ty(MirExpr::Local(span(x)), Type::Int)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("binop should emit");
        // Numeric path — both operands stamped Int → no `&` on
        // the right side.
        assert!(
            emit.contains(" + ") && !emit.contains(" + &"),
            "Int+Int should emit plain `+`, got: {emit}"
        );
    }

    #[test]
    fn emits_str_binop_add_as_concat() {
        // When both operands are stamped `Str`,
        // the BinOp::Add path emits `(l + &r)` to match HIR's
        // `AverStr` concat shape.
        let s = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "s".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(s.clone())), Type::Str)),
            rhs: Box::new(span_ty(MirExpr::Local(span(s)), Type::Str)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("binop should emit");
        assert!(
            emit.contains(" + &"),
            "Str+Str should emit `+ &` for AverStr concat: {emit}"
        );
    }

    #[test]
    fn emits_neg_as_paren_minus_inner() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Neg(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg should emit");
        assert_eq!(emit, "(-7i64)");
    }

    #[test]
    fn returns_none_for_builtin_call_without_table() {
        // On the coverage / test path the `mir_builtins` table is
        // empty, so a `BuiltinId` resolves to nothing → HIR fallback.
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hello".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
    }

    /// `MirEmitCtx` carrying a one-entry builtin table so `Call(Builtin)`
    /// resolves `BuiltinId(0)` → `name`. Leaks the backing `Vec` to give
    /// it a `'static` lifetime (test-only).
    fn ctx_with_builtin(name: &str) -> MirEmitCtx<'static> {
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        let builtins: &'static [String] = Box::leak(vec![name.to_string()].into_boxed_slice());
        let mut ctx = MirEmitCtx::for_test(
            SYMBOLS.get_or_init(SymbolTable::default),
            PREFIXES.get_or_init(HashSet::new),
        );
        ctx.mir_builtins = builtins;
        ctx
    }

    fn int_lit(n: i64) -> Spanned<MirExpr> {
        span_ty(
            MirExpr::Literal(span(crate::ast::Literal::Int(n))),
            Type::Int,
        )
    }

    #[test]
    fn emits_pure_builtin_int_mod_with_into_aver() {
        // `Int.mod` is a covered PURE builtin; it carries the
        // `.into_aver()` post-step (`builtin_needs_str_conversion`).
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![int_lit(7), int_lit(3)],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &ctx_with_builtin("Int.mod")).expect("Int.mod emits");
        assert_eq!(
            emit,
            "(if (3i64) == 0i64 { Err(\"Int.mod: divisor must not be zero\".to_string()) } else { Ok((7i64).rem_euclid(3i64)) }).into_aver()"
        );
    }

    #[test]
    fn emits_pure_builtin_bool_or() {
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![
                span_ty(
                    MirExpr::Literal(span(crate::ast::Literal::Bool(true))),
                    Type::Bool,
                ),
                span_ty(
                    MirExpr::Literal(span(crate::ast::Literal::Bool(false))),
                    Type::Bool,
                ),
            ],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &ctx_with_builtin("Bool.or")).expect("Bool.or emits");
        assert_eq!(emit, "(true || false)");
    }

    #[test]
    fn effectful_builtin_returns_none_without_codegen_ctx() {
        // Wave 3b: effectful builtins DO emit on the production path, but
        // they need the `CodegenContext` (for `ctx.policy` /
        // `ctx.emit_replay_runtime`). The coverage / test path carries no
        // ctx, so `Console.print` returns `None` → HIR fallback there,
        // which the coverage walk reads conservatively. (Production emit
        // is exercised by the differential security test.)
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hi".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(
            emit_mir_expr(&expr, &ctx_with_builtin("Console.print")).is_none(),
            "effectful Console.print needs a CodegenContext; without one it \
             falls back to HIR"
        );
    }

    #[test]
    fn emits_buf_finalize_intrinsic() {
        // `__buf_finalize(buf)` → `aver_rt::AverStr::from(buf)`.
        let buf = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "b".to_string(),
        };
        let call = MirCall {
            callee: MirCallee::Intrinsic(BuiltinIntrinsic::BufFinalize),
            args: vec![span(MirExpr::Local(span(buf)))],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("__buf_finalize emits");
        assert_eq!(emit, "aver_rt::AverStr::from(b)");
    }

    #[test]
    fn emits_return_keyword() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Return(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("return should emit");
        assert_eq!(emit, "return 7i64");
    }

    fn symbols_with_one_type(name: &str, scoped: bool) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        let key = if scoped {
            TypeKey::in_module("Tcp", name)
        } else {
            TypeKey::entry(name)
        };
        st.types.push(TypeEntry {
            key,
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
        });
        st
    }

    #[test]
    fn emits_record_create_unscoped() {
        // `Point { x: 1, y: 2 }`. HIR-parity: the walker emits the
        // verbatim source-level `type_name` (`MirRecordCreate.type_name`),
        // the same string the HIR walker reads — not a symbol-table
        // lookup. The resolver leaves the user-typed name bare.
        let field_x = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let field_y = crate::ir::mir::MirRecordField {
            name: "y".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(2)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Point".to_string(),
            fields: vec![field_x, field_y],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Point", false);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("record create should emit");
        assert_eq!(emit, "Point { x: 1i64, y: 2i64 }");
    }

    #[test]
    fn emits_tcp_connection_record_with_rename() {
        // `Tcp.Connection` is the lone hardcoded special-case: HIR
        // renames it to the re-exported `Tcp_Connection` struct
        // inline. The MIR walker mirrors that exactly (no bounce) so
        // the output is byte-identical to HIR.
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Tcp.Connection".to_string(),
            fields: vec![],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Connection", true);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("tcp connection record should emit");
        assert_eq!(emit, "Tcp_Connection {  }");
    }

    #[test]
    fn emits_record_create_dep_module_as_bare_name() {
        // A dep-module record emits the bare type name the user
        // typed (`Expr { … }`) — the resolver doesn't dot-prefix
        // `RecordCreate.type_name`, and the consumer module's import
        // makes `Expr` resolve. HIR-parity via the verbatim
        // `type_name` string.
        let field = crate::ir::mir::MirRecordField {
            name: "tag".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Expr".to_string(),
            fields: vec![field],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.types.push(TypeEntry {
            key: TypeKey::in_module("ast", "Expr"),
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
        });
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("dep-module record should emit");
        assert_eq!(emit, "Expr { tag: 1i64 }");
    }

    #[test]
    fn emits_record_update_unscoped() {
        // `T { field: v, ..base }`. Verbatim `type_name`; `base`
        // routed through `clone_arg` (here the empty borrow policy
        // means a non-last-use local clones).
        let base = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "base".to_string(),
        };
        let update = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(9)))),
        };
        let upd = crate::ir::mir::MirRecordUpdate {
            base: Box::new(span(MirExpr::Local(span(base)))),
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Point".to_string(),
            updates: vec![update],
        };
        let expr = span(MirExpr::RecordUpdate(span(upd)));
        let st = symbols_with_one_type("Point", false);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("record update should emit");
        // `base` is a non-last-use, non-Copy local → `clone_arg`
        // clones it, exactly as HIR's `maybe_clone` does for a
        // `Resolved { last_use: false }` non-Copy local. (A
        // `MirLocal` is always a local read — the resolver's
        // global-Ident passthrough doesn't apply.)
        assert_eq!(emit, "Point { x: 9i64, ..base.clone() }");
    }

    fn symbols_with_one_fn(name: &str) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::FnKey;
        use crate::ir::symbol_table::{FnEntry, ModuleEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.fns.push(FnEntry {
            key: FnKey::entry(name),
            module: ModuleId(0),
            index_in_module: 0,
        });
        st
    }

    #[test]
    fn emits_tail_call_as_regular_call() {
        // Outside-loop `TailCall` mirrors HIR's
        // regular-call emit shape — `name(args)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let tc = span(MirExpr::TailCall(span(crate::ir::mir::MirTailCall {
            target: crate::ir::FnId(0),
            args: vec![arg],
        })));
        let st = symbols_with_one_fn("loop_step");
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&tc, &ctx).expect("tail call should emit");
        assert_eq!(emit, "loop_step(7i64)");
    }

    #[test]
    fn returns_none_for_unsupported_variant() {
        // Pick a variant the walker doesn't cover — `InterpolatedStr`.
        // (The pipeline contract guarantees `ir::interp_lower` rewrites it
        // away before Rust codegen, so the walker deliberately leaves it in
        // the `_ => None` catch-all; reaching it raw signals fall back to
        // HIR.)
        let interp = span(MirExpr::InterpolatedStr(vec![
            crate::ir::mir::MirStrPart::Literal("x".to_string()),
        ]));
        assert!(emit_mir_expr(&interp, &empty_ctx()).is_none());
    }

    #[test]
    fn emits_empty_map_as_hashmap_new() {
        // Empty map literal.
        let expr = span(MirExpr::MapLiteral(vec![]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("map should emit");
        assert_eq!(emit, "HashMap::new()");
    }

    #[test]
    fn emits_nonempty_map_as_vec_into_iter_collect() {
        // Non-empty map literal.
        let k1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let v1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(10))));
        let k2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let v2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(20))));
        let expr = span(MirExpr::MapLiteral(vec![(k1, v1), (k2, v2)]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("map should emit");
        assert_eq!(
            emit,
            "vec![(1i64, 10i64), (2i64, 20i64)].into_iter().collect::<HashMap<_, _>>()"
        );
    }

    #[test]
    fn emits_try_as_question_mark() {
        // `Try(inner)` → `inner?`.
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Try(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("try should emit");
        assert_eq!(emit, "7i64?");
    }

    #[test]
    fn emits_tuple_literal_as_paren_list() {
        // `(7, 9)` tuple.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::Tuple(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("tuple should emit");
        assert_eq!(emit, "(7i64, 9i64)");
    }

    #[test]
    fn emits_bare_independent_product_as_parallel_tuple() {
        // `(7, 9)!` — bare product (no unwrap). No replay (empty ctx),
        // so the parallel `thread::scope` body folds straight into a
        // tuple via `emit_tuple_from_vars`. Byte-identical to HIR's
        // `ResolvedExpr::IndependentProduct` `!` arm.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![a, b],
                unwrap_results: false,
            },
        )));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("bare product should emit");
        assert_eq!(
            emit,
            "std::thread::scope(|_s| { let _h0 = _s.spawn(move || 7i64); \
             let _h1 = _s.spawn(move || 9i64); let _r0 = _h0.join().unwrap(); \
             let _r1 = _h1.join().unwrap(); (_r0, _r1) }) "
        );
    }

    #[test]
    fn emits_unwrap_independent_product_with_cancel_flag() {
        // `(7, 9)?!` — unwrap product. No replay (empty ctx), so the
        // `?!` path emits the shared `__cancel_flag`, one
        // `run_cancelable_branch` spawn per element, joins, then the
        // `emit_parallel_result_tuple_unwrap` fold + trailing `?`.
        // Byte-identical to HIR's `ResolvedExpr::IndependentProduct`
        // `?!` arm.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![a, b],
                unwrap_results: true,
            },
        )));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("unwrap product should emit");
        assert!(
            emit.starts_with(
                "{ let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); \
                 std::thread::scope(|_s| { "
            ),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::run_cancelable_branch(__cancel_flag0"),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::run_cancelable_branch(__cancel_flag1"),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::ParallelBranch::Completed"),
            "got: {emit}"
        );
        assert!(emit.trim_end().ends_with("})? }"), "got: {emit}");
    }

    #[test]
    fn emits_empty_list_as_averlist_empty() {
        let expr = span(MirExpr::List(vec![]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("list should emit");
        assert_eq!(emit, "aver_rt::AverList::empty()");
    }

    #[test]
    fn emits_nonempty_list_as_from_vec() {
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let expr = span(MirExpr::List(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("list should emit");
        assert_eq!(emit, "aver_rt::AverList::from_vec(vec![1i64, 2i64])");
    }

    #[test]
    fn emits_project_as_dotted_field() {
        // `base.field` projection.
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "user".to_string(),
        };
        let base = span(MirExpr::Local(span(local)));
        let proj = crate::ir::mir::MirProject {
            base: Box::new(base),
            field: "name".to_string(),
        };
        let expr = span(MirExpr::Project(span(proj)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("project should emit");
        assert!(
            emit.ends_with(".name"),
            "project should end with `.name`, got: {emit}"
        );
    }

    #[test]
    fn emits_result_ok_as_ok_call() {
        // BuiltinCtor::ResultOk → `Ok(arg)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("construct should emit");
        assert_eq!(emit, "Ok(42i64)");
    }

    #[test]
    fn emits_option_none_as_bare_none() {
        // BuiltinCtor::OptionNone has no args
        // and emits `None` without parens.
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
            args: vec![],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("construct should emit");
        assert_eq!(emit, "None");
    }

    #[test]
    fn emits_let_as_block_expr() {
        // `let x = 7; x` → `{ let x = 7i64; x }`.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let body = span(MirExpr::Local(span(body_local)));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("let should emit");
        assert_eq!(emit, "{ let x = 7i64; x }");
    }

    #[test]
    fn synthetic_let_emits_bare_statement_not_none() {
        // A synthetic Let (intermediate effectful `Stmt::Expr` at non-tail
        // position, or a `_ = effect()` discard) carries an empty
        // `binding_name`. Stage-3 closes the former None gap: the walker
        // now emits the value as a bare statement (`{ value; body }`,
        // result dropped) instead of bailing to HIR.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body = span(MirExpr::Literal(span(crate::ast::Literal::Int(0))));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(7),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        assert_eq!(
            emit_mir_expr(&expr, &empty_ctx()).as_deref(),
            Some("{ 7i64; 0i64 }")
        );
    }

    /// Build a symbol table holding one type + one variant ctor.
    /// `scope_prefix == Some("foo")` for module-scoped types.
    fn symbols_with_one_user_ctor(
        scope_prefix: Option<&str>,
        type_name: &str,
        variant_name: &str,
    ) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{CtorEntry, ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        let key = match scope_prefix {
            Some(p) => TypeKey::in_module(p, type_name),
            None => TypeKey::entry(type_name),
        };
        st.types.push(TypeEntry {
            key,
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![crate::ir::CtorId(0)],
            is_product: false,
        });
        st.ctors.push(CtorEntry {
            owning_type: crate::ir::TypeId(0),
            name: variant_name.to_string(),
        });
        st
    }

    #[test]
    fn emits_user_ctor_unscoped() {
        // `Shape.Circle(r)` (bare type) →
        // `Shape::Circle(r)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::User(crate::ir::CtorId(0)),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let st = symbols_with_one_user_ctor(None, "Shape", "Circle");
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("user ctor should emit");
        assert_eq!(emit, "Shape::Circle(7i64)");
    }

    #[test]
    fn emits_user_ctor_scoped_via_module_prefix() {
        // Dep-module ctor resolved through
        // `module_prefixes` + `module_prefix_to_rust_path`.
        // `ast.Expr.App(x)` → `crate::aver_generated::ast::Expr::App(x)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::User(crate::ir::CtorId(0)),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let st = symbols_with_one_user_ctor(Some("ast"), "Expr", "App");
        let mut prefixes = HashSet::new();
        prefixes.insert("ast".to_string());
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("scoped user ctor should emit");
        assert_eq!(emit, "crate::aver_generated::ast::Expr::App(1i64)");
    }

    #[test]
    fn first_blocker_names_a_top_level_match() {
        // A bare `Match` is an uncovered variant — `first_blocker`
        // must name it "Match" so the coverage histogram reads as a
        // worklist.
        let m = span(MirExpr::Match(span(crate::ir::mir::MirMatch {
            subject: Box::new(span(MirExpr::Literal(span(crate::ast::Literal::Int(0))))),
            arms: vec![],
        })));
        assert!(emit_mir_expr(&m, &empty_ctx()).is_none());
        assert_eq!(first_blocker(&m, &empty_ctx()), Some("Match"));
    }

    #[test]
    fn first_blocker_recurses_to_deepest_builtin_call() {
        // `return (builtinCall(...))` — the outer Return emits cleanly
        // over a covered child, so the blocker the histogram reports
        // must be the *builtin call kind*, not the Return wrapper.
        let call = span(MirExpr::Call(span(MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Int(1))))],
        })));
        let ret = span(MirExpr::Return(Box::new(call)));
        assert!(emit_mir_expr(&ret, &empty_ctx()).is_none());
        assert_eq!(first_blocker(&ret, &empty_ctx()), Some("Call(Builtin)"));
    }

    #[test]
    fn first_blocker_is_none_for_fully_covered_body() {
        // A clean integer literal has no blocker.
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert!(first_blocker(&lit, &empty_ctx()).is_none());
    }

    /// Minimal `MirFn` carrying just a body — every other field is a
    /// neutral default so the coverage walk (which only reads `body`)
    /// has something well-formed to traverse.
    fn fn_with_body(fn_id: crate::ir::FnId, body: Spanned<MirExpr>) -> crate::ir::mir::MirFn {
        crate::ir::mir::MirFn {
            fn_id,
            name: String::new(),
            params: vec![],
            return_type: String::new(),
            effects: vec![],
            body,
            local_count: 0,
            aliased_slots: std::sync::Arc::new(vec![]),
        }
    }

    #[test]
    fn coverage_with_blockers_counts_and_buckets() {
        // Build a two-fn program: one emits (a literal), one blocks on
        // Match. The report must read 1 covered / 1 fallback with a
        // single "Match" bucket of count 1.
        let mut program = MirProgram::default();
        let covered_body = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let blocked_body = span(MirExpr::Match(span(crate::ir::mir::MirMatch {
            subject: Box::new(span(MirExpr::Literal(span(crate::ast::Literal::Int(0))))),
            arms: vec![],
        })));
        program.fns.insert(
            crate::ir::FnId(0),
            fn_with_body(crate::ir::FnId(0), covered_body),
        );
        program.fns.insert(
            crate::ir::FnId(1),
            fn_with_body(crate::ir::FnId(1), blocked_body),
        );

        let (report, blockers) = coverage_report_with_blockers(&program, &empty_ctx());
        assert_eq!(report.total, 2);
        assert_eq!(report.mir_covered, 1);
        assert_eq!(report.hir_fallback, 1);
        assert_eq!(blockers.get("Match"), Some(&1));
    }

    // ── Wave 4 ──────────────────────────────────────────────────────

    /// Build `let a = <a_val>; let b = <b_val>; <body>` as a nested
    /// MIR `Let` chain.
    fn let_chain(
        a: (&str, Spanned<MirExpr>),
        b: (&str, Spanned<MirExpr>),
        body: Spanned<MirExpr>,
    ) -> Spanned<MirExpr> {
        let inner = MirExpr::Let(span(crate::ir::mir::MirLet {
            binding: LocalId(1),
            binding_name: b.0.to_string(),
            value: Box::new(b.1),
            body: Box::new(body),
        }));
        span(MirExpr::Let(span(crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: a.0.to_string(),
            value: Box::new(a.1),
            body: Box::new(span(inner)),
        })))
    }

    #[test]
    fn fn_body_emits_let_chain_as_flat_statement_lines() {
        // A top-level `Let` chain must render as flat `let …;`-lines —
        // the format HIR's `Block` body arm produces — NOT the nested
        // block-expr `{ let a = …; { let b = …; … } }` that an inline
        // `Let` renders. This is the Wave-4 multi-statement boundary.
        let a_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "a".to_string(),
        };
        let body = let_chain(
            ("a", int_lit(1)),
            ("b", int_lit(2)),
            span(MirExpr::Local(span(a_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("let chain emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    let a = 1i64;\n    let b = 2i64;\n    a"
        );
    }

    #[test]
    fn fn_body_emits_discarded_intermediate_as_bare_statement() {
        // A discarded intermediate (`Stmt::Expr` at non-tail position, or
        // a `_ = effect()` discard) is modeled as a `Let` with an EMPTY
        // `binding_name`. It must render as a bare `value;` statement (the
        // value evaluated, result dropped) — the mirror of HIR's non-last
        // `ResolvedStmt::Expr` arm — NOT fall back to HIR. This is the
        // dominant Stage-3 None gap.
        //
        // Shape: `g = <1>; <2 discarded>; g`
        let g_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "g".to_string(),
        };
        let body = let_chain(
            ("g", int_lit(1)),
            ("", int_lit(2)), // discarded intermediate — empty binding_name
            span(MirExpr::Local(span(g_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("discarded stmt emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    let g = 1i64;\n    2i64;\n    g"
        );
    }

    #[test]
    fn fn_body_emits_leading_discarded_statement() {
        // A body whose FIRST statement is a discard (empty binding_name)
        // must still take the flat path (no first-binding guard) and emit
        // the leading bare statement.
        //
        // Shape: `<1 discarded>; g = <2>; g`
        let g_local = MirLocal {
            slot: LocalId(1),
            last_use: true,
            name: "g".to_string(),
        };
        let body = let_chain(
            ("", int_lit(1)), // leading discard
            ("g", int_lit(2)),
            span(MirExpr::Local(span(g_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("leading discard emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    1i64;\n    let g = 2i64;\n    g"
        );
    }

    #[test]
    fn inline_discarded_let_renders_as_bare_block_statement() {
        // An inline `Let` with an empty binding_name (discard not at the
        // body top-level) renders as `{ value; body }` — bare statement,
        // result dropped — not a `let _ = …`.
        let value = int_lit(7);
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "x".to_string(),
        };
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(span(MirExpr::Local(span(body_local)))),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("inline discard emits");
        assert_eq!(emit, "{ 7i64; x }");
    }

    #[test]
    fn inline_let_still_renders_as_block_expr() {
        // An inline `Let` (NOT at top-level body position) still renders
        // as a nested block-expr — only the fn-body path flattens.
        let value = int_lit(7);
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "x".to_string(),
        };
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(value),
            body: Box::new(span(MirExpr::Local(span(body_local)))),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("inline let emits");
        assert_eq!(emit, "{ let x = 7i64; x }");
    }

    #[test]
    fn neg_folded_int_literal_re_wraps_like_hir_neg() {
        // `const_fold` collapses `Neg(Int(5))` → `Literal(-5)`; the
        // walker re-wraps it as `(-5i64)` to match HIR's `Neg` arm
        // (which never folds).
        let expr = span(MirExpr::Literal(span(crate::ast::Literal::Int(-5))));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg int literal emits");
        assert_eq!(emit, "(-5i64)");
    }

    #[test]
    fn neg_folded_float_literal_re_wraps_like_hir_neg() {
        // `Neg(Float(273.15))` folds to `Literal(-273.15)`; re-wrap to
        // `(-273.15f64)` to match HIR's `(-273.15f64)`.
        let expr = span(MirExpr::Literal(span(crate::ast::Literal::Float(-273.15))));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg float literal emits");
        assert_eq!(emit, "(-273.15f64)");
    }

    #[test]
    fn positive_literals_unchanged_by_neg_rewrap() {
        // Positive literals are never wrapped.
        let i = span(MirExpr::Literal(span(crate::ast::Literal::Int(5))));
        assert_eq!(emit_mir_expr(&i, &empty_ctx()).as_deref(), Some("5i64"));
        let f = span(MirExpr::Literal(span(crate::ast::Literal::Float(1.5))));
        assert_eq!(emit_mir_expr(&f, &empty_ctx()).as_deref(), Some("1.5f64"));
    }

    /// Build an `IfThenElse` with a comparison `cond` of the given op
    /// over two named Int locals, and `Int` literal branches.
    fn if_compare(op: BinOp) -> Spanned<MirExpr> {
        let lhs = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "code".to_string(),
        };
        let cond = MirExpr::BinOp(span(crate::ir::mir::MirBinOp {
            op,
            lhs: Box::new(span_ty(MirExpr::Local(span(lhs)), Type::Int)),
            rhs: Box::new(int_lit(48)),
        }));
        span(MirExpr::IfThenElse(span(crate::ir::mir::MirIfThenElse {
            cond: Box::new(span(cond)),
            then_branch: Box::new(int_lit(1)),
            else_branch: Box::new(int_lit(0)),
        })))
    }

    #[test]
    fn if_then_else_keeps_lt_canonical_no_swap() {
        // `<` is canonical (invert=false): keep operator, branches in
        // source order.
        let emit = emit_mir_expr(&if_compare(BinOp::Lt), &empty_ctx()).expect("if emits");
        assert_eq!(emit, "if (code < 48i64) { 1i64 } else { 0i64 }");
    }

    #[test]
    fn if_then_else_inverts_gte_to_lt_and_swaps_branches() {
        // `>=` → HIR canonicalizes to `<` + invert (swap branches):
        // `if (code < 48) { else_branch } else { then_branch }`.
        let emit = emit_mir_expr(&if_compare(BinOp::Gte), &empty_ctx()).expect("if emits");
        assert_eq!(emit, "if (code < 48i64) { 0i64 } else { 1i64 }");
    }

    #[test]
    fn if_then_else_inverts_lte_to_gt_and_swaps_branches() {
        let emit = emit_mir_expr(&if_compare(BinOp::Lte), &empty_ctx()).expect("if emits");
        assert_eq!(emit, "if (code > 48i64) { 0i64 } else { 1i64 }");
    }

    #[test]
    fn if_then_else_inverts_neq_to_eq_and_swaps_branches() {
        let emit = emit_mir_expr(&if_compare(BinOp::Neq), &empty_ctx()).expect("if emits");
        assert_eq!(emit, "if (code == 48i64) { 0i64 } else { 1i64 }");
    }

    #[test]
    fn if_then_else_cond_does_not_deref_string_literal() {
        // HIR's bool-if-else condition uses a plain `emit_expr` — it
        // does NOT apply the `BinOp` arm's `&*name == "lit"` deref. So
        // `match name == "_"` emits `name == AverStr::from("_")` in the
        // cond, matching HIR byte-for-byte.
        let name = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "name".to_string(),
        };
        let cond = MirExpr::BinOp(span(crate::ir::mir::MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(span_ty(MirExpr::Local(span(name)), Type::Str)),
            rhs: Box::new(span_ty(
                MirExpr::Literal(span(crate::ast::Literal::Str("_".to_string()))),
                Type::Str,
            )),
        }));
        let expr = span(MirExpr::IfThenElse(span(crate::ir::mir::MirIfThenElse {
            cond: Box::new(span(cond)),
            then_branch: Box::new(int_lit(1)),
            else_branch: Box::new(int_lit(0)),
        })));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (name == AverStr::from(\"_\")) { 1i64 } else { 0i64 }"
        );
    }
}
