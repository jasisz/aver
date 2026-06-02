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
//! `Call` (`Fn` / `Builtin`), `Return`, `TailCall` (emitted as a plain
//! call; the HIR self-TCO `continue` rewrite needs `ectx`, so the
//! wire-up's parity check is the safety net), `Try` (`?`), `Tuple`,
//! `List`, `MapLiteral`, `Let` (block-expr `{ let x = …; … }`),
//! `Project`, `RecordCreate` / `RecordUpdate`, `Construct` (built-in and
//! user ctors, including dep-module records resolved through
//! `module_prefixes`), and `IfThenElse`.
//!
//! Not covered — these fall back to the HIR walker: `Match`,
//! `IndependentProduct`, and `FnValue`. `InterpolatedStr` never reaches
//! the walker — `interp_lower` lowers it away before codegen runs.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Spanned, Type};
use crate::codegen::CodegenContext;
use crate::codegen::common::module_prefix_to_rust_path;
use crate::ir::SymbolTable;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirLocal, MirProgram};

use super::emit_ctx::{is_copy_type, should_borrow_param};
use super::expr::{callee_borrow_mask, constructor_boxed_positions, emit_literal};
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
    /// Build the borrow policy from a `ResolvedFnDef`'s param
    /// types. `borrow_by_default` mirrors `EmitCtx::for_fn` (true)
    /// vs `EmitCtx::for_fn_no_borrow` (false, the TCO / memo path):
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
            // A `Builtin` / `Intrinsic` / `LocalSlot` callee is itself
            // the blocker — report the callee kind so the histogram
            // distinguishes "builtin call" from "closure call".
            match &c.node.callee {
                MirCallee::Builtin(_) => return Some("Call(Builtin)"),
                MirCallee::Intrinsic(_) => return Some("Call(Intrinsic)"),
                MirCallee::LocalSlot { .. } => return Some("Call(LocalSlot)"),
                MirCallee::Fn(_) => {}
            }
            for a in &c.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            Some("Call(Fn)")
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
        MirExpr::Literal(lit) => Some(emit_literal(&lit.node)),
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
                // Builtin / closure / unresolved callees ride the
                // HIR walker's classification (`CallPlan`) — too
                // many shapes to mirror here. Buffer intrinsics
                // likewise fall back (the Rust backend deforests
                // differently).
                MirCallee::Builtin(_) | MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => {
                    None
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
            // Synthetic locals (intermediate effectful
            // `Stmt::Expr` at non-tail position) carry
            // `binding_name.is_empty()` — the Rust walker can't
            // emit them as named idents, so we fall back to
            // HIR. Mirror of the `MirLocal { name }` empty-name
            // fallback on the read side.
            let let_node = &spanned_let.node;
            if let_node.binding_name.is_empty() {
                return None;
            }
            let value = emit_mir_expr(&let_node.value, emit_ctx)?;
            let body = emit_mir_expr(&let_node.body, emit_ctx)?;
            let name = aver_name_to_rust(&let_node.binding_name);
            Some(format!("{{ let {} = {}; {} }}", name, value, body))
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
        MirExpr::IfThenElse(spanned_ite) => {
            // Direct conditional. Emits as a
            // Rust `if … { … } else { … }` expression. Each
            // subtree must emit cleanly or the whole node
            // falls back to HIR.
            let ite = &spanned_ite.node;
            let cond = emit_mir_expr(&ite.cond, emit_ctx)?;
            let then_branch = emit_mir_expr(&ite.then_branch, emit_ctx)?;
            let else_branch = emit_mir_expr(&ite.else_branch, emit_ctx)?;
            Some(format!(
                "if {} {{ {} }} else {{ {} }}",
                cond, then_branch, else_branch
            ))
        }
        _ => None,
    }
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
/// without it). Multi-statement bodies (a top-level `Let` chain)
/// emit as a Rust block-expr `{ let … ; … }`, which never matches
/// HIR's flat `let …;`-line block format — so they fall back via
/// the byte comparison. That's the intended Wave-1 boundary.
pub(super) fn emit_mir_fn_body(
    body: &Spanned<MirExpr>,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
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
/// `borrow_by_default` is `false` only on the TCO/memo no-borrow
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

/// Mirror of HIR's `clone_arg`: emit an expression as an owning
/// argument. The Project field-Copy elision the HIR walker does
/// requires record-field-type introspection (`attr_result_is_copy`)
/// that the MIR walker can't reproduce identically, so a `Project`
/// on a Copy-typed field would diverge from HIR — the parity gate
/// catches that as a mismatch and falls back. For the common case
/// (non-Project args) this matches HIR exactly.
fn mir_clone_arg(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    mir_maybe_clone(code, expr, ctx)
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
    fn returns_none_for_builtin_call() {
        // Builtin calls ride the HIR walker (call-plan
        // classification). MIR walker returns None.
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hello".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
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
        // Pick a variant the walker doesn't cover — IndependentProduct.
        let ip = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![],
                unwrap_results: false,
            },
        )));
        assert!(emit_mir_expr(&ip, &empty_ctx()).is_none());
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
    fn returns_none_for_synthetic_let() {
        // Synthetic Let (intermediate
        // effectful Stmt::Expr at non-tail position) carries an
        // empty `binding_name`. Walker must fall back to HIR
        // since there's no source ident to bind in Rust.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body = span(MirExpr::Literal(span(crate::ast::Literal::Int(0))));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(7),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
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
}
