//! Rust backend: emit expressions from Core MIR.
//!
//! Mirror of [`super::expr::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and emits the
//! same Rust source string — the same deduplication MIR brought to the
//! VM: one semantic walker per construct lives in MIR, and every backend
//! reads from it instead of forking `ResolvedExpr`.
//!
//! [`emit_mir_expr`] is the dispatcher; [`coverage_report`] measures how
//! much of a program it can already render. It is **not yet wired into
//! the Rust backend's production emit path** — that path still uses the
//! HIR walker; this is the tested groundwork for moving the Rust backend
//! onto MIR (hence `#[allow(dead_code)]` on the entry points). Once
//! wired, a construct it returns `None` for is the signal to fall back
//! to the HIR walker for that expression.
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

use std::collections::HashSet;

use crate::ast::{BinOp, Spanned, Type};
use crate::codegen::CodegenContext;
use crate::codegen::common::module_prefix_to_rust_path;
use crate::ir::SymbolTable;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirProgram};

use super::expr::emit_literal;
use super::syntax::aver_name_to_rust;

/// Walker-side emit context. Holds *only* the slice of the
/// `CodegenContext` the MIR-to-Rust walker actually reads —
/// keeping the dependency surface explicit so future
/// `CodegenContext` refactors don't ripple through the walker,
/// and so other backends (wasm-gc, wasip2) can introduce their
/// own emit-ctx structs without inheriting Rust-specific fields.
///
/// Fields grow only when the walker needs them. Today's scope:
/// - `symbol_table` — `FnId` / `TypeId` / `CtorId` resolution.
/// - `module_prefixes` — `resolve_module_call` for module-scoped
///   record types and (eventually) User-ctor module paths.
#[derive(Debug, Clone, Copy)]
pub struct MirEmitCtx<'a> {
    pub symbol_table: &'a SymbolTable,
    pub module_prefixes: &'a HashSet<String>,
}

impl<'a> MirEmitCtx<'a> {
    /// Construct a walker ctx from the full `CodegenContext`.
    /// Production callers (codegen pipeline, `coverage_report`
    /// driven from `aver compile --explain-mir-coverage`) use
    /// this; test fixtures use [`Self::for_test`] with hand-
    /// rolled symbol-table + empty prefixes.
    pub fn for_codegen(ctx: &'a CodegenContext) -> Self {
        Self {
            symbol_table: &ctx.symbol_table,
            module_prefixes: &ctx.module_prefixes,
        }
    }

    /// Construct a minimal walker ctx for tests. Caller supplies
    /// a hand-built symbol table; `module_prefixes` defaults to
    /// the caller's owned empty set (or a populated one when the
    /// test needs to exercise module-scoped name resolution).
    pub fn for_test(symbol_table: &'a SymbolTable, module_prefixes: &'a HashSet<String>) -> Self {
        Self {
            symbol_table,
            module_prefixes,
        }
    }
}

/// Mirror of `RustSourceCallCtx::resolve_module_call` in
/// `toplevel.rs`: find the longest registered module prefix
/// inside a dotted name. Returns `(prefix, suffix)` on hit,
/// `None` when no registered prefix matches.
#[allow(dead_code)]
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
/// Mirror of [`super::expr::emit_expr`] for the covered
/// subset; output strings should be character-for-character
/// identical to the HIR walker's output on the same input
/// (modulo type-disambiguation paths the HIR walker takes via
/// `EmitCtx`, which we don't have access to here).
///
/// Allowed dead: not yet called from the production emit path
/// (see the module docs) — wiring it into [`super::expr::emit_expr`]
/// (try MIR first, fall back to the HIR walker on `None`) is the
/// step that switches the Rust backend onto MIR.
#[allow(dead_code)]
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
            // walker takes via `ectx.expr_is_numeric`. When both
            // operands are non-numeric (Str), the right side is
            // borrowed via `&` for `AverStr`'s `Add<&AverStr>`
            // impl.
            if matches!(bop.op, BinOp::Add)
                && !ty_is_numeric(bop.lhs.ty())
                && !ty_is_numeric(bop.rhs.ty())
            {
                Some(format!("({} + &{})", l, r))
            } else {
                Some(format!("({} {} {})", l, op_str, r))
            }
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match &call.callee {
                MirCallee::Fn(fn_id) => {
                    // Resolve canonical name through the same
                    // symbol table the HIR walker uses; emit
                    // `name(arg1, arg2, …)` in source order.
                    let name = emit_ctx.symbol_table.fn_entry(*fn_id).key.canonical();
                    let mut args = Vec::with_capacity(call.args.len());
                    for a in &call.args {
                        args.push(emit_mir_expr(a, emit_ctx)?);
                    }
                    Some(format!("{}({})", aver_name_to_rust(&name), args.join(", ")))
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
            let mut args = Vec::with_capacity(tc.args.len());
            for a in &tc.args {
                args.push(emit_mir_expr(a, emit_ctx)?);
            }
            Some(format!("{}({})", aver_name_to_rust(&name), args.join(", ")))
        }
        MirExpr::Try(inner) => {
            // `value?` propagation. Mirror of
            // HIR's `ResolvedExpr::ErrorProp` emit — append `?`
            // to the inner expression's Rust form.
            Some(format!("{}?", emit_mir_expr(inner, emit_ctx)?))
        }
        MirExpr::Tuple(items) => {
            // `(a, b, c)` tuple literal. Mirror
            // of HIR's `ResolvedExpr::Tuple` emit, minus the
            // `clone_arg` insertion (no `ectx` here — borrowed-
            // param Locals signal the gap by returning their
            // raw name and the outer caller still gets a
            // well-formed string). For pure-value subtrees the
            // output is character-identical to HIR.
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(emit_mir_expr(item, emit_ctx)?);
            }
            Some(format!("({})", parts.join(", ")))
        }
        MirExpr::List(items) => {
            // `[a, b, c]` list literal. Mirror
            // of HIR's `ResolvedExpr::List` — empty case folds
            // to `aver_rt::AverList::empty()`, non-empty to
            // `from_vec(vec![...])`.
            if items.is_empty() {
                return Some("aver_rt::AverList::empty()".to_string());
            }
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(emit_mir_expr(item, emit_ctx)?);
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
            // `vec![(k, v), …].into_iter().collect::<HashMap<_, _>>()`.
            // No clone_arg insertion; pure-value subtrees match
            // HIR character-for-character.
            if entries.is_empty() {
                return Some("HashMap::new()".to_string());
            }
            let mut parts = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                let key_str = emit_mir_expr(k, emit_ctx)?;
                let val_str = emit_mir_expr(v, emit_ctx)?;
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
            // shape — `{type_name} { field: value, … }`. HIR
            // reads the source-level `type_name` string which
            // is always the bare ident the user typed (resolver
            // doesn't dot-prefix it); we mirror that by emitting
            // `entry.key.name` regardless of `scope`.
            //
            // One special-case: `Tcp.Connection` is a runtime-
            // re-exported struct (`pub use … as Tcp_Connection`)
            // — HIR hardcodes the rename. The walker bounces so
            // HIR handles it.
            let rec = &spanned_rec.node;
            // Built-in records (no user `TypeId`) ride the HIR walker.
            let type_id = rec.type_id?;
            let entry = emit_ctx.symbol_table.type_entry(type_id);
            if entry.key.canonical() == "Tcp.Connection" {
                return None;
            }
            let type_name = entry.key.name.clone();
            let mut parts = Vec::with_capacity(rec.fields.len());
            for f in &rec.fields {
                let val = emit_mir_expr(&f.value, emit_ctx)?;
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!("{} {{ {} }}", type_name, parts.join(", ")))
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            // `T.update(base, field = v, …)` →
            // `{type_name} { field: value, …, ..base }`. Same
            // bare-name + Tcp.Connection gating as RecordCreate.
            let upd = &spanned_upd.node;
            // Built-in records (no user `TypeId`) ride the HIR walker.
            let type_id = upd.type_id?;
            let entry = emit_ctx.symbol_table.type_entry(type_id);
            if entry.key.canonical() == "Tcp.Connection" {
                return None;
            }
            let type_name = entry.key.name.clone();
            let base = emit_mir_expr(&upd.base, emit_ctx)?;
            let mut parts = Vec::with_capacity(upd.updates.len());
            for f in &upd.updates {
                let val = emit_mir_expr(&f.value, emit_ctx)?;
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!(
                "{} {{ {}, ..{} }}",
                type_name,
                parts.join(", "),
                base
            ))
        }
        MirExpr::Construct(spanned_ctor) => {
            // Built-in ctors emit Result /
            // Option wrappers; user ctors resolve through the
            // symbol table for module-qualified path mangling.
            //
            // Boxed-position handling (recursive-field wrapping
            // in `std::sync::Arc::new`) is HIR-only — the walker
            // emits raw args, so recursive types would diverge
            // from HIR's output. That's tolerated until wire-up
            // adds a parity check; the coverage diagnostic
            // reports `Some` either way.
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
                        args.push(emit_mir_expr(a, emit_ctx)?);
                    }
                    Some(format!("{}({})", name, args.join(", ")))
                }
                MirCtor::User(ctor_id) => {
                    // Resolve `CtorId` → owning
                    // type → variant name via the symbol table,
                    // then route the qualified type name through
                    // `resolve_module_call` for module-path
                    // mangling. Mirror of HIR's
                    // `emit_type_constructor_call`.
                    let ctor_entry = emit_ctx.symbol_table.ctor_entry(ctor_id);
                    let variant_name = ctor_entry.name.clone();
                    let type_entry = emit_ctx.symbol_table.type_entry(ctor_entry.owning_type);
                    let qualified = type_entry.key.canonical();
                    let mut args = Vec::with_capacity(con.args.len());
                    for a in &con.args {
                        args.push(emit_mir_expr(a, emit_ctx)?);
                    }
                    let args_str = args.join(", ");
                    let head = if let Some((prefix, suffix)) =
                        resolve_module_call(&qualified, emit_ctx.module_prefixes)
                    {
                        format!("{}::{}", module_prefix_to_rust_path(prefix), suffix)
                    } else {
                        qualified
                    };
                    Some(format!("{}::{}({})", head, variant_name, args_str))
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

/// Is the type stamp a primitive numeric?
/// `Int` / `Float` / `Byte` count; everything else (incl. `Str`)
/// doesn't. Mirror of HIR's `EmitCtx::expr_is_numeric` for the
/// MIR walker's `+` dispatch.
fn ty_is_numeric(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int | Type::Float))
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
        // `Point { x: 1, y: 2 }` for a
        // module-unscoped record type.
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
            type_name: "Test".to_string(),
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
    fn returns_none_for_tcp_connection_record() {
        // `Tcp.Connection` is the lone
        // hardcoded special-case in HIR (re-exported as
        // `Tcp_Connection`) — the walker bounces so HIR
        // handles the rename.
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Test".to_string(),
            fields: vec![],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Connection", true);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        assert!(emit_mir_expr(&expr, &ctx).is_none());
    }

    #[test]
    fn emits_record_create_dep_module_as_bare_name() {
        // A dep-module record (e.g.
        // `ast.Expr` resolving to scope=`ast`, name=`Expr`)
        // emits the bare `Expr { … }` — the consumer module's
        // import statement makes `Expr` resolve correctly,
        // mirror of HIR's source-name-passthrough.
        let field = crate::ir::mir::MirRecordField {
            name: "tag".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Test".to_string(),
            fields: vec![field],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        // Scoped under `ast`, but `Tcp.Connection` ≠ canonical
        // so it doesn't hit the bounce.
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
        // `T { field: v, ..base }`.
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
            type_name: "Test".to_string(),
            updates: vec![update],
        };
        let expr = span(MirExpr::RecordUpdate(span(upd)));
        let st = symbols_with_one_type("Point", false);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("record update should emit");
        assert_eq!(emit, "Point { x: 9i64, ..base }");
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
