//! ETAP-2 SLICE 1 — the Int-representation MIR→MIR rewrite.
//!
//! Turns the [`super::bare_i64`] range+escape ANALYSIS (a side table) into
//! EXPLICIT representation in the MIR: it tags each fn's bare slots /
//! params / return on [`MirFn::repr`](crate::ir::mir::MirFn) and inserts
//! [`MirExpr::Box`] / [`MirExpr::Unbox`] boundary nodes at every position
//! where a raw `i64` value crosses into an `Int` (`AverInt`) context or
//! vice-versa. After this pass, the Rust backend lowers TRIVIALLY: a bare
//! slot reads native `i64`, raw arithmetic stays raw, and a `Box`/`Unbox`
//! node is the only place a boundary conversion is emitted — codegen no
//! longer DECIDES where boundaries go, so the boundary-completeness bug
//! class (PR #519) becomes structurally impossible.
//!
//! ## Where it runs
//!
//! LATE and Rust-codegen-ONLY. The shared `optimize()` pipeline (consumed
//! by the VM, wasm-gc, proof, Dafny, Lean) does NOT run this — those keep
//! the all-`Int` representation. The Rust backend applies it to a CLONE of
//! the optimized MIR at the point it codegens. Running after proof export
//! and shape recognition is mandatory: both key on the syntactic structure
//! this rewrite rewrites.
//!
//! ## Equivalence to the prior side-table codegen
//!
//! The rewrite inserts a boundary node at EXACTLY the positions the old
//! `from_mir.rs` inserted a conversion (the 5 sites: boxed-arithmetic
//! operand, call-arg at a boxed/bare callee param, a boxed `Let` crossing,
//! a boxed-return tail leaf), reusing the SAME [`FnBareFacts`] predicates
//! (`is_bare`, `expr_is_bare_i64`, `param_is_bare`, `bare_return`). So the
//! emitted Rust is behavior-equivalent: the conversions land in the same
//! places, just decided ONCE here instead of re-derived per emit site.
//!
//! ## Fail-closed
//!
//! Pure function of the analysis output. A value is raw only where the
//! analysis proved it `Bare`; a missing/unknown fact ⇒ boxed. A wrongly
//! raw value would still fail to type-check in the emitted Rust (raw `i64`
//! into an `AverInt` slot is `E0308`), the same backstop as before.

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::mir::program::LocalId;
use crate::ir::mir::{
    BareI64Facts, FnBareFacts, MirCallee, MirExpr, MirFn, MirFnRepr, MirProgram, MirStrPart,
};

/// Rewrite `program` so Int representation is explicit (ETAP-2). Runs the
/// existing range+escape analysis, then for every fn populates `repr` and
/// inserts `Box`/`Unbox` boundary nodes. Pure: returns a fresh program;
/// the caller (the Rust backend) feeds it the clone it codegens from.
pub fn rewrite_for_rust(mut program: MirProgram) -> MirProgram {
    let facts = super::bare_i64::analyze(&program);
    // Collect ids first to avoid borrowing `program.fns` while mutating it.
    let ids: Vec<crate::ir::FnId> = program.fns.keys().copied().collect();
    for id in ids {
        let Some(fn_facts) = facts.for_fn(id).cloned() else {
            continue;
        };
        if let Some(f) = program.fns.get_mut(&id) {
            rewrite_fn(f, &fn_facts, &facts);
        }
    }
    program
}

/// Populate `f.repr` from the analysis and insert the explicit boundary
/// nodes throughout `f.body`.
fn rewrite_fn(f: &mut MirFn, facts: &FnBareFacts, all: &BareI64Facts) {
    // 1. Make the per-value representation explicit on the fn.
    let mut bare_slots = std::collections::HashSet::new();
    for (slot, fact) in &facts.values {
        if fact.is_bare() {
            bare_slots.insert(*slot);
        }
    }
    f.repr = MirFnRepr {
        bare_slots,
        bare_params: facts.bare_params.clone(),
        bare_return: facts.bare_return,
    };

    // 2. Insert boundary nodes. The body is in RETURN position: a bare
    //    return fn expects its tail raw; a boxed return fn expects it
    //    boxed (and a bare leaf there is `Box`ed — defect Q5 / subj_ret).
    let ctx = RewriteCtx { facts, all };
    let body = std::mem::replace(
        &mut f.body,
        Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
    );
    f.body = rewrite_tail(body, facts.bare_return, &ctx);
}

struct RewriteCtx<'a> {
    facts: &'a FnBareFacts,
    all: &'a BareI64Facts,
}

/// Wrap `e` in a fresh `Box` node (raw i64 -> Int), preserving the span +
/// the logical `Int` type stamp (a representation boundary keeps the value's
/// logical type — only its machine representation changes — so downstream
/// numeric-disambiguation reads still see `Int`).
fn box_node(e: Spanned<MirExpr>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    let out = Spanned::new(MirExpr::Box(std::boxed::Box::new(e)), line);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

/// Wrap `e` in a fresh `Unbox` node (Int -> raw i64), preserving the span +
/// logical type stamp (see [`box_node`]).
fn unbox_node(e: Spanned<MirExpr>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    let out = Spanned::new(MirExpr::Unbox(std::boxed::Box::new(e)), line);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

/// Does `e` render as a raw `i64` value (a bare leaf or a whole-tree-bare
/// `Add`/`Sub`/`Mul`/`Neg`)? Same gate the codegen `emit_bare_i64` path
/// keys on — `FnBareFacts::expr_is_bare_i64`.
fn renders_raw(e: &MirExpr, facts: &FnBareFacts) -> bool {
    facts.expr_is_bare_i64(e)
}

/// Rewrite an expression appearing in a BOXED (`Int`/`AverInt`) context:
/// every Int sub-value here must be an `AverInt`. A sub-expression that
/// renders raw is wrapped in `Box`. This is the structural relocation of
/// the codegen `boxed_int_operand` coercion + the boxed-return tail boxing.
fn rewrite_boxed(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A standalone Int literal already lowers to `AverInt::from_i64(N)` on
    // the boxed path — never wrap it (would double-box). Mirrors
    // `boxed_int_operand`'s literal skip.
    if matches!(&e.node, MirExpr::Literal(l) if matches!(l.node, Literal::Int(_))) {
        return rewrite_children(e, ctx);
    }
    // A call to a bare-RETURN callee renders as raw `i64` (its signature is
    // `-> i64`); in a boxed context it must be boxed (defect Q5).
    if call_returns_raw(&e.node, ctx) {
        let inner = rewrite_children(e, ctx);
        return box_node(inner);
    }
    // A directly raw-rendering leaf / compound (a bare `Local`, or an
    // in-range `Add`/`Sub`/`Mul`/`Neg` tree): box it. Its raw operands stay
    // raw inside (rewrite_children leaves them — they are consumed by the
    // native arithmetic that the Box wraps).
    if renders_raw(&e.node, ctx.facts) {
        return box_node(e);
    }
    // Otherwise it is already an `AverInt` — recurse so any nested boxed
    // contexts (call args, aggregate elements, …) get their own boundaries.
    rewrite_children(e, ctx)
}

/// Rewrite an expression appearing in a RAW (`i64`) context: the consumer
/// expects a native `i64`. A sub-expression that is already raw stays; one
/// that is a boxed `AverInt` is narrowed via `Unbox`. This is the
/// relocation of the codegen call-arg `to_i64` coercion at a bare param.
fn rewrite_raw(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A call to a bare-return callee is already raw `i64`.
    if call_returns_raw(&e.node, ctx) {
        return rewrite_children(e, ctx);
    }
    // A raw-rendering leaf / compound: stays raw, but recurse so a nested
    // boxed operand inside an arithmetic tree is handled. (By construction
    // a whole-tree-bare compound has only raw leaves, so this is a no-op,
    // but recursing keeps the walk total.)
    if renders_raw(&e.node, ctx.facts) {
        return rewrite_children(e, ctx);
    }
    // A boxed `AverInt` value reaching a raw context: narrow it. (Rare by
    // construction — the analysis only marks a param bare when every caller
    // supplies a raw or literal value; this covers the residual `to_i64`
    // call-arg path.)
    let inner = rewrite_children(e, ctx);
    unbox_node(inner)
}

/// Is `e` a `Call(Fn)` / `TailCall` whose callee's return the analysis
/// proved bare (renders as raw `i64`)? Mirror of codegen's
/// `mir_call_returns_bare`.
fn call_returns_raw(e: &MirExpr, ctx: &RewriteCtx<'_>) -> bool {
    let target = match e {
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(t) => t,
            _ => return false,
        },
        MirExpr::TailCall(tc) => tc.node.target,
        _ => return false,
    };
    ctx.all.for_fn(target).is_some_and(|f| f.bare_return)
}

/// Rewrite a value in TAIL/return position. `bare_return` is the enclosing
/// fn's return representation: when bare, the tail is a raw context; when
/// boxed, a boxed context (and the descent boxes a bare leaf — defects
/// Q5 / subj_ret). Match / IfThenElse / Let / Return recurse into their
/// tail leaves so each arm/branch is handled in the right context.
fn rewrite_tail(e: Spanned<MirExpr>, bare_return: bool, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    let line = e.line;
    match e.node {
        MirExpr::Match(mut m) => {
            // Subject is an ordinary value position (handled by its own
            // children rewrite); the arm bodies are tails.
            let subj = std::mem::replace(
                &mut m.node.subject,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            m.node.subject = std::boxed::Box::new(rewrite_value(*subj, ctx));
            for arm in &mut m.node.arms {
                let body = std::mem::replace(
                    &mut arm.body,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                arm.body = rewrite_tail(body, bare_return, ctx);
            }
            Spanned::new(MirExpr::Match(m), line)
        }
        MirExpr::IfThenElse(mut ite) => {
            let cond = std::mem::replace(
                &mut ite.node.cond,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Bool(false),
                )))),
            );
            ite.node.cond = std::boxed::Box::new(rewrite_value(*cond, ctx));
            let then_b = std::mem::replace(
                &mut ite.node.then_branch,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            ite.node.then_branch = std::boxed::Box::new(rewrite_tail(*then_b, bare_return, ctx));
            let else_b = std::mem::replace(
                &mut ite.node.else_branch,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            ite.node.else_branch = std::boxed::Box::new(rewrite_tail(*else_b, bare_return, ctx));
            Spanned::new(MirExpr::IfThenElse(ite), line)
        }
        MirExpr::Let(mut l) => {
            // The let VALUE is a value position (boxed unless its binding
            // slot is bare); the let BODY is the tail.
            let value = std::mem::replace(
                &mut l.node.value,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            l.node.value = std::boxed::Box::new(rewrite_let_value(*value, l.node.binding, ctx));
            let body = std::mem::replace(
                &mut l.node.body,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            l.node.body = std::boxed::Box::new(rewrite_tail(*body, bare_return, ctx));
            Spanned::new(MirExpr::Let(l), line)
        }
        MirExpr::Return(inner) => {
            let r = rewrite_tail(*inner, bare_return, ctx);
            Spanned::new(MirExpr::Return(std::boxed::Box::new(r)), line)
        }
        // A self-tail-call's value is the recurrence, not a base value — its
        // args are handled by `rewrite_value` (call-arg boundaries).
        other => {
            let spanned = Spanned::new(other, line);
            if bare_return {
                rewrite_raw(spanned, ctx)
            } else {
                rewrite_boxed(spanned, ctx)
            }
        }
    }
}

/// Rewrite the VALUE of a `Let` binding. The binding's representation is
/// its slot repr: a bare binding wants a raw value, a boxed binding wants
/// an `AverInt` (defect esc_match — a `let x = n - 1` whose `x` escapes is
/// boxed, so the raw `n - 1` is `Box`ed at the crossing).
fn rewrite_let_value(
    e: Spanned<MirExpr>,
    binding: LocalId,
    ctx: &RewriteCtx<'_>,
) -> Spanned<MirExpr> {
    if ctx.facts.is_bare(binding) {
        rewrite_raw(e, ctx)
    } else {
        rewrite_boxed(e, ctx)
    }
}

/// Rewrite an expression in an ordinary VALUE position (not tail, not a
/// specially-typed context). Defaults to a boxed context — the general Int
/// world — except where the node itself imposes a raw sub-context
/// (arithmetic over bare operands, a bare callee param). The per-node
/// boundary logic lives in `rewrite_children`.
fn rewrite_value(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A raw-rendering compound/leaf in a value position is itself raw; its
    // CONSUMER (the parent) decides whether to box it. So here we only
    // recurse into children (which place their own boundaries); the parent
    // arm that produced this `rewrite_value` call already applied the
    // boxed/raw wrapper if it needed one.
    rewrite_children(e, ctx)
}

/// Recurse into the children of `e`, placing boundary nodes per-node:
///
/// - `BinOp` arithmetic over Int operands: a bare operand consumed by a
///   BOXED `AverInt` op is `Box`ed (`boxed_int_operand`); a whole-tree-bare
///   op keeps raw operands.
/// - `Call(Fn)` / `TailCall`: each arg is rewritten in the callee's i-th
///   param context (raw if the param is bare, boxed otherwise).
/// - aggregates / records / maps / interpolation: each element is a boxed
///   context (a general-Int store).
/// - everything else: structural recursion in a value context.
fn rewrite_children(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // Preserve the node's type stamp across the rebuild: the codegen
    // `int_arith` / numeric disambiguation reads `lhs.ty()` / `rhs.ty()`, so
    // dropping the stamp here would silently flip an `acc * n` Int multiply
    // onto the raw `*`-operator path (which `AverInt` has no impl for).
    let ty = e.ty().cloned();
    let out = rewrite_children_inner(e, ctx);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

fn rewrite_children_inner(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    let line = e.line;
    match e.node {
        MirExpr::BinOp(mut b) => {
            let op = b.node.op;
            let int_arith = matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div);
            // Query whole-tree bareness on the ORIGINAL node before taking
            // the operands apart (it inspects `b.node` verbatim).
            let whole_raw =
                int_arith && renders_raw(&MirExpr::BinOp(clone_binop_shell(&b)), ctx.facts);
            let lhs = take_box(&mut b.node.lhs);
            let rhs = take_box(&mut b.node.rhs);
            // Whole-tree-bare arithmetic stays raw: keep operands raw.
            if whole_raw {
                b.node.lhs = std::boxed::Box::new(rewrite_raw(lhs, ctx));
                b.node.rhs = std::boxed::Box::new(rewrite_raw(rhs, ctx));
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            if int_arith
                && (super::bare_i64::type_is_int(operand_ty(&lhs))
                    || super::bare_i64::type_is_int(operand_ty(&rhs)))
            {
                // Boxed `AverInt` arithmetic: each operand must be an
                // `AverInt`. A raw operand is `Box`ed (`boxed_int_operand`).
                b.node.lhs = std::boxed::Box::new(rewrite_boxed(lhs, ctx));
                b.node.rhs = std::boxed::Box::new(rewrite_boxed(rhs, ctx));
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            // Comparison or non-Int op: a comparison between two raw
            // operands stays raw (codegen emits `i64 == i64`); otherwise
            // recurse as value positions.
            if matches!(
                op,
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
            ) && renders_raw(&lhs.node, ctx.facts)
                && renders_raw(&rhs.node, ctx.facts)
            {
                b.node.lhs = std::boxed::Box::new(rewrite_raw(lhs, ctx));
                b.node.rhs = std::boxed::Box::new(rewrite_raw(rhs, ctx));
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            b.node.lhs = std::boxed::Box::new(rewrite_value(lhs, ctx));
            b.node.rhs = std::boxed::Box::new(rewrite_value(rhs, ctx));
            Spanned::new(MirExpr::BinOp(b), line)
        }
        MirExpr::Neg(inner) => {
            // Negation over a bare operand renders boxed by codegen (the
            // bare `Neg` path bails to `AverInt::neg`), so the operand is a
            // boxed context.
            let r = rewrite_boxed(*inner, ctx);
            Spanned::new(MirExpr::Neg(std::boxed::Box::new(r)), line)
        }
        MirExpr::Call(mut c) => {
            let callee = c.node.callee.clone();
            let args = std::mem::take(&mut c.node.args);
            c.node.args = rewrite_call_args(args, &callee, ctx);
            Spanned::new(MirExpr::Call(c), line)
        }
        MirExpr::TailCall(mut tc) => {
            let target = tc.node.target;
            let callee = MirCallee::Fn(target);
            let args = std::mem::take(&mut tc.node.args);
            tc.node.args = rewrite_call_args(args, &callee, ctx);
            Spanned::new(MirExpr::TailCall(tc), line)
        }
        MirExpr::List(items) => Spanned::new(MirExpr::List(rewrite_boxed_each(items, ctx)), line),
        MirExpr::Tuple(items) => Spanned::new(MirExpr::Tuple(rewrite_boxed_each(items, ctx)), line),
        MirExpr::MapLiteral(pairs) => {
            let pairs = pairs
                .into_iter()
                .map(|(k, v)| (rewrite_boxed(k, ctx), rewrite_boxed(v, ctx)))
                .collect();
            Spanned::new(MirExpr::MapLiteral(pairs), line)
        }
        MirExpr::Construct(mut c) => {
            let args = std::mem::take(&mut c.node.args);
            c.node.args = rewrite_boxed_each(args, ctx);
            Spanned::new(MirExpr::Construct(c), line)
        }
        MirExpr::RecordCreate(mut r) => {
            for fld in &mut r.node.fields {
                let v = std::mem::replace(
                    &mut fld.value,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                fld.value = rewrite_boxed(v, ctx);
            }
            Spanned::new(MirExpr::RecordCreate(r), line)
        }
        MirExpr::RecordUpdate(mut u) => {
            let base = take_box(&mut u.node.base);
            u.node.base = std::boxed::Box::new(rewrite_value(base, ctx));
            for fld in &mut u.node.updates {
                let v = std::mem::replace(
                    &mut fld.value,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                fld.value = rewrite_boxed(v, ctx);
            }
            Spanned::new(MirExpr::RecordUpdate(u), line)
        }
        MirExpr::InterpolatedStr(parts) => {
            let parts = parts
                .into_iter()
                .map(|p| match p {
                    MirStrPart::Expr(ex) => MirStrPart::Expr(rewrite_value(ex, ctx)),
                    MirStrPart::Literal(s) => MirStrPart::Literal(s),
                })
                .collect();
            Spanned::new(MirExpr::InterpolatedStr(parts), line)
        }
        MirExpr::IndependentProduct(mut ip) => {
            let items = std::mem::take(&mut ip.node.items);
            ip.node.items = items.into_iter().map(|it| rewrite_value(it, ctx)).collect();
            Spanned::new(MirExpr::IndependentProduct(ip), line)
        }
        MirExpr::Project(mut p) => {
            let base = take_box(&mut p.node.base);
            p.node.base = std::boxed::Box::new(rewrite_value(base, ctx));
            Spanned::new(MirExpr::Project(p), line)
        }
        MirExpr::Try(inner) => {
            let r = rewrite_value(*inner, ctx);
            Spanned::new(MirExpr::Try(std::boxed::Box::new(r)), line)
        }
        MirExpr::Return(inner) => {
            let r = rewrite_value(*inner, ctx);
            Spanned::new(MirExpr::Return(std::boxed::Box::new(r)), line)
        }
        MirExpr::Let(mut l) => {
            let value = take_box(&mut l.node.value);
            l.node.value = std::boxed::Box::new(rewrite_let_value(value, l.node.binding, ctx));
            let body = take_box(&mut l.node.body);
            l.node.body = std::boxed::Box::new(rewrite_value(body, ctx));
            Spanned::new(MirExpr::Let(l), line)
        }
        MirExpr::Match(mut m) => {
            let subj = take_box(&mut m.node.subject);
            m.node.subject = std::boxed::Box::new(rewrite_value(subj, ctx));
            for arm in &mut m.node.arms {
                let body = std::mem::replace(
                    &mut arm.body,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                arm.body = rewrite_value(body, ctx);
            }
            Spanned::new(MirExpr::Match(m), line)
        }
        MirExpr::IfThenElse(mut ite) => {
            let cond = take_box(&mut ite.node.cond);
            ite.node.cond = std::boxed::Box::new(rewrite_value(cond, ctx));
            let then_b = take_box(&mut ite.node.then_branch);
            ite.node.then_branch = std::boxed::Box::new(rewrite_value(then_b, ctx));
            let else_b = take_box(&mut ite.node.else_branch);
            ite.node.else_branch = std::boxed::Box::new(rewrite_value(else_b, ctx));
            Spanned::new(MirExpr::IfThenElse(ite), line)
        }
        // Leaves + already-inserted boundary nodes: nothing to recurse.
        node @ (MirExpr::Literal(_)
        | MirExpr::Local(_)
        | MirExpr::FnValue(_)
        | MirExpr::Box(_)
        | MirExpr::Unbox(_)) => Spanned::new(node, line),
    }
}

/// Rewrite the args of a `Call(Fn)` / `TailCall`: each arg is rewritten in
/// the callee's i-th param context — raw if the callee param is bare
/// (`Unbox` a boxed arg, keep a raw one), boxed otherwise (`Box` a raw arg).
/// A non-`Fn` callee (builtin / intrinsic / fn-value) treats every Int arg
/// as a general-Int (boxed) context.
fn rewrite_call_args(
    args: Vec<Spanned<MirExpr>>,
    callee: &MirCallee,
    ctx: &RewriteCtx<'_>,
) -> Vec<Spanned<MirExpr>> {
    let callee_facts = match callee {
        MirCallee::Fn(t) => ctx.all.for_fn(*t),
        _ => None,
    };
    args.into_iter()
        .enumerate()
        .map(|(i, a)| {
            if callee_facts.is_some_and(|f| f.param_is_bare(i)) {
                rewrite_raw(a, ctx)
            } else {
                rewrite_boxed(a, ctx)
            }
        })
        .collect()
}

fn rewrite_boxed_each(items: Vec<Spanned<MirExpr>>, ctx: &RewriteCtx<'_>) -> Vec<Spanned<MirExpr>> {
    items.into_iter().map(|it| rewrite_boxed(it, ctx)).collect()
}

/// Take a boxed child out, leaving a Unit placeholder (immediately
/// overwritten by the caller). Avoids cloning a whole subtree.
fn take_box(slot: &mut std::boxed::Box<Spanned<MirExpr>>) -> Spanned<MirExpr> {
    let placeholder = std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
        Literal::Unit,
    ))));
    *std::mem::replace(slot, placeholder)
}

/// The operand's type stamp (for the int-arith disambiguation, same as
/// codegen reads `bop.lhs.ty()`).
fn operand_ty(e: &Spanned<MirExpr>) -> Option<&crate::ast::Type> {
    e.ty()
}

/// Build a throwaway `BinOp` shell (same op, cloned operands) used only to
/// query `expr_is_bare_i64` on the WHOLE tree without consuming the real
/// node. Cloning is cheap here (only fires for Int arithmetic nodes).
fn clone_binop_shell(b: &Spanned<crate::ir::mir::MirBinOp>) -> Spanned<crate::ir::mir::MirBinOp> {
    Spanned::new(
        crate::ir::mir::MirBinOp {
            op: b.node.op,
            lhs: b.node.lhs.clone(),
            rhs: b.node.rhs.clone(),
        },
        b.line,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::mir::{lower_program, optimize};
    use crate::source::parse_source;

    fn rewritten(src: &str) -> MirProgram {
        let mut items = parse_source(src).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        assert!(
            result
                .typecheck
                .as_ref()
                .is_none_or(|t| t.errors.is_empty()),
            "typecheck errors: {:?}",
            result.typecheck.map(|t| t.errors)
        );
        let mir_items = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        rewrite_for_rust(program)
    }

    fn fn_named<'a>(program: &'a MirProgram, name: &str) -> &'a MirFn {
        program
            .iter()
            .find(|(_, f)| f.name == name)
            .map(|(_, f)| f)
            .unwrap_or_else(|| panic!("fn `{name}` not in program"))
    }

    /// Walk the body counting `Box` / `Unbox` boundary nodes.
    fn count_boundaries(e: &MirExpr) -> (usize, usize) {
        let mut boxes = 0;
        let mut unboxes = 0;
        count_rec(e, &mut boxes, &mut unboxes);
        (boxes, unboxes)
    }

    fn count_rec(e: &MirExpr, boxes: &mut usize, unboxes: &mut usize) {
        match e {
            MirExpr::Box(_) => *boxes += 1,
            MirExpr::Unbox(_) => *unboxes += 1,
            _ => {}
        }
        super::super::bare_i64::tests_visit_children(e, &mut |c| count_rec(c, boxes, unboxes));
    }

    #[test]
    fn countdown_repr_is_explicit_and_no_spurious_boundary() {
        // The countdown counter is bare; its body is pure raw arithmetic
        // toward a bare return, so NO boundary node is needed.
        let src = r#"
module Countdown
    intent = "t"
    depends []

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(20000)
"#;
        let program = rewritten(src);
        let f = fn_named(&program, "countdown");
        assert!(f.repr.param_is_bare(0), "counter param tagged bare on MIR");
        assert!(f.repr.bare_return, "bare return tagged on MIR");
        let (boxes, unboxes) = count_boundaries(&f.body.node);
        assert_eq!(
            (boxes, unboxes),
            (0, 0),
            "an all-bare countdown needs no boundary node"
        );
    }

    #[test]
    fn factorial_boxes_bare_operand_at_mul() {
        // `acc * n`: `acc` boxed, `n` bare. The rewrite must `Box(n)` at the
        // boxed-multiply boundary (exactly one Box, no Unbox).
        let src = r#"
module Factorial
    intent = "t"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(10, 1)
"#;
        let program = rewritten(src);
        let f = fn_named(&program, "factorial");
        assert!(f.repr.param_is_bare(0), "n is bare on MIR");
        assert!(!f.repr.param_is_bare(1), "acc stays boxed on MIR");
        let (boxes, unboxes) = count_boundaries(&f.body.node);
        assert_eq!(boxes, 1, "exactly the `Box(n)` at `acc * n`");
        assert_eq!(unboxes, 0, "no Unbox needed");
    }

    #[test]
    fn q5_bare_return_into_boxed_return_boxes_at_crossing() {
        // Defect Q5: `g` has a bare return; `h` returns the call result as a
        // boxed Int. The rewrite must `Box` the bare-returning call in `h`.
        let src = r#"
module M
    intent = "t"
    depends []

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn h() -> Int
    g(2)

fn main() -> Int
    h()
"#;
        let program = rewritten(src);
        let g = fn_named(&program, "g");
        assert!(g.repr.bare_return, "g keeps its bare return");
        let h = fn_named(&program, "h");
        assert!(!h.repr.bare_return, "h's return stays boxed");
        let (boxes, _unboxes) = count_boundaries(&h.body.node);
        assert!(boxes >= 1, "h boxes the bare-returning call result");
    }
}
