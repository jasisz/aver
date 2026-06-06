//! Phase 6 wave 5 — const-fold pass on MIR.
//!
//! Evaluate `BinOp` / `Neg` over literal operands at compile
//! time. Numeric arithmetic via `checked_*` (overflow leaves the
//! node intact); comparisons (`Eq / Neq / Lt / Gt / Lte / Gte`)
//! over matching `Int` / `Float` / `Bool` / `Str` / `Unit`
//! literal pairs collapse to `Bool` literals; unary `Neg` on
//! `Int` / `Float` literals preserves IEEE-754 `-0.0`.
//!
//! ## 0.24 "Divide" — const-divisor `Int.div` / `Int.mod`
//!
//! Two composing structural folds make the `Int.div` / `Int.mod`
//! idiom zero-cost when the divisor is a compile-time constant:
//!
//! - **Fold A** rewrites `Int.div(a, k)` / `Int.mod(a, k)` with a
//!   literal `Int` divisor into the bare Euclidean intrinsic wrapped
//!   in a *known* `Result` constructor (or, for the partial cases,
//!   directly into `Result.Err("division by zero")`). The `k == -1`
//!   case for `Int.div` is deliberately left untouched — `i64::MIN /
//!   -1` overflows, so it stays the runtime `Result`.
//! - **Fold B** collapses a consumer (`Result.withDefault` /
//!   `Option.withDefault`, a `match`, or error-propagation `?`) applied to
//!   a statically-known constructor, dropping the dead branch.
//!
//! Post-order traversal makes them cascade in one pass:
//! `Result.withDefault(Int.div(a, 10), 0)` → A →
//! `Result.withDefault(Result.Ok(IntDivEuclid(a, 10)), 0)` → B →
//! `IntDivEuclid(a, 10)` — bare division, no `Result`, no dead branch.

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::hir::{BuiltinCtor, BuiltinIntrinsic};
use crate::types::Type;

use super::super::expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirCtor, MirExpr, MirLet, MirMatch, MirMatchArm,
    MirPattern,
};
use super::super::program::MirProgram;

/// Apply const-fold to every fn body in `program`. Returns the
/// (transformed) program by value so the caller can chain
/// further optimization passes.
pub fn const_fold(mut program: MirProgram) -> MirProgram {
    // The structural folds (A / B) resolve `MirCallee::Builtin(id)` to
    // its canonical name through `program.builtins`; clone the table out
    // up front so the fn-body walk can hold a shared `&[String]` while it
    // mutably borrows `program.fns`.
    let builtins = program.builtins.clone();
    for mir_fn in program.fns.values_mut() {
        fold_in_place(&mut mir_fn.body, &builtins);
    }
    program
}

/// Post-order fold: rewrite children first, then try the
/// current node — this lets a fold cascade up through nested
/// arithmetic (`(1 + 2) * 3` → `3 * 3` → `9`) and lets Fold A's
/// constructor feed Fold B's consumer in a single bottom-up pass.
fn fold_in_place(expr: &mut Spanned<MirExpr>, builtins: &[String]) {
    walk_children(&mut expr.node, builtins);
    if let Some(folded) = try_fold(&expr.node) {
        // Preserve the original `Spanned`'s line + type stamp;
        // only the node shape changes.
        let span = literal_span(folded, expr);
        expr.node = MirExpr::Literal(span);
    }
    // Structural rewrites (Fold A / Fold B) that replace the node with
    // a non-literal `MirExpr`. Runs after the literal fold so a literal
    // node is never reconsidered structurally. Takes the whole `Spanned`
    // so it can carry type stamps onto synthesised nodes — the wasm-gc
    // backend panics on an un-typed node.
    if let Some(replacement) = try_struct_fold(expr, builtins) {
        *expr = replacement;
    }
}

fn literal_span(lit: Literal, source: &Spanned<MirExpr>) -> Spanned<Literal> {
    let ty = std::sync::OnceLock::new();
    if let Some(t) = source.ty() {
        let _ = ty.set(t.clone());
    }
    Spanned {
        node: lit,
        line: source.line,
        ty,
    }
}

fn walk_children(node: &mut MirExpr, builtins: &[String]) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
        MirExpr::Neg(inner) => fold_in_place(inner, builtins),
        MirExpr::BinOp(spanned_bop) => {
            let bop: &mut MirBinOp = &mut spanned_bop.node;
            fold_in_place(&mut bop.lhs, builtins);
            fold_in_place(&mut bop.rhs, builtins);
        }
        MirExpr::Let(spanned_let) => {
            let let_node: &mut MirLet = &mut spanned_let.node;
            fold_in_place(&mut let_node.value, builtins);
            fold_in_place(&mut let_node.body, builtins);
        }
        MirExpr::Call(spanned_call) => {
            let call: &mut MirCall = &mut spanned_call.node;
            for arg in &mut call.args {
                fold_in_place(arg, builtins);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                fold_in_place(arg, builtins);
            }
        }
        MirExpr::Match(spanned_match) => {
            fold_in_place(&mut spanned_match.node.subject, builtins);
            for arm in &mut spanned_match.node.arms {
                fold_arm(arm, builtins);
            }
        }
        MirExpr::IfThenElse(spanned_ite) => {
            fold_in_place(&mut spanned_ite.node.cond, builtins);
            fold_in_place(&mut spanned_ite.node.then_branch, builtins);
            fold_in_place(&mut spanned_ite.node.else_branch, builtins);
        }
        MirExpr::Construct(spanned_ctor) => {
            let ctor: &mut MirConstruct = &mut spanned_ctor.node;
            for arg in &mut ctor.args {
                fold_in_place(arg, builtins);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                fold_in_place(&mut f.value, builtins);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            fold_in_place(&mut spanned_upd.node.base, builtins);
            for f in &mut spanned_upd.node.updates {
                fold_in_place(&mut f.value, builtins);
            }
        }
        MirExpr::Project(spanned_proj) => fold_in_place(&mut spanned_proj.node.base, builtins),
        MirExpr::Try(inner) => fold_in_place(inner, builtins),
        MirExpr::Return(inner) => fold_in_place(inner, builtins),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                fold_in_place(item, builtins);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                fold_in_place(k, builtins);
                fold_in_place(v, builtins);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::super::expr::MirStrPart::Expr(e) = part {
                    fold_in_place(e, builtins);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                fold_in_place(item, builtins);
            }
        }
    }
}

fn fold_arm(arm: &mut MirMatchArm, builtins: &[String]) {
    // Don't recurse into the pattern — patterns are structural
    // and don't contain `MirExpr` subtrees.
    let _ = &arm.pattern;
    let _: &MirPattern = &arm.pattern;
    fold_in_place(&mut arm.body, builtins);
}

fn try_fold(node: &MirExpr) -> Option<Literal> {
    match node {
        MirExpr::Neg(inner) => {
            let lit = literal_of(&inner.node)?;
            fold_neg(lit)
        }
        MirExpr::BinOp(spanned_bop) => {
            let bop = &spanned_bop.node;
            let lhs = literal_of(&bop.lhs.node)?;
            let rhs = literal_of(&bop.rhs.node)?;
            fold_binop(bop.op, lhs, rhs)
        }
        _ => None,
    }
}

fn literal_of(node: &MirExpr) -> Option<&Literal> {
    if let MirExpr::Literal(spanned) = node {
        Some(&spanned.node)
    } else {
        None
    }
}

// ── 0.24 "Divide" structural folds (A + B) ────────────────────────

/// Verbatim error string for both `Int.div(_, 0)` and `Int.mod(_, 0)` —
/// must match `src/types/int.rs`.
const DIV_BY_ZERO: &str = "division by zero";

/// Resolve the canonical name of a `MirCallee::Builtin` against the
/// program's interned builtin table. Returns `None` for any other
/// callee kind (or an out-of-range id).
fn builtin_callee_name<'a>(callee: &MirCallee, builtins: &'a [String]) -> Option<&'a str> {
    if let MirCallee::Builtin(id) = callee {
        builtins.get(id.0 as usize).map(String::as_str)
    } else {
        None
    }
}

/// Build a `Spanned<T>` carrying no line and no type stamp.
fn bare<T>(node: T) -> Spanned<T> {
    Spanned::bare(node)
}

/// Build a `Spanned<MirExpr>` stamped with `ty` (when known). The wasm-gc
/// backend panics on an un-typed node, so every synthesised node a fold
/// produces must carry the type its value will have at runtime.
fn typed(node: MirExpr, ty: Option<&Type>) -> Spanned<MirExpr> {
    let s = Spanned::bare(node);
    if let Some(t) = ty {
        s.set_ty(t.clone());
    }
    s
}

/// `IntDivEuclid` / `IntModEuclid` call over `(a, k)`, typed `Int`.
fn euclid_call(
    intrinsic: BuiltinIntrinsic,
    a: Spanned<MirExpr>,
    k: Spanned<MirExpr>,
) -> Spanned<MirExpr> {
    typed(
        MirExpr::Call(bare(MirCall {
            callee: MirCallee::Intrinsic(intrinsic),
            args: vec![a, k],
        })),
        Some(&Type::Int),
    )
}

/// `Builtin(ctor)(args…)` constructor node, stamped `result_ty` (the
/// `Result<Int,String>` the original `Int.div` / `Int.mod` call carried).
fn builtin_ctor(
    ctor: BuiltinCtor,
    args: Vec<Spanned<MirExpr>>,
    result_ty: Option<&Type>,
) -> Spanned<MirExpr> {
    typed(
        MirExpr::Construct(bare(MirConstruct {
            ctor: MirCtor::Builtin(ctor),
            args,
        })),
        result_ty,
    )
}

/// `Result.Err("division by zero")`, stamped `result_ty`.
fn div_by_zero_err(result_ty: Option<&Type>) -> Spanned<MirExpr> {
    builtin_ctor(
        BuiltinCtor::ResultErr,
        vec![typed(
            MirExpr::Literal(Spanned::bare(Literal::Str(DIV_BY_ZERO.to_string()))),
            Some(&Type::Str),
        )],
        result_ty,
    )
}

/// Structural rewrites that replace `*expr` with a *non-literal*
/// `Spanned<MirExpr>`. Returns `Some(replacement)` to swap the node,
/// `None` to leave it. Takes the whole `Spanned` so it can read the
/// original type stamp and move sub-expressions out without cloning.
fn try_struct_fold(expr: &mut Spanned<MirExpr>, builtins: &[String]) -> Option<Spanned<MirExpr>> {
    fold_a_partial_int_builtin(expr, builtins).or_else(|| fold_b_consume_ctor(expr, builtins))
}

/// Fold A — const-fold `Int.div` / `Int.mod` with a literal divisor.
///
/// ```text
/// Int.div, k ∉ {0, -1}  → Result.Ok(IntDivEuclid(a, k))
/// Int.div, k == 0       → Result.Err("division by zero")
/// Int.div, k == -1      → UNCHANGED (i64::MIN / -1 overflows)
/// Int.mod, k != 0       → Result.Ok(IntModEuclid(a, k))
/// Int.mod, k == 0       → Result.Err("division by zero")
/// ```
fn fold_a_partial_int_builtin(
    expr: &mut Spanned<MirExpr>,
    builtins: &[String],
) -> Option<Spanned<MirExpr>> {
    let MirExpr::Call(spanned_call) = &expr.node else {
        return None;
    };
    let call = &spanned_call.node;
    if call.args.len() != 2 {
        return None;
    }
    let name = builtin_callee_name(&call.callee, builtins)?;
    let is_div = match name {
        "Int.div" => true,
        "Int.mod" => false,
        _ => return None,
    };
    // The divisor must be a literal `Int`.
    let Literal::Int(k) = literal_of(&call.args[1].node)? else {
        return None;
    };
    let k = *k;
    // The wrapping `Result.Ok` / `Result.Err` keeps the original call's
    // `Result<Int,String>` type stamp.
    let result_ty = expr.ty().cloned();

    let MirExpr::Call(spanned_call) = &mut expr.node else {
        unreachable!("guarded above");
    };
    // Move the dividend + divisor out of the call (no clone).
    let mut drain = std::mem::take(&mut spanned_call.node.args).into_iter();
    let a = drain.next()?;
    let divisor = drain.next()?;

    Some(if is_div {
        match k {
            0 => div_by_zero_err(result_ty.as_ref()),
            // `i64::MIN / -1` overflows → leave the runtime `Result`.
            // (Restore the moved-out args first; the node stays a Call.)
            -1 => {
                spanned_call.node.args = vec![a, divisor];
                return None;
            }
            _ => builtin_ctor(
                BuiltinCtor::ResultOk,
                vec![euclid_call(BuiltinIntrinsic::IntDivEuclid, a, divisor)],
                result_ty.as_ref(),
            ),
        }
    } else {
        match k {
            0 => div_by_zero_err(result_ty.as_ref()),
            _ => builtin_ctor(
                BuiltinCtor::ResultOk,
                vec![euclid_call(BuiltinIntrinsic::IntModEuclid, a, divisor)],
                result_ty.as_ref(),
            ),
        }
    })
}

/// Fold B — collapse a consumer over a statically-known constructor.
///
/// - `Result.withDefault(Result.Ok(x), _)` → `x`
/// - `Result.withDefault(Result.Err(_), d)` → `d`
/// - `Option.withDefault(Option.Some(x), _)` → `x`
/// - `Option.withDefault(Option.None, d)` → `d`
/// - `match Construct(Builtin(C), ctor_args) { … }` → the first arm whose
///   pattern statically matches `C`, with each `ctor_args[i]` bound to
///   the arm's `i`-th field-binding via a wrapping `Let` (so every arg
///   is evaluated exactly once).
fn fold_b_consume_ctor(
    expr: &mut Spanned<MirExpr>,
    builtins: &[String],
) -> Option<Spanned<MirExpr>> {
    match &expr.node {
        MirExpr::Call(_) => fold_b_with_default(expr, builtins),
        MirExpr::Match(_) => fold_b_match_over_ctor(expr),
        MirExpr::Try(_) => fold_b_try_over_ctor(expr),
        _ => None,
    }
}

/// `Result.Ok(x)?` / `Result.Err(e)?` over a statically-known constructor —
/// the third Fold-B consumer, besides `withDefault` and `match`. It lets the
/// const-divisor `Int.div(a, k)?` / `Int.mod(a, k)?` idiom fold all the way to
/// a bare Euclidean division (or an unconditional error `Return`), the same
/// zero-round-trip the other two consumers already get.
///
/// - `Result.Ok(x)?`  → `x`              (statically Ok — `?` is just an unwrap)
/// - `Result.Err(e)?` → `Return(Err(e))` (statically Err — `?` always propagates)
fn fold_b_try_over_ctor(expr: &mut Spanned<MirExpr>) -> Option<Spanned<MirExpr>> {
    // The `Try` node's stamp is the unwrapped (Ok-payload) value type; carry
    // it onto the synthesised `Return` so the wasm-gc backend stays happy.
    let value_ty = expr.ty().cloned();

    let MirExpr::Try(inner) = &expr.node else {
        return None;
    };
    let MirExpr::Construct(ctor_node) = &inner.node else {
        return None;
    };
    let MirCtor::Builtin(ctor) = ctor_node.node.ctor else {
        return None;
    };
    // `?` is Result-only, so a well-typed subject is always a `Result` ctor;
    // an `Option` ctor here would be ill-typed — leave it untouched.
    match ctor {
        BuiltinCtor::ResultOk => {
            let MirExpr::Try(inner) = &mut expr.node else {
                unreachable!("guarded above");
            };
            let MirExpr::Construct(ctor_node) = &mut inner.node else {
                unreachable!("guarded above");
            };
            // `Ok(x)` → `x` (the payload already carries its own type stamp).
            std::mem::take(&mut ctor_node.node.args).into_iter().next()
        }
        BuiltinCtor::ResultErr => {
            let MirExpr::Try(inner) = &mut expr.node else {
                unreachable!("guarded above");
            };
            // Move the `Result.Err(e)` ctor out and wrap it in an
            // unconditional `Return` — the `?` propagates it every time.
            let err_ctor = std::mem::replace(
                inner.as_mut(),
                Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
            );
            Some(typed(
                MirExpr::Return(Box::new(err_ctor)),
                value_ty.as_ref(),
            ))
        }
        _ => None,
    }
}

/// `Result.withDefault` / `Option.withDefault` over a known constructor.
/// The surviving sub-expression (`x` or `d`) already carries its own type
/// stamp from lowering, so the replacement is correctly typed.
fn fold_b_with_default(
    expr: &mut Spanned<MirExpr>,
    builtins: &[String],
) -> Option<Spanned<MirExpr>> {
    let MirExpr::Call(spanned_call) = &expr.node else {
        return None;
    };
    if spanned_call.node.args.len() != 2 {
        return None;
    }
    let name = builtin_callee_name(&spanned_call.node.callee, builtins)?;
    let is_result = match name {
        "Result.withDefault" => true,
        "Option.withDefault" => false,
        _ => return None,
    };
    // The first arg must be a statically-known constructor of the
    // matching sum type.
    let MirExpr::Construct(inner) = &spanned_call.node.args[0].node else {
        return None;
    };
    let MirCtor::Builtin(ctor) = inner.node.ctor else {
        return None;
    };
    let take_payload = match (is_result, ctor) {
        (true, BuiltinCtor::ResultOk) | (false, BuiltinCtor::OptionSome) => true,
        (true, BuiltinCtor::ResultErr) | (false, BuiltinCtor::OptionNone) => false,
        // A `Result` consumer over an `Option` ctor (or vice-versa)
        // can't happen on well-typed input; leave it untouched.
        _ => return None,
    };

    let MirExpr::Call(spanned_call) = &mut expr.node else {
        unreachable!("guarded above");
    };
    let mut args = std::mem::take(&mut spanned_call.node.args).into_iter();
    let ctor_arg = args.next()?; // the Construct
    let default = args.next()?; // the default value

    if take_payload {
        // `Ok(x)` / `Some(x)` → the constructor's payload (already typed).
        let MirExpr::Construct(inner) = ctor_arg.node else {
            return None;
        };
        inner.node.args.into_iter().next()
    } else {
        // `Err(_)` / `None` → the default (already typed). The `Err`
        // payload is dropped; it's pure (a literal / already-evaluated
        // value), so no eval is lost.
        Some(default)
    }
}

/// `match Construct(Builtin(C), ctor_args) { arms }` over a statically
/// known constructor: pick the first arm that matches `C` and rewrite to
/// its body, threading each `ctor_arg` into the arm's field bindings.
///
/// All the recognition (subject is a known ctor, an arm matches) runs on
/// the *borrowed* node first; the node is only moved out once a definite
/// rewrite is committed, so there is no reconstruction / bail-restore.
fn fold_b_match_over_ctor(expr: &mut Spanned<MirExpr>) -> Option<Spanned<MirExpr>> {
    // The whole `match` value-type (the arm bodies' common type) — used to
    // stamp the wrapping `Let`s so every synthesised node is typed.
    let match_ty = expr.ty().cloned();

    let MirExpr::Match(spanned_match) = &expr.node else {
        return None;
    };
    // Subject must be a *literal* constructor (statically known).
    let MirExpr::Construct(subj) = &spanned_match.node.subject.node else {
        return None;
    };
    let MirCtor::Builtin(subj_ctor) = subj.node.ctor else {
        return None;
    };
    let subj_arity = subj.node.args.len();

    // Find the first arm that statically matches this constructor. A
    // matching `Ctor` arm must agree on field count — well-typed input
    // guarantees this, so a mismatch means "no match" (skip the arm)
    // rather than a fold we'd have to undo.
    let arm_idx = spanned_match
        .node
        .arms
        .iter()
        .position(|arm| arm_matches_ctor(&arm.pattern, subj_ctor, subj_arity))?;

    // The subject's `Result` / `Option` type — used to re-stamp the ctor
    // if the chosen arm binds the whole subject (`Bind`).
    let subj_ty = spanned_match.node.subject.ty().cloned();

    // Committed: move the match out and consume the subject + chosen arm.
    let MirExpr::Match(spanned_match) = std::mem::replace(
        &mut expr.node,
        MirExpr::Literal(Spanned::bare(Literal::Unit)),
    ) else {
        unreachable!("guarded by the let-else above");
    };
    let MirMatch { subject, mut arms } = spanned_match.node;
    let MirExpr::Construct(subj) = subject.node else {
        unreachable!("guarded by the Construct check above");
    };
    let ctor_args = subj.node.args;
    let arm = arms.swap_remove(arm_idx);

    Some(match arm.pattern {
        // `Ctor(Builtin(C), bindings)` — bind each field, then the body.
        // Arity already checked by `arm_matches_ctor`.
        MirPattern::Ctor {
            bindings,
            binding_names,
            ..
        } => wrap_in_lets(
            &bindings,
            &binding_names,
            ctor_args,
            arm.body,
            match_ty.as_ref(),
        ),
        // `Bind(local, name)` — bind the whole subject value (the ctor).
        MirPattern::Bind(local, name) => typed(
            MirExpr::Let(bare(MirLet {
                binding: local,
                binding_name: name,
                value: Box::new(builtin_ctor(subj_ctor, ctor_args, subj_ty.as_ref())),
                body: Box::new(arm.body),
            })),
            match_ty.as_ref(),
        ),
        // `Wildcard` — body unchanged (subject is pure / discarded).
        MirPattern::Wildcard => arm.body,
        // `arm_matches_ctor` only returns `true` for the three arms
        // above, so this is unreachable; return the body to stay total.
        _ => arm.body,
    })
}

/// Does `pattern` statically match constructor `ctor` with `arity`
/// fields? Catch-alls (`Wildcard` / `Bind`) match any subject; a
/// `Ctor` pattern matches only the *same* built-in variant with the
/// same field count.
fn arm_matches_ctor(pattern: &MirPattern, ctor: BuiltinCtor, arity: usize) -> bool {
    match pattern {
        MirPattern::Wildcard | MirPattern::Bind(_, _) => true,
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(pc),
            bindings,
            ..
        } => *pc == ctor && bindings.len() == arity,
        _ => false,
    }
}

/// Wrap `body` in one `Let` per constructor field, binding `bindings[i]`
/// to `ctor_args[i]`. Evaluation order matches source: the outermost
/// `Let` binds the first field, so args evaluate left-to-right, once each.
///
/// Each `Let` carries the pattern field's source binder name
/// (`binding_names[i]`) — this is load-bearing: the wasm-gc / Rust
/// backends resolve a `Let`'s slot by *name* (`self_local_slot`), and an
/// empty name routes them down the synthetic-discard path that drops the
/// value. The VM keys off `binding` (the `LocalId`). Each `Let` also
/// carries the body's value type (`match_ty`) so wasm-gc has a stamp on
/// every node.
fn wrap_in_lets(
    bindings: &[crate::ir::mir::LocalId],
    binding_names: &[String],
    ctor_args: Vec<Spanned<MirExpr>>,
    body: Spanned<MirExpr>,
    match_ty: Option<&Type>,
) -> Spanned<MirExpr> {
    let mut acc = body;
    // Build inside-out: fold from the last field to the first so the
    // first field ends up as the outermost (first-evaluated) `Let`.
    let n = ctor_args.len();
    for (i, (slot, arg)) in bindings.iter().zip(ctor_args).enumerate().rev() {
        // `binding_names` is parallel to `bindings`; fall back to empty
        // (synthetic) only if it's somehow short — never for well-typed
        // input. `_ = n` keeps the index in range for the zip.
        let name = binding_names.get(i).cloned().unwrap_or_default();
        debug_assert!(i < n);
        acc = typed(
            MirExpr::Let(bare(MirLet {
                binding: *slot,
                binding_name: name,
                value: Box::new(arg),
                body: Box::new(acc),
            })),
            match_ty,
        );
    }
    acc
}

fn fold_neg(lit: &Literal) -> Option<Literal> {
    match lit {
        Literal::Int(i) => i.checked_neg().map(Literal::Int),
        Literal::Float(f) => Some(Literal::Float(-f)),
        _ => None,
    }
}

fn fold_binop(op: BinOp, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    // Numeric arithmetic: `Int` uses `checked_*` so overflow
    // leaves the node intact and the VM's runtime arithmetic
    // error path stays the source of truth.
    match (op, lhs, rhs) {
        // ── Int arithmetic ────────────────────────────────
        (BinOp::Add, Literal::Int(a), Literal::Int(b)) => a.checked_add(*b).map(Literal::Int),
        (BinOp::Sub, Literal::Int(a), Literal::Int(b)) => a.checked_sub(*b).map(Literal::Int),
        (BinOp::Mul, Literal::Int(a), Literal::Int(b)) => a.checked_mul(*b).map(Literal::Int),
        (BinOp::Div, Literal::Int(a), Literal::Int(b)) => a.checked_div(*b).map(Literal::Int),
        // ── Float arithmetic ──────────────────────────────
        (BinOp::Add, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a + b)),
        (BinOp::Sub, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a - b)),
        (BinOp::Mul, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a * b)),
        (BinOp::Div, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a / b)),
        // ── Comparisons ──────────────────────────────────
        (BinOp::Eq, a, b) => literal_eq(a, b).map(Literal::Bool),
        (BinOp::Neq, a, b) => literal_eq(a, b).map(|e| Literal::Bool(!e)),
        (BinOp::Lt, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a < b)),
        (BinOp::Lt, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a < b)),
        (BinOp::Gt, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a > b)),
        (BinOp::Gt, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a > b)),
        (BinOp::Lte, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a <= b)),
        (BinOp::Lte, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a <= b)),
        (BinOp::Gte, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a >= b)),
        (BinOp::Gte, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a >= b)),
        _ => None,
    }
}

/// Used by both `const_fold` (for `Eq` / `Neq` reductions over
/// matching literal kinds) and `algebraic` (which currently
/// re-implements its own structural equality check locally).
/// Exported `pub(super)` so the sibling pass can reuse it
/// without duplicating the match arms.
pub(super) fn literal_eq(a: &Literal, b: &Literal) -> Option<bool> {
    match (a, b) {
        (Literal::Int(x), Literal::Int(y)) => Some(x == y),
        (Literal::Float(x), Literal::Float(y)) => Some(x == y),
        (Literal::Bool(x), Literal::Bool(y)) => Some(x == y),
        (Literal::Str(x), Literal::Str(y)) => Some(x == y),
        (Literal::Unit, Literal::Unit) => Some(true),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_helpers::{body_of, one_fn_program, span};
    use super::*;

    #[test]
    fn folds_int_add() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(3)))
        );
    }

    #[test]
    fn folds_nested_arithmetic() {
        // `(1 + 2) * 3` → `3 * 3` → `9`.
        let inner = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let outer = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(inner)),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(3))))),
        }));
        let folded = const_fold(one_fn_program(outer));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(9)))
        );
    }

    #[test]
    fn folds_neg_on_int_literal() {
        let body = MirExpr::Neg(Box::new(span(MirExpr::Literal(span(Literal::Int(7))))));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(-7)))
        );
    }

    #[test]
    fn folds_eq_to_bool() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Bool(true)))
        );
    }

    #[test]
    fn folds_lt_on_floats() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Lt,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Float(1.5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Float(2.0))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Bool(true)))
        );
    }

    #[test]
    fn leaves_int_overflow_unfolded() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(i64::MAX))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn leaves_div_by_zero_unfolded() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Div,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn leaves_non_literal_operands_untouched() {
        use super::super::super::expr::MirLocal;
        use super::super::super::program::LocalId;
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn folds_inside_let_value_and_body() {
        use super::super::super::expr::MirLet;
        use super::super::super::program::LocalId;
        let value = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let body_expr = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(3))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(4))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(value)),
            body: Box::new(span(body_expr)),
        }));
        let folded = const_fold(one_fn_program(body));
        let MirExpr::Let(let_node) = body_of(&folded) else {
            panic!("expected Let at root");
        };
        assert!(matches!(
            &let_node.node.value.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(3))
        ));
        assert!(matches!(
            &let_node.node.body.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(7))
        ));
    }

    // ── 0.24 "Divide" Fold A / Fold B ─────────────────────────────

    use super::super::super::expr::MirLocal;
    use super::super::super::program::LocalId;
    use crate::ir::BuiltinId;

    fn int_lit(n: i64) -> Spanned<MirExpr> {
        span(MirExpr::Literal(span(Literal::Int(n))))
    }

    /// Build a one-fn program whose body is `body`, with `builtin_name`
    /// interned at `BuiltinId(0)` so a `MirCallee::Builtin(BuiltinId(0))`
    /// inside `body` resolves to it during `const_fold`.
    fn program_with_builtin(builtin_name: &str, body: MirExpr) -> MirProgram {
        let mut p = one_fn_program(body);
        let id = p.intern_builtin(builtin_name);
        assert_eq!(id, BuiltinId(0), "test assumes the first interned id");
        p
    }

    fn builtin_call(args: Vec<Spanned<MirExpr>>) -> MirExpr {
        MirExpr::Call(span(MirCall {
            callee: MirCallee::Builtin(BuiltinId(0)),
            args,
        }))
    }

    // ---- Fold A ----

    #[test]
    fn fold_a_div_const_to_euclid_ok() {
        // `Int.div(a, 10)` → `Result.Ok(IntDivEuclid(a, 10))`.
        let body = builtin_call(vec![
            span(MirExpr::Local(span(MirLocal::at(LocalId(0))))),
            int_lit(10),
        ]);
        let folded = const_fold(program_with_builtin("Int.div", body));
        let MirExpr::Construct(c) = body_of(&folded) else {
            panic!("expected Result.Ok Construct, got {:?}", body_of(&folded));
        };
        assert!(matches!(
            c.node.ctor,
            MirCtor::Builtin(BuiltinCtor::ResultOk)
        ));
        let MirExpr::Call(inner) = &c.node.args[0].node else {
            panic!("expected IntDivEuclid call");
        };
        assert!(matches!(
            inner.node.callee,
            MirCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid)
        ));
    }

    #[test]
    fn fold_a_div_by_zero_to_err() {
        let body = builtin_call(vec![int_lit(99), int_lit(0)]);
        let folded = const_fold(program_with_builtin("Int.div", body));
        let MirExpr::Construct(c) = body_of(&folded) else {
            panic!("expected Result.Err Construct");
        };
        assert!(matches!(
            c.node.ctor,
            MirCtor::Builtin(BuiltinCtor::ResultErr)
        ));
        let MirExpr::Literal(s) = &c.node.args[0].node else {
            panic!("expected Err payload literal");
        };
        assert!(matches!(&s.node, Literal::Str(m) if m == "division by zero"));
    }

    #[test]
    fn fold_a_div_by_minus_one_left_unchanged() {
        // `i64::MIN / -1` overflows → the fold must NOT touch the node;
        // it stays the runtime `Int.div` Builtin call.
        let body = builtin_call(vec![int_lit(123), int_lit(-1)]);
        let folded = const_fold(program_with_builtin("Int.div", body));
        let MirExpr::Call(c) = body_of(&folded) else {
            panic!("expected the Int.div Builtin call to survive");
        };
        assert!(matches!(c.node.callee, MirCallee::Builtin(BuiltinId(0))));
        assert_eq!(c.node.args.len(), 2, "args must be restored intact");
    }

    #[test]
    fn fold_a_mod_const_to_euclid_ok() {
        // `Int.mod(a, 7)` → `Result.Ok(IntModEuclid(a, 7))` (any non-zero).
        let body = builtin_call(vec![
            span(MirExpr::Local(span(MirLocal::at(LocalId(0))))),
            int_lit(7),
        ]);
        let folded = const_fold(program_with_builtin("Int.mod", body));
        let MirExpr::Construct(c) = body_of(&folded) else {
            panic!("expected Result.Ok Construct");
        };
        assert!(matches!(
            c.node.ctor,
            MirCtor::Builtin(BuiltinCtor::ResultOk)
        ));
        let MirExpr::Call(inner) = &c.node.args[0].node else {
            panic!("expected IntModEuclid call");
        };
        assert!(matches!(
            inner.node.callee,
            MirCallee::Intrinsic(BuiltinIntrinsic::IntModEuclid)
        ));
    }

    #[test]
    fn fold_a_mod_by_zero_to_err() {
        let body = builtin_call(vec![int_lit(5), int_lit(0)]);
        let folded = const_fold(program_with_builtin("Int.mod", body));
        let MirExpr::Construct(c) = body_of(&folded) else {
            panic!("expected Result.Err Construct");
        };
        assert!(matches!(
            c.node.ctor,
            MirCtor::Builtin(BuiltinCtor::ResultErr)
        ));
    }

    #[test]
    fn fold_a_div_non_literal_divisor_unchanged() {
        // Divisor is a Local, not a literal → no fold.
        let body = builtin_call(vec![
            int_lit(40),
            span(MirExpr::Local(span(MirLocal::at(LocalId(1))))),
        ]);
        let folded = const_fold(program_with_builtin("Int.div", body));
        assert!(matches!(body_of(&folded), MirExpr::Call(_)));
    }

    // ---- Fold B: withDefault ----

    /// `Result.Ok(payload)` / `Result.Err(payload)` etc. constructor.
    fn ctor(c: BuiltinCtor, args: Vec<Spanned<MirExpr>>) -> Spanned<MirExpr> {
        span(MirExpr::Construct(span(MirConstruct {
            ctor: MirCtor::Builtin(c),
            args,
        })))
    }

    #[test]
    fn fold_b_result_withdefault_ok_takes_payload() {
        // `Result.withDefault(Result.Ok(7), 0)` → `7`.
        let body = builtin_call(vec![
            ctor(BuiltinCtor::ResultOk, vec![int_lit(7)]),
            int_lit(0),
        ]);
        let folded = const_fold(program_with_builtin("Result.withDefault", body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(7)))
        );
    }

    #[test]
    fn fold_b_result_withdefault_err_takes_default() {
        // `Result.withDefault(Result.Err("x"), 0)` → `0`.
        let body = builtin_call(vec![
            ctor(
                BuiltinCtor::ResultErr,
                vec![span(MirExpr::Literal(span(Literal::Str("x".into()))))],
            ),
            int_lit(0),
        ]);
        let folded = const_fold(program_with_builtin("Result.withDefault", body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(0)))
        );
    }

    #[test]
    fn fold_b_option_withdefault_some_takes_payload() {
        let body = builtin_call(vec![
            ctor(BuiltinCtor::OptionSome, vec![int_lit(9)]),
            int_lit(0),
        ]);
        let folded = const_fold(program_with_builtin("Option.withDefault", body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(9)))
        );
    }

    #[test]
    fn fold_b_option_withdefault_none_takes_default() {
        let body = builtin_call(vec![ctor(BuiltinCtor::OptionNone, vec![]), int_lit(42)]);
        let folded = const_fold(program_with_builtin("Option.withDefault", body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(42)))
        );
    }

    // ---- Fold B: ?-propagation (error-prop operator) ----

    #[test]
    fn fold_b_try_over_ok_unwraps() {
        // `Result.Ok(7)?` → `7` (statically Ok — the `?` is just an unwrap).
        let body = MirExpr::Try(Box::new(ctor(BuiltinCtor::ResultOk, vec![int_lit(7)])));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(7)))
        );
    }

    #[test]
    fn fold_b_try_over_err_returns() {
        // `Result.Err("x")?` → `return Result.Err("x")` (statically Err — the
        // `?` always propagates, so the enclosing fn returns the error here).
        let err = ctor(
            BuiltinCtor::ResultErr,
            vec![span(MirExpr::Literal(span(Literal::Str("x".into()))))],
        );
        let folded = const_fold(one_fn_program(MirExpr::Try(Box::new(err))));
        let MirExpr::Return(inner) = body_of(&folded) else {
            panic!(
                "expected `return Result.Err(...)`, got {:?}",
                body_of(&folded)
            );
        };
        let MirExpr::Construct(c) = &inner.node else {
            panic!("expected Result.Err under the return");
        };
        assert!(matches!(
            c.node.ctor,
            MirCtor::Builtin(BuiltinCtor::ResultErr)
        ));
    }

    #[test]
    fn fold_a_then_b_try_div_const_to_bare_euclid() {
        // `Int.div(a, 10)?` → A → `Result.Ok(IntDivEuclid(a, 10))?`
        // → B → `IntDivEuclid(a, 10)` — bare division, no `Result`, no `?`.
        let div = builtin_call(vec![
            span(MirExpr::Local(span(MirLocal::at(LocalId(0))))),
            int_lit(10),
        ]);
        let folded = const_fold(program_with_builtin(
            "Int.div",
            MirExpr::Try(Box::new(span(div))),
        ));
        let MirExpr::Call(c) = body_of(&folded) else {
            panic!(
                "expected bare IntDivEuclid call, got {:?}",
                body_of(&folded)
            );
        };
        assert!(matches!(
            c.node.callee,
            MirCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid)
        ));
    }

    // ---- Fold B: match over a known ctor ----

    #[test]
    fn fold_b_match_ok_binds_payload_in_let() {
        // `match Result.Ok(x) { Ok(q) -> q ; Err(_) -> -1 }`
        // → `let q = x; q`.
        let subject = ctor(
            BuiltinCtor::ResultOk,
            vec![span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))],
        );
        let ok_arm = MirMatchArm {
            pattern: MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
                bindings: vec![LocalId(1)],
                binding_names: vec!["q".to_string()],
            },
            body: span(MirExpr::Local(span(MirLocal::at(LocalId(1))))),
        };
        let err_arm = MirMatchArm {
            pattern: MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultErr),
                bindings: vec![LocalId(2)],
                binding_names: vec!["_".to_string()],
            },
            body: int_lit(-1),
        };
        let body = MirExpr::Match(span(MirMatch {
            subject: Box::new(subject),
            arms: vec![ok_arm, err_arm],
        }));
        let folded = const_fold(one_fn_program(body));
        let MirExpr::Let(l) = body_of(&folded) else {
            panic!("expected `let q = x; q`, got {:?}", body_of(&folded));
        };
        assert_eq!(l.node.binding, LocalId(1));
        assert_eq!(l.node.binding_name, "q");
        // value is the bound payload local (slot 0), body reads slot 1.
        assert!(matches!(
            &l.node.value.node,
            MirExpr::Local(s) if s.node.slot == LocalId(0)
        ));
        assert!(matches!(
            &l.node.body.node,
            MirExpr::Local(s) if s.node.slot == LocalId(1)
        ));
    }

    #[test]
    fn fold_b_match_none_picks_nullary_arm_no_let() {
        // `match Option.None { Some(v) -> v ; None -> 5 }` → `5`.
        let subject = ctor(BuiltinCtor::OptionNone, vec![]);
        let some_arm = MirMatchArm {
            pattern: MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                bindings: vec![LocalId(1)],
                binding_names: vec!["v".to_string()],
            },
            body: span(MirExpr::Local(span(MirLocal::at(LocalId(1))))),
        };
        let none_arm = MirMatchArm {
            pattern: MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                bindings: vec![],
                binding_names: vec![],
            },
            body: int_lit(5),
        };
        let body = MirExpr::Match(span(MirMatch {
            subject: Box::new(subject),
            arms: vec![some_arm, none_arm],
        }));
        let folded = const_fold(one_fn_program(body));
        // No fields → no `Let`; the None arm's body folds in directly.
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(5)))
        );
    }

    #[test]
    fn fold_b_match_non_literal_subject_unchanged() {
        // Subject is a Local, not a Construct → no fold.
        let body = MirExpr::Match(span(MirMatch {
            subject: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
            arms: vec![MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: int_lit(1),
            }],
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::Match(_)));
    }

    #[test]
    fn fold_a_then_b_compose_to_bare_euclid() {
        // The end-to-end win:
        // `Result.withDefault(Int.div(a, 10), 0)` → bare `IntDivEuclid(a, 10)`.
        let mut p = one_fn_program(MirExpr::Literal(span(Literal::Unit)));
        let div_id = p.intern_builtin("Int.div");
        let wd_id = p.intern_builtin("Result.withDefault");
        let inner_div = MirExpr::Call(span(MirCall {
            callee: MirCallee::Builtin(div_id),
            args: vec![
                span(MirExpr::Local(span(MirLocal::at(LocalId(0))))),
                int_lit(10),
            ],
        }));
        let body = MirExpr::Call(span(MirCall {
            callee: MirCallee::Builtin(wd_id),
            args: vec![span(inner_div), int_lit(0)],
        }));
        p.fns.get_mut(&crate::ir::FnId(0)).unwrap().body = span(body);
        let folded = const_fold(p);
        let MirExpr::Call(c) = body_of(&folded) else {
            panic!(
                "expected bare IntDivEuclid call, got {:?}",
                body_of(&folded)
            );
        };
        assert!(
            matches!(
                c.node.callee,
                MirCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid)
            ),
            "withDefault(Int.div(a, 10), 0) must fold to a bare Euclidean intrinsic"
        );
    }
}
