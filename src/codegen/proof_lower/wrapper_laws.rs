//! Int-wrapper algebraic-law detectors (commutative / associative /
//! identity / anti-commutative / unary-equals-binary) and their
//! private AST matchers.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Stage 8 of #232: detect `wrapper(g) == other(g)` where `wrapper`
/// is a `ModulePattern::WrapperOverRecursion` and the inner fn has a
/// monoidal-accumulator shape we know how to emit Dafny support
/// theorems for. Returns the typed strategy carrying enough payload
/// for the backend to emit the aux acc-decomposition lemma without
/// re-walking the AST.
pub(super) fn detect_wrapper_over_recursion(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::analysis::shape::ModulePattern;
    use crate::ast::{BinOp, Expr, Stmt};

    // Pre-pipeline AST has `Expr::Ident(n)`; post-pipeline the
    // resolver rewrites local/param idents to `Expr::Resolved { name }`.
    // Both forms identify the same surface name.
    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    let shape = inputs.program_shape?;

    // Only handle single-given laws — the template is parameterized
    // by one universal `xs`.
    if law.givens.len() != 1 {
        return None;
    }
    let given_name = &law.givens[0].name;

    // Read the loop fn + its fold roles from the shared `AccumulatorFold`
    // pattern (recognized once by `analysis::shape`) — no bespoke inner-body
    // re-walk for the OP. Two drivers: the original `List<_>` fold (inline
    // `acc <op> h` step) and the Peano-`Nat` countdown (`factTR`, a named
    // `combine(n, acc)` step). The monoidal strategy needs an additive /
    // multiplicative step and an identity finish (the base arm returns the
    // accumulator).
    let (inner_fn, fold_driver_type, value_first, step_op, step_combine_fn) =
        shape.patterns.iter().find_map(|p| match p {
            ModulePattern::AccumulatorFold {
                wrapper_fn,
                loop_fn,
                step_op,
                step_fn,
                finish_fn: None,
                driver_type,
                step_value_first,
                ..
            } if wrapper_fn == fn_name => Some((
                loop_fn.clone(),
                driver_type.clone(),
                *step_value_first,
                *step_op,
                step_fn.clone(),
            )),
            _ => None,
        })?;
    let wrapper_fn = fn_name.to_string();

    // Resolve the combine op + typed driver from the fold roles. `List` reads
    // the inline binop; `PeanoNat` classifies the named combine fn by its base
    // arm (`Z -> y` is `+`, `Z -> Z` is `*`).
    let (combine_op, driver, combine_fn) = match fold_driver_type {
        None => {
            let op = step_op.filter(|op| matches!(op, BinOp::Add | BinOp::Mul))?;
            (op, crate::ir::WrapperDriver::List, None)
        }
        Some(type_name) => {
            let combine = step_combine_fn?;
            let combine_fd = inputs.find_fn_def_by_call_name(&combine)?;
            let op = nat_monoid_op_of(combine_fd)?;
            (
                op,
                crate::ir::WrapperDriver::PeanoNat {
                    type_name,
                    value_first,
                },
                Some(combine),
            )
        }
    };

    // Law shape: `wrapper(g) == other(g)` (either side).
    let extract = |expr: &Spanned<crate::ast::Expr>| -> Option<String> {
        let Expr::FnCall(callee, args) = &expr.node else {
            return None;
        };
        let name = ident_name(callee)?;
        if args.len() != 1 {
            return None;
        }
        if ident_name(&args[0])? != given_name {
            return None;
        }
        Some(name.to_string())
    };
    let lhs_call = extract(&law.lhs);
    let rhs_call = extract(&law.rhs);
    let other_fn = match (lhs_call, rhs_call) {
        (Some(l), Some(r)) if l == wrapper_fn && r != wrapper_fn => r,
        (Some(l), Some(r)) if r == wrapper_fn && l != wrapper_fn => l,
        _ => return None,
    };

    // Wrapper body must be `inner(<given>, neutral)` where neutral is
    // 0 for Add and 1 for Mul. Verify against the fn def to be sure.
    let wrapper_fd = inputs.find_fn_def_by_call_name(&wrapper_fn)?;
    let wstmts = wrapper_fd.body.stmts();
    if wstmts.len() != 1 {
        return None;
    }
    let Stmt::Expr(wbody) = &wstmts[0] else {
        return None;
    };
    let (wcallee_name, wargs) = match &wbody.node {
        Expr::FnCall(c, a) => (ident_name(c)?, a.clone()),
        Expr::TailCall(td) => (td.target.as_str(), td.args.clone()),
        _ => return None,
    };
    if wcallee_name != inner_fn {
        return None;
    }
    if wargs.len() != 2 {
        return None;
    }
    let neutral_matches = match &driver {
        crate::ir::WrapperDriver::List => {
            let expected_neutral: i64 = match combine_op {
                BinOp::Add | BinOp::Sub => 0,
                BinOp::Mul => 1,
                _ => return None,
            };
            matches!(
                &wargs[1].node,
                Expr::Literal(crate::ast::Literal::Int(n)) if *n == expected_neutral
            )
        }
        crate::ir::WrapperDriver::PeanoNat { .. } => match combine_op {
            // multiplicative identity `S(Z)`, additive identity `Z`.
            BinOp::Mul => is_peano_one(&wargs[1]),
            BinOp::Add => is_peano_zero(&wargs[1]),
            _ => return None,
        },
    };
    if !neutral_matches {
        return None;
    }

    Some(crate::ir::ProofStrategy::WrapperOverRecursion {
        wrapper_fn,
        inner_fn,
        other_fn,
        combine_op,
        driver,
        combine_fn,
    })
}

/// TIP prop_35 (`exp x y = qexp x y one`): a tail-recursive fold with a FIXED
/// base parameter. Detect the law `spec(x, y) == loop(x, y, neutral)` where the
/// extra leading param `x` is threaded unchanged through both fns, the driver
/// `y` is the structurally-shrinking Peano `Nat`, and the combine multiplies the
/// accumulator by the fixed `x` (not by the matched subject). Distinct from
/// `WrapperOverRecursion`: TWO givens, the wrapper call written inline (no
/// separate non-recursive wrapper fn), a 3-arg loop, and combine-by-fixed-param.
///
/// `spec` must be `match y { Z -> neutral; S(n) -> combine(x, spec(x, n)) }`
/// and `loop` `match y { Z -> z; S(n) -> loop(x, n, combine(x, z)) }`, with the
/// inline neutral matching the combine op (`S(Z)` for `Mul`, `Z` for `Add`).
pub(super) fn detect_tailrec_fixed_base_fold(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::ast::{Expr, Pattern, Stmt};

    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }
    // A 0-arg call (`one()` → `FnCall(one, [])`) or bare ctor value the law
    // wrote inline as the loop's neutral accumulator argument.
    let is_neutral_one = |e: &Spanned<Expr>| -> bool {
        if is_peano_one(e) {
            return true;
        }
        // `one()` resolves to a nullary fn whose body is `S(Z)`.
        if let Expr::FnCall(c, a) = &e.node
            && a.is_empty()
            && let Some(name) = expr_to_dotted_name(&c.node)
            && let Some(fd) = inputs.find_fn_def_by_call_name(&name)
            && let Some(b) = body_terminal_expr(fd.body.as_ref())
        {
            return is_peano_one(b);
        }
        false
    };
    let is_neutral_zero = |e: &Spanned<Expr>| -> bool {
        if is_peano_zero(e) {
            return true;
        }
        if let Expr::FnCall(c, a) = &e.node
            && a.is_empty()
            && let Some(name) = expr_to_dotted_name(&c.node)
            && let Some(fd) = inputs.find_fn_def_by_call_name(&name)
            && let Some(b) = body_terminal_expr(fd.body.as_ref())
        {
            return is_peano_zero(b);
        }
        false
    };

    if law.givens.len() != 2 || law.when.is_some() {
        return None;
    }
    let g0 = law.givens[0].name.as_str();
    let g1 = law.givens[1].name.as_str();
    let type_name = law.givens[1].type_name.clone();

    // Law sides: `spec(x, y)` (2 args, both givens) and `loop(x, y, neutral)`
    // (3 args, first two givens, third the inline neutral).
    let as_spec = |e: &Spanned<Expr>| -> Option<String> {
        let Expr::FnCall(c, a) = &e.node else {
            return None;
        };
        if a.len() != 2 || ident_name(&a[0])? != g0 || ident_name(&a[1])? != g1 {
            return None;
        }
        expr_to_dotted_name(&c.node)
    };
    fn as_loop<'e>(
        e: &'e Spanned<Expr>,
        g0: &str,
        g1: &str,
    ) -> Option<(String, &'e Spanned<Expr>)> {
        let id = |x: &Spanned<Expr>| -> Option<String> {
            match &x.node {
                Expr::Ident(n) => Some(n.clone()),
                Expr::Resolved { name, .. } => Some(name.clone()),
                _ => None,
            }
        };
        let Expr::FnCall(c, a) = &e.node else {
            return None;
        };
        if a.len() != 3 || id(&a[0]).as_deref() != Some(g0) || id(&a[1]).as_deref() != Some(g1) {
            return None;
        }
        Some((expr_to_dotted_name(&c.node)?, &a[2]))
    }
    let (spec_fn, loop_fn, neutral) = match (as_spec(&law.lhs), as_loop(&law.rhs, g0, g1)) {
        (Some(s), Some((l, n))) => (s, l, n),
        _ => match (as_spec(&law.rhs), as_loop(&law.lhs, g0, g1)) {
            (Some(s), Some((l, n))) => (s, l, n),
            _ => return None,
        },
    };
    if spec_fn == loop_fn {
        return None;
    }

    // The shared combine must be a Peano monoid fn; its op fixes the neutral.
    let spec_fd = inputs.find_fn_def_by_call_name(&spec_fn)?;
    let loop_fd = inputs.find_fn_def_by_call_name(&loop_fn)?;
    if spec_fd.params.len() != 2 || loop_fd.params.len() != 3 {
        return None;
    }
    let base_p = loop_fd.params[0].0.as_str();
    let drv_p = loop_fd.params[1].0.as_str();
    let acc_p = loop_fd.params[2].0.as_str();

    // ── spec: match <drv> { Z -> neutral; S(n) -> combine(base, spec(base, n)) }
    let spec_base = spec_fd.params[0].0.as_str();
    let spec_drv = spec_fd.params[1].0.as_str();
    let [Stmt::Expr(sbody)] = spec_fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &sbody.node else {
        return None;
    };
    if ident_name(subject) != Some(spec_drv) || arms.len() != 2 {
        return None;
    }
    let mut spec_combine: Option<String> = None;
    for arm in arms {
        let Pattern::Constructor(_, binders) = &arm.pattern else {
            return None;
        };
        match binders.len() {
            0 => {} // base arm returns the neutral; validated against the op below
            1 => {
                // S(n) -> combine(base, spec(base, n))
                let Expr::FnCall(cc, cargs) = &arm.body.node else {
                    return None;
                };
                let cname = expr_to_dotted_name(&cc.node)?;
                if cargs.len() != 2 || ident_name(&cargs[0])? != spec_base {
                    return None;
                }
                let Expr::FnCall(rc, rargs) = &cargs[1].node else {
                    return None;
                };
                if expr_to_dotted_name(&rc.node)? != spec_fn
                    || rargs.len() != 2
                    || ident_name(&rargs[0])? != spec_base
                    || ident_name(&rargs[1])? != binders[0].as_str()
                {
                    return None;
                }
                spec_combine = Some(cname);
            }
            _ => return None,
        }
    }
    let spec_combine = spec_combine?;

    // ── loop: match <drv> { Z -> acc; S(n) -> loop(base, n, combine(base, acc)) }
    let [Stmt::Expr(lbody)] = loop_fd.body.stmts() else {
        return None;
    };
    let Expr::Match {
        subject: lsubj,
        arms: larms,
    } = &lbody.node
    else {
        return None;
    };
    if ident_name(lsubj) != Some(drv_p) || larms.len() != 2 {
        return None;
    }
    let mut loop_combine: Option<String> = None;
    for arm in larms {
        let Pattern::Constructor(_, binders) = &arm.pattern else {
            return None;
        };
        match binders.len() {
            0 => {
                if ident_name(&arm.body) != Some(acc_p) {
                    return None;
                }
            }
            1 => {
                // S(n) -> loop(base, n, combine(base, acc))
                let (callee, cargs) = match &arm.body.node {
                    Expr::FnCall(c, a) => (expr_to_dotted_name(&c.node)?, a.clone()),
                    Expr::TailCall(td) => (td.target.clone(), td.args.clone()),
                    _ => return None,
                };
                if callee != loop_fn
                    || cargs.len() != 3
                    || ident_name(&cargs[0])? != base_p
                    || ident_name(&cargs[1])? != binders[0].as_str()
                {
                    return None;
                }
                let Expr::FnCall(sc, sargs) = &cargs[2].node else {
                    return None;
                };
                if expr_to_dotted_name(&sc.node)? != spec_combine
                    || sargs.len() != 2
                    || ident_name(&sargs[0])? != base_p
                    || ident_name(&sargs[1])? != acc_p
                {
                    return None;
                }
                loop_combine = Some(expr_to_dotted_name(&sc.node)?);
            }
            _ => return None,
        }
    }
    if loop_combine.as_deref() != Some(spec_combine.as_str()) {
        return None;
    }

    // Classify the combine op + check the inline neutral matches it.
    let combine_fd = inputs.find_fn_def_by_call_name(&spec_combine)?;
    let combine_op = nat_monoid_op_of(combine_fd)?;
    let neutral_ok = match combine_op {
        crate::ast::BinOp::Mul => is_neutral_one(neutral),
        crate::ast::BinOp::Add => is_neutral_zero(neutral),
        _ => return None,
    };
    if !neutral_ok {
        return None;
    }

    Some(crate::ir::ProofStrategy::TailRecFixedBaseFold {
        spec_fn,
        loop_fn,
        combine_fn: spec_combine,
        combine_op,
        type_name,
    })
}

/// A constructor VALUE in any surface form this stage's AST presents: a bare
/// nullary `Nat.Z` parses as `Attr(Ident("Nat"), "Z")`, a `Nat.S(x)` as
/// `FnCall`, and some passes leave an `Expr::Constructor`. Returns the short
/// (type-prefix-stripped) constructor name + its argument exprs, or `None` for
/// a non-constructor expression. Mirrors `proof_recognize::call_or_ctor`.
fn peano_ctor_value(
    e: &Spanned<crate::ast::Expr>,
) -> Option<(String, Vec<&Spanned<crate::ast::Expr>>)> {
    use crate::ast::Expr;
    let short = |name: &str| name.rsplit('.').next().unwrap_or(name).to_string();
    match &e.node {
        Expr::FnCall(callee, args) => {
            let name = expr_to_dotted_name(&callee.node)?;
            Some((short(&name), args.iter().collect()))
        }
        Expr::Constructor(name, arg) => {
            Some((short(name), arg.iter().map(|b| b.as_ref()).collect()))
        }
        Expr::Attr(..) => expr_to_dotted_name(&e.node).map(|n| (short(&n), Vec::new())),
        _ => None,
    }
}

/// Classify a 2-param Peano monoid fn by its base arm: `Z -> y` (returns the
/// second param) is addition; `Z -> Z` (returns a nullary constructor) is
/// multiplication. `None` for anything else — the wrapper strategy then
/// declines and the law falls back to the generic ladder.
fn nat_monoid_op_of(fd: &crate::ast::FnDef) -> Option<crate::ast::BinOp> {
    use crate::ast::{BinOp, Expr, Pattern, Stmt};
    if fd.params.len() != 2 {
        return None;
    }
    let second = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { arms, .. } = &body.node else {
        return None;
    };
    for arm in arms {
        let Pattern::Constructor(_, binders) = &arm.pattern else {
            continue;
        };
        if !binders.is_empty() {
            continue;
        }
        if matches_ident_expr(&arm.body, second) {
            return Some(BinOp::Add);
        }
        if peano_ctor_value(&arm.body).is_some_and(|(_, args)| args.is_empty()) {
            return Some(BinOp::Mul);
        }
    }
    None
}

/// `Z` — a nullary constructor value (the additive Peano neutral). Name-
/// agnostic: a Peano `Nat` has exactly one nullary constructor, and a
/// mis-shaped neutral only degrades the proof to an honest `sorry`.
fn is_peano_zero(e: &Spanned<crate::ast::Expr>) -> bool {
    peano_ctor_value(e).is_some_and(|(_, args)| args.is_empty())
}

/// `S(Z)` — successor of zero (the multiplicative Peano neutral).
fn is_peano_one(e: &Spanned<crate::ast::Expr>) -> bool {
    peano_ctor_value(e).is_some_and(|(_, args)| args.len() == 1 && is_peano_zero(args[0]))
}

/// Return `Some(op)` iff `fn_name` resolves to a 2-arg Int wrapper
/// `fn f(p1: Int, p2: Int) -> Int :- p1 <op> p2`. The op family
/// covers `Add` / `Mul` (commutative + associative lemmas) and
/// `Sub` (anti-commutative + right-identity); other ops fall back
/// to `BackendDispatch`.
pub(super) fn wrapper_binop(fn_name: &str, inputs: &ProofLowerInputs) -> Option<crate::ast::BinOp> {
    use crate::ast::{BinOp, Expr};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Int" {
        return None;
    }
    let (p1, t1) = &fd.params[0];
    let (p2, t2) = &fd.params[1];
    if t1 != "Int" || t2 != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    if !matches_ident_expr(left, p1) || !matches_ident_expr(right, p2) {
        return None;
    }
    matches!(op, BinOp::Add | BinOp::Mul | BinOp::Sub).then_some(*op)
}

pub(super) fn detect_wrapper_commutative(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    _op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 2 || law.givens.iter().any(|g| g.type_name != "Int") {
        return false;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    matches_binary_call(&law.lhs, fn_name, a, b) && matches_binary_call(&law.rhs, fn_name, b, a)
        || matches_binary_call(&law.lhs, fn_name, b, a)
            && matches_binary_call(&law.rhs, fn_name, a, b)
}

pub(super) fn detect_wrapper_associative(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    _op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 3 || law.givens.iter().any(|g| g.type_name != "Int") {
        return false;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    let c = &law.givens[2].name;
    let nested = |side| matches_assoc_nested(side, fn_name, a, b, c);
    let flat = |side| matches_assoc_flat(side, fn_name, a, b, c);
    (nested(&law.lhs) && flat(&law.rhs)) || (nested(&law.rhs) && flat(&law.lhs))
}

/// Detect a unary↔binary wrapper equivalence shape:
/// outer side: `outer(g)` where `fn outer(p) -> p <op> K`
/// other side: `inner(g, K)` or `inner(K, g)` where `fn inner(a, b) -> a <op> b`
/// Both sides must agree on op + constant + var-position.
/// Returns the inner fn's source name, or `None` if no match.
pub(super) fn detect_wrapper_unary_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let unary = unary_int_wrapper(fn_name, inputs)?;
    let g = &law.givens[0].name;

    let try_side = |call_side: &Spanned<crate::ast::Expr>,
                    other_side: &Spanned<crate::ast::Expr>|
     -> Option<String> {
        if !matches_unary_call(call_side, fn_name, g) {
            return None;
        }
        let (callee_name, var_first, lit) = binary_call_var_const(other_side, g)?;
        if lit != unary.constant || var_first != unary.var_first {
            return None;
        }
        let inner_op = wrapper_binop(&callee_name, inputs)?;
        if inner_op != unary.op {
            return None;
        }
        Some(callee_name)
    };
    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

#[derive(Debug, Clone, Copy)]
pub(super) struct UnaryIntWrapper {
    op: crate::ast::BinOp,
    constant: i64,
    var_first: bool,
}

/// Resolve `fn outer(p: Int) -> Int :- p <op> K` or `K <op> p`.
/// Returns the op + literal + which side carries the param.
pub(super) fn unary_int_wrapper(
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<UnaryIntWrapper> {
    use crate::ast::{Expr, Literal};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 1 || fd.return_type != "Int" {
        return None;
    }
    let (param, param_ty) = &fd.params[0];
    if param_ty != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    let lit_of = |e: &Spanned<Expr>| -> Option<i64> {
        match &e.node {
            Expr::Literal(Literal::Int(n)) => Some(*n),
            _ => None,
        }
    };
    if matches_ident_expr(left, param) {
        let n = lit_of(right)?;
        return Some(UnaryIntWrapper {
            op: *op,
            constant: n,
            var_first: true,
        });
    }
    if matches_ident_expr(right, param) {
        let n = lit_of(left)?;
        return Some(UnaryIntWrapper {
            op: *op,
            constant: n,
            var_first: false,
        });
    }
    None
}

pub(super) fn matches_unary_call(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    arg: &str,
) -> bool {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    args.len() == 1 && callee_matches_name(callee, fn_name) && matches_ident_expr(&args[0], arg)
}

/// `inner(var, K)` or `inner(K, var)` shape. Returns
/// `(callee_name, var_first, K)` on match.
/// **syntax-discovery-only** (epic #170 Phase 7). Detects a
/// 2-arg fn call whose first arg is `Ident(var_name)` and second
/// is an `Int` literal (or symmetric). Returns the callee's dotted
/// name + the literal — caller dispatches on the name string. Used
/// only against builtin namespace methods (`Int.add`, etc.); the
/// returned name string is not stored or used for identity past
/// the dispatch site.
pub(super) fn binary_call_var_const(
    expr: &Spanned<crate::ast::Expr>,
    var_name: &str,
) -> Option<(String, bool, i64)> {
    use crate::ast::{Expr, Literal};
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let callee_name = expr_to_dotted_name(&callee.node)?;
    match (&args[0].node, &args[1].node) {
        (Expr::Ident(v) | Expr::Resolved { name: v, .. }, Expr::Literal(Literal::Int(n)))
            if v == var_name =>
        {
            Some((callee_name, true, *n))
        }
        (Expr::Literal(Literal::Int(n)), Expr::Ident(v) | Expr::Resolved { name: v, .. })
            if v == var_name =>
        {
            Some((callee_name, false, *n))
        }
        _ => None,
    }
}

pub(super) fn detect_wrapper_sub_right_identity(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
) -> bool {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return false;
    }
    let g = &law.givens[0].name;
    matches_sub_right_identity_side(&law.lhs, &law.rhs, fn_name, g)
        || matches_sub_right_identity_side(&law.rhs, &law.lhs, fn_name, g)
}

/// Detect `sub(a, b) => -sub(b, a)` or the swapped arrangement.
/// Returns `Some(neg_on_rhs)` — `true` when the negation is on the
/// rhs (canonical direction); `false` when swapped (call on rhs,
/// negation on lhs). `None` when the shape doesn't fit.
pub(super) fn detect_wrapper_sub_anti_commutative(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
) -> Option<bool> {
    if law.givens.len() != 2 || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    if matches_binary_call(&law.lhs, fn_name, a, b)
        && matches_neg_binary_call(&law.rhs, fn_name, b, a)
    {
        return Some(true);
    }
    if matches_binary_call(&law.rhs, fn_name, a, b)
        && matches_neg_binary_call(&law.lhs, fn_name, b, a)
    {
        return Some(false);
    }
    None
}

pub(super) fn detect_wrapper_identity(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return false;
    }
    let identity = match op {
        crate::ast::BinOp::Add => 0,
        crate::ast::BinOp::Mul => 1,
        _ => return false,
    };
    let g = &law.givens[0].name;
    matches_identity_side(&law.lhs, &law.rhs, fn_name, g, identity)
        || matches_identity_side(&law.rhs, &law.lhs, fn_name, g, identity)
}

// ── AST matchers — ported from `lean::law_auto::shared` ─────────
//
// Kept private to proof_lower to preserve layering (proof_lower
// must not reach into lean codegen). The shapes are backend-neutral
// — pure AST pattern matching — so the duplication is local-cost
// only. A future cleanup could consolidate these into a shared
// `codegen::ast_match` module.

pub(super) fn body_terminal_expr(body: &crate::ast::FnBody) -> Option<&Spanned<crate::ast::Expr>> {
    use crate::ast::Stmt;
    match body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

/// Fold the algebraic identities that survive a body-substitution
/// pass with a literal-int arg: `a + 0 == a`, `0 + a == a`,
/// `a - 0 == a`, `a * 1 == a`, `1 * a == a`, `a * 0 == 0`,
/// `0 * a == 0`. Recursive over BinOp / Neg / Attr / FnCall. Used
/// by `detect_spec_equivalence` so impl bodies like `n + 0` and
/// spec bodies like `n` are recognised as equivalent under arg
/// substitution. Matches the legacy `simp_normalized` shape.
pub(super) fn simplify_identity_expr(
    expr: &Spanned<crate::ast::Expr>,
) -> Spanned<crate::ast::Expr> {
    use crate::ast::{BinOp, Expr, Literal};
    let line = expr.line;
    let int_lit = |e: &Expr| -> Option<i64> {
        match e {
            Expr::Literal(Literal::Int(n)) => Some(*n),
            _ => None,
        }
    };
    let new_node = match &expr.node {
        Expr::BinOp(op, left, right) => {
            let left = simplify_identity_expr(left);
            let right = simplify_identity_expr(right);
            match op {
                BinOp::Add => {
                    if int_lit(&left.node) == Some(0) {
                        return right;
                    } else if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Sub => {
                    if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Mul => {
                    if int_lit(&left.node) == Some(0) || int_lit(&right.node) == Some(0) {
                        Expr::Literal(Literal::Int(0))
                    } else if int_lit(&left.node) == Some(1) {
                        return right;
                    } else if int_lit(&right.node) == Some(1) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                _ => Expr::BinOp(*op, Box::new(left), Box::new(right)),
            }
        }
        Expr::Neg(inner) => Expr::Neg(Box::new(simplify_identity_expr(inner))),
        Expr::Attr(base, field) => {
            Expr::Attr(Box::new(simplify_identity_expr(base)), field.clone())
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(simplify_identity_expr(callee)),
            args.iter().map(simplify_identity_expr).collect(),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(simplify_identity_expr(subject)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(simplify_identity_expr(&arm.body)),
                    binding_slots: arm.binding_slots.clone(),
                })
                .collect(),
        },
        other => other.clone(),
    };
    Spanned::new(new_node, line)
}

/// **syntax-discovery-only** (epic #170 Phase 7). Returns true iff
/// `expr` is a bare `Ident(name)` / `Resolved { name }` matching
/// the given name. Used by shape detectors to recognise "the
/// callsite mentions this binder" — no identity lookup, just
/// source-name shape.
pub(super) fn matches_ident_expr(expr: &Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    matches!(&expr.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

/// **syntax-discovery-only** (epic #170 Phase 7). Compares the
/// callee's dotted source name against a target string for shape
/// recognition. EXACT MATCH ONLY — the previous suffix-match clause
/// (`name.rsplit('.').next() == Some(target)`) was an identity leak:
/// it accepted `Foo.helper` as a match for target `"helper"`. Bounded
/// today by the parser invariant (`vb.fn_name` is bare entry-only),
/// but the suffix clause stayed live as dead code that any future
/// module-scoped verify would silently exercise.
pub(super) fn callee_matches_name(expr: &Spanned<crate::ast::Expr>, target: &str) -> bool {
    let Some(name) = expr_to_dotted_name(&expr.node) else {
        return false;
    };
    name == target
}

pub(super) fn call2_args<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&args[0], &args[1]))
}

pub(super) fn matches_binary_call(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
) -> bool {
    let Some((x, y)) = call2_args(expr, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b)
}

pub(super) fn matches_assoc_nested(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((ab, z)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((x, y)) = call2_args(ab, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b) && matches_ident_expr(z, c)
}

pub(super) fn matches_assoc_flat(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((x, bc)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((y, z)) = call2_args(bc, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b) && matches_ident_expr(z, c)
}

pub(super) fn matches_sub_right_identity_side(
    call_side: &Spanned<crate::ast::Expr>,
    ident_side: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    given_name: &str,
) -> bool {
    use crate::ast::{Expr, Literal};
    if !matches_ident_expr(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    matches_ident_expr(x, given_name)
        && matches!(&y.node, Expr::Literal(Literal::Int(n)) if *n == 0)
}

pub(super) fn matches_neg_binary_call(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Neg(inner) => matches_binary_call(inner, fn_name, a, b),
        _ => false,
    }
}

pub(super) fn matches_identity_side(
    call_side: &Spanned<crate::ast::Expr>,
    ident_side: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    given_name: &str,
    identity: i64,
) -> bool {
    use crate::ast::{Expr, Literal};
    if !matches_ident_expr(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    let is_int_lit = |e: &Spanned<Expr>, n: i64| -> bool {
        matches!(&e.node, Expr::Literal(Literal::Int(m)) if *m == n)
    };
    (matches_ident_expr(x, given_name) && is_int_lit(y, identity))
        || (is_int_lit(x, identity) && matches_ident_expr(y, given_name))
}
