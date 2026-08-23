//! Dafny proof lemma library.
//!
//! Per-shape recognizers that emit PROVED helper lemmas + `forall`-lifted
//! facts + induction-step bridges for `toplevel::emit_verify_law`. Each
//! strategy is CONSERVATIVE (a non-matching shape simply fails to fire) and
//! every emitted lemma is discharged by Dafny itself — `aver proof --check`'s
//! `axioms:0 / omitted:0 / errors:0` gate is the hard safety net when adding a
//! new entry, since a too-broad recognizer would emit a false lemma that Dafny
//! then refuses to prove.
//!
//! Two aggregators are the registry surface `emit_verify_law` consumes:
//! [`algebra_lemmas`] (proved helper lemmas prepended before the law lemma +
//! `forall`-lifts injected at the body top) and [`list_bridges`] (the
//! cons-decomposition asserts injected into the list-induction case split).
use crate::ast::*;
use crate::codegen::CodegenContext;

use super::expr::{aver_name_to_dafny, emit_expr};

/// Conservative recognition of a left-concatenation `concat(<ind_var>, b)`
/// whose first operand is the list-induction variable — either the
/// `List.concat` builtin or a user wrapper whose body is exactly
/// `List.concat(p0, p1)` over its two params in order (e.g. `appendNat`).
///
/// Returns the Dafny renderings the list-induction emitter needs to supply
/// cons-decomposition bridge asserts: `(c_full, c_tail)` where `c_full` is
/// the concat over the whole induction var and `c_tail` is the same over
/// `<var>[1..]`. The structural guard makes a false match impossible, so the
/// asserts the emitter builds from this are always true for a genuine concat
/// (a fold over `xs ++ ys` left-decomposes as `[xs[0]] ++ (xs[1..] ++ ys)`),
/// which is what guides Z3 to unfold the fold during the inductive step.
fn concat_left_fold_render(
    callee_name: &str,
    second_dafny: &str,
    ind_var_dafny: &str,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    let is_builtin = callee_name == "List.concat";
    let is_wrapper = !is_builtin
        && ctx
            .fn_def_by_name(callee_name, ctx.active_module_scope().as_deref())
            .is_some_and(|fd| {
                fd.params.len() == 2
                    && fd.body.tail_expr().is_some_and(|tail| match &tail.node {
                        Expr::FnCall(c, args) => {
                            crate::codegen::common::expr_to_dotted_name(&c.node).as_deref()
                                == Some("List.concat")
                                && args.len() == 2
                                && crate::codegen::recursion::detect::local_name_of(&args[0])
                                    .is_some_and(|n| n == fd.params[0].0)
                                && crate::codegen::recursion::detect::local_name_of(&args[1])
                                    .is_some_and(|n| n == fd.params[1].0)
                        }
                        _ => false,
                    })
            });
    if !(is_builtin || is_wrapper) {
        return None;
    }
    if is_builtin {
        Some((
            format!("({} + {})", ind_var_dafny, second_dafny),
            format!("({}[1..] + {})", ind_var_dafny, second_dafny),
        ))
    } else {
        let d = aver_name_to_dafny(callee_name);
        Some((
            format!("{}({}, {})", d, ind_var_dafny, second_dafny),
            format!("{}({}[1..], {})", d, ind_var_dafny, second_dafny),
        ))
    }
}

/// Walk a law side collecting every left-concat over the induction variable.
/// Each entry `(c_full, c_tail, second)` feeds two bridge asserts in the
/// list-induction skeleton: base `c_full == second` (empty ++ ys == ys) and
/// step `c_full == [<var>[0]] + c_tail`. These are pure cons-decomposition
/// facts that turn an otherwise-timing-out fold-over-concat homomorphism
/// (e.g. `count(n, xs ++ ys) == plus(count n xs, count n ys)`) into a proof
/// Z3 closes in one step.
fn collect_concat_bridges(
    expr: &Spanned<Expr>,
    ind_var_src: &str,
    ind_var_dafny: &str,
    ctx: &CodegenContext,
    out: &mut Vec<(String, String, String)>,
) {
    if let Expr::FnCall(callee, args) = &expr.node
        && args.len() == 2
        && crate::codegen::recursion::detect::local_name_of(&args[0])
            .is_some_and(|n| n == ind_var_src)
        && let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node)
    {
        let second = emit_expr(&super::toplevel::resolve_rewrite_output(&args[1], ctx), ctx);
        if let Some((c_full, c_tail)) = concat_left_fold_render(&name, &second, ind_var_dafny, ctx)
        {
            let entry = (c_full, c_tail, second);
            if !out.contains(&entry) {
                out.push(entry);
            }
        }
    }
    match &expr.node {
        Expr::FnCall(f, args) => {
            collect_concat_bridges(f, ind_var_src, ind_var_dafny, ctx, out);
            for a in args {
                collect_concat_bridges(a, ind_var_src, ind_var_dafny, ctx, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_concat_bridges(l, ind_var_src, ind_var_dafny, ctx, out);
            collect_concat_bridges(r, ind_var_src, ind_var_dafny, ctx, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_concat_bridges(subject, ind_var_src, ind_var_dafny, ctx, out);
            for arm in arms {
                collect_concat_bridges(&arm.body, ind_var_src, ind_var_dafny, ctx, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Neg(inner) | Expr::Constructor(_, Some(inner)) => {
            collect_concat_bridges(inner, ind_var_src, ind_var_dafny, ctx, out)
        }
        Expr::Attr(obj, _) => collect_concat_bridges(obj, ind_var_src, ind_var_dafny, ctx, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_concat_bridges(e, ind_var_src, ind_var_dafny, ctx, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_concat_bridges(e, ind_var_src, ind_var_dafny, ctx, out);
            }
        }
        _ => {}
    }
}

fn dafny_find_type_def<'a>(ctx: &'a CodegenContext, name: &str) -> Option<&'a TypeDef> {
    let bare = name.rsplit('.').next().unwrap_or(name);
    ctx.type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .find(|td| crate::codegen::common::type_def_name(td) == bare)
}

/// An additive-monoid operator: a binary structural recursion on a Peano-
/// shaped ADT `T` (a nullary base ctor + a unary self-recursive succ ctor)
/// whose body is exactly `match a { Base -> b; Succ(p) -> Succ(op(p, b)) }`.
/// `plus` is the canonical instance. For this EXACT shape two algebraic
/// facts are provable by induction on the first argument and hold
/// unconditionally — right-identity `op(a, Base) == a` and succ-shift
/// `op(a, Succ b) == Succ(op(a, b))` — which is what a homomorphism-into-T
/// proof needs when the induction variable lands in the op's SECOND
/// argument (e.g. `length(append(x,y)) == plus(length y, length x)`).
struct AdditiveOp {
    /// Source fn name of the operator (e.g. `plus`).
    name: String,
    /// ADT type name (e.g. `Nat`).
    type_name: String,
    /// Short nullary base ctor name (e.g. `Z`).
    base_ctor: String,
    /// Short unary succ ctor name (e.g. `S`).
    succ_ctor: String,
}

/// Recognize the canonical additive-monoid shape (see [`AdditiveOp`]).
/// Conservative: every structural requirement is checked, so a non-additive
/// binary fn never matches and the lemmas built from the result are always
/// true.
fn detect_additive_op(fd: &FnDef, ctx: &CodegenContext) -> Option<AdditiveOp> {
    if fd.params.len() != 2 {
        return None;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 || &fd.return_type != t0 {
        return None;
    }
    let TypeDef::Sum {
        name: tname,
        variants,
        ..
    } = dafny_find_type_def(ctx, t0)?
    else {
        return None;
    };
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if crate::codegen::recursion::detect::local_name_of(subject)? != p0 || arms.len() != 2 {
        return None;
    }
    // A constructor application `Nat.S(arg)` parses as a FnCall whose callee
    // dots to the ctor name — not an `Expr::Constructor` (which is reserved
    // for the bare-nullary / pattern forms).
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let mut base_ctor: Option<String> = None;
    let mut succ_ctor: Option<String> = None;
    for arm in arms {
        match &arm.pattern {
            // base arm: `Base -> p1`
            Pattern::Constructor(cname, binders)
                if binders.is_empty()
                    && crate::codegen::recursion::detect::local_name_of(&arm.body)
                        .is_some_and(|n| n == p1) =>
            {
                base_ctor = Some(crate::codegen::proof_recognize::short_ctor(cname).to_string());
            }
            // succ arm: `Succ(q) -> Succ(op(q, p1))`
            Pattern::Constructor(cname, binders) if binders.len() == 1 => {
                let q = &binders[0];
                if let Expr::FnCall(body_callee, body_args) = &arm.body.node
                    && body_args.len() == 1
                    && dotted(body_callee)
                        .as_deref()
                        .map(crate::codegen::proof_recognize::short_ctor)
                        == Some(crate::codegen::proof_recognize::short_ctor(cname))
                    && let Expr::FnCall(rec_callee, rec_args) = &body_args[0].node
                    && dotted(rec_callee)
                        .as_deref()
                        .map(crate::codegen::proof_recognize::short_ctor)
                        == Some(fd.name.as_str())
                    && rec_args.len() == 2
                    && crate::codegen::recursion::detect::local_name_of(&rec_args[0])
                        .is_some_and(|n| n == q)
                    && crate::codegen::recursion::detect::local_name_of(&rec_args[1])
                        .is_some_and(|n| n == p1)
                {
                    succ_ctor =
                        Some(crate::codegen::proof_recognize::short_ctor(cname).to_string());
                }
            }
            _ => {}
        }
    }
    let base_ctor = base_ctor?;
    let succ_ctor = succ_ctor?;
    // The ctors must really be a nullary base + a unary self-recursive succ
    // of this type — guards against a same-named-but-different-arity variant.
    let base_ok = variants
        .iter()
        .any(|v| v.name == base_ctor && v.fields.is_empty());
    let succ_ok = variants
        .iter()
        .any(|v| v.name == succ_ctor && v.fields.len() == 1 && v.fields[0].trim() == tname);
    if !base_ok || !succ_ok {
        return None;
    }
    Some(AdditiveOp {
        name: fd.name.clone(),
        type_name: tname.clone(),
        base_ctor,
        succ_ctor,
    })
}

/// Collect the distinct additive operators a law's two sides invoke.
fn collect_additive_ops_in_law(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<AdditiveOp> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut names);
    crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            crate::codegen::proof_recognize::collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(|fd| detect_additive_op(fd, ctx))
        .collect()
}

/// For each additive op a law uses, build (a) the standalone proved
/// right-identity + succ-shift lemmas (law-scoped names so multi-law
/// modules don't collide) and (b) the `forall`-lift lines that hoist them
/// to quantified facts inside the law lemma's body, so Z3 instantiates
/// them itself during the induction — no per-law term surgery, fully
/// generic across any additive op / Peano-shaped codomain.
fn additive_op_lemmas(ops: &[AdditiveOp], law_uid: &str) -> (Vec<String>, Vec<String>) {
    let mut defs = Vec::new();
    let mut lifts = Vec::new();
    for op in ops {
        let f = aver_name_to_dafny(&op.name);
        let t = &op.type_name;
        let base = &op.base_ctor;
        let succ = &op.succ_ctor;
        let rid = format!("{law_uid}_{f}_rid");
        let succ_lemma = format!("{law_uid}_{f}_succ");
        defs.push(format!(
            "lemma {rid}(a: {t})\n  ensures {f}(a, {t}.{base}) == a\n  decreases a\n{{\n  match a {{ case {base} => case {succ}(p) => {rid}(p); }}\n}}"
        ));
        defs.push(format!(
            "lemma {succ_lemma}(a: {t}, b: {t})\n  ensures {f}(a, {t}.{succ}(b)) == {t}.{succ}({f}(a, b))\n  decreases a\n{{\n  match a {{ case {base} => case {succ}(p) => {succ_lemma}(p, b); }}\n}}"
        ));
        lifts.push(format!(
            "  forall a: {t} ensures {f}(a, {t}.{base}) == a {{ {rid}(a); }}"
        ));
        lifts.push(format!(
            "  forall a: {t}, b: {t} ensures {f}(a, {t}.{succ}(b)) == {t}.{succ}({f}(a, b)) {{ {succ_lemma}(a, b); }}"
        ));
    }
    (defs, lifts)
}

/// Proved helper lemmas (prepended before the law lemma) and the `forall`
/// facts that hoist them into the law lemma body. Branch-agnostic: emitted for
/// every inductive law regardless of the induction variable's type.
pub(super) struct AlgebraLemmas {
    pub defs: Vec<String>,
    pub lifts: Vec<String>,
}

/// Run the helper-lemma strategies (additive monoid ops, rev anti-homomorphism,
/// length-snoc) over a law and merge their contributions. Adding a new
/// helper-lemma family means adding its recognizer plus emitter here.
pub(super) fn algebra_lemmas(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    law_uid: &str,
) -> AlgebraLemmas {
    let additive = collect_additive_ops_in_law(law, ctx);
    let (defs, lifts) = additive_op_lemmas(&additive, law_uid);
    AlgebraLemmas { defs, lifts }
}

fn render(e: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    emit_expr(&super::toplevel::resolve_rewrite_output(e, ctx), ctx)
}

fn same_atom(a: &Spanned<Expr>, b: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    render(a, ctx) == render(b, ctx)
}

fn expr_dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(base, field) => expr_dotted_name(base).map(|p| format!("{p}.{field}")),
        _ => None,
    }
}

fn find_fn_def_by_call_name<'a>(ctx: &'a CodegenContext, call_name: &str) -> Option<&'a FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .find(|fd| fd.name == call_name)
        .or_else(|| {
            let short = call_name.rsplit('.').next()?;
            ctx.modules
                .iter()
                .flat_map(|m| m.fn_defs.iter())
                .chain(ctx.fn_defs.iter())
                .find(|fd| fd.name == short)
        })
}

fn is_param_ident(e: &Spanned<Expr>, name: &str) -> bool {
    matches!(&e.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

fn is_euclidean_floor_fn(floor_src: &str, ctx: &CodegenContext) -> bool {
    let Some(fd) = find_fn_def_by_call_name(ctx, floor_src) else {
        return false;
    };
    let [(num_param, num_ty), (den_param, den_ty)] = fd.params.as_slice() else {
        return false;
    };
    if !fd.effects.is_empty() || num_ty != "Int" || den_ty != "Int" || fd.return_type != "Int" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::FnCall(callee, args) = &body.node else {
        return false;
    };
    if expr_dotted_name(callee).as_deref() != Some("Result.withDefault") || args.len() != 2 {
        return false;
    }
    if !matches!(&args[1].node, Expr::Literal(Literal::Int(0))) {
        return false;
    }
    let Expr::FnCall(div_callee, div_args) = &args[0].node else {
        return false;
    };
    expr_dotted_name(div_callee).as_deref() == Some("Int.div")
        && div_args.len() == 2
        && is_param_ident(&div_args[0], num_param)
        && is_param_ident(&div_args[1], den_param)
}

fn floor_call<'a>(
    e: &'a Spanned<Expr>,
    floor_src: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    if expr_dotted_name(callee).as_deref() != Some(floor_src) || args.len() != 2 {
        return None;
    }
    Some((&args[0], &args[1]))
}

/// Flatten a `Bool.and` conjunction of `when` clauses, canonicalizing every
/// leaf comparison (`a > b` -> `b < a`, `a >= b` -> `b <= a`) so the Dafny
/// recognizers below only match `Lt`/`Lte` — the direction-blind counterpart
/// of the Lean `law_auto::shared::flatten_and` choke point. Recognition-only:
/// the emitted `requires` still renders `law.when` verbatim.
fn flatten_and(e: &Spanned<Expr>, out: &mut Vec<Spanned<Expr>>) {
    match &e.node {
        Expr::FnCall(callee, args)
            if expr_dotted_name(callee).as_deref() == Some("Bool.and") && args.len() == 2 =>
        {
            flatten_and(&args[0], out);
            flatten_and(&args[1], out);
        }
        _ => out.push(crate::codegen::common::canonicalize_comparison(e)),
    }
}

fn clause_gives_pos(clause: &Spanned<Expr>, x_render: &str, ctx: &CodegenContext) -> bool {
    let Expr::BinOp(op, l, r) = &clause.node else {
        return false;
    };
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    // `flatten_and` canonicalizes `x > c` / `x >= c` to `c < x` / `c <= x`.
    match op {
        BinOp::Lt => int_lit(l).is_some_and(|c| c >= 0) && render(r, ctx) == x_render,
        BinOp::Lte => int_lit(l).is_some_and(|c| c >= 1) && render(r, ctx) == x_render,
        _ => false,
    }
}

fn clause_gives_nonneg(clause: &Spanned<Expr>, x_render: &str, ctx: &CodegenContext) -> bool {
    if clause_gives_pos(clause, x_render, ctx) {
        return true;
    }
    let Expr::BinOp(op, l, r) = &clause.node else {
        return false;
    };
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    // `flatten_and` canonicalizes `x >= c` to `c <= x`.
    match op {
        BinOp::Lte => int_lit(l).is_some_and(|c| c >= 0) && render(r, ctx) == x_render,
        _ => false,
    }
}

fn clause_is_lt(
    clause: &Spanned<Expr>,
    x_render: &str,
    y_render: &str,
    ctx: &CodegenContext,
) -> bool {
    let Expr::BinOp(BinOp::Lt, l, r) = &clause.node else {
        return false;
    };
    render(l, ctx) == x_render && render(r, ctx) == y_render
}

fn split_shared(l_r: &str, r_r: &str, target: &str) -> Option<(String, bool)> {
    if l_r == target {
        Some((r_r.to_string(), true))
    } else if r_r == target {
        Some((l_r.to_string(), false))
    } else {
        None
    }
}

struct NestedFloorShape {
    a: String,
    d: String,
    e: String,
}

fn recognize_nested_floor(law: &VerifyLaw, ctx: &CodegenContext) -> Option<NestedFloorShape> {
    let Expr::FnCall(callee, args) = &law.rhs.node else {
        return None;
    };
    let floor_src = expr_dotted_name(callee)?;
    if args.len() != 2 {
        return None;
    }
    let a_r = &args[0];
    let Expr::BinOp(BinOp::Mul, d_r, e_r) = &args[1].node else {
        return None;
    };
    let (inner, e_l) = floor_call(&law.lhs, &floor_src)?;
    let (a_l, d_l) = floor_call(inner, &floor_src)?;
    if !same_atom(a_l, a_r, ctx) || !same_atom(d_l, d_r, ctx) || !same_atom(e_l, e_r, ctx) {
        return None;
    }
    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    let d = render(d_r, ctx);
    let e = render(e_r, ctx);
    if !clauses.iter().any(|c| clause_gives_pos(c, &d, ctx))
        || !clauses.iter().any(|c| clause_gives_pos(c, &e, ctx))
    {
        return None;
    }
    Some(NestedFloorShape {
        a: render(a_r, ctx),
        d,
        e,
    })
}

struct CancelShape {
    a: String,
    d: String,
    c: String,
    dividend: String,
    divisor: String,
}

fn recognize_cancel_common_factor(law: &VerifyLaw, ctx: &CodegenContext) -> Option<CancelShape> {
    let Expr::FnCall(callee, args) = &law.rhs.node else {
        return None;
    };
    let floor_src = expr_dotted_name(callee)?;
    if args.len() != 2 {
        return None;
    }
    let a = render(&args[0], ctx);
    let d = render(&args[1], ctx);
    let (prod_a, prod_d) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Mul, a_l, c_a) = &prod_a.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, d_l, c_d) = &prod_d.node else {
        return None;
    };
    let (c_from_a, _) = split_shared(&render(a_l, ctx), &render(c_a, ctx), &a)?;
    let (c_from_d, _) = split_shared(&render(d_l, ctx), &render(c_d, ctx), &d)?;
    if c_from_a != c_from_d || !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    if !clauses.iter().any(|cl| clause_gives_pos(cl, &d, ctx))
        || !clauses
            .iter()
            .any(|cl| clause_gives_pos(cl, &c_from_a, ctx))
    {
        return None;
    }
    Some(CancelShape {
        a,
        d,
        c: c_from_a,
        dividend: render(prod_a, ctx),
        divisor: render(prod_d, ctx),
    })
}

struct AbsorbShape {
    d: String,
    q: String,
    r: String,
    dividend: String,
}

fn split_sum<'a>(
    add_l: &'a Spanned<Expr>,
    add_r: &'a Spanned<Expr>,
    d_render: &str,
    q_render: &str,
    ctx: &CodegenContext,
) -> Option<&'a Spanned<Expr>> {
    for (prod, rem) in [(add_l, add_r), (add_r, add_l)] {
        let Expr::BinOp(BinOp::Mul, x, y) = &prod.node else {
            continue;
        };
        let (xr, yr) = (render(x, ctx), render(y, ctx));
        if (xr == d_render && yr == q_render) || (yr == d_render && xr == q_render) {
            return Some(rem);
        }
    }
    None
}

fn recognize_absorb_remainder(law: &VerifyLaw, ctx: &CodegenContext) -> Option<AbsorbShape> {
    let Expr::FnCall(callee, _) = &law.lhs.node else {
        return None;
    };
    let floor_src = expr_dotted_name(callee)?;
    let (dividend, d_l) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Add, add_l, add_r) = &dividend.node else {
        return None;
    };
    let d = render(d_l, ctx);
    let q = render(&law.rhs, ctx);
    let rem = split_sum(add_l, add_r, &d, &q, ctx)?;
    let r = render(rem, ctx);
    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    if !clauses.iter().any(|cl| clause_gives_pos(cl, &d, ctx))
        || !clauses.iter().any(|cl| clause_gives_nonneg(cl, &r, ctx))
        || !clauses.iter().any(|cl| clause_is_lt(cl, &r, &d, ctx))
    {
        return None;
    }
    Some(AbsorbShape {
        d,
        q,
        r,
        dividend: render(dividend, ctx),
    })
}

fn floor_arith_helpers(prefix: &str) -> String {
    format!(
        "lemma {prefix}mul_mono(p: int, q: int, d: int)\n  requires p <= q && d >= 0\n  ensures p * d <= q * d\n{{ }}\nlemma {{:vcs_split_on_every_assert}} {prefix}div_lower(x: int, d: int, k: int)\n  requires d >= 1 && k * d <= x\n  ensures k <= x / d\n{{\n  assert x == d * (x / d) + x % d;\n  if k > x / d {{\n    {prefix}mul_mono(k, x / d + 1, d);\n    assert false;\n  }}\n}}\nlemma {{:vcs_split_on_every_assert}} {prefix}div_upper(x: int, d: int, k: int)\n  requires d >= 1 && x < k * d\n  ensures x / d < k\n{{\n  assert x == d * (x / d) + x % d;\n  if x / d >= k {{\n    {prefix}mul_mono(k, x / d, d);\n    assert false;\n  }}\n}}\nlemma {prefix}exact_div(x: int, d: int, q: int, r: int)\n  requires d >= 1 && x == d * q + r && 0 <= r && r < d\n  ensures x / d == q\n{{\n  {prefix}div_lower(x, d, q);\n  assert x < (q + 1) * d;\n  {prefix}div_upper(x, d, q + 1);\n}}\n"
    )
}

fn law_fuel_attrs(law: &VerifyLaw, ctx: &CodegenContext) -> String {
    let mut law_fns = std::collections::BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut law_fns);
    crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut law_fns);
    let mut transitive_fns = std::collections::BTreeSet::new();
    for f in &law_fns {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            crate::codegen::proof_recognize::collect_called_fns_in_body(
                &fd.body,
                &mut transitive_fns,
            );
        }
    }
    law_fns.extend(transitive_fns);
    law_fns
        .iter()
        .filter(|f| {
            ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref())
                .is_some()
        })
        .map(|f| format!("{{:fuel {}, 5}}", aver_name_to_dafny(f)))
        .collect::<Vec<_>>()
        .join(" ")
}

fn law_params(law: &VerifyLaw) -> String {
    law.givens
        .iter()
        .map(|g| {
            format!(
                "{}: {}",
                aver_name_to_dafny(&g.name),
                super::toplevel::emit_type(&g.type_name)
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn floor_arith_prelude(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    fn_name: &str,
    law_name: &str,
    shape: &str,
) -> (String, String, String, String, String) {
    let main = format!("{fn_name}_{law_name}");
    let prefix = format!("{main}__");
    let fuel_attrs = law_fuel_attrs(law, ctx);
    let attrs = if fuel_attrs.is_empty() {
        "{:vcs_split_on_every_assert}".to_string()
    } else {
        format!("{{:vcs_split_on_every_assert}} {fuel_attrs}")
    };
    let params = law_params(law);
    let when = law
        .when
        .as_ref()
        .map(|w| render(w, ctx))
        .unwrap_or_default();
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);
    let support = format!(
        "// Law: {fn_name}.{law_name} -- Euclidean floor-division {shape}\n{}",
        floor_arith_helpers(&prefix)
    );
    (
        main,
        prefix,
        support,
        format!("lemma {attrs}"),
        format!("{params})\n  requires {when}\n  ensures {lhs} == {rhs}"),
    )
}

/// Shape-keyed Dafny proof bodies for the Euclidean floor-division family:
/// nested floor collapse, common-factor cancel, and bounded-remainder absorb.
/// Each recognizer keys only on the law AST plus the callee body
/// `Result.withDefault(Int.div(num, den), 0)`, never on law or function names.
pub(super) fn floor_arith_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    fn_name: &str,
    law_name: &str,
) -> Option<String> {
    if let Some(NestedFloorShape { a, d, e }) = recognize_nested_floor(law, ctx) {
        let (main, prefix, support, lemma_kw, signature) =
            floor_arith_prelude(law, ctx, fn_name, law_name, "nested collapse");
        return Some(format!(
            "{support}{lemma_kw} {main}({signature}\n{{\n  var q := ({a}) / (({d}) * ({e}));\n  var r := ({a}) % (({d}) * ({e}));\n  assert ({d}) * ({e}) > 0;\n  assert ({a}) == (({d}) * ({e})) * q + r;\n  assert 0 <= r < ({d}) * ({e});\n  assert ({a}) == ({d}) * (({e}) * q) + r;\n  {prefix}exact_div({a}, {d}, ({e}) * q + r / ({d}), r % ({d}));\n  assert ({a}) / ({d}) == ({e}) * q + r / ({d});\n  assert 0 <= r / ({d}) < ({e});\n  {prefix}exact_div(({e}) * q + r / ({d}), {e}, q, r / ({d}));\n}}\n"
        ));
    }
    if let Some(CancelShape {
        a,
        d,
        c,
        dividend,
        divisor,
    }) = recognize_cancel_common_factor(law, ctx)
    {
        let (main, prefix, support, lemma_kw, signature) =
            floor_arith_prelude(law, ctx, fn_name, law_name, "common-factor cancel");
        return Some(format!(
            "{support}{lemma_kw} {main}({signature}\n{{\n  var q := ({a}) / ({d});\n  var r := ({a}) % ({d});\n  assert ({divisor}) > 0;\n  assert ({a}) == ({d}) * q + r;\n  assert 0 <= r < ({d});\n  assert ({dividend}) == ({a}) * ({c});\n  assert ({divisor}) == ({d}) * ({c});\n  assert ({dividend}) == ({divisor}) * q + r * ({c});\n  assert 0 <= r * ({c}) < ({divisor});\n  {prefix}exact_div({dividend}, {divisor}, q, r * ({c}));\n}}\n"
        ));
    }
    if let Some(AbsorbShape { d, q, r, dividend }) = recognize_absorb_remainder(law, ctx) {
        let (main, prefix, support, lemma_kw, signature) =
            floor_arith_prelude(law, ctx, fn_name, law_name, "bounded-remainder absorb");
        return Some(format!(
            "{support}{lemma_kw} {main}({signature}\n{{\n  assert ({dividend}) == ({d}) * ({q}) + ({r});\n  {prefix}exact_div({dividend}, {d}, {q}, {r});\n}}\n"
        ));
    }
    None
}

/// Cons-decomposition asserts for the list-induction case split: `base` lands in
/// the `|xs| == 0` arm, `step` in the `else` arm (before the IH call).
pub(super) struct ListBridges {
    pub base: Vec<String>,
    pub step: Vec<String>,
}

/// Run the per-step bridge strategies (concat folds + rev unfold) for a law
/// inducting on `list_param` (source name `ind_var_src`). Produces the exact
/// assert lines the emitter pushes — adding a new bridge family = extend here.
pub(super) fn list_bridges(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    list_param: &str,
    ind_var_src: &str,
) -> ListBridges {
    let mut cb = Vec::new();
    collect_concat_bridges(&law.lhs, ind_var_src, list_param, ctx, &mut cb);
    collect_concat_bridges(&law.rhs, ind_var_src, list_param, ctx, &mut cb);
    let mut base = Vec::new();
    let mut step = Vec::new();
    for (c_full, _c_tail, second) in &cb {
        base.push(format!("    assert {} == {};", c_full, second));
    }
    for (c_full, c_tail, _second) in &cb {
        step.push(format!(
            "    assert {} == [{}[0]] + {};",
            c_full, list_param, c_tail
        ));
    }
    ListBridges { base, step }
}
