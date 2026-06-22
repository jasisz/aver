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

use super::expr::{aver_name_to_dafny, emit_expr_legacy};

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
        let second = emit_expr_legacy(&args[1], ctx, None);
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
