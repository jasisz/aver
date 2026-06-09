//! Layer 3 — STRUCTURE-DIRECTED guarded conjectures (the "bricks").
//!
//! Shape detectors + templated lemma renderers for the
//! accumulator-generalization family: counted-repeat, field invariants,
//! encoder/roundtrip shapes, relational coverage, and monoidal specs.
//! See [`super`] for the full pipeline.

use super::*;

// ===========================================================================
// Layer 3 (first brick) — STRUCTURE-DIRECTED guarded conjectures.
//
// Some lemmas an inductive proof needs are GUARDED and arithmetic
// (`repeat(c, n+1) = repeat(c, n) ++ [c]` under `0 <= n`) — unreachable by the
// equational enumerator (no `n+1`/literals in its vocabulary). They are instead
// CONJECTURED from a recognized recursion shape and proved with a dedicated
// template (here: a fixed `Int.natAbs` bridge + a fuel-unfold). This is the
// first member of the accumulator-generalization family (the RLE retirement
// target); `count_nonneg` / `flush_fold_step` / `loop_gen` follow the same
// detect-shape → conjecture → templated-proof pattern (charter layer 3).
// ===========================================================================

/// A counted-append fn: `fn f(.., n, ..) = match n <= 0 { true -> []; false ->
/// List.concat(f(.., n - 1, ..), [E]) }` — any arity, `n` any `Int` param, and
/// the appended element `E` a parameter or a literal (so it covers both rle's
/// binary `repeat(c, n)` and a unary `repeat0(n) = .. ++ [0]`). Detected by
/// SHAPE, not name. Yields `f(.., n+1, ..) = f(.., n, ..) ++ [E]` under `0<=n`.
struct CountedRepeat {
    fn_name: String,
    params: Vec<(String, String)>,
    count_idx: usize,
    elem_lean: String,
}

fn cr_ident_name(e: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    match &e.node {
        crate::ast::Expr::Ident(n) => Some(n.clone()),
        crate::ast::Expr::Resolved { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn cr_is_int_lit(e: &crate::ast::Spanned<crate::ast::Expr>, n: i64) -> bool {
    matches!(&e.node, crate::ast::Expr::Literal(crate::ast::Literal::Int(k)) if *k == n)
}

fn cr_fn_call(
    e: &crate::ast::Spanned<crate::ast::Expr>,
) -> Option<(String, &[crate::ast::Spanned<crate::ast::Expr>])> {
    if let crate::ast::Expr::FnCall(callee, args) = &e.node {
        let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
        Some((name, args.as_slice()))
    } else {
        None
    }
}

/// Render a counted-append's appended element to Lean: a parameter variable
/// (its name) or an `Int` literal. Anything else is unsupported.
fn counted_repeat_elem_lean(e: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    use crate::ast::{Expr, Literal};
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
        Expr::Literal(Literal::Int(k)) => Some(k.to_string()),
        _ => None,
    }
}

/// Recognize the counted-append shape by structure (see [`CountedRepeat`]).
fn detect_counted_repeat(fd: &crate::ast::FnDef) -> Option<CountedRepeat> {
    use crate::ast::{BinOp, Expr, Literal, Pattern, Stmt};
    if fd.params.is_empty() {
        return None;
    }
    let Some(Stmt::Expr(term)) = fd.body.stmts().last() else {
        return None;
    };
    let Expr::Match { subject, arms } = &term.node else {
        return None;
    };
    // subject = `<count> <= 0`, where <count> is some Int parameter.
    let Expr::BinOp(BinOp::Lte, l, r) = &subject.node else {
        return None;
    };
    if !cr_is_int_lit(r, 0) {
        return None;
    }
    let count_name = cr_ident_name(l)?;
    let count_idx = fd
        .params
        .iter()
        .position(|(n, t)| *n == count_name && t == "Int")?;

    if arms.len() != 2 {
        return None;
    }
    let mut true_body = None;
    let mut false_body = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
            Pattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
            _ => return None,
        }
    }
    // true arm: `[]`
    if !matches!(&true_body?.node, Expr::List(items) if items.is_empty()) {
        return None;
    }
    // false arm: `List.concat(f(.., n - 1, ..), [E])`
    let (concat, cargs) = cr_fn_call(false_body?)?;
    if concat != "List.concat" || cargs.len() != 2 {
        return None;
    }
    let (rec, rargs) = cr_fn_call(&cargs[0])?;
    if rec != fd.name || rargs.len() != fd.params.len() {
        return None;
    }
    // Recursive args: the count slot is `count - 1`; every other slot is its
    // parameter unchanged.
    for (j, ra) in rargs.iter().enumerate() {
        if j == count_idx {
            let Expr::BinOp(BinOp::Sub, sl, sr) = &ra.node else {
                return None;
            };
            if cr_ident_name(sl).as_deref() != Some(count_name.as_str()) || !cr_is_int_lit(sr, 1) {
                return None;
            }
        } else if cr_ident_name(ra).as_deref() != Some(fd.params[j].0.as_str()) {
            return None;
        }
    }
    let Expr::List(tail) = &cargs[1].node else {
        return None;
    };
    if tail.len() != 1 {
        return None;
    }
    let elem_lean = counted_repeat_elem_lean(&tail[0])?;
    Some(CountedRepeat {
        fn_name: fd.name.clone(),
        params: fd.params.clone(),
        count_idx,
        elem_lean,
    })
}

/// The Lean theorems for a counted-append advance, in dependency order: a fixed
/// `Int.natAbs` successor bridge, then the guarded advance proved by the
/// fuel-unfold template. Names/types use the same Lean mapping the program defs
/// are emitted with, so the theorem references the generated fn/fuel defs.
fn counted_repeat_lemmas(shape: &CountedRepeat, base: &str) -> Vec<(String, String)> {
    let f_l = crate::codegen::lean::aver_name_to_lean(&shape.fn_name);
    let fuel_l = crate::codegen::lean::aver_name_to_lean(
        &crate::codegen::recursion::fuel_helper_name(&shape.fn_name),
    );
    let count_name = &shape.params[shape.count_idx].0;
    let binders: Vec<String> = shape
        .params
        .iter()
        .map(|(p, t)| {
            format!(
                "({p} : {})",
                crate::codegen::lean::type_to_lean(&crate::codegen::common::parse_type_annotation(
                    t
                ))
            )
        })
        .collect();
    // Applied argument lists: lhs bumps the count slot to `count + 1`, rhs keeps it.
    let lhs_args: Vec<String> = shape
        .params
        .iter()
        .enumerate()
        .map(|(i, (p, _))| {
            if i == shape.count_idx {
                format!("({count_name} + 1)")
            } else {
                p.clone()
            }
        })
        .collect();
    let rhs_args: Vec<String> = shape.params.iter().map(|(p, _)| p.clone()).collect();
    let elem = &shape.elem_lean;
    let natabs = format!("{base}_natAbs_succ");
    let succ = format!("{base}_repeat_succ");

    let natabs_thm = format!(
        "theorem {natabs} (n : Int) (hn : 0 <= n) : Int.natAbs (n + 1) = Int.natAbs n + 1 := by\n  \
         apply Int.ofNat_inj.mp\n  \
         change (Int.natAbs (n + 1) : Int) = (Int.natAbs n : Int) + 1\n  \
         rw [Int.natAbs_of_nonneg (by omega), Int.natAbs_of_nonneg hn]\n"
    );
    let succ_thm = format!(
        "theorem {succ} {binders} (hn : 0 <= {count_name}) : \
         {f_l} {lhs_args} = {f_l} {rhs_args} ++ [{elem}] := by\n  \
         unfold {f_l}\n  \
         rw [{natabs} {count_name} hn]\n  \
         have hpos : ¬ {count_name} + 1 <= 0 := by omega\n  \
         simp [{fuel_l}, hpos]\n",
        binders = binders.join(" "),
        lhs_args = lhs_args.join(" "),
        rhs_args = rhs_args.join(" "),
    );
    vec![(natabs, natabs_thm), (succ, succ_thm)]
}

/// The strongest sound invariant a single Int accumulator field admits, by
/// structure. `Nonneg` is the conditional `0 <= field' given 0 <= field` (the
/// canonical, law-consumable one, generalized brick 2 — keys on the field's
/// update arithmetic, not the RLE step shape). `Bounded { lo, hi }` is the
/// unconditional two-sided step bound `acc.field + lo <= field' <= acc.field + hi`,
/// the generalization that also covers DECREASING fields (drain's `+1`/`-1`).
/// Per field nonneg is preferred when both hold.
enum FieldInvariant {
    Nonneg,
    Bounded { lo: i64, hi: i64 },
}

/// `acc.<field>` → the field name, if `e` is an attribute access of `acc`.
fn cr_attr_of(e: &crate::ast::Spanned<crate::ast::Expr>, acc: &str) -> Option<String> {
    if let crate::ast::Expr::Attr(obj, field) = &e.node
        && cr_ident_name(obj).as_deref() == Some(acc)
    {
        return Some(field.clone());
    }
    None
}

/// Collect every `RecordCreate` of `acc_type` reachable in `e` (through match
/// arms, operands, call args, …), as borrowed field lists.
fn collect_acc_records<'a>(
    e: &'a crate::ast::Spanned<crate::ast::Expr>,
    acc_type: &str,
    out: &mut Vec<&'a [(String, crate::ast::Spanned<crate::ast::Expr>)]>,
) {
    use crate::ast::Expr;
    match &e.node {
        Expr::RecordCreate { type_name, fields } if type_name == acc_type => {
            out.push(fields.as_slice());
        }
        Expr::Match { subject, arms } => {
            collect_acc_records(subject, acc_type, out);
            for arm in arms {
                collect_acc_records(&arm.body, acc_type, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_acc_records(l, acc_type, out);
            collect_acc_records(r, acc_type, out);
        }
        Expr::FnCall(_, args) => {
            for a in args {
                collect_acc_records(a, acc_type, out);
            }
        }
        Expr::Attr(obj, _) => collect_acc_records(obj, acc_type, out),
        Expr::List(items) => {
            for i in items {
                collect_acc_records(i, acc_type, out);
            }
        }
        _ => {}
    }
}

/// `true` if `v` keeps a `≥ 0` field `≥ 0`: a non-negative `Int` literal,
/// `acc.<field>` unchanged, `acc.<field> + <nonneg literal>`, or
/// `acc.<field> * <nonneg literal>`. All four are closed under `≥ 0` AND stay
/// linear in the field (multiplication is by a constant), so the `omega` leaf of
/// the proof template discharges the resulting goal.
fn nonneg_preserving(v: &crate::ast::Spanned<crate::ast::Expr>, acc: &str, field: &str) -> bool {
    use crate::ast::{BinOp, Expr, Literal};
    if let Expr::Literal(Literal::Int(k)) = &v.node {
        return *k >= 0;
    }
    if cr_attr_of(v, acc).as_deref() == Some(field) {
        return true;
    }
    // `field + nonneg` and `field * nonneg` both preserve non-negativity; a
    // negative literal factor is rejected here and handled (if at all) as a
    // bounded step. `field * field` is non-linear, so `nonneg_lit` excludes it.
    if let Expr::BinOp(BinOp::Add | BinOp::Mul, l, r) = &v.node {
        let is_field = |e: &crate::ast::Spanned<Expr>| cr_attr_of(e, acc).as_deref() == Some(field);
        let nonneg_lit = |e: &crate::ast::Spanned<Expr>| matches!(&e.node, Expr::Literal(Literal::Int(k)) if *k >= 0);
        return (is_field(l) && nonneg_lit(r)) || (nonneg_lit(l) && is_field(r));
    }
    false
}

/// Classify each Int field of a `step` fn `(Acc, …) -> Acc` (Acc a user record)
/// by the strongest sound invariant it admits, returning one entry PER field
/// that admits any — so a record with several Int fields (e.g. a nonneg counter
/// alongside a decreasing gauge) yields a lemma for each, not just the first.
/// Per field: the conditional nonneg invariant is preferred (law-consumable);
/// failing that, the unconditional bounded step when every update is a constant
/// shift of the field.
fn detect_field_invariants(
    fd: &crate::ast::FnDef,
    inputs: &ProofLowerInputs,
) -> Vec<(String, FieldInvariant)> {
    use crate::ast::{Stmt, TypeDef};
    // step : (Acc, …) -> Acc, Acc a user record.
    if fd.params.is_empty() || fd.params[0].1 != fd.return_type {
        return Vec::new();
    }
    let acc_param = fd.params[0].0.as_str();
    let acc_type = &fd.params[0].1;
    let Some(TypeDef::Product {
        fields: acc_fields, ..
    }) = inputs.find_type_def(acc_type)
    else {
        return Vec::new();
    };
    let int_fields: Vec<&str> = acc_fields
        .iter()
        .filter(|(_, ty)| ty == "Int")
        .map(|(n, _)| n.as_str())
        .collect();

    let Some(Stmt::Expr(term)) = fd.body.stmts().last() else {
        return Vec::new();
    };
    let mut records = Vec::new();
    collect_acc_records(term, acc_type, &mut records);
    if records.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    for f in int_fields {
        // The value this field is assigned in each built record. Every Aver
        // record build is total, so a field missing from any record is anomalous
        // — skip it rather than reason about a partial update.
        let values: Vec<&crate::ast::Spanned<crate::ast::Expr>> = records
            .iter()
            .filter_map(|rec| rec.iter().find(|(name, _)| name == f).map(|(_, v)| v))
            .collect();
        if values.len() != records.len() {
            continue;
        }
        // Prefer the conditional nonneg invariant…
        if values.iter().all(|v| nonneg_preserving(v, acc_param, f)) {
            out.push((f.to_string(), FieldInvariant::Nonneg));
            continue;
        }
        // …otherwise the unconditional bounded step, if every update is a
        // constant shift of the field.
        let deltas: Option<Vec<i64>> = values
            .iter()
            .map(|v| relative_delta(v, acc_param, f))
            .collect();
        if let Some(deltas) = deltas
            && !deltas.is_empty()
        {
            out.push((
                f.to_string(),
                FieldInvariant::Bounded {
                    lo: deltas.iter().copied().min().unwrap(),
                    hi: deltas.iter().copied().max().unwrap(),
                },
            ));
        }
    }
    out
}

/// Lean binders `(p : T) …` and the matching applied-argument list `p …` for a
/// step fn's parameters, in the canonical Lean type mapping.
fn lemma_binders_args(params: &[(String, String)]) -> (String, String) {
    let mut binders = Vec::new();
    let mut args = Vec::new();
    for (pname, ptype) in params {
        let ty_l = crate::codegen::lean::type_to_lean(
            &crate::codegen::common::parse_type_annotation(ptype),
        );
        binders.push(format!("({pname} : {ty_l})"));
        args.push(pname.clone());
    }
    (binders.join(" "), args.join(" "))
}

/// The Lean theorem for a field's conditional nonneg invariant. The proof is
/// SHAPE-AGNOSTIC: unfold the step, split on whatever branches it has, and close
/// each leaf with `omega` (every leaf is a non-negative literal, `acc.field +
/// nonneg`, or `acc.field * nonneg`, given the hypothesis) — no dependence on
/// the RLE discriminants.
fn nonneg_lemma(
    fn_name: &str,
    params: &[(String, String)],
    field: &str,
    base: &str,
) -> (String, String) {
    let step_l = crate::codegen::lean::aver_name_to_lean(fn_name);
    let (binders, args) = lemma_binders_args(params);
    let acc = &params[0].0;
    let name = format!("{base}_{field}_nonneg");
    let text = format!(
        "theorem {name} {binders} (hcount : 0 <= {acc}.{field}) : \
         0 <= ({step_l} {args}).{field} := by\n  \
         unfold {step_l}\n  \
         split <;> (try split) <;> simp_all <;> omega\n",
    );
    (name, text)
}

/// `Some(d)` if `v` is a RELATIVE update of `acc.<field>`: the field unchanged
/// (`d = 0`), `acc.<field> + K` / `K + acc.<field>` (`d = K`), or
/// `acc.<field> - K` (`d = -K`), with `K` an `Int` literal. `None` for a reset
/// to a literal, an unrelated param (`x`), or any other shape — i.e. anything
/// not expressible as `field` shifted by a constant. `K` may be negative, so
/// this admits steps that DECREASE the field (unlike [`nonneg_preserving`]).
fn relative_delta(
    v: &crate::ast::Spanned<crate::ast::Expr>,
    acc: &str,
    field: &str,
) -> Option<i64> {
    use crate::ast::{BinOp, Expr, Literal};
    if cr_attr_of(v, acc).as_deref() == Some(field) {
        return Some(0);
    }
    let Expr::BinOp(op, l, r) = &v.node else {
        return None;
    };
    let is_field = |e: &crate::ast::Spanned<Expr>| cr_attr_of(e, acc).as_deref() == Some(field);
    let int_lit = |e: &crate::ast::Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(k)) => Some(*k),
        _ => None,
    };
    match op {
        // field + K  or  K + field
        BinOp::Add if is_field(l) => int_lit(r),
        BinOp::Add if is_field(r) => int_lit(l),
        // field - K   (K - field is NOT a constant shift of the field)
        BinOp::Sub if is_field(l) => int_lit(r).map(|k| -k),
        _ => None,
    }
}

/// Render `acc.field` shifted by a (possibly negative) literal: `acc.f`,
/// `acc.f + k`, or `acc.f - k`.
fn render_acc_offset(acc: &str, field: &str, k: i64) -> String {
    match k.cmp(&0) {
        std::cmp::Ordering::Equal => format!("{acc}.{field}"),
        std::cmp::Ordering::Greater => format!("{acc}.{field} + {k}"),
        std::cmp::Ordering::Less => format!("{acc}.{field} - {}", -k),
    }
}

/// The Lean theorem for a field's bounded step: an UNCONDITIONAL two-sided bound,
/// proved by the same shape-agnostic template as the nonneg invariant — unfold
/// the step, split on its branches, and `omega` closes each leaf (here a
/// conjunction of two linear bounds, every branch being `field + d` with `d` in
/// `[lo, hi]`).
fn bounded_lemma(
    fn_name: &str,
    params: &[(String, String)],
    field: &str,
    lo: i64,
    hi: i64,
    base: &str,
) -> (String, String) {
    let step_l = crate::codegen::lean::aver_name_to_lean(fn_name);
    let (binders, args) = lemma_binders_args(params);
    let acc = &params[0].0;
    let lo_s = render_acc_offset(acc, field, lo);
    let hi_s = render_acc_offset(acc, field, hi);
    let name = format!("{base}_{field}_bounds");
    let text = format!(
        "theorem {name} {binders} : \
         {lo_s} <= ({step_l} {args}).{field} ∧ ({step_l} {args}).{field} <= {hi_s} := by\n  \
         unfold {step_l}\n  \
         split <;> (try split) <;> simp_all <;> omega\n",
    );
    (name, text)
}

/// Structure-directed GUARDED lemma groups (layer 3): conjectures derived from
/// a recognized recursion shape rather than blind enumeration. Each group is a
/// set of co-dependent Lean theorems proved TOGETHER (one `lake build`, in
/// dependency order). Families: the counted-repeat advance (`repeat_succ`,
/// brick 1, a co-dependent pair) and the per-field accumulator invariants —
/// `<field>_nonneg` (the conditional `0 <= field`, generalized brick 2) and,
/// where nonneg does not hold, `<field>_bounds` (the unconditional two-sided
/// step bound, which admits DECREASING fields like drain's `+1`/`-1` counter,
/// never the false `0 <= n`). Each field invariant is its OWN single-theorem
/// group, so independent invariants never share a build fate, and a record with
/// several Int fields yields a lemma for each.
pub fn structural_lemma_groups(inputs: &ProofLowerInputs) -> Vec<Vec<(String, String)>> {
    // A running, globally-unique base index across both group kinds — bases need
    // only be distinct (so names never clash in the committed artifact), not
    // positional, so the two passes can share one counter.
    let mut next_base = 0usize;
    let mut next = |kind: &str| {
        let b = format!("aver_{kind}_{next_base}");
        next_base += 1;
        b
    };

    // Pass 1: the relational roundtrip chains. Emitted FIRST so pass 2 knows
    // which standalone bricks a chain already re-proves internally and can skip
    // them (dedup) — but appended LAST, to keep the standalone-first artifact
    // order. Each chain is self-contained (its own homomorphism / counted-repeat
    // / nonneg copies under a unique `base`), so it builds with one `lake build`.
    let mut relational: Vec<Vec<(String, String)>> = Vec::new();
    let mut covered_counted: HashSet<String> = HashSet::new();
    let mut covered_nonneg: HashSet<(String, String)> = HashSet::new();
    for enc in detect_encoders(inputs) {
        let base = next("relational");
        if let Some((group, cov)) = relational_lemma_group(&enc, inputs, &base) {
            covered_counted.insert(cov.counted_fn);
            covered_nonneg.insert((cov.step_fn, cov.field));
            relational.push(group);
        }
    }
    // The monoidal flavor of the same accumulator-generalization schema:
    // `wrapper(xs) = direct(xs)` for an additive tail-recursive fold (sum vs
    // sumDirect). Shares `loop_gen`'s induct-and-instantiate skeleton with the
    // codec roundtrip; closes with `omega` instead of the codec step bricks.
    for shape in detect_monoidal_specs(inputs) {
        let base = next("monoidal");
        relational.push(monoidal_spec_group(&shape, &base));
    }

    // Pass 2: standalone structure-directed bricks, SKIPPING any a relational
    // chain already subsumes (the chain's counted-repeat helper and its
    // step's nonneg field) — no double-proving the same lemma under two names.
    let mut groups: Vec<Vec<(String, String)>> = Vec::new();
    for fd in inputs.pure_fns() {
        if let Some(shape) = detect_counted_repeat(fd) {
            if !covered_counted.contains(&fd.name) {
                let base = next("structural");
                groups.push(counted_repeat_lemmas(&shape, &base));
            }
            continue;
        }
        for (field, inv) in detect_field_invariants(fd, inputs) {
            if matches!(inv, FieldInvariant::Nonneg)
                && covered_nonneg.contains(&(fd.name.clone(), field.clone()))
            {
                continue;
            }
            let base = next("structural");
            let lemma = match inv {
                FieldInvariant::Nonneg => nonneg_lemma(&fd.name, &fd.params, &field, &base),
                FieldInvariant::Bounded { lo, hi } => {
                    bounded_lemma(&fd.name, &fd.params, &field, lo, hi, &base)
                }
            };
            groups.push(vec![lemma]);
        }
    }

    groups.extend(relational);
    groups
}

/// The function-call name + args of `e`, whether it is a direct `FnCall` or a
/// tail call the TCO pass rewrote into a `TailCall` node (the loop/wrapper/finish
/// bodies are all tail position, so they arrive as `TailCall`).
fn as_call(
    e: &crate::ast::Spanned<crate::ast::Expr>,
) -> Option<(String, &[crate::ast::Spanned<crate::ast::Expr>])> {
    match &e.node {
        crate::ast::Expr::FnCall(..) => cr_fn_call(e),
        crate::ast::Expr::TailCall(tc) => Some((tc.target.clone(), tc.args.as_slice())),
        _ => None,
    }
}

/// The loop roles `analysis::shape` extracted for `wrapper`'s `AccumulatorFold`
/// — the loop fn plus its step (named `step_fn` or inline `step_op`) and finish
/// (named `finish_fn` or identity). Both fold-flavor detectors source these from
/// the shared shape vocabulary instead of re-walking the loop body; `None` when
/// no program shape is available or the wrapper isn't an accumulator fold.
struct ShapeFoldRoles {
    loop_fn: String,
    step_fn: Option<String>,
    step_op: Option<crate::ast::BinOp>,
    finish_fn: Option<String>,
}

fn shape_fold_roles(inputs: &ProofLowerInputs, wrapper: &str) -> Option<ShapeFoldRoles> {
    use crate::analysis::shape::ModulePattern;
    inputs.program_shape?.patterns.iter().find_map(|p| match p {
        ModulePattern::AccumulatorFold {
            wrapper_fn,
            loop_fn,
            step_fn,
            step_op,
            finish_fn,
            ..
        } if wrapper_fn == wrapper => Some(ShapeFoldRoles {
            loop_fn: loop_fn.clone(),
            step_fn: step_fn.clone(),
            step_op: *step_op,
            finish_fn: finish_fn.clone(),
        }),
        _ => None,
    })
}

/// The single trailing expression of a fn body, if it is `… = expr` shaped.
fn last_body_expr(fd: &crate::ast::FnDef) -> Option<&crate::ast::Spanned<crate::ast::Expr>> {
    match fd.body.stmts().last() {
        Some(crate::ast::Stmt::Expr(e)) => Some(e),
        _ => None,
    }
}

/// The structural roles of a fold-encoder-with-inverse, recovered from a
/// roundtrip law `inverse(wrapper(var)) = var`. This is the substrate for the
/// RELATIONAL bricks (flush_fold_step / loop_gen / roundtrip): once the roles
/// are known, the proof chain is the encoder-agnostic template proven (by hand,
/// axiom-free) on BOTH rle and sparse. Detection is by AST shape, so it fires on
/// any encoder of this family, not a single program.
#[derive(Debug, Clone)]
pub(super) struct EncoderShape {
    /// `encode` — the law subject; `wrapper(var) = loop(var, initAcc)`.
    pub(super) wrapper: String,
    /// `decode` — the homomorphic inverse; `[] -> []`, `h::t -> concat(expand h, inverse t)`.
    pub(super) inverse: String,
    /// `encodeLoop` — `[] -> finish acc`, `h::t -> loop t (step acc h)`.
    pub(super) loop_fn: String,
    /// `flushAcc` — closes the accumulator into the encoded form.
    pub(super) finish: String,
    /// `encodeFold` — the fold step `(acc, x) -> acc`.
    pub(super) step: String,
    /// `expandRun` — decodes one encoded element back to a list.
    pub(super) expand: String,
    /// The law's universally-quantified input list variable. Used to validate
    /// the law shape during detection and surfaced in the role-detector test;
    /// the emitter re-quantifies over a fresh `xs`, so it reads this only there.
    #[allow(dead_code)]
    pub(super) var: String,
    /// The neutral accumulator from `wrapper`'s body (`loop(var, initAcc)`),
    /// rendered to a Lean record literal (`{ runs := [], current := "", count := 0 }`).
    init_acc: String,
}

/// Recognize a fold-encoder-with-inverse from one roundtrip law (see
/// [`EncoderShape`]). Returns `None` unless every role lines up by structure.
fn detect_encoder(
    law: &crate::ast::VerifyLaw,
    subject_fn: &str,
    inputs: &ProofLowerInputs,
) -> Option<EncoderShape> {
    use crate::ast::{Expr, Pattern};

    // Law shape: inverse(wrapper(var)) = var, with var a declared given.
    let var = cr_ident_name(&law.rhs)?;
    if !law.givens.iter().any(|g| g.name == var) {
        return None;
    }
    let (inverse, inv_args) = as_call(&law.lhs)?;
    if inv_args.len() != 1 {
        return None;
    }
    let (wrapper, wrap_args) = as_call(&inv_args[0])?;
    if wrapper != subject_fn
        || wrap_args.len() != 1
        || cr_ident_name(&wrap_args[0]).as_deref() != Some(var.as_str())
    {
        return None;
    }

    // wrapper(var) = loop(var, initAcc). Shape-anchored: `analysis::shape` must
    // classify `wrapper` as an `AccumulatorFold`, and the codec flavor's loop
    // has a NAMED step + NAMED finish (the inline-op / identity-finish forms are
    // the monoidal flavor). The roles come from the shape pattern — no loop-body
    // re-walk here.
    let roles = shape_fold_roles(inputs, &wrapper)?;
    let loop_fn = roles.loop_fn;
    let (Some(step), Some(finish)) = (roles.step_fn, roles.finish_fn) else {
        return None;
    };
    let wf = inputs.find_fn_def_by_call_name(&wrapper)?;
    let (wbody_loop, loop_args) = as_call(last_body_expr(wf)?)?;
    if wbody_loop != loop_fn || loop_args.len() != 2 {
        return None;
    }
    // The neutral accumulator (2nd loop arg) must render to a Lean literal — the
    // roundtrip lemma instantiates `loop_gen` at it. A non-literal init is out
    // of this family's scope (graceful skip).
    let init_acc = render_init_literal(&loop_args[1])?;

    // inverse(list): match list { [] -> []; h::t -> List.concat(expand h, inverse t) }.
    let invf = inputs.find_fn_def_by_call_name(&inverse)?;
    if invf.params.len() != 1 {
        return None;
    }
    let inv_list_p = invf.params[0].0.as_str();
    let Expr::Match {
        subject: isubj,
        arms: iarms,
    } = &last_body_expr(invf)?.node
    else {
        return None;
    };
    if cr_ident_name(isubj).as_deref() != Some(inv_list_p) || iarms.len() != 2 {
        return None;
    }
    let mut expand: Option<String> = None;
    for arm in iarms {
        match &arm.pattern {
            Pattern::EmptyList => {
                if !matches!(&arm.body.node, Expr::List(items) if items.is_empty()) {
                    return None;
                }
            }
            Pattern::Cons(h, t) => {
                let (concat, cargs) = as_call(&arm.body)?;
                if concat != "List.concat" || cargs.len() != 2 {
                    return None;
                }
                let (e, eargs) = as_call(&cargs[0])?;
                if eargs.len() != 1 || cr_ident_name(&eargs[0]).as_deref() != Some(h.as_str()) {
                    return None;
                }
                let (inv2, iargs) = as_call(&cargs[1])?;
                if inv2 != inverse
                    || iargs.len() != 1
                    || cr_ident_name(&iargs[0]).as_deref() != Some(t.as_str())
                {
                    return None;
                }
                expand = Some(e);
            }
            _ => return None,
        }
    }

    Some(EncoderShape {
        wrapper,
        inverse,
        loop_fn,
        finish,
        step,
        expand: expand?,
        var,
        init_acc,
    })
}

/// Every fold-encoder-with-inverse recovered from the entry module's roundtrip
/// laws. The substrate for emitting the relational brick chain.
pub(super) fn detect_encoders(inputs: &ProofLowerInputs) -> Vec<EncoderShape> {
    use crate::ast::{TopLevel, VerifyKind};
    let mut out = Vec::new();
    for item in inputs.entry_items {
        if let TopLevel::Verify(vb) = item
            && let VerifyKind::Law(law) = &vb.kind
            && let Some(enc) = detect_encoder(law, &vb.fn_name, inputs)
        {
            out.push(enc);
        }
    }
    out
}

// ===========================================================================
// Layer 3 (relational bricks) — the full roundtrip chain per detected encoder.
//
// Once `detect_encoder` recovers the roles (wrapper/inverse/loop/finish/step/
// expand + the counted-repeat helper), the proof of `inverse(wrapper xs) = xs`
// is the ENCODER-AGNOSTIC template proven by hand (axiom-free) on BOTH rle and
// sparse — the durable record is `prompts/relational-ground-truth-{rle,sparse}
// .lean`. This emits that chain as ONE self-contained dependency-ordered group
// (so `prove_discovered_lemmas_lean` can build it with a single `lake build`):
//
//   inv_append → counted_one → natAbs_succ → counted_succ → field_nonneg
//             → flush_fold_step → loop_gen → roundtrip
//
// The crux is `flush_fold_step`: a SINGLE tactic
//   `unfold step finish; split <;> (try split) <;> (try split) <;>
//    simp_all [inverse, expand, inv_append, counted_one, counted_succ,
//    beq_iff_eq] <;> omega`
// closes BOTH encoders — the only per-encoder difference (number of guard
// splits) is absorbed by the `(try split)` chain.
// ===========================================================================

/// Render a literal-valued expression to Lean for the neutral accumulator:
/// empty/literal lists, `Int`/`Bool`/`String`/`Unit` literals, and nested
/// record literals (`{ field := value, … }`). `None` for anything else — a
/// non-literal init is out of the roundtrip family's scope.
fn render_init_literal(e: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    use crate::ast::{Expr, Literal};
    match &e.node {
        Expr::List(items) => {
            let parts: Option<Vec<String>> = items.iter().map(render_init_literal).collect();
            Some(format!("[{}]", parts?.join(", ")))
        }
        Expr::Literal(Literal::Int(k)) => Some(k.to_string()),
        Expr::Literal(Literal::Bool(b)) => Some(if *b { "true" } else { "false" }.to_string()),
        Expr::Literal(Literal::Str(s)) => Some(format!(
            "\"{}\"",
            s.replace('\\', "\\\\").replace('"', "\\\"")
        )),
        Expr::Literal(Literal::Unit) => Some("()".to_string()),
        Expr::RecordCreate { fields, .. } => {
            let parts: Option<Vec<String>> = fields
                .iter()
                .map(|(name, v)| Some(format!("{name} := {}", render_init_literal(v)?)))
                .collect();
            Some(format!("{{ {} }}", parts?.join(", ")))
        }
        _ => None,
    }
}

/// Collect every fn-call callee name reachable in an expression (direct or
/// TCO-rewritten tail call). Used to find the counted-repeat helper an
/// `expand` body calls.
fn collect_callees(e: &crate::ast::Spanned<crate::ast::Expr>, out: &mut BTreeSet<String>) {
    use crate::ast::Expr;
    match &e.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node) {
                out.insert(name);
            }
            for a in args {
                collect_callees(a, out);
            }
        }
        Expr::TailCall(tc) => {
            out.insert(tc.target.clone());
            for a in &tc.args {
                collect_callees(a, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_callees(subject, out);
            for arm in arms {
                collect_callees(&arm.body, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_callees(l, out);
            collect_callees(r, out);
        }
        Expr::Neg(x) | Expr::Attr(x, _) | Expr::ErrorProp(x) => collect_callees(x, out),
        Expr::List(items) | Expr::Tuple(items) => {
            for i in items {
                collect_callees(i, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_callees(v, out);
            }
        }
        _ => {}
    }
}

/// `<base>_counted_one : <counted> <fixed-args> 1 = [<elem>]` — the unit case of
/// the counted-repeat helper (rle `repeat' c 1 = [c]` with a `(c : String)`
/// binder; sparse `repeat0 1 = [0]` with none). Non-count params become binders;
/// the count slot is fixed to `1`. Proof: `simp [<counted>, <counted>__fuel]`.
fn counted_one_lemma(shape: &CountedRepeat, base: &str) -> (String, String) {
    let f_l = crate::codegen::lean::aver_name_to_lean(&shape.fn_name);
    let fuel_l = crate::codegen::lean::aver_name_to_lean(
        &crate::codegen::recursion::fuel_helper_name(&shape.fn_name),
    );
    let binders: Vec<String> = shape
        .params
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != shape.count_idx)
        .map(|(_, (p, t))| {
            format!(
                "({p} : {})",
                crate::codegen::lean::type_to_lean(&crate::codegen::common::parse_type_annotation(
                    t
                ))
            )
        })
        .collect();
    let args: Vec<String> = shape
        .params
        .iter()
        .enumerate()
        .map(|(i, (p, _))| {
            if i == shape.count_idx {
                "1".to_string()
            } else {
                p.clone()
            }
        })
        .collect();
    let name = format!("{base}_counted_one");
    let binders_s = binders.join(" ");
    let text = format!(
        "theorem {name} {binders_s} : {f_l} {args} = [{elem}] := by\n  simp [{f_l}, {fuel_l}]\n",
        args = args.join(" "),
        elem = shape.elem_lean,
    );
    (name, text)
}

/// `<base>_inv_append (a b : List <Elem>) : <inverse> (a ++ b) = <inverse> a ++
/// <inverse> b` — the inverse is a list homomorphism, proved by induction on `a`.
fn inv_append_lemma(inverse: &str, elem_ty_lean: &str, base: &str) -> (String, String) {
    let inv_l = crate::codegen::lean::aver_name_to_lean(inverse);
    let name = format!("{base}_inv_append");
    let text = format!(
        "theorem {name} (a b : List {elem_ty_lean}) : \
         {inv_l} (a ++ b) = {inv_l} a ++ {inv_l} b := by\n  \
         induction a with\n  \
         | nil => simp [{inv_l}]\n  \
         | cons x xs ih => simp [{inv_l}, ih, List.append_assoc]\n",
    );
    (name, text)
}

/// **The crux.** `<base>_flush_fold_step (acc) (x) (h : 0 <= acc.<field>) :
/// <inverse> (<finish> (<step> acc x)) = <inverse> (<finish> acc) ++ [x]`.
/// The single generic tactic (see module header) that closes BOTH rle and
/// sparse: unfold step+finish, split out every guard, then `simp_all` with the
/// homomorphism / counted-repeat lemmas and `omega` for the arithmetic guards.
fn flush_fold_step_lemma(
    enc: &EncoderShape,
    acc_ty_lean: &str,
    x_ty_lean: &str,
    field: &str,
    base: &str,
) -> (String, String) {
    let inv_l = crate::codegen::lean::aver_name_to_lean(&enc.inverse);
    let step_l = crate::codegen::lean::aver_name_to_lean(&enc.step);
    let finish_l = crate::codegen::lean::aver_name_to_lean(&enc.finish);
    let expand_l = crate::codegen::lean::aver_name_to_lean(&enc.expand);
    let name = format!("{base}_flush_fold_step");
    let text = format!(
        "theorem {name} (acc : {acc_ty_lean}) (x : {x_ty_lean}) (h : 0 <= acc.{field}) :\n    \
         {inv_l} ({finish_l} ({step_l} acc x)) = {inv_l} ({finish_l} acc) ++ [x] := by\n  \
         unfold {step_l} {finish_l}\n  \
         split <;> (try split) <;> (try split) <;>\n    \
         simp_all [{inv_l}, {expand_l}, {base}_inv_append, {base}_counted_one, {base}_repeat_succ, beq_iff_eq] <;>\n    \
         omega\n",
    );
    (name, text)
}

/// `<base>_loop_gen : ∀ list acc, 0 <= acc.<field> → <inverse> (<loop> list acc)
/// = <inverse> (<finish> acc) ++ list` — the strengthened (over-the-accumulator)
/// loop invariant; induction on `list`, the cons case rewriting by the field's
/// nonneg invariant then `flush_fold_step`. IDENTICAL across encoders.
fn loop_gen_lemma(
    enc: &EncoderShape,
    acc_ty_lean: &str,
    x_ty_lean: &str,
    field: &str,
    base: &str,
) -> (String, String) {
    let inv_l = crate::codegen::lean::aver_name_to_lean(&enc.inverse);
    let loop_l = crate::codegen::lean::aver_name_to_lean(&enc.loop_fn);
    let finish_l = crate::codegen::lean::aver_name_to_lean(&enc.finish);
    let step_l = crate::codegen::lean::aver_name_to_lean(&enc.step);
    let name = format!("{base}_loop_gen");
    let text = format!(
        "theorem {name} : ∀ (list : List {x_ty_lean}) (acc : {acc_ty_lean}), 0 <= acc.{field} →\n    \
         {inv_l} ({loop_l} list acc) = {inv_l} ({finish_l} acc) ++ list := by\n  \
         intro list\n  \
         induction list with\n  \
         | nil => intro acc _; simp [{loop_l}]\n  \
         | cons c rest ih =>\n    \
         intro acc h\n    \
         simp only [{loop_l}]\n    \
         rw [ih ({step_l} acc c) ({base}_{field}_nonneg acc c h), {base}_flush_fold_step acc c h]\n    \
         simp\n",
    );
    (name, text)
}

/// `<base>_roundtrip (xs : List <X>) : <inverse> (<wrapper> xs) = xs` — the law
/// itself, instantiating `loop_gen` at the neutral accumulator (`0 <= 0` by
/// `decide`) and discharging the empty-accumulator base by `simp`.
fn roundtrip_lemma(enc: &EncoderShape, x_ty_lean: &str, base: &str) -> (String, String) {
    let inv_l = crate::codegen::lean::aver_name_to_lean(&enc.inverse);
    let wrapper_l = crate::codegen::lean::aver_name_to_lean(&enc.wrapper);
    let finish_l = crate::codegen::lean::aver_name_to_lean(&enc.finish);
    let name = format!("{base}_roundtrip");
    let text = format!(
        "theorem {name} (xs : List {x_ty_lean}) : {inv_l} ({wrapper_l} xs) = xs := by\n  \
         unfold {wrapper_l}\n  \
         rw [{base}_loop_gen xs {init_acc} (by decide)]\n  \
         simp [{finish_l}, {inv_l}]\n",
        init_acc = enc.init_acc,
    );
    (name, text)
}

/// The standalone structural bricks a relational chain re-proves internally, so
/// `structural_lemma_groups` can skip emitting them a second time: the
/// counted-repeat helper fn and the `(step fn, nonneg field)` the chain covers.
struct RelationalCoverage {
    counted_fn: String,
    step_fn: String,
    field: String,
}

/// Emit the full relational brick chain for one detected encoder as a single
/// dependency-ordered group (one `lake build`), plus the [`RelationalCoverage`]
/// it subsumes. Returns `None` if any required role can't be resolved (the
/// step's accumulator/element types, the nonneg count field, the inverse's
/// element type, or the counted-repeat helper the `expand` body calls) — a
/// graceful skip, never a partial chain.
fn relational_lemma_group(
    enc: &EncoderShape,
    inputs: &ProofLowerInputs,
    base: &str,
) -> Option<(Vec<(String, String)>, RelationalCoverage)> {
    // step : (Acc, X) -> Acc — the accumulator and folded-element types.
    let step_fd = inputs.find_fn_def_by_call_name(&enc.step)?;
    if step_fd.params.len() != 2 {
        return None;
    }
    let acc_ty_lean = crate::codegen::lean::type_to_lean(
        &crate::codegen::common::parse_type_annotation(&step_fd.params[0].1),
    );
    let x_ty_lean = crate::codegen::lean::type_to_lean(
        &crate::codegen::common::parse_type_annotation(&step_fd.params[1].1),
    );

    // The counted accumulator field — the one with the nonneg invariant the
    // loop generalization threads (rle `count`, sparse `pending`).
    let field = detect_field_invariants(step_fd, inputs)
        .into_iter()
        .find_map(|(f, inv)| matches!(inv, FieldInvariant::Nonneg).then_some(f))?;

    // The inverse's list element type (rle `Run`, sparse `Token`).
    let inverse_fd = inputs.find_fn_def_by_call_name(&enc.inverse)?;
    let Type::List(elem) =
        crate::codegen::common::parse_type_annotation(&inverse_fd.params.first()?.1)
    else {
        return None;
    };
    let elem_ty_lean = crate::codegen::lean::type_to_lean(&elem);

    // The counted-repeat helper the `expand` body calls (rle `repeat`, sparse
    // `repeat0`) — find it by recognizing the shape on a callee of `expand`.
    let expand_fd = inputs.find_fn_def_by_call_name(&enc.expand)?;
    let mut callees = BTreeSet::new();
    if let Some(body) = last_body_expr(expand_fd) {
        collect_callees(body, &mut callees);
    }
    let cr = inputs
        .pure_fns()
        .iter()
        .filter(|fd| callees.contains(&fd.name))
        .find_map(|fd| detect_counted_repeat(fd))?;

    // The chain, in dependency order (one self-contained group). `counted_one`
    // and `counted_repeat_lemmas` (natAbs_succ + repeat_succ) feed
    // `flush_fold_step`; `nonneg_lemma` and `flush_fold_step` feed `loop_gen`;
    // `loop_gen` feeds `roundtrip`. All names share `base`, so the group never
    // collides with other groups in the committed artifact.
    let mut group = Vec::new();
    group.push(inv_append_lemma(&enc.inverse, &elem_ty_lean, base));
    group.push(counted_one_lemma(&cr, base));
    group.extend(counted_repeat_lemmas(&cr, base));
    group.push(nonneg_lemma(&enc.step, &step_fd.params, &field, base));
    group.push(flush_fold_step_lemma(
        enc,
        &acc_ty_lean,
        &x_ty_lean,
        &field,
        base,
    ));
    group.push(loop_gen_lemma(enc, &acc_ty_lean, &x_ty_lean, &field, base));
    group.push(roundtrip_lemma(enc, &x_ty_lean, base));
    let coverage = RelationalCoverage {
        counted_fn: cr.fn_name.clone(),
        step_fn: enc.step.clone(),
        field,
    };
    Some((group, coverage))
}

// ===========================================================================
// The accumulator-generalization schema, MONOIDAL flavor.
//
// The codec roundtrip above and the monoidal spec-equivalence here are the two
// flavors of ONE schema: "a tail-recursive fold equals a direct spec, proved by
// strengthening the IH over the threaded accumulator". The shared skeleton is
// `loop_gen` — induct on the list, instantiate at the neutral accumulator:
//
//   codec:    inverse (loop list acc) = inverse (finish acc) ++ list   (combine = ++,  spec = id)
//   monoidal: loop list acc           = acc + direct list              (combine = +,   spec = direct)
//
// Only the per-element step lemma and the closer differ (flush_fold_step + the
// counted/nonneg bricks for the codec; a plain `omega` for the additive
// monoid). This flavor proves `wrapper(xs) = direct(xs)` for the canonical
// `sum(xs) = sumTR(xs, 0)` vs `sumDirect(xs)` shape. Additive ops only — `omega`
// closes `+`/`-`; a multiplicative monoid would need `ring` (Mathlib), out of
// scope for core-Lean export.
// ===========================================================================

/// A monoidal wrapper-over-recursion: a non-recursive `wrapper(xs) = loop(xs,
/// neutral)` whose `loop` folds the list into an accumulator via an additive
/// step, paired with a `direct` structural recurrence the law equates it to.
struct MonoidalShape {
    /// `sum` — the law subject; `wrapper(xs) = loop(xs, neutral)`.
    wrapper: String,
    /// `sumTR` — the accumulator fold `[] -> acc`, `h::t -> loop t (acc + h)`.
    loop_fn: String,
    /// `sumDirect` — the direct recurrence `[] -> 0`, `h::t -> h + direct t`.
    direct: String,
    /// Lean type of the list element (`Int`).
    x_ty_lean: String,
    /// Lean type of the accumulator (`Int`).
    acc_ty_lean: String,
    /// The neutral accumulator from the wrapper body, rendered (`0`).
    neutral: String,
}

/// Recognize a monoidal wrapper-over-recursion from a spec-equivalence law
/// `wrapper(var) = direct(var)` (see [`MonoidalShape`]). `None` unless the fold
/// is the additive accumulator shape the `omega`-closed template proves.
fn detect_monoidal_spec(
    law: &crate::ast::VerifyLaw,
    subject_fn: &str,
    inputs: &ProofLowerInputs,
) -> Option<MonoidalShape> {
    use crate::ast::Expr;

    // Law: wrapper(var) = direct(var), var a declared given, both sides unary.
    let (lf, largs) = as_call(&law.lhs)?;
    let (rf, rargs) = as_call(&law.rhs)?;
    if largs.len() != 1 || rargs.len() != 1 {
        return None;
    }
    let var = cr_ident_name(&largs[0])?;
    if cr_ident_name(&rargs[0]).as_deref() != Some(var.as_str())
        || !law.givens.iter().any(|g| g.name == var)
    {
        return None;
    }
    // The subject is the wrapper; the other side is the direct spec.
    let (wrapper, direct) = if lf == subject_fn {
        (lf, rf)
    } else if rf == subject_fn {
        (rf, lf)
    } else {
        return None;
    };

    // wrapper(var) = loop(var, neutral). Shape-anchored: the monoidal flavor is
    // an `AccumulatorFold` with an INLINE additive step (`acc + h`) and an
    // IDENTITY finish (nil arm returns `acc`). Roles from the shape pattern —
    // no loop-body re-walk.
    let roles = shape_fold_roles(inputs, &wrapper)?;
    let loop_fn = roles.loop_fn;
    if roles.finish_fn.is_some() || roles.step_op != Some(crate::ast::BinOp::Add) {
        return None;
    }
    let wf = inputs.find_fn_def_by_call_name(&wrapper)?;
    let (wbody_loop, loop_args) = as_call(last_body_expr(wf)?)?;
    if wbody_loop != loop_fn
        || loop_args.len() != 2
        || cr_ident_name(&loop_args[0]).as_deref() != Some(var.as_str())
    {
        return None;
    }
    let neutral = render_init_literal(&loop_args[1])?;

    // The loop's element + accumulator types, for the lemma binders.
    let lpf = inputs.find_fn_def_by_call_name(&loop_fn)?;
    if lpf.params.len() != 2 {
        return None;
    }
    let Type::List(elem) = crate::codegen::common::parse_type_annotation(&lpf.params[0].1) else {
        return None;
    };
    let x_ty_lean = crate::codegen::lean::type_to_lean(&elem);
    let acc_ty_lean = crate::codegen::lean::type_to_lean(
        &crate::codegen::common::parse_type_annotation(&lpf.params[1].1),
    );

    // The direct spec must exist and be a structural recurrence simp can unfold.
    let df = inputs.find_fn_def_by_call_name(&direct)?;
    if !matches!(
        last_body_expr(df).map(|e| &e.node),
        Some(Expr::Match { .. })
    ) {
        return None;
    }

    Some(MonoidalShape {
        wrapper,
        loop_fn,
        direct,
        x_ty_lean,
        acc_ty_lean,
        neutral,
    })
}

/// Every monoidal wrapper-over-recursion recovered from the entry module's laws.
fn detect_monoidal_specs(inputs: &ProofLowerInputs) -> Vec<MonoidalShape> {
    use crate::ast::{TopLevel, VerifyKind};
    let mut out = Vec::new();
    for item in inputs.entry_items {
        if let TopLevel::Verify(vb) = item
            && let VerifyKind::Law(law) = &vb.kind
            && let Some(s) = detect_monoidal_spec(law, &vb.fn_name, inputs)
        {
            out.push(s);
        }
    }
    out
}

/// Emit the monoidal accumulator-generalization chain (the additive flavor of
/// the shared schema): `loop_gen` by induction + the law by instantiation at the
/// neutral accumulator. Two lemmas, dependency-ordered, one `lake build`.
fn monoidal_spec_group(shape: &MonoidalShape, base: &str) -> Vec<(String, String)> {
    let loop_l = crate::codegen::lean::aver_name_to_lean(&shape.loop_fn);
    let direct_l = crate::codegen::lean::aver_name_to_lean(&shape.direct);
    let wrapper_l = crate::codegen::lean::aver_name_to_lean(&shape.wrapper);
    let MonoidalShape {
        x_ty_lean,
        acc_ty_lean,
        neutral,
        ..
    } = shape;

    let loop_gen = format!("{base}_loop_gen");
    let loop_gen_text = format!(
        "theorem {loop_gen} : ∀ (list : List {x_ty_lean}) (acc : {acc_ty_lean}), \
         {loop_l} list acc = acc + {direct_l} list := by\n  \
         intro list\n  \
         induction list with\n  \
         | nil => intro acc; simp [{loop_l}, {direct_l}]\n  \
         | cons h t ih =>\n    \
         intro acc\n    \
         simp only [{loop_l}, {direct_l}]\n    \
         rw [ih (acc + h)]\n    \
         omega\n",
    );

    let spec = format!("{base}_spec_equiv");
    let spec_text = format!(
        "theorem {spec} (xs : List {x_ty_lean}) : {wrapper_l} xs = {direct_l} xs := by\n  \
         unfold {wrapper_l}\n  \
         rw [{loop_gen} xs {neutral}]\n  \
         simp\n",
    );

    vec![(loop_gen, loop_gen_text), (spec, spec_text)]
}
