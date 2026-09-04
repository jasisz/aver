//! Fuel induction over a well-founded Int-countdown fn.
//!
//! A pure fn emitted as a native well-founded def on `param.toNat`
//! (`RecursionContract::WellFoundedToNat` — the guard-validated floor-
//! division countdown `f(Int.div(v, k), …)` and the graduated subtractive
//! countdown `f(v - k, …)`) has an UNCONDITIONAL unfold equation, so every
//! blind `simp [f]` / `fun_induction f … <;> simp_all [f]` rung in the generic
//! ladder unfolds the recursive call forever and dies in a heartbeat timeout —
//! a hard build error `first | … | sorry` cannot catch. This strategy closes
//! laws over such a fn the way a human would: a `Nat` fuel `k` bounding
//! `x.toNat`, induction on `k` with every law given re-quantified, one GROUND
//! instance of the IH per unfolded recursive call (computed from the fn's own
//! self-call arguments), the countdown fn unfolded exactly ONCE, and a
//! `split`-then-`simp_all`/`omega` closer over a simp set that never contains
//! the countdown fn. Earlier laws about the same fn that would loop as simp
//! rules (the accumulator law) are cited as ground instances too.
//!
//! Shape-keyed and name-blind: the fn is discovered from its contract and the
//! law's calls, never from a name. Fail-closed: the portfolio ends in `sorry`,
//! so a non-closing instance degrades to the honest floor and `#print axioms`
//! decides credit.

use std::collections::{BTreeSet, HashMap};

use super::super::expr::aver_name_to_lean;
use super::AutoProof;
use super::shared::{
    call_name_args, child_exprs, direct_user_calls, find_fn_def, find_fn_def_by_call_name,
    ident_name, law_simp_defs_blind, render, same_file_verify_blocks, simp_def_name,
    substitute_expr, wf_countdown_fn_names, wf_countdown_param,
};
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

/// A recognized fuel-induction law: the single well-founded countdown fn the
/// law mentions, the law given driving its countdown, and the call shapes the
/// emitter instantiates the induction hypothesis at.
pub(in crate::codegen::lean) struct WfFuelPlan {
    /// The countdown fn (source name).
    pub f: String,
    /// Its `unfold` spelling (module-qualified for entry-module fns).
    pub f_lean: String,
    /// The law given at the countdown position of every occurrence (Lean name).
    pub x_lean: String,
    /// `f`'s parameter names, in order.
    pub params: Vec<String>,
    /// Argument vectors of every call to `f` in the law (lhs, rhs, when).
    pub occurrences: Vec<Vec<Spanned<Expr>>>,
    /// Argument vectors of every self-call in `f`'s body (verbatim).
    pub self_calls: Vec<Vec<Spanned<Expr>>>,
}

/// Recognize the fuel-induction shape (see the module doc). Declines unless
/// exactly one well-founded countdown fn `f` is called in the law's claim
/// (lhs/rhs — a `when`-only mention gives `unfold` nothing to unfold), every
/// call passes the SAME bare `Int` given at `f`'s countdown position, `f` is
/// self-recursive only (no other countdown fn in its body, no mutual SCC),
/// and every self-call shrinks the countdown INLINE (`Int.div(p, k)` /
/// `p - k`, the forms `omega` reads — never through a wrapper fn).
pub(in crate::codegen::lean) fn recognize_wf_fuel_induction(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<WfFuelPlan> {
    if law.givens.is_empty() {
        return None;
    }
    // An IR pin that hands the law to a bespoke template (the floor-division
    // window family, the tail-recursive fold, ring identities, …) keeps it:
    // those templates drive the countdown fn's own `.induct` and support
    // stacks on purpose. Only the generic pins (structural induction, the
    // discovery re-pin, backend dispatch, sorry) and unpinned laws are eligible.
    match super::law_strategy_for(ctx, &vb.fn_name, &law.name) {
        None
        | Some(crate::ir::ProofStrategy::Induction { .. })
        | Some(crate::ir::ProofStrategy::SimpOverLemmas(_))
        | Some(crate::ir::ProofStrategy::BackendDispatch)
        | Some(crate::ir::ProofStrategy::Sorry) => {}
        Some(_) => return None,
    }
    let wf = wf_countdown_fn_names(ctx);
    if wf.is_empty() {
        return None;
    }
    // 1. Every call to a countdown fn in the law, deep. The CLAIM (lhs/rhs)
    //    must mention the fn: each arm `unfold`s it in the goal, and `unfold`
    //    throws on no progress — inside an `induction` alternative that is a
    //    logged build error, not a fall-through to the `sorry` floor. Calls
    //    in the `when` premise still feed the IH tuples.
    let mut calls: Vec<(String, Vec<Spanned<Expr>>)> = Vec::new();
    collect_wf_calls(&law.lhs, ctx, &wf, &mut calls);
    collect_wf_calls(&law.rhs, ctx, &wf, &mut calls);
    if calls.is_empty() {
        return None;
    }
    if let Some(when) = &law.when {
        collect_wf_calls(when, ctx, &wf, &mut calls);
    }
    let distinct: BTreeSet<&str> = calls.iter().map(|(n, _)| n.as_str()).collect();
    if distinct.len() != 1 {
        return None;
    }
    let f = calls[0].0.clone();
    let fd = find_fn_def(ctx, &f)?;
    // 2. The countdown position carries the same bare `Int` given everywhere.
    let param = wf_countdown_param(ctx, fd)?;
    let pos = fd.params.iter().position(|(n, _)| n == param)?;
    let mut x: Option<&str> = None;
    for (_, args) in &calls {
        if args.len() != fd.params.len() {
            return None;
        }
        let arg = ident_name(&args[pos])?;
        match x {
            None => x = Some(arg),
            Some(seen) if seen == arg => {}
            Some(_) => return None,
        }
    }
    let x = x?;
    let given = law.givens.iter().find(|g| g.name == x)?;
    if given.type_name.trim() != "Int" {
        return None;
    }
    // 3. Self-recursive only.
    if !crate::codegen::lean::recursive_pure_fn_names(ctx).contains(&f) {
        return None;
    }
    let direct = direct_user_calls(&f, ctx);
    for callee in direct.iter().filter(|c| **c != f) {
        if wf.contains(callee) || reaches(callee, &f, ctx) {
            return None;
        }
    }
    // 4. The self-call argument vectors, verbatim (params only — a local
    //    binding or pattern variable in a self-call arg has no meaning at the
    //    law's level, so decline rather than emit a dangling name).
    let params: Vec<String> = fd.params.iter().map(|(n, _)| n.clone()).collect();
    let mut self_calls: Vec<Vec<Spanned<Expr>>> = Vec::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => collect_self_calls(e, &f, &mut self_calls),
        }
    }
    if self_calls.is_empty() {
        return None;
    }
    let param_set: BTreeSet<&str> = params.iter().map(String::as_str).collect();
    for args in &self_calls {
        if args.len() != params.len() || args.iter().any(|a| !idents_within(a, &param_set)) {
            return None;
        }
    }
    // 5. The fuel-decrease premise of every IH instance (`x.toNat ≤ k + 1 →
    //    a.toNat ≤ k` for the shrunk `a`) is discharged by `omega`, which
    //    reads `p / <lit>` and `p - <lit>` but treats a wrapper call
    //    (`halve(a)`) as an atom. So the countdown position of every
    //    self-call must be that inline arithmetic on the countdown param: a
    //    wrapper shrink (`FloorDivShrink { helper_fn: Some(..) }`) and the
    //    legacy `Result.withDefault(Int.div(p, k), d)` form both decline.
    if !wf_shrink_is_inline(ctx, fd)
        || self_calls
            .iter()
            .any(|args| !omega_readable_shrink(&args[pos], param))
    {
        return None;
    }
    Some(WfFuelPlan {
        f_lean: simp_def_name(ctx, &f),
        f,
        x_lean: aver_name_to_lean(x),
        params,
        occurrences: calls.into_iter().map(|(_, args)| args).collect(),
        self_calls,
    })
}

/// Deep-collect every call to a fn in `wf` (by resolved source name).
fn collect_wf_calls(
    e: &Spanned<Expr>,
    ctx: &CodegenContext,
    wf: &BTreeSet<String>,
    out: &mut Vec<(String, Vec<Spanned<Expr>>)>,
) {
    if let Some((name, args)) = call_name_args(e)
        && let Some(fd) = find_fn_def_by_call_name(ctx, &name)
        && wf.contains(&fd.name)
    {
        out.push((fd.name.clone(), args.to_vec()));
    }
    for c in child_exprs(e) {
        collect_wf_calls(c, ctx, wf, out);
    }
}

/// Deep-collect the argument vectors of every call to `f` (a `FnCall` or the
/// post-TCO `TailCall`) in `e`, match arms included.
fn collect_self_calls(e: &Spanned<Expr>, f: &str, out: &mut Vec<Vec<Spanned<Expr>>>) {
    if let Some((name, args)) = call_name_args(e)
        && name == f
    {
        out.push(args.to_vec());
    }
    for c in child_exprs(e) {
        collect_self_calls(c, f, out);
    }
}

/// Every value identifier in `e` (callee positions excluded) is in `allowed`.
fn idents_within(e: &Spanned<Expr>, allowed: &BTreeSet<&str>) -> bool {
    if let Some(n) = ident_name(e) {
        return allowed.contains(n);
    }
    let children: Vec<&Spanned<Expr>> = match &e.node {
        Expr::FnCall(_, args) => args.iter().collect(),
        Expr::Match { .. } => return false,
        _ => child_exprs(e),
    };
    children.iter().all(|c| idents_within(c, allowed))
}

/// `Int.div(p, <int literal>)` or `p - <int literal>`: the countdown shrink
/// forms `omega` reads once rendered (`p / k`, `p - k`).
fn omega_readable_shrink(arg: &Spanned<Expr>, p: &str) -> bool {
    let is_p = |e: &Spanned<Expr>| ident_name(e) == Some(p);
    let is_lit = |e: &Spanned<Expr>| matches!(&e.node, Expr::Literal(Literal::Int(_)));
    match &arg.node {
        Expr::BinOp(BinOp::Sub, a, b) => is_p(a) && is_lit(b),
        _ => matches!(
            call_name_args(arg),
            Some((name, [a, b])) if name == "Int.div" && is_p(a) && is_lit(b)
        ),
    }
}

/// Whether `fd`'s `WellFoundedToNat` contract shrinks its countdown inline
/// (no wrapper fn the kernel unfolds by name in `decreasing_by`).
fn wf_shrink_is_inline(ctx: &CodegenContext, fd: &FnDef) -> bool {
    let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd) else {
        return false;
    };
    match contract.recursion.as_ref() {
        Some(crate::ir::RecursionContract::WellFoundedToNat { floor_div, .. }) => floor_div
            .as_ref()
            .is_none_or(|shrink| shrink.helper_fn.is_none()),
        _ => false,
    }
}

/// Whether `from`'s transitive user-fn call closure reaches `target`.
fn reaches(from: &str, target: &str, ctx: &CodegenContext) -> bool {
    let mut seen: BTreeSet<String> = BTreeSet::new();
    let mut stack = vec![from.to_string()];
    while let Some(name) = stack.pop() {
        if name == target {
            return true;
        }
        if !seen.insert(name.clone()) {
            continue;
        }
        stack.extend(direct_user_calls(&name, ctx));
    }
    false
}

/// An earlier law whose LHS is a direct call to `f` on bare, distinct givens
/// covering all of its givens: `f(g_1..g_n)`. Cited as a ground instance at
/// every IH tuple (the accumulator law is exactly this shape and is a looping
/// simp rule, so it never joins a simp set). Two sources: laws earlier in the
/// FILE this theorem lands in, and the exposed laws of the dependency module
/// that OWNS `f` (cited by their namespace-qualified theorem name; the
/// consumer file imports and opens every dependency, and a dependency always
/// precedes its consumers in the module DAG, so a cycle is impossible).
struct GroundLaw {
    theorem: String,
    /// For each of the law's givens, the `f` argument position it occupies.
    given_positions: Vec<usize>,
}

fn ground_laws_about(vb: &VerifyBlock, plan: &WfFuelPlan, ctx: &CodegenContext) -> Vec<GroundLaw> {
    // Candidate blocks, each with the dependency module it comes from (`None`
    // = this file). `ctx.items` holds the ENTRY module only, so the in-file
    // sequence is read through `same_file_verify_blocks`.
    let mut candidates: Vec<(Option<&crate::codegen::ModuleInfo>, &VerifyBlock)> = Vec::new();
    for prev in same_file_verify_blocks(ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        candidates.push((None, prev));
    }
    let active = ctx.active_module_scope();
    if let Some(owner) = ctx
        .modules
        .iter()
        .find(|m| m.fn_defs.iter().any(|d| d.name == plan.f))
        && active.as_deref() != Some(owner.prefix.as_str())
    {
        // `verify_laws` holds only the laws whose subject the module exposes.
        for prev in &owner.verify_laws {
            candidates.push((Some(owner), prev));
        }
    }
    let mut out = Vec::new();
    for (owner, prev) in candidates {
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        let Some((name, args)) = call_name_args(&prev_law.lhs) else {
            continue;
        };
        let Some(fd) = find_fn_def_by_call_name(ctx, &name) else {
            continue;
        };
        if fd.name != plan.f || args.len() != plan.params.len() {
            continue;
        }
        let mut given_positions = Vec::with_capacity(prev_law.givens.len());
        let mut covered = true;
        for g in &prev_law.givens {
            match args
                .iter()
                .position(|a| ident_name(a) == Some(g.name.as_str()))
            {
                Some(j) => given_positions.push(j),
                None => {
                    covered = false;
                    break;
                }
            }
        }
        if !covered || args.iter().any(|a| ident_name(a).is_none()) {
            continue;
        }
        // The theorem must exist in its universal form. An unconditional law
        // is stated so exactly when `law_as_lemma_statement` accepts it; a
        // `when`-law is stated so only when this strategy recognises it
        // (verify.rs classes exactly those `conditional_universal`), and is
        // then cited as the implication `<when> = true -> claim`, which
        // `simp_all` uses as a conditional rewrite — never discharged here.
        let base = ctx.with_module_scope(owner.map(|m| m.prefix.as_str()), || {
            if prev_law.when.is_some() {
                recognize_wf_fuel_induction(prev, prev_law, ctx)
                    .map(|_| crate::codegen::lean::toplevel::law_theorem_base(prev, prev_law, ctx))
            } else {
                crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
                    .map(|(name, _)| name)
            }
        });
        let Some(base) = base else {
            continue;
        };
        let theorem = match owner {
            Some(m) => format!(
                "{}.{}",
                crate::codegen::lean::syntax::aver_path_to_lean(&m.prefix),
                base
            ),
            None => base,
        };
        out.push(GroundLaw {
            theorem,
            given_positions,
        });
    }
    out
}

/// Render a Lean application argument: parenthesize anything that is not a
/// single token or one already-parenthesized group.
fn arg(text: &str) -> String {
    if !text.chars().any(char::is_whitespace) || is_one_group(text) {
        text.to_string()
    } else {
        format!("({text})")
    }
}

/// `(…)` whose opening paren matches its closing one (`(a) + (b)` is not).
fn is_one_group(text: &str) -> bool {
    if !(text.starts_with('(') && text.ends_with(')')) {
        return false;
    }
    let mut depth = 0usize;
    for (i, c) in text.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 && i + 1 < text.len() {
                    return false;
                }
            }
            _ => {}
        }
    }
    true
}

/// Emit the fuel-induction proof for a recognized law. `quant_params` and
/// `theorem_prop` are the caller's theorem statement pieces, used verbatim
/// inside the `key` lemma; `intro_names` are the givens' Lean names in
/// `quant_params` order.
pub(in crate::codegen::lean) fn emit_wf_fuel_induction_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    quant_params: &str,
    theorem_prop: &str,
) -> Option<AutoProof> {
    let plan = recognize_wf_fuel_induction(vb, law, ctx)?;
    let conditional = law.when.is_some();
    let givens: Vec<String> = intro_names.to_vec();
    let given_source: Vec<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    // The proof's own binders must not shadow a law given (a given named `k`
    // is common): pick each name fresh against the intro names.
    let fresh = |base: &str| -> String {
        let mut name = base.to_string();
        let mut n = 1;
        while givens.iter().any(|g| g == &name) {
            name = format!("{base}_{n}");
            n += 1;
        }
        name
    };
    let fuel = fresh("k");
    let ih = fresh("ih");
    let hk = fresh("hk");
    let key = fresh("key");

    // The IH tuples: every self-call instantiated at every occurrence
    // (`f`'s params := the occurrence's args), rendered per position.
    let mut tuples: Vec<Vec<String>> = Vec::new();
    for occ in &plan.occurrences {
        let bindings: HashMap<&str, &Spanned<Expr>> = plan
            .params
            .iter()
            .map(String::as_str)
            .zip(occ.iter())
            .collect();
        for call in &plan.self_calls {
            let tuple: Vec<String> = call
                .iter()
                .map(|a| render(&substitute_expr(a, &bindings), ctx))
                .collect();
            if !tuples.contains(&tuple) {
                tuples.push(tuple);
            }
        }
    }

    // IH instances: for each tuple and each occurrence PATTERN, the given
    // vector that makes the pattern's bare-given positions equal the tuple
    // (the countdown position always; other givens not pinned by the pattern
    // stay themselves — an earlier ground law bridges the rest).
    let mut ih_instances: Vec<Vec<String>> = Vec::new();
    for tuple in &tuples {
        for occ in &plan.occurrences {
            let mut inst: Vec<String> = givens.clone();
            let mut consistent = true;
            for (j, a) in occ.iter().enumerate() {
                let Some(n) = ident_name(a) else {
                    continue;
                };
                let Some(gi) = given_source.iter().position(|g| *g == n) else {
                    continue;
                };
                let value = arg(&tuple[j]);
                if inst[gi] != givens[gi] && inst[gi] != value {
                    consistent = false;
                    break;
                }
                inst[gi] = value;
            }
            if consistent && !ih_instances.contains(&inst) {
                ih_instances.push(inst);
            }
        }
    }

    // Ground instances of earlier direct laws about `f`, at every tuple.
    let mut ground: Vec<String> = Vec::new();
    for gl in ground_laws_about(vb, &plan, ctx) {
        for tuple in &tuples {
            let args: Vec<String> = gl.given_positions.iter().map(|j| arg(&tuple[*j])).collect();
            let cite = format!("{} {}", gl.theorem, args.join(" "));
            if !ground.contains(&cite) {
                ground.push(cite);
            }
        }
    }

    // The safe simp set: the blind cone (no countdown fn) plus the earlier
    // laws that are not looping rewrites (the pool is already filtered).
    let mut safe: Vec<String> = law_simp_defs_blind(ctx, vb, law).into_iter().collect();
    for lemma in super::induction::earlier_law_lemmas(vb, law, ctx) {
        if !safe.contains(&lemma.name) {
            safe.push(lemma.name);
        }
    }
    let simp_all = if safe.is_empty() {
        "simp_all".to_string()
    } else {
        format!("simp_all [{}]", safe.join(", "))
    };
    // Every inner portfolio ends in its own `sorry` floor. Lean runs the LAST
    // alternative of a `first` with error recovery on: an alternative there
    // that makes progress and then fails (`simp_all` leaving a goal, `omega`
    // after a rewrite) is LOGGED as a build error and the goal admitted — the
    // enclosing `first | (have key …) | sorry` never sees a failure to fall
    // through on. With `sorry` last, a non-closing arm degrades to `sorryAx`
    // (no credit, no build error); the middle alternatives fail cleanly.
    let closer = format!(
        "first | (split <;> {simp_all} <;> omega) | ({simp_all} <;> omega) | (split <;> {simp_all} <;> done) | sorry"
    );
    // The `when` premise at the shrunk value. A bare comparison premise is
    // stated `(P) = true`, which elaborates as the Prop equation `P = (true =
    // true)`; a Bool-valued premise (`f(..) && g(..)`) as `decide`-coerced
    // Bool. Normalize both to Prop, then `omega`. Same recovery rule as the
    // closers: the floor is `sorry`, never a progress-then-fail alternative.
    let guard = "first | omega | (simp only [eq_iff_iff, iff_true, decide_eq_true_eq, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true', ge_iff_le, gt_iff_lt] at h_when ⊢; omega) | sorry";

    let mut arm_intro = givens.clone();
    arm_intro.push(hk.clone());
    let mut outer_intro = givens.clone();
    if conditional {
        arm_intro.push("h_when".to_string());
        outer_intro.push("h_when".to_string());
    }
    let arm_intro = arm_intro.join(" ");

    let mut lines = vec![
        format!("  intro {}", outer_intro.join(" ")),
        "  first".to_string(),
        format!(
            "  | (have {key} : ∀ ({fuel} : Nat) {quant_params}, {}.toNat ≤ {fuel} → {theorem_prop} := by",
            plan.x_lean
        ),
        format!("       intro {fuel}"),
        format!("       induction {fuel} with"),
        "       | zero =>".to_string(),
        format!("         intro {arm_intro}"),
        format!("         unfold {}", plan.f_lean),
        format!("         {closer}"),
        format!("       | succ {fuel} {ih} =>"),
        format!("         intro {arm_intro}"),
    ];
    // The fuel-decrease discharge is floored like the closers: the arm runs
    // under `induction`'s error recovery, so a bound `omega` cannot close
    // (never expected — the recognizer admits only inline `p / k` / `p - k`
    // shrinks — but the floor is what keeps the class fail-closed) degrades
    // to `sorryAx`, never to a logged build error.
    for (i, inst) in ih_instances.iter().enumerate() {
        let premise = if conditional {
            format!(" (by {guard})")
        } else {
            String::new()
        };
        lines.push(format!(
            "         have {ih}{} := {ih} {} (by first | omega | sorry){premise}",
            i + 1,
            inst.join(" ")
        ));
    }
    for (i, cite) in ground.iter().enumerate() {
        lines.push(format!("         have {}{} := {cite}", fresh("l"), i + 1));
    }
    lines.push(format!("         clear {ih}"));
    lines.push(format!("         unfold {}", plan.f_lean));
    lines.push(format!("         {closer}"));
    let when_arg = if conditional { " h_when" } else { "" };
    lines.push(format!(
        "     exact {key} _ {} (Nat.le_refl _){when_arg})",
        givens.join(" ")
    ));
    lines.push("  | sorry".to_string());

    Some(AutoProof {
        support_lines: Vec::new(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(lines),
        replaces_theorem: false,
        first_arm_is_guaranteed_closer: false,
    })
}
