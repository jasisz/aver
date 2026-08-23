use std::collections::{HashMap, HashSet};

use super::expr::aver_name_to_lean;
use super::fn_def::{emit_fn_body_for, lower_pure_question_bang_for_emit};
use super::is_pure_fn;
use super::render::{emit_doc_comment, emit_fn_params, ret_type_or_unit};
use crate::ast::*;
use crate::codegen::CodegenContext;

// ---------------------------------------------------------------------------
// Termination-as-a-law: length-monotonicity WF for computed-arg mutual SCCs.
//
// A mutual SCC like quicksort's `sort` / `sortWithPivot` recurses through
// COMPUTED list arguments (`sort (smallerOrEqual rest pivot)`) that Lean's
// structural recursion can't see decrease. Instead of a fuel encoding, this
// path synthesises + KERNEL-PROVES a length-monotonicity lemma for each
// partition helper (`(g s …).length ≤ s.length`) and emits a genuine
// well-founded `mutual` block whose per-def `decreasing_by` cites the lemma.
//
// Soundness rides on the Lean kernel: each `…_len_le` lemma is proved by the
// validated `induction … split … List.length_cons; omega` tactic, or the SCC
// is rejected and the caller falls back to fuel. A lemma that doesn't hold
// (helper grows the list) makes `lake build` fail rather than slip through.
// ---------------------------------------------------------------------------

/// How a recursive call's measure-list argument relates to the caller's
/// measure-list param.
#[derive(Clone)]
enum LexListArg {
    /// The argument is the caller's measure param itself or a `cons`-tail
    /// subterm of it — the structural-subterm case. Tie-break by rank.
    Subterm {
        /// `true` when the argument is a strict subterm (the cons-tail),
        /// i.e. the callee receives `caller_len - 1`. `false` when it is
        /// the param verbatim (callee receives `caller_len`).
        strict: bool,
    },
    /// The argument is `g(s, …)` where `g` is a non-growing structural
    /// list filter and `s` is the caller's measure param or its cons-tail.
    /// `helper` is the Aver fn name of `g`; `lemma_args` is the Lean source
    /// of `g`'s actual arguments (`rest pivot`), used to instantiate the
    /// synthesised `<g>_len_le` lemma in `decreasing_by`.
    Filtered { helper: String, lemma_args: String },
}

/// One recognised recursive edge inside the SCC, in body source order
/// (the order Lean generates `decreasing_by` goals).
struct LexListEdge {
    callee: String,
    arg: LexListArg,
}

/// A length lemma to synthesise: `(<helper> xs <rest…>).length ≤ xs.length`.
#[derive(Clone)]
struct LengthLemma {
    /// Lean name of the filter helper.
    helper_lean: String,
    /// Lean param list of the helper (`(xs : List Int) (pivot : Int)`).
    params: String,
    /// Lean names of the helper's params, in order.
    param_names: Vec<String>,
}

impl LengthLemma {
    fn lemma_name(&self) -> String {
        format!("{}_len_le", self.helper_lean)
    }
}

/// Per-fn plan for the lex-list WF mutual block.
struct LexListMemberPlan<'a> {
    fd: &'a FnDef,
    /// Lean name of the measure list param.
    list_param_lean: String,
    /// `+offset` added to `list_param.length` in the lex first component.
    offset: usize,
    /// Lex second component (rank).
    rank: usize,
    /// Recognised recursive edges, in body source order.
    edges: Vec<LexListEdge>,
}

/// Pull the single measure list param (a `List<_>` param) of a fn, as
/// (param index, Lean name). Returns `None` when the fn has zero or more
/// than one `List<_>` param (the synthesiser only reasons about a single
/// list measure) or any non-`List` sizeOf-relevant param.
fn lex_list_measure_param(fd: &FnDef, ctx: &CodegenContext) -> Option<(usize, String)> {
    let rfd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id))?;
    let mut found: Option<(usize, String)> = None;
    for (idx, (name, ty)) in rfd.params.iter().enumerate() {
        match ty {
            crate::types::Type::List(_) => {
                if found.is_some() {
                    // More than one List param — outside the single-measure
                    // shape this synthesiser reasons about.
                    return None;
                }
                found = Some((idx, aver_name_to_lean(name)));
            }
            // Any other sizeOf-relevant param (Vector / String / recursive
            // ADT) means the measure isn't a single List length — back off.
            crate::types::Type::Vector(_) | crate::types::Type::Str => return None,
            _ => {}
        }
    }
    found
}

/// True iff `g` is a non-growing structural list filter: a recursive fn
/// `(xs : List _, …) -> List _` whose body matches on its first list param
/// with `[] -> []` and `[x, ..rest] -> <if-cond> { x :: g(rest, …) | g(rest, …) }`.
/// This is exactly the shape the validated
/// `induction … split … List.length_cons; omega` proof closes, giving
/// `(g xs …).length ≤ xs.length`. Anything else returns `None`.
fn lex_list_filter_helper<'a>(helper: &'a FnDef, ctx: &CodegenContext) -> Option<&'a FnDef> {
    // First param must be `List<_>`, and the fn must return `List<_>`.
    let rfd = crate::codegen::common::fn_id_for_decl(ctx, helper)
        .and_then(|id| ctx.resolved_program.fn_by_id(id))?;
    let first_param = rfd.params.first()?;
    if !matches!(first_param.1, crate::types::Type::List(_)) {
        return None;
    }
    if !matches!(rfd.return_type, crate::types::Type::List(_)) {
        return None;
    }
    let list_param_name = first_param.0.clone();

    // Body must be a single tail expression: `match <list_param> { … }`.
    let tail = helper.body.tail_expr()?;
    if helper.body.stmts().len() != 1 {
        return None;
    }
    let Expr::Match { subject, arms } = &tail.node else {
        return None;
    };
    // Subject must be the (first) list param.
    let subject_name = crate::codegen::recursion::detect::local_name_of(subject)?;
    if subject_name != list_param_name {
        return None;
    }
    let mut saw_nil_empty = false;
    let mut saw_cons_filter = false;
    for arm in arms {
        match &arm.pattern {
            // `[] -> []`
            Pattern::EmptyList if matches!(&arm.body.node, Expr::List(items) if items.is_empty()) =>
            {
                saw_nil_empty = true;
            }
            Pattern::Cons(head, tail_bind)
                if lex_filter_cons_arm_ok(&arm.body, helper, head, tail_bind) =>
            {
                saw_cons_filter = true;
            }
            // Any other arm (non-`[]` nil body, non-filter cons arm,
            // literal / wildcard / ctor) breaks the recognised shape.
            _ => return None,
        }
    }
    (saw_nil_empty && saw_cons_filter).then_some(helper)
}

/// A `cons`-arm body of a filter helper: `<bool-match> { true -> head :: g(tail, …) ; false -> g(tail, …) }`
/// (either branch may be the keep / drop side). Both branches must be
/// either `g(tail, …)` or `head :: g(tail, …)` — i.e. each recursive call
/// passes the cons-tail and prepends at most one element. That guarantees
/// `(g xs …).length ≤ xs.length` under the validated proof.
fn lex_filter_cons_arm_ok(
    body: &Spanned<Expr>,
    helper: &FnDef,
    head: &str,
    tail_bind: &str,
) -> bool {
    match &body.node {
        // `match <cond> { true -> …; false -> … }` over a Bool subject.
        Expr::Match { arms, .. } => arms
            .iter()
            .all(|arm| lex_filter_branch_ok(&arm.body, helper, head, tail_bind)),
        // A direct branch (no inner conditional) — accept the same leaf
        // shapes so a `[x, ..rest] -> g(rest, …)` arm is recognised too.
        _ => lex_filter_branch_ok(body, helper, head, tail_bind),
    }
}

/// Leaf of a filter cons-arm: `g(tail, …)` or `head :: g(tail, …)`
/// (`List.prepend(head, g(tail, …))`). The recursive call must target the
/// helper itself and pass the cons-tail in the first (list) position.
fn lex_filter_branch_ok(body: &Spanned<Expr>, helper: &FnDef, head: &str, tail_bind: &str) -> bool {
    // A recursive call on the cons-tail — `g(tail, …)`. The TCO pass may
    // have rewritten a tail-position self-call into `Expr::TailCall`, so
    // accept both shapes. Self-identity uses the shared
    // `canonical_callee_name` helper (syntax-discovery-only) so the
    // suffix-match category rule lives in one place.
    let self_set: HashSet<String> = std::iter::once(helper.name.clone()).collect();
    let recur_on_tail = |target: &str, args: &[Spanned<Expr>]| -> bool {
        let is_self =
            crate::codegen::recursion::detect::canonical_callee_name(target, &self_set).is_some();
        is_self
            && args
                .first()
                .and_then(crate::codegen::recursion::detect::local_name_of)
                .filter(|n| *n == tail_bind)
                .is_some()
    };
    match &body.node {
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::recursion::detect::expr_to_dotted_name(callee);
            match name.as_deref() {
                // `head :: g(tail, …)` → encoded as `List.prepend(head, recur)`.
                Some("List.prepend") if args.len() == 2 => {
                    crate::codegen::recursion::detect::local_name_of(&args[0])
                        .filter(|n| *n == head)
                        .is_some()
                        && lex_filter_branch_ok(&args[1], helper, head, tail_bind)
                }
                // `g(tail, …)` — drop-element branch.
                Some(n) => recur_on_tail(n, args),
                None => false,
            }
        }
        // TCO'd tail-position self-call: `g(tail, …)`.
        Expr::TailCall(boxed) => recur_on_tail(&boxed.target, &boxed.args),
        _ => false,
    }
}

/// Lean source of a recursive call's measure argument, classified.
/// `caller_list_param` is the caller's measure param name; `tail_binders`
/// is the set of cons-tail binders of a match on that param in the caller
/// body; `scc` is the SCC member names; `ctx` resolves filter helpers.
fn classify_lex_list_arg(
    arg: &Spanned<Expr>,
    caller_list_param: &str,
    tail_binders: &HashSet<String>,
    ctx: &CodegenContext,
) -> Option<LexListArg> {
    use crate::codegen::recursion::detect::{expr_to_dotted_name, local_name_of};
    // Subterm: the param itself or a cons-tail of it.
    if let Some(name) = local_name_of(arg) {
        if name == caller_list_param {
            return Some(LexListArg::Subterm { strict: false });
        }
        if tail_binders.contains(name) {
            return Some(LexListArg::Subterm { strict: true });
        }
        return None;
    }
    // Filtered: `g(s, …)` where `s` is the param or its cons-tail and `g`
    // is a non-growing list filter defined in the program.
    if let Expr::FnCall(callee, args) = &arg.node {
        let dotted = expr_to_dotted_name(callee)?;
        let bare = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
        let first = args.first()?;
        let s = local_name_of(first)?;
        let s_ok = s == caller_list_param || tail_binders.contains(s);
        if !s_ok {
            return None;
        }
        let helper_fd = find_user_fn_by_name(ctx, &bare)?;
        // Kernel-provable non-growing filter shape required.
        lex_list_filter_helper(helper_fd, ctx)?;
        // Render the call's actual args as Lean — these instantiate the
        // synthesised `<g>_len_le` lemma in `decreasing_by`.
        let lemma_args = args
            .iter()
            .map(|a| {
                super::expr::emit_expr(&super::expr::resolve_rewrite_output(a, ctx, None), ctx)
            })
            .collect::<Vec<_>>()
            .join(" ");
        return Some(LexListArg::Filtered {
            helper: bare,
            lemma_args,
        });
    }
    None
}

/// Locate a user fn def by bare name across entry + dep-module scopes.
fn find_user_fn_by_name<'a>(ctx: &'a CodegenContext, name: &str) -> Option<&'a FnDef> {
    ctx.fn_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.fn_defs.iter()))
        .find(|fd| fd.name == name)
}

/// Recognise a mutual SCC whose every recursive call decreases either by a
/// structural subterm (cons-tail) or through a non-growing list filter,
/// and assign each member a lex measure `(list_param.length + offset, rank)`
/// that discharges every obligation. Returns the per-member plans + the
/// distinct length lemmas to synthesise, or `None` to back off to fuel.
fn recognize_lex_list_wf_scc<'a>(
    fns: &'a [&'a FnDef],
    ctx: &CodegenContext,
) -> Option<(Vec<LexListMemberPlan<'a>>, Vec<LengthLemma>)> {
    let names: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    if fns.len() < 2 {
        // Single-fn "mutual" SCCs are emitted by the self-recursive path,
        // not here; this synthesiser targets genuine mutual blocks.
        return None;
    }

    // Per-fn body-derived classified edges (list param + tail binders are
    // consumed inline to classify each edge; only the edges survive).
    struct Raw<'a> {
        fd: &'a FnDef,
        edges: Vec<LexListEdge>,
    }
    let mut raws: Vec<Raw<'a>> = Vec::new();
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        let (_idx, list_param) = lex_list_measure_param(fd, ctx)?;
        let tail_binders =
            crate::codegen::recursion::detect::collect_list_tail_binders(fd, &list_param);
        let mut edges: Vec<LexListEdge> = Vec::new();
        for (callee_raw, args) in
            crate::codegen::recursion::detect::collect_calls_from_body(fd.body.as_ref())
        {
            let Some(callee) =
                crate::codegen::recursion::detect::canonical_callee_name(&callee_raw, &names)
            else {
                continue;
            };
            // The callee's measure-list arg position (its single List param).
            let callee_fd = fns.iter().find(|f| f.name == callee)?;
            let (callee_list_idx, _) = lex_list_measure_param(callee_fd, ctx)?;
            let arg = args.get(callee_list_idx)?;
            let classified = classify_lex_list_arg(arg, &list_param, &tail_binders, ctx)?;
            edges.push(LexListEdge {
                callee,
                arg: classified,
            });
        }
        if edges.is_empty() {
            // A member with no intra-SCC recursive call isn't part of the
            // recursion this synthesiser models.
            return None;
        }
        raws.push(Raw { fd, edges });
    }

    // Offset propagation along subterm edges: a strict cons-tail edge f→c
    // makes `len_c = len_f - 1`, so `off_c = off_f + 1` keeps the lex first
    // component equal and the rank tie-break carries the decrease. A
    // non-strict subterm edge (param verbatim) keeps `off_c = off_f`.
    // Filter edges must end up STRICTLY lower in offset (their length only
    // weakly shrinks, so the first component must drop via offset).
    let mut offset: HashMap<String, usize> = HashMap::new();
    offset.insert(raws[0].fd.name.clone(), 0);
    // Fixed-point propagation (SCC has ≤ small node count).
    let mut changed = true;
    let mut iterations = 0;
    while changed {
        changed = false;
        iterations += 1;
        if iterations > raws.len() * raws.len() + 4 {
            // Non-convergent (cyclic strict subterm path) — back off.
            return None;
        }
        for raw in &raws {
            let Some(&off_f) = offset.get(&raw.fd.name) else {
                continue;
            };
            for edge in &raw.edges {
                if let LexListArg::Subterm { strict } = &edge.arg {
                    let want = if *strict { off_f + 1 } else { off_f };
                    match offset.get(&edge.callee) {
                        Some(&existing) if existing == want => {}
                        Some(_) => return None, // conflicting offset → back off
                        None => {
                            offset.insert(edge.callee.clone(), want);
                            changed = true;
                        }
                    }
                }
            }
        }
        // Seed any node not yet reached by a subterm edge with offset 0.
        for raw in &raws {
            if !offset.contains_key(&raw.fd.name) {
                offset.insert(raw.fd.name.clone(), 0);
                changed = true;
            }
        }
    }

    // Every filter edge f→c must satisfy `off_c < off_f` so the first
    // component strictly decreases (filter only proves `≤`, never `<`).
    for raw in &raws {
        let off_f = offset[&raw.fd.name];
        for edge in &raw.edges {
            if matches!(edge.arg, LexListArg::Filtered { .. }) {
                let off_c = offset[&edge.callee];
                if off_c >= off_f {
                    return None;
                }
            }
        }
    }

    // Ranks: subterm edges must tie-break, so a strict subterm edge f→c
    // needs `rank_c < rank_f`. Assign `rank = max_off - off` so a `+1`
    // offset step (strict subterm) lowers the rank by exactly 1.
    let max_off = offset.values().copied().max().unwrap_or(0);
    let mut plans: Vec<LexListMemberPlan<'a>> = Vec::new();
    for raw in raws {
        let off = offset[&raw.fd.name];
        let rank = max_off - off;
        let (_idx, list_param_lean) = lex_list_measure_param(raw.fd, ctx)?;
        plans.push(LexListMemberPlan {
            fd: raw.fd,
            list_param_lean,
            offset: off,
            rank,
            edges: raw.edges,
        });
    }

    // Validate the rank tie-break for every subterm edge.
    let rank_of: HashMap<String, usize> =
        plans.iter().map(|p| (p.fd.name.clone(), p.rank)).collect();
    for plan in &plans {
        for edge in &plan.edges {
            if let LexListArg::Subterm { strict } = &edge.arg {
                if *strict {
                    // strict subterm: equal first component, need rank to drop.
                    if rank_of[&edge.callee] >= plan.rank {
                        return None;
                    }
                } else {
                    // non-strict subterm (param verbatim, no shrink at all):
                    // first component equal AND rank equal would loop. We
                    // can't prove termination on a no-shrink self/peer call.
                    return None;
                }
            }
        }
    }

    // Distinct length lemmas referenced by any filter edge.
    let mut lemmas: Vec<LengthLemma> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for plan in &plans {
        for edge in &plan.edges {
            if let LexListArg::Filtered { helper, .. } = &edge.arg {
                if !seen.insert(helper.clone()) {
                    continue;
                }
                let helper_fd = find_user_fn_by_name(ctx, helper)?;
                let helper_lean = aver_name_to_lean(helper);
                let params = emit_fn_params(&helper_fd.params);
                let param_names: Vec<String> = helper_fd
                    .params
                    .iter()
                    .map(|(n, _)| aver_name_to_lean(n))
                    .collect();
                lemmas.push(LengthLemma {
                    helper_lean,
                    params,
                    param_names,
                });
            }
        }
    }

    Some((plans, lemmas))
}

/// Emit the kernel-proved length-monotonicity lemma for one filter helper:
/// `theorem <h>_len_le : ∀ <params>, (<h> <args>).length ≤ <list_param>.length`.
/// The proof is the validated `induction … split … List.length_cons; omega`.
fn emit_length_lemma(lemma: &LengthLemma) -> String {
    let first = &lemma.param_names[0];
    let rest_args = lemma.param_names[1..].join(" ");
    let call = if rest_args.is_empty() {
        format!("{} {}", lemma.helper_lean, first)
    } else {
        format!("{} {} {}", lemma.helper_lean, first, rest_args)
    };
    // `∀`-quantify exactly the helper's params, in declared order.
    let forall_binders = lemma.params.clone();
    let intro_names = lemma.param_names.join(" ");
    let mut lines = Vec::new();
    lines.push(format!(
        "theorem {} : ∀ {}, ({}).length ≤ {}.length := by",
        lemma.lemma_name(),
        forall_binders,
        call,
        first
    ));
    lines.push(format!("  intro {}", intro_names));
    lines.push(format!("  induction {} with", first));
    lines.push(format!("  | nil => simp [{}]", lemma.helper_lean));
    lines.push(format!(
        "  | cons x rest ih => simp only [{}]; split",
        lemma.helper_lean
    ));
    lines.push("                      · simp only [List.length_cons]; omega".to_string());
    lines.push("                      · simp only [List.length_cons]; omega".to_string());
    lines.join("\n")
}

/// Emit a genuine well-founded mutual block for a length-monotonicity SCC:
/// the synthesised + kernel-proved length lemmas, then the `mutual … end`
/// block with per-def lex `termination_by (list.length + offset, rank)` and
/// `decreasing_by` clauses citing the lemmas. Returns `None` (back off to
/// fuel) when the SCC isn't recognised as length-monotone-WF.
pub(super) fn emit_native_mutual_lex_list_wf_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Option<String> {
    // NB: unlike `emit_native_mutual_sizeof_group`, this path does NOT gate
    // on `scc_has_growing_accumulator` — a computed filter arg
    // (`smallerOrEqual rest pivot`) trips that conservative check, and
    // proving it shrinks via a synthesised length lemma is exactly the job
    // here. The classifier below rejects anything it can't prove.
    let (plans, lemmas) = recognize_lex_list_wf_scc(fns, ctx)?;

    let mut lines: Vec<String> = Vec::new();
    // Kernel-proved length lemmas first — they reference the filter helpers
    // emitted earlier in topological order, and are cited by `decreasing_by`.
    for lemma in &lemmas {
        lines.push(emit_length_lemma(lemma));
        lines.push(String::new());
    }

    lines.push("mutual".to_string());
    for plan in &plans {
        let fd = plan.fd;
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        let body_ast = lowered
            .as_ref()
            .map(|l| l.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        let body = emit_fn_body_for(body_fn, body_ast, ctx);

        lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        for body_line in body.lines() {
            lines.push(format!("  {body_line}"));
        }
        let measure_first = if plan.offset == 0 {
            format!("{}.length", plan.list_param_lean)
        } else {
            format!("{}.length + {}", plan.list_param_lean, plan.offset)
        };
        lines.push(format!(
            "  termination_by ({}, {})",
            measure_first, plan.rank
        ));
        // One `decreasing_by` goal per recursive call, in body source order.
        lines.push("  decreasing_by".to_string());
        for edge in &plan.edges {
            match &edge.arg {
                LexListArg::Subterm { .. } => {
                    // Equal first component, decrease lives in the rank — a
                    // `Prod.Lex` goal omega can't discharge directly.
                    lines.push("    · simp_wf; exact Prod.Lex.right _ (by omega)".to_string());
                }
                LexListArg::Filtered { helper, lemma_args } => {
                    let lemma_name = format!("{}_len_le", aver_name_to_lean(helper));
                    // Cite the lemma at exactly the call's arguments so omega
                    // can relate `(g rest pivot).length` to `rest.length`.
                    lines.push(format!(
                        "    · simp_wf; have := {} {}; omega",
                        lemma_name, lemma_args
                    ));
                }
            }
        }
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}
