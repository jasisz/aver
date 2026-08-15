//! The same loop, written as two functions.
//!
//! Real parser code writes a collecting loop as a pair: a DRIVER that
//! matches and terminates, and a STEP companion that does one unit of
//! work and recurses back into the driver — the docstrings in the code
//! that motivated this stage say why: the accumulator form keeps the
//! recursion in tail position, and splitting the step out keeps each
//! match arm on one line.
//!
//! ```aver
//! fn parseAll(records: List<String>, acc: List<Change>) -> Result<List<Change>, String>
//!     match records
//!         [] -> Result.Ok(List.reverse(acc))
//!         [head, ..tail] -> parseNext(head, tail, acc)
//!
//! fn parseNext(head: String, tail: List<String>, acc: List<Change>) -> Result<List<Change>, String>
//!     parsed = parseOne(head)?
//!     parseAll(tail, List.prepend(parsed, acc))
//! ```
//!
//! The single-function spelling of that loop fuses today; the pair never
//! reaches candidacy, because the recognisers in [`super::list_build`]
//! read one body at a time and the append lives in the other one. This
//! stage closes that gap by INLINING the step into the driver before
//! candidacy, so the existing recognisers judge the merged loop with
//! zero changes to their guards.
//!
//! The inline is deliberately narrow. A step is inlined only when:
//!
//! - it has EXACTLY ONE call site in the whole module, the tail call in
//!   the driver — a step called from anywhere else is shared code, not
//!   the idiom. The census walks every expression-bearing top-level
//!   item, verify blocks and top-level statements included, so the
//!   condition means what it says. The one carve-out is the step's own
//!   verify block: the step is never edited or removed, so its spec
//!   keeps the exact function it names — those calls are not a second
//!   consumer;
//! - the module cannot be called around: the step must not be visible
//!   outside the module (`exposes` list, or the `_` convention). The
//!   pass sees one module at a time, so a step some other module could
//!   call is a step whose call sites this walk cannot count — decline
//!   rather than guess;
//! - it declares no effects. The inline moves the step's statements
//!   into the driver's branch structure, which reshapes the trace an
//!   effectful body would record;
//! - it does not recurse into itself — the idiom recurses into the
//!   DRIVER, and a self-recursive step is its own loop.
//!
//! Binder hygiene is mechanical rather than argued per-case: every
//! name this stage synthesizes — binder renames and argument binders
//! alike — comes from ONE allocator, so no two are ever equal.
//! Parameters are substituted only when the argument is a bare
//! identifier or a literal — anything else is bound first, in argument
//! order, so evaluation order is the call's — and a step whose body
//! reads a name the driver re-binds anywhere is declined outright.
//! Substitution is SIMULTANEOUS: one walk, from a complete
//! param-to-argument map, never descending into what it inserts — an
//! argument identifier spelled like another parameter of the step must
//! not be captured by that parameter's substitution. This family has
//! already shipped binder-capture bugs; the rules here prefer a lost
//! fusion over a wrong answer, every time.
//!
//! The inline happens on a COPY of the driver's body, and the copy is
//! committed only when the merged loop then actually fuses — the same
//! candidacy walk and variant build the main pass runs, plus at least
//! one call site that starts the accumulator empty. A pair that would
//! not fuse keeps both its functions exactly as written; there is no
//! inline-without-payoff residue. The step itself is never edited or
//! removed: its verify blocks and any future callers keep the function
//! they named.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::list_build::{
    ListBuildShape, build_collected_variant, builder_namespace_taken, list_build_acc_of,
    try_rewrite_list_build_site,
};
use super::*;
use crate::ast::TopLevel;

/// The namespace every name this stage synthesizes lives in. The
/// names are `__stp<n>` with `n` handed out by [`FreshNames`] — one
/// allocator per driver build, shared by binder renames and argument
/// binders across every inline round — so no two synthesized names
/// are ever equal, by construction. A program that binds any name
/// starting with this prefix takes the stage away for the whole
/// module: the check is on the prefix, so it covers every `n` at
/// once.
pub(super) const STEP_PREFIX: &str = "__stp";

/// The single fresh-name allocator behind every name this stage
/// synthesizes. One monotone counter: two allocations can never be
/// equal BY CONSTRUCTION, with no second namespace to reason about.
/// This replaced two overlapping hand-rolled schemes (`__stp<k>_<name>`
/// binder renames beside `__stp<k>_p<idx>` argument binders), where a
/// step binder literally named `p0` renamed to exactly the fresh name
/// minted for the bound argument at index 0 and shadowed it.
#[derive(Default)]
struct FreshNames {
    next: usize,
}

impl FreshNames {
    /// The next name. No other call on this allocator has returned it
    /// or ever will.
    fn next_name(&mut self) -> String {
        let n = self.next;
        self.next += 1;
        format!("{STEP_PREFIX}{n}")
    }
}

/// Why the driver-and-step normalization left a pair alone.
///
/// Reported per driver so `--explain-passes` can say which pair stayed
/// two functions and what about it stopped the inline. A driver whose
/// merged loop then failed candidacy is reported with the recogniser's
/// own reason instead — that is the more precise fact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PairDecline {
    /// The step is visible outside this module, so callers this walk
    /// cannot see may exist and the call-site count below cannot be
    /// trusted.
    StepVisible,
    /// The step has more than one call site, or is referenced as a
    /// value — it is shared code, not this driver's step.
    StepShared,
    /// The step declares effects; inlining would reshape its trace.
    StepEffects,
    /// The step recurses into itself instead of only into the driver.
    StepSelfRecursive,
    /// The step's body binds names the mechanical rename cannot carry:
    /// a binder that shadows a parameter, a top-level name, or a name
    /// that does not start lowercase — or its statements are not the
    /// binding-then-answer shape the encoding knows.
    StepShape,
    /// The step reads a name the driver binds, so the inlined body
    /// would read the driver's binder instead of what the step meant.
    Capture,
    /// The `__stp` namespace or the `<fn>__collected` variant name is
    /// already bound in this program.
    NameTaken,
    /// The merged loop is a collecting loop, but no call site starts
    /// its accumulator empty — nothing would move, so nothing is
    /// committed.
    NoCallSite,
}

impl PairDecline {
    /// One-line explanation, rendered by `--explain-passes`.
    pub(super) const fn reason(self) -> &'static str {
        match self {
            Self::StepVisible => "the step fn is visible outside this module",
            Self::StepShared => "the step fn has more than one call site",
            Self::StepEffects => "the step fn declares effects",
            Self::StepSelfRecursive => "the step fn recurses into itself",
            Self::StepShape => "the step fn's body is not a shape the inline can carry",
            Self::Capture => "the step fn reads a name the driver binds",
            Self::NameTaken => "the __stp namespace or the __collected variant name is taken",
            Self::NoCallSite => "no call site starts the merged loop's accumulator empty",
        }
    }
}

/// How many rounds of step-inlining one driver may take. Robin's
/// deepest chain is driver → step → step — three functions, two rounds.
/// The bound exists so a shape nobody has imagined cannot spin; it is
/// not a budget anyone is expected to reach.
const MAX_ROUNDS: usize = 8;

/// Is there a driver-and-step pair here at all? Read-only twin of the
/// detection below, for callers that must decide whether to COPY a
/// module before running the pass — same contract as
/// [`super::has_list_build_shape`], which calls this.
pub(super) fn has_driver_step_shape(items: &[TopLevel]) -> bool {
    crate::ir::chars_fusion::fn_defs(items).any(|fd| is_driver_shaped(fd, items))
}

/// Run the normalization: find every driver whose self-recursive tail
/// path goes through inlinable step companions, inline them on a copy,
/// and commit the copy only when the merged loop fuses. Records what it
/// did — and what it declined — on the pass report.
pub(super) fn run_driver_step_normalize(items: &mut [TopLevel], report: &mut ListBuildPassReport) {
    let drivers: Vec<String> = crate::ir::chars_fusion::fn_defs(items)
        .filter(|fd| is_driver_shaped(fd, items))
        .map(|fd| fd.name.clone())
        .collect();
    if drivers.is_empty() {
        return;
    }

    // Every fact below is computed against the PRISTINE module, before
    // any driver's body is committed: reference counts, visibility,
    // binder sets. An inline never edits a step and never adds a call,
    // so the pristine answers stay true for every driver in turn.
    let taken = crate::ir::chars_fusion::taken_names(items);
    let stp_taken = taken.iter().any(|n| n.starts_with(STEP_PREFIX));
    let exposes: Option<Vec<String>> =
        crate::visibility::effective_exposes(items).map(|e| e.to_vec());
    let top_level = top_level_names(items);
    let facts = StepFacts {
        items: items.to_vec(),
        taken,
        exposes,
        top_level,
    };

    for driver_name in drivers {
        let fd = crate::ir::chars_fusion::fn_defs(&facts.items)
            .find(|fd| fd.name == driver_name)
            .expect("driver came from this item list");
        let outcome = if stp_taken {
            Err(PairDecline::NameTaken.reason())
        } else {
            build_and_validate(fd, &facts)
        };
        match outcome {
            Ok((body, steps)) => {
                let committed = crate::ir::chars_fusion::fn_defs_mut(items)
                    .find(|fd| fd.name == driver_name)
                    .expect("driver came from this item list");
                committed.body = Arc::new(body);
                report.pair_inlined_by_fn.insert(driver_name, steps);
            }
            Err(reason) => {
                report.pair_declined.insert(driver_name, reason);
            }
        }
    }
}

/// The pristine-module facts every step check reads.
struct StepFacts {
    items: Vec<TopLevel>,
    taken: HashSet<String>,
    exposes: Option<Vec<String>>,
    top_level: HashSet<String>,
}

/// Inline the driver's step chain on a copy and validate that the
/// merged loop fuses. Returns the body to commit and the steps that
/// went into it, or the reason the pair keeps its two functions.
fn build_and_validate(
    fd: &FnDef,
    facts: &StepFacts,
) -> Result<(FnBody, Vec<String>), &'static str> {
    let mut body = fd.body.as_ref().clone();
    let mut steps: Vec<String> = Vec::new();
    let driver_bound = bound_names_of(fd);
    // One allocator for the whole build: every synthesized name in
    // every round comes from here, so no two are ever equal.
    let mut fresh = FreshNames::default();

    for _ in 0..MAX_ROUNDS {
        let Some(target) = first_companion_target(&body, &fd.name) else {
            break;
        };
        // A tail call's target is a fn of this module by construction —
        // the TCO pass only marks calls between SCC members — but a
        // shape that breaks the assumption costs a fusion, never a
        // panic.
        let Some(step) = crate::ir::chars_fusion::fn_defs(&facts.items).find(|s| s.name == target)
        else {
            return Err(PairDecline::StepShape.reason());
        };
        check_step_conditions(step, facts).map_err(PairDecline::reason)?;
        let inlined = inline_step_at(&mut body, step, &driver_bound, &facts.top_level, &mut fresh)
            .map_err(PairDecline::reason)?;
        if !inlined {
            // The walk saw the target but the replacement did not land —
            // a shape mismatch this stage does not know. Leave the pair.
            return Err(PairDecline::StepShape.reason());
        }
        steps.push(target);
    }
    if steps.is_empty() || first_companion_target(&body, &fd.name).is_some() {
        // Either nothing was inlined, or the round budget ran out with
        // companions still in the tail path — both mean the merged body
        // is not the single loop candidacy expects.
        return Err(PairDecline::StepShape.reason());
    }

    // The merged loop must now clear exactly the bars the main pass
    // holds it to — candidacy, the variant build, the names — plus one
    // call site that starts the accumulator empty, so the commit is
    // never an inline without a payoff.
    let mut merged = fd.clone();
    merged.body = Arc::new(body);
    let Some(acc) = list_build_acc_of(&merged) else {
        return Err(PairDecline::StepShape.reason());
    };
    let new_name = format!("{}{}", fd.name, super::list_build::COLLECTED_SUFFIX);
    if builder_namespace_taken(&facts.taken) || facts.taken.contains(&new_name) {
        return Err(PairDecline::NameTaken.reason());
    }
    let (_, kind) = build_collected_variant(&merged, &acc, &new_name).map_err(|d| d.reason())?;

    let mut accepted: HashMap<String, (String, ListBuildShape)> = HashMap::new();
    accepted.insert(fd.name.clone(), (new_name, ListBuildShape { acc, kind }));
    let has_site = crate::ir::chars_fusion::fn_defs(&facts.items).any(|other| {
        let body = if other.name == fd.name {
            merged.body.as_ref()
        } else {
            other.body.as_ref()
        };
        body.stmts().iter().any(|stmt| {
            expr_has_rewritable_site(crate::ir::chars_fusion::stmt_expr(stmt), &accepted)
        })
    });
    if !has_site {
        return Err(PairDecline::NoCallSite.reason());
    }

    let FnBody::Block(_) = merged.body.as_ref();
    Ok((merged.body.as_ref().clone(), steps))
}

/// Every condition a step must clear before its body travels.
fn check_step_conditions(step: &FnDef, facts: &StepFacts) -> Result<(), PairDecline> {
    if crate::visibility::is_exposed(&step.name, facts.exposes.as_deref()) {
        return Err(PairDecline::StepVisible);
    }
    if !step.effects.is_empty() {
        return Err(PairDecline::StepEffects);
    }
    if references_in_fn(step, &step.name) > 0 {
        return Err(PairDecline::StepSelfRecursive);
    }
    let refs: usize = facts
        .items
        .iter()
        .map(|item| references_in_top_level(item, &step.name))
        .sum();
    if refs != 1 {
        return Err(PairDecline::StepShared);
    }
    Ok(())
}

/// A fn is driver-shaped when it has an accumulator-position `List`
/// parameter and its tail path runs through a companion whose chain
/// tail-calls back into it with a `List.prepend` in that accumulator's
/// position — the orientation that tells the driver of a pair from its
/// step, which sees the same mutual recursion from the other side.
fn is_driver_shaped(fd: &FnDef, items: &[TopLevel]) -> bool {
    let Some((acc_idx, _)) = rightmost_list_param(fd) else {
        return false;
    };
    let mut queue: Vec<String> = Vec::new();
    for stmt in fd.body.stmts() {
        walk_tail_calls(crate::ir::chars_fusion::stmt_expr(stmt), &mut |data| {
            if data.target != fd.name {
                queue.push(data.target.clone());
            }
        });
    }
    if queue.is_empty() {
        return false;
    }
    // Follow the companion chain's tail calls, looking for the append
    // aimed back at this fn. Bounded by the visited set: each fn is
    // walked once.
    let mut visited: HashSet<String> = HashSet::new();
    while let Some(name) = queue.pop() {
        if name == fd.name || !visited.insert(name.clone()) {
            continue;
        }
        let Some(step) = crate::ir::chars_fusion::fn_defs(items).find(|s| s.name == name) else {
            continue;
        };
        let mut found_append = false;
        for stmt in step.body.stmts() {
            walk_tail_calls(crate::ir::chars_fusion::stmt_expr(stmt), &mut |data| {
                if data.target == fd.name {
                    if let Some(arg) = data.args.get(acc_idx)
                        && is_prepend_to_some_ident(&arg.node)
                    {
                        found_append = true;
                    }
                } else if data.target != step.name {
                    queue.push(data.target.clone());
                }
            });
        }
        if found_append {
            return true;
        }
    }
    false
}

/// Is `expr` `List.prepend(<anything>, <some identifier>)` — the append
/// shape with the accumulator's identity left to the real recogniser?
fn is_prepend_to_some_ident(expr: &Expr) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    is_dotted_ident(&callee.node, "List", "prepend")
        && args.len() == 2
        && matches!(&args[1].node, Expr::Ident(_))
}

/// The rightmost `List<…>` parameter — the same accumulator rule the
/// recognisers use.
fn rightmost_list_param(fd: &FnDef) -> Option<(usize, String)> {
    fd.params
        .iter()
        .enumerate()
        .rfind(|(_, (_, ty))| is_list_type_str(ty))
        .map(|(i, (name, _))| (i, name.clone()))
}

/// The first tail call in `body` whose target is another fn — the
/// companion the next round inlines. Preorder, so the answer is
/// deterministic. Every `TailCall` node is in tail position by the TCO
/// pass's construction, so a full walk and a tail-position walk find
/// the same set.
fn first_companion_target(body: &FnBody, self_name: &str) -> Option<String> {
    for stmt in body.stmts() {
        let mut found: Option<String> = None;
        walk_tail_calls(crate::ir::chars_fusion::stmt_expr(stmt), &mut |data| {
            if found.is_none() && data.target != self_name {
                found = Some(data.target.clone());
            }
        });
        if found.is_some() {
            return found;
        }
    }
    None
}

/// Visit every `TailCall` under `expr`, preorder.
fn walk_tail_calls(expr: &Spanned<Expr>, f: &mut impl FnMut(&TailCallData)) {
    if let Expr::TailCall(data) = &expr.node {
        f(data);
    }
    crate::ir::chars_fusion::walk_children(expr, &mut |child| walk_tail_calls(child, f));
}

/// How many times `fd`'s body references `name` — identifier reads plus
/// tail-call targets, which the identifier count cannot see because a
/// target is a string on the node rather than an `Ident` under it.
fn references_in_fn(fd: &FnDef, name: &str) -> usize {
    fd.body
        .stmts()
        .iter()
        .map(|stmt| references_in_expr(crate::ir::chars_fusion::stmt_expr(stmt), name))
        .sum()
}

fn references_in_expr(expr: &Spanned<Expr>, name: &str) -> usize {
    count_ident_reads(&expr.node, name) + count_tail_targets(expr, name)
}

/// How many times any top-level item references `name` — the
/// module-wide census behind the one-call-site condition. The claim is
/// "exactly one call site in the whole module", so the census walks
/// every place an expression can live, not just fn bodies: verify
/// blocks (cases, per-case givens, and the law form's template,
/// `when`, sample guards, and explicit given domains) and top-level
/// statements. Decision blocks carry no expressions — their impacts
/// are documentation strings (`DecisionImpact`), so a call cannot be
/// spelled there — and the module header, type defs, and capability
/// declarations have no expression positions at all.
///
/// One carve-out, from this stage's own contract: the step is never
/// edited or removed, so a verify block ON THE STEP ITSELF keeps the
/// exact function it specifies — its calls are the spec's, not a
/// second consumer's, and real corpora verify their step fns directly.
/// A step called from any OTHER verify block is shared code and
/// declines like any other extra call site.
fn references_in_top_level(item: &TopLevel, name: &str) -> usize {
    match item {
        TopLevel::FnDef(fd) => references_in_fn(fd, name),
        TopLevel::Verify(vb) if vb.fn_name == name => 0,
        TopLevel::Verify(vb) => {
            let in_domain = |given: &crate::ast::VerifyGiven| match &given.domain {
                crate::ast::VerifyGivenDomain::Explicit(values) => values
                    .iter()
                    .map(|v| references_in_expr(v, name))
                    .sum::<usize>(),
                crate::ast::VerifyGivenDomain::IntRange { .. } => 0,
            };
            let mut refs: usize = vb
                .cases
                .iter()
                .flat_map(|(lhs, rhs)| [lhs, rhs])
                .chain(vb.case_givens.iter().flatten().map(|(_, value)| value))
                .map(|e| references_in_expr(e, name))
                .sum();
            refs += vb.cases_givens.iter().map(in_domain).sum::<usize>();
            if let crate::ast::VerifyKind::Law(law) = &vb.kind {
                refs += references_in_expr(&law.lhs, name) + references_in_expr(&law.rhs, name);
                refs += law
                    .when
                    .as_ref()
                    .map_or(0, |when| references_in_expr(when, name));
                refs += law
                    .sample_guards
                    .iter()
                    .map(|g| references_in_expr(g, name))
                    .sum::<usize>();
                refs += law.givens.iter().map(in_domain).sum::<usize>();
            }
            refs
        }
        TopLevel::Stmt(stmt) => references_in_expr(crate::ir::chars_fusion::stmt_expr(stmt), name),
        TopLevel::Module(_)
        | TopLevel::Decision(_)
        | TopLevel::TypeDef(_)
        | TopLevel::Capability(_) => 0,
    }
}

/// How many `TailCall` nodes under `expr` target `name`.
fn count_tail_targets(expr: &Spanned<Expr>, name: &str) -> usize {
    let mut count = 0usize;
    walk_tail_calls(expr, &mut |data| {
        if data.target == name {
            count += 1;
        }
    });
    count
}

/// Every top-level name in the program — fns, types, program-level
/// bindings. A step binder that shadows one of these is a binder whose
/// reads the mechanical rename cannot tell from reads of the top-level
/// thing, so such steps are declined.
fn top_level_names(items: &[TopLevel]) -> HashSet<String> {
    items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            TopLevel::TypeDef(
                crate::ast::TypeDef::Sum { name, .. } | crate::ast::TypeDef::Product { name, .. },
            ) => Some(name.clone()),
            TopLevel::Stmt(Stmt::Binding(name, _, _)) => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// Every name `fd` binds anywhere — parameters, statement bindings,
/// pattern binders. The capture check reads this program-wide rather
/// than scope-by-scope: a lost fusion is acceptable, a captured read is
/// not, and one set is one answer.
fn bound_names_of(fd: &FnDef) -> HashSet<String> {
    let mut out: HashSet<String> = fd.params.iter().map(|(n, _)| n.clone()).collect();
    for stmt in fd.body.stmts() {
        if let Stmt::Binding(name, _, _) = stmt {
            out.insert(name.clone());
        }
        collect_pattern_binders_into(crate::ir::chars_fusion::stmt_expr(stmt), &mut out);
    }
    out
}

/// Every name a pattern under `expr` binds.
fn collect_pattern_binders_into(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    if let Expr::Match { arms, .. } = &expr.node {
        for arm in arms {
            collect_binders_of_pattern(&arm.pattern, out);
        }
    }
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        collect_pattern_binders_into(child, out);
    });
}

fn collect_binders_of_pattern(pattern: &Pattern, out: &mut HashSet<String>) {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        Pattern::Ident(n) => {
            out.insert(n.clone());
        }
        Pattern::Cons(head, tail) => {
            out.insert(head.clone());
            out.insert(tail.clone());
        }
        Pattern::Tuple(items) => items
            .iter()
            .for_each(|p| collect_binders_of_pattern(p, out)),
        Pattern::Constructor(_, bindings) => out.extend(bindings.iter().cloned()),
    }
}

/// Find the one tail call to `step` in `body` and replace it with the
/// step's inlined body. Returns whether a replacement landed.
fn inline_step_at(
    body: &mut FnBody,
    step: &FnDef,
    driver_bound: &HashSet<String>,
    top_level: &HashSet<String>,
    fresh: &mut FreshNames,
) -> Result<bool, PairDecline> {
    // Build the replacement first, so a decline leaves the body copy
    // exactly as it was.
    let mut done = false;
    let mut outcome: Result<(), PairDecline> = Ok(());
    for stmt in body.stmts_mut() {
        replace_step_call(
            crate::ir::chars_fusion::stmt_expr_mut(stmt),
            step,
            driver_bound,
            top_level,
            fresh,
            &mut done,
            &mut outcome,
        );
    }
    outcome.map(|()| done)
}

/// Depth-first hunt for `TailCall(step)`, replacing the first one found.
#[allow(clippy::too_many_arguments)]
fn replace_step_call(
    expr: &mut Spanned<Expr>,
    step: &FnDef,
    driver_bound: &HashSet<String>,
    top_level: &HashSet<String>,
    fresh: &mut FreshNames,
    done: &mut bool,
    outcome: &mut Result<(), PairDecline>,
) {
    if *done || outcome.is_err() {
        return;
    }
    if let Expr::TailCall(data) = &expr.node
        && data.target == step.name
    {
        match encode_step_body(step, &data.args, driver_bound, top_level, fresh) {
            Ok(inlined) => {
                *expr = inlined;
                *done = true;
            }
            Err(decline) => *outcome = Err(decline),
        }
        return;
    }
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        replace_step_call(child, step, driver_bound, top_level, fresh, done, outcome);
    });
}

/// The step's body as one expression, hygienically renamed and with the
/// call's arguments substituted in — the whole inline, or the reason it
/// cannot be done.
fn encode_step_body(
    step: &FnDef,
    args: &[Spanned<Expr>],
    driver_bound: &HashSet<String>,
    top_level: &HashSet<String>,
    fresh: &mut FreshNames,
) -> Result<Spanned<Expr>, PairDecline> {
    if args.len() != step.params.len() {
        return Err(PairDecline::StepShape);
    }

    // The step's own binders, checked against everything the mechanical
    // rename relies on: distinct from the parameters (a shadowed
    // parameter would make substitution rewrite the shadow's reads),
    // distinct from top-level names (a read above the binder would be
    // renamed with it), and lowercase (an uppercase name could be a
    // namespace or constructor read the rename must not touch).
    let mut binders: HashSet<String> = HashSet::new();
    for stmt in step.body.stmts() {
        if let Stmt::Binding(name, _, _) = stmt {
            binders.insert(name.clone());
        }
        collect_pattern_binders_into(crate::ir::chars_fusion::stmt_expr(stmt), &mut binders);
    }
    let params: HashSet<&str> = step.params.iter().map(|(n, _)| n.as_str()).collect();
    for binder in &binders {
        let lowercase_start = binder
            .chars()
            .next()
            .is_some_and(|c| c.is_lowercase() || c == '_');
        if params.contains(binder.as_str()) || top_level.contains(binder) || !lowercase_start {
            return Err(PairDecline::StepShape);
        }
    }

    // Capture: a name the step reads that is neither its parameter nor
    // its binder resolves at top level — unless the driver binds it, in
    // which case the inlined read would resolve to the driver's binder
    // instead. Tail-call targets ride along for the same reason.
    let mut free: HashSet<String> = HashSet::new();
    for stmt in step.body.stmts() {
        collect_free_names(
            crate::ir::chars_fusion::stmt_expr(stmt),
            &binders,
            &params,
            &mut free,
        );
    }
    if free.iter().any(|name| driver_bound.contains(name)) {
        return Err(PairDecline::Capture);
    }

    // The body travels as a copy: rename the binders, substitute the
    // parameters, then fold the statements into one expression.
    let FnBody::Block(stmts) = step.body.as_ref();
    let mut stmts = stmts.clone();
    let Some(Stmt::Expr(_)) = stmts.last() else {
        return Err(PairDecline::StepShape);
    };

    // The rename map's keys are the step's original binder names;
    // every output comes from the one allocator. Allocation walks the
    // binders in sorted order so the synthesized names are
    // deterministic across runs — the set's own order is not.
    let mut ordered: Vec<String> = binders.iter().cloned().collect();
    ordered.sort();
    let rename: HashMap<String, String> = ordered
        .into_iter()
        .map(|n| (n, fresh.next_name()))
        .collect();
    for stmt in stmts.iter_mut() {
        if let Stmt::Binding(name, _, _) = stmt
            && let Some(fresh) = rename.get(name)
        {
            *name = fresh.clone();
        }
        rename_names(crate::ir::chars_fusion::stmt_expr_mut(stmt), &rename);
    }

    // Parameters: a bare identifier or a literal substitutes directly —
    // its reads are pure and cannot change between the call and the
    // read. Anything else is bound first, in argument order, so
    // whatever the argument computes happens exactly once and exactly
    // where the call evaluated it. The map is built COMPLETE before any
    // rewriting and applied in ONE walk — substitution must be
    // SIMULTANEOUS, because a call-site argument may itself be spelled
    // like another parameter of the step. Substituting one parameter at
    // a time re-visited the identifiers just inserted for the earlier
    // parameters, so an argument spelled like a LATER parameter was
    // rewritten again by that parameter's pass: `step(b, c, t2, acc)`
    // against params `(a, b, st, sacc)` turned `a*10 + b` into
    // `c*10 + c` — the same wrong answer on both backends, invisible to
    // the cross-backend differential.
    let mut bound_args: Vec<(String, Spanned<Expr>)> = Vec::new();
    let mut subst: HashMap<String, Spanned<Expr>> = HashMap::new();
    for ((param, _), arg) in step.params.iter().zip(args.iter()) {
        let replacement = match &arg.node {
            Expr::Ident(_) | Expr::Literal(_) => arg.clone(),
            _ => {
                // The argument binder comes from the same allocator as
                // the binder renames above, so no binder rename can
                // spell it — the shadow a `p<idx>`-shaped second
                // namespace let a step binder cast.
                let name = fresh.next_name();
                let read = sp_at(arg.line, Expr::Ident(name.clone()));
                if let Some(ty) = arg.ty() {
                    read.set_ty(ty.clone());
                }
                bound_args.push((name, arg.clone()));
                read
            }
        };
        subst.insert(param.clone(), replacement);
    }
    for stmt in stmts.iter_mut() {
        substitute_reads(crate::ir::chars_fusion::stmt_expr_mut(stmt), &subst);
    }

    // Fold the statements into one expression, back to front: each
    // binding becomes a single-arm match on its value — the let-in this
    // AST spells as an irrefutable match — and a bare statement, which
    // a pure body can only have as a value it then ignores, binds to a
    // wildcard.
    let Some(Stmt::Expr(mut expr)) = stmts.pop() else {
        unreachable!("the last statement was checked above");
    };
    for stmt in stmts.into_iter().rev() {
        expr = match stmt {
            Stmt::Binding(name, _, value) => bind_over(value, Pattern::Ident(name), expr),
            Stmt::Expr(value) => bind_over(value, Pattern::Wildcard, expr),
        };
    }
    for (fresh, arg) in bound_args.into_iter().rev() {
        expr = bind_over(arg, Pattern::Ident(fresh), expr);
    }
    Ok(expr)
}

/// `match <value> { <pattern> -> <body> }` — the let-in encoding.
/// Carries the body's type: the match answers what its only arm does.
fn bind_over(value: Spanned<Expr>, pattern: Pattern, body: Spanned<Expr>) -> Spanned<Expr> {
    let line = value.line;
    let ty = body.ty().cloned();
    let arm = MatchArm::new(pattern, body);
    let out = sp_at(
        line,
        Expr::Match {
            subject: Box::new(value),
            arms: vec![arm],
        },
    );
    if let Some(ty) = ty {
        out.set_ty(ty);
    }
    out
}

/// Every name read under `expr` that is neither a parameter nor a
/// binder of the step — the names that must still mean their top-level
/// thing after the move. Tail-call targets are included: they are fn
/// references spelled as strings.
fn collect_free_names(
    expr: &Spanned<Expr>,
    binders: &HashSet<String>,
    params: &HashSet<&str>,
    out: &mut HashSet<String>,
) {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. }
            if !binders.contains(n) && !params.contains(n.as_str()) =>
        {
            out.insert(n.clone());
        }
        Expr::TailCall(data) => {
            out.insert(data.target.clone());
        }
        _ => {}
    }
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        collect_free_names(child, binders, params, out);
    });
}

/// Rename every identifier read and pattern binder whose name is in the
/// map. Statement-binding names are the caller's half — they are not
/// expressions.
fn rename_names(expr: &mut Spanned<Expr>, rename: &HashMap<String, String>) {
    match &mut expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => {
            if let Some(fresh) = rename.get(n) {
                *n = fresh.clone();
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms.iter_mut() {
                rename_pattern(&mut arm.pattern, rename);
            }
        }
        _ => {}
    }
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| rename_names(child, rename));
}

fn rename_pattern(pattern: &mut Pattern, rename: &HashMap<String, String>) {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        Pattern::Ident(n) => {
            if let Some(fresh) = rename.get(n) {
                *n = fresh.clone();
            }
        }
        Pattern::Cons(head, tail) => {
            if let Some(fresh) = rename.get(head) {
                *head = fresh.clone();
            }
            if let Some(fresh) = rename.get(tail) {
                *tail = fresh.clone();
            }
        }
        Pattern::Tuple(items) => items.iter_mut().for_each(|p| rename_pattern(p, rename)),
        Pattern::Constructor(_, bindings) => {
            for b in bindings.iter_mut() {
                if let Some(fresh) = rename.get(b) {
                    *b = fresh.clone();
                }
            }
        }
    }
}

/// Replace every parameter read with its argument expression — all
/// parameters SIMULTANEOUSLY, from one complete map. A replacement is
/// inserted and never descended into, so an argument identifier
/// spelled like another parameter of the step cannot be rewritten by
/// that parameter's substitution — the capture the sequential
/// per-parameter walk this replaced committed. Patterns never bind a
/// parameter here — a step whose body shadows a parameter was declined
/// before this runs — so every `Ident(param)` is the read.
fn substitute_reads(expr: &mut Spanned<Expr>, subst: &HashMap<String, Spanned<Expr>>) {
    if let Expr::Ident(n) = &expr.node
        && let Some(arg) = subst.get(n)
    {
        *expr = arg.clone();
        return;
    }
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        substitute_reads(child, subst);
    });
}

/// Does any expression under `expr` match a rewritable call site of an
/// accepted loop? Read-only twin of the site rewrite, used to prove the
/// commit will have a payoff.
fn expr_has_rewritable_site(
    expr: &Spanned<Expr>,
    accepted: &HashMap<String, (String, ListBuildShape)>,
) -> bool {
    if try_rewrite_list_build_site(expr, accepted).is_some() {
        return true;
    }
    let mut found = false;
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        found = found || expr_has_rewritable_site(child, accepted);
    });
    found
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse, tail-call-convert, and run the list-build pass — the
    /// same prefix of the pipeline the pass sees in production.
    fn pass_on(source: &str) -> super::super::list_build::ListBuildPassReport {
        let mut items = crate::source::parse_source(source).expect("fixture parses");
        crate::tco::transform_program(&mut items);
        super::super::list_build::run_list_build_pass(&mut items)
    }

    /// The canonical pair, parameterized so the matrix tests below can
    /// vary the step's binder name and where the binder sits. The step
    /// works through a binding, so the inline has a statement to carry.
    fn pair_program(binder: &str, binder_in_pattern: bool) -> String {
        let step_body = if binder_in_pattern {
            format!(
                "    match Option.Some(sh * 2)\n        Option.Some({binder}) -> drive(st, List.prepend({binder}, sacc))\n        Option.None -> []"
            )
        } else {
            format!("    {binder} = sh * 2\n    drive(st, List.prepend({binder}, sacc))")
        };
        format!(
            r#"module Pairs
    intent = "Driver-and-step fixture."
    exposes [entry]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(sh: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
{step_body}

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#
        )
    }

    #[test]
    fn the_canonical_pair_fuses() {
        let report = pass_on(&pair_program("v", false));
        assert_eq!(
            report.pair_inlined_by_fn.get("drive"),
            Some(&vec!["step".to_string()]),
            "the step is inlined into the driver: {report:?}"
        );
        assert_eq!(report.builder_fns, vec!["drive".to_string()]);
        assert_eq!(report.rewrites, 1, "the entry call site moves");
        assert!(report.pair_declined.is_empty());
    }

    /// The bare-name identity class recurs in this family, so hygiene
    /// is asserted as an ORDER-CONTROLLED MATRIX over binder positions
    /// rather than a single case: every name the driver already owns,
    /// in both binder positions the step can put it in. Every cell must
    /// fuse — the mechanical rename makes the collision unspellable —
    /// and the answers for the same collisions are pinned end-to-end in
    /// `tests/driver_step_pairs.rs`.
    #[test]
    fn step_binders_colliding_with_driver_names_fuse_in_every_position() {
        // xs / acc: the driver's params. h / t: the driver's pattern
        // binders, bound BETWEEN the driver's head and the inline
        // point. v: the no-collision control.
        for binder in ["xs", "acc", "h", "t", "v"] {
            for in_pattern in [false, true] {
                let report = pass_on(&pair_program(binder, in_pattern));
                assert!(
                    report.pair_inlined_by_fn.contains_key("drive"),
                    "binder {binder:?} (in_pattern: {in_pattern}) must not block the \
                     inline: {report:?}"
                );
                assert_eq!(
                    report.builder_fns,
                    vec!["drive".to_string()],
                    "binder {binder:?} (in_pattern: {in_pattern}) must fuse"
                );
            }
        }
    }

    /// One cell of the ARGUMENT-SPELLING matrix: the driver peels two
    /// elements per round, so its call site has two value arguments to
    /// dress in another parameter's name. The bound flags wrap an
    /// argument in `+ 0`, pushing it off the substituted path onto the
    /// bound-args path.
    fn argument_program(first: &str, second: &str, bound: (bool, bool)) -> String {
        let arg1 = if bound.0 {
            format!("{first} + 0")
        } else {
            first.to_string()
        };
        let arg2 = if bound.1 {
            format!("{second} + 0")
        } else {
            second.to_string()
        };
        format!(
            r#"module ArgMatrix
    intent = "Argument-spelling fixture."
    exposes [entry]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [{first}, ..t] -> match t
            [] -> List.reverse(acc)
            [{second}, ..t2] -> step({arg1}, {arg2}, t2, acc)

fn step(sa: Int, sb: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    v = sa * 10 + sb
    drive(st, List.prepend(v, sacc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#
        )
    }

    /// The ARGUMENT/PARAMETER axis, order-controlled like the binder
    /// matrix: a call-site argument spelled like an earlier parameter,
    /// a later one, the same one, and a step binder — each on the
    /// substituted path and on the bound-args path. Substitution is
    /// simultaneous, so every cell fuses; the later-param spelling is
    /// the cell sequential substitution silently got wrong (the
    /// end-to-end answers for all cells are pinned in
    /// `tests/driver_step_pairs.rs`).
    #[test]
    fn call_site_arguments_spelled_like_step_names_fuse_in_every_cell() {
        let cells: &[(&str, &str, (bool, bool))] = &[
            // Like a LATER param: sb (the step's second) worn by arg 1.
            ("sb", "y", (false, false)),
            ("sb", "y", (true, false)),
            // Like an EARLIER param: sa worn by arg 2.
            ("x", "sa", (false, false)),
            ("x", "sa", (false, true)),
            // Like the SAME param: sa worn by its own argument.
            ("sa", "y", (false, false)),
            ("sa", "y", (true, false)),
            // Like a STEP BINDER: v is the step's own binding.
            ("v", "y", (false, false)),
            ("v", "y", (true, false)),
        ];
        for (first, second, bound) in cells {
            let report = pass_on(&argument_program(first, second, *bound));
            assert_eq!(
                report.pair_inlined_by_fn.get("drive"),
                Some(&vec!["step".to_string()]),
                "arguments ({first:?}, {second:?}) (bound: {bound:?}) must not block \
                 the inline: {report:?}"
            );
            assert_eq!(
                report.builder_fns,
                vec!["drive".to_string()],
                "arguments ({first:?}, {second:?}) (bound: {bound:?}) must fuse"
            );
        }
    }

    /// One cell of the P-BINDER matrix: the step's binder is spelled
    /// like the index-derived half of a synthesized argument name
    /// (`p0`, `p1`), and the step reads both parameters again after
    /// the binding, so a collision between synthesized names changes
    /// the answer instead of hiding.
    fn p_binder_program(binder: &str, bound: (bool, bool)) -> String {
        let arg1 = if bound.0 { "x + 0" } else { "x" };
        let arg2 = if bound.1 { "y + 0" } else { "y" };
        format!(
            r#"module PBinderMatrix
    intent = "P-binder collision fixture."
    exposes [entry]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [x, ..t] -> match t
            [] -> List.reverse(acc)
            [y, ..t2] -> step({arg1}, {arg2}, t2, acc)

fn step(sa: Int, sb: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    {binder} = sa + sb
    drive(st, List.prepend({binder} * 10 + sa + sb, sacc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#
        )
    }

    /// The P-BINDER axis of the hygiene matrix: a step binder spelled
    /// like the index-derived half of a synthesized argument name,
    /// with the argument at that index substituted, bound, and bound
    /// at the OTHER index. Every synthesized name comes from one
    /// allocator, so the spelling cannot collide with the name minted
    /// for the bound argument and every cell fuses; the running
    /// answers for the same cells are pinned end-to-end in
    /// `tests/driver_step_pairs.rs`.
    #[test]
    fn step_binders_spelled_like_argument_indices_fuse_in_every_cell() {
        let cells: &[(&str, (bool, bool))] = &[
            ("p0", (false, false)),
            ("p0", (true, false)),
            ("p0", (false, true)),
            ("p1", (false, false)),
            ("p1", (false, true)),
            ("p1", (true, false)),
        ];
        for (binder, bound) in cells {
            let report = pass_on(&p_binder_program(binder, *bound));
            assert_eq!(
                report.pair_inlined_by_fn.get("drive"),
                Some(&vec!["step".to_string()]),
                "binder {binder:?} (bound: {bound:?}) must not block the inline: {report:?}"
            );
            assert_eq!(
                report.builder_fns,
                vec!["drive".to_string()],
                "binder {binder:?} (bound: {bound:?}) must fuse"
            );
        }
    }

    /// A step binder that shadows one of the step's own parameters:
    /// substitution would rewrite the shadow's reads, so the pair is
    /// declined — in both binder positions.
    #[test]
    fn a_step_binder_shadowing_a_step_param_declines_in_every_position() {
        for binder in ["sh", "st", "sacc"] {
            for in_pattern in [false, true] {
                // The binder wears one of the step's own param names,
                // shadowing it for everything underneath.
                let report = pass_on(&pair_program(binder, in_pattern));
                assert_eq!(
                    report.pair_declined.get("drive").copied(),
                    Some(PairDecline::StepShape.reason()),
                    "binder {binder:?} (in_pattern: {in_pattern}): {report:?}"
                );
                assert!(report.pair_inlined_by_fn.is_empty());
            }
        }
    }

    /// A step binder that shares a top-level fn's name: reads before
    /// the binder would be renamed with it, so the pair is declined.
    #[test]
    fn a_step_binder_named_like_a_top_level_fn_declines() {
        let report = pass_on(&pair_program("entry", false));
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepShape.reason()),
            "{report:?}"
        );
    }

    /// The step reads a top-level fn the driver re-binds — inlined, the
    /// read would resolve to the driver's binder. Declined, and the
    /// matrix runs the driver's binder through both of its positions.
    #[test]
    fn a_step_reading_a_name_the_driver_binds_declines_in_every_position() {
        for driver_arm in [
            // The colliding name bound by the driver's own cons pattern.
            "        [scale, ..t] -> step(scale, t, acc)",
            // The colliding name bound by a nested pattern around the call.
            "        [h, ..t] -> match Int.div(h, 1)\n            Result.Ok(scale) -> step(scale, t, acc)\n            Result.Err(msg) -> acc",
        ] {
            let source = format!(
                r#"module Capture
    intent = "The step reads scale; the driver binds it."
    exposes [entry]

fn scale(n: Int) -> Int
    n * 10

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
{driver_arm}

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    drive(t, List.prepend(scale(h), acc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#
            );
            let report = pass_on(&source);
            assert_eq!(
                report.pair_declined.get("drive").copied(),
                Some(PairDecline::Capture.reason()),
                "arm {driver_arm:?}: {report:?}"
            );
            assert!(report.pair_inlined_by_fn.is_empty());
        }
    }

    /// The same program with the driver binder renamed is the control:
    /// nothing is captured, and the pair fuses.
    #[test]
    fn the_capture_control_with_a_fresh_driver_binder_fuses() {
        let source = r#"module CaptureControl
    intent = "The step reads scale; the driver does not bind it."
    exposes [entry]

fn scale(n: Int) -> Int
    n * 10

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    drive(t, List.prepend(scale(h), acc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert!(
            report.pair_inlined_by_fn.contains_key("drive"),
            "{report:?}"
        );
    }

    /// The one-call-site condition claims the WHOLE MODULE, so a
    /// second call in a verify block counts: the census walks every
    /// expression-bearing top-level item, not just fn bodies. Here the
    /// extra call lives in ANOTHER fn's verify block — a second
    /// consumer of the step, spelled in spec position.
    #[test]
    fn a_step_called_again_from_a_verify_block_declines() {
        let source = format!(
            "{}\nverify entry\n    entry([2]) => step(2, [], [])\n",
            pair_program("v", false)
        );
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepShared.reason()),
            "{report:?}"
        );
        assert!(report.pair_inlined_by_fn.is_empty());
    }

    /// The carve-out: a verify block ON THE STEP ITSELF is the step's
    /// spec, and the step it names is never edited or removed — so the
    /// pair still fuses. Real corpora verify their step fns directly;
    /// counting the spec as a second consumer would turn this stage
    /// off exactly where it was built to fire.
    #[test]
    fn a_verify_block_on_the_step_itself_does_not_block_the_inline() {
        let source = format!(
            "{}\nverify step\n    step(2, [1], []) => [4, 2]\n",
            pair_program("v", false)
        );
        let report = pass_on(&source);
        assert!(
            report.pair_inlined_by_fn.contains_key("drive"),
            "{report:?}"
        );
    }

    /// The control for the verify-block census: a verify block on the
    /// DRIVER references only the driver, so the pair still fuses.
    #[test]
    fn a_verify_block_on_the_driver_does_not_block_the_inline() {
        let source = format!(
            "{}\nverify drive\n    drive([3], []) => [6]\n",
            pair_program("v", false)
        );
        let report = pass_on(&source);
        assert!(
            report.pair_inlined_by_fn.contains_key("drive"),
            "{report:?}"
        );
    }

    /// A top-level statement calling the step is a call site too.
    #[test]
    fn a_step_called_again_from_a_top_level_statement_declines() {
        let source = format!("{}\nwarm = step(2, [1], [])\n", pair_program("v", false));
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepShared.reason()),
            "{report:?}"
        );
    }

    /// A second call site for the step — the decline the idiom's
    /// one-call-site condition exists for.
    #[test]
    fn a_step_with_a_second_call_site_declines() {
        let source = r#"module SharedStep
    intent = "The step is also called from another fn."
    exposes [entry, other]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    drive(t, List.prepend(h * 2, acc))

fn other(h: Int) -> List<Int>
    step(h, [], [])

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepShared.reason()),
            "{report:?}"
        );
        assert!(report.synthesized.is_empty(), "nothing fuses: {report:?}");
    }

    /// A step the module exposes is API surface; callers this pass
    /// cannot see may exist, so the call-site count cannot be trusted.
    #[test]
    fn an_exposed_step_declines() {
        let source = pair_program("v", false).replace("exposes [entry]", "exposes [entry, step]");
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepVisible.reason()),
            "{report:?}"
        );
    }

    /// No module declaration means no exposes list, and the default
    /// rule makes every fn visible — same decline, other spelling.
    #[test]
    fn a_pair_in_a_module_without_an_exposes_list_declines() {
        let source = pair_program("v", false).replace(
            "module Pairs\n    intent = \"Driver-and-step fixture.\"\n    exposes [entry]\n\n",
            "",
        );
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepVisible.reason()),
            "{report:?}"
        );
    }

    /// An effectful step would have its trace reshaped by the move.
    #[test]
    fn an_effectful_step_declines() {
        let source = r#"module EffectfulStep
    intent = "The step prints as it works."
    exposes [entry]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ! [Console.print]
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ! [Console.print]
    Console.print("{h}")
    drive(t, List.prepend(h * 2, acc))

fn entry(xs: List<Int>) -> List<Int>
    ! [Console.print]
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepEffects.reason()),
            "{report:?}"
        );
    }

    /// A self-recursive step is its own loop, not this driver's step.
    #[test]
    fn a_self_recursive_step_declines() {
        let source = r#"module SelfStep
    intent = "The step recurses into itself before the driver."
    exposes [entry]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    match h > 100
        true -> step(h - 100, t, acc)
        false -> drive(t, List.prepend(h, acc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::StepSelfRecursive.reason()),
            "{report:?}"
        );
    }

    /// A program that binds into the `__stp` namespace takes the stage
    /// away — the fresh names must be unspellable by the program.
    #[test]
    fn a_program_binding_into_the_step_namespace_declines() {
        let source =
            pair_program("v", false).replace("    v = sh * 2", "    __stp0_v = 1\n    v = sh * 2");
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::NameTaken.reason()),
            "{report:?}"
        );
    }

    /// A pair whose merged loop would not fuse — the step reads the
    /// accumulator beside the append — is left untouched, with the
    /// recogniser's own reason on the report.
    #[test]
    fn a_pair_whose_merged_loop_cannot_fuse_is_left_untouched() {
        let source = pair_program("v", false).replace(
            "    drive(st, List.prepend(v, sacc))",
            "    drive(st, List.prepend(v + List.len(sacc), sacc))",
        );
        let report = pass_on(&source);
        assert!(
            report.pair_inlined_by_fn.is_empty(),
            "no commit without a fusion: {report:?}"
        );
        assert!(
            report.pair_declined.contains_key("drive"),
            "the pair is reported: {report:?}"
        );
        assert!(report.synthesized.is_empty());
    }

    /// A pair with no empty-started call site fuses to nothing, so
    /// nothing is committed.
    #[test]
    fn a_pair_nobody_calls_with_an_empty_accumulator_is_left_untouched() {
        let source = pair_program("v", false).replace("    drive(xs, [])", "    drive(xs, [9])");
        let report = pass_on(&source);
        assert_eq!(
            report.pair_declined.get("drive").copied(),
            Some(PairDecline::NoCallSite.reason()),
            "{report:?}"
        );
        assert!(report.pair_inlined_by_fn.is_empty());
    }

    /// A three-fn chain — driver, step, step — inlines both, in order.
    #[test]
    fn a_driver_step_step_chain_inlines_both() {
        let source = r#"module Chain
    intent = "Driver, step, and the step's own step."
    exposes [entry]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> stepOne(h, t, acc)

fn stepOne(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    v = h * 2
    stepTwo(v, t, acc)

fn stepTwo(v: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    drive(t, List.prepend(v + 1, acc))

fn entry(xs: List<Int>) -> List<Int>
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert_eq!(
            report.pair_inlined_by_fn.get("drive"),
            Some(&vec!["stepOne".to_string(), "stepTwo".to_string()]),
            "{report:?}"
        );
        assert_eq!(report.builder_fns, vec!["drive".to_string()]);
    }

    /// The record-constructor terminator, driven through a pair: the
    /// reverse lands inside a field, the other fields are ordinary
    /// reads, and the merged loop fuses.
    #[test]
    fn a_pair_with_a_record_constructor_terminator_fuses() {
        let source = r#"module RecordExit
    intent = "The exit wraps the reverse in a record field."
    exposes [entry]

record Gathered
    items: List<Int>
    seen: Int

fn drive(xs: List<Int>, seen: Int, acc: List<Int>) -> Gathered
    match xs
        [] -> Gathered(items = List.reverse(acc), seen = seen)
        [h, ..t] -> step(h, t, seen, acc)

fn step(h: Int, t: List<Int>, seen: Int, acc: List<Int>) -> Gathered
    drive(t, seen + 1, List.prepend(h * 2, acc))

fn entry(xs: List<Int>) -> Gathered
    drive(xs, 0, [])
"#;
        let report = pass_on(source);
        assert!(
            report.pair_inlined_by_fn.contains_key("drive"),
            "{report:?}"
        );
        assert_eq!(report.builder_fns, vec!["drive".to_string()]);
    }

    /// A record-constructor exit whose OTHER field reads the
    /// accumulator is two reads on one path — the discipline the
    /// occurs-check enforces — so the merged loop declines and nothing
    /// is committed.
    #[test]
    fn a_record_exit_whose_other_field_reads_the_accumulator_declines() {
        let source = r#"module RecordLeak
    intent = "A second field reads the accumulator."
    exposes [entry]

record Gathered
    items: List<Int>
    seen: Int

fn drive(xs: List<Int>, acc: List<Int>) -> Gathered
    match xs
        [] -> Gathered(items = List.reverse(acc), seen = List.len(acc))
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> Gathered
    drive(t, List.prepend(h * 2, acc))

fn entry(xs: List<Int>) -> Gathered
    drive(xs, [])
"#;
        let report = pass_on(source);
        assert!(report.pair_inlined_by_fn.is_empty(), "{report:?}");
        assert!(report.synthesized.is_empty(), "{report:?}");
        assert!(report.pair_declined.contains_key("drive"), "{report:?}");
    }
}
