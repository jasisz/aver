//! Bounded, backend-neutral search for concrete earlier-lemma applications.
//! Source steps and rewrite closure are search data, not evidence. Both targets
//! must check every supplier and its application through their ordinary gates.

mod terms;

use std::collections::BTreeSet;

use crate::ast::{TopLevel, VerifyKind};
use crate::ir::proof_ir::{LawApplication, LawTheorem, ProofIR};

use crate::codegen::proof_lower::ProofLowerInputs;
use terms::{Bindings, Term};

/// Limits candidate discovery only. Both backends recheck all applications.
#[derive(Debug, Clone, Copy)]
pub struct ApplicationSearchBudget {
    pub rounds: usize,
    pub terms: usize,
    pub nodes: usize,
    pub applications: usize,
}

impl Default for ApplicationSearchBudget {
    fn default() -> Self {
        Self {
            rounds: 4,
            terms: 64,
            nodes: 512,
            applications: 32,
        }
    }
}

/// Diagnostic search outcome; no field grants proof credit.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ApplicationSearchReport {
    pub disabled: bool,
    pub applications: usize,
    /// Steps whose search reached a resource limit; not a claim of incompleteness.
    pub limited_steps: usize,
}

struct Rule<'a> {
    theorem: &'a LawTheorem,
    lhs: Term,
    rhs: Term,
    variables: BTreeSet<String>,
}

fn rewrite(
    t: &Term,
    rule: &Rule,
    applications: &mut Vec<LawApplication>,
    budget: ApplicationSearchBudget,
    limited: &mut bool,
) -> Term {
    let mut bindings = Bindings::new();
    if terms::matches(&rule.lhs, t, &rule.variables, &mut bindings) {
        let args: Option<Vec<_>> = rule
            .theorem
            .quantifiers
            .iter()
            .map(|q| bindings.get(&q.name).cloned())
            .collect();
        if let Some(arguments) = args {
            let rhs = terms::normalize(&terms::substitute(&rule.rhs, &bindings));
            // A normalized reflexive instance adds no information. In
            // particular, an empty-seed identity can be a looping local simp
            // fact before its empty containers have been normalized by Lean.
            if &rhs == t {
                return terms::map(t, &mut |child| {
                    rewrite(child, rule, applications, budget, limited)
                });
            }
            if !applications.iter().any(|a| {
                a.fn_id == rule.theorem.fn_id
                    && a.law_name == rule.theorem.law_name
                    && a.arguments == arguments
            }) && applications.len() < budget.applications
            {
                applications.push(LawApplication {
                    fn_id: rule.theorem.fn_id,
                    law_name: rule.theorem.law_name.clone(),
                    arguments,
                });
            }
            if applications.len() >= budget.applications {
                *limited = true;
            }
            // Permutation rules may be useful calls, but must not grow the pool.
            if !terms::matches(&rule.lhs, &rule.rhs, &rule.variables, &mut Bindings::new()) {
                return rhs;
            }
        }
    }
    terms::map(t, &mut |child| {
        rewrite(child, rule, applications, budget, limited)
    })
}

pub fn populate(
    inputs: &ProofLowerInputs,
    ir: &mut ProofIR,
    budget: ApplicationSearchBudget,
) -> ApplicationSearchReport {
    let mut report = ApplicationSearchReport::default();
    // Re-running search replaces suggestions; it never changes the claim or
    // the source-derived induction itself.
    for theorem in &mut ir.law_theorems {
        if let Some(induction) = &mut theorem.induction {
            for call in &mut induction.calls {
                call.applications.clear();
            }
        }
    }
    if budget.rounds == 0 || budget.terms < 2 || budget.nodes == 0 || budget.applications == 0 {
        report.disabled = true;
        return report;
    }
    // Backend signatures can be specialized; the first supported common lane
    // is ordinary, unconditional laws with plain scalar/sequence binders.
    fn plain(ty: &str) -> bool {
        matches!(ty, "Int" | "Bool" | "String")
            || ty
                .strip_prefix("List<")
                .and_then(|s| s.strip_suffix('>'))
                .is_some_and(plain)
    }
    let mut eligible = BTreeSet::new();
    let entry = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::Verify(vb) => Some((None, vb)),
        _ => None,
    });
    let deps = inputs.dep_modules.iter().flat_map(|m| {
        m.verify_blocks
            .iter()
            .map(move |vb| (Some(m.prefix.as_str()), vb))
    });
    for (scope, vb) in entry.chain(deps) {
        if let VerifyKind::Law(law) = &vb.kind
            && law.when.is_none()
            && law.because.is_empty()
            && law.using.is_none()
            && law.givens.iter().all(|g| plain(&g.type_name))
            && let Some(id) = inputs.symbol_table.resolve_fn_id_in(&vb.fn_name, scope)
        {
            eligible.insert((id, law.name.clone()));
        }
    }
    for index in 0..ir.law_theorems.len() {
        let (earlier, remaining) = ir.law_theorems.split_at_mut(index);
        let theorem = &mut remaining[0];
        if !terms::supported(&theorem.claim_lhs) || !terms::supported(&theorem.claim_rhs) {
            continue;
        }
        let Some(induction) = &mut theorem.induction else {
            continue;
        };
        let scope = inputs.symbol_table.fn_entry(theorem.fn_id).key.scope_str();
        let rules: Vec<_> = earlier
            .iter()
            .filter(|t| {
                eligible.contains(&(t.fn_id, t.law_name.clone()))
                    // A concrete user-function signature anchors the match's
                    // argument types. Bare-variable and polymorphic builtin
                    // roots need a separate typed unifier; never guess there.
                    && matches!(t.claim_lhs.node, crate::ir::hir::ResolvedExpr::Call(crate::ir::hir::ResolvedCallee::Fn(_), _))
                    && terms::supported(&t.claim_lhs)
                    && terms::supported(&t.claim_rhs)
                    && inputs.symbol_table.fn_entry(t.fn_id).key.scope_str() == scope
                    && theorem.function_cone.contains(&t.fn_id)
            })
            .map(|t| Rule {
                theorem: t,
                lhs: terms::normalize(&t.claim_lhs),
                rhs: terms::normalize(&t.claim_rhs),
                variables: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
            })
            .collect();
        for step in &mut induction.calls {
            let Some(source_step) = &step.source_step else {
                continue;
            };
            if !terms::supported(source_step) {
                continue;
            }
            let mut pool: Vec<_> = [&theorem.claim_lhs, &theorem.claim_rhs]
                .into_iter()
                .map(|t| terms::normalize(&terms::replace(t, &step.source_call, source_step)))
                .collect();
            let mut frontier = pool.clone();
            let mut limited = false;
            'closure: for round in 0..budget.rounds {
                let mut next = Vec::new();
                for term in &frontier {
                    for rule in &rules {
                        let candidate = terms::normalize(&rewrite(
                            term,
                            rule,
                            &mut step.applications,
                            budget,
                            &mut limited,
                        ));
                        let candidate_size = terms::size(&candidate);
                        if candidate_size <= budget.nodes
                            && !pool.contains(&candidate)
                            && !next.contains(&candidate)
                        {
                            next.push(candidate);
                        }
                        if candidate_size > budget.nodes {
                            limited = true;
                        }
                        if pool.len() + next.len() >= budget.terms
                            || step.applications.len() >= budget.applications
                        {
                            limited = true;
                            break 'closure;
                        }
                    }
                }
                if next.is_empty() {
                    break;
                }
                if round + 1 == budget.rounds {
                    limited = true;
                }
                pool.extend(next.iter().cloned());
                frontier = next;
            }
            report.applications += step.applications.len();
            report.limited_steps += usize::from(limited);
        }
    }
    report
}
