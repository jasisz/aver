//! Mode-parametrized expansion of `verify ... law` cases.
//!
//! A law is parameterized by `given` clauses; running it means picking
//! values for each given, substituting them into the template lhs / rhs /
//! when, and producing a flat list of cases. The parser already does this
//! with the values the user wrote (`Declared` mode). Under
//! `aver verify --hostile` we run the same expansion with the same
//! template, but the value source for typed givens is augmented with
//! the per-type boundary set (`Hostile` mode) — so the same code path
//! produces the dual-run pair (declared cases vs hostile cases) without
//! a second implementation.
//!
//! The parser keeps its own copy of declared-mode expansion for now to
//! avoid a wider refactor in this commit. When the hostile pipeline is
//! wired into vm_verify, both call sites will move here and the parser
//! will delegate.

use std::collections::HashMap;

use crate::ast::{Expr, Literal, Spanned, VerifyGiven, VerifyGivenDomain, VerifyLaw};
use crate::ast_rewrite::rewrite_idents_scoped;
use crate::types::checker::hostile_values::boundary_values;
use crate::types::parse_type_str_strict;

/// Which values the expansion should draw from for each `given` clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpansionMode {
    /// Use only the values written by the user (literal list or `IntRange`).
    /// Mirrors the parser's expansion exactly.
    Declared,
    /// Use the declared values *plus* the boundary set for the parsed
    /// type — `i64::MIN`, `i64::MAX`, `0`, `1`, `-1` for `Int`; empty,
    /// long, NUL, multi-byte for `Str`, etc. Failures here that would
    /// not arise under `Declared` reveal one-sided assumptions: either
    /// the law silently relies on a nice-world / narrow-range
    /// precondition (encode it as `when`), or the law is genuinely
    /// universal and the failure is a bug.
    Hostile,
}

/// One expanded case: lhs, rhs, the per-given binding map that produced
/// it, and the substituted `when` guard if the law had one. The vm-verify
/// runner takes ownership and produces VM helper fns from these. Origin
/// flag distinguishes cases that came from the user's own values from
/// cases the hostile expansion added.
#[derive(Debug, Clone)]
pub struct ExpandedCase {
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
    pub guard: Option<Spanned<Expr>>,
    pub bindings: Vec<(String, Spanned<Expr>)>,
    pub from_hostile: bool,
}

/// Expand a `verify ... law` template under the chosen mode. Returns the
/// flat list of cases ready for vm-verify plan construction. Cases from
/// the declared values come first (in the same order the parser would
/// produce them); when the mode is `Hostile`, additional cases follow
/// with `from_hostile = true` and any combination already covered by the
/// declared run is suppressed (no duplicate work).
pub fn expand_law_cases(law: &VerifyLaw, mode: ExpansionMode) -> Vec<ExpandedCase> {
    let declared_combos = collect_combinations(law, ExpansionMode::Declared);
    match mode {
        ExpansionMode::Declared => declared_combos
            .into_iter()
            .map(|combo| materialize_case(law, &combo, false))
            .collect(),
        ExpansionMode::Hostile => {
            let mut out: Vec<ExpandedCase> = declared_combos
                .iter()
                .map(|combo| materialize_case(law, combo, false))
                .collect();
            let declared_set: std::collections::HashSet<Vec<String>> = declared_combos
                .iter()
                .map(|c| combination_signature(c))
                .collect();
            for combo in collect_combinations(law, ExpansionMode::Hostile) {
                if declared_set.contains(&combination_signature(&combo)) {
                    continue;
                }
                out.push(materialize_case(law, &combo, true));
            }
            out
        }
    }
}

fn collect_combinations(law: &VerifyLaw, mode: ExpansionMode) -> Vec<Vec<(String, Spanned<Expr>)>> {
    let per_given: Vec<Vec<Spanned<Expr>>> = law
        .givens
        .iter()
        .map(|g| values_for_given(g, mode))
        .collect();
    cartesian(&law.givens, &per_given)
}

fn values_for_given(g: &VerifyGiven, mode: ExpansionMode) -> Vec<Spanned<Expr>> {
    let mut values = declared_values(&g.domain);
    if mode == ExpansionMode::Hostile
        && let Ok(ty) = parse_type_str_strict(&g.type_name)
    {
        // Only append boundary values that aren't already covered by
        // the declared set — keeps the cartesian product tight and
        // avoids re-running the same combination twice.
        let existing: std::collections::HashSet<String> = values.iter().map(render_expr).collect();
        for lit in boundary_values(&ty) {
            let candidate = Spanned::bare(Expr::Literal(lit));
            if !existing.contains(&render_expr(&candidate)) {
                values.push(candidate);
            }
        }
    }
    values
}

fn declared_values(domain: &VerifyGivenDomain) -> Vec<Spanned<Expr>> {
    match domain {
        VerifyGivenDomain::Explicit(values) => values.clone(),
        VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
            .map(|n| Spanned::bare(Expr::Literal(Literal::Int(n))))
            .collect(),
    }
}

fn cartesian(
    givens: &[VerifyGiven],
    per_given: &[Vec<Spanned<Expr>>],
) -> Vec<Vec<(String, Spanned<Expr>)>> {
    let mut out: Vec<Vec<(String, Spanned<Expr>)>> = vec![Vec::new()];
    for (g, choices) in givens.iter().zip(per_given) {
        let mut next = Vec::with_capacity(out.len() * choices.len().max(1));
        for partial in &out {
            for choice in choices {
                let mut extended = partial.clone();
                extended.push((g.name.clone(), choice.clone()));
                next.push(extended);
            }
        }
        out = next;
    }
    out
}

fn materialize_case(
    law: &VerifyLaw,
    bindings: &[(String, Spanned<Expr>)],
    from_hostile: bool,
) -> ExpandedCase {
    let map: HashMap<&str, &Spanned<Expr>> =
        bindings.iter().map(|(n, v)| (n.as_str(), v)).collect();
    ExpandedCase {
        lhs: substitute(&law.lhs, &map),
        rhs: substitute(&law.rhs, &map),
        guard: law.when.as_ref().map(|w| substitute(w, &map)),
        bindings: bindings.to_vec(),
        from_hostile,
    }
}

fn substitute(expr: &Spanned<Expr>, bindings: &HashMap<&str, &Spanned<Expr>>) -> Spanned<Expr> {
    rewrite_idents_scoped(expr, |name| bindings.get(name).map(|v| (*v).clone()))
}

/// Stable signature for a binding combination, used to suppress
/// duplicates when the hostile set happens to contain a value the user
/// already wrote.
fn combination_signature(combo: &[(String, Spanned<Expr>)]) -> Vec<String> {
    combo
        .iter()
        .map(|(n, v)| format!("{}={}", n, render_expr(v)))
        .collect()
}

fn render_expr(expr: &Spanned<Expr>) -> String {
    match &expr.node {
        Expr::Literal(Literal::Int(i)) => i.to_string(),
        Expr::Literal(Literal::Float(f)) => format!("{:?}", f),
        Expr::Literal(Literal::Str(s)) => format!("{:?}", s),
        Expr::Literal(Literal::Bool(b)) => b.to_string(),
        Expr::Literal(Literal::Unit) => "Unit".to_string(),
        // Non-literal declared values are rare (programmer wrote a
        // constructor like `Option.None`); render through Debug so the
        // signature is at least unique. Hostile mode never produces
        // these, so dedup against declared still works.
        other => format!("{:?}", other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lit_int(n: i64) -> Spanned<Expr> {
        Spanned::bare(Expr::Literal(Literal::Int(n)))
    }

    fn law_one_int(name: &str, ty: &str, declared: Vec<i64>) -> VerifyLaw {
        VerifyLaw {
            name: "test".to_string(),
            givens: vec![VerifyGiven {
                name: name.to_string(),
                type_name: ty.to_string(),
                domain: VerifyGivenDomain::Explicit(declared.into_iter().map(lit_int).collect()),
            }],
            when: None,
            lhs: Spanned::bare(Expr::Ident(name.to_string())),
            rhs: Spanned::bare(Expr::Ident(name.to_string())),
            sample_guards: vec![],
        }
    }

    #[test]
    fn declared_mode_returns_only_user_values() {
        let law = law_one_int("x", "Int", vec![1, 2]);
        let cases = expand_law_cases(&law, ExpansionMode::Declared);
        assert_eq!(cases.len(), 2);
        assert!(cases.iter().all(|c| !c.from_hostile));
    }

    #[test]
    fn hostile_mode_appends_boundary_set() {
        let law = law_one_int("x", "Int", vec![5]);
        let declared = expand_law_cases(&law, ExpansionMode::Declared);
        let hostile = expand_law_cases(&law, ExpansionMode::Hostile);
        assert_eq!(declared.len(), 1);
        // Boundary set for Int is 5 values; user provided 5, which isn't
        // in the boundary set, so all 5 boundary values are added.
        assert_eq!(hostile.len(), 1 + 5);
        assert!(!hostile[0].from_hostile);
        assert!(hostile[1..].iter().all(|c| c.from_hostile));
    }

    #[test]
    fn hostile_dedupes_against_declared() {
        // User declared `0` — boundary set for Int also contains `0`,
        // so it must not appear twice.
        let law = law_one_int("x", "Int", vec![0]);
        let hostile = expand_law_cases(&law, ExpansionMode::Hostile);
        let zero_count = hostile
            .iter()
            .filter(|c| matches!(&c.lhs.node, Expr::Literal(Literal::Int(0))))
            .count();
        assert_eq!(zero_count, 1, "zero should appear exactly once");
    }

    #[test]
    fn cartesian_two_givens() {
        let law = VerifyLaw {
            name: "two".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "x".to_string(),
                    type_name: "Bool".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![Spanned::bare(Expr::Literal(
                        Literal::Bool(true),
                    ))]),
                },
                VerifyGiven {
                    name: "y".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![lit_int(1)]),
                },
            ],
            when: None,
            lhs: Spanned::bare(Expr::Ident("x".to_string())),
            rhs: Spanned::bare(Expr::Ident("y".to_string())),
            sample_guards: vec![],
        };
        let declared = expand_law_cases(&law, ExpansionMode::Declared);
        assert_eq!(declared.len(), 1);
        let hostile = expand_law_cases(&law, ExpansionMode::Hostile);
        // Bool boundary = 2 values, Int boundary = 5 values.
        // Cartesian: 2 × 5 = 10. Declared (true, 1) is in the cartesian,
        // so total unique = 10 (1 declared + 9 hostile-only).
        assert_eq!(hostile.iter().filter(|c| !c.from_hostile).count(), 1);
        assert_eq!(hostile.len(), 10);
    }

    #[test]
    fn unknown_type_falls_back_to_declared_only() {
        // For a user-defined type the boundary set is empty; hostile
        // mode degenerates to declared — no extra cases injected, no
        // crash on unknown type names.
        let law = law_one_int("x", "Shape", vec![1]);
        let hostile = expand_law_cases(&law, ExpansionMode::Hostile);
        assert_eq!(hostile.len(), 1);
        assert!(!hostile[0].from_hostile);
    }
}
