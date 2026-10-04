use crate::ast::{BinOp, Spanned};
use crate::ir::hir::{ResolvedExpr, ResolvedMatchArm, ResolvedPattern};

use super::check::{check_script, conclusion};
use super::term::{self, var};
use super::{Eqn, Obligation, Proof, Script, WallRule};

fn script(lhs: super::Term, rhs: super::Term, proof: Proof) -> Script {
    Script {
        obligation: Obligation {
            key: "f.law".into(),
            givens: vec!["a".into(), "b".into()],
            premise: None,
            lhs,
            rhs,
        },
        defs: Vec::new(),
        laws: Vec::new(),
        proof,
    }
}

fn add(a: super::Term, b: super::Term) -> super::Term {
    term::binop(BinOp::Add, a, b)
}

#[test]
fn a_rule_instance_proves_its_conclusion_and_nothing_else() {
    let comm = Proof::Rule {
        rule: WallRule::AddComm,
        subst: vec![("a".into(), var("x")), ("b".into(), var("y"))],
        premises: Vec::new(),
    };
    let good = script(
        add(var("x"), var("y")),
        add(var("y"), var("x")),
        comm.clone(),
    );
    assert!(check_script(&good).is_ok());
    let wrong = script(add(var("x"), var("y")), add(var("x"), var("y")), comm);
    assert!(check_script(&wrong).is_err());
    let unbound = Proof::Rule {
        rule: WallRule::AddComm,
        subst: vec![("a".into(), var("x"))],
        premises: Vec::new(),
    };
    assert!(conclusion(&unbound, &good, &Vec::new()).is_err());
}

#[test]
fn substitution_refuses_to_capture_a_pattern_binder() {
    let arm = ResolvedMatchArm {
        pattern: ResolvedPattern::Ident("x".into()),
        body: Box::new(var("y")),
        binding_slots: std::sync::OnceLock::new(),
    };
    let m = Spanned::bare(ResolvedExpr::Match {
        subject: Box::new(var("s")),
        arms: vec![arm],
    });
    assert!(term::subst(&m, &[("y".into(), var("x"))]).is_err());
    assert!(term::subst(&m, &[("y".into(), var("z"))]).is_ok());
}

#[test]
fn a_congruence_needs_exactly_one_hole_and_a_hypothesis_must_be_in_scope() {
    let ctx = add(term::hole(), term::hole());
    let p = Proof::Congr {
        ctx,
        inner: Box::new(Proof::Refl(var("a"))),
    };
    let s = script(var("a"), var("a"), Proof::Refl(var("a")));
    assert!(conclusion(&p, &s, &Vec::new()).is_err());
    let hyp = Proof::Hyp("h".into());
    assert!(conclusion(&hyp, &s, &Vec::new()).is_err());
    let scoped = vec![("h".to_string(), Eqn::new(var("c"), term::boolean(true)))];
    assert!(conclusion(&hyp, &s, &scoped).is_ok());
}

#[test]
fn compute_decides_closed_terms_only() {
    let s = script(var("a"), var("a"), Proof::Refl(var("a")));
    let eight_minus_one = term::binop(BinOp::Sub, term::int(&8.into()), term::int(&1.into()));
    let good = Proof::Compute {
        lhs: eight_minus_one.clone(),
        rhs: term::int(&7.into()),
    };
    assert!(conclusion(&good, &s, &Vec::new()).is_ok());
    let wrong = Proof::Compute {
        lhs: eight_minus_one,
        rhs: term::int(&6.into()),
    };
    assert!(conclusion(&wrong, &s, &Vec::new()).is_err());
    let open = Proof::Compute {
        lhs: add(var("a"), term::int(&0.into())),
        rhs: var("a"),
    };
    assert!(conclusion(&open, &s, &Vec::new()).is_err());
}

#[test]
fn every_wall_rule_round_trips_its_identifier() {
    for rule in WallRule::ALL {
        assert_eq!(WallRule::from_id(rule.id()), Some(rule));
        let subst: Vec<(String, super::Term)> = rule
            .binders()
            .iter()
            .map(|b| (b.to_string(), var(&format!("v_{b}"))))
            .collect();
        assert!(rule.instantiate(&subst).is_some(), "{}", rule.id());
    }
}
