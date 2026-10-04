use crate::ast::{BinOp, Spanned};
use crate::ir::hir::{ResolvedExpr, ResolvedMatchArm, ResolvedPattern};

use super::check::{check_script, conclusion};
use super::term::{self, var};
use super::{Const, Eqn, Obligation, Proof, Script, WallRule};

fn script(lhs: super::Term, rhs: super::Term, proof: Proof) -> Script {
    Script {
        obligation: Obligation {
            key: "f.law".into(),
            givens: vec!["a".into(), "b".into()],
            finite: Vec::new(),
            premise: None,
            lhs,
            rhs,
        },
        defs: Vec::new(),
        consts: Vec::new(),
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

/// A module-level binding opens to its value and to nothing else: the step
/// names the binding, the script carries its value, and a binding the
/// script does not carry proves nothing.
#[test]
fn a_binding_unfolds_to_the_value_the_script_carries() {
    let forty = term::int(&40.into());
    let mut s = script(
        add(var("Lib.base"), var("a")),
        add(forty.clone(), var("a")),
        Proof::Congr {
            ctx: add(term::hole(), var("a")),
            inner: Box::new(Proof::UnfoldConst {
                name: "Lib.base".into(),
            }),
        },
    );
    s.consts.push(Const {
        name: "Lib.base".into(),
        value: forty.clone(),
    });
    assert_eq!(check_script(&s), Ok(()));
    let step = Proof::UnfoldConst {
        name: "base".into(),
    };
    assert!(conclusion(&step, &s, &Vec::new()).is_err());
    s.consts[0].value = term::int(&41.into());
    assert!(check_script(&s).is_err());
}

#[test]
fn a_definition_opens_its_local_bindings_in_order() {
    let def = super::Def {
        fn_id: crate::ir::identity::FnId(0),
        name: "f".into(),
        params: vec!["x".into()],
        lets: vec![
            ("y".into(), add(var("x"), var("x"))),
            ("z".into(), add(var("y"), var("x"))),
        ],
        body: var("z"),
    };
    let outer = def.outer(&[var("a")]).unwrap();
    assert_eq!(
        term::subst(&def.body, &outer).unwrap(),
        add(add(var("a"), var("a")), var("a"))
    );
    assert!(def.outer(&[]).is_err());
}

#[test]
fn an_enum_split_needs_one_case_per_value_of_a_finite_given() {
    let claim = term::binop(BinOp::Eq, var("b"), var("b"));
    let case = |v: bool| Proof::Compute {
        lhs: term::binop(BinOp::Eq, term::boolean(v), term::boolean(v)),
        rhs: term::boolean(true),
    };
    let split = |cases: Vec<Proof>| Proof::Enum {
        var: "b".into(),
        lhs: claim.clone(),
        rhs: term::boolean(true),
        cases,
    };
    let mut s = script(
        claim.clone(),
        term::boolean(true),
        split(vec![case(false), case(true)]),
    );
    s.obligation.givens = vec!["b".into()];
    s.obligation.finite = vec![("b".into(), super::Finite::Bool)];
    assert!(check_script(&s).is_ok());
    s.proof = split(vec![case(false)]);
    assert!(check_script(&s).is_err());
    s.proof = split(vec![case(true), case(false)]);
    assert!(check_script(&s).is_err());
    s.proof = split(vec![case(false), case(true)]);
    s.obligation.finite.clear();
    assert!(check_script(&s).is_err());
}

#[test]
fn a_catch_all_arm_is_chosen_only_for_a_value_the_earlier_arms_exclude() {
    use crate::ast::Literal;
    let arm = |pattern, body| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let arms = vec![
        arm(
            ResolvedPattern::Literal(Literal::Int(0)),
            term::int(&0.into()),
        ),
        arm(ResolvedPattern::Ident("n".into()), var("n")),
    ];
    let five = term::int(&5.into());
    let (premise, body) =
        super::check::arm_equation(&var("c"), &arms, 2, &[], std::slice::from_ref(&five)).unwrap();
    assert_eq!(premise, Eqn::new(var("c"), five.clone()));
    assert_eq!(body, five);
    let zero = term::int(&0.into());
    assert!(super::check::arm_equation(&var("c"), &arms, 2, &[], &[zero]).is_err());
    assert!(super::check::arm_equation(&var("c"), &arms, 2, &[], &[var("x")]).is_err());
}

#[test]
fn a_recursive_definition_opens_only_when_it_recurses_on_a_part_of_its_match() {
    use crate::ir::hir::ResolvedCallee;
    use crate::ir::identity::FnId;
    let call = |args: Vec<super::Term>| {
        Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(FnId(7)), args))
    };
    let arm = |pattern, body| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let def = |tail_call: super::Term| super::Def {
        fn_id: FnId(7),
        name: "f".into(),
        params: vec!["xs".into()],
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("xs")),
            arms: vec![
                arm(ResolvedPattern::EmptyList, term::int(&0.into())),
                arm(ResolvedPattern::Cons("h".into(), "t".into()), tail_call),
            ],
        }),
    };
    let on_tail = def(call(vec![var("t")]));
    assert_eq!(super::induct::structural_param(&on_tail), Ok(Some(0)));
    let on_itself = def(call(vec![var("xs")]));
    assert!(super::induct::structural_param(&on_itself).is_err());
    let mut other = on_tail.clone();
    other.fn_id = FnId(8);
    other.name = "g".into();
    other.body = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(7)),
        vec![var("xs")],
    ));
    let mut back = on_tail.clone();
    back.body = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(8)),
        vec![var("xs")],
    ));
    assert!(super::induct::refuse_mutual_recursion(&[back, other]).is_err());
}

#[test]
fn closed_lists_compute_with_aver_slice_semantics() {
    let list = |xs: &[i64]| {
        Spanned::bare(ResolvedExpr::List(
            xs.iter().map(|x| term::int(&(*x).into())).collect(),
        ))
    };
    let take = term::builtin(
        "List.take",
        vec![list(&[1, 2, 3]), term::int(&(-1).into())],
        None,
    );
    assert_eq!(term::eval_closed(&take), Some(list(&[])));
    let len = term::builtin("List.len", vec![list(&[4, 5])], None);
    assert_eq!(term::eval_closed(&len), Some(term::int(&2.into())));
    let cons = term::builtin("List.prepend", vec![term::int(&1.into()), list(&[2])], None);
    assert_eq!(term::eval_closed(&cons), Some(list(&[1, 2])));
    let open = term::builtin("List.len", vec![var("xs")], None);
    assert_eq!(term::eval_closed(&open), None);
}
