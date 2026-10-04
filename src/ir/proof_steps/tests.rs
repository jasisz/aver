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
            lists: Vec::new(),
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

/// Each wall rule with at most one premise of the form `p = true`, as a one-step script whose
/// `when` is the premise: both checkers accept it, and both refuse it once
/// its conclusion is changed.
#[test]
fn both_checkers_agree_on_every_wall_rule() {
    use super::sexpr::{BuiltinsOnly, script as serialise};
    for rule in WallRule::ALL {
        let (premises, concl) = rule.schema();
        // A `when` states a premise `p = true`; the others are covered
        // by the producers' tests.
        if premises.len() > 1
            || premises
                .iter()
                .any(|p| term::bool_value(&p.rhs) != Some(true))
        {
            continue;
        }
        let premise = premises.first().map(|p| p.lhs.clone());
        let subst: Vec<(String, super::Term)> = rule
            .binders()
            .iter()
            .map(|b| (b.to_string(), var(b)))
            .collect();
        let step = Proof::Rule {
            rule,
            subst,
            premises: premise.iter().map(|_| Proof::Hyp("when".into())).collect(),
        };
        let mut s = script(concl.lhs.clone(), concl.rhs.clone(), step);
        s.obligation.givens = rule.binders().iter().map(|b| b.to_string()).collect();
        s.obligation.premise = premise;
        assert_eq!(check_script(&s), Ok(()), "{}", rule.id());
        let text = serialise(&s, &BuiltinsOnly).unwrap();
        assert_eq!(
            crate::proof_kernel::verdict(&text),
            Ok("f.law".to_string()),
            "{}",
            rule.id()
        );
        s.obligation.rhs = term::builtin("List.reverse", vec![concl.rhs.clone()], None);
        assert!(check_script(&s).is_err(), "{}", rule.id());
        let text = serialise(&s, &BuiltinsOnly).unwrap();
        assert!(
            crate::proof_kernel::verdict(&text).is_err(),
            "{}",
            rule.id()
        );
    }
}

/// `List.concat(xs, []) = xs` by induction on `xs`, a given of list type:
/// both checkers accept it, and refuse it when `xs` is not declared a list,
/// when the hypothesis is used in the empty-list case, when the cell's two
/// names are the same, and when a hypothesis in scope mentions `xs`.
#[test]
fn both_checkers_induct_on_a_list_given_and_refuse_mutations() {
    use super::sexpr::{BuiltinsOnly, script as serialise};
    let concat = |a: super::Term, b: super::Term| term::builtin("List.concat", vec![a, b], None);
    let cell = |h: &str, t: &str| term::builtin("List.prepend", vec![var(h), var(t)], None);
    let rule = |rule: WallRule, subst: Vec<(&str, super::Term)>| Proof::Rule {
        rule,
        subst: subst.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
        premises: Vec::new(),
    };
    let proof = |nil: Proof, head: &str, tail: &str| Proof::InductList {
        var: "xs".into(),
        lhs: concat(var("xs"), term::nil()),
        rhs: var("xs"),
        nil: Box::new(nil),
        head: head.into(),
        tail: tail.into(),
        ih: "ih".into(),
        cons: Box::new(Proof::Trans {
            terms: vec![
                concat(cell(head, tail), term::nil()),
                term::builtin(
                    "List.prepend",
                    vec![var(head), concat(var(tail), term::nil())],
                    None,
                ),
                cell(head, tail),
            ],
            steps: vec![
                rule(
                    WallRule::ConcatCons,
                    vec![("x", var(head)), ("a", var(tail)), ("b", term::nil())],
                ),
                Proof::Congr {
                    ctx: term::builtin("List.prepend", vec![var(head), term::hole()], None),
                    inner: Box::new(Proof::Hyp("ih".into())),
                },
            ],
        }),
    };
    let base = || rule(WallRule::ConcatNil, vec![("b", term::nil())]);
    let mut good = script(
        concat(var("xs"), term::nil()),
        var("xs"),
        proof(base(), "h", "t"),
    );
    good.obligation.givens = vec!["xs".into()];
    good.obligation.lists = vec!["xs".into()];
    let both = |s: &Script| {
        let rust = check_script(s);
        let kernel = crate::proof_kernel::verdict(&serialise(s, &BuiltinsOnly).unwrap());
        (rust, kernel)
    };
    assert_eq!(both(&good), (Ok(()), Ok("f.law".to_string())));
    let mut untyped = good.clone();
    untyped.obligation.lists.clear();
    let mut ih_in_base = good.clone();
    ih_in_base.proof = proof(Proof::Hyp("ih".into()), "h", "t");
    let mut same_names = good.clone();
    same_names.proof = proof(base(), "t", "t");
    let mut when_mentions = good.clone();
    when_mentions.obligation.premise = Some(term::binop(BinOp::Eq, var("xs"), var("xs")));
    for (kind, s) in [
        ("not a list", untyped),
        ("hypothesis in the base case", ih_in_base),
        ("one name twice", same_names),
        ("a when on the list", when_mentions),
    ] {
        let (rust, kernel) = both(&s);
        assert!(rust.is_err(), "{kind}: Rust accepted");
        assert!(kernel.is_err(), "{kind}: kernel accepted");
    }
}

/// Every builtin fact proves its own statement in both checkers, and a
/// citation is refused when it states the fact differently from the proof
/// it carries.
#[test]
fn every_builtin_fact_checks_and_a_misstated_citation_is_refused() {
    use super::sexpr::{BuiltinsOnly, script as serialise};
    for fact in super::facts::all() {
        assert!(super::facts::is_fact_name(fact.key), "{}", fact.key);
        assert_eq!(check_script(&fact.script), Ok(()), "{}", fact.key);
        assert_eq!(
            crate::proof_kernel::verdict(&serialise(&fact.script, &BuiltinsOnly).unwrap()),
            Ok(fact.key.to_string())
        );
        let ob = &fact.script.obligation;
        let cite = Proof::Law {
            law: fact.key.into(),
            subst: ob.givens.iter().map(|g| (g.clone(), var(g))).collect(),
            premise: None,
        };
        let mut citing = script(ob.lhs.clone(), ob.rhs.clone(), cite);
        citing.obligation.givens = ob.givens.clone();
        citing.laws.push(fact.law_ref());
        assert_eq!(check_script(&citing), Ok(()), "{}", fact.key);
        let mut misstated = citing.clone();
        misstated.laws[0].rhs = misstated.laws[0].lhs.clone();
        misstated.obligation.rhs = misstated.obligation.lhs.clone();
        assert!(check_script(&misstated).is_err(), "{}", fact.key);
    }
}

/// Two closed lists compute equal only when they are the same list.
#[test]
fn compute_compares_whole_lists() {
    let l = |vs: &[i64]| term::list(vs.iter().map(|v| term::int(&(*v).into())).collect());
    let take = term::builtin("List.take", vec![l(&[1, 2, 3]), term::int(&2.into())], None);
    let s = |rhs: super::Term| {
        script(
            take.clone(),
            rhs.clone(),
            Proof::Compute {
                lhs: take.clone(),
                rhs,
            },
        )
    };
    assert_eq!(check_script(&s(l(&[1, 2]))), Ok(()));
    assert!(check_script(&s(l(&[1, 3]))).is_err());
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

#[test]
fn the_ring_step_compares_polynomials_and_leaves_text_joining_alone() {
    use crate::ast::Type;
    let mul = |a, b| term::binop(BinOp::Mul, a, b);
    let two = || term::int(&2.into());
    // (a + b) * (a + b) = a*a + 2*a*b + b*b
    let lhs = mul(add(var("a"), var("b")), add(var("a"), var("b")));
    let rhs = add(
        add(mul(var("a"), var("a")), mul(mul(two(), var("a")), var("b"))),
        mul(var("b"), var("b")),
    );
    assert!(super::ring::same_polynomial(&lhs, &rhs));
    assert!(!super::ring::same_polynomial(
        &lhs,
        &mul(var("a"), var("b"))
    ));
    let join = |x: super::Term, y: super::Term| {
        let t = Spanned::bare(ResolvedExpr::BinOp(BinOp::Add, Box::new(x), Box::new(y)));
        t.set_ty(Type::Str);
        t
    };
    assert!(!super::ring::same_polynomial(
        &join(var("s"), var("t")),
        &join(var("t"), var("s"))
    ));
}

#[test]
fn a_linear_certificate_needs_nonnegative_weights_that_reach_a_negative_constant() {
    use super::linear;
    let gt = |a, b| term::binop(BinOp::Gt, a, b);
    let one = || term::int(&1.into());
    let zero = || term::int(&0.into());
    let mut atoms = Vec::new();
    // x > 0 and not (x + 1 > 1) contradict each other.
    let facts = vec![
        linear::as_nonneg(&gt(var("x"), zero()), true, &mut atoms).unwrap(),
        linear::as_nonneg(&gt(add(var("x"), one()), one()), false, &mut atoms).unwrap(),
    ];
    let w = linear::certificate(&facts).expect("a certificate");
    assert!(linear::contradicts(&linear::combine(&facts, &w).unwrap()));
    assert!(linear::combine(&facts, &[1.into(), (-1).into()]).is_none());
    let alone = vec![facts[0].clone()];
    assert!(linear::certificate(&alone).is_none());
}
