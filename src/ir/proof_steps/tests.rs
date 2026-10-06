use crate::ast::{BinOp, Spanned};
use crate::ir::hir::{ResolvedExpr, ResolvedMatchArm, ResolvedPattern};

use super::claim::claim;
use super::sexpr::{BuiltinsOnly, script as serialise};
use super::term::{self, var};
use super::{Const, Eqn, Obligation, Proof, Script, WallRule};

fn script(lhs: super::Term, rhs: super::Term, proof: Proof) -> Script {
    Script {
        obligation: Obligation {
            key: "f.law".into(),
            givens: vec!["a".into(), "b".into()],
            finite: Vec::new(),
            lists: Vec::new(),
            ints: Vec::new(),
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

/// The kernel written in Aver on a script: the one checker of step proofs.
fn kernel(s: &Script) -> Result<(), String> {
    crate::proof_kernel::verdict(&serialise(s, &BuiltinsOnly)?).map(|_| ())
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
    assert!(kernel(&good).is_ok());
    let wrong = script(add(var("x"), var("y")), add(var("x"), var("y")), comm);
    assert!(kernel(&wrong).is_err());
    let unbound = Proof::Rule {
        rule: WallRule::AddComm,
        subst: vec![("a".into(), var("x"))],
        premises: Vec::new(),
    };
    assert!(claim(&unbound, &good, &Vec::new()).is_err());
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
    let two_holes = script(add(var("a"), var("a")), add(var("a"), var("a")), p);
    assert!(kernel(&two_holes).is_err());
    let s = script(var("a"), var("a"), Proof::Refl(var("a")));
    let hyp = Proof::Hyp("h".into());
    assert!(claim(&hyp, &s, &Vec::new()).is_err());
    let scoped = vec![("h".to_string(), Eqn::new(var("c"), term::boolean(true)))];
    assert!(claim(&hyp, &s, &scoped).is_ok());
}

#[test]
fn compute_decides_closed_terms_only() {
    let s = |p: Proof| match &p {
        Proof::Compute { lhs, rhs } => script(lhs.clone(), rhs.clone(), p.clone()),
        _ => unreachable!(),
    };
    let eight_minus_one = term::binop(BinOp::Sub, term::int(&8.into()), term::int(&1.into()));
    let good = Proof::Compute {
        lhs: eight_minus_one.clone(),
        rhs: term::int(&7.into()),
    };
    assert!(kernel(&s(good)).is_ok());
    let wrong = Proof::Compute {
        lhs: eight_minus_one,
        rhs: term::int(&6.into()),
    };
    assert!(kernel(&s(wrong)).is_err());
    let open = Proof::Compute {
        lhs: add(var("a"), term::int(&0.into())),
        rhs: var("a"),
    };
    assert!(kernel(&s(open)).is_err());
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
/// `when` is the premise: the kernel accepts it, and refuses it once its
/// conclusion is changed.
#[test]
fn the_kernel_checks_every_wall_rule() {
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
        assert_eq!(kernel(&s), Ok(()), "{}", rule.id());
        s.obligation.rhs = term::builtin("List.reverse", vec![concl.rhs.clone()], None);
        assert!(kernel(&s).is_err(), "{}", rule.id());
    }
}

/// A true `a == b` between Floats does not make `a` and `b` the same value
/// (`0.0 == -0.0`, and `String.fromFloat` tells them apart), so the
/// equality rule refuses it: Float `==` is spelled `==.`, also inside a
/// list, while between Ints it rewrites.
#[test]
fn a_float_equality_does_not_rewrite() {
    use crate::ast::Type;
    let typed_var = |name: &str, ty: Type| {
        let t = var(name);
        t.set_ty(ty);
        t
    };
    for (ty, accepted) in [
        (Type::Int, true),
        (Type::Float, false),
        (Type::List(Box::new(Type::Float)), false),
    ] {
        let (a, b) = (typed_var("a", ty.clone()), typed_var("b", ty.clone()));
        let show = |x: super::Term| term::builtin("String.fromFloat", vec![x], None);
        let step = Proof::Congr {
            ctx: show(term::hole()),
            inner: Box::new(Proof::Rule {
                rule: WallRule::EqOfBeq,
                subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
                premises: vec![Proof::Hyp("when".into())],
            }),
        };
        let mut s = script(show(a.clone()), show(b.clone()), step);
        s.obligation.premise = Some(term::binop(BinOp::Eq, a, b));
        assert_eq!(kernel(&s).is_ok(), accepted, "{ty:?}: {:?}", kernel(&s));
    }
}

/// A split into the true and false cases holds only for a Bool: on an Int
/// neither case happens, and two branches proving the claim under
/// impossible hypotheses would prove anything. The kernel splits on a
/// comparison, and on a call only when the definition it opens is marked as
/// returning a Bool.
#[test]
fn a_split_is_on_a_bool() {
    use crate::ir::hir::ResolvedCallee;
    use crate::ir::identity::FnId;
    let zero = term::int(&0.into());
    let split = |on: super::Term| Proof::Cases {
        on,
        hyp: "h".into(),
        if_true: Box::new(Proof::Refl(zero.clone())),
        if_false: Box::new(Proof::Refl(zero.clone())),
    };
    let call = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(0)),
        vec![var("a")],
    ));
    let def = |returns_bool: bool| super::Def {
        fn_id: FnId(0),
        name: "__fn_0".into(),
        params: vec!["x".into()],
        returns_bool,
        lets: Vec::new(),
        body: term::boolean(true),
    };
    let s = script(zero.clone(), zero.clone(), split(var("a")));
    assert!(kernel(&s).is_err(), "an Int split on");
    let lt = term::binop(BinOp::Lt, var("a"), zero.clone());
    assert!(kernel(&script(zero.clone(), zero.clone(), split(lt))).is_ok());
    for (defs, accepted) in [
        (vec![], false),
        (vec![def(false)], false),
        (vec![def(true)], true),
    ] {
        let mut s = script(zero.clone(), zero.clone(), split(call.clone()));
        s.defs = defs;
        assert_eq!(kernel(&s).is_ok(), accepted, "{:?}", kernel(&s));
    }
}

/// `List.concat(xs, []) = xs` by induction on `xs`, a given of list type:
/// the kernel accepts it, and refuses it when `xs` is not declared a list,
/// when the hypothesis is used in the empty-list case, when the cell's two
/// names are the same, and when a hypothesis in scope mentions `xs`.
#[test]
fn the_kernel_inducts_on_a_list_given_and_refuses_mutations() {
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
        general: Vec::new(),
        ihs: vec![super::IhAt {
            name: "ih".into(),
            at: Vec::new(),
            carry: Vec::new(),
        }],
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
    assert_eq!(kernel(&good), Ok(()));
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
        assert!(kernel(&s).is_err(), "{kind}: the kernel accepted");
    }
}

/// Every builtin fact proves its own statement in the kernel, and a
/// citation is refused when it states the fact differently from the proof
/// it carries.
#[test]
fn every_builtin_fact_checks_and_a_misstated_citation_is_refused() {
    for fact in super::facts::all() {
        assert!(super::facts::is_fact_name(fact.key), "{}", fact.key);
        assert_eq!(
            crate::proof_kernel::verdict(&serialise(&fact.script, &BuiltinsOnly).unwrap()),
            Ok(fact.key.to_string())
        );
        let ob = &fact.script.obligation;
        let cite = Proof::Law {
            law: fact.key.into(),
            subst: ob.givens.iter().map(|g| (g.clone(), var(g))).collect(),
            premise: ob
                .premise
                .as_ref()
                .map(|_| Box::new(Proof::Hyp("when".into()))),
        };
        let mut citing = script(ob.lhs.clone(), ob.rhs.clone(), cite);
        citing.obligation.givens = ob.givens.clone();
        citing.obligation.premise = ob.premise.clone();
        citing.laws.push(fact.law_ref());
        assert_eq!(kernel(&citing), Ok(()), "{}", fact.key);
        let mut misstated = citing.clone();
        misstated.laws[0].rhs = misstated.laws[0].lhs.clone();
        misstated.obligation.rhs = misstated.obligation.lhs.clone();
        assert!(kernel(&misstated).is_err(), "{}", fact.key);
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
    assert_eq!(kernel(&s(l(&[1, 2]))), Ok(()));
    assert!(kernel(&s(l(&[1, 3]))).is_err());
}

/// `docs/builtin-facts.md` is generated from the registry.
#[test]
fn the_builtin_facts_page_is_generated_from_the_registry() {
    assert_eq!(
        super::facts::markdown(""),
        include_str!("../../../docs/builtin-facts.md"),
        "run `aver facts --markdown > docs/builtin-facts.md`"
    );
}

/// The two arithmetic steps: an honest `ring` and `linear` step is
/// accepted, and each way of getting the arithmetic wrong is refused by the
/// kernel written in Aver.
#[test]
fn the_kernel_refuses_wrong_ring_and_linear_steps() {
    let i = |n: i64| term::int(&n.into());
    let (a, b) = (|| var("a"), || var("b"));
    let mul = |x: super::Term, y: super::Term| term::binop(BinOp::Mul, x, y);
    let sub = |x: super::Term, y: super::Term| term::binop(BinOp::Sub, x, y);
    // (a + b) * (a + b) = a*a + 2*a*b + b*b.
    let square = mul(add(a(), b()), add(a(), b()));
    let expanded = |k: i64| add(add(mul(a(), a()), mul(mul(i(k), a()), b())), mul(b(), b()));
    let ring = |lhs: super::Term, rhs: super::Term, claim: super::Term| {
        script(square.clone(), claim, Proof::Ring { lhs, rhs })
    };
    assert_eq!(
        kernel(&ring(square.clone(), expanded(2), expanded(2))),
        Ok(())
    );
    let a_minus_b = sub(a(), b());
    let b_minus_a = sub(b(), a());
    let mut swapped = script(
        a_minus_b.clone(),
        b_minus_a.clone(),
        Proof::Ring {
            lhs: a_minus_b,
            rhs: b_minus_a,
        },
    );
    swapped.obligation.givens = vec!["a".into(), "b".into()];
    for (kind, s) in [
        (
            "a wrong normal form",
            ring(square.clone(), expanded(3), expanded(3)),
        ),
        ("two different polynomials", swapped),
    ] {
        assert!(kernel(&s).is_err(), "{kind}: the kernel accepted");
    }
    // `when a >= 1`, so `a > 0` is true: the opposite (`a <= 0`) and the
    // hypothesis add up to `-1 >= 0`.
    let goal = term::binop(BinOp::Gt, a(), i(0));
    let linear = |premise: super::Term, hyps: Vec<&str>, weights: Vec<i64>| {
        let mut s = script(
            goal.clone(),
            term::boolean(true),
            Proof::Linear {
                goal: goal.clone(),
                value: true,
                hyps: hyps.into_iter().map(String::from).collect(),
                weights: weights.into_iter().map(Into::into).collect(),
            },
        );
        s.obligation.premise = Some(premise);
        s
    };
    let at_least = |k: i64| term::binop(BinOp::Gte, a(), i(k));
    assert_eq!(
        kernel(&linear(at_least(1), vec!["when"], vec![1, 1])),
        Ok(())
    );
    for (kind, s) in [
        (
            "a weight that leaves a variable",
            linear(at_least(1), vec!["when"], vec![1, 2]),
        ),
        (
            "a negative weight",
            linear(at_least(1), vec!["when"], vec![1, -1]),
        ),
        (
            "a weight on a name that is no hypothesis",
            linear(at_least(1), vec!["h_nope"], vec![1, 1]),
        ),
        (
            "fewer weights than facts",
            linear(at_least(1), vec!["when"], vec![1]),
        ),
        (
            "a sum whose constant is not negative",
            linear(at_least(0), vec!["when"], vec![1, 1]),
        ),
        (
            "a hypothesis that is not an Int comparison",
            linear(
                term::bool_and(at_least(1), term::boolean(true)),
                vec!["when"],
                vec![1, 1],
            ),
        ),
    ] {
        assert!(kernel(&s).is_err(), "{kind}: the kernel accepted");
    }
}

/// A list literal with an element is that element in front of the rest,
/// and nothing else, in the kernel.
#[test]
fn the_kernel_reads_a_list_literal_as_a_cell() {
    let lit = term::list(vec![var("y"), var("z")]);
    let cell = |h: &str, t: super::Term| term::builtin("List.prepend", vec![var(h), t], None);
    let verdicts = |lhs: super::Term, rhs: super::Term, list: super::Term| {
        let mut s = script(lhs, rhs, Proof::Cell { list });
        s.obligation.givens = vec!["y".into(), "z".into()];
        kernel(&s)
    };
    let good = cell("y", term::list(vec![var("z")]));
    assert_eq!(verdicts(lit.clone(), good, lit.clone()), Ok(()));
    for (kind, verdict) in [
        (
            "the elements swapped",
            verdicts(
                lit.clone(),
                cell("z", term::list(vec![var("y")])),
                lit.clone(),
            ),
        ),
        (
            "the empty list",
            verdicts(term::nil(), term::nil(), term::nil()),
        ),
    ] {
        assert!(verdict.is_err(), "{kind}: the kernel accepted");
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
    assert_eq!(kernel(&s), Ok(()));
    let step = Proof::UnfoldConst {
        name: "base".into(),
    };
    assert!(claim(&step, &s, &Vec::new()).is_err());
    s.consts[0].value = term::int(&41.into());
    assert!(kernel(&s).is_err());
}

#[test]
fn a_definition_opens_its_local_bindings_in_order() {
    let def = super::Def {
        fn_id: crate::ir::identity::FnId(0),
        name: "f".into(),
        params: vec!["x".into()],
        returns_bool: false,
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
    assert!(kernel(&s).is_ok());
    s.proof = split(vec![case(false)]);
    assert!(kernel(&s).is_err());
    s.proof = split(vec![case(true), case(false)]);
    assert!(kernel(&s).is_err());
    s.proof = split(vec![case(false), case(true)]);
    s.obligation.finite.clear();
    assert!(kernel(&s).is_err());
}

#[test]
fn a_catch_all_arm_binds_the_value_it_is_chosen_for() {
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
        super::claim::arm_equation(&var("c"), &arms, 2, &[], std::slice::from_ref(&five)).unwrap();
    assert_eq!(premise, Eqn::new(var("c"), five.clone()));
    assert_eq!(body, five);
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
        returns_bool: false,
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

/// The name of a step's constructor. The match has no catch-all, so a new
/// constructor does not compile until it is named here, and
/// [`every_step_constructor_is_accepted_and_refused_by_the_kernel`] then
/// fails until it has an accepted and a refused sample.
fn constructor(p: &Proof) -> &'static str {
    match p {
        Proof::Refl(_) => "refl",
        Proof::Symm(_) => "symm",
        Proof::Trans { .. } => "trans",
        Proof::Congr { .. } => "congr",
        Proof::Unfold { .. } => "unfold",
        Proof::UnfoldConst { .. } => "const",
        Proof::Arm { .. } => "arm",
        Proof::Proj { .. } => "proj",
        Proof::Cell { .. } => "cell",
        Proof::Hyp(_) => "hyp",
        Proof::Rule { .. } => "rule",
        Proof::Law { .. } => "law",
        Proof::Compute { .. } => "compute",
        Proof::Cases { .. } => "cases",
        Proof::Have { .. } => "have",
        Proof::Absurd { .. } => "absurd",
        Proof::Induct { .. } => "induct",
        Proof::InductList { .. } => "listinduct",
        Proof::InductInt { .. } => "intinduct",
        Proof::Linear { .. } => "linear",
        Proof::Ring { .. } => "ring",
        Proof::Enum { .. } => "enum",
    }
}

const CONSTRUCTORS: [&str; 22] = [
    "have",
    "refl",
    "symm",
    "trans",
    "congr",
    "unfold",
    "const",
    "arm",
    "proj",
    "cell",
    "hyp",
    "rule",
    "law",
    "compute",
    "cases",
    "absurd",
    "induct",
    "listinduct",
    "intinduct",
    "linear",
    "ring",
    "enum",
];

/// For every constructor, one script whose proof is that constructor and
/// that the kernel accepts, and the same script changed so that the step
/// no longer proves the claim, which the kernel refuses. The kernel is the
/// one checker of step proofs in the compiler; this is the table a new
/// constructor has to join.
#[test]
fn every_step_constructor_is_accepted_and_refused_by_the_kernel() {
    use crate::ast::Literal;
    use crate::ir::hir::{ResolvedCallee, ResolvedCtor};
    use crate::ir::identity::FnId;
    let (a, b) = (|| var("a"), || var("b"));
    let i = |n: i64| term::int(&n.into());
    let comm = |x: super::Term, y: super::Term| Proof::Rule {
        rule: WallRule::AddComm,
        subst: vec![("a".into(), x), ("b".into(), y)],
        premises: Vec::new(),
    };
    let with_premise = |mut s: Script, p: super::Term| {
        s.obligation.premise = Some(p);
        s
    };
    let arm = |pattern, body| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let positive = || term::binop(BinOp::Gt, a(), i(0));

    // f(x) = x + x
    let f = FnId(3);
    let call_f = |x: super::Term| Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(f), vec![x]));
    let def_f = super::Def {
        fn_id: f,
        name: "__fn_3".into(),
        params: vec!["x".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: add(var("x"), var("x")),
    };
    let unfold = |claim: super::Term| {
        let mut s = script(
            call_f(a()),
            claim,
            Proof::Unfold {
                fn_id: f,
                arm: 0,
                args: vec![a()],
                binders: Vec::new(),
                premise: None,
            },
        );
        s.defs.push(def_f.clone());
        s
    };

    // g(xs) = match xs { [] -> 0, [h, ..t] -> g(t) }, so g(xs) = 0.
    let g = FnId(4);
    let call_g = |x: super::Term| Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(g), vec![x]));
    let cell = |h: &str, t: &str| term::builtin("List.prepend", vec![var(h), var(t)], None);
    let def_g = super::Def {
        fn_id: g,
        name: "__fn_4".into(),
        params: vec!["xs".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("xs")),
            arms: vec![
                arm(ResolvedPattern::EmptyList, i(0)),
                arm(
                    ResolvedPattern::Cons("h".into(), "t".into()),
                    call_g(var("t")),
                ),
            ],
        }),
    };
    let induct = |claim: i64| {
        let open = |arm: u32, at: super::Term, binders: Vec<super::Term>| Proof::Unfold {
            fn_id: g,
            arm,
            args: vec![at.clone()],
            binders,
            premise: Some(Box::new(Proof::Refl(at))),
        };
        let mut s = script(
            call_g(var("xs")),
            i(claim),
            Proof::Induct {
                fn_id: g,
                args: vec![var("xs")],
                lhs: call_g(var("xs")),
                rhs: i(claim),
                carried: Vec::new(),
                cases: vec![
                    super::InductCase {
                        binders: Vec::new(),
                        ihs: Vec::new(),
                        carry: Vec::new(),
                        more: Vec::new(),
                        proof: open(1, term::nil(), Vec::new()),
                    },
                    super::InductCase {
                        binders: vec!["h".into(), "t".into()],
                        ihs: vec!["ih".into()],
                        carry: vec![Vec::new()],
                        more: Vec::new(),
                        proof: Proof::Trans {
                            terms: vec![call_g(cell("h", "t")), call_g(var("t")), i(claim)],
                            steps: vec![
                                open(2, cell("h", "t"), vec![var("h"), var("t")]),
                                Proof::Hyp("ih".into()),
                            ],
                        },
                    },
                ],
            },
        );
        s.obligation.givens = vec!["xs".into()];
        s.defs.push(def_g.clone());
        s
    };

    let choose = Spanned::bare(ResolvedExpr::Match {
        subject: Box::new(term::boolean(true)),
        arms: vec![
            arm(ResolvedPattern::Literal(Literal::Bool(true)), a()),
            arm(ResolvedPattern::Literal(Literal::Bool(false)), b()),
        ],
    });
    let pick = |k: u32| {
        script(
            choose.clone(),
            if k == 1 { a() } else { b() },
            Proof::Arm {
                term: choose.clone(),
                arm: k,
                binders: Vec::new(),
                premise: Box::new(Proof::Refl(term::boolean(true))),
            },
        )
    };

    let record = Spanned::bare(ResolvedExpr::Attr(
        Box::new(Spanned::bare(ResolvedExpr::RecordCreate {
            type_id: None,
            type_name: "P".into(),
            fields: vec![("x".into(), a())],
        })),
        "x".into(),
    ));
    let proj = |claim: super::Term| {
        script(
            record.clone(),
            claim,
            Proof::Proj {
                term: record.clone(),
            },
        )
    };

    let lit = term::list(vec![a(), b()]);
    let as_cell = |h: super::Term, t: super::Term| term::builtin("List.prepend", vec![h, t], None);

    let based = |value: super::Term| {
        let mut s = script(
            var("Lib.base"),
            value,
            Proof::UnfoldConst {
                name: "Lib.base".into(),
            },
        );
        s.consts.push(Const {
            name: "Lib.base".into(),
            value: i(40),
        });
        s
    };

    let fact = &super::facts::all()[0];
    let cite = |misstate: bool| {
        let ob = &fact.script.obligation;
        let mut s = script(
            ob.lhs.clone(),
            ob.rhs.clone(),
            Proof::Law {
                law: fact.key.into(),
                subst: ob.givens.iter().map(|g| (g.clone(), var(g))).collect(),
                premise: ob
                    .premise
                    .as_ref()
                    .map(|_| Box::new(Proof::Hyp("when".into()))),
            },
        );
        s.obligation.givens = ob.givens.clone();
        s.obligation.premise = ob.premise.clone();
        s.laws.push(fact.law_ref());
        if misstate {
            s.laws[0].rhs = s.laws[0].lhs.clone();
            s.obligation.rhs = s.obligation.lhs.clone();
        }
        s
    };

    let split = |if_false: Proof| {
        script(
            a(),
            a(),
            Proof::Cases {
                on: positive(),
                hyp: "h".into(),
                if_true: Box::new(Proof::Refl(a())),
                if_false: Box::new(if_false),
            },
        )
    };

    let absurd = |premise: bool| {
        with_premise(
            script(
                a(),
                b(),
                Proof::Absurd {
                    contradiction: Box::new(Proof::Hyp("when".into())),
                    lhs: a(),
                    rhs: b(),
                },
            ),
            term::boolean(premise),
        )
    };

    let mut enum_split = |cases: Vec<bool>| {
        let claim = term::binop(BinOp::Eq, var("c"), var("c"));
        let mut s = script(
            claim.clone(),
            term::boolean(true),
            Proof::Enum {
                var: "c".into(),
                lhs: claim,
                rhs: term::boolean(true),
                cases: cases
                    .into_iter()
                    .map(|v| Proof::Compute {
                        lhs: term::binop(BinOp::Eq, term::boolean(v), term::boolean(v)),
                        rhs: term::boolean(true),
                    })
                    .collect(),
            },
        );
        s.obligation.givens = vec!["c".into()];
        s.obligation.finite = vec![("c".into(), super::Finite::Bool)];
        s
    };

    let concat = |x: super::Term, y: super::Term| term::builtin("List.concat", vec![x, y], None);
    let list_induct = |nil: Proof| {
        let mut s = script(
            concat(var("xs"), term::nil()),
            var("xs"),
            Proof::InductList {
                var: "xs".into(),
                lhs: concat(var("xs"), term::nil()),
                rhs: var("xs"),
                nil: Box::new(nil),
                head: "h".into(),
                tail: "t".into(),
                general: Vec::new(),
                ihs: vec![super::IhAt {
                    name: "ih".into(),
                    at: Vec::new(),
                    carry: Vec::new(),
                }],
                cons: Box::new(Proof::Trans {
                    terms: vec![
                        concat(cell("h", "t"), term::nil()),
                        term::builtin(
                            "List.prepend",
                            vec![var("h"), concat(var("t"), term::nil())],
                            None,
                        ),
                        cell("h", "t"),
                    ],
                    steps: vec![
                        Proof::Rule {
                            rule: WallRule::ConcatCons,
                            subst: vec![
                                ("x".into(), var("h")),
                                ("a".into(), var("t")),
                                ("b".into(), term::nil()),
                            ],
                            premises: Vec::new(),
                        },
                        Proof::Congr {
                            ctx: term::builtin("List.prepend", vec![var("h"), term::hole()], None),
                            inner: Box::new(Proof::Hyp("ih".into())),
                        },
                    ],
                }),
            },
        );
        s.obligation.givens = vec!["xs".into()];
        s.obligation.lists = vec!["xs".into()];
        s
    };
    let concat_nil = Proof::Rule {
        rule: WallRule::ConcatNil,
        subst: vec![("b".into(), term::nil())],
        premises: Vec::new(),
    };

    // n = n down to zero; the changed sample reads the claim at n - 1 in
    // the case n <= 0, where it is not in scope.
    let int_induct = |base: Proof| {
        let mut s = script(
            var("n"),
            var("n"),
            Proof::InductInt {
                var: "n".into(),
                lhs: var("n"),
                rhs: var("n"),
                guard: "g".into(),
                base: Box::new(base),
                carried: Vec::new(),
                general: Vec::new(),
                ihs: vec![super::IhAt {
                    name: "ih".into(),
                    at: Vec::new(),
                    carry: Vec::new(),
                }],
                step: Box::new(Proof::Refl(var("n"))),
            },
        );
        s.obligation.givens = vec!["n".into()];
        s.obligation.ints = vec!["n".into()];
        s
    };

    let linear = |bound: i64| {
        with_premise(
            script(
                positive(),
                term::boolean(true),
                Proof::Linear {
                    goal: positive(),
                    value: true,
                    hyps: vec!["when".into()],
                    weights: vec![1.into(), 1.into()],
                },
            ),
            term::binop(BinOp::Gte, a(), i(bound)),
        )
    };

    let mul = |x: super::Term, y: super::Term| term::binop(BinOp::Mul, x, y);
    let ring = |k: i64| {
        let lhs = mul(add(a(), b()), add(a(), b()));
        let rhs = add(add(mul(a(), a()), mul(mul(i(k), a()), b())), mul(b(), b()));
        script(lhs.clone(), rhs.clone(), Proof::Ring { lhs, rhs })
    };

    let eight_minus_one = term::binop(BinOp::Sub, i(8), i(1));
    let compute = |n: i64| {
        script(
            eight_minus_one.clone(),
            i(n),
            Proof::Compute {
                lhs: eight_minus_one.clone(),
                rhs: i(n),
            },
        )
    };

    // A cut whose proof must state its fact: `a > 0` from `when`, then
    // the claim from the cut.
    let have = |fact: super::Term| {
        with_premise(
            script(
                positive(),
                term::boolean(true),
                Proof::Have {
                    name: "h".into(),
                    fact,
                    proof: Box::new(Proof::Hyp("when".into())),
                    body: Box::new(Proof::Hyp("h".into())),
                },
            ),
            positive(),
        )
    };

    let samples: Vec<(Script, Script)> = vec![
        (
            script(a(), a(), Proof::Refl(a())),
            script(a(), b(), Proof::Refl(a())),
        ),
        (
            script(
                add(b(), a()),
                add(a(), b()),
                Proof::Symm(Box::new(comm(a(), b()))),
            ),
            script(
                add(a(), b()),
                add(b(), a()),
                Proof::Symm(Box::new(comm(a(), b()))),
            ),
        ),
        (
            script(
                add(a(), b()),
                add(b(), a()),
                Proof::Trans {
                    terms: vec![add(a(), b()), add(b(), a())],
                    steps: vec![comm(a(), b())],
                },
            ),
            script(
                add(a(), b()),
                add(a(), b()),
                Proof::Trans {
                    terms: vec![add(a(), b()), add(a(), b())],
                    steps: vec![comm(a(), b())],
                },
            ),
        ),
        (
            script(
                add(add(a(), b()), a()),
                add(add(b(), a()), a()),
                Proof::Congr {
                    ctx: add(term::hole(), a()),
                    inner: Box::new(comm(a(), b())),
                },
            ),
            script(
                add(add(a(), b()), add(a(), b())),
                add(add(b(), a()), add(b(), a())),
                Proof::Congr {
                    ctx: add(term::hole(), term::hole()),
                    inner: Box::new(comm(a(), b())),
                },
            ),
        ),
        (unfold(add(a(), a())), unfold(a())),
        (based(i(40)), based(i(41))),
        (pick(1), pick(2)),
        (proj(a()), proj(b())),
        (
            script(
                lit.clone(),
                as_cell(a(), term::list(vec![b()])),
                Proof::Cell { list: lit.clone() },
            ),
            script(
                lit.clone(),
                as_cell(b(), term::list(vec![a()])),
                Proof::Cell { list: lit.clone() },
            ),
        ),
        (
            with_premise(
                script(positive(), term::boolean(true), Proof::Hyp("when".into())),
                positive(),
            ),
            script(positive(), term::boolean(true), Proof::Hyp("when".into())),
        ),
        (
            script(add(a(), b()), add(b(), a()), comm(a(), b())),
            script(add(a(), b()), add(a(), b()), comm(a(), b())),
        ),
        (cite(false), cite(true)),
        (compute(7), compute(6)),
        (split(Proof::Refl(a())), split(Proof::Refl(b()))),
        (have(positive()), have(term::binop(BinOp::Gt, b(), i(0)))),
        (absurd(false), absurd(true)),
        (induct(0), induct(1)),
        (
            list_induct(concat_nil.clone()),
            list_induct(Proof::Hyp("ih".into())),
        ),
        (
            int_induct(Proof::Refl(var("n"))),
            int_induct(Proof::Hyp("ih".into())),
        ),
        (linear(1), linear(0)),
        (ring(2), ring(3)),
        (enum_split(vec![false, true]), enum_split(vec![false])),
    ];
    let mut seen = Vec::new();
    for (good, bad) in &samples {
        let name = constructor(&good.proof);
        assert_eq!(
            constructor(&bad.proof),
            name,
            "a sample changes its constructor"
        );
        assert_eq!(
            kernel(good),
            Ok(()),
            "{name}: the kernel refused the honest sample"
        );
        assert!(
            kernel(bad).is_err(),
            "{name}: the kernel accepted the changed sample"
        );
        seen.push(name);
    }
    for name in CONSTRUCTORS {
        assert!(seen.contains(&name), "{name}: no sample in this table");
    }
}
