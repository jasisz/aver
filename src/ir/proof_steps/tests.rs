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
        sums: Vec::new(),
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
    assert_eq!(
        super::induct::recursion(&on_tail),
        Ok(Some(super::induct::Recursion { at: 0, guard: None }))
    );
    let on_itself = def(call(vec![var("xs")]));
    assert!(super::induct::recursion(&on_itself).is_err());
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

/// A chain of equalities, each two facts `p >= 0` and `-p >= 0`, is
/// substituted away before elimination pairs bounds: eight digits of a
/// value read back, where pairing every bound with every other would pass
/// the row limit. The weights found still add up to a contradiction.
#[test]
fn a_linear_certificate_substitutes_equalities_away() {
    use super::linear;
    let lit = |n: i64| term::int(&n.into());
    let cmp = |op, a, b| term::binop(op, a, b);
    let mul = |a, k: i64| term::binop(BinOp::Mul, a, lit(k));
    let mut atoms = Vec::new();
    let mut facts = Vec::new();
    // v = q1 * 256 + d0, q1 = q2 * 256 + d1, ..., with 0 <= d < 256,
    // 0 <= v < 256^8 and 0 <= q8 < 1.
    let digits = 8;
    let q = |i: usize| {
        if i == 0 {
            var("v")
        } else {
            var(&format!("q{i}"))
        }
    };
    let d = |i: usize| var(&format!("d{i}"));
    for i in 0..digits {
        let whole = add(mul(q(i + 1), 256), d(i));
        for op in [BinOp::Lte, BinOp::Gte] {
            facts.push(linear::as_nonneg(&cmp(op, whole.clone(), q(i)), true, &mut atoms).unwrap());
        }
        facts.push(linear::as_nonneg(&cmp(BinOp::Lte, lit(0), d(i)), true, &mut atoms).unwrap());
        facts.push(linear::as_nonneg(&cmp(BinOp::Lt, d(i), lit(256)), true, &mut atoms).unwrap());
    }
    facts.push(linear::as_nonneg(&cmp(BinOp::Lte, lit(0), q(digits)), true, &mut atoms).unwrap());
    facts.push(linear::as_nonneg(&cmp(BinOp::Lt, q(digits), lit(1)), true, &mut atoms).unwrap());
    // The digits read back: not (sum <= v).
    let mut sum = d(digits - 1);
    for i in (0..digits - 1).rev() {
        sum = add(mul(sum, 256), d(i));
    }
    facts.insert(
        0,
        linear::as_nonneg(&cmp(BinOp::Lte, sum, q(0)), false, &mut atoms).unwrap(),
    );
    let w = linear::certificate(&facts).expect("a certificate");
    assert!(linear::contradicts(&linear::combine(&facts, &w).unwrap()));
    // Without `0 <= q8` the digits may add up to more than `v`.
    let mut open = facts.clone();
    open.remove(open.len() - 2);
    assert!(linear::certificate(&open).is_none());
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
        Proof::Split { .. } => "split",
        Proof::Have { .. } => "have",
        Proof::Absurd { .. } => "absurd",
        Proof::Induct { .. } => "induct",
        Proof::InductList { .. } => "listinduct",
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
    "split",
    "absurd",
    "induct",
    "listinduct",
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
        (linear(1), linear(0)),
        (ring(2), ring(3)),
        (enum_split(vec![false, true]), enum_split(vec![false])),
        (list_split(true), list_split(false)),
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

/// Pieces of scripts that induct along a function counting an Int toward
/// zero: `f` (FnId 9) over parameters `params`, matching `subject` with
/// the arm `true` first.
mod toward_zero {
    use super::{kernel, script};
    use crate::ast::{BinOp, Literal, Spanned};
    use crate::ir::hir::{
        BuiltinIntrinsic, ResolvedCallee, ResolvedExpr, ResolvedMatchArm, ResolvedPattern,
    };
    use crate::ir::identity::FnId;
    use crate::ir::proof_steps::term::{self, var};
    use crate::ir::proof_steps::{Def, IhAt, InductCase, Proof, Script, Term};

    pub const F: FnId = FnId(9);

    pub fn i(n: i64) -> Term {
        term::int(&n.into())
    }
    pub fn call(args: Vec<Term>) -> Term {
        Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(F), args))
    }
    pub fn div(x: Term, k: Term) -> Term {
        term::intrinsic(BuiltinIntrinsic::IntDivEuclid, vec![x, k])
    }
    pub fn less(x: Term, k: i64) -> Term {
        term::binop(BinOp::Sub, x, i(k))
    }
    pub fn gt0(x: Term) -> Term {
        term::binop(BinOp::Gt, x, i(0))
    }
    pub fn arm(pattern: ResolvedPattern, body: Term) -> ResolvedMatchArm {
        ResolvedMatchArm {
            pattern,
            body: Box::new(body),
            binding_slots: std::sync::OnceLock::new(),
        }
    }
    pub fn when(b: bool) -> ResolvedPattern {
        ResolvedPattern::Literal(Literal::Bool(b))
    }
    /// `f(params) = match subject { true -> on_true, false -> on_false }`.
    pub fn def(params: &[&str], subject: Term, on_true: Term, on_false: Term) -> Def {
        Def {
            fn_id: F,
            name: "__fn_9".into(),
            params: params.iter().map(|p| p.to_string()).collect(),
            returns_bool: false,
            lets: Vec::new(),
            body: Spanned::bare(ResolvedExpr::Match {
                subject: Box::new(subject),
                arms: vec![arm(when(true), on_true), arm(when(false), on_false)],
            }),
        }
    }
    /// `f(n) = match n > 0 { true -> f(go), false -> 0 }`.
    pub fn halving(go: Term) -> Def {
        def(&["n"], gt0(var("n")), call(vec![go]), i(0))
    }
    pub fn case(guard: &str, ihs: &[&str], more: Vec<(usize, IhAt)>, proof: Proof) -> InductCase {
        InductCase {
            binders: vec![guard.into()],
            ihs: ihs.iter().map(|h| h.to_string()).collect(),
            carry: ihs.iter().map(|_| Vec::new()).collect(),
            more,
            proof,
        }
    }
    pub fn ih(name: &str, at: Vec<Term>) -> IhAt {
        IhAt {
            name: name.into(),
            at,
            carry: Vec::new(),
        }
    }
    /// The claim `lhs = rhs` by induction along `d` at `args`, the cases in
    /// arm order (`true`, then `false`); `n`, and the givens `more_givens`,
    /// are givens, `n` of type Int.
    pub fn along(
        d: Def,
        args: Vec<Term>,
        lhs: Term,
        rhs: Term,
        cases: Vec<InductCase>,
        more_givens: &[&str],
    ) -> Script {
        let mut s = script(
            lhs.clone(),
            rhs.clone(),
            Proof::Induct {
                fn_id: F,
                args,
                lhs,
                rhs,
                carried: Vec::new(),
                cases,
            },
        );
        s.obligation.givens = std::iter::once("n")
            .chain(more_givens.iter().copied())
            .map(String::from)
            .collect();
        s.obligation.ints = vec!["n".into()];
        s.defs.push(d);
        s
    }
    /// `(n > 0) = (n > 0)` along `d`, each case proved by `proofs`.
    pub fn trivially(d: Def, ihs: &[&str], on_true: Proof, on_false: Proof) -> Script {
        let c = gt0(var("n"));
        along(
            d,
            vec![var("n")],
            c.clone(),
            c,
            vec![
                case("g", ihs, Vec::new(), on_true),
                case("g", &[], Vec::new(), on_false),
            ],
            &[],
        )
    }
    pub fn refl() -> Proof {
        Proof::Refl(gt0(var("n")))
    }
    pub fn accepted(s: &Script) {
        assert_eq!(kernel(s), Ok(()));
    }
    pub fn refused(s: &Script, why: &str) {
        match kernel(s) {
            Ok(()) => panic!("accepted: {why}"),
            Err(m) => assert!(m.contains(why), "refused for another reason: {m}"),
        }
    }
}

/// The gate decides which descents an induction along a function that
/// counts an Int toward zero may follow: `n - 1` and `n / k` for a literal
/// `k` of at least 2, under `n <= 0` false or `n > 0` true. Every other
/// descent, a call in the arm where `n` is at most 0, a comparison other
/// than those two, and `n` rebound around the call are refused, both by
/// the kernel and by the gate the producer and Lean read.
#[test]
fn an_induction_toward_zero_follows_only_the_descents_the_gate_checked() {
    use toward_zero::*;
    let n = || var("n");
    for go in [less(n(), 1), div(n(), i(2)), div(n(), i(256))] {
        let d = halving(go);
        assert!(matches!(
            super::induct::recursion(&d),
            Ok(Some(super::induct::Recursion {
                at: 0,
                guard: Some(_)
            }))
        ));
        accepted(&trivially(d, &["ih"], refl(), refl()));
    }
    // Under `n <= 0` false as well, the arm `false` first in the body.
    let counted = def(
        &["n"],
        term::binop(BinOp::Lte, n(), i(0)),
        i(0),
        call(vec![div(n(), i(3))]),
    );
    let c = gt0(n());
    accepted(&along(
        counted,
        vec![n()],
        c.clone(),
        c,
        vec![
            case("g", &[], Vec::new(), refl()),
            case("g", &["ih"], Vec::new(), refl()),
        ],
        &[],
    ));
    let forged = [
        less(n(), 0),
        less(n(), 2),
        term::binop(BinOp::Add, n(), i(1)),
        div(n(), i(1)),
        div(n(), i(0)),
        div(n(), i(-2)),
        div(n(), var("n")),
        div(var("m"), i(2)),
        term::intrinsic(
            crate::ir::hir::BuiltinIntrinsic::IntModEuclid,
            vec![n(), i(2)],
        ),
        n(),
    ];
    for go in forged {
        let d = halving(go.clone());
        assert!(super::induct::recursion(&d).is_err(), "gate took {go:?}");
        refused(
            &trivially(d, &["ih"], refl(), refl()),
            "does not count n down to zero",
        );
    }
    // A recursive call where n is at most 0.
    let stops_late = def(&["n"], gt0(n()), i(0), call(vec![less(n(), 1)]));
    refused(
        &trivially(stops_late, &[], refl(), refl()),
        "does not count n down to zero",
    );
    // `n >= 0` lets n = 0 call itself at n - 1.
    let at_zero = def(
        &["n"],
        term::binop(BinOp::Gte, n(), i(0)),
        call(vec![less(n(), 1)]),
        i(0),
    );
    refused(
        &trivially(at_zero, &["ih"], refl(), refl()),
        "does not count n down to zero",
    );
    // n rebound by an inner arm around the call.
    let rebound = halving(Spanned::bare(ResolvedExpr::Match {
        subject: Box::new(i(5)),
        arms: vec![arm(
            ResolvedPattern::Ident("n".into()),
            call(vec![less(n(), 1)]),
        )],
    }));
    assert!(super::induct::recursion(&rebound).is_err());
    refused(
        &trivially(rebound, &["ih"], refl(), refl()),
        "does not count n down to zero",
    );
}

/// An induction hypothesis exists only where the comparison lets the call
/// happen: none in the case where `n` is at most 0, as a named one or a
/// further one; and the hypothesis at `n / 2` does not prove the claim at
/// `n`. The comparison's value is the case's own: read at the other value
/// it proves nothing. The given must be an Int, and the case's one name
/// is the comparison's hypothesis.
#[test]
fn an_induction_toward_zero_has_no_hypothesis_outside_the_comparison() {
    use toward_zero::*;
    let n = || var("n");
    let d = || halving(div(n(), i(2)));
    // A hypothesis in the case where n is at most 0.
    let mut base_ih = trivially(d(), &["ih"], refl(), refl());
    if let Proof::Induct { cases, .. } = &mut base_ih.proof {
        cases[1].ihs = vec!["ih".into()];
        cases[1].carry = vec![Vec::new()];
    }
    refused(&base_ih, "0 recursive calls, 1 hypotheses");
    let mut base_more = trivially(d(), &["ih"], refl(), refl());
    if let Proof::Induct { cases, .. } = &mut base_more.proof {
        cases[1].more = vec![(0, ih("m", Vec::new()))];
    }
    refused(&base_more, "no recursive call 0");
    // The hypothesis is the claim at n / 2, not at n.
    refused(
        &trivially(d(), &["ih"], Proof::Hyp("ih".into()), refl()),
        "the case proves a different equation",
    );
    // `(n > 0) = true` holds by the comparison where it is true only.
    let holds = |on_false: Proof| {
        along(
            d(),
            vec![n()],
            gt0(n()),
            term::boolean(true),
            vec![
                case("g", &["_"], Vec::new(), Proof::Hyp("g".into())),
                case("g", &[], Vec::new(), on_false),
            ],
            &[],
        )
    };
    refused(
        &holds(Proof::Hyp("g".into())),
        "the case proves a different equation",
    );
    // Not an Int.
    let mut untyped = trivially(d(), &["ih"], refl(), refl());
    untyped.obligation.ints.clear();
    refused(&untyped, "n is not a given of type Int");
    // The comparison's hypothesis has one name.
    let mut unnamed = trivially(d(), &["ih"], refl(), refl());
    if let Proof::Induct { cases, .. } = &mut unnamed.proof {
        cases[0].binders.clear();
    }
    refused(&unnamed, "wrong number of names");
}

/// `f(n) = 0` for `f(n) = match n > 0 { true -> f(n / 2) + f(n / 3),
/// false -> 0 }`, for every Int, zero and the negative ones in the case
/// where the comparison is false: each recursive call has its own
/// hypothesis at its own divisor, so swapping them is refused.
#[test]
fn an_induction_toward_zero_gives_each_call_its_own_descent() {
    use toward_zero::*;
    let n = || var("n");
    let half = || div(n(), i(2));
    let third = || div(n(), i(3));
    let d = def(
        &["n"],
        gt0(n()),
        term::binop(BinOp::Add, call(vec![half()]), call(vec![third()])),
        i(0),
    );
    let open = |arm: u32| Proof::Unfold {
        fn_id: F,
        arm,
        args: vec![n()],
        binders: Vec::new(),
        premise: Some(Box::new(Proof::Hyp("g".into()))),
    };
    let plus = |a: super::Term, b: super::Term| term::binop(BinOp::Add, a, b);
    let proof = |first: &str, second: &str| {
        let step = Proof::Trans {
            terms: vec![
                call(vec![n()]),
                plus(call(vec![half()]), call(vec![third()])),
                plus(i(0), call(vec![third()])),
                plus(i(0), i(0)),
                i(0),
            ],
            steps: vec![
                open(1),
                Proof::Congr {
                    ctx: plus(term::hole(), call(vec![third()])),
                    inner: Box::new(Proof::Hyp(first.into())),
                },
                Proof::Congr {
                    ctx: plus(i(0), term::hole()),
                    inner: Box::new(Proof::Hyp(second.into())),
                },
                Proof::Compute {
                    lhs: plus(i(0), i(0)),
                    rhs: i(0),
                },
            ],
        };
        along(
            d.clone(),
            vec![n()],
            call(vec![n()]),
            i(0),
            vec![
                case("g", &["a", "b"], Vec::new(), step),
                case("g", &[], Vec::new(), open(2)),
            ],
            &[],
        )
    };
    accepted(&proof("a", "b"));
    refused(&proof("b", "a"), "the step does not join the written terms");
}

/// An accumulator the recursive call changes is generalised: the call's
/// hypothesis is the claim at its own value of it, and a further one is
/// the claim at any value, at the Int that call passes. A name an inner
/// match binds may stand there only where it is not generalised.
#[test]
fn an_induction_toward_zero_generalises_what_the_call_changes() {
    use toward_zero::*;
    let n = || var("n");
    let acc = || var("acc");
    // f(n, acc) = match n > 0 { true -> f(n / 2, acc + 1), false -> acc }
    let d = def(
        &["n", "acc"],
        gt0(n()),
        call(vec![div(n(), i(2)), term::binop(BinOp::Add, acc(), i(1))]),
        acc(),
    );
    // acc = acc: the hypothesis is `acc + 1 = acc + 1`, a further one
    // `7 = 7`; neither is the claim.
    let claim = |on_true: Proof, more: Vec<(usize, super::IhAt)>| {
        along(
            d.clone(),
            vec![n(), acc()],
            acc(),
            acc(),
            vec![
                case("g", &["ih"], more, on_true),
                case("g", &[], Vec::new(), Proof::Refl(acc())),
            ],
            &["acc"],
        )
    };
    accepted(&claim(Proof::Refl(acc()), Vec::new()));
    refused(
        &claim(Proof::Hyp("ih".into()), Vec::new()),
        "the case proves a different equation",
    );
    accepted(&claim(Proof::Refl(acc()), vec![(0, ih("m", vec![i(7)]))]));
    refused(
        &claim(Proof::Hyp("m".into()), vec![(0, ih("m", vec![i(7)]))]),
        "the case proves a different equation",
    );
    refused(
        &claim(Proof::Refl(acc()), vec![(0, ih("m", Vec::new()))]),
        "gives 0 values for 1 varied givens",
    );
    // A hypothesis about the accumulator is out of scope unless carried.
    let positive = gt0(acc());
    let mut about_acc = along(
        d.clone(),
        vec![n(), acc()],
        positive.clone(),
        term::boolean(true),
        vec![
            case("g", &["_"], Vec::new(), Proof::Hyp("when".into())),
            case("g", &[], Vec::new(), Proof::Hyp("when".into())),
        ],
        &["acc"],
    );
    about_acc.obligation.premise = Some(positive);
    refused(&about_acc, "hypothesis when is not in scope");
    // f(n, xs) = match n > 0 { true -> match xs { [] -> 0, [h, ..t] ->
    // f(n - 1, t) }, false -> 0 }: `t` is bound inside the arm.
    let inner = def(
        &["n", "xs"],
        gt0(n()),
        Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("xs")),
            arms: vec![
                arm(ResolvedPattern::EmptyList, i(0)),
                arm(
                    ResolvedPattern::Cons("h".into(), "t".into()),
                    call(vec![less(n(), 1), var("t")]),
                ),
            ],
        }),
        i(0),
    );
    let on = |second: super::Term, givens: &[&str]| {
        let c = gt0(n());
        along(
            inner.clone(),
            vec![n(), second],
            c.clone(),
            c,
            vec![
                case("g", &["ih"], Vec::new(), refl()),
                case("g", &[], Vec::new(), refl()),
            ],
            givens,
        )
    };
    refused(
        &on(var("ys"), &["ys"]),
        "a recursive call reads a name an inner match binds",
    );
    accepted(&on(term::nil(), &[]));
}

/// Two definitions that call each other are refused before any induction,
/// counting toward zero or not.
#[test]
fn an_induction_toward_zero_through_another_definition_is_refused() {
    use crate::ir::identity::FnId;
    use toward_zero::*;
    let n = || var("n");
    let mut s = trivially(halving(less(n(), 1)), &["ih"], refl(), refl());
    let other = Spanned::bare(crate::ir::hir::ResolvedExpr::Call(
        crate::ir::hir::ResolvedCallee::Fn(FnId(10)),
        vec![less(n(), 1)],
    ));
    s.defs[0] = def(&["n"], gt0(n()), other, i(0));
    s.defs.push(super::Def {
        fn_id: FnId(10),
        name: "__fn_10".into(),
        ..halving(less(n(), 1))
    });
    refused(&s, "part of a mutual recursion");
}

/// A split on a constructor: `f(a) >= 0` for `f(x) = match x { [] -> 0,
/// [h, ..t] -> 1 }`, one case per constructor of the list `a` under
/// `a = []` and `a = [h1, ..t1]`. The kernel refuses it without a case,
/// on a term that is not the subject of `f`'s match, with a name that is
/// not fresh, or with a cell case that does not bind two names; and, for
/// a sum, with a constructor an arm reads left out, or with a case that
/// proves an equation about its own names.
#[test]
fn a_split_on_a_constructor_covers_the_type_with_fresh_names() {
    use super::{Def, SplitCase, SplitCtor};
    use crate::ir::hir::{ResolvedCallee, ResolvedCtor};
    use crate::ir::identity::{CtorId, FnId, TypeId};
    let zero = term::int(&0.into());
    let one = term::int(&1.into());
    let arm = |pattern: ResolvedPattern, body: super::Term| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let def = |arms: Vec<ResolvedMatchArm>| Def {
        fn_id: FnId(0),
        name: "__fn_0".into(),
        params: vec!["x".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("x")),
            arms,
        }),
    };
    let call = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(0)),
        vec![var("a")],
    ));
    let ge = |t: super::Term| term::binop(BinOp::Gte, t, zero.clone());
    // `f(a) >= 0` from arm `k` of `f` at `a`, chosen by `h`, giving `value`.
    let case = |k: u32, binders: Vec<&str>, value: &super::Term| Proof::Trans {
        terms: vec![ge(call.clone()), ge(value.clone()), term::boolean(true)],
        steps: vec![
            Proof::Congr {
                ctx: ge(term::hole()),
                inner: Box::new(Proof::Unfold {
                    fn_id: FnId(0),
                    arm: k,
                    args: vec![var("a")],
                    binders: binders.into_iter().map(var).collect(),
                    premise: Some(Box::new(Proof::Hyp("h".into()))),
                }),
            },
            Proof::Compute {
                lhs: ge(value.clone()),
                rhs: term::boolean(true),
            },
        ],
    };
    let split = |on: super::Term, cases: Vec<SplitCase>| Proof::Split {
        fn_id: FnId(0),
        args: vec![var("a")],
        on,
        hyp: "h".into(),
        cases,
    };
    let nil = SplitCase {
        ctor: SplitCtor::Nil,
        binders: Vec::new(),
        proof: case(1, vec![], &zero),
    };
    let cons = |h: &str| SplitCase {
        ctor: SplitCtor::Cons,
        binders: vec![h.into(), "t1".into()],
        proof: case(2, vec![h, "t1"], &one),
    };
    let list = def(vec![
        arm(ResolvedPattern::EmptyList, zero.clone()),
        arm(ResolvedPattern::Cons("h".into(), "t".into()), one.clone()),
    ]);
    // `C.A` and `C.B(n)`, as the program declares the sum.
    let ctor = |k: u32, name: &str| ResolvedCtor::User {
        ctor_id: CtorId(k),
        type_id: TypeId(0),
        name: name.into(),
    };
    let declared = vec![vec![(ctor(0, "C.A"), 0), (ctor(1, "C.B"), 1)]];
    let run = |proof: Proof, d: Def| {
        let mut s = script(ge(call.clone()), term::boolean(true), proof);
        s.defs = vec![d];
        s.sums = declared.clone();
        kernel(&s)
    };
    assert_eq!(
        run(split(var("a"), vec![nil.clone(), cons("h1")]), list.clone()),
        Ok(())
    );
    assert!(
        run(split(var("a"), vec![nil.clone()]), list.clone()).is_err(),
        "a case left out"
    );
    assert!(
        run(split(var("b"), vec![nil.clone(), cons("h1")]), list.clone()).is_err(),
        "not the subject"
    );
    assert!(
        run(split(var("a"), vec![nil.clone(), cons("a")]), list.clone()).is_err(),
        "a name not fresh"
    );
    let short = SplitCase {
        binders: vec!["h1".into()],
        ..cons("h1")
    };
    assert!(
        run(split(var("a"), vec![nil.clone(), short]), list.clone()).is_err(),
        "one name for a cell"
    );

    // A sum: `C.A` and `C.B(n)`.
    let sum = def(vec![
        arm(
            ResolvedPattern::Ctor(ctor(0, "C.A"), Vec::new()),
            zero.clone(),
        ),
        arm(
            ResolvedPattern::Ctor(ctor(1, "C.B"), vec!["n".into()]),
            one.clone(),
        ),
    ]);
    let a = SplitCase {
        ctor: SplitCtor::Ctor(ctor(0, "C.A")),
        binders: Vec::new(),
        proof: case(1, vec![], &zero),
    };
    let b = SplitCase {
        ctor: SplitCtor::Ctor(ctor(1, "C.B")),
        binders: vec!["n1".into()],
        proof: case(2, vec!["n1"], &one),
    };
    assert_eq!(
        run(split(var("a"), vec![a.clone(), b.clone()]), sum.clone()),
        Ok(())
    );
    assert!(
        run(split(var("a"), vec![a.clone()]), sum.clone()).is_err(),
        "C.B left out"
    );
    // Behind a catch-all arm, `f(x) = match x { C.A -> 0, _ -> 1 }`: the
    // split covers every constructor the program declares, not only the
    // ones an arm names, and not a sum the program does not declare.
    let behind = def(vec![
        arm(
            ResolvedPattern::Ctor(ctor(0, "C.A"), Vec::new()),
            zero.clone(),
        ),
        arm(ResolvedPattern::Wildcard, one.clone()),
    ]);
    let b_value = Spanned::bare(ResolvedExpr::Ctor(ctor(1, "C.B"), vec![var("n1")]));
    let b_other = SplitCase {
        proof: case(2, vec![], &one),
        ..b.clone()
    };
    let b_other = SplitCase {
        proof: match b_other.proof {
            Proof::Trans { terms, mut steps } => {
                if let Proof::Congr { inner, .. } = &mut steps[0]
                    && let Proof::Unfold { binders, .. } = inner.as_mut()
                {
                    *binders = vec![b_value.clone()];
                }
                Proof::Trans { terms, steps }
            }
            other => other,
        },
        ..b_other
    };
    assert_eq!(
        run(
            split(var("a"), vec![a.clone(), b_other.clone()]),
            behind.clone()
        ),
        Ok(())
    );
    assert!(
        run(split(var("a"), vec![a.clone()]), behind.clone()).is_err(),
        "only the constructor an arm names"
    );
    let mut undeclared = script(
        ge(call.clone()),
        term::boolean(true),
        split(var("a"), vec![a.clone(), b_other.clone()]),
    );
    undeclared.defs = vec![behind.clone()];
    assert!(
        kernel(&undeclared).is_err(),
        "a sum the program does not declare"
    );
    let mut twice = undeclared.clone();
    twice.sums = vec![declared[0].clone(), vec![(ctor(0, "C.A"), 0)]];
    assert!(
        kernel(&twice).is_err(),
        "a constructor declared in two sums"
    );
    let about_own = |c: SplitCase| SplitCase {
        proof: Proof::Refl(var("n1")),
        ..c
    };
    let mut s = script(
        var("n1"),
        var("n1"),
        split(var("a"), vec![about_own(a), about_own(b)]),
    );
    s.defs = vec![sum];
    s.sums = declared.clone();
    assert!(kernel(&s).is_err(), "a case about its own names");
}

/// `f(a) >= 0` for `f(x) = match x { [] -> 0, [h, ..t] -> 1 }` by a split
/// on the list `a`, with both cases or with the cell case left out.
fn list_split(both: bool) -> Script {
    use super::{Def, SplitCase, SplitCtor};
    use crate::ir::hir::ResolvedCallee;
    use crate::ir::identity::FnId;
    let zero = term::int(&0.into());
    let one = term::int(&1.into());
    let arm = |pattern: ResolvedPattern, body: super::Term| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let call = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(0)),
        vec![var("a")],
    ));
    let ge = |t: super::Term| term::binop(BinOp::Gte, t, zero.clone());
    let case = |k: u32, binders: Vec<&str>, value: &super::Term| Proof::Trans {
        terms: vec![ge(call.clone()), ge(value.clone()), term::boolean(true)],
        steps: vec![
            Proof::Congr {
                ctx: ge(term::hole()),
                inner: Box::new(Proof::Unfold {
                    fn_id: FnId(0),
                    arm: k,
                    args: vec![var("a")],
                    binders: binders.into_iter().map(var).collect(),
                    premise: Some(Box::new(Proof::Hyp("h".into()))),
                }),
            },
            Proof::Compute {
                lhs: ge(value.clone()),
                rhs: term::boolean(true),
            },
        ],
    };
    let mut cases = vec![SplitCase {
        ctor: SplitCtor::Nil,
        binders: Vec::new(),
        proof: case(1, vec![], &zero),
    }];
    if both {
        cases.push(SplitCase {
            ctor: SplitCtor::Cons,
            binders: vec!["h1".into(), "t1".into()],
            proof: case(2, vec!["h1", "t1"], &one),
        });
    }
    let mut s = script(
        ge(call.clone()),
        term::boolean(true),
        Proof::Split {
            fn_id: FnId(0),
            args: vec![var("a")],
            on: var("a"),
            hyp: "h".into(),
            cases,
        },
    );
    s.defs = vec![Def {
        fn_id: FnId(0),
        name: "__fn_0".into(),
        params: vec!["x".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("x")),
            arms: vec![
                arm(ResolvedPattern::EmptyList, zero.clone()),
                arm(ResolvedPattern::Cons("h".into(), "t".into()), one.clone()),
            ],
        }),
    }];
    s
}

/// A split on the arms of a literal match: `f(a) >= 0` for `f(x) = match x
/// { 1 -> 0, 2 -> 5, _ -> 7 }`, one case per arm, under `a = 1`, `a = 2`,
/// and `(a == 1) = false`, `(a == 2) = false` for the catch-all. The
/// catch-all arm is chosen for `a` only where hypotheses rule out every
/// literal arm before it, from the split or from Bool splits on `a == k`;
/// the kernel refuses the split without the catch-all case or with one
/// hypothesis short, cases that are not the arms, and the catch-all arm
/// chosen with a literal arm not ruled out.
#[test]
fn a_split_on_literal_arms_has_one_case_per_arm() {
    use super::{Def, SplitCase, SplitCtor};
    use crate::ast::Literal;
    use crate::ir::hir::ResolvedCallee;
    use crate::ir::identity::FnId;
    let n = |k: i64| term::int(&k.into());
    let arm = |pattern: ResolvedPattern, body: super::Term| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let lit = |k: i64| ResolvedPattern::Literal(Literal::Int(k));
    let def = |arms: Vec<ResolvedMatchArm>| Def {
        fn_id: FnId(0),
        name: "__fn_0".into(),
        params: vec!["x".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("x")),
            arms,
        }),
    };
    let f = def(vec![
        arm(lit(1), n(0)),
        arm(lit(2), n(5)),
        arm(ResolvedPattern::Wildcard, n(7)),
    ]);
    let call = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(0)),
        vec![var("a")],
    ));
    let ge = |t: super::Term| term::binop(BinOp::Gte, t, n(0));
    // `f(a) >= 0` from arm `k` of `f` at `a`, with `binders` and `premise`.
    let case = |k: u32, binders: Vec<super::Term>, premise: Proof, value: i64| Proof::Trans {
        terms: vec![ge(call.clone()), ge(n(value)), term::boolean(true)],
        steps: vec![
            Proof::Congr {
                ctx: ge(term::hole()),
                inner: Box::new(Proof::Unfold {
                    fn_id: FnId(0),
                    arm: k,
                    args: vec![var("a")],
                    binders,
                    premise: Some(Box::new(premise)),
                }),
            },
            Proof::Compute {
                lhs: ge(n(value)),
                rhs: term::boolean(true),
            },
        ],
    };
    let other = case(3, vec![var("a")], Proof::Refl(var("a")), 7);
    let split = |cases: Vec<SplitCase>| Proof::Split {
        fn_id: FnId(0),
        args: vec![var("a")],
        on: var("a"),
        hyp: "h".into(),
        cases,
    };
    let one = SplitCase {
        ctor: SplitCtor::Lit(Literal::Int(1)),
        binders: Vec::new(),
        proof: case(1, vec![], Proof::Hyp("h".into()), 0),
    };
    let two = SplitCase {
        ctor: SplitCtor::Lit(Literal::Int(2)),
        binders: Vec::new(),
        proof: case(2, vec![], Proof::Hyp("h".into()), 5),
    };
    let rest = |names: Vec<&str>| SplitCase {
        ctor: SplitCtor::Other,
        binders: names.into_iter().map(String::from).collect(),
        proof: other.clone(),
    };
    let run = |proof: Proof, d: &Def| {
        let mut s = script(ge(call.clone()), term::boolean(true), proof);
        s.defs = vec![d.clone()];
        kernel(&s)
    };
    assert_eq!(
        run(
            split(vec![one.clone(), two.clone(), rest(vec!["n1", "n2"])]),
            &f
        ),
        Ok(())
    );
    assert!(
        run(split(vec![one.clone(), two.clone()]), &f).is_err(),
        "the catch-all case left out"
    );
    assert!(
        run(split(vec![one.clone(), two.clone(), rest(vec!["n1"])]), &f).is_err(),
        "one hypothesis short"
    );
    assert!(
        run(split(vec![one.clone(), rest(vec!["n1"])]), &f).is_err(),
        "a literal arm left out"
    );
    assert!(
        run(other.clone(), &f).is_err(),
        "the catch-all arm with no literal arm ruled out"
    );
    let no_catch_all = def(vec![arm(lit(1), n(0)), arm(lit(2), n(5))]);
    assert!(
        run(split(vec![one.clone(), two.clone()]), &no_catch_all).is_err(),
        "literal arms without a catch-all"
    );

    // The same from Bool splits on `a == 1` and `a == 2`: the catch-all arm
    // where both are false, a literal arm by `eq_of_beq` where one is true.
    let beq = |k: i64| term::binop(BinOp::Eq, var("a"), n(k));
    let equal = |k: i64, h: &str| Proof::Rule {
        rule: WallRule::EqOfBeq,
        subst: vec![("a".into(), var("a")), ("b".into(), n(k))],
        premises: vec![Proof::Hyp(h.into())],
    };
    let cases = |on: super::Term, h: &str, if_true: Proof, if_false: Proof| Proof::Cases {
        on,
        hyp: h.into(),
        if_true: Box::new(if_true),
        if_false: Box::new(if_false),
    };
    let by_bools = |inner_split: bool| {
        let after_one = if inner_split {
            cases(
                beq(2),
                "h2",
                case(2, vec![], equal(2, "h2"), 5),
                other.clone(),
            )
        } else {
            other.clone()
        };
        cases(beq(1), "h1", case(1, vec![], equal(1, "h1"), 0), after_one)
    };
    assert_eq!(run(by_bools(true), &f), Ok(()));
    assert!(
        run(by_bools(false), &f).is_err(),
        "the catch-all arm with the literal 2 not ruled out"
    );
}

/// A constructor pattern with a wildcard field: arm `C.B(_, m)` of `g(x) =
/// match x { C.A -> 0, C.B(_, m) -> m }` at `C.B(p, q)` gives `q`, the
/// field the name stands at, not the first one.
#[test]
fn a_wildcard_field_takes_its_place_among_the_binders() {
    use super::Def;
    use crate::ir::hir::{ResolvedCallee, ResolvedCtor};
    use crate::ir::identity::{CtorId, FnId, TypeId};
    let ctor = |k: u32, name: &str| ResolvedCtor::User {
        ctor_id: CtorId(k),
        type_id: TypeId(0),
        name: name.into(),
    };
    let arm = |pattern: ResolvedPattern, body: super::Term| ResolvedMatchArm {
        pattern,
        body: Box::new(body),
        binding_slots: std::sync::OnceLock::new(),
    };
    let g = Def {
        fn_id: FnId(0),
        name: "__fn_0".into(),
        params: vec!["x".into()],
        returns_bool: false,
        lets: Vec::new(),
        body: Spanned::bare(ResolvedExpr::Match {
            subject: Box::new(var("x")),
            arms: vec![
                arm(
                    ResolvedPattern::Ctor(ctor(0, "C.A"), Vec::new()),
                    term::int(&0.into()),
                ),
                arm(
                    ResolvedPattern::Ctor(ctor(1, "C.B"), vec!["_".into(), "m".into()]),
                    var("m"),
                ),
            ],
        }),
    };
    let value = Spanned::bare(ResolvedExpr::Ctor(ctor(1, "C.B"), vec![var("a"), var("b")]));
    let call = Spanned::bare(ResolvedExpr::Call(
        ResolvedCallee::Fn(FnId(0)),
        vec![value.clone()],
    ));
    let unfold = Proof::Unfold {
        fn_id: FnId(0),
        arm: 2,
        args: vec![value.clone()],
        binders: vec![var("a"), var("b")],
        premise: Some(Box::new(Proof::Refl(value.clone()))),
    };
    let run = |rhs: super::Term| {
        let mut s = script(call.clone(), rhs, unfold.clone());
        s.defs = vec![g.clone()];
        kernel(&s)
    };
    assert_eq!(run(var("b")), Ok(()));
    assert!(run(var("a")).is_err(), "the wildcard's field");
}
