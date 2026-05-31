//! Phase 2a of #252 — pin the Core MIR data model.
//!
//! No HIR → MIR lowering yet; Phase 3 lands that in waves. These
//! tests construct tiny `MirProgram` values by hand to:
//!
//! 1. lock the public API in (so a Phase 2b dump / Phase 3 lowering
//!    PR can't quietly rename or move things), and
//! 2. prove the design decisions pinned in `src/ir/mir/RFC.md`
//!    survive contact with real Rust types — `Try` is its own
//!    variant, `Match` carries `MirMatchArm`s, constructors use
//!    `CtorId`, locals use `LocalId`.

use aver::ast::{BinOp, Literal, Spanned};
use aver::ir::mir::{
    LocalId, MirBinOp, MirCall, MirCallee, MirCtor, MirEffectAnnotation, MirExpr, MirFn, MirLet,
    MirLocal, MirMatch, MirMatchArm, MirParam, MirPattern, MirProgram,
};
use aver::ir::{CtorId, FnId};

fn span<T>(node: T) -> Spanned<T> {
    Spanned {
        node,
        line: 0,
        ty: std::sync::OnceLock::new(),
    }
}

fn fn_id(n: u32) -> FnId {
    // `FnId(pub u32)` — the public field is the test-construction
    // hook for now. Phase 3 lowering populates ids via
    // `SymbolTable`; consumers in production code never construct
    // raw ids.
    FnId(n)
}

fn ctor_id(n: u32) -> CtorId {
    CtorId(n)
}

#[test]
fn empty_program_round_trips() {
    let program = MirProgram::empty();
    assert!(program.fns.is_empty());
    assert!(program.modules.is_empty());
    assert!(program.iter().next().is_none());
    assert!(program.fn_by_id(fn_id(0)).is_none());
}

#[test]
fn pinned_decision_try_is_its_own_variant() {
    // RFC pin: `Try` lives as `MirExpr::Try` — not as `Match`
    // sugar. If a future refactor desugars `?` into `Match`, this
    // test stops compiling (variant gone) or stops type-checking
    // (signature changed). Either way the design intent shows up
    // in CI before review.
    let value = Box::new(span(MirExpr::Literal(span(Literal::Int(7)))));
    let try_expr = MirExpr::Try(value);
    match try_expr {
        MirExpr::Try(_) => {}
        _ => panic!("Try must not be desugared at the MirExpr level"),
    }
}

#[test]
fn pinned_decision_match_is_structured() {
    // RFC pin: `Match { subject, arms: Vec<MirMatchArm> }`. No
    // basic-block jumps, no terminator-style switch.
    let subject = Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0))))));
    let arms = vec![
        MirMatchArm {
            pattern: MirPattern::EmptyList,
            body: span(MirExpr::Literal(span(Literal::Int(0)))),
        },
        MirMatchArm {
            pattern: MirPattern::Cons {
                head: LocalId(1),
                head_name: String::new(),
                tail: LocalId(2),
                tail_name: String::new(),
            },
            body: span(MirExpr::Local(span(MirLocal::at(LocalId(1))))),
        },
    ];
    let m = MirExpr::Match(span(MirMatch { subject, arms }));
    let MirExpr::Match(spanned_match) = m else {
        panic!("expected Match variant");
    };
    assert_eq!(spanned_match.node.arms.len(), 2);
    assert!(matches!(
        &spanned_match.node.arms[0].pattern,
        MirPattern::EmptyList
    ));
    assert!(matches!(
        &spanned_match.node.arms[1].pattern,
        MirPattern::Cons { .. }
    ));
}

#[test]
fn pinned_decision_identity_is_typed() {
    // RFC pin: declaration refs go through `FnId` / `CtorId` /
    // `TypeId`, never strings. `MirCallee::Fn` carries `FnId`,
    // constructor patterns carry `CtorId`.
    let call = MirExpr::Call(span(MirCall {
        callee: MirCallee::Fn(fn_id(42)),
        args: vec![span(MirExpr::Literal(span(Literal::Int(1))))],
    }));
    let MirExpr::Call(spanned_call) = call else {
        unreachable!()
    };
    let MirCallee::Fn(callee_id) = spanned_call.node.callee else {
        panic!("Fn callee must carry FnId, not String");
    };
    assert_eq!(callee_id, fn_id(42));

    let ctor_pattern = MirPattern::Ctor {
        ctor: MirCtor::User(ctor_id(7)),
        bindings: vec![LocalId(0)],
        binding_names: vec![String::new()],
    };
    let MirPattern::Ctor { ctor, bindings, .. } = ctor_pattern else {
        unreachable!()
    };
    assert_eq!(ctor, MirCtor::User(ctor_id(7)));
    assert_eq!(bindings, vec![LocalId(0)]);
}

#[test]
fn try_bind_is_let_with_try_value() {
    // `let x = step()?; body` is expressed as composition:
    //   Let { binding: x, value: Try(step()), body }
    // The original design had a dedicated `TryBind` variant; we
    // dropped it because `Let { value: Try(_), ... }` already
    // captures the same semantics, with no walker complexity
    // penalty. This test pins the composition contract.
    let step_call = MirExpr::Call(span(MirCall {
        callee: MirCallee::Fn(fn_id(1)),
        args: vec![],
    }));
    let bind = MirExpr::Let(span(MirLet {
        binding: LocalId(0),
        value: Box::new(span(MirExpr::Try(Box::new(span(step_call))))),
        body: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
    }));
    let MirExpr::Let(let_node) = bind else {
        panic!("expected Let composition");
    };
    assert_eq!(let_node.node.binding, LocalId(0));
    // The Let's value side must hold the Try wrapping the step call.
    assert!(
        matches!(&let_node.node.value.node, MirExpr::Try(_)),
        "Let value must be Try(_) for ?-bind composition; got {:?}",
        let_node.node.value.node
    );
}

#[test]
fn build_a_tiny_function_by_hand() {
    // Smoke-test the whole shape: a one-param int-doubling fn.
    //
    //     fn double(x: Int) -> Int :- x + x
    //
    // Phase 3 lowering would produce this from `ResolvedFnDef`;
    // here we build it manually so any rename / move on the
    // public API breaks this test.
    let x = LocalId(0);
    let body = span(MirExpr::BinOp(span(MirBinOp {
        op: BinOp::Add,
        lhs: Box::new(span(MirExpr::Local(span(MirLocal::at(x))))),
        rhs: Box::new(span(MirExpr::Local(span(MirLocal::at(x))))),
    })));
    let f = MirFn {
        fn_id: fn_id(0),
        name: "double".to_string(),
        params: vec![MirParam {
            local: x,
            name: "x".to_string(),
            ty: "Int".to_string(),
        }],
        return_type: "Int".to_string(),
        effects: vec![MirEffectAnnotation {
            name: "Console.print".to_string(),
        }],
        body,
    };
    let mut program = MirProgram::empty();
    program.fns.insert(f.fn_id, f);
    assert_eq!(program.fns.len(), 1);
    assert_eq!(
        program.fn_by_id(fn_id(0)).map(|f| f.name.as_str()),
        Some("double")
    );
}
