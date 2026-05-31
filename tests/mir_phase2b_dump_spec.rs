//! Phase 2b of #252 — pin the Core MIR textual dump.
//!
//! Phase 3 will land HIR → MIR lowering and these fixtures will
//! get replaced by real-program snapshots driven through
//! `aver compile --emit-ir-after=mir`. Until then, we hand-
//! construct small `MirProgram`s + assert their `Display` output.
//! The point isn't to validate lowering; it's to lock the dump
//! format so future passes don't quietly reshape it.

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
    FnId(n)
}

#[test]
fn empty_program_dumps_skeleton() {
    let p = MirProgram::empty();
    let dump = format!("{p}");
    assert_eq!(dump, "MirProgram {\n}\n");
}

#[test]
fn one_fn_dump_pins_skeleton() {
    // fn double(x: Int) -> Int = (x + x)
    let x = LocalId(0);
    let body = span(MirExpr::BinOp(span(MirBinOp {
        op: BinOp::Add,
        lhs: Box::new(span(MirExpr::Local(span(MirLocal::at(x))))),
        rhs: Box::new(span(MirExpr::Local(span(MirLocal::at(x))))),
    })));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "double".to_string(),
            params: vec![MirParam {
                local: x,
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body,
            local_count: 0,
        },
    );

    let dump = format!("{program}");
    let expected = "\
MirProgram {
  // FnId(0)
  fn double(%0x: Int) -> Int {
    (%0 Add %0)
  }
}
";
    assert_eq!(
        dump, expected,
        "MirProgram dump drift:\n--- expected ---\n{expected}\n--- got ---\n{dump}"
    );
}

#[test]
fn try_bind_via_let_composition_dump_shape() {
    // `let x = step()?; x` is expressed as `Let { binding: x,
    // value: Try(step()), body: x }`. The original design had a
    // dedicated `TryBind` variant; we dropped it because the
    // composition already captures the same semantics, with the
    // same dump appearance (one `let`, one `?`).
    let body = span(MirExpr::Let(span(MirLet {
        binding: LocalId(0),
        binding_name: String::new(),
        value: Box::new(span(MirExpr::Try(Box::new(span(MirExpr::Call(span(
            MirCall {
                callee: MirCallee::Fn(fn_id(1)),
                args: vec![],
            },
        ))))))),
        body: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
    })));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "use_step".to_string(),
            params: vec![],
            return_type: "Result<Int, String>".to_string(),
            effects: vec![],
            body,
            local_count: 0,
        },
    );
    let dump = format!("{program}");
    assert!(
        dump.contains("fn use_step()"),
        "dump missing fn header:\n{dump}"
    );
    assert!(
        dump.contains("let %0 ="),
        "dump missing Let header:\n{dump}"
    );
    assert!(
        dump.contains("FnId(1).call()?"),
        "dump missing FnId-typed callee + `?`:\n{dump}"
    );
}

#[test]
fn match_dump_shape() {
    // fn first(xs: List<Int>) -> Int = match xs { [] => 0; [h, ..t] => h; }
    let xs = LocalId(0);
    let h = LocalId(1);
    let t = LocalId(2);
    let body = span(MirExpr::Match(span(MirMatch {
        subject: Box::new(span(MirExpr::Local(span(MirLocal::at(xs))))),
        arms: vec![
            MirMatchArm {
                pattern: MirPattern::EmptyList,
                body: span(MirExpr::Literal(span(Literal::Int(0)))),
            },
            MirMatchArm {
                pattern: MirPattern::Cons {
                    head: h,
                    head_name: String::new(),
                    tail: t,
                    tail_name: String::new(),
                },
                body: span(MirExpr::Local(span(MirLocal::at(h)))),
            },
        ],
    })));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "first".to_string(),
            params: vec![MirParam {
                local: xs,
                name: "xs".to_string(),
                ty: "List<Int>".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body,
            local_count: 0,
        },
    );
    let dump = format!("{program}");
    assert!(
        dump.contains("match %0"),
        "match subject didn't render:\n{dump}"
    );
    assert!(
        dump.contains("[] =>"),
        "EmptyList arm didn't render:\n{dump}"
    );
    assert!(
        dump.contains("[%1, ..%2] =>"),
        "Cons arm didn't render:\n{dump}"
    );
}

#[test]
fn ctor_pattern_dump_uses_ctor_id() {
    // pattern: Module.Variant(b1) — make sure CtorId appears (not
    // a string name) so reviewers can confirm identity wiring.
    let p = MirPattern::Ctor {
        ctor: MirCtor::User(CtorId(7)),
        bindings: vec![LocalId(0)],
        binding_names: vec![String::new()],
    };
    // Use the arm-rendering path via a tiny Match.
    let body = span(MirExpr::Match(span(MirMatch {
        subject: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(99)))))),
        arms: vec![MirMatchArm {
            pattern: p,
            body: span(MirExpr::Local(span(MirLocal::at(LocalId(0))))),
        }],
    })));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "match_ctor".to_string(),
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            body,
            local_count: 0,
        },
    );
    let dump = format!("{program}");
    assert!(
        dump.contains("CtorId(7)(%0)"),
        "ctor pattern should render the typed identity, not a string:\n{dump}"
    );
}

#[test]
fn effects_render_on_fn_header() {
    let body = span(MirExpr::Literal(span(Literal::Int(0))));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "noisy".to_string(),
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![
                MirEffectAnnotation {
                    name: "Console.print".to_string(),
                },
                MirEffectAnnotation {
                    name: "Disk.readText".to_string(),
                },
            ],
            body,
            local_count: 0,
        },
    );
    let dump = format!("{program}");
    assert!(
        dump.contains("effects: [Console.print, Disk.readText]"),
        "effects list missing or reordered:\n{dump}"
    );
}

#[test]
fn fns_render_in_fn_id_order() {
    // Insert in reverse order; assert dump walks them ascending
    // by FnId. Snapshot stability across HashMap iteration drift
    // depends on this.
    let mut program = MirProgram::empty();
    for raw in [3u32, 1, 2, 0] {
        program.fns.insert(
            fn_id(raw),
            MirFn {
                fn_id: fn_id(raw),
                name: format!("f{raw}"),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(MirExpr::Literal(span(Literal::Int(raw as i64)))),
                local_count: 0,
            },
        );
    }
    let dump = format!("{program}");
    let pos = |needle: &str| dump.find(needle).expect(needle);
    assert!(
        pos("// FnId(0)") < pos("// FnId(1)")
            && pos("// FnId(1)") < pos("// FnId(2)")
            && pos("// FnId(2)") < pos("// FnId(3)"),
        "fns not rendered in ascending FnId order:\n{dump}"
    );
}

#[test]
fn let_dump_shape() {
    // let %0 = 7; %0
    let body = span(MirExpr::Let(span(MirLet {
        binding: LocalId(0),
        binding_name: String::new(),
        value: Box::new(span(MirExpr::Literal(span(Literal::Int(7))))),
        body: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
    })));
    let mut program = MirProgram::empty();
    program.fns.insert(
        fn_id(0),
        MirFn {
            fn_id: fn_id(0),
            name: "seven".to_string(),
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            body,
            local_count: 0,
        },
    );
    let dump = format!("{program}");
    assert!(dump.contains("let %0 ="), "Let header missing:\n{dump}");
    assert!(
        dump.contains("in %0"),
        "Let body continuation missing:\n{dump}"
    );
}
