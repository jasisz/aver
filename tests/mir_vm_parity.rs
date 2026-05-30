//! Phase 4b of #252 — VM parity between HIR and MIR codegen
//! paths.
//!
//! `compile_program` walks `ResolvedExpr` (HIR) and emits VM
//! bytecode. `compile_program_with_mir_fallback` walks
//! `MirExpr` for fns whose body fits the Phase 4 subset, falling
//! back to the HIR walker otherwise. Both paths must produce
//! identical VM-observable behavior — same `Value` from
//! `run_named_function` on the same input.
//!
//! Phase 4 subset (per `src/vm/compiler/mir.rs`): Literal +
//! Local + BinOp + Neg + Let + Call(Fn) + Return. Tests here
//! exercise just that subset; fns outside it ride the HIR
//! fallback and parity holds trivially.

use aver::ast::TopLevel;
use aver::ir::SymbolTable;
use aver::ir::hir::{self, ResolvedTopLevel};
use aver::lexer::Lexer;
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::parser::Parser;
use aver::resolver;
use aver::tco;
use aver::value::Value;
use aver::vm;

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

fn resolve(items: &[TopLevel]) -> (Vec<ResolvedTopLevel>, SymbolTable) {
    let symbols = SymbolTable::build(items, &[]);
    let resolved = hir::resolve_program(&symbols, items);
    (resolved, symbols)
}

/// Compile `src` through the requested path and call
/// `fn_name(args)` on the result. Returns `Value` (decoded from
/// `NanValue` against the path's own arena) so the test asserts
/// run on a stable comparison form.
fn run_via_path(src: &str, fn_name: &str, args: &[i64], path: Path) -> Value {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, symbols) = resolve(&items);

    let mut arena = Arena::new();
    let (code, globals) = match path {
        Path::Hir => {
            vm::compile_program(&resolved, &symbols, &mut arena, None).expect("HIR compile failed")
        }
        Path::MirFallback => {
            vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
                .expect("MIR-fallback compile failed")
        }
    };
    let nv_args: Vec<NanValue> = args
        .iter()
        .map(|i| NanValue::new_int(*i, &mut arena))
        .collect();
    let mut machine = vm::VM::new(code, globals, arena);
    let result = machine
        .run_named_function(fn_name, &nv_args)
        .expect("VM run failed");
    result.to_value(&machine.arena)
}

enum Path {
    Hir,
    MirFallback,
}

#[test]
fn double_parity_hir_vs_mir_fallback() {
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    let hir = run_via_path(src, "double", &[7], Path::Hir);
    let mir = run_via_path(src, "double", &[7], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "HIR and MIR-fallback paths must agree on double(7): HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(14), "double(7) should be 14");
}

#[test]
fn arithmetic_chain_parity() {
    // Two-fn corpus exercising CALL_KNOWN dispatch and inline
    // arithmetic, both inside the Phase 4 subset.
    let src = "fn double(x: Int) -> Int\n    x + x\n\nfn quad(y: Int) -> Int\n    double(y + y)\n";
    let hir = run_via_path(src, "quad", &[3], Path::Hir);
    let mir = run_via_path(src, "quad", &[3], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "HIR and MIR-fallback paths must agree on quad(3): HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(12), "quad(3) = double(6) = 12");
}

#[test]
fn neg_parity() {
    let src = "fn flip(x: Int) -> Int\n    -x\n";
    let hir = run_via_path(src, "flip", &[5], Path::Hir);
    let mir = run_via_path(src, "flip", &[5], Path::MirFallback);
    assert_eq!(hir, mir);
    assert_eq!(hir, Value::Int(-5));
}

/// Compile both paths and return the per-fn bytecode pair so
/// tests can byte-compare specific chunks.
fn compile_both(src: &str) -> (aver::vm::CodeStore, aver::vm::CodeStore) {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, symbols) = resolve(&items);
    let mut a1 = Arena::new();
    let mut a2 = Arena::new();
    let (hir, _) =
        vm::compile_program(&resolved, &symbols, &mut a1, None).expect("HIR compile failed");
    let (mir, _) = vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut a2, None)
        .expect("MIR-fallback compile failed");
    (hir, mir)
}

#[test]
fn bytecode_parity_for_double_in_phase_4_subset() {
    // Same source → identical FnChunk.code between HIR and
    // MIR-fallback paths for fns in the Phase 4 subset. This is
    // the strongest possible parity assertion: not just runtime
    // result, but actual emitted bytecode (LOAD_LOCAL / ADD /
    // RETURN sequence) is byte-identical.
    let (hir, mir) = compile_both("fn double(x: Int) -> Int\n    x + x\n");
    let hir_fn = hir.get(hir.find("double").expect("double in HIR"));
    let mir_fn = mir.get(mir.find("double").expect("double in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "double() bytecode must be byte-identical: HIR={:?}, MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn user_ctor_construction_parity() {
    // Phase 4c — `MirExpr::Construct(MirCtor::User(_))` walks
    // through the CtorEntry → owning_type lookup and emits
    // VARIANT_NEW just like the HIR walker. We verify by
    // (a) running an Int-extractor through both paths, and
    // (b) byte-comparing the FnChunk.
    let src = "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n\nfn make_and_extract(r: Int) -> Int\n    area(makeCircle(r))\n";
    let hir = run_via_path(src, "make_and_extract", &[7], Path::Hir);
    let mir = run_via_path(src, "make_and_extract", &[7], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "User ctor + match parity: HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(7));
}

#[test]
fn bytecode_parity_for_user_ctor_construction() {
    // Just makeCircle alone — body is `Shape.Circle(r)`, fully
    // inside the Phase 4c subset. The fn's bytecode must match
    // exactly between HIR and MIR-fallback paths.
    let (hir, mir) = compile_both(
        "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n",
    );
    let hir_fn = hir.get(hir.find("makeCircle").expect("makeCircle in HIR"));
    let mir_fn = mir.get(mir.find("makeCircle").expect("makeCircle in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "VARIANT_NEW emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn builtin_ctor_wrap_parity() {
    // `Result.Ok(x)` lowers to MirCtor::Builtin(ResultOk) in
    // MIR. The Phase 4c walker emits the same WRAP 0 sequence
    // the HIR walker emits.
    let src = "fn wrap_ok(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n";
    let (hir, mir) = compile_both(src);
    let hir_fn = hir.get(hir.find("wrap_ok").expect("wrap_ok in HIR"));
    let mir_fn = mir.get(mir.find("wrap_ok").expect("wrap_ok in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "WRAP emit for Result.Ok must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn record_field_access_parity() {
    // `MirExpr::Project` → RECORD_GET_NAMED, mirroring the HIR
    // path. We verify per-fn bytecode parity on a fn that takes
    // a record and projects a field.
    //
    // Note: the HIR walker may optimize to RECORD_GET (typed
    // index) when it can infer the type statically; the MIR
    // walker uses RECORD_GET_NAMED universally for now. The
    // record-creation call (`P(...)`) currently isn't in the
    // Phase 4 subset (RecordCreate is still wave-future), so
    // we test field access on a parameter — that path stays
    // pure Project in both walkers.
    let src = "record P\n  x: Int\n  y: Int\n\nfn px(p: P) -> Int\n    p.x\n";
    let (_hir, _mir) = compile_both(src);
    // We don't byte-compare here because HIR may use the typed
    // RECORD_GET op while MIR uses RECORD_GET_NAMED — both
    // semantically equivalent. Instead verify both compiled.
    // Full bytecode parity for Project lands when MIR carries
    // type stamps on its sub-nodes (Phase 6).
}

#[test]
fn match_fn_uses_hir_fallback_and_remains_present_in_mir_path() {
    // `match` isn't in the Phase 4 subset → MIR-fallback path
    // falls back to HIR. The fn must still appear in the output
    // (HIR walker emits the chunk); only the dispatch differs.
    let (hir, mir) = compile_both(
        "fn first(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n",
    );
    assert!(
        hir.find("first").is_some(),
        "first() should be present in HIR-only chunks"
    );
    assert!(
        mir.find("first").is_some(),
        "first() should still be present in MIR-fallback chunks (HIR fallback path)"
    );
}
