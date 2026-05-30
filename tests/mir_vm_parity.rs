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
