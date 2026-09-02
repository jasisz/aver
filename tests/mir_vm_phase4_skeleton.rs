//! The VM's MIR walker has no fallback: a shape it cannot emit is a hard
//! compile error, not a quiet detour to another walker. So the question
//! these fixtures ask is simply whether each shape compiles.
//!
//! They were written when the walker covered a subset and a coverage
//! classifier counted which fns still needed the HIR walker. That walker
//! is gone, the classifier with it, and each fixture below is one shape
//! whose arrival in the walker the classifier used to record: plain
//! arithmetic, a user-fn call, a nested structural subpattern inside a
//! tuple, and a builtin call.
//!
//! Opcode selection is pinned in `tests/mir_vm_codegen.rs`; runtime
//! results in `tests/hir_mir_differential.rs`.

use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::source::parse_source;
use aver::vm;

/// Compile `source` the way production does. Panics with the compiler's
/// own message when the walker refuses a shape.
fn compile(source: &str) -> aver::vm::CodeStore {
    let mut items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e}"));
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, _) = vm::compile_program(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        None,
    )
    .unwrap_or_else(|e| panic!("compile: {}", e.msg));
    code
}

/// Every fn in `source` has a compiled chunk.
fn assert_every_fn_compiles(source: &str, names: &[&str]) {
    let code = compile(source);
    for name in names {
        assert!(code.find(name).is_some(), "`{name}` has no compiled chunk");
    }
}

#[test]
fn arithmetic_over_locals_compiles() {
    // `fn double(x: Int) -> Int = x + x`: one BinOp, two Locals.
    assert_every_fn_compiles("fn double(x: Int) -> Int\n    x + x\n", &["double"]);
}

#[test]
fn a_user_fn_call_compiles() {
    assert_every_fn_compiles(
        "fn double(x: Int) -> Int\n    x + x\n\nfn quad(y: Int) -> Int\n    double(y + y)\n",
        &["double", "quad"],
    );
}

#[test]
fn a_nested_structural_subpattern_inside_a_tuple_compiles() {
    assert_every_fn_compiles(
        "fn double(x: Int) -> Int\n    x + x\n\nfn classify(p: Tuple<List<Int>, Int>) -> Int\n    match p\n        ([], _) -> 0\n        _ -> 1\n",
        &["double", "classify"],
    );
}

#[test]
fn a_builtin_call_compiles() {
    assert_every_fn_compiles(
        "fn print_hello() -> Int\n    ! [Console.print]\n    Console.print(\"hello\")\n    0\n",
        &["print_hello"],
    );
}
