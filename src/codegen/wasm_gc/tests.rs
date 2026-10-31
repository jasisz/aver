//! Phase-1 sanity tests. Each later phase adds its own block here.

use crate::source::parse_source;

use super::compile_to_wasm_gc;

fn parse(src: &str) -> Vec<crate::ast::TopLevel> {
    parse_source(src).expect("parse failed")
}

#[test]
fn phase1_int_literal_main_emits_valid_module() {
    // Step 3 (typed-ABI refactor): codegen reads `Spanned::ty()` for
    // every node, so the pipeline (typecheck + resolve) must run
    // before the codegen entry — even for the simplest module. This
    // mirrors what every real caller does (`aver compile`,
    // playground, bench harness).
    let mut items = parse(
        r#"
module Hello
    intent = "smoke"
    depends []

fn main() -> Int
    42
"#,
    );
    use crate::ir::{PipelineConfig, TypecheckMode};
    let _result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let bytes = compile_to_wasm_gc(&items, None).expect("compile");
    assert!(!bytes.is_empty(), "module bytes should be non-empty");
    // Module starts with the standard wasm magic + version.
    assert_eq!(&bytes[0..4], b"\0asm");
    assert_eq!(&bytes[4..8], &[1, 0, 0, 0]);
}

#[test]
fn phase2_int_arithmetic_compiles() {
    // Phase 2 covers Int arithmetic, fn calls, parameters. The pipeline
    // (typecheck + resolve) runs before codegen so the items are
    // post-Resolve when they reach `compile_to_wasm_gc`. Bench scenarios
    // hit this path; we don't run the full pipeline here, just verify
    // the codegen entry accepts a reasonably-shaped program.
    let mut items = parse(
        r#"
module Arith
    intent = "phase 2 — Int arithmetic + comparison"
    depends []

fn add(a: Int, b: Int) -> Int
    a + b

fn main() -> Int
    add(7, 35)
"#,
    );
    // Run minimal pipeline: typecheck + resolve so Idents become
    // `Resolved { slot }` (which body.rs assumes).
    use crate::ir::{PipelineConfig, TypecheckMode};
    let _result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let bytes = compile_to_wasm_gc(&items, None).expect("compile");
    assert!(!bytes.is_empty());
}

#[test]
fn phase4_match_and_tail_call_compile() {
    // Phase 4: match dispatch + native return_call. Same pipeline
    // dance as phase 2 — typecheck + resolve required.
    let mut items = parse(
        r#"
module Fact
    intent = "phase 4 — match + tail recursion"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(10, 1)
"#,
    );
    use crate::ir::{PipelineConfig, TypecheckMode};
    let _result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let bytes = compile_to_wasm_gc(&items, None).expect("compile");
    assert!(!bytes.is_empty());
}

#[test]
fn pooled_int_literals_are_not_built_by_global_initializers() {
    // Wasmtime compiles every global initializer that allocates into one
    // module-init function, and Cranelift's cost on it grows faster than
    // the number of allocations: `aver verify --wasm-gc` with a few hundred
    // cases spent most of its time there. The pooled `$AverInt` literals
    // start null and are made by their first read instead.
    let mut items = parse(
        r#"
module Pool
    intent = "pooled Int literals"
    depends []

fn shifted(n: Int) -> Int
    n * 1000003 + 77

fn main() -> Int
    shifted(5) + 11
"#,
    );
    use crate::ir::{PipelineConfig, TypecheckMode};
    let _result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let bytes = compile_to_wasm_gc(&items, None).expect("compile");
    let wat = wasmprinter::print_bytes(&bytes).expect("print");
    let globals: Vec<&str> = wat
        .lines()
        .filter(|line| line.trim_start().starts_with("(global "))
        .collect();
    assert!(
        globals
            .iter()
            .any(|g| g.contains("(mut (ref null") && g.contains("ref.null")),
        "expected a pooled literal global starting null:\n{}",
        globals.join("\n")
    );
    assert!(
        !globals
            .iter()
            .any(|g| g.contains("i64.const") && g.contains("struct.new")),
        "a global initializer still boxes an Int literal:\n{}",
        globals.join("\n")
    );
}
