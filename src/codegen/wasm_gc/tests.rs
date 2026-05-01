//! Phase-1 sanity tests. Each later phase adds its own block here.

use crate::source::parse_source;

use super::compile_to_wasm_gc;

fn parse(src: &str) -> Vec<crate::ast::TopLevel> {
    parse_source(src).expect("parse failed")
}

#[test]
fn phase1_int_literal_main_emits_valid_module() {
    let items = parse(
        r#"
module Hello
    intent = "smoke"
    depends []

fn main() -> Int
    42
"#,
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
