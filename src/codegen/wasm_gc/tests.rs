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
fn phase1_rejects_main_with_params() {
    let items = parse(
        r#"
module Hello
    intent = "smoke"
    depends []

fn main(n: Int) -> Int
    n
"#,
    );
    let err = compile_to_wasm_gc(&items, None).unwrap_err();
    assert!(
        format!("{err}").contains("phase 1"),
        "expected phase-1 limit error, got: {err}"
    );
}

#[test]
fn phase1_rejects_non_int_literal_main() {
    // Phase 1 doesn't lower expressions — even simple arithmetic gets
    // bounced. This locks the scope so adding an emitter for `+` is
    // a deliberate phase-2 bump, not an accidental drift.
    let items = parse(
        r#"
module Hello
    intent = "smoke"
    depends []

fn main() -> Int
    1 + 1
"#,
    );
    let err = compile_to_wasm_gc(&items, None).unwrap_err();
    assert!(
        format!("{err}").contains("phase 1"),
        "expected phase-1 limit error, got: {err}"
    );
}
