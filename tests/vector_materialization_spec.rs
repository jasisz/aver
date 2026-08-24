//! Cross-layer guards for the `Vector.new` materialization policy.
//!
//! Runtime boundary behavior lives in `eval_spec`; literal typing lives in
//! `typechecker_spec`; wasm-gc executes the exact boundary in `wasm_gc_spec`.
//! This file pins the architectural invariant between them: emitters may
//! render the shared number, but none may own a duplicate numeric policy.

#[test]
fn vector_emitters_do_not_duplicate_the_materialization_number() {
    let decimal = aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS.to_string();
    let sources = [
        ("src/ast/mod.rs", include_str!("../src/ast/mod.rs")),
        (
            "src/types/vector.rs",
            include_str!("../src/types/vector.rs"),
        ),
        (
            "src/vm/execute/dispatch.rs",
            include_str!("../src/vm/execute/dispatch.rs"),
        ),
        (
            "src/codegen/rust/from_mir.rs",
            include_str!("../src/codegen/rust/from_mir.rs"),
        ),
        (
            "src/codegen/lean/builtins.rs",
            include_str!("../src/codegen/lean/builtins.rs"),
        ),
        (
            "src/codegen/dafny/expr.rs",
            include_str!("../src/codegen/dafny/expr.rs"),
        ),
        (
            "src/codegen/wasm_gc/types.rs",
            include_str!("../src/codegen/wasm_gc/types.rs"),
        ),
        (
            "src/codegen/wasm_gc/body/from_mir/builtins.rs",
            include_str!("../src/codegen/wasm_gc/body/from_mir/builtins.rs"),
        ),
    ];

    for (path, source) in sources {
        let duplicates_exact_number = source
            .split(|character: char| !character.is_ascii_digit())
            .any(|digits| digits == decimal.as_str());
        assert!(
            !duplicates_exact_number,
            "{path} duplicates the Vector.new materialization boundary {decimal}; derive it from aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS instead"
        );
    }
}

#[test]
fn vector_error_text_is_derived_from_the_same_boundary() {
    assert_eq!(
        aver_rt::vector_size_error_message(),
        format!(
            "Vector.new: size must be between 0 and {}",
            aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS
        )
    );
}
