//! Cross-layer guards for fixed-width Int → Bytes materialization.

#[test]
fn endian_emitters_derive_the_shared_sequence_limit() {
    let decimal = aver_rt::MAX_MATERIALIZED_SEQUENCE_ELEMENTS.to_string();
    let sources = [
        ("src/ast/mod.rs", include_str!("../src/ast/mod.rs")),
        (
            "src/codegen/rust/from_mir.rs",
            include_str!("../src/codegen/rust/from_mir.rs"),
        ),
        (
            "src/codegen/lean/transpile.rs",
            include_str!("../src/codegen/lean/transpile.rs"),
        ),
        (
            "src/codegen/dafny/expr.rs",
            include_str!("../src/codegen/dafny/expr.rs"),
        ),
        (
            "src/codegen/wasm_gc/builtins/endian.rs",
            include_str!("../src/codegen/wasm_gc/builtins/endian.rs"),
        ),
    ];

    for (path, source) in sources {
        let duplicates_exact_number = source
            .split(|character: char| !character.is_ascii_digit())
            .any(|digits| digits == decimal.as_str());
        assert!(
            !duplicates_exact_number,
            "{path} duplicates the endian materialization boundary {decimal}"
        );
    }
}

#[test]
fn endian_errors_are_built_once_for_every_backend() {
    assert_eq!(
        aver_rt::int_endian_width_error_message("Int.toBigEndian"),
        format!(
            "Int.toBigEndian: width must be between 0 and {}",
            aver_rt::MAX_MATERIALIZED_SEQUENCE_ELEMENTS
        )
    );
    assert_eq!(
        aver_rt::int_endian_value_error_message("Int.toLittleEndian"),
        "Int.toLittleEndian: value does not fit in the requested width"
    );
}
