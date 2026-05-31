//! Phase 6 wave 12b — corpus tests for `discover_instantiations`.
//!
//! Validates that the MIR-side instantiation walk surfaces the
//! built-in generic shapes (`List<T>`, `Option<T>`, `Result<T, E>`,
//! `Map<K, V>`, `Vector<T>`, `Tuple<T₁..Tₙ>`) on real-shape Aver
//! source — the kind of input the wasm-gc backend's
//! `types_discovery.rs` (897 lines) walks today. These tests are
//! the confidence layer before wave 12c ports wasm-gc onto the
//! shared MIR manifest.

use aver::ast::Type;
use aver::ir::mir::{discover_instantiations, lower_program};
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn registry_from_source(src: &str) -> aver::ir::mir::InstantiationRegistry {
    let mut items = parse_source(src).expect("parse");
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
    let program = lower_program(&result.resolved_items);
    discover_instantiations(&program)
}

#[test]
fn empty_program_yields_empty_registry() {
    let r = registry_from_source("");
    assert_eq!(r.total(), 0);
}

#[test]
fn primitive_int_arith_doesnt_pollute_registry() {
    // Pure-Int fn body — no generic shapes anywhere. Registry
    // should stay empty.
    let r = registry_from_source("fn double(x: Int) -> Int\n    x + x\n");
    assert_eq!(r.total(), 0, "no generic shapes expected, got: {r:?}");
}

#[test]
fn list_int_param_registers_list_int() {
    // `fn first(xs: List<Int>) -> Int` — typecheck stamps the
    // param read with `List<Int>`. The walker should pick that
    // up from the Match subject's stamp.
    let src = "fn first(xs: List<Int>) -> Int\n\
               \x20   match xs\n\
               \x20       [] -> 0\n\
               \x20       [head, ..tail] -> head\n";
    let r = registry_from_source(src);
    assert!(
        r.lists.contains(&Type::Int),
        "expected List<Int> in lists bucket, got: {:?}",
        r.lists
    );
}

#[test]
fn option_some_construction_registers_option_t() {
    // `Option.Some(42)` — the constructor's arg stamps the
    // outer Option<Int>.
    let src = "fn wrapped() -> Option<Int>\n    Option.Some(42)\n";
    let r = registry_from_source(src);
    assert!(
        r.options.contains(&Type::Int),
        "expected Option<Int>, got: {:?}",
        r.options
    );
}

#[test]
fn result_ok_construction_registers_result_pair() {
    let src = "fn wrapped() -> Result<Int, String>\n    Result.Ok(42)\n";
    let r = registry_from_source(src);
    // Result has at least one entry; T side is Int. E side is
    // Invalid placeholder when the Ok constructor is observed
    // without a matching Err in the same walk — that's the
    // documented behavior.
    assert!(
        !r.results.is_empty(),
        "expected Result<…> registration, got: {:?}",
        r.results
    );
    // Most-permissive check: the registered T at minimum is Int
    // somewhere in the pair list.
    let has_int_t = r.results.iter().any(|(t, _)| t == &Type::Int);
    assert!(
        has_int_t,
        "Result registration should mention Int on T side, got: {:?}",
        r.results
    );
}

#[test]
fn nested_list_of_option_int_registers_both_layers() {
    // `fn first(xs: List<Option<Int>>) -> Option<Int>`. Both
    // layers should surface. The `[]` arm returns `Option.Some(0)`
    // instead of `Option.None` to avoid the bare-nullary-ctor
    // lower drop (`MirExpr::Construct(Option.None)` lowers fine,
    // but the bare `Option.None` in value position currently
    // bounces with `UnresolvedIdent` per `MIR wave 3c-i`).
    let src = "fn first(xs: List<Option<Int>>) -> Option<Int>\n\
               \x20   match xs\n\
               \x20       [] -> Option.Some(0)\n\
               \x20       [head, ..tail] -> head\n";
    let r = registry_from_source(src);

    let opt_int = Type::Option(Box::new(Type::Int));
    assert!(
        r.lists.contains(&opt_int),
        "expected List<Option<Int>> — `Option<Int>` should appear in lists bucket, got: {:?}",
        r.lists
    );
    assert!(
        r.options.contains(&Type::Int),
        "expected Option<Int> in options bucket, got: {:?}",
        r.options
    );
}

#[test]
fn distinct_list_t_kinds_register_separately() {
    // Two fns: `first(List<Int>) -> Int` and `head(List<String>) -> String`.
    // Both `List<Int>` and `List<String>` must register.
    let src = "fn firstInt(xs: List<Int>) -> Int\n\
               \x20   match xs\n\
               \x20       [] -> 0\n\
               \x20       [head, ..tail] -> head\n\
               \n\
               fn headStr(xs: List<String>) -> String\n\
               \x20   match xs\n\
               \x20       [] -> \"\"\n\
               \x20       [head, ..tail] -> head\n";
    let r = registry_from_source(src);
    assert!(r.lists.contains(&Type::Int), "List<Int> missing");
    assert!(r.lists.contains(&Type::Str), "List<Str> missing");
}

#[test]
fn same_shape_used_twice_collapses_to_one_registration() {
    // Two fns both consuming `List<Int>` — should produce a
    // SINGLE `List<Int>` registration, not two.
    let src = "fn firstInt(xs: List<Int>) -> Int\n\
               \x20   match xs\n\
               \x20       [] -> 0\n\
               \x20       [head, ..tail] -> head\n\
               \n\
               fn sumInts(xs: List<Int>) -> Int\n\
               \x20   match xs\n\
               \x20       [] -> 0\n\
               \x20       [head, ..tail] -> head + sumInts(tail)\n";
    let r = registry_from_source(src);
    let list_int_count = r.lists.iter().filter(|t| **t == Type::Int).count();
    assert_eq!(
        list_int_count, 1,
        "List<Int> should dedupe across multiple fns, got: {:?}",
        r.lists
    );
}
