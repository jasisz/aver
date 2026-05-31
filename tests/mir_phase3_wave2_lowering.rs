//! Phase 3 wave 2 of #252 — HIR → MIR lowering for call shapes
//! and product types: user calls, builtin calls, user constructors,
//! record create / update / project.
//!
//! Same `parse → pipeline → lower → Display` rig as wave 1. New
//! coverage:
//! - `Fn(FnId).call(...)` — user fn invocation, identity-typed
//! - `Builtin(...).call(...)` — built-in namespace methods
//! - `CtorId(N)(...)` — user variant constructors
//! - `TypeId(N) { … }` — record create
//! - `TypeId(N).update(...)` — record update
//! - `base.field` — projection
//!
//! Wave 2 still requires single-stmt expr bodies (multi-stmt with
//! `Let` lands in wave 3 alongside `match` / `Try`). Built-in
//! constructors (`Result.Ok`, `Option.Some`, …) and record types
//! with no `TypeId` (`HttpResponse`, `Header`, …) are also wave 3.

use aver::ir::mir::lower_program;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn lower(source: &str) -> String {
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
    let program = lower_program(&result.resolved_items);
    format!("{program}")
}

#[test]
fn lowers_user_fn_call_with_typed_callee() {
    let dump = lower(
        "fn double(x: Int) -> Int\n    x + x\n\n\
         fn use_double() -> Int\n    double(7)\n",
    );
    // The `use_double` body is `double(7)`. The dump should carry
    // the typed callee identity, not a string name.
    assert!(
        dump.contains("FnId(") && dump.contains(").call(Int(7))"),
        "expected typed `FnId(N).call(Int(7))` in dump:\n{dump}"
    );
}

#[test]
fn lowers_builtin_call() {
    // `String.len(s)` is a built-in namespace method.
    let dump = lower("fn length_of(s: String) -> Int\n    String.len(s)\n");
    // The `s` arg is the final read in the fn body, so MIR's
    // wave-4 last-use annotation renders it as `%0*`.
    assert!(
        dump.contains("Builtin(String.len).call(%0*)"),
        "expected built-in callee + last-use local arg:\n{dump}"
    );
}

#[test]
fn lowers_user_ctor_to_construct_with_ctor_id() {
    // `record Shape { name: String }` then `Shape.create(\"x\")` —
    // hmm, records don't take ctor args like sum types. Use a sum
    // type instead so we exercise the `Ctor` path.
    let dump = lower(
        "type Color\n  Red\n  Green(Int)\n\n\
         fn make() -> Color\n    Color.Green(42)\n",
    );
    // User constructors carry `CtorId(N)` in the dump; the source
    // name (\"Green\") is the source-level diagnostic field on
    // `ResolvedCtor` but doesn't ride into MIR — identity-only.
    assert!(
        dump.contains("CtorId(") && dump.contains(")(Int(42))"),
        "expected CtorId-typed construct with Int(42) arg:\n{dump}"
    );
}

#[test]
fn lowers_record_create_and_project() {
    let dump = lower(
        "record Point\n  x: Int\n  y: Int\n\n\
         fn origin() -> Point\n    Point(x = 0, y = 0)\n\n\
         fn x_of(p: Point) -> Int\n    p.x\n",
    );
    assert!(
        dump.contains("TypeId(") && dump.contains("{ x = Int(0), y = Int(0) }"),
        "expected TypeId-typed record-create with field list:\n{dump}"
    );
    // `p` is the final read in `x_of`, so MIR's wave-4 last-use
    // annotation renders it as `%0*.x`.
    assert!(
        dump.contains("%0*.x"),
        "expected `%0*.x` projection in x_of dump:\n{dump}"
    );
}

#[test]
fn lowers_record_update() {
    let dump = lower(
        "record Point\n  x: Int\n  y: Int\n\n\
         fn shift_x(p: Point) -> Point\n    Point.update(p, x = 99)\n",
    );
    // `p` is last-use here too — renders as `%0*`.
    assert!(
        dump.contains(".update(%0*, x = Int(99))"),
        "expected record-update with last-use base local + override:\n{dump}"
    );
}

#[test]
fn builtin_ctor_fn_lowers_after_wave_3c_i() {
    // `Result.Ok(_)` is a built-in constructor. Wave 2 dropped fns
    // touching them; wave 3c-i landed `MirCtor::Builtin` so the fn
    // now lowers cleanly with the canonical builtin name in the dump.
    let dump = lower("fn ok_seven() -> Result<Int, String>\n    Result.Ok(7)\n");
    assert!(
        dump.contains("fn ok_seven"),
        "built-in ctor fn must lower after wave 3c-i:\n{dump}"
    );
    assert!(
        dump.contains("Result.Ok("),
        "dump should render the canonical builtin name (`Result.Ok`):\n{dump}"
    );
}
