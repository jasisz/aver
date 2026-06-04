//! Direct graduation assertions for the `own_param` refinement.
//!
//! The soundness suite (`own_param_soundness.rs`) proves the pass never
//! *corrupts* (an aliased param is never un-flagged). This file proves
//! the dual, precision side: a genuinely linearly-threaded collection
//! param MUST still graduate (its `aliased_slots` bit cleared) so the
//! VM #382 / wasm-gc owned-mutate fast path keeps firing — a sound but
//! over-conservative pass would silently regress `vector_ops` /
//! `map_build`. Where the soundness suite uses observable values, this
//! reaches into the lowered MIR and reads the bit directly.

use aver::ir::mir::lower_program;
use aver::ir::mir::optimize::own_param_refine;
use aver::ir::mir::program::MirProgram;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

/// Lower `source` to MIR and run the `own_param` refinement, returning
/// the refined program.
fn refine(source: &str) -> MirProgram {
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
    own_param_refine(program)
}

/// Read the `aliased_slots` bit for param `idx` of fn `name`.
fn param_flagged(program: &MirProgram, name: &str, idx: usize) -> bool {
    let f = program
        .iter()
        .map(|(_, f)| f)
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("fn {name} not found"));
    let slot = f.params[idx].local.0 as usize;
    f.aliased_slots.get(slot).copied().unwrap_or(false)
}

/// The headline fast path: `fillVector` threads its Vector param linearly
/// through `Vector.set` + the tail call, with no alias binding and no
/// capture. The refinement MUST clear its `aliased_slots` bit so the
/// owned-mutate fast path fires. (Soundness side checked in
/// `own_param_soundness::linearly_threaded_fill_sum_is_correct`.)
#[test]
fn fill_vector_param_graduates() {
    let src = r#"module Fill
    intent = "linearly-threaded vector fill — the refinement target"
    depends []
    effects []

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn main() -> Int
    v = fillVector(Vector.new(5, 0), 5, 0)
    Option.withDefault(Vector.get(v, 0), 0)
"#;
    let program = refine(src);
    assert!(
        !param_flagged(&program, "fillVector", 0),
        "fillVector's Vector param must graduate (bit cleared) — the fast path regressed"
    );
}

/// The Map fast path: `buildMap` threads its Map param linearly through
/// `Map.set` + the tail call. The refinement MUST clear its bit.
#[test]
fn build_map_param_graduates() {
    let src = r#"module MapBuild
    intent = "linearly-threaded map build — the refinement target"
    depends []
    effects []

fn buildMap(n: Int, m: Map<String, Int>) -> Map<String, Int>
    ? "tail-recursively insert n entries"
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, String.fromInt(n), n))

fn main() -> Int
    m = buildMap(5, {})
    Map.len(m)
"#;
    let program = refine(src);
    assert!(
        !param_flagged(&program, "buildMap", 1),
        "buildMap's Map param must graduate (bit cleared) — the fast path regressed"
    );
}

/// Precision dual of the escape soundness suite: the aliased params from
/// the three corruption classes must STAY flagged after the refinement.
/// (Their observable correctness is covered in `own_param_soundness`;
/// here we pin the bit so a future change can't silently re-clear it.)
#[test]
fn aliased_corruption_class_params_stay_flagged() {
    // Class 1 — let-rename capture: `cap`'s `v` is captured through an
    // alias, so it must stay flagged.
    let class1 = r#"module C1Let
    intent = "let-rename aliased capture"
    depends []
    effects []

record Box
    inner: Vector<Int>

fn cap(v: Vector<Int>) -> Int
    ? "w = v aliases v; capture w; own-mutate v"
    w = v
    b = Box(inner = w)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    Option.withDefault(Vector.get(b.inner, 0), 0 - 1)

fn main() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)
"#;
    let p1 = refine(class1);
    assert!(
        param_flagged(&p1, "cap", 0),
        "class-1 aliased-capture param must stay flagged"
    );

    // Class 3 — cross-fn capture: `mutateBacking`'s param must stay
    // flagged because its caller's value escaped into `store`'s capture.
    let class3 = r#"module C3Vec
    intent = "cross-fn capture-then-mutate"
    depends []
    effects []

record Box
    items: Vector<Int>

fn store(v: Vector<Int>) -> Box
    ? "captures v"
    Box(items = v)

fn mutateBacking(v: Vector<Int>) -> Vector<Int>
    ? "own-mutate"
    Option.withDefault(Vector.set(v, 0, 999), v)

fn main() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    b = store(base)
    m = mutateBacking(base)
    Option.withDefault(Vector.get(b.items, 0), 0 - 1)
"#;
    let p3 = refine(class3);
    // `store` captures its param — must stay flagged.
    assert!(
        param_flagged(&p3, "store", 0),
        "class-3 capturing-fn param (store) must stay flagged"
    );
    // `mutateBacking` mutates a value that escaped in its caller —
    // must stay flagged.
    assert!(
        param_flagged(&p3, "mutateBacking", 0),
        "class-3 own-mutating param (mutateBacking) must stay flagged"
    );
}

/// Per-slot precision: a single fn that CAPTURES one Vector param (`a`)
/// while threading another (`b`) linearly must keep `a` flagged AND
/// still graduate `b`. The pass flags slots, not whole fns, so the
/// linear `b` is not collateral-damaged by `a`'s capture — the fast
/// path survives alongside the soundness pin.
#[test]
fn capturing_one_param_still_graduates_a_linear_sibling() {
    let src = r#"module Mixed
    intent = "capture param a, thread param b linearly"
    depends []
    effects []

record Box
    held: Vector<Int>

fn f(a: Vector<Int>, b: Vector<Int>) -> Int
    ? "capture a into a Box; mutate b in place; read both"
    box = Box(held = a)
    b2 = Option.withDefault(Vector.set(b, 0, 999), b)
    h = Option.withDefault(Vector.get(box.held, 0), 0 - 1)
    m = Option.withDefault(Vector.get(b2, 0), 0 - 1)
    h * 1000 + m

fn main() -> Int
    va = Option.withDefault(Vector.set(Vector.new(2, 0), 0, 7), Vector.new(2, 0))
    vb = Option.withDefault(Vector.set(Vector.new(2, 0), 0, 3), Vector.new(2, 0))
    f(va, vb)
"#;
    let program = refine(src);
    assert!(
        param_flagged(&program, "f", 0),
        "captured param `a` must stay flagged"
    );
    assert!(
        !param_flagged(&program, "f", 1),
        "linearly-threaded param `b` must still graduate (per-slot precision)"
    );
}

/// Round-3 precision dual: a param stored as the VALUE/ELEMENT arg of a
/// collection builtin (not the target arg 0) must STAY flagged. The
/// sound-by-construction scan treats every builtin arg at index >= 1 as
/// retaining, so `cap`'s `p` — used as the element of `Vector.new(1, p)`
/// — never graduates. (Observable correctness in
/// `own_param_soundness::round3_value_into_vector_set_*`.)
#[test]
fn value_into_collection_param_stays_flagged() {
    let src = r#"module ValIntoVecSet
    intent = "param stored as the element arg of a collection builtin"
    depends []
    effects []

fn cap(p: Vector<Int>) -> Int
    ? "store p as the element of a vector-of-vectors, then own-mutate p"
    outer = Vector.new(1, p)
    mutated = Option.withDefault(Vector.set(p, 0, 999), p)
    inner = Option.withDefault(Vector.get(outer, 0), Vector.new(3, 0 - 1))
    Option.withDefault(Vector.get(inner, 0), 0 - 1)

fn main() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    cap(base)
"#;
    let program = refine(src);
    assert!(
        param_flagged(&program, "cap", 0),
        "param stored as a collection element/value arg must stay flagged"
    );
}
