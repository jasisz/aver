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
use aver::ir::mir::optimize::{own_param_refine, own_param_refine_for_rust};
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

/// Run the shared arena-safe pass followed by the Rust-only drop-aware
/// refinement, exactly like the generated-Rust pipeline.
fn refine_rust(source: &str) -> MirProgram {
    own_param_refine_for_rust(refine(source))
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

/// A named helper that returns the accumulator must preserve the same linear
/// ownership proof as spelling `Map.set` directly in the recursive argument.
///
/// This is the exact shape from issue #890. Treating every named-function
/// result as potentially aliased leaves both collection params flagged, so
/// generated Rust clones the whole backing table once per insertion.
#[test]
fn map_param_graduates_through_a_named_set_helper() {
    let src = r#"module MapBuildThroughHelper
    intent = "linearly-threaded map build through a named helper"
    depends []
    effects []

fn setOne(key: String, into: Map<String, Int>) -> Map<String, Int>
    ? "insert one entry and return the successor map"
    Map.set(into, key, 1)

fn build(keys: List<String>, into: Map<String, Int>) -> Map<String, Int>
    ? "tail-recursively insert every key through setOne"
    match keys
        [] -> into
        [head, ..tail] -> build(tail, setOne(head, into))

fn main() -> Int
    Map.len(build(["a", "b", "c"], {}))
"#;
    let program = refine(src);
    assert!(
        !param_flagged(&program, "setOne", 1),
        "setOne's Map param must graduate — it returns Map.set's linear successor"
    );
    assert!(
        !param_flagged(&program, "build", 1),
        "build's Map param must graduate through the named setOne result"
    );
}

/// `Result.Ok(Map.set(...))` is ownership-transparent only for generated
/// Rust: `?` consumes and drops the wrapper. The VM keeps the wrapper in its
/// arena after logical unwrapping, so the shared pass must remain conservative.
#[test]
fn result_wrapped_map_graduates_only_for_rust() {
    let src = r#"module MapBuildThroughResultHelper
    intent = "linearly-threaded map build through a Result-returning helper"
    depends []
    effects []

fn setOne(key: String, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "insert one entry and wrap the successor map"
    Result.Ok(Map.set(into, key, 1))

fn build(keys: List<String>, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "tail-recursively insert every key through setOne"
    match keys
        [] -> Result.Ok(into)
        [head, ..tail] -> build(tail, setOne(head, into)?)

fn main() -> Result<Int, String>
    built = build(["a", "b", "c"], {})?
    Result.Ok(Map.len(built))
"#;

    let shared = refine(src);
    assert!(
        param_flagged(&shared, "setOne", 1),
        "the arena-safe pass must keep the Map param flagged through Result.Ok"
    );
    assert!(
        param_flagged(&shared, "build", 1),
        "the arena-safe pass must keep recursive Result<Map> threading flagged"
    );

    let rust = refine_rust(src);
    assert!(
        !param_flagged(&rust, "setOne", 1),
        "Rust may move the Map through the consumed Result wrapper"
    );
    assert!(
        !param_flagged(&rust, "build", 1),
        "Rust must preserve the owned move across the Result-returning helper"
    );
}

/// Regression for #1196: a mutually-tail-recursive dispatcher owns its
/// accumulator at the point where it hands that accumulator to a normal
/// helper. The helper returns the successor inside `Result<Tuple<...>>`.
/// Generated Rust consumes the wrapper and tuple, so the normal helper may
/// take the last-use Map by value; borrowing it here makes the helper's first
/// `Map.set` copy the complete backing table once per dispatcher round.
#[test]
fn map_crossing_from_mutual_tco_into_result_tuple_helper_graduates_for_rust() {
    let src = r#"module CrossCallMap
    intent = "move a Map from a mutual-TCO dispatcher through a normal helper"
    depends []
    effects []

fn changed(into: Map<String, Int>) -> Result<Tuple<Map<String, Int>, Int>, String>
    ? "return the map successor in the aggregate consumed by the dispatcher"
    Result.Ok((Map.set(into, "key", 1), 1))

fn kept(into: Map<String, Int>) -> Map<String, Int>
    ? "model a read helper whose conservative result may alias its input"
    into

fn absorbing(left: Int, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "hand the last-use map to changed before tail-calling continued"
    snapshot = kept(into)
    observedLen = Map.len(snapshot)
    match changed(into)
        Result.Err(why) -> Result.Err(why)
        Result.Ok(parts) -> match parts
            (next, _) -> continued(left, next)

fn continued(left: Int, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "mutually tail-call absorbing until the requested rounds are complete"
    match left
        0 -> Result.Ok(into)
        _ -> absorbing(left - 1, into)

fn main() -> Result<Int, String>
    built = absorbing(3, {})?
    Result.Ok(Map.len(built))
"#;

    let shared = refine(src);
    assert!(
        param_flagged(&shared, "changed", 0),
        "arena backends must not infer ownership through destructured wrapper entries"
    );

    let rust = own_param_refine_for_rust(shared);
    assert!(
        !param_flagged(&rust, "changed", 0),
        "the normal helper must own the dispatcher's last-use Map argument"
    );
    assert!(
        !param_flagged(&rust, "absorbing", 1),
        "the first mutual-TCO state must preserve ownership through the helper result"
    );
    assert!(
        !param_flagged(&rust, "continued", 1),
        "the next mutual-TCO state must receive the destructured Map by value"
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

/// The seed decides, and it decides the same way for both collections.
///
/// `Vector.fromList` and `Map.fromList` build their collection from scratch and
/// hand back either an immediate empty value or a slot nothing else has an
/// index to (`vec_from_list_nv` in `src/types/vector.rs`, `from_list_nv` in
/// `src/types/map.rs`), so an accumulator spelled either way is exactly as
/// fresh as `Vector.new(5, 0)` or `{}`. Both must graduate.
///
/// This is issue #900 at the analysis level: while `Map.fromList` was missing
/// from the freshness list, a fold seeded from it kept its accumulator flagged
/// for the whole run and every insert preserved the map by copying it.
/// `Vector.fromList` sat in the same gap with the same proof.
#[test]
fn from_list_seeded_params_graduate() {
    let vector_src = r#"module FillFromList
    intent = "vector fill seeded from Vector.fromList"
    depends []
    effects []

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn main() -> Int
    v = fillVector(Vector.fromList([0, 0, 0, 0, 0]), 5, 0)
    Option.withDefault(Vector.get(v, 0), 0)
"#;
    let program = refine(vector_src);
    assert!(
        !param_flagged(&program, "fillVector", 0),
        "a Vector accumulator seeded from Vector.fromList must graduate — \
         fromList hands back a fresh handle, exactly like Vector.new"
    );

    let map_src = r#"module MapBuildFromList
    intent = "map build seeded from Map.fromList"
    depends []
    effects []

fn buildMap(n: Int, m: Map<String, Int>) -> Map<String, Int>
    ? "tail-recursively insert n entries"
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, String.fromInt(n), n))

fn main() -> Int
    m = buildMap(5, Map.fromList([]))
    Map.len(m)
"#;
    let program = refine(map_src);
    assert!(
        !param_flagged(&program, "buildMap", 1),
        "a Map accumulator seeded from Map.fromList must graduate — this is the \
         copy-per-insert gap from issue #900"
    );
}

/// The same seed, named before it is passed — the spelling the test above
/// structurally cannot see.
///
/// Freshness is decided in two places that have to agree. `own_param`'s
/// `uniquely_owned` reads a call ARGUMENT, so it sees `Map.fromList(..)`
/// written inline at the call. `ir::alias`'s `is_fresh_collection_builtin`
/// decides a BINDING, and that is what a *named* seed goes through:
/// `slot_owned` answers false for a flagged non-param slot without ever
/// consulting what the binding was built from, so one missing name there
/// un-graduates the callee's accumulator however well `own_param` knows the
/// builtin. `Vector.fromList` was in that list and `Map.fromList` was not,
/// which left the named map spelling exactly as quadratic as issue #900
/// reported while the inline one was already fixed — 80,200 entries copied at
/// n=400 and 320,400 at n=800, measured in
/// `vm::execute::tests::growing_a_map_seeded_from_a_named_from_list_result_consumes_it_too`.
///
/// The seed is passed from the tail expression rather than through a further
/// binding, because a bare collection local that flows into another binding's
/// value is flagged by the ESCAPE half whatever it was built from — a separate
/// condition this entry does not touch, and one the vector control below shares.
///
/// Both collections are checked on purpose: an entry present in one list and
/// not the other is the defect this pair exists to catch.
#[test]
fn from_list_seeds_graduate_when_the_seed_is_named_first_vector() {
    let vector_src = r#"module FillFromNamedList
    intent = "vector fill seeded from a named Vector.fromList result"
    depends []
    effects []

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn main() -> Int
    seed = Vector.fromList([0, 0, 0, 0, 0])
    Option.withDefault(Vector.get(fillVector(seed, 5, 0), 0), 0)
"#;
    let program = refine(vector_src);
    assert!(
        !param_flagged(&program, "fillVector", 0),
        "a Vector accumulator seeded from a NAMED Vector.fromList result must \
         graduate — naming the seed cannot make a fresh handle shared"
    );
}

#[test]
fn from_list_seeds_graduate_when_the_seed_is_named_first_map() {
    let map_src = r#"module MapBuildFromNamedList
    intent = "map build seeded from a named Map.fromList result"
    depends []
    effects []

fn seedPairs() -> List<Tuple<String, Int>>
    ? "one pair, so the seed's element type is known without a call-site hint"
    [("s", 0)]

fn buildMap(n: Int, m: Map<String, Int>) -> Map<String, Int>
    ? "tail-recursively insert n entries"
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, String.fromInt(n), n))

fn main() -> Int
    seed = Map.fromList(seedPairs())
    Map.len(buildMap(5, seed))
"#;
    let program = refine(map_src);
    assert!(
        !param_flagged(&program, "buildMap", 1),
        "a Map accumulator seeded from a NAMED Map.fromList result must \
         graduate — this is issue #900 surviving in the hoisted spelling"
    );
}

// ─── call results never grant ownership ─────────────────────────────────
//
// `uniquely_owned` answers `false` for every call whose callee is not a
// builtin: a callee may hand back one of its own arguments, so the result
// can share a collection the caller still holds. Two constructors reach
// that arm from real source — `MirCallee::Fn` (a named user function) and
// `MirCallee::LocalSlot` (a first-class fn value held in a slot) — and each
// one needs its own pin, because a change that only relaxes one of them
// leaves the other test green.
//
// Both shapes below hand a call result STRAIGHT into another call's
// argument, never through a `let`. That is what keeps the decision on this
// arm: a `let`-bound result is a `MirExpr::Local` at the call site, which
// the binding rules settle long before `uniquely_owned` sees a `Call`.

/// The `MirCallee::Fn` edge. `keepFirst` returns one of its two argument
/// maps and that result is `growth`'s argument with no binding in between,
/// while `main` reads `base` back afterwards. `growth`'s map param must
/// stay flagged — granting it ownership lets `growth` mutate `base` in
/// place. Behavioural twin: the
/// `rust_fn_result_argument_keeps_the_callers_map_intact` differential.
#[test]
fn named_fn_call_result_argument_keeps_the_param_flagged() {
    let src = r#"module OwnedFnResultMap
    intent = "a helper's Map result flows straight into another call"
    depends []
    effects []

fn keepFirst(a: Map<String, Int>, b: Map<String, Int>) -> Map<String, Int>
    ? "returns one of its arguments, so the result shares a caller value"
    match Map.len(a) > 0
        true -> a
        false -> b

fn growth(m: Map<String, Int>, n: Int) -> Int
    ? "threads the map linearly, then reports its size"
    match n == 0
        true -> Map.len(m)
        false -> growth(Map.set(m, "g{n}", n), n - 1)

fn main() -> Int
    base = Map.set(Map.set({}, "a", 7), "b", 8)
    grown = growth(keepFirst(base, {}), 4)
    grown + Map.len(base)
"#;
    let program = refine(src);
    assert!(
        param_flagged(&program, "growth", 0),
        "a named-fn call result must not grant ownership of the caller's map"
    );
}

/// The `MirCallee::LocalSlot` edge — the same class through a first-class
/// fn value. `viaValue` calls its `Fn(..)` parameter and hands the result
/// straight to `growth` while keeping `base` live, so `growth`'s map param
/// must stay flagged. `aliasIt` returning its own parameter is what makes a
/// grant here observable, but the pin does not depend on which function is
/// passed: the pass cannot see through the slot at all.
#[test]
fn fn_value_call_result_argument_keeps_the_param_flagged() {
    let src = r#"module OwnedSlotResultMap
    intent = "a fn value's Map result flows straight into another call"
    depends []
    effects []

fn aliasIt(m: Map<String, Int>) -> Map<String, Int>
    ? "returns its own parameter, so the result shares a caller value"
    m

fn growth(m: Map<String, Int>, n: Int) -> Int
    ? "threads the map linearly, then reports its size"
    match n == 0
        true -> Map.len(m)
        false -> growth(Map.set(m, "g{n}", n), n - 1)

fn viaValue(f: Fn(Map<String, Int>) -> Map<String, Int>, base: Map<String, Int>) -> Int
    ? "calls the fn value and hands its result straight to another call"
    grown = growth(f(base), 4)
    grown + Map.len(base)

fn main() -> Int
    base = Map.set(Map.set({}, "a", 7), "b", 8)
    viaValue(aliasIt, base)
"#;
    let program = refine(src);
    assert!(
        param_flagged(&program, "growth", 0),
        "a fn-value call result must not grant ownership of the caller's map"
    );
}
