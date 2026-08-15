#![cfg(feature = "wasm")]

//! Differential harness for the `__aint_to_index` host contract and the fused
//! `Option.withDefault(Vector.get(vec, idx), d)` bounds check.
//!
//! The certificate wall assumes ONE relational law of the wired
//! `__aint_to_index` helper (`CertPrelude.toIndexW`): a represented integer in
//! `[0, 2^31)` extracts to its own i32 value; a negative or `>= 2^31` value —
//! including every limb-carrying big value — collapses to the out-of-bounds
//! sentinel `-1`. The fused vector-read face additionally models the emitter's
//! `idx >= 0 (signed) AND idx < len (unsigned)` check with a TRUE unsigned
//! `i32.lt_u` clause (`emod 2^32` on both operands), so the sentinel compares
//! as `2^32 - 1` and can never sit below an engine array length.
//!
//! This test runs the REAL compiled artifacts under wasmtime on the edge
//! values of that contract (`-1`, `0`, `1`, `2^31 - 1`, `2^31`, `2^31 + 1`,
//! i64 extremes, and a limb-carrying big index) and compares:
//!  * the `__aint_to_index` export against the Rust twin of `toIndexW`, and
//!  * the fused `cellAt` read against BOTH the face model (in-bounds read or
//!    literal default) and the machine-chain prediction derived from the
//!    interpreter clauses (`ge_s`/`lt_u` over the extracted index) — the two
//!    must agree with each other (the template theorem's content, sampled
//!    concretely) and with the running artifact.

use aver::codegen::wasm_gc::compile_to_wasm_gc;
use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::source::parse_source;

const SOURCE: &str = r#"
module ToIndexDiff
    intent =
        "Differential fixture for the to-index contract and the fused"
        "bounds-checked vector read."
    exposes [cellAt, probeVec, bigify]
    effects []

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    ? "Fused bounds-checked read; 0 outside the vector."
    Option.withDefault(Vector.get(grid, idx), 0)

fn probeVec() -> Vector<Int>
    ? "Three distinguishable elements."
    Vector.fromList([10, 20, 30])

fn bigify(x: Int) -> Int
    ? "Promote an in-range value into a limb-carrying big integer."
    x * 4611686018427387904
"#;

/// Rust twin of `CertPrelude.toIndexW` (the assumed helper contract).
fn to_index_model(n: i128) -> i32 {
    if 0 <= n && n < (1_i128 << 31) {
        n as i32
    } else {
        -1
    }
}

/// Rust twin of `StandardFace.vecModel`: in-bounds read, else the default.
fn vec_model(v: &[i64], i: i128, d: i64) -> i64 {
    if 0 <= i && i < v.len() as i128 {
        v[i as usize]
    } else {
        d
    }
}

/// Machine-chain prediction from the audited interpreter clauses: extract the
/// index through the `toIndexW` contract, test `extracted >= 0` (signed,
/// `i32GeS`) AND `extracted <u len` with the `i32LtU` clause's `emod 2^32`
/// semantics, then read or default.
fn machine_chain_model(v: &[i64], i: i128, d: i64) -> i64 {
    let extracted = i128::from(to_index_model(i));
    let ge = extracted >= 0;
    let lt = extracted.rem_euclid(1 << 32) < (v.len() as i128).rem_euclid(1 << 32);
    if ge && lt { v[extracted as usize] } else { d }
}

fn compile_bytes(source: &str) -> Vec<u8> {
    let mut items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_list_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        panic!("typecheck failed: {:?}", tc.errors);
    }
    compile_to_wasm_gc(&items, result.analysis.as_ref())
        .unwrap_or_else(|e| panic!("wasm-gc compile failed: {e:?}"))
}

struct Harness {
    store: wasmtime::Store<()>,
    from_i64: wasmtime::Func,
    to_index: wasmtime::Func,
    cell_at: wasmtime::Func,
    probe_vec: wasmtime::Func,
    bigify: wasmtime::Func,
}

fn harness() -> Harness {
    let bytes = compile_bytes(SOURCE);
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.max_wasm_stack(8 * 1024 * 1024);
    config.async_stack_size(12 * 1024 * 1024);
    let engine = wasmtime::Engine::new(&config).expect("wasmtime engine");
    let module = wasmtime::Module::new(&engine, &bytes)
        .unwrap_or_else(|e| panic!("wasmtime rejected wasm-gc bytes: {e}"));
    let mut store = wasmtime::Store::new(&engine, ());
    let linker = wasmtime::Linker::new(&engine);
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap_or_else(|e| panic!("instantiate failed: {e}"));
    let func = |store: &mut wasmtime::Store<()>, name: &str| {
        instance
            .get_func(&mut *store, name)
            .unwrap_or_else(|| panic!("export `{name}` missing"))
    };
    let from_i64 = func(&mut store, "__rt_aint_from_i64");
    let to_index = func(&mut store, "__aint_to_index");
    let cell_at = func(&mut store, "cellAt");
    let probe_vec = func(&mut store, "probeVec");
    let bigify = func(&mut store, "bigify");
    Harness {
        store,
        from_i64,
        to_index,
        cell_at,
        probe_vec,
        bigify,
    }
}

fn call_one(
    store: &mut wasmtime::Store<()>,
    func: &wasmtime::Func,
    args: &[wasmtime::Val],
) -> wasmtime::Val {
    let result_placeholder = wasmtime::Val::I32(0);
    let mut results = [result_placeholder];
    func.call(&mut *store, args, &mut results)
        .unwrap_or_else(|e| panic!("call trapped: {e}"));
    results[0].clone()
}

fn carrier_of(h: &mut Harness, v: i64) -> wasmtime::Val {
    call_one(&mut h.store, &h.from_i64.clone(), &[wasmtime::Val::I64(v)])
}

fn big_carrier_of(h: &mut Harness, seed: i64) -> wasmtime::Val {
    let seed = carrier_of(h, seed);
    call_one(&mut h.store, &h.bigify.clone(), &[seed])
}

fn to_index_of(h: &mut Harness, carrier: wasmtime::Val) -> i32 {
    match call_one(&mut h.store, &h.to_index.clone(), &[carrier]) {
        wasmtime::Val::I32(v) => v,
        other => panic!("__aint_to_index returned a non-i32: {other:?}"),
    }
}

fn decode_small(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> i64 {
    let anyref = match val {
        wasmtime::Val::AnyRef(Some(a)) => *a,
        other => panic!("expected an Int carrier, got {other:?}"),
    };
    let structref = anyref
        .as_struct(&mut *store)
        .expect("anyref-to-struct query")
        .expect("Int carrier is not a struct");
    match structref.field(&mut *store, 1).expect("read $magf field") {
        wasmtime::Val::AnyRef(None) => {}
        _ => panic!("expected a Small Int result"),
    }
    match structref.field(&mut *store, 0).expect("read $small field") {
        wasmtime::Val::I64(v) => v,
        other => panic!("$small field was not an i64: {other:?}"),
    }
}

/// The i64-representable contract edge values: around zero, around the
/// `2^31` cut, and the i64 extremes.
const EDGE_VALUES: &[i64] = &[
    -1,
    0,
    1,
    2,
    2147483646,  // 2^31 - 2
    2147483647,  // 2^31 - 1 (last pass-through value)
    2147483648,  // 2^31 (first sentinel value)
    2147483649,  // 2^31 + 1
    4294967295,  // 2^32 - 1
    4294967296,  // 2^32
    -2147483648, // -2^31
    i64::MAX,
    i64::MIN,
];

#[test]
fn to_index_helper_matches_the_relational_contract_on_edge_values() {
    let mut h = harness();
    for &v in EDGE_VALUES {
        let carrier = carrier_of(&mut h, v);
        let got = to_index_of(&mut h, carrier);
        let expected = to_index_model(i128::from(v));
        assert_eq!(
            got, expected,
            "__aint_to_index({v}) diverged from the toIndexW contract"
        );
    }
}

#[test]
fn to_index_helper_collapses_big_values_to_the_sentinel() {
    let mut h = harness();
    // 2^62 * 2^62 = 2^124 (positive Big) and its negation.
    for seed in [4611686018427387904_i64, -4611686018427387904_i64] {
        let big = big_carrier_of(&mut h, seed);
        let got = to_index_of(&mut h, big);
        assert_eq!(
            got, -1,
            "__aint_to_index on a big carrier (seed {seed}) must be the -1 sentinel"
        );
    }
}

/// The fused read's own bounds boundary for `probeVec`'s three elements: the
/// last in-bounds index, the first out-of-bounds one (`idx == len`, where the
/// `i32LtU` guard must flip), and one past it. The contract edge values above
/// exercise the `2^31` cut but never land on the vector's length.
const LENGTH_EDGE_INDICES: &[i64] = &[2, 3, 4];

#[test]
fn fused_vector_read_matches_face_model_and_machine_chain_on_edge_values() {
    let mut h = harness();
    let elements: &[i64] = &[10, 20, 30];
    for &v in EDGE_VALUES.iter().chain(LENGTH_EDGE_INDICES) {
        let face = vec_model(elements, i128::from(v), 0);
        let chain = machine_chain_model(elements, i128::from(v), 0);
        assert_eq!(
            face, chain,
            "face model and interpreter-chain prediction diverged at index {v}"
        );
        let grid = call_one(&mut h.store, &h.probe_vec.clone(), &[]);
        let idx = carrier_of(&mut h, v);
        let got_val = call_one(&mut h.store, &h.cell_at.clone(), &[grid, idx]);
        let got = decode_small(&mut h.store, &got_val);
        assert_eq!(
            got, face,
            "cellAt({v}) diverged from the fused vector-read face model"
        );
    }
}

#[test]
fn fused_vector_read_defaults_on_big_indices() {
    let mut h = harness();
    for seed in [4611686018427387904_i64, -4611686018427387904_i64] {
        let grid = call_one(&mut h.store, &h.probe_vec.clone(), &[]);
        let idx = big_carrier_of(&mut h, seed);
        let got_val = call_one(&mut h.store, &h.cell_at.clone(), &[grid, idx]);
        let got = decode_small(&mut h.store, &got_val);
        assert_eq!(
            got, 0,
            "cellAt on a big index (seed {seed}) must take the default arm"
        );
    }
}
