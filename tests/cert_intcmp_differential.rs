#![cfg(feature = "wasm")]

//! Differential harness for the two Int value-comparison host contracts,
//! `__aint_cmp` and `__aint_eq`.
//!
//! The certificate wall assumes ONE relational law per helper
//! (`CertPrelude.cmpW` / `CertPrelude.eqW`): applied to two carriers that
//! REPRESENT the integers `a` and `b`, the three-way helper returns exactly
//! `-1`/`0`/`1` according to `a < b` / `a = b` / `a > b`, and the equality
//! helper returns exactly `1`/`0`. Nothing in the wall interprets either
//! body: the bodies are pinned to a byte template and the laws are assumed of
//! whatever sits at the pinned index. So the laws have to be tested against
//! the real machine, and this is where that happens.
//!
//! The harness runs the REAL compiled artifact under wasmtime over a value set
//! that crosses every boundary the two-limb-plus-sign carrier has:
//!  * Small/Small, Small/Big and Big/Big operand pairs (the class of each
//!    operand is READ OFF the carrier, not assumed, and the test asserts that
//!    all three combinations really occur);
//!  * negatives, zero, and equal-magnitude-opposite-sign pairs (the arm of
//!    `__aint_cmp` that negates the unsigned magnitude verdict);
//!  * `±2^63 ± 1` — the exact Small/Big cut, where a one-off in the carrier's
//!    normalisation would flip a class.
//!
//! The three faces built on the two roles are checked the same way: `atLeast`
//! (cmp + `i32.ge_s`), `sameKey` (eq alone) and `minInt` (cmp + select) are run
//! against the Rust twins of their face models.

use aver::codegen::wasm_gc::compile_to_wasm_gc;
use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::source::parse_source;

const SOURCE: &str = r#"
module IntCmpDiff
    intent =
        "Differential fixture for the two Int value-comparison host contracts"
        "and the three faces built on them."
    exposes [compose, atLeast, sameKey, minInt]
    effects []

fn compose(hi: Int, lo: Int) -> Int
    ? "Assemble an arbitrary-width Int from a 2^62-radix quotient/remainder pair."
    hi * 4611686018427387904 + lo

fn atLeast(a: Int, b: Int) -> Bool
    ? "The cmp-plus-relop face."
    a >= b

fn sameKey(a: Int, b: Int) -> Bool
    ? "The eq face."
    a == b

fn minInt(a: Int, b: Int) -> Int
    ? "The select face: the result is a passthrough of one argument."
    match a < b
        true -> a
        false -> b
"#;

/// Rust twin of `CertPrelude.cmpW` (the assumed `__aint_cmp` contract).
fn cmp_model(a: i128, b: i128) -> i32 {
    if a < b {
        -1
    } else if a == b {
        0
    } else {
        1
    }
}

/// Rust twin of `CertPrelude.eqW` (the assumed `__aint_eq` contract).
fn eq_model(a: i128, b: i128) -> i32 {
    i32::from(a == b)
}

/// Rust twin of `StandardFace.classifyIntCmpBool`'s model at `ge`: the source
/// Boolean IS the `i32.ge_s` of the helper verdict against zero.
fn at_least_model(a: i128, b: i128) -> i32 {
    i32::from(cmp_model(a, b) >= 0)
}

/// Rust twin of `StandardFace.classifyIntSelect`'s model at `lt`.
fn min_model(a: i128, b: i128) -> i128 {
    if cmp_model(a, b) < 0 { a } else { b }
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
    cmp: wasmtime::Func,
    eq: wasmtime::Func,
    compose: wasmtime::Func,
    at_least: wasmtime::Func,
    same_key: wasmtime::Func,
    min_int: wasmtime::Func,
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
    let cmp = func(&mut store, "__aint_cmp");
    let eq = func(&mut store, "__aint_eq");
    let compose = func(&mut store, "compose");
    let at_least = func(&mut store, "atLeast");
    let same_key = func(&mut store, "sameKey");
    let min_int = func(&mut store, "minInt");
    Harness {
        store,
        from_i64,
        cmp,
        eq,
        compose,
        at_least,
        same_key,
        min_int,
    }
}

fn call_one(
    store: &mut wasmtime::Store<()>,
    func: &wasmtime::Func,
    args: &[wasmtime::Val],
) -> wasmtime::Val {
    let mut results = [wasmtime::Val::I32(0)];
    func.call(&mut *store, args, &mut results)
        .unwrap_or_else(|e| panic!("call trapped: {e}"));
    results[0]
}

fn call_i32(store: &mut wasmtime::Store<()>, func: &wasmtime::Func, args: &[wasmtime::Val]) -> i32 {
    match call_one(store, func, args) {
        wasmtime::Val::I32(v) => v,
        other => panic!("expected an i32 result, got {other:?}"),
    }
}

fn small_carrier(h: &mut Harness, v: i64) -> wasmtime::Val {
    call_one(&mut h.store, &h.from_i64.clone(), &[wasmtime::Val::I64(v)])
}

/// Build the carrier for an arbitrary `i128` through the module's own
/// arithmetic: `v = q * 2^62 + r` with `0 <= r < 2^62`, both halves i64.
fn carrier_of(h: &mut Harness, v: i128) -> wasmtime::Val {
    let base = 1_i128 << 62;
    let q = v.div_euclid(base);
    let r = v.rem_euclid(base);
    let hi = small_carrier(h, i64::try_from(q).expect("quotient fits an i64"));
    let lo = small_carrier(h, i64::try_from(r).expect("remainder fits an i64"));
    call_one(&mut h.store, &h.compose.clone(), &[hi, lo])
}

/// Whether a carrier is in the Small (no magnitude array) representation.
fn is_small(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> bool {
    let anyref = match val {
        wasmtime::Val::AnyRef(Some(a)) => *a,
        other => panic!("expected an Int carrier, got {other:?}"),
    };
    let structref = anyref
        .as_struct(&mut *store)
        .expect("anyref-to-struct query")
        .expect("Int carrier is not a struct");
    matches!(
        structref.field(&mut *store, 1).expect("read $magf field"),
        wasmtime::Val::AnyRef(None)
    )
}

/// The value set: zero, small magnitudes, both signs, the `2^62` limb cut, the
/// `±2^63 ± 1` Small/Big cut, and values two and six limbs wide.
const VALUES: &[i128] = &[
    0,
    1,
    -1,
    2,
    -2,
    7,
    -7,
    1 << 62,
    -(1 << 62),
    (1 << 62) + 1,
    i64::MAX as i128,       // 2^63 - 1, the last Small
    (i64::MIN as i128) + 1, // -2^63 + 1
    i64::MIN as i128,       // -2^63, the first Small going down
    1 << 63,                // 2^63, the first Big going up
    (1 << 63) + 1,          // 2^63 + 1
    -(1 << 63) - 1,         // -2^63 - 1, the first Big going down
    1 << 64,
    -(1 << 64),
    (1 << 64) + 1,
    1 << 100,
    -(1 << 100),
    (1 << 100) + 1,
];

/// The carrier's Small band is exactly the i64 range; everything else carries
/// a magnitude array. Stated here so the class assertions below are a CHECK of
/// the normalisation, not a restatement of it.
fn expected_small(v: i128) -> bool {
    i64::try_from(v).is_ok()
}

#[test]
fn the_value_set_really_covers_all_three_operand_class_pairs() {
    let mut h = harness();
    let mut small = 0usize;
    let mut big = 0usize;
    for &v in VALUES {
        let carrier = carrier_of(&mut h, v);
        let observed = is_small(&mut h.store, &carrier);
        assert_eq!(
            observed,
            expected_small(v),
            "carrier class of {v} diverged from the i64 Small band"
        );
        if observed {
            small += 1;
        } else {
            big += 1;
        }
    }
    assert!(
        small >= 2 && big >= 2,
        "the value set must exercise Small/Small, Small/Big and Big/Big \
         (saw {small} Small and {big} Big)"
    );
}

#[test]
fn cmp_helper_matches_the_relational_contract_on_every_pair() {
    let mut h = harness();
    for &a in VALUES {
        for &b in VALUES {
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            let got = call_i32(&mut h.store, &h.cmp.clone(), &[ca, cb]);
            assert_eq!(
                got,
                cmp_model(a, b),
                "__aint_cmp({a}, {b}) diverged from the cmpW contract"
            );
        }
    }
}

#[test]
fn eq_helper_matches_the_relational_contract_on_every_pair() {
    let mut h = harness();
    for &a in VALUES {
        for &b in VALUES {
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            let got = call_i32(&mut h.store, &h.eq.clone(), &[ca, cb]);
            assert_eq!(
                got,
                eq_model(a, b),
                "__aint_eq({a}, {b}) diverged from the eqW contract"
            );
        }
    }
}

/// Equal magnitude, opposite sign: the arm of `__aint_cmp` that decides on the
/// signs alone, without ever consulting the magnitudes.
#[test]
fn cmp_helper_orders_equal_magnitudes_by_sign() {
    let mut h = harness();
    for &v in VALUES {
        if v <= 0 {
            continue;
        }
        let pos = carrier_of(&mut h, v);
        let neg = carrier_of(&mut h, -v);
        assert_eq!(
            call_i32(&mut h.store, &h.cmp.clone(), &[pos, neg]),
            1,
            "__aint_cmp({v}, {}) must be 1",
            -v
        );
        assert_eq!(
            call_i32(&mut h.store, &h.cmp.clone(), &[neg, pos]),
            -1,
            "__aint_cmp({}, {v}) must be -1",
            -v
        );
        assert_eq!(
            call_i32(&mut h.store, &h.eq.clone(), &[pos, neg]),
            0,
            "__aint_eq({v}, {}) must be 0",
            -v
        );
    }
}

#[test]
fn the_three_faces_match_their_models_on_every_pair() {
    let mut h = harness();
    for &a in VALUES {
        for &b in VALUES {
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            assert_eq!(
                call_i32(&mut h.store, &h.at_least.clone(), &[ca, cb]),
                at_least_model(a, b),
                "atLeast({a}, {b}) diverged from the cmp-plus-relop face model"
            );
            assert_eq!(
                call_i32(&mut h.store, &h.same_key.clone(), &[ca, cb]),
                eq_model(a, b),
                "sameKey({a}, {b}) diverged from the eq face model"
            );
            // The selection face returns one of its ARGUMENTS unchanged, so the
            // result is checked by identity of value against the expected
            // argument — through both helpers, each of which the two tests above
            // have already pinned to its contract independently.
            let picked = call_one(&mut h.store, &h.min_int.clone(), &[ca, cb]);
            let expected = carrier_of(&mut h, min_model(a, b));
            assert_eq!(
                call_i32(&mut h.store, &h.eq.clone(), &[picked, expected]),
                1,
                "minInt({a}, {b}) did not return the model's choice {}",
                min_model(a, b)
            );
            assert_eq!(
                call_i32(&mut h.store, &h.cmp.clone(), &[picked, expected]),
                0,
                "minInt({a}, {b}) did not return the model's choice {}",
                min_model(a, b)
            );
        }
    }
}
