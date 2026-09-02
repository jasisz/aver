#![cfg(feature = "wasm")]

//! Differential harness for the Int carrier's runtime contracts: the two
//! value-comparison helpers `__aint_cmp` / `__aint_eq`, and the canonicity the
//! three arithmetic helpers promise about their results.
//!
//! The certificate wall assumes ONE law per comparison helper
//! (`CertPrelude.cmpW` / `CertPrelude.eqW`): applied to a CANONICAL CARRIER
//! PAIR — two represented operands in the runtime's normal form — the
//! three-way helper returns exactly `-1`/`0`/`1` according to `a < b` /
//! `a = b` / `a > b`, and the equality helper returns exactly `1`/`0`. It also
//! assumes that `Int.add` / `Int.sub` / `Int.mul` return a carrier that is
//! itself canonical. Nothing in the wall interprets any of those bodies: they
//! are pinned to a byte template and the laws are assumed of whatever sits at
//! the pinned index. So the laws have to be tested against the real machine,
//! and this is where that happens.
//!
//! The harness runs the REAL compiled artifact under wasmtime over a value set
//! that crosses every boundary the two-limb-plus-sign carrier has. Since the
//! contracts became canonical rather than small-band, EVERY row here is
//! contract evidence, not merely a runtime regression:
//!  * Small/Small, Small/Big and Big/Big operand pairs (the class of each
//!    operand is READ OFF the carrier, not assumed, and the test asserts that
//!    all three combinations really occur);
//!  * negatives, zero, and equal-magnitude-opposite-sign pairs (the arm of
//!    `__aint_cmp` that negates the unsigned magnitude verdict);
//!  * `±2^63 ± 1` — the exact Small/Big cut, where a one-off in the carrier's
//!    normalisation would flip a class and so break canonicity itself.
//!
//! Canonicity is checked directly, as the wall states it
//! (`CarrierSpec.canonSmall` / `CarrierSpec.canonBig`): every carrier the
//! runtime builds is Small exactly when its value fits the i64 band, and
//! otherwise carries limbs. The check DECODES the carrier's own fields — for a
//! Big, the whole magnitude array, enforcing the tight normal form of
//! `src/codegen/wasm_gc/builtins/bignum.rs` (non-empty, 32-bit magnitude
//! digits, non-zero top limb, sign in `{-1, 1}`) and reconstructing the signed
//! value — and compares it against an i128 oracle computed in Rust from the
//! operands. Nothing on that path calls a helper of the module under test, so
//! a regression shared by a producer and by `__aint_cmp`/`__aint_eq` (a
//! leading zero limb on every Big result, say) cannot pass it.
//!
//! Canonicity rows cover every carrier producer a certified comparison can be
//! fed from: the boxing helper, add/sub/mul, Euclidean `Int.div`/`Int.mod`,
//! the power-of-two producer behind `Bits.shiftLeft`, and the bitwise reader.
//!
//! The three faces built on the two comparison roles are checked the same way:
//! `atLeast` (cmp + `i32.ge_s`), `sameKey` (eq alone) and `minInt` (cmp +
//! select) are run against the Rust twins of their face models. The inline
//! sign template gets its own pair of exports (`productNonNeg`,
//! `productNonPos`) over products that really are limb-carrying, so its
//! `magf != null` arm — which decides on the sign field alone, and which no
//! earlier harness ever executed — is exercised on both operators.

use aver::codegen::wasm_gc::compile_to_wasm_gc;
use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::source::parse_source;

const SOURCE: &str = r#"
module IntCmpDiff
    intent =
        "Differential fixture for the two Int value-comparison host contracts"
        "and the three faces built on them."
    exposes [compose, addI, subI, mulI, divBy7, modBy7, shiftUp, andWith, atLeast, sameKey, minInt, productNonNeg, productNonPos]
    effects []

fn compose(hi: Int, lo: Int) -> Int
    ? "Assemble an arbitrary-width Int from a 2^62-radix quotient/remainder pair."
    hi * 4611686018427387904 + lo

fn addI(a: Int, b: Int) -> Int
    ? "Drives the add helper so its result carrier can be inspected."
    a + b

fn subI(a: Int, b: Int) -> Int
    ? "Drives the sub helper so its result carrier can be inspected."
    a - b

fn mulI(a: Int, b: Int) -> Int
    ? "Drives the mul helper so its result carrier can be inspected."
    a * b

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

fn divBy7(a: Int) -> Int
    ? "Drives the Euclidean division helper so its result carrier can be inspected."
    Int.div(a, 7)

fn modBy7(a: Int) -> Int
    ? "Drives the Euclidean remainder helper so its result carrier can be inspected."
    Int.mod(a, 7)

fn shiftUp(a: Int) -> Int
    ? "Drives the power-of-two producer: a left shift is a multiplication by 2^33."
    Bits.shiftLeft(a, 33)

fn andWith(a: Int, b: Int) -> Int
    ? "Drives the bitwise producer under infinite two's complement."
    Bits.and(a, b)

fn productNonNeg(a: Int, b: Int) -> Bool
    ? "The inline sign template on a COMPUTED operand: the product is compared against the literal zero without calling the three-way helper."
    a * b >= 0

fn productNonPos(a: Int, b: Int) -> Bool
    ? "The same template with the other operator."
    a * b <= 0
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

/// Rust twin of the inline sign template's source meaning
/// (`RecordComputeBridge.symIntCmpDenote` at `ge` / `le`): the COMPUTED
/// operand compared against the literal zero. Deliberately not routed through
/// `cmp_model` — the template exists precisely so that no helper is called.
fn product_non_neg_model(a: i128, b: i128) -> i32 {
    i32::from(a * b >= 0)
}

fn product_non_pos_model(a: i128, b: i128) -> i32 {
    i32::from(a * b <= 0)
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
            run_string_index: false,
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
    cmp: wasmtime::Func,
    eq: wasmtime::Func,
    compose: wasmtime::Func,
    add_i: wasmtime::Func,
    sub_i: wasmtime::Func,
    mul_i: wasmtime::Func,
    div_by_7: wasmtime::Func,
    mod_by_7: wasmtime::Func,
    shift_up: wasmtime::Func,
    and_with: wasmtime::Func,
    at_least: wasmtime::Func,
    same_key: wasmtime::Func,
    min_int: wasmtime::Func,
    product_non_neg: wasmtime::Func,
    product_non_pos: wasmtime::Func,
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
    let add_i = func(&mut store, "addI");
    let sub_i = func(&mut store, "subI");
    let mul_i = func(&mut store, "mulI");
    let div_by_7 = func(&mut store, "divBy7");
    let mod_by_7 = func(&mut store, "modBy7");
    let shift_up = func(&mut store, "shiftUp");
    let and_with = func(&mut store, "andWith");
    let at_least = func(&mut store, "atLeast");
    let same_key = func(&mut store, "sameKey");
    let min_int = func(&mut store, "minInt");
    let product_non_neg = func(&mut store, "productNonNeg");
    let product_non_pos = func(&mut store, "productNonPos");
    Harness {
        store,
        from_i64,
        cmp,
        eq,
        compose,
        add_i,
        sub_i,
        mul_i,
        div_by_7,
        mod_by_7,
        shift_up,
        and_with,
        at_least,
        same_key,
        min_int,
        product_non_neg,
        product_non_pos,
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

/// Read one field of an Int carrier.
fn carrier_field(
    store: &mut wasmtime::Store<()>,
    val: &wasmtime::Val,
    index: usize,
) -> wasmtime::Val {
    let anyref = match val {
        wasmtime::Val::AnyRef(Some(a)) => *a,
        other => panic!("expected an Int carrier, got {other:?}"),
    };
    let structref = anyref
        .as_struct(&mut *store)
        .expect("anyref-to-struct query")
        .expect("Int carrier is not a struct");
    structref
        .field(&mut *store, index)
        .expect("read Int carrier field")
}

/// Whether a carrier is in the Small (no magnitude array) representation.
fn is_small(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> bool {
    matches!(carrier_field(store, val, 1), wasmtime::Val::AnyRef(None))
}

/// The carrier's `$small` i64 field.
fn small_field(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> i64 {
    match carrier_field(store, val, 0) {
        wasmtime::Val::I64(v) => v,
        other => panic!("expected an i64 $small field, got {other:?}"),
    }
}

/// The carrier's `$sign` i32 field.
fn sign_field(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> i32 {
    match carrier_field(store, val, 2) {
        wasmtime::Val::I32(v) => v,
        other => panic!("expected an i32 $sign field, got {other:?}"),
    }
}

/// The carrier's `$magf` limb array, element by element, or `None` for a Small.
fn limb_array(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> Option<Vec<i64>> {
    let anyref = match carrier_field(store, val, 1) {
        wasmtime::Val::AnyRef(Some(a)) => a,
        wasmtime::Val::AnyRef(None) => return None,
        other => panic!("expected the carrier's limb field, got {other:?}"),
    };
    let arrayref = anyref
        .as_array(&mut *store)
        .expect("anyref-to-array query")
        .expect("the carrier's limb field is not an array");
    let len = arrayref
        .len(&mut *store)
        .expect("read the limb array length");
    let mut limbs = Vec::with_capacity(len as usize);
    for index in 0..len {
        match arrayref.get(&mut *store, index).expect("read a limb") {
            wasmtime::Val::I64(v) => limbs.push(v),
            other => panic!("expected an i64 limb, got {other:?}"),
        }
    }
    Some(limbs)
}

/// The integer a carrier denotes, decoded from its FIELDS and nothing else —
/// no helper of the module under test is called, so a regression shared by the
/// producer and by `__aint_cmp`/`__aint_eq` cannot hide behind this oracle.
///
/// A Small denotes its `$small` field. A Big denotes
/// `$sign * Σ limb_i * 2^(32i)`, and the decode also enforces the TIGHT normal
/// form documented in `src/codegen/wasm_gc/builtins/bignum.rs`: the magnitude
/// array is non-empty, every limb is a 32-bit magnitude digit (the internal
/// radix), the top limb is non-zero, and the sign is `-1` or `1`. A leading
/// zero limb appended to every Big result — the exact shared regression a
/// round-trip oracle would miss — fails here.
fn decode_carrier(store: &mut wasmtime::Store<()>, val: &wasmtime::Val, label: &str) -> i128 {
    let Some(limbs) = limb_array(store, val) else {
        return i128::from(small_field(store, val));
    };
    assert!(
        !limbs.is_empty(),
        "{label}: a limb-carrying carrier has an empty magnitude array"
    );
    for (index, limb) in limbs.iter().enumerate() {
        assert!(
            (0..(1_i64 << 32)).contains(limb),
            "{label}: limb {index} is {limb}, outside the 32-bit magnitude radix"
        );
    }
    assert_ne!(
        *limbs.last().expect("the magnitude array is non-empty"),
        0,
        "{label}: the top limb is zero, so the magnitude is not tight"
    );
    let mut magnitude: i128 = 0;
    for limb in limbs.iter().rev() {
        magnitude = magnitude
            .checked_mul(1_i128 << 32)
            .and_then(|shifted| shifted.checked_add(i128::from(*limb)))
            .unwrap_or_else(|| panic!("{label}: magnitude does not fit the test's i128 oracle"));
    }
    let sign = sign_field(store, val);
    assert!(
        sign == 1 || sign == -1,
        "{label}: canonical Big carrier must carry a sign of 1 or -1, got {sign}"
    );
    i128::from(sign) * magnitude
}

/// The wall's `CarrierSpec.Canon`, read off a real carrier: Small exactly on
/// the i64 band, limb-carrying otherwise — and, either way, denoting exactly
/// the value the caller's own i128 oracle says it should. `label` names the row.
fn assert_canonical(
    store: &mut wasmtime::Store<()>,
    val: &wasmtime::Val,
    value: i128,
    label: &str,
) {
    assert_eq!(
        is_small(store, val),
        i64::try_from(value).is_ok(),
        "{label} = {value}: carrier class is not the i64 band (canonSmall/canonBig)"
    );
    assert_eq!(
        decode_carrier(store, val, label),
        value,
        "{label}: the carrier's own fields denote a different integer than {value}"
    );
}

/// The value set: zero, small magnitudes, both signs, the `2^62` limb cut, the
/// `±2^63 ± 1` Small/Big cut, and values two and six limbs wide. Every value
/// here is one the runtime can build, so every row is inside the certified
/// (canonical) domain — the band cut matters because it is where canonicity
/// changes shape, not because it bounds the claim.
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

/// The carrier's Small band is exactly the i64 range — the band
/// `CarrierSpec.canonSmall` / `canonBig` split the normal form on — and
/// everything else carries a magnitude array. Stated here so the class
/// assertions below are a CHECK of the normalisation, not a restatement of it.
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

/// Every carrier the runtime hands the rest of the module is canonical: the
/// boxing helper's output, and the outputs of the three arithmetic helpers.
/// This is the empirical half of `CarrierSpec.canonSmall` / `canonBig` and of
/// the `∧ Canon w` conclusion the three arithmetic contracts carry.
#[test]
fn every_carrier_the_runtime_builds_is_canonical() {
    let mut h = harness();
    for &v in VALUES {
        let carrier = carrier_of(&mut h, v);
        assert_canonical(&mut h.store, &carrier, v, "compose");
        if let Ok(small) = i64::try_from(v) {
            let boxed = small_carrier(&mut h, small);
            assert_canonical(&mut h.store, &boxed, v, "__rt_aint_from_i64");
        }
    }
    for &a in VALUES {
        for &b in VALUES {
            for (label, func, expected) in [
                ("add", h.add_i.clone(), a.checked_add(b)),
                ("sub", h.sub_i.clone(), a.checked_sub(b)),
                ("mul", h.mul_i.clone(), a.checked_mul(b)),
            ] {
                // A row whose true result leaves `i128` cannot be modelled
                // here; the carrier itself is unbounded, the harness is not.
                let Some(expected) = expected else { continue };
                if i64::try_from(expected.div_euclid(1_i128 << 62)).is_err() {
                    continue;
                }
                let ca = carrier_of(&mut h, a);
                let cb = carrier_of(&mut h, b);
                let got = call_one(&mut h.store, &func, &[ca, cb]);
                // `assert_canonical` decodes the result carrier's own fields,
                // so this compares the helper against an oracle that touches
                // no helper. The earlier cross-check through `__aint_cmp` /
                // `__aint_eq` is deliberately gone: those decide structurally,
                // so a regression shared by the producer and by them (a
                // leading zero limb on every Big, say) passed it. They are
                // pinned against their contracts by their own tests below.
                assert_canonical(&mut h.store, &got, expected, &format!("{label}({a}, {b})"));
            }
        }
    }
}

/// The other carrier producers that feed the assumed-canonical inputs of a
/// certified comparison: Euclidean division and remainder, the power-of-two
/// producer behind a left shift, and the bitwise reader. Every one of them has
/// its own limb path and its own normalisation epilogue, so each needs its own
/// canonicity row. The values cross both band edges.
#[test]
fn the_other_carrier_producers_are_canonical_too() {
    let mut h = harness();
    for &v in VALUES {
        let carrier = carrier_of(&mut h, v);
        assert_canonical(&mut h.store, &carrier, v, "compose");

        let arg = carrier_of(&mut h, v);
        let quotient = call_one(&mut h.store, &h.div_by_7.clone(), &[arg]);
        assert_canonical(
            &mut h.store,
            &quotient,
            v.div_euclid(7),
            &format!("divBy7({v})"),
        );

        let arg = carrier_of(&mut h, v);
        let remainder = call_one(&mut h.store, &h.mod_by_7.clone(), &[arg]);
        assert_canonical(
            &mut h.store,
            &remainder,
            v.rem_euclid(7),
            &format!("modBy7({v})"),
        );

        // `Bits.shiftLeft(x, 33)` is `x * 2^33`, produced through the
        // power-of-two helper. Skip the rows whose true result leaves i128.
        if let Some(shifted) = v.checked_mul(1_i128 << 33) {
            let arg = carrier_of(&mut h, v);
            let got = call_one(&mut h.store, &h.shift_up.clone(), &[arg]);
            assert_canonical(&mut h.store, &got, shifted, &format!("shiftUp({v})"));
        }
    }

    // The bitwise reader is infinite two's complement, which is exactly what
    // `i128::bitand` computes on values that fit.
    for &a in VALUES {
        for &b in VALUES {
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            let got = call_one(&mut h.store, &h.and_with.clone(), &[ca, cb]);
            assert_canonical(&mut h.store, &got, a & b, &format!("andWith({a}, {b})"));
        }
    }
}

/// Operand pairs whose PRODUCT is limb-carrying, so the inline sign template
/// has to take its `magf != null` arm and decide on the sign field alone. Both
/// signs on both factors, plus zero and small controls for the Small arm and
/// for the equality boundary.
const SIGN_TEMPLATE_FACTORS: &[i128] = &[
    0,
    1,
    -1,
    3,
    -3,
    1 << 40,
    -(1 << 40),
    (1 << 40) + 7,
    -((1 << 40) + 7),
    1 << 41,
    -(1 << 41),
];

/// The inline sign template on a COMPUTED operand, over products that really
/// are limb-carrying. Until this test the template's `magf != null` arm was
/// never executed by any harness: every fixture compared a product that fit
/// the i64 band, so only the `i64.ge_s` arm ran.
#[test]
fn the_sign_template_matches_its_model_on_limb_carrying_products() {
    let mut h = harness();
    let mut big_products = 0usize;
    let mut small_products = 0usize;
    for &a in SIGN_TEMPLATE_FACTORS {
        for &b in SIGN_TEMPLATE_FACTORS {
            let product = a * b;
            if expected_small(product) {
                small_products += 1;
            } else {
                big_products += 1;
            }
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            assert_eq!(
                call_i32(&mut h.store, &h.product_non_neg.clone(), &[ca, cb]),
                product_non_neg_model(a, b),
                "productNonNeg({a}, {b}) diverged from the sign template's model \
                 (product {product})"
            );
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            assert_eq!(
                call_i32(&mut h.store, &h.product_non_pos.clone(), &[ca, cb]),
                product_non_pos_model(a, b),
                "productNonPos({a}, {b}) diverged from the sign template's model \
                 (product {product})"
            );
            // The product itself must be canonical, which is what makes the
            // template's sign-field arm exact in the first place.
            let ca = carrier_of(&mut h, a);
            let cb = carrier_of(&mut h, b);
            let got = call_one(&mut h.store, &h.mul_i.clone(), &[ca, cb]);
            assert_canonical(&mut h.store, &got, product, &format!("mul({a}, {b})"));
        }
    }
    assert!(
        big_products >= 16 && small_products >= 16,
        "the factor set must exercise BOTH template arms \
         (saw {big_products} limb-carrying and {small_products} small products)"
    );
}

#[test]
fn cmp_helper_matches_the_contract_on_every_pair() {
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
fn eq_helper_matches_the_contract_on_every_pair() {
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

/// The three faces against their models, over the same canonical value set.
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
