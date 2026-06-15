//! bignum slice 1 — arbitrary-precision `Int` WAT helpers for wasm-gc.
//!
//! wasm-gc is the last backend still wrapping `i64`. VM and Rust are
//! `Int = ℤ` via `aver-rt::AverInt`; wasm-gc emits WebAssembly GC
//! directly and does not link aver-rt, so the small-int-optimized
//! bignum lives here, IN the emitter, matching `aver-rt/src/int.rs`
//! semantics EXACTLY.
//!
//! ## Representation (mirrors `AverInt`)
//!
//! ```text
//! (struct $aint
//!   (field $small (mut i64))                    ;; value when $magf == null
//!   (field $magf  (mut (ref null (array i64))))  ;; magnitude limbs
//!   (field $sign  (mut i32)))                    ;; -1 / 0 / +1, Big only
//! ```
//!
//! - `$magf == null` → **Small**: value is `$small` (the i64 fast path).
//! - `$magf != null` → **Big**: `$sign ∈ {-1,+1}`, `$magf` the
//!   little-endian unsigned magnitude, canonical (no leading-zero
//!   limbs, top limb non-zero).
//!
//! ### Limb radix (an internal detail, invisible to the ABI)
//!
//! The struct field type is `(array i64)`, but each limb carries only
//! **32 bits** of magnitude (`0..2^32`). This is a deliberate
//! implementation choice confined to these helpers: with 32-bit limbs a
//! limb add `a+b+carry` and a limb multiply `a*b` both fit losslessly in
//! a 64-bit lane, so carry/borrow propagation is exact with no
//! add-with-carry primitive (wasm has none). The `$AverInt` struct shape
//! is identical to the plan's — only the radix used inside decompose /
//! recompose / arithmetic differs, and nothing outside this module ever
//! observes a limb.
//!
//! ## Canonical invariant (non-negotiable)
//!
//! Any value that fits `i64` is ALWAYS Small. Every helper that can
//! produce a Big funnels through the inlined normalize epilogue, which
//! demotes an in-range magnitude back to Small — mirroring
//! `AverInt::from_bigint`. Eq / Ord / (future) Hash depend on this.
//!
//! ## Single-function discipline (the trap)
//!
//! `compile_wat_helper` keeps only the FIRST function's body and
//! discards the rest, AND does not rewrite call indices. So every helper
//! is ONE function with NO `call` to any sibling — all limb routines are
//! inlined. A stray `call` would be a silent miscompile only wasmtime
//! catches.
//!
//! ## Overflow detection on the i64 fast path (no wasm flag)
//!
//! - ADD: `(a^s) & (b^s) < 0`, `s = a+b`.
//! - SUB: `(a^b) & (a^d) < 0`, `d = a-b`.
//! - MUL: round-trip-divide oracle `p == 0 || (a != 0 && p/a == b)`,
//!   guarding `a == 0` and the `a == -1 && b == i64::MIN` `div_s` trap.
//! - NEG: overflow only at `i64::MIN`.

use wasm_encoder::Function;

use super::super::WasmGcError;
use super::super::types::TypeRegistry;
use super::super::wat_helper;

/// The `$mag` / `$aint` type declarations a bignum helper WAT needs, at
/// the exact indices the user module's TypeRegistry recorded, so the
/// spliced-in body's `struct.new $aint` / `array.* $mag` reference the
/// right slots. `padding_types(mag_idx)` reserves `0..mag_idx`; the mag
/// array lands at `mag_idx` and the struct at `mag_idx + 1`.
fn aint_type_decls(registry: &TypeRegistry) -> Result<String, WasmGcError> {
    let mag_idx = registry.aint_mag_array_idx.ok_or(WasmGcError::Validation(
        "bignum helper needs the $AverInt magnitude array slot, but it wasn't allocated".into(),
    ))?;
    let struct_idx = registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "bignum helper needs the $AverInt struct slot, but it wasn't allocated".into(),
    ))?;
    debug_assert_eq!(struct_idx, mag_idx + 1, "struct must sit right above mag");
    let padding = wat_helper::padding_types(mag_idx);
    Ok(format!(
        "{padding}\
         (type $mag (array (mut i64)))\n\
         (type $aint (struct (field $small (mut i64)) \
                             (field $magf (mut (ref null $mag))) \
                             (field $sign (mut i32))))\n"
    ))
}

/// Inlined WAT: decompose `$AverInt` operand local `$op` into a non-null
/// 32-bit-limb magnitude array local `$opm` (little-endian, leading
/// zeros possible) and a sign local `$ops` (-1/0/+1). A Small splits
/// `|small|` into two 32-bit limbs; zero → sign 0 + empty magnitude.
/// `|i64::MIN|` is recovered via `0 - small` (wrapping), giving the
/// correct unsigned `2^63` whose low/high 32-bit halves are then split.
fn decompose(op: &str, opm: &str, ops: &str, umag: &str) -> String {
    // $umag is an i64 scratch holding |small| during the split.
    format!(
        r#"
            (if (ref.is_null (struct.get $aint $magf (local.get ${op})))
              (then
                (if (i64.eqz (struct.get $aint $small (local.get ${op})))
                  (then
                    (local.set ${ops} (i32.const 0))
                    (local.set ${opm} (array.new_default $mag (i32.const 0))))
                  (else
                    (local.set ${ops}
                      (if (result i32) (i64.lt_s (struct.get $aint $small (local.get ${op})) (i64.const 0))
                        (then (i32.const -1)) (else (i32.const 1))))
                    (local.set ${umag}
                      (if (result i64) (i32.lt_s (local.get ${ops}) (i32.const 0))
                        (then (i64.sub (i64.const 0) (struct.get $aint $small (local.get ${op}))))
                        (else (struct.get $aint $small (local.get ${op})))))
                    (local.set ${opm} (array.new_default $mag (i32.const 2)))
                    (array.set $mag (local.get ${opm}) (i32.const 0)
                      (i64.and (local.get ${umag}) (i64.const 0xffffffff)))
                    (array.set $mag (local.get ${opm}) (i32.const 1)
                      (i64.shr_u (local.get ${umag}) (i64.const 32))))))
              (else
                (local.set ${opm} (struct.get $aint $magf (local.get ${op})))
                (local.set ${ops} (struct.get $aint $sign (local.get ${op}))))) "#
    )
}

/// Inlined WAT normalize epilogue. Takes a working 32-bit-limb magnitude
/// array local `$rm` (leading zeros allowed) and a raw sign local `$rs`,
/// leaves a canonical `$AverInt` on the stack. Scratch: `$rlen $i $hi
/// $lo` (`$rlen $i` i32, `$hi $lo` i64) plus result-array local `$tmpm`.
///
/// Strips leading zeros; magnitude 0 → Small(0); a magnitude of ≤2 limbs
/// whose 64-bit value fits the signed-i64 range for `$rs` → Small (incl.
/// the lone `i64::MIN`); else a tight Big.
fn normalize(rm: &str, rs: &str, rlen: &str, i: &str, lo: &str, hi: &str, tmpm: &str) -> String {
    format!(
        r#"
            ;; strip leading zero limbs
            (local.set ${rlen} (array.len (local.get ${rm})))
            (block $strip_done (loop $strip
              (br_if $strip_done (i32.eqz (local.get ${rlen})))
              (br_if $strip_done
                (i64.ne (array.get $mag (local.get ${rm}) (i32.sub (local.get ${rlen}) (i32.const 1))) (i64.const 0)))
              (local.set ${rlen} (i32.sub (local.get ${rlen}) (i32.const 1)))
              (br $strip)))
            (if (result (ref null $aint)) (i32.eqz (local.get ${rlen}))
              (then
                (struct.new $aint (i64.const 0) (ref.null $mag) (i32.const 0)))
              (else
                (if (result (ref null $aint)) (i32.le_u (local.get ${rlen}) (i32.const 2))
                  (then
                    ;; reassemble ≤2 limbs into a 64-bit unsigned value
                    (local.set ${lo} (i64.and (array.get $mag (local.get ${rm}) (i32.const 0)) (i64.const 0xffffffff)))
                    (local.set ${hi}
                      (if (result i64) (i32.eq (local.get ${rlen}) (i32.const 2))
                        (then (i64.and (array.get $mag (local.get ${rm}) (i32.const 1)) (i64.const 0xffffffff)))
                        (else (i64.const 0))))
                    (local.set ${lo} (i64.or (local.get ${lo}) (i64.shl (local.get ${hi}) (i64.const 32))))
                    ;; $lo now holds the full magnitude as an unsigned i64.
                    (if (result (ref null $aint))
                        (i64.eqz (i64.shr_u (local.get ${lo}) (i64.const 63)))
                      (then
                        ;; top bit clear → fits i64::MAX either sign → Small
                        (struct.new $aint
                          (if (result i64) (i32.lt_s (local.get ${rs}) (i32.const 0))
                            (then (i64.sub (i64.const 0) (local.get ${lo}))) (else (local.get ${lo})))
                          (ref.null $mag) (i32.const 0)))
                      (else
                        ;; top bit set: only -2^63 (i64::MIN) demotes.
                        (if (result (ref null $aint))
                            (i32.and (i32.lt_s (local.get ${rs}) (i32.const 0))
                                     (i64.eq (local.get ${lo}) (i64.const 0x8000000000000000)))
                          (then
                            (struct.new $aint (i64.const 0x8000000000000000) (ref.null $mag) (i32.const 0)))
                          (else
                            {tight_big})))))
                  (else
                    {tight_big})))) "#,
        tight_big = tight_big(rm, rs, rlen, i, tmpm),
    )
}

/// Inlined WAT that copies the surviving `$rlen` limbs of `$rm` into a
/// tight array `$tmpm` and builds a Big `$AverInt` (used by `normalize`
/// for the genuinely-out-of-range case). `$i` is i32 scratch.
fn tight_big(rm: &str, rs: &str, rlen: &str, i: &str, tmpm: &str) -> String {
    format!(
        r#"
                        (local.set ${tmpm} (array.new_default $mag (local.get ${rlen})))
                        (local.set ${i} (i32.const 0))
                        (block $copy_done (loop $copy
                          (br_if $copy_done (i32.ge_u (local.get ${i}) (local.get ${rlen})))
                          (array.set $mag (local.get ${tmpm}) (local.get ${i})
                            (array.get $mag (local.get ${rm}) (local.get ${i})))
                          (local.set ${i} (i32.add (local.get ${i}) (i32.const 1)))
                          (br $copy)))
                        (struct.new $aint (i64.const 0) (local.get ${tmpm})
                          (if (result i32) (i32.lt_s (local.get ${rs}) (i32.const 0)) (then (i32.const -1)) (else (i32.const 1)))) "#
    )
}

/// Inlined unsigned-magnitude compare of arrays `$am` (stripped len
/// `$alen`) and `$bm` (stripped len `$blen`) → -1/0/1 into i32 `$cmp`.
/// i32 scratch `$j`.
fn umag_cmp(am: &str, alen: &str, bm: &str, blen: &str, cmp: &str, j: &str) -> String {
    format!(
        r#"
            (if (i32.ne (local.get ${alen}) (local.get ${blen}))
              (then
                (local.set ${cmp}
                  (if (result i32) (i32.gt_u (local.get ${alen}) (local.get ${blen})) (then (i32.const 1)) (else (i32.const -1)))))
              (else
                (local.set ${cmp} (i32.const 0))
                (local.set ${j} (local.get ${alen}))
                (block $ucmp_done (loop $ucmp
                  (br_if $ucmp_done (i32.eqz (local.get ${j})))
                  (local.set ${j} (i32.sub (local.get ${j}) (i32.const 1)))
                  (if (i64.ne (array.get $mag (local.get ${am}) (local.get ${j}))
                              (array.get $mag (local.get ${bm}) (local.get ${j})))
                    (then
                      (local.set ${cmp}
                        (if (result i32) (i64.gt_u (array.get $mag (local.get ${am}) (local.get ${j}))
                                                   (array.get $mag (local.get ${bm}) (local.get ${j})))
                          (then (i32.const 1)) (else (i32.const -1))))
                      (br $ucmp_done)))
                  (br $ucmp))))) "#
    )
}

/// Type declarations for the bignum formatter, which references the
/// `$string` array slot AS WELL AS `$mag` / `$aint`. The registry
/// allocates `string_idx < mag_idx < struct_idx`, so we pad to each in
/// turn: `padding_types(n)` injects `n` empty `(type (struct))` so the
/// NEXT declared type lands at index `n`. Returns the full WAT prelude.
fn aint_and_string_decls(registry: &TypeRegistry) -> Result<String, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "bignum String.fromInt needs the String slot, but it wasn't allocated".into(),
        ))?;
    let mag_idx = registry.aint_mag_array_idx.ok_or(WasmGcError::Validation(
        "bignum String.fromInt needs the $mag slot, but it wasn't allocated".into(),
    ))?;
    let struct_idx = registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "bignum String.fromInt needs the $aint slot, but it wasn't allocated".into(),
    ))?;
    if !(string_idx < mag_idx && mag_idx + 1 == struct_idx) {
        return Err(WasmGcError::Validation(format!(
            "bignum String.fromInt expects string({string_idx}) < mag({mag_idx}) and \
             struct({struct_idx}) == mag+1; layout invariant broken"
        )));
    }
    let pad_to_string = wat_helper::padding_types(string_idx);
    // After declaring $string we are at index string_idx+1; pad the gap
    // up to mag_idx, then declare $mag and $aint.
    let gap = wat_helper::padding_types(mag_idx - (string_idx + 1));
    Ok(format!(
        "{pad_to_string}\
         (type $string (array (mut i8)))\n\
         {gap}\
         (type $mag (array (mut i64)))\n\
         (type $aint (struct (field $small (mut i64)) \
                             (field $magf (mut (ref null $mag))) \
                             (field $sign (mut i32))))\n"
    ))
}

/// `String.fromInt` under bignum — formats an `$AverInt` to a decimal
/// `(ref null $string)`. Small reads `$small` and runs the same i64
/// digit loop the scalar helper uses; Big repeatedly divides the 32-bit
/// limb magnitude by 10 (the ONLY divmod in slice 1, and it is by a
/// constant), collecting remainder digits low→high, then reverses them
/// into the output array with a leading `-` for negative sign.
pub(super) fn emit_string_from_aint(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_and_string_decls(registry)?;
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result (ref null $string))
            (local $n i64) (local $abs i64) (local $copy i64)
            (local $digits i32) (local $total i32) (local $i i32) (local $neg i32)
            (local $arr (ref null $string))
            (local $magc (ref null $mag)) (local $len i32) (local $li i32)
            (local $cur i64) (local $rem i64) (local $q i64) (local $allzero i32)
            (local $digbuf (ref null $string)) (local $dcount i32)
            (if (result (ref null $string)) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then
                ;; ── Small: format $small as the scalar helper does ──
                (local.set $n (struct.get $aint $small (local.get $a)))
                (if (result (ref null $string)) (i64.eqz (local.get $n))
                  (then (array.new $string (i32.const 48) (i32.const 1)))
                  (else
                    (local.set $neg (if (result i32) (i64.lt_s (local.get $n) (i64.const 0)) (then (i32.const 1)) (else (i32.const 0))))
                    (local.set $abs (if (result i64) (local.get $neg) (then (i64.sub (i64.const 0) (local.get $n))) (else (local.get $n))))
                    ;; count digits
                    (local.set $copy (local.get $abs)) (local.set $digits (i32.const 0))
                    (block $cd (loop $c
                      (br_if $cd (i64.eqz (local.get $copy)))
                      (local.set $digits (i32.add (local.get $digits) (i32.const 1)))
                      (local.set $copy (i64.div_u (local.get $copy) (i64.const 10)))
                      (br $c)))
                    (local.set $total (i32.add (local.get $digits) (local.get $neg)))
                    (local.set $arr (array.new_default $string (local.get $total)))
                    (local.set $i (i32.sub (local.get $total) (i32.const 1)))
                    (local.set $copy (local.get $abs))
                    (block $fd (loop $f
                      (br_if $fd (i32.lt_s (local.get $i) (local.get $neg)))
                      (array.set $string (local.get $arr) (local.get $i)
                        (i32.add (i32.const 48) (i32.wrap_i64 (i64.rem_u (local.get $copy) (i64.const 10)))))
                      (local.set $copy (i64.div_u (local.get $copy) (i64.const 10)))
                      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                      (br $f)))
                    (if (local.get $neg) (then (array.set $string (local.get $arr) (i32.const 0) (i32.const 45))))
                    (local.get $arr))))
              (else
                ;; ── Big: divmod-by-10 over 32-bit limbs ──
                (local.set $neg (if (result i32) (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0)) (then (i32.const 1)) (else (i32.const 0))))
                ;; work on a mutable copy of the magnitude
                (local.set $len (array.len (struct.get $aint $magf (local.get $a))))
                (local.set $magc (array.new_default $mag (local.get $len)))
                (local.set $li (i32.const 0))
                (block $cpd (loop $cp
                  (br_if $cpd (i32.ge_u (local.get $li) (local.get $len)))
                  (array.set $mag (local.get $magc) (local.get $li)
                    (array.get $mag (struct.get $aint $magf (local.get $a)) (local.get $li)))
                  (local.set $li (i32.add (local.get $li) (i32.const 1)))
                  (br $cp)))
                ;; collect digits low→high into $digbuf (max ~10 digits/limb*len; len*10 is safe)
                (local.set $digbuf (array.new_default $string (i32.mul (local.get $len) (i32.const 10))))
                (local.set $dcount (i32.const 0))
                (block $dvd (loop $dv
                  ;; rem = 0; for li=len-1 down to 0: cur=(rem<<32)|limb; q=cur/10; rem=cur%10; limb=q
                  (local.set $rem (i64.const 0))
                  (local.set $li (local.get $len))
                  (block $dl (loop $d
                    (br_if $dl (i32.eqz (local.get $li)))
                    (local.set $li (i32.sub (local.get $li) (i32.const 1)))
                    (local.set $cur (i64.or (i64.shl (local.get $rem) (i64.const 32))
                                            (i64.and (array.get $mag (local.get $magc) (local.get $li)) (i64.const 0xffffffff))))
                    (local.set $q (i64.div_u (local.get $cur) (i64.const 10)))
                    (local.set $rem (i64.rem_u (local.get $cur) (i64.const 10)))
                    (array.set $mag (local.get $magc) (local.get $li) (local.get $q))
                    (br $d)))
                  ;; emit digit = rem
                  (array.set $string (local.get $digbuf) (local.get $dcount)
                    (i32.add (i32.const 48) (i32.wrap_i64 (local.get $rem))))
                  (local.set $dcount (i32.add (local.get $dcount) (i32.const 1)))
                  ;; loop while magnitude is non-zero
                  (local.set $allzero (i32.const 1))
                  (local.set $li (i32.const 0))
                  (block $zd (loop $z
                    (br_if $zd (i32.ge_u (local.get $li) (local.get $len)))
                    (if (i64.ne (array.get $mag (local.get $magc) (local.get $li)) (i64.const 0))
                      (then (local.set $allzero (i32.const 0)) (br $zd)))
                    (local.set $li (i32.add (local.get $li) (i32.const 1)))
                    (br $z)))
                  ;; exit when the magnitude has become all-zero; else loop.
                  (br_if $dvd (local.get $allzero))
                  (br $dv)))
                ;; build output: [neg '-'] then digits reversed
                (local.set $total (i32.add (local.get $dcount) (local.get $neg)))
                (local.set $arr (array.new_default $string (local.get $total)))
                (if (local.get $neg) (then (array.set $string (local.get $arr) (i32.const 0) (i32.const 45))))
                (local.set $i (i32.const 0))
                (block $od (loop $o
                  (br_if $od (i32.ge_u (local.get $i) (local.get $dcount)))
                  (array.set $string (local.get $arr)
                    (i32.add (local.get $neg) (local.get $i))
                    (array.get_u $string (local.get $digbuf) (i32.sub (i32.sub (local.get $dcount) (i32.const 1)) (local.get $i))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $o)))
                (local.get $arr)))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__aint_from_i64(n: i64) -> $AverInt` — canonical Small constructor.
pub(super) fn emit_aint_from_i64(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $n i64) (result (ref null $aint))
            (struct.new $aint (local.get $n) (ref.null $mag) (i32.const 0))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__aint_neg(a) -> $AverInt`. Small fast path with `i64::MIN`
/// promotion; Big flips the sign field (magnitude unchanged, stays
/// canonical).
pub(super) fn emit_aint_neg(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result (ref null $aint))
            (if (result (ref null $aint)) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then
                (if (result (ref null $aint))
                    (i64.eq (struct.get $aint $small (local.get $a)) (i64.const 0x8000000000000000))
                  (then
                    ;; -(i64::MIN) = 2^63 → Big, positive, two 32-bit limbs
                    ;; (low=0, high=0x80000000).
                    (struct.new $aint (i64.const 0)
                      (array.new_fixed $mag 2 (i64.const 0) (i64.const 0x80000000))
                      (i32.const 1)))
                  (else
                    (struct.new $aint
                      (i64.sub (i64.const 0) (struct.get $aint $small (local.get $a)))
                      (ref.null $mag) (i32.const 0)))))
              (else
                (struct.new $aint (i64.const 0)
                  (struct.get $aint $magf (local.get $a))
                  (i32.sub (i32.const 0) (struct.get $aint $sign (local.get $a))))))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__aint_eq(a, b) -> i32` (1/0). Leans on the canonical invariant:
/// equal Small ↔ equal $small; equal Big ↔ same sign + same limbs; a
/// Small and a Big are never equal.
pub(super) fn emit_aint_eq(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result i32)
            (local $am (ref null $mag)) (local $bm (ref null $mag))
            (local $i i32) (local $n i32) (local $eq i32)
            (local.set $am (struct.get $aint $magf (local.get $a)))
            (local.set $bm (struct.get $aint $magf (local.get $b)))
            (if (result i32) (ref.is_null (local.get $am))
              (then
                (if (result i32) (ref.is_null (local.get $bm))
                  (then (i64.eq (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b))))
                  (else (i32.const 0))))
              (else
                (if (result i32) (ref.is_null (local.get $bm))
                  (then (i32.const 0))
                  (else
                    (if (result i32) (i32.ne (struct.get $aint $sign (local.get $a)) (struct.get $aint $sign (local.get $b)))
                      (then (i32.const 0))
                      (else
                        (if (result i32) (i32.ne (array.len (local.get $am)) (array.len (local.get $bm)))
                          (then (i32.const 0))
                          (else
                            (local.set $n (array.len (local.get $am)))
                            (local.set $i (i32.const 0))
                            (local.set $eq (i32.const 1))
                            (block $done (loop $lp
                              (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                              (if (i64.ne (array.get $mag (local.get $am) (local.get $i))
                                          (array.get $mag (local.get $bm) (local.get $i)))
                                (then (local.set $eq (i32.const 0)) (br $done)))
                              (local.set $i (i32.add (local.get $i) (i32.const 1)))
                              (br $lp)))
                            (local.get $eq)))))))))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__aint_cmp(a, b) -> i32` (-1/0/1). Sign first, then unsigned
/// magnitude (flipped for two negatives). Mirrors `AverInt::cmp`.
pub(super) fn emit_aint_cmp(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let decomp_a = decompose("a", "am", "as_", "umag");
    let decomp_b = decompose("b", "bm", "bs", "umag");
    let strip_a = strip("am", "alen", "sa", "la");
    let strip_b = strip("bm", "blen", "sb", "lb");
    let cmp = umag_cmp("am", "alen", "bm", "blen", "cmp", "j");
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result i32)
            (local $am (ref null $mag)) (local $as_ i32)
            (local $bm (ref null $mag)) (local $bs i32)
            (local $alen i32) (local $blen i32)
            (local $cmp i32) (local $j i32) (local $umag i64)
            {decomp_a}
            {decomp_b}
            (if (result i32) (i32.ne (local.get $as_) (local.get $bs))
              (then
                (if (result i32) (i32.gt_s (local.get $as_) (local.get $bs)) (then (i32.const 1)) (else (i32.const -1))))
              (else
                (if (result i32) (i32.eqz (local.get $as_))
                  (then (i32.const 0))
                  (else
                    {strip_a}
                    {strip_b}
                    {cmp}
                    (if (result i32) (i32.lt_s (local.get $as_) (i32.const 0))
                      (then (i32.sub (i32.const 0) (local.get $cmp)))
                      (else (local.get $cmp)))))))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// Inlined leading-zero strip of array `$arr` into i32 len local `$len`,
/// using unique block/loop label names `$bl`/`$lp`.
fn strip(arr: &str, len: &str, bl: &str, lp: &str) -> String {
    format!(
        r#"
            (local.set ${len} (array.len (local.get ${arr})))
            (block ${bl} (loop ${lp}
              (br_if ${bl} (i32.eqz (local.get ${len})))
              (br_if ${bl} (i64.ne (array.get $mag (local.get ${arr}) (i32.sub (local.get ${len}) (i32.const 1))) (i64.const 0)))
              (local.set ${len} (i32.sub (local.get ${len}) (i32.const 1)))
              (br ${lp}))) "#
    )
}

/// Inlined signed-magnitude combine. Operands are decomposed locals
/// `(am, as_)` and `(bm, beff)` where `beff` is the EFFECTIVE sign of b
/// (caller passes `bs` for add, the negation `-bs` for sub). Produces a
/// working magnitude into `$rm` + raw sign into `$rs`, NOT normalized.
///
/// Assumes the caller has stripped `am`→`$alen`, `bm`→`$blen` and
/// declared scratch `$rlen $i $carry $cmp $j $borrow $diff` with the
/// types named below. 32-bit limbs make carry/borrow exact.
fn signed_combine() -> String {
    let cmp = umag_cmp("am", "alen", "bm", "blen", "cmp", "j");
    format!(
        r#"
            ;; signs "agree" when one is zero, or the two non-zero signs match.
            (if (i32.or (i32.eqz (local.get $as_))
                  (i32.or (i32.eqz (local.get $beff)) (i32.eq (local.get $as_) (local.get $beff))))
              (then
                ;; ── magnitude ADD ── rlen = max(alen,blen)+1
                (local.set $rlen (i32.add (i32.const 1)
                  (if (result i32) (i32.gt_u (local.get $alen) (local.get $blen)) (then (local.get $alen)) (else (local.get $blen)))))
                (local.set $rm (array.new_default $mag (local.get $rlen)))
                (local.set $i (i32.const 0))
                (local.set $carry (i64.const 0))
                (block $add_done (loop $add_lp
                  (br_if $add_done (i32.ge_u (local.get $i) (local.get $rlen)))
                  (local.set $carry (i64.add (local.get $carry)
                    (i64.add
                      (if (result i64) (i32.lt_u (local.get $i) (local.get $alen)) (then (array.get $mag (local.get $am) (local.get $i))) (else (i64.const 0)))
                      (if (result i64) (i32.lt_u (local.get $i) (local.get $blen)) (then (array.get $mag (local.get $bm) (local.get $i))) (else (i64.const 0))))))
                  (array.set $mag (local.get $rm) (local.get $i) (i64.and (local.get $carry) (i64.const 0xffffffff)))
                  (local.set $carry (i64.shr_u (local.get $carry) (i64.const 32)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $add_lp)))
                (local.set $rs (if (result i32) (i32.eqz (local.get $as_)) (then (local.get $beff)) (else (local.get $as_)))))
              (else
                ;; ── magnitude SUBTRACT (differing signs) ── larger - smaller
                {cmp}
                (if (i32.eqz (local.get $cmp))
                  (then
                    (local.set $rm (array.new_default $mag (i32.const 0)))
                    (local.set $rs (i32.const 0)))
                  (else
                    ;; result sign = sign of the larger magnitude operand
                    (local.set $rs (if (result i32) (i32.gt_s (local.get $cmp) (i32.const 0)) (then (local.get $as_)) (else (local.get $beff))))
                    (local.set $rlen (if (result i32) (i32.gt_u (local.get $alen) (local.get $blen)) (then (local.get $alen)) (else (local.get $blen))))
                    (local.set $rm (array.new_default $mag (local.get $rlen)))
                    (local.set $i (i32.const 0))
                    (local.set $borrow (i64.const 0))
                    (block $sub_done (loop $sub_lp
                      (br_if $sub_done (i32.ge_u (local.get $i) (local.get $rlen)))
                      ;; diff = larger_limb - smaller_limb - borrow, in 64 bits.
                      ;; "larger" is am if cmp>0 else bm.
                      (local.set $diff (i64.sub
                        (i64.sub
                          (if (result i64) (i32.gt_s (local.get $cmp) (i32.const 0))
                            (then (if (result i64) (i32.lt_u (local.get $i) (local.get $alen)) (then (array.get $mag (local.get $am) (local.get $i))) (else (i64.const 0))))
                            (else (if (result i64) (i32.lt_u (local.get $i) (local.get $blen)) (then (array.get $mag (local.get $bm) (local.get $i))) (else (i64.const 0)))))
                          (if (result i64) (i32.gt_s (local.get $cmp) (i32.const 0))
                            (then (if (result i64) (i32.lt_u (local.get $i) (local.get $blen)) (then (array.get $mag (local.get $bm) (local.get $i))) (else (i64.const 0))))
                            (else (if (result i64) (i32.lt_u (local.get $i) (local.get $alen)) (then (array.get $mag (local.get $am) (local.get $i))) (else (i64.const 0))))))
                        (local.get $borrow)))
                      ;; if diff < 0 (as signed) add 2^32 and set borrow=1
                      (if (i64.lt_s (local.get $diff) (i64.const 0))
                        (then
                          (local.set $diff (i64.add (local.get $diff) (i64.const 0x100000000)))
                          (local.set $borrow (i64.const 1)))
                        (else (local.set $borrow (i64.const 0))))
                      (array.set $mag (local.get $rm) (local.get $i) (i64.and (local.get $diff) (i64.const 0xffffffff)))
                      (local.set $i (i32.add (local.get $i) (i32.const 1)))
                      (br $sub_lp)))))))
    "#
    )
}

/// Common locals declaration shared by add/sub: decomposed operands,
/// lengths, carry/borrow scratch, and normalize scratch.
fn arith_locals() -> &'static str {
    r#"
            (local $am (ref null $mag)) (local $as_ i32)
            (local $bm (ref null $mag)) (local $bs i32) (local $beff i32)
            (local $alen i32) (local $blen i32) (local $rlen i32)
            (local $rm (ref null $mag)) (local $rs i32)
            (local $i i32) (local $j i32) (local $cmp i32)
            (local $carry i64) (local $borrow i64) (local $diff i64) (local $umag i64)
            (local $lo i64) (local $hi i64) (local $tmpm (ref null $mag)) "#
}

/// `__aint_add(a, b) -> $AverInt`. i64 fast path with `(a^s)&(b^s)<0`
/// overflow detection; on overflow OR either operand Big, decompose +
/// signed-magnitude add + normalize.
pub(super) fn emit_aint_add(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    emit_aint_addsub(registry, false)
}

/// `__aint_sub(a, b) -> $AverInt`. i64 fast path with `(a^b)&(a^d)<0`
/// overflow detection; slow path negates b's effective sign.
pub(super) fn emit_aint_sub(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    emit_aint_addsub(registry, true)
}

fn emit_aint_addsub(registry: &TypeRegistry, is_sub: bool) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let locals = arith_locals();
    let decomp_a = decompose("a", "am", "as_", "umag");
    let decomp_b = decompose("b", "bm", "bs", "umag");
    let strip_a = strip("am", "alen", "sa", "la");
    let strip_b = strip("bm", "blen", "sb", "lb");
    let combine = signed_combine();
    let norm = normalize("rm", "rs", "rlen", "i", "lo", "hi", "tmpm");
    // i64 fast-path: both Small.
    let (fast_op, overflow_check) = if is_sub {
        (
            "(i64.sub (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b)))",
            // (a^b) & (a^d) < 0, d = result
            "(i64.lt_s (i64.and (i64.xor (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b))) (i64.xor (struct.get $aint $small (local.get $a)) (local.get $r))) (i64.const 0))",
        )
    } else {
        (
            "(i64.add (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b)))",
            // (a^s) & (b^s) < 0, s = result
            "(i64.lt_s (i64.and (i64.xor (struct.get $aint $small (local.get $a)) (local.get $r)) (i64.xor (struct.get $aint $small (local.get $b)) (local.get $r))) (i64.const 0))",
        )
    };
    let beff_set = if is_sub {
        "(local.set $beff (i32.sub (i32.const 0) (local.get $bs)))"
    } else {
        "(local.set $beff (local.get $bs))"
    };
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result (ref null $aint))
            (local $r i64)
            {locals}
            ;; fast path: both Small.
            (if (result (ref null $aint))
                (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (local.set $r {fast_op})
                (if (result (ref null $aint)) {overflow_check}
                  (then
                    ;; overflow → slow path
                    {decomp_a}
                    {decomp_b}
                    {beff_set}
                    {strip_a}
                    {strip_b}
                    {combine}
                    {norm})
                  (else
                    (struct.new $aint (local.get $r) (ref.null $mag) (i32.const 0)))))
              (else
                ;; at least one Big → slow path
                {decomp_a}
                {decomp_b}
                {beff_set}
                {strip_a}
                {strip_b}
                {combine}
                {norm}))))
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__aint_mul(a, b) -> $AverInt`. i64 fast path with the round-trip-
/// divide overflow oracle (`p == 0 || (a != 0 && p/a == b)`), guarding
/// `a == 0` and the `a == -1 && b == i64::MIN` `div_s` trap. Slow path:
/// schoolbook 32-bit-limb magnitude multiply, sign = product of signs.
pub(super) fn emit_aint_mul(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let decls = aint_type_decls(registry)?;
    let locals = arith_locals();
    let decomp_a = decompose("a", "am", "as_", "umag");
    let decomp_b = decompose("b", "bm", "bs", "umag");
    let strip_a = strip("am", "alen", "sa", "la");
    let strip_b = strip("bm", "blen", "sb", "lb");
    let norm = normalize("rm", "rs", "rlen", "i", "lo", "hi", "tmpm");
    let wat = format!(
        r#"
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result (ref null $aint))
            (local $r i64) (local $av i64) (local $bv i64)
            (local $k i32) (local $prod i64) (local $ok i32)
            {locals}
            (if (result (ref null $aint))
                (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (local.set $av (struct.get $aint $small (local.get $a)))
                (local.set $bv (struct.get $aint $small (local.get $b)))
                (local.set $r (i64.mul (local.get $av) (local.get $bv)))
                ;; overflow oracle (mirrors i64::checked_mul): NO overflow
                ;; iff a == 0 (product genuinely 0), OR — a != 0 AND it is
                ;; not the `a == -1 && b == i64::MIN` div_s trap edge (that
                ;; edge IS an overflow) AND the round-trip `r / a == b`.
                ;;
                ;; CRUCIAL: wasm `i32.and`/`i32.or` are NOT short-circuiting
                ;; — both arms evaluate eagerly. A flat boolean expression
                ;; would run `i64.div_s` on the trap edge and TRAP. So we
                ;; compute `$ok` with lazy `if` control flow, never reaching
                ;; the divide unless the divisor is safe.
                (if (i64.eqz (local.get $av))
                  (then (local.set $ok (i32.const 1)))
                  (else
                    (if (i32.and (i64.eq (local.get $av) (i64.const -1))
                                 (i64.eq (local.get $bv) (i64.const 0x8000000000000000)))
                      (then (local.set $ok (i32.const 0)))  ;; trap edge → overflow
                      (else
                        (local.set $ok (i64.eq (i64.div_s (local.get $r) (local.get $av)) (local.get $bv)))))))
                (if (result (ref null $aint)) (local.get $ok)
                  (then
                    ;; no overflow → Small result
                    (struct.new $aint (local.get $r) (ref.null $mag) (i32.const 0)))
                  (else
                    ;; overflow → slow path
                    {decomp_a} {decomp_b} {strip_a} {strip_b}
                    {mul_body}
                    {norm})))
              (else
                {decomp_a} {decomp_b} {strip_a} {strip_b}
                {mul_body}
                {norm}))))
    "#,
        mul_body = mul_magnitude(),
    );
    wat_helper::compile_wat_helper(&wat)
}

/// Inlined schoolbook 32-bit-limb magnitude multiply. Reads stripped
/// `am`/`$alen`, `bm`/`$blen`; writes the product magnitude into `$rm`
/// (len `$alen+$blen`) and the result sign into `$rs` (= as_ * bs, with
/// 0 when either is zero). Scratch i32 `$i $j $k`, i64 `$carry $prod`.
fn mul_magnitude() -> String {
    r#"
            ;; result sign: 0 if either operand zero, else product of signs.
            (local.set $rs (i32.mul (local.get $as_) (local.get $bs)))
            (local.set $rlen (i32.add (local.get $alen) (local.get $blen)))
            (if (i32.eqz (local.get $rlen)) (then (local.set $rlen (i32.const 1))))
            (local.set $rm (array.new_default $mag (local.get $rlen)))
            (local.set $i (i32.const 0))
            (block $mi_done (loop $mi
              (br_if $mi_done (i32.ge_u (local.get $i) (local.get $alen)))
              (local.set $carry (i64.const 0))
              (local.set $j (i32.const 0))
              (block $mj_done (loop $mj
                (br_if $mj_done (i32.ge_u (local.get $j) (local.get $blen)))
                (local.set $k (i32.add (local.get $i) (local.get $j)))
                ;; prod = am[i]*bm[j] + rm[k] + carry  (fits in 64 bits:
                ;; (2^32-1)^2 + 2*(2^32-1) < 2^64)
                (local.set $prod (i64.add
                  (i64.add
                    (i64.mul (array.get $mag (local.get $am) (local.get $i))
                             (array.get $mag (local.get $bm) (local.get $j)))
                    (array.get $mag (local.get $rm) (local.get $k)))
                  (local.get $carry)))
                (array.set $mag (local.get $rm) (local.get $k) (i64.and (local.get $prod) (i64.const 0xffffffff)))
                (local.set $carry (i64.shr_u (local.get $prod) (i64.const 32)))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $mj)))
              ;; ripple the final carry up from rm[i+blen], masking each
              ;; limb to 32 bits so no lane is left holding > 2^32-1.
              (local.set $k (i32.add (local.get $i) (local.get $blen)))
              (block $mc_done (loop $mc
                (br_if $mc_done (i64.eqz (local.get $carry)))
                (local.set $prod (i64.add (array.get $mag (local.get $rm) (local.get $k)) (local.get $carry)))
                (array.set $mag (local.get $rm) (local.get $k) (i64.and (local.get $prod) (i64.const 0xffffffff)))
                (local.set $carry (i64.shr_u (local.get $prod) (i64.const 32)))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $mc)))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $mi)))
    "#
    .to_string()
}
