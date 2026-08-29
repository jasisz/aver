//! `String.toLower` / `String.toUpper` for wasm-gc — full Unicode,
//! byte-identical to the VM and to the generated Rust.
//!
//! Both of those lower to Rust's `str::to_lowercase` /
//! `str::to_uppercase`, so this helper has to reproduce three things a
//! byte-wise ASCII shift cannot express:
//!
//! - mappings outside ASCII (`Ą` → `ą`, `Ω` → `ω`, Deseret, Adlam, …),
//! - one-to-many expansions (`ß` → `SS`, `ﬀ` → `FF`, `İ` → `i` + U+0307),
//!   which change the output's byte length,
//! - the final-sigma rule: a Greek capital sigma lowercases to `ς` when
//!   it is preceded by a cased character and not followed by one
//!   (skipping case-ignorable characters on both sides), and to `σ`
//!   otherwise.
//!
//! The mappings and the two context predicates live in
//! [`super::case_tables`], packed into one passive data segment the
//! module carries only when it actually calls one of these two
//! builtins. The helper decodes that segment once into a wasm global
//! and binary-searches it.
//!
//! Shape of the body:
//!
//! 1. Scan the input for a byte ≥ 0x80. If there is none, run the old
//!    ASCII loop — same allocation, same shifts, same cost as before.
//!    That path is exact because no ASCII scalar has a non-ASCII, a
//!    length-changing, or a context-dependent case mapping in either
//!    direction, and a final sigma cannot occur. It is what keeps
//!    `Bytes.hexDigitValue` (inside the certified JSON parser) at its
//!    previous cost and never materialises the table.
//! 2. Otherwise walk the string scalar by scalar, writing into a
//!    buffer over-allocated by the largest growth factor the tables
//!    admit, then copy out at the exact length.

use wasm_encoder::Function;

use super::super::{WasmGcError, wat_helper};
use super::TypeRegistry;
use super::case_tables::{CaseWiring, MAX_LOWER_GROWTH, MAX_UPPER_GROWTH, TableSpan};

/// Read the little-endian `u24` at `t[<off> + k]`. Every field of every
/// table is one of these, so all four record layouts share this one
/// routine.
fn read_u24(off: &str, k: u32) -> String {
    let (k1, k2) = (k + 1, k + 2);
    format!(
        "local.get $t local.get {off} i32.const {k} i32.add array.get_u $string
         local.get $t local.get {off} i32.const {k1} i32.add array.get_u $string i32.const 8 i32.shl i32.or
         local.get $t local.get {off} i32.const {k2} i32.add array.get_u $string i32.const 16 i32.shl i32.or"
    )
}

/// Binary-search one table for `key`, leaving `$found` (0/1) and, when
/// found, `$off` at the matching record's byte offset.
///
/// `point` searches tables keyed by a single scalar (the expansion
/// tables); otherwise the record's second field is an inclusive range
/// end. `id` only has to make the two block labels unique inside the
/// function.
fn search(span: TableSpan, record: u32, point: bool, key: &str, id: &str) -> String {
    let end_field = if point { 0 } else { 3 };
    let start_read = read_u24("$off", 0);
    let end_read = read_u24("$off", end_field);
    let TableSpan { off, count } = span;
    format!(
        r#"
            i32.const 0 local.set $lo
            i32.const {count} local.set $hi
            i32.const 0 local.set $found
            (block $sd{id}
              (loop $sl{id}
                local.get $lo local.get $hi i32.ge_u br_if $sd{id}
                local.get $lo local.get $hi i32.add i32.const 1 i32.shr_u local.set $mid
                local.get $mid i32.const {record} i32.mul i32.const {off} i32.add local.set $off
                local.get {key}
                {start_read}
                i32.lt_u
                (if (then local.get $mid local.set $hi)
                    (else
                      local.get {key}
                      {end_read}
                      i32.gt_u
                      (if (then local.get $mid i32.const 1 i32.add local.set $lo)
                          (else i32.const 1 local.set $found br $sd{id}))))
                br $sl{id}))
        "#
    )
}

/// Decode the UTF-8 scalar starting at byte `idx` of `$s` into `code`,
/// with its byte length in `clen`. `byte` is scratch for the lead byte.
/// Input is always well-formed UTF-8 — String values come from
/// literals, concatenation, scalar-boundary slicing, or the validating
/// `String.fromUtf8` — so there is no error arm.
pub(super) fn decode(idx: &str, byte: &str, code: &str, clen: &str) -> String {
    format!(
        r#"
            local.get $s local.get {idx} array.get_u $string local.set {byte}
            local.get {byte} i32.const 0x80 i32.lt_u
            (if (then
                  local.get {byte} local.set {code}
                  i32.const 1 local.set {clen})
                (else
                  local.get {byte} i32.const 0xE0 i32.lt_u
                  (if (then
                        local.get {byte} i32.const 0x1F i32.and i32.const 6 i32.shl
                        local.get $s local.get {idx} i32.const 1 i32.add array.get_u $string
                        i32.const 0x3F i32.and
                        i32.or
                        local.set {code}
                        i32.const 2 local.set {clen})
                      (else
                        local.get {byte} i32.const 0xF0 i32.lt_u
                        (if (then
                              local.get {byte} i32.const 0x0F i32.and i32.const 12 i32.shl
                              local.get $s local.get {idx} i32.const 1 i32.add array.get_u $string
                              i32.const 0x3F i32.and i32.const 6 i32.shl
                              i32.or
                              local.get $s local.get {idx} i32.const 2 i32.add array.get_u $string
                              i32.const 0x3F i32.and
                              i32.or
                              local.set {code}
                              i32.const 3 local.set {clen})
                            (else
                              local.get {byte} i32.const 0x07 i32.and i32.const 18 i32.shl
                              local.get $s local.get {idx} i32.const 1 i32.add array.get_u $string
                              i32.const 0x3F i32.and i32.const 12 i32.shl
                              i32.or
                              local.get $s local.get {idx} i32.const 2 i32.add array.get_u $string
                              i32.const 0x3F i32.and i32.const 6 i32.shl
                              i32.or
                              local.get $s local.get {idx} i32.const 3 i32.add array.get_u $string
                              i32.const 0x3F i32.and
                              i32.or
                              local.set {code}
                              i32.const 4 local.set {clen}))))))
        "#
    )
}

/// Append the UTF-8 encoding of scalar `c` to `$out` at `$n`, advancing
/// `$n`.
fn encode(c: &str) -> String {
    format!(
        r#"
            local.get {c} i32.const 0x80 i32.lt_u
            (if (then
                  local.get $out local.get $n local.get {c} array.set $string
                  local.get $n i32.const 1 i32.add local.set $n)
                (else
                  local.get {c} i32.const 0x800 i32.lt_u
                  (if (then
                        local.get $out local.get $n
                          local.get {c} i32.const 6 i32.shr_u i32.const 0xC0 i32.or
                          array.set $string
                        local.get $out local.get $n i32.const 1 i32.add
                          local.get {c} i32.const 0x3F i32.and i32.const 0x80 i32.or
                          array.set $string
                        local.get $n i32.const 2 i32.add local.set $n)
                      (else
                        local.get {c} i32.const 0x10000 i32.lt_u
                        (if (then
                              local.get $out local.get $n
                                local.get {c} i32.const 12 i32.shr_u i32.const 0xE0 i32.or
                                array.set $string
                              local.get $out local.get $n i32.const 1 i32.add
                                local.get {c} i32.const 6 i32.shr_u i32.const 0x3F i32.and
                                i32.const 0x80 i32.or
                                array.set $string
                              local.get $out local.get $n i32.const 2 i32.add
                                local.get {c} i32.const 0x3F i32.and i32.const 0x80 i32.or
                                array.set $string
                              local.get $n i32.const 3 i32.add local.set $n)
                            (else
                              local.get $out local.get $n
                                local.get {c} i32.const 18 i32.shr_u i32.const 0xF0 i32.or
                                array.set $string
                              local.get $out local.get $n i32.const 1 i32.add
                                local.get {c} i32.const 12 i32.shr_u i32.const 0x3F i32.and
                                i32.const 0x80 i32.or
                                array.set $string
                              local.get $out local.get $n i32.const 2 i32.add
                                local.get {c} i32.const 6 i32.shr_u i32.const 0x3F i32.and
                                i32.const 0x80 i32.or
                                array.set $string
                              local.get $out local.get $n i32.const 3 i32.add
                                local.get {c} i32.const 0x3F i32.and i32.const 0x80 i32.or
                                array.set $string
                              local.get $n i32.const 4 i32.add local.set $n))))))
        "#
    )
}

/// The pre-Unicode body, kept verbatim as the all-ASCII fast path.
fn ascii_pass(to_upper: bool) -> String {
    let (lo, hi, delta) = if to_upper {
        ("0x61", "0x7A", "i32.const 32 i32.sub")
    } else {
        ("0x41", "0x5A", "i32.const 32 i32.add")
    };
    format!(
        r#"
            local.get $len array.new_default $string local.set $out
            i32.const 0 local.set $i
            (block $adone
              (loop $acp
                local.get $i local.get $len i32.ge_u br_if $adone
                local.get $s local.get $i array.get_u $string local.set $b
                local.get $b i32.const {lo} i32.ge_u
                local.get $b i32.const {hi} i32.le_u
                i32.and
                (if (then local.get $b {delta} local.set $b))
                local.get $out local.get $i local.get $b array.set $string
                local.get $i i32.const 1 i32.add local.set $i
                br $acp))
            local.get $out
            return
        "#
    )
}

/// Missing wiring means the module registered the helper but never
/// emitted its table — fail loudly rather than silently shipping the
/// ASCII-only mapping again.
pub(super) fn missing(what: &str) -> WasmGcError {
    WasmGcError::Validation(format!(
        "String case helper requires the Unicode case table wiring ({what})"
    ))
}

pub(super) fn emit(
    registry: &TypeRegistry,
    to_upper: bool,
    case: Option<&CaseWiring<'_>>,
) -> Result<Function, WasmGcError> {
    let (_, preamble) = super::string_module_preamble(registry)?;
    let wiring = case.ok_or_else(|| missing("no segment"))?;
    let blob = wiring.blob;
    let (simple, expand) = if to_upper {
        (blob.upper_simple, blob.upper_expand)
    } else {
        (blob.lower_simple, blob.lower_expand)
    };
    let simple = simple.ok_or_else(|| missing("mapping table"))?;
    let expand = expand.ok_or_else(|| missing("expansion table"))?;

    // Context tables — only `toLower` needs them, and only `toLower`
    // packs them into the blob.
    let sigma = if to_upper {
        String::new()
    } else {
        let cased = blob.cased.ok_or_else(|| missing("cased table"))?;
        let ignorable = blob.ignorable.ok_or_else(|| missing("ignorable table"))?;
        format!(
            r#"
            local.get $code i32.const 0x3A3 i32.eq
            (if (then
                  local.get $prev
                  i32.eqz
                  (if (then i32.const 0x3C3 local.set $m0)
                      (else
                        i32.const 0 local.set $fwd
                        local.get $i local.get $clen i32.add local.set $j
                        (block $fd
                          (loop $fl
                            local.get $j local.get $len i32.ge_u br_if $fd
                            {decode_j}
                            {ign_j}
                            local.get $found
                            (if (then
                                  local.get $j local.get $cl2 i32.add local.set $j
                                  br $fl))
                            {cased_j}
                            local.get $found local.set $fwd
                            br $fd))
                        local.get $fwd
                        (if (then i32.const 0x3C3 local.set $m0)
                            (else i32.const 0x3C2 local.set $m0)))))
                (else
                  {generic}))
            "#,
            decode_j = decode("$j", "$b2", "$c2", "$cl2"),
            ign_j = search(ignorable, 6, false, "$c2", "fi"),
            cased_j = search(cased, 6, false, "$c2", "fc"),
            generic = generic_map(simple, expand),
        )
    };

    // Carry "the most recent non-ignorable scalar was cased" forward.
    // That is exactly what `str::to_lowercase` computes by scanning
    // backwards, at one lookup per scalar instead of a rescan.
    let context = if to_upper {
        String::new()
    } else {
        let cased = blob.cased.ok_or_else(|| missing("cased table"))?;
        let ignorable = blob.ignorable.ok_or_else(|| missing("ignorable table"))?;
        format!(
            r#"
            {ign}
            local.get $found
            i32.eqz
            (if (then
                  {cas}
                  local.get $found local.set $prev))
            "#,
            ign = search(ignorable, 6, false, "$code", "ci"),
            cas = search(cased, 6, false, "$code", "cc"),
        )
    };

    let map = if to_upper {
        generic_map(simple, expand)
    } else {
        sigma
    };

    let growth = if to_upper {
        MAX_UPPER_GROWTH
    } else {
        MAX_LOWER_GROWTH
    };
    let blob_len = blob.bytes.len();
    let segment = wiring.data_segment_idx;
    let global = wiring.global_idx;

    // The standalone WAT module needs a global and a data segment at
    // the user module's indices for `global.get` / `array.new_data` to
    // resolve; only the function body transfers, so the fillers cost
    // nothing in the emitted module.
    let global_fillers = (0..=global)
        .map(|_| "(global (mut (ref null $string)) (ref.null $string))\n")
        .collect::<String>();
    let data_fillers = (0..=segment).map(|_| "(data \"\")\n").collect::<String>();

    let wat = format!(
        r#"
        (module
          {preamble}
          {global_fillers}
          {data_fillers}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $string))
            (local $len i32)
            (local $i i32)
            (local $b i32)
            (local $out (ref null $string))
            (local $res (ref null $string))
            (local $n i32)
            (local $cap i32)
            (local $t (ref null $string))
            (local $code i32)
            (local $clen i32)
            (local $m0 i32)
            (local $m1 i32)
            (local $m2 i32)
            (local $mc i32)
            (local $prev i32)
            (local $fwd i32)
            (local $j i32)
            (local $b2 i32)
            (local $c2 i32)
            (local $cl2 i32)
            (local $ascii i32)
            (local $lo i32)
            (local $hi i32)
            (local $mid i32)
            (local $off i32)
            (local $found i32)

            local.get $s array.len local.set $len

            ;; Is every byte ASCII? Then the old shift is exact.
            i32.const 1 local.set $ascii
            i32.const 0 local.set $i
            (block $pdone
              (loop $pcp
                local.get $i local.get $len i32.ge_u br_if $pdone
                local.get $s local.get $i array.get_u $string
                i32.const 0x80 i32.ge_u
                (if (then i32.const 0 local.set $ascii br $pdone))
                local.get $i i32.const 1 i32.add local.set $i
                br $pcp))
            local.get $ascii
            (if (then {ascii_pass}))

            ;; Decode the case tables once per instance.
            global.get {global}
            ref.is_null
            (if (then
                  i32.const 0
                  i32.const {blob_len}
                  array.new_data $string {segment}
                  global.set {global}))
            global.get {global} local.set $t

            local.get $len i32.const {growth} i32.mul local.set $cap
            local.get $cap array.new_default $string local.set $out
            i32.const 0 local.set $n
            i32.const 0 local.set $i
            i32.const 0 local.set $prev
            (block $done
              (loop $cp
                local.get $i local.get $len i32.ge_u br_if $done

                {decode_i}

                local.get $code local.set $m0
                i32.const 1 local.set $mc
                {map}

                {emit_m0}
                local.get $mc i32.const 2 i32.ge_u
                (if (then {emit_m1}))
                local.get $mc i32.const 3 i32.ge_u
                (if (then {emit_m2}))

                {context}

                local.get $i local.get $clen i32.add local.set $i
                br $cp))

            local.get $n array.new_default $string local.set $res
            local.get $res
            i32.const 0
            local.get $out
            i32.const 0
            local.get $n
            array.copy $string $string
            local.get $res)
        )
    "#,
        ascii_pass = ascii_pass(to_upper),
        decode_i = decode("$i", "$b", "$code", "$clen"),
        emit_m0 = encode("$m0"),
        emit_m1 = encode("$m1"),
        emit_m2 = encode("$m2"),
    );
    wat_helper::compile_wat_helper(&wat)
}

/// Expansion first (a scalar is in at most one of the two tables), then
/// the compressed one-to-one runs; a scalar in neither maps to itself.
pub(super) fn generic_map(simple: TableSpan, expand: TableSpan) -> String {
    format!(
        r#"
            {ex}
            local.get $found
            (if (then
                  {to0} local.set $m0
                  {to1} local.set $m1
                  {to2} local.set $m2
                  local.get $m2
                  i32.eqz
                  (if (then i32.const 2 local.set $mc)
                      (else i32.const 3 local.set $mc)))
                (else
                  {si}
                  local.get $found
                  (if (then
                        {delta}
                        i32.const 8 i32.shl i32.const 8 i32.shr_s
                        local.get $code i32.add
                        local.set $m0))))
        "#,
        ex = search(expand, 12, true, "$code", "ex"),
        si = search(simple, 9, false, "$code", "si"),
        to0 = read_u24("$off", 3),
        to1 = read_u24("$off", 6),
        to2 = read_u24("$off", 9),
        delta = read_u24("$off", 6),
    )
}
