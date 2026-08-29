//! Compiler-internal UTF-8 cursor and single-scalar case helpers.
//!
//! `chars_fusion` replaces a temporary `List<String>` from `String.chars`
//! with a byte offset into the existing wasm-gc `(array i8)` String carrier.
//! These helpers are the complete backend contract for that fabricated shape.

use wasm_encoder::Function;

use super::case_tables::CaseWiring;
use super::string_case::{decode, generic_map, missing};
use super::{TypeRegistry, string_module_preamble};
use crate::codegen::wasm_gc::{WasmGcError, wat_helper};

fn compile(registry: &TypeRegistry, signature_and_body: &str) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!("(module\n{preamble}(func (export \"helper\") {signature_and_body}))");
    wat_helper::compile_wat_helper(&wat)
}

/// Move a possibly non-boundary byte offset back to the scalar's lead byte.
/// Fused code only supplies boundaries, but this preserves the VM/Rust helper's
/// fail-safe behaviour if an internal caller ever supplies a continuation byte.
fn normalize_offset() -> &'static str {
    r#"
        local.get $i i32.wrap_i64 local.set $p
        (block $boundary
          (loop $back
            local.get $s local.get $p array.get_u $string
            i32.const 0xC0 i32.and i32.const 0x80 i32.ne
            br_if $boundary
            local.get $p i32.const 1 i32.sub local.set $p
            br $back))
    "#
}

pub(super) fn emit_cursor_end(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    compile(
        registry,
        r#"(param $s (ref null $string)) (param $i i64) (result i32)
            local.get $i
            local.get $s array.len i64.extend_i32_u
            i64.ge_u"#,
    )
}

pub(super) fn emit_cursor_head(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $s (ref null $string)) (param $i i64) (result (ref null $string))
            (local $p i32)
            (local $b i32)
            (local $code i32)
            (local $clen i32)
            (local $out (ref null $string))
            local.get $i local.get $s array.len i64.extend_i32_u i64.ge_u
            (if (then
                  i32.const 0 array.new_default $string
                  return))
            {normalize}
            {decode}
            local.get $clen array.new_default $string local.set $out
            local.get $out i32.const 0
            local.get $s local.get $p local.get $clen
            array.copy $string $string
            local.get $out"#,
        normalize = normalize_offset(),
        decode = decode("$p", "$b", "$code", "$clen"),
    );
    compile(registry, &body)
}

pub(super) fn emit_cursor_next(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $s (ref null $string)) (param $i i64) (result i64)
            (local $p i32)
            (local $b i32)
            (local $code i32)
            (local $clen i32)
            local.get $i local.get $s array.len i64.extend_i32_u i64.ge_u
            (if (then
                  local.get $s array.len i64.extend_i32_u
                  return))
            {normalize}
            {decode}
            local.get $p local.get $clen i32.add i64.extend_i32_u"#,
        normalize = normalize_offset(),
        decode = decode("$p", "$b", "$code", "$clen"),
    );
    compile(registry, &body)
}

pub(super) fn emit_cursor_code(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $s (ref null $string)) (param $i i64) (result i64)
            (local $p i32)
            (local $b i32)
            (local $code i32)
            (local $clen i32)
            local.get $i local.get $s array.len i64.extend_i32_u i64.ge_u
            (if (then i64.const -1 return))
            {normalize}
            {decode}
            local.get $code i64.extend_i32_u"#,
        normalize = normalize_offset(),
        decode = decode("$p", "$b", "$code", "$clen"),
    );
    compile(registry, &body)
}

pub(super) fn emit_code1(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $s (ref null $string)) (result i64)
            (local $len i32)
            (local $p i32)
            (local $b i32)
            (local $code i32)
            (local $clen i32)
            local.get $s array.len local.tee $len i32.eqz
            (if (then i64.const -1 return))
            {decode}
            local.get $clen local.get $len i32.ne
            (if (then i64.const -1 return))
            local.get $code i64.extend_i32_u"#,
        decode = decode("$p", "$b", "$code", "$clen"),
    );
    compile(registry, &body)
}

/// Locals and instructions shared by `__str_fold_*` and
/// `__str_code1_{lower,upper}` after `$code` contains one valid scalar.
fn case_map(case: &CaseWiring<'_>, to_upper: bool) -> Result<String, WasmGcError> {
    let blob = case.blob;
    let (simple, expand) = if to_upper {
        (blob.upper_simple, blob.upper_expand)
    } else {
        (blob.lower_simple, blob.lower_expand)
    };
    let simple = simple.ok_or_else(|| missing("mapping table"))?;
    let expand = expand.ok_or_else(|| missing("expansion table"))?;
    let global = case.global_idx;
    let segment = case.data_segment_idx;
    let blob_len = blob.bytes.len();
    Ok(format!(
        r#"
            ;; ASCII has no length-changing case mapping.
            local.get $code i32.const 0x80 i32.lt_u
            (if (then
                  local.get $code local.set $m0
                  local.get $code i32.const {lo} i32.ge_u
                  local.get $code i32.const {hi} i32.le_u i32.and
                  (if (then local.get $code {delta} local.set $m0))
                  local.get $m0 i64.extend_i32_u
                  return))

            global.get {global}
            ref.is_null
            (if (then
                  i32.const 0 i32.const {blob_len}
                  array.new_data $string {segment}
                  global.set {global}))
            global.get {global} local.set $t
            local.get $code local.set $m0
            i32.const 1 local.set $mc
            {mapping}
            local.get $mc i32.const 1 i32.ne
            (if (then i64.const -1 return))
            local.get $m0 i64.extend_i32_u
        "#,
        lo = if to_upper { "0x61" } else { "0x41" },
        hi = if to_upper { "0x7A" } else { "0x5A" },
        delta = if to_upper {
            "i32.const 32 i32.sub"
        } else {
            "i32.const 32 i32.add"
        },
        mapping = generic_map(simple, expand),
    ))
}

fn case_module(
    registry: &TypeRegistry,
    signature_and_body: &str,
    case: Option<&CaseWiring<'_>>,
) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wiring = case.ok_or_else(|| missing("no segment"))?;
    let global_fillers = (0..=wiring.global_idx)
        .map(|_| "(global (mut (ref null $string)) (ref.null $string))\n")
        .collect::<String>();
    let data_fillers = (0..=wiring.data_segment_idx)
        .map(|_| "(data \"\")\n")
        .collect::<String>();
    let wat = format!(
        "(module\n{preamble}{global_fillers}{data_fillers}(func (export \"helper\") {signature_and_body}))"
    );
    wat_helper::compile_wat_helper(&wat)
}

fn case_locals() -> &'static str {
    r#"
        (local $t (ref null $string))
        (local $m0 i32)
        (local $m1 i32)
        (local $m2 i32)
        (local $mc i32)
        (local $lo i32)
        (local $hi i32)
        (local $mid i32)
        (local $off i32)
        (local $found i32)
    "#
}

pub(super) fn emit_fold(
    registry: &TypeRegistry,
    to_upper: bool,
    case: Option<&CaseWiring<'_>>,
) -> Result<Function, WasmGcError> {
    let wiring = case.ok_or_else(|| missing("no segment"))?;
    let body = format!(
        r#"(param $c i64) (result i64)
            (local $code i32)
            {locals}
            local.get $c i64.const 0 i64.lt_s
            local.get $c i64.const 0x10FFFF i64.gt_u i32.or
            (if (then i64.const -1 return))
            local.get $c i32.wrap_i64 local.tee $code
            i32.const 0xD800 i32.ge_u
            local.get $code i32.const 0xDFFF i32.le_u i32.and
            (if (then i64.const -1 return))
            {mapping}"#,
        locals = case_locals(),
        mapping = case_map(wiring, to_upper)?,
    );
    case_module(registry, &body, case)
}

pub(super) fn emit_code1_fold(
    registry: &TypeRegistry,
    to_upper: bool,
    case: Option<&CaseWiring<'_>>,
) -> Result<Function, WasmGcError> {
    let wiring = case.ok_or_else(|| missing("no segment"))?;
    let body = format!(
        r#"(param $s (ref null $string)) (result i64)
            (local $len i32)
            (local $p i32)
            (local $b i32)
            (local $code i32)
            (local $clen i32)
            {locals}
            local.get $s array.len local.tee $len i32.eqz
            (if (then i64.const -1 return))
            {decode}
            local.get $clen local.get $len i32.ne
            (if (then i64.const -1 return))
            {mapping}"#,
        locals = case_locals(),
        decode = decode("$p", "$b", "$code", "$clen"),
        mapping = case_map(wiring, to_upper)?,
    );
    case_module(registry, &body, case)
}
