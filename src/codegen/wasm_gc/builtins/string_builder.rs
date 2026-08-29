//! Compiler-internal growable UTF-8 String builder.
//!
//! `buffer_build` removes a temporary `List<String>` from `String.join`, and
//! `interp_lower` can express interpolation through the same closed contract.
//! The wasm-gc carrier stores a logical byte length beside a mutable String
//! array whose physical length is the capacity. Appends grow geometrically;
//! finalization returns an exact-length String array.

use wasm_encoder::Function;

use super::{TypeRegistry, string_module_preamble};
use crate::codegen::wasm_gc::{WasmGcError, wat_helper};

fn preamble(registry: &TypeRegistry) -> Result<String, WasmGcError> {
    let (string_idx, string) = string_module_preamble(registry)?;
    let buffer_idx = registry
        .string_buffer_type_idx
        .ok_or(WasmGcError::Validation(
            "String builder helper requires its hidden type slot".into(),
        ))?;
    if buffer_idx <= string_idx {
        return Err(WasmGcError::Validation(format!(
            "String builder type index {buffer_idx} must follow String index {string_idx}"
        )));
    }
    let between = wat_helper::padding_types(buffer_idx - string_idx - 1);
    Ok(format!(
        "{string}{between}(type $buffer (struct (field (mut i32)) (field (mut i32)) (field (mut (ref null $string)))))\n"
    ))
}

fn compile(registry: &TypeRegistry, signature_and_body: &str) -> Result<Function, WasmGcError> {
    let preamble = preamble(registry)?;
    let wat = format!("(module\n{preamble}(func (export \"helper\") {signature_and_body}))");
    wat_helper::compile_wat_helper(&wat)
}

/// Instructions shared by the ordinary append and separator append after
/// `$buf`, `$part`, `$len`, `$part_len`, and `$data` have been initialized.
///
/// All arithmetic determining an allocation length is widened to i64 first.
/// wasm-gc arrays are indexed by i32; a total beyond signed-i32 capacity is an
/// allocation failure and traps explicitly rather than wrapping into a short
/// array followed by an out-of-bounds copy.
fn append_bytes() -> &'static str {
    r#"
        local.get $len i64.extend_i32_u
        local.get $part_len i64.extend_i32_u
        i64.add local.tee $needed64
        i64.const 0x7fffffff i64.gt_u
        (if (then unreachable))
        local.get $needed64 i32.wrap_i64 local.set $needed

        local.get $data array.len local.set $capacity
        local.get $needed local.get $capacity i32.gt_u
        (if (then
          local.get $capacity i64.extend_i32_u
          i64.const 2 i64.mul local.set $grown64
          local.get $grown64 i64.const 16 i64.lt_u
          (if (then i64.const 16 local.set $grown64))
          local.get $grown64 local.get $needed64 i64.lt_u
          (if (then local.get $needed64 local.set $grown64))
          local.get $grown64 i64.const 0x7fffffff i64.gt_u
          (if (then local.get $needed64 local.set $grown64))

          local.get $grown64 i32.wrap_i64
          array.new_default $string local.set $new_data
          local.get $new_data i32.const 0
          local.get $data i32.const 0
          local.get $len array.copy $string $string
          local.get $buf local.get $new_data struct.set $buffer 2
          local.get $new_data local.set $data))

        local.get $data local.get $len
        local.get $part i32.const 0
        local.get $part_len array.copy $string $string
        local.get $buf local.get $needed struct.set $buffer 0
    "#
}

pub(super) fn emit_append(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $buf (ref null $buffer))
            (param $part (ref null $string))
            (result (ref null $buffer))
            (local $len i32)
            (local $part_len i32)
            (local $capacity i32)
            (local $needed i32)
            (local $needed64 i64)
            (local $grown64 i64)
            (local $data (ref null $string))
            (local $new_data (ref null $string))
            local.get $buf struct.get $buffer 0 local.set $len
            local.get $part array.len local.set $part_len
            local.get $buf struct.get $buffer 2 local.set $data
            {append}
            local.get $buf i32.const 1 struct.set $buffer 1
            local.get $buf"#,
        append = append_bytes(),
    );
    compile(registry, &body)
}

pub(super) fn emit_append_sep_unless_first(
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $buf (ref null $buffer))
            (param $part (ref null $string))
            (result (ref null $buffer))
            (local $len i32)
            (local $part_len i32)
            (local $capacity i32)
            (local $needed i32)
            (local $needed64 i64)
            (local $grown64 i64)
            (local $data (ref null $string))
            (local $new_data (ref null $string))
            local.get $buf struct.get $buffer 1 i32.eqz
            (if (then local.get $buf return))
            local.get $buf struct.get $buffer 0 local.set $len
            local.get $part array.len local.set $part_len
            local.get $buf struct.get $buffer 2 local.set $data
            {append}
            local.get $buf"#,
        append = append_bytes(),
    );
    compile(registry, &body)
}

pub(super) fn emit_finalize(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    compile(
        registry,
        r#"(param $buf (ref null $buffer)) (result (ref null $string))
            (local $len i32)
            (local $data (ref null $string))
            (local $out (ref null $string))
            local.get $buf struct.get $buffer 0 local.set $len
            local.get $buf struct.get $buffer 2 local.set $data
            local.get $len local.get $data array.len i32.eq
            (if (then local.get $data return))
            local.get $len array.new_default $string local.set $out
            local.get $out i32.const 0
            local.get $data i32.const 0
            local.get $len array.copy $string $string
            local.get $out"#,
    )
}
