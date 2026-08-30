//! Compiler-internal growable packed-byte sink.
//!
//! The byte-only half of `list_build` removes the temporary `List<Int>` fed
//! directly into the standard `Bytes.fromList` validator. The wasm-gc carrier
//! grows the nominal packed-u8 `Bytes` array geometrically and remembers the
//! first invalid arbitrary-precision `Int`; finalization returns either that
//! exact packed array or the source library's byte-identical error message.

use wasm_encoder::Function;

use super::TypeRegistry;
use crate::codegen::wasm_gc::{WasmGcError, wat_helper};

const RESULT_TYPE: &str = "Result<__BytePayload,String>";
const ERROR_PREFIX: &[u8] = b"byte ";
const ERROR_MIDDLE: &[u8] = b" at index ";
const ERROR_SUFFIX: &[u8] = b" is outside 0..=255";

struct ContractIndices {
    string: u32,
    bytes: u32,
    builder: u32,
    mag: u32,
    aint: u32,
    result: u32,
}

fn contract_indices(registry: &TypeRegistry) -> Result<ContractIndices, WasmGcError> {
    let string = registry
        .string_array_type_idx
        .ok_or_else(|| WasmGcError::Validation("byte sink requires the String carrier".into()))?;
    let bytes = registry
        .byte_payload_packed_sequence()
        .map(|packed| packed.type_idx)
        .ok_or_else(|| {
            WasmGcError::Validation("byte sink requires proven packed-u8 `Bytes`".into())
        })?;
    let builder = registry.byte_builder_type_idx.ok_or_else(|| {
        WasmGcError::Validation("byte sink requires its hidden builder type".into())
    })?;
    let mag = registry.aint_mag_array_idx.ok_or_else(|| {
        WasmGcError::Validation("byte sink requires the Int magnitude carrier".into())
    })?;
    let aint = registry.aint_struct_idx.ok_or_else(|| {
        WasmGcError::Validation("byte sink requires arbitrary-precision Int".into())
    })?;
    let result = registry.result_type_idx(RESULT_TYPE).ok_or_else(|| {
        WasmGcError::Validation(format!(
            "byte sink result `{RESULT_TYPE}` was not registered"
        ))
    })?;
    Ok(ContractIndices {
        string,
        bytes,
        builder,
        mag,
        aint,
        result,
    })
}

/// Declare the exact production type indices in one recursive group. The byte
/// builder is allocated before proof-derived packed arrays, so its `$bytes`
/// field is intentionally a forward reference within this group.
fn preamble(registry: &TypeRegistry) -> Result<String, WasmGcError> {
    let idx = contract_indices(registry)?;
    let max = [
        idx.string,
        idx.bytes,
        idx.builder,
        idx.mag,
        idx.aint,
        idx.result,
    ]
    .into_iter()
    .max()
    .expect("contract index set is non-empty");
    let mut declarations = vec!["(type (struct))".to_string(); max as usize + 1];
    declarations[idx.string as usize] = "(type $string (array (mut i8)))".to_string();
    declarations[idx.bytes as usize] = "(type $bytes (array (mut i8)))".to_string();
    declarations[idx.mag as usize] = "(type $mag (array (mut i64)))".to_string();
    declarations[idx.aint as usize] =
        "(type $aint (struct (field i64) (field (ref null $mag)) (field i32)))".to_string();
    declarations[idx.builder as usize] = "(type $builder (struct \
        (field (mut i32)) \
        (field (mut (ref null $bytes))) \
        (field (mut (ref null $aint))) \
        (field (mut i32))))"
        .to_string();
    declarations[idx.result as usize] = "(type $result (struct \
        (field i32) \
        (field (ref null $bytes)) \
        (field (ref null $string))))"
        .to_string();
    Ok(format!("(rec\n  {}\n)\n", declarations.join("\n  ")))
}

fn compile(
    registry: &TypeRegistry,
    callees: &str,
    signature_and_body: &str,
) -> Result<Function, WasmGcError> {
    let declarations = preamble(registry)?;
    let wat =
        format!("(module\n{declarations}{callees}(func (export \"helper\") {signature_and_body}))");
    wat_helper::compile_wat_helper(&wat)
}

/// Push one boxed Int. Once an invalid value has been observed later pushes
/// are semantic no-ops, preserving `Bytes.fromList`'s first-error contract.
pub(super) fn emit_push(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let body = format!(
        r#"(param $buf (ref null $builder))
            (param $value (ref null $aint))
            (result (ref null $builder))
            (local $small i64)
            (local $len i32)
            (local $capacity i32)
            (local $needed i32)
            (local $needed64 i64)
            (local $grown64 i64)
            (local $data (ref null $bytes))
            (local $new_data (ref null $bytes))

            local.get $buf struct.get $builder 2 ref.is_null i32.eqz
            (if (then local.get $buf return))

            local.get $value ref.as_non_null struct.get $aint 1 ref.is_null
            (if (result i32)
              (then
                local.get $value ref.as_non_null struct.get $aint 0 local.tee $small
                i64.const 0 i64.ge_s
                local.get $small i64.const 255 i64.le_s
                i32.and)
              (else i32.const 0))
            (if
              (then
                local.get $buf struct.get $builder 0 local.tee $len
                i64.extend_i32_u i64.const 1 i64.add local.tee $needed64
                i64.const 0x7fffffff i64.gt_u
                (if (then unreachable))
                local.get $needed64 i32.wrap_i64 local.set $needed

                local.get $buf struct.get $builder 1 local.tee $data
                array.len local.set $capacity
                local.get $needed local.get $capacity i32.gt_u
                (if (then
                  local.get $capacity i64.extend_i32_u i64.const {growth} i64.mul
                  local.set $grown64
                  local.get $grown64 i64.const {initial} i64.lt_u
                  (if (then i64.const {initial} local.set $grown64))
                  local.get $grown64 local.get $needed64 i64.lt_u
                  (if (then local.get $needed64 local.set $grown64))
                  local.get $grown64 i64.const 0x7fffffff i64.gt_u
                  (if (then local.get $needed64 local.set $grown64))

                  local.get $grown64 i32.wrap_i64
                  array.new_default $bytes local.set $new_data
                  local.get $new_data i32.const 0
                  local.get $data i32.const 0
                  local.get $len array.copy $bytes $bytes
                  local.get $buf local.get $new_data struct.set $builder 1
                  local.get $new_data local.set $data))

                local.get $data local.get $len local.get $small i32.wrap_i64
                array.set $bytes
                local.get $buf local.get $needed struct.set $builder 0)
              (else
                local.get $buf local.get $value struct.set $builder 2
                local.get $buf local.get $buf struct.get $builder 0
                struct.set $builder 3))
            local.get $buf"#,
        growth = super::BYTE_SINK_GROWTH_FACTOR,
        initial = super::BYTE_SINK_INITIAL_CAPACITY,
    );
    compile(registry, "", &body)
}

fn fixed_array(bytes: &[u8], ty: &str) -> String {
    let values = bytes
        .iter()
        .map(|byte| format!("i32.const {byte}"))
        .collect::<Vec<_>>()
        .join(" ");
    format!("{values} array.new_fixed {ty} {}", bytes.len())
}

/// Finalize to the exact packed byte array, or reproduce the standard
/// library's first-error String including arbitrary-precision decimal text.
pub(super) fn emit_finalize(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let formatter = registry.byte_string_from_int_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("byte sink lacks String.fromInt formatter slot".into())
    })?;
    let callees = wat_helper::func_placeholders(&[wat_helper::CalleeStub {
        abs_idx: formatter,
        sig: "(param (ref null $aint)) (result (ref null $string))",
    }]);
    let fixed_len = ERROR_PREFIX.len() + ERROR_MIDDLE.len() + ERROR_SUFFIX.len();
    let body = format!(
        r#"(param $buf (ref null $builder)) (result (ref null $result))
            (local $len i32)
            (local $data (ref null $bytes))
            (local $exact (ref null $bytes))
            (local $bad (ref null $aint))
            (local $value_s (ref null $string))
            (local $index_s (ref null $string))
            (local $message (ref null $string))
            (local $cursor i32)

            local.get $buf struct.get $builder 2 local.tee $bad ref.is_null
            (if (result (ref null $result))
              (then
                local.get $buf struct.get $builder 0 local.set $len
                local.get $buf struct.get $builder 1 local.set $data
                local.get $len local.get $data array.len i32.eq
                (if
                  (then local.get $data local.set $exact)
                  (else
                    local.get $len array.new_default $bytes local.set $exact
                    local.get $exact i32.const 0
                    local.get $data i32.const 0
                    local.get $len array.copy $bytes $bytes))
                i32.const 1 local.get $exact ref.null $string struct.new $result)
              (else
                local.get $bad call {formatter} local.set $value_s
                local.get $buf struct.get $builder 3 i64.extend_i32_u
                ref.null $mag i32.const 0 struct.new $aint
                call {formatter} local.set $index_s

                local.get $value_s array.len
                local.get $index_s array.len i32.add
                i32.const {fixed_len} i32.add
                array.new_default $string local.set $message

                local.get $message i32.const 0
                {prefix} i32.const 0 i32.const {prefix_len}
                array.copy $string $string
                i32.const {prefix_len} local.set $cursor

                local.get $message local.get $cursor
                local.get $value_s i32.const 0 local.get $value_s array.len
                array.copy $string $string
                local.get $cursor local.get $value_s array.len i32.add local.set $cursor

                local.get $message local.get $cursor
                {middle} i32.const 0 i32.const {middle_len}
                array.copy $string $string
                local.get $cursor i32.const {middle_len} i32.add local.set $cursor

                local.get $message local.get $cursor
                local.get $index_s i32.const 0 local.get $index_s array.len
                array.copy $string $string
                local.get $cursor local.get $index_s array.len i32.add local.set $cursor

                local.get $message local.get $cursor
                {suffix} i32.const 0 i32.const {suffix_len}
                array.copy $string $string

                i32.const 0 ref.null $bytes local.get $message struct.new $result))"#,
        formatter = formatter,
        fixed_len = fixed_len,
        prefix = fixed_array(ERROR_PREFIX, "$string"),
        prefix_len = ERROR_PREFIX.len(),
        middle = fixed_array(ERROR_MIDDLE, "$string"),
        middle_len = ERROR_MIDDLE.len(),
        suffix = fixed_array(ERROR_SUFFIX, "$string"),
        suffix_len = ERROR_SUFFIX.len(),
    );
    compile(registry, &callees, &body)
}

#[cfg(test)]
mod tests {
    /// The production helper uses this exact recurrence. Counting the model is
    /// a deterministic regression guard against accidentally returning to one
    /// allocation per pushed byte.
    fn geometric_growths(elements: usize) -> usize {
        let mut capacity = super::super::BYTE_SINK_INITIAL_CAPACITY as usize;
        let mut growths = 0usize;
        while capacity < elements {
            capacity *= super::super::BYTE_SINK_GROWTH_FACTOR as usize;
            growths += 1;
        }
        growths
    }

    #[test]
    fn one_mebibyte_needs_only_logarithmic_growth() {
        assert_eq!(geometric_growths(1024 * 1024), 16);
    }
}
