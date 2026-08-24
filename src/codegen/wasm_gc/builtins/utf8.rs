//! Direct UTF-8 bridges between Aver `String` and the standard `Bytes` type.
//!
//! The normal proof-selected representation is `(array i8)` on both sides
//! (with distinct nominal wasm types). Encoding is one `array.copy`. Decoding
//! uses one streaming pass which copies each octet and validates UTF-8 with a
//! three-integer continuation state. The boxed `List<Int>` representation is
//! kept as a differential fallback for `AVER_NO_PACKED_SEQUENCES`.

use wasm_encoder::Function;

use super::super::{WasmGcError, wat_helper};
use super::TypeRegistry;

struct Utf8Types {
    string_idx: u32,
    bytes_idx: u32,
    result_idx: Option<u32>,
    list_idx: Option<u32>,
    aint_idx: Option<u32>,
    packed: bool,
}

impl Utf8Types {
    fn discover(registry: &TypeRegistry, with_result: bool) -> Result<Self, WasmGcError> {
        let string_idx = registry
            .string_array_type_idx
            .ok_or_else(|| WasmGcError::Validation("UTF-8 helper requires String".into()))?;
        let packed_bytes = registry.packed_sequence("Bytes");
        if let Some(packed) = packed_bytes
            && !matches!(
                packed.layout.element,
                crate::codegen::proof_lower::PackedIntElement::U8
            )
        {
            return Err(WasmGcError::Validation(
                "UTF-8 helper requires an unsigned-octet Bytes layout".into(),
            ));
        }
        let bytes_idx = packed_bytes
            .map(|packed| packed.type_idx)
            .or_else(|| registry.record_type_idx("Bytes"))
            .ok_or_else(|| WasmGcError::Validation("UTF-8 helper requires Bytes".into()))?;
        let result_idx = with_result
            .then(|| registry.result_type_idx("Result<String,String>"))
            .flatten();
        if with_result && result_idx.is_none() {
            return Err(WasmGcError::Validation(
                "String.fromUtf8 requires Result<String,String>".into(),
            ));
        }
        let list_idx = packed_bytes
            .is_none()
            .then(|| registry.list_type_idx("List<Int>"))
            .flatten();
        let aint_idx = packed_bytes
            .is_none()
            .then_some(registry.aint_struct_idx)
            .flatten();
        if packed_bytes.is_none() && (list_idx.is_none() || aint_idx.is_none()) {
            return Err(WasmGcError::Validation(
                "boxed UTF-8 bridge requires List<Int> and Int".into(),
            ));
        }
        Ok(Self {
            string_idx,
            bytes_idx,
            result_idx,
            list_idx,
            aint_idx,
            packed: packed_bytes.is_some(),
        })
    }

    fn declarations(&self) -> Result<String, WasmGcError> {
        let mut indices = vec![self.string_idx, self.bytes_idx];
        indices.extend(self.result_idx);
        indices.extend(self.list_idx);
        indices.extend(self.aint_idx);
        let mut unique = indices.clone();
        unique.sort_unstable();
        unique.dedup();
        if unique.len() != indices.len() {
            return Err(WasmGcError::Validation(
                "UTF-8 helper type indices overlap".into(),
            ));
        }
        let max = *unique.last().expect("UTF-8 types are non-empty");
        let mut out = String::from("(rec\n");
        for idx in 0..=max {
            let ty = if idx == self.string_idx {
                "(type $string (array (mut i8)))"
            } else if idx == self.bytes_idx && self.packed {
                "(type $bytes (array (mut i8)))"
            } else if idx == self.bytes_idx {
                "(type $bytes (struct (field (ref null $list_int))))"
            } else if Some(idx) == self.result_idx {
                "(type $result (struct (field (mut i32)) (field (mut (ref null $string))) (field (mut (ref null $string)))))"
            } else if Some(idx) == self.list_idx {
                "(type $list_int (struct (field (ref null $aint)) (field (ref null $list_int))))"
            } else if Some(idx) == self.aint_idx {
                "(type $aint (struct))"
            } else {
                "(type (struct))"
            };
            out.push_str(ty);
            out.push('\n');
        }
        out.push_str(")\n");
        Ok(out)
    }
}

/// `String.toUtf8 : String -> Bytes`.
pub(super) fn emit_to_utf8(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let types = Utf8Types::discover(registry, false)?;
    let declarations = types.declarations()?;
    let (callees, locals, body) = if types.packed {
        (
            String::new(),
            "(local $out (ref null $bytes))",
            r#"local.get $s
  array.len
  array.new_default $bytes
  local.set $out
  local.get $out
  i32.const 0
  local.get $s
  i32.const 0
  local.get $s
  array.len
  array.copy $bytes $string
  local.get $out"#
                .to_string(),
        )
    } else {
        let from_i64_idx = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("boxed String.toUtf8 requires Int.from-i64".into())
        })?;
        (
            wat_helper::func_placeholders(&[wat_helper::CalleeStub {
                abs_idx: from_i64_idx,
                sig: "(param i64) (result (ref null $aint))",
            }]),
            "(local $i i32) (local $node (ref null $list_int))",
            format!(
                r#"local.get $s array.len local.set $i
  (block $done
    (loop $copy
      local.get $i i32.eqz br_if $done
      local.get $i i32.const 1 i32.sub local.set $i
      local.get $s local.get $i array.get_u $string i64.extend_i32_u
      call {from_i64_idx}
      local.get $node
      struct.new $list_int
      local.set $node
      br $copy))
  local.get $node
  struct.new $bytes"#
            ),
        )
    };
    let wat = format!(
        r#"(module
  {declarations}
  {callees}
  (func (export "helper") (param $s (ref null $string)) (result (ref null $bytes))
    {locals}
    {body})
)"#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.fromUtf8 : Bytes -> Result<String,String>`.
pub(super) fn emit_from_utf8(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let types = Utf8Types::discover(registry, true)?;
    let declarations = types.declarations()?;
    let (callees, carrier_locals, length, load_byte, advance) = if types.packed {
        (
            String::new(),
            String::new(),
            "local.get $input array.len local.set $len".to_string(),
            "local.get $input local.get $i array.get_u $bytes local.set $byte".to_string(),
            String::new(),
        )
    } else {
        let to_i64_idx = registry.aint_to_i64_checked_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("boxed String.fromUtf8 requires Int.to-i64".into())
        })?;
        (
            wat_helper::func_placeholders(&[wat_helper::CalleeStub {
                abs_idx: to_i64_idx,
                sig: "(param (ref null $aint)) (result i64)",
            }]),
            "(local $node (ref null $list_int))".to_string(),
            r#"local.get $input ref.as_non_null struct.get $bytes 0 local.set $node
  (block $count_done
    (loop $count
      local.get $node ref.is_null br_if $count_done
      local.get $len i32.const 1 i32.add local.set $len
      local.get $node ref.as_non_null struct.get $list_int 1 local.set $node
      br $count))
  local.get $input ref.as_non_null struct.get $bytes 0 local.set $node"#
                .to_string(),
            format!(
                "local.get $node ref.as_non_null struct.get $list_int 0 call {to_i64_idx} i32.wrap_i64 local.set $byte"
            ),
            "local.get $node ref.as_non_null struct.get $list_int 1 local.set $node".to_string(),
        )
    };

    // Streaming validator state:
    // - need == 0: the next byte must be ASCII or a valid leading octet;
    // - need > 0: min_next..max_next bounds the next continuation byte.
    // Special first-continuation bounds reject overlong encodings, UTF-16
    // surrogates, and scalar values above U+10FFFF. Later continuations reset
    // to 0x80..=0xBF.
    let wat = format!(
        r#"(module
  {declarations}
  {callees}
  (func (export "helper") (param $input (ref null $bytes)) (result (ref null $result))
    (local $len i32)
    (local $i i32)
    (local $byte i32)
    (local $need i32)
    (local $min_next i32)
    (local $max_next i32)
    (local $out (ref null $string))
    {carrier_locals}

    {length}
    local.get $len array.new_default $string local.set $out
    i32.const 0x80 local.set $min_next
    i32.const 0xBF local.set $max_next

    (block $invalid
      (block $done
        (loop $scan
          local.get $i local.get $len i32.ge_u br_if $done
          {load_byte}

          local.get $out local.get $i local.get $byte array.set $string

          local.get $need i32.eqz
          (if
            (then
              local.get $byte i32.const 0x80 i32.lt_u
              (if
                (then)
                (else
                  local.get $byte i32.const 0xC2 i32.lt_u br_if $invalid
                  local.get $byte i32.const 0xDF i32.le_u
                  (if
                    (then
                      i32.const 1 local.set $need
                      i32.const 0x80 local.set $min_next
                      i32.const 0xBF local.set $max_next)
                    (else
                      local.get $byte i32.const 0xEF i32.le_u
                      (if
                        (then
                          i32.const 2 local.set $need
                          i32.const 0x80 local.set $min_next
                          i32.const 0xBF local.set $max_next
                          local.get $byte i32.const 0xE0 i32.eq
                          (if (then i32.const 0xA0 local.set $min_next))
                          local.get $byte i32.const 0xED i32.eq
                          (if (then i32.const 0x9F local.set $max_next)))
                        (else
                          local.get $byte i32.const 0xF4 i32.gt_u br_if $invalid
                          i32.const 3 local.set $need
                          i32.const 0x80 local.set $min_next
                          i32.const 0xBF local.set $max_next
                          local.get $byte i32.const 0xF0 i32.eq
                          (if (then i32.const 0x90 local.set $min_next))
                          local.get $byte i32.const 0xF4 i32.eq
                          (if (then i32.const 0x8F local.set $max_next)))))))))
            (else
              local.get $byte local.get $min_next i32.lt_u br_if $invalid
              local.get $byte local.get $max_next i32.gt_u br_if $invalid
              local.get $need i32.const 1 i32.sub local.set $need
              i32.const 0x80 local.set $min_next
              i32.const 0xBF local.set $max_next))

          {advance}
          local.get $i i32.const 1 i32.add local.set $i
          br $scan))

      local.get $need i32.eqz
      (if
        (then
          i32.const 1
          local.get $out
          ref.null $string
          struct.new $result
          return)))

    i32.const 0
    ref.null $string
    i32.const 105 i32.const 110 i32.const 118 i32.const 97
    i32.const 108 i32.const 105 i32.const 100 i32.const 32
    i32.const 85 i32.const 84 i32.const 70 i32.const 45 i32.const 56
    array.new_fixed $string 13
    struct.new $result)
)"#
    );
    wat_helper::compile_wat_helper(&wat)
}
