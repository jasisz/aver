//! Unsigned fixed-width Int ↔ Bytes codecs for wasm-gc.
//!
//! The source calls force the `$AverInt` representation: encoding reads its
//! little-endian 32-bit magnitude limbs directly into the proof-selected
//! Bytes carrier, while decoding fills the same magnitude shape and funnels
//! through `__aint_normalize`. No i64 narrowing and no `List<Int>` exist on
//! the packed path.

use wasm_encoder::Function;

use super::super::{WasmGcError, wat_helper};
use super::TypeRegistry;

struct EndianTypes {
    string_idx: u32,
    bytes_idx: u32,
    result_idx: u32,
    list_idx: Option<u32>,
    mag_idx: u32,
    aint_idx: u32,
    packed: bool,
}

impl EndianTypes {
    fn discover(registry: &TypeRegistry) -> Result<Self, WasmGcError> {
        if !registry.bignum {
            return Err(WasmGcError::Validation(
                "Int endian codecs require the unbounded Int representation".into(),
            ));
        }
        let string_idx = registry
            .string_array_type_idx
            .ok_or_else(|| WasmGcError::Validation("Int endian codec requires String".into()))?;
        let packed_bytes = registry.packed_sequence("Bytes");
        if let Some(packed) = packed_bytes
            && !matches!(
                packed.layout.element,
                crate::codegen::proof_lower::PackedIntElement::U8
            )
        {
            return Err(WasmGcError::Validation(
                "Int endian codec requires an unsigned-octet Bytes layout".into(),
            ));
        }
        let bytes_idx = packed_bytes
            .map(|packed| packed.type_idx)
            .or_else(|| registry.record_type_idx("Bytes"))
            .ok_or_else(|| WasmGcError::Validation("Int endian codec requires Bytes".into()))?;
        let result_idx = registry
            .result_type_idx("Result<Bytes,String>")
            .ok_or_else(|| {
                WasmGcError::Validation("Int endian codec requires Result<Bytes,String>".into())
            })?;
        let list_idx = packed_bytes
            .is_none()
            .then(|| registry.list_type_idx("List<Int>"))
            .flatten();
        if packed_bytes.is_none() && list_idx.is_none() {
            return Err(WasmGcError::Validation(
                "boxed Int endian codec requires List<Int>".into(),
            ));
        }
        let mag_idx = registry.aint_mag_array_idx.ok_or_else(|| {
            WasmGcError::Validation("Int endian codec requires the magnitude array".into())
        })?;
        let aint_idx = registry.aint_struct_idx.ok_or_else(|| {
            WasmGcError::Validation("Int endian codec requires the AverInt struct".into())
        })?;
        Ok(Self {
            string_idx,
            bytes_idx,
            result_idx,
            list_idx,
            mag_idx,
            aint_idx,
            packed: packed_bytes.is_some(),
        })
    }

    fn declarations(&self) -> Result<String, WasmGcError> {
        let mut indices = vec![
            self.string_idx,
            self.bytes_idx,
            self.result_idx,
            self.mag_idx,
            self.aint_idx,
        ];
        indices.extend(self.list_idx);
        let mut unique = indices.clone();
        unique.sort_unstable();
        unique.dedup();
        if unique.len() != indices.len() {
            return Err(WasmGcError::Validation(
                "Int endian codec type indices overlap".into(),
            ));
        }
        let max = *unique.last().expect("endian codec types are non-empty");
        let mut out = String::from("(rec\n");
        for idx in 0..=max {
            let ty = if idx == self.string_idx {
                "(type $string (array (mut i8)))"
            } else if idx == self.bytes_idx && self.packed {
                "(type $bytes (array (mut i8)))"
            } else if idx == self.bytes_idx {
                "(type $bytes (struct (field (ref null $list_int))))"
            } else if idx == self.result_idx {
                "(type $result (struct (field (mut i32)) (field (mut (ref null $bytes))) (field (mut (ref null $string)))))"
            } else if Some(idx) == self.list_idx {
                "(type $list_int (struct (field (ref null $aint)) (field (ref null $list_int))))"
            } else if idx == self.mag_idx {
                "(type $mag (array (mut i64)))"
            } else if idx == self.aint_idx {
                "(type $aint (struct (field $small (mut i64)) (field $magf (mut (ref null $mag))) (field $sign (mut i32))))"
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

fn string_literal(text: &str) -> String {
    let bytes = text
        .as_bytes()
        .iter()
        .map(|byte| format!("i32.const {byte}"))
        .collect::<Vec<_>>()
        .join(" ");
    format!("{bytes} array.new_fixed $string {}", text.len())
}

pub(super) fn emit_to_endian(
    registry: &TypeRegistry,
    big_endian: bool,
) -> Result<Function, WasmGcError> {
    let types = EndianTypes::discover(registry)?;
    let declarations = types.declarations()?;
    let decompose = registry.aint_decompose_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Int endian codec requires __aint_decompose".into())
    })?;
    let strip = registry
        .aint_strip_fn_idx
        .ok_or_else(|| WasmGcError::Validation("Int endian codec requires __aint_strip".into()))?;
    let mut stubs = vec![
        wat_helper::CalleeStub {
            abs_idx: decompose,
            sig: "(param (ref null $aint)) (result (ref null $mag) i32)",
        },
        wat_helper::CalleeStub {
            abs_idx: strip,
            sig: "(param (ref null $mag)) (result i32)",
        },
    ];
    let from_i64 = if types.packed {
        None
    } else {
        let idx = registry.aint_from_i64_fn_idx.ok_or_else(|| {
            WasmGcError::Validation("boxed Int endian codec requires __aint_from_i64".into())
        })?;
        stubs.push(wat_helper::CalleeStub {
            abs_idx: idx,
            sig: "(param i64) (result (ref null $aint))",
        });
        Some(idx)
    };
    let callees = wat_helper::func_placeholders(&stubs);
    let operation = if big_endian {
        "Int.toBigEndian"
    } else {
        "Int.toLittleEndian"
    };
    let width_error = string_literal(&aver_rt::int_endian_width_error_message(operation));
    let value_error = string_literal(&aver_rt::int_endian_value_error_message(operation));
    let limit = aver_rt::MAX_MATERIALIZED_SEQUENCE_ELEMENTS;
    let position = if big_endian {
        "local.get $width32 i32.const 1 i32.sub local.get $i i32.sub"
    } else {
        "local.get $i"
    };
    let load_byte = format!(
        r#"{position} local.set $pos
      local.get $pos i32.const 2 i32.shr_u local.set $limb_idx
      i32.const 0 local.set $byte
      local.get $limb_idx local.get $mag_len i32.lt_u
      (if (then
        local.get $mag local.get $limb_idx array.get $mag
        local.get $pos i32.const 3 i32.and i32.const 3 i32.shl i64.extend_i32_u
        i64.shr_u i32.wrap_i64 i32.const 255 i32.and local.set $byte))"#
    );
    let (carrier_locals, build) = if types.packed {
        (
            "(local $out (ref null $bytes))".to_string(),
            format!(
                r#"local.get $width32 array.new_default $bytes local.set $out
  i32.const 0 local.set $i
  (block $copy_done (loop $copy
    local.get $i local.get $width32 i32.ge_u br_if $copy_done
    {load_byte}
    local.get $out local.get $i local.get $byte array.set $bytes
    local.get $i i32.const 1 i32.add local.set $i
    br $copy))
  local.get $out"#
            ),
        )
    } else {
        let from_i64 = from_i64.expect("boxed carrier registered from-i64");
        (
            "(local $node (ref null $list_int))".to_string(),
            format!(
                r#"local.get $width32 local.set $i
  (block $copy_done (loop $copy
    local.get $i i32.eqz br_if $copy_done
    local.get $i i32.const 1 i32.sub local.set $i
    {load_byte}
    local.get $byte i64.extend_i32_u call {from_i64}
    local.get $node struct.new $list_int local.set $node
    br $copy))
  local.get $node struct.new $bytes"#
            ),
        )
    };
    let wat = format!(
        r#"(module
  {declarations}
  {callees}
  (func (export "helper")
    (param $value (ref null $aint)) (param $width (ref null $aint))
    (result (ref null $result))
    (local $width64 i64) (local $width32 i32)
    (local $mag (ref null $mag)) (local $sign i32) (local $mag_len i32)
    (local $required i64) (local $top i64)
    (local $i i32) (local $pos i32) (local $limb_idx i32) (local $byte i32)
    {carrier_locals}

    ;; A Big width is necessarily outside the portable element budget.
    local.get $width ref.as_non_null struct.get $aint $magf ref.is_null i32.eqz
    (if (then
      i32.const 0 ref.null $bytes {width_error} struct.new $result return))
    local.get $width ref.as_non_null struct.get $aint $small local.set $width64
    local.get $width64 i64.const 0 i64.lt_s
    local.get $width64 i64.const {limit} i64.gt_s i32.or
    (if (then
      i32.const 0 ref.null $bytes {width_error} struct.new $result return))
    local.get $width64 i32.wrap_i64 local.set $width32

    local.get $value call {decompose} local.set $sign local.set $mag
    local.get $mag call {strip} local.set $mag_len
    local.get $sign i32.const 0 i32.lt_s
    (if (then
      i32.const 0 ref.null $bytes {value_error} struct.new $result return))

    i64.const 0 local.set $required
    local.get $sign i32.eqz i32.eqz
    (if (then
      local.get $mag local.get $mag_len i32.const 1 i32.sub array.get $mag local.set $top
      local.get $mag_len i64.extend_i32_u i64.const 1 i64.sub i64.const 4 i64.mul
      i64.const 32 local.get $top i32.wrap_i64 i32.clz i64.extend_i32_u i64.sub
      i64.const 7 i64.add i64.const 8 i64.div_u i64.add local.set $required))
    local.get $required local.get $width64 i64.gt_u
    (if (then
      i32.const 0 ref.null $bytes {value_error} struct.new $result return))

    i32.const 1
    {build}
    ref.null $string
    struct.new $result)
)"#
    );
    wat_helper::compile_wat_helper(&wat)
}

pub(super) fn emit_from_endian(
    registry: &TypeRegistry,
    big_endian: bool,
) -> Result<Function, WasmGcError> {
    let types = EndianTypes::discover(registry)?;
    let declarations = types.declarations()?;
    let normalize = registry.aint_normalize_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Int endian decoder requires __aint_normalize".into())
    })?;
    let mut stubs = vec![wat_helper::CalleeStub {
        abs_idx: normalize,
        sig: "(param (ref null $mag) i32) (result (ref null $aint))",
    }];
    let to_i64 = if types.packed {
        None
    } else {
        let idx = registry.aint_to_i64_checked_fn_idx.ok_or_else(|| {
            WasmGcError::Validation(
                "boxed Int endian decoder requires __aint_to_i64_checked".into(),
            )
        })?;
        stubs.push(wat_helper::CalleeStub {
            abs_idx: idx,
            sig: "(param (ref null $aint)) (result i64)",
        });
        Some(idx)
    };
    let callees = wat_helper::func_placeholders(&stubs);
    let position = if big_endian {
        "local.get $len i32.const 1 i32.sub local.get $i i32.sub"
    } else {
        "local.get $i"
    };
    let (carrier_locals, length, load, advance) = if types.packed {
        (
            String::new(),
            "local.get $input array.len local.set $len".to_string(),
            "local.get $input local.get $i array.get_u $bytes local.set $byte".to_string(),
            String::new(),
        )
    } else {
        let to_i64 = to_i64.expect("boxed carrier registered to-i64");
        (
            "(local $node (ref null $list_int))".to_string(),
            r#"local.get $input ref.as_non_null struct.get $bytes 0 local.set $node
  (block $count_done (loop $count
    local.get $node ref.is_null br_if $count_done
    local.get $len i32.const 1 i32.add local.set $len
    local.get $node ref.as_non_null struct.get $list_int 1 local.set $node
    br $count))
  local.get $input ref.as_non_null struct.get $bytes 0 local.set $node"#
                .to_string(),
            format!(
                "local.get $node ref.as_non_null struct.get $list_int 0 call {to_i64} i32.wrap_i64 local.set $byte"
            ),
            "local.get $node ref.as_non_null struct.get $list_int 1 local.set $node".to_string(),
        )
    };
    let wat = format!(
        r#"(module
  {declarations}
  {callees}
  (func (export "helper") (param $input (ref null $bytes)) (result (ref null $aint))
    (local $len i32) (local $mag_len i32)
    (local $mag (ref null $mag))
    (local $i i32) (local $pos i32) (local $limb_idx i32)
    (local $shift i64) (local $byte i32) (local $limb i64)
    {carrier_locals}

    {length}
    local.get $len i32.const 3 i32.add i32.const 2 i32.shr_u local.set $mag_len
    local.get $mag_len i32.eqz (if (then i32.const 1 local.set $mag_len))
    local.get $mag_len array.new_default $mag local.set $mag

    i32.const 0 local.set $i
    (block $done (loop $copy
      local.get $i local.get $len i32.ge_u br_if $done
      {load}
      {position} local.set $pos
      local.get $pos i32.const 2 i32.shr_u local.set $limb_idx
      local.get $pos i32.const 3 i32.and i32.const 3 i32.shl i64.extend_i32_u local.set $shift
      local.get $mag local.get $limb_idx array.get $mag local.set $limb
      local.get $mag local.get $limb_idx
      local.get $limb local.get $byte i64.extend_i32_u local.get $shift i64.shl i64.or
      array.set $mag
      {advance}
      local.get $i i32.const 1 i32.add local.set $i
      br $copy))

    local.get $mag i32.const 1 call {normalize})
)"#
    );
    wat_helper::compile_wat_helper(&wat)
}
