;; Heap-object header inspection + field access. Every aver heap value
;; carries an 8-byte header at offset 0:
;;   bits 56..63   kind   (u8)   - OBJ_* tag (string, list cons, tuple, etc.)
;;   bits 48..55   tag    (u8)   - variant tag for sum types
;;   bits 32..47   meta   (u16)  - type id / namespace meta
;;   bits  0..31   fields (u32)  - field count / byte length
;; Field cells live at ptr + 8 + idx*8 (8-byte slots regardless of width).

(func $rt_obj_kind (param $ptr i32) (result i32)
  local.get $ptr
  i64.load
  i64.const 56
  i64.shr_u
  i32.wrap_i64
  i32.const 0xFF
  i32.and
)
(export "rt_obj_kind" (func $rt_obj_kind))

(func $rt_obj_tag (param $ptr i32) (result i32)
  local.get $ptr
  i64.load
  i64.const 48
  i64.shr_u
  i32.wrap_i64
  i32.const 0xFF
  i32.and
)
(export "rt_obj_tag" (func $rt_obj_tag))

(func $rt_obj_meta (param $ptr i32) (result i32)
  local.get $ptr
  i64.load
  i64.const 32
  i64.shr_u
  i32.wrap_i64
  i32.const 0xFFFF
  i32.and
)
(export "rt_obj_meta" (func $rt_obj_meta))

(func $rt_obj_field (param $ptr i32) (param $idx i32) (result i64)
  local.get $ptr
  i32.const 8
  i32.add
  local.get $idx
  i32.const 8
  i32.mul
  i32.add
  i64.load
)
(export "rt_obj_field" (func $rt_obj_field))

(func $rt_obj_field_f64 (param $ptr i32) (param $idx i32) (result f64)
  local.get $ptr
  i32.const 8
  i32.add
  local.get $idx
  i32.const 8
  i32.mul
  i32.add
  f64.load
)
(export "rt_obj_field_f64" (func $rt_obj_field_f64))

(func $rt_obj_field_i32 (param $ptr i32) (param $idx i32) (result i32)
  local.get $ptr
  i32.const 8
  i32.add
  local.get $idx
  i32.const 8
  i32.mul
  i32.add
  i64.load
  i32.wrap_i64
)
(export "rt_obj_field_i32" (func $rt_obj_field_i32))
