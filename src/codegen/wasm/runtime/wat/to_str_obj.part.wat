;; Numeric → OBJ_STRING heap object. Both fns format into the
;; runtime's IO scratch (IO_INT_BUF at 16, IO_FLOAT_BUF at 48), then
;; alloc + copy a fresh OBJ_STRING that survives beyond the next
;; format call.

(func $rt_i64_to_str_obj (param $val i64) (result i32)
  (local $packed i32)
  (local $pos i32)
  (local $len i32)
  (local $ptr i32)

  local.get $val
  i32.const 16   ;; IO_INT_BUF
  call $rt_int_to_str
  local.set $packed

  ;; pos = packed >> 16
  local.get $packed
  i32.const 16
  i32.shr_u
  local.set $pos

  ;; len = packed & 0xFFFF
  local.get $packed
  i32.const 0xFFFF
  i32.and
  local.set $len

  ;; alloc 8 + align8(len)
  i32.const 8
  local.get $len
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  ;; header = (OBJ_STRING << 56) | len   (OBJ_STRING is 0)
  local.get $ptr
  i64.const 0
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  ;; memory.copy: dst = ptr+8, src = IO_INT_BUF + pos, len = len
  local.get $ptr
  i32.const 8
  i32.add
  i32.const 16
  local.get $pos
  i32.add
  local.get $len
  memory.copy

  local.get $ptr
)
(export "rt_i64_to_str_obj" (func $rt_i64_to_str_obj))

(func $rt_f64_to_str_obj (param $val f64) (result i32)
  (local $packed i32)
  (local $pos i32)
  (local $len i32)
  (local $ptr i32)

  local.get $val
  i32.const 48   ;; IO_FLOAT_BUF
  call $rt_float_to_str
  local.set $packed

  local.get $packed
  i32.const 16
  i32.shr_u
  local.set $pos

  local.get $packed
  i32.const 0xFFFF
  i32.and
  local.set $len

  i32.const 8
  local.get $len
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  local.get $ptr
  i32.const 8
  i32.add
  i32.const 48
  local.get $pos
  i32.add
  local.get $len
  memory.copy

  local.get $ptr
)
(export "rt_f64_to_str_obj" (func $rt_f64_to_str_obj))
