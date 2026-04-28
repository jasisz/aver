;; Box a scalar into a heap object so it survives polymorphic call
;; boundaries (Vector cells, Map values, Option/Result payloads). The
;; layout is an 8-byte header + an 8-byte cell at offset 8 — the same
;; one rt_unwrap_* reads from. ptr_flag goes into the header's meta
;; field so a copying GC pass can tell which boxed cells hold heap
;; pointers vs raw scalars.

(func $rt_wrap (param $tag i32) (param $inner i64) (param $ptr_flag i32) (result i32)
  (local $obj i32)
  i32.const 16
  call $rt_alloc
  local.set $obj
  ;; header = (OBJ_WRAPPER << 56) | (tag << 48) | (ptr_flag << 32) | 1
  local.get $obj
  i64.const 0x0300000000000000   ;; OBJ_WRAPPER (3) << 56
  local.get $tag
  i64.extend_i32_u
  i64.const 48
  i64.shl
  i64.or
  local.get $ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.const 1
  i64.or
  i64.store
  ;; cell = inner
  local.get $obj
  local.get $inner
  i64.store offset=8
  local.get $obj
)
(export "rt_wrap" (func $rt_wrap))

(func $rt_wrap_f64 (param $tag i32) (param $inner f64) (result i32)
  (local $obj i32)
  i32.const 16
  call $rt_alloc
  local.set $obj
  ;; header = (OBJ_WRAPPER_F64 << 56) | (tag << 48) | 1
  local.get $obj
  i64.const 0x0700000000000000   ;; OBJ_WRAPPER_F64 (7) << 56
  local.get $tag
  i64.extend_i32_u
  i64.const 48
  i64.shl
  i64.or
  i64.const 1
  i64.or
  i64.store
  ;; cell = inner
  local.get $obj
  local.get $inner
  f64.store offset=8
  local.get $obj
)
(export "rt_wrap_f64" (func $rt_wrap_f64))

(func $rt_wrap_i32 (param $tag i32) (param $inner i32) (param $ptr_flag i32) (result i32)
  (local $obj i32)
  i32.const 16
  call $rt_alloc
  local.set $obj
  ;; header = (OBJ_WRAPPER_I32 << 56) | (tag << 48) | (ptr_flag << 32) | 1
  local.get $obj
  i64.const 0x0800000000000000   ;; OBJ_WRAPPER_I32 (8) << 56
  local.get $tag
  i64.extend_i32_u
  i64.const 48
  i64.shl
  i64.or
  local.get $ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.const 1
  i64.or
  i64.store
  ;; cell = inner (sign-extended to fill the 8-byte slot)
  local.get $obj
  local.get $inner
  i64.extend_i32_s
  i64.store offset=8
  local.get $obj
)
(export "rt_wrap_i32" (func $rt_wrap_i32))
