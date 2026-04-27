;; Linked-list cons cells. Layout:
;;   offset 0   : 8-byte header (kind, head_ptr_flag in meta, field_count = 2)
;;   offset 8   : head (i64 for OBJ_LIST_CONS, f64 for OBJ_LIST_CONS_F64)
;;   offset 16  : tail (i32 sign-extended into 8-byte slot — null tail = 0)
;; Both cells take 24 bytes; rt_alloc rounds to 8-byte alignment.

(func $rt_list_cons (param $head i64) (param $tail i32) (param $head_ptr_flag i32) (result i32)
  (local $ptr i32)
  i32.const 24
  call $rt_alloc
  local.set $ptr

  ;; header = (OBJ_LIST_CONS << 56) | (head_ptr_flag << 32) | 2
  local.get $ptr
  i64.const 0x0400000000000000   ;; OBJ_LIST_CONS (4) << 56
  local.get $head_ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.const 2
  i64.or
  i64.store

  ;; head
  local.get $ptr
  local.get $head
  i64.store offset=8

  ;; tail (i32 → sign-extended to fill the 8-byte slot)
  local.get $ptr
  local.get $tail
  i64.extend_i32_s
  i64.store offset=16

  local.get $ptr
)
(export "rt_list_cons" (func $rt_list_cons))

(func $rt_list_cons_f64 (param $head f64) (param $tail i32) (result i32)
  (local $ptr i32)
  i32.const 24
  call $rt_alloc
  local.set $ptr

  ;; header = make_header(OBJ_LIST_CONS_F64, 0, 0, 2) — kind shift only.
  local.get $ptr
  i64.const 0x0900000000000002   ;; (9 << 56) | 2
  i64.store

  local.get $ptr
  local.get $head
  f64.store offset=8

  local.get $ptr
  local.get $tail
  i64.extend_i32_s
  i64.store offset=16

  local.get $ptr
)
(export "rt_list_cons_f64" (func $rt_list_cons_f64))
