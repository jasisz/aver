;; Concatenate two OBJ_STRING heap objects into a fresh one. Allocates
;; 8 (header) + align8(total_len) bytes, sets header.kind = OBJ_STRING
;; (0) and header.byte_len = total_len, then memory.copy each side's
;; payload into place.

(func $rt_str_concat (param $a i32) (param $b i32) (result i32)
  (local $len_a i32)
  (local $len_b i32)
  (local $ptr i32)
  (local $total i32)

  ;; len_a = header_a & 0xFFFFFFFF
  local.get $a
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len_a

  ;; len_b
  local.get $b
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len_b

  ;; total = len_a + len_b
  local.get $len_a
  local.get $len_b
  i32.add
  local.set $total

  ;; alloc 8 + ((total + 7) & ~7)
  i32.const 8
  local.get $total
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  ;; header = (OBJ_STRING << 56) | total
  local.get $ptr
  i64.const 0    ;; OBJ_STRING (0) << 56
  local.get $total
  i64.extend_i32_u
  i64.or
  i64.store

  ;; memory.copy a-bytes: dst = ptr+8, src = a+8, len = len_a
  local.get $ptr
  i32.const 8
  i32.add
  local.get $a
  i32.const 8
  i32.add
  local.get $len_a
  memory.copy

  ;; memory.copy b-bytes: dst = ptr+8+len_a, src = b+8, len = len_b
  local.get $ptr
  i32.const 8
  i32.add
  local.get $len_a
  i32.add
  local.get $b
  i32.const 8
  i32.add
  local.get $len_b
  memory.copy

  local.get $ptr
)
(export "rt_str_concat" (func $rt_str_concat))
