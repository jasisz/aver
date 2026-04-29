;; Flat-array vector operations. Layout:
;;   offset 0   : 8-byte header — kind = OBJ_VECTOR (10), meta carries
;;                 the element-ptr flag, low 32 bits hold the length.
;;   offset 8+i*8: i64 cell per element (8-byte stride for either i64
;;                 scalars or i32 pointers, sign-extended at write time
;;                 by callers).
;; rt_vec_get / rt_vec_set return Option (rt_wrap with WRAP_SOME or
;; the NONE_SENTINEL i32 value -1).

(func $rt_vec_len (param $vec i32) (result i64)
  local.get $vec
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
)
(export "rt_vec_len" (func $rt_vec_len))

(func $rt_vec_from_list (param $list i32) (param $elem_ptr_flag i32) (result i32)
  (local $len i32)
  (local $ptr i32)
  (local $vec i32)
  (local $i i32)
  (local $cur i32)

  ;; First pass — count length.
  i32.const 0
  local.set $len
  local.get $list
  local.set $ptr
  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1
      local.get $len
      i32.const 1
      i32.add
      local.set $len
      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr
      br 0
    end
  end

  ;; alloc 8 + len*8.
  i32.const 8
  local.get $len
  i32.const 8
  i32.mul
  i32.add
  call $rt_alloc
  local.set $vec

  ;; header = (OBJ_VECTOR << 56) | (elem_ptr_flag << 32) | len
  local.get $vec
  i64.const 0x0A00000000000000   ;; OBJ_VECTOR (10) << 56
  local.get $elem_ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  ;; Second pass — copy elements.
  i32.const 0
  local.set $i
  local.get $list
  local.set $cur
  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      ;; vec[i] = head(cur)  (stored at offset 8 + i*8)
      local.get $vec
      local.get $i
      i32.const 8
      i32.mul
      i32.add
      local.get $cur
      i32.const 0
      call $rt_obj_field
      i64.store offset=8

      local.get $i
      i32.const 1
      i32.add
      local.set $i

      local.get $cur
      i32.const 1
      call $rt_obj_field_i32
      local.set $cur
      br 0
    end
  end
  local.get $vec
)
(export "rt_vec_from_list" (func $rt_vec_from_list))

(func $rt_vec_get (param $vec i32) (param $idx i64) (result i32)
  (local $len i32)
  (local $i i32)
  (local $meta i32)

  local.get $vec
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  local.get $vec
  call $rt_obj_meta
  local.set $meta

  local.get $idx
  i32.wrap_i64
  local.set $i

  ;; Bounds check.
  local.get $i
  i32.const 0
  i32.lt_s
  local.get $i
  local.get $len
  i32.ge_s
  i32.or
  if (result i32)
    i32.const -1   ;; NONE_SENTINEL
  else
    i32.const 2    ;; WRAP_SOME
    local.get $vec
    local.get $i
    i32.const 8
    i32.mul
    i32.add
    i64.load offset=8
    local.get $meta
    i32.const 1
    i32.and
    call $rt_wrap
  end
)
(export "rt_vec_get" (func $rt_vec_get))

(func $rt_vec_set (param $vec i32) (param $idx i64) (param $val i64) (result i32)
  (local $len i32)
  (local $new_vec i32)
  (local $i i32)
  (local $bytes i32)

  local.get $vec
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  local.get $idx
  i32.wrap_i64
  local.set $i

  local.get $i
  i32.const 0
  i32.lt_s
  local.get $i
  local.get $len
  i32.ge_s
  i32.or
  if (result i32)
    i32.const -1
  else
    ;; bytes = 8 + len*8
    i32.const 8
    local.get $len
    i32.const 8
    i32.mul
    i32.add
    local.set $bytes

    local.get $bytes
    call $rt_alloc
    local.set $new_vec

    ;; copy entire old vector
    local.get $new_vec
    local.get $vec
    local.get $bytes
    memory.copy

    ;; overwrite element at idx
    local.get $new_vec
    local.get $i
    i32.const 8
    i32.mul
    i32.add
    local.get $val
    i64.store offset=8

    i32.const 2   ;; WRAP_SOME
    local.get $new_vec
    i64.extend_i32_u
    i32.const 1
    call $rt_wrap
  end
)
(export "rt_vec_set" (func $rt_vec_set))

(func $rt_vec_new (param $size i64) (param $fill i64) (param $fill_ptr_flag i32) (result i32)
  (local $len i32)
  (local $vec i32)
  (local $i i32)

  local.get $size
  i32.wrap_i64
  local.set $len

  i32.const 8
  local.get $len
  i32.const 8
  i32.mul
  i32.add
  call $rt_alloc
  local.set $vec

  ;; header = (OBJ_VECTOR << 56) | (fill_ptr_flag << 32) | len
  local.get $vec
  i64.const 0x0A00000000000000
  local.get $fill_ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  ;; Fill loop.
  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $len
      i32.ge_u
      br_if 1

      local.get $vec
      local.get $i
      i32.const 8
      i32.mul
      i32.add
      local.get $fill
      i64.store offset=8

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $vec
)
(export "rt_vec_new" (func $rt_vec_new))

(func $rt_vec_to_list (param $vec i32) (result i32)
  (local $len i32)
  (local $idx i32)
  (local $acc i32)
  (local $meta i32)

  local.get $vec
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  local.get $vec
  call $rt_obj_meta
  local.set $meta

  i32.const 0
  local.set $acc

  local.get $len
  local.set $idx

  block
    loop
      local.get $idx
      i32.eqz
      br_if 1

      local.get $idx
      i32.const 1
      i32.sub
      local.set $idx

      ;; acc = cons(vec[idx], acc, head_ptr_flag)
      local.get $vec
      local.get $idx
      i32.const 8
      i32.mul
      i32.add
      i64.load offset=8

      local.get $acc

      local.get $meta
      i32.const 1
      i32.and

      call $rt_list_cons
      local.set $acc
      br 0
    end
  end

  local.get $acc
)
(export "rt_vec_to_list" (func $rt_vec_to_list))
