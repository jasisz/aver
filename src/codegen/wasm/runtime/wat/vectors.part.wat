;; Persistent vector operations. Flat vectors keep the original layout:
;;   offset 0   : header — kind = OBJ_VECTOR (10), meta bit 0 carries
;;                 the element-ptr flag, low 32 bits hold the length.
;;   offset 8+i*8: i64 cell per element.
;;
;; Vector.set returns a tiny patch vector instead of copying the flat array:
;;   meta bit 1 set
;;   offset 8  : base vector pointer
;;   offset 16 : patched index
;;   offset 24 : patched value
;;
;; This keeps Vector.set O(1) while preserving immutable old-vector semantics.
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

(func $rt_vec_get_cell (param $vec i32) (param $idx i32) (result i64)
  (local $cur i32)

  local.get $vec
  local.set $cur

  block
    loop
      local.get $cur
      call $rt_obj_meta
      i32.const 2
      i32.and
      i32.eqz
      if
        local.get $cur
        local.get $idx
        i32.const 8
        i32.mul
        i32.add
        i64.load offset=8
        return
      end

      local.get $cur
      i64.load offset=16
      local.get $idx
      i64.extend_i32_s
      i64.eq
      if
        local.get $cur
        i64.load offset=24
        return
      end

      local.get $cur
      i64.load offset=8
      i32.wrap_i64
      local.set $cur
      br 0
    end
  end

  unreachable
)
(export "rt_vec_get_cell" (func $rt_vec_get_cell))

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
    call $rt_vec_get_cell
    local.get $meta
    i32.const 1
    i32.and
    call $rt_wrap
  end
)
(export "rt_vec_get" (func $rt_vec_get))

(func $rt_vec_set (param $vec i32) (param $idx i64) (param $val i64) (result i32)
  (local $len i32)
  (local $patch i32)
  (local $i i32)
  (local $meta i32)

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
    local.get $vec
    call $rt_obj_meta
    i32.const 1
    i32.and
    local.set $meta

    i32.const 32
    call $rt_alloc
    local.set $patch

    ;; header = OBJ_VECTOR | elem_ptr_flag | patch_flag | len
    local.get $patch
    i64.const 0x0A00000000000000
    local.get $meta
    i32.const 2
    i32.or
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.or
    local.get $len
    i64.extend_i32_u
    i64.or
    i64.store

    local.get $patch
    local.get $vec
    i64.extend_i32_u
    i64.store offset=8

    local.get $patch
    local.get $idx
    i64.store offset=16

    local.get $patch
    local.get $val
    i64.store offset=24

    i32.const 2   ;; WRAP_SOME
    local.get $patch
    i64.extend_i32_u
    i32.const 1
    call $rt_wrap
  end
)
(export "rt_vec_set" (func $rt_vec_set))

(func $rt_vec_set_or_keep (param $vec i32) (param $idx i64) (param $val i64) (result i32)
  (local $len i32)
  (local $i i32)
  (local $patch i32)
  (local $meta i32)

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
    local.get $vec
  else
    local.get $vec
    call $rt_obj_meta
    i32.const 1
    i32.and
    local.set $meta

    i32.const 32
    call $rt_alloc
    local.set $patch

    local.get $patch
    i64.const 0x0A00000000000000
    local.get $meta
    i32.const 2
    i32.or
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.or
    local.get $len
    i64.extend_i32_u
    i64.or
    i64.store

    local.get $patch
    local.get $vec
    i64.extend_i32_u
    i64.store offset=8

    local.get $patch
    local.get $idx
    i64.store offset=16

    local.get $patch
    local.get $val
    i64.store offset=24

    local.get $patch
  end
)
(export "rt_vec_set_or_keep" (func $rt_vec_set_or_keep))

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
      call $rt_vec_get_cell

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
