;; Linked-list traversal helpers. Layout per cons cell (24 bytes):
;;   0  : header
;;   8  : head (i64 — value or pointer; meta bit 0 says which)
;;   16 : tail (i32 sign-extended into 8-byte slot — 0 = nil)
;; All ops here operate over i64-headed cons cells; f64 lists go
;; through their own paths in codegen.

(func $rt_list_take (param $list i32) (param $n i32) (result i32)
  (local $acc i32)
  (local $ptr i32)

  i32.const 0
  local.set $acc

  local.get $list
  local.set $ptr

  block
    loop
      local.get $n
      i32.eqz
      br_if 1
      local.get $ptr
      i32.eqz
      br_if 1

      ;; acc = cons(head(ptr), acc, head_ptr_flag)
      local.get $ptr
      i32.const 0
      call $rt_obj_field

      local.get $acc

      local.get $ptr
      call $rt_obj_meta
      i32.const 1
      i32.and

      call $rt_list_cons
      local.set $acc

      ;; ptr = tail(ptr)
      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr

      ;; n--
      local.get $n
      i32.const 1
      i32.sub
      local.set $n
      br 0
    end
  end

  local.get $acc
  call $rt_list_reverse
)
(export "rt_list_take" (func $rt_list_take))

(func $rt_list_drop (param $list i32) (param $n i32) (result i32)
  block
    loop
      local.get $n
      i32.eqz
      br_if 1
      local.get $list
      i32.eqz
      br_if 1

      ;; list = tail(list)  (read i64 at offset 16, wrap to i32)
      local.get $list
      i64.load offset=16
      i32.wrap_i64
      local.set $list

      local.get $n
      i32.const 1
      i32.sub
      local.set $n
      br 0
    end
  end
  local.get $list
)
(export "rt_list_drop" (func $rt_list_drop))

(func $rt_list_reverse (param $list i32) (result i32)
  (local $acc i32)
  (local $ptr i32)

  i32.const 0
  local.set $acc

  local.get $list
  local.set $ptr

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      local.get $ptr
      i32.const 0
      call $rt_obj_field

      local.get $acc

      local.get $ptr
      call $rt_obj_meta
      i32.const 1
      i32.and

      call $rt_list_cons
      local.set $acc

      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr
      br 0
    end
  end
  local.get $acc
)
(export "rt_list_reverse" (func $rt_list_reverse))

(func $rt_list_concat (param $a i32) (param $b i32) (result i32)
  (local $rev i32)
  (local $ptr i32)

  ;; rev = reverse(a)
  local.get $a
  call $rt_list_reverse
  local.set $rev

  local.get $rev
  local.set $ptr

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      ;; b = cons(head(ptr), b, head_ptr_flag)
      local.get $ptr
      i32.const 0
      call $rt_obj_field

      local.get $b

      local.get $ptr
      call $rt_obj_meta
      i32.const 1
      i32.and

      call $rt_list_cons
      local.set $b

      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr
      br 0
    end
  end
  local.get $b
)
(export "rt_list_concat" (func $rt_list_concat))

(func $rt_list_contains (param $list i32) (param $val i64) (result i32)
  block
    loop
      local.get $list
      i32.eqz
      br_if 1

      local.get $list
      i32.const 0
      call $rt_obj_field
      local.get $val
      i64.eq
      if
        i32.const 1
        return
      end

      local.get $list
      i32.const 1
      call $rt_obj_field_i32
      local.set $list
      br 0
    end
  end
  i32.const 0
)
(export "rt_list_contains" (func $rt_list_contains))

(func $rt_list_zip (param $a i32) (param $b i32) (result i32)
  (local $acc i32)
  (local $head_a i64)
  (local $head_b i64)
  (local $tuple_ptr i32)
  (local $meta_a i32)
  (local $meta_b i32)

  i32.const 0
  local.set $acc

  block
    loop
      local.get $a
      i32.eqz
      br_if 1
      local.get $b
      i32.eqz
      br_if 1

      ;; head_a / head_b
      local.get $a
      i32.const 0
      call $rt_obj_field
      local.set $head_a

      local.get $b
      i32.const 0
      call $rt_obj_field
      local.set $head_b

      ;; meta_a / meta_b — head_ptr_flag of each source list (bit 0)
      local.get $a
      call $rt_obj_meta
      i32.const 1
      i32.and
      local.set $meta_a

      local.get $b
      call $rt_obj_meta
      i32.const 1
      i32.and
      local.set $meta_b

      ;; alloc(24) → tuple_ptr
      i32.const 24
      call $rt_alloc
      local.set $tuple_ptr

      ;; tuple header:
      ;;   (OBJ_TUPLE << 56) | (((meta_a | meta_b<<1)) << 32) | 2
      local.get $tuple_ptr
      i64.const 0x0500000000000000   ;; OBJ_TUPLE (5) << 56
      local.get $meta_a
      local.get $meta_b
      i32.const 1
      i32.shl
      i32.or
      i64.extend_i32_u
      i64.const 32
      i64.shl
      i64.or
      i64.const 2
      i64.or
      i64.store

      ;; tuple cells
      local.get $tuple_ptr
      local.get $head_a
      i64.store offset=8
      local.get $tuple_ptr
      local.get $head_b
      i64.store offset=16

      ;; acc = cons(tuple_ptr_as_i64, acc, head_ptr_flag = 1)
      local.get $tuple_ptr
      i64.extend_i32_u
      local.get $acc
      i32.const 1
      call $rt_list_cons
      local.set $acc

      ;; a = tail(a), b = tail(b)
      local.get $a
      i32.const 1
      call $rt_obj_field_i32
      local.set $a

      local.get $b
      i32.const 1
      call $rt_obj_field_i32
      local.set $b
      br 0
    end
  end

  local.get $acc
  call $rt_list_reverse
)
(export "rt_list_zip" (func $rt_list_zip))
