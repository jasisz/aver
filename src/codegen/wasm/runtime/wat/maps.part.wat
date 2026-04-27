;; Map operations: association lists of (key_ptr, value_i64) tuples,
;; chained through OBJ_MAP_ENTRY cons cells (same shape as
;; OBJ_LIST_CONS but tagged differently so codegen can distinguish
;; maps from plain lists). Keys compared by rt_str_eq.

(func $rt_map_get (param $map i32) (param $key i32) (result i32)
  (local $ptr i32)
  (local $entry i32)
  (local $entry_key i32)
  (local $entry_meta i32)

  local.get $map
  local.set $ptr

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      ;; entry = head(ptr) — tuple ptr
      local.get $ptr
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      local.set $entry

      ;; entry_meta = entry.meta
      local.get $entry
      call $rt_obj_meta
      local.set $entry_meta

      ;; entry_key = entry.field0
      local.get $entry
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      local.set $entry_key

      ;; if str_eq(entry_key, key) → return Some(value)
      local.get $entry_key
      local.get $key
      call $rt_str_eq
      if
        ;; wrap(WRAP_SOME, entry.field1, value_ptr_flag)
        i32.const 2   ;; WRAP_SOME
        local.get $entry
        i32.const 1
        call $rt_obj_field
        local.get $entry_meta
        i32.const 1
        i32.shr_u
        i32.const 1
        i32.and
        call $rt_wrap
        return
      end

      ;; ptr = tail
      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr
      br 0
    end
  end

  i32.const -1   ;; NONE_SENTINEL
)
(export "rt_map_get" (func $rt_map_get))

(func $rt_map_has (param $map i32) (param $key i32) (result i32)
  (local $ptr i32)

  local.get $map
  local.set $ptr

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      ;; tuple_ptr = head(ptr); entry_key = tuple.field0
      local.get $ptr
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64

      local.get $key
      call $rt_str_eq
      if
        i32.const 1
        return
      end

      local.get $ptr
      i32.const 1
      call $rt_obj_field_i32
      local.set $ptr
      br 0
    end
  end
  i32.const 0
)
(export "rt_map_has" (func $rt_map_has))

(func $rt_map_keys (param $map i32) (result i32)
  (local $ptr i32)
  (local $acc i32)
  (local $key i64)

  local.get $map
  local.set $ptr
  i32.const 0
  local.set $acc

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      ;; key = entry_tuple.field0 (kept as i64)
      local.get $ptr
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      i32.const 0
      call $rt_obj_field
      local.set $key

      ;; acc = cons(key, acc, head_ptr_flag = 1)
      local.get $key
      local.get $acc
      i32.const 1
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
  call $rt_list_reverse
)
(export "rt_map_keys" (func $rt_map_keys))

(func $rt_map_entries (param $map i32) (result i32)
  (local $ptr i32)
  (local $acc i32)

  local.get $map
  local.set $ptr
  i32.const 0
  local.set $acc

  block
    loop
      local.get $ptr
      i32.eqz
      br_if 1

      ;; tuple_ptr_as_i64 = head(ptr)
      local.get $ptr
      i32.const 0
      call $rt_obj_field

      local.get $acc
      i32.const 1
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
  call $rt_list_reverse
)
(export "rt_map_entries" (func $rt_map_entries))

;; rt_map_set — fast path (no duplicate key) prepends a fresh
;; (tuple, map) cons. Slow path filters out the existing entry,
;; rebuilds in original order, then prepends the new entry.
(func $rt_map_set
    (param $map i32)
    (param $key i32)
    (param $value i64)
    (param $value_ptr_flag i32)
    (result i32)
  (local $tuple_ptr i32)
  (local $cur i32)
  (local $entry_tuple i32)
  (local $entry_key i32)
  (local $kept_rev i32)
  (local $filtered_tail i32)
  (local $map_cell i32)
  (local $found_dup i32)

  ;; Build the (key, value) tuple — header tag bit 1 carries
  ;; value_ptr_flag (so a future GC pass can mark the value cell).
  i32.const 24
  call $rt_alloc
  local.set $tuple_ptr

  ;; header = (OBJ_TUPLE << 56)
  ;;        | ((1 | (value_ptr_flag << 1)) << 32)
  ;;        | 2
  local.get $tuple_ptr
  i64.const 0x0500000000000000   ;; OBJ_TUPLE (5) << 56
  i64.const 1
  local.get $value_ptr_flag
  i64.extend_i32_u
  i64.const 1
  i64.shl
  i64.or
  i64.const 32
  i64.shl
  i64.or
  i64.const 2
  i64.or
  i64.store

  ;; field0 = key (i32 → i64)
  local.get $tuple_ptr
  local.get $key
  i64.extend_i32_u
  i64.store offset=8

  ;; field1 = value (i64)
  local.get $tuple_ptr
  local.get $value
  i64.store offset=16

  ;; --- Fast path: scan for duplicate ---
  i32.const 0
  local.set $found_dup
  local.get $map
  local.set $cur

  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      ;; entry_key
      local.get $cur
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64

      local.get $key
      call $rt_str_eq
      if
        i32.const 1
        local.set $found_dup
        br 2
      end

      local.get $cur
      i32.const 1
      call $rt_obj_field_i32
      local.set $cur
      br 0
    end
  end

  ;; If no duplicate: prepend new tuple to original map.
  local.get $found_dup
  i32.eqz
  if (result i32)
    i32.const 24
    call $rt_alloc
    local.set $map_cell

    local.get $map_cell
    i64.const 0x0B00000000000002   ;; (OBJ_MAP_ENTRY << 56) | field_count 2
    i64.store

    local.get $map_cell
    local.get $tuple_ptr
    i64.extend_i32_u
    i64.store offset=8

    local.get $map_cell
    local.get $map
    i64.extend_i32_s
    i64.store offset=16

    local.get $map_cell
  else
    ;; Slow path: collect non-dup entries in reverse via list_cons,
    ;; then rebuild forward order with fresh OBJ_MAP_ENTRY cells,
    ;; then prepend the new tuple.
    i32.const 0
    local.set $kept_rev
    local.get $map
    local.set $cur

    block
      loop
        local.get $cur
        i32.eqz
        br_if 1

        local.get $cur
        i32.const 0
        call $rt_obj_field
        i32.wrap_i64
        local.set $entry_tuple

        local.get $entry_tuple
        i32.const 0
        call $rt_obj_field
        i32.wrap_i64
        local.set $entry_key

        local.get $entry_key
        local.get $key
        call $rt_str_eq
        if
          ;; skip duplicate
        else
          local.get $entry_tuple
          i64.extend_i32_u
          local.get $kept_rev
          i32.const 1
          call $rt_list_cons
          local.set $kept_rev
        end

        local.get $cur
        i32.const 1
        call $rt_obj_field_i32
        local.set $cur
        br 0
      end
    end

    ;; Rebuild forward (filtered_tail starts empty).
    i32.const 0
    local.set $filtered_tail
    local.get $kept_rev
    local.set $cur

    block
      loop
        local.get $cur
        i32.eqz
        br_if 1

        i32.const 24
        call $rt_alloc
        local.set $map_cell

        local.get $map_cell
        i64.const 0x0B00000000000002
        i64.store

        local.get $map_cell
        local.get $cur
        i32.const 0
        call $rt_obj_field
        i64.store offset=8

        local.get $map_cell
        local.get $filtered_tail
        i64.extend_i32_s
        i64.store offset=16

        local.get $map_cell
        local.set $filtered_tail

        local.get $cur
        i32.const 1
        call $rt_obj_field_i32
        local.set $cur
        br 0
      end
    end

    ;; Prepend new tuple to filtered_tail.
    i32.const 24
    call $rt_alloc
    local.set $map_cell

    local.get $map_cell
    i64.const 0x0B00000000000002
    i64.store

    local.get $map_cell
    local.get $tuple_ptr
    i64.extend_i32_u
    i64.store offset=8

    local.get $map_cell
    local.get $filtered_tail
    i64.extend_i32_s
    i64.store offset=16

    local.get $map_cell
  end
)
(export "rt_map_set" (func $rt_map_set))
