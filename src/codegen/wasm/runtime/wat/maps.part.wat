;; Flat hashtable Map runtime — open-addressing, linear probing.
;; Single-chunk allocation: header + buckets in one contiguous block.
;;
;; Layout
;; ------
;;   offset 0  : header word
;;     bits 56-63 : kind = 12 (OBJ_MAP)
;;     bits 32-47 : meta — bit 0 = value_ptr_flag (set if values are heap ptrs)
;;     bits  0-31 : count (number of occupied buckets)
;;   offset 8  : capacity word (i64)
;;     bits  0-31 : capacity (power of 2)
;;     bits 32-39 : key_kind (0=Int, 1=Float, 2=Bool, 3=String, 4=heap)
;;   offset 16 : bucket array (capacity × 24 bytes each)
;;
;; Bucket layout (24 bytes)
;;   offset 0   : state — 0 = empty, 1 = occupied, 2 = tombstone
;;   offset 4   : hash i32 (cached, valid when state == 1)
;;   offset 8   : key i64
;;   offset 16  : value i64
;;
;; Empty map sentinel: pointer 0 (no allocation). `rt_map_set` allocates
;; a fresh table with `INITIAL_CAP` buckets on first insert.
;;
;; Polymorphic key ABI (unchanged from prior HAMT runtime):
;;   rt_map_get(map, key, kind) → Option<V>
;;   rt_map_has(map, key, kind) → bool
;;   rt_map_set(map, key, kind, value, value_ptr_flag) → new map
;;   rt_map_set_owned(map, key, kind, value, value_ptr_flag) → mutated map
;;   rt_map_keys(map) → List<K>
;;   rt_map_entries(map) → List<(K, V)>
;;   rt_map_len(map) → i64
;;   rt_map_from_list(list, key_kind, value_ptr_flag) → map
;;
;; Owned-mutate (`rt_map_set_owned`) is the canonical TCO build path.
;; When last-use analysis proves `map` has no other observer, the
;; emitter calls _owned and the runtime mutates buckets in place
;; (zero alloc on update + amortized O(1) on insert; resize still
;; allocates a fresh table).

;; ---------------------------------------------------------------------
;; Hashing — same surface as before so existing rt_hash_key callers
;; (rt_map_from_list internally) keep working.
;; ---------------------------------------------------------------------

(func $rt_mix_i64 (param $x i64) (result i32)
  (local $h i64)
  local.get $x
  local.set $h

  local.get $h
  local.get $h
  i64.const 33
  i64.shr_u
  i64.xor
  i64.const 0xff51afd7ed558ccd
  i64.mul
  local.set $h

  local.get $h
  local.get $h
  i64.const 33
  i64.shr_u
  i64.xor
  i64.const 0xc4ceb9fe1a85ec53
  i64.mul
  local.set $h

  local.get $h
  local.get $h
  i64.const 33
  i64.shr_u
  i64.xor
  i32.wrap_i64
)

(func $rt_hash_string_obj (param $ptr i32) (result i32)
  (local $len i32)
  (local $i i32)
  (local $h i32)

  local.get $ptr
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  i32.const 5381
  local.set $h
  i32.const 0
  local.set $i

  block
    loop
      local.get $i
      local.get $len
      i32.ge_u
      br_if 1

      local.get $h
      i32.const 5
      i32.shl
      local.get $h
      i32.add
      local.get $ptr
      i32.const 8
      i32.add
      local.get $i
      i32.add
      i32.load8_u
      i32.add
      local.set $h

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $h
)

(func $rt_deep_hash_fwd (param $ptr i32) (result i32)
  local.get $ptr
  call $rt_deep_hash
)

;; Deep structural hash for heap keys (kind == 4). Walks the object by
;; its OBJ_KIND header byte and recurses on i64 fields whose
;; per-field ptr-flag is set in the meta byte.
(func $rt_deep_hash (param $ptr i32) (result i32)
  (local $kind i32)
  (local $tag i32)
  (local $meta i32)
  (local $count i32)
  (local $i i32)
  (local $h i32)
  (local $field i64)
  (local $is_ptr i32)
  (local $sub i32)

  local.get $ptr
  i32.eqz
  if
    i32.const 0
    return
  end

  local.get $ptr
  call $rt_obj_kind
  local.set $kind

  local.get $ptr
  call $rt_obj_tag
  local.set $tag

  local.get $ptr
  call $rt_obj_meta
  local.set $meta

  ;; OBJ_STRING — djb2 of bytes
  local.get $kind
  i32.const 0
  i32.eq
  if
    local.get $ptr
    call $rt_hash_string_obj
    return
  end

  ;; LIST_CONS family — head + rotated tail hash
  local.get $kind
  i32.const 4
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field
    local.set $field
    local.get $meta
    i32.const 1
    i32.and
    local.set $is_ptr

    local.get $is_ptr
    if (result i32)
      local.get $field
      i32.wrap_i64
      call $rt_deep_hash_fwd
    else
      local.get $field
      call $rt_mix_i64
    end
    local.set $h

    local.get $ptr
    i32.const 1
    call $rt_obj_field_i32
    local.set $sub

    local.get $h
    i32.const 16
    i32.rotl
    local.get $sub
    call $rt_deep_hash_fwd
    i32.xor
    return
  end

  local.get $kind
  i32.const 9
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field_f64
    i64.reinterpret_f64
    call $rt_mix_i64
    local.set $h

    local.get $ptr
    i32.const 1
    call $rt_obj_field_i32
    local.set $sub

    local.get $h
    i32.const 16
    i32.rotl
    local.get $sub
    call $rt_deep_hash_fwd
    i32.xor
    return
  end

  ;; OBJ_TUPLE / OBJ_RECORD / OBJ_VARIANT — meta byte = ptr bitmap
  local.get $kind
  i32.const 5
  i32.eq
  local.get $kind
  i32.const 1
  i32.eq
  i32.or
  local.get $kind
  i32.const 2
  i32.eq
  i32.or
  if
    local.get $ptr
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $count

    local.get $kind
    i32.const 8
    i32.shl
    local.get $tag
    i32.or
    local.set $h

    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1

        local.get $ptr
        local.get $i
        call $rt_obj_field
        local.set $field

        local.get $meta
        local.get $i
        i32.shr_u
        i32.const 1
        i32.and
        local.set $is_ptr

        local.get $is_ptr
        if (result i32)
          local.get $field
          i32.wrap_i64
          call $rt_deep_hash_fwd
        else
          local.get $field
          call $rt_mix_i64
        end
        local.set $sub

        local.get $h
        i32.const 5
        i32.rotl
        local.get $sub
        i32.xor
        local.set $h

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end

    local.get $h
    return
  end

  ;; OBJ_WRAPPER families — wrap kind + tag + inner.
  local.get $kind
  i32.const 3
  i32.eq
  if
    local.get $kind
    i32.const 8
    i32.shl
    local.get $tag
    i32.or
    local.set $h
    local.get $ptr
    i32.const 0
    call $rt_obj_field
    call $rt_mix_i64
    local.get $h
    i32.xor
    return
  end

  local.get $kind
  i32.const 7
  i32.eq
  if
    local.get $kind
    i32.const 8
    i32.shl
    local.get $tag
    i32.or
    local.set $h
    local.get $ptr
    i32.const 0
    call $rt_obj_field_f64
    i64.reinterpret_f64
    call $rt_mix_i64
    local.get $h
    i32.xor
    return
  end

  local.get $kind
  i32.const 8
  i32.eq
  if
    local.get $kind
    i32.const 8
    i32.shl
    local.get $tag
    i32.or
    local.set $h
    local.get $ptr
    i32.const 0
    call $rt_obj_field_i32
    i64.extend_i32_s
    call $rt_mix_i64
    local.get $h
    i32.xor
    return
  end

  ;; Fallback: identity-of-ptr.
  local.get $ptr
  i64.extend_i32_s
  call $rt_mix_i64
)

;; Hash a key under the polymorphic key kind (0=Int, 1=Float, 2=Bool,
;; 3=String OBJ_STRING ptr, 4=heap arbitrary).
(func $rt_hash_key (param $key i64) (param $kind i32) (result i32)
  local.get $kind
  i32.const 3
  i32.eq
  if
    local.get $key
    i32.wrap_i64
    call $rt_hash_string_obj
    return
  end
  local.get $kind
  i32.const 4
  i32.eq
  if
    local.get $key
    i32.wrap_i64
    call $rt_deep_hash_fwd
    return
  end
  local.get $key
  call $rt_mix_i64
)

;; Forward decl alias for recursive equality.
(func $rt_map_key_eq_fwd (param $a i64) (param $b i64) (param $kind i32) (result i32)
  local.get $a
  local.get $b
  local.get $kind
  call $rt_map_key_eq
)

;; Deep equality for two heap pointers (kind == 4): compare object
;; structure recursively. Mirrors `rt_deep_hash`.
(func $rt_deep_eq (param $a i32) (param $b i32) (result i32)
  (local $ka i32)
  (local $kb i32)
  (local $ta i32)
  (local $tb i32)
  (local $ma i32)
  (local $mb i32)
  (local $ca i32)
  (local $cb i32)
  (local $i i32)
  (local $fa i64)
  (local $fb i64)
  (local $is_ptr i32)
  (local $sa i32)
  (local $sb i32)

  local.get $a
  local.get $b
  i32.eq
  if
    i32.const 1
    return
  end

  local.get $a
  i32.eqz
  local.get $b
  i32.eqz
  i32.or
  if
    i32.const 0
    return
  end

  local.get $a
  call $rt_obj_kind
  local.set $ka
  local.get $b
  call $rt_obj_kind
  local.set $kb
  local.get $ka
  local.get $kb
  i32.ne
  if
    i32.const 0
    return
  end

  local.get $a
  call $rt_obj_tag
  local.set $ta
  local.get $b
  call $rt_obj_tag
  local.set $tb
  local.get $ta
  local.get $tb
  i32.ne
  if
    i32.const 0
    return
  end

  local.get $a
  call $rt_obj_meta
  local.set $ma
  local.get $b
  call $rt_obj_meta
  local.set $mb

  ;; STRING: byte equality via existing rt_str_eq.
  local.get $ka
  i32.const 0
  i32.eq
  if
    local.get $a
    local.get $b
    call $rt_str_eq
    return
  end

  ;; LIST_CONS i64 head: head equality + recursive tail.
  local.get $ka
  i32.const 4
  i32.eq
  if
    local.get $a
    i32.const 0
    call $rt_obj_field
    local.set $fa
    local.get $b
    i32.const 0
    call $rt_obj_field
    local.set $fb
    local.get $ma
    i32.const 1
    i32.and
    local.set $is_ptr

    local.get $is_ptr
    if (result i32)
      local.get $fa
      i32.wrap_i64
      local.get $fb
      i32.wrap_i64
      call $rt_deep_eq
    else
      local.get $fa
      local.get $fb
      i64.eq
    end
    i32.eqz
    if
      i32.const 0
      return
    end

    local.get $a
    i32.const 1
    call $rt_obj_field_i32
    local.set $sa
    local.get $b
    i32.const 1
    call $rt_obj_field_i32
    local.set $sb
    local.get $sa
    local.get $sb
    call $rt_deep_eq
    return
  end

  ;; LIST_CONS f64 head.
  local.get $ka
  i32.const 9
  i32.eq
  if
    local.get $a
    i32.const 0
    call $rt_obj_field_f64
    local.get $b
    i32.const 0
    call $rt_obj_field_f64
    f64.eq
    i32.eqz
    if
      i32.const 0
      return
    end
    local.get $a
    i32.const 1
    call $rt_obj_field_i32
    local.set $sa
    local.get $b
    i32.const 1
    call $rt_obj_field_i32
    local.set $sb
    local.get $sa
    local.get $sb
    call $rt_deep_eq
    return
  end

  ;; TUPLE / RECORD / VARIANT: walk fields with meta as ptr-bitmap.
  local.get $ka
  i32.const 5
  i32.eq
  local.get $ka
  i32.const 1
  i32.eq
  i32.or
  local.get $ka
  i32.const 2
  i32.eq
  i32.or
  if
    local.get $a
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $ca
    local.get $b
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $cb
    local.get $ca
    local.get $cb
    i32.ne
    if
      i32.const 0
      return
    end

    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $ca
        i32.ge_u
        br_if 1

        local.get $a
        local.get $i
        call $rt_obj_field
        local.set $fa
        local.get $b
        local.get $i
        call $rt_obj_field
        local.set $fb
        local.get $ma
        local.get $i
        i32.shr_u
        i32.const 1
        i32.and
        local.set $is_ptr

        local.get $is_ptr
        if (result i32)
          local.get $fa
          i32.wrap_i64
          local.get $fb
          i32.wrap_i64
          call $rt_deep_eq
        else
          local.get $fa
          local.get $fb
          i64.eq
        end
        i32.eqz
        if
          i32.const 0
          return
        end

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end

    i32.const 1
    return
  end

  ;; WRAPPER families: compare inner.
  local.get $ka
  i32.const 3
  i32.eq
  if
    local.get $a
    i32.const 0
    call $rt_obj_field
    local.get $b
    i32.const 0
    call $rt_obj_field
    i64.eq
    return
  end

  local.get $ka
  i32.const 7
  i32.eq
  if
    local.get $a
    i32.const 0
    call $rt_obj_field_f64
    local.get $b
    i32.const 0
    call $rt_obj_field_f64
    f64.eq
    return
  end

  local.get $ka
  i32.const 8
  i32.eq
  if
    local.get $a
    i32.const 0
    call $rt_obj_field_i32
    local.get $b
    i32.const 0
    call $rt_obj_field_i32
    i32.eq
    return
  end

  ;; Fallback: pointer equality (already tested above).
  i32.const 0
)

;; Polymorphic key equality dispatch.
(func $rt_map_key_eq (param $a i64) (param $b i64) (param $kind i32) (result i32)
  local.get $kind
  i32.const 3
  i32.eq
  if
    local.get $a
    i32.wrap_i64
    local.get $b
    i32.wrap_i64
    call $rt_str_eq
    return
  end
  local.get $kind
  i32.const 4
  i32.eq
  if
    local.get $a
    i32.wrap_i64
    local.get $b
    i32.wrap_i64
    call $rt_deep_eq
    return
  end
  local.get $a
  local.get $b
  i64.eq
)

;; ---------------------------------------------------------------------
;; Layout helpers
;; ---------------------------------------------------------------------

;; Initial capacity for a fresh map allocated by `rt_map_set` /
;; `rt_map_set_owned` when the input was the empty sentinel (0).
(global $g_map_initial_cap i32 (i32.const 8))

;; Read capacity from header at offset 8 (low 32 bits).
(func $rt_map_capacity (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i64.load offset=8
  i32.wrap_i64
)

;; Read key_kind from capacity-word bits 32..39.
(func $rt_map_key_kind (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i64.load offset=8
  i64.const 32
  i64.shr_u
  i64.const 0xFF
  i64.and
  i32.wrap_i64
)

;; Read count from header low 32 bits.
(func $rt_map_count (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
)

;; Read value_ptr_flag from meta bit 0 (bits 32..47 of header word).
(func $rt_map_value_ptr_flag (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i64.load
  i64.const 32
  i64.shr_u
  i64.const 0xFFFF
  i64.and
  i32.wrap_i64
  i32.const 1
  i32.and
)

;; Compute bucket address: map + 16 + idx * 24.
(func $rt_map_bucket_addr (param $map i32) (param $idx i32) (result i32)
  local.get $map
  i32.const 16
  i32.add
  local.get $idx
  i32.const 24
  i32.mul
  i32.add
)

;; Mutate header in place: write count + value_ptr_flag back.
(func $rt_map_write_header (param $map i32) (param $count i32) (param $value_ptr_flag i32)
  local.get $map
  ;; header = (kind=12 << 56) | (value_ptr_flag << 32) | count
  i64.const 0x0C00000000000000
  local.get $value_ptr_flag
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  local.get $count
  i64.extend_i32_u
  i64.or
  i64.store
)

;; Allocate fresh empty map with given capacity (must be power of 2).
;; Buckets are zero-initialized so every state byte starts at 0 (empty).
(func $rt_map_alloc
    (param $cap i32) (param $key_kind i32) (param $value_ptr_flag i32)
    (result i32)
  (local $size i32)
  (local $ptr i32)
  (local $i i32)
  (local $bucket_addr i32)

  ;; size = 16 + cap * 24
  i32.const 16
  local.get $cap
  i32.const 24
  i32.mul
  i32.add
  local.set $size

  local.get $size
  call $rt_alloc
  local.set $ptr

  ;; Header (count=0)
  local.get $ptr
  i32.const 0
  local.get $value_ptr_flag
  call $rt_map_write_header

  ;; Capacity word at offset 8: capacity in low 32 bits,
  ;; key_kind in bits 32..39.
  local.get $ptr
  local.get $cap
  i64.extend_i32_u
  local.get $key_kind
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.store offset=8

  ;; Zero state in every bucket (rt_alloc gives un-zeroed bump memory).
  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $cap
      i32.ge_u
      br_if 1

      local.get $ptr
      local.get $i
      call $rt_map_bucket_addr
      local.set $bucket_addr

      local.get $bucket_addr
      i32.const 0
      i32.store ;; state = 0 (empty)

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $ptr
)

;; Probe for a key. Returns:
;;   high bit (1<<31) set    → key was found at low-31-bit slot index
;;   high bit clear          → key not found, low-31-bit slot is the
;;                              insertion target (the first empty or
;;                              tombstone slot encountered).
;; Caller must guarantee the table is not full (count < capacity).
(func $rt_map_probe
  (param $map i32) (param $hash i32) (param $key i64) (param $kind i32)
  (result i32)
  (local $cap i32)
  (local $mask i32)
  (local $idx i32)
  (local $tomb i32)
  (local $tomb_seen i32)
  (local $bucket_addr i32)
  (local $state i32)
  (local $bucket_hash i32)
  (local $bucket_key i64)

  local.get $map
  i64.load offset=8
  i32.wrap_i64
  local.set $cap

  local.get $cap
  i32.const 1
  i32.sub
  local.set $mask

  local.get $hash
  local.get $mask
  i32.and
  local.set $idx

  i32.const 0
  local.set $tomb_seen
  i32.const 0
  local.set $tomb

  block
    loop
      local.get $map
      local.get $idx
      call $rt_map_bucket_addr
      local.set $bucket_addr

      local.get $bucket_addr
      i32.load
      local.set $state

      ;; Empty slot: not found. Use first tombstone if we saw one,
      ;; otherwise this empty slot.
      local.get $state
      i32.eqz
      if
        local.get $tomb_seen
        if (result i32)
          local.get $tomb
        else
          local.get $idx
        end
        return
      end

      ;; Occupied: check hash + key.
      local.get $state
      i32.const 1
      i32.eq
      if
        local.get $bucket_addr
        i32.load offset=4
        local.set $bucket_hash

        local.get $bucket_hash
        local.get $hash
        i32.eq
        if
          local.get $bucket_addr
          i64.load offset=8
          local.set $bucket_key
          local.get $bucket_key
          local.get $key
          local.get $kind
          call $rt_map_key_eq
          if
            local.get $idx
            i32.const 0x80000000
            i32.or
            return
          end
        end
      else
        ;; Tombstone (state == 2): remember first one, keep probing.
        local.get $tomb_seen
        i32.eqz
        if
          i32.const 1
          local.set $tomb_seen
          local.get $idx
          local.set $tomb
        end
      end

      local.get $idx
      i32.const 1
      i32.add
      local.get $mask
      i32.and
      local.set $idx
      br 0
    end
  end

  unreachable
)

;; Internal: insert (key, value) into a map that has guaranteed slack.
;; Returns 1 if a new slot was used (count++), 0 if an existing key was
;; updated. Mutates the map in place.
(func $rt_map_insert_raw
  (param $map i32) (param $hash i32) (param $key i64) (param $kind i32)
  (param $value i64)
  (result i32)
  (local $probe i32)
  (local $idx i32)
  (local $found i32)
  (local $bucket_addr i32)

  local.get $map
  local.get $hash
  local.get $key
  local.get $kind
  call $rt_map_probe
  local.set $probe

  local.get $probe
  i32.const 0x80000000
  i32.and
  local.set $found

  local.get $probe
  i32.const 0x7FFFFFFF
  i32.and
  local.set $idx

  local.get $map
  local.get $idx
  call $rt_map_bucket_addr
  local.set $bucket_addr

  ;; state = 1
  local.get $bucket_addr
  i32.const 1
  i32.store
  ;; hash
  local.get $bucket_addr
  local.get $hash
  i32.store offset=4
  ;; key
  local.get $bucket_addr
  local.get $key
  i64.store offset=8
  ;; value
  local.get $bucket_addr
  local.get $value
  i64.store offset=16

  local.get $found
  if (result i32)
    i32.const 0  ;; updated, count unchanged
  else
    i32.const 1  ;; new slot, count + 1
  end
)

;; Resize: allocate a new map with double capacity and re-insert every
;; occupied bucket. Returns the new map. Tombstones disappear.
(func $rt_map_resize_to (param $old i32) (param $new_cap i32) (result i32)
  (local $new i32)
  (local $cap i32)
  (local $i i32)
  (local $bucket_addr i32)
  (local $state i32)
  (local $hash i32)
  (local $key i64)
  (local $value i64)
  (local $kkind i32)
  (local $vflag i32)
  (local $count i32)

  local.get $old
  call $rt_map_value_ptr_flag
  local.set $vflag

  local.get $old
  call $rt_map_key_kind
  local.set $kkind

  local.get $new_cap
  local.get $kkind
  local.get $vflag
  call $rt_map_alloc
  local.set $new

  local.get $old
  call $rt_map_capacity
  local.set $cap
  i32.const 0
  local.set $count

  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $cap
      i32.ge_u
      br_if 1

      local.get $old
      local.get $i
      call $rt_map_bucket_addr
      local.set $bucket_addr

      local.get $bucket_addr
      i32.load
      local.set $state

      local.get $state
      i32.const 1
      i32.eq
      if
        local.get $bucket_addr
        i32.load offset=4
        local.set $hash
        local.get $bucket_addr
        i64.load offset=8
        local.set $key
        local.get $bucket_addr
        i64.load offset=16
        local.set $value

        ;; kind doesn't matter for raw insert (it's only used for key
        ;; equality which fires on collisions; rehash → new slot).
        ;; We pass kind=0; collisions still resolve correctly because
        ;; identical bytes hash identically and the early hash check
        ;; weeds out non-matches before key_eq.
        local.get $new
        local.get $hash
        local.get $key
        i32.const 0
        local.get $value
        call $rt_map_insert_raw
        drop

        local.get $count
        i32.const 1
        i32.add
        local.set $count
      end

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  ;; Write the count back into header.
  local.get $new
  local.get $count
  local.get $vflag
  call $rt_map_write_header

  local.get $new
)

;; Clone a map: fresh allocation of identical capacity, copy every
;; bucket including tombstones (so probe sequences are preserved).
(func $rt_map_clone (param $old i32) (result i32)
  (local $cap i32)
  (local $vflag i32)
  (local $kkind i32)
  (local $new i32)
  (local $size i32)

  local.get $old
  call $rt_map_capacity
  local.set $cap

  local.get $old
  call $rt_map_value_ptr_flag
  local.set $vflag

  local.get $old
  call $rt_map_key_kind
  local.set $kkind

  local.get $cap
  local.get $kkind
  local.get $vflag
  call $rt_map_alloc
  local.set $new

  ;; Copy bucket array verbatim. We only need bytes [16..16+cap*24];
  ;; the header (offset 0..16) was already written by rt_map_alloc.
  local.get $cap
  i32.const 24
  i32.mul
  local.set $size

  local.get $new
  i32.const 16
  i32.add
  local.get $old
  i32.const 16
  i32.add
  local.get $size
  memory.copy

  ;; Restore count from the source map (rt_map_alloc sets count = 0).
  local.get $new
  local.get $old
  call $rt_map_count
  local.get $vflag
  call $rt_map_write_header

  local.get $new
)

;; ---------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------

(func $rt_map_get (param $map i32) (param $key i64) (param $kind i32) (result i32)
  (local $probe i32)
  (local $idx i32)
  (local $bucket_addr i32)
  (local $value i64)
  (local $vflag i32)

  local.get $map
  i32.eqz
  if
    i32.const -1   ;; NONE_SENTINEL
    return
  end

  local.get $map
  local.get $key
  local.get $kind
  call $rt_hash_key
  local.get $key
  local.get $kind
  call $rt_map_probe
  local.set $probe

  local.get $probe
  i32.const 0x80000000
  i32.and
  i32.eqz
  if
    i32.const -1
    return
  end

  local.get $probe
  i32.const 0x7FFFFFFF
  i32.and
  local.set $idx

  local.get $map
  local.get $idx
  call $rt_map_bucket_addr
  local.set $bucket_addr

  local.get $bucket_addr
  i64.load offset=16
  local.set $value

  local.get $map
  call $rt_map_value_ptr_flag
  local.set $vflag

  ;; Wrap into Some(value)
  i32.const 2 ;; WRAP_SOME
  local.get $value
  local.get $vflag
  call $rt_wrap
)
(export "rt_map_get" (func $rt_map_get))

(func $rt_map_has (param $map i32) (param $key i64) (param $kind i32) (result i32)
  (local $probe i32)

  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end

  local.get $map
  local.get $key
  local.get $kind
  call $rt_hash_key
  local.get $key
  local.get $kind
  call $rt_map_probe
  local.set $probe

  local.get $probe
  i32.const 0x80000000
  i32.and
  i32.const 0
  i32.ne
)
(export "rt_map_has" (func $rt_map_has))

(func $rt_map_len (param $map i32) (result i64)
  local.get $map
  call $rt_map_count
  i64.extend_i32_u
)
(export "rt_map_len" (func $rt_map_len))

;; Internal: prepare the destination map. If the input is empty (0),
;; allocate a fresh table. If load factor would exceed 0.7 after one
;; more insert, allocate the next power-of-two capacity. Else return
;; input unchanged. Returns the destination map for `_owned` to mutate
;; in place; callers using copy-on-write should clone the result first.
(func $rt_map_ensure_capacity_owned
  (param $map i32) (param $key_kind i32) (param $value_ptr_flag i32)
  (result i32)
  (local $cap i32)
  (local $count i32)
  (local $threshold i32)

  local.get $map
  i32.eqz
  if
    global.get $g_map_initial_cap
    local.get $key_kind
    local.get $value_ptr_flag
    call $rt_map_alloc
    return
  end

  local.get $map
  call $rt_map_capacity
  local.set $cap
  local.get $map
  call $rt_map_count
  local.set $count

  ;; threshold = cap * 7 / 10 (load factor 0.7).
  local.get $cap
  i32.const 7
  i32.mul
  i32.const 10
  i32.div_u
  local.set $threshold

  local.get $count
  i32.const 1
  i32.add
  local.get $threshold
  i32.gt_u
  if
    local.get $map
    local.get $cap
    i32.const 1
    i32.shl
    call $rt_map_resize_to
    return
  end

  local.get $map
)

(func $rt_map_set
  (param $map i32) (param $key i64) (param $kind i32)
  (param $value i64) (param $value_ptr_flag i32)
  (result i32)
  (local $dst i32)
  (local $hash i32)
  (local $delta i32)
  (local $count i32)

  ;; Copy-on-write: clone first, then mutate the clone (unless empty).
  local.get $map
  i32.eqz
  if
    global.get $g_map_initial_cap
    local.get $kind
    local.get $value_ptr_flag
    call $rt_map_alloc
    local.set $dst
  else
    local.get $map
    call $rt_map_clone
    local.set $dst
  end

  ;; Then ensure there's room (clone preserved load factor, but if the
  ;; original was at threshold, we need to grow).
  local.get $dst
  local.get $kind
  local.get $value_ptr_flag
  call $rt_map_ensure_capacity_owned
  local.set $dst

  local.get $key
  local.get $kind
  call $rt_hash_key
  local.set $hash

  local.get $dst
  local.get $hash
  local.get $key
  local.get $kind
  local.get $value
  call $rt_map_insert_raw
  local.set $delta

  local.get $dst
  call $rt_map_count
  local.get $delta
  i32.add
  local.set $count

  local.get $dst
  local.get $count
  local.get $value_ptr_flag
  call $rt_map_write_header

  local.get $dst
)
(export "rt_map_set" (func $rt_map_set))

;; Owned-mutate variant. Caller has guaranteed via last-use analysis
;; that no other observer holds `$map`, so we mutate it in place
;; instead of cloning. Resizing still allocates a fresh table because
;; capacity is fixed at allocation time.
(func $rt_map_set_owned
  (param $map i32) (param $key i64) (param $kind i32)
  (param $value i64) (param $value_ptr_flag i32)
  (result i32)
  (local $dst i32)
  (local $hash i32)
  (local $delta i32)
  (local $count i32)

  local.get $map
  local.get $kind
  local.get $value_ptr_flag
  call $rt_map_ensure_capacity_owned
  local.set $dst

  local.get $key
  local.get $kind
  call $rt_hash_key
  local.set $hash

  local.get $dst
  local.get $hash
  local.get $key
  local.get $kind
  local.get $value
  call $rt_map_insert_raw
  local.set $delta

  local.get $dst
  call $rt_map_count
  local.get $delta
  i32.add
  local.set $count

  local.get $dst
  local.get $count
  local.get $value_ptr_flag
  call $rt_map_write_header

  local.get $dst
)
(export "rt_map_set_owned" (func $rt_map_set_owned))

;; Walk every occupied bucket, prepend its key onto the result list.
;; Order is unspecified (probe order, not insertion order).
(func $rt_map_keys (param $map i32) (result i32)
  (local $cap i32)
  (local $i i32)
  (local $bucket_addr i32)
  (local $state i32)
  (local $key i64)
  (local $acc i32)

  i32.const 0
  local.set $acc

  local.get $map
  i32.eqz
  if
    local.get $acc
    return
  end

  local.get $map
  call $rt_map_capacity
  local.set $cap

  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $cap
      i32.ge_u
      br_if 1

      local.get $map
      local.get $i
      call $rt_map_bucket_addr
      local.set $bucket_addr

      local.get $bucket_addr
      i32.load
      local.set $state

      local.get $state
      i32.const 1
      i32.eq
      if
        local.get $bucket_addr
        i64.load offset=8
        local.set $key
        ;; Keys may be heap pointers or scalars — use kind=4 ptr-flag
        ;; convention from header? Actually we don't track per-map key
        ;; ptr flag; pass 1 conservatively for pointer keys (kind=3,4)
        ;; and 0 for scalars. We don't know kind from the map itself,
        ;; so we use the most permissive flag. The list_cons head_ptr
        ;; flag affects only GC, and string/heap keys must be tracked.
        ;; Default to ptr-flag = 1; for scalar Maps it's a harmless
        ;; over-tag (GC will treat the i64 cells as ptrs and try to
        ;; rebase, but rt_rebase_i32 returns the input unchanged for
        ;; values outside the heap range).
        local.get $key
        local.get $acc
        i32.const 1
        call $rt_list_cons
        local.set $acc
      end

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $acc
)
(export "rt_map_keys" (func $rt_map_keys))

;; Walk every occupied bucket, build OBJ_TUPLE (key, value), prepend
;; onto the list.
(func $rt_map_entries (param $map i32) (result i32)
  (local $cap i32)
  (local $i i32)
  (local $bucket_addr i32)
  (local $state i32)
  (local $key i64)
  (local $value i64)
  (local $vflag i32)
  (local $tuple i32)
  (local $acc i32)

  i32.const 0
  local.set $acc

  local.get $map
  i32.eqz
  if
    local.get $acc
    return
  end

  local.get $map
  call $rt_map_capacity
  local.set $cap

  local.get $map
  call $rt_map_value_ptr_flag
  local.set $vflag

  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $cap
      i32.ge_u
      br_if 1

      local.get $map
      local.get $i
      call $rt_map_bucket_addr
      local.set $bucket_addr

      local.get $bucket_addr
      i32.load
      local.set $state

      local.get $state
      i32.const 1
      i32.eq
      if
        local.get $bucket_addr
        i64.load offset=8
        local.set $key
        local.get $bucket_addr
        i64.load offset=16
        local.set $value

        ;; Allocate OBJ_TUPLE { key, value }: 24 bytes.
        i32.const 24
        call $rt_alloc
        local.set $tuple

        ;; Header: kind=OBJ_TUPLE(5), meta = bit 0 (key_ptr) | bit 1 (val_ptr)
        ;; field_count = 2.
        local.get $tuple
        i64.const 0x0500000000000000
        i64.const 1
        i64.const 32
        i64.shl
        i64.or
        local.get $vflag
        i64.extend_i32_u
        i64.const 33
        i64.shl
        i64.or
        i64.const 2
        i64.or
        i64.store

        local.get $tuple
        local.get $key
        i64.store offset=8

        local.get $tuple
        local.get $value
        i64.store offset=16

        local.get $tuple
        i64.extend_i32_u
        local.get $acc
        i32.const 1
        call $rt_list_cons
        local.set $acc
      end

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $acc
)
(export "rt_map_entries" (func $rt_map_entries))

;; Build a Map<K, V> by folding a List<(K, V)> through rt_map_set_owned.
;; All keys share the same `$key_kind`; values share `$value_ptr_flag`.
(func $rt_map_from_list
    (param $list i32) (param $key_kind i32) (param $value_ptr_flag i32)
    (result i32)
  (local $cur i32)
  (local $tuple i32)
  (local $key i64)
  (local $value i64)
  (local $map i32)

  i32.const 0
  local.set $map

  local.get $list
  local.set $cur

  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      ;; head is i64 stored in cell at offset 8 — for OBJ_LIST_CONS
      ;; with head_ptr_flag set we treat the i64 as a 32-bit pointer.
      local.get $cur
      i64.load offset=8
      i32.wrap_i64
      local.set $tuple

      local.get $tuple
      i64.load offset=8
      local.set $key
      local.get $tuple
      i64.load offset=16
      local.set $value

      local.get $map
      local.get $key
      local.get $key_kind
      local.get $value
      local.get $value_ptr_flag
      call $rt_map_set_owned
      local.set $map

      local.get $cur
      i64.load offset=16
      i32.wrap_i64
      local.set $cur
      br 0
    end
  end

  local.get $map
)
(export "rt_map_from_list" (func $rt_map_from_list))
