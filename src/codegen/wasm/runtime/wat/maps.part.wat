;; Persistent HAMT (hash array mapped trie) over polymorphic keys.
;; Branching factor 16 — 4-bit chunks of a 32-bit hash, max depth 8.
;; Structural sharing: every set returns a fresh root, but only the
;; nodes on the path from root to leaf are reallocated; sibling
;; subtries are reused by pointer.
;;
;; Object kinds laid out for the trie. All headers follow the standard
;; runtime convention so the GC compactor can compute size and pointer
;; mask from header bytes alone:
;;   meta (bits 32..47) = pointer-bitmap or kind-specific metadata
;;   low32 (bits  0..31) = field count (size = 8 + field_count*8)
;;
;;   12 OBJ_HAMT          wrapper:   2 fields = (root_ptr, count_i64).
;;                                    meta = 0b01 (only field 0 is ptr).
;;   13 OBJ_HAMT_NODE     internal:  popcount children, all i32 ptrs.
;;                                    meta = bitmap (low 16 bits).
;;                                    low32 = popcount(bitmap).
;;                                    GC-special: every field is a ptr.
;;   14 OBJ_HAMT_LEAF     leaf:      3 fields = (hash_i64, key_i64, value_i64).
;;                                    meta bit 1 = key_ptr_flag,
;;                                    meta bit 2 = value_ptr_flag,
;;                                    meta bits 4..6 = key_kind (0..4).
;;                                    Field 0 (hash) is never a ptr.
;;   15 OBJ_HAMT_COLL     collision: leaf-ptr array.
;;                                    low32 = count = field count.
;;                                    GC-special: every field is a ptr.
;;
;; Polymorphic key ABI exposed to user.wasm:
;;   rt_map_get(map: i32, key: i64, kind: i32) -> i32   ;; Option<V>
;;   rt_map_has(map: i32, key: i64, kind: i32) -> i32   ;; bool
;;   rt_map_set(map, key, kind, value: i64, value_ptr_flag) -> i32
;;   rt_map_keys(map) -> List<K>
;;   rt_map_entries(map) -> List<(K,V)>
;;   rt_map_len(map) -> i64

;; ---------------------------------------------------------------------
;; Hashing
;; ---------------------------------------------------------------------

;; Mix an i64 down to i32 with a small finalizer (Murmur3-inspired).
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

;; djb2 over OBJ_STRING bytes (header low 32 bits = byte length).
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

      ;; h = h*33 + byte
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

;; Forward decl alias — used recursively.
(func $rt_deep_hash_fwd (param $ptr i32) (result i32)
  local.get $ptr
  call $rt_deep_hash
)

;; Deep structural hash for heap keys (kind==4). Walks the object by
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
  ;; OBJ_LIST_CONS (4) head=i64, OBJ_LIST_CONS_F64 (9) head=f64
  local.get $kind
  i32.const 4
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field
    local.set $field
    ;; head_ptr_flag stored in meta bit 0 (rt_list_cons sets it)
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

    ;; tail
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

  ;; OBJ_TUPLE (5) / OBJ_RECORD (1) / OBJ_VARIANT (2) — fields scanned
  ;; with meta byte as ptr-bitmap (bit i = field i is heap ptr).
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
    ;; field count from header low 32 bits
    local.get $ptr
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $count

    ;; seed = (kind << 8) | tag
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

  ;; OBJ_WRAPPER (3) wraps i64 / OBJ_WRAPPER_F64 (7) / OBJ_WRAPPER_I32 (8).
  ;; Tag in header.tag distinguishes Some/Ok/Err.
  local.get $kind
  i32.const 3
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field
    local.set $field
    ;; meta bit 0 = inner_ptr_flag
    local.get $meta
    i32.const 1
    i32.and
    if (result i32)
      local.get $field
      i32.wrap_i64
      call $rt_deep_hash_fwd
    else
      local.get $field
      call $rt_mix_i64
    end
    local.get $tag
    i32.const 11
    i32.shl
    i32.xor
    return
  end

  local.get $kind
  i32.const 7
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field_f64
    i64.reinterpret_f64
    call $rt_mix_i64
    local.get $tag
    i32.const 11
    i32.shl
    i32.xor
    return
  end

  local.get $kind
  i32.const 8
  i32.eq
  if
    local.get $ptr
    i32.const 0
    call $rt_obj_field_i32
    call $rt_deep_hash_fwd
    local.get $tag
    i32.const 11
    i32.shl
    i32.xor
    return
  end

  ;; Fallback: hash pointer bits (identity). Covers OBJ_VECTOR etc.
  local.get $ptr
  i64.extend_i32_u
  call $rt_mix_i64
)

;; Deep structural equality. Identity short-circuit, then per-kind walk.
(func $rt_deep_eq_fwd (param $a i32) (param $b i32) (result i32)
  local.get $a
  local.get $b
  call $rt_deep_eq
)

(func $rt_deep_eq (param $a i32) (param $b i32) (result i32)
  (local $ka i32)
  (local $kb i32)
  (local $meta i32)
  (local $count i32)
  (local $i i32)
  (local $fa i64)
  (local $fb i64)

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

  ;; OBJ_STRING
  local.get $ka
  i32.const 0
  i32.eq
  if
    local.get $a
    local.get $b
    call $rt_str_eq
    return
  end

  ;; tag must match for variants/wrappers
  local.get $a
  call $rt_obj_tag
  local.get $b
  call $rt_obj_tag
  i32.ne
  if
    i32.const 0
    return
  end

  ;; LIST_CONS — head/tail recursive
  local.get $ka
  i32.const 4
  i32.eq
  if
    local.get $a
    call $rt_obj_meta
    local.set $meta

    local.get $a
    i32.const 0
    call $rt_obj_field
    local.set $fa
    local.get $b
    i32.const 0
    call $rt_obj_field
    local.set $fb

    local.get $meta
    i32.const 1
    i32.and
    if (result i32)
      local.get $fa
      i32.wrap_i64
      local.get $fb
      i32.wrap_i64
      call $rt_deep_eq_fwd
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
    local.get $b
    i32.const 1
    call $rt_obj_field_i32
    call $rt_deep_eq_fwd
    return
  end

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
    local.get $b
    i32.const 1
    call $rt_obj_field_i32
    call $rt_deep_eq_fwd
    return
  end

  ;; TUPLE / RECORD / VARIANT — field-by-field, ptr-bitmap from meta
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
    call $rt_obj_meta
    local.set $meta

    local.get $a
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $count

    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
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

        local.get $meta
        local.get $i
        i32.shr_u
        i32.const 1
        i32.and
        if (result i32)
          local.get $fa
          i32.wrap_i64
          local.get $fb
          i32.wrap_i64
          call $rt_deep_eq_fwd
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

  ;; OBJ_WRAPPER — single inner field
  local.get $ka
  i32.const 3
  i32.eq
  if
    local.get $a
    call $rt_obj_meta
    local.set $meta
    local.get $a
    i32.const 0
    call $rt_obj_field
    local.set $fa
    local.get $b
    i32.const 0
    call $rt_obj_field
    local.set $fb
    local.get $meta
    i32.const 1
    i32.and
    if (result i32)
      local.get $fa
      i32.wrap_i64
      local.get $fb
      i32.wrap_i64
      call $rt_deep_eq_fwd
    else
      local.get $fa
      local.get $fb
      i64.eq
    end
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
    call $rt_deep_eq_fwd
    return
  end

  ;; Fallback: pointer identity already failed up top.
  i32.const 0
)

;; Hash a polymorphic key (i64 lane + kind tag) down to i32.
(func $rt_hash_key (param $key i64) (param $kind i32) (result i32)
  ;; kind 3 (String) — djb2
  local.get $kind
  i32.const 3
  i32.eq
  if
    local.get $key
    i32.wrap_i64
    call $rt_hash_string_obj
    return
  end
  ;; kind 4 (Heap) — deep hash
  local.get $kind
  i32.const 4
  i32.eq
  if
    local.get $key
    i32.wrap_i64
    call $rt_deep_hash_fwd
    return
  end
  ;; Int / Float bits / Bool — mix
  local.get $key
  call $rt_mix_i64
)

;; Polymorphic key equality, dispatched on kind tag.
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
    call $rt_deep_eq_fwd
    return
  end
  local.get $a
  local.get $b
  i64.eq
)

;; ---------------------------------------------------------------------
;; HAMT node helpers
;; ---------------------------------------------------------------------

;; popcount of low 16 bits (CTZ-loop; bf=16 max, only 16 iterations).
(func $rt_popcount16 (param $x i32) (result i32)
  (local $c i32)
  i32.const 0
  local.set $c
  block
    loop
      local.get $x
      i32.eqz
      br_if 1
      local.get $c
      local.get $x
      i32.const 1
      i32.and
      i32.add
      local.set $c
      local.get $x
      i32.const 1
      i32.shr_u
      local.set $x
      br 0
    end
  end
  local.get $c
)

;; Build a HAMT_LEAF carrying (hash, key, value) plus the per-cell
;; ptr/kind flags packed into the header meta byte. Layout (RECORD-like):
;;   header  [14<<56 | meta<<32 | 3]  (3 = field count)
;;   field 0 hash  i64 (zero-extended; never a ptr — meta bit 0 = 0)
;;   field 1 key   i64
;;   field 2 value i64
;;   meta bit 1 = key_ptr_flag,  meta bit 2 = value_ptr_flag,
;;   meta bits 4..6 = key_kind (0=Int 1=Float 2=Bool 3=String 4=Heap)
(func $rt_hamt_alloc_leaf
    (param $hash i32) (param $key i64) (param $value i64)
    (param $kind i32) (param $value_ptr_flag i32)
    (result i32)
  (local $ptr i32)
  (local $key_ptr_flag i32)
  (local $meta i32)

  ;; key_ptr_flag = (kind == 3) | (kind == 4)
  local.get $kind
  i32.const 3
  i32.eq
  local.get $kind
  i32.const 4
  i32.eq
  i32.or
  local.set $key_ptr_flag

  ;; meta = (key_ptr<<1) | (value_ptr<<2) | ((kind & 7)<<4)
  local.get $key_ptr_flag
  i32.const 1
  i32.shl
  local.get $value_ptr_flag
  i32.const 2
  i32.shl
  i32.or
  local.get $kind
  i32.const 7
  i32.and
  i32.const 4
  i32.shl
  i32.or
  local.set $meta

  i32.const 32
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0x0E00000000000000
  local.get $meta
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.const 3
  i64.or
  i64.store

  local.get $ptr
  local.get $hash
  i64.extend_i32_u
  i64.store offset=8

  local.get $ptr
  local.get $key
  i64.store offset=16

  local.get $ptr
  local.get $value
  i64.store offset=24

  local.get $ptr
)

(func $rt_hamt_leaf_hash (param $leaf i32) (result i32)
  local.get $leaf
  i64.load offset=8
  i32.wrap_i64
)

(func $rt_hamt_leaf_meta (param $leaf i32) (result i32)
  local.get $leaf
  call $rt_obj_meta
)

(func $rt_hamt_leaf_kind (param $leaf i32) (result i32)
  local.get $leaf
  call $rt_obj_meta
  i32.const 4
  i32.shr_u
  i32.const 7
  i32.and
)

(func $rt_hamt_leaf_value_ptr (param $leaf i32) (result i32)
  local.get $leaf
  call $rt_obj_meta
  i32.const 2
  i32.shr_u
  i32.const 1
  i32.and
)

(func $rt_hamt_leaf_key_ptr (param $leaf i32) (result i32)
  local.get $leaf
  call $rt_obj_meta
  i32.const 1
  i32.shr_u
  i32.const 1
  i32.and
)

(func $rt_hamt_leaf_key (param $leaf i32) (result i64)
  local.get $leaf
  i64.load offset=16
)

(func $rt_hamt_leaf_value (param $leaf i32) (result i64)
  local.get $leaf
  i64.load offset=24
)

;; Allocate a HAMT_NODE. Bitmap goes in the meta byte; field count
;; (= popcount(bitmap), supplied by caller as $nchildren) goes in
;; header.low32 so the GC computes size = 8 + nchildren*8 from the
;; standard formula.
(func $rt_hamt_alloc_node (param $bitmap i32) (param $nchildren i32) (result i32)
  (local $ptr i32)

  local.get $nchildren
  i32.const 8
  i32.mul
  i32.const 8
  i32.add
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0x0D00000000000000
  local.get $bitmap
  i32.const 0xFFFF
  i32.and
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  local.get $nchildren
  i64.extend_i32_u
  i64.or
  i64.store

  local.get $ptr
)

(func $rt_hamt_node_bitmap (param $node i32) (result i32)
  local.get $node
  call $rt_obj_meta
)

;; Allocate a HAMT_COLLISION node carrying $count leaf pointers.
;; low32 = count = field count (GC-friendly).
(func $rt_hamt_alloc_coll (param $count i32) (result i32)
  (local $ptr i32)
  local.get $count
  i32.const 8
  i32.mul
  i32.const 8
  i32.add
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0x0F00000000000000
  local.get $count
  i64.extend_i32_u
  i64.or
  i64.store

  local.get $ptr
)

(func $rt_hamt_coll_count (param $coll i32) (result i32)
  local.get $coll
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
)

;; ---------------------------------------------------------------------
;; HAMT recursion: get / set / has
;; ---------------------------------------------------------------------

;; rt_hamt_get_at — walk down from $node looking for $key.
;;   returns: i32 ptr to leaf if found, else 0
(func $rt_hamt_get_at
    (param $node i32) (param $hash i32) (param $level i32)
    (param $key i64) (param $kind i32)
    (result i32)
  (local $k i32)
  (local $bitmap i32)
  (local $bit i32)
  (local $idx i32)
  (local $count i32)
  (local $i i32)
  (local $child i32)

  local.get $node
  i32.eqz
  if
    i32.const 0
    return
  end

  local.get $node
  call $rt_obj_kind
  local.set $k

  ;; LEAF — direct compare
  local.get $k
  i32.const 14
  i32.eq
  if
    local.get $node
    call $rt_hamt_leaf_hash
    local.get $hash
    i32.eq
    if
      local.get $node
      call $rt_hamt_leaf_key
      local.get $key
      local.get $kind
      call $rt_map_key_eq
      if
        local.get $node
        return
      end
    end
    i32.const 0
    return
  end

  ;; COLLISION — linear scan over leaves
  local.get $k
  i32.const 15
  i32.eq
  if
    local.get $node
    call $rt_hamt_coll_count
    local.set $count
    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1
        local.get $node
        local.get $i
        call $rt_obj_field_i32
        local.set $child
        local.get $child
        call $rt_hamt_leaf_key
        local.get $key
        local.get $kind
        call $rt_map_key_eq
        if
          local.get $child
          return
        end
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end
    i32.const 0
    return
  end

  ;; NODE — branch on hash chunk
  local.get $node
  call $rt_hamt_node_bitmap
  local.set $bitmap

  ;; bit = 1 << ((hash >> (level*4)) & 0xF)
  i32.const 1
  local.get $hash
  local.get $level
  i32.const 2
  i32.shl
  i32.shr_u
  i32.const 0xF
  i32.and
  i32.shl
  local.set $bit

  local.get $bitmap
  local.get $bit
  i32.and
  i32.eqz
  if
    i32.const 0
    return
  end

  ;; idx = popcount(bitmap & (bit-1))
  local.get $bitmap
  local.get $bit
  i32.const 1
  i32.sub
  i32.and
  call $rt_popcount16
  local.set $idx

  local.get $node
  local.get $idx
  call $rt_obj_field_i32
  local.get $hash
  local.get $level
  i32.const 1
  i32.add
  local.get $key
  local.get $kind
  call $rt_hamt_get_at
)

;; Materialize a NODE from src node by replacing/inserting child at $idx.
;;   $src        : original node (may be 0 → fresh from-scratch)
;;   $bitmap_new : final bitmap of new node
;;   $idx        : insert/replace position in children
;;   $new_child  : i32 child pointer
;;   $insert     : 1 = insert (shift right), 0 = replace at idx
(func $rt_hamt_node_with
    (param $src i32) (param $bitmap_new i32)
    (param $idx i32) (param $new_child i32) (param $insert i32)
    (result i32)
  (local $count_src i32)
  (local $count_new i32)
  (local $node i32)
  (local $i i32)
  (local $j i32)

  local.get $src
  i32.eqz
  if (result i32)
    i32.const 0
  else
    local.get $src
    call $rt_hamt_node_bitmap
    call $rt_popcount16
  end
  local.set $count_src

  local.get $bitmap_new
  call $rt_popcount16
  local.set $count_new

  local.get $bitmap_new
  local.get $count_new
  call $rt_hamt_alloc_node
  local.set $node

  i32.const 0
  local.set $i
  i32.const 0
  local.set $j

  ;; Copy children with insert/replace at idx.
  block
    loop
      local.get $j
      local.get $count_new
      i32.ge_u
      br_if 1

      local.get $j
      local.get $idx
      i32.eq
      if
        local.get $node
        i32.const 8
        i32.add
        local.get $j
        i32.const 8
        i32.mul
        i32.add
        local.get $new_child
        i64.extend_i32_u
        i64.store

        local.get $insert
        i32.eqz
        if
          ;; replace mode: also advance src index
          local.get $i
          i32.const 1
          i32.add
          local.set $i
        end
      else
        ;; copy from src[i]
        local.get $node
        i32.const 8
        i32.add
        local.get $j
        i32.const 8
        i32.mul
        i32.add
        local.get $src
        local.get $i
        call $rt_obj_field_i32
        i64.extend_i32_u
        i64.store

        local.get $i
        i32.const 1
        i32.add
        local.set $i
      end

      local.get $j
      i32.const 1
      i32.add
      local.set $j
      br 0
    end
  end

  local.get $node
)

;; Merge two distinct leaves into a NODE/COLL subtree at depth $level.
(func $rt_hamt_merge_leaves
    (param $a i32) (param $b i32) (param $level i32)
    (result i32)
  (local $ha i32)
  (local $hb i32)
  (local $ca i32)
  (local $cb i32)
  (local $bitmap i32)
  (local $node i32)
  (local $coll i32)
  (local $sub i32)

  local.get $a
  call $rt_hamt_leaf_hash
  local.set $ha
  local.get $b
  call $rt_hamt_leaf_hash
  local.set $hb

  ;; Same hash → COLLISION (capped at depth where chunk exhausts).
  local.get $ha
  local.get $hb
  i32.eq
  if
    i32.const 2
    call $rt_hamt_alloc_coll
    local.set $coll
    local.get $coll
    i32.const 8
    i32.add
    local.get $a
    i64.extend_i32_u
    i64.store
    local.get $coll
    i32.const 16
    i32.add
    local.get $b
    i64.extend_i32_u
    i64.store
    local.get $coll
    return
  end

  ;; chunk_a / chunk_b at this level
  local.get $ha
  local.get $level
  i32.const 2
  i32.shl
  i32.shr_u
  i32.const 0xF
  i32.and
  local.set $ca
  local.get $hb
  local.get $level
  i32.const 2
  i32.shl
  i32.shr_u
  i32.const 0xF
  i32.and
  local.set $cb

  local.get $ca
  local.get $cb
  i32.eq
  if
    ;; Same chunk: recurse one level deeper into a single-child NODE.
    local.get $a
    local.get $b
    local.get $level
    i32.const 1
    i32.add
    call $rt_hamt_merge_leaves
    local.set $sub

    i32.const 1
    local.get $ca
    i32.shl
    local.set $bitmap
    local.get $bitmap
    i32.const 1
    call $rt_hamt_alloc_node
    local.set $node
    local.get $node
    i32.const 8
    i32.add
    local.get $sub
    i64.extend_i32_u
    i64.store
    local.get $node
    return
  end

  ;; Different chunks: 2-child node, sorted by chunk index.
  i32.const 1
  local.get $ca
  i32.shl
  i32.const 1
  local.get $cb
  i32.shl
  i32.or
  local.set $bitmap

  local.get $bitmap
  i32.const 2
  call $rt_hamt_alloc_node
  local.set $node

  local.get $ca
  local.get $cb
  i32.lt_u
  if
    local.get $node
    i32.const 8
    i32.add
    local.get $a
    i64.extend_i32_u
    i64.store
    local.get $node
    i32.const 16
    i32.add
    local.get $b
    i64.extend_i32_u
    i64.store
  else
    local.get $node
    i32.const 8
    i32.add
    local.get $b
    i64.extend_i32_u
    i64.store
    local.get $node
    i32.const 16
    i32.add
    local.get $a
    i64.extend_i32_u
    i64.store
  end

  local.get $node
)

;; rt_hamt_set_at — returns (new_subtree, count_delta).
;; count_delta = 1 if this insert increased map size, else 0.
;; To return both, we stash count_delta in a global scratch cell.
;; Implementing as void-set on global `g_hamt_delta` keeps signature
;; simple inside the recursion.
(global $g_hamt_delta (mut i32) (i32.const 0))

(func $rt_hamt_set_at
    (param $node i32) (param $hash i32) (param $level i32)
    (param $key i64) (param $kind i32)
    (param $value i64) (param $value_ptr_flag i32)
    (result i32)
  (local $k i32)
  (local $new_leaf i32)
  (local $bitmap i32)
  (local $bit i32)
  (local $idx i32)
  (local $child i32)
  (local $sub i32)
  (local $count i32)
  (local $i i32)
  (local $j i32)
  (local $other i32)
  (local $coll i32)

  local.get $node
  i32.eqz
  if
    ;; Empty subtree → fresh leaf, count goes up.
    i32.const 1
    global.set $g_hamt_delta
    local.get $hash
    local.get $key
    local.get $value
    local.get $kind
    local.get $value_ptr_flag
    call $rt_hamt_alloc_leaf
    return
  end

  local.get $node
  call $rt_obj_kind
  local.set $k

  ;; LEAF
  local.get $k
  i32.const 14
  i32.eq
  if
    local.get $node
    call $rt_hamt_leaf_hash
    local.get $hash
    i32.eq
    if
      local.get $node
      call $rt_hamt_leaf_key
      local.get $key
      local.get $kind
      call $rt_map_key_eq
      if
        ;; Same key → replace, count unchanged.
        i32.const 0
        global.set $g_hamt_delta
        local.get $hash
        local.get $key
        local.get $value
        local.get $kind
        local.get $value_ptr_flag
        call $rt_hamt_alloc_leaf
        return
      end
      ;; Same hash, different key → COLLISION node containing both.
      i32.const 1
      global.set $g_hamt_delta
      i32.const 2
      call $rt_hamt_alloc_coll
      local.set $coll
      local.get $coll
      i32.const 8
      i32.add
      local.get $node
      i64.extend_i32_u
      i64.store
      local.get $coll
      i32.const 16
      i32.add
      local.get $hash
      local.get $key
      local.get $value
      local.get $kind
      local.get $value_ptr_flag
      call $rt_hamt_alloc_leaf
      i64.extend_i32_u
      i64.store
      local.get $coll
      return
    end
    ;; Different hash → split via merge_leaves.
    i32.const 1
    global.set $g_hamt_delta
    local.get $hash
    local.get $key
    local.get $value
    local.get $kind
    local.get $value_ptr_flag
    call $rt_hamt_alloc_leaf
    local.set $new_leaf
    local.get $node
    local.get $new_leaf
    local.get $level
    call $rt_hamt_merge_leaves
    return
  end

  ;; COLLISION
  local.get $k
  i32.const 15
  i32.eq
  if
    local.get $node
    call $rt_hamt_coll_count
    local.set $count
    ;; Linear scan: if key matches an existing leaf → replace it.
    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1
        local.get $node
        local.get $i
        call $rt_obj_field_i32
        local.set $child
        local.get $child
        call $rt_hamt_leaf_key
        local.get $key
        local.get $kind
        call $rt_map_key_eq
        if
          ;; replace at i, count unchanged
          i32.const 0
          global.set $g_hamt_delta
          local.get $count
          call $rt_hamt_alloc_coll
          local.set $coll
          ;; Copy all children, swapping i for new leaf.
          i32.const 0
          local.set $j
          block
            loop
              local.get $j
              local.get $count
              i32.ge_u
              br_if 1
              local.get $coll
              i32.const 8
              i32.add
              local.get $j
              i32.const 8
              i32.mul
              i32.add
              local.get $j
              local.get $i
              i32.eq
              if (result i64)
                local.get $hash
                local.get $key
                local.get $value
                local.get $kind
                local.get $value_ptr_flag
                call $rt_hamt_alloc_leaf
                i64.extend_i32_u
              else
                local.get $node
                local.get $j
                call $rt_obj_field
              end
              i64.store
              local.get $j
              i32.const 1
              i32.add
              local.set $j
              br 0
            end
          end
          local.get $coll
          return
        end
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end
    ;; Append new leaf to collision.
    i32.const 1
    global.set $g_hamt_delta
    local.get $count
    i32.const 1
    i32.add
    call $rt_hamt_alloc_coll
    local.set $coll
    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1
        local.get $coll
        i32.const 8
        i32.add
        local.get $i
        i32.const 8
        i32.mul
        i32.add
        local.get $node
        local.get $i
        call $rt_obj_field
        i64.store
        local.get $i
        i32.const 1
        i32.add
        local.set $i
      br 0
      end
    end
    local.get $coll
    i32.const 8
    i32.add
    local.get $count
    i32.const 8
    i32.mul
    i32.add
    local.get $hash
    local.get $key
    local.get $value
    local.get $kind
    local.get $value_ptr_flag
    call $rt_hamt_alloc_leaf
    i64.extend_i32_u
    i64.store
    local.get $coll
    return
  end

  ;; NODE — chunk dispatch
  local.get $node
  call $rt_hamt_node_bitmap
  local.set $bitmap

  i32.const 1
  local.get $hash
  local.get $level
  i32.const 2
  i32.shl
  i32.shr_u
  i32.const 0xF
  i32.and
  i32.shl
  local.set $bit

  local.get $bitmap
  local.get $bit
  i32.const 1
  i32.sub
  i32.and
  call $rt_popcount16
  local.set $idx

  local.get $bitmap
  local.get $bit
  i32.and
  i32.eqz
  if
    ;; Slot empty: insert fresh leaf, bitmap |= bit.
    i32.const 1
    global.set $g_hamt_delta
    local.get $hash
    local.get $key
    local.get $value
    local.get $kind
    local.get $value_ptr_flag
    call $rt_hamt_alloc_leaf
    local.set $new_leaf
    local.get $node
    local.get $bitmap
    local.get $bit
    i32.or
    local.get $idx
    local.get $new_leaf
    i32.const 1
    call $rt_hamt_node_with
    return
  end

  ;; Slot occupied: recurse, then replace child.
  local.get $node
  local.get $idx
  call $rt_obj_field_i32
  local.set $child

  local.get $child
  local.get $hash
  local.get $level
  i32.const 1
  i32.add
  local.get $key
  local.get $kind
  local.get $value
  local.get $value_ptr_flag
  call $rt_hamt_set_at
  local.set $sub

  local.get $node
  local.get $bitmap
  local.get $idx
  local.get $sub
  i32.const 0
  call $rt_hamt_node_with
)

;; ---------------------------------------------------------------------
;; HAMT_MAP wrapper helpers
;; ---------------------------------------------------------------------

;; HAMT wrapper layout: 2 fields = (root_ptr, count_i64). meta = 0b01
;; (only field 0 is a pointer). low32 = 2 = field count.
(func $rt_hamt_alloc_wrapper (param $count i32) (param $root i32) (result i32)
  (local $ptr i32)
  i32.const 24
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0x0C00000000000000
  i64.const 0x0000000100000002   ;; meta=1 (field 0 is ptr) | field_count=2
  i64.or
  i64.store

  local.get $ptr
  local.get $root
  i64.extend_i32_u
  i64.store offset=8

  local.get $ptr
  local.get $count
  i64.extend_i32_u
  i64.store offset=16

  local.get $ptr
)

(func $rt_hamt_count (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i64.load offset=16
  i32.wrap_i64
)

(func $rt_hamt_root (param $map i32) (result i32)
  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end
  local.get $map
  i32.const 0
  call $rt_obj_field_i32
)

;; ---------------------------------------------------------------------
;; Public ABI
;; ---------------------------------------------------------------------

(func $rt_map_get
    (param $map i32) (param $key i64) (param $kind i32)
    (result i32)
  (local $leaf i32)
  (local $hash i32)

  local.get $map
  i32.eqz
  if
    i32.const -1   ;; NONE_SENTINEL
    return
  end

  local.get $key
  local.get $kind
  call $rt_hash_key
  local.set $hash

  local.get $map
  call $rt_hamt_root
  local.get $hash
  i32.const 0
  local.get $key
  local.get $kind
  call $rt_hamt_get_at
  local.set $leaf

  local.get $leaf
  i32.eqz
  if
    i32.const -1
    return
  end

  ;; Wrap leaf.value as Some(_).
  i32.const 2   ;; WRAP_SOME
  local.get $leaf
  call $rt_hamt_leaf_value
  local.get $leaf
  call $rt_hamt_leaf_value_ptr
  call $rt_wrap
)
(export "rt_map_get" (func $rt_map_get))

(func $rt_map_has
    (param $map i32) (param $key i64) (param $kind i32)
    (result i32)
  (local $hash i32)

  local.get $map
  i32.eqz
  if
    i32.const 0
    return
  end

  local.get $key
  local.get $kind
  call $rt_hash_key
  local.set $hash

  local.get $map
  call $rt_hamt_root
  local.get $hash
  i32.const 0
  local.get $key
  local.get $kind
  call $rt_hamt_get_at
  i32.const 0
  i32.ne
)
(export "rt_map_has" (func $rt_map_has))

(func $rt_map_set
    (param $map i32) (param $key i64) (param $kind i32)
    (param $value i64) (param $value_ptr_flag i32)
    (result i32)
  (local $hash i32)
  (local $old_root i32)
  (local $new_root i32)
  (local $old_count i32)

  local.get $key
  local.get $kind
  call $rt_hash_key
  local.set $hash

  local.get $map
  call $rt_hamt_root
  local.set $old_root

  local.get $map
  call $rt_hamt_count
  local.set $old_count

  local.get $old_root
  local.get $hash
  i32.const 0
  local.get $key
  local.get $kind
  local.get $value
  local.get $value_ptr_flag
  call $rt_hamt_set_at
  local.set $new_root

  local.get $old_count
  global.get $g_hamt_delta
  i32.add
  local.get $new_root
  call $rt_hamt_alloc_wrapper
)
(export "rt_map_set" (func $rt_map_set))

;; rt_map_from_list — fold a `List<(K, V)>` into a HAMT by repeatedly
;; calling `rt_map_set`. Keys all share `$key_kind` (the polymorphic
;; key code that the emitter computes from the key's static type:
;; 0=Int, 1=Float, 2=Bool, 3=String, 4=heap). Values share
;; `$value_ptr_flag` (1 if values are heap pointers, 0 if scalar).
;;
;; Each list cell is OBJ_LIST_CONS with head = i64-stored tuple ptr,
;; tail at offset 16. Each tuple is OBJ_TUPLE with field 0 = key
;; (i64 raw bits) and field 1 = value (i64 raw bits) — exactly the
;; encoding `rt_map_set` consumes.
;;
;; Replaces the pre-HAMT "list of tuples IS a map" identity that
;; the emitter used for `Map.fromList` and `{k => v, …}` literals.
(func $rt_map_from_list
    (param $list i32) (param $key_kind i32) (param $value_ptr_flag i32)
    (result i32)
  (local $map i32)
  (local $cur i32)
  (local $tuple i32)

  ;; map = empty map (null ptr — `rt_map_set` allocates the wrapper)
  i32.const 0
  local.set $map

  local.get $list
  local.set $cur

  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      ;; tuple = head(cur) — `i64.load offset=8` then truncate.
      local.get $cur
      i64.load offset=8
      i32.wrap_i64
      local.set $tuple

      ;; map = rt_map_set(map, tuple.key, key_kind,
      ;;                  tuple.value, value_ptr_flag)
      local.get $map
      local.get $tuple
      i64.load offset=8                ;; tuple field 0 = key (i64)
      local.get $key_kind
      local.get $tuple
      i64.load offset=16               ;; tuple field 1 = value (i64)
      local.get $value_ptr_flag
      call $rt_map_set
      local.set $map

      ;; cur = tail(cur)
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

;; ---------------------------------------------------------------------
;; Iteration: keys / entries / len
;; ---------------------------------------------------------------------

;; Walk subtree, prepending leaves onto $acc (cons of key); returns new acc.
(func $rt_hamt_collect_keys (param $node i32) (param $acc i32) (result i32)
  (local $k i32)
  (local $count i32)
  (local $i i32)
  (local $child i32)
  (local $key i64)
  (local $key_ptr_flag i32)

  local.get $node
  i32.eqz
  if
    local.get $acc
    return
  end

  local.get $node
  call $rt_obj_kind
  local.set $k

  local.get $k
  i32.const 14
  i32.eq
  if
    local.get $node
    call $rt_hamt_leaf_key
    local.set $key
    local.get $node
    call $rt_hamt_leaf_key_ptr
    local.set $key_ptr_flag
    local.get $key
    local.get $acc
    local.get $key_ptr_flag
    call $rt_list_cons
    return
  end

  local.get $k
  i32.const 15
  i32.eq
  if
    local.get $node
    call $rt_hamt_coll_count
    local.set $count
    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1
        local.get $node
        local.get $i
        call $rt_obj_field_i32
        local.get $acc
        call $rt_hamt_collect_keys
        local.set $acc
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end
    local.get $acc
    return
  end

  ;; NODE
  local.get $node
  call $rt_hamt_node_bitmap
  call $rt_popcount16
  local.set $count
  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $count
      i32.ge_u
      br_if 1
      local.get $node
      local.get $i
      call $rt_obj_field_i32
      local.get $acc
      call $rt_hamt_collect_keys
      local.set $acc
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end
  local.get $acc
)

(func $rt_map_keys (param $map i32) (result i32)
  local.get $map
  call $rt_hamt_root
  i32.const 0
  call $rt_hamt_collect_keys
)
(export "rt_map_keys" (func $rt_map_keys))

;; Build a fresh OBJ_TUPLE (k, v) carrying the leaf's ptr-flag bits.
;; Leaf meta has key_ptr in bit 1 and value_ptr in bit 2; the tuple
;; expects key_ptr in bit 0 and value_ptr in bit 1 — shift right by 1.
(func $rt_hamt_leaf_to_tuple (param $leaf i32) (result i32)
  (local $tuple i32)
  (local $tuple_meta i32)

  local.get $leaf
  call $rt_obj_meta
  i32.const 1
  i32.shr_u
  i32.const 3
  i32.and
  local.set $tuple_meta

  i32.const 24
  call $rt_alloc
  local.set $tuple

  local.get $tuple
  i64.const 0x0500000000000000
  local.get $tuple_meta
  i64.extend_i32_u
  i64.const 32
  i64.shl
  i64.or
  i64.const 2
  i64.or
  i64.store

  local.get $tuple
  local.get $leaf
  call $rt_hamt_leaf_key
  i64.store offset=8

  local.get $tuple
  local.get $leaf
  call $rt_hamt_leaf_value
  i64.store offset=16

  local.get $tuple
)

(func $rt_hamt_collect_entries (param $node i32) (param $acc i32) (result i32)
  (local $k i32)
  (local $count i32)
  (local $i i32)

  local.get $node
  i32.eqz
  if
    local.get $acc
    return
  end

  local.get $node
  call $rt_obj_kind
  local.set $k

  local.get $k
  i32.const 14
  i32.eq
  if
    local.get $node
    call $rt_hamt_leaf_to_tuple
    i64.extend_i32_u
    local.get $acc
    i32.const 1
    call $rt_list_cons
    return
  end

  local.get $k
  i32.const 15
  i32.eq
  if
    local.get $node
    call $rt_hamt_coll_count
    local.set $count
    i32.const 0
    local.set $i
    block
      loop
        local.get $i
        local.get $count
        i32.ge_u
        br_if 1
        local.get $node
        local.get $i
        call $rt_obj_field_i32
        local.get $acc
        call $rt_hamt_collect_entries
        local.set $acc
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0
      end
    end
    local.get $acc
    return
  end

  local.get $node
  call $rt_hamt_node_bitmap
  call $rt_popcount16
  local.set $count
  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $count
      i32.ge_u
      br_if 1
      local.get $node
      local.get $i
      call $rt_obj_field_i32
      local.get $acc
      call $rt_hamt_collect_entries
      local.set $acc
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end
  local.get $acc
)

(func $rt_map_entries (param $map i32) (result i32)
  local.get $map
  call $rt_hamt_root
  i32.const 0
  call $rt_hamt_collect_entries
)
(export "rt_map_entries" (func $rt_map_entries))

(func $rt_map_len (param $map i32) (result i64)
  local.get $map
  call $rt_hamt_count
  i64.extend_i32_u
)
(export "rt_map_len" (func $rt_map_len))
