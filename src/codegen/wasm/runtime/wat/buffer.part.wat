;; Growable byte buffer — backing storage for the deforestation
;; lowering planned for 0.15 "Traversal". Builders that match the
;; canonical `List.prepend → reverse → String.join` shape compile to
;; a sequence of `rt_buffer_append_str` calls that write straight into
;; this buffer, skipping cons cells and the intermediate List entirely.
;;
;; Layout:
;;   offset 0  : i64 header — (kind=13, OBJ_BUFFER) << 56 | byte_len
;;   offset 8  : i64 cap — total payload bytes allocated (excludes
;;               header).
;;   offset 16+: raw payload bytes.
;;
;; `rt_buffer_finalize` converts an OBJ_BUFFER in-place to an
;; OBJ_STRING by `memory.copy`-moving the payload from offset 16 down
;; to offset 8 (the OBJ_STRING payload offset) and rewriting the
;; header with `kind=0`. The finalized object is shorter than what
;; was reserved (we leave the old cap field as dead bytes), but
;; subsequent allocations bump past it via the heap_ptr global so it
;; doesn't matter for correctness — only for fragmentation, which
;; the next GC collect cycle reclaims.
;;
;; The collector treats OBJ_BUFFER as a leaf object: payload is raw
;; bytes, no inner pointers to retain. Size for traversal is
;; 16 + align8(cap).

;; rt_buffer_new(cap_hint) -> ptr
;; Allocate a fresh OBJ_BUFFER with capacity `cap_hint` bytes and
;; len=0. Caller is responsible for picking a reasonable cap_hint
;; (the lowering pass uses a heuristic; runtime grows on demand).
(func $rt_buffer_new (param $cap_hint i32) (result i32)
  (local $ptr i32)
  (local $alloc_size i32)

  ;; alloc_size = 16 (header + cap) + align8(cap_hint)
  local.get $cap_hint
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.const 16
  i32.add
  local.set $alloc_size

  local.get $alloc_size
  call $rt_alloc
  local.set $ptr

  ;; header at offset 0 = (kind=13 << 56) | len=0
  local.get $ptr
  i64.const 0x0D00000000000000
  i64.store

  ;; cap at offset 8
  local.get $ptr
  local.get $cap_hint
  i64.extend_i32_u
  i64.store offset=8

  local.get $ptr
)
(export "rt_buffer_new" (func $rt_buffer_new))

;; rt_buffer_grow(buf, min_cap) -> new_ptr
;; Internal: allocate a new buffer with capacity max(min_cap, 2*old_cap),
;; memory.copy the old payload over, return the new pointer. The old
;; buffer becomes garbage for the next GC pass.
(func $rt_buffer_grow (param $buf i32) (param $min_cap i32) (result i32)
  (local $old_cap i32)
  (local $old_len i32)
  (local $new_cap i32)
  (local $doubled i32)
  (local $new_buf i32)

  ;; old_cap = i32 truncation of i64 at offset 8
  local.get $buf
  i64.load offset=8
  i32.wrap_i64
  local.set $old_cap

  ;; old_len = low 32 bits of header
  local.get $buf
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $old_len

  ;; doubled = old_cap * 2
  local.get $old_cap
  i32.const 1
  i32.shl
  local.set $doubled

  ;; new_cap = max(min_cap, doubled). Branch-free: pick whichever is
  ;; larger via select.
  local.get $min_cap
  local.get $doubled
  local.get $min_cap
  local.get $doubled
  i32.gt_u
  select
  local.set $new_cap

  ;; Allocate new buffer.
  local.get $new_cap
  call $rt_buffer_new
  local.set $new_buf

  ;; memory.copy(new_buf+16, old_buf+16, old_len). Skip if old_len=0.
  local.get $old_len
  i32.eqz
  if
  else
    local.get $new_buf
    i32.const 16
    i32.add
    local.get $buf
    i32.const 16
    i32.add
    local.get $old_len
    memory.copy
  end

  ;; new_buf header = (kind=13 << 56) | old_len. Carry the existing
  ;; len through; caller's append will bump it after copying its own
  ;; bytes in.
  local.get $new_buf
  i64.const 0x0D00000000000000
  local.get $old_len
  i64.extend_i32_u
  i64.or
  i64.store

  local.get $new_buf
)

;; rt_buffer_append_str(buf, str) -> ptr
;; Copy the payload bytes of OBJ_STRING `str` into `buf` at offset
;; 16 + buf.len, then bump buf.len by str.byte_len. Returns the
;; possibly-relocated buffer pointer; caller MUST use the returned
;; value (not the input `buf`) for any subsequent operations.
(func $rt_buffer_append_str (param $buf i32) (param $str i32) (result i32)
  (local $cur_buf i32)
  (local $buf_len i32)
  (local $buf_cap i32)
  (local $str_len i32)
  (local $needed i32)

  local.get $buf
  local.set $cur_buf

  ;; str_len = low 32 bits of str's header.
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $str_len

  ;; Fast path: zero-length string is a no-op.
  local.get $str_len
  i32.eqz
  if
    local.get $cur_buf
    return
  end

  ;; buf_len = low 32 bits of buf's header.
  local.get $cur_buf
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $buf_len

  ;; buf_cap = i32 of cap field at offset 8.
  local.get $cur_buf
  i64.load offset=8
  i32.wrap_i64
  local.set $buf_cap

  ;; needed = buf_len + str_len.
  local.get $buf_len
  local.get $str_len
  i32.add
  local.set $needed

  ;; If needed > buf_cap, grow.
  local.get $needed
  local.get $buf_cap
  i32.gt_u
  if
    local.get $cur_buf
    local.get $needed
    call $rt_buffer_grow
    local.set $cur_buf
    ;; After grow, refresh buf_len (carried over by grow).
    local.get $cur_buf
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $buf_len
  end

  ;; memory.copy(cur_buf + 16 + buf_len, str + 8, str_len).
  local.get $cur_buf
  i32.const 16
  i32.add
  local.get $buf_len
  i32.add
  local.get $str
  i32.const 8
  i32.add
  local.get $str_len
  memory.copy

  ;; Update header: len = buf_len + str_len, kind stays 11.
  local.get $cur_buf
  i64.const 0x0D00000000000000
  local.get $needed
  i64.extend_i32_u
  i64.or
  i64.store

  local.get $cur_buf
)
(export "rt_buffer_append_str" (func $rt_buffer_append_str))

;; rt_buffer_finalize(buf) -> str
;; In-place conversion: shift payload from offset 16 down to offset
;; 8 (the OBJ_STRING payload position), rewrite header to OBJ_STRING
;; (kind=0). Returns the same pointer, now usable as an OBJ_STRING.
;; The dead cap field at offset 8 (now overwritten by payload) is
;; discarded; the +8 bytes of slack at the end stay until GC.
(func $rt_buffer_finalize (param $buf i32) (result i32)
  (local $len i32)

  ;; len = low 32 bits of header.
  local.get $buf
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  ;; memory.copy(buf+8, buf+16, len). memory.copy handles overlapping
  ;; regions (semantics: as if temporary buffer used). Skip if len=0.
  local.get $len
  i32.eqz
  if
  else
    local.get $buf
    i32.const 8
    i32.add
    local.get $buf
    i32.const 16
    i32.add
    local.get $len
    memory.copy
  end

  ;; Header = (kind=0 << 56) | len. Just keep low 32 bits, clear
  ;; kind nibble.
  local.get $buf
  local.get $len
  i64.extend_i32_u
  i64.store

  local.get $buf
)
(export "rt_buffer_finalize" (func $rt_buffer_finalize))
