;; Bump allocator. Bumps $heap_ptr by `size` bytes (8-byte aligned),
;; growing memory if needed, returns the pre-bump pointer. Traps on
;; memory.grow failure (out-of-memory).

(func $rt_alloc (param $size i32) (result i32)
  (local $ptr i32)
  (local $aligned i32)
  (local $end i32)
  (local $mem_bytes i32)
  (local $grow_pages i32)

  ;; ptr = heap_ptr
  global.get $heap_ptr
  local.set $ptr

  ;; aligned = (size + 7) & ~7
  local.get $size
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  local.set $aligned

  ;; end = heap_ptr + aligned
  global.get $heap_ptr
  local.get $aligned
  i32.add
  local.set $end

  ;; mem_bytes = memory.size << 16
  memory.size
  i32.const 16
  i32.shl
  local.set $mem_bytes

  ;; if end > mem_bytes, grow memory
  local.get $end
  local.get $mem_bytes
  i32.gt_u
  if
    ;; grow_pages = ((end - mem_bytes) + 65535) >> 16
    local.get $end
    local.get $mem_bytes
    i32.sub
    i32.const 65535
    i32.add
    i32.const 16
    i32.shr_u
    local.set $grow_pages

    ;; if memory.grow returns -1, trap
    local.get $grow_pages
    memory.grow
    i32.const -1
    i32.eq
    if
      unreachable
    end
  end

  ;; heap_ptr = end
  local.get $end
  global.set $heap_ptr

  ;; return ptr
  local.get $ptr
)
(export "rt_alloc" (func $rt_alloc))
