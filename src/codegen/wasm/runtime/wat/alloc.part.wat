;; Bump allocator. Bumps $heap_ptr by `size` bytes (8-byte aligned),
;; returns the pre-bump pointer.
;;
;; This is the WAT proof-of-concept for the runtime rewrite — once
;; this lands wired into emit, the rest of runtime/*.rs migrates
;; one function per commit.

(func $rt_alloc (param $size i32) (result i32)
  (local $ptr i32)
  ;; ptr = heap_ptr
  global.get $heap_ptr
  local.set $ptr
  ;; heap_ptr = ptr + ((size + 7) & ~7)
  local.get $ptr
  local.get $size
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  global.set $heap_ptr
  local.get $ptr
)
(export "rt_alloc" (func $rt_alloc))
