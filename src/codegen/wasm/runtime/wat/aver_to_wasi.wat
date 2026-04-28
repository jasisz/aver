;; aver/* → wasi_snapshot_preview1.* translation shim.
;;
;; user.wasm always emits Aver-flavored host imports (aver/console_print,
;; aver/print_value, ...) regardless of --adapter. Under --adapter wasi
;; this module satisfies those imports by re-exporting them and
;; translating each call into a wasi_snapshot_preview1 call. It shares
;; aver_runtime's memory so it can read OBJ_STRING bytes and write
;; iovec scratch in the same address space as user.wasm.
;;
;; Memory layout it uses (already reserved by aver_runtime IO_SCRATCH):
;;   0..7   : iovec  (i32 ptr, i32 len)
;;   8..11  : nwritten count
;;
;; Coverage for now:
;;   - console_print  → fd_write(stdout=1, ...)
;;   - console_error  → fd_write(stderr=2, ...)
;;   - print_value    → tag-dispatched format + fd_write(1)
;;
;; The remaining aver/* effects (console_readLine, args_*, time_*,
;; random_*, math_*, terminal_*, format_value) trap as `unreachable`
;; today — wasm-merge will keep only the imports user.wasm actually
;; references, so simple programs (Console.print, math, etc.) work
;; while the rest is filled in incrementally.

(module
  (import "aver_runtime" "memory" (memory $mem 1))
  (import "aver_runtime" "rt_int_to_str" (func $rt_int_to_str (param i64 i32) (result i32)))
  (import "aver_runtime" "rt_float_to_str" (func $rt_float_to_str (param f64 i32) (result i32)))
  (import "aver_runtime" "rt_alloc" (func $rt_alloc (param i32) (result i32)))
  (import "aver_runtime" "rt_i64_to_str_obj"
    (func $rt_i64_to_str_obj (param i64) (result i32)))
  (import "aver_runtime" "rt_f64_to_str_obj"
    (func $rt_f64_to_str_obj (param f64) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write"
    (func $wasi_fd_write (param i32 i32 i32 i32) (result i32)))

  ;; "true" / "false" data literals at fixed offsets in shared memory.
  ;; Picked above IO_SCRATCH (128) but below the heap_base bumped on
  ;; user-module instantiate — these are short, immutable UTF-8 bytes.
  (data (i32.const 96) "true")
  (data (i32.const 100) "false")

  ;; Internal helper: write `len` bytes starting at `ptr` to fd. Sets
  ;; up the iovec at offset 0..7 and the nwritten cell at 8..11.
  (func $write_fd (param $fd i32) (param $ptr i32) (param $len i32)
    i32.const 0
    local.get $ptr
    i32.store
    i32.const 0
    local.get $len
    i32.store offset=4
    local.get $fd
    i32.const 0       ;; iovec ptr
    i32.const 1       ;; iovec count
    i32.const 8       ;; nwritten ptr
    call $wasi_fd_write
    drop
  )

  (func $console_print (param $ptr i32) (param $len i32)
    i32.const 1
    local.get $ptr
    local.get $len
    call $write_fd
  )
  (export "console_print" (func $console_print))

  (func $console_error (param $ptr i32) (param $len i32)
    i32.const 2
    local.get $ptr
    local.get $len
    call $write_fd
  )
  (export "console_error" (func $console_error))

  ;; print_value(tag, val): dispatch on tag and write to stdout.
  ;; tag 0 = Int, 1 = Float bits, 2 = Bool, 3 = String ptr,
  ;; 4 = Heap ptr (today rendered as raw bytes via OBJ_STRING shape),
  ;; 5 = Unit (nothing).
  ;;
  ;; Numbers go through aver_runtime's int_to_str / float_to_str which
  ;; format into the runtime IO_INT_BUF / IO_FLOAT_BUF scratch, then
  ;; we extract (pos, len) from the packed return and fd_write that.
  (func $print_value (param $tag i32) (param $val i64)
    (local $packed i32)
    (local $pos i32)
    (local $len i32)
    (local $ptr i32)

    ;; tag == 0 → Int
    local.get $tag
    i32.const 0
    i32.eq
    if
      local.get $val
      i32.const 16    ;; IO_INT_BUF (re-uses runtime scratch)
      call $rt_int_to_str
      local.set $packed

      i32.const 16
      local.get $packed
      i32.const 16
      i32.shr_u
      i32.add
      local.get $packed
      i32.const 0xFFFF
      i32.and
      i32.const 1
      call $write_fd
      return
    end

    ;; tag == 1 → Float
    local.get $tag
    i32.const 1
    i32.eq
    if
      local.get $val
      f64.reinterpret_i64
      i32.const 48    ;; IO_FLOAT_BUF
      call $rt_float_to_str
      local.set $packed

      i32.const 48
      local.get $packed
      i32.const 16
      i32.shr_u
      i32.add
      local.get $packed
      i32.const 0xFFFF
      i32.and
      i32.const 1
      call $write_fd
      return
    end

    ;; tag == 2 → Bool ("true" / "false" data literals)
    local.get $tag
    i32.const 2
    i32.eq
    if
      local.get $val
      i64.eqz
      if
        i32.const 1
        i32.const 100   ;; offset of "false"
        i32.const 5
        call $write_fd
      else
        i32.const 1
        i32.const 96    ;; offset of "true"
        i32.const 4
        call $write_fd
      end
      return
    end

    ;; tag == 3 (String) or tag == 4 (Heap → assume OBJ_STRING)
    ;; Both render via the OBJ_STRING shape: 8-byte header + bytes,
    ;; header low 32 bits = byte length.
    local.get $tag
    i32.const 3
    i32.eq
    local.get $tag
    i32.const 4
    i32.eq
    i32.or
    if
      local.get $val
      i32.wrap_i64
      local.set $ptr

      local.get $ptr
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      local.set $len

      i32.const 1
      local.get $ptr
      i32.const 8
      i32.add
      local.get $len
      call $write_fd
      return
    end

    ;; tag == 5 → Unit, write nothing.
  )
  (export "print_value" (func $print_value))

  ;; format_value(tag, val) -> (ptr, len). Aver host-side this allocs
  ;; a freshly-formatted string in guest memory; under the bridge we
  ;; do the same: pick the right rt formatter, point at the bytes.
  ;;   tag 0 = Int       → i64_to_str_obj → (ptr+8, len_from_header)
  ;;   tag 1 = Float     → f64_to_str_obj → same shape
  ;;   tag 2 = Bool      → "true"/"false" data literal
  ;;   tag 3 = String    → identity (already an OBJ_STRING ptr)
  ;;   tag 4 = Heap      → assume OBJ_STRING shape (best effort)
  ;;   tag 5 = Unit      → empty
  (func $format_value (param $tag i32) (param $val i64) (result i32 i32)
    (local $obj i32)

    local.get $tag
    i32.const 0
    i32.eq
    if
      local.get $val
      call $rt_i64_to_str_obj
      local.set $obj

      local.get $obj
      i32.const 8
      i32.add
      local.get $obj
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      return
    end

    local.get $tag
    i32.const 1
    i32.eq
    if
      local.get $val
      f64.reinterpret_i64
      call $rt_f64_to_str_obj
      local.set $obj

      local.get $obj
      i32.const 8
      i32.add
      local.get $obj
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      return
    end

    local.get $tag
    i32.const 2
    i32.eq
    if
      local.get $val
      i64.eqz
      if
        i32.const 100   ;; "false"
        i32.const 5
        return
      else
        i32.const 96    ;; "true"
        i32.const 4
        return
      end
    end

    ;; tag == 3 or 4 → assume OBJ_STRING ptr in low 32 bits of val.
    local.get $tag
    i32.const 3
    i32.eq
    local.get $tag
    i32.const 4
    i32.eq
    i32.or
    if
      local.get $val
      i32.wrap_i64
      local.set $obj

      local.get $obj
      i32.const 8
      i32.add
      local.get $obj
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      return
    end

    ;; tag == 5 → empty
    i32.const 0
    i32.const 0
  )
  (export "format_value" (func $format_value))
)
