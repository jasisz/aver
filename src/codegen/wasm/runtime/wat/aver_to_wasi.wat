;; aver/* → wasi_snapshot_preview1.* translation shim.
;;
;; user.wasm always emits Aver-flavored host imports (aver/console_print,
;; aver/print_value, ...) regardless of how it is later deployed. Under
;; `--bridge wasi` this module satisfies those imports by re-exporting
;; them and translating each call into a wasi_snapshot_preview1 call.
;; It shares aver_runtime's memory so it can read OBJ_STRING bytes and
;; write iovec scratch in the same address space as user.wasm.
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
  (import "wasi_snapshot_preview1" "random_get"
    (func $wasi_random_get (param i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "clock_time_get"
    (func $wasi_clock_time_get (param i32 i64 i32) (result i32)))
  (import "wasi_snapshot_preview1" "environ_sizes_get"
    (func $wasi_environ_sizes_get (param i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "environ_get"
    (func $wasi_environ_get (param i32 i32) (result i32)))

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

  ;; WASI has no separate "warning" stream — fd 2 (stderr) is the
  ;; conventional landing for both warnings and errors. JS hosts
  ;; (workers, deno, bun) route `console.warn` separately, but for
  ;; standalone wasmtime we route through stderr.
  (func $console_warn (param $ptr i32) (param $len i32)
    i32.const 2
    local.get $ptr
    local.get $len
    call $write_fd
  )
  (export "console_warn" (func $console_warn))

  ;; Env.get: walk the WASI environ table looking for `name=…`.
  ;; WASI preview 1 exposes the whole environment as a flat
  ;; (table + buffer) pair via `environ_sizes_get` + `environ_get`.
  ;; Each table slot points at a `KEY=VALUE\0` C string in the
  ;; buffer. We allocate scratch via `rt_alloc` (the bump
  ;; allocator never frees, so this leaks a few hundred bytes per
  ;; call — acceptable for a config lookup that fires once per
  ;; request handler).
  ;;
  ;; Returns -1 (Aver's NONE_SENTINEL) if the name isn't bound;
  ;; otherwise allocates an `OBJ_STRING` for the value and returns
  ;; its pointer. The caller (emit-side `Env.get` lowering) wraps
  ;; that into `Option.Some(_)`.
  (func $env_get (param $name_ptr i32) (param $name_len i32) (result i32)
    (local $sizes_ptr i32)
    (local $env_count i32)
    (local $env_buf_size i32)
    (local $table_ptr i32)
    (local $buf_ptr i32)
    (local $i i32)
    (local $entry_str i32)
    (local $value_start i32)
    (local $value_len i32)

    ;; Scratch: 8 bytes for (count, buf_size) returned by environ_sizes_get.
    i32.const 8
    call $rt_alloc
    local.set $sizes_ptr

    local.get $sizes_ptr
    local.get $sizes_ptr
    i32.const 4
    i32.add
    call $wasi_environ_sizes_get
    drop

    local.get $sizes_ptr
    i32.load
    local.set $env_count
    local.get $sizes_ptr
    i32.const 4
    i32.add
    i32.load
    local.set $env_buf_size

    ;; No env vars at all.
    local.get $env_count
    i32.eqz
    if
      i32.const -1
      return
    end

    ;; Allocate table (count * 4 bytes for u32 ptrs) and buffer.
    local.get $env_count
    i32.const 4
    i32.mul
    call $rt_alloc
    local.set $table_ptr
    local.get $env_buf_size
    call $rt_alloc
    local.set $buf_ptr

    local.get $table_ptr
    local.get $buf_ptr
    call $wasi_environ_get
    drop

    ;; Walk table[0..count].
    i32.const 0
    local.set $i
    block $miss
      loop $next
        local.get $i
        local.get $env_count
        i32.ge_u
        br_if $miss

        ;; entry_str = table[i]
        local.get $table_ptr
        local.get $i
        i32.const 4
        i32.mul
        i32.add
        i32.load
        local.set $entry_str

        ;; Match: entry_str[0..name_len] == name AND entry_str[name_len] == '='
        local.get $entry_str
        local.get $name_ptr
        local.get $name_len
        call $env_match_prefix
        if
          ;; Found — value starts after the '='.
          local.get $entry_str
          local.get $name_len
          i32.add
          i32.const 1
          i32.add
          local.set $value_start

          ;; Length of NUL-terminated value.
          local.get $value_start
          call $cstr_len
          local.set $value_len

          ;; Allocate OBJ_STRING(value_start, value_len) and return.
          local.get $value_start
          local.get $value_len
          call $alloc_obj_string_copy
          return
        end

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $next
      end
    end

    ;; Walked the whole table, no match.
    i32.const -1
  )
  (export "env_get" (func $env_get))

  ;; Helper: returns 1 if `entry_str` starts with the `name_len`
  ;; bytes at `name_ptr` followed by `=`, otherwise 0.
  (func $env_match_prefix
        (param $entry_str i32) (param $name_ptr i32) (param $name_len i32)
        (result i32)
    (local $i i32)
    (local $a i32)
    (local $b i32)

    i32.const 0
    local.set $i
    block $mismatch
      loop $cmp
        local.get $i
        local.get $name_len
        i32.ge_u
        br_if 1
        local.get $entry_str
        local.get $i
        i32.add
        i32.load8_u
        local.set $a
        local.get $name_ptr
        local.get $i
        i32.add
        i32.load8_u
        local.set $b
        local.get $a
        local.get $b
        i32.ne
        br_if $mismatch
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $cmp
      end
    end
    ;; If we exited via mismatch, $i < name_len → fail.
    local.get $i
    local.get $name_len
    i32.lt_u
    if
      i32.const 0
      return
    end
    ;; Names matched; require '=' separator at entry_str[name_len].
    local.get $entry_str
    local.get $name_len
    i32.add
    i32.load8_u
    i32.const 0x3D ;; '='
    i32.eq
  )

  ;; Helper: length of NUL-terminated C string at `ptr` (excludes NUL).
  (func $cstr_len (param $ptr i32) (result i32)
    (local $i i32)
    i32.const 0
    local.set $i
    block $done
      loop $next
        local.get $ptr
        local.get $i
        i32.add
        i32.load8_u
        i32.eqz
        br_if $done
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $next
      end
    end
    local.get $i
  )

  ;; Helper: allocate an OBJ_STRING (kind=0) with a header carrying
  ;; the byte length, then memcpy the bytes from `src` into the
  ;; payload area. Returns the OBJ_STRING handle.
  (func $alloc_obj_string_copy
        (param $src i32) (param $len i32) (result i32)
    (local $obj i32)

    ;; Allocate header (8 bytes) + payload, 8-byte aligned.
    local.get $len
    i32.const 8
    i32.add
    call $rt_alloc
    local.set $obj

    ;; Header: kind=0 (OBJ_STRING) << 56 | len in low 32 bits.
    ;; Kind 0 means high byte stays zero; just store the length as i64.
    local.get $obj
    local.get $len
    i64.extend_i32_u
    i64.store

    ;; Copy `len` bytes from `src` into `obj + 8`.
    local.get $obj
    i32.const 8
    i32.add
    local.get $src
    local.get $len
    memory.copy

    local.get $obj
  )

  ;; WASI preview 1 has no `setenv`; honour the
  ;; `Env.set: (..) -> Unit` contract by accepting the call and
  ;; dropping it. Programs that need persistent env mutation should
  ;; either use a different runtime (Workers `env` is also frozen,
  ;; same shape) or wait for a preview 2 host that exposes write.
  (func $env_set
        (param $name_ptr i32) (param $name_len i32)
        (param $value_ptr i32) (param $value_len i32))
  (export "env_set" (func $env_set))

  ;; Http.* under WASI preview 1: there's no native HTTP client,
  ;; just sockets (`fd_read`/`fd_write`); wasi-http preview 2 isn't
  ;; in mainstream wasmtime yet. The bridge satisfies the import so
  ;; user.wasm links, but every send returns a transport error so
  ;; programs branch through `Result.Err` instead of crashing.
  ;; Real HTTP from `--bridge wasi` lands in 0.15 once preview 2
  ;; (or a JS host above wasmtime) is available.
  ;;
  ;; Build the error message bytes lazily into a fresh `rt_alloc`
  ;; buffer on every call. The bump allocator never reclaims, so a
  ;; loop hammering `Http.*` will leak ~50 bytes per call — the
  ;; assumption is "if you're calling Http.* under wasi-bridge
  ;; you're hitting a fallback path, not a hot loop". A static
  ;; data offset would be neater but coordinating it with user.wasm's
  ;; interned-literal table at merge time isn't worth the bytes.
  ;; `request_headers_load` under WASI: there's no host-side
  ;; request to read headers from (the WASI bridge runs Aver
  ;; programs as standalone CLIs, not request handlers). Return
  ;; empty Map handle (`0`) — `req.headers` on a CLI program has
  ;; nothing to read from.
  (func $request_headers_load (result i32)
    i32.const 0
  )
  (export "request_headers_load" (func $request_headers_load))

  (func $http_send
        (param $method_ptr i32) (param $method_len i32)
        (param $url_ptr i32)    (param $url_len i32)
        (param $body_ptr i32)   (param $body_len i32)
        (param $ct_ptr i32)     (param $ct_len i32)
        (result i64 i32 i32 i32)
    (local $msg_buf i32)
    (local $obj i32)

    ;; Allocate scratch + write the message bytes one by one. 40 chars
    ;; (no NUL — `alloc_obj_string_copy` records the length in the
    ;; OBJ_STRING header).
    i32.const 40
    call $rt_alloc
    local.set $msg_buf
    local.get $msg_buf i32.const 0  i32.add  i32.const 0x48 i32.store8 ;; H
    local.get $msg_buf i32.const 1  i32.add  i32.const 0x74 i32.store8 ;; t
    local.get $msg_buf i32.const 2  i32.add  i32.const 0x74 i32.store8 ;; t
    local.get $msg_buf i32.const 3  i32.add  i32.const 0x70 i32.store8 ;; p
    local.get $msg_buf i32.const 4  i32.add  i32.const 0x2E i32.store8 ;; .
    local.get $msg_buf i32.const 5  i32.add  i32.const 0x2A i32.store8 ;; *
    local.get $msg_buf i32.const 6  i32.add  i32.const 0x20 i32.store8 ;; (space)
    local.get $msg_buf i32.const 7  i32.add  i32.const 0x6E i32.store8 ;; n
    local.get $msg_buf i32.const 8  i32.add  i32.const 0x6F i32.store8 ;; o
    local.get $msg_buf i32.const 9  i32.add  i32.const 0x74 i32.store8 ;; t
    local.get $msg_buf i32.const 10 i32.add  i32.const 0x20 i32.store8
    local.get $msg_buf i32.const 11 i32.add  i32.const 0x61 i32.store8 ;; a
    local.get $msg_buf i32.const 12 i32.add  i32.const 0x76 i32.store8 ;; v
    local.get $msg_buf i32.const 13 i32.add  i32.const 0x61 i32.store8 ;; a
    local.get $msg_buf i32.const 14 i32.add  i32.const 0x69 i32.store8 ;; i
    local.get $msg_buf i32.const 15 i32.add  i32.const 0x6C i32.store8 ;; l
    local.get $msg_buf i32.const 16 i32.add  i32.const 0x61 i32.store8 ;; a
    local.get $msg_buf i32.const 17 i32.add  i32.const 0x62 i32.store8 ;; b
    local.get $msg_buf i32.const 18 i32.add  i32.const 0x6C i32.store8 ;; l
    local.get $msg_buf i32.const 19 i32.add  i32.const 0x65 i32.store8 ;; e
    local.get $msg_buf i32.const 20 i32.add  i32.const 0x20 i32.store8
    local.get $msg_buf i32.const 21 i32.add  i32.const 0x75 i32.store8 ;; u
    local.get $msg_buf i32.const 22 i32.add  i32.const 0x6E i32.store8 ;; n
    local.get $msg_buf i32.const 23 i32.add  i32.const 0x64 i32.store8 ;; d
    local.get $msg_buf i32.const 24 i32.add  i32.const 0x65 i32.store8 ;; e
    local.get $msg_buf i32.const 25 i32.add  i32.const 0x72 i32.store8 ;; r
    local.get $msg_buf i32.const 26 i32.add  i32.const 0x20 i32.store8
    local.get $msg_buf i32.const 27 i32.add  i32.const 0x2D i32.store8 ;; -
    local.get $msg_buf i32.const 28 i32.add  i32.const 0x2D i32.store8 ;; -
    local.get $msg_buf i32.const 29 i32.add  i32.const 0x62 i32.store8 ;; b
    local.get $msg_buf i32.const 30 i32.add  i32.const 0x72 i32.store8 ;; r
    local.get $msg_buf i32.const 31 i32.add  i32.const 0x69 i32.store8 ;; i
    local.get $msg_buf i32.const 32 i32.add  i32.const 0x64 i32.store8 ;; d
    local.get $msg_buf i32.const 33 i32.add  i32.const 0x67 i32.store8 ;; g
    local.get $msg_buf i32.const 34 i32.add  i32.const 0x65 i32.store8 ;; e
    local.get $msg_buf i32.const 35 i32.add  i32.const 0x20 i32.store8
    local.get $msg_buf i32.const 36 i32.add  i32.const 0x77 i32.store8 ;; w
    local.get $msg_buf i32.const 37 i32.add  i32.const 0x61 i32.store8 ;; a
    local.get $msg_buf i32.const 38 i32.add  i32.const 0x73 i32.store8 ;; s
    local.get $msg_buf i32.const 39 i32.add  i32.const 0x69 i32.store8 ;; i

    i64.const 0
    i32.const 0
    i32.const 0       ;; headers handle = 0 (empty Map)
    local.get $msg_buf
    i32.const 40
    call $alloc_obj_string_copy
  )
  (export "http_send" (func $http_send))

  ;; The pending-request-header list is host-side state on JS hosts;
  ;; the WASI bridge just drops every entry to keep the type
  ;; contracts honoured. No-op clear, no-op add.
  (func $http_clear_request_headers)
  (export "http_clear_request_headers" (func $http_clear_request_headers))

  (func $http_add_request_header
        (param $name_ptr i32) (param $name_len i32)
        (param $value_ptr i32) (param $value_len i32))
  (export "http_add_request_header" (func $http_add_request_header))

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

  ;; ─── Random ─────────────────────────────────────────────────────────
  ;; aver/random_int(min: i64, max: i64) -> i64
  ;; Pull 8 random bytes via wasi.random_get into IO_INT_BUF (16..23),
  ;; reduce mod (max - min + 1), shift to [min, max].
  (func $random_int (param $min i64) (param $max i64) (result i64)
    (local $range i64)
    (local $bits i64)

    i32.const 16   ;; IO_INT_BUF
    i32.const 8
    call $wasi_random_get
    drop

    i32.const 16
    i64.load
    local.set $bits

    local.get $max
    local.get $min
    i64.sub
    i64.const 1
    i64.add
    local.set $range

    local.get $range
    i64.eqz
    if (result i64)
      local.get $min
    else
      local.get $min
      local.get $bits
      local.get $range
      i64.rem_u
      i64.add
    end
  )
  (export "random_int" (func $random_int))

  ;; aver/random_float() -> f64 in [0.0, 1.0)
  ;; Pull 8 random bytes, drop top 11 bits (so 53-bit mantissa fits),
  ;; divide by 2^53.
  (func $random_float (result f64)
    i32.const 16
    i32.const 8
    call $wasi_random_get
    drop

    i32.const 16
    i64.load
    i64.const 11
    i64.shr_u
    f64.convert_i64_u
    f64.const 9007199254740992  ;; 2^53
    f64.div
  )
  (export "random_float" (func $random_float))

  ;; ─── Time ───────────────────────────────────────────────────────────
  ;; aver/time_unixMs() -> i64 (milliseconds since Unix epoch)
  ;; wasi.clock_time_get(clockid, precision, *out_time) returns errno.
  ;; Clock id 0 = REALTIME. Result is nanoseconds; divide by 1e6 → ms.
  (func $time_unixMs (result i64)
    i32.const 0               ;; clock id: REALTIME
    i64.const 1000000         ;; precision (ns) — 1ms is plenty
    i32.const 16              ;; *time_ptr (write into IO_INT_BUF)
    call $wasi_clock_time_get
    drop

    i32.const 16
    i64.load
    i64.const 1000000
    i64.div_u
  )
  (export "time_unixMs" (func $time_unixMs))

)
