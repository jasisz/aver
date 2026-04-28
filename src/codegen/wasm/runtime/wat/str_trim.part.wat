;; Trim ASCII whitespace (space, tab, LF, CR) from both ends of an
;; OBJ_STRING. If nothing is trimmed (whole string is non-whitespace
;; or empty), returns the original pointer unchanged. Otherwise
;; allocates a fresh OBJ_STRING and memory.copy's the inner slice.

(func $rt_str_trim (param $str i32) (result i32)
  (local $byte_len i32)
  (local $start i32)
  (local $end i32)
  (local $ch i32)
  (local $new_len i32)
  (local $ptr i32)

  ;; byte_len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $byte_len

  ;; Walk forward over leading whitespace.
  i32.const 0
  local.set $start
  block
    loop
      local.get $start
      local.get $byte_len
      i32.ge_u
      br_if 1

      local.get $str
      local.get $start
      i32.add
      i32.load8_u offset=8
      local.set $ch

      ;; if not whitespace → break
      local.get $ch
      i32.const 0x20   ;; ' '
      i32.eq
      local.get $ch
      i32.const 0x09   ;; '\t'
      i32.eq
      i32.or
      local.get $ch
      i32.const 0x0A   ;; '\n'
      i32.eq
      i32.or
      local.get $ch
      i32.const 0x0D   ;; '\r'
      i32.eq
      i32.or
      i32.eqz
      br_if 1

      local.get $start
      i32.const 1
      i32.add
      local.set $start
      br 0
    end
  end

  ;; Walk backward over trailing whitespace.
  local.get $byte_len
  local.set $end
  block
    loop
      local.get $end
      local.get $start
      i32.le_u
      br_if 1

      local.get $str
      local.get $end
      i32.const 1
      i32.sub
      i32.add
      i32.load8_u offset=8
      local.set $ch

      local.get $ch
      i32.const 0x20
      i32.eq
      local.get $ch
      i32.const 0x09
      i32.eq
      i32.or
      local.get $ch
      i32.const 0x0A
      i32.eq
      i32.or
      local.get $ch
      i32.const 0x0D
      i32.eq
      i32.or
      i32.eqz
      br_if 1

      local.get $end
      i32.const 1
      i32.sub
      local.set $end
      br 0
    end
  end

  ;; new_len = end - start
  local.get $end
  local.get $start
  i32.sub
  local.set $new_len

  ;; If nothing was trimmed, return the original pointer.
  local.get $new_len
  local.get $byte_len
  i32.eq
  if (result i32)
    local.get $str
  else
    ;; alloc + memory.copy slice
    i32.const 8
    local.get $new_len
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.add
    call $rt_alloc
    local.set $ptr

    local.get $ptr
    i64.const 0
    local.get $new_len
    i64.extend_i32_u
    i64.or
    i64.store

    local.get $ptr
    i32.const 8
    i32.add
    local.get $str
    i32.const 8
    i32.add
    local.get $start
    i32.add
    local.get $new_len
    memory.copy

    local.get $ptr
  end
)
(export "rt_str_trim" (func $rt_str_trim))
