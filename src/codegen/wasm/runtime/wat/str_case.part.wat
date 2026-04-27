;; ASCII-only case mapping. Allocates a fresh OBJ_STRING with the
;; same byte length as the input, then walks bytes one-at-a-time:
;; ASCII letters in the source range get ±32, every other byte is
;; copied verbatim. Multi-byte UTF-8 payloads pass through untouched
;; because every continuation/lead byte falls outside the matched
;; ASCII range.

(func $rt_str_to_lower (param $str i32) (result i32)
  (local $len i32)
  (local $i i32)
  (local $ptr i32)
  (local $ch i32)

  ;; len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  ;; alloc 8 + align8(len)
  i32.const 8
  local.get $len
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  ;; header = (OBJ_STRING << 56) | len
  local.get $ptr
  i64.const 0
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $len
      i32.ge_u
      br_if 1

      local.get $str
      local.get $i
      i32.add
      i32.load8_u offset=8
      local.set $ch

      ;; if 'A' <= ch <= 'Z' → ch += 32
      local.get $ch
      i32.const 0x41
      i32.ge_u
      local.get $ch
      i32.const 0x5A
      i32.le_u
      i32.and
      if
        local.get $ch
        i32.const 32
        i32.add
        local.set $ch
      end

      local.get $ptr
      local.get $i
      i32.add
      local.get $ch
      i32.store8 offset=8

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $ptr
)
(export "rt_str_to_lower" (func $rt_str_to_lower))

(func $rt_str_to_upper (param $str i32) (result i32)
  (local $len i32)
  (local $i i32)
  (local $ptr i32)
  (local $ch i32)

  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  i32.const 8
  local.get $len
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0
  local.get $len
  i64.extend_i32_u
  i64.or
  i64.store

  i32.const 0
  local.set $i
  block
    loop
      local.get $i
      local.get $len
      i32.ge_u
      br_if 1

      local.get $str
      local.get $i
      i32.add
      i32.load8_u offset=8
      local.set $ch

      ;; if 'a' <= ch <= 'z' → ch -= 32
      local.get $ch
      i32.const 0x61
      i32.ge_u
      local.get $ch
      i32.const 0x7A
      i32.le_u
      i32.and
      if
        local.get $ch
        i32.const 32
        i32.sub
        local.set $ch
      end

      local.get $ptr
      local.get $i
      i32.add
      local.get $ch
      i32.store8 offset=8

      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br 0
    end
  end

  local.get $ptr
)
(export "rt_str_to_upper" (func $rt_str_to_upper))
