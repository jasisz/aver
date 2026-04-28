;; UTF-8 decode of the first code point in an OBJ_STRING. Traps
;; (unreachable) on empty input or truncated multi-byte sequence.
;; Returns the codepoint as i64 so a 4-byte sequence can fit.

(func $rt_char_to_code (param $str i32) (result i64)
  (local $byte_len i32)
  (local $lead i32)
  (local $b1 i32)
  (local $b2 i32)
  (local $b3 i32)
  (local $code i32)

  ;; byte_len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $byte_len

  ;; Empty string → trap
  local.get $byte_len
  i32.eqz
  if
    unreachable
  end

  ;; lead = byte at str+8
  local.get $str
  i32.load8_u offset=8
  local.set $lead

  local.get $lead
  i32.const 0x80
  i32.lt_u
  if
    ;; ASCII — code = lead
    local.get $lead
    local.set $code
  else
    local.get $lead
    i32.const 0xE0
    i32.lt_u
    if
      ;; 2-byte sequence
      local.get $byte_len
      i32.const 2
      i32.lt_u
      if
        unreachable
      end

      local.get $str
      i32.load8_u offset=9
      local.set $b1

      local.get $lead
      i32.const 0x1F
      i32.and
      i32.const 6
      i32.shl
      local.get $b1
      i32.const 0x3F
      i32.and
      i32.or
      local.set $code
    else
      local.get $lead
      i32.const 0xF0
      i32.lt_u
      if
        ;; 3-byte sequence
        local.get $byte_len
        i32.const 3
        i32.lt_u
        if
          unreachable
        end

        local.get $str
        i32.load8_u offset=9
        local.set $b1
        local.get $str
        i32.load8_u offset=10
        local.set $b2

        local.get $lead
        i32.const 0x0F
        i32.and
        i32.const 12
        i32.shl
        local.get $b1
        i32.const 0x3F
        i32.and
        i32.const 6
        i32.shl
        i32.or
        local.get $b2
        i32.const 0x3F
        i32.and
        i32.or
        local.set $code
      else
        ;; 4-byte sequence
        local.get $byte_len
        i32.const 4
        i32.lt_u
        if
          unreachable
        end

        local.get $str
        i32.load8_u offset=9
        local.set $b1
        local.get $str
        i32.load8_u offset=10
        local.set $b2
        local.get $str
        i32.load8_u offset=11
        local.set $b3

        local.get $lead
        i32.const 0x07
        i32.and
        i32.const 18
        i32.shl
        local.get $b1
        i32.const 0x3F
        i32.and
        i32.const 12
        i32.shl
        i32.or
        local.get $b2
        i32.const 0x3F
        i32.and
        i32.const 6
        i32.shl
        i32.or
        local.get $b3
        i32.const 0x3F
        i32.and
        i32.or
        local.set $code
      end
    end
  end

  local.get $code
  i64.extend_i32_u
)
(export "rt_char_to_code" (func $rt_char_to_code))
