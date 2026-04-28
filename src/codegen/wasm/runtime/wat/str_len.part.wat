;; Number of UTF-8 code points in an OBJ_STRING. Walks the byte
;; payload, advancing by 1/2/3/4 bytes per code point depending on
;; the lead byte's high bits.

(func $rt_str_len (param $str i32) (result i64)
  (local $byte_len i32)
  (local $byte_pos i32)
  (local $char_len i32)
  (local $lead i32)
  (local $width i32)

  ;; byte_len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $byte_len

  i32.const 0
  local.set $byte_pos
  i32.const 0
  local.set $char_len

  block
    loop
      local.get $byte_pos
      local.get $byte_len
      i32.ge_u
      br_if 1

      ;; lead = byte at str+8+byte_pos
      local.get $str
      local.get $byte_pos
      i32.add
      i32.load8_u offset=8
      local.set $lead

      ;; width = utf8 char width by lead-byte range
      local.get $lead
      i32.const 0x80
      i32.lt_u
      if
        i32.const 1
        local.set $width
      else
        local.get $lead
        i32.const 0xE0
        i32.lt_u
        if
          i32.const 2
          local.set $width
        else
          local.get $lead
          i32.const 0xF0
          i32.lt_u
          if
            i32.const 3
            local.set $width
          else
            i32.const 4
            local.set $width
          end
        end
      end

      local.get $byte_pos
      local.get $width
      i32.add
      local.set $byte_pos

      local.get $char_len
      i32.const 1
      i32.add
      local.set $char_len
      br 0
    end
  end

  local.get $char_len
  i64.extend_i32_u
)
(export "rt_str_len" (func $rt_str_len))
