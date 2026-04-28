;; UTF-8 indexed char access. Walks code points until char_pos == idx
;; or end-of-string, then alloc + copy that single char's bytes into a
;; fresh OBJ_STRING, wrapped as Some(string). Out-of-range or negative
;; idx → NONE_SENTINEL (-1).

(func $rt_str_char_at (param $str i32) (param $idx i64) (result i32)
  (local $byte_len i32)
  (local $target i32)
  (local $byte_pos i32)
  (local $char_pos i32)
  (local $lead i32)
  (local $width i32)
  (local $ptr i32)

  ;; Negative idx → None
  local.get $idx
  i64.const 0
  i64.lt_s
  if
    i32.const -1
    return
  end

  ;; byte_len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $byte_len

  local.get $idx
  i32.wrap_i64
  local.set $target

  i32.const 0
  local.set $byte_pos
  i32.const 0
  local.set $char_pos

  block
    loop
      local.get $byte_pos
      local.get $byte_len
      i32.ge_u
      br_if 1

      ;; lead byte
      local.get $str
      local.get $byte_pos
      i32.add
      i32.load8_u offset=8
      local.set $lead

      ;; width
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

      ;; if char_pos == target → emit and return
      local.get $char_pos
      local.get $target
      i32.eq
      if
        ;; alloc OBJ_STRING with len = width
        i32.const 8
        local.get $width
        i32.const 7
        i32.add
        i32.const -8
        i32.and
        i32.add
        call $rt_alloc
        local.set $ptr

        local.get $ptr
        i64.const 0
        local.get $width
        i64.extend_i32_u
        i64.or
        i64.store

        ;; memory.copy: dst = ptr+8, src = str+8+byte_pos, len = width
        local.get $ptr
        i32.const 8
        i32.add
        local.get $str
        i32.const 8
        i32.add
        local.get $byte_pos
        i32.add
        local.get $width
        memory.copy

        i32.const 2   ;; WRAP_SOME
        local.get $ptr
        i32.const 1
        call $rt_wrap_i32
        return
      end

      local.get $byte_pos
      local.get $width
      i32.add
      local.set $byte_pos

      local.get $char_pos
      i32.const 1
      i32.add
      local.set $char_pos
      br 0
    end
  end

  i32.const -1   ;; NONE_SENTINEL
)
(export "rt_str_char_at" (func $rt_str_char_at))
