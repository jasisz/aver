;; Encode a Unicode code point as UTF-8 inside a fresh OBJ_STRING.
;; Returns NONE_SENTINEL (-1) on out-of-range or surrogate code
;; points. Otherwise wraps the new string with rt_wrap_i32 + WRAP_SOME.

(func $rt_char_from_code (param $code64 i64) (result i32)
  (local $code i32)
  (local $len i32)
  (local $ptr i32)

  ;; Reject negatives, > 0x10FFFF, or surrogate range [0xD800, 0xDFFF].
  local.get $code64
  i64.const 0
  i64.lt_s
  local.get $code64
  i64.const 0x10FFFF
  i64.gt_s
  i32.or
  local.get $code64
  i64.const 0xD800
  i64.ge_s
  local.get $code64
  i64.const 0xDFFF
  i64.le_s
  i32.and
  i32.or
  if (result i32)
    i32.const -1   ;; NONE_SENTINEL
  else
    local.get $code64
    i32.wrap_i64
    local.set $code

    ;; len = 1/2/3/4 by code range
    local.get $code
    i32.const 0x80
    i32.lt_u
    if (result i32)
      i32.const 1
    else
      local.get $code
      i32.const 0x800
      i32.lt_u
      if (result i32)
        i32.const 2
      else
        local.get $code
        i32.const 0x10000
        i32.lt_u
        if (result i32)
          i32.const 3
        else
          i32.const 4
        end
      end
    end
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

    ;; Encode bytes by len.
    local.get $len
    i32.const 1
    i32.eq
    if
      local.get $ptr
      local.get $code
      i32.store8 offset=8
    else
      local.get $len
      i32.const 2
      i32.eq
      if
        local.get $ptr
        local.get $code
        i32.const 6
        i32.shr_u
        i32.const 0xC0
        i32.or
        i32.store8 offset=8

        local.get $ptr
        local.get $code
        i32.const 0x3F
        i32.and
        i32.const 0x80
        i32.or
        i32.store8 offset=9
      else
        local.get $len
        i32.const 3
        i32.eq
        if
          local.get $ptr
          local.get $code
          i32.const 12
          i32.shr_u
          i32.const 0xE0
          i32.or
          i32.store8 offset=8

          local.get $ptr
          local.get $code
          i32.const 6
          i32.shr_u
          i32.const 0x3F
          i32.and
          i32.const 0x80
          i32.or
          i32.store8 offset=9

          local.get $ptr
          local.get $code
          i32.const 0x3F
          i32.and
          i32.const 0x80
          i32.or
          i32.store8 offset=10
        else
          ;; len == 4
          local.get $ptr
          local.get $code
          i32.const 18
          i32.shr_u
          i32.const 0xF0
          i32.or
          i32.store8 offset=8

          local.get $ptr
          local.get $code
          i32.const 12
          i32.shr_u
          i32.const 0x3F
          i32.and
          i32.const 0x80
          i32.or
          i32.store8 offset=9

          local.get $ptr
          local.get $code
          i32.const 6
          i32.shr_u
          i32.const 0x3F
          i32.and
          i32.const 0x80
          i32.or
          i32.store8 offset=10

          local.get $ptr
          local.get $code
          i32.const 0x3F
          i32.and
          i32.const 0x80
          i32.or
          i32.store8 offset=11
        end
      end
    end

    ;; Wrap as Some(ptr), is_ptr = 1
    i32.const 2   ;; WRAP_SOME
    local.get $ptr
    i32.const 1
    call $rt_wrap_i32
  end
)
(export "rt_char_from_code" (func $rt_char_from_code))
