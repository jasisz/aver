;; Byte-length helpers and substring search over OBJ_STRING heap
;; objects. Header layout: low 32 bits of the i64 header hold the
;; byte length; payload starts at offset 8.

(func $rt_str_byte_len (param $str i32) (result i64)
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
)
(export "rt_str_byte_len" (func $rt_str_byte_len))

;; rt_str_find: first byte index of `needle` in `hay` at-or-after
;; `start`. Returns -1 if absent. Empty needle returns clamp(start, 0,
;; hay_len). Negative `start` clamps to 0.
(func $rt_str_find (param $hay i32) (param $needle i32) (param $start i32) (result i32)
  (local $hay_len i32)
  (local $needle_len i32)
  (local $search_start i32)
  (local $limit i32)
  (local $pos i32)
  (local $i i32)
  (local $found i32)

  ;; hay_len = header.byte_len
  local.get $hay
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $hay_len

  ;; needle_len = header.byte_len
  local.get $needle
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $needle_len

  ;; search_start = max(start, 0)
  local.get $start
  i32.const 0
  i32.lt_s
  if (result i32)
    i32.const 0
  else
    local.get $start
  end
  local.set $search_start

  ;; needle empty → return min(search_start, hay_len)
  local.get $needle_len
  i32.eqz
  if
    local.get $search_start
    local.get $hay_len
    i32.le_u
    if (result i32)
      local.get $search_start
    else
      local.get $hay_len
    end
    return
  end

  ;; needle longer than hay → not found
  local.get $hay_len
  local.get $needle_len
  i32.lt_u
  if
    i32.const -1
    return
  end

  ;; limit = hay_len - needle_len  (last possible position)
  local.get $hay_len
  local.get $needle_len
  i32.sub
  local.set $limit

  ;; search_start past limit → not found
  local.get $search_start
  local.get $limit
  i32.gt_u
  if
    i32.const -1
    return
  end

  local.get $search_start
  local.set $pos

  block
    loop
      local.get $pos
      local.get $limit
      i32.gt_u
      br_if 1

      i32.const 0
      local.set $i
      i32.const 1
      local.set $found

      block
        loop
          local.get $i
          local.get $needle_len
          i32.ge_u
          br_if 1

          local.get $hay
          local.get $pos
          i32.add
          local.get $i
          i32.add
          i32.load8_u offset=8

          local.get $needle
          local.get $i
          i32.add
          i32.load8_u offset=8

          i32.ne
          if
            i32.const 0
            local.set $found
            br 2
          end

          local.get $i
          i32.const 1
          i32.add
          local.set $i
          br 0
        end
      end

      local.get $found
      if
        local.get $pos
        return
      end

      local.get $pos
      i32.const 1
      i32.add
      local.set $pos
      br 0
    end
  end

  i32.const -1
)
(export "rt_str_find" (func $rt_str_find))

(func $rt_str_starts_with (param $str i32) (param $prefix i32) (result i32)
  local.get $str
  local.get $prefix
  i32.const 0
  call $rt_str_find
  i32.const 0
  i32.eq
)
(export "rt_str_starts_with" (func $rt_str_starts_with))

(func $rt_str_ends_with (param $str i32) (param $suffix i32) (result i32)
  (local $str_len i32)
  (local $suffix_len i32)
  (local $start i32)

  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $str_len

  local.get $suffix
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $suffix_len

  ;; suffix longer than str → false
  local.get $suffix_len
  local.get $str_len
  i32.gt_u
  if
    i32.const 0
    return
  end

  local.get $str_len
  local.get $suffix_len
  i32.sub
  local.set $start

  local.get $str
  local.get $suffix
  local.get $start
  call $rt_str_find
  local.get $start
  i32.eq
)
(export "rt_str_ends_with" (func $rt_str_ends_with))

(func $rt_str_contains (param $str i32) (param $needle i32) (result i32)
  local.get $str
  local.get $needle
  i32.const 0
  call $rt_str_find
  i32.const -1
  i32.ne
)
(export "rt_str_contains" (func $rt_str_contains))
