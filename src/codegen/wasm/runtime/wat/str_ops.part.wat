;; UTF-8 string operations: slice (by code point), chars (one-char
;; strings), split (by delimiter), join (with separator), replace
;; (split + join).

(func $rt_str_slice (param $str i32) (param $start i32) (param $end i32) (result i32)
  (local $start_idx i32)
  (local $end_idx i32)
  (local $byte_len i32)
  (local $byte_pos i32)
  (local $char_pos i32)
  (local $byte_start i32)
  (local $byte_end i32)
  (local $lead i32)
  (local $width i32)
  (local $ptr i32)
  (local $new_len i32)

  ;; start_idx = max(start, 0)
  local.get $start
  i32.const 0
  i32.lt_s
  if (result i32)
    i32.const 0
  else
    local.get $start
  end
  local.set $start_idx

  ;; end_idx = max(end, 0)
  local.get $end
  i32.const 0
  i32.lt_s
  if (result i32)
    i32.const 0
  else
    local.get $end
  end
  local.set $end_idx

  ;; start >= end → return empty OBJ_STRING
  local.get $start_idx
  local.get $end_idx
  i32.ge_s
  if
    i32.const 8
    call $rt_alloc
    local.set $ptr
    local.get $ptr
    i64.const 0
    i64.store
    local.get $ptr
    return
  end

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
  local.set $char_pos
  local.get $byte_len
  local.set $byte_start
  local.get $byte_len
  local.set $byte_end

  block
    loop
      ;; if char_pos == start_idx → record byte_start
      local.get $char_pos
      local.get $start_idx
      i32.eq
      if
        local.get $byte_pos
        local.set $byte_start
      end

      ;; if char_pos == end_idx → record byte_end and break
      local.get $char_pos
      local.get $end_idx
      i32.eq
      if
        local.get $byte_pos
        local.set $byte_end
        br 2
      end

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

  ;; new_len = byte_end - byte_start
  local.get $byte_end
  local.get $byte_start
  i32.sub
  local.set $new_len

  ;; alloc + memcpy
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
  local.get $byte_start
  i32.add
  local.get $new_len
  memory.copy

  local.get $ptr
)
(export "rt_str_slice" (func $rt_str_slice))

;; rt_str_chars: turn each UTF-8 code point into its own OBJ_STRING
;; (1..4 bytes), accumulate into a list, reverse for natural order.
(func $rt_str_chars (param $str i32) (result i32)
  (local $byte_len i32)
  (local $byte_pos i32)
  (local $rev_list i32)
  (local $lead i32)
  (local $width i32)
  (local $ptr i32)

  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $byte_len

  i32.const 0
  local.set $byte_pos
  i32.const 0
  local.set $rev_list

  block
    loop
      local.get $byte_pos
      local.get $byte_len
      i32.ge_u
      br_if 1

      local.get $str
      local.get $byte_pos
      i32.add
      i32.load8_u offset=8
      local.set $lead

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

      ;; rev_list = cons(ptr, rev_list)
      local.get $ptr
      i64.extend_i32_u
      local.get $rev_list
      i32.const 1
      call $rt_list_cons
      local.set $rev_list

      local.get $byte_pos
      local.get $width
      i32.add
      local.set $byte_pos
      br 0
    end
  end

  local.get $rev_list
  call $rt_list_reverse
)
(export "rt_str_chars" (func $rt_str_chars))

;; Helper: alloc OBJ_STRING with byte_len = `len`, then memory.copy
;; bytes from `src + 8 + start` for `len` bytes. Returns the new ptr.
(func $rt_str_copy_range (param $src i32) (param $start i32) (param $len i32) (result i32)
  (local $ptr i32)

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

  local.get $ptr
  i32.const 8
  i32.add
  local.get $src
  i32.const 8
  i32.add
  local.get $start
  i32.add
  local.get $len
  memory.copy

  local.get $ptr
)
(export "rt_str_copy_range" (func $rt_str_copy_range))

;; rt_str_split: returns a list of OBJ_STRINGs. Empty delim splits
;; per UTF-8 code point. Otherwise iterates rt_str_find calls and
;; collects the chunks before each match.
(func $rt_str_split (param $str i32) (param $delim i32) (result i32)
  (local $str_len i32)
  (local $delim_len i32)
  (local $search_pos i32)
  (local $part_start i32)
  (local $match_pos i32)
  (local $part_len i32)
  (local $rev_list i32)
  (local $ptr i32)
  (local $byte_pos i32)
  (local $lead i32)
  (local $width i32)

  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $str_len

  local.get $delim
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $delim_len

  ;; Empty delim: per-char split with leading + trailing empties (matches Rust impl).
  local.get $delim_len
  i32.eqz
  if
    i32.const 0
    local.set $rev_list

    ;; Push initial empty.
    local.get $str
    i32.const 0
    i32.const 0
    call $rt_str_copy_range
    i64.extend_i32_u
    local.get $rev_list
    i32.const 1
    call $rt_list_cons
    local.set $rev_list

    i32.const 0
    local.set $byte_pos
    block
      loop
        local.get $byte_pos
        local.get $str_len
        i32.ge_u
        br_if 1

        local.get $str
        local.get $byte_pos
        i32.add
        i32.load8_u offset=8
        local.set $lead

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

        ;; Push the next character as a 1-element string slice.
        local.get $str
        local.get $byte_pos
        local.get $width
        call $rt_str_copy_range
        i64.extend_i32_u
        local.get $rev_list
        i32.const 1
        call $rt_list_cons
        local.set $rev_list

        local.get $byte_pos
        local.get $width
        i32.add
        local.set $byte_pos
        br 0
      end
    end

    ;; Trailing empty.
    local.get $str
    i32.const 0
    i32.const 0
    call $rt_str_copy_range
    i64.extend_i32_u
    local.get $rev_list
    i32.const 1
    call $rt_list_cons
    local.set $rev_list

    local.get $rev_list
    call $rt_list_reverse
    return
  end

  ;; Non-empty delim: search loop.
  i32.const 0
  local.set $rev_list
  i32.const 0
  local.set $search_pos
  i32.const 0
  local.set $part_start

  block
    loop
      local.get $str
      local.get $delim
      local.get $search_pos
      call $rt_str_find
      local.set $match_pos

      local.get $match_pos
      i32.const -1
      i32.eq
      br_if 1

      ;; part_len = match_pos - part_start
      local.get $match_pos
      local.get $part_start
      i32.sub
      local.set $part_len

      local.get $str
      local.get $part_start
      local.get $part_len
      call $rt_str_copy_range
      i64.extend_i32_u
      local.get $rev_list
      i32.const 1
      call $rt_list_cons
      local.set $rev_list

      ;; advance search_pos and part_start past the match
      local.get $match_pos
      local.get $delim_len
      i32.add
      local.set $search_pos
      local.get $search_pos
      local.set $part_start
      br 0
    end
  end

  ;; final chunk = str[part_start..]
  local.get $str_len
  local.get $part_start
  i32.sub
  local.set $part_len

  local.get $str
  local.get $part_start
  local.get $part_len
  call $rt_str_copy_range
  i64.extend_i32_u
  local.get $rev_list
  i32.const 1
  call $rt_list_cons
  local.set $rev_list

  local.get $rev_list
  call $rt_list_reverse
)
(export "rt_str_split" (func $rt_str_split))

;; rt_str_join: walk the list once to compute total length, alloc the
;; buffer, walk again to copy each element + separator into place.
(func $rt_str_join (param $list i32) (param $sep i32) (result i32)
  (local $total i32)
  (local $cur i32)
  (local $first i32)
  (local $sep_len i32)
  (local $head i32)
  (local $head_len i32)
  (local $ptr i32)
  (local $write_pos i32)

  local.get $sep
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $sep_len

  ;; Pass 1: total length.
  i32.const 0
  local.set $total
  local.get $list
  local.set $cur
  i32.const 1
  local.set $first

  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      local.get $first
      i32.eqz
      if
        local.get $total
        local.get $sep_len
        i32.add
        local.set $total
      end
      i32.const 0
      local.set $first

      local.get $cur
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      local.set $head

      local.get $total
      local.get $head
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      i32.add
      local.set $total

      local.get $cur
      i32.const 1
      call $rt_obj_field_i32
      local.set $cur
      br 0
    end
  end

  ;; alloc 8 + align8(total)
  i32.const 8
  local.get $total
  i32.const 7
  i32.add
  i32.const -8
  i32.and
  i32.add
  call $rt_alloc
  local.set $ptr

  local.get $ptr
  i64.const 0
  local.get $total
  i64.extend_i32_u
  i64.or
  i64.store

  ;; Pass 2: copy.
  i32.const 0
  local.set $write_pos
  local.get $list
  local.set $cur
  i32.const 1
  local.set $first

  block
    loop
      local.get $cur
      i32.eqz
      br_if 1

      ;; If not first, copy separator first.
      local.get $first
      i32.eqz
      if
        local.get $ptr
        i32.const 8
        i32.add
        local.get $write_pos
        i32.add
        local.get $sep
        i32.const 8
        i32.add
        local.get $sep_len
        memory.copy

        local.get $write_pos
        local.get $sep_len
        i32.add
        local.set $write_pos
      end
      i32.const 0
      local.set $first

      local.get $cur
      i32.const 0
      call $rt_obj_field
      i32.wrap_i64
      local.set $head

      local.get $head
      i64.load
      i64.const 0xFFFFFFFF
      i64.and
      i32.wrap_i64
      local.set $head_len

      local.get $ptr
      i32.const 8
      i32.add
      local.get $write_pos
      i32.add
      local.get $head
      i32.const 8
      i32.add
      local.get $head_len
      memory.copy

      local.get $write_pos
      local.get $head_len
      i32.add
      local.set $write_pos

      local.get $cur
      i32.const 1
      call $rt_obj_field_i32
      local.set $cur
      br 0
    end
  end

  local.get $ptr
)
(export "rt_str_join" (func $rt_str_join))

(func $rt_str_replace (param $str i32) (param $old i32) (param $new i32) (result i32)
  local.get $str
  local.get $old
  call $rt_str_split
  local.get $new
  call $rt_str_join
)
(export "rt_str_replace" (func $rt_str_replace))
