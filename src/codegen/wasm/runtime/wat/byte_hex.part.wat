;; 8-bit byte ↔ two-character hex string. Both return Result<…,…>:
;; rt_byte_to_hex returns Result<String, String> (Err on out-of-range);
;; rt_byte_from_hex returns Result<Int, String> (Err on bad input).
;; The error path always carries an empty OBJ_STRING.

(func $rt_byte_to_hex (param $n i64) (result i32)
  (local $byte i32)
  (local $ptr i32)
  (local $nibble i32)

  ;; Range check: n < 0 || n > 255 → Err("")
  local.get $n
  i64.const 0
  i64.lt_s
  local.get $n
  i64.const 255
  i64.gt_s
  i32.or
  if
    ;; alloc empty OBJ_STRING (8 bytes header, no payload)
    i32.const 8
    call $rt_alloc
    local.set $ptr
    local.get $ptr
    i64.const 0   ;; OBJ_STRING (0) << 56 | byte_len 0
    i64.store

    i32.const 1   ;; WRAP_ERR
    local.get $ptr
    i32.const 1
    call $rt_wrap_i32
    return
  end

  local.get $n
  i32.wrap_i64
  local.set $byte

  ;; alloc 2-byte OBJ_STRING (8 + align8(2) = 16 bytes)
  i32.const 16
  call $rt_alloc
  local.set $ptr
  local.get $ptr
  i64.const 2   ;; OBJ_STRING (0) << 56 | byte_len 2
  i64.store

  ;; high nibble at offset 8
  local.get $byte
  i32.const 4
  i32.shr_u
  i32.const 0xF
  i32.and
  local.set $nibble
  local.get $ptr
  local.get $nibble
  i32.const 10
  i32.lt_u
  if (result i32)
    local.get $nibble
    i32.const 0x30   ;; '0'
    i32.add
  else
    local.get $nibble
    i32.const 10
    i32.sub
    i32.const 0x61   ;; 'a'
    i32.add
  end
  i32.store8 offset=8

  ;; low nibble at offset 9
  local.get $byte
  i32.const 0xF
  i32.and
  local.set $nibble
  local.get $ptr
  local.get $nibble
  i32.const 10
  i32.lt_u
  if (result i32)
    local.get $nibble
    i32.const 0x30
    i32.add
  else
    local.get $nibble
    i32.const 10
    i32.sub
    i32.const 0x61
    i32.add
  end
  i32.store8 offset=9

  i32.const 0   ;; WRAP_OK
  local.get $ptr
  i32.const 1
  call $rt_wrap_i32
)
(export "rt_byte_to_hex" (func $rt_byte_to_hex))

(func $rt_byte_from_hex (param $str i32) (result i32)
  (local $len i32)
  (local $hi i32)
  (local $lo i32)
  (local $ch i32)
  (local $value i32)
  (local $err_ptr i32)

  ;; len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  ;; len != 2 → Err("")
  local.get $len
  i32.const 2
  i32.ne
  if
    i32.const 8
    call $rt_alloc
    local.set $err_ptr
    local.get $err_ptr
    i64.const 0
    i64.store
    i32.const 1
    local.get $err_ptr
    i32.const 1
    call $rt_wrap_i32
    return
  end

  ;; hi nibble
  local.get $str
  i32.load8_u offset=8
  local.set $ch

  local.get $ch
  i32.const 0x30   ;; '0'
  i32.ge_u
  local.get $ch
  i32.const 0x39   ;; '9'
  i32.le_u
  i32.and
  if
    local.get $ch
    i32.const 0x30
    i32.sub
    local.set $hi
  else
    local.get $ch
    i32.const 0x61   ;; 'a'
    i32.ge_u
    local.get $ch
    i32.const 0x66   ;; 'f'
    i32.le_u
    i32.and
    if
      local.get $ch
      i32.const 0x61
      i32.sub
      i32.const 10
      i32.add
      local.set $hi
    else
      local.get $ch
      i32.const 0x41   ;; 'A'
      i32.ge_u
      local.get $ch
      i32.const 0x46   ;; 'F'
      i32.le_u
      i32.and
      if
        local.get $ch
        i32.const 0x41
        i32.sub
        i32.const 10
        i32.add
        local.set $hi
      else
        ;; bad nibble → Err("")
        i32.const 8
        call $rt_alloc
        local.set $err_ptr
        local.get $err_ptr
        i64.const 0
        i64.store
        i32.const 1
        local.get $err_ptr
        i32.const 1
        call $rt_wrap_i32
        return
      end
    end
  end

  ;; lo nibble
  local.get $str
  i32.load8_u offset=9
  local.set $ch

  local.get $ch
  i32.const 0x30
  i32.ge_u
  local.get $ch
  i32.const 0x39
  i32.le_u
  i32.and
  if
    local.get $ch
    i32.const 0x30
    i32.sub
    local.set $lo
  else
    local.get $ch
    i32.const 0x61
    i32.ge_u
    local.get $ch
    i32.const 0x66
    i32.le_u
    i32.and
    if
      local.get $ch
      i32.const 0x61
      i32.sub
      i32.const 10
      i32.add
      local.set $lo
    else
      local.get $ch
      i32.const 0x41
      i32.ge_u
      local.get $ch
      i32.const 0x46
      i32.le_u
      i32.and
      if
        local.get $ch
        i32.const 0x41
        i32.sub
        i32.const 10
        i32.add
        local.set $lo
      else
        i32.const 8
        call $rt_alloc
        local.set $err_ptr
        local.get $err_ptr
        i64.const 0
        i64.store
        i32.const 1
        local.get $err_ptr
        i32.const 1
        call $rt_wrap_i32
        return
      end
    end
  end

  ;; value = (hi << 4) | lo
  local.get $hi
  i32.const 4
  i32.shl
  local.get $lo
  i32.or
  local.set $value

  ;; Wrap as Ok(value as i64), value_ptr_flag = 0
  i32.const 0   ;; WRAP_OK
  local.get $value
  i64.extend_i32_u
  i32.const 0
  call $rt_wrap
)
(export "rt_byte_from_hex" (func $rt_byte_from_hex))
