;; Numeric parsing — both return Result<numeric, String>:
;;   rt_int_from_str   — Result<Int, String>; never errors today
;;                        (out-of-spec input silently parses as 0).
;;   rt_float_from_str — Result<Float, String>; rejects non-digits
;;                        (other than `-`, `.`, `e`/`E`, `+`/`-`),
;;                        decimal-point twice, dangling exponent
;;                        sign, empty/lone-`-` input. Failure case
;;                        wraps the original input str as the error.

(func $rt_int_from_str (param $str i32) (result i32)
  (local $len i32)
  (local $value i64)
  (local $idx i32)
  (local $negative i32)

  ;; len = header.byte_len
  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  ;; Optional leading '-'
  local.get $len
  i32.const 0
  i32.gt_s
  if
    local.get $str
    i32.load8_u offset=8
    i32.const 0x2D   ;; '-'
    i32.eq
    if
      i32.const 1
      local.set $negative
      i32.const 1
      local.set $idx
    end
  end

  block
    loop
      local.get $idx
      local.get $len
      i32.ge_u
      br_if 1

      local.get $value
      i64.const 10
      i64.mul
      local.get $str
      local.get $idx
      i32.add
      i32.load8_u offset=8
      i32.const 0x30   ;; '0'
      i32.sub
      i64.extend_i32_u
      i64.add
      local.set $value

      local.get $idx
      i32.const 1
      i32.add
      local.set $idx
      br 0
    end
  end

  local.get $negative
  if
    i64.const 0
    local.get $value
    i64.sub
    local.set $value
  end

  i32.const 0   ;; WRAP_OK
  local.get $value
  i32.const 0
  call $rt_wrap
)
(export "rt_int_from_str" (func $rt_int_from_str))

(func $rt_float_from_str (param $str i32) (result i32)
  (local $len i32)
  (local $idx i32)
  (local $negative i32)
  (local $seen_dot i32)
  (local $saw_digit i32)
  (local $ch i32)
  (local $digit i32)
  (local $exp_state i32)        ;; 0=none, 1=just-saw-e, 2=after-sign-or-digit
  (local $exp_negative i32)
  (local $saw_exp_digit i32)
  (local $exp_value i32)
  (local $value f64)
  (local $frac_div f64)

  local.get $str
  i64.load
  i64.const 0xFFFFFFFF
  i64.and
  i32.wrap_i64
  local.set $len

  f64.const 0
  local.set $value
  f64.const 1
  local.set $frac_div

  ;; Leading '-'
  local.get $len
  i32.const 0
  i32.gt_s
  if
    local.get $str
    i32.load8_u offset=8
    i32.const 0x2D   ;; '-'
    i32.eq
    if
      i32.const 1
      local.set $negative
      i32.const 1
      local.set $idx
    end
  end

  block
    loop
      local.get $idx
      local.get $len
      i32.ge_u
      br_if 1

      local.get $str
      local.get $idx
      i32.add
      i32.load8_u offset=8
      local.set $ch

      ;; Exponent sign right after e/E.
      local.get $exp_state
      i32.const 1
      i32.eq
      local.get $ch
      i32.const 0x2B   ;; '+'
      i32.eq
      i32.and
      if
        i32.const 2
        local.set $exp_state
        local.get $idx
        i32.const 1
        i32.add
        local.set $idx
        br 1
      end

      local.get $exp_state
      i32.const 1
      i32.eq
      local.get $ch
      i32.const 0x2D   ;; '-'
      i32.eq
      i32.and
      if
        i32.const 1
        local.set $exp_negative
        i32.const 2
        local.set $exp_state
        local.get $idx
        i32.const 1
        i32.add
        local.set $idx
        br 1
      end

      ;; Inside exponent digits.
      local.get $exp_state
      if
        local.get $ch
        i32.const 0x30   ;; '0'
        i32.lt_u
        if
          i32.const 1   ;; WRAP_ERR
          local.get $str
          i32.const 1
          call $rt_wrap_i32
          return
        end
        local.get $ch
        i32.const 0x39   ;; '9'
        i32.gt_u
        if
          i32.const 1
          local.get $str
          i32.const 1
          call $rt_wrap_i32
          return
        end

        i32.const 1
        local.set $saw_exp_digit
        i32.const 2
        local.set $exp_state

        local.get $ch
        i32.const 0x30
        i32.sub
        local.set $digit

        local.get $exp_value
        i32.const 10
        i32.mul
        local.get $digit
        i32.add
        local.set $exp_value

        local.get $idx
        i32.const 1
        i32.add
        local.set $idx
        br 1
      end

      ;; Decimal point.
      local.get $ch
      i32.const 0x2E   ;; '.'
      i32.eq
      if
        local.get $seen_dot
        if
          i32.const 1
          local.get $str
          i32.const 1
          call $rt_wrap_i32
          return
        end
        i32.const 1
        local.set $seen_dot
        local.get $idx
        i32.const 1
        i32.add
        local.set $idx
        br 1
      end

      ;; Exponent marker.
      local.get $ch
      i32.const 0x65   ;; 'e'
      i32.eq
      local.get $ch
      i32.const 0x45   ;; 'E'
      i32.eq
      i32.or
      if
        local.get $saw_digit
        i32.eqz
        if
          i32.const 1
          local.get $str
          i32.const 1
          call $rt_wrap_i32
          return
        end
        i32.const 1
        local.set $exp_state
        local.get $idx
        i32.const 1
        i32.add
        local.set $idx
        br 1
      end

      ;; Mantissa digit.
      local.get $ch
      i32.const 0x30
      i32.lt_u
      if
        i32.const 1
        local.get $str
        i32.const 1
        call $rt_wrap_i32
        return
      end
      local.get $ch
      i32.const 0x39
      i32.gt_u
      if
        i32.const 1
        local.get $str
        i32.const 1
        call $rt_wrap_i32
        return
      end

      i32.const 1
      local.set $saw_digit

      local.get $ch
      i32.const 0x30
      i32.sub
      local.set $digit

      local.get $seen_dot
      if
        local.get $frac_div
        f64.const 10
        f64.mul
        local.set $frac_div
        local.get $value
        local.get $digit
        f64.convert_i32_u
        local.get $frac_div
        f64.div
        f64.add
        local.set $value
      else
        local.get $value
        f64.const 10
        f64.mul
        local.get $digit
        f64.convert_i32_u
        f64.add
        local.set $value
      end

      local.get $idx
      i32.const 1
      i32.add
      local.set $idx
      br 0
    end
  end

  ;; Reject empty / lone '-'.
  local.get $saw_digit
  i32.eqz
  if
    i32.const 1
    local.get $str
    i32.const 1
    call $rt_wrap_i32
    return
  end

  ;; Reject dangling exponent marker (state == 1).
  local.get $exp_state
  i32.const 1
  i32.eq
  if
    i32.const 1
    local.get $str
    i32.const 1
    call $rt_wrap_i32
    return
  end

  ;; Reject exponent sign with no digits (state == 2 && !saw_exp_digit).
  local.get $exp_state
  i32.const 2
  i32.eq
  local.get $saw_exp_digit
  i32.eqz
  i32.and
  if
    i32.const 1
    local.get $str
    i32.const 1
    call $rt_wrap_i32
    return
  end

  ;; Apply exponent.
  block
    loop
      local.get $exp_value
      i32.eqz
      br_if 1

      local.get $exp_negative
      if
        local.get $value
        f64.const 10
        f64.div
        local.set $value
      else
        local.get $value
        f64.const 10
        f64.mul
        local.set $value
      end

      local.get $exp_value
      i32.const 1
      i32.sub
      local.set $exp_value
      br 0
    end
  end

  ;; Apply sign.
  local.get $negative
  if
    f64.const 0
    local.get $value
    f64.sub
    local.set $value
  end

  ;; Result.Ok(value)
  i32.const 0   ;; WRAP_OK
  local.get $value
  call $rt_wrap_f64
)
(export "rt_float_from_str" (func $rt_float_from_str))
