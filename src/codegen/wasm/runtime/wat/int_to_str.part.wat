;; Numeric → string formatters that write into a caller-provided byte
;; buffer (typically the runtime's IO_INT_BUF / IO_FLOAT_BUF scratch
;; area). Both return the digits packed as `(pos << 16) | len`, where
;; `pos` is the offset within the buffer where the first digit lives
;; (digits are written right-to-left toward index 21). Caller copies
;; the [pos, pos+len) slice somewhere durable (e.g. into an
;; OBJ_STRING heap object).

(func $rt_int_to_str (param $val i64) (param $buf i32) (result i32)
  (local $n i64)
  (local $is_neg i32)
  (local $pos i32)

  local.get $val
  local.set $n

  ;; Special case: 0 → write '0' at buf[0], return (0<<16)|1
  local.get $n
  i64.eqz
  if (result i32)
    local.get $buf
    i32.const 0x30   ;; '0'
    i32.store8
    i32.const 1
  else
    ;; is_neg = n < 0; if so, negate.
    local.get $n
    i64.const 0
    i64.lt_s
    local.set $is_neg

    local.get $is_neg
    if
      i64.const 0
      local.get $n
      i64.sub
      local.set $n
    end

    ;; Write digits right-to-left starting at index 21.
    i32.const 21
    local.set $pos

    block
      loop
        local.get $n
        i64.eqz
        br_if 1

        local.get $pos
        i32.const 1
        i32.sub
        local.set $pos

        local.get $buf
        local.get $pos
        i32.add
        local.get $n
        i64.const 10
        i64.rem_u
        i32.wrap_i64
        i32.const 0x30
        i32.add
        i32.store8

        local.get $n
        i64.const 10
        i64.div_u
        local.set $n
        br 0
      end
    end

    ;; Negative sign one byte earlier.
    local.get $is_neg
    if
      local.get $pos
      i32.const 1
      i32.sub
      local.set $pos

      local.get $buf
      local.get $pos
      i32.add
      i32.const 0x2D   ;; '-'
      i32.store8
    end

    ;; (pos << 16) | (21 - pos)
    local.get $pos
    i32.const 16
    i32.shl
    i32.const 21
    local.get $pos
    i32.sub
    i32.or
  end
)
(export "rt_int_to_str" (func $rt_int_to_str))

;; Shortest-roundtrip f64 → string. Whole numbers print without a
;; decimal point. For fractional values we search N = 1..15 for the
;; smallest count of fractional digits where
;; trunc(|val| * 10^N) / 10^N == |val|, then write N digits and strip
;; trailing zeros.
(func $rt_float_to_str (param $val f64) (param $buf i32) (result i32)
  (local $is_neg i32)
  (local $abs_val f64)
  (local $int_part i64)
  (local $pos i32)
  (local $start_pos i32)
  (local $pow f64)
  (local $n i32)
  (local $scaled i64)
  (local $frac_int i64)
  (local $frac_pos i32)
  (local $frac_digits i32)

  local.get $val
  f64.const 0
  f64.lt
  local.set $is_neg

  local.get $val
  f64.abs
  local.set $abs_val

  local.get $abs_val
  f64.floor
  i64.trunc_f64_s
  local.set $int_part

  i32.const 21
  local.set $pos

  ;; Integer part right-to-left at buf[0..21].
  local.get $int_part
  i64.eqz
  if
    local.get $pos
    i32.const 1
    i32.sub
    local.set $pos

    local.get $buf
    local.get $pos
    i32.add
    i32.const 0x30
    i32.store8
  else
    block
      loop
        local.get $int_part
        i64.eqz
        br_if 1

        local.get $pos
        i32.const 1
        i32.sub
        local.set $pos

        local.get $buf
        local.get $pos
        i32.add
        local.get $int_part
        i64.const 10
        i64.rem_u
        i32.wrap_i64
        i32.const 0x30
        i32.add
        i32.store8

        local.get $int_part
        i64.const 10
        i64.div_u
        local.set $int_part
        br 0
      end
    end
  end

  ;; Negative sign.
  local.get $is_neg
  if
    local.get $pos
    i32.const 1
    i32.sub
    local.set $pos

    local.get $buf
    local.get $pos
    i32.add
    i32.const 0x2D
    i32.store8
  end

  local.get $pos
  local.set $start_pos

  ;; Whole number? abs_val == floor(abs_val) → no fractional digits.
  local.get $abs_val
  local.get $abs_val
  f64.floor
  f64.eq
  if (result i32)
    local.get $start_pos
    i32.const 16
    i32.shl
    i32.const 21
    local.get $start_pos
    i32.sub
    i32.or
  else
    ;; Find shortest N (1..15).
    f64.const 1
    local.set $pow
    i32.const 0
    local.set $n

    block
      loop
        ;; n++
        local.get $n
        i32.const 1
        i32.add
        local.set $n

        ;; pow *= 10
        local.get $pow
        f64.const 10
        f64.mul
        local.set $pow

        ;; scaled = trunc(abs_val * pow)
        local.get $abs_val
        local.get $pow
        f64.mul
        f64.floor
        i64.trunc_f64_s
        local.set $scaled

        ;; if (scaled / pow) == abs_val → done
        local.get $scaled
        f64.convert_i64_s
        local.get $pow
        f64.div
        local.get $abs_val
        f64.eq
        br_if 1

        ;; Cap at 15 digits.
        local.get $n
        i32.const 15
        i32.ge_s
        br_if 1

        br 0
      end
    end

    ;; frac_int = ((scaled % pow_i64) + pow_i64) % pow_i64
    local.get $scaled
    local.get $pow
    i64.trunc_f64_s
    i64.rem_s
    local.get $pow
    i64.trunc_f64_s
    i64.add
    local.get $pow
    i64.trunc_f64_s
    i64.rem_s
    local.set $frac_int

    ;; '.' at offset 21
    local.get $buf
    i32.const 0x2E
    i32.store8 offset=21

    ;; Write n frac digits right-to-left into buf[22..22+n].
    i32.const 22
    local.get $n
    i32.add
    i32.const 1
    i32.sub
    local.set $frac_pos

    local.get $n
    local.set $frac_digits

    block
      loop
        local.get $frac_digits
        i32.eqz
        br_if 1

        local.get $buf
        local.get $frac_pos
        i32.add
        local.get $frac_int
        i64.const 10
        i64.rem_u
        i32.wrap_i64
        i32.const 0x30
        i32.add
        i32.store8

        local.get $frac_int
        i64.const 10
        i64.div_u
        local.set $frac_int

        local.get $frac_pos
        i32.const 1
        i32.sub
        local.set $frac_pos

        local.get $frac_digits
        i32.const 1
        i32.sub
        local.set $frac_digits
        br 0
      end
    end

    ;; Strip trailing zeros: frac_end = 22 + n; while frac_end > 22 and
    ;; buf[frac_end-1] == '0', frac_end--.
    i32.const 22
    local.get $n
    i32.add
    local.set $frac_pos   ;; reused as frac_end

    block
      loop
        local.get $frac_pos
        i32.const 22
        i32.le_s
        br_if 1

        local.get $buf
        local.get $frac_pos
        i32.add
        i32.const 1
        i32.sub
        i32.load8_u
        i32.const 0x30
        i32.ne
        br_if 1

        local.get $frac_pos
        i32.const 1
        i32.sub
        local.set $frac_pos
        br 0
      end
    end

    ;; (start_pos << 16) | (frac_end - start_pos)
    local.get $start_pos
    i32.const 16
    i32.shl
    local.get $frac_pos
    local.get $start_pos
    i32.sub
    i32.or
  end
)
(export "rt_float_to_str" (func $rt_float_to_str))
