
        (module
          {decls}
          (func (export "helper") (param $s (ref null $string)) (result (ref null $result))
            (local $len i32) (local $idx i32) (local $start i32)
            (local $negative i32) (local $saw_digit i32)
            (local $ch i32) (local $digit i64)
            (local $rm (ref null $mag)) (local $mlen i32)
            (local $i i32) (local $carry i64) (local $cur i64)
            (local $err (ref null $string))
            ;; normalize epilogue scratch
            (local $rs i32) (local $rlen i32) (local $lo i64) (local $hi i64)
            (local $tmpm (ref null $mag))

            (local.set $len (array.len (local.get $s)))

            ;; Empty input → Err.
            (if (i32.eqz (local.get $len))
              (then
                {err_build}
                return))

            ;; Optional single leading '+' / '-'.
            (local.set $start (i32.const 0))
            (local.set $ch (array.get_u $string (local.get $s) (i32.const 0)))
            (if (i32.eq (local.get $ch) (i32.const 0x2D))
              (then
                (local.set $negative (i32.const 1))
                (local.set $start (i32.const 1)))
              (else
                (if (i32.eq (local.get $ch) (i32.const 0x2B))
                  (then (local.set $start (i32.const 1))))))

            ;; Magnitude accumulator: ceil(len/8)+2 limbs holds any decimal
            ;; (each 32-bit limb absorbs >9 decimal digits; len/8+2 is a
            ;; safe over-estimate). 32-bit-in-i64 limbs keep mul*10+digit
            ;; exact (max limb value < 2^32, so limb*10+9 < 2^36 < 2^64).
            (local.set $mlen (i32.add (i32.div_u (local.get $len) (i32.const 8)) (i32.const 2)))
            (local.set $rm (array.new_default $mag (local.get $mlen)))

            (local.set $idx (local.get $start))
            (block $loop_done (loop $loop
              (br_if $loop_done (i32.ge_u (local.get $idx) (local.get $len)))
              (local.set $ch (array.get_u $string (local.get $s) (local.get $idx)))
              ;; Reject any non-digit byte → Err.
              (if (i32.or (i32.lt_u (local.get $ch) (i32.const 0x30))
                          (i32.gt_u (local.get $ch) (i32.const 0x39)))
                (then
                  {err_build}
                  return))
              (local.set $saw_digit (i32.const 1))
              (local.set $digit (i64.extend_i32_u (i32.sub (local.get $ch) (i32.const 0x30))))
              ;; rm = rm*10 + digit  (little-endian limb multiply-add)
              (local.set $carry (local.get $digit))
              (local.set $i (i32.const 0))
              (block $ma_done (loop $ma
                (br_if $ma_done (i32.ge_u (local.get $i) (local.get $mlen)))
                (local.set $cur (i64.add
                  (i64.mul (array.get $mag (local.get $rm) (local.get $i)) (i64.const 10))
                  (local.get $carry)))
                (array.set $mag (local.get $rm) (local.get $i) (i64.and (local.get $cur) (i64.const 0xffffffff)))
                (local.set $carry (i64.shr_u (local.get $cur) (i64.const 32)))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $ma)))
              (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
              (br $loop)))

            ;; No digit consumed (empty, lone '+'/'-') → Err.
            (if (i32.eqz (local.get $saw_digit))
              (then
                {err_build}
                return))

            ;; Sign for normalize: -1 if negative else +1 (normalize maps a
            ;; zero magnitude to canonical Small(0) regardless of sign).
            (local.set $rs (if (result i32) (local.get $negative) (then (i32.const -1)) (else (i32.const 1))))

            ;; tag 1 (Ok), ok = normalized $aint, null err.
            (i32.const 1)
            {norm}
            (ref.null $string)
            (struct.new $result)))
