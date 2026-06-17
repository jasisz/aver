
        (module
          {decls}
          {func_pad}
          (func (export "helper") (param $f f64) (result (ref null $aint))
            (local $t f64) (local $bits i64)
            (local $exp i64) (local $mant i64)
            (local $word i32) (local $bit i64)
            (local $low64 i64) (local $high i64)
            (local $rm (ref null $mag)) (local $mlen i32)
            ;; normalize epilogue scratch
            (local $rs i32) (local $rlen i32) (local $i i32) (local $lo i64) (local $hi i64)
            (local $tmpm (ref null $mag))

            ;; Non-finite (NaN / ±inf) → 0, matching `from_f64_trunc`.
            (if (result (ref null $aint))
                (i32.eqz (f64.eq (local.get $f) (local.get $f)))
              (then (struct.new $aint (i64.const 0) (ref.null $mag) (i32.const 0)))
              (else
                ;; abs(f) == +inf ?  (NaN already handled above)
                (if (result (ref null $aint))
                    (f64.eq (f64.abs (local.get $f)) (f64.const inf))
                  (then (struct.new $aint (i64.const 0) (ref.null $mag) (i32.const 0)))
                  (else
                    ;; truncate toward zero.
                    (local.set $t (f64.trunc (local.get $f)))
                    ;; In i64 range?  -2^63 <= t < 2^63  → Small via trunc_s
                    ;; (won't trap given the bounds check).
                    (if (result (ref null $aint))
                        (i32.and
                          (f64.ge (local.get $t) (f64.const -9223372036854775808))
                          (f64.lt (local.get $t) (f64.const 9223372036854775808)))
                      (then
                        (struct.new $aint (i64.trunc_f64_s (local.get $t)) (ref.null $mag) (i32.const 0)))
                      (else
                        ;; Out of i64 range but finite — build the EXACT Big.
                        ;; Decompose |t| = mantissa * 2^exp (IEEE-754 binary64):
                        ;;   bits  = reinterpret(|t|)
                        ;;   mant  = (bits & 2^52-1) | 2^52   (53-bit, normal)
                        ;;   exp   = ((bits>>52)&0x7ff) - 1075
                        ;; |t| >= 2^63 here so exp >= 11 (>0): a pure left shift.
                        (local.set $bits (i64.reinterpret_f64 (f64.abs (local.get $t))))
                        (local.set $mant (i64.or
                          (i64.and (local.get $bits) (i64.const 0xfffffffffffff))
                          (i64.const 0x10000000000000)))
                        (local.set $exp (i64.sub
                          (i64.and (i64.shr_u (local.get $bits) (i64.const 52)) (i64.const 0x7ff))
                          (i64.const 1075)))
                        ;; word = exp / 32, bit = exp % 32
                        (local.set $word (i32.wrap_i64 (i64.div_u (local.get $exp) (i64.const 32))))
                        (local.set $bit (i64.rem_u (local.get $exp) (i64.const 32)))
                        ;; shifted = mant << bit, spanning up to 3 limbs from $word.
                        ;;   low64 = (mant << bit) mod 2^64
                        ;;   high  = mant >> (64 - bit)   (the bits past bit 63),
                        ;;           or 0 when bit == 0.
                        (local.set $low64 (i64.shl (local.get $mant) (local.get $bit)))
                        (local.set $high (if (result i64) (i64.eqz (local.get $bit))
                          (then (i64.const 0))
                          (else (i64.shr_u (local.get $mant) (i64.sub (i64.const 64) (local.get $bit))))))
                        ;; magnitude: $word + 3 limbs (the shifted mantissa) is
                        ;; enough; normalize strips the trailing zeros.
                        (local.set $mlen (i32.add (local.get $word) (i32.const 3)))
                        (local.set $rm (array.new_default $mag (local.get $mlen)))
                        (array.set $mag (local.get $rm) (local.get $word)
                          (i64.and (local.get $low64) (i64.const 0xffffffff)))
                        (array.set $mag (local.get $rm) (i32.add (local.get $word) (i32.const 1))
                          (i64.and (i64.shr_u (local.get $low64) (i64.const 32)) (i64.const 0xffffffff)))
                        (array.set $mag (local.get $rm) (i32.add (local.get $word) (i32.const 2))
                          (i64.and (local.get $high) (i64.const 0xffffffff)))
                        ;; sign from the ORIGINAL float (negative → -1).
                        (local.set $rs (if (result i32) (f64.lt (local.get $f) (f64.const 0)) (then (i32.const -1)) (else (i32.const 1))))
                        {norm}))))))))
