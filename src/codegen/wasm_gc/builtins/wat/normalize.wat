
        (module
          {decls}
          (func (export "helper") (param $rm (ref null $mag)) (param $rs i32) (result (ref null $aint))
            (local $rlen i32) (local $i i32) (local $lo i64) (local $hi i64) (local $tmpm (ref null $mag))
            ;; strip leading zero limbs
            (local.set $rlen (array.len (local.get $rm)))
            (block $strip_done (loop $strip
              (br_if $strip_done (i32.eqz (local.get $rlen)))
              (br_if $strip_done
                (i64.ne (array.get $mag (local.get $rm) (i32.sub (local.get $rlen) (i32.const 1))) (i64.const 0)))
              (local.set $rlen (i32.sub (local.get $rlen) (i32.const 1)))
              (br $strip)))
            (if (result (ref null $aint)) (i32.eqz (local.get $rlen))
              (then
                (struct.new $aint (i64.const 0) (ref.null $mag) (i32.const 0)))
              (else
                (if (result (ref null $aint)) (i32.le_u (local.get $rlen) (i32.const 2))
                  (then
                    ;; reassemble <=2 limbs into a 64-bit unsigned value
                    (local.set $lo (i64.and (array.get $mag (local.get $rm) (i32.const 0)) (i64.const 0xffffffff)))
                    (local.set $hi
                      (if (result i64) (i32.eq (local.get $rlen) (i32.const 2))
                        (then (i64.and (array.get $mag (local.get $rm) (i32.const 1)) (i64.const 0xffffffff)))
                        (else (i64.const 0))))
                    (local.set $lo (i64.or (local.get $lo) (i64.shl (local.get $hi) (i64.const 32))))
                    ;; $lo now holds the full magnitude as an unsigned i64.
                    (if (result (ref null $aint))
                        (i64.eqz (i64.shr_u (local.get $lo) (i64.const 63)))
                      (then
                        ;; top bit clear -> fits i64::MAX either sign -> Small
                        (struct.new $aint
                          (if (result i64) (i32.lt_s (local.get $rs) (i32.const 0))
                            (then (i64.sub (i64.const 0) (local.get $lo))) (else (local.get $lo)))
                          (ref.null $mag) (i32.const 0)))
                      (else
                        ;; top bit set: only -2^63 (i64::MIN) demotes.
                        (if (result (ref null $aint))
                            (i32.and (i32.lt_s (local.get $rs) (i32.const 0))
                                     (i64.eq (local.get $lo) (i64.const 0x8000000000000000)))
                          (then
                            (struct.new $aint (i64.const 0x8000000000000000) (ref.null $mag) (i32.const 0)))
                          (else
                            {tight_big})))))
                  (else
                    {tight_big}))))))

