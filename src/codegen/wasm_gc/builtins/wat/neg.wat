
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result (ref null $aint))
            (local $m (ref null $mag))
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (result (ref null $aint)) (ref.is_null (local.get $m))
              (then
                (if (result (ref null $aint))
                    (i64.eq (struct.get $aint $small (local.get $a)) (i64.const 0x8000000000000000))
                  (then
                    ;; -(i64::MIN) = 2^63 → Big, positive, two 32-bit limbs
                    ;; (low=0, high=0x80000000).
                    (struct.new $aint (i64.const 0)
                      (array.new_fixed $mag 2 (i64.const 0) (i64.const 0x80000000))
                      (i32.const 1)))
                  (else
                    (struct.new $aint
                      (i64.sub (i64.const 0) (struct.get $aint $small (local.get $a)))
                      (ref.null $mag) (i32.const 0)))))
              (else
                ;; Big. Negation keeps the magnitude and flips the sign, and the
                ;; result stays a canonical Big EXCEPT for +2^63 (the smallest
                ;; Big, = i64::MAX + 1): -(2^63) = i64::MIN re-enters i64 range
                ;; and MUST demote to Small, or `eq` would report it unequal to
                ;; the same value reached as a Small. +2^63's canonical limbs
                ;; are [0, 0x80000000] (len 2); every other Big negation is
                ;; still a canonical Big, so a plain sign flip is correct there.
                (if (result (ref null $aint))
                    (i32.and
                      (i32.eq (struct.get $aint $sign (local.get $a)) (i32.const 1))
                      (i32.and (i32.eq (array.len (local.get $m)) (i32.const 2))
                        (i32.and
                          (i64.eqz (array.get $mag (local.get $m) (i32.const 0)))
                          (i64.eq (array.get $mag (local.get $m) (i32.const 1)) (i64.const 0x80000000)))))
                  (then
                    (struct.new $aint (i64.const 0x8000000000000000) (ref.null $mag) (i32.const 0)))
                  (else
                    (struct.new $aint (i64.const 0)
                      (local.get $m)
                      (i32.sub (i32.const 0) (struct.get $aint $sign (local.get $a))))))))))
    