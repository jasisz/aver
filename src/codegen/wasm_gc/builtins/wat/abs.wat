
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result (ref null $aint))
            (local $m (ref null $mag))
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (result (ref null $aint)) (ref.is_null (local.get $m))
              (then
                ;; Small. Non-negative → unchanged. i64::MIN → +2^63 (Big,
                ;; two 32-bit limbs low=0 high=0x80000000). Other negatives →
                ;; the negated value still fits i64 (stays Small).
                (if (result (ref null $aint)) (i64.ge_s (struct.get $aint $small (local.get $a)) (i64.const 0))
                  (then (local.get $a))
                  (else
                    (if (result (ref null $aint))
                        (i64.eq (struct.get $aint $small (local.get $a)) (i64.const 0x8000000000000000))
                      (then
                        (struct.new $aint (i64.const 0)
                          (array.new_fixed $mag 2 (i64.const 0) (i64.const 0x80000000))
                          (i32.const 1)))
                      (else
                        (struct.new $aint
                          (i64.sub (i64.const 0) (struct.get $aint $small (local.get $a)))
                          (ref.null $mag) (i32.const 0)))))))
              (else
                ;; Big. |a| keeps the magnitude with sign +1 (a Big is never
                ;; in i64 range, so its magnitude stays a canonical Big — no
                ;; demote needed; the only Big that could demote under negate
                ;; is +2^63, and that is already positive here).
                (struct.new $aint (i64.const 0) (local.get $m) (i32.const 1))))))

