        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i64)
            (local $m (ref null $mag)) (local $n i32) (local $lo i64)
            ;; The lowest 64 bits of `a` under infinite two's complement, what
            ;; `Bits.low(a, 64)` keeps (`Float.fromBits`). A Small already is
            ;; its own word. A Big takes the two low limbs of its magnitude
            ;; and negates them modulo 2^64 when the value is negative.
            (if (result i64) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then (struct.get $aint $small (local.get $a)))
              (else
                (local.set $m (struct.get $aint $magf (local.get $a)))
                (local.set $n (array.len (local.get $m)))
                (if (i32.gt_u (local.get $n) (i32.const 0))
                  (then (local.set $lo
                    (i64.and (array.get $mag (local.get $m) (i32.const 0)) (i64.const 0xffffffff)))))
                (if (i32.gt_u (local.get $n) (i32.const 1))
                  (then (local.set $lo (i64.or (local.get $lo)
                    (i64.shl (array.get $mag (local.get $m) (i32.const 1)) (i64.const 32))))))
                (if (result i64) (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0))
                  (then (i64.sub (i64.const 0) (local.get $lo)))
                  (else (local.get $lo)))))))
