
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result f64)
            (local $m (ref null $mag)) (local $len i32) (local $i i32)
            (local $acc f64) (local $limb f64)
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (result f64) (ref.is_null (local.get $m))
              (then
                ;; Small — exact i64→f64 (round-to-nearest, as the VM's `n as f64`).
                (f64.convert_i64_s (struct.get $aint $small (local.get $a))))
              (else
                ;; Big — Horner over limbs high→low: acc = acc*2^32 + limb.
                ;; Magnitudes beyond f64 range overflow to +inf naturally
                ;; (IEEE), matching `BigInt::to_f64`'s ±inf saturation. The
                ;; top 53 significant bits drive the mantissa; lower limbs add
                ;; below the ULP and round to nearest. Sign applied last.
                (local.set $len (array.len (local.get $m)))
                (local.set $acc (f64.const 0))
                (local.set $i (local.get $len))
                (block $done (loop $lp
                  (br_if $done (i32.eqz (local.get $i)))
                  (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                  ;; limb is an unsigned 32-bit value held in an i64 lane.
                  (local.set $limb (f64.convert_i64_u
                    (i64.and (array.get $mag (local.get $m) (local.get $i)) (i64.const 0xffffffff))))
                  ;; acc = acc * 2^32 + limb
                  (local.set $acc (f64.add
                    (f64.mul (local.get $acc) (f64.const 4294967296))
                    (local.get $limb)))
                  (br $lp)))
                (if (result f64) (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0))
                  (then (f64.neg (local.get $acc)))
                  (else (local.get $acc)))))))
