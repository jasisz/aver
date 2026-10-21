        (module
          {decls}
          (func (export "helper") (param $x (ref null $aint)) (param $w i64) (result i64)
            (local $y i64) (local $m (ref null $mag)) (local $n i32) (local $i i32)
            (local $limb i64) (local $borrow i64) (local $acc i64)

            ;; The one bits among the lowest `w` bits of `x` when `x >= 0`, and
            ;; of `-x - 1` when `x < 0`: the bits that differ from the sign
            ;; tail. The caller turns the second into `w - count`. `w` is the
            ;; saturated non-negative width, so a Big width reaches every limb.
            (if (result i64) (ref.is_null (struct.get $aint $magf (local.get $x)))
              (then
                (local.set $y (struct.get $aint $small (local.get $x)))
                (if (i64.lt_s (local.get $y) (i64.const 0))
                  (then (local.set $y (i64.xor (local.get $y) (i64.const -1)))))
                (if (result i64) (i64.ge_u (local.get $w) (i64.const 64))
                  (then (i64.popcnt (local.get $y)))
                  (else
                    (i64.popcnt (i64.and (local.get $y)
                      (i64.sub (i64.shl (i64.const 1) (local.get $w)) (i64.const 1)))))))
              (else
                ;; Big: walk the 32-bit limbs of the magnitude. For a negative
                ;; value `-x - 1` is the magnitude minus one, so a borrow runs
                ;; through the low zero limbs, turning each into all ones.
                (local.set $m (struct.get $aint $magf (local.get $x)))
                (local.set $n (array.len (local.get $m)))
                (local.set $borrow
                  (i64.extend_i32_u (i32.lt_s (struct.get $aint $sign (local.get $x)) (i32.const 0))))
                (block $done
                  (loop $next
                    (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                    (br_if $done (i64.eqz (local.get $w)))
                    (local.set $limb
                      (i64.sub (array.get $mag (local.get $m) (local.get $i)) (local.get $borrow)))
                    (if (i64.lt_s (local.get $limb) (i64.const 0))
                      (then (local.set $limb (i64.const 0xFFFFFFFF)))
                      (else (local.set $borrow (i64.const 0))))
                    (if (i64.lt_u (local.get $w) (i64.const 32))
                      (then
                        (local.set $limb (i64.and (local.get $limb)
                          (i64.sub (i64.shl (i64.const 1) (local.get $w)) (i64.const 1))))
                        (local.set $w (i64.const 0)))
                      (else (local.set $w (i64.sub (local.get $w) (i64.const 32)))))
                    (local.set $acc (i64.add (local.get $acc) (i64.popcnt (local.get $limb))))
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (br $next)))
                (local.get $acc)))))
