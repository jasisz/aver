        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i64)
            (local $m (ref null $mag)) (local $limbs i32) (local $at i32)
            (local $small i64) (local $neg i64) (local $n i64) (local $count i64)
            (local $k i64) (local $i i64) (local $d i64) (local $t i64) (local $h i64)

            ;; `Int.sampleHash`, as `aver_rt::AverInt::sample_hash`: the sign,
            ;; the count `n` of base-2^64 digits of the magnitude, and at most
            ;; 64 of those digits at their positions, mixed by the splitmix64
            ;; finalizer. A Small is its own one digit (`|i64::MIN|` is the
            ;; word 2^63); a Big pairs its 32-bit limbs into 64-bit digits.
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (ref.is_null (local.get $m))
              (then
                (local.set $small (struct.get $aint $small (local.get $a)))
                (local.set $neg (i64.extend_i32_u (i64.lt_s (local.get $small) (i64.const 0))))
                (if (i64.lt_s (local.get $small) (i64.const 0))
                  (then (local.set $small (i64.sub (i64.const 0) (local.get $small)))))
                (local.set $n (i64.extend_i32_u (i64.ne (local.get $small) (i64.const 0)))))
              (else
                (local.set $neg (i64.extend_i32_u
                  (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0))))
                ;; The digit count ignores zero limbs on top, if any.
                (local.set $limbs (array.len (local.get $m)))
                (block $top
                  (loop $trim
                    (br_if $top (i32.eqz (local.get $limbs)))
                    (br_if $top (i64.ne
                      (i64.and (array.get $mag (local.get $m) (i32.sub (local.get $limbs) (i32.const 1)))
                        (i64.const 0xffffffff))
                      (i64.const 0)))
                    (local.set $limbs (i32.sub (local.get $limbs) (i32.const 1)))
                    (br $trim)))
                (local.set $n (i64.shr_u (i64.add (i64.extend_i32_u (local.get $limbs)) (i64.const 1))
                  (i64.const 1)))))

            ;; h = mix(K0 ^ n ^ (s << 63))
            (local.set $h (i64.xor (i64.xor (i64.const 0x9e3779b97f4a7c15) (local.get $n))
              (i64.shl (local.get $neg) (i64.const 63))))
            (local.set $h (i64.mul (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 30)))
              (i64.const 0xbf58476d1ce4e5b9)))
            (local.set $h (i64.mul (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 27)))
              (i64.const 0x94d049bb133111eb)))
            (local.set $h (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 31))))

            ;; Every position when n <= 64, else floor(k * (n - 1) / 63).
            (local.set $count (select (local.get $n) (i64.const 64)
              (i64.le_u (local.get $n) (i64.const 64))))
            (block $done
              (loop $next
                (br_if $done (i64.ge_u (local.get $k) (local.get $count)))
                (local.set $i (select (local.get $k)
                  (i64.div_u (i64.mul (local.get $k) (i64.sub (local.get $n) (i64.const 1)))
                    (i64.const 63))
                  (i64.le_u (local.get $n) (i64.const 64))))
                ;; d_i
                (if (ref.is_null (local.get $m))
                  (then (local.set $d (local.get $small)))
                  (else
                    (local.set $at (i32.wrap_i64 (i64.shl (local.get $i) (i64.const 1))))
                    (local.set $d (i64.and (array.get $mag (local.get $m) (local.get $at))
                      (i64.const 0xffffffff)))
                    (if (i32.lt_u (i32.add (local.get $at) (i32.const 1)) (local.get $limbs))
                      (then (local.set $d (i64.or (local.get $d)
                        (i64.shl (array.get $mag (local.get $m) (i32.add (local.get $at) (i32.const 1)))
                          (i64.const 32))))))))
                ;; t = mix(i + K1)
                (local.set $t (i64.add (local.get $i) (i64.const 0x6a09e667f3bcc908)))
                (local.set $t (i64.mul (i64.xor (local.get $t) (i64.shr_u (local.get $t) (i64.const 30)))
                  (i64.const 0xbf58476d1ce4e5b9)))
                (local.set $t (i64.mul (i64.xor (local.get $t) (i64.shr_u (local.get $t) (i64.const 27)))
                  (i64.const 0x94d049bb133111eb)))
                (local.set $t (i64.xor (local.get $t) (i64.shr_u (local.get $t) (i64.const 31))))
                ;; h = mix(h ^ d_i ^ t)
                (local.set $h (i64.xor (i64.xor (local.get $h) (local.get $d)) (local.get $t)))
                (local.set $h (i64.mul (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 30)))
                  (i64.const 0xbf58476d1ce4e5b9)))
                (local.set $h (i64.mul (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 27)))
                  (i64.const 0x94d049bb133111eb)))
                (local.set $h (i64.xor (local.get $h) (i64.shr_u (local.get $h) (i64.const 31))))
                (local.set $k (i64.add (local.get $k) (i64.const 1)))
                (br $next)))
            (i64.shr_u (local.get $h) (i64.const 2))))
