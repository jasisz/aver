
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i32)
            (local $m (ref null $mag))
            (local $i i32) (local $n i32) (local $h i32) (local $s i64) (local $limb i64)
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (result i32) (ref.is_null (local.get $m))
              (then
                ;; Small — fold the i64 `$small` to i32: (s ^ (s >>> 32)),
                ;; then wrap. Distributes both halves of the 64-bit value.
                (local.set $s (struct.get $aint $small (local.get $a)))
                (i32.wrap_i64 (i64.xor (local.get $s) (i64.shr_u (local.get $s) (i64.const 32)))))
              (else
                ;; Big — DJB2-style fold over the 32-bit limb magnitude,
                ;; then mix the sign in. The canonical invariant guarantees
                ;; a Big never equals a Small, so the two schemes need not
                ;; agree; equal Bigs share sign + limbs ⇒ same hash.
                (local.set $h (i32.const 5381))
                (local.set $n (array.len (local.get $m)))
                (local.set $i (i32.const 0))
                (block $done (loop $lp
                  (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                  (local.set $limb (array.get $mag (local.get $m) (local.get $i)))
                  ;; h = h * 33 + low32(limb)
                  (local.set $h (i32.add
                    (i32.add (i32.shl (local.get $h) (i32.const 5)) (local.get $h))
                    (i32.wrap_i64 (local.get $limb))))
                  ;; h = h * 33 + high32(limb)
                  (local.set $h (i32.add
                    (i32.add (i32.shl (local.get $h) (i32.const 5)) (local.get $h))
                    (i32.wrap_i64 (i64.shr_u (local.get $limb) (i64.const 32)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))
                ;; mix sign (-1 / +1; Big is never zero-magnitude canonical)
                (i32.add
                  (i32.add (i32.shl (local.get $h) (i32.const 5)) (local.get $h))
                  (struct.get $aint $sign (local.get $a)))))))
