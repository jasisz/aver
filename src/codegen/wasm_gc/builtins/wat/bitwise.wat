        (module
          {decls}
          {func_pad}
          (func (export "helper")
                (param $a (ref null $aint)) (param $b (ref null $aint)) (param $op i32)
                (result (ref null $aint))
            (local $am (ref null $mag)) (local $as_ i32)
            (local $bm (ref null $mag)) (local $bs i32)
            (local $alen i32) (local $blen i32) (local $rlen i32)
            (local $rm (ref null $mag)) (local $rs i32)
            (local $i i32) (local $n i32)
            (local $umag i64) (local $lo i64) (local $hi i64) (local $tmpm (ref null $mag))
            (local $r i64) (local $ta i64) (local $tb i64) (local $ca i64) (local $cb i64)
            (local $aneg i32) (local $bneg i32) (local $rneg i32)

            ;; Fast path: both Small. An i64 IS the infinite two's-complement
            ;; sequence restricted to i64-representable values — sign extension
            ;; is exactly the infinite tail — and and/or/xor of two such values
            ;; is always representable again, so the native op is EXACT, not an
            ;; approximation. No overflow check is possible or needed.
            (if (result (ref null $aint))
                (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (local.set $r
                  (if (result i64) (i32.eq (local.get $op) (i32.const 0))
                    (then (i64.and (struct.get $aint $small (local.get $a))
                                   (struct.get $aint $small (local.get $b))))
                    (else
                      (if (result i64) (i32.eq (local.get $op) (i32.const 1))
                        (then (i64.or (struct.get $aint $small (local.get $a))
                                      (struct.get $aint $small (local.get $b))))
                        (else (i64.xor (struct.get $aint $small (local.get $a))
                                       (struct.get $aint $small (local.get $b))))))))
                (struct.new $aint (local.get $r) (ref.null $mag) (i32.const 0)))
              (else
                ;; At least one Big. Decompose to sign + magnitude, then walk
                ;; the operands as two's-complement 32-bit limbs.
                {decomp_a}
                {decomp_b}
                (local.set $aneg (i32.lt_s (local.get $as_) (i32.const 0)))
                (local.set $bneg (i32.lt_s (local.get $bs) (i32.const 0)))
                {strip_a}
                {strip_b}
                ;; One limb PAST the longer operand, so the top limb of every
                ;; two's-complement expansion is pure sign extension and the
                ;; result's sign is decided by the sign tails alone.
                (local.set $n
                  (i32.add (i32.const 1)
                    (if (result i32) (i32.gt_u (local.get $alen) (local.get $blen))
                      (then (local.get $alen)) (else (local.get $blen)))))
                (local.set $rm (array.new_default $mag (local.get $n)))
                ;; Two's complement of a magnitude, limb by limb: acc starts at
                ;; 1 and carries the +1; each step emits `~limb + carry`. Past
                ;; the magnitude's own length the limb reads 0, so this keeps
                ;; producing the all-ones tail a negative value needs.
                (local.set $ca (i64.const 1))
                (local.set $cb (i64.const 1))
                (local.set $i (i32.const 0))
                (block $bw_done (loop $bw
                  (br_if $bw_done (i32.ge_u (local.get $i) (local.get $n)))
                  (local.set $ta
                    (if (result i64) (i32.lt_u (local.get $i) (local.get $alen))
                      (then (i64.and (array.get $mag (local.get $am) (local.get $i)) (i64.const 0xffffffff)))
                      (else (i64.const 0))))
                  (if (local.get $aneg)
                    (then
                      (local.set $ca
                        (i64.add (local.get $ca)
                          (i64.and (i64.xor (local.get $ta) (i64.const 0xffffffff)) (i64.const 0xffffffff))))
                      (local.set $ta (i64.and (local.get $ca) (i64.const 0xffffffff)))
                      (local.set $ca (i64.shr_u (local.get $ca) (i64.const 32)))))
                  (local.set $tb
                    (if (result i64) (i32.lt_u (local.get $i) (local.get $blen))
                      (then (i64.and (array.get $mag (local.get $bm) (local.get $i)) (i64.const 0xffffffff)))
                      (else (i64.const 0))))
                  (if (local.get $bneg)
                    (then
                      (local.set $cb
                        (i64.add (local.get $cb)
                          (i64.and (i64.xor (local.get $tb) (i64.const 0xffffffff)) (i64.const 0xffffffff))))
                      (local.set $tb (i64.and (local.get $cb) (i64.const 0xffffffff)))
                      (local.set $cb (i64.shr_u (local.get $cb) (i64.const 32)))))
                  (array.set $mag (local.get $rm) (local.get $i)
                    (if (result i64) (i32.eq (local.get $op) (i32.const 0))
                      (then (i64.and (local.get $ta) (local.get $tb)))
                      (else
                        (if (result i64) (i32.eq (local.get $op) (i32.const 1))
                          (then (i64.or (local.get $ta) (local.get $tb)))
                          (else (i64.xor (local.get $ta) (local.get $tb)))))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $bw)))
                ;; The result's sign tail is the operation applied to the two
                ;; input sign tails (0/1 booleans, so the i32 ops suffice).
                (local.set $rneg
                  (if (result i32) (i32.eq (local.get $op) (i32.const 0))
                    (then (i32.and (local.get $aneg) (local.get $bneg)))
                    (else
                      (if (result i32) (i32.eq (local.get $op) (i32.const 1))
                        (then (i32.or (local.get $aneg) (local.get $bneg)))
                        (else (i32.xor (local.get $aneg) (local.get $bneg)))))))
                (if (local.get $rneg)
                  (then
                    ;; Back from two's complement to sign+magnitude. Negation is
                    ;; an involution modulo 2^(32n), so the SAME carry loop that
                    ;; produced the complement recovers |result|.
                    (local.set $ca (i64.const 1))
                    (local.set $i (i32.const 0))
                    (block $ng_done (loop $ng
                      (br_if $ng_done (i32.ge_u (local.get $i) (local.get $n)))
                      (local.set $ca
                        (i64.add (local.get $ca)
                          (i64.and (i64.xor (array.get $mag (local.get $rm) (local.get $i))
                                            (i64.const 0xffffffff))
                                   (i64.const 0xffffffff))))
                      (array.set $mag (local.get $rm) (local.get $i)
                        (i64.and (local.get $ca) (i64.const 0xffffffff)))
                      (local.set $ca (i64.shr_u (local.get $ca) (i64.const 32)))
                      (local.set $i (i32.add (local.get $i) (i32.const 1)))
                      (br $ng)))))
                (local.set $rs
                  (if (result i32) (local.get $rneg)
                    (then (i32.const -1)) (else (i32.const 1))))
                {norm}))))
