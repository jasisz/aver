
        (module
          {decls}
          {func_pad}
          (func (export "helper")
              (param $a (ref null $aint)) (param $b (ref null $aint)) (param $want_mod i32)
              (result (ref null $aint))
            (local $sa i64) (local $sb i64) (local $sq i64) (local $sr i64)
            {locals}
            ;; ── fast path: both Small AND not the i64::MIN / -1 overflow ──
            ;; (`i64.div_s` traps on MIN/-1; over ℤ that quotient is +2^63,
            ;; a Big, so it must take the limb path). Caller guarantees b != 0.
            (if (result (ref null $aint))
                (i32.and
                  (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                           (ref.is_null (struct.get $aint $magf (local.get $b))))
                  (i32.eqz
                    (i32.and
                      (i64.eq (struct.get $aint $small (local.get $a)) (i64.const 0x8000000000000000))
                      (i64.eq (struct.get $aint $small (local.get $b)) (i64.const -1)))))
              (then
                (local.set $sa (struct.get $aint $small (local.get $a)))
                (local.set $sb (struct.get $aint $small (local.get $b)))
                ;; Euclidean rem: q = a rem_s b; if q<0, q += |b|.
                (local.set $sr (i64.rem_s (local.get $sa) (local.get $sb)))
                (if (i64.lt_s (local.get $sr) (i64.const 0))
                  (then
                    (local.set $sr (i64.add (local.get $sr)
                      (if (result i64) (i64.lt_s (local.get $sb) (i64.const 0))
                        (then (i64.sub (i64.const 0) (local.get $sb)))
                        (else (local.get $sb)))))))
                (if (result (ref null $aint)) (local.get $want_mod)
                  (then
                    (struct.new $aint (local.get $sr) (ref.null $mag) (i32.const 0)))
                  (else
                    ;; Euclidean quotient: trunc q, step toward -inf when rem<0.
                    (local.set $sq (i64.div_s (local.get $sa) (local.get $sb)))
                    (local.set $sr (i64.rem_s (local.get $sa) (local.get $sb)))
                    (if (i64.lt_s (local.get $sr) (i64.const 0))
                      (then
                        (local.set $sq
                          (if (result i64) (i64.gt_s (local.get $sb) (i64.const 0))
                            (then (i64.sub (local.get $sq) (i64.const 1)))
                            (else (i64.add (local.get $sq) (i64.const 1)))))))
                    (struct.new $aint (local.get $sq) (ref.null $mag) (i32.const 0)))))
              (else
                ;; ── slow path: limb long division ──
                {decomp_a}
                {decomp_b}
                {strip_a}
                {strip_b}
                ;; Unsigned long division of |a| (am, alen) by |b| (bm, blen),
                ;; shift-subtract, MSB-to-LSB over the bit width of |a|. Produces
                ;; unsigned quotient magnitude $qm (len $alen) and remainder
                ;; magnitude held in $rwm (len $blen+1, working) → stripped len $rwlen.
                (local.set $qm (array.new_default $mag (if (result i32) (i32.eqz (local.get $alen)) (then (i32.const 1)) (else (local.get $alen)))))
                (local.set $rwm (array.new_default $mag (i32.add (local.get $blen) (i32.const 1))))
                (local.set $rwlen (i32.const 0))
                ;; A single 32-bit divisor needs one unsigned division per limb.
                ;; carry < divisor, so (carry << 32) | limb fits in u64 and
                ;; the quotient limb is below 2^32. Shared sign correction below
                ;; still supplies Euclidean division for every sign combination.
                (if (i32.eq (local.get $blen) (i32.const 1))
                  (then
                    (local.set $sb (array.get $mag (local.get $bm) (i32.const 0)))
                    (local.set $sr (i64.const 0))
                    (local.set $i (i32.sub (local.get $alen) (i32.const 1)))
                    (block $word_div_done (loop $word_div
                      (br_if $word_div_done (i32.lt_s (local.get $i) (i32.const 0)))
                      (local.set $acc (i64.or
                        (i64.shl (local.get $sr) (i64.const 32))
                        (array.get $mag (local.get $am) (local.get $i))))
                      (array.set $mag (local.get $qm) (local.get $i)
                        (i64.div_u (local.get $acc) (local.get $sb)))
                      (local.set $sr (i64.rem_u (local.get $acc) (local.get $sb)))
                      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                      (br $word_div)))
                    (array.set $mag (local.get $rwm) (i32.const 0) (local.get $sr)))
                  (else
                    (block $unsigned_done
                      ;; If |a| < |b|, the quotient is zero and the remainder is |a|.
                      {cmp_a_b}
                      (if (i32.lt_s (local.get $cmp) (i32.const 0))
                        (then
                          (array.copy $mag $mag (local.get $rwm) (i32.const 0)
                            (local.get $am) (i32.const 0) (local.get $alen))
                          (br $unsigned_done)))
                      ;; The highest blen-1 limbs are strictly below |b|.
                      ;; Seed that prefix as the remainder, skipping quotient zeros.
                      (local.set $rwlen (i32.sub (local.get $blen) (i32.const 1)))
                      (local.set $word (i32.sub (local.get $alen) (local.get $rwlen)))
                      (array.copy $mag $mag (local.get $rwm) (i32.const 0)
                        (local.get $am) (local.get $word) (local.get $rwlen))
                      (local.set $bit (i32.sub (i32.mul (local.get $word) (i32.const 32)) (i32.const 1)))
                      (block $bits_done (loop $bits
                        (br_if $bits_done (i32.lt_s (local.get $bit) (i32.const 0)))
                        ;; A shift can grow the live remainder by at most one limb.
                        ;; Higher allocated limbs remain zero; do not scan the full
                        ;; divisor width while the partial remainder is still small.
                        (local.set $rwlen
                          (i32.add
                            (if (result i32) (i32.lt_u (local.get $rwlen) (local.get $blen))
                              (then (local.get $rwlen)) (else (local.get $blen)))
                            (i32.const 1)))
                        ;; r <<= 1  (shift the working remainder magnitude left by one bit)
                        (local.set $carry (i64.const 0))
                        (local.set $i (i32.const 0))
                        (block $shl_done (loop $shl
                          (br_if $shl_done (i32.ge_u (local.get $i) (local.get $rwlen)))
                          (local.set $acc (i64.or
                            (i64.and (i64.shl (array.get $mag (local.get $rwm) (local.get $i)) (i64.const 1)) (i64.const 0xffffffff))
                            (local.get $carry)))
                          (local.set $carry (i64.shr_u (array.get $mag (local.get $rwm) (local.get $i)) (i64.const 31)))
                          (array.set $mag (local.get $rwm) (local.get $i) (local.get $acc))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $shl)))
                        ;; r |= bit_(bit) of |a|
                        (local.set $word (i32.div_u (local.get $bit) (i32.const 32)))
                        (local.set $off (i32.rem_u (local.get $bit) (i32.const 32)))
                        (if (i64.ne
                              (i64.and (i64.shr_u (array.get $mag (local.get $am) (local.get $word)) (i64.extend_i32_u (local.get $off))) (i64.const 1))
                              (i64.const 0))
                          (then
                            (array.set $mag (local.get $rwm) (i32.const 0)
                              (i64.or (array.get $mag (local.get $rwm) (i32.const 0)) (i64.const 1)))))
                        ;; Strip only the live upper bound computed before shifting.
                        (block $rstrip_done (loop $rstrip
                          (br_if $rstrip_done (i32.eqz (local.get $rwlen)))
                          (br_if $rstrip_done (i64.ne (array.get $mag (local.get $rwm) (i32.sub (local.get $rwlen) (i32.const 1))) (i64.const 0)))
                          (local.set $rwlen (i32.sub (local.get $rwlen) (i32.const 1)))
                          (br $rstrip)))
                        ;; if r >= |b|: r -= |b|; set quotient bit.
                        {cmp_r_b}
                        (if (i32.ge_s (local.get $cmp) (i32.const 0))
                          (then
                            ;; r -= |b|  (borrow subtract, r >= |b| guaranteed)
                            (local.set $borrow (i64.const 0))
                            (local.set $i (i32.const 0))
                            (block $rsub_done (loop $rsub
                              (br_if $rsub_done (i32.ge_u (local.get $i) (i32.add (local.get $blen) (i32.const 1))))
                              (local.set $diff (i64.sub
                                (i64.sub
                                  (array.get $mag (local.get $rwm) (local.get $i))
                                  (if (result i64) (i32.lt_u (local.get $i) (local.get $blen)) (then (array.get $mag (local.get $bm) (local.get $i))) (else (i64.const 0))))
                                (local.get $borrow)))
                              (if (i64.lt_s (local.get $diff) (i64.const 0))
                                (then
                                  (local.set $diff (i64.add (local.get $diff) (i64.const 0x100000000)))
                                  (local.set $borrow (i64.const 1)))
                                (else (local.set $borrow (i64.const 0))))
                              (array.set $mag (local.get $rwm) (local.get $i) (i64.and (local.get $diff) (i64.const 0xffffffff)))
                              (local.set $i (i32.add (local.get $i) (i32.const 1)))
                              (br $rsub)))
                            ;; set quotient bit (bit) — word/off computed above.
                            (array.set $mag (local.get $qm) (local.get $word)
                              (i64.or (array.get $mag (local.get $qm) (local.get $word))
                                (i64.shl (i64.const 1) (i64.extend_i32_u (local.get $off)))))))
                        (local.set $bit (i32.sub (local.get $bit) (i32.const 1)))
                        (br $bits)))
                    )
                  ))
                ;; $rwm now holds the unsigned truncating remainder magnitude;
                ;; $qm the unsigned truncating quotient. Re-strip $rwm to its
                ;; canonical length $rwlen — the in-loop $rwlen reflects the
                ;; PRE-subtract remainder of the last bit, so it is stale here.
                (local.set $rwlen (i32.add (local.get $blen) (i32.const 1)))
                (block $rwfin_done (loop $rwfin
                  (br_if $rwfin_done (i32.eqz (local.get $rwlen)))
                  (br_if $rwfin_done (i64.ne (array.get $mag (local.get $rwm) (i32.sub (local.get $rwlen) (i32.const 1))) (i64.const 0)))
                  (local.set $rwlen (i32.sub (local.get $rwlen) (i32.const 1)))
                  (br $rwfin)))
                ;;
                ;; Result signs: trunc-q sign = as_ * bs (0 if either 0);
                ;; trunc-r sign follows the dividend (as_), 0 if rem is 0.
                ;; Euclidean lift (dividend negative & remainder nonzero):
                ;;   quotient magnitude += 1 (sign unchanged = as_ * bs);
                ;;   remainder = |b| - rwm, sign +.
                ;;
                ;; Result signs: trunc-q sign = as_ * bs (0 if either 0);
                ;; trunc-r sign follows the dividend (as_), 0 if rem is 0.
                ;; Euclidean lift (dividend negative & remainder nonzero):
                ;;   quotient magnitude += 1 (sign unchanged = as_ * bs);
                ;;   remainder = |b| - rwm, sign +.
                (local.set $negadj
                  (i32.and (i32.lt_s (local.get $as_) (i32.const 0)) (i32.ne (local.get $rwlen) (i32.const 0))))
                (if (result (ref null $aint)) (local.get $want_mod)
                  (then
                    ;; ── remainder ──
                    (if (local.get $negadj)
                      (then
                        ;; rwout = |b| - rwm  (rwm < |b|, so a clean borrow subtract)
                        (local.set $rwout (array.new_default $mag (local.get $blen)))
                        (local.set $borrow (i64.const 0))
                        (local.set $i (i32.const 0))
                        (block $rfin_done (loop $rfin
                          (br_if $rfin_done (i32.ge_u (local.get $i) (local.get $blen)))
                          (local.set $diff (i64.sub
                            (i64.sub
                              (array.get $mag (local.get $bm) (local.get $i))
                              (if (result i64) (i32.lt_u (local.get $i) (local.get $rwlen)) (then (array.get $mag (local.get $rwm) (local.get $i))) (else (i64.const 0))))
                            (local.get $borrow)))
                          (if (i64.lt_s (local.get $diff) (i64.const 0))
                            (then
                              (local.set $diff (i64.add (local.get $diff) (i64.const 0x100000000)))
                              (local.set $borrow (i64.const 1)))
                            (else (local.set $borrow (i64.const 0))))
                          (array.set $mag (local.get $rwout) (local.get $i) (i64.and (local.get $diff) (i64.const 0xffffffff)))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $rfin)))
                        (local.set $rm (local.get $rwout)))
                      (else
                        ;; trunc remainder magnitude as-is; sign +.
                        (local.set $rm (local.get $rwm))))
                    (local.set $rs (i32.const 1))
                    {norm_r})
                  (else
                    ;; ── quotient ──
                    (local.set $rs (i32.mul (local.get $as_) (local.get $bs)))
                    (if (local.get $negadj)
                      (then
                        ;; quotient magnitude += 1 (ripple carry; grow by one limb
                        ;; so a top-limb carry has room).
                        (local.set $rwout (array.new_default $mag (i32.add (local.get $alen) (i32.const 1))))
                        (local.set $i (i32.const 0))
                        (block $qcp_done (loop $qcp
                          (br_if $qcp_done (i32.ge_u (local.get $i) (local.get $alen)))
                          (array.set $mag (local.get $rwout) (local.get $i) (array.get $mag (local.get $qm) (local.get $i)))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $qcp)))
                        (local.set $carry (i64.const 1))
                        (local.set $i (i32.const 0))
                        (block $qinc_done (loop $qinc
                          (br_if $qinc_done (i64.eqz (local.get $carry)))
                          (local.set $acc (i64.add (array.get $mag (local.get $rwout) (local.get $i)) (local.get $carry)))
                          (array.set $mag (local.get $rwout) (local.get $i) (i64.and (local.get $acc) (i64.const 0xffffffff)))
                          (local.set $carry (i64.shr_u (local.get $acc) (i64.const 32)))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $qinc)))
                        (local.set $rm (local.get $rwout)))
                      (else
                        (local.set $rm (local.get $qm))))
                    {norm_q}))))))

