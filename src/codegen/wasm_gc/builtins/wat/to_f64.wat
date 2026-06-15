
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result f64)
            (local $m (ref null $mag)) (local $len i32)
            (local $toplimb i64) (local $topbits i32) (local $sigbits i32)
            (local $s i32) (local $word i32) (local $bit i64)
            (local $l0 i64) (local $l1 i64) (local $l2 i64)
            (local $mreg i64) (local $sticky i32) (local $i i32)
            (local $v0 f64) (local $pow f64)
            (local.set $m (struct.get $aint $magf (local.get $a)))
            (if (result f64) (ref.is_null (local.get $m))
              (then
                ;; Small — exact i64→f64 (round-to-nearest, as the VM's `n as f64`).
                (f64.convert_i64_s (struct.get $aint $small (local.get $a))))
              (else
                ;; Big — correctly-rounded |mag|→f64 via the sticky-jam technique,
                ;; then apply the sign. `$magf` is the canonical little-endian
                ;; 32-bit-limb magnitude (top limb non-zero, length >= 1), so the
                ;; significant bit count `sigbits >= 1`.
                ;;
                ;; Reduce the magnitude to its TOP 64 significant bits `mreg`,
                ;; with shift exponent `s = sigbits - 64` (clamped at 0), so the
                ;; true magnitude = mreg*2^s + dropped, where `dropped` is the
                ;; bits below position `s`. `sticky` = (dropped != 0). Jamming
                ;; sticky into mreg's LSB makes `f64.convert_i64_u(mreg)`'s
                ;; round-to-nearest-even break half-way ties in the correct
                ;; direction (the true value sits above the 64-bit mreg when
                ;; dropped != 0); when dropped == 0 the conversion is exact.
                ;; Finally multiply by 2^s (an EXACT power of two in f64 — the
                ;; multiply introduces NO rounding except saturation to +inf for
                ;; magnitudes past f64 range, which matches `BigInt::to_f64`).
                (local.set $len (array.len (local.get $m)))
                ;; significant bits of the top (non-zero) limb: 32 - clz32.
                (local.set $toplimb
                  (i64.and (array.get $mag (local.get $m) (i32.sub (local.get $len) (i32.const 1)))
                           (i64.const 0xffffffff)))
                (local.set $topbits
                  (i32.sub (i32.const 32) (i32.clz (i32.wrap_i64 (local.get $toplimb)))))
                (local.set $sigbits
                  (i32.add (i32.mul (i32.sub (local.get $len) (i32.const 1)) (i32.const 32))
                           (local.get $topbits)))
                ;; s = max(0, sigbits - 64)
                (local.set $s
                  (if (result i32) (i32.gt_s (local.get $sigbits) (i32.const 64))
                    (then (i32.sub (local.get $sigbits) (i32.const 64)))
                    (else (i32.const 0))))
                ;; word = s / 32, bit = s % 32: the boundary limb + sub-limb offset.
                (local.set $word (i32.div_u (local.get $s) (i32.const 32)))
                (local.set $bit (i64.extend_i32_u (i32.rem_u (local.get $s) (i32.const 32))))
                ;; Read the three limbs that hold the top 64 bits: limbs
                ;; word, word+1, word+2 (64 bits + offset bit spans <= 3 limbs).
                ;; Out-of-range limb indices read as 0.
                (local.set $l0
                  (if (result i64) (i32.lt_u (local.get $word) (local.get $len))
                    (then (i64.and (array.get $mag (local.get $m) (local.get $word)) (i64.const 0xffffffff)))
                    (else (i64.const 0))))
                (local.set $l1
                  (if (result i64) (i32.lt_u (i32.add (local.get $word) (i32.const 1)) (local.get $len))
                    (then (i64.and (array.get $mag (local.get $m) (i32.add (local.get $word) (i32.const 1))) (i64.const 0xffffffff)))
                    (else (i64.const 0))))
                (local.set $l2
                  (if (result i64) (i32.lt_u (i32.add (local.get $word) (i32.const 2)) (local.get $len))
                    (then (i64.and (array.get $mag (local.get $m) (i32.add (local.get $word) (i32.const 2))) (i64.const 0xffffffff)))
                    (else (i64.const 0))))
                ;; mreg = bits [s, s+64) = combine(l0,l1,l2) >> bit, as a u64.
                ;;   term0 = l0 >> bit               (bit in [0,31])
                ;;   term1 = l1 << (32 - bit)        (shift in [1,32])
                ;;   term2 = l2 << (64 - bit), but 0 when bit == 0 (would be a
                ;;           no-op shift-by-64 == shift-by-0 otherwise).
                (local.set $mreg
                  (i64.or
                    (i64.or
                      (i64.shr_u (local.get $l0) (local.get $bit))
                      (i64.shl (local.get $l1) (i64.sub (i64.const 32) (local.get $bit))))
                    (if (result i64) (i64.eqz (local.get $bit))
                      (then (i64.const 0))
                      (else (i64.shl (local.get $l2) (i64.sub (i64.const 64) (local.get $bit)))))))
                ;; sticky = (dropped != 0), dropped = bits [0, s):
                ;;   - every limb at index < word non-zero, plus
                ;;   - the low `bit` bits of limb `word`.
                (local.set $sticky (i32.const 0))
                (if (i64.ne
                      (i64.and (local.get $l0)
                               (i64.sub (i64.shl (i64.const 1) (local.get $bit)) (i64.const 1)))
                      (i64.const 0))
                  (then (local.set $sticky (i32.const 1))))
                (local.set $i (i32.const 0))
                (block $sdone (loop $slp
                  (br_if $sdone (i32.ge_u (local.get $i) (local.get $word)))
                  (if (i64.ne (array.get $mag (local.get $m) (local.get $i)) (i64.const 0))
                    (then (local.set $sticky (i32.const 1))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $slp)))
                ;; Jam sticky into mreg's LSB, convert (rounds at bit 53,
                ;; round-to-nearest-even — correct for a <= 64-bit value).
                (local.set $v0
                  (f64.convert_i64_u (i64.or (local.get $mreg) (i64.extend_i32_u (local.get $sticky)))))
                ;; pow = 2^s as an EXACT f64. For s <= 1023 build it from the
                ;; biased exponent field (s + 1023) << 52; for s >= 1024 it is
                ;; +inf, so v0 * inf = inf (correct saturation; v0 > 0 always).
                (local.set $pow
                  (if (result f64) (i32.le_s (local.get $s) (i32.const 1023))
                    (then (f64.reinterpret_i64
                            (i64.shl (i64.add (i64.extend_i32_u (local.get $s)) (i64.const 1023))
                                     (i64.const 52))))
                    (else (f64.const inf))))
                ;; result = (mreg jammed → f64) * 2^s, with the sign applied.
                (if (result f64) (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0))
                  (then (f64.neg (f64.mul (local.get $v0) (local.get $pow))))
                  (else (f64.mul (local.get $v0) (local.get $pow))))))))
