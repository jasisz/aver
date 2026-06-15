
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result (ref null $aint))
            (local $r i64) (local $av i64) (local $bv i64)
            (local $k i32) (local $prod i64) (local $ok i32)
            {locals}
            (if (result (ref null $aint))
                (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (local.set $av (struct.get $aint $small (local.get $a)))
                (local.set $bv (struct.get $aint $small (local.get $b)))
                (local.set $r (i64.mul (local.get $av) (local.get $bv)))
                ;; overflow oracle (mirrors i64::checked_mul): NO overflow
                ;; iff a == 0 (product genuinely 0), OR — a != 0 AND it is
                ;; not the `a == -1 && b == i64::MIN` div_s trap edge (that
                ;; edge IS an overflow) AND the round-trip `r / a == b`.
                ;;
                ;; CRUCIAL: wasm `i32.and`/`i32.or` are NOT short-circuiting
                ;; — both arms evaluate eagerly. A flat boolean expression
                ;; would run `i64.div_s` on the trap edge and TRAP. So we
                ;; compute `$ok` with lazy `if` control flow, never reaching
                ;; the divide unless the divisor is safe.
                (if (i64.eqz (local.get $av))
                  (then (local.set $ok (i32.const 1)))
                  (else
                    (if (i32.and (i64.eq (local.get $av) (i64.const -1))
                                 (i64.eq (local.get $bv) (i64.const 0x8000000000000000)))
                      (then (local.set $ok (i32.const 0)))  ;; trap edge → overflow
                      (else
                        (local.set $ok (i64.eq (i64.div_s (local.get $r) (local.get $av)) (local.get $bv)))))))
                (if (result (ref null $aint)) (local.get $ok)
                  (then
                    ;; no overflow → Small result
                    (struct.new $aint (local.get $r) (ref.null $mag) (i32.const 0)))
                  (else
                    ;; overflow → slow path
                    {decomp_a} {decomp_b} {strip_a} {strip_b}
                    {mul_body}
                    {norm})))
              (else
                {decomp_a} {decomp_b} {strip_a} {strip_b}
                {mul_body}
                {norm}))))
    