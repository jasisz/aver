        (module
          {decls}
          (func (export "helper") (param $u i64) (result (ref null $aint))
            ;; The unsigned reading of a 64-bit pattern (`Float.toBits`). A
            ;; clear top bit is a Small; a set one is the canonical two-limb
            ;; Big `2^63 <= u < 2^64`, whose high limb is never zero.
            (if (result (ref null $aint)) (i64.ge_s (local.get $u) (i64.const 0))
              (then (struct.new $aint (local.get $u) (ref.null $mag) (i32.const 0)))
              (else
                (struct.new $aint (i64.const 0)
                  (array.new_fixed $mag 2
                    (i64.and (local.get $u) (i64.const 0xffffffff))
                    (i64.shr_u (local.get $u) (i64.const 32)))
                  (i32.const 1))))))
