
        (module
          {decls}
          (func (export "helper") (param $op (ref null $aint)) (result (ref null $mag) i32)
            (local $opm (ref null $mag)) (local $ops i32) (local $umag i64)
            (if (ref.is_null (struct.get $aint $magf (local.get $op)))
              (then
                (if (i64.eqz (struct.get $aint $small (local.get $op)))
                  (then
                    (local.set $ops (i32.const 0))
                    (local.set $opm (array.new_default $mag (i32.const 0))))
                  (else
                    (local.set $ops
                      (if (result i32) (i64.lt_s (struct.get $aint $small (local.get $op)) (i64.const 0))
                        (then (i32.const -1)) (else (i32.const 1))))
                    (local.set $umag
                      (if (result i64) (i32.lt_s (local.get $ops) (i32.const 0))
                        (then (i64.sub (i64.const 0) (struct.get $aint $small (local.get $op))))
                        (else (struct.get $aint $small (local.get $op)))))
                    (local.set $opm (array.new_default $mag (i32.const 2)))
                    (array.set $mag (local.get $opm) (i32.const 0)
                      (i64.and (local.get $umag) (i64.const 0xffffffff)))
                    (array.set $mag (local.get $opm) (i32.const 1)
                      (i64.shr_u (local.get $umag) (i64.const 32))))))
              (else
                (local.set $opm (struct.get $aint $magf (local.get $op)))
                (local.set $ops (struct.get $aint $sign (local.get $op)))))
            (local.get $opm) (local.get $ops)))

