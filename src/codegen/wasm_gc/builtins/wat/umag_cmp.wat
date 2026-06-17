
        (module
          {decls}
          (func (export "helper")
              (param $am (ref null $mag)) (param $alen i32)
              (param $bm (ref null $mag)) (param $blen i32) (result i32)
            (local $cmp i32) (local $j i32)
            (if (i32.ne (local.get $alen) (local.get $blen))
              (then
                (local.set $cmp
                  (if (result i32) (i32.gt_u (local.get $alen) (local.get $blen)) (then (i32.const 1)) (else (i32.const -1)))))
              (else
                (local.set $cmp (i32.const 0))
                (local.set $j (local.get $alen))
                (block $ucmp_done (loop $ucmp
                  (br_if $ucmp_done (i32.eqz (local.get $j)))
                  (local.set $j (i32.sub (local.get $j) (i32.const 1)))
                  (if (i64.ne (array.get $mag (local.get $am) (local.get $j))
                              (array.get $mag (local.get $bm) (local.get $j)))
                    (then
                      (local.set $cmp
                        (if (result i32) (i64.gt_u (array.get $mag (local.get $am) (local.get $j))
                                                   (array.get $mag (local.get $bm) (local.get $j)))
                          (then (i32.const 1)) (else (i32.const -1))))
                      (br $ucmp_done)))
                  (br $ucmp)))))
            (local.get $cmp)))

