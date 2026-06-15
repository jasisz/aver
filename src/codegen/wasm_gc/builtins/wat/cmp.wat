
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result i32)
            (local $am (ref null $mag)) (local $as_ i32)
            (local $bm (ref null $mag)) (local $bs i32)
            (local $alen i32) (local $blen i32)
            (local $cmp i32) (local $j i32) (local $umag i64)
            {decomp_a}
            {decomp_b}
            (if (result i32) (i32.ne (local.get $as_) (local.get $bs))
              (then
                (if (result i32) (i32.gt_s (local.get $as_) (local.get $bs)) (then (i32.const 1)) (else (i32.const -1))))
              (else
                (if (result i32) (i32.eqz (local.get $as_))
                  (then (i32.const 0))
                  (else
                    {strip_a}
                    {strip_b}
                    {cmp}
                    (if (result i32) (i32.lt_s (local.get $as_) (i32.const 0))
                      (then (i32.sub (i32.const 0) (local.get $cmp)))
                      (else (local.get $cmp)))))))))
    