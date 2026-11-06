
        (module
          {decls}
          {func_pad}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result i32)
            (local $am (ref null $mag)) (local $as_ i32)
            (local $bm (ref null $mag)) (local $bs i32)
            (local $alen i32) (local $blen i32)
            (local $cmp i32) (local $j i32) (local $umag i64)
            ;; Two Small operands (no limbs) compare as machine words, with no
            ;; limb arrays built.
            (if (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (return
                  (i32.sub
                    (i64.gt_s (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b)))
                    (i64.lt_s (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b)))))))
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
    