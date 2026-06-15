
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result i32)
            (local $am (ref null $mag)) (local $bm (ref null $mag))
            (local $i i32) (local $n i32) (local $eq i32)
            (local.set $am (struct.get $aint $magf (local.get $a)))
            (local.set $bm (struct.get $aint $magf (local.get $b)))
            (if (result i32) (ref.is_null (local.get $am))
              (then
                (if (result i32) (ref.is_null (local.get $bm))
                  (then (i64.eq (struct.get $aint $small (local.get $a)) (struct.get $aint $small (local.get $b))))
                  (else (i32.const 0))))
              (else
                (if (result i32) (ref.is_null (local.get $bm))
                  (then (i32.const 0))
                  (else
                    (if (result i32) (i32.ne (struct.get $aint $sign (local.get $a)) (struct.get $aint $sign (local.get $b)))
                      (then (i32.const 0))
                      (else
                        (if (result i32) (i32.ne (array.len (local.get $am)) (array.len (local.get $bm)))
                          (then (i32.const 0))
                          (else
                            (local.set $n (array.len (local.get $am)))
                            (local.set $i (i32.const 0))
                            (local.set $eq (i32.const 1))
                            (block $done (loop $lp
                              (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                              (if (i64.ne (array.get $mag (local.get $am) (local.get $i))
                                          (array.get $mag (local.get $bm) (local.get $i)))
                                (then (local.set $eq (i32.const 0)) (br $done)))
                              (local.set $i (i32.add (local.get $i) (i32.const 1)))
                              (br $lp)))
                            (local.get $eq)))))))))))
    