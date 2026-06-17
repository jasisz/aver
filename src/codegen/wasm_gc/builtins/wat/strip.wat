
        (module
          {decls}
          (func (export "helper") (param $arr (ref null $mag)) (result i32)
            (local $len i32)
            (local.set $len (array.len (local.get $arr)))
            (block $bl (loop $lp
              (br_if $bl (i32.eqz (local.get $len)))
              (br_if $bl (i64.ne (array.get $mag (local.get $arr) (i32.sub (local.get $len) (i32.const 1))) (i64.const 0)))
              (local.set $len (i32.sub (local.get $len) (i32.const 1)))
              (br $lp)))
            (local.get $len)))

