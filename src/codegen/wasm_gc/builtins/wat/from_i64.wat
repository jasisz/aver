
        (module
          {decls}
          (func (export "helper") (param $n i64) (result (ref null $aint))
            (struct.new $aint (local.get $n) (ref.null $mag) (i32.const 0))))
    