
        (module
          {decls}
          {func_pad}
          (func (export "helper") (param $a (ref null $aint)) (param $b (ref null $aint)) (result (ref null $aint))
            (local $r i64)
            {locals}
            ;; fast path: both Small.
            (if (result (ref null $aint))
                (i32.and (ref.is_null (struct.get $aint $magf (local.get $a)))
                         (ref.is_null (struct.get $aint $magf (local.get $b))))
              (then
                (local.set $r {fast_op})
                (if (result (ref null $aint)) {overflow_check}
                  (then
                    ;; overflow → slow path
                    {decomp_a}
                    {decomp_b}
                    {beff_set}
                    {strip_a}
                    {strip_b}
                    {combine}
                    {norm})
                  (else
                    (struct.new $aint (local.get $r) (ref.null $mag) (i32.const 0)))))
              (else
                ;; at least one Big → slow path
                {decomp_a}
                {decomp_b}
                {beff_set}
                {strip_a}
                {strip_b}
                {combine}
                {norm}))))
    