        (module
          {decls}
          {func_pad}
          (func (export "helper") (param $n (ref null $aint)) (result (ref null $aint))
            (local $s i64) (local $limbs i32)
            (local $rm (ref null $mag)) (local $rs i32)
            (local $rlen i32) (local $i i32)
            (local $lo i64) (local $hi i64) (local $tmpm (ref null $mag))

            ;; A Big count cannot name a bit position on any machine, and a
            ;; negative one is refused by the caller before it gets here. Both
            ;; are unreachable from generated code, so trap rather than invent
            ;; a value — a contained wasm trap, mirroring the VM's runtime
            ;; abort on an unrepresentable count.
            (if (i32.eqz (ref.is_null (struct.get $aint $magf (local.get $n))))
              (then unreachable))
            (local.set $s (struct.get $aint $small (local.get $n)))
            (if (i64.lt_s (local.get $s) (i64.const 0))
              (then unreachable))
            ;; Past this the limb count would not fit an i32, and the array
            ;; allocation could not succeed anyway (2^31 bits is 256 MB).
            (if (i64.gt_u (local.get $s) (i64.const 0x7fffffff))
              (then unreachable))

            (if (result (ref null $aint)) (i64.lt_s (local.get $s) (i64.const 63))
              (then
                ;; Up to 2^62 stays inside i64, so the canonical form is Small.
                (struct.new $aint (i64.shl (i64.const 1) (local.get $s)) (ref.null $mag) (i32.const 0)))
              (else
                ;; A single set bit at position s: limb s/32, bit s%32.
                (local.set $limbs
                  (i32.add (i32.wrap_i64 (i64.div_u (local.get $s) (i64.const 32))) (i32.const 1)))
                (local.set $rm (array.new_default $mag (local.get $limbs)))
                (array.set $mag (local.get $rm) (i32.sub (local.get $limbs) (i32.const 1))
                  (i64.shl (i64.const 1) (i64.rem_u (local.get $s) (i64.const 32))))
                (local.set $rs (i32.const 1))
                {norm}))))
