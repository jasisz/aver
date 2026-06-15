
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i64)
            ;; Saturating `$AverInt` -> i64. A Small passes its $small
            ;; through unchanged. A Big saturates by sign: a positive Big
            ;; (necessarily > i64::MAX) -> i64::MAX, a negative Big
            ;; (< i64::MIN) -> i64::MIN. Used to lower an Int argument into
            ;; an i64-typed builtin slot where out-of-i64 magnitudes only
            ;; need to be clamped to "all"/"none" (List.take / List.drop
            ;; counts) or a String char index past the string length, which
            ;; the receiving helper already treats as out of range.
            (if (result i64) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then (struct.get $aint $small (local.get $a)))
              (else
                (if (result i64) (i32.gt_s (struct.get $aint $sign (local.get $a)) (i32.const 0))
                  (then (i64.const 0x7fffffffffffffff))
                  (else (i64.const 0x8000000000000000)))))))
