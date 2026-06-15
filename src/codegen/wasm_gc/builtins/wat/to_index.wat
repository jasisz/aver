
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i32)
            (local $n i64)
            ;; A Big index is necessarily out of bounds (no engine array spans
            ;; 2^31 entries), so it maps to the OOB sentinel -1. A Small index
            ;; in [0, 2^31) passes through as its i32 value; a negative or
            ;; >= 2^31 Small is likewise -1. The caller bounds-checks
            ;; `idx >= 0 (signed)` AND `idx < len (unsigned)`; -1 fails BOTH
            ;; (signed: -1 < 0; unsigned: 0xffffffff >= len), so a Big or
            ;; out-of-range index yields the VM's `Option.None` / bounds miss,
            ;; never a wrapped in-range access.
            (if (result i32) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then
                (local.set $n (struct.get $aint $small (local.get $a)))
                (if (result i32)
                    (i32.and
                      (i64.ge_s (local.get $n) (i64.const 0))
                      (i64.lt_s (local.get $n) (i64.const 0x80000000)))
                  (then (i32.wrap_i64 (local.get $n)))
                  (else (i32.const -1))))
              (else (i32.const -1)))))
