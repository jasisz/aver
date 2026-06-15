
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result i64)
            ;; Checked `$AverInt` -> i64 for an Int argument crossing the
            ;; HOST EFFECT boundary (a Random bound, Time.sleep ms, a
            ;; network port, a Terminal coordinate). The host ABI is i64;
            ;; the VM's host services do a checked `AverInt::to_i64()` and
            ;; ERROR when the value does not fit i64. Mirror that: a Small
            ;; passes its $small through unchanged; a Big is BY DEFINITION
            ;; of the canonical form outside i64 range, so it TRAPS
            ;; (`unreachable`) — the program rejects on wasm-gc exactly as
            ;; the VM rejects, instead of silently saturating an out-of-
            ;; range effect arg to i64::MAX/MIN and proceeding.
            (if (result i64) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then (struct.get $aint $small (local.get $a)))
              (else (unreachable)))))
