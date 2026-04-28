;; Unwrap a heap-boxed scalar back to its native value. Boxed scalars
;; live as an 8-byte header plus a single 8-byte cell at offset 8;
;; rt_wrap_* on the codegen side allocates and stores, rt_unwrap_*
;; just loads from that cell.

(func $rt_unwrap (param $ptr i32) (result i64)
  local.get $ptr
  i64.load offset=8
)
(export "rt_unwrap" (func $rt_unwrap))

(func $rt_unwrap_f64 (param $ptr i32) (result f64)
  local.get $ptr
  f64.load offset=8
)
(export "rt_unwrap_f64" (func $rt_unwrap_f64))

(func $rt_unwrap_i32 (param $ptr i32) (result i32)
  local.get $ptr
  i64.load offset=8
  i32.wrap_i64
)
(export "rt_unwrap_i32" (func $rt_unwrap_i32))
