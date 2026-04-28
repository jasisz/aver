(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func (result i32 i32)))
  (type (;2;) (func (param i64 i32) (result i32)))
  (type (;3;) (func (param f64 i32) (result i32)))
  (type (;4;) (func (param i64) (result i32)))
  (type (;5;) (func (param f64) (result i32)))
  (type (;6;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;7;) (func (param i32 i32 i32)))
  (type (;8;) (func (param i32 i64)))
  (type (;9;) (func (param i32 i64) (result i32 i32)))
  (import "aver_runtime" "rt_int_to_str" (func (;0;) (type 2)))
  (import "aver_runtime" "rt_float_to_str" (func (;1;) (type 3)))
  (import "aver_runtime" "rt_i64_to_str_obj" (func (;2;) (type 4)))
  (import "aver_runtime" "rt_f64_to_str_obj" (func (;3;) (type 5)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;4;) (type 6)))
  (import "aver_runtime" "memory" (memory (;0;) 1))
  (export "console_print" (func 6))
  (export "console_error" (func 7))
  (export "print_value" (func 8))
  (export "format_value" (func 9))
  (func (;5;) (type 7) (param i32 i32 i32)
    i32.const 0
    local.get 1
    i32.store
    i32.const 4
    local.get 2
    i32.store
    local.get 0
    i32.const 0
    i32.const 1
    i32.const 8
    call 4
    drop
  )
  (func (;6;) (type 0) (param i32 i32)
    i32.const 1
    local.get 0
    local.get 1
    call 5
  )
  (func (;7;) (type 0) (param i32 i32)
    i32.const 2
    local.get 0
    local.get 1
    call 5
  )
  (func (;8;) (type 8) (param i32 i64)
    local.get 0
    i32.eqz
    if ;; label = @1
      local.get 1
      i32.const 16
      call 0
      local.tee 0
      i32.const 16
      i32.shr_u
      i32.const 16
      i32.add
      local.get 0
      i32.const 65535
      i32.and
      i32.const 1
      call 5
      return
    end
    local.get 0
    i32.const 1
    i32.eq
    if ;; label = @1
      local.get 1
      f64.reinterpret_i64
      i32.const 48
      call 1
      local.tee 0
      i32.const 16
      i32.shr_u
      i32.const 48
      i32.add
      local.get 0
      i32.const 65535
      i32.and
      i32.const 1
      call 5
      return
    end
    local.get 0
    i32.const 2
    i32.eq
    if ;; label = @1
      local.get 1
      i64.eqz
      if ;; label = @2
        i32.const 1
        i32.const 100
        i32.const 5
        call 5
      else
        i32.const 1
        i32.const 96
        i32.const 4
        call 5
      end
      return
    end
    local.get 0
    i32.const 3
    i32.eq
    local.get 0
    i32.const 4
    i32.eq
    i32.or
    if ;; label = @1
      i32.const 1
      local.get 1
      i32.wrap_i64
      local.tee 0
      i32.const 8
      i32.add
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      call 5
    end
  )
  (func (;9;) (type 9) (param i32 i64) (result i32 i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      local.get 1
      call 2
      local.tee 0
      i32.const 8
      i32.add
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      return
    end
    local.get 0
    i32.const 1
    i32.eq
    if ;; label = @1
      local.get 1
      f64.reinterpret_i64
      call 3
      local.tee 0
      i32.const 8
      i32.add
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      return
    end
    local.get 0
    i32.const 2
    i32.eq
    if ;; label = @1
      local.get 1
      i64.eqz
      if (type 1) (result i32 i32) ;; label = @2
        i32.const 100
        i32.const 5
      else
        i32.const 96
        i32.const 4
      end
      return
    end
    local.get 0
    i32.const 3
    i32.eq
    local.get 0
    i32.const 4
    i32.eq
    i32.or
    if ;; label = @1
      local.get 1
      i32.wrap_i64
      local.tee 0
      i32.const 8
      i32.add
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      return
    end
    i32.const 0
    i32.const 0
  )
  (data (;0;) (i32.const 96) "true")
  (data (;1;) (i32.const 100) "false")
)
