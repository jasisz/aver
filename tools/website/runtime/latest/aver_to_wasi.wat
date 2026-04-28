(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (result i32 i32)))
  (type (;3;) (func (param i64 i32) (result i32)))
  (type (;4;) (func (param f64 i32) (result i32)))
  (type (;5;) (func (param i32) (result i32)))
  (type (;6;) (func (param i64) (result i32)))
  (type (;7;) (func (param f64) (result i32)))
  (type (;8;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;9;) (func (param i32 i64 i32) (result i32)))
  (type (;10;) (func (param i32 i32 i32)))
  (type (;11;) (func (param i32 i32 i32 i32)))
  (type (;12;) (func (result i32)))
  (type (;13;) (func (param i32 i32 i32 i32 i32 i32 i32 i32) (result i64 i32 i32 i32)))
  (type (;14;) (func))
  (type (;15;) (func (param i32 i64)))
  (type (;16;) (func (param i32 i64) (result i32 i32)))
  (type (;17;) (func (param i64 i64) (result i64)))
  (type (;18;) (func (result f64)))
  (type (;19;) (func (result i64)))
  (type (;20;) (func (result i64 i32 i32 i32)))
  (import "aver_runtime" "rt_int_to_str" (func (;0;) (type 3)))
  (import "aver_runtime" "rt_float_to_str" (func (;1;) (type 4)))
  (import "aver_runtime" "rt_alloc" (func (;2;) (type 5)))
  (import "aver_runtime" "rt_i64_to_str_obj" (func (;3;) (type 6)))
  (import "aver_runtime" "rt_f64_to_str_obj" (func (;4;) (type 7)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;5;) (type 8)))
  (import "wasi_snapshot_preview1" "random_get" (func (;6;) (type 0)))
  (import "wasi_snapshot_preview1" "clock_time_get" (func (;7;) (type 9)))
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func (;8;) (type 0)))
  (import "wasi_snapshot_preview1" "environ_get" (func (;9;) (type 0)))
  (import "aver_runtime" "memory" (memory (;0;) 1))
  (export "console_print" (func 11))
  (export "console_error" (func 12))
  (export "console_warn" (func 12))
  (export "env_get" (func 13))
  (export "env_set" (func 15))
  (export "request_headers_load" (func 16))
  (export "http_send" (func 17))
  (export "http_clear_request_headers" (func 18))
  (export "http_add_request_header" (func 15))
  (export "print_value" (func 19))
  (export "format_value" (func 20))
  (export "random_int" (func 21))
  (export "random_float" (func 22))
  (export "time_unixMs" (func 23))
  (func (;10;) (type 10) (param i32 i32 i32)
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
    call 5
    drop
  )
  (func (;11;) (type 1) (param i32 i32)
    i32.const 1
    local.get 0
    local.get 1
    call 10
  )
  (func (;12;) (type 1) (param i32 i32)
    i32.const 2
    local.get 0
    local.get 1
    call 10
  )
  (func (;13;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 8
    call 2
    local.tee 2
    local.get 2
    i32.const 4
    i32.add
    local.tee 3
    call 8
    drop
    local.get 3
    i32.load
    local.set 3
    local.get 2
    i32.load
    local.tee 5
    i32.eqz
    if ;; label = @1
      i32.const -1
      return
    end
    local.get 5
    i32.const 2
    i32.shl
    call 2
    local.tee 6
    local.get 3
    call 2
    call 9
    drop
    i32.const 0
    local.set 3
    loop ;; label = @1
      local.get 3
      local.get 5
      i32.lt_u
      if ;; label = @2
        block (result i32) ;; label = @3
          local.get 6
          local.get 3
          i32.const 2
          i32.shl
          i32.add
          i32.load
          local.set 4
          i32.const 0
          local.set 2
          loop ;; label = @4
            block ;; label = @5
              local.get 1
              local.get 2
              i32.le_u
              br_if 0 (;@5;)
              local.get 2
              local.get 4
              i32.add
              i32.load8_u
              local.get 0
              local.get 2
              i32.add
              i32.load8_u
              i32.ne
              br_if 0 (;@5;)
              local.get 2
              i32.const 1
              i32.add
              local.set 2
              br 1 (;@4;)
            end
          end
          i32.const 0
          local.get 1
          local.get 2
          i32.gt_u
          br_if 0 (;@3;)
          drop
          local.get 1
          local.get 4
          i32.add
          i32.load8_u
          i32.const 61
          i32.eq
        end
        if ;; label = @3
          local.get 1
          local.get 4
          i32.add
          i32.const 1
          i32.add
          local.tee 0
          i32.const 0
          local.set 1
          loop ;; label = @4
            local.get 0
            local.get 1
            i32.add
            i32.load8_u
            if ;; label = @5
              local.get 1
              i32.const 1
              i32.add
              local.set 1
              br 1 (;@4;)
            end
          end
          local.get 1
          call 14
          return
        else
          local.get 3
          i32.const 1
          i32.add
          local.set 3
          br 2 (;@1;)
        end
        unreachable
      end
    end
    i32.const -1
  )
  (func (;14;) (type 0) (param i32 i32) (result i32)
    (local i32)
    local.get 1
    i32.const 8
    i32.add
    call 2
    local.tee 2
    local.get 1
    i64.extend_i32_u
    i64.store
    local.get 2
    i32.const 8
    i32.add
    local.get 0
    local.get 1
    memory.copy
    local.get 2
  )
  (func (;15;) (type 11) (param i32 i32 i32 i32))
  (func (;16;) (type 12) (result i32)
    i32.const 0
  )
  (func (;17;) (type 13) (param i32 i32 i32 i32 i32 i32 i32 i32) (result i64 i32 i32 i32)
    i32.const 40
    call 2
    local.tee 0
    i32.const 72
    i32.store8
    local.get 0
    i32.const 1
    i32.add
    i32.const 116
    i32.store8
    local.get 0
    i32.const 2
    i32.add
    i32.const 116
    i32.store8
    local.get 0
    i32.const 3
    i32.add
    i32.const 112
    i32.store8
    local.get 0
    i32.const 4
    i32.add
    i32.const 46
    i32.store8
    local.get 0
    i32.const 5
    i32.add
    i32.const 42
    i32.store8
    local.get 0
    i32.const 6
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 7
    i32.add
    i32.const 110
    i32.store8
    local.get 0
    i32.const 8
    i32.add
    i32.const 111
    i32.store8
    local.get 0
    i32.const 9
    i32.add
    i32.const 116
    i32.store8
    local.get 0
    i32.const 10
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 11
    i32.add
    i32.const 97
    i32.store8
    local.get 0
    i32.const 12
    i32.add
    i32.const 118
    i32.store8
    local.get 0
    i32.const 13
    i32.add
    i32.const 97
    i32.store8
    local.get 0
    i32.const 14
    i32.add
    i32.const 105
    i32.store8
    local.get 0
    i32.const 15
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 16
    i32.add
    i32.const 97
    i32.store8
    local.get 0
    i32.const 17
    i32.add
    i32.const 98
    i32.store8
    local.get 0
    i32.const 18
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 19
    i32.add
    i32.const 101
    i32.store8
    local.get 0
    i32.const 20
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 21
    i32.add
    i32.const 117
    i32.store8
    local.get 0
    i32.const 22
    i32.add
    i32.const 110
    i32.store8
    local.get 0
    i32.const 23
    i32.add
    i32.const 100
    i32.store8
    local.get 0
    i32.const 24
    i32.add
    i32.const 101
    i32.store8
    local.get 0
    i32.const 25
    i32.add
    i32.const 114
    i32.store8
    local.get 0
    i32.const 26
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 27
    i32.add
    i32.const 45
    i32.store8
    local.get 0
    i32.const 28
    i32.add
    i32.const 45
    i32.store8
    local.get 0
    i32.const 29
    i32.add
    i32.const 98
    i32.store8
    local.get 0
    i32.const 30
    i32.add
    i32.const 114
    i32.store8
    local.get 0
    i32.const 31
    i32.add
    i32.const 105
    i32.store8
    local.get 0
    i32.const 32
    i32.add
    i32.const 100
    i32.store8
    local.get 0
    i32.const 33
    i32.add
    i32.const 103
    i32.store8
    local.get 0
    i32.const 34
    i32.add
    i32.const 101
    i32.store8
    local.get 0
    i32.const 35
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 36
    i32.add
    i32.const 119
    i32.store8
    local.get 0
    i32.const 37
    i32.add
    i32.const 97
    i32.store8
    local.get 0
    i32.const 38
    i32.add
    i32.const 115
    i32.store8
    local.get 0
    i32.const 39
    i32.add
    i32.const 105
    i32.store8
    i64.const 0
    i32.const 0
    i32.const 0
    local.get 0
    i32.const 40
    call 14
  )
  (func (;18;) (type 14))
  (func (;19;) (type 15) (param i32 i64)
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
      call 10
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
      call 10
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
        call 10
      else
        i32.const 1
        i32.const 96
        i32.const 4
        call 10
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
      call 10
    end
  )
  (func (;20;) (type 16) (param i32 i64) (result i32 i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      local.get 1
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
    i32.const 1
    i32.eq
    if ;; label = @1
      local.get 1
      f64.reinterpret_i64
      call 4
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
      if (type 2) (result i32 i32) ;; label = @2
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
  (func (;21;) (type 17) (param i64 i64) (result i64)
    (local i64)
    i32.const 16
    i32.const 8
    call 6
    drop
    i32.const 16
    i64.load
    local.set 2
    local.get 1
    local.get 0
    i64.sub
    i64.const 1
    i64.add
    local.tee 1
    i64.eqz
    if (result i64) ;; label = @1
      local.get 0
    else
      local.get 0
      local.get 2
      local.get 1
      i64.rem_u
      i64.add
    end
  )
  (func (;22;) (type 18) (result f64)
    i32.const 16
    i32.const 8
    call 6
    drop
    i32.const 16
    i64.load
    i64.const 11
    i64.shr_u
    f64.convert_i64_u
    f64.const 0x1p-53 (;=0.00000000000000011102230246251565;)
    f64.mul
  )
  (func (;23;) (type 19) (result i64)
    i32.const 0
    i64.const 1000000
    i32.const 16
    call 7
    drop
    i32.const 16
    i64.load
    i64.const 1000000
    i64.div_u
  )
  (data (;0;) (i32.const 96) "true")
  (data (;1;) (i32.const 100) "false")
)
