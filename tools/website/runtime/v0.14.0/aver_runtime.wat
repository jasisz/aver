(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32) (result i32)))
  (type (;3;) (func (param i32) (result i64)))
  (type (;4;) (func (param i64) (result i32)))
  (type (;5;) (func (param i32 i64 i32) (result i32)))
  (type (;6;) (func (param i32 i64) (result i32)))
  (type (;7;) (func (param i32)))
  (type (;8;) (func (param i32 i32) (result i64)))
  (type (;9;) (func (param f64 i32) (result i32)))
  (type (;10;) (func (param i64 i32) (result i32)))
  (type (;11;) (func (param i64 i64 i32) (result i32)))
  (type (;12;) (func (param i32 i64 i64) (result i32)))
  (type (;13;) (func (param i32 i32) (result f64)))
  (type (;14;) (func (param i32) (result f64)))
  (type (;15;) (func (param i32 f64) (result i32)))
  (type (;16;) (func (param i64 i32 i32) (result i32)))
  (type (;17;) (func (param i32 i64 i64 i32 i32) (result i32)))
  (type (;18;) (func (param i32 i32 i32 i64 i32) (result i32)))
  (type (;19;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;20;) (func (param i32 i32 i32 i64 i32 i64 i32) (result i32)))
  (type (;21;) (func (param i32 i64 i32 i64 i32) (result i32)))
  (type (;22;) (func (param f64) (result i32)))
  (type (;23;) (func))
  (memory (;0;) 1)
  (global (;0;) (mut i32) i32.const 128)
  (global (;1;) (mut i32) i32.const 0)
  (global (;2;) (mut i32) i32.const 0)
  (global (;3;) (mut i32) i32.const 0)
  (global (;4;) (mut i32) i32.const 0)
  (export "memory" (memory 0))
  (export "heap_ptr" (global 0))
  (export "collect_mark" (global 1))
  (export "collect_from" (global 2))
  (export "collect_dst" (global 3))
  (export "rt_alloc" (func 0))
  (export "rt_truncate" (func 1))
  (export "rt_obj_kind" (func 2))
  (export "rt_obj_tag" (func 3))
  (export "rt_obj_meta" (func 4))
  (export "rt_obj_field" (func 5))
  (export "rt_obj_field_f64" (func 6))
  (export "rt_obj_field_i32" (func 7))
  (export "rt_unwrap" (func 8))
  (export "rt_unwrap_f64" (func 9))
  (export "rt_unwrap_i32" (func 10))
  (export "rt_wrap" (func 11))
  (export "rt_wrap_f64" (func 12))
  (export "rt_wrap_i32" (func 13))
  (export "rt_str_eq" (func 14))
  (export "rt_str_concat" (func 15))
  (export "rt_list_cons" (func 16))
  (export "rt_list_cons_f64" (func 17))
  (export "rt_str_byte_len" (func 18))
  (export "rt_str_find" (func 19))
  (export "rt_str_starts_with" (func 20))
  (export "rt_str_ends_with" (func 21))
  (export "rt_str_contains" (func 22))
  (export "rt_list_take" (func 23))
  (export "rt_list_drop" (func 24))
  (export "rt_list_reverse" (func 25))
  (export "rt_list_concat" (func 26))
  (export "rt_list_contains" (func 27))
  (export "rt_list_zip" (func 28))
  (export "rt_map_get" (func 46))
  (export "rt_map_has" (func 47))
  (export "rt_map_set" (func 48))
  (export "rt_map_from_list" (func 49))
  (export "rt_map_keys" (func 51))
  (export "rt_map_entries" (func 53))
  (export "rt_map_len" (func 54))
  (export "rt_vec_len" (func 18))
  (export "rt_vec_from_list" (func 55))
  (export "rt_vec_get_cell" (func 56))
  (export "rt_vec_get" (func 57))
  (export "rt_vec_set" (func 58))
  (export "rt_vec_set_or_keep" (func 59))
  (export "rt_vec_new" (func 60))
  (export "rt_vec_to_list" (func 61))
  (export "rt_int_to_str" (func 62))
  (export "rt_float_to_str" (func 63))
  (export "rt_i64_to_str_obj" (func 64))
  (export "rt_f64_to_str_obj" (func 65))
  (export "rt_str_len" (func 66))
  (export "rt_char_to_code" (func 67))
  (export "rt_byte_to_hex" (func 68))
  (export "rt_byte_from_hex" (func 69))
  (export "rt_char_from_code" (func 70))
  (export "rt_str_char_at" (func 71))
  (export "rt_str_to_lower" (func 72))
  (export "rt_str_to_upper" (func 73))
  (export "rt_str_trim" (func 74))
  (export "rt_str_slice" (func 75))
  (export "rt_str_chars" (func 76))
  (export "rt_str_copy_range" (func 77))
  (export "rt_str_split" (func 78))
  (export "rt_str_join" (func 79))
  (export "rt_str_replace" (func 80))
  (export "rt_int_from_str" (func 81))
  (export "rt_float_from_str" (func 82))
  (export "rt_collect_begin" (func 83))
  (export "rt_rebase_i32" (func 84))
  (export "rt_collect_end" (func 85))
  (export "rt_retain_i32" (func 86))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    global.get 0
    local.get 0
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.add
    local.tee 0
    memory.size
    i32.const 16
    i32.shl
    local.tee 1
    i32.gt_u
    if ;; label = @1
      local.get 0
      local.get 1
      i32.sub
      i32.const 65535
      i32.add
      i32.const 16
      i32.shr_u
      memory.grow
      i32.const -1
      i32.eq
      if ;; label = @2
        unreachable
      end
    end
    global.get 0
    local.get 0
    global.set 0
  )
  (func (;1;) (type 7) (param i32)
    local.get 0
    global.set 0
  )
  (func (;2;) (type 0) (param i32) (result i32)
    local.get 0
    i64.load
    i64.const 56
    i64.shr_u
    i32.wrap_i64
  )
  (func (;3;) (type 0) (param i32) (result i32)
    local.get 0
    i64.load
    i64.const 48
    i64.shr_u
    i32.wrap_i64
    i32.const 255
    i32.and
  )
  (func (;4;) (type 0) (param i32) (result i32)
    local.get 0
    i64.load
    i64.const 32
    i64.shr_u
    i32.wrap_i64
    i32.const 65535
    i32.and
  )
  (func (;5;) (type 8) (param i32 i32) (result i64)
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.const 3
    i32.shl
    i32.add
    i64.load
  )
  (func (;6;) (type 13) (param i32 i32) (result f64)
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.const 3
    i32.shl
    i32.add
    f64.load
  )
  (func (;7;) (type 1) (param i32 i32) (result i32)
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.const 3
    i32.shl
    i32.add
    i64.load
    i32.wrap_i64
  )
  (func (;8;) (type 3) (param i32) (result i64)
    local.get 0
    i64.load offset=8
  )
  (func (;9;) (type 14) (param i32) (result f64)
    local.get 0
    f64.load offset=8
  )
  (func (;10;) (type 0) (param i32) (result i32)
    local.get 0
    i64.load offset=8
    i32.wrap_i64
  )
  (func (;11;) (type 5) (param i32 i64 i32) (result i32)
    (local i32)
    i32.const 16
    call 0
    local.tee 3
    local.get 0
    i64.extend_i32_u
    i64.const 48
    i64.shl
    i64.const 216172782113783808
    i64.or
    local.get 2
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.or
    i64.const 1
    i64.or
    i64.store
    local.get 3
    local.get 1
    i64.store offset=8
    local.get 3
  )
  (func (;12;) (type 15) (param i32 f64) (result i32)
    (local i32)
    i32.const 16
    call 0
    local.tee 2
    local.get 0
    i64.extend_i32_u
    i64.const 48
    i64.shl
    i64.const 504403158265495553
    i64.or
    i64.store
    local.get 2
    local.get 1
    f64.store offset=8
    local.get 2
  )
  (func (;13;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    i32.const 16
    call 0
    local.tee 3
    local.get 0
    i64.extend_i32_u
    i64.const 48
    i64.shl
    i64.const 576460752303423488
    i64.or
    local.get 2
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.or
    i64.const 1
    i64.or
    i64.store
    local.get 3
    local.get 1
    i64.extend_i32_s
    i64.store offset=8
    local.get 3
  )
  (func (;14;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.eq
    if (result i32) ;; label = @1
      i32.const 1
    else
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      local.tee 3
      local.get 1
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      i32.ne
      if (result i32) ;; label = @2
        i32.const 0
      else
        loop ;; label = @3
          local.get 2
          local.get 3
          i32.lt_u
          if ;; label = @4
            local.get 0
            local.get 2
            i32.add
            i32.load8_u offset=8
            local.get 1
            local.get 2
            i32.add
            i32.load8_u offset=8
            i32.ne
            if ;; label = @5
              i32.const 0
              return
            else
              local.get 2
              i32.const 1
              i32.add
              local.set 2
              br 2 (;@3;)
            end
            unreachable
          end
        end
        i32.const 1
      end
    end
  )
  (func (;15;) (type 1) (param i32 i32) (result i32)
    (local i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 3
    local.get 1
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 5
    i32.add
    local.tee 2
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 4
    local.get 2
    i64.extend_i32_u
    i64.store
    local.get 4
    i32.const 8
    i32.add
    local.tee 2
    local.get 0
    i32.const 8
    i32.add
    local.get 3
    memory.copy
    local.get 2
    local.get 3
    i32.add
    local.get 1
    i32.const 8
    i32.add
    local.get 5
    memory.copy
    local.get 4
  )
  (func (;16;) (type 16) (param i64 i32 i32) (result i32)
    (local i32)
    i32.const 24
    call 0
    local.tee 3
    local.get 2
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.const 288230376151711746
    i64.or
    i64.store
    local.get 3
    local.get 0
    i64.store offset=8
    local.get 3
    local.get 1
    i64.extend_i32_s
    i64.store offset=16
    local.get 3
  )
  (func (;17;) (type 9) (param f64 i32) (result i32)
    (local i32)
    i32.const 24
    call 0
    local.tee 2
    i64.const 648518346341351426
    i64.store
    local.get 2
    local.get 0
    f64.store offset=8
    local.get 2
    local.get 1
    i64.extend_i32_s
    i64.store offset=16
    local.get 2
  )
  (func (;18;) (type 3) (param i32) (result i64)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
  )
  (func (;19;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 3
    local.get 2
    i32.const 0
    local.get 2
    i32.const 0
    i32.ge_s
    select
    local.set 2
    local.get 1
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 4
    i32.eqz
    if ;; label = @1
      local.get 2
      local.get 3
      local.get 2
      local.get 3
      i32.le_u
      select
      return
    end
    local.get 3
    local.get 4
    i32.lt_u
    if ;; label = @1
      i32.const -1
      return
    end
    local.get 3
    local.get 4
    i32.sub
    local.tee 5
    local.get 2
    i32.lt_u
    if ;; label = @1
      i32.const -1
      return
    end
    loop ;; label = @1
      local.get 2
      local.get 5
      i32.le_u
      if ;; label = @2
        i32.const 0
        local.set 3
        loop (result i32) ;; label = @3
          local.get 3
          local.get 4
          i32.lt_u
          if (result i32) ;; label = @4
            local.get 0
            local.get 2
            i32.add
            local.get 3
            i32.add
            i32.load8_u offset=8
            local.get 1
            local.get 3
            i32.add
            i32.load8_u offset=8
            i32.ne
            if (result i32) ;; label = @5
              i32.const 0
            else
              local.get 3
              i32.const 1
              i32.add
              local.set 3
              br 2 (;@3;)
            end
          else
            i32.const 1
          end
        end
        if ;; label = @3
          local.get 2
          return
        else
          local.get 2
          i32.const 1
          i32.add
          local.set 2
          br 2 (;@1;)
        end
        unreachable
      end
    end
    i32.const -1
  )
  (func (;20;) (type 1) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.const 0
    call 19
    i32.eqz
  )
  (func (;21;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 2
    local.get 1
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 3
    i32.lt_u
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    i32.sub
    local.tee 0
    call 19
    local.get 0
    i32.eq
  )
  (func (;22;) (type 1) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.const 0
    call 19
    i32.const -1
    i32.ne
  )
  (func (;23;) (type 1) (param i32 i32) (result i32)
    (local i32)
    loop ;; label = @1
      local.get 1
      i32.eqz
      local.get 0
      i32.eqz
      i32.or
      i32.eqz
      if ;; label = @2
        local.get 0
        i32.const 0
        call 5
        local.get 2
        local.get 0
        call 4
        i32.const 1
        i32.and
        call 16
        local.set 2
        local.get 0
        i32.const 1
        call 7
        local.set 0
        local.get 1
        i32.const 1
        i32.sub
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 2
    call 25
  )
  (func (;24;) (type 1) (param i32 i32) (result i32)
    loop ;; label = @1
      local.get 1
      i32.eqz
      local.get 0
      i32.eqz
      i32.or
      i32.eqz
      if ;; label = @2
        local.get 0
        i64.load offset=16
        i32.wrap_i64
        local.set 0
        local.get 1
        i32.const 1
        i32.sub
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 0
  )
  (func (;25;) (type 0) (param i32) (result i32)
    (local i32)
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 0
        i32.const 0
        call 5
        local.get 1
        local.get 0
        call 4
        i32.const 1
        i32.and
        call 16
        local.set 1
        local.get 0
        i32.const 1
        call 7
        local.set 0
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;26;) (type 1) (param i32 i32) (result i32)
    local.get 0
    call 25
    local.set 0
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 0
        i32.const 0
        call 5
        local.get 1
        local.get 0
        call 4
        i32.const 1
        i32.and
        call 16
        local.set 1
        local.get 0
        i32.const 1
        call 7
        local.set 0
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;27;) (type 6) (param i32 i64) (result i32)
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 0
        i32.const 0
        call 5
        local.get 1
        i64.eq
        if ;; label = @3
          i32.const 1
          return
        else
          local.get 0
          i32.const 1
          call 7
          local.set 0
          br 2 (;@1;)
        end
        unreachable
      end
    end
    i32.const 0
  )
  (func (;28;) (type 1) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i64 i64)
    loop ;; label = @1
      local.get 0
      i32.eqz
      local.get 1
      i32.eqz
      i32.or
      i32.eqz
      if ;; label = @2
        local.get 0
        i32.const 0
        call 5
        local.set 6
        local.get 1
        i32.const 0
        call 5
        local.set 7
        local.get 0
        call 4
        i32.const 1
        i32.and
        local.set 4
        local.get 1
        call 4
        i32.const 1
        i32.and
        local.set 5
        i32.const 24
        call 0
        local.tee 2
        local.get 4
        local.get 5
        i32.const 1
        i32.shl
        i32.or
        i64.extend_i32_u
        i64.const 32
        i64.shl
        i64.const 360287970189639682
        i64.or
        i64.store
        local.get 2
        local.get 6
        i64.store offset=8
        local.get 2
        local.get 7
        i64.store offset=16
        local.get 2
        i64.extend_i32_u
        local.get 3
        i32.const 1
        call 16
        local.set 3
        local.get 0
        i32.const 1
        call 7
        local.set 0
        local.get 1
        i32.const 1
        call 7
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 3
    call 25
  )
  (func (;29;) (type 4) (param i64) (result i32)
    local.get 0
    local.get 0
    i64.const 33
    i64.shr_u
    i64.xor
    i64.const -49064778989728563
    i64.mul
    local.tee 0
    i64.const 33
    i64.shr_u
    local.get 0
    i64.xor
    i64.const -4265267296055464877
    i64.mul
    local.tee 0
    i64.const 33
    i64.shr_u
    local.get 0
    i64.xor
    i32.wrap_i64
  )
  (func (;30;) (type 0) (param i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 3
    i32.const 5381
    local.set 1
    loop ;; label = @1
      local.get 2
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.add
        i32.load8_u
        local.get 1
        i32.const 5
        i32.shl
        local.get 1
        i32.add
        i32.add
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;31;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i64)
    block (result i32) ;; label = @1
      i32.const 0
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      drop
      local.get 0
      call 2
      local.set 1
      local.get 0
      call 3
      local.set 2
      local.get 0
      call 4
      local.set 3
      local.get 1
      i32.eqz
      if ;; label = @2
        local.get 0
        call 30
        br 1 (;@1;)
      end
      block ;; label = @2
        local.get 1
        i32.const 4
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 0
          call 5
          local.set 5
          local.get 3
          i32.const 1
          i32.and
          if (result i32) ;; label = @4
            local.get 5
            i32.wrap_i64
            call 31
          else
            local.get 5
            call 29
          end
          local.set 1
          br 1 (;@2;)
        end
        local.get 1
        i32.const 9
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 0
          call 6
          i64.reinterpret_f64
          call 29
          local.set 1
          br 1 (;@2;)
        end
        local.get 1
        i32.const 5
        i32.eq
        local.get 1
        i32.const 1
        i32.eq
        i32.or
        local.get 1
        i32.const 2
        i32.eq
        i32.or
        if ;; label = @3
          local.get 0
          i64.load
          i64.const 4294967295
          i64.and
          i32.wrap_i64
          local.set 4
          local.get 1
          i32.const 8
          i32.shl
          local.get 2
          i32.or
          local.set 2
          i32.const 0
          local.set 1
          loop ;; label = @4
            local.get 1
            local.get 4
            i32.lt_u
            if ;; label = @5
              local.get 0
              local.get 1
              call 5
              local.set 5
              local.get 3
              local.get 1
              i32.shr_u
              i32.const 1
              i32.and
              if (result i32) ;; label = @6
                local.get 5
                i32.wrap_i64
                call 31
              else
                local.get 5
                call 29
              end
              local.get 2
              i32.const 5
              i32.rotl
              i32.xor
              local.set 2
              local.get 1
              i32.const 1
              i32.add
              local.set 1
              br 1 (;@4;)
            end
          end
          local.get 2
          br 2 (;@1;)
        end
        local.get 1
        i32.const 3
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 0
          call 5
          local.set 5
          local.get 3
          i32.const 1
          i32.and
          if (result i32) ;; label = @4
            local.get 5
            i32.wrap_i64
            call 31
          else
            local.get 5
            call 29
          end
          local.get 2
          i32.const 11
          i32.shl
          i32.xor
          br 2 (;@1;)
        end
        local.get 1
        i32.const 7
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 0
          call 6
          i64.reinterpret_f64
          call 29
          local.get 2
          i32.const 11
          i32.shl
          i32.xor
          br 2 (;@1;)
        end
        local.get 1
        i32.const 8
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 0
          call 7
          call 31
          local.get 2
          i32.const 11
          i32.shl
          i32.xor
          br 2 (;@1;)
        end
        local.get 0
        i64.extend_i32_u
        call 29
        br 1 (;@1;)
      end
      local.get 0
      i32.const 1
      call 7
      call 31
      local.get 1
      i32.const 16
      i32.rotl
      i32.xor
    end
  )
  (func (;32;) (type 1) (param i32 i32) (result i32)
    (local i32 i32 i32 i64 i64)
    block (result i32) ;; label = @1
      i32.const 1
      local.get 0
      local.get 1
      i32.eq
      br_if 0 (;@1;)
      drop
      block ;; label = @2
        block ;; label = @3
          local.get 0
          i32.eqz
          local.get 1
          i32.eqz
          i32.or
          br_if 0 (;@3;)
          local.get 0
          call 2
          local.tee 2
          local.get 1
          call 2
          i32.ne
          br_if 0 (;@3;)
          local.get 2
          i32.eqz
          if ;; label = @4
            local.get 0
            local.get 1
            call 14
            br 3 (;@1;)
          end
          local.get 0
          call 3
          local.get 1
          call 3
          i32.ne
          br_if 0 (;@3;)
          local.get 2
          i32.const 4
          i32.eq
          if ;; label = @4
            local.get 0
            call 4
            local.get 0
            i32.const 0
            call 5
            local.set 5
            local.get 1
            i32.const 0
            call 5
            local.set 6
            i32.const 1
            i32.and
            if (result i32) ;; label = @5
              local.get 5
              i32.wrap_i64
              local.get 6
              i32.wrap_i64
              call 32
            else
              local.get 5
              local.get 6
              i64.eq
            end
            i32.eqz
            br_if 1 (;@3;)
            br 2 (;@2;)
          end
          local.get 2
          i32.const 9
          i32.eq
          if ;; label = @4
            local.get 0
            i32.const 0
            call 6
            local.get 1
            i32.const 0
            call 6
            f64.ne
            br_if 1 (;@3;)
            br 2 (;@2;)
          end
          local.get 2
          i32.const 5
          i32.eq
          local.get 2
          i32.const 1
          i32.eq
          i32.or
          local.get 2
          i32.const 2
          i32.eq
          i32.or
          if ;; label = @4
            local.get 0
            call 4
            local.set 3
            local.get 0
            i64.load
            i64.const 4294967295
            i64.and
            i32.wrap_i64
            local.set 4
            i32.const 0
            local.set 2
            loop ;; label = @5
              local.get 2
              local.get 4
              i32.lt_u
              if ;; label = @6
                local.get 0
                local.get 2
                call 5
                local.set 5
                local.get 1
                local.get 2
                call 5
                local.set 6
                local.get 3
                local.get 2
                i32.shr_u
                i32.const 1
                i32.and
                if (result i32) ;; label = @7
                  local.get 5
                  i32.wrap_i64
                  local.get 6
                  i32.wrap_i64
                  call 32
                else
                  local.get 5
                  local.get 6
                  i64.eq
                end
                i32.eqz
                br_if 3 (;@3;)
                local.get 2
                i32.const 1
                i32.add
                local.set 2
                br 1 (;@5;)
              end
            end
            i32.const 1
            br 3 (;@1;)
          end
          local.get 2
          i32.const 3
          i32.eq
          if ;; label = @4
            local.get 0
            call 4
            local.get 0
            i32.const 0
            call 5
            local.set 5
            local.get 1
            i32.const 0
            call 5
            local.set 6
            i32.const 1
            i32.and
            if (result i32) ;; label = @5
              local.get 5
              i32.wrap_i64
              local.get 6
              i32.wrap_i64
              call 32
            else
              local.get 5
              local.get 6
              i64.eq
            end
            br 3 (;@1;)
          end
          local.get 2
          i32.const 7
          i32.eq
          if ;; label = @4
            local.get 0
            i32.const 0
            call 6
            local.get 1
            i32.const 0
            call 6
            f64.eq
            br 3 (;@1;)
          end
          local.get 2
          i32.const 8
          i32.eq
          if ;; label = @4
            local.get 0
            i32.const 0
            call 7
            local.get 1
            i32.const 0
            call 7
            call 32
            br 3 (;@1;)
          end
          i32.const 0
          br 2 (;@1;)
        end
        i32.const 0
        br 1 (;@1;)
      end
      local.get 0
      i32.const 1
      call 7
      local.get 1
      i32.const 1
      call 7
      call 32
    end
  )
  (func (;33;) (type 10) (param i64 i32) (result i32)
    local.get 1
    i32.const 3
    i32.eq
    if ;; label = @1
      local.get 0
      i32.wrap_i64
      call 30
      return
    end
    local.get 1
    i32.const 4
    i32.eq
    if ;; label = @1
      local.get 0
      i32.wrap_i64
      call 31
      return
    end
    local.get 0
    call 29
  )
  (func (;34;) (type 11) (param i64 i64 i32) (result i32)
    local.get 2
    i32.const 3
    i32.eq
    if ;; label = @1
      local.get 0
      i32.wrap_i64
      local.get 1
      i32.wrap_i64
      call 14
      return
    end
    local.get 2
    i32.const 4
    i32.eq
    if ;; label = @1
      local.get 0
      i32.wrap_i64
      local.get 1
      i32.wrap_i64
      call 32
      return
    end
    local.get 0
    local.get 1
    i64.eq
  )
  (func (;35;) (type 0) (param i32) (result i32)
    (local i32)
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 1
        local.get 0
        i32.const 1
        i32.and
        i32.add
        local.set 1
        local.get 0
        i32.const 1
        i32.shr_u
        local.set 0
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;36;) (type 17) (param i32 i64 i64 i32 i32) (result i32)
    (local i32)
    i32.const 32
    call 0
    local.tee 5
    local.get 3
    i32.const 3
    i32.eq
    local.get 3
    i32.const 4
    i32.eq
    i32.or
    i32.const 1
    i32.shl
    local.get 4
    i32.const 2
    i32.shl
    i32.or
    local.get 3
    i32.const 7
    i32.and
    i32.const 4
    i32.shl
    i32.or
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.const 1008806316530991107
    i64.or
    i64.store
    local.get 5
    local.get 0
    i64.extend_i32_u
    i64.store offset=8
    local.get 5
    local.get 1
    i64.store offset=16
    local.get 5
    local.get 2
    i64.store offset=24
    local.get 5
  )
  (func (;37;) (type 1) (param i32 i32) (result i32)
    (local i32)
    local.get 1
    i32.const 3
    i32.shl
    i32.const 8
    i32.add
    call 0
    local.tee 2
    local.get 1
    i64.extend_i32_u
    local.get 0
    i32.const 65535
    i32.and
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.const 936748722493063168
    i64.or
    i64.or
    i64.store
    local.get 2
  )
  (func (;38;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 3
    i32.shl
    i32.const 8
    i32.add
    call 0
    local.tee 1
    local.get 0
    i64.extend_i32_u
    i64.const 1080863910568919040
    i64.or
    i64.store
    local.get 1
  )
  (func (;39;) (type 0) (param i32) (result i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
  )
  (func (;40;) (type 18) (param i32 i32 i32 i64 i32) (result i32)
    (local i32 i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    call 2
    local.tee 5
    i32.const 14
    i32.eq
    if ;; label = @1
      local.get 0
      call 10
      local.get 1
      i32.eq
      if ;; label = @2
        local.get 0
        i64.load offset=16
        local.get 3
        local.get 4
        call 34
        if ;; label = @3
          local.get 0
          return
        end
      end
      i32.const 0
      return
    end
    local.get 5
    i32.const 15
    i32.eq
    if ;; label = @1
      local.get 0
      call 39
      local.set 2
      i32.const 0
      local.set 1
      loop ;; label = @2
        local.get 1
        local.get 2
        i32.lt_u
        if ;; label = @3
          local.get 0
          local.get 1
          call 7
          local.tee 5
          i64.load offset=16
          local.get 3
          local.get 4
          call 34
          if ;; label = @4
            local.get 5
            return
          else
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          unreachable
        end
      end
      i32.const 0
      return
    end
    local.get 0
    call 4
    local.tee 5
    i32.const 1
    local.get 1
    local.get 2
    i32.const 2
    i32.shl
    i32.shr_u
    i32.const 15
    i32.and
    i32.shl
    local.tee 6
    i32.and
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    local.get 5
    local.get 6
    i32.const 1
    i32.sub
    i32.and
    call 35
    call 7
    local.get 1
    local.get 2
    i32.const 1
    i32.add
    local.get 3
    local.get 4
    call 40
  )
  (func (;41;) (type 19) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    if (result i32) ;; label = @1
      local.get 0
      call 4
      call 35
    else
      i32.const 0
    end
    drop
    local.get 1
    local.get 1
    call 35
    local.tee 7
    call 37
    local.set 6
    i32.const 0
    local.set 1
    loop ;; label = @1
      local.get 1
      local.get 7
      i32.lt_u
      if ;; label = @2
        local.get 1
        local.get 2
        i32.eq
        if (result i32) ;; label = @3
          local.get 6
          i32.const 8
          i32.add
          local.get 1
          i32.const 3
          i32.shl
          i32.add
          local.get 3
          i64.extend_i32_u
          i64.store
          local.get 5
          local.get 5
          i32.const 1
          i32.add
          local.get 4
          select
        else
          local.get 6
          i32.const 8
          i32.add
          local.get 1
          i32.const 3
          i32.shl
          i32.add
          local.get 0
          local.get 5
          call 7
          i64.extend_i32_u
          i64.store
          local.get 5
          i32.const 1
          i32.add
        end
        local.set 5
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 6
  )
  (func (;42;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    call 10
    local.tee 3
    local.get 1
    call 10
    local.tee 4
    i32.eq
    if ;; label = @1
      i32.const 2
      call 38
      local.tee 2
      i32.const 8
      i32.add
      local.get 0
      i64.extend_i32_u
      i64.store
      local.get 2
      i32.const 16
      i32.add
      local.get 1
      i64.extend_i32_u
      i64.store
      local.get 2
      return
    end
    local.get 3
    local.get 2
    i32.const 2
    i32.shl
    local.tee 5
    i32.shr_u
    i32.const 15
    i32.and
    local.tee 3
    local.get 4
    local.get 5
    i32.shr_u
    i32.const 15
    i32.and
    local.tee 4
    i32.eq
    if ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      i32.const 1
      i32.add
      call 42
      local.set 0
      i32.const 1
      local.get 3
      i32.shl
      i32.const 1
      call 37
      local.tee 1
      i32.const 8
      i32.add
      local.get 0
      i64.extend_i32_u
      i64.store
      local.get 1
      return
    end
    i32.const 1
    local.get 3
    i32.shl
    i32.const 1
    local.get 4
    i32.shl
    i32.or
    i32.const 2
    call 37
    local.set 2
    local.get 3
    local.get 4
    i32.lt_u
    if ;; label = @1
      local.get 2
      i32.const 8
      i32.add
      local.get 0
      i64.extend_i32_u
      i64.store
      local.get 2
      i32.const 16
      i32.add
      local.get 1
      i64.extend_i32_u
      i64.store
    else
      local.get 2
      i32.const 8
      i32.add
      local.get 1
      i64.extend_i32_u
      i64.store
      local.get 2
      i32.const 16
      i32.add
      local.get 0
      i64.extend_i32_u
      i64.store
    end
    local.get 2
  )
  (func (;43;) (type 20) (param i32 i32 i32 i64 i32 i64 i32) (result i32)
    (local i32 i32 i32)
    block ;; label = @1
      local.get 0
      i32.eqz
      if ;; label = @2
        i32.const 1
        global.set 4
        br 1 (;@1;)
      end
      local.get 0
      call 2
      local.tee 8
      i32.const 14
      i32.eq
      if ;; label = @2
        local.get 0
        call 10
        local.get 1
        i32.eq
        if ;; label = @3
          local.get 0
          i64.load offset=16
          local.get 3
          local.get 4
          call 34
          if ;; label = @4
            i32.const 0
            global.set 4
            br 3 (;@1;)
          end
          i32.const 1
          global.set 4
          i32.const 2
          call 38
          local.tee 2
          i32.const 8
          i32.add
          local.get 0
          i64.extend_i32_u
          i64.store
          local.get 2
          i32.const 16
          i32.add
          local.get 1
          local.get 3
          local.get 5
          local.get 4
          local.get 6
          call 36
          i64.extend_i32_u
          i64.store
          local.get 2
          return
        end
        i32.const 1
        global.set 4
        local.get 0
        local.get 1
        local.get 3
        local.get 5
        local.get 4
        local.get 6
        call 36
        local.get 2
        call 42
        return
      end
      local.get 8
      i32.const 15
      i32.eq
      if ;; label = @2
        local.get 0
        call 39
        local.set 8
        i32.const 0
        local.set 2
        loop ;; label = @3
          local.get 2
          local.get 8
          i32.lt_u
          if ;; label = @4
            local.get 0
            local.get 2
            call 7
            i64.load offset=16
            local.get 3
            local.get 4
            call 34
            if ;; label = @5
              i32.const 0
              global.set 4
              local.get 8
              call 38
              local.set 9
              loop ;; label = @6
                local.get 7
                local.get 8
                i32.lt_u
                if ;; label = @7
                  local.get 9
                  i32.const 8
                  i32.add
                  local.get 7
                  i32.const 3
                  i32.shl
                  i32.add
                  local.get 2
                  local.get 7
                  i32.eq
                  if (result i64) ;; label = @8
                    local.get 1
                    local.get 3
                    local.get 5
                    local.get 4
                    local.get 6
                    call 36
                    i64.extend_i32_u
                  else
                    local.get 0
                    local.get 7
                    call 5
                  end
                  i64.store
                  local.get 7
                  i32.const 1
                  i32.add
                  local.set 7
                  br 1 (;@6;)
                end
              end
              local.get 9
              return
            else
              local.get 2
              i32.const 1
              i32.add
              local.set 2
              br 2 (;@3;)
            end
            unreachable
          end
        end
        i32.const 1
        global.set 4
        local.get 8
        i32.const 1
        i32.add
        call 38
        local.set 7
        i32.const 0
        local.set 2
        loop ;; label = @3
          local.get 2
          local.get 8
          i32.lt_u
          if ;; label = @4
            local.get 7
            i32.const 8
            i32.add
            local.get 2
            i32.const 3
            i32.shl
            i32.add
            local.get 0
            local.get 2
            call 5
            i64.store
            local.get 2
            i32.const 1
            i32.add
            local.set 2
            br 1 (;@3;)
          end
        end
        local.get 7
        i32.const 8
        i32.add
        local.get 8
        i32.const 3
        i32.shl
        i32.add
        local.get 1
        local.get 3
        local.get 5
        local.get 4
        local.get 6
        call 36
        i64.extend_i32_u
        i64.store
        local.get 7
        return
      end
      local.get 0
      call 4
      local.tee 8
      i32.const 1
      local.get 1
      local.get 2
      i32.const 2
      i32.shl
      i32.shr_u
      i32.const 15
      i32.and
      i32.shl
      local.tee 9
      i32.const 1
      i32.sub
      i32.and
      call 35
      local.set 7
      local.get 8
      local.get 9
      i32.and
      i32.eqz
      if ;; label = @2
        i32.const 1
        global.set 4
        local.get 0
        local.get 8
        local.get 9
        i32.or
        local.get 7
        local.get 1
        local.get 3
        local.get 5
        local.get 4
        local.get 6
        call 36
        i32.const 1
        call 41
        return
      end
      local.get 0
      local.get 8
      local.get 7
      local.get 0
      local.get 7
      call 7
      local.get 1
      local.get 2
      i32.const 1
      i32.add
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      call 43
      i32.const 0
      call 41
      return
    end
    local.get 1
    local.get 3
    local.get 5
    local.get 4
    local.get 6
    call 36
  )
  (func (;44;) (type 0) (param i32) (result i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    i64.load offset=16
    i32.wrap_i64
  )
  (func (;45;) (type 0) (param i32) (result i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    i32.const 0
    call 7
  )
  (func (;46;) (type 5) (param i32 i64 i32) (result i32)
    (local i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      i32.const -1
      return
    end
    local.get 1
    local.get 2
    call 33
    local.set 3
    local.get 0
    call 45
    local.get 3
    i32.const 0
    local.get 1
    local.get 2
    call 40
    local.tee 0
    i32.eqz
    if ;; label = @1
      i32.const -1
      return
    end
    i32.const 2
    local.get 0
    i64.load offset=24
    local.get 0
    call 4
    i32.const 2
    i32.shr_u
    i32.const 1
    i32.and
    call 11
  )
  (func (;47;) (type 5) (param i32 i64 i32) (result i32)
    (local i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      i32.const 0
      return
    end
    local.get 1
    local.get 2
    call 33
    local.set 3
    local.get 0
    call 45
    local.get 3
    i32.const 0
    local.get 1
    local.get 2
    call 40
    i32.const 0
    i32.ne
  )
  (func (;48;) (type 21) (param i32 i64 i32 i64 i32) (result i32)
    (local i32 i32)
    local.get 1
    local.get 2
    call 33
    local.set 5
    local.get 0
    call 45
    local.get 0
    call 44
    local.set 0
    local.get 5
    i32.const 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call 43
    local.set 2
    local.get 0
    global.get 4
    i32.add
    local.set 4
    i32.const 24
    call 0
    local.tee 0
    i64.const 864691132750102530
    i64.store
    local.get 0
    local.get 2
    i64.extend_i32_u
    i64.store offset=8
    local.get 0
    local.get 4
    i64.extend_i32_u
    i64.store offset=16
    local.get 0
  )
  (func (;49;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 3
        local.get 0
        i64.load offset=8
        i32.wrap_i64
        local.tee 3
        i64.load offset=8
        local.get 1
        local.get 3
        i64.load offset=16
        local.get 2
        call 48
        local.set 3
        local.get 0
        i64.load offset=16
        i32.wrap_i64
        local.set 0
        br 1 (;@1;)
      end
    end
    local.get 3
  )
  (func (;50;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      local.get 1
      return
    end
    local.get 0
    call 2
    local.tee 2
    i32.const 14
    i32.eq
    if ;; label = @1
      local.get 0
      i64.load offset=16
      local.get 1
      local.get 0
      call 4
      i32.const 1
      i32.shr_u
      i32.const 1
      i32.and
      call 16
      return
    end
    local.get 2
    i32.const 15
    i32.eq
    if ;; label = @1
      local.get 0
      call 39
      local.set 3
      i32.const 0
      local.set 2
      loop ;; label = @2
        local.get 2
        local.get 3
        i32.lt_u
        if ;; label = @3
          local.get 0
          local.get 2
          call 7
          local.get 1
          call 50
          local.set 1
          local.get 2
          i32.const 1
          i32.add
          local.set 2
          br 1 (;@2;)
        end
      end
      local.get 1
      return
    end
    local.get 0
    call 4
    call 35
    local.set 3
    i32.const 0
    local.set 2
    loop ;; label = @1
      local.get 2
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 2
        call 7
        local.get 1
        call 50
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;51;) (type 0) (param i32) (result i32)
    local.get 0
    call 45
    i32.const 0
    call 50
  )
  (func (;52;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    i32.eqz
    if ;; label = @1
      local.get 1
      return
    end
    local.get 0
    call 2
    local.tee 2
    i32.const 14
    i32.eq
    if ;; label = @1
      local.get 0
      call 4
      i32.const 1
      i32.shr_u
      i32.const 3
      i32.and
      local.set 3
      i32.const 24
      call 0
      local.tee 2
      local.get 3
      i64.extend_i32_u
      i64.const 32
      i64.shl
      i64.const 360287970189639682
      i64.or
      i64.store
      local.get 2
      local.get 0
      i64.load offset=16
      i64.store offset=8
      local.get 2
      local.get 0
      i64.load offset=24
      i64.store offset=16
      local.get 2
      i64.extend_i32_u
      local.get 1
      i32.const 1
      call 16
      return
    end
    local.get 2
    i32.const 15
    i32.eq
    if ;; label = @1
      local.get 0
      call 39
      local.set 3
      i32.const 0
      local.set 2
      loop ;; label = @2
        local.get 2
        local.get 3
        i32.lt_u
        if ;; label = @3
          local.get 0
          local.get 2
          call 7
          local.get 1
          call 52
          local.set 1
          local.get 2
          i32.const 1
          i32.add
          local.set 2
          br 1 (;@2;)
        end
      end
      local.get 1
      return
    end
    local.get 0
    call 4
    call 35
    local.set 3
    i32.const 0
    local.set 2
    loop ;; label = @1
      local.get 2
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 2
        call 7
        local.get 1
        call 52
        local.set 1
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 1
  )
  (func (;53;) (type 0) (param i32) (result i32)
    local.get 0
    call 45
    i32.const 0
    call 52
  )
  (func (;54;) (type 3) (param i32) (result i64)
    local.get 0
    call 44
    i64.extend_i32_u
  )
  (func (;55;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.set 2
    loop ;; label = @1
      local.get 2
      if ;; label = @2
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 2
        i32.const 1
        call 7
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 3
    i32.const 3
    i32.shl
    i32.const 8
    i32.add
    call 0
    local.tee 2
    local.get 3
    i64.extend_i32_u
    local.get 1
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.const 720575940379279360
    i64.or
    i64.or
    i64.store
    i32.const 0
    local.set 1
    loop ;; label = @1
      local.get 0
      if ;; label = @2
        local.get 2
        local.get 1
        i32.const 3
        i32.shl
        i32.add
        local.get 0
        i32.const 0
        call 5
        i64.store offset=8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 0
        i32.const 1
        call 7
        local.set 0
        br 1 (;@1;)
      end
    end
    local.get 2
  )
  (func (;56;) (type 8) (param i32 i32) (result i64)
    loop (result i64) ;; label = @1
      local.get 0
      call 4
      i32.const 2
      i32.and
      i32.eqz
      if ;; label = @2
        local.get 0
        local.get 1
        i32.const 3
        i32.shl
        i32.add
        i64.load offset=8
        return
      end
      local.get 0
      i64.load offset=16
      local.get 1
      i64.extend_i32_s
      i64.eq
      if (result i64) ;; label = @2
        local.get 0
        i64.load offset=24
      else
        local.get 0
        i64.load offset=8
        i32.wrap_i64
        local.set 0
        br 1 (;@1;)
      end
    end
  )
  (func (;57;) (type 6) (param i32 i64) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 3
    local.get 0
    call 4
    local.set 4
    local.get 1
    i32.wrap_i64
    local.tee 2
    i32.const 0
    i32.lt_s
    local.get 2
    local.get 3
    i32.ge_s
    i32.or
    if (result i32) ;; label = @1
      i32.const -1
    else
      i32.const 2
      local.get 0
      local.get 2
      call 56
      local.get 4
      i32.const 1
      i32.and
      call 11
    end
  )
  (func (;58;) (type 12) (param i32 i64 i64) (result i32)
    (local i32 i32 i32)
    local.get 1
    i32.wrap_i64
    local.tee 3
    i32.const 0
    i32.lt_s
    local.get 3
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 4
    i32.ge_s
    i32.or
    if (result i32) ;; label = @1
      i32.const -1
    else
      local.get 0
      call 4
      i32.const 1
      i32.and
      local.set 5
      i32.const 32
      call 0
      local.tee 3
      local.get 4
      i64.extend_i32_u
      local.get 5
      i32.const 2
      i32.or
      i64.extend_i32_u
      i64.const 32
      i64.shl
      i64.const 720575940379279360
      i64.or
      i64.or
      i64.store
      local.get 3
      local.get 0
      i64.extend_i32_u
      i64.store offset=8
      local.get 3
      local.get 1
      i64.store offset=16
      local.get 3
      local.get 2
      i64.store offset=24
      i32.const 2
      local.get 3
      i64.extend_i32_u
      i32.const 1
      call 11
    end
  )
  (func (;59;) (type 12) (param i32 i64 i64) (result i32)
    (local i32 i32 i32)
    local.get 1
    i32.wrap_i64
    local.tee 3
    i32.const 0
    i32.lt_s
    local.get 3
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 4
    i32.ge_s
    i32.or
    if (result i32) ;; label = @1
      local.get 0
    else
      local.get 0
      call 4
      i32.const 1
      i32.and
      local.set 5
      i32.const 32
      call 0
      local.tee 3
      local.get 4
      i64.extend_i32_u
      local.get 5
      i32.const 2
      i32.or
      i64.extend_i32_u
      i64.const 32
      i64.shl
      i64.const 720575940379279360
      i64.or
      i64.or
      i64.store
      local.get 3
      local.get 0
      i64.extend_i32_u
      i64.store offset=8
      local.get 3
      local.get 1
      i64.store offset=16
      local.get 3
      local.get 2
      i64.store offset=24
      local.get 3
    end
  )
  (func (;60;) (type 11) (param i64 i64 i32) (result i32)
    (local i32 i32)
    local.get 0
    i32.wrap_i64
    local.tee 3
    i32.const 3
    i32.shl
    i32.const 8
    i32.add
    call 0
    local.tee 4
    local.get 3
    i64.extend_i32_u
    local.get 2
    i64.extend_i32_u
    i64.const 32
    i64.shl
    i64.const 720575940379279360
    i64.or
    i64.or
    i64.store
    i32.const 0
    local.set 2
    loop ;; label = @1
      local.get 2
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 4
        local.get 2
        i32.const 3
        i32.shl
        i32.add
        local.get 1
        i64.store offset=8
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 4
  )
  (func (;61;) (type 0) (param i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 1
    local.get 0
    call 4
    local.set 3
    loop ;; label = @1
      local.get 1
      if ;; label = @2
        local.get 0
        local.get 1
        i32.const 1
        i32.sub
        local.tee 1
        call 56
        local.get 2
        local.get 3
        i32.const 1
        i32.and
        call 16
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 2
  )
  (func (;62;) (type 10) (param i64 i32) (result i32)
    (local i32 i32)
    local.get 0
    i64.eqz
    if (result i32) ;; label = @1
      local.get 1
      i32.const 48
      i32.store8
      i32.const 1
    else
      i64.const 0
      local.get 0
      i64.sub
      local.get 0
      local.get 0
      i64.const 0
      i64.lt_s
      local.tee 3
      select
      local.set 0
      i32.const 21
      local.set 2
      loop ;; label = @2
        local.get 0
        i64.eqz
        i32.eqz
        if ;; label = @3
          local.get 2
          i32.const 1
          i32.sub
          local.tee 2
          local.get 1
          i32.add
          local.get 0
          i64.const 10
          i64.rem_u
          i32.wrap_i64
          i32.const 48
          i32.add
          i32.store8
          local.get 0
          i64.const 10
          i64.div_u
          local.set 0
          br 1 (;@2;)
        end
      end
      local.get 3
      if ;; label = @2
        local.get 2
        i32.const 1
        i32.sub
        local.tee 2
        local.get 1
        i32.add
        i32.const 45
        i32.store8
      end
      i32.const 21
      local.get 2
      i32.sub
      local.get 2
      i32.const 16
      i32.shl
      i32.or
    end
  )
  (func (;63;) (type 9) (param f64 i32) (result i32)
    (local i64 i32 i32 i32 i32 f64)
    i32.const 21
    local.set 3
    local.get 0
    f64.abs
    local.tee 7
    f64.floor
    i64.trunc_f64_s
    local.tee 2
    i64.eqz
    if ;; label = @1
      i32.const 20
      local.set 3
      local.get 1
      i32.const 20
      i32.add
      i32.const 48
      i32.store8
    else
      loop ;; label = @2
        local.get 2
        i64.eqz
        i32.eqz
        if ;; label = @3
          local.get 3
          i32.const 1
          i32.sub
          local.tee 3
          local.get 1
          i32.add
          local.get 2
          i64.const 10
          i64.rem_u
          i32.wrap_i64
          i32.const 48
          i32.add
          i32.store8
          local.get 2
          i64.const 10
          i64.div_u
          local.set 2
          br 1 (;@2;)
        end
      end
    end
    local.get 0
    f64.const 0x0p+0 (;=0;)
    f64.lt
    if ;; label = @1
      local.get 3
      i32.const 1
      i32.sub
      local.tee 3
      local.get 1
      i32.add
      i32.const 45
      i32.store8
    end
    local.get 7
    local.get 7
    f64.floor
    f64.eq
    if (result i32) ;; label = @1
      i32.const 21
      local.get 3
      i32.sub
      local.get 3
      i32.const 16
      i32.shl
      i32.or
    else
      f64.const 0x1p+0 (;=1;)
      local.set 0
      loop ;; label = @2
        block ;; label = @3
          local.get 5
          i32.const 1
          i32.add
          local.set 5
          local.get 7
          local.get 0
          f64.const 0x1.4p+3 (;=10;)
          f64.mul
          local.tee 0
          f64.mul
          f64.floor
          i64.trunc_f64_s
          local.tee 2
          f64.convert_i64_s
          local.get 0
          f64.div
          local.get 7
          f64.eq
          br_if 0 (;@3;)
          local.get 5
          i32.const 15
          i32.lt_s
          br_if 1 (;@2;)
        end
      end
      local.get 0
      i64.trunc_f64_s
      local.get 2
      local.get 0
      i64.trunc_f64_s
      i64.rem_s
      i64.add
      local.get 0
      i64.trunc_f64_s
      i64.rem_s
      local.set 2
      local.get 1
      i32.const 46
      i32.store8 offset=21
      local.get 5
      i32.const 21
      i32.add
      local.set 4
      local.get 5
      local.set 6
      loop ;; label = @2
        local.get 6
        if ;; label = @3
          local.get 1
          local.get 4
          i32.add
          local.get 2
          i64.const 10
          i64.rem_u
          i32.wrap_i64
          i32.const 48
          i32.add
          i32.store8
          local.get 2
          i64.const 10
          i64.div_u
          local.set 2
          local.get 4
          i32.const 1
          i32.sub
          local.set 4
          local.get 6
          i32.const 1
          i32.sub
          local.set 6
          br 1 (;@2;)
        end
      end
      local.get 5
      i32.const 22
      i32.add
      local.set 4
      loop ;; label = @2
        block ;; label = @3
          local.get 4
          i32.const 22
          i32.le_s
          br_if 0 (;@3;)
          local.get 1
          local.get 4
          i32.add
          i32.const 1
          i32.sub
          i32.load8_u
          i32.const 48
          i32.ne
          br_if 0 (;@3;)
          local.get 4
          i32.const 1
          i32.sub
          local.set 4
          br 1 (;@2;)
        end
      end
      local.get 4
      local.get 3
      i32.sub
      local.get 3
      i32.const 16
      i32.shl
      i32.or
    end
  )
  (func (;64;) (type 4) (param i64) (result i32)
    (local i32 i32 i32)
    local.get 0
    i32.const 16
    call 62
    local.tee 3
    i32.const 65535
    i32.and
    local.tee 1
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 2
    local.get 1
    i64.extend_i32_u
    i64.store
    local.get 2
    i32.const 8
    i32.add
    local.get 3
    i32.const 16
    i32.shr_u
    i32.const 16
    i32.add
    local.get 1
    memory.copy
    local.get 2
  )
  (func (;65;) (type 22) (param f64) (result i32)
    (local i32 i32 i32)
    local.get 0
    i32.const 48
    call 63
    local.tee 3
    i32.const 65535
    i32.and
    local.tee 1
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 2
    local.get 1
    i64.extend_i32_u
    i64.store
    local.get 2
    i32.const 8
    i32.add
    local.get 3
    i32.const 16
    i32.shr_u
    i32.const 48
    i32.add
    local.get 1
    memory.copy
    local.get 2
  )
  (func (;66;) (type 3) (param i32) (result i64)
    (local i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 4
    loop ;; label = @1
      local.get 1
      local.get 4
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 1
        i32.add
        i32.load8_u offset=8
        local.tee 2
        i32.const 128
        i32.lt_u
        if (result i32) ;; label = @3
          i32.const 1
        else
          i32.const 2
          i32.const 3
          i32.const 4
          local.get 2
          i32.const 240
          i32.lt_u
          select
          local.get 2
          i32.const 224
          i32.lt_u
          select
        end
        local.get 1
        i32.add
        local.set 1
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        br 1 (;@1;)
      end
    end
    local.get 3
    i64.extend_i32_u
  )
  (func (;67;) (type 3) (param i32) (result i64)
    (local i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 2
    i32.eqz
    if ;; label = @1
      unreachable
    end
    local.get 0
    i32.load8_u offset=8
    local.tee 1
    i32.const 128
    i32.ge_u
    if (result i32) ;; label = @1
      local.get 1
      i32.const 224
      i32.lt_u
      if (result i32) ;; label = @2
        local.get 2
        i32.const 2
        i32.lt_u
        if ;; label = @3
          unreachable
        end
        local.get 0
        i32.load8_u offset=9
        i32.const 63
        i32.and
        local.get 1
        i32.const 31
        i32.and
        i32.const 6
        i32.shl
        i32.or
      else
        local.get 1
        i32.const 240
        i32.lt_u
        if (result i32) ;; label = @3
          local.get 2
          i32.const 3
          i32.lt_u
          if ;; label = @4
            unreachable
          end
          local.get 0
          i32.load8_u offset=10
          i32.const 63
          i32.and
          local.get 1
          i32.const 15
          i32.and
          i32.const 12
          i32.shl
          local.get 0
          i32.load8_u offset=9
          i32.const 63
          i32.and
          i32.const 6
          i32.shl
          i32.or
          i32.or
        else
          local.get 2
          i32.const 4
          i32.lt_u
          if ;; label = @4
            unreachable
          end
          local.get 0
          i32.load8_u offset=11
          i32.const 63
          i32.and
          local.get 1
          i32.const 7
          i32.and
          i32.const 18
          i32.shl
          local.get 0
          i32.load8_u offset=9
          i32.const 63
          i32.and
          i32.const 12
          i32.shl
          i32.or
          local.get 0
          i32.load8_u offset=10
          i32.const 63
          i32.and
          i32.const 6
          i32.shl
          i32.or
          i32.or
        end
      end
    else
      local.get 1
    end
    i64.extend_i32_u
  )
  (func (;68;) (type 4) (param i64) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.const 0
    i64.lt_s
    local.get 0
    i64.const 255
    i64.gt_s
    i32.or
    if ;; label = @1
      i32.const 8
      call 0
      local.tee 1
      i64.const 0
      i64.store
      i32.const 1
      local.get 1
      i32.const 1
      call 13
      return
    end
    i32.const 16
    call 0
    local.tee 1
    i64.const 2
    i64.store
    local.get 1
    local.get 0
    i32.wrap_i64
    local.tee 3
    i32.const 4
    i32.shr_u
    i32.const 15
    i32.and
    local.tee 2
    i32.const 48
    i32.add
    local.get 2
    i32.const 87
    i32.add
    local.get 2
    i32.const 10
    i32.lt_u
    select
    i32.store8 offset=8
    local.get 1
    local.get 3
    i32.const 15
    i32.and
    local.tee 2
    i32.const 48
    i32.add
    local.get 2
    i32.const 87
    i32.add
    local.get 2
    i32.const 10
    i32.lt_u
    select
    i32.store8 offset=9
    i32.const 0
    local.get 1
    i32.const 1
    call 13
  )
  (func (;69;) (type 0) (param i32) (result i32)
    (local i32)
    block ;; label = @1
      local.get 0
      i64.load
      i64.const 4294967295
      i64.and
      i32.wrap_i64
      i32.const 2
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      i32.load8_u offset=8
      local.tee 1
      i32.const 57
      i32.le_u
      local.get 1
      i32.const 48
      i32.ge_u
      i32.and
      if (result i32) ;; label = @2
        local.get 1
        i32.const 48
        i32.sub
      else
        local.get 1
        i32.const 102
        i32.le_u
        local.get 1
        i32.const 97
        i32.ge_u
        i32.and
        if (result i32) ;; label = @3
          local.get 1
          i32.const 87
          i32.sub
        else
          local.get 1
          i32.const 70
          i32.le_u
          local.get 1
          i32.const 65
          i32.ge_u
          i32.and
          if (result i32) ;; label = @4
            local.get 1
            i32.const 55
            i32.sub
          else
            br 3 (;@1;)
          end
        end
      end
      local.set 1
      i32.const 0
      local.get 0
      i32.load8_u offset=9
      local.tee 0
      i32.const 57
      i32.le_u
      local.get 0
      i32.const 48
      i32.ge_u
      i32.and
      if (result i32) ;; label = @2
        local.get 0
        i32.const 48
        i32.sub
      else
        local.get 0
        i32.const 102
        i32.le_u
        local.get 0
        i32.const 97
        i32.ge_u
        i32.and
        if (result i32) ;; label = @3
          local.get 0
          i32.const 87
          i32.sub
        else
          local.get 0
          i32.const 70
          i32.le_u
          local.get 0
          i32.const 65
          i32.ge_u
          i32.and
          if (result i32) ;; label = @4
            local.get 0
            i32.const 55
            i32.sub
          else
            br 3 (;@1;)
          end
        end
      end
      local.get 1
      i32.const 4
      i32.shl
      i32.or
      i64.extend_i32_u
      i32.const 0
      call 11
      return
    end
    i32.const 8
    call 0
    local.tee 0
    i64.const 0
    i64.store
    i32.const 1
    local.get 0
    i32.const 1
    call 13
  )
  (func (;70;) (type 4) (param i64) (result i32)
    (local i32 i32 i32)
    local.get 0
    i64.const 57343
    i64.le_s
    local.get 0
    i64.const 55296
    i64.ge_s
    i32.and
    local.get 0
    i64.const 0
    i64.lt_s
    local.get 0
    i64.const 1114111
    i64.gt_s
    i32.or
    i32.or
    if (result i32) ;; label = @1
      i32.const -1
    else
      i32.const 1
      i32.const 2
      i32.const 3
      i32.const 4
      local.get 0
      i32.wrap_i64
      local.tee 1
      i32.const 65536
      i32.lt_u
      select
      local.get 1
      i32.const 2048
      i32.lt_u
      select
      local.get 1
      i32.const 128
      i32.lt_u
      select
      local.tee 3
      i32.const 7
      i32.add
      i32.const -8
      i32.and
      i32.const 8
      i32.add
      call 0
      local.tee 2
      local.get 3
      i64.extend_i32_u
      i64.store
      local.get 3
      i32.const 1
      i32.eq
      if ;; label = @2
        local.get 2
        local.get 1
        i32.store8 offset=8
      else
        local.get 3
        i32.const 2
        i32.eq
        if ;; label = @3
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8 offset=8
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=9
        else
          local.get 3
          i32.const 3
          i32.eq
          if ;; label = @4
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 224
            i32.or
            i32.store8 offset=8
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=9
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=10
          else
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 240
            i32.or
            i32.store8 offset=8
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=9
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=10
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=11
          end
        end
      end
      i32.const 2
      local.get 2
      i32.const 1
      call 13
    end
  )
  (func (;71;) (type 6) (param i32 i64) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 1
    i64.const 0
    i64.lt_s
    if ;; label = @1
      i32.const -1
      return
    end
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 5
    local.get 1
    i32.wrap_i64
    local.set 6
    loop ;; label = @1
      local.get 4
      local.get 5
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 4
        i32.add
        i32.load8_u offset=8
        local.tee 2
        i32.const 128
        i32.lt_u
        if (result i32) ;; label = @3
          i32.const 1
        else
          i32.const 2
          i32.const 3
          i32.const 4
          local.get 2
          i32.const 240
          i32.lt_u
          select
          local.get 2
          i32.const 224
          i32.lt_u
          select
        end
        local.set 2
        local.get 3
        local.get 6
        i32.eq
        if ;; label = @3
          local.get 2
          i32.const 7
          i32.add
          i32.const -8
          i32.and
          i32.const 8
          i32.add
          call 0
          local.tee 3
          local.get 2
          i64.extend_i32_u
          i64.store
          local.get 3
          i32.const 8
          i32.add
          local.get 0
          i32.const 8
          i32.add
          local.get 4
          i32.add
          local.get 2
          memory.copy
          i32.const 2
          local.get 3
          i32.const 1
          call 13
          return
        else
          local.get 2
          local.get 4
          i32.add
          local.set 4
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
  (func (;72;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 3
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 4
    local.get 3
    i64.extend_i32_u
    i64.store
    loop ;; label = @1
      local.get 1
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 1
        local.get 4
        i32.add
        local.get 0
        local.get 1
        i32.add
        i32.load8_u offset=8
        local.tee 2
        i32.const 90
        i32.le_u
        local.get 2
        i32.const 65
        i32.ge_u
        i32.and
        if (result i32) ;; label = @3
          local.get 2
          i32.const 32
          i32.add
        else
          local.get 2
        end
        i32.store8 offset=8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 4
  )
  (func (;73;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 3
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 4
    local.get 3
    i64.extend_i32_u
    i64.store
    loop ;; label = @1
      local.get 1
      local.get 3
      i32.lt_u
      if ;; label = @2
        local.get 1
        local.get 4
        i32.add
        local.get 0
        local.get 1
        i32.add
        i32.load8_u offset=8
        local.tee 2
        i32.const 122
        i32.le_u
        local.get 2
        i32.const 97
        i32.ge_u
        i32.and
        if (result i32) ;; label = @3
          local.get 2
          i32.const 32
          i32.sub
        else
          local.get 2
        end
        i32.store8 offset=8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 4
  )
  (func (;74;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 4
    loop ;; label = @1
      block ;; label = @2
        local.get 2
        local.get 4
        i32.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.get 2
        i32.add
        i32.load8_u offset=8
        local.tee 1
        i32.const 32
        i32.eq
        local.get 1
        i32.const 9
        i32.eq
        i32.or
        local.get 1
        i32.const 10
        i32.eq
        i32.or
        local.get 1
        i32.const 13
        i32.eq
        i32.or
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 4
    local.set 1
    loop ;; label = @1
      block ;; label = @2
        local.get 1
        local.get 2
        i32.le_u
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        i32.const 1
        i32.sub
        local.tee 3
        i32.add
        i32.load8_u offset=8
        local.tee 5
        i32.const 32
        i32.eq
        local.get 5
        i32.const 9
        i32.eq
        i32.or
        local.get 5
        i32.const 10
        i32.eq
        i32.or
        local.get 5
        i32.const 13
        i32.eq
        i32.or
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.set 1
        br 1 (;@1;)
      end
    end
    local.get 1
    local.get 2
    i32.sub
    local.tee 1
    local.get 4
    i32.eq
    if (result i32) ;; label = @1
      local.get 0
    else
      local.get 1
      i32.const 7
      i32.add
      i32.const -8
      i32.and
      i32.const 8
      i32.add
      call 0
      local.tee 3
      local.get 1
      i64.extend_i32_u
      i64.store
      local.get 3
      i32.const 8
      i32.add
      local.get 0
      i32.const 8
      i32.add
      local.get 2
      i32.add
      local.get 1
      memory.copy
      local.get 3
    end
  )
  (func (;75;) (type 2) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 1
    i32.const 0
    local.get 1
    i32.const 0
    i32.ge_s
    select
    local.tee 6
    local.get 2
    i32.const 0
    local.get 2
    i32.const 0
    i32.ge_s
    select
    local.tee 7
    i32.ge_s
    if ;; label = @1
      i32.const 8
      call 0
      local.tee 0
      i64.const 0
      i64.store
      local.get 0
      return
    end
    i32.const 0
    local.set 1
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 2
    local.set 3
    loop ;; label = @1
      block ;; label = @2
        local.get 1
        local.get 3
        local.get 4
        local.get 6
        i32.eq
        select
        local.set 3
        local.get 4
        local.get 7
        i32.eq
        if ;; label = @3
          local.get 1
          local.set 2
          br 1 (;@2;)
        end
        local.get 1
        local.get 2
        i32.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        i32.add
        i32.load8_u offset=8
        local.tee 5
        i32.const 128
        i32.lt_u
        if (result i32) ;; label = @3
          i32.const 1
        else
          i32.const 2
          i32.const 3
          i32.const 4
          local.get 5
          i32.const 240
          i32.lt_u
          select
          local.get 5
          i32.const 224
          i32.lt_u
          select
        end
        local.get 1
        i32.add
        local.set 1
        local.get 4
        i32.const 1
        i32.add
        local.set 4
        br 1 (;@1;)
      end
    end
    local.get 2
    local.get 3
    i32.sub
    local.tee 1
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 2
    local.get 1
    i64.extend_i32_u
    i64.store
    local.get 2
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    local.get 3
    i32.add
    local.get 1
    memory.copy
    local.get 2
  )
  (func (;76;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 5
    loop ;; label = @1
      local.get 2
      local.get 5
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 2
        i32.add
        i32.load8_u offset=8
        local.tee 1
        i32.const 128
        i32.lt_u
        if (result i32) ;; label = @3
          i32.const 1
        else
          i32.const 2
          i32.const 3
          i32.const 4
          local.get 1
          i32.const 240
          i32.lt_u
          select
          local.get 1
          i32.const 224
          i32.lt_u
          select
        end
        local.tee 1
        i32.const 7
        i32.add
        i32.const -8
        i32.and
        i32.const 8
        i32.add
        call 0
        local.tee 3
        local.get 1
        i64.extend_i32_u
        i64.store
        local.get 3
        i32.const 8
        i32.add
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.add
        local.get 1
        memory.copy
        local.get 3
        i64.extend_i32_u
        local.get 4
        i32.const 1
        call 16
        local.set 4
        local.get 1
        local.get 2
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 4
    call 25
  )
  (func (;77;) (type 2) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 2
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 3
    local.get 2
    i64.extend_i32_u
    i64.store
    local.get 3
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.add
    local.get 2
    memory.copy
    local.get 3
  )
  (func (;78;) (type 1) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 4
    local.get 1
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 6
    i32.eqz
    if ;; label = @1
      local.get 0
      i32.const 0
      i32.const 0
      call 77
      i64.extend_i32_u
      i32.const 0
      i32.const 1
      call 16
      local.set 3
      i32.const 0
      local.set 1
      loop ;; label = @2
        local.get 1
        local.get 4
        i32.lt_u
        if ;; label = @3
          local.get 0
          local.get 1
          local.get 0
          local.get 1
          i32.add
          i32.load8_u offset=8
          local.tee 2
          i32.const 128
          i32.lt_u
          if (result i32) ;; label = @4
            i32.const 1
          else
            i32.const 2
            i32.const 3
            i32.const 4
            local.get 2
            i32.const 240
            i32.lt_u
            select
            local.get 2
            i32.const 224
            i32.lt_u
            select
          end
          local.tee 2
          call 77
          i64.extend_i32_u
          local.get 3
          i32.const 1
          call 16
          local.set 3
          local.get 1
          local.get 2
          i32.add
          local.set 1
          br 1 (;@2;)
        end
      end
      local.get 0
      i32.const 0
      i32.const 0
      call 77
      i64.extend_i32_u
      local.get 3
      i32.const 1
      call 16
      call 25
      return
    end
    loop ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      call 19
      local.tee 5
      i32.const -1
      i32.ne
      if ;; label = @2
        local.get 0
        local.get 2
        local.get 5
        local.get 2
        i32.sub
        call 77
        i64.extend_i32_u
        local.get 3
        i32.const 1
        call 16
        local.set 3
        local.get 5
        local.get 6
        i32.add
        local.set 2
        br 1 (;@1;)
      end
    end
    local.get 0
    local.get 2
    local.get 4
    local.get 2
    i32.sub
    call 77
    i64.extend_i32_u
    local.get 3
    i32.const 1
    call 16
    call 25
  )
  (func (;79;) (type 1) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 1
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.set 5
    local.get 0
    local.set 3
    i32.const 1
    local.set 4
    loop ;; label = @1
      local.get 3
      if ;; label = @2
        local.get 2
        local.get 2
        local.get 5
        i32.add
        local.get 4
        select
        i32.const 0
        local.set 4
        local.get 3
        i32.const 0
        call 5
        i32.wrap_i64
        i64.load
        i64.const 4294967295
        i64.and
        i32.wrap_i64
        i32.add
        local.set 2
        local.get 3
        i32.const 1
        call 7
        local.set 3
        br 1 (;@1;)
      end
    end
    local.get 2
    i32.const 7
    i32.add
    i32.const -8
    i32.and
    i32.const 8
    i32.add
    call 0
    local.tee 6
    local.get 2
    i64.extend_i32_u
    i64.store
    i32.const 0
    local.set 2
    local.get 0
    local.set 3
    i32.const 1
    local.set 4
    loop ;; label = @1
      local.get 3
      if ;; label = @2
        local.get 4
        i32.eqz
        if ;; label = @3
          local.get 6
          i32.const 8
          i32.add
          local.get 2
          i32.add
          local.get 1
          i32.const 8
          i32.add
          local.get 5
          memory.copy
          local.get 2
          local.get 5
          i32.add
          local.set 2
        end
        i32.const 0
        local.set 4
        local.get 3
        i32.const 0
        call 5
        i32.wrap_i64
        local.tee 7
        i64.load
        i64.const 4294967295
        i64.and
        i32.wrap_i64
        local.set 0
        local.get 6
        i32.const 8
        i32.add
        local.get 2
        i32.add
        local.get 7
        i32.const 8
        i32.add
        local.get 0
        memory.copy
        local.get 0
        local.get 2
        i32.add
        local.set 2
        local.get 3
        i32.const 1
        call 7
        local.set 3
        br 1 (;@1;)
      end
    end
    local.get 6
  )
  (func (;80;) (type 2) (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    call 78
    local.get 2
    call 79
  )
  (func (;81;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i64)
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 2
    i32.const 0
    i32.gt_s
    if ;; label = @1
      local.get 0
      i32.load8_u offset=8
      i32.const 45
      i32.eq
      if ;; label = @2
        i32.const 1
        local.set 3
        i32.const 1
        local.set 1
      end
    end
    loop ;; label = @1
      local.get 1
      local.get 2
      i32.lt_u
      if ;; label = @2
        local.get 0
        local.get 1
        i32.add
        i32.load8_u offset=8
        i32.const 48
        i32.sub
        i64.extend_i32_u
        local.get 4
        i64.const 10
        i64.mul
        i64.add
        local.set 4
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        br 1 (;@1;)
      end
    end
    i32.const 0
    i64.const 0
    local.get 4
    i64.sub
    local.get 4
    local.get 3
    select
    i32.const 0
    call 11
  )
  (func (;82;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64)
    f64.const 0x1p+0 (;=1;)
    local.set 12
    local.get 0
    i64.load
    i64.const 4294967295
    i64.and
    i32.wrap_i64
    local.tee 7
    i32.const 0
    i32.gt_s
    if ;; label = @1
      local.get 0
      i32.load8_u offset=8
      i32.const 45
      i32.eq
      if ;; label = @2
        i32.const 1
        local.set 8
        i32.const 1
        local.set 1
      end
    end
    block ;; label = @1
      loop ;; label = @2
        local.get 1
        local.get 7
        i32.lt_u
        if ;; label = @3
          local.get 3
          i32.const 1
          i32.eq
          local.get 0
          local.get 1
          i32.add
          i32.load8_u offset=8
          local.tee 2
          i32.const 43
          i32.eq
          i32.and
          if ;; label = @4
            i32.const 2
            local.set 3
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          local.get 3
          i32.const 1
          i32.eq
          local.get 2
          i32.const 45
          i32.eq
          i32.and
          if ;; label = @4
            i32.const 1
            local.set 9
            i32.const 2
            local.set 3
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          local.get 3
          if ;; label = @4
            local.get 2
            i32.const 48
            i32.lt_u
            local.get 2
            i32.const 57
            i32.gt_u
            i32.or
            br_if 3 (;@1;)
            i32.const 1
            local.set 10
            i32.const 2
            local.set 3
            local.get 2
            i32.const 48
            i32.sub
            local.get 4
            i32.const 10
            i32.mul
            i32.add
            local.set 4
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          local.get 2
          i32.const 46
          i32.eq
          if ;; label = @4
            local.get 5
            br_if 3 (;@1;)
            i32.const 1
            local.set 5
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          local.get 2
          i32.const 101
          i32.eq
          local.get 2
          i32.const 69
          i32.eq
          i32.or
          if ;; label = @4
            local.get 6
            i32.eqz
            br_if 3 (;@1;)
            i32.const 1
            local.set 3
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            br 2 (;@2;)
          end
          local.get 2
          i32.const 48
          i32.lt_u
          local.get 2
          i32.const 57
          i32.gt_u
          i32.or
          br_if 2 (;@1;)
          i32.const 1
          local.set 6
          local.get 2
          i32.const 48
          i32.sub
          local.set 2
          local.get 5
          if (result f64) ;; label = @4
            local.get 11
            local.get 2
            f64.convert_i32_u
            local.get 12
            f64.const 0x1.4p+3 (;=10;)
            f64.mul
            local.tee 12
            f64.div
            f64.add
          else
            local.get 11
            f64.const 0x1.4p+3 (;=10;)
            f64.mul
            local.get 2
            f64.convert_i32_u
            f64.add
          end
          local.set 11
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          br 1 (;@2;)
        end
      end
      local.get 10
      i32.eqz
      local.get 3
      i32.const 2
      i32.eq
      i32.and
      local.get 6
      i32.eqz
      local.get 3
      i32.const 1
      i32.eq
      i32.or
      i32.or
      br_if 0 (;@1;)
      loop ;; label = @2
        local.get 4
        if ;; label = @3
          local.get 11
          f64.const 0x1.4p+3 (;=10;)
          f64.div
          local.get 11
          f64.const 0x1.4p+3 (;=10;)
          f64.mul
          local.get 9
          select
          local.set 11
          local.get 4
          i32.const 1
          i32.sub
          local.set 4
          br 1 (;@2;)
        end
      end
      i32.const 0
      f64.const 0x0p+0 (;=0;)
      local.get 11
      f64.sub
      local.get 11
      local.get 8
      select
      call 12
      return
    end
    i32.const 1
    local.get 0
    i32.const 1
    call 13
  )
  (func (;83;) (type 7) (param i32)
    local.get 0
    global.set 1
    global.get 0
    global.set 2
    global.get 0
    global.set 3
  )
  (func (;84;) (type 0) (param i32) (result i32)
    local.get 0
    local.get 0
    local.get 0
    local.get 0
    global.get 1
    global.get 2
    i32.sub
    i32.add
    local.get 0
    global.get 3
    i32.ge_u
    select
    local.get 0
    global.get 2
    i32.lt_u
    select
    local.get 0
    i32.const 0
    i32.le_s
    select
  )
  (func (;85;) (type 23)
    (local i32 i32 i32 i32 i32 i32 i32 i64)
    global.get 0
    global.set 3
    global.get 2
    local.set 0
    global.get 3
    local.set 6
    loop ;; label = @1
      local.get 0
      local.get 6
      i32.lt_u
      if ;; label = @2
        local.get 0
        i64.load
        local.tee 7
        i64.const 56
        i64.shr_u
        i32.wrap_i64
        local.set 1
        local.get 7
        i64.const 32
        i64.shr_u
        i32.wrap_i64
        i32.const 65535
        i32.and
        local.tee 3
        i32.const 1
        i32.and
        local.get 1
        i32.const 3
        i32.eq
        local.get 1
        i32.const 8
        i32.eq
        i32.or
        i32.and
        if ;; label = @3
          local.get 0
          local.get 0
          i64.load offset=8
          i32.wrap_i64
          call 84
          i64.extend_i32_s
          i64.store offset=8
        end
        local.get 1
        i32.const 4
        i32.eq
        if ;; label = @3
          local.get 3
          i32.const 1
          i32.and
          if ;; label = @4
            local.get 0
            local.get 0
            i64.load offset=8
            i32.wrap_i64
            call 84
            i64.extend_i32_s
            i64.store offset=8
          end
          local.get 0
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call 84
          i64.extend_i32_s
          i64.store offset=16
        end
        local.get 1
        i32.const 9
        i32.eq
        if ;; label = @3
          local.get 0
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call 84
          i64.extend_i32_s
          i64.store offset=16
        end
        local.get 7
        i32.wrap_i64
        local.set 4
        local.get 1
        i32.const 1
        i32.eq
        local.get 1
        i32.const 5
        i32.eq
        i32.or
        local.get 1
        i32.const 2
        i32.eq
        i32.or
        local.get 1
        i32.const 12
        i32.eq
        i32.or
        local.get 1
        i32.const 14
        i32.eq
        i32.or
        if ;; label = @3
          i32.const 0
          local.set 2
          loop ;; label = @4
            local.get 2
            local.get 4
            i32.ge_u
            local.get 2
            i32.const 16
            i32.ge_u
            i32.or
            i32.eqz
            if ;; label = @5
              local.get 3
              i32.const 1
              local.get 2
              i32.shl
              i32.and
              if ;; label = @6
                local.get 0
                i32.const 8
                i32.add
                local.get 2
                i32.const 3
                i32.shl
                i32.add
                local.tee 5
                local.get 5
                i64.load
                i32.wrap_i64
                call 84
                i64.extend_i32_s
                i64.store
              end
              local.get 2
              i32.const 1
              i32.add
              local.set 2
              br 1 (;@4;)
            end
          end
        end
        local.get 1
        i32.const 13
        i32.eq
        local.get 1
        i32.const 15
        i32.eq
        i32.or
        if ;; label = @3
          i32.const 0
          local.set 2
          loop ;; label = @4
            local.get 2
            local.get 4
            i32.lt_u
            if ;; label = @5
              local.get 0
              i32.const 8
              i32.add
              local.get 2
              i32.const 3
              i32.shl
              i32.add
              local.tee 5
              local.get 5
              i64.load
              i32.wrap_i64
              call 84
              i64.extend_i32_s
              i64.store
              local.get 2
              i32.const 1
              i32.add
              local.set 2
              br 1 (;@4;)
            end
          end
        end
        local.get 1
        i32.const 10
        i32.eq
        if ;; label = @3
          local.get 3
          i32.const 2
          i32.and
          if ;; label = @4
            local.get 0
            local.get 0
            i64.load offset=8
            i32.wrap_i64
            call 84
            i64.extend_i32_s
            i64.store offset=8
            local.get 3
            i32.const 1
            i32.and
            if ;; label = @5
              local.get 0
              local.get 0
              i64.load offset=24
              i32.wrap_i64
              call 84
              i64.extend_i32_s
              i64.store offset=24
            end
          else
            local.get 3
            i32.const 1
            i32.and
            if ;; label = @5
              i32.const 0
              local.set 2
              loop ;; label = @6
                local.get 2
                local.get 4
                i32.lt_u
                if ;; label = @7
                  local.get 0
                  i32.const 8
                  i32.add
                  local.get 2
                  i32.const 3
                  i32.shl
                  i32.add
                  local.tee 5
                  local.get 5
                  i64.load
                  i32.wrap_i64
                  call 84
                  i64.extend_i32_s
                  i64.store
                  local.get 2
                  i32.const 1
                  i32.add
                  local.set 2
                  br 1 (;@6;)
                end
              end
            end
          end
        end
        local.get 1
        i32.const 11
        i32.eq
        if ;; label = @3
          local.get 0
          local.get 0
          i64.load offset=8
          i32.wrap_i64
          call 84
          i64.extend_i32_s
          i64.store offset=8
          local.get 0
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call 84
          i64.extend_i32_s
          i64.store offset=16
        end
        i32.const 32
        i32.const 16
        i32.const 24
        local.get 4
        i32.const 3
        i32.shl
        i32.const 8
        i32.add
        local.get 1
        i32.const 4
        i32.eq
        local.get 1
        i32.const 9
        i32.eq
        i32.or
        local.get 1
        i32.const 11
        i32.eq
        i32.or
        select
        local.get 1
        i32.const 3
        i32.eq
        local.get 1
        i32.const 7
        i32.eq
        i32.or
        local.get 1
        i32.const 8
        i32.eq
        i32.or
        select
        local.get 4
        i32.const 15
        i32.add
        i32.const -8
        i32.and
        local.get 1
        select
        local.get 1
        i32.const 10
        i32.eq
        local.get 3
        i32.const 2
        i32.and
        i32.const 0
        i32.ne
        i32.and
        select
        local.get 0
        i32.add
        local.set 0
        br 1 (;@1;)
      end
    end
    global.get 1
    global.get 2
    global.get 3
    global.get 2
    i32.sub
    local.tee 0
    memory.copy
    global.get 1
    local.get 0
    i32.add
    global.set 0
  )
  (func (;86;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i64)
    block ;; label = @1
      local.get 0
      i32.const 0
      i32.le_s
      if (result i32) ;; label = @2
        local.get 0
      else
        local.get 0
        global.get 1
        i32.lt_u
        if (result i32) ;; label = @3
          local.get 0
        else
          local.get 0
          global.get 0
          i32.lt_u
          local.get 0
          global.get 2
          i32.ge_u
          i32.and
          if (result i32) ;; label = @4
            local.get 0
          else
            local.get 0
            local.tee 1
            i64.load
            local.tee 8
            i64.const 56
            i64.shr_u
            i32.wrap_i64
            local.tee 2
            i32.const 255
            i32.eq
            if (result i32) ;; label = @5
              local.get 8
              i32.wrap_i64
            else
              i32.const 32
              i32.const 16
              i32.const 24
              local.get 8
              i32.wrap_i64
              local.tee 4
              i32.const 3
              i32.shl
              i32.const 8
              i32.add
              local.get 2
              i32.const 4
              i32.eq
              local.get 2
              i32.const 9
              i32.eq
              i32.or
              local.get 2
              i32.const 11
              i32.eq
              i32.or
              select
              local.get 2
              i32.const 8
              i32.eq
              local.tee 5
              local.get 2
              i32.const 3
              i32.eq
              local.tee 6
              local.get 2
              i32.const 7
              i32.eq
              i32.or
              i32.or
              select
              local.get 4
              i32.const 15
              i32.add
              i32.const -8
              i32.and
              local.get 2
              select
              local.get 2
              i32.const 10
              i32.eq
              local.get 8
              i64.const 32
              i64.shr_u
              i32.wrap_i64
              i32.const 65535
              i32.and
              local.tee 3
              i32.const 2
              i32.and
              i32.const 0
              i32.ne
              i32.and
              select
              local.tee 7
              call 0
              local.tee 0
              local.get 1
              local.get 7
              memory.copy
              local.get 1
              local.get 0
              i64.extend_i32_u
              i64.const -72057594037927936
              i64.or
              i64.store
              local.get 3
              i32.const 1
              i32.and
              local.get 5
              local.get 6
              i32.or
              i32.and
              if ;; label = @6
                local.get 0
                local.get 0
                i64.load offset=8
                i32.wrap_i64
                call 86
                i64.extend_i32_s
                i64.store offset=8
              end
              local.get 2
              i32.const 4
              i32.eq
              if ;; label = @6
                local.get 3
                i32.const 1
                i32.and
                if ;; label = @7
                  local.get 0
                  local.get 0
                  i64.load offset=8
                  i32.wrap_i64
                  call 86
                  i64.extend_i32_s
                  i64.store offset=8
                end
                br 5 (;@1;)
              end
              local.get 2
              i32.const 9
              i32.eq
              br_if 4 (;@1;)
              local.get 2
              i32.const 1
              i32.eq
              local.get 2
              i32.const 5
              i32.eq
              i32.or
              local.get 2
              i32.const 2
              i32.eq
              i32.or
              local.get 2
              i32.const 12
              i32.eq
              i32.or
              local.get 2
              i32.const 14
              i32.eq
              i32.or
              if ;; label = @6
                i32.const 0
                local.set 1
                loop ;; label = @7
                  local.get 1
                  local.get 4
                  i32.ge_u
                  local.get 1
                  i32.const 16
                  i32.ge_u
                  i32.or
                  i32.eqz
                  if ;; label = @8
                    local.get 3
                    i32.const 1
                    local.get 1
                    i32.shl
                    i32.and
                    if ;; label = @9
                      local.get 0
                      i32.const 8
                      i32.add
                      local.get 1
                      i32.const 3
                      i32.shl
                      i32.add
                      local.tee 5
                      local.get 5
                      i64.load
                      i32.wrap_i64
                      call 86
                      i64.extend_i32_s
                      i64.store
                    end
                    local.get 1
                    i32.const 1
                    i32.add
                    local.set 1
                    br 1 (;@7;)
                  end
                end
              end
              local.get 2
              i32.const 13
              i32.eq
              local.get 2
              i32.const 15
              i32.eq
              i32.or
              if ;; label = @6
                i32.const 0
                local.set 1
                loop ;; label = @7
                  local.get 1
                  local.get 4
                  i32.lt_u
                  if ;; label = @8
                    local.get 0
                    i32.const 8
                    i32.add
                    local.get 1
                    i32.const 3
                    i32.shl
                    i32.add
                    local.tee 5
                    local.get 5
                    i64.load
                    i32.wrap_i64
                    call 86
                    i64.extend_i32_s
                    i64.store
                    local.get 1
                    i32.const 1
                    i32.add
                    local.set 1
                    br 1 (;@7;)
                  end
                end
              end
              local.get 2
              i32.const 10
              i32.eq
              if ;; label = @6
                local.get 3
                i32.const 2
                i32.and
                if ;; label = @7
                  local.get 0
                  local.get 0
                  i64.load offset=8
                  i32.wrap_i64
                  call 86
                  i64.extend_i32_s
                  i64.store offset=8
                  local.get 3
                  i32.const 1
                  i32.and
                  if ;; label = @8
                    local.get 0
                    local.get 0
                    i64.load offset=24
                    i32.wrap_i64
                    call 86
                    i64.extend_i32_s
                    i64.store offset=24
                  end
                else
                  local.get 3
                  i32.const 1
                  i32.and
                  if ;; label = @8
                    i32.const 0
                    local.set 1
                    loop ;; label = @9
                      local.get 1
                      local.get 4
                      i32.lt_u
                      if ;; label = @10
                        local.get 0
                        i32.const 8
                        i32.add
                        local.get 1
                        i32.const 3
                        i32.shl
                        i32.add
                        local.tee 3
                        local.get 3
                        i64.load
                        i32.wrap_i64
                        call 86
                        i64.extend_i32_s
                        i64.store
                        local.get 1
                        i32.const 1
                        i32.add
                        local.set 1
                        br 1 (;@9;)
                      end
                    end
                  end
                end
              end
              local.get 2
              i32.const 11
              i32.eq
              if ;; label = @6
                local.get 0
                local.get 0
                i64.load offset=8
                i32.wrap_i64
                call 86
                i64.extend_i32_s
                i64.store offset=8
                br 5 (;@1;)
              end
              local.get 0
            end
          end
        end
      end
      return
    end
    local.get 0
    loop ;; label = @1
      block ;; label = @2
        local.get 0
        i64.load offset=16
        i32.wrap_i64
        local.tee 1
        i32.const 0
        i32.le_s
        if ;; label = @3
          local.get 0
          i32.const 16
          i32.add
          local.get 1
          i64.extend_i32_s
          i64.store
          br 1 (;@2;)
        end
        local.get 1
        global.get 1
        i32.lt_u
        if ;; label = @3
          local.get 0
          i32.const 16
          i32.add
          local.get 1
          i64.extend_i32_s
          i64.store
          br 1 (;@2;)
        end
        local.get 1
        global.get 0
        i32.lt_u
        local.get 1
        global.get 2
        i32.ge_u
        i32.and
        if ;; label = @3
          local.get 1
          local.set 0
          br 2 (;@1;)
        end
        local.get 1
        i64.load
        local.tee 8
        i64.const 56
        i64.shr_u
        i32.wrap_i64
        local.tee 3
        i32.const 255
        i32.eq
        if ;; label = @3
          local.get 0
          i32.const 16
          i32.add
          local.get 8
          i32.wrap_i64
          local.tee 0
          i64.extend_i32_s
          i64.store
          br 2 (;@1;)
        end
        local.get 3
        i32.const 4
        i32.eq
        local.get 3
        i32.const 9
        i32.eq
        i32.or
        local.get 3
        i32.const 11
        i32.eq
        i32.or
        if ;; label = @3
          local.get 1
          local.set 3
          loop ;; label = @4
            block ;; label = @5
              local.get 1
              global.get 0
              i32.lt_u
              local.get 1
              global.get 2
              i32.ge_u
              i32.and
              local.get 1
              global.get 1
              i32.lt_u
              local.get 1
              i32.const 0
              i32.le_s
              i32.or
              i32.or
              br_if 0 (;@5;)
              local.get 1
              i64.load
              local.tee 8
              i64.const 56
              i64.shr_u
              i32.wrap_i64
              local.tee 5
              i32.const 4
              i32.eq
              local.get 5
              i32.const 9
              i32.eq
              i32.or
              local.get 5
              i32.const 11
              i32.eq
              i32.or
              i32.eqz
              local.get 5
              i32.const 255
              i32.eq
              i32.or
              br_if 0 (;@5;)
              i32.const 24
              call 0
              local.tee 4
              local.get 1
              i32.const 24
              memory.copy
              local.get 1
              local.get 4
              i64.extend_i32_u
              i64.const -72057594037927936
              i64.or
              i64.store
              local.get 8
              i64.const 32
              i64.shr_u
              i32.wrap_i64
              i32.const 1
              i32.and
              local.get 5
              i32.const 4
              i32.eq
              i32.and
              if ;; label = @6
                local.get 4
                local.get 4
                i64.load offset=8
                i32.wrap_i64
                call 86
                i64.extend_i32_s
                i64.store offset=8
              end
              local.get 5
              i32.const 11
              i32.eq
              if ;; label = @6
                local.get 4
                local.get 4
                i64.load offset=8
                i32.wrap_i64
                call 86
                i64.extend_i32_s
                i64.store offset=8
              end
              local.get 4
              i64.load offset=16
              i32.wrap_i64
              local.set 1
              br 1 (;@4;)
            end
          end
          local.get 0
          i32.const 16
          i32.add
          local.get 3
          i64.load
          i32.wrap_i64
          local.tee 0
          i64.extend_i32_s
          i64.store
          br 2 (;@1;)
        end
        local.get 0
        i32.const 16
        i32.add
        local.get 1
        call 86
        i64.extend_i32_s
        i64.store
      end
    end
  )
)
