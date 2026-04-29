;; Boundary compaction GC. After a function frame allocates
;; everything beyond a saved heap_ptr mark, $collect_begin records
;; the boundary, the caller evacuates its survivors using
;; $retain_i32 (deep-copies into the post-mark window),
;; $collect_end walks the new region and rebases inner pointers
;; via $rebase_i32 so they point inside the compacted frame.
;;
;; Globals come from `runtime/wat/prelude.part.wat`:
;;   $heap_ptr     — the bump cursor (also used by rt_alloc)
;;   $collect_mark — start of the compacted frame on the heap
;;   $collect_from — start of the scratch region (pre-evac heap)
;;   $collect_dst  — end of the scratch region (post-evac heap)

  (func $rt_collect_begin (param i32)
    local.get 0
    global.set $collect_mark
    global.get $heap_ptr
    global.set $collect_from
    global.get $heap_ptr
    global.set $collect_dst
  )
  (func $rt_rebase_i32 (param i32) (result i32)
    local.get 0
    i32.const 0
    i32.le_s
    if (result i32) ;; label = @1
      local.get 0
    else
      local.get 0
      global.get $collect_from
      i32.lt_u
      if (result i32) ;; label = @2
        local.get 0
      else
        local.get 0
        global.get $collect_dst
        i32.ge_u
        if (result i32) ;; label = @3
          local.get 0
        else
          local.get 0
          global.get $collect_mark
          global.get $collect_from
          i32.sub
          i32.add
        end
      end
    end
  )
  (func $rt_collect_end
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64)
    global.get $heap_ptr
    global.set $collect_dst
    global.get $collect_from
    local.set 0
    global.get $collect_dst
    local.set 1
    block ;; label = @1
      loop ;; label = @2
        local.get 0
        local.get 1
        i32.ge_u
        br_if 1 (;@1;)
        local.get 0
        i64.load
        local.set 10
        local.get 10
        i64.const 56
        i64.shr_u
        i32.wrap_i64
        i32.const 255
        i32.and
        local.set 3
        local.get 10
        i64.const 32
        i64.shr_u
        i32.wrap_i64
        i32.const 65535
        i32.and
        local.set 4
        local.get 10
        i32.wrap_i64
        local.set 5
        local.get 3
        i32.const 0
        i32.eq
        if (result i32) ;; label = @3
          i32.const 8
          local.get 5
          i32.add
          i32.const 7
          i32.add
          i32.const -8
          i32.and
        else
          local.get 3
          i32.const 3
          i32.eq
          local.get 3
          i32.const 7
          i32.eq
          i32.or
          local.get 3
          i32.const 8
          i32.eq
          i32.or
          if (result i32) ;; label = @4
            i32.const 16
          else
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
            if (result i32) ;; label = @5
              i32.const 24
            else
              i32.const 8
              local.get 5
              i32.const 8
              i32.mul
              i32.add
            end
          end
        end
        local.set 2
        local.get 3
        i32.const 3
        i32.eq
        local.get 3
        i32.const 8
        i32.eq
        i32.or
        if ;; label = @3
          local.get 4
          i32.const 1
          i32.and
          if ;; label = @4
            local.get 0
            i64.load offset=8
            i32.wrap_i64
            call $rt_rebase_i32
            local.set 7
            local.get 0
            local.get 7
            i64.extend_i32_s
            i64.store offset=8
          end
        end
        local.get 3
        i32.const 4
        i32.eq
        if ;; label = @3
          local.get 4
          i32.const 1
          i32.and
          if ;; label = @4
            local.get 0
            i64.load offset=8
            i32.wrap_i64
            call $rt_rebase_i32
            local.set 7
            local.get 0
            local.get 7
            i64.extend_i32_s
            i64.store offset=8
          end
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call $rt_rebase_i32
          local.set 7
          local.get 0
          local.get 7
          i64.extend_i32_s
          i64.store offset=16
        end
        local.get 3
        i32.const 9
        i32.eq
        if ;; label = @3
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call $rt_rebase_i32
          local.set 7
          local.get 0
          local.get 7
          i64.extend_i32_s
          i64.store offset=16
        end
        local.get 3
        i32.const 1
        i32.eq
        local.get 3
        i32.const 5
        i32.eq
        i32.or
        local.get 3
        i32.const 2
        i32.eq
        i32.or
        local.get 3
        i32.const 12
        i32.eq
        i32.or
        local.get 3
        i32.const 14
        i32.eq
        i32.or
        if ;; label = @3
          i32.const 0
          local.set 6
          block ;; label = @4
            loop ;; label = @5
              local.get 6
              local.get 5
              i32.ge_u
              br_if 1 (;@4;)
              local.get 6
              i32.const 16
              i32.ge_u
              br_if 1 (;@4;)
              local.get 4
              i32.const 1
              local.get 6
              i32.shl
              i32.and
              i32.eqz
              if ;; label = @6
              else
                local.get 0
                i32.const 8
                i32.add
                local.get 6
                i32.const 8
                i32.mul
                i32.add
                local.set 8
                local.get 8
                i64.load
                i32.wrap_i64
                call $rt_rebase_i32
                local.set 7
                local.get 8
                local.get 7
                i64.extend_i32_s
                i64.store
              end
              local.get 6
              i32.const 1
              i32.add
              local.set 6
              br 0 (;@5;)
            end
          end
        end
        ;; HAMT_NODE (13) / HAMT_COLLISION (15): every field is a pointer.
        local.get 3
        i32.const 13
        i32.eq
        local.get 3
        i32.const 15
        i32.eq
        i32.or
        if ;; label = @3
          i32.const 0
          local.set 6
          block ;; label = @4
            loop ;; label = @5
              local.get 6
              local.get 5
              i32.ge_u
              br_if 1 (;@4;)
              local.get 0
              i32.const 8
              i32.add
              local.get 6
              i32.const 8
              i32.mul
              i32.add
              local.set 8
              local.get 8
              i64.load
              i32.wrap_i64
              call $rt_rebase_i32
              local.set 7
              local.get 8
              local.get 7
              i64.extend_i32_s
              i64.store
              local.get 6
              i32.const 1
              i32.add
              local.set 6
              br 0 (;@5;)
            end
          end
        end
        local.get 3
        i32.const 10
        i32.eq
        if ;; label = @3
          local.get 4
          i32.const 1
          i32.and
          if ;; label = @4
            i32.const 0
            local.set 6
            block ;; label = @5
              loop ;; label = @6
                local.get 6
                local.get 5
                i32.ge_u
                br_if 1 (;@5;)
                local.get 0
                i32.const 8
                i32.add
                local.get 6
                i32.const 8
                i32.mul
                i32.add
                local.set 8
                local.get 8
                i64.load
                i32.wrap_i64
                call $rt_rebase_i32
                local.set 7
                local.get 8
                local.get 7
                i64.extend_i32_s
                i64.store
                local.get 6
                i32.const 1
                i32.add
                local.set 6
                br 0 (;@6;)
              end
            end
          end
        end
        local.get 3
        i32.const 11
        i32.eq
        if ;; label = @3
          local.get 0
          i64.load offset=8
          i32.wrap_i64
          call $rt_rebase_i32
          local.set 7
          local.get 0
          local.get 7
          i64.extend_i32_s
          i64.store offset=8
          local.get 0
          i64.load offset=16
          i32.wrap_i64
          call $rt_rebase_i32
          local.set 7
          local.get 0
          local.get 7
          i64.extend_i32_s
          i64.store offset=16
        end
        local.get 0
        local.get 2
        i32.add
        local.set 0
        br 0 (;@2;)
      end
    end
    global.get $collect_dst
    global.get $collect_from
    i32.sub
    local.set 9
    global.get $collect_mark
    global.get $collect_from
    local.get 9
    memory.copy
    global.get $collect_mark
    local.get 9
    i32.add
    global.set $heap_ptr
  )
  (func $rt_retain_i32 (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64)
    local.get 0
    i32.const 0
    i32.le_s
    if (result i32) ;; label = @1
      local.get 0
    else
      local.get 0
      global.get $collect_mark
      i32.lt_u
      if (result i32) ;; label = @2
        local.get 0
      else
        local.get 0
        global.get $collect_from
        i32.ge_u
        local.get 0
        global.get $heap_ptr
        i32.lt_u
        i32.and
        if (result i32) ;; label = @3
          local.get 0
        else
          local.get 0
          local.set 1
          local.get 1
          i64.load
          local.set 11
          local.get 11
          i64.const 56
          i64.shr_u
          i32.wrap_i64
          i32.const 255
          i32.and
          local.set 4
          local.get 4
          i32.const 255
          i32.eq
          if (result i32) ;; label = @4
            local.get 11
            i32.wrap_i64
          else
            local.get 11
            i64.const 32
            i64.shr_u
            i32.wrap_i64
            i32.const 65535
            i32.and
            local.set 5
            local.get 11
            i32.wrap_i64
            local.set 6
            local.get 4
            i32.const 0
            i32.eq
            if (result i32) ;; label = @5
              i32.const 8
              local.get 6
              i32.add
              i32.const 7
              i32.add
              i32.const -8
              i32.and
            else
              local.get 4
              i32.const 3
              i32.eq
              local.get 4
              i32.const 7
              i32.eq
              i32.or
              local.get 4
              i32.const 8
              i32.eq
              i32.or
              if (result i32) ;; label = @6
                i32.const 16
              else
                local.get 4
                i32.const 4
                i32.eq
                local.get 4
                i32.const 9
                i32.eq
                i32.or
                local.get 4
                i32.const 11
                i32.eq
                i32.or
                if (result i32) ;; label = @7
                  i32.const 24
                else
                  i32.const 8
                  local.get 6
                  i32.const 8
                  i32.mul
                  i32.add
                end
              end
            end
            local.set 3
            local.get 3
            call $rt_alloc
            local.set 2
            local.get 2
            local.get 1
            local.get 3
            memory.copy
            local.get 1
            i64.const -72057594037927936
            local.get 2
            i64.extend_i32_u
            i64.or
            i64.store
            local.get 4
            i32.const 3
            i32.eq
            local.get 4
            i32.const 8
            i32.eq
            i32.or
            if ;; label = @5
              local.get 5
              i32.const 1
              i32.and
              if ;; label = @6
                local.get 2
                i64.load offset=8
                i32.wrap_i64
                call $rt_retain_i32
                local.set 8
                local.get 2
                local.get 8
                i64.extend_i32_s
                i64.store offset=8
              end
            end
            local.get 4
            i32.const 4
            i32.eq
            if ;; label = @5
              local.get 5
              i32.const 1
              i32.and
              if ;; label = @6
                local.get 2
                i64.load offset=8
                i32.wrap_i64
                call $rt_retain_i32
                local.set 8
                local.get 2
                local.get 8
                i64.extend_i32_s
                i64.store offset=8
              end
              local.get 2
              local.set 9
              local.get 2
              local.set 10
              block ;; label = @6
                loop ;; label = @7
                  local.get 10
                  i64.load offset=16
                  i32.wrap_i64
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.le_s
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_mark
                  i32.lt_u
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_from
                  i32.ge_u
                  local.get 8
                  global.get $heap_ptr
                  i32.lt_u
                  i32.and
                  if ;; label = @8
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  local.set 1
                  local.get 1
                  i64.load
                  local.set 11
                  local.get 11
                  i64.const 56
                  i64.shr_u
                  i32.wrap_i64
                  i32.const 255
                  i32.and
                  local.set 4
                  local.get 4
                  i32.const 255
                  i32.eq
                  if ;; label = @8
                    local.get 11
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 4
                  i32.const 4
                  i32.eq
                  local.get 4
                  i32.const 9
                  i32.eq
                  i32.or
                  local.get 4
                  i32.const 11
                  i32.eq
                  i32.or
                  if ;; label = @8
                    local.get 8
                    local.set 7
                    block ;; label = @9
                      loop ;; label = @10
                        local.get 8
                        i32.const 0
                        i32.le_s
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_mark
                        i32.lt_u
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_from
                        i32.ge_u
                        local.get 8
                        global.get $heap_ptr
                        i32.lt_u
                        i32.and
                        br_if 1 (;@9;)
                        local.get 8
                        local.set 1
                        local.get 1
                        i64.load
                        local.set 11
                        local.get 11
                        i64.const 56
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 255
                        i32.and
                        local.set 4
                        local.get 4
                        i32.const 255
                        i32.eq
                        br_if 1 (;@9;)
                        local.get 4
                        i32.const 4
                        i32.eq
                        local.get 4
                        i32.const 9
                        i32.eq
                        i32.or
                        local.get 4
                        i32.const 11
                        i32.eq
                        i32.or
                        i32.eqz
                        br_if 1 (;@9;)
                        local.get 11
                        i64.const 32
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 65535
                        i32.and
                        local.set 5
                        i32.const 24
                        call $rt_alloc
                        local.set 2
                        local.get 2
                        local.get 1
                        i32.const 24
                        memory.copy
                        local.get 1
                        i64.const -72057594037927936
                        local.get 2
                        i64.extend_i32_u
                        i64.or
                        i64.store
                        local.get 4
                        i32.const 4
                        i32.eq
                        if ;; label = @11
                          local.get 5
                          i32.const 1
                          i32.and
                          if ;; label = @12
                            local.get 2
                            i64.load offset=8
                            i32.wrap_i64
                            call $rt_retain_i32
                            local.set 8
                            local.get 2
                            local.get 8
                            i64.extend_i32_s
                            i64.store offset=8
                          end
                        end
                        local.get 4
                        i32.const 11
                        i32.eq
                        if ;; label = @11
                          local.get 2
                          i64.load offset=8
                          i32.wrap_i64
                          call $rt_retain_i32
                          local.set 8
                          local.get 2
                          local.get 8
                          i64.extend_i32_s
                          i64.store offset=8
                        end
                        local.get 2
                        i64.load offset=16
                        i32.wrap_i64
                        local.set 8
                        br 0 (;@10;)
                      end
                    end
                    local.get 7
                    i64.load
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  call $rt_retain_i32
                  local.set 8
                  local.get 10
                  i32.const 16
                  i32.add
                  local.get 8
                  i64.extend_i32_s
                  i64.store
                  br 1 (;@6;)
                end
              end
              local.get 9
              return
            end
            local.get 4
            i32.const 9
            i32.eq
            if ;; label = @5
              local.get 2
              local.set 9
              local.get 2
              local.set 10
              block ;; label = @6
                loop ;; label = @7
                  local.get 10
                  i64.load offset=16
                  i32.wrap_i64
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.le_s
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_mark
                  i32.lt_u
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_from
                  i32.ge_u
                  local.get 8
                  global.get $heap_ptr
                  i32.lt_u
                  i32.and
                  if ;; label = @8
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  local.set 1
                  local.get 1
                  i64.load
                  local.set 11
                  local.get 11
                  i64.const 56
                  i64.shr_u
                  i32.wrap_i64
                  i32.const 255
                  i32.and
                  local.set 4
                  local.get 4
                  i32.const 255
                  i32.eq
                  if ;; label = @8
                    local.get 11
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 4
                  i32.const 4
                  i32.eq
                  local.get 4
                  i32.const 9
                  i32.eq
                  i32.or
                  local.get 4
                  i32.const 11
                  i32.eq
                  i32.or
                  if ;; label = @8
                    local.get 8
                    local.set 7
                    block ;; label = @9
                      loop ;; label = @10
                        local.get 8
                        i32.const 0
                        i32.le_s
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_mark
                        i32.lt_u
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_from
                        i32.ge_u
                        local.get 8
                        global.get $heap_ptr
                        i32.lt_u
                        i32.and
                        br_if 1 (;@9;)
                        local.get 8
                        local.set 1
                        local.get 1
                        i64.load
                        local.set 11
                        local.get 11
                        i64.const 56
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 255
                        i32.and
                        local.set 4
                        local.get 4
                        i32.const 255
                        i32.eq
                        br_if 1 (;@9;)
                        local.get 4
                        i32.const 4
                        i32.eq
                        local.get 4
                        i32.const 9
                        i32.eq
                        i32.or
                        local.get 4
                        i32.const 11
                        i32.eq
                        i32.or
                        i32.eqz
                        br_if 1 (;@9;)
                        local.get 11
                        i64.const 32
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 65535
                        i32.and
                        local.set 5
                        i32.const 24
                        call $rt_alloc
                        local.set 2
                        local.get 2
                        local.get 1
                        i32.const 24
                        memory.copy
                        local.get 1
                        i64.const -72057594037927936
                        local.get 2
                        i64.extend_i32_u
                        i64.or
                        i64.store
                        local.get 4
                        i32.const 4
                        i32.eq
                        if ;; label = @11
                          local.get 5
                          i32.const 1
                          i32.and
                          if ;; label = @12
                            local.get 2
                            i64.load offset=8
                            i32.wrap_i64
                            call $rt_retain_i32
                            local.set 8
                            local.get 2
                            local.get 8
                            i64.extend_i32_s
                            i64.store offset=8
                          end
                        end
                        local.get 4
                        i32.const 11
                        i32.eq
                        if ;; label = @11
                          local.get 2
                          i64.load offset=8
                          i32.wrap_i64
                          call $rt_retain_i32
                          local.set 8
                          local.get 2
                          local.get 8
                          i64.extend_i32_s
                          i64.store offset=8
                        end
                        local.get 2
                        i64.load offset=16
                        i32.wrap_i64
                        local.set 8
                        br 0 (;@10;)
                      end
                    end
                    local.get 7
                    i64.load
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  call $rt_retain_i32
                  local.set 8
                  local.get 10
                  i32.const 16
                  i32.add
                  local.get 8
                  i64.extend_i32_s
                  i64.store
                  br 1 (;@6;)
                end
              end
              local.get 9
              return
            end
            local.get 4
            i32.const 1
            i32.eq
            local.get 4
            i32.const 5
            i32.eq
            i32.or
            local.get 4
            i32.const 2
            i32.eq
            i32.or
            local.get 4
            i32.const 12
            i32.eq
            i32.or
            local.get 4
            i32.const 14
            i32.eq
            i32.or
            if ;; label = @5
              i32.const 0
              local.set 7
              block ;; label = @6
                loop ;; label = @7
                  local.get 7
                  local.get 6
                  i32.ge_u
                  br_if 1 (;@6;)
                  local.get 7
                  i32.const 16
                  i32.ge_u
                  br_if 1 (;@6;)
                  local.get 5
                  i32.const 1
                  local.get 7
                  i32.shl
                  i32.and
                  i32.eqz
                  if ;; label = @8
                  else
                    local.get 2
                    i32.const 8
                    i32.add
                    local.get 7
                    i32.const 8
                    i32.mul
                    i32.add
                    local.set 9
                    local.get 9
                    i64.load
                    i32.wrap_i64
                    call $rt_retain_i32
                    local.set 8
                    local.get 9
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                  end
                  local.get 7
                  i32.const 1
                  i32.add
                  local.set 7
                  br 0 (;@7;)
                end
              end
            end
            ;; HAMT_NODE (13) / HAMT_COLLISION (15): retain every field as ptr.
            local.get 4
            i32.const 13
            i32.eq
            local.get 4
            i32.const 15
            i32.eq
            i32.or
            if ;; label = @5
              i32.const 0
              local.set 7
              block ;; label = @6
                loop ;; label = @7
                  local.get 7
                  local.get 6
                  i32.ge_u
                  br_if 1 (;@6;)
                  local.get 2
                  i32.const 8
                  i32.add
                  local.get 7
                  i32.const 8
                  i32.mul
                  i32.add
                  local.set 9
                  local.get 9
                  i64.load
                  i32.wrap_i64
                  call $rt_retain_i32
                  local.set 8
                  local.get 9
                  local.get 8
                  i64.extend_i32_s
                  i64.store
                  local.get 7
                  i32.const 1
                  i32.add
                  local.set 7
                  br 0 (;@7;)
                end
              end
            end
            local.get 4
            i32.const 10
            i32.eq
            if ;; label = @5
              local.get 5
              i32.const 1
              i32.and
              if ;; label = @6
                i32.const 0
                local.set 7
                block ;; label = @7
                  loop ;; label = @8
                    local.get 7
                    local.get 6
                    i32.ge_u
                    br_if 1 (;@7;)
                    local.get 2
                    i32.const 8
                    i32.add
                    local.get 7
                    i32.const 8
                    i32.mul
                    i32.add
                    local.set 9
                    local.get 9
                    i64.load
                    i32.wrap_i64
                    call $rt_retain_i32
                    local.set 8
                    local.get 9
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 7
                    i32.const 1
                    i32.add
                    local.set 7
                    br 0 (;@8;)
                  end
                end
              end
            end
            local.get 4
            i32.const 11
            i32.eq
            if ;; label = @5
              local.get 2
              i64.load offset=8
              i32.wrap_i64
              call $rt_retain_i32
              local.set 8
              local.get 2
              local.get 8
              i64.extend_i32_s
              i64.store offset=8
              local.get 2
              local.set 9
              local.get 2
              local.set 10
              block ;; label = @6
                loop ;; label = @7
                  local.get 10
                  i64.load offset=16
                  i32.wrap_i64
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.le_s
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_mark
                  i32.lt_u
                  if ;; label = @8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    br 2 (;@6;)
                  end
                  local.get 8
                  global.get $collect_from
                  i32.ge_u
                  local.get 8
                  global.get $heap_ptr
                  i32.lt_u
                  i32.and
                  if ;; label = @8
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  local.set 1
                  local.get 1
                  i64.load
                  local.set 11
                  local.get 11
                  i64.const 56
                  i64.shr_u
                  i32.wrap_i64
                  i32.const 255
                  i32.and
                  local.set 4
                  local.get 4
                  i32.const 255
                  i32.eq
                  if ;; label = @8
                    local.get 11
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 4
                  i32.const 4
                  i32.eq
                  local.get 4
                  i32.const 9
                  i32.eq
                  i32.or
                  local.get 4
                  i32.const 11
                  i32.eq
                  i32.or
                  if ;; label = @8
                    local.get 8
                    local.set 7
                    block ;; label = @9
                      loop ;; label = @10
                        local.get 8
                        i32.const 0
                        i32.le_s
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_mark
                        i32.lt_u
                        br_if 1 (;@9;)
                        local.get 8
                        global.get $collect_from
                        i32.ge_u
                        local.get 8
                        global.get $heap_ptr
                        i32.lt_u
                        i32.and
                        br_if 1 (;@9;)
                        local.get 8
                        local.set 1
                        local.get 1
                        i64.load
                        local.set 11
                        local.get 11
                        i64.const 56
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 255
                        i32.and
                        local.set 4
                        local.get 4
                        i32.const 255
                        i32.eq
                        br_if 1 (;@9;)
                        local.get 4
                        i32.const 4
                        i32.eq
                        local.get 4
                        i32.const 9
                        i32.eq
                        i32.or
                        local.get 4
                        i32.const 11
                        i32.eq
                        i32.or
                        i32.eqz
                        br_if 1 (;@9;)
                        local.get 11
                        i64.const 32
                        i64.shr_u
                        i32.wrap_i64
                        i32.const 65535
                        i32.and
                        local.set 5
                        i32.const 24
                        call $rt_alloc
                        local.set 2
                        local.get 2
                        local.get 1
                        i32.const 24
                        memory.copy
                        local.get 1
                        i64.const -72057594037927936
                        local.get 2
                        i64.extend_i32_u
                        i64.or
                        i64.store
                        local.get 4
                        i32.const 4
                        i32.eq
                        if ;; label = @11
                          local.get 5
                          i32.const 1
                          i32.and
                          if ;; label = @12
                            local.get 2
                            i64.load offset=8
                            i32.wrap_i64
                            call $rt_retain_i32
                            local.set 8
                            local.get 2
                            local.get 8
                            i64.extend_i32_s
                            i64.store offset=8
                          end
                        end
                        local.get 4
                        i32.const 11
                        i32.eq
                        if ;; label = @11
                          local.get 2
                          i64.load offset=8
                          i32.wrap_i64
                          call $rt_retain_i32
                          local.set 8
                          local.get 2
                          local.get 8
                          i64.extend_i32_s
                          i64.store offset=8
                        end
                        local.get 2
                        i64.load offset=16
                        i32.wrap_i64
                        local.set 8
                        br 0 (;@10;)
                      end
                    end
                    local.get 7
                    i64.load
                    i32.wrap_i64
                    local.set 8
                    local.get 10
                    i32.const 16
                    i32.add
                    local.get 8
                    i64.extend_i32_s
                    i64.store
                    local.get 8
                    local.set 10
                    br 1 (;@7;)
                  end
                  local.get 8
                  call $rt_retain_i32
                  local.set 8
                  local.get 10
                  i32.const 16
                  i32.add
                  local.get 8
                  i64.extend_i32_s
                  i64.store
                  br 1 (;@6;)
                end
              end
              local.get 9
              return
            end
            local.get 2
          end
        end
      end
    end
  )

(export "rt_collect_begin" (func $rt_collect_begin))
(export "rt_rebase_i32" (func $rt_rebase_i32))
(export "rt_collect_end" (func $rt_collect_end))
(export "rt_retain_i32" (func $rt_retain_i32))
