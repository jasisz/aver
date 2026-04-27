;; Byte-equal compare of two OBJ_STRING heap objects. Layout of each
;; object is (8-byte header, byte payload at offset 8). The header's
;; low 32 bits hold the byte length. Same pointer short-circuits to
;; equal; different lengths short-circuit to not-equal.

(func $rt_str_eq (param $a i32) (param $b i32) (result i32)
  (local $len_a i32)
  (local $len_b i32)
  (local $i i32)

  ;; Same pointer → equal.
  local.get $a
  local.get $b
  i32.eq
  if (result i32)
    i32.const 1
  else
    ;; len_a = header_a & 0xFFFFFFFF
    local.get $a
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $len_a

    ;; len_b = header_b & 0xFFFFFFFF
    local.get $b
    i64.load
    i64.const 0xFFFFFFFF
    i64.and
    i32.wrap_i64
    local.set $len_b

    ;; Different lengths → not equal.
    local.get $len_a
    local.get $len_b
    i32.ne
    if (result i32)
      i32.const 0
    else
      i32.const 0
      local.set $i

      block
        loop
          ;; if i >= len_a → done (equal)
          local.get $i
          local.get $len_a
          i32.ge_u
          br_if 1

          ;; Compare byte at $a+$i (offset 8) vs $b+$i (offset 8).
          local.get $a
          local.get $i
          i32.add
          i32.load8_u offset=8

          local.get $b
          local.get $i
          i32.add
          i32.load8_u offset=8

          i32.ne
          if
            i32.const 0
            return
          end

          local.get $i
          i32.const 1
          i32.add
          local.set $i
          br 0
        end
      end
      i32.const 1
    end
  end
)
(export "rt_str_eq" (func $rt_str_eq))
