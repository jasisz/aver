
        (module
          {decls}
          (func (export "helper") (param $a (ref null $aint)) (result (ref null $string))
            (local $n i64) (local $abs i64) (local $copy i64)
            (local $digits i32) (local $total i32) (local $i i32) (local $neg i32)
            (local $arr (ref null $string))
            (local $magc (ref null $mag)) (local $len i32) (local $li i32)
            (local $cur i64) (local $rem i64) (local $q i64) (local $allzero i32)
            (local $digbuf (ref null $string)) (local $dcount i32)
            (if (result (ref null $string)) (ref.is_null (struct.get $aint $magf (local.get $a)))
              (then
                ;; ── Small: format $small as the scalar helper does ──
                (local.set $n (struct.get $aint $small (local.get $a)))
                (if (result (ref null $string)) (i64.eqz (local.get $n))
                  (then (array.new $string (i32.const 48) (i32.const 1)))
                  (else
                    (local.set $neg (if (result i32) (i64.lt_s (local.get $n) (i64.const 0)) (then (i32.const 1)) (else (i32.const 0))))
                    (local.set $abs (if (result i64) (local.get $neg) (then (i64.sub (i64.const 0) (local.get $n))) (else (local.get $n))))
                    ;; count digits
                    (local.set $copy (local.get $abs)) (local.set $digits (i32.const 0))
                    (block $cd (loop $c
                      (br_if $cd (i64.eqz (local.get $copy)))
                      (local.set $digits (i32.add (local.get $digits) (i32.const 1)))
                      (local.set $copy (i64.div_u (local.get $copy) (i64.const 10)))
                      (br $c)))
                    (local.set $total (i32.add (local.get $digits) (local.get $neg)))
                    (local.set $arr (array.new_default $string (local.get $total)))
                    (local.set $i (i32.sub (local.get $total) (i32.const 1)))
                    (local.set $copy (local.get $abs))
                    (block $fd (loop $f
                      (br_if $fd (i32.lt_s (local.get $i) (local.get $neg)))
                      (array.set $string (local.get $arr) (local.get $i)
                        (i32.add (i32.const 48) (i32.wrap_i64 (i64.rem_u (local.get $copy) (i64.const 10)))))
                      (local.set $copy (i64.div_u (local.get $copy) (i64.const 10)))
                      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                      (br $f)))
                    (if (local.get $neg) (then (array.set $string (local.get $arr) (i32.const 0) (i32.const 45))))
                    (local.get $arr))))
              (else
                ;; ── Big: divmod-by-10 over 32-bit limbs ──
                (local.set $neg (if (result i32) (i32.lt_s (struct.get $aint $sign (local.get $a)) (i32.const 0)) (then (i32.const 1)) (else (i32.const 0))))
                ;; work on a mutable copy of the magnitude
                (local.set $len (array.len (struct.get $aint $magf (local.get $a))))
                (local.set $magc (array.new_default $mag (local.get $len)))
                (local.set $li (i32.const 0))
                (block $cpd (loop $cp
                  (br_if $cpd (i32.ge_u (local.get $li) (local.get $len)))
                  (array.set $mag (local.get $magc) (local.get $li)
                    (array.get $mag (struct.get $aint $magf (local.get $a)) (local.get $li)))
                  (local.set $li (i32.add (local.get $li) (i32.const 1)))
                  (br $cp)))
                ;; collect digits low→high into $digbuf (max ~10 digits/limb*len; len*10 is safe)
                (local.set $digbuf (array.new_default $string (i32.mul (local.get $len) (i32.const 10))))
                (local.set $dcount (i32.const 0))
                (block $dvd (loop $dv
                  ;; rem = 0; for li=len-1 down to 0: cur=(rem<<32)|limb; q=cur/10; rem=cur%10; limb=q
                  (local.set $rem (i64.const 0))
                  (local.set $li (local.get $len))
                  (block $dl (loop $d
                    (br_if $dl (i32.eqz (local.get $li)))
                    (local.set $li (i32.sub (local.get $li) (i32.const 1)))
                    (local.set $cur (i64.or (i64.shl (local.get $rem) (i64.const 32))
                                            (i64.and (array.get $mag (local.get $magc) (local.get $li)) (i64.const 0xffffffff))))
                    (local.set $q (i64.div_u (local.get $cur) (i64.const 10)))
                    (local.set $rem (i64.rem_u (local.get $cur) (i64.const 10)))
                    (array.set $mag (local.get $magc) (local.get $li) (local.get $q))
                    (br $d)))
                  ;; emit digit = rem
                  (array.set $string (local.get $digbuf) (local.get $dcount)
                    (i32.add (i32.const 48) (i32.wrap_i64 (local.get $rem))))
                  (local.set $dcount (i32.add (local.get $dcount) (i32.const 1)))
                  ;; loop while magnitude is non-zero
                  (local.set $allzero (i32.const 1))
                  (local.set $li (i32.const 0))
                  (block $zd (loop $z
                    (br_if $zd (i32.ge_u (local.get $li) (local.get $len)))
                    (if (i64.ne (array.get $mag (local.get $magc) (local.get $li)) (i64.const 0))
                      (then (local.set $allzero (i32.const 0)) (br $zd)))
                    (local.set $li (i32.add (local.get $li) (i32.const 1)))
                    (br $z)))
                  ;; exit when the magnitude has become all-zero; else loop.
                  (br_if $dvd (local.get $allzero))
                  (br $dv)))
                ;; build output: [neg '-'] then digits reversed
                (local.set $total (i32.add (local.get $dcount) (local.get $neg)))
                (local.set $arr (array.new_default $string (local.get $total)))
                (if (local.get $neg) (then (array.set $string (local.get $arr) (i32.const 0) (i32.const 45))))
                (local.set $i (i32.const 0))
                (block $od (loop $o
                  (br_if $od (i32.ge_u (local.get $i) (local.get $dcount)))
                  (array.set $string (local.get $arr)
                    (i32.add (local.get $neg) (local.get $i))
                    (array.get_u $string (local.get $digbuf) (i32.sub (i32.sub (local.get $dcount) (i32.const 1)) (local.get $i))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $o)))
                (local.get $arr)))))
    