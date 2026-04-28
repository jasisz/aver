;; Reset $heap_ptr back to a previously-saved boundary mark. Used by
;; the per-call frame compaction: caller saves heap_ptr on entry, on
;; return we drop everything allocated since.

(func $rt_truncate (param $mark i32)
  local.get $mark
  global.set $heap_ptr
)
(export "rt_truncate" (func $rt_truncate))
