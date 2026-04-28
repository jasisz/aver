;; Memory and globals shared across all runtime fragments.
;; This fragment must come first in the concat order — other fragments
;; reference $heap_ptr and rely on memory being declared.

(memory $mem 1)
(export "memory" (memory $mem))

(global $heap_ptr (mut i32) (i32.const 128))
(export "heap_ptr" (global $heap_ptr))

;; Compaction state — one mutable scratch global per phase. Used by
;; rt_collect_begin / rt_collect_end / rt_rebase_i32 / rt_retain_i32.
;; See `runtime/wat/collect.part.wat` for the bodies.
(global $collect_mark (mut i32) (i32.const 0))
(export "collect_mark" (global $collect_mark))
(global $collect_from (mut i32) (i32.const 0))
(export "collect_from" (global $collect_from))
(global $collect_dst (mut i32) (i32.const 0))
(export "collect_dst" (global $collect_dst))
