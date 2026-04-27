;; Memory and globals shared across all runtime fragments.
;; This fragment must come first in the concat order — other fragments
;; reference $heap_ptr and rely on memory being declared.

(memory $mem 1)
(export "memory" (memory $mem))

(global $heap_ptr (mut i32) (i32.const 128))
(export "heap_ptr" (global $heap_ptr))
