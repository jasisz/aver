//! Allocation + heap-compaction GC helpers — every body has been
//! migrated to a `runtime/wat/*.part.wat` fragment. Kept as a stub
//! so its `mod alloc;` declaration in `runtime/mod.rs` still
//! resolves. Migrated:
//!
//!   - rt_alloc, rt_truncate         → wat/alloc.part.wat,
//!                                      wat/truncate.part.wat
//!   - rt_obj_*, rt_unwrap*, rt_wrap* → wat/obj.part.wat,
//!                                      wat/unwrap.part.wat,
//!                                      wat/wrap.part.wat
//!   - rt_collect_begin / rt_collect_end / rt_rebase_i32 /
//!     rt_retain_i32                 → wat/collect.part.wat
//!     (uses $collect_mark / $collect_from / $collect_dst mutable
//!      globals also imported from aver_runtime)
