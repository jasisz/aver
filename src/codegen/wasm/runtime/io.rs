//! IO helpers — every body has been migrated:
//!
//!   - rt_int_to_str / rt_float_to_str → wat/int_to_str.part.wat
//!   - rt_fd_write_buf was the per-adapter shim that branched on
//!     Aver vs WASI inline. With the aver→wasi bridge (separate
//!     `aver_to_wasi.wasm` module), user.wasm always emits Aver-style
//!     `aver/console_print(ptr, len)` calls directly — fd_write_buf
//!     is gone, the WASI translation lives in the bridge.
//!
//! Kept as a stub so the `mod io;` declaration in `runtime/mod.rs`
//! still resolves.
