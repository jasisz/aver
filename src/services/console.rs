//! Console capture compatibility for compiler tests and embedded wasm-gc.
//!
//! Console effect dispatch belongs to the source-owned `Console` capability.
//! These helpers only expose `aver-rt`'s capture-aware stdio sink to hosts.

pub fn capture_output<F, R>(f: F) -> (R, Vec<u8>, Vec<u8>)
where
    F: FnOnce() -> R,
{
    aver_rt::capture_console_output(f)
}

pub fn write_stdout_str(message: &str) {
    aver_rt::console_print(&message.to_string());
}

pub fn write_stderr_plain_str(message: &str) {
    aver_rt::console_error(&message.to_string());
}
