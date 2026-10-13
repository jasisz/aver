//! The stack the embedded proof code runs on. The proof kernel
//! (`crate::proof_kernel`) and the standard plans (`crate::proof_std`) are
//! Aver compiled to Rust: they recurse over terms and proofs, and how deep
//! is decided by the script they are given, not by the thread that happens
//! to call them. Every entry into either tree goes through [`run`], so the
//! caller's own stack (the 2 MiB of a test thread, a lowered `ulimit -s`)
//! does not decide whether a proof is checked.

/// The same reservation generated Rust programs and the self-host give
/// their main thread (`src/codegen/rust/mod.rs`, `src/self_host/main.rs`).
/// It is address space, not memory: pages are committed only as the stack
/// grows, so a shallow check costs what it touches.
#[cfg(not(target_family = "wasm"))]
const STACK_BYTES: usize = 256 * 1024 * 1024;

/// Run `work` on a thread with [`STACK_BYTES`] of stack and return what it
/// returns. A panic in `work`, or a thread that cannot be started, is an
/// `Err` naming `what`: the caller reads it as a refusal, never as an
/// accepted proof.
#[cfg(not(target_family = "wasm"))]
pub(crate) fn run<T, F>(what: &str, work: F) -> Result<T, String>
where
    T: Send,
    F: FnOnce() -> T + Send,
{
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .name("aver-proof".to_string())
            .stack_size(STACK_BYTES)
            .spawn_scoped(scope, work)
            .map_err(|why| format!("{what} could not start its thread: {why}"))?
            .join()
            .map_err(|_| format!("{what} stopped with an internal error"))
    })
}

/// A wasm build has no threads to start: the work runs on the caller's
/// stack. The wasm targets build with `panic = "abort"`, so a panic there
/// traps the module instead of unwinding to this `catch_unwind`; the `Err`
/// arm is reached only on a build that unwinds. Either way a panic is never
/// an accepted proof.
#[cfg(target_family = "wasm")]
pub(crate) fn run<T, F>(what: &str, work: F) -> Result<T, String>
where
    T: Send,
    F: FnOnce() -> T + Send,
{
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(work))
        .map_err(|_| format!("{what} stopped with an internal error"))
}

#[cfg(all(test, not(target_family = "wasm")))]
mod tests {
    use super::run;

    /// Stack a frame of this recursion certainly uses, so `depth` frames
    /// need more than the caller's thread has.
    fn descend(depth: usize) -> usize {
        let frame = std::hint::black_box([depth as u8; 1024]);
        if depth == 0 {
            return frame[0] as usize;
        }
        1 + descend(depth - 1) + (frame[1023] as usize & 0)
    }

    #[test]
    fn work_too_deep_for_the_calling_thread_runs() {
        // 8 MiB of frames at the least, from a thread with 256 KiB.
        let depth = 8 * 1024;
        let got = std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(move || run("the work", || descend(depth)))
            .unwrap()
            .join()
            .unwrap();
        assert_eq!(got, Ok(depth));
    }

    #[test]
    fn a_panic_in_the_work_is_an_error_not_an_answer() {
        let got: Result<bool, String> = run("the work", || panic!("stop"));
        assert_eq!(
            got,
            Err("the work stopped with an internal error".to_string())
        );
    }

    #[test]
    fn the_work_may_borrow_from_the_caller() {
        let text = String::from("borrowed");
        assert_eq!(run("the work", || text.len()), Ok(8));
    }
}
