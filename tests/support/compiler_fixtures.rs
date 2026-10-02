//! The compiler's own fixtures that name the generated `__` protocol.
//!
//! A program may not write a name the compiler generates; the parser
//! refuses it. Fixtures that test the generated protocol itself (laws over
//! generated functions, a hand-driven protocol) are let through by
//! `AVER_INTERNAL_COMPILER_FIXTURES=1`, an exception for this test suite
//! only. Every test binary that reads those fixtures calls [`allow`] before
//! parsing one or spawning `aver` on one; a spawned `aver` inherits it.

use std::sync::Once;

pub const COMPILER_FIXTURES_ENV: &str = "AVER_INTERNAL_COMPILER_FIXTURES";

/// Let this test process, and every `aver` it spawns, read the compiler's
/// own fixtures.
pub fn allow() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        // SAFETY: set once, to the same value, before the fixtures are read;
        // nothing in the test process reads the environment outside std.
        unsafe { std::env::set_var(COMPILER_FIXTURES_ENV, "1") };
    });
}
