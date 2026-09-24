//! Whether a tool a Lean-backed certificate test needs is on `PATH`.
//!
//! A test whose toolchain is missing skips, so a developer without Lean can
//! still run the suite. The certification lanes set `AVER_CERT_REQUIRE_LEAN=1`:
//! there a missing tool fails the test, so a lane can never pass with nothing
//! checked.

use std::process::Command;

/// The variable the certification lanes set to turn a skip into a failure.
pub const REQUIRE_LEAN_ENV: &str = "AVER_CERT_REQUIRE_LEAN";

/// `true` when `program --version` runs. When it does not, `false` so the
/// caller skips, or a panic when `AVER_CERT_REQUIRE_LEAN=1`.
pub fn tool_available(program: &str) -> bool {
    if Command::new(program).arg("--version").output().is_ok() {
        return true;
    }
    if std::env::var(REQUIRE_LEAN_ENV).is_ok_and(|value| value == "1") {
        panic!(
            "`{program}` is not available, and {REQUIRE_LEAN_ENV}=1 requires every \
             Lean-backed certificate test to run"
        );
    }
    false
}

/// `true` when `lake` runs; see [`tool_available`].
pub fn lake_available() -> bool {
    tool_available("lake")
}
