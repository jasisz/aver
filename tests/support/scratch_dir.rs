//! Self-removing scratch directories for the certificate suites.
//!
//! Every certificate test compiles a fixture into a temporary directory and
//! then builds the emitted Lean package there, so one directory is around
//! 119 MB — nearly all of it the `cert/.lake` build tree. Cleanup used to be a
//! trailing `remove_dir_all` statement at the end of each test, which only runs
//! when the test reaches its last line: a failing assertion unwinds straight
//! past it. That is backwards, because a failing certificate test is exactly
//! the one a developer re-runs.
//!
//! Binding the directory to a value whose `Drop` removes it makes the path that
//! removes the directory the same path that leaves the scope — return, `?`,
//! panic, or assertion failure alike.
//!
//! The random name suffix also settles the collisions the nanosecond-stamped
//! helpers hit when parallel tests asked for a directory in the same tick.

use std::ffi::OsStr;
use std::path::Path;

/// A temporary directory that removes itself, and everything under it, when it
/// goes out of scope.
///
/// It stands in for the `PathBuf` the suites used to pass around: it derefs to
/// `Path`, so `join`, `display`, and `&dir` arguments keep working, and it
/// converts to `OsStr` so it can be handed to `Command` as an argument or an
/// environment value.
pub struct ScratchDir(tempfile::TempDir);

/// Creates an empty scratch directory named `aver-{prefix}-` plus a random
/// suffix, removed when the returned value is dropped.
pub fn temp_dir(prefix: &str) -> ScratchDir {
    ScratchDir(
        tempfile::Builder::new()
            .prefix(&format!("aver-{prefix}-"))
            .tempdir()
            .expect("create scratch directory"),
    )
}

impl std::ops::Deref for ScratchDir {
    type Target = Path;

    fn deref(&self) -> &Path {
        self.0.path()
    }
}

impl AsRef<Path> for ScratchDir {
    fn as_ref(&self) -> &Path {
        self.0.path()
    }
}

impl AsRef<OsStr> for ScratchDir {
    fn as_ref(&self) -> &OsStr {
        self.0.path().as_os_str()
    }
}
