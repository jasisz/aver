//! Typed error for the wasip2 codegen path.
//!
//! Kept separate so the surface is small and easy to extend as
//! Phase 1 fills in effect mappings (Phase 1.2-1.4) and Phase 1.6
//! adds the `target-effect-unsupported` reject path for `Terminal`,
//! `Http`, `Tcp`, and `HttpServer` under `--target wasip2`.

use std::fmt;

#[derive(Debug)]
pub enum Wasip2Error {
    /// Component encoder rejected the wrap step. Wraps the
    /// `wit-component` error message verbatim — usually a missing
    /// import the adapter cannot satisfy or a malformed core
    /// module.
    Wrap(String),
    /// `wasmparser::Validator` rejected the produced component.
    /// Should never fire under normal flow — `wit-component`
    /// validates its own output. Carries the validator's message
    /// when it does.
    Validation(String),
    /// A surface feature is not yet wired by the current Phase 1
    /// increment. Distinct from `target-effect-unsupported` (which
    /// is a permanent rejection because the target cannot ever
    /// support the effect — see Phase 1.6 / `Terminal.*`).
    NotImplemented(String),
}

impl fmt::Display for Wasip2Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Wasip2Error::Wrap(msg) => write!(f, "wasip2: component wrap failed — {msg}"),
            Wasip2Error::Validation(msg) => {
                write!(f, "wasip2: component validation failed — {msg}")
            }
            Wasip2Error::NotImplemented(msg) => {
                write!(f, "wasip2: not yet implemented — {msg}")
            }
        }
    }
}

impl std::error::Error for Wasip2Error {}
