//! Typed error for the wasip2 codegen path.
//!
//! Kept separate so the surface is small and easy to extend as
//! the Component Model wrapper evolves. Standard capability target
//! availability is decided before this layer is entered.

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
    /// increment. Distinct from a capability target-manifest rejection,
    /// which describes an operation the selected target cannot bind.
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
