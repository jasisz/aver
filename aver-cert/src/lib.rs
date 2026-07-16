//! Compiler-independent artifact certification for Aver WebAssembly.
//!
//! This crate owns the byte-derived certificate engine, the public certificate
//! format, and the exact Lean soundness wall. It deliberately has no dependency
//! on the Aver compiler, language runtime, or memory runtime.

#![forbid(unsafe_code)]

pub mod format;
#[cfg(any(feature = "engine", feature = "verify"))]
pub mod wall;

#[cfg(any(feature = "engine", feature = "plans"))]
mod engine;
#[cfg(any(feature = "engine", feature = "plans"))]
pub use engine::*;

#[cfg(feature = "verify")]
mod cache;
#[cfg(feature = "verify")]
mod lean_process;
#[cfg(feature = "verify")]
mod prelude_cache;
#[cfg(feature = "verify")]
mod verifier;
#[cfg(feature = "verify")]
pub use verifier::{
    ARTIFACT_DECODE_LINE, CheckVerdict, Explanation, Verdict, check, explain, verify,
};
