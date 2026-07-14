//! Compiler-independent artifact certification for Aver WebAssembly.
//!
//! This crate owns the byte-derived certificate engine, the public certificate
//! format, and the exact Lean soundness wall. It deliberately has no dependency
//! on the Aver compiler, language runtime, or memory runtime.

#![forbid(unsafe_code)]

pub mod format;

#[cfg(feature = "engine")]
mod engine;
#[cfg(feature = "engine")]
pub use engine::*;
