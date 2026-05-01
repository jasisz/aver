//! `Float.*` builtin helpers.
//!
//! Stubs for now — `Float.toString` lands when String repr is wired
//! (phase 3c proper). `Float.fromInt` and `Int.fromFloat` already
//! emit inline at the call site (single wasm instruction each), no
//! helper needed.

use super::StaticBuiltin;

pub(super) const SPECS: &[StaticBuiltin] = &[];
