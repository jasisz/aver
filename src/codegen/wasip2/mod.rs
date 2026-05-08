//! `--target wasip2` — WASI 0.2 / Component Model output.
//!
//! Wraps a wasm-gc core module via `wit-component` with the
//! preview-1 adapter from `wasi-preview1-component-adapter-provider`,
//! emits `.component.wasm` plus a sibling `.wit`. Peer target with
//! `--target wasm-gc`, not a successor — the wasm-gc backend keeps
//! shipping for browsers / Workers / JS hosts via `aver/*` host
//! imports; `wasip2` is for wasmtime / Spin / NGINX Unit / wasmCloud
//! / every Component Model host via canonical WIT imports.
//!
//! Phase 1 of 0.18 "Span" — see `docs/wasip2.md` for the full
//! seven-point contract this module enforces and the effect map.
//!
//! # Architectural assumption
//!
//! GC values do not cross the component boundary. The component
//! contract guarantees per-instantiation `Map<K, V>` / `List<T>` /
//! `Vector<T>` / records / variants stay inside the user core
//! module; the public WIT surface uses canonical types only. The
//! POC tests in `tests/wasip2_poc.rs` pin this — a core module
//! that uses `struct.new` / `struct.get` / `struct.set` internally
//! validates as a Component as long as the boundary stays
//! canonical.
//!
//! # Sub-modules
//!
//! - `error.rs` — typed error enum, source-side diagnostics.
//! - `wrap.rs` — `wit-component::ComponentEncoder` invocation,
//!   adapter selection per world.
//! - `wit.rs` — WIT source emission. Phase 1.2a uses inline
//!   `format!` since the world body is small and template-like.
//!   Switches to `wit-encoder` (structured builder) only if
//!   per-effect imports beyond the standard WASI worlds force it.
//! - `wasi_bundle.rs` — vendored WASI 0.2.4 WIT package set,
//!   embedded via `include_str!` and pushed into the per-build
//!   `Resolve` so the user world can `include wasi:cli/command;`.

mod effect_check;
mod error;
mod wasi_bundle;
mod wit;
mod wrap;

pub use effect_check::{UnsupportedEffect, UnsupportedReason, check_supported_effects, render_errors};
pub use error::Wasip2Error;
pub use wrap::{Wasip2World, compile_to_component};
