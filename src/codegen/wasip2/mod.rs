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
//! - `wit.rs` — WIT emission via `wit-encoder`. The artifact
//!   shipped alongside the component is human-readable and is the
//!   source of truth for the import / export surface.

mod error;
mod wasi_bundle;
mod wit;
mod wrap;

pub use error::Wasip2Error;
pub use wrap::{Wasip2World, compile_to_component};
