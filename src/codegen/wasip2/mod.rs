//! `--target wasip2` — WASI 0.2 / Component Model output.
//!
//! Wraps a wasm-gc core module via `wit-component`. The core module
//! already imports canonical-ABI WIT functions (e.g. `wasi:cli/stdout`,
//! `wasi:filesystem/preopens`, `wasi:io/streams`) — there is no
//! preview-1 adapter, no shim layer; effects lower directly. Emits
//! `.component.wasm` plus a sibling `.wit`. Peer target with
//! `--target wasm-gc`, not a successor — the wasm-gc backend keeps
//! shipping for browsers / Workers / JS hosts via `aver/*` host
//! imports; `wasip2` is for wasmtime / Spin / NGINX Unit / wasmCloud
//! / every Component Model host via canonical WIT imports.
//!
//! Shipped in 0.18 "Span" — see `docs/wasip2.md` for the full
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
//! - `wrap.rs` — `wit-component::ComponentEncoder` invocation. No
//!   adapters: the core module already speaks canonical ABI.
//! - `plan.rs` — the feature-independent, typed custom-capability
//!   transport plan shared by target accounting and code generation.
//! - `wit.rs` — structured WIT package emission through `wit-encoder`.
//!   Generated interfaces and embedded component metadata consume the
//!   same plan rather than rediscovering one another.
//! - `wasi_bundle.rs` — vendored WASI 0.2.4 WIT package set,
//!   embedded via `include_str!` and pushed into the per-build
//!   `Resolve` so the user world can `include wasi:cli/command;`.

#[cfg(feature = "wasip2")]
mod error;
mod plan;
#[cfg(feature = "wasip2")]
mod wasi_bundle;
#[cfg(feature = "wasip2")]
mod wit;
#[cfg(feature = "wasip2")]
mod wrap;

#[cfg(feature = "wasip2")]
pub use error::Wasip2Error;
pub use plan::{
    CapabilityWitInterfacePlan, CapabilityWitOperationPlan, CapabilityWitParameterPlan,
    CapabilityWitPlan, CapabilityWitType, CapabilityWitTypePosition, CapabilityWitUnsupported,
};
#[cfg(feature = "wasip2")]
pub use wit::{emit_world_wit, emit_world_wit_with_capabilities};
#[cfg(feature = "wasip2")]
pub use wrap::{Wasip2World, compile_to_component, compile_to_component_with_capabilities};
