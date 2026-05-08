//! Component wrapping: core wasm + preview-1 adapter →
//! `.component.wasm`.
//!
//! The actual lowering of effects to preview-1 imports lives in the
//! wasm-gc backend (Phase 1.2+ — modifies `src/codegen/wasm_gc/
//! effects.rs` to emit `wasi_snapshot_preview1::*` imports when the
//! target is `Wasip2`). This module is target-agnostic past that
//! point: it accepts core wasm bytes, picks the adapter that
//! matches the requested world, and asks `wit-component` to wrap.

use wasi_preview1_component_adapter_provider::{
    WASI_SNAPSHOT_PREVIEW1_ADAPTER_NAME, WASI_SNAPSHOT_PREVIEW1_COMMAND_ADAPTER,
    WASI_SNAPSHOT_PREVIEW1_PROXY_ADAPTER,
};
use wit_component::ComponentEncoder;

use super::error::Wasip2Error;

/// Which WIT world the component targets. Keep in sync with
/// `crate::main::cli::Wasip2World` — we duplicate the enum here so
/// the codegen module is independent of CLI types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Wasip2World {
    /// `wasi:cli/command` — long-running process exporting
    /// `wasi:cli/run`. Default for 0.18 "Span" Phase 1.
    CliCommand,
    /// `wasi:http/proxy` — HTTP server shape. Phase 3 / 0.19;
    /// compile-rejected in 0.18 unless trivially landable.
    HttpProxy,
}

impl Wasip2World {
    /// Display name in WIT shape (`wasi:NS/world`). Used in the
    /// emitted `.wit` and in CLI diagnostics.
    pub fn wit_name(self) -> &'static str {
        match self {
            Wasip2World::CliCommand => "wasi:cli/command",
            Wasip2World::HttpProxy => "wasi:http/proxy",
        }
    }

    fn adapter_bytes(self) -> &'static [u8] {
        match self {
            Wasip2World::CliCommand => WASI_SNAPSHOT_PREVIEW1_COMMAND_ADAPTER,
            Wasip2World::HttpProxy => WASI_SNAPSHOT_PREVIEW1_PROXY_ADAPTER,
        }
    }
}

/// Wrap a core wasm-gc module as a Component.
///
/// `core_wasm` is the output of the wasm-gc backend re-targeted at
/// `wasi_snapshot_preview1` imports. Returns the component bytes
/// alongside the WIT source emitted next to the artifact (per the
/// component contract in `docs/wasip2.md` — point 5).
///
/// Phase 1 status:
/// - `CliCommand` is supported end-to-end through this wrap call as
///   long as the core module's imports match what the COMMAND adapter
///   provides.
/// - `HttpProxy` is rejected with `Wasip2Error::NotImplemented` —
///   Phase 3 work, lands in 0.19 unless trivial.
pub fn compile_to_component(
    core_wasm: &[u8],
    world: Wasip2World,
) -> Result<(Vec<u8>, String), Wasip2Error> {
    if matches!(world, Wasip2World::HttpProxy) {
        return Err(Wasip2Error::NotImplemented(
            "world `wasi:http/proxy` (Phase 3) is not wired in 0.18 Phase 1 — \
             use `--world wasi:cli/command` for the long-running process shape, \
             or wait for the Phase 3 / 0.19 increment"
                .to_string(),
        ));
    }

    let component = ComponentEncoder::default()
        .module(core_wasm)
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::module rejected core: {e}")))?
        .validate(true)
        .adapter(WASI_SNAPSHOT_PREVIEW1_ADAPTER_NAME, world.adapter_bytes())
        .map_err(|e| {
            Wasip2Error::Wrap(format!(
                "ComponentEncoder::adapter (preview1 → {}) rejected: {e}",
                world.wit_name()
            ))
        })?
        .encode()
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::encode failed: {e}")))?;

    let wit = super::wit::emit_world_wit(world);
    Ok((component, wit))
}
