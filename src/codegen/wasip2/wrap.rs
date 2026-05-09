//! Component wrapping: core wasm + component-type metadata →
//! `.component.wasm`.
//!
//! Direct WIT lowering — no preview-1 adapter. The wasm-gc backend
//! emits core imports/exports in canonical-ABI-compatible shapes;
//! we encode a `component-type:<world>` custom section describing
//! the WIT world (via `wit-component::metadata`) and append it to
//! the core bytes; `wit-component::ComponentEncoder::module(...)
//! .encode()` then produces the component. The host sees the WIT
//! view; it never sees Aver runtime layout. See
//! `feedback_aver_no_preview1_adapter` for the architectural
//! decision and `docs/wasip2.md` for the contract.

use wit_component::{ComponentEncoder, StringEncoding, embed_component_metadata};
use wit_parser::{Resolve, UnresolvedPackageGroup};

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
    /// compile-rejected in 0.18.
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

    /// Kebab-case world identifier inside our generated package.
    /// Phase 1 emits this as a top-level world in `aver:user`; later
    /// phases include or extend the upstream WASI world of the same
    /// shape (`wasi:cli/command` etc.) once the WASI WIT bundle is
    /// wired into the binary.
    pub(super) fn local_name(self) -> &'static str {
        match self {
            Wasip2World::CliCommand => "command",
            Wasip2World::HttpProxy => "http-proxy",
        }
    }
}

/// Wrap a core wasm-gc module as a Component.
///
/// `core_wasm` is the output of the wasm-gc backend re-targeted at
/// canonical-ABI-compatible boundary shapes. Returns the component
/// bytes alongside the WIT source emitted next to the artifact (per
/// the component contract in `docs/wasip2.md` — point 5).
///
/// Phase 1 transitional shape: the world declared in metadata is an
/// empty `world command {}` — Aver's effect-bearing core imports
/// arrive in Phase 1.2+, when WASI WIT bundles are wired so the
/// world can `include wasi:cli/command;`. The wrap pipeline itself
/// is end-to-end here; only the imported surface grows.
///
/// `HttpProxy` is rejected with `Wasip2Error::NotImplemented` —
/// Phase 3 / 0.19 work, lands when designed deliberately.
pub fn compile_to_component(
    core_wasm: &[u8],
    world: Wasip2World,
) -> Result<(Vec<u8>, String), Wasip2Error> {
    if matches!(world, Wasip2World::HttpProxy) {
        return Err(Wasip2Error::NotImplemented(
            "world `wasi:http/proxy` (Phase 3) is not wired in 0.18 — \
             use `--world wasi:cli/command` for the long-running process \
             shape, or wait for the deliberate Phase 3 / 0.19+ design"
                .to_string(),
        ));
    }

    // Build a Resolve seeded with the bundled WASI 0.2.4 WIT
    // package set, then push the user package on top. Order matters:
    // the user world `include`s `wasi:cli/command`, so wasi:* packages
    // must be available first.
    let mut resolve = Resolve::default();
    super::wasi_bundle::push_wasi_packages(&mut resolve)?;

    let wit_source = super::wit::emit_world_wit(world);

    // Parse our generated WIT into the same `Resolve`. `parse` reads
    // from a string — the path argument is for error messages only,
    // no filesystem access.
    let unresolved = UnresolvedPackageGroup::parse("aver-generated.wit", &wit_source)
        .map_err(|e| Wasip2Error::Wrap(format!("WIT parse failed for generated package: {e}")))?;
    let pkg_id = resolve
        .push_group(unresolved)
        .map_err(|e| Wasip2Error::Wrap(format!("push aver:user package into Resolve: {e}")))?;
    let world_id = resolve
        .select_world(&[pkg_id], Some(world.local_name()))
        .map_err(|e| {
            Wasip2Error::Wrap(format!(
                "select world `{}` in generated package: {e}",
                world.local_name()
            ))
        })?;

    // Embed `component-type:<world>` custom section into a copy of the
    // core wasm. ComponentEncoder reads this section to know which
    // WIT world the core's signatures correspond to.
    let mut core = core_wasm.to_vec();
    embed_component_metadata(&mut core, &resolve, world_id, StringEncoding::UTF8)
        .map_err(|e| Wasip2Error::Wrap(format!("embed component-type metadata: {e}")))?;

    // Wrap as a component WITHOUT the preview-1 adapter.
    let component = ComponentEncoder::default()
        .module(&core)
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::module rejected core: {e}")))?
        .validate(true)
        .encode()
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::encode failed: {e}")))?;

    Ok((component, wit_source))
}
