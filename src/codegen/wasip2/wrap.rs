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

use super::CapabilityWitPlan;
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

/// Declared component envelope used by the wasip2 certificate work.
///
/// The producer is allowed to discover this split while constructing the
/// component. The trusted verifier path must not rediscover it by walking the
/// component bytes; it will receive the declaration and confirm only the byte
/// equality `component == prefix ++ embedded_core_module ++ suffix` before
/// applying the existing core-module certificate checks to `embedded_core_module`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Wasip2ComponentEnvelope {
    /// Component bytes before the embedded core module section payload.
    pub prefix: Vec<u8>,
    /// Exact core wasm module payload embedded by `wit-component`.
    /// This is the post-encoder module bytes found in the component's core
    /// module section; it is not assumed to be byte-identical to the input
    /// `core_wasm` passed to the wrapper.
    pub embedded_core_module: Vec<u8>,
    /// Component bytes after the embedded core module section payload.
    pub suffix: Vec<u8>,
}

impl Wasip2ComponentEnvelope {
    /// Declare the single embedded core module section of a produced component.
    ///
    /// This walks the component only on the producer side, immediately after
    /// `wit-component` returns bytes. Certificate verification must consume the
    /// resulting declaration and check byte equality instead of repeating this
    /// discovery step on the trusted path.
    fn from_component(component: &[u8]) -> Result<Self, Wasip2Error> {
        use wasmparser::{Parser, Payload};

        let mut ranges = Vec::new();
        for payload in Parser::new(0).parse_all(component) {
            match payload.map_err(|error| {
                Wasip2Error::Envelope(format!("cannot parse produced component: {error}"))
            })? {
                Payload::ModuleSection {
                    unchecked_range, ..
                } => ranges.push(unchecked_range),
                _ => {}
            }
        }
        let range = match ranges.as_slice() {
            [range] => range.clone(),
            [] => {
                return Err(Wasip2Error::Envelope(
                    "component contains no embedded core module section".to_string(),
                ));
            }
            _ => {
                let marked = ranges
                    .iter()
                    .filter_map(|range| {
                        module_range(component, range).ok().and_then(|module| {
                            module_has_aver_user_core_marker(module).then(|| range.clone())
                        })
                    })
                    .collect::<Vec<_>>();
                match marked.as_slice() {
                    [range] => range.clone(),
                    [] => {
                        return Err(Wasip2Error::Envelope(format!(
                            "component contains {} embedded core module sections, but none carries the Aver user-core marker exports",
                            ranges.len()
                        )));
                    }
                    _ => {
                        return Err(Wasip2Error::Envelope(format!(
                            "component contains {} embedded core module sections with Aver user-core marker exports; exactly one user core is supported",
                            marked.len()
                        )));
                    }
                }
            }
        };
        if range.start >= range.end || range.end > component.len() {
            return Err(Wasip2Error::Envelope(format!(
                "component parser returned out-of-bounds core module range {}..{} for {} bytes",
                range.start,
                range.end,
                component.len()
            )));
        }
        Ok(Self {
            prefix: component[..range.start].to_vec(),
            embedded_core_module: component[range.clone()].to_vec(),
            suffix: component[range.end..].to_vec(),
        })
    }

    /// Reconstruct the full component from the declared envelope.
    pub fn component_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(
            self.prefix.len() + self.embedded_core_module.len() + self.suffix.len(),
        );
        bytes.extend_from_slice(&self.prefix);
        bytes.extend_from_slice(&self.embedded_core_module);
        bytes.extend_from_slice(&self.suffix);
        bytes
    }
}

/// Component output plus the explicit envelope declaration needed by the
/// wasip2 certificate path.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Wasip2ComponentArtifact {
    /// Delivered `.component.wasm` bytes.
    pub component_bytes: Vec<u8>,
    /// Sibling `.wit` source emitted for human/tooling inspection.
    pub wit_source: String,
    /// Declared `prefix/core/suffix` split of `component_bytes`.
    pub envelope: Wasip2ComponentEnvelope,
}

/// Wrap a core wasm-gc module as a Component.
///
/// `core_wasm` is the output of the wasm-gc backend re-targeted at
/// canonical-ABI-compatible boundary shapes. Returns the component
/// bytes alongside the WIT source emitted next to the artifact (per
/// the component contract in `docs/wasip2.md` — point 5).
///
/// Two worlds, two entry-point shapes:
/// - `CliCommand` — long-lived process exporting `wasi:cli/run.run`.
///   `wasmtime run` / Spin component-mode / wasmCloud invoke this
///   shape with the same convention as a POSIX binary.
/// - `HttpProxy` — request/response shape exporting
///   `wasi:http/incoming-handler.handle`. `wasmtime serve --http=:N`
///   is the canonical local runner; production hosts (Spin, NGINX
///   Unit, Fastly compute) bind the same export to their listener.
pub fn compile_to_component(
    core_wasm: &[u8],
    world: Wasip2World,
) -> Result<(Vec<u8>, String), Wasip2Error> {
    compile_to_component_with_capabilities(core_wasm, world, &CapabilityWitPlan::default())
}

/// Wrap a core wasm-gc module as a Component and retain the declared component
/// envelope that future wasip2 certificate emission will serialize.
pub fn compile_to_component_artifact(
    core_wasm: &[u8],
    world: Wasip2World,
) -> Result<Wasip2ComponentArtifact, Wasip2Error> {
    compile_to_component_artifact_with_capabilities(core_wasm, world, &CapabilityWitPlan::default())
}

/// Wrap a core module whose custom canonical imports were generated
/// from `capabilities`. The same plan emits the sibling WIT and the
/// embedded component metadata; neither surface is reconstructed by
/// scanning the other.
pub fn compile_to_component_with_capabilities(
    core_wasm: &[u8],
    world: Wasip2World,
    capabilities: &CapabilityWitPlan,
) -> Result<(Vec<u8>, String), Wasip2Error> {
    compile_to_component_bytes_with_capabilities(core_wasm, world, capabilities)
}

/// Wrap a core module and return the component plus a producer-declared
/// `prefix/core/suffix` envelope for the embedded core payload.
pub fn compile_to_component_artifact_with_capabilities(
    core_wasm: &[u8],
    world: Wasip2World,
    capabilities: &CapabilityWitPlan,
) -> Result<Wasip2ComponentArtifact, Wasip2Error> {
    let (component_bytes, wit_source) =
        compile_to_component_bytes_with_capabilities(core_wasm, world, capabilities)?;
    let envelope = Wasip2ComponentEnvelope::from_component(&component_bytes)?;

    Ok(Wasip2ComponentArtifact {
        component_bytes,
        wit_source,
        envelope,
    })
}

fn compile_to_component_bytes_with_capabilities(
    core_wasm: &[u8],
    world: Wasip2World,
    capabilities: &CapabilityWitPlan,
) -> Result<(Vec<u8>, String), Wasip2Error> {
    // Build a Resolve seeded with the bundled WASI 0.2.4 WIT
    // package set, then push the user package on top. Order matters:
    // the user world `include`s `wasi:cli/command` or
    // `wasi:http/proxy`, so wasi:* packages must be available first.
    let mut resolve = Resolve::default();
    super::wasi_bundle::push_wasi_packages(&mut resolve)?;

    // Inspect the core module's imports to decide whether the
    // `wasi:cli/command` world needs an extra
    // `import wasi:http/outgoing-handler@0.2.4`. We scan because the
    // codegen path that registers wasi:http slots (`module.rs`
    // `EffectName::HttpGet` arm) and the wrap path live in different
    // modules — having `compile_to_component` derive the bit from the
    // only ground truth (the actual emitted imports) keeps the world
    // exactly tracking the core's surface and avoids a second source
    // of truth that could drift. The `HttpProxy` world already
    // includes `wasi:http/types` + `outgoing-handler` transitively via
    // `include wasi:http/proxy@0.2.4`, so the bit only matters for
    // the CLI command path.
    let needs_http = core_imports_use_wasi_http(core_wasm);

    let wit_source = super::wit::emit_world_wit_with_capabilities(world, needs_http, capabilities);

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
    let component_bytes = ComponentEncoder::default()
        .module(&core)
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::module rejected core: {e}")))?
        .validate(true)
        .encode()
        .map_err(|e| Wasip2Error::Wrap(format!("ComponentEncoder::encode failed: {e}")))?;
    Ok((component_bytes, wit_source))
}

fn module_range<'a>(
    component: &'a [u8],
    range: &std::ops::Range<usize>,
) -> Result<&'a [u8], Wasip2Error> {
    if range.start >= range.end || range.end > component.len() {
        return Err(Wasip2Error::Envelope(format!(
            "component parser returned out-of-bounds core module range {}..{} for {} bytes",
            range.start,
            range.end,
            component.len()
        )));
    }
    Ok(&component[range.clone()])
}

fn module_has_aver_user_core_marker(module: &[u8]) -> bool {
    use wasmparser::{ExternalKind, Parser, Payload};

    let mut caller_fn_count = false;
    let mut caller_fn_name = false;
    for payload in Parser::new(0).parse_all(module) {
        let Ok(Payload::ExportSection(reader)) = payload else {
            continue;
        };
        for export in reader.into_iter().flatten() {
            if export.kind != ExternalKind::Func {
                continue;
            }
            match export.name {
                "__caller_fn_count" => caller_fn_count = true,
                "__caller_fn_name" => caller_fn_name = true,
                _ => {}
            }
        }
    }
    caller_fn_count && caller_fn_name
}

/// Scan a core wasm module's imports for any module name starting
/// with `"wasi:http/"`. Used by `compile_to_component` to decide
/// whether the generated WIT world should additionally
/// `import wasi:http/outgoing-handler@0.2.4`.
///
/// Implemented via `wasmparser` to walk only the import section —
/// avoids re-decoding the full module the way `Module::new` would,
/// and treats malformed bytes as "no http imports" (the encoder
/// later rejects malformed input with a clearer message).
fn core_imports_use_wasi_http(core_wasm: &[u8]) -> bool {
    use wasmparser::{Parser, Payload};
    for payload in Parser::new(0).parse_all(core_wasm).flatten() {
        if let Payload::ImportSection(reader) = payload {
            // `into_imports()` flattens the grouped `Imports` enum
            // (Single / Compact1 / Compact2) into a per-item iterator
            // of `Import` values, each with its own `module` field.
            for import in reader.into_imports().flatten() {
                if import.module.starts_with("wasi:http/") {
                    return true;
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn component_envelope_rejects_bytes_with_no_embedded_core_payload() {
        let err = Wasip2ComponentEnvelope::from_component(b"\0asm\x01\0\0\0")
            .unwrap_err()
            .to_string();
        assert!(err.contains("no embedded core module section"), "{err}");
    }
}
