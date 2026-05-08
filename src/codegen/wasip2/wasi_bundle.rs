//! WASI 0.2.4 WIT bundle — vendored under `wit/wasi-0.2.4/` in the
//! repo and embedded into the binary via `include_str!`.
//!
//! The wasm-gc backend lowers Aver effects to canonical-ABI-
//! compatible imports of these interfaces (e.g., `wasi:io/streams.
//! [method]write`, `wasi:cli/stdout.get-stdout`), and the
//! component-type metadata we embed references them. Without the
//! bundle in the `Resolve`, `wit-component` cannot type-check the
//! generated world.
//!
//! Pinned to WASI 0.2.4 — the version `wasip2-1.0.1+wasi-0.2.4`
//! ships and that wasmtime 44.0.x speaks. Newer 0.2.x point
//! releases (`0.2.6`, `0.2.11` per upstream tracker as of
//! 2026-05-08) bring no additions Aver consumes today; if a host
//! we want to support requires a newer point release, the bundle
//! gets rev'd in one drop-in re-vendor.

use wit_parser::{Resolve, SourceMap};

use super::error::Wasip2Error;

/// Push the bundled WASI WIT package set into `resolve`. Each
/// vendored sub-directory under `wit/wasi-0.2.4/` is one WIT
/// package; `SourceMap::parse` is per-package, so files are
/// grouped by their leading path component and parsed separately.
///
/// Order matters: `Resolve::push_group` requires every referenced
/// package to be present beforehand. The fixed sequence below is
/// the topological order of the WASI 0.2.4 dependency graph —
/// `io` has no deps, `clocks`/`filesystem`/`random`/`sockets`
/// depend only on `io`, `cli` includes interfaces from
/// `io` + `clocks` + `filesystem` + `sockets`, and `http` is on
/// top of everything else.
pub fn push_wasi_packages(resolve: &mut Resolve) -> Result<(), Wasip2Error> {
    const PACKAGE_ORDER: &[&str] = &[
        "io", "clocks", "random", "filesystem", "sockets", "cli", "http",
    ];
    for pkg_name in PACKAGE_ORDER {
        let mut map = SourceMap::default();
        let mut found_any = false;
        for (path, contents) in WASI_WIT_FILES {
            if package_dir(path) == Some(pkg_name) {
                map.push_str(path, *contents);
                found_any = true;
            }
        }
        if !found_any {
            return Err(Wasip2Error::Wrap(format!(
                "vendored WASI bundle is missing files for package `{pkg_name}`"
            )));
        }
        let group = map.parse().map_err(|(_, e)| {
            Wasip2Error::Wrap(format!(
                "parse vendored WASI WIT package `{pkg_name}`: {e}"
            ))
        })?;
        resolve.push_group(group).map_err(|e| {
            Wasip2Error::Wrap(format!(
                "push vendored WASI WIT package `{pkg_name}`: {e}"
            ))
        })?;
    }
    Ok(())
}

/// Extract the package sub-directory from a vendored bundle path,
/// e.g., `wit/wasi-0.2.4/cli/command.wit` → `cli`.
fn package_dir(path: &str) -> Option<&str> {
    path.strip_prefix("wit/wasi-0.2.4/")
        .and_then(|rest| rest.split('/').next())
}

/// Vendored set of WIT files under `wit/wasi-0.2.4/`. Path strings
/// are display-only (used in WIT parse error messages); contents
/// are embedded at compile time so the binary is self-contained.
const WASI_WIT_FILES: &[(&str, &str)] = &[
    (
        "wit/wasi-0.2.4/cli/command.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/command.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/environment.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/environment.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/exit.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/exit.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/imports.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/imports.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/run.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/run.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/stdio.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/stdio.wit"),
    ),
    (
        "wit/wasi-0.2.4/cli/terminal.wit",
        include_str!("../../../wit/wasi-0.2.4/cli/terminal.wit"),
    ),
    (
        "wit/wasi-0.2.4/clocks/monotonic-clock.wit",
        include_str!("../../../wit/wasi-0.2.4/clocks/monotonic-clock.wit"),
    ),
    (
        "wit/wasi-0.2.4/clocks/timezone.wit",
        include_str!("../../../wit/wasi-0.2.4/clocks/timezone.wit"),
    ),
    (
        "wit/wasi-0.2.4/clocks/wall-clock.wit",
        include_str!("../../../wit/wasi-0.2.4/clocks/wall-clock.wit"),
    ),
    (
        "wit/wasi-0.2.4/clocks/world.wit",
        include_str!("../../../wit/wasi-0.2.4/clocks/world.wit"),
    ),
    (
        "wit/wasi-0.2.4/filesystem/preopens.wit",
        include_str!("../../../wit/wasi-0.2.4/filesystem/preopens.wit"),
    ),
    (
        "wit/wasi-0.2.4/filesystem/types.wit",
        include_str!("../../../wit/wasi-0.2.4/filesystem/types.wit"),
    ),
    (
        "wit/wasi-0.2.4/filesystem/world.wit",
        include_str!("../../../wit/wasi-0.2.4/filesystem/world.wit"),
    ),
    (
        "wit/wasi-0.2.4/http/handler.wit",
        include_str!("../../../wit/wasi-0.2.4/http/handler.wit"),
    ),
    (
        "wit/wasi-0.2.4/http/proxy.wit",
        include_str!("../../../wit/wasi-0.2.4/http/proxy.wit"),
    ),
    (
        "wit/wasi-0.2.4/http/types.wit",
        include_str!("../../../wit/wasi-0.2.4/http/types.wit"),
    ),
    (
        "wit/wasi-0.2.4/io/error.wit",
        include_str!("../../../wit/wasi-0.2.4/io/error.wit"),
    ),
    (
        "wit/wasi-0.2.4/io/poll.wit",
        include_str!("../../../wit/wasi-0.2.4/io/poll.wit"),
    ),
    (
        "wit/wasi-0.2.4/io/streams.wit",
        include_str!("../../../wit/wasi-0.2.4/io/streams.wit"),
    ),
    (
        "wit/wasi-0.2.4/io/world.wit",
        include_str!("../../../wit/wasi-0.2.4/io/world.wit"),
    ),
    (
        "wit/wasi-0.2.4/random/insecure-seed.wit",
        include_str!("../../../wit/wasi-0.2.4/random/insecure-seed.wit"),
    ),
    (
        "wit/wasi-0.2.4/random/insecure.wit",
        include_str!("../../../wit/wasi-0.2.4/random/insecure.wit"),
    ),
    (
        "wit/wasi-0.2.4/random/random.wit",
        include_str!("../../../wit/wasi-0.2.4/random/random.wit"),
    ),
    (
        "wit/wasi-0.2.4/random/world.wit",
        include_str!("../../../wit/wasi-0.2.4/random/world.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/instance-network.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/instance-network.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/ip-name-lookup.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/ip-name-lookup.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/network.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/network.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/tcp-create-socket.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/tcp-create-socket.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/tcp.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/tcp.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/udp-create-socket.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/udp-create-socket.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/udp.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/udp.wit"),
    ),
    (
        "wit/wasi-0.2.4/sockets/world.wit",
        include_str!("../../../wit/wasi-0.2.4/sockets/world.wit"),
    ),
];
