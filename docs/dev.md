# Developer notes

Local-development tips for working on the Aver compiler itself. None of this is
needed to *use* Aver — install `aver-lang` for the compiler and `aver-cert` for
independent certificate verification.

## Faster rebuilds

The full release build (`cargo install --path . --force`) takes ~3 minutes on first compile because `[profile.release]` runs LTO with `codegen-units = 1`. That's the right setting for shipped binaries — don't change it. For the local edit-build-test loop the answer is *not* to ship faster, it's to skip work you don't need:

| What you're doing                          | Fastest command                                       | Typical incremental |
|--------------------------------------------|-------------------------------------------------------|---------------------|
| "Does it still compile?"                   | `cargo check --features wasm`                         | ~3 s                |
| Run the debugger / test `aver shape` etc.  | `cargo build --bin aver --features wasm`              | ~5 s                |
| Reproduce a perf-sensitive bug             | `cargo build --release --bin aver --features wasm`    | ~3 min (full LTO)   |

Debug binary is good enough for testing `aver shape`, `aver verify`, `aver check`, and the rest of the CLI — none are perf-critical.

## Linker

`.cargo/config.toml` in this repo configures `lld` as the linker on macOS hosts. The default Apple linker (`ld` from Xcode) is the slowest step in incremental rebuilds; `lld` cuts the link step roughly in half on this workspace.

Install once:

```bash
brew install llvm     # provides `lld` at /opt/homebrew/bin/lld
```

If `lld` is missing the build fails with a clear pointer; either install `llvm` via Homebrew or remove the `[target.*-apple-darwin]` block in `.cargo/config.toml`.

## Cross-crate cache

For clean builds and branch switching, enable `sccache` per-machine. It caches compiled crates by content hash, so two branches that share most of their dependency tree only pay for the deltas.

```bash
brew install sccache
mkdir -p ~/.cargo
printf '[build]\nrustc-wrapper = "sccache"\n' >> ~/.cargo/config.toml
```

First build after enabling is normal (filling the cache). Subsequent clean builds typically run 40–60% faster because the workspace's dependency crates aren't recompiled. `sccache --show-stats` reports hit / miss rates.

Linux: `apt install sccache` or `cargo install sccache`.

## Test runner

`cargo nextest` parallelizes test execution and gives fast filtering; it does
not cache successful test results. Install it once:

```bash
cargo install cargo-nextest
```

Use `cargo nextest run` for the Rust inner loop, or select the crate/test you are
changing. Certificate integration tests still launch Lean and remain much
slower than ordinary Rust tests, so there is no useful single time estimate for
the whole workspace.

For an emitted certificate, use the two explicit trust levels:

```bash
aver cert check out/app.wasm out/cert    # developer preflight: CHECKED
aver cert verify out/app.wasm out/cert   # release/admission gate: CERTIFIED
```

`check` skips only the final `leanchecker --fresh` replay and trusts the built
or explicitly cached `.olean` closure. It is the inner-loop command, never a
replacement for strict `verify`.

## Releases

Release `aver-cert` through `python3 tools/release.py X.Y.Z`, never with a
separate manual `cargo publish`. The verifier keeps its own `0.1.x` version
line, but the release tool coordinates it with Aver: it publishes the first
`0.1.0`, patch-bumps it only when its source changes, updates `aver-lang`'s
exact producer dependency pin, and publishes `aver-cert` before `aver-lang`.
Users still install the two executables separately.
