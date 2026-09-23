# Developer notes

Tips for local development on the Aver compiler itself. You do not need any of this to *use* Aver: install `aver-lang` for the compiler and `aver-cert` for independent certificate verification.

## Faster rebuilds

The full release build (`cargo install --path . --force`) takes about 3 minutes on a first compile, because `[profile.release]` runs LTO with `codegen-units = 1`. Keep that setting; it is correct for shipped binaries. For the local edit-build-test loop, skip the work you do not need instead:

| What you're doing                          | Fastest command                                       | Typical incremental |
|--------------------------------------------|-------------------------------------------------------|---------------------|
| "Does it still compile?"                   | `cargo check --features wasm`                         | ~3 s                |
| Run the debugger / test `aver shape` etc.  | `cargo build --bin aver --features wasm`              | ~5 s                |
| Reproduce a perf-sensitive bug             | `cargo build --release --bin aver --features wasm`    | ~3 min (full LTO)   |

A debug binary is fine for testing `aver shape`, `aver verify`, `aver check` and the rest of the CLI. None of them is performance-critical.

## Linker

`.cargo/config.toml` in this repo sets `lld` as the linker on macOS hosts. The default Apple linker (`ld` from Xcode) is the slowest step of an incremental rebuild, and `lld` roughly halves the link step on this workspace.

Install it once:

```bash
brew install llvm     # provides `lld` at /opt/homebrew/bin/lld
```

If `lld` is missing, the build fails with a message saying so. Either install `llvm` with Homebrew or remove the `[target.*-apple-darwin]` block from `.cargo/config.toml`.

## Cross-crate cache

For clean builds and branch switching, enable `sccache` on your machine. It caches compiled crates by content hash, so two branches that share most of their dependency tree only pay for the differences.

```bash
brew install sccache
mkdir -p ~/.cargo
printf '[build]\nrustc-wrapper = "sccache"\n' >> ~/.cargo/config.toml
```

The first build after enabling it runs at normal speed while it fills the cache. Later clean builds are usually 40–60% faster, because the workspace's dependency crates are not recompiled. `sccache --show-stats` shows hit and miss rates.

Linux: `apt install sccache` or `cargo install sccache`.

## Test runner

`cargo nextest` runs tests in parallel and filters quickly. It does not cache successful test results. Install it once:

```bash
cargo install cargo-nextest
```

Use `cargo nextest run` for the Rust inner loop, or select the crate or test you are changing. Certificate integration tests still launch Lean and are much slower than ordinary Rust tests, so no single time estimate for the whole workspace is useful.

For an emitted certificate there are two explicit trust levels:

```bash
aver cert check out/app.wasm out/cert    # developer preflight: CHECKED
aver cert verify out/app.wasm out/cert   # release/admission gate: CERTIFIED
```

`check` skips only the final `leanchecker --fresh` replay and trusts the `.olean` closure that was built or explicitly cached. Use it in the inner loop. It never replaces strict `verify`.

## Releases

Release `aver-cert` with `python3 tools/release.py X.Y.Z --prepare`, wait for the CI/Proof/Certification runs on that exact candidate, then continue with `python3 tools/release.py X.Y.Z`. Never run a separate manual `cargo publish`. The verifier has its own `0.1.x` version line, and the release tool coordinates it with Aver. It publishes the first `0.1.0`, bumps the patch version only when the verifier's source changes, updates the exact producer dependency pin in `aver-lang`, and publishes `aver-cert` before `aver-lang`. Users still install the two executables separately.
