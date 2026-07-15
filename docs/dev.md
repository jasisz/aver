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

`cargo nextest` parallelizes test execution and avoids re-running unchanged tests; install once:

```bash
cargo install cargo-nextest
```

Then use `cargo nextest run` instead of `cargo test`. The full suite drops from ~40 s to ~15 s on this machine.
