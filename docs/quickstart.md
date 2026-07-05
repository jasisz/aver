# Quickstart

From a clean checkout with Docker available, run:

```bash
docker build -t aver-one-command . && docker run --rm aver-one-command
```

That one command builds a local image and then runs the image's default smoke test. The Dockerfile also runs the same smoke test while building the image, so the build fails before producing a usable image if either step regresses.

Expected first-build cost on a native `linux/amd64` host is roughly 10-20 minutes and 1-2 GB of downloads for Docker base layers, Rust crates, Lean, and Dafny. Later builds are much smaller when Docker and Cargo caches are warm.

Apple Silicon warning: this image is currently `linux/amd64` only because Dafny `4.11.0` publishes the Ubuntu x64 asset used here, but not a Linux ARM64 asset. Docker Desktop runs the image under qemu on Apple Silicon; expect the first build to take roughly 30-60 minutes and the default smoke-test run to take a few minutes under full emulation.

The image pins:

- Rust `1.95.0`
- Lean toolchain `leanprover/lean4:v4.31.0`
- Dafny `4.11.0` (`dafny-4.11.0-x64-ubuntu-22.04.zip`)

The Rust build is a debug build. That keeps the local quickstart bounded; release LTO is intentionally left out of this Docker path.

## What It Runs

The smoke test runs:

```bash
aver run examples/core/hello.av
aver proof examples/formal/validated_wrapper_law.av --backend lean --check -o /tmp/aver-proof-smoke-run
```

The first command executes the hello example on the Aver VM.

The second command exports `examples/formal/validated_wrapper_law.av` to Lean and asks `lake build` to re-check the generated theorem on the Lean kernel. The checked law is `checkedDiv.returnsCore`: when the divisor is nonzero, the error-checking wrapper returns `Result.Ok(coreDiv(a, b))`. The check is strict: the default budgets allow no Lean build errors and no residual `sorry`.

## Five-Minute Follow-Up

The durable-promise demo is not on `main` yet. If it has not been merged, inspect it from the `demo-durable-promise` branch:

```bash
git fetch origin demo-durable-promise
git switch demo-durable-promise
cd projects/durable_promise
```

The demo path on that branch is:

```text
projects/durable_promise/main.av
```

Add one `verify ... law ...` block to the demo, then ask the proof checker to show the missing proof obligation:

```bash
aver proof main.av --backend lean --check-json --explain -o /tmp/aver-durable-proof
```

If the law does not close universally, the check-json summary names it: `sorry_laws` lists the `fn.law` identities whose theorem carries the residual `sorry`, and `--explain` adds an `open_goals` object with each failing law's residual open goal (keyed by the same identity). The per-law `proof_manifest.json` also carries the residual on a law's `open_goal` field — but only for a law that earns a manifest entry; a law that fails outright (its theorem is just a `sorry`) is named in `sorry_laws`/`open_goals` instead, so read those for the "which law failed" answer.

## CI

No Docker CI job is wired for this quickstart. The image downloads and materializes three toolchains, including Lean and Dafny, and is kept as a manual verification path unless a later CI environment can show it adds less than 10 minutes with no flake risk.
