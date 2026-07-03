# Quickstart

From a clean checkout with Docker available, run:

```bash
docker build -t aver-one-command . && docker run --rm aver-one-command
```

That one command builds a local image and then runs the image's default smoke test. The Dockerfile also runs the same smoke test while building the image, so the build fails before producing a usable image if either step regresses.

The image pins:

- Rust `1.95.0`
- Lean toolchain `leanprover/lean4:v4.31.0`
- Dafny `4.11.0`

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
```

The demo path on that branch is:

```text
projects/durable_promise/main.av
```

Add one `verify ... law ...` block to the demo, then ask the proof checker to show the missing proof obligation:

```bash
aver proof projects/durable_promise/main.av --backend lean --check --explain -o /tmp/aver-durable-proof
```

If the law does not close universally, `--explain` records the residual open goal in the generated proof manifest.

## CI

No Docker CI job is wired for this quickstart. The image downloads and materializes three toolchains, including Lean and Dafny, and is kept as a manual verification path unless a later CI environment can show it adds less than 10 minutes with no flake risk.
