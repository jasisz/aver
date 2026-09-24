# Quickstart

From a clean checkout with Docker available, run:

```bash
docker build -t aver-one-command . && docker run --rm aver-one-command
```

It builds a local image and then runs the image's default smoke test. The Dockerfile also runs that smoke test during the build, so if either step regresses the build fails before it produces a usable image.

The first build downloads roughly 1-2 GB of Docker layers, Rust crates and Lean, and can take tens of minutes. Later builds download much less once the Docker and Cargo caches are warm.

Apple Silicon warning: the image is currently `linux/amd64` only. Docker Desktop runs the image under qemu, so the first build and the smoke test are both much slower than on native `linux/amd64`.

The image pins:

- Rust `1.95.0`
- Lean toolchain `leanprover/lean4:v4.34.0`

The Rust build is a debug build, which keeps the local quickstart's time in check. Release LTO is deliberately left out of this Docker path.

## What It Runs

The smoke test runs:

```bash
aver run examples/core/hello.av
aver proof examples/formal/validated_wrapper_law.av --backend lean --check -o /tmp/aver-proof-smoke-run
aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-smoke-run
aver-cert check /tmp/aver-cert-smoke-run/add_one.wasm /tmp/aver-cert-smoke-run/cert
```

The first command runs the hello example on the Aver VM.

The second command exports `examples/formal/validated_wrapper_law.av` to Lean and has `lake build` re-check the generated theorem on the Lean kernel. The law being checked is `checkedDiv.returnsCore`: when the divisor is nonzero, the error-checking wrapper returns `Result.Ok(coreDiv(a, b))`. The check is strict. The default budgets allow no Lean build errors and no residual `sorry`.

The last two commands compile a tiny wasm-gc function with an Artifact Behavioral Certificate and run the faster developer preflight. On success it prints `CHECKED`. It does not print `CERTIFIED`, because the Docker smoke test is not a release gate.

## Full Certificate Follow-Up

Run the strict whole-closure replay in a one-off container:

```bash
docker run --rm aver-one-command sh -c '
  rm -rf /tmp/aver-cert-verify &&
  aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-verify &&
  aver-cert verify /tmp/aver-cert-verify/add_one.wasm /tmp/aver-cert-verify/cert
'
```

Only this command may print `CERTIFIED`. It is slower on purpose, because it adds `leanchecker --fresh` over the complete imported closure. See the [certificate guide](certification.md) for the exact guarantee and trust boundary.

## CI

No Docker CI job runs this quickstart. The image downloads and installs the Rust and Lean toolchains. It stays a manual verification path until a CI environment can show that it adds less than 10 minutes and carries no flake risk.
