# Quickstart

From a clean checkout with Docker available, run:

```bash
docker build -t aver-one-command . && docker run --rm aver-one-command
```

That one command builds a local image and then runs the image's default smoke test. The Dockerfile also runs the same smoke test while building the image, so the build fails before producing a usable image if either step regresses.

The first build downloads roughly 1-2 GB of Docker layers, Rust crates, Lean,
and Dafny, and can take tens of minutes. Later builds are much smaller when
Docker and Cargo caches are warm.

Apple Silicon warning: this image is currently `linux/amd64` only because Dafny
`4.11.0` publishes the Ubuntu x64 asset used here, but not a Linux ARM64 asset.
Docker Desktop runs the image under qemu, so both the first build and the smoke
test are substantially slower than on native `linux/amd64`.

The image pins:

- Rust `1.95.0`
- Lean toolchain `leanprover/lean4:v4.32.0`
- Dafny `4.11.0` (`dafny-4.11.0-x64-ubuntu-22.04.zip`)

The Rust build is a debug build. That keeps the local quickstart bounded; release LTO is intentionally left out of this Docker path.

## What It Runs

The smoke test runs:

```bash
aver run examples/core/hello.av
aver proof examples/formal/validated_wrapper_law.av --backend lean --check -o /tmp/aver-proof-smoke-run
aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-smoke-run
aver-cert check /tmp/aver-cert-smoke-run/add_one.wasm /tmp/aver-cert-smoke-run/cert
```

The first command executes the hello example on the Aver VM.

The second command exports `examples/formal/validated_wrapper_law.av` to Lean
and asks `lake build` to re-check the generated theorem on the Lean kernel. The
checked law is `checkedDiv.returnsCore`: when the divisor is nonzero, the
error-checking wrapper returns `Result.Ok(coreDiv(a, b))`. The check is strict:
the default budgets allow no Lean build errors and no residual `sorry`.

The last two commands compile a tiny wasm-gc function with an Artifact
Behavioral Certificate and run the faster developer preflight. Its success
word is `CHECKED`, not `CERTIFIED`; the Docker smoke deliberately does not
pretend to be a release gate.

## Full Certificate Follow-Up

Run the strict whole-closure replay in a one-off container:

```bash
docker run --rm aver-one-command sh -c '
  rm -rf /tmp/aver-cert-verify &&
  aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-verify &&
  aver-cert verify /tmp/aver-cert-verify/add_one.wasm /tmp/aver-cert-verify/cert
'
```

Only this command may print `CERTIFIED`. It is intentionally slower because it
adds `leanchecker --fresh` over the complete imported closure. See the
[certificate guide](certification.md) for the exact guarantee and trust
boundary.

## CI

No Docker CI job is wired for this quickstart. The image downloads and materializes three toolchains, including Lean and Dafny, and is kept as a manual verification path unless a later CI environment can show it adds less than 10 minutes with no flake risk.
