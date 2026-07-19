# aver-cert

`aver-cert` is the independent producer engine and verifier for Aver
WebAssembly artifact certificates. Its verifier checks an artifact-specific
Lean proof against the exact `.wasm` bytes without trusting `aver-lang` or the
compiler process that emitted the package.

The crate starts its own public version line at `0.1.0`; its version is not tied
to the `aver-lang` release number.

## Install

```bash
cargo install aver-lang --features wasm
cargo install aver-cert
```

The first command installs the certificate producer; omit it if you only need
to verify an existing package.

Verification uses the checker-pinned `leanprover/lean4:v4.32.0` toolchain and
requires a standard Elan installation. The verifier resolves
`$ELAN_HOME/bin/elan`, or the platform's default Elan home when `ELAN_HOME` is
unset (`$HOME/.elan` on Unix, `%USERPROFILE%\.elan` on Windows). It invokes that
canonical executable directly and does not resolve its trusted `lake`, `lean`,
or `leanchecker` subprocesses through `PATH`.

## Commands

Given a module produced with
`aver compile app.av --target wasm-gc --certify -o out/`:

```bash
aver-cert check out/app.wasm out/cert
aver-cert verify out/app.wasm out/cert
aver-cert explain out/app.wasm out/cert
aver-cert inspect out/app.wasm out/cert
```

`verify` exits zero only when at least one export is certified and the full
Lean check succeeds. `check` is a faster developer/CI preflight: it performs
the same Rust gates, `lake build`, and fresh checker-witness elaboration, but
trusts the freshly built or explicitly cached `.olean` closure and skips
`leanchecker --fresh`. Its green output is `CHECKED`, never `CERTIFIED`, and it
must not gate a release or admission.

`explain` performs the same trusted check as `verify` before printing the
accepted semantic faces, policies, contracts, and declined exports. `inspect`
is an alias of `explain`.

Installing `aver-lang` adds the process-level shortcut:

```bash
aver cert verify out/app.wasm out/cert
```

The shortcut forwards raw arguments, standard streams, and the child exit
status to a sibling `aver-cert` executable or one found on `PATH`. There is no
linked verifier inside `aver`.

## Package contract

The first public certificate schema and package format are both version `1`.
`cert-manifest.json` is a transport/report envelope. `Plans.lean` is the sole
authoritative plan data; public certificate packages contain neither
`fragments/*.plan` files nor `ArtifactBytes.lean`. The verifier generates
`ArtifactBytes.lean` from the module it actually reads.

The checker owns the hash-selected Lean soundness wall, toolchain, build files,
and witness. Lean derives the standard semantic face with `StandardFace` and
the policy, termination, totality, and contract axes with `ClaimAxes`. The
named root is replayed with an exact axiom whitelist and
`leanchecker --fresh`.

The verifier deliberately retains one `wasmparser::Validator` gate because the
Lean relevant-subset decoder is not a complete WebAssembly stack/control type
validator. It does not run the producer classifier or Rust obligation
rederivation on the positive path.

Verification always runs the mandatory `lake build`, checker-witness
elaboration, and `leanchecker --fresh` replay. The strict default uses no build
cache. `AVER_CERT_DATA_CACHE=/trusted/path` opts into artifact-specific Lake
output, and `AVER_CERT_PRELUDE_CACHE=/trusted/path` additionally reuses
artifact-independent wall output. These directories become trusted local state:
their manifests detect accidental corruption, not an active writer able to
replace both `.olean` files and Lake traces. Package-supplied caches are always
ignored, and neither cache skips the mandatory trusted checks.

Developer preflight with `check` deliberately has a weaker trust boundary: it
omits only the final fresh replay and therefore trusts the locally built or
explicitly cached `.olean` graph. It still writes and elaborates a fresh
checker-owned witness on every run, including the report pins and axiom guard.

Every Lean toolchain step runs under a wall-clock limit (15 minutes per step
by default), so no certificate can hang the verifier forever. On expiry the
step's whole process tree is stopped and the certificate is not accepted.
`AVER_CERT_PHASE_TIMEOUT_SECS=N` replaces the per-step limit.

See the [certificate guide](../docs/certification.md) and
[architecture](../docs/certification-architecture.md) for the guarantee and
trust boundary, and the [format specification](../docs/certificate-format.md)
for the normative reference, trust inventory, and versioning policy.

## Library and features

The default `verify` feature exposes the fail-closed `aver_cert::verify` and
`aver_cert::explain` library APIs and builds the `aver-cert` executable. The
optional `producer` feature contains the emission engine used by `aver-lang`;
it is intentionally absent from the verifier path.
