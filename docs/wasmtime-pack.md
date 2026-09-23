# Self-contained Wasmtime packs

`--pack wasmtime` puts a wasm-gc program, together with everything its host
side needs, into a directory you can copy to another machine:

```bash
aver compile app.av --target wasm-gc --pack wasmtime -o out/
./out/aver-wasmtime-host arg-one arg-two

# Explicit diagnostic execution; `--` separates host and program arguments.
./out/aver-wasmtime-host --artifact canonical -- arg-one arg-two
./out/aver-wasmtime-host --artifact optimized -- arg-one arg-two
```

The destination does not need Aver, Cargo, a separately installed Wasmtime,
the Aver sources, the provider sources, or `aver.toml`. It needs only the
bundle directory, on the same OS and architecture the native host was built
for.

## Bundle layout

```text
out/
  aver-wasmtime-host   native launcher, Wasmtime, and linked providers
  app.wasm             canonical wasm-gc artifact; certificate subject
  app.optimized.wasm   optional Binaryen result; deployment Wasm
  app.cwasm            AOT image of the deployment Wasm
  manifest.json        artifacts, ABI, capability, provider, and policy facts
```

Arguments given after `aver-wasmtime-host` become the program's `Args` values.
When a host option is present, `--` separates it from the program arguments.
So `aver-wasmtime-host -- --artifact canonical` passes both words to the
program and does not select an artifact. The host always calls the exported
`main` function.

By default the host loads `app.cwasm` directly, so Cranelift does not run on
the destination at first start. Without `--optimize`, the AOT image is built
straight from `app.wasm` and the middle file is absent.

With `--certify --optimize`, all three stages stay visible on purpose:

```text
app.wasm --Binaryen (unproved)--> app.optimized.wasm --Cranelift (unproved)--> app.cwasm
    |
    +-- cert/ proves this exact artifact
```

The certificate makes no claim about either transformation. The proof boundary
stays where it really is, and the manifest records the exact hashes and the
selected optimization mode of the deployment chain.

## Selecting an artifact

The native host also has two explicit diagnostic paths:

```bash
aver-wasmtime-host --artifact aot          # default; deserialize app.cwasm
aver-wasmtime-host --artifact canonical    # JIT the certificate-subject app.wasm
aver-wasmtime-host --artifact optimized    # JIT app.optimized.wasm
```

`canonical` and `optimized` use the same linked providers, runtime policy and
entry path as AOT, so a difference between stages can be pinned on one stage.
If only canonical succeeds, Binaryen is at fault. If optimized succeeds and AOT
fails, the problem is in producing or loading the native image. A pack built
without `--optimize` rejects the `optimized` selection instead of quietly
running the canonical file.

There is no automatic fallback, by design. Production keeps the zero-JIT `aot`
default and fails closed if that chain is invalid. Each diagnostic mode checks
only the artifact it was asked to run, so it still works when a later
derivative is the broken stage.

## Providers and the build cache

The pack uses the same `[providers]` composition as `aver run --wasm-gc`.
Each configured Rust `ProviderBinding` is statically linked into the native
host. The build machine needs Cargo when it sees a new provider composition;
the destination machine never does.

The release host is content-addressed by the Aver version, Rust toolchain,
platform, provider packages and factories, and provider source state. Aver
reuses that host when only the `.av` program changes. A change to provider
source or composition builds a separate host. Different compositions also get
different internal filenames, so concurrent cache entries cannot overwrite
each other.

## Checks before execution

The host treats the manifest as data to check, and does not trust it as an
instruction. On the default AOT path, before Wasmtime instantiates the module,
the host:

1. hashes the canonical `.wasm`, optional `.optimized.wasm`, and `.cwasm`
   bytes and compares every present stage with the manifest;
2. requires a Wasmtime precompiled-module envelope and the exact engine
   compatibility fingerprint recorded by the host that built the pack;
3. deserializes the checked image, then compares every import module, name,
   parameter, and result type with the manifest;
4. rebuilds the bundled custom capability contracts and recomputes their
   contract and replay-model hashes;
5. compares the required operations and the identity/fingerprint of every
   provider with the bindings compiled into the executable; and
6. parses and enforces the runtime effect policy carried from `aver.toml`.

Any mismatch stops before instantiation with a `wasmtime-bundle-*` diagnostic.
The canonical and optimized diagnostic paths check the selected file's hash
and its own recorded import surface, then apply the same contract, provider,
entry and policy checks. They do not require later derivatives to be intact.

`--optimize` writes a sibling file and leaves the canonical artifact in place.
When the optimized sibling exists, the AOT image is built from it. `--certify`
certifies only the canonical `.wasm` and puts its `cert/` directory beside the
bundle artifacts. Certificate verification is still the separate
`aver cert check` / `aver-cert check` operation; the host's deployment checks
do not stand in for it. The certificate binds the Wasm and says nothing about
Cranelift's native output. Wasmtime stays in the trusted execution path, as it
is when it compiles the module at startup.

The `.cwasm` file contains native executable code, and Wasmtime deliberately
deserializes that format with fewer checks than portable Wasm. The host gets to
that step only after the digest, envelope and engine fingerprint checks. Those
checks catch partial or accidental replacement. They are not a bundle
signature. Deployment integrity or code signing must cover the host, manifest,
canonical/runtime Wasm files and `.cwasm` as one trust unit. Replacing the
whole unit is the same as replacing any other native application.

## Current boundary

The first pack surface is kept narrow on purpose:

- target: `wasm-gc` on Wasmtime GC;
- entry: `main` (not an incoming HTTP `--handler`);
- execution mode: live effects; a toolchain-free record/replay control surface
  is not part of schema 3;
- platform: the build machine's OS and architecture;
- standard capabilities: compiler-shipped wasm-gc adapters; replacing a
  standard adapter with custom Rust is rejected explicitly.

The `wasip2` target stays a host-neutral Component Model artifact and will not
get an Aver-owned host pack. Downloading standard hosts for other platforms and
cross-compiling arbitrary custom providers are separate deployment concerns.
The supported path is to build the pack on its destination platform or in a
matching CI runner.

## Work jobs

A pack includes the program's job contracts and keeps `[work] max-jobs`. Its
host runs pure jobs on threads with separate Stores and a shared precompiled
Module. Workers need no toolchain or JIT on the destination.
See [Parallel Work on wasm-gc](wasm-work.md) for the ABI and JavaScript adapter.
