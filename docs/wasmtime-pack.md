# Self-contained Wasmtime packs

`--pack wasmtime` turns a wasm-gc program and its host-side meaning into a
directory that can be copied to another machine:

```bash
aver compile app.av --target wasm-gc --pack wasmtime -o out/
./out/aver-wasmtime-host arg-one arg-two

# Explicit diagnostic execution; `--` separates host and program arguments.
./out/aver-wasmtime-host --artifact canonical -- arg-one arg-two
./out/aver-wasmtime-host --artifact optimized -- arg-one arg-two
```

The destination does not need Aver, Cargo, a separately installed Wasmtime,
the Aver sources, the provider sources, or `aver.toml`. It needs only the
bundle directory and the same OS/architecture for which its native host was
built.

## Bundle layout

```text
out/
  aver-wasmtime-host   native launcher, Wasmtime, and linked providers
  app.wasm             canonical wasm-gc artifact; certificate subject
  app.optimized.wasm   optional Binaryen result; deployment Wasm
  app.cwasm            AOT image of the deployment Wasm
  manifest.json        artifacts, ABI, capability, provider, and policy facts
```

Program arguments passed after `aver-wasmtime-host` become the program's
`Args` values. When a host option is present, `--` separates it from those
program arguments; `aver-wasmtime-host -- --artifact canonical` therefore
passes both words to the program instead of selecting an artifact. The host
always invokes the exported `main` function.

By default the host loads `app.cwasm` directly, so the destination does not run
Cranelift on first start. Without `--optimize`, the AOT image is derived
directly from `app.wasm` and the middle file is absent.

With `--certify --optimize`, all three stages remain visible on purpose:

```text
app.wasm --Binaryen (unproved)--> app.optimized.wasm --Cranelift (unproved)--> app.cwasm
    |
    +-- cert/ proves this exact artifact
```

The certificate makes no claim about either transformation. This keeps the
proof boundary honest while the manifest records the exact hashes and selected
optimization mode of the deployment chain.

## Selecting an artifact

The native host also exposes two explicit diagnostic paths:

```bash
aver-wasmtime-host --artifact aot          # default; deserialize app.cwasm
aver-wasmtime-host --artifact canonical    # JIT the certificate-subject app.wasm
aver-wasmtime-host --artifact optimized    # JIT app.optimized.wasm
```

`canonical` and `optimized` use the same linked providers, runtime policy, and
entry path as AOT. This makes a stage difference attributable: canonical-only
success points at Binaryen, while optimized success with AOT failure points at
native-image production or loading. A pack built without `--optimize` rejects
the `optimized` selection instead of aliasing it to the canonical file.

There is deliberately no automatic fallback. Production keeps the zero-JIT
`aot` default and fails closed if that chain is invalid. Each diagnostic mode
checks only the artifact it was asked to execute, so it remains usable when a
later derivative is the broken stage.

## Providers and the build cache

The pack uses the same `[providers]` composition as `aver run --wasm-gc`.
Each configured Rust `ProviderBinding` is statically linked into the native
host. Cargo is therefore required on the build machine when a new provider
composition is seen, but never on the destination machine.

The release host is content-addressed by the Aver version, Rust toolchain,
platform, provider packages and factories, and provider source state. Aver
reuses that host when only the `.av` program changes. A provider source or
composition change builds a distinct host; distinct compositions also have
distinct internal filenames, so concurrent cache entries cannot overwrite one
another.

## Checks before execution

The manifest is data, not an instruction to trust. On the default AOT path,
before Wasmtime instantiates the module, the host:

1. hashes the canonical `.wasm`, optional `.optimized.wasm`, and `.cwasm`
   bytes and compares every present stage with the manifest;
2. requires a Wasmtime precompiled-module envelope and the exact engine
   compatibility fingerprint recorded by the host that built the pack;
3. deserializes the checked image, then compares every import module, name,
   parameter, and result type with the manifest;
4. reconstructs bundled custom capability contracts and recomputes their
   contract and replay-model hashes;
5. compares the required operations and the identity/fingerprint of every
   provider with the bindings compiled into the executable; and
6. parses and enforces the runtime effect policy carried from `aver.toml`.

Any mismatch stops before instantiation with a `wasmtime-bundle-*` diagnostic.
The canonical and optimized diagnostic paths validate the selected file's hash
and its own recorded import surface, then apply the same contract, provider,
entry, and policy checks. They intentionally do not require later derivatives
to be intact.

`--optimize` writes a sibling instead of replacing the canonical artifact, and
the AOT image is derived from the optimized sibling when present. `--certify`
certifies only the canonical `.wasm` and leaves its `cert/`
directory beside the bundle artifacts; certificate verification remains the
separate `aver cert check` / `aver-cert check` operation and is not silently
replaced by the host's deployment checks. The certificate binds the Wasm, not
Cranelift's native output; Wasmtime remains in the trusted execution path just
as it is when compiling the module at startup.

The `.cwasm` file contains native executable code and Wasmtime intentionally
deserializes that format with fewer checks than portable Wasm. The host reaches
that operation only after the digest, envelope, and engine fingerprint checks.
Those checks detect partial or accidental replacement; they are not a bundle
signature. Deployment integrity or code signing must cover the host, manifest,
canonical/runtime Wasm files, and `.cwasm` as one trust unit. Replacing the
complete unit is equivalent to replacing any other native application.

## Current boundary

The first pack surface is deliberately narrow:

- target: `wasm-gc` on Wasmtime GC;
- entry: `main` (not an incoming HTTP `--handler`);
- execution mode: live effects; a toolchain-free record/replay control surface
  is not part of schema 3;
- platform: the build machine's OS and architecture;
- standard capabilities: compiler-shipped wasm-gc adapters; custom Rust
  replacement of a standard adapter is rejected explicitly.

The `wasip2` target stays a host-neutral Component Model artifact and does not
grow an Aver-owned host pack. Cross-platform standard-host downloads and
cross-compiling arbitrary custom providers are separate deployment concerns;
the supported path is to build the pack on its destination platform or in a
matching CI runner.
