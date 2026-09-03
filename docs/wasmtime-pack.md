# Self-contained Wasmtime packs

`--pack wasmtime` turns a wasm-gc program and its host-side meaning into a
directory that can be copied to another machine:

```bash
aver compile app.av --target wasm-gc --pack wasmtime -o out/
./out/aver-wasmtime-host arg-one arg-two
```

The destination does not need Aver, Cargo, a separately installed Wasmtime,
the Aver sources, the provider sources, or `aver.toml`. It needs only the
bundle directory and the same OS/architecture for which its native host was
built.

## Bundle layout

```text
out/
  aver-wasmtime-host   native launcher, Wasmtime, and linked providers
  app.wasm             exact wasm-gc artifact
  manifest.json        artifact, ABI, capability, provider, and policy facts
```

Program arguments passed after `aver-wasmtime-host` become the program's
`Args` values. The host always invokes the exported `main` function.

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

The manifest is data, not an instruction to trust. Before Wasmtime
instantiates the module, the host:

1. hashes the actual `.wasm` bytes and compares them with the manifest;
2. reads the module import section and compares every module, name, parameter,
   and result type;
3. reconstructs bundled custom capability contracts and recomputes their
   contract and replay-model hashes;
4. compares the required operations and the identity/fingerprint of every
   provider with the bindings compiled into the executable; and
5. parses and enforces the runtime effect policy carried from `aver.toml`.

Any mismatch stops before instantiation with a `wasmtime-bundle-*` diagnostic.
`--optimize` is applied before these facts are recorded, so the manifest binds
the delivered bytes. `--certify` likewise certifies the delivered module and
leaves its `cert/` directory beside the three runtime files; certificate
verification remains the separate `aver cert check` / `aver-cert check`
operation and is not silently replaced by the host's deployment checks.

## Current boundary

The first pack surface is deliberately narrow:

- target: `wasm-gc` on Wasmtime GC;
- entry: `main` (not an incoming HTTP `--handler`);
- execution mode: live effects; a toolchain-free record/replay control surface
  is not part of schema 1;
- platform: the build machine's OS and architecture;
- standard capabilities: compiler-shipped wasm-gc adapters; custom Rust
  replacement of a standard adapter is rejected explicitly.

A future `--target wasip2 --pack wasmtime` can carry the Component Model and
WASI implementation in a separate, larger host. Cross-platform standard-host
downloads, cross-compiling custom providers, and serialized Wasmtime AOT
images are also independent follow-ups rather than hidden assumptions of this
format.
