# Rust Backend

Rust is the deployment backend for Aver.

Use it when you want:
- a native Cargo project
- a normal Rust build/test/run loop
- deployment without the Aver runtime

## Quick start

```bash
aver compile examples/core/hello.av -o /tmp/hello-rs
cd /tmp/hello-rs && cargo build && cargo run
```

Output:
```
Compiled examples/core/hello.av → /tmp/hello-rs/ [Rust]
  cd /tmp/hello-rs && cargo build && cargo run
```

## What it generates

Generates a complete Cargo project:

```
out/
  Cargo.toml
  src/
    main.rs
    runtime_support.rs
    aver_generated/
      mod.rs
      entry/
        mod.rs
      ...
```

The generated project includes:
- `src/lib.rs` with the public host boundary when capabilities are present
- `src/main.rs` with the runtime prelude and final entrypoint
- `src/runtime_support.rs` for the shared `aver-rt` bridge and shared runtime types
- `src/provider_support.rs` when the program calls a capability operation
- `src/replay_support.rs` when `--with-replay` is enabled
- `src/aver_generated/.../mod.rs` files that preserve the Aver module graph as Rust modules
- `src/verify.rs` when the entry module has `verify` blocks

The generated Rust keeps:
- user-defined types as Rust `struct`s and `enum`s inside their originating modules
- direct `depends [...]` modules as explicit Rust imports inside generated module files
- module-qualified Aver calls such as `Domain.Tasks.replayTask(...)` as qualified Rust paths
- `fn main()` in `src/main.rs` delegating to `aver_generated::entry::main()`
- `#[cfg(test)]` verify blocks as Rust tests for the entry module

`src/main.rs` includes:
- runtime bridge (`aver_rt` module re-exporting the shared `aver-rt` crate)
- shared runtime type imports for built-in service records when needed
- the root `aver_generated` module tree
- the final `fn main()` entry point

Generated Cargo projects now target Rust edition 2024.

## Runtime dependency

`Cargo.toml` is generated around the shared `aver-rt` runtime crate. Service-specific runtime features are enabled only when needed:

| Aver service | Rust crate |
|-------------|------------|
| no `Http` effects | `aver-rt = { version = "=0.2.1" }` |
| `Http` effects present | `aver-rt = { version = "=0.2.1", features = ["http"] }` |

`ureq` is pulled transitively by `aver-rt/http`; generated projects do not declare it directly.

For local runtime development from the Aver repository, set `AVER_RUNTIME_PATH` before running `aver compile` to force a path dependency instead of the crates.io release:

```bash
AVER_RUNTIME_PATH="$(pwd)/aver-rt" aver compile examples/core/hello.av -o /tmp/hello-rs
```

## Native custom capability providers

A provider crate exports a public zero-argument factory returning the same checked binding accepted by an embedded VM:

```rust
pub fn binding() -> aver_rt::provider::ProviderBinding {
    aver_rt::provider::ProviderBinding::new(
        "Clock",
        CLOCK_CONTRACT_HASH,
        ["Clock.now"],
        std::sync::Arc::new(ClockProvider),
    )
}
```

Here `ClockProvider` implements `aver_rt::provider::CapabilityProvider`, and the hash and operation set describe the complete checked Aver contract.

Declare explicit static composition in the `aver.toml` beside the module root:

```toml
[providers]
schema = 1

[[providers.bindings]]
capability = "Clock"
crate = "clock_provider"
package = "aver-clock-provider"
version = "=0.1.0"
factory = "binding"
```

For local development, replace `version` with a path relative to that `aver.toml`; the generated output directory does not affect it:

```toml
path = "providers/clock"
```

Then the ordinary generated binary is the host:

```bash
aver compile app.av --module-root . --target rust -o build/app
cd build/app
cargo run
```

Or keep the ordinary bytecode VM and explicitly compose the same packages into
a cached host:

```bash
aver run app.av --module-root . --providers
aver verify app.av --module-root . --providers
aver audit . --module-root . --providers
aver run app.av --module-root . --providers --wasip2
```

The first invocation builds a thin Rust binary that links `aver-lang` and the
declared factories. Later invocations with the same checked composition reuse
it directly. Normally the Aver program still compiles to bytecode. With
`--wasip2`, the host enables the Component Model runner and adapts the same
binding to the generated WIT import; this route currently accepts the exact WIT
subset `Unit`, `Bool`, `Float`, and `String`. Both routes retain the checked
registry and panic/fault isolation. VM runs additionally retain the resource
store, replay, and provenance behavior. Changing only `.av` source does not
rebuild the host. Changing a local provider source lets Cargo perform an
incremental rebuild. The cache defaults to the platform user cache and can be
redirected with `AVER_PROVIDER_HOST_CACHE`.

`aver verify --providers` may execute a configured pure provider in a normal
case. An exact `given name: Capability.operation = [stub]` remains a
case-local override and wins without mutating the process binding. A directory
verify or audit composes the project host once, then installs only the subset
of bindings whose capability contracts exist in each file. A single unrelated
module likewise ignores project bindings it does not reach. The same
project-to-program projection applies to `run` and generated Rust: `aver.toml`
may describe more capabilities than one probe, benchmark, migration, or entry
program uses, and inactive bindings are not linked into that artifact.
`--providers` is opt-in: plain `run`, `verify`, and `audit` never invoke Cargo
or execute provider package code, and a matching missing-provider diagnostic
prints the exact command that enables it. On `run`, the flag conflicts with
`--self-host` and bare `--wasm-gc`; it can be combined with `--wasip2`. On
`verify`, it conflicts with `--wasm-gc`.

`aver compile` validates the manifest, emits each reached Cargo dependency and its typed `clock_provider::binding()` bootstrap call, and stops. It does not run Cargo, download a package, or manage a lockfile; Cargo resolves the active dependencies when the generated project is built. The separate `--providers` run/verify/audit workflow above is the only stock CLI path that builds provider code. The generated stock binary installs all active configured bindings exactly once, then runs required-provider preflight before benchmarks or Aver entry code. A missing factory or wrong return type is therefore a normal Rust compile error, while an incomplete operation set or wrong contract hash fails at bootstrap in the shared provider registry.

Schema 1 requires exactly one of `version` or `path` per binding. Capability names and Cargo aliases must be unique. Once `[providers]` is present, every required custom capability needs one binding. A binding whose canonical capability module exists under the project module root but is absent from the current program closure is inactive, not erroneous, and is neither built nor installed. A capability name with no project contract remains an error, so this rule does not hide typos or foreign bindings. Compiler defaults such as `Time` need no entry, but an explicit checked `Time` binding replaces the default when that program reaches it. Provider runtime configuration and secrets stay in the provider's normal host environment, not in `aver.toml`.

Without `[providers]`, compatibility stays unchanged: a custom-capability project remains host-bound, and its stock binary exits with `error[capability-provider-missing]`. Custom embedders can still add their own host binary and use the generated library API directly:

```rust
use generated_app as generated;

fn main() {
    generated::install_provider_bindings(vec![my_provider::clock_binding()])
        .expect("install capability provider");
    generated::preflight_required_providers().expect("provider preflight");
    let answer = generated::aver_generated::entry::main();
    println!("{answer}");
}
```

The binding contains an `Arc<dyn aver_rt::provider::CapabilityProvider>`, the exact contract hash, and the complete operation set. Calls use the transport-neutral `ProviderValue` tree and support all contract-v1 values, represented records/sums, and opaque resources. One once-installed registry and resource store is shared by direct calls and every `!` / `?!` branch. `install_provider_bindings_exact` is available to hosts that want no compiler-shipped defaults; unlike `install_provider_bindings`, it does not add the standard `Time` provider.

Repeated installation in one process fails rather than racing a mutable global replacement.

## Scoped replay runtime

Use `--with-replay` when the generated binary should understand deterministic record/replay:

```bash
aver compile self_hosted/main.av \
  --module-root self_hosted \
  --with-replay \
  --guest-entry runGuestProgram \
  -o /tmp/aver-self
```

This emits `src/replay_support.rs` and adds the `serde` / `serde_json` / `toml` dependencies needed for recording files and guest-scoped runtime policy. Without `--with-replay`, generated projects stay smaller and do not carry replay support.

Use `--with-self-host-support` only for generated programs that are themselves self-host-like meta-runtimes and need `SelfHostRuntime.*` builtins such as the self-hosted `HttpServer` bridge:

```bash
aver compile self_hosted/main.av \
  --module-root self_hosted \
  --with-replay \
  --policy runtime \
  --guest-entry runGuestCliProgram \
  --with-self-host-support \
  -o /tmp/aver-self
```

This emits a separate `src/self_host_support.rs` module. It is intentionally not part of the generic generated runtime.

Generated Rust also exposes policy mode explicitly:

```bash
aver compile app.av --policy embed
aver compile app.av --policy runtime
```

- `--policy embed` bakes the current `aver.toml` into the generated project
- `--policy runtime` loads `aver.toml` from the active module root when the binary runs
- default: `embed` for plain `compile`, `runtime` when `--with-replay` is enabled

`--guest-entry` matters for meta-runtimes such as the self-hosted interpreter:

- bootstrap/tooling work stays outside record/replay and policy scope
- only the chosen guest entry runs inside the scoped runtime
- `aver.toml` policy and replay interception start at that boundary
- policy is loaded at runtime from the guest module root instead of being baked into the binary

For `--with-self-host-support`, the chosen `--guest-entry` has an additional explicit contract:

- it must declare `prog: Program`
- it must declare `moduleFns: List<FnDef>`

Generated Rust uses those two parameters to install the temporary self-host callback store around the guest execution boundary. If the contract is not met, `aver compile` now fails early with a readable error instead of generating a broken project.

When the guest entry has a parameter named `guestArgs: List<String>`, generated replay support treats that parameter as the guest CLI input:

- `Args.get()` inside the scoped guest run returns `guestArgs`
- replay `input` records only `guestArgs`, not the outer wrapper arguments
- self-host bootstrap args such as `program_file` and `module_root` stay outside the guest trace

`SelfHostRuntime.*` is also gated explicitly now:

- if generated code uses `SelfHostRuntime.*`, `aver compile` requires `--with-self-host-support`
- this detection includes top-level statements, not only function bodies

## Supported features

All language features are transpilable:

| Feature | Status |
|---------|--------|
| Arithmetic, comparisons, string interpolation | OK |
| `match` with all pattern types | OK |
| `Result<T,E>`, `Option<T>` constructors + match | OK |
| User-defined sum types (`type Shape`) | OK |
| User-defined records (`record User`) | OK |
| Record update (`User.update(u, field = val)`) | OK |
| List literals, `List.*` operations | OK |
| Map literals, `Map.*` operations | OK |
| Tuple literals, tuple patterns | OK |
| Error propagation (`?`) | OK |
| Tail-call optimization | OK |
| Module imports (`depends [X]`) | OK |
| `Console` service | OK |
| `Http` service | OK |
| `HttpServer` service (`listen`, `listenWith`) | OK |
| `Tcp` capability (provider-backed persistent connections) | OK |
| `Disk` service | OK |
| `Env` service | OK |
| `Random` service | OK |
| `Time` service | OK |
| `Terminal` service (feature-gated) | OK |
| `Args` service | OK |
| `verify` blocks → `#[cfg(test)]` | OK |
| Exact method-level effects (`Http.get`, `Disk.readText`, etc.) | OK |

## Running verify blocks

Verify blocks are emitted as `#[test]` functions:

```bash
aver compile examples/core/calculator.av -o /tmp/calc
cd /tmp/calc && cargo test
```

## Module lowering

When a program has `depends [Data.Fibonacci]`, the transpiler:
1. loads the dependent `.av` file recursively, with circular import detection
2. lowers each Aver module into a Rust module under `src/aver_generated/...`
3. imports direct `depends [...]` modules explicitly in the generated Rust
4. keeps qualified calls module-qualified: `Data.Fibonacci.fib` becomes `crate::aver_generated::data::fibonacci::fib`

This avoids the old giant single-file output and keeps medium projects reviewable in generated Rust.

## Service runtime architecture

Generated Rust uses `aver-rt` as the shared runtime. The actual service implementations live there:

- `Tcp`: standard capability provider over the shared `aver-rt::tcp` runtime;
  `Tcp.Connection` crosses generated code as a provider-owned opaque resource
- `Http`: shared `aver-rt::http` client, enabled by the `http` feature
- `HttpServer`: shared `aver-rt::http_server` loop and request/response types
- `Console`, `Time`, `Disk`, `Env`, `Args`: shared helpers from `aver-rt`
