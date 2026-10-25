# Rust Backend

Rust is Aver's deployment backend. Use it when you want a native Cargo project, the usual Rust build/test/run loop, and deployment without the Aver runtime.

## Quick start

```bash
aver compile examples/core/hello.av -o /tmp/hello-rs
cd /tmp/hello-rs && cargo run --profile iteration
```

Output:
```
Compiled examples/core/hello.av → /tmp/hello-rs/ [Rust]
  cd /tmp/hello-rs && cargo run --profile iteration  # final: cargo build --release
```

Add `--check` to validate the generated crate right away:

```bash
aver compile examples/core/hello.av -o /tmp/hello-rs --check
```

For the Rust target, `--check` runs `cargo check` against the generated manifest, forwards Cargo's diagnostics, and exits non-zero when Cargo or rustc rejects the project. The generated files stay on disk after a failure so you can inspect them. Without the flag, `aver compile` only emits the project and never invokes Cargo.

## Check, iterate, release

Generated projects keep three workflows apart:

| Goal | Command | What it optimises for |
|---|---|---|
| validate emitted Rust | `cargo check` | fastest type and borrow checking; no executable |
| edit and run | `cargo run --profile iteration` | incremental codegen, no LTO, 256 codegen units, `opt-level = 1` |
| deploy | `cargo build --release` | whole-program LTO and one codegen unit |

`aver compile` writes a generated `README.md` listing these commands. When you compile again into the same output directory, every generated file is compared byte for byte, and an unchanged file keeps its mtime. Cargo then sees a true no-op, and when only one Aver module changed it can reuse its incremental state.

The iteration profile inherits release semantics and leaves the final release profile as strong as it was. The measurements, and the current decision to keep a single generated crate, are in [Rust iteration build measurements](rust-iteration-builds.md).

## What it generates

The output is a complete Cargo project:

```
out/
  Cargo.toml
  README.md
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

Generated Cargo projects target Rust edition 2024.

## Representation capabilities

The Rust backend derives representation traits from an explicit capability contract per fully qualified Aver type. Two modules can therefore expose types with the same bare name and still make separate `Clone`, equality, hashing and display decisions. A composite type gets each trait independently, and only when the selected Rust representation of every field supports it.

| Aver representation | Generated capabilities |
|---|---|
| `Int`, `Bool`, `Unit`, `String` | `Clone`, `PartialEq`, `Eq`, `Hash`, Aver display |
| `Float` | `Clone`, `PartialEq`, Aver display |
| `List<T>`, `Vector<T>`, `Map<K, V>` | inherited from the selected runtime carrier and its element/key/value types |
| packed byte refinement | the same equality, hashing, and display contract as ordinary `List<Int>` |
| capability resource | cloneable opaque handle and opaque Aver display; no Aver equality or hashing |

`Hash` and ordering are separate capabilities. A representation can be hashable and still not be an admissible ordered map key. Provider resource carriers keep the host-side comparison they need to move and clone composites. That implementation detail does not make resource identity observable in Aver: source equality, hashing, and any resource-containing operation that would expose it are rejected before Rust is emitted.

### Recursive sums

A recursive sum (one a value of which can hold a value of the same type with no `List`, `Vector` or `Map` in between, directly or through records, other sums, `Option`, `Result` or tuples) is declared with `aver_rt::thin_sum!`. A value is one pointer, 8 bytes, and so is an `Option` of one. It points to a node holding a 32-bit count, the 32-bit tag of the constructor and then the struct `T__Variant` of that constructor's fields only, so a node is as large as its own constructor, not the largest one. A constructor without fields has no node: its value is a pointer to the address `tag + 1`, never read through, and copying or dropping it touches no memory. Copying a value copies the pointer; the count is atomic, because values cross threads in `(a, b)!`, and a copy past two billion owners of one node aborts, as `Arc` does past its limit.

Generated code builds a value with `T::Variant(T__Variant(..))`, or `T::Empty` for a constructor without fields, and matches on its view, `match t.view() { T__view::Variant(fields) => .., T__view::Empty => .. }`, where `fields` is a `&T__Variant`; `==` and ordering compare two values of the same constructor through `T::view_pair`. `Debug` and `Hash` read the view, which lists the constructors in declaration order, so they print and hash as an enum would. Hand-written Rust that held such a value as an enum matches on `.view()` the same way. All the unsafe code is in `aver_rt::thin` and the expansion of the macro, which pairs each tag with its struct.

Dropping a value takes a bounded amount of stack however deep it is. Each `T__Variant`, and each record on the cycle of a recursive sum, implements `aver_rt::thin::HandOver`: when its node is freed, the fields that hold values of a recursive sum, directly or in an `Option`, are handed to the drop that began, which frees the nodes they were the last owner of up to 64 deep inside one another and puts the rest on a list it frees one at a time. Every other field is dropped as it is, so a value deep only through a `Result`, a tuple or a collection inside a node is still freed one frame per level.

What `main` still holds after its last expression is not freed. When `main` returns `Unit`, `Int` or `Result<Unit, String>`, its last expression goes through `aver_rt::exit_after_main`, which flushes stdout and stderr and ends the process with `main`'s status, so the system takes the memory back at once instead of the program walking every node. The binary's output and exit status are the same. A run under `AVER_BENCH_ITER` and a build with `--with-replay` return from `main` as before, because the benchmark loop calls `main` again and the recording is written after it returns.

### Shared constructors

A constructor of any other sum much larger than its siblings keeps its fields in one struct `T__Variant` behind an `aver_rt::Shared<T__Variant>`, so copying the value copies a pointer. `Shared` is `std::sync::Arc` without weak references: its node is one atomic count followed by the fields, 8 bytes of header instead of the 16 of an `Arc`, and the atomic operations are those of `Arc`'s strong count, because values cross threads in `(a, b)!`. It has the `Arc` methods generated code uses (`new`, `clone`, `ptr_eq`, `as_ptr`, `strong_count`, `get_mut`, `make_mut`, `try_unwrap`, `unwrap_or_clone`) and `Deref`, `AsRef<T>`, `Borrow<T>`, `Clone`, `Debug`, `Display`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash` and `Default`, all reading the value. Its `==` never compares addresses; a payload whose `==` is reflexive (no `Float` inside) starts its own `==` with `std::ptr::eq`, which is what `Arc` does for such a type. Hand-written Rust that holds an `Arc<T>` converts with `Shared::from(arc)` and `Arc::from(shared)`: the value moves when the source is its only owner and is cloned otherwise. The runtime's lists, vectors, maps and strings keep their nodes in `Shared` too (`AverStr` holds a `Shared<str>`, one node with the count and the bytes; `AverStr::from` also takes an `Arc<str>`, copying the text). Provider objects are still registered as `std::sync::Arc<dyn CapabilityProvider>`, and the work engine, the TCP reactor and the string index keep `Arc`.

### Int

An `Int` is one machine word, `aver_rt::AverInt` (8 bytes, `Option<Int>` too): a value in `[-2^62, 2^62)` is stored in the word and arithmetic on it stays in registers, while a larger magnitude goes to the heap as a `BigInt`, so a program that packs a full 63 bits into one `Int` (a 64-bit hash, several fields in one word) pays an allocation for every value past `2^62`.

### Refinements over Int

A refinement over `Int` is a record of one `Int` field, listed under `exposes opaque`, built by a smart constructor whose check bounds the value within `i64` (`Bool.and(n >= 0, n <= 4611686018427387903)` for a word below `2^62`). Such a type is a struct whose field is an `i64`, not an `AverInt`. It keeps its name and every trait, so it goes into variants, records, lists, options, tuples and maps, as a key or a value, as any record does, and it has nothing to drop. Building one narrows the checked value to `i64` once. Reading its field gives that `i64`: arithmetic on it runs on `i64` where the bound proves every step stays within `i64`, and so do `Int.mod` and `Int.div` by a literal above zero, `Bits.and`, `Bits.or`, `Bits.xor`, `Bits.shiftRight`, `Bits.shiftLeft` and `Bits.low` by a literal count, `Int.min` and `Int.max`. The result becomes an `AverInt` only where it meets a general `Int`: a parameter, a return, a field of another type, a value printed. Those operations run on `i64` for any `Int` the backend already holds as one, such as a loop counter or a string position, as well.

The type keeps its `AverInt` field when anything but its smart constructor builds it from anything but a literal inside the bound (in a function or in a module binding; a `verify` case may build it from any value, since it runs as a test with overflow checks), and when it is a capability's boundary type or the key of a wait set. Types are told apart by identity, so a type of the same name in another module changes nothing. `aver compile --target rust` prints a note for each type of this shape that keeps its `Int` field, with the reason and the place: a build at a given line of a function, a module binding, a bound that does not fit 64 bits, a smart constructor on a type not listed under `exposes opaque`.

A comparison of an `Int` with a literal outside `[-2^62, 2^62)` (`n < 4611686018427387904`), and a `match` arm with such a literal, compares with the machine integer instead of building the literal, which would put a big integer on the heap each time. Inside that range a literal costs nothing to build.

## Runtime dependency

`Cargo.toml` is generated around the shared `aver-rt` runtime crate. Service-specific runtime features are turned on only when needed:

| Aver service | Rust crate |
|-------------|------------|
| no `Http` effects | `aver-rt = { version = "=0.2.1" }` |
| `Http` effects present | `aver-rt = { version = "=0.2.1", features = ["http"] }` |

`ureq` comes in transitively through `aver-rt/http`. Generated projects do not declare it themselves.

The generated binary uses the platform's allocator. A project can ask for mimalloc instead with `[rust]` `allocator = "mimalloc"` in its `aver.toml` (the default is `"system"`; any other value is an error): `Cargo.toml` then turns on the `mimalloc` feature of `aver-rt` and `src/main.rs` declares `aver_rt::MiMalloc` as the `#[global_allocator]`. It sends requests aligned to 16 bytes or less to plain `mi_malloc`, so a 48-byte value takes a 48-byte block rather than the 64-byte one that `mi_malloc_aligned` falls back to when the 48-byte page is full. It helps programs that allocate heavily, most of all on macOS, and needs a C compiler to build.

When developing the runtime inside the Aver repository, set `AVER_RUNTIME_PATH` before running `aver compile`. The generated project then uses a path dependency instead of the crates.io release:

```bash
AVER_RUNTIME_PATH="$(pwd)/aver-rt" aver compile examples/core/hello.av -o /tmp/hello-rs
```

## Native custom capability providers

A provider crate exports a public factory with no arguments. It returns the same checked binding that an embedded VM accepts:

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

Here `ClockProvider` implements `aver_rt::provider::CapabilityProvider`. The hash and the operation set describe the complete checked Aver contract.

Declare the static composition explicitly in the `aver.toml` next to the module root:

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

For local development, replace `version` with a path relative to that `aver.toml`. The generated output directory has no effect on it:

```toml
path = "providers/clock"
```

The ordinary generated binary is then the host:

```bash
aver compile app.av --module-root . --target rust -o build/app
cd build/app
cargo run
```

The same manifest is part of what the program means on the ordinary bytecode VM. Whenever a program reaches a bound capability, these commands compose the same packages into a cached host, with no flag needed:

```bash
aver run app.av --module-root .
aver verify app.av --module-root .
aver audit . --module-root .
aver run app.av --module-root . --wasip2
```

The first invocation builds a thin Rust binary that links `aver-lang` and the declared factories, and says so. The message `Building provider host for Clock: clock-provider from providers/clock (cached at …)` names every package being built and where it comes from (a path relative to the project, or the registry). The `[providers]` table in the project's own `aver.toml` is the consent, and there is no prompt. Later invocations with the same checked composition reuse the host without printing anything.

Normally the Aver program still compiles to bytecode. With `--wasip2`, the host turns on the Component Model runner and adapts the same binding to the generated WIT import. This route currently accepts exactly the WIT subset `Unit`, `Bool`, `Float`, and `String`. Both routes keep the checked registry and panic/fault isolation. VM runs also keep the resource store, replay, and provenance behavior.

Changing only `.av` source does not rebuild the host. Changing a local provider's source lets Cargo do an incremental rebuild. The cache lives in the platform user cache by default, and `AVER_PROVIDER_HOST_CACHE` redirects it.

On unix the host then takes over the command's own process. The process id, the terminal, the exit status and every signal belong to the running program. A SIGINT sent to `aver run` is the SIGINT the program observes through `Process.stopRequested`, exactly as in a binary built with `--target rust`. It follows that a signal no longer ends the command. From the program's first `Process.stopRequested` call the handler is installed, both signals only raise its flag, and the run ends when the program returns. Ctrl-C gives the prompt back once the program has answered the request. A program that stops polling the flag holds its terminal until SIGKILL. On Windows the host is a second process that the command waits on.

`aver verify` may run a configured pure provider in a normal case. An exact `given name: Capability.operation = [stub]` is still a case-local override, and it wins without changing the process binding.

A directory verify or audit composes the project host once, then installs in each module only the bindings whose capability contracts exist there. A single module likewise ignores project bindings it does not reach, and runs in process. `run` and generated Rust use the same projection from project to program. `aver.toml` may describe more capabilities than one probe, benchmark, migration or entry program uses, and inactive bindings are not linked into that artifact. A project without `[providers]` never invokes Cargo or runs provider package code, and its missing-provider diagnostic points at the `[[providers.bindings]]` entry to add.

Backends are a separate question. `--wasip2` adapts WIT-lowerable bindings through the same host. `run --wasm-gc` and `replay --wasm-gc` adapt the same binding through the raw wasm-gc ABI derived from the contract. `--self-host` (and, for now, `verify --wasm-gc`) has no provider host. It refuses a program that reaches a bound capability with `error[capability-provider-unhosted]`, which names the binding and the backend, instead of running without the binding.

`aver compile` validates the manifest, emits each reached Cargo dependency together with its typed `clock_provider::binding()` bootstrap call, and stops there. It does not run Cargo, download packages or manage a lockfile. Cargo resolves the active dependencies when the generated project is built. The cached run/verify/audit host described above is the only path in the stock CLI that builds provider code. The generated stock binary installs all active configured bindings exactly once, then runs the required-provider preflight before benchmarks or Aver entry code. A missing factory or a wrong return type is therefore an ordinary Rust compile error. An incomplete operation set or a wrong contract hash fails at bootstrap, in the shared provider registry.

Schema 1 requires exactly one of `version` or `path` per binding. Capability names and Cargo aliases must be unique. Once `[providers]` is present, every required custom capability needs a binding. A binding whose canonical capability module exists under the project module root, but which the current program closure does not reach, is inactive. That is not an error, and the binding is neither built nor installed. A capability name with no contract in the project is still an error, so typos and foreign bindings do not slip through. Compiler defaults such as `Time` need no entry, but an explicit checked `Time` binding replaces the default when the program reaches it. Provider runtime configuration and secrets stay in the provider's normal host environment and do not go into `aver.toml`.

Without `[providers]`, nothing changes for compatibility. A project with a custom capability stays host-bound, and its stock binary exits with `error[capability-provider-missing]`. Custom embedders can still write their own host binary and use the generated library API directly:

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

The binding holds an `Arc<dyn aver_rt::provider::CapabilityProvider>`, the exact contract hash, and the complete operation set. Calls go through the transport-neutral `ProviderValue` tree, which supports all contract-v1 values, represented records and sums, and capability resources. Direct calls and every `!` / `?!` branch share one registry and one resource store, installed once. Hosts that want none of the compiler-shipped defaults can use `install_provider_bindings_exact`. Unlike `install_provider_bindings`, it does not add the standard `Time` provider.

Installing twice in one process fails. It does not race to replace a mutable global.

## Scoped replay runtime

Use `--with-replay` when the generated binary should understand deterministic record/replay:

```bash
aver compile self_hosted/main.av \
  --module-root self_hosted \
  --with-replay \
  --guest-entry runGuestProgram \
  -o /tmp/aver-self
```

This emits `src/replay_support.rs` and adds the `serde` / `serde_json` / `toml` dependencies used for recording files and guest-scoped runtime policy. Without `--with-replay`, generated projects are smaller and carry no replay support.

Use `--with-self-host-support` only for generated programs that are themselves meta-runtimes in the style of the self-host and need the evaluator's scoped function store:

```bash
aver compile self_hosted/main.av \
  --module-root self_hosted \
  --with-replay \
  --policy runtime \
  --guest-entry runGuestCliProgram \
  --with-self-host-support \
  -o /tmp/aver-self
```

This emits a separate `src/self_host_support.rs` module, which is deliberately kept out of the generic generated runtime.

Generated Rust also takes an explicit policy mode:

```bash
aver compile app.av --policy embed
aver compile app.av --policy runtime
```

- `--policy embed` bakes the current `aver.toml` into the generated project
- `--policy runtime` loads `aver.toml` from the active module root when the binary runs
- default: `embed` for plain `compile`, `runtime` when `--with-replay` is enabled

`--guest-entry` matters for meta-runtimes such as the self-hosted interpreter. Bootstrap and tooling work stays outside record/replay and policy scope. Only the chosen guest entry runs inside the scoped runtime, and `aver.toml` policy and replay interception start at that boundary. The policy is loaded at runtime from the guest module root and is not baked into the binary.

With `--with-self-host-support`, the chosen `--guest-entry` has an extra explicit contract:

- it must declare `prog: Program`
- it must declare `moduleFns: List<FnDef>`

Generated Rust uses those two parameters to install the temporary self-host callback store around the guest execution boundary. If the contract is not met, `aver compile` fails early with a readable error and does not generate a broken project.

When the guest entry has a parameter named `guestArgs: List<String>`, generated replay support treats it as the guest's CLI input:

- `Args.get()` inside the scoped guest run returns `guestArgs`
- replay `input` records only `guestArgs`, not the outer wrapper arguments
- self-host bootstrap args such as `program_file` and `module_root` stay outside the guest trace

`SelfHostRuntime.*` is also gated explicitly. If generated code uses `SelfHostRuntime.*`, `aver compile` requires `--with-self-host-support`, and the detection covers top-level statements as well as function bodies.

## Supported features

Every language feature can be transpiled:

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
| `HttpWire` / `HttpServer` standard modules | OK |
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

Verify blocks become `#[test]` functions:

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

This replaces the old output, which was one giant file, and keeps the generated Rust of medium-sized projects reviewable.

## Service runtime architecture

Generated Rust uses `aver-rt` as its shared runtime, and the service implementations live there:

- `Tcp`: standard capability provider over the shared `aver-rt::tcp` runtime; `Tcp.Connection` crosses generated code as a provider-owned resource
- `Http`: shared `aver-rt::http` client, enabled by the `http` feature
- incoming HTTP: pure `HttpWire` framing plus the Aver `HttpServer` loop over provider-backed `Tcp`; fetch-style targets expose an explicit `--handler`
- `Console`, `Time`, `Disk`, `Env`, `Args`: shared helpers from `aver-rt`
