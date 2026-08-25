# Changelog

All notable changes to Aver are documented here. Starting with 0.10.0, minor releases get a codename — short, evocative, and it tells you what the release was really about.

## Unreleased

### Added

- **Incoming HTTP is now ordinary Aver code, with one explicit deployment boundary.** The pure `HttpWire` module frames bounded content-length HTTP/1.1 requests and renders responses over `Bytes`; `HttpServer` owns the native listener, one typed `Tcp.Socket` poll map, sessions, keep-alive/pipelining, per-peer failure isolation, and cooperative `Process.stopRequested` cleanup. Request headers retain repeated values, paths and queries are split, conflicting content lengths, transfer encoding, and `Expect` are rejected explicitly, and malformed streams receive one bounded 400 before close. Native code calls `HttpServer.listen(port, handler)` or `serve(listener, handler)`; Cloudflare/wasm-gc and `wasi:http/proxy` keep the same `Fn(HttpRequest) -> HttpResponse` handler and select it only through `--handler`. There is no provider-owned request token and no host-to-VM callback bridge. Addresses [#1124](https://github.com/jasisz/aver/issues/1124).

- **Higher-order helpers can forward a named callback's exact effects.** A callback parameter written `Fn(A) -> B ! [_]` accepts any concrete callback effect set, and a call through that parameter requires the caller to declare the effects of the named function passed at that call site. `_` is therefore statically resolved forwarding, not an ambient wildcard or a hidden capability grant. `HttpServer` uses this so a handler remains a normal unary function while its `Http.get`, `Tcp.*`, `Console.*`, or other effects stay visible through the entire call graph.

- **Long-running programs can now stop cooperatively without sacrificing cleanup.** `Process.stopRequested() -> Bool` is a recorded, generative standard capability backed by one native SIGINT/SIGTERM flag and by the `aver.process_stop_requested` wasm-gc host import. The flag only transitions from false to true; the same cross-call monotonicity law is emitted into Lean and Dafny and independently guarded across every hostile profile. Recursive proof lifting threads the oracle occurrence instead of restarting at zero, so the `stopAfterThree` litmus both terminates under `aver verify` and builds in Lean without `sorry`. wasip2 reports the capability as unsupported—WASI 0.2 has no signal delivery binding—and directs users to wasm-gc. Polling is cooperative and deliberately does not interrupt blocking effects or independent-product branches. Closes [#1114](https://github.com/jasisz/aver/issues/1114). Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **`Vector.new` now has one portable materialization policy instead of treating addressability as safety.** Sizes are bounded to one mebielement (1,048,576 elements) across the VM, generated Rust, wasm-gc, Lean, and Dafny. The unit is deliberately an element count: Aver has no backend-independent byte layout for arbitrary `T`, but every implementation performs one slot and one clone per element. Dynamic sizes outside the inclusive range return `Result.Err`; only syntactic literals inside it discharge to plain `Vector<T>`. The runtime constant owns the predicate and error text, while `u32` remains only wasm array addressability. Closes [#1110](https://github.com/jasisz/aver/issues/1110).

- **`verify-coverage` now measures reachable shapes and permits narrow, explained residue.** Output-constructor coverage uses the closed image of the function body and transitive local return calls—including mutually recursive groups—instead of every constructor of the declared sum type; an opaque return path suppresses the count rather than inventing a denominator. Verify expressions through local helpers are credited when their outer Result, Option, Bool, list, or sum shape is statically unique. Arbitrary builtin/provider calls remain deliberately unevaluated during `check` and are documented as opaque. `[[check.suppress]]` gains an optional exact `fn` scope, so one uninhabited `Result` arm can be accepted with a mandatory reason without hiding the same warning on neighbouring functions. Closes [#1108](https://github.com/jasisz/aver/issues/1108).

- **`aver verify` now uses the machine instead of repeatedly rebuilding the same program.** The command resolves and parses one deterministic project graph, checks and prepares each project module once, and runs independent files and declared cases in one bounded Rayon pool. `-j N` / `--jobs N` selects the worker count, the default is the machine's available parallelism, and `-j 1` keeps the same graph-reuse path without concurrency. Module ownership, summaries, failures, and JSON remain collected in input/case order, so `-j 1` and `-j N` are byte-identical. On btc-listener's public 98-file corpus (6,016 cases), the full provider-backed command fell from 307.17 s to 7.17 s at `-j 1`, then to 3.98 s at `-j 8`; most of the win was eliminating repeated compiler/provider preflight work, not parallel execution. The same graph input cache now serves whole-directory `check` and `audit`, and provider-host planning no longer performs a second full pipeline per entry. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **Generated Cargo projects now inherit the toolchain's actual source.** A path-built Aver emits path dependencies, a git-installed Aver pins both `aver-lang` and `aver-rt` to the build repository and exact revision, and a registry installation emits exact registry versions. The generated Rust project and cached provider host share this one build-time provenance, so a git development build no longer pairs its generated code with an older same-version runtime from crates.io.

- **Code generation now keeps stable identities all the way to backend rendering.** Rust root-main and tail-call paths consume resolved functions directly; Lean and Dafny resolve proof rewrites once at their producer boundary and no longer carry source-expression rendering adapters. Module-scoped verify reachability is a single transitive `FnId` walk over `ResolvedProgram`, so same-bare-name functions and types in entry and dependency modules cannot cross-wire through a backend cache or name lookup.

- **`aver proof` now checks the whole program's declared proof surface.** Every dependency module's `verify` examples and laws are lowered, emitted in that module's Lean/Dafny namespace, rooted for effectful reachability, evaluated for ground truth in the declaring module, and audited under a module-qualified identity. Visibility still controls whether another module may cite a law; it no longer makes the law itself disappear. The old “dependency verify cases were not sampled” warning is gone: unsupported cases are explicit, scoped declines, and Lean's audit and Dafny's trust counts recurse through the generated module tree. A real 28-module btc-listener proof slice now builds end to end; symbolic-provider cases crossing kernel-opaque recursion decline honestly instead of leaving an undecidable Lean theorem.

- **A verify case can now cost more than the default budget, if the project says so.** `aver verify` caps each case at 1,000,000 VM opcodes. Real corpora contain cases that need more and are not runaway at all — a Bitcoin script at the consensus 10,000-byte limit, a transaction whose every input hashes the whole transaction — and until now the only thing the cap could say about them was `case aborted`, which reads as a defect and invites deleting the case. A project raises the budget for the function it knows about, in `aver.toml`, with a written reason:

  ```toml
  [[verify.costly]]
  fn         = "checkScript"
  files      = ["domain/scriptcases*.av"]
  step-limit = 50_000_000
  max-cases  = 40_000
  reason     = "Bitcoin Core corpus includes consensus-max 10,000-byte scripts"
  ```

  `fn` and a non-empty `reason` are required, `files` is optional and uses the same anchored globs `[[check.suppress]]` uses, and an entry that raised nothing during a run is reported on stderr. The report names what the raise bought — `checkScript case 41: 8.2M steps (limit 50M, aver.toml [[verify.costly]] fn = "checkScript")` — so a raised budget is never a silent licence.

  An entry carries both budgets and must set at least one: `step-limit` for a function whose cases are slow, `max-cases` for one whose `given` domain is wide, both for one that is both. `[verify] step-limit` and `[verify] max-cases` move the same two numbers for the whole project — the per-case opcode budget and the ten-thousand-case ceiling on `given`-domain expansion, which also bounds the `--hostile` cartesian. Both dials resolve the same way for a verify block: the project setting, otherwise the built-in default, raised by the matching entry that grants the most. An entry may only raise — a `step-limit` or `max-cases` at or below the number already in force is a config error, because `[[verify.costly]]` says "this case is expensive, give it room" and a lower number says the opposite. So the order of the entries in `aver.toml` is not part of what the file means: each entry is an independent statement about one function, and adding one can only loosen, never re-point. A project with no `[verify]` section behaves exactly as before, byte for byte.

  The case ceiling belongs to the file, not to one command. `check`, `run`, `compile`, `proof`, `audit`, `format`, `shape`, `context`, `why`, `capabilities`, `replay`, and `bench` all read your project files under the number your project wrote down, as does the dependency walk each of them performs. A `given` domain you declared legal is accepted at every door, one over the ceiling is refused at every door, and the message names the number that actually applied instead of the built-in ten thousand.

- **A verify case that runs out of budget is now declined, not failed.** Exceeding the budget — default or raised — is a third case outcome: `fail[verify-declined]: case not answered`, counted separately from passes and failures in the report and in `--json`, and failing the run. It is not a counter-example, because nothing disagreed; it is Aver saying the case was not checked, which must never read as "this checks out". The proof lane follows: a declined case is exported as a declined claim with its reason instead of a theorem, so `aver proof` no longer emits `impl(sample) = <the expected expression the author wrote>` for a case nothing ran — the exact shape ground-truth literalization exists to prevent, and the one that used to drop out precisely on the largest inputs. `aver verify --wasm-gc` runs on the same budget through one documented conversion factor, so a case is runnable on both lanes or on neither. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A type error inside a dependency's function body is now reported at the dependency's file.** `aver check main.av` used to print such an error with the entry's file name and the dependency's line number; it now names the dependency (`at: lib.av:8:5`) and shows its source, and when the dependency is itself part of the checked program the error appears once, under that module.
- **Map iteration now has one canonical order on wasm-gc too.** `Map.keys`, `Map.values`, and `Map.entries` sort a shared scratch permutation of occupied slots instead of exposing hash-bucket order, preserving key/value pairing without copying map contents. Monomorphized comparators cover numeric `Int`, Unicode `String`, false-first `Bool`, lexicographic lists and proof-packed byte sequences, componentwise tuples, records by field name, and variants by constructor name then payload. List keys are boxed at the table boundary so the valid empty list (`[]`, represented by a null list ref) no longer collides with the empty-bucket marker. The obsolete wasm-gc map-order carve-out has been removed from proof trust headers.

- **TCP now has one event-loop wait for inbound, outbound, and established sockets.** Distinct provider-owned `Tcp.Listener`, `Tcp.Dial`, and `Tcp.Connection` resources make illegal state operations fail at typecheck time, while the represented `Tcp.Socket.{Listening,Dialing,Connected}` sum lets one caller-owned `Map<Int, Tcp.Socket>` carry the complete state machine without parallel-map invariants. `Tcp.listen`/`accept`/`peerAddress` add the server side; `Tcp.beginConnect`/`dialled` add non-blocking outbound establishment; `Tcp.poll` waits over all three states with one poller, returns every observed ready caller key sorted and duplicate-free, and clips its timeout to the nearest dial deadline. False wakes remain legal (`accept`/`dialled` may answer `None`). `closeDial` and `closeListener` release their exact resource kinds.

  `[effects.Tcp].max_connections` (default 256) is one shared bound for established and accepted connections plus in-flight dials. A full pool does not drain the listener backlog, and one-shot `send`/`sendBytes`/`ping` remain outside it. VM, generated Rust, and the in-process wasm-gc host implement the complete API with record/replay and hostile profiles. wasip2 keeps native blocking `connect`, connected-socket `poll`, and `readSome`; the new dial/listener operations compile to typed Level-B `Result.Err` values, and a non-default connection limit produces a deployment warning. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A project provider manifest may be a superset of one entry program.** `aver run probe.av --providers` and `aver compile probe.av --target rust` now treat a binding for another project capability as inactive instead of rejecting the probe; generated Cargo projects and cached hosts omit its dependency and factory entirely. The planner resolves such names through the explicit module root, so a capability with no project contract remains an error and every custom capability required by the current program still needs a binding. The same one-law projection now serves run, compile, verify, and audit.

- **Configured providers now work as a whole-project verify gate.** `aver verify file-or-dir --providers` projects the project-wide binding set onto each file's capability contracts, so a provider used by one module no longer makes independent modules fail as “unknown capability” or disappear under a misleading type-error skip. Single-file verify likewise ignores unrelated project bindings. `aver audit file-or-dir --providers` now uses the same cached Rust host for its verify axis, and genuine provider setup failures surface as `verify-provider-setup` / provider-composition errors instead of being swallowed or attributed to source type checking.

- **One configured Rust provider now runs unchanged through VM, generated Rust, and the wasip2 Component Model.** `aver run app.av --wasip2 --providers` reuses the schema-1 manifest and cached provider host, validates each exact `ProviderBinding` through the shared native registry, and dynamically supplies the component's generated WIT imports in embedded wasmtime. The adapter converts the existing WIT subset (`Unit`, `Bool`, `Float`, `String`) to and from the same transport-neutral `ProviderValue` tree; pure and effectful operations share it, and provider faults, panics, or wrong result shapes retain provider-boundary diagnostics instead of degrading into canonical-ABI traps. The cached host enables the heavyweight `wasip2` feature only for this combined command. Plain `run --wasip2` remains inert, while `compile --target wasip2` still emits a portable unresolved component plus sibling WIT for external hosts.

- **Configured Rust providers can now run on the ordinary VM.** `aver run app.av --providers` and `aver verify app.av --providers` reuse the schema-1 `[providers]` composition already accepted by generated Rust, build a deterministic thin Rust host outside the project tree, and execute the stock bytecode VM with every checked binding installed in-process. The host cache is keyed by Aver host ABI, Rust toolchain, platform, and the resolved package/factory plan; `.av` edits reuse it, while local provider/runtime edits trigger Cargo's incremental rebuild. Plain `run` and `verify` remain inert—no Cargo, registry access, or provider package execution—and a matching missing-provider error now prints the explicit opt-in command. VM options and program args cross the host unchanged, recordings retain provider provenance, direct and `!`/`?!` branch VMs share the one registry, pure verify cases can call the real provider, and an exact source-local `given` still wins only for its case.

- **Native provider packages now have an ordinary generated-Rust workflow.** A versioned `[providers]` schema in `aver.toml` maps each required custom capability to an explicit Cargo package or module-root-relative path plus a validated zero-argument `ProviderBinding` factory. `aver compile --target rust` emits the reached dependencies and a stock-binary bootstrap that constructs every active binding once, installs the shared registry, and preflights the complete required operation set before benchmarks or Aver code. Cargo remains the resolver: compilation does not download packages, invoke Cargo, create a lockfile, or make plain `aver run` load arbitrary Rust. Missing/unsupported schema, conflicting sources, unsafe identifiers, duplicate or project-unknown bindings, missing local packages, incomplete opt-in composition, Rust factory type errors, and runtime contract mismatches all fail at their owning boundary. Projects without `[providers]` retain the manual host-bound API; compiler defaults such as `Time` remain implicit and may be explicitly replaced through the same checked binding path.

- **One native Rust provider binding now runs unchanged through both the bytecode VM and generated Rust.** The contract-checked binding registry, panic/fault isolation, provider provenance, and capability-resource store now live in `aver-rt`; the VM is a language-value adapter over that shared core, while generated Cargo projects expose `install_provider_bindings` and accept the same `ProviderBinding` backed by the same `Arc<dyn CapabilityProvider>`. Generated custom calls cross the complete `ProviderValue` vocabulary—scalars, tuples, lists, vectors, deterministic maps, `Result`, `Option`, represented records/sums, and representation-less resources—and validate the declared return shape before Aver code sees it. Direct calls and `!`/`?!` branches share one immutable registry and resource store.

  Native record/replay now follows the VM matrix in generated Rust: pure calls stay live, recorded/suppressed calls consume without a provider, and reissued calls consume then run live. Recordings pin sorted contract/model/provider/fingerprint provenance and serialize capability resources only as trace-local tokens. Missing bindings fail the stock generated binary at preflight with `error[capability-provider-missing]`; duplicate, incomplete, extra-operation, hash-mismatched, wrong-shape, faulting, and panicking providers fail closed at the shared boundary. Standard `Time` also uses this registry, with an explicit exact-binding host mode proving there is no handwritten fallback. The boundary is static: Aver never implicitly discovers or downloads provider code, and generated projects without an explicit `[providers]` composition remain host-bound.

- **Custom capabilities can now cross the wasip2 Component Model boundary.** When a program calls a capability whose complete contract uses only `Unit`, `Bool`, `Float`, and `String`, `aver compile --target wasip2` emits a deterministic typed WIT interface, canonical-ABI core imports, and Aver GC↔canonical value glue. The interface identity pins the full `contract_hash`; its docs carry both contract and model hashes; operation order and positional parameter names keep emitted WIT stable across declaration reordering and source parameter renames. One used operation imports the full contract, while an entirely unused capability emits nothing.

  This makes the artifact **host-bound**, not provided: a Component Model host must supply the implementation. A separate wasmtime linker can install the interface and execute both pure and effectful calls end to end; plain `aver run --wasip2` detects the missing custom binding before linking and reports `error[capability-provider-missing]`. There is no implicit package download or composition—the explicit `--providers` adapter described above is the opt-in local-host route. `Int` is deliberately excluded because Aver integers are arbitrary precision; `Result`, collections, represented types, and capability resources likewise fail the whole contract with an exact `wit-boundary-type-unsupported` path. `aver capabilities` reports the new `host-bound[component-import-required]` state deterministically, the native Rust row reports `host-bound[runtime-provider-required]`, and bare wasm-gc retains its explicit unsupported reason.

- **`Bits` — a bit-level view of `Int`.** `Bits.and`, `Bits.or`, `Bits.xor` and `Bits.not` are `Int -> Int`; `Bits.shiftLeft(x, n)`, `Bits.shiftRight(x, n)` and `Bits.low(x, width)` return `Result<Int, String>`. Bit-level algorithms — CRCs, checksums, Bech32, bit-packed wire formats — no longer have to be rebuilt out of `Int.div` and `Int.mod` one bit at a time.

  `Bits` is a **namespace, not a type**. Its arguments and results are ordinary mathematical `Int` values; the namespace only says how to *read* them for the duration of one call, as an infinite two's-complement bit sequence (a non-negative integer has infinitely many leading zeroes, a negative one infinitely many leading ones). So `Bits.not(x) == -x - 1`, `Bits.and(-1, x) == x`, `Bits.or(-1, x) == -1`. There is no `Word32`, no `Word64`, no machine word and no wraparound: `Bits.shiftLeft(1, 100)` is `1267650600228229401496703205376`, exactly, on every backend.

  For a non-negative count, `Bits.shiftLeft(x, n)` is `x * 2^n`, `Bits.shiftRight(x, n)` is `floor(x / 2^n)` — an **arithmetic** shift, so `Bits.shiftRight(-3, 1) == -2` — and `Bits.low(x, width)` is `x mod 2^width`, the non-negative value of the lowest `width` bits (`Bits.low(-1, 8) == 255`, `Bits.low(x, 0) == 0`). Fixed width is always requested explicitly through `Bits.low` rather than implied by a mask, so `Bits.shiftLeft(Bits.low(checksum, 25), 5)` states the protocol invariant instead of hiding it in `Bits.and(checksum, 33554431)`.

  A negative shift count or width is `Result.Err`, never a panic and never a silent direction flip. The 16,777,216-bit materialization bound applies where output can grow: `shiftLeft`, and `low` for a negative input. It does not apply to shrinking `shiftRight`, which reaches `0`/`-1` without count-sized allocation, or to positive `low` once the requested width reaches the input's existing bit length. As with `Int.div` and `Int.mod`, a syntactic bounded non-negative literal discharges `shiftLeft`/`low`; any non-negative literal discharges `shiftRight`. Dynamic counts keep `Result<Int, String>` because they may be negative.

  No bitwise operators were added — the named API is deliberate, the same choice `/` and `%` already made. The compiler now says so: see below.

- **Every rejected operator now names the function that replaces it.** Writing `a ^ b` reported `Unknown character: '^'`, which tells you nothing about `Bits.xor` — so a deliberate design choice read as a gap. `^`, `&`, `&&`, `|`, `||`, `~`, `%`, `<<` and `>>` now each report what they are and what to use instead, carrying the `rejected-operator` slug and a repair that spells out the call. `/` on `Int` already did this and is unchanged. Nested generics (`Map<String, List<Int>>`) and ordinary comparisons are unaffected — the shift diagnostic only fires on two adjacent `<` or `>` in operator position, which a type annotation never reaches.

- **Capability contracts now participate in the language, proof system, and replay.** A capability is declared as a module with `kind = capability` and a mandatory homogeneous `semantics = pure` or `semantics = effectful`; there is no second `capability Foo` syntax and no namespace compatibility mode. Its `operation` items are provider-bound signatures with no Aver body, while `resource Name` declares a representation-less value that only a provider can mint. This is distinct from `exposes opaque [T]`, which hides an ordinary represented Aver type. Pure operations are modeled as total deterministic opaque functions. Effectful operations have their own effect identity and must declare an Oracle dimension plus compatible replay behavior; program-defined `snapshot` claims are refused.

  The entry module and its full dependency closure are registered before typechecking, verification, or proof export. `given`, declared hostile profiles, trace verification, effect lifting, and direct `!`/`?!` calls all share the same operation classification and VM dispatch. Operations are deliberately not values and cannot cross the boundary as callbacks. `recorded` and `suppressed` replay consume a trace without a provider; `reissued` requires a live one. A missing VM or generated-Rust binding fails before the first operation; generated Rust accepts the native binding described above, bare wasm-gc still fails closed, and the wasip2 subset described above emits a host-bound WIT import. An unused contract does not block compilation.

  Every module gets a canonical `contract_hash` over its provider ABI and reachable boundary types, plus a `model_hash` over that contract, Oracle/replay declarations, and the transitive source closure of hostile profiles. Lean and Dafny trust headers pin both and state that provider implementations are outside the theorem. Capability resources — including represented wrappers that transitively contain one — have no equality or map-key semantics, so provider token identity cannot leak into the language model. Provider selection is deliberately absent from the hashes, leaving a clean future binding seam for in-process hosts, IPC, or WIT/Component Model adapters.

  Rust embedders can now satisfy that seam with typed in-process `ProviderBinding`s in either the VM or generated Rust. The public `aver-rt::provider` boundary is a closed first-order value tree rather than `Value`/`NanValue`; binding registration pins the full operation set and exact contract hash, catches provider faults and panics, validates return shapes, and carries provider-owned capability resources without serializing their payloads. One shared registry/resource store reaches direct calls and every independent-product branch. Replay recordings add sorted contract/model/provider/fingerprint provenance, enforce live pure/reissued implementation identity, and reject old custom-capability traces that have no semantic hashes.

  `Time.now`, `Time.unixMs`, and `Time.sleep` are the standard end-to-end canary. Their contract and hostile profiles now live once in `stdlib/capabilities/time.av`; the old handwritten VM signature/classification/service path is gone. The VM and generated Rust share the `aver-rt` adapter, while wasm-gc and wasip2 register their existing host/WASI implementations as target bindings of the same exact contract. `aver check` and artifact compilation report those four bindings with contract/model identities, and removing the native binding now makes `Time` fail instead of falling through a legacy builtin.

  `Random.int` and `Random.float` now exercise the same architecture as a second standard capability rather than a second set of exceptions. Their contract and hostile profiles live in `stdlib/capabilities/random.av`; the handwritten checker, classification, hostile-stub, service, and VM builtin paths are removed. A contract-checked native provider serves VM and generated Rust, the existing wasm-gc/WASI lowerings are registered as target bindings, record/replay carries Random provenance, and removing the binding fails closed instead of reaching legacy random dispatch. Standard provider installation, target accounting, generated-Rust defaults, and old builtin-session compatibility are now multi-capability mechanisms rather than `Time`-only branches.

  `Disk` makes the first policy-gated standard service to move onto that provider architecture. Its thirteen operations and thirty-two hostile profiles now live in `stdlib/capabilities/disk.av`; the handwritten checker, classification, hostile-stub, service, VM builtin, and generated-Rust direct-call paths are removed. The contract adds byte-exact `readBytes`, `writeBytes`, and `appendBytes`, a bounded positional `readBytesAt`, and metadata-only `size`. `readBytesAt(path, offset, length)` is one race-free effect that returns at most `length` octets; EOF is a successful short read, including empty beyond EOF, while negative arguments are errors. Whole-file `readBytes` remains separate so it does not need a racy `size`-then-read sequence.

  VM and generated Rust share the exact-contract `aver-rt` provider, wasm-gc uses the corresponding host imports, and wasip2 binds the same operations directly to WASI filesystem streams and metadata. Binary values cross the provider and replay boundaries as canonical octets rather than UTF-8 strings, including `0x80` through `0xff`. Record/replay carries Disk provenance and can replay binary reads offline, removing the native binding fails closed, and native path policy applies uniformly to text, binary, metadata, and directory operations before provider invocation.

  `Tcp` now follows the same architecture. Its nine operations, Oracle model,
  replay declarations, and hostile profiles live in
  `stdlib/capabilities/tcp.av`; VM and generated Rust share the exact-contract
  `aver-rt` provider, while wasm-gc and wasip2 register their existing network
  lowerings against the same contract. `Tcp.Connection` is now a genuine
  provider-owned resource rather than a constructable runtime record,
  and successful hostile `Tcp.connect` worlds receive a fresh token from the
  Oracle boundary instead of forging an id. The handwritten checker,
  classification, hostile-profile, VM service, and generated-Rust direct-call
  paths are removed, so uninstalling the Tcp binding fails closed instead of
  falling through a legacy builtin.

  Persistent Tcp sessions no longer inherit the old hard-coded 30-second
  socket timeout. Their reads and writes may wait indefinitely, and any actual
  I/O error after an operation begins poisons the connection handle so a
  partial frame or write cannot be mistaken for a clean retry. One-shot
  `send`/`sendBytes` retain a configurable request idle timeout, while
  connection establishment retains its own configurable timeout under
  `[effects.Tcp]`. VM, generated Rust, and the in-process wasm-gc host pass
  those settings into the standard provider; wasip2 warns when its host-owned
  socket timing cannot honour an explicit value. Effect sections now reject
  unknown or misplaced keys instead of silently dropping typos.

  Target binding accounting is now total rather than a positive-only list. `aver capabilities FILE` (or `--json`) emits one deterministic row for every loaded contract across VM, Rust, wasm-gc, and wasip2, separating `provided`, VM `host-bound`, and explicit `unsupported(reason)` states. The full declared operation set and the program-required subset are both present, so an unused capability remains visible without becoming a compile requirement. Generated Rust, wasm-gc, wasip2, run shortcuts, replay provenance, and `aver check` project from this same table. A missing VM installation keeps `error[capability-provider-missing]`; an artifact target with no adapter now reports `error[capability-target-unsupported]` with a stable mechanism-specific reason plus contract/model identities. This table is the direct input for later contract-to-WIT and Component Model composition.

- **Strings and nominal bytes now have explicit UTF-8 conversion.** `String.toUtf8 : String -> Bytes` encodes every string losslessly, while `String.fromUtf8 : Bytes -> Result<String, String>` validates input and reports invalid UTF-8 instead of replacing or dropping bytes. Both operations have matching VM, Rust, wasm-gc, Lean, and Dafny semantics.

### Changed

- **`HttpServer` has changed ownership from the host to ordinary Aver.** The callback-backed service implementation is removed, including `HttpServer.listenWith`, the `SelfHostRuntime` aliases, the `aver-rt` server loop, VM re-entry plumbing, replay special cases, and wasip2 no-op lowering. The namespace name survives because the concept survives; it now resolves through `depends [HttpServer]` to the standard Aver module described above. This is source-breaking even where `HttpServer.listen(port, handler)` keeps the same spelling: the program now owns the loop and must declare its concrete `Tcp.*` and `Process.stopRequested` effects.

  | Before | Now | Migration |
  |---|---|---|
  | `HttpServer.listen(port, handler)` | `HttpServer.listen(port, handler)` | Add `depends [HttpServer]`; replace the `HttpServer` effect with the exact `Tcp.*` and `Process.stopRequested` effects reported by `aver check`. |
  | `HttpServer.listenWith(port, context, handler)` | unary `HttpServer.listen(port, handler)` | Keep request handling explicit: acquire request-scoped dependencies in the unary handler, or own a specialised Aver loop when persistent application state is required. There is no hidden callback context. |
  | A native-style `main` retained for proxy builds | `--handler <fn>` only | Remove the listener call from fetch/proxy source. Cloudflare and `wasi:http/proxy` hosts already own the listener and invoke the named handler directly. |
  | `HttpRequest { method, path, body, headers }` | `HttpRequest { method, path, query, body, headers }` | Read the query without the leading `?` from `req.query`; `req.path` remains query-free on every host. |

- **`Tcp.poll` now takes the socket state explicitly.** This is source-breaking but removes the runtime invariant that separate connection, dial, and listener maps must have disjoint, collectively complete keys.

  | Before | Now | Migration |
  |---|---|---|
  | `Tcp.poll(connections: Map<Int, Tcp.Connection>, timeoutMs)` | `Tcp.poll(sockets: Map<Int, Tcp.Socket>, timeoutMs)` | Wrap each established peer as `Tcp.Socket.Connected(connection)`. Add listeners and in-flight attempts to the same map as `Listening` and `Dialing`. |
  | Blocking-only outbound establishment | `Tcp.beginConnect(host, port)` + `Tcp.dialled(dial)` | Keep `Tcp.connect` when blocking convenience is desired; use `Dialing` in the shared map when one loop must stay responsive. |
  | No native TCP listener capability | `Tcp.listen`, `Tcp.accept`, `Tcp.peerAddress`, `Tcp.closeListener` | Store the listener as `Tcp.Socket.Listening`; after readiness, drain `accept` until `Ok(None)`. |
  | Connection-shaped in-flight state | `Tcp.Dial` promoted to `Tcp.Connection` | Replace hand-maintained `dialing: List<Int>` flags with an exhaustive match on `Tcp.Socket`. The connected variant is named `Connected`, not `Open`, because a listener is also an open socket. |

- **Standard-library names and partiality contracts now say who owns a conversion and where failure is catchable.** This is a source-breaking cleanup; the migration table is the complete user-facing map:

  | Before | Now | Migration |
  |---|---|---|
  | `Char.toCode(text) -> Int` | `String.firstCodePoint(text) -> Option<Int>` | Match `Option.None` for empty text. |
  | `Char.fromCode(n)` | `String.fromCodePoint(n)` | Namespace-only rename; the `Option<String>` result is unchanged. |
  | `Option.toResult(option, error)` | `Result.fromOption(option, error)` | Namespace-only rename. |
  | `Bytes.toList(bytes)` | `Bytes.octets(bytes)` | Namespace-local rename clarifying that the exposed integers are validated octets. |
  | `Crypto.Digest32.toBytes(digest)` | `Crypto.Digest32.bytes(digest)` | Namespace-local rename; only the length refinement is forgotten. |
  | `Vector.new(size, fill) -> Vector<T>` | `Result<Vector<T>, String>` for a dynamic size | Add `?`/`match`; a literal in `0..=u32::MAX` still types directly as `Vector<T>`. |
  | `Random.int(min, max) -> Int` | `Result<Int, String>` for dynamic bounds | Add `?`/`match`; valid literal i64 bounds still type directly as `Int`. |
  | `Time.sleep(ms) -> Unit` | `Result<Unit, String>` for a dynamic duration | Add `?`/`match`; a valid non-negative i64 literal still types directly as `Unit`. |
  | `BranchPath.child(path, index) -> BranchPath` | `Result<BranchPath, String>` for a dynamic index | Add `?`/`match`; a non-negative literal index still types directly as `BranchPath`. |
  | `BranchPath.parse(text) -> BranchPath` | `Result<BranchPath, String>` for dynamic text | Add `?`/`match`; a syntactically valid path literal still types directly as `BranchPath`. |
  | `Terminal.size() -> Terminal.Size` | `Result<Terminal.Size, String>` | Add `?`/`match`. |
  | Terminal control/output calls returning `Unit` | `Result<Unit, String>` | Add `?`/`match` to `enableRawMode`, `disableRawMode`, `clear`, `print`, `setColor`, `resetColor`, `hideCursor`, `showCursor`, and `flush`; `moveTo` has the same result shape. |
  | `Terminal.readKey() -> Option<String>` | `Result<Option<String>, String>` | First handle/propagate host input failure, then match key presence. |
  | `Env.set(...) -> Unit` | `Result<Unit, String>` | Add `?`/`match` for invalid host keys/values or write failure. |

  Literal discharge removes only ceremony. The compiler now uses a private fail-closed unwrap for proven `Random.int` and `Time.sleep` calls; if a provider or Oracle stub returns `Err` after the arguments were proven valid, execution faults as a provider-contract violation. It never uses `Result.withDefault`, so a failed random request cannot silently become `min`, `0`, or another plausible sample. The public `Result.withDefault` function remains available for code that intentionally chooses fallback semantics.

  Terminal operations are one consistent fallible family because they share the same host adapter failure boundary. Their outcomes are recorded, as is `Time.sleep`; recordings made with the old suppressed/unit shapes are not replay-compatible with these contracts.

- **`Bits` materialization limits now follow actual allocation.** `Bits.shiftRight` rejects only a negative count: an arbitrarily large non-negative count returns the arithmetic sign tail (`0` or `-1`) without constructing `2^count`. `Bits.low` applies the 16,777,216-bit cap only to negative inputs; for non-negative input, a width at or above the existing bit length returns the input directly. `shiftLeft` retains the cap because its result really grows. Accordingly, any non-negative literal discharges `shiftRight`, while `shiftLeft` and `low` retain bounded literal discharge. The numeric boundary and its public refusal messages now originate in `aver-rt`; parser guidance and the Rust, wasm-gc, Lean, and Dafny emitters derive from that source, with a structural test rejecting duplicated decimal limits.

- **The wasm-gc boxed sequence fallback is tested without a process-global environment switch.** `AVER_NO_PACKED_SEQUENCES` has been removed. Representation-differential tests pass an internal emitter option directly, including an explicit multibyte `String.toUtf8`/`String.fromUtf8` round trip through boxed `Bytes`; production compilation always uses proof-selected packing when eligible.

- **`aver audit` walks the whole program, like `check` and `verify`.** `aver audit main.av` now audits every module the entry reaches through `depends [...]`, leaves first, one `Audit:` section per module, running each module's verify blocks and format check; a directory is the union of the programs rooted at its files, each module audited once, so a dependency outside the directory is audited too and a module two entries share is audited once. The summary counts modules (`Audit: N modules | …`); in `--json` the summary's `files` now counts the inputs and the new `modules` field carries the old per-file meaning. Provider bindings from `aver.toml` apply to every module of that program. What starts failing: `aver audit main.av` on a project whose dependency modules carry failing cases, errors, or unformatted source. The unused-exposes warning now names its scope — `exposes not used by the checked program(s): …`, under its documented `unused-expose` slug — since `aver check entry.av` judges only the checked program and a directory judges every program in it.
- **A file names a program, and every command walks it.** A program is the entry module plus everything it reaches through `depends [...]`, plus the `[providers]` bindings of the project's `aver.toml`. `aver check` and `aver verify` now report every module of that program, leaves first, one section per module, and a failure in any module fails the command; a directory is the union of the programs rooted at its files, each module reported once. Summaries count modules (`Summary: N modules | …`, `Checked N module(s)`); in the JSON summaries `files` now counts the inputs while the new `modules` field carries the old per-file meaning, and check's `passed`/`failed` now count modules. The `--deps` flag is gone from both commands, and there is no `--no-deps`: pointing at a single module is still the fast way to iterate on it. The `--providers` flag is gone from `run`, `verify`, and `audit`: a project that binds providers in `aver.toml` gets the cached Rust host built once and reused whenever a program reaches a bound capability, and the first build names every package it links and where it comes from; a project without `[providers]` never invokes Cargo. `--wasm-gc` and `--self-host` have no provider host and now refuse such a program with `error[capability-provider-unhosted]` instead of failing at runtime. Embedded standard modules are part of the program for typing and codegen but are not units of the report: their own verify blocks are checked per release, not re-sampled by every project. The "non-law verify block(s) … are NOT checked" warning is gone from `run`, `compile`, and `verify`; `aver proof` still prints one line per program, since its export does not sample dependency cases yet, and names the `aver verify` invocation that does. What starts failing: `aver verify main.av` on a project whose dependency modules carry failing cases, and `aver check main.av` on a project whose dependency modules carry errors. On a large project `aver verify main.av` now runs every case of every module rather than the entry's few (issue #857).

- **Proof-bounded byte sequences now stay compact in generated Rust.** The native backend consumes the same generic packed-sequence proof facts as wasm-gc: a one-field `List<Int>` refinement whose smart constructor proves every element lies in `0..=255`, and whose whole-program construction audit finds no bypass, stores an immutable `Arc<Vec<u8>>` slice view instead of one `AverInt` per octet. This is not keyed to `Bytes`; an unrelated refinement with the same proven shape earns the same layout, while an ungated constructor fails closed to the ordinary representation. The semantic `List<Int>` boundary is a packed-or-wide hybrid that widens rather than truncates when an operation introduces `-1`, `256`, or any larger integer; matching, equality, ordering, hashing, providers, replay, vectors, maps, and independent products retain their source semantics. `Crypto.sha256` and canonical byte providers consume the packed slice directly. On Robin Owens's [`btc-listener`](https://github.com/n1bor/btc-listener) parsing Bitcoin Core block 413567 (999,887 bytes, 1,557 transactions; release builds, median of five 20-block runs on an M2 Pro), parse time fell from 138.65 to 113.50 ms/block (18.1%) and peak RSS from 26.71 to 9.96 MB (62.7%); the full check path fell from 144.60 to 117.85 ms/block (18.5%) and 29.10 to 10.34 MB (64.5%). Inspired by and measured against the workload built by [@n1bor](https://github.com/n1bor).

- **Qualified project types now retain their module identity in generated Rust signatures.** A source type such as `Worker.Shape` inside `Fn(Worker.Shape) -> Int` emits `crate::aver_generated::worker::Shape` instead of the nonexistent flat name `Worker_Shape`. The scoped renderer follows the resolved `TypeId` / symbol-table owner through ordinary functions, type declarations, self- and mutual-TCO wrappers, and capability returns, while compiler-owned dotted resources such as `Tcp.Connection` keep their established flat aliases. This fixes the generated-Cargo E0425 failure from issue #1033.

- **Generated Rust now preserves the source `Fn` ABI when passing named functions.** A higher-order parameter such as `Fn(Shape) -> Int` keeps its canonical value-taking pointer type even though ordinary generated functions internally borrow non-scalar parameters. First-class references to those functions now use non-capturing adapters that lend only the borrow-by-default positions, while scalar and owned/TCO signatures retain the direct function-item path. This fixes the E0308 build failure from issue #952 without weakening the normal call-path borrowing optimization.

- **An unbounded fuel fallback can no longer certify a sampled Lean claim.** Some mutual-recursion groups cannot expose a decreasing measure because a peer receives an opaque computed successor such as `next = pairsOf(level, [])`. Their executable `__fuel` seed is not a proof that evaluation cannot reach zero. Under the default automatic verify mode, a `verify` case or law whose transitive call cone reaches such a helper is now explicitly declined instead of being closed with `native_decide`. Lean's `panic!` returns the result type's default value, so two exhausted computations — or one exhausted computation whose expected value is itself the default — could previously evaluate equal and produce a false theorem. The refusal is embedded as a comment, reported through `declined_claims`, and charged by `aver proof --check`; its diagnostic names the functions and says that the seed is not a proven recursion bound. Explicit `sorry` and theorem-skeleton modes still emit their visibly unproven obligations. Fuel with a justified bound (Int countdown, string position, ranked structural recursion), functions that graduate to a real `termination_by`, and ordinary non-recursive sampled claims keep their existing proof paths.

- **Returning a growing `Map` from a helper no longer turns generated Rust quadratic.** The MIR ownership pass now summarizes which named-function parameters may back its result, so a linearly threaded `Map` keeps its owned move across ordinary helper returns instead of forcing a full copy on every `Map.set` or `Map.remove`. The Rust backend extends that proof through consumed `Result` wrappers without applying the rule to arena backends, whose wrapper entries remain live holders. A returned record successor can likewise move the `Map` field it replaces, so `Result<Store, E>` is linear when `Store` carries the accumulator. Unknown first-class functions and results that may return any of several live aliases remain on the conservative COW path.

- **Walking a `List.concat` rope no longer pays for its construction topology at every element.** The first destructuring step now iteratively compiles the rope into a shared traversal index whose entries are constant-time flat ranges or individual prepend heads; every returned tail advances a small cursor over the same index. `concat` itself remains O(1), persistent, and sharing, values are never flattened or copied, and an indexed tail can be concatenated again without expanding it element by element. On the pinned 8,200-element runtime benchmark, the right-nested walk falls from about 1.16 ms to 37 µs and the left-nested walk from 122 ms to 39 µs, both close to the 33 µs flat body. The old construction `Segments` used by append-heavy lists remains separate and retains its Drop and sharing behavior.

- **Generated Rust now gives every independent-product thread its own captured values.** A non-`Copy` value read by two branches of `!` or `?!` was cloned inside the first closure and moved into the second, but constructing the first `move` closure had already consumed the original; `aver check` and `aver compile` passed while Cargo rejected the generated project with E0382. The MIR emitter now identifies each branch's free locals and creates branch-scoped owned captures before spawning, preserving borrowed and native-`Copy` values without extra allocation. Recursive fan-out may share `Int`, `String`, records, and other context across branches; ordinary and replay-enabled generated Rust both build and remain behaviorally identical to the VM.

- **A proof export no longer claims a termination measure for a step it cannot see shrink.** When two functions call each other through a value one of them computed — `next = pairsOf(level, [])?` handed to a peer — the export stated the measure as the size of that value and left Lean an obligation nothing could close: whether `next` is smaller than `level` is a fact about `pairsOf`, not about the shape of the call. The module then failed to build and lost every claim in it, including claims with nothing to do with the recursion. Such a group now falls through to the strategy that fits it: a computed value produced by a non-growing filter still gets a real well-founded measure, with the shrinking lemma synthesised and proved by the Lean kernel, and anything else lowers with fuel. Nothing that had a genuine measure loses it. Measured against an outside project, 6 more modules build — mostly because one dependency that used to fail took every module importing it down with it.

- **A proof export no longer substitutes a default value on the expected side of a `verify` case.** A case written `takeFrom(...)?.payload => Bytes.fromHex("aabb")?` stated its left side as an action that propagates the error, and then stated the right side as `(Bytes.fromHex "aabb").withDefault default` — a value the model cannot stand behind. Where the type has no default at all, which a refined type such as `Bytes` deliberately does not, nothing in the module built and every claim in the file was lost. Where a default did exist, a case that fails at run time could still be true as a theorem whenever the expected value happened to equal it. Both sides now run in the same monad, so a `?` on the expected side is bound the way the left side binds one, and whichever side is a plain value is lifted to meet the other.

  In the same spirit, a record or sum carrying a refined type no longer claims an `Inhabited` instance it has no witness for. A refined type is a carrier plus a proof, and the model deliberately states no default for one — so a record with a `Bytes` field simply has no default either, rather than a derive that fails and takes the module's claims down with it. Measured against an outside project, this takes the missing-`Inhabited` failure out of that project entirely: 13 modules stop failing on it, and 4 of them go all the way to a clean build.

- **A proof export no longer breaks on a type that carries a capability resource.** A record or sum with a field such as `Database(Infra.Kv.Handle)` exported a model in which the handle's type said nothing at all, so nothing that carried one could be compared, printed or decided — and since the type would not build, every claim in its module went with it, including the ones about arms carrying no handle. A handle is now modelled as an identity and nothing else: two handles are the same exactly when the host says they are, which is what your program's runtime already compares. Deliberately no default value — a handle the runtime never produced is not something the model should be able to invent — so a type reachable only through a handle simply has no default rather than a fabricated one, and its module keeps building either way. The language is unchanged: no Aver source can build a handle, read its identity, compare two, or use one as a map key. Measured against an outside project, this took one module from 22 build errors to 7.

- **A proof export now states why a group of mutually recursive functions terminates, for any size of group.** The termination measure of a recursion group was chosen one function at a time, and a group of three where each member descends on a *different* list — a byte-by-byte lexicographic comparison, met on the first external project — had no measure that held on every call: the step that hands a tail on unchanged compared the wrong two lists, the termination checker answered with a counterexample, and every module importing that file failed before its own claims were even looked at. The export now reads every call between the members, argument position by argument position, and counts exactly what travels around the cycle: a parameter is measured only when every call into it hands it a measured parameter of the caller or a strictly smaller part of one (the head or tail of a matched list, a field of a matched constructor, a guarded `n - 1`), so the measure never grows along a call and shrinks on every call that consumes something; calls that leave it unchanged are ordered. An `Int` that a guard counts down joins the measure where the lists alone leave a cycle with no decrease; what a call passes for a parameter the measure does not count — a list grown by concatenation, say — is not looked at. Two parts of one parameter count together only when one match binds them side by side: a function that matches the same list twice and hands both tails on counts one of them, where counting both stated a measure the checker refuted. Groups whose measure the export already stated and proved keep it, byte for byte. A group with no such measure — one call hands on a list that grew and nothing counts down, or a helper is handed the same tail under two names — is not stated at all: it lowers with fuel, and since a fuel seed that counts its lists once bounds nothing about such a recursion, its claims are declined, with the refusal naming the call the export could not see shrink, rather than evaluated until the seed runs out and a true claim reports a panic. One shape is still outside this: a type whose recursion sits in a tuple field of a constructor (`Node(Tuple<T, Int>)`) is counted short by its own measure, so a cycle descending through it can still run its fuel out; that is unchanged. A group whose measure counts an `Int` down but which the export still lowers with fuel (its measure also sums two recursive types) has its claims declined too, with that reason, and so does one in which a call hands a value it computed into a position the seed counts, with that call named. The Dafny export keeps measuring a group by the length of its sequence parameters, takes an ordering of the members only when it was chosen for that measure, and does not take the head of a matched list for a shorter list.

- **A proof export no longer breaks on a list operation applied to the result of another.** `List.take(List.drop(xs, 1), 32)` emitted `(xs).drop (Int.toNat 1).take (Int.toNat 32)`: the inner operation begins with a bracket, the emitter took that for self-contained, and Lean attached `.take` to the inner operation's last argument (`Invalid field take: The environment does not contain Nat.take`). A receiver that is an application is now parenthesised; receivers that are one bracketed group, possibly followed by field projections, are emitted as before.

- **A proof export no longer breaks on a pair of functions that call each other.** When two functions in a recursion group each take a list, the exported termination measure counted every list they took — including one that is only ever returned, never handed on. The step from the forwarding function to its peer then decreased only if that returned list was non-empty: true of any list, but not something the termination checker has a reason to believe, so the whole group failed to build and every claim in the module was lost. The measure now counts what actually travels between the two, which also lets the export see when a step leaves the measure unchanged and order the pair accordingly instead of assuming it shrinks.

- **A proof export no longer breaks on a list operation used as an argument.** `Result.Ok(List.take(xs, 4))` — and the same for `List.drop`, `List.contains`, `List.zip`, `List.find`, `List.any`, `String.replace`, `String.startsWith` and their neighbours — emitted Lean that would not elaborate whenever the receiver was anything more than a bare name. The emitter decided a subexpression was already self-contained by looking at whether it *began* with a bracket, and `(doubled xs).take (Int.toNat 4)` begins with one without being self-contained, so it went out unwrapped and Lean read it as one function applied to two arguments. `aver verify` passed the same program; only the exported proof failed to build. Measured against an outside project, this single shape accounted for 86% of the claims whose emitted Lean would not elaborate, and fixing it took that project from 9 provable modules to 21.

- **A missing comma in an effect list is now a parse error.** `! [Console.error Console.print]` was read as two effects and accepted in silence — the program was correct, so nothing downstream had a reason to complain, and `aver format` reprinted the line as written. A comma dropped by a scripted edit in a wide diff passed check, verify, format and compile, and the only way to find it was to read the diff. The separator is now required wherever an effect list appears — a module's `effects [...]`, a function's `! [...]`, and an `Fn(...) -> T ! [...]` type — and the error names the effect it stopped after. Lists written across several lines and a trailing comma are unaffected.

- **A map iterates the same way whichever backend runs it, whatever it is keyed on.** `Map.keys`, `Map.values` and `Map.entries` return entries sorted by key — but only `Int`, `String` and `Bool` keys were actually compared by value. Everything else was ordered by its PRINTED form when you ran a program, and by the value itself once compiled, so the same source gave two answers: a map keyed on `(2, 1)`, `(10, 1)` and `(3, 1)` read `ten, two, three` under `aver run` and `two, three, ten` from the compiled binary. A composite key now has one canonical order, stated the same way everywhere: lists and `Bytes` lexicographically, tuples componentwise, records by their FIELD NAMES, variants by their CONSTRUCTOR NAME and then their payload.

  Records and variants order by name rather than by the order they were declared in, deliberately. Declaration order is not observable anywhere else — a record is built and read by name, there is no positional pattern — so ordering by it would have made swapping two fields, a change that means nothing, silently change how every map on that key iterates.

- **A map key type must be one that orders, and `Float` is not.** A NaN has no place in the finite range; a compiled binary could not even build such a map (`f64` is neither `Eq` nor `Hash`, so the generated Rust failed to compile after `aver check` had passed it); and the proof model has no faithful counterpart for the runtime's total order. `Float` in key position is now a type error, wherever the key is decided — an annotation, a map literal, the key argument of `Map.set`, the pair list handed to `Map.fromList`, a read of an empty map, a `verify` given, a capability operation, or a signature reached through a dependency. The rule reaches through your own types, so a record with a `Float` field cannot key a map either and the error names the field. A `Map` and a `Vector` are refused for the same reason: neither has an order of its own. Float stays legal as a map *value*, and every other type — including your own records and variants — keys a map as before.

- **`aver proof` reads the key type of the map a claim actually observes.** It collected key types from every signature the claim's cone mentioned, so one modelled key anywhere in reach opened the export for all of them: a function with an unused `Map<String, Int>` parameter that built a map with an unmodelled key in a local binding exported its case as a kernel-certified theorem while `aver verify` reported the same case failing. The refusal now follows the map being read. A claim about the order a map iterates in is still declined when the proof model has no ordering for that key — the program orders it the same way on every backend, the model just cannot state it yet — and that refusal is counted and named as before.

- **CSE diagnostics now respect mutually exclusive `match` arms.** Repeating the same pure builtin call or arithmetic expression in sibling arms no longer produces an impossible “extract to a binding” repair. Counts still add across sequential work and within one arm, while alternative arms contribute their maximum reachable count, so genuine duplicates remain visible.

- **Capability-backed modules no longer fall out of Lean proof export.** A plain `verify` case on a branch that never dispatches its declared capability effect is exported against the Oracle-lifted function and quantified over the unused oracle, so the generated theorem states that the concrete result is provider-independent. A case with an explicit `given` keeps using that concrete stub. Qualified dependency operations such as `Infra.Kv.get` are now lifted through the same path instead of surviving as unknown host identifiers in an otherwise empty capability namespace.

- **Verify stubs now reach capabilities below module namespaces by their canonical operation path.** `given probe: Sub.Probe.answer = [stub]` parses, type-checks, and installs the same `Sub.Probe.answer` identity used by calls, diagnostics, and provider dispatch. Dotted type annotations remain distinct and may also contain multiple capitalized namespace segments. A shortened or misspelled operation-shaped binding such as `Probe.answer` is rejected statically; when it uniquely matches a loaded canonical suffix, the diagnostic suggests `Sub.Probe.answer` instead of silently installing nothing and failing later with `capability-provider-missing`.

- **Cross-module capability ownership errors now point at the declaration that is actually invalid.** When an operation in a dependency uses a named type owned by another module, `aver check` retains that dependency's filename and source text instead of combining its line number with the consumer file. The diagnostic also identifies `parameter N` or `result` and suppresses repeated hits for the same named type within one position, so parameter and result violations are distinct rather than duplicated verbatim.

- **A name means one thing in its scope: shadowing is now a compile error.** A binder — a function parameter, a statement binding, or a match-pattern binding — may no longer reuse a name that is already visible at that point: an enclosing local, a top-level function or operation of the same module, or the enclosing function's own name. The error names both sides and says where the other one lives — `the pattern binding 'dbl' shadows the function 'dbl' defined at line 3; every name means one thing in its scope — rename one of them` — and the fix is that one rename. Sibling match arms are unaffected: two arms of the same `match` may still bind the same name, because neither is in the other's scope. Cross-module names are always written `Module.fn`, so nothing outside the file can collide. Binders inside `verify` blocks are left as they are for now. Statement-level rebinding in one scope was already an error; this closes the nested case, where the same spelling silently meant two different things — the shape behind a real three-way backend divergence. Every command that reads a program applies the rule, `aver repl` included: a REPL session is one scope, so a binder entered now may not spell a function defined in an earlier entry, a refused entry is not added to the session, and `:clear` starts a fresh scope.

- **`aver run --wasm-gc` and `aver run --wasip2` run allocation-heavy programs about four times faster.** The embedded WebAssembly engine is upgraded from wasmtime 44 to 46 and now uses its copying garbage collector — the same collector the standalone `wasmtime` CLI defaults to — instead of the reference-counting one. Decoding 1 MiB of hexadecimal with `Bytes.fromHex` drops from about 1.7 s to about 0.42 s, within a few percent of running the same emitted component under the external CLI.

- **Building a string out of a list loop is recognised in more shapes, and `aver run` optimises it the same way `aver compile` does.** When a program builds up a list of pieces with a tail-recursive loop and hands the result to `String.join`, Aver skips the list entirely and writes the pieces straight into the finished string. Two spellings of that loop were recognised. The one most Aver code actually writes was not — the loop that walks the input list and reverses in its own empty-list case:

  ```aver
  fn parts(values: List<Int>, acc: List<String>) -> List<String>
      match values
          [] -> List.reverse(acc)
          [head, ..tail] -> parts(tail, List.prepend(render(head), acc))
  ```

  Aver's own `Bytes.toHex` is written exactly like that, so the standard library was missing an optimisation the standard library ships. It no longer is, and neither is your code.

  The other half is *where* it applies. A loop living in a module you `depends` on was optimised when you ran `aver compile` and left alone when you ran `aver run` — the same source, two different programs, and the only visible difference was speed. `aver run` now applies it too, so what you measure on the VM is what the compiled binary does. `--target wasm-gc` and `--target wasip2` are unchanged: that backend has nothing to write the pieces into, and the loop stays a list there.

  A loop is only rewritten when its shape says the rewrite cannot change the answer. Anything else — an accumulator that starts non-empty, a reverse in a place that would then happen twice, a base case returning something other than the accumulator — is left exactly as written, silently and safely. The same goes for a loop that *reads* what it has collected so far: once the pieces go straight into the string there is no list left to ask, so a loop that calls `List.contains(acc, head)` to mark repeats, or stops on `List.len(acc)`, keeps its list and its answer. `aver compile --explain-passes` is how you check which side of that line your loop landed on.

- **Walking a string with `String.chars` no longer builds the list, and deciding a character no longer compares strings.** `String.chars(text)` builds one one-character string per character *and* a cons cell per character, and the loop that consumes it takes them apart again immediately. When the list goes straight into a recursive function that only destructures it, Aver now walks `text` itself — the loop keeps its shape, the list is gone:

  ```aver
  fn digitSum(characters: List<String>, acc: Int) -> Int
      match characters
          [] -> acc
          [head, ..tail] -> digitSum(tail, acc + digitValue(head))

  fn total(text: String) -> Int
      digitSum(String.chars(text), 0)
  ```

  The other half is the function that loop calls. A `match` whose arms are single-character literals compares strings once per arm — sixteen of them for a hexadecimal digit, behind a `String.toLower` that builds a string of its own. Aver now reads the character's codepoint once and compares numbers. Both halves are the standard library's own: `Bytes.fromHex` is written exactly this way, so decoding hexadecimal — and anything built on it, a checksum or a 32-byte digest — is roughly twice as fast for the same source, on the VM and in compiled Rust alike. As with the string-building rewrite, `--target wasm-gc` and `--target wasip2` are unchanged.

  The walk is by *character*, not by byte, because `String.chars` is: `"éx"` is two characters however many bytes it takes, so a loop that checks for an even count still gets the answer it always got. Case folding keeps whatever arm `String.toLower` would have chosen, including the characters where that is surprising — `U+212A KELVIN SIGN` lowercases to the ASCII `"k"` and still takes the `"k"` arm.

  And a loop is only rewritten when its shape says the rewrite cannot change the answer. A loop that measures the list it is walking, hands it to another function, or recurses on a *different* list that happens to reuse the tail's name keeps its list and its answer, as does a character match with an arm that binds the character or a literal that is more than one character long. Names count as shape: the rewrite gives the loop a `<loop>__cursor` twin and a couple of `__cur_`-prefixed locals, so a program that already spells either of those — a function, a parameter, a binding, a pattern — keeps its list rather than let the two meanings collide. `aver compile --explain-passes` now reports which loops were rewritten, which were not, and why.

- **A loop that collects a list no longer builds it backwards and turns it around.** The standard way to build a list in Aver is to prepend onto an accumulator and reverse once on the way out. That costs a cons cell per element and then a second walk over all of them. Aver now appends into the answer directly — same source, no cons cells, no reversal:

  ```aver
  fn doubled(values: List<Int>, acc: List<Int>) -> List<Int>
      match values
          [] -> List.reverse(acc)
          [head, ..tail] -> doubled(tail, List.prepend(head * 2, acc))
  ```

  The other spelling works too: a loop that hands its accumulator back bare and leaves the reversing to its caller — `List.reverse(collectInto(values, []))`, the `*Into` convention — is rewritten at the caller, which stops reversing. A caller that wanted the elements backwards and wrote the call without a reverse still gets them backwards.

  This composes with the two rewrites above, which is where it pays for itself. `Bytes.fromHex` reads a list of characters and writes a list of octets; the character list went away last release and the octet list goes away now, so what runs is a loop with a cursor on one side and a builder on the other. Measured for this note in one fresh session — whole program, wall clock, minimum of nine runs — decoding one megabyte of hexadecimal text takes 39 ms in compiled Rust and 300 ms on the VM, and a list-building loop on its own — a million elements, nothing else in the way — takes 15 ms compiled. Before this run of work the same decode measured about 200 ms compiled and 580 ms on the VM, and the bare loop about 120 ms compiled; those earlier figures come from a different measurement session, so read the comparison as approximate — about five times on the compiled decode, about twice on the VM, about eight times on the bare loop.

  As before, a loop is only rewritten when its shape says the rewrite cannot change the answer, and the accumulator is what the shape is about. It may be appended to in the recursive call and handed back at an exit, and read nowhere else: a loop that measures what it has collected to decide when to stop, or to decide what to collect, keeps its list and its answer. So does one whose exits disagree about the reverse, one that leaves the reversing to its caller but can exit with a value that never came from the accumulator, one that restarts itself with a fresh list, and one whose pattern re-binds the accumulator's name. Names count as shape here too — the rewrite gives the loop a `<loop>__collected` twin and calls three `__lst_`-prefixed builders, so a program that already spells any of those keeps its list. `aver compile --explain-passes` reports which loops were rewritten, which were not, and why. `--target wasm-gc` and `--target wasip2` are unchanged.

- **A character classifier now receives the character's code, not a one-character string.** The cursor walk above stopped building the list, but it still built a fresh one-character string per step, because the function it hands each character to — `Bytes.hexDigitValue`, or any classifier like it — takes a `String`. When such a classifier is exactly one match on the character's codepoint (which the codepoint rewrite above already made it), it now gets a twin that takes the code itself, and the loop reads the codepoint at the cursor instead of materialising a string. Nothing changes in your source, and the `String` version stays for every other caller. Error messages that print the offending character — `Bytes.fromHex: invalid hexadecimal character 'é'` — come out byte-for-byte the same, because a read the classifier cannot take re-reads the character at the position the loop was on, multibyte characters included. Case folding across the call keeps every arm the string route chose, checked exhaustively over all 1,114,112 Unicode scalar values.

  Measured for this note in one fresh session — whole program, wall clock, nine runs after a warmup, nothing subtracted, before/after built from the neighbouring commits: decoding one megabyte of hexadecimal with the untouched standard library went from 76 ms to 27 ms in compiled Rust (about 2.8×), and a bare classifier loop over two million characters from 55 ms to 14 ms (about 4×). On the VM the same programs improved only a few percent — 581 ms to 562 ms and 300 ms to 284 ms — the interpreter's time goes to dispatch, not to these allocations. The `Option` each decoded digit is wrapped in is still there; that is the next boundary, not this change. As with the rest of this family, `--target wasm-gc` and `--target wasip2` are unchanged, and `aver compile --explain-passes` reports the classifier variants and the calls that moved.

- **Recursive `String.charAt` dispatch no longer allocates an `Option` and one-character String per step.** The shared codepoint index already made repeated `charAt` and `slice` lookup constant-time; an indexed `Option.None` / `Option.Some(c)` match now reads the Unicode scalar directly when every use of `c` is character classification. Pure helpers receive generated `__code` variants, including multi-argument dispatchers and the common `Option.withDefault(String.firstCodePoint(c), 0)` spelling. Their compiler-owned scalar parameter stays a native `i64` across both indexed and existing cursor dispatch, and is boxed only if it reaches a genuine general-`Int` sink. A character that reaches output, interpolation, length, or any unrecognised consumer keeps the general indexed `Option<String>` path. The sentinel and UTF-8 decode are implemented on the VM, generated Rust, wasm-gc, and wasip2; `examples/data/json.av` moves 17 of its 26 indexed sites and seven helper boundaries onto the allocation-free route while preserving public codepoint positions. Closes [#958](https://github.com/jasisz/aver/issues/958).

- **A loop whose only reader is `Bytes.fromList` collects bytes directly, and the second walk over the list is gone.** `Bytes.fromList` takes a list of integers, checks every one against `0..=255`, and wraps the result — so a loop that collects octets for it built half a million list elements just to have them read once more and thrown away. When the collected list's only reader is the standard library's `fromList`, Aver now runs the range check on every element as it is collected and answers the `Result<Bytes, String>` directly: no intermediate list, no second walk. The answers are byte-for-byte what the pair always produced — the same bytes in the same order, the same error naming the first out-of-range value and its position, values wider than a machine word included, and a parse error in the loop still wins over a later range error because the loop stopped first.

  `Bytes.fromHex` is written exactly this way, so this closes the run of work above: one megabyte of hexadecimal now decodes with no intermediate structure at all — text to cursor to bytes. Measured for this note in one fresh session — whole program, wall clock, nine runs after a warmup, nothing subtracted, before/after interleaved from the neighbouring commits: that decode went from 15.1 ms to 8.0 ms in compiled Rust (about 1.9×) and from 268 ms to 196 ms on the VM (about 27% off — the walk it deletes was an interpreted recursion, so the VM finally gains real ground in this family). A bare loop collecting half a million octets straight into bytes went from 14.4 ms to 6.0 ms compiled (about 2.4×) and from 95 ms to 43 ms on the VM (about 2.2×).

  The rewrite fires only where it provably cannot change the answer: the `fromList` must be the standard library's own — a vendored copy that changed so much as a word of the message keeps its second walk — the collected result may be read nowhere else, the call must sit where the loop's answer is produced, and a loop with an exit whose list never came from the accumulator keeps its list, because `fromList` judges that list too. Names count as shape as before: the byte builder lives in a `__byt_` namespace, and a program that spells such a name keeps every list. `aver compile --explain-passes` reports which loops now collect bytes, which were turned down, and why. `--target wasm-gc` and `--target wasip2` are unchanged.

- **A proof can no longer describe code the compiler invented, and a certificate describes the program its binary was built from.** Some optimisations write code of their own: the string-building rewrite above replaces a loop with a buffer and a function that appears nowhere in your source, and `"a${x}b"` becomes a chain of calls to the same machinery. The Lean and Dafny exports read the program the compiler is holding, so keeping those inventions out of your theorems was a convention — every path that exports a proof had to remember to switch those passes off first, and one that forgot would have exported theorems about functions you never wrote. It is now structural: the compiler takes your program aside before the first pass that invents anything, and the exporters read that copy, whatever the same build does on its way to a binary.

  The other half is the certificate. `aver compile --target wasm-gc --certify` ships a Lean model of your program next to the binary, and the certificate's theorems tie the two together — so the model has to be the program the binary was built from, including the optimisations that only rearrange code you did write. It is: both halves are taken from one view. Where the compiler folds a record you build at a call site into the function that reads it, the model folds it too, because that is what the certified bytes compute. Every `.lean` and `.dfy` Aver emits for `examples/`, `stdlib/` and its own fixtures is byte-for-byte what it was, and so is every certificate.

- **`aver compile --explain-passes` reports what happened in dependency modules, not just the entry file.** It ran the pipeline over the file you named and nothing else, so a program whose dependency *was* optimised got told `no fusion sites detected` — the report contradicting the binary next to it. Sites in dependency modules are now counted too, with the module name in front of the function (`Bytes.hexParts`), in both the text and `--json` output. The report also says which builds the count is about: this optimisation is applied for the default Rust target and for `aver run`, and the count belongs to those — a `--target wasm-gc` or `--target wasip2` artifact carries none of it.

- **Stepping through a list in compiled code no longer allocates per element.** Destructuring a list — `match items` with a `[head, ..rest]` arm, the shape every recursive list function is written in — used to build a fresh list for the rest on every single step, so walking a million elements allocated and freed a million times over whatever the walk was actually doing. The rest of a list is now the same list read from one element further in: a step costs a reference count and nothing else. A walk that does little per element — counting, summing, range-checking — is about three times faster in a release build, and a walk that does more per element saves the same time in absolute terms. Nothing in your code changes; every compiled program that steps through a list gets the constant back. `List.drop` already handed back a view of the list it stepped into, and destructuring now steps the same way.
- **A list consumed two cells at a time now proves terminating, so hex parsing is checked by the Lean kernel.** `Bytes.fromHex` reads its characters in pairs, and the proof export used to give up on that shape: the parser came out as an opaque `partial def`, and every `verify` case whose answer passed through it — anything decoding hexadecimal, a 32-byte digest included — fell back to `native_decide`, which trusts Lean's compiler and evaluator instead of its kernel. Termination now follows a tail through a nested match, on the reading that the tail of a tail is still a tail, so the parser exports as an ordinary total definition with its list-length measure and those cases close with `decide +kernel`: the kernel recomputes the answer itself, and `Lean.ofReduceBool` is gone from their axiom trace. Any function that peels a fixed number of cells per step — pairs, triples, a fixed-width record read off a stream — is recognised the same way.

- **An unrecognised field in a module header is now an error.** The header used to stop at the first line it did not recognise and hand that line back to the top level. A typo'd `expose [main]` therefore produced no diagnostic at all when its right-hand side happened to resolve, and a mistyped field silently did nothing. Header-shaped lines — `name = value` or `name [list]` — are now checked against the five allowed fields.

  This is breaking for one shape: a top-level binding written *indented*, directly under the header, used to be accepted and now is not. The fix is to unindent it — bindings belong at column 0, outside the header — and the error says so.

### Fixed

- **Proof export no longer lets a module you never imported answer for a type name.** The Lean and Dafny exporters resolved a bare type reference by walking every loaded module and taking the first that declared the name, so in a program where two modules both declare, say, `Progress`, a declaration from a module nobody imports could answer for somebody else's type — decided by nothing the program controls. The consequences were quiet rather than loud: on the Lean side a pure capability operation whose result type resolved to the wrong twin lost its `Nonempty` witness and was dropped from the export, replaced by a comment; on the Dafny side the same collision could make one type look like the recursion cycle another was already inside, sending its whole mutual group to an axiom. Both exporters now resolve a bare name inside the asking module's `depends` closure, preferring the module's own declarations, and Dafny's recursion guard keys on the declaration it reached rather than the name that led there. Entry-module resolution is unchanged. Found while fixing [#1134](https://github.com/jasisz/aver/issues/1134).

- **A record no longer inherits `Eq` and `Hash` from a same-named record in another module.** When two modules each declared, say, `Progress`, the analysis deciding which generated types can key a map resolved the type it was emitting by its unqualified name and answered with whichever declaration it found first. A record holding something that can never be `Eq` — a pool of connections, an opaque handle, a `Float` — came out with `#[derive(.., Eq, Hash)]` and an `Ord` implementation over fields that have no order, so `cargo build` failed on the generated project after `check`, `verify` and `compile` had all passed. The decision is now made from the declaration being emitted, and a field naming a duplicated type resolves in the module that declares the field — the same rule the emitted field type already followed. Duplicate type names across modules stay ordinary; renaming one is no longer the workaround. Fixes [#1134](https://github.com/jasisz/aver/issues/1134). Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **Generated Rust no longer moves a match binding while an earlier call argument still borrows it.** Calls such as `slopeOf(newest, lastOf(older, newest))` now detach the first borrow before the nested owning use, preserving Aver's value semantics instead of failing the generated Cargo build with Rust E0505. Fixes [#1130](https://github.com/jasisz/aver/issues/1130). Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A plain Lean verify case no longer leaks a symbolic provider oracle through a transparent wrapper around partial recursion.** The exporter already declined a case that called a kernel-opaque `partial def` directly, because `native_decide` cannot decide a proposition with a free function variable. A one-line total wrapper hid the same callee from that gate, so `simp` exposed it only inside Lean and `lake build` failed with `Expected type must not contain free variables`. The gate now follows only unconditional forwarding wrappers to the opaque callee; a wrapper whose concrete branch simplifies the partial call away remains provable. Improves [#1010](https://github.com/jasisz/aver/issues/1010).

- **`Tcp.connect` now makes its existing deadline observable instead of looking unbounded.** Native and in-process wasm-gc socket establishment still uses the project deployment policy—five seconds by default, configurable with `[effects.Tcp].connect_timeout_secs`—but timeout errors now name the selected deadline and the capability reference states it at the operation. wasip2 now warns whenever a required connect or one-shot operation depends on an Aver default or configured deadline that its host-controlled WASI socket timing cannot honour; persistent-session I/O alone does not trigger the warning. This keeps one deployment-policy source of truth rather than adding a competing per-call timeout API. Improves the observable part of [#1118](https://github.com/jasisz/aver/issues/1118), which remains open pending confirmation of the reported timing. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **Checked-in self-host output can no longer drift from its source and generator.** CI now regenerates the release-shaped Rust self-host in a temporary directory, normalizes it to rustfmt's fixed point, and compares the complete source tree with `src/self_host` without touching the worktree. The existing build-and-corpus parity canary remains a separate ignored test, so the ordinary freshness invariant costs seconds rather than minutes. Self-TCO `Arc` wrappers are emitted in source-parameter order instead of `HashSet` iteration order, making regeneration deterministic across compiler processes. Run `python3 tools/regenerate_self_host.py` to refresh a stale tree. Closes [#1113](https://github.com/jasisz/aver/issues/1113).

- **wasm-gc now says when a provider breaks a statically discharged contract.** A call such as `Time.sleep(1)` or `Random.int(1, 6)` still executes without source-level `Result` ceremony, but if its host returns `Err` anyway the guest now sends that exact error text through the internal `aver.provider_contract_violation` diagnostic import before trapping. The branch remains fail-closed—there is still no fallback value—and the import is compiler/runtime protocol rather than a recordable Aver effect. Closes [#1112](https://github.com/jasisz/aver/issues/1112).

- **The wasm-gc hosts and certificate wall now fully admit `Process.stopRequested`.** The embedded wasmtime runner polls the native monotonic SIGINT/SIGTERM provider and records/replays the observation; browser and Cloudflare hosts explicitly return `false`, where no process signal exists, instead of relying on a generic missing-import stub. The independent Lean wall's closed host-import registry now includes both this binding and the new provider-contract diagnostic, with a recomputed wall identity.

- **A source file with many diagnostics is linear to render again.** Diagnostic construction now indexes the file's lines once and shares that table across every finding instead of splitting and rescanning the complete source for every error. A half-written large module no longer turns `aver check` diagnostics into quadratic work.

- **The nightly wasm-gc/wasip2 fuzz lanes build again.** The production multi-module flattener gained capability-registry and runtime/verification-surface arguments, but its three fuzz callers kept the old two-argument call and failed during compilation before AFL started. The codegen and VM↔wasm-gc parity targets now pass the typechecked program registry and runtime surface, matching the production compile path they exercise.

- **A value whose type comes from a module you never imported is compiled as itself again.** A type can reach your module without being nameable there: a record field declared next door — `record Rules { policy: Policy }` — arrives whenever you project `.policy` off a `Rules`, and neither `Policy` nor its module is anything your file names. The Rust backend kept an identity for such a type that had been minted while the other module was checked on its own, and in the whole-program view that number pointed at a different declaration entirely. Reading the representation off it could then decide that a `Bool` field was a packed byte sequence and emit `.to_int_list()` on it. `aver check`, `aver verify` and `aver compile` all reported success; `cargo build` on the generated crate said `no method named to_int_list found for type bool`. A type's identity is now re-derived from its name each time, and dropped rather than guessed when the name means two different things — and the Rust backend refuses, by name, to pick a representation it cannot justify. On the first external project seven of eighty-nine programs were emitting the wrong method call. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A type declared in a module nobody imports no longer changes what a name means everywhere else.** A bare type reference — `Step` rather than `Domain.State.Step` — was resolved against every type in the program, so adding a second declaration of that name anywhere, even in a module the file never imports, made the reference ambiguous. Nothing said so: `aver check` passed, `aver verify` passed, and `aver compile --target rust` exited 0 having written `compile_error!` into the generated crate for every function that touched the type; on the first external project one injected record left 76 functions unrendered against a baseline of none, and the failure surfaced only at `cargo build`. The VM refused the same program with an internal error. A bare name is now answered only by the modules the asking one can see — itself, what it names in `depends [...]` together with the types those dependencies re-expose, and the standard modules — so an unimported declaration cannot participate. A collision between two modules a file really does import is still an error, and it names both candidates. Separately, `aver compile --target rust` now exits non-zero when the backend substitutes a compile error for a construct it cannot render, instead of reporting success — read off what the emitter recorded, so a program whose own string data quotes `compile_error!` compiles normally and keeps its `verify` cases. Found on the first external project, where it cost a day and was mis-diagnosed as a problem with records and recursion. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).
- **The file you point a command at no longer changes what a type name means inside a module it pulls in.** A bare type reference written in a dependency — `Step` rather than `Domain.ScriptState.Step` — was answered by the entry module's own declarations before the dependency's own imports were consulted. A dependency never names the entry; the relation runs the other way. So a `record Step` in the file you happened to run took over the name inside a module that had never heard of it, and `aver verify` answered `internal error: dep fn 'checked' did not lower to MIR` about a function in another file while `aver check` on the same program reported nothing at all. Worse when the entry's own type carried a variant of the same name: the constructor then resolved, to the wrong declaration, with nothing said, and one unchanged dependency compiled to different code depending on which entry you built it from. The entry's scope is now consulted only for the entry's own code — in name resolution and in the Rust backend's type naming alike. Found on the first external project. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A constructor that names no variant is now reported where it is written.** `Step.Kontinue(n)` came back as `` `Step` is a type, not a value ``, at the line of the enclosing function's header rather than the line that writes it; it now says `Unknown constructor 'Step.Kontinue': type 'Step' declares Continue, Stop`, pointing at the constructor, and a head that names nothing at all (`Stepp.Continue(n)`) is reported at its own line too. When a name defeats the compiler anyway, the backends say which name and where instead of naming a function in another file: the VM reports the file and line, `aver compile --target rust` carries that sentence into the compile error it substitutes, and the wasm-gc backend refuses the module rather than quietly emitting a function that traps when it is called.

- **Matching an imported type's variants by their module path compiles to a real function on the wasm-gc target.** `match wrapper.status` with `Dep.Status.Open ->` as an arm's head kept that spelling after the backend had inlined the dependency and renamed its declaration to `Status`, so nothing resolved the name, the function was dropped, and what shipped in its place was a body that traps the first time anything calls it — while the build said nothing. Writing the same constructor in expression position was already rewritten; arm heads now are too, in the entry and in every dependency, including the arms of a tuple pattern.

- **`aver verify` no longer prints a green summary while a whole file went unchecked.** A file whose program could not be run — a type error anywhere in it, a backend or provider failure, or a refusal by verify itself — was dropped from the counts, so a directory where one file failed and one passed reported `Summary: 1 module | 1 block | 1/1 cases passed | 0 failed` in green, and the `--json` summary was byte-identical to a run over a file that simply has no verify blocks. Only the exit code said otherwise. The summary line now carries a `| N file(s) not checked` member and is never green while N is above zero; `--json` carries `files_skipped` and `blocks_unchecked`; each unchecked file is listed with the number of verify blocks that went unchecked with it. When the fault is in a dependency the dependency is named rather than the entry, and the false `No verify blocks found in <entry>` is gone. Modules that did verify before the walk stopped now keep their results instead of being discarded with their failing sibling. A stop that happens before the first module runs — a project file that does not parse, a dependency that cannot be found — counts every module of that program and every block it declares, not just the entry with a bare zero. A refusal by verify itself gets its own reason line instead of being reported as a type error and sending the user to `aver check`, which passes; the reverse mistake is closed too, so a dependency that does not parse is named as the source error it is rather than being called something `aver check` cannot show. A clean run prints exactly what it printed before. Found on the first external project. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **A dependency module's effectful functions now reach the Lean export, threaded like the entry's own.** A proof whose cone crossed into another file's effectful function got ``Unknown identifier `Infra.Store.get` `` out of Lean, and every claim behind it cascaded into `declaration uses sorry`: a dependency file exported no effectful function at all, and a qualified call site (`Infra.Store.get(store, key)`) received none of the `(path, oracle...)` arguments a bare call already did. Every module now exports the effectful functions some claim reaches — inside its own namespace, with the same `(path : BranchPath)` and oracle parameters the entry's own get — and a qualified call site passes the callee's own oracle list. Three threading holes inside a single file close with it: a helper no `verify` block names but a claim still reaches is exported instead of being left undefined, a tail call keeps the threaded arguments it used to drop after tail-call optimisation, and two effectful functions that call each other are exported as one `mutual` block instead of the forward reference Lean rejects. A function nobody proves anything about — a loop that reads the console forever, `main` — is still not exported, in a dependency as in the entry, so a program that proves nothing about a dependency's effects exports byte for byte what it did before. Found on the first external project, where it blocked five modules.
- **`Http.head` now works, and a response that carries no body is no longer reported as a truncated one.** A `HEAD` request, a `204 No Content` and a `304 Not Modified` all answer with header fields and nothing else. Aver used to read a body from them anyway; because the client offers compressed transfer on every request and a server repeats the `Content-Encoding` of the equivalent `GET`, the decoder was handed an empty stream and every such call failed with `Http: failed to read response body: unexpected end of file` — `Http.head` always, and `Http.get` on any conditional request the server answered `304`. Aver now asks for the identity encoding on `HEAD` and skips the body read for these three cases, so `Http.head` returns the headers and an empty body. The size is preserved too: a compressed response has its `Content-Length` removed by the client (it describes the compressed bytes, not the resource), so asking for `HEAD` uncompressed is what makes `content-length` readable — the usual reason to send a `HEAD` at all. A genuinely truncated `GET` still fails. Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).

- **The Lean export's entry file now opens only its direct dependencies, like every dependency file already did, and a user type from another module is spelled by its owner.** The entry used to `open` every module of the transitive closure, so a match binder spelled like a function two distant modules export (`message`, `named`, `from`) failed as an ambiguous pattern, and a user function named `none` made `Option.none` an ambiguous term; the same function built cleanly as a dependency copy and failed as the entry. The entry now opens exactly what its `depends` names (imports stay transitive), a function name two direct dependencies both declare is hidden from both `open`s (members of a `mutual` block included), and the Option constructors are spelled `Option.some` / `Option.none`. A type declared in another module is now written with that module's path in signatures, record fields and constructor expressions (`A.Fraction`, `X.Shape.circle`), a capability resource included (`Kv.Handle`), so a type a direct dependency only re-exposes, two opened modules declaring the same type name, a user type named like a Lean root type (`Sum`), and the resource of a capability the entry only threads an operation of resolve without an `open`; and a function or binder called `some` or `none` is renamed `some'` / `none'` in the export, like `id`, `max` and `min` already were. Every emitted file now sets `autoImplicit false`, so a type name the export leaves unresolved fails the build instead of becoming an implicit type variable that silently changes the theorem. Found on the first external project, where it blocked 14 modules.

- **Plain verify now stops at the effect boundary it actually reaches, instead of warning from a function-wide effect list.** A case over an in-memory arm of a sum stays an ordinary deterministic example even when another arm calls a provider; `aver check` no longer claims every case will flap or demands a dummy stub for an unreachable operation. Conversely, a plain case that does reach an effect without an exact `given` now aborts before host dispatch and points to `verify <fn> trace` or record/replay. This closes the previous silent mode where output was suppressed but generative and snapshot effects could still read or mutate the real host during `aver verify`.

- **The Rust target can now compile a user record or sum that carries an opaque capability resource.** Generated records and sums require their fields to support Rust equality and Aver display, but the generated resource wrapper supplied neither, so a field such as `token: Vault.Token` passed `aver check` and then failed in `cargo build`. Resource wrappers now provide identity-based host equality/hash and render only their canonical opaque marker, including in replay-enabled projects. This is generated-host plumbing only: it does not add resource equality or map-key semantics to the Aver language, and neither debug nor display exposes the provider-owned payload.

- **Certifying a program whose module carries a record no longer fails to build its model.** `aver compile --target wasm-gc --certify` emitted a Lean model that gave a record's default value as that record's own default value — circular, so the model did not build and `aver cert verify` answered `DECLINED certificate data did not build`, naming a missing `Inhabited` instance rather than anything about your program. Any record with exactly one field hit this, as did any record carrying one as a field, so a single `record Offset` with a single `by: Int` was enough to make a whole artifact unverifiable. The model now gives a record's default by naming its fields, and such a program certifies and verifies end to end. Single-field records were the shape this fixes; a sum type whose first variant recurses had the same problem and is fixed in the next entry. Every certificate that verified before verifies unchanged.

- **Certifying a program whose module carries a recursive sum type no longer fails to build its model.** The certificate model stated a sum type's default value as its first constructor with every argument defaulted, so for a `type Chain` whose variants are `More(Chain)` then `Stop(Int)` that default was `More` of the type's own default — circular in the same way as the record above, and `aver cert verify` answered the same `DECLINED certificate data did not build`, naming a missing `Inhabited` instance. The model now walks the constructors in declaration order and takes the first one whose arguments all bottom out — `Stop` here — which is the constructor Lean's own `deriving Inhabited` would pick, and such a program certifies and verifies end to end. An argument bottoms out only when every type it names has a stated default of its own: a refined record (one built through a validating smart constructor) deliberately has none, so a constructor carrying one is skipped the same way — `Raw(Natural)` then `Empty` seeds from `Empty` — and a constructor carrying another sum type is skipped when that sum has no default of its own. A variant holding the type inside a `List`, `Option` or `Map` still counts as bottoming out, since an empty one is a fine default. A type where no constructor bottoms out gets no stated default at all: a model that never needs one builds as before, and one that does is declined at the Lean build with the same message as today. Every certificate that verified before verifies unchanged.

- **Folding a map through a helper no longer copies the whole map on every step under `aver run`.** Building a map by threading it through a fold was linear only where the compiler could follow the accumulator by name. Extract the insert into a helper that hands the map back — `viaHelper(rest, setOne(key, into))` — or seed the fold from your own function, and it could not: every insert duplicated the entire table, so the work grew with the square of the entries. Writing 8,000 keys through a helper took 190 ms against 8 ms for the same fold written inline, and the gap quadrupled every time the count doubled.

  `aver run` now asks at each write whether anything at all can still see the map, instead of asking whether the compiler could prove it in advance. When nothing can, the insert reuses the table it was given. Those 8,000 keys take 8 ms — the inline fold's own time, to the millisecond — and the two spellings stay level as the map grows: 1.16 s against 1.16 s at 16,000 keys, 1.78 s against 1.78 s at 20,000. `Map.remove` threaded the same way came down from 359 ms to 14 ms at 8,000 keys. A map literal with entries in it stops duplicating its own entries as it builds, which is small but no longer nothing.

  Nothing you can observe changed: a map two names still share, one stored in a record field, a list or another map, one bound at the top level, or one handed to another branch of an independent product `(f(m), g(m))!`, is copied exactly as before, so reading it afterwards gives what it always gave. Compiled Rust and the self-hosted interpreter are unaffected — this is `aver run` catching up on speed, not a change of meaning, and a `Map` returned from a function in compiled code is still copied. `AVER_NO_RUNTIME_MAP_OWNERSHIP=1` puts every such write back on the copying path if you ever need to rule it out.

- **An error propagated with `?` from inside an argument now returns from the function that wrote it.** `Disk.readText(Disk.readText(path)?)` — read a file name out of one file, then read that file — handed `Result.Err` back to the caller in compiled code, but under `aver run` the error came out of the wrong function. Binding the inner call to a name first (`name = Disk.readText(path)?`) always behaved correctly, which is what made the difference read as a quirk of nesting.

  What you saw depended on who called the function containing the `?`, and only the first of the three said anything at all:

  - **called from `main`:** the run ended with `main returned error` and exit 1. The caller's `match` on the result never ran, and neither did anything after it — an error the program was written to handle, reported as a failure of `main`.
  - **called from any other function:** no error and no exit code. The `Result.Err` was delivered to *that function's* caller as its return value — a `Result` where a `String` was expected, with the rest of the function's body skipped — and the program carried on and finished successfully with the wrong answer. This was the quiet one, and it is the shape most code is in.
  - **called from a top-level binding:** the error disappeared. The binding was left unassigned, every top-level binding after it was skipped, and nothing was reported.

  The middle case had a second effect on top of the wrong value: the return address of the abandoned call was left behind, so the next `return` in the same run used it a second time and resumed in the middle of the wrong function, on the wrong stack frame. A later, unrelated call would come back with a value produced by re-running code that had already finished.

  `aver run` now agrees with compiled code and with the self-hosted interpreter on every spelling: a `?` anywhere in an argument list — first argument or a later one, one call deep or several — returns from the function whose body contains it, to that function's caller. A failing `?!` product written the same way is fixed alongside it. `verify` runs on the same engine, so a case exercising either shape is now judged on the corrected behaviour.

  What decided whether you hit it was the enclosing function: only bodies that call nothing of your own and bind no name were affected — which is exactly how the short "read the name, then read the file" helpers get written. Add a call to one of your own functions, or bind the value first, and the same program was already correct.
- **Updating a vector element inside a function no longer hands back garbage once the function returns.** `Vector.set` on a vector the function owns writes the element straight into place, and the value written was often built by that same function. Returning threw that value away: reading the element back afterwards gave whatever unrelated value had since taken over its storage — a string from three lines later, a record from the wrong row — and a program that filled a vector in a loop could come back with every slot holding the same stray value. Reading was silent about it; nothing failed, the answers were simply wrong. Every way a function can return is fixed — including the return out of a callback or an effect handler, where the vector had already outlived one call — so the element you stored is the element you read back. Vectors were the only shape affected: `Map` and `List` updates always allocated a fresh entry and were never at risk.
- **A map on the wasm-gc backend now grows, so filling one no longer hangs.** A `Map` compiled with `--target wasm-gc` used to have 16384 buckets and no way to get more. Storing the 16385th distinct key left the insert walking those buckets forever looking for a free one: no output, no error, no end — on a data size, not on anything visible in the source, so a program that worked against a few thousand entries stopped returning in production. The table now doubles whenever an insert would take it past three quarters full and rehashes what it holds into the wider table, so `Map.set` keeps taking keys until memory runs out, the same as `aver run` and the Rust target have always done. There is no size that is a cliff, and nothing to tune.

  An empty map got cheaper on the way: it starts at 16 buckets rather than 16384, which is about 128 KB of zeroes it no longer allocates. That matters most where a map is created and barely filled — a `{}` literal, and on `--target wasip2` a fresh header map for every `Http.get` call and every inbound request.

  **`Map.fromList` on this target was unusable and now is not.** It copied every bucket for every pair it was handed, so a thousand pairs took over two and a half minutes and nothing larger finished at all — a table too big to copy, copied once per entry. It fills its map directly now, since that map is its own until it hands it back: the same thousand pairs take well under a second, and twenty thousand take about as long.

  This was reachable from outside the program on `--target wasip2`: a request's headers become a `Map` one header at a time, so a peer sending more than 16384 distinct header names stopped the guest. The header maps are built by the same code, so they grow with it and that surface is gone.

  The insert helpers are still named in the module — `Map.set Map<Int,Int> in place (table grows; a stop here is a resize bug)` — so that if one ever does stop, the backtrace says which map it was instead of `<wasm function 12>`. **`--optimize` costs you that name**: the optimizer drops the name section and folds the helper into its caller. Compiling a program that uses a `Map` with `--optimize` prints a note saying so.
- **Removing a key from a map compiled to wasm-gc no longer strands other keys.** Keys that fall into the same bucket are stored one behind the other, and finding the second one means walking past the first. Removing the first used to leave that slot empty and stop there, so the walk ran into the gap and gave up: `Map.get` reported a key that was still in the map as absent, `Map.has` said `false` for it, while `Map.len` went on counting it and `Map.keys` went on listing it. A removal now carries every entry that had been stored past the emptied slot back into it.

  Nothing about the program said which keys were at risk — for small non-negative `Int` keys two collide when they differ by a multiple of 16384, and for anything else — negative, large, or `String` keys — it depends on the hash — and the answer differed by backend: `aver run` and compiled Rust were always right, only `--target wasm-gc` and `--target wasip2` lost entries. A program that removes from a map should be re-run on a build that includes this fix.
- **Filling a map no longer duplicates it once per entry.** Several independent things made `aver run` allocate as though every insert copied the whole map, and they are all fixed. `Map.fromList` rebuilt its own table for every pair it was handed, so turning a list of pairs into a map was quadratic on its own — which is what a program that replays a log or decodes a document does; 20,000 pairs took 180 ms of work and now take about 2 ms. A fold seeded from `Map.fromList([])` — how you spell an empty accumulator when the type has to come from somewhere other than a literal — copied its accumulator on every `Map.set`, where the same fold seeded from `{}` was already free. Naming that seed first, as `seed = Map.fromList(pairs)`, kept the copying whichever way the seed was written, and so did writing `Map.set` straight onto a named `Map.fromList` result. And a map handed across a boundary — into a branch of an independent product, back out of one, or through a replayed effect — was rebuilt entry by entry as it crossed. The program from the report needed 8.4 GB for 20,000 short string pairs and now holds under 20 MB.

  A `Map.set` may only update its target in place when nothing else can still see that map, which the compiler decides before the program runs. It knew a map literal is a fresh map that nobody else holds; it did not know that `Map.fromList` is one too, and it did not know it about `Vector.fromList` either. Both are now recognised as fresh — both where the call is written at the point of use and where its result is given a name first, which were decided separately and disagreed. So a map built with `fromList` is now consumed like a literal-built one always was, however you spell it. Converting a map into the runtime's own representation — the path a replayed value crosses — held the same rebuild-per-entry loop and is fixed with it.

  **Memory is the half this fixes.** The other half is the reading — every collection that sees a live map walks it entry by entry to establish that nothing in it moved — and it is fixed for maps of plain values by the entry below, not by this one.

  Two spellings are still quadratic in memory and are not fixed here. Passing the accumulator through a helper function that returns a map stays quadratic: a result handed back by a function is not yet known to be unshared, whatever the function does. So does handing a fresh map on to another binding before filling it — `seed = Map.fromList(pairs)` followed by `held = seed` or by any call that could keep it — because from that point something else might still be holding the map, and the compiler has to assume it is.

- **Folding over a map of plain numbers no longer re-reads the whole map on every step.** Under `aver run`, a map that stays live while the program keeps allocating was read entry by entry every time the garbage collector looked at it, to establish that nothing in it had moved — once per entry per step, so the cost grew with the square of the input and the reading was nearly all of the time. A map whose keys and values are all plain values — `Int`, `Float`, `Bool`, and strings of five UTF-8 bytes or fewer — holds nothing that can move, so it is now left alone entirely. A fold building a `Map<Int, Int>` while it also builds strings it throws away took 0.77 s at 16,000 keys, 3.1 s at 32,000 and 12.2 s at 64,000; it now takes about 10 ms, 20 ms and 30 ms.

  This is the map counterpart of what lists already got, and originally had the same limit. A map holding anything that lives on the heap — longer strings, lists, records, variants, tuples, nested maps, big integers — still had to be read one entry at a time on every collection just to establish that nothing moved. That remaining repeated-read path is fixed by the next entry.

- **Keeping an untouched map or list live no longer makes unrelated allocation quadratic under `aver run`.** The collector used to reopen every heap-backed element of the collection at every function boundary, even after an earlier boundary had established that the whole immutable body predated the frame's allocations. In the reporter's exact case, merely passing an untouched `Map<String, String>` beside a loop that creates strings made 40,000 iterations take 7,156 ms; the same run now takes 13 ms beside the map and 13 ms without it. A traversal over a flat `List<String>` likewise drops from 80,600 element visits at 400 items and 321,200 at 800 to zero repeated visits at both sizes.

  Immutable map and flat-list entries now carry a receipt from the arena's allocation-lane clock. A return or tail-call may reuse that receipt only when it proves the body was complete before the frame's allocation watermark; a collection created later, a collection containing an in-place write, an index-reuse cycle, or an exhausted clock takes the full conservative walk. The receipt is internal and changes no value, ordering, ownership rule or compiled backend. Compound segmented list storage remains on the conservative path so the receipt does not enlarge every arena entry.

- **Reopening a large append-only store no longer takes quadratic time or temporary memory under `aver run`.** The reported store first parses every log line into a list of heap-backed changes and then applies those changes to one map. Both phases repeated a whole-collection walk per entry: 40,000 records took 107.95 seconds and about 30 GB of peak resident memory. The same unmodified store now opens those 40,000 records in 0.22 seconds with about 76 MB resident; its collector traffic is exactly linear — three list-element reads and four map-entry reads per record — and no existing map entry is copied.

  Two conservative proofs were losing information. A nested parser call advanced the collector's global epoch even when it left the caller's immutable input list untouched, so the caller could not reuse an older receipt. Receipts now use one monotonic allocation serial and remain valid across unrelated nested boundaries. The map half was a module-loading bug: the VM reparsed dependency modules and resolved their slots without restoring last-use and alias annotations, leaving the accumulator's own dead parameter cell looking like a second live owner. Dependency code now goes through the same ownership reannotation as entry code; the parameter remains statically alias-prone, and the runtime only reuses its table after checking that no real stack, global or arena holder remains. Programs, map values and compiled backends are unchanged.

- **Walking a list while another one grows no longer exhausts memory.** Recursing over a list with `[head, ..tail]` while an accumulator list grows — the shape most parsers and decoders in Aver are written in — made `aver run` allocate as though it were copying the whole remaining input on every single step. Copying a 64,000-element list needed 7.2 GB; it now needs 17 MB for a list of integers and 22 MB for a list of strings. A program that used to take the machine down on a few hundred thousand elements now holds flat, whatever the list holds.

  The cause was in the garbage collector, not in `List.prepend` and not in how lists are represented. Whenever a frame returned or tail-called with a list still live, the collector rebuilt that list's element storage from scratch — even when every element was an integer that relocates to itself, and even when the list was an O(1) view onto storage it already shared. The rebuild threw away both the sharing and the view's offset, so one traversal copied n²/2 elements. Storage in which nothing actually moved is now handed back untouched.

  **Speed was initially the narrower half of this.** Storage built entirely out of immediate values — `Int`, `Float`, `Bool`, and strings of five UTF-8 bytes or fewer — could be skipped without being read at all, while longer strings, records, variants, tuples, nested lists and big integers still made the collector read the shared body repeatedly. The allocation-lane receipt in the entry above closes that remaining repeated-read path for flat list storage too.

- **`aver format` accepts a tuple type.** Naming a tuple in a parameter or return type — `fn first(pair: Tuple<String, String>) -> String` — made `aver format` fail with `paren-tuple types removed — use Tuple<A, B> instead`, pointing at the `Tuple<A, B>` the source already used. The formatter re-printed the annotation with the paren spelling `(A, B)` that was removed from type position, then refused to parse its own output; `aver audit` failed with it, since it runs the format check. Anything naming a tuple was affected, including the `List<Tuple<K, V>>` that `Map.entries` returns. Formatting is unchanged everywhere else — a type annotation is still canonicalized, and effect lists inside an `Fn` type are still sorted.

- **Deciding whether two maps are the same map is now refused for the key types the proof model cannot order.** The model keeps a map's entries sorted by key, which is what makes two maps holding the same entries the same value — but the sort needs an ordering, and there is none for `Float` or for a non-scalar key. For those the model keeps the entries in the order they were written, so it can tell two maps apart that the runtime considers equal. `a != b` over two float-keyed maps holding the same pairs is `false` when you run it and provable in Lean.

  Comparing map iteration order was already refused; this is the other half, and it is not syntactically an order observation, so it was slipping through. It is now refused wherever the equality is actually decided — with `==` or `!=`, in a `when` premise, or through `List.contains`, which is the same structural equality without an operator in sight.

  Which map is being compared is read from the type of the thing being compared, so a map built entirely inside a local binding and named in no signature is still found, and comparing two ordinary records inside a function that happens to touch a float-keyed map elsewhere is not refused. Order-blind reads (`Map.len`, `Map.get`, `Map.has`) and comparisons over `Int`-, `String`- and `Bool`-keyed maps export exactly as before.

- **The map iteration-order refusal now follows a call through an interpolated string, through a tail call, and into a function passed by name.** The refusal was meant to follow calls rather than spelling, so an observation hidden a few functions below the law is still caught. Three shapes escaped it. A call inside `"{firstValue(m)}"` was invisible, because proof export deliberately keeps interpolation unlowered and the walk skipped the segment. A call in tail position inside a recursion group was invisible, because the tail-call transform runs before typechecking, so every such call has already changed shape by the time the gate looks — a law over a function whose whole body is `readValues(m, n)` saw a cone containing nothing but itself. And a function handed to a callback parameter — `run(step: Fn(Map<Float, Int>) -> List<Float>)` applied as `step(m)`, called as `run(floatKeys, m)` — was invisible, because a name in argument position is not a call: the walk stopped at the caller and never looked inside the function whose body does the reading.

  All three exported as kernel-certified theorems that `aver verify` refutes on the same source: `aver verify` gave 0/1, `aver proof --check` gave exit 0 and 0 sorries. The tail-call one had a clean control — moving the identical call out of tail position was enough to make the same law refuse.

  Order-blind claims are unaffected, and no claim anywhere in the shipped examples changes status.

- **A claim `aver proof` refuses to export is now counted, named and charged.** The refusal above — a law or a plain `verify` case that reads map iteration order over a key type the proof model cannot reproduce — left exactly one trace: a comment inside the generated Lean. Nothing was printed, no count was reported, and the exit code did not move. `aver proof tests/fixtures/map_order_unmodelled_keys.av --check` dropped four claims on the floor and then said `0 sorries, universal: yes` and exited 0. A green check meant "four laws certified" to anyone reading it.

  `aver proof` now prints each refused claim by name with the reason, alongside what it compiled, whether or not you asked for `--check`. `--check-json` carries a `declined` count and a `declined_claims` array (identity, kind, reason), and `proof_manifest.json` records the same list next to `laws`, so a claim that moves from proved to refused shows up in a baseline diff rather than vanishing.

  **`--check` now fails on a refused claim.** This changes an exit code: a file whose claims are all refused used to exit 0 and now exits 1. Acknowledge a refusal you have decided to live with by passing `--declined-budget N` — deliberately its own budget, not `--sorry-budget`, because "we tried and failed" and "we refused to try" are different facts and a budget granted for an open induction should not quietly license a refusal. The reason this is a failure rather than a note: widening a refusal moves a claim out of the error and sorry counts, so without a charge of its own, widening the gate could turn a red `--check` green and the regression signal would disappear exactly when it is needed.

- **A map iterates in one order on the VM, in compiled Rust and in the exported proof.** `Map.keys`, `Map.values` and `Map.entries` return entries sorted by key — that part was already true when you ran a program — but `aver proof` exported a model that kept insertion order instead. A map built by writing `z`, `a`, `m` has values `[2, 3, 1]`, which is what `aver verify` reports; the exported Lean said the values were `[1, 2, 3]` and the kernel accepted it. Either tool could confirm a claim the other refuted, on the same source, on the same commit.

  The wasm-gc backend is the exception and stays one: it returns hash-bucket order, so on a ten-key map it reads `beta,iota,epsilon,...` where every other backend reads `alpha,beta,delta,...`. It is internally consistent — `keys[i]` still pairs with `values[i]` — but a claim about the *sequence* a map iterates is not carried to wasm-gc, and the trust header on every exported artifact now says so. Order-blind reads (`Map.len`, `Map.get`, `Map.has`) agree on every backend.

  The proof model now keeps the association list sorted by key with no duplicate entries, so it iterates the way the runtime does — and, because that form is canonical, two maps holding the same entries are now the same value in the model, as they already were at runtime. A law comparing maps with `==` or `!=`, or using one in a `when` premise, could previously be *proved* about maps the runtime considers equal.

  Map literals are affected too: `{"z" => 1, "a" => 2}` exports in key order, so it is the same value as the same map built with `Map.set`.

  Two consequences to know about if you have integer or float keys. Integer keys sort numerically — a map holding `2` and `10` iterates `[2, 10]`; running a program used to give `[10, 2]`, because that path sorted keys by their printed form rather than by the documented comparator. Float keys are ordered by IEEE 754 total order, which puts a NaN outside the finite range, and non-scalar keys (tuples, variants, lists) are ordered by their printed form; neither is reproducible in a proof, so anything you write in a `verify` block that *reads* iteration order over such a map — a law or a plain case — is now refused by `aver proof` with a message saying why. Order-blind claims over the same maps — `Map.len`, `Map.get`, `Map.has` — export exactly as before, whatever the key type.

  The refusal follows calls rather than spelling: it fires whether the `Map.keys` sits in the law itself or several functions down inside what the law reaches, and the key type is read through your own types too, so a `String`-keyed map that only ever appears inside a variant payload is still recognised as one the proof can order.

- **Reading the keys of a float map that holds a NaN no longer aborts the program.** Ordering float keys fell back to comparing raw bit patterns whenever a comparison was undecided, and that is not a total order: a NaN sits above `1.0` and below `-1.0` by bit pattern while `-1.0` is below `1.0`, a three-way cycle. On a map wide enough for the sort to notice — around sixty keys — `Map.keys`, `Map.values` and `Map.entries` ended the process with `user-provided comparison function does not correctly implement a total order` instead of returning. Float keys now order by IEEE 754 total order, so a NaN takes a fixed position outside the finite range and the sort always returns the same sequence. `-0.0` and `0.0` remain one key, as they always were.

- **`Map.values` in compiled Rust returns the values in key order.** It walked the underlying hash map directly while `Map.keys` and `Map.entries` next to it sorted, so a compiled binary could print a different value sequence on each run, and `keys[i]` did not line up with `values[i]`. Programs that only read `Map.len` or looked values up by key were unaffected.

- **`Args.get` now appears in the verify trace.** The VM did not recognise it as an effect, so a call read the real process arguments and then left nothing behind in the trace that `verify ... trace` laws are checked against. A law asserting the wrong event count passed and the law asserting the true one failed — `trace.length()`, `trace.count(...)`, `trace.event(k)` and `trace.contains(Args.get())` were all answering about an incomplete history. `Env.get` was never affected, which is why the gap looked like correct behaviour rather than a missing entry. If you have a trace-shaped law over a function that reads `Args.get`, its expected counts and event positions will shift by one per call, and the new numbers are the true ones.

- **The error that lists which effects `verify law` can cover now lists all of them.** Writing a law over an effect outside the proof subset reports what that subset contains — and the sentence naming it was maintained by hand and had fallen thirteen effects behind, among them `Terminal.size`, `Env.set`, `Tcp.connect` and every byte-carrying `Tcp` method. `Terminal.size` was missing while `examples/formal/terminal_size_snapshot.av` shipped as a worked example of proving over it, so the diagnostic told you a supported effect was unsupported. The list is now derived from the same table the checker enforces, and grouped by namespace, so it cannot fall behind again.

- **A law over an effect call nested inside another effect call's arguments is provable again.** `Random.int(1, Random.int(2, 6))` ran one way and exported a proof about the other: the run charges the inner read the first oracle index and the outer read the second, but the export numbered them the other way round. Nothing said so. The law that matched the run passed `aver verify` and then `aver proof --check` refuted it in Lean, while the law that matched the export failed `verify` — so a correct program was simply unprovable and each tool pointed at the other. Oracle indices are now assigned in evaluation order on both sides. Laws that only read effects one at a time, or read them in separate statements, were never affected and their exported proofs are unchanged.

- **Walking a list with `List.drop` no longer re-copies the remainder on every step.** `List.drop(xs, n)` cost the length of what was left rather than the length of what it stepped over, so advancing through a list a little at a time — what every length-prefixed binary format does — was quadratic, while the same walk written as `[head, ..tail]` was linear. Stepping now hands back a view onto the list it was given rather than a fresh copy of the rest of it, which is what destructuring has always done, so the two walks cost the same. The reporter of the issue measured decoding a 1.6 MB Bitcoin block at 6 minutes 25 seconds through `List.drop`, and at 5.6 seconds after hand-replacing it with destructuring — the walk `List.drop` now equals; on our own measurement, walking 400,000 elements in steps of 400 went from 727 ms to 3 ms, and doubling the list now doubles the time instead of quadrupling it.

  Nothing about what `List.drop` *means* changed — same elements, same order, same empty list at the end, on `aver run`, `--self-host`, `--target rust` and `--target wasm-gc`. Only the cost is different, and only where the list is stepped into: a list that was built by prepending is still walked link by link, exactly as destructuring walks it. One consequence worth knowing: a view keeps the elements it stepped over alive for as long as you hold it, so a small tail of a very large list holds the large list's memory. That has always been true of `[head, ..tail]` and is now true of `List.drop` too.

- **A negative count means the same thing to `List.take` and `List.drop` in a compiled binary as it does everywhere else.** `aver run` reads a negative count as "step over nothing" — `List.drop(xs, -3)` is `xs` and `List.take(xs, -3)` is `[]` — and so does `--target wasm-gc`. A binary built with `--target rust` had the two exactly backwards, dropping the whole list and taking the whole list. The clamp is now written once and shared by all of them.

- **A compiled project with an `aver.toml` no longer computes a `Disk`, `Env` or `Http` argument twice.** When the project has an `aver.toml` — any `aver.toml`, including one that only carries `[[check.suppress]]` waivers and says nothing about effects — `aver compile` puts a policy check in front of every `Disk`, `Env` and `Http` call, and it used to build the value it checks by working the argument out a second time. If that argument named a parameter, the check consumed the parameter and the call was left using something that had been given away: `cargo build` on the generated project failed with `use of moved value`, while `aver check` passed and `aver run` printed the right answer, so the only sign of it was in Rust you did not write. If the argument did something on its own — read the clock, print, read another file — it did that thing twice, and the check was then looking at a different value from the one the call used. The argument is now worked out once and both of them read it, whichever effect it is and however many arguments it takes. Binding the path to a name first, `path = pathOf(dir, n)`, was the workaround and is no longer needed.

- **An `Http` call now appears in the verify trace in the browser playground too.** `Http.get`, `Http.head`, `Http.delete`, `Http.post`, `Http.put` and `Http.patch` reported no effects at all in a build that carries no networking — the playground is the one you are likely to meet — so the VM treated a network call as a call that does nothing: the effect check waved it through without checking anything, no event was written to the trace that `verify ... trace` laws are read against, and Record and Replay were skipped over. A trace-shaped law over a function that calls `Http.get` counted zero events and passed while proving nothing. Wherever networking does ship, the release binaries included, the effects were reported all along, which is why the gap read as a limit of the browser rather than a missing entry. If you have a trace-shaped law over a function that makes an `Http` call and you run it in the playground, its expected counts and event positions will shift by one per call, and the new numbers are the true ones.

### Changed

- **A module named `Bits` can no longer define a function named `and`, `or`, `xor`, `not`, `shiftLeft`, `shiftRight` or `low`.** This is the existing rule for every builtin namespace — a module named `Bool` has never been able to define `fn and` — now extended to seven more names. A project-local `Bits` module still shadows the namespace for any *other* function name; only a direct collision is rejected, with the same "already defined in this module" diagnostic.

## 0.28.1 — 2026-08-12

### Changed

- **Breaking: `Str` is no longer accepted as a spelling of `String`.** The string type has exactly one name, the way `Int`, `Float`, `Bool` and `Unit` each do. `Str` was an undocumented alias that `aver check` and the VM accepted but the wasm-gc backend did not, so `fn label() -> Str` checked, ran, and then failed to compile with `cannot lower type 'Str'`. Replace every `Str` in a type annotation with `String`.

### Fixed

- **A type annotation naming a type that does not exist is now an error where you wrote it.** `fn twice(s: Wibble) -> Wibble` passed `aver check`, `aver verify` and `aver proof`; only `aver compile --target wasm-gc` refused it, and the exported Lean project failed at `lake build` naming a type Lean had never heard of — so a typo in a type annotation surfaced in generated code in another toolchain instead of at the source line. Every position that takes an annotation now reports the undeclared name: parameters, return types, binding annotations, record fields, sum-variant fields, and verify-law `given` binders, including names nested inside `List<...>`, `Result<...>` and the other compound forms. Types a dependency exposes, opaque imports, and the types the compiler provides itself (`HttpResponse`, `HttpRequest`, `Tcp.Connection`, `Terminal.Size`, `Trace`, `EffectEvent`, `BranchPath`, `Bytes`, `Digest32`) are unaffected, and a type may still be declared later in the file than the function that names it. This is also what now points at a leftover `Str`, so the alias removal above reports at the annotation rather than at `aver compile`.
- **Type declarations are now order-independent within a file.** Record fields, sum-variant fields, and constructor parameters can name types declared later, and a local type consistently shadows a dependency exposing the same bare name, preventing dependency values from being read through an unrelated local record layout.
- **A mismatch between two same-named types now says which module each one comes from.** When your module declares a type whose bare name a dependency also exposes, the local declaration shadows the imported one and the two stay distinct — but the mismatch used to read `expects Thing, got Thing`, naming a type and itself. Every "expected X, got Y" diagnostic now qualifies both sides when their spellings would otherwise be identical (`expects Main.Thing, got Alpha.Thing`), in record fields, call arguments, list and tuple elements, binding annotations, return types, and operators alike. Messages between two differently named types are unchanged.
- **A sum type may share its bare name with one a dependency declares.** Two modules each declaring a `Colour` used to abort `aver run` on an internal assertion naming neither the type nor the modules, or — in a build without debug assertions — resolve one module's constructor to the other's. Constructors are now keyed by the declaring module, so a local sum type shadows a dependency's the same way a record already does.
- **Standalone Lean proof exports no longer fail when an entry module and a dependency share a function name.** Entry definitions now use the same module namespace layout as dependency definitions, so standard-library modules such as `Crypto.Digest32` build on their own.
- **Disk allowlists now honor explicit project and filesystem roots.** `paths = ["./**"]` now allows relative paths inside the project and denies absolute paths outside it, while `/` and `/**` allow absolute paths from the filesystem root. Bare `**`, empty entries, unsupported glob spellings, and `..`-rooted paths are rejected when `aver.toml` loads, as are non-matching `*`/`**` host entries and `**` environment-key entries.
- **The VM keeps same-named record types from different modules distinct.** A private dependency record can no longer corrupt the field layout of an entry-module record with the same bare name.
- **Lean proof exports preserve `?` error propagation inside nested expressions.** Proofs can now rely on calls, operators, interpolation, bindings, and match arms returning the original `Result.Err` instead of continuing with a default value.
- **A failing `verify` case can no longer export as a provable Lean theorem.** A case whose `?` hits `Err` fails under `aver verify`, and the Lean export now states it faithfully — the case runs as a `Result` computation compared against `Result.Ok(<expected>)` — so `aver proof --check` fails it too. Previously such a case continued with a default value, and whenever the expected value happened to equal that default the proof project built green while the test suite was red. Only cases whose left side contains `?` change shape.
- **Dafny proof exports of a function whose `?` sits inside a larger expression now verify.** Such a function is exported as an opaque function the verifier does not look inside, the same treatment other unsupported shapes already get. Previously the export contained a placeholder that `dafny verify` rejected with an argument-count error naming an unrelated helper.
- **A `verify … law` whose `?` hits `Err` can no longer export as a provable Lean theorem.** All three statements a law emits — the quantified theorem, the checked-domain conjunction, and each sample — now run the law body as a `Result` computation compared against `Result.Ok(<expected>)`, so a sample `aver verify` rejects is rejected by `aver proof --check` too. Previously the body continued with a default value, and whenever the expected value happened to equal that default the proof project built green while the test suite was red. The quantified theorem now also claims what the law really says: the `?` reaches `Ok` for every value of the givens. Only laws whose left side contains `?` change shape, and such a law is no longer offered to later laws as a rewrite rule.
- **`[[check.suppress]]` no longer depends on how you spell the path.** A waiver written for `domain/version.av` now applies to `aver check .`, `aver check ./domain/version.av`, the absolute path, and dependency modules under `--deps --module-root .` — previously only some spellings matched. A glob may itself be written with a leading `./`. `aver audit` honours the same waivers as `aver check`, and reports how many warnings they removed; as a consequence it now also stops on a malformed `aver.toml` instead of ignoring it. Suppression still applies to warnings only and can never hide an error or a verify failure.
- **A `[[check.suppress]]` rule that waives nothing now says so.** Whole-directory runs of `aver check` and `aver audit` report on stderr any rule that removed no warning, distinguishing a glob that matched no checked file from one whose warning no longer fires. Exit codes are unchanged.
- **`verify-coverage` reads `?` as a `Result.Ok` case.** A case such as `readOne([7, 9])?.value => 7` now counts: an error would have propagated and failed the case, so it pins the `Ok` arm at least as firmly as `=> Result.Ok(...)` — and it pins a field as well.
- **`verify-coverage` no longer asks a private helper to repeat its caller's return-shape cases.** A function outside the module's `exposes` list, reached by exactly one caller from outside its own recursion group, inherits that caller's `Result`/`Option`/`Bool`/sum-type coverage when the caller's examples pin every arm. Input-shape warnings — list, `Bool`, enum and base-case arguments — still fire: they are about the helper's own argument domain, which no caller can vouch for.
- **`aver run --self-host` now matches a dependency's sum type by its qualified name.** `match s` with arms like `Palette.Shade.Dark` took no arm at all and failed with "no matching arm", even though the type name was unique across the program and `aver check` passed. The same mismatch made `==` between a variant a dependency produced and the same variant written qualified answer `false`, and made a value the caller built qualified unmatchable by the dependency's own `match`. A constructor now has one identity whichever module spells it, so all four combinations agree with the VM.
- **A module may spell its own constructors with its own module name.** `Main.Shade.Dark` inside `module Main` passed `aver check` and then stopped `aver run` with "namespace Main has no member 'Shade'"; the wasm-gc backend refused the same line. Only a constructor carrying fields — `Main.Tone.Solid(7)` — worked, so whether the spelling ran at all depended on whether the variant happened to take arguments. Any number of module segments may now precede a `Type.Variant` reference wherever a constructor may be written, and a value built with one spelling matches a pattern written with the other.
- **An empty `[]` or `{}` passed as the default to `Result.withDefault`, `Option.withDefault`, or `Option.toResult` now takes its element type from the surrounding code.** `Result.withDefault(parsed, [])` works in return position, in an annotated binding, as an argument, and in a plain binding — the "expression has type `List<T>`, annotation says `List<Int>`" mismatch, and the workaround of binding the empty literal with an annotation first, are both gone. In the plain-binding case the old behaviour was worse than an error: it passed `aver check` and was then refused by the wasm-gc backend with an unlocated "`List<T>` helper wasn't registered". Reported by Robin Owens ([@n1bor](https://github.com/n1bor)).
- **A local type that shadows a dependency's same-named one now compiles on wasm-gc.** Such a program passed `aver check` and ran on the VM and the self-hosted interpreter, but `aver run --wasm-gc` and `aver compile --target wasm-gc` refused it with an unlocated "supertypes must be defined before subtypes", "expression has no type", or "record `Dep.Name` missing field list" — so it only failed at the point where it was packaged. The local type and the dependency's now get separate representations, and the dependency's type stays usable through its module path (`Palette.Colour.Blue`).

## 0.28.0 "Oktet" — 2026-08-10

Named for the humble byte: this release gives Aver a real binary story — validated bytes as a type, exact TCP reads and writes, SHA-256 — with the proofs to match, and it starts deleting `Result` ceremony wherever the compiler can see the answer.

### Added

- **A `Bytes` type and `Crypto.Digest32`, the first embedded standard-library modules.** `Bytes.fromList` / `Bytes.fromHex` validate octets once at construction; `toHex`/`toList` round-trip; `Digest32` guarantees exactly 32 bytes. Both are ordinary Aver source, so every backend and both proof backends share one definition.
- **Binary TCP: `Tcp.sendBytes`, `Tcp.readBytes`, and `Tcp.writeBytes`.** One-shot calls and persistent connections can now put exact bytes on the wire and read exact-length frames (short read is an error, not a truncated success), with hostile test profiles included. Binary protocols — length-prefixed frames, non-UTF-8 payloads — are now expressible end to end.
- **`Crypto.sha256 : Bytes -> Digest32` — pure, total, no `Result`.** The types carry the guarantees. Four independent implementations (VM, generated Rust, in-module WASM, and the Lean/Dafny proof models) are differential-tested against the FIPS vectors; on WASM the hash is computed inside the sealed module, so a hostile host cannot lie about a digest.
- **Literal discharge: the compiler deletes `Result` where it can decide the answer at compile time.** `Int.div(x, 16)` and `Int.mod(x, 16)` with a nonzero literal divisor are plain `Int`; `Bytes.fromList([1, 2, 3])` with in-range literals is plain `Bytes`. The boundary is strict and syntactic — a variable divisor or a computed list keeps the explicit `Result` path.
- **Certificates cover more programs.** Record type declarations and field reads are certified end to end, along with runtime `Int` comparisons, enum-to-constant classifiers, bounds-checked vector reads, eager `Bool.and`, and string concatenation in carrierless modules; a coverage counter with a checked baseline tracks certified functions per release.
- **Decidable verify cases are now checked by the Lean kernel itself.** Concrete examples over `Int`/`Bool`/`String`/`List` — including the SHA-256 FIPS vectors — close with `decide +kernel` and carry no native-evaluation axioms; cases the kernel cannot decide (floats, panics, oversized terms) conservatively keep `native_decide`.
- **`Bytes`-like refinements are real byte arrays on wasm-gc.** A record over `List<Int>` whose constructor proves an octet range is stored packed — the proof justifies the layout — so hashing and byte I/O run without per-element boxing.
- **`aver check` warns when a project module is shadowed by the standard library**, instead of silently ignoring the project file.

### Changed

- **Breaking: string interpolation renders primitives only** (`Int`, `Float`, `Bool`, `String`). Any other embed — including one whose type inference cannot pin — is a type error; write a named conversion function returning `String` and interpolate its result. Display decisions belong in source, not in the compiler.
- **Breaking: literal-divisor division no longer returns `Result`,** so existing `match`/`?`/`withDefault` handling around `Int.div(x, LITERAL)` is a type error — the fix is deleting the ceremony.
- **The pinned Lean toolchain is v4.32.2,** picking up two upstream kernel soundness fixes. The certificate wall identity rotates with it: previously issued certificates verify only with their paired release — re-certify with 0.28.0.

### Fixed

- **Programs may name things after Lean tokens.** Functions, types, and modules called `Type`, `sorry`, `at`, and the rest of Lean's reserved tokens now export buildable proof projects, guarded against future toolchain drift in CI.
- **The `?` operator compiles inside verify blocks on the generated-Rust backend** for every error shape.
- **Interpolating an unsupported value can no longer compile into a silent runtime trap on wasm-gc** — it is a type error at check time, and internal pipelines get a loud codegen error instead of a function replaced by `unreachable`.
- **Carrier-erased memory layouts resolve by exact type identity, never by name,** closing a class where a same-named type from another module could silently inherit a representation, and module-qualified type annotations over dependency types work throughout.
- **`--self-host` supports top-level bindings** with host-VM semantics, and self-qualified module calls no longer trap on wasm-gc.

Binary TCP and `Crypto.sha256` were contributed by Robin Owens ([@n1bor](https://github.com/n1bor)) — Aver's first external contributor.

## 0.27.1 — 2026-07-19

### Added

- **A normative certificate format specification** for anyone reimplementing the verifier independently: `docs/certificate-format.md` documents the on-disk package, exactly what acceptance requires (as opposed to what the producer happens to emit), the trust inventory, and the versioning and freeze policy.

### Changed

- **`aver compile --certify` now certifies modules that don't use the Int runtime helper.** Carrierless modules no longer hard-fail; the host-role table is optional and pinned three ways, so a module without the `__rt_aint_from_i64` helper is proven carrierless from the bytes rather than rejected.
- **The trusted certificate report prints only kernel-pinned facts.** Values that are declared but not kernel-pinned — such as the source-level domain and codomain prose — now appear only under `aver cert explain`, labelled as declared.
- **`aver cert verify` is faster.** The final `leanchecker --fresh` replay no longer re-checks Lean's metaprogramming library, which the certificate does not depend on.

### Fixed

- **User-defined ADT, string, and arithmetic obligations are pinned to the exact module bytes.** This closes a gap where such an obligation could be satisfied by a value that did not match the shipped bytes; the acceptance proof now ties each obligation to the module's actual bytes.
- **The verifier enforces wall-clock timeouts on its subprocesses,** so a hung or slow checker step fails closed instead of blocking indefinitely.

## 0.27.0 "Witness" — 2026-07-15

Named for the proof carried beside the artifact: Aver binaries can now ship a Lean witness about their exact bytes, checked independently of the compiler.

### Added

- **Aver binaries can carry an Artifact Behavioral Certificate.** `aver compile --certify` emits one beside wasm-gc output, and the independent `aver-cert verify` checks the exact shipped bytes with the pinned Lean kernel. Unsupported exports remain explicitly uncertified with a reason.
- **Certificates cover real programs:** arithmetic, comparisons, ADTs, list construction and payloads, safe float comparisons and results, strings, calls, recursion, and mutually recursive families. They account for the whole module and can include checked termination witnesses for total correctness.
- **`aver cert check` provides a faster development loop.** It reports `CHECKED`; strict `verify` performs the final `leanchecker --fresh` replay and remains the release or admission gate.
- **Egg Catch demonstrates the full workflow** with twenty-one kernel-proven engine laws. The playground also gains touch controls and a practical mobile layout.

### Changed

- **wasm-gc sum types use precise nominal references** instead of a catch-all reference type.

### Fixed

- **The VM keeps every NaN as a Float** instead of mistaking rare payloads for boxed values.

## 0.26.0 "Zahlen" — 2026-06-26

Named for ℤ — the *Zahlen*. `Int` becomes the mathematical integer on every backend, so a verified program can no longer disagree with its proof at runtime; and the prover learns to close whole families of laws from the helper laws you write, instead of carrying a built-in proof for each shape.

### Added

- **One generic prover closes membership, sortedness, and conditional inductive laws, fed by your helper laws.** Instead of a built-in proof per shape, the prover inducts generically and uses the `verify ... law` blocks you (or the Method loop) write as lemmas — so a figure that needs a new fact is closed by *stating that fact as an Aver law*. Lean backend.
- **Predicate laws can be stated with `holds`** — a law that must hold for all inputs, like `sorted(sort(xs)) holds`, is written directly instead of being forced into an `lhs => rhs` equation, and proven for every input.
- **Tail-recursive accumulator functions prove equal to their spec on both backends** — `qrev(x, []) = rev(x)`, `factTR(n, 1) = fact(n)`, and the rest of the `{List, Nat} × {+, ×}` grid.
- **Single-list conditional laws prove universally when they can, and keep an honest bounded check when they can't** — the choice is made from a real build, no annotation needed. Lean backend.
- **Proofs reuse earlier proven laws — in the same file and across module boundaries.** A later law can be proven by citing an earlier one, and a law a module `exposes` and proves becomes a lemma its consumers can cite; editing the earlier law re-checks everything that depends on it. A private or unproven law never crosses the boundary, so a citation can't manufacture a false pass. Lean backend.
- **`aver proof --minimize` rewrites each proof to the tactic that actually closed it**, so a proven law reads like a hand-written proof instead of carrying the prover's fallback portfolio. It can only tighten a proof, never break one (Lean-only, implies `--check`).
- **`aver proof --gate <baseline.json>` fails if a previously-proven law is removed or weakened** — dropping to a weaker tier, leaning on a new kernel axiom, or switching backend is caught as a regression. Accept a deliberate change by regenerating the baseline with `--write-baseline`. Lean backend.

### Changed

- **`Int` is now arbitrary-precision (ℤ) on every backend — VM, Rust, and wasm-gc.** Integer arithmetic no longer overflows or wraps; `Int` is the mathematical integer everywhere, matching what proofs and `verify` already assumed. Large literals are written directly — `let big = 1267650600228229401496703205376`, no wrapper — and small values keep a native fast path. See `examples/core/big_integers.av`.
- **CI gates the shipped example corpus** — `examples/core` and `examples/data` must pass `aver check` on every pull request.

### Removed

- **The brute-force lemma-discovery enumerator is gone** (`aver proof --discover` and `--emit-laws`). It closed none of the hard tasks and carried the prover's heaviest soundness surface. Helper laws are now written deliberately — by hand or by the Method loop (the `the-method` skill) — and feed a proof the same way. The source is archived at the git tag `archive/lemma-discovery-enumerator`.

### Fixed

- **`aver verify` fails loudly on a file that doesn't parse** instead of reporting "No verify blocks found" and exiting 0 — a syntax error can no longer slip a broken file past a CI step as green.
- **A `verify` case that wraps onto a continuation line no longer breaks the cases after it** — a block can freely mix wrapped and single-line cases, and `aver fmt` handles them.
- **wasm-gc strings count characters, not bytes** — `String.len`, `String.charAt`, `String.slice`, and `String.chars` use Unicode scalar counts matching the VM (`String.byteLength` still counts bytes), and `Char.toCode`/`Char.fromCode` roundtrip multi-byte characters like `ż`, `…`, `😀`. `examples/data/json.av` passes `aver verify --wasm-gc` in full.
- **wasm-gc verifies and compares tuple results carrying generic constructors** — values like `(Option.None, n)` and `Result.Ok`/`Result.Err` keep their payload types and run like they do on the VM, and a mismatched tuple literal is now rejected at check time. `examples/core/order_total.av` passes `aver verify --wasm-gc`.
- **`aver audit` counts every check error in its total and exit code** — a file whose only errors were `error[verify-rhs]` no longer audits green.
- **A law whose generated name clashes with another in the same proof is skipped with a clear note** instead of quietly costing a neighbouring law its proof.
- **Large conditional-law domains export in smaller proof pieces** so a broad sampled grid no longer fails just for being too large for Lean to elaborate at once; when samples are capped, the generated file says so.
- **`Http`/`Tcp`/`HttpServer` programs compile on the Rust backend under the ℤ `Int` default.** A service value's integer field — an HTTP response `status`, a TCP connection `port` — is the mathematical `Int` at the Aver surface while the host keeps it as a machine integer; the Rust backend now converts at that boundary in both directions, so reading `resp.status` or returning an `HttpResponse` from a handler builds and runs (matching the wasm-gc backend, which already did this).

### Performance

- **Bounded integers keep native speed and code size under the ℤ default.** A value the compiler can prove stays in a fixed range — a record whose smart constructor bounds its field — is stored and computed as a native 64-bit integer rather than a big integer, and comparisons against integer constants lower to a direct machine compare. Size-sensitive programs like the games corpus shed most of the cost the ℤ default would otherwise add.

## 0.25.0 "The Method" — 2026-06-12

The release where the prover learned to be steered by lemmas — named after the proof methodology of ACL2's Kaufmann & Moore, whose 30-year-old terrain this release kept rediscovering.

### Added

- **The JSON example proves end-to-end on Lean.** The escaped-string parse/serialize roundtrip laws — the example's last two caught `sorry`s — now close kernel-genuine, no Mathlib, no `native_decide`. The strategy recognizes any escaper/parser pair of that shape, not just this one.
- **Exact-rational arithmetic ships as a worked example** (`examples/data/rational.av`) — its full ring algebra proves universally on both backends. Any all-`Int` record with pure `+`/`-`/`*` laws qualifies.
- **Floor-division laws prove universally on both backends** — halving recursion (binary exponent search, digit extraction) gets genuine well-founded termination instead of falling outside the proof subset, and power-of-two window laws close as true universal proofs.
- **Conditional `when` laws over sign and equality premises prove universally** — for every input satisfying the premise, not just the sampled domain; `--check-json` reports the count as `when_universal`.
- **Your proven laws help prove your later laws** — decomposing a hard law into helper laws in the same file is a working proof strategy; opt-in `aver proof --discover` additionally conjectures and kernel-proves helpers automatically.
- **Recursive functions over your own types prove termination structurally** — no more fuel wrappers for tree-shaped data; mutual recursion (e.g. quicksort) gets genuine well-founded termination.
- **More laws close push-button**: finite given-domains (`Bool`, field-less enums), decimal render/parse roundtrips, built-in string/int facts, ground enum laws, `Int.max`/`Int.min` arithmetic, general-key `Map` laws, and `?` pipelines.
- **`aver proof --check-json` reports `universal_laws` and `bounded_laws`** — CI budgets can pin exact law counts instead of re-deriving them.

### Changed

- **`universal: true` now means genuinely universal** — bounded `when`-law checks still pass, they just no longer count as proven-for-all-inputs.
- **`aver check` catches compound expected-side type mismatches in verify cases at check time** instead of leaving them as guaranteed runtime failures.

### Fixed

- **`when` laws export honestly to both backends**: Dafny samples respect the premise, sample assertions stop pushing Z3 into minutes-long unfolding, Lean premises pin their numerals to `Int` (no false-as-stated theorems), and nonlinear laws degrade to honest caught `sorry`s instead of failing the build.
- **Proof exports decline instead of guessing**: a law over an unknown helper name degrades to a caught `sorry` (not a broken build), large given-domain grids split into bounded chunks in every export mode, and Dafny no longer guesses `decreases` measures that fail verification on correct functions.
- **Bounded proof checks compare against actual program results**, and any model panic during a proof is a hard check error (`model_panicked` in `--check-json`).
- **Tuple-carrying functions get correct termination fuel in Lean exports** — deeply nested values no longer make the exported model disagree with the running program.
- **wasm-gc runs much more of the verify corpus**: `Bool == Bool` compiles (so every `=> true` case works), Option-returning functions verify — including nested and compound payloads — and backend compile failures are reported as backend errors instead of "type errors" on files that type-check. Known residuals are documented: `Vector<Option<T>>`, Option inside tuple-typed results, and two pre-existing VM↔wasm-gc divergences (`String.len` bytes vs scalars; a `Result`-equality trap).
- **wasm-gc: `String.split`/`String.join` inside string interpolation compile correctly.**
- **`aver verify` labels `law` blocks as `law`** — they printed as `spec`.
- **Example corpus honesty**: overstated laws are plain verify blocks or trace checks now, and every example under `examples/core` and `examples/data` passes `aver check` clean.

## 0.24.1 — 2026-06-06

Patch release on top of "Divide" — a correctness fix in the optimizer and broader `aver check` hints.

### Fixed

- **Constant folding no longer drops a side effect or a non-terminating computation.** When the optimizer collapsed a `match` / `Result.withDefault` / `?` over a statically-known `Result`/`Option`, or an `Int.div`/`Int.mod` by a literal zero, it could discard a sub-expression that still had to run — silently dropping its effects (e.g. a `Console.print`), or turning a program that should loop forever into one that returns; one wildcard case could also crash the VM. Folded expressions now always run their effects, in source order.

### Changed

- **`aver check` flags more redundant work** — a loop invariant built directly inside a recursive call's arguments, and a pure function computed more than once with the same arguments in a single expression.

## 0.24.0 "Divide" — 2026-06-06

> _One middle-end under every backend; the last operator that could crash is now a function._

### Breaking

- **Integer `/` is removed; use `Int.div(a, b) : Result<Int, String>`.** It was the last partial operation posing as a total operator. `a / b` on two `Int`s is now a type error pointing at `Int.div`, which returns `Result.Err` on a zero divisor or the `i64::MIN / -1` overflow. `Int.div` is Euclidean — the partner of `Int.mod` (`Int.div(-7, 2) == -4`). Float `/` stays total. **Migration:** `match Int.div(a, b)`, or `Result.withDefault(Int.div(a, b), <fallback>)` when the divisor is known non-zero.

### Added

- **`aver compile --emit-ir-after=mir`** dumps the textual `MirProgram` — the executable middle-end the VM runs, after HIR → MIR lowering and the optimize pipeline.
- **`aver compile --explain-mir-coverage`** reports how much of a program lowers to MIR (per function, with the dominant blocker); `--target wasm-gc` retargets the meter at the wasm-gc backend's reach.

### Changed

- **`Fn(...)` is allowed only as a function-parameter type.** Aver has no closures, so a function value can only be a named fn / builtin / constructor passed as an argument. Using `Fn(...)` as a return type, a field, a collection element, or a local binding is now a type error — keeping the concrete callee (and its effects) statically known at every call. To choose between functions dynamically, branch at the call site or model the choice as a sum type.

### Fixed

- **wasm-gc: a tuple binding that follows `_` is no longer dropped.** `match pair { (_, value) -> value }` returned the field's zero default instead of the bound element; tuple fields are now paired with their binding by position regardless of where the wildcards sit.
- **`aver run --self-host` keeps pace with the language.** The self-hosted interpreter now handles `Int.div`, float literals inside string interpolation, and Unit-returning `main` under record/replay — three places it had lagged the VM. (Higher-order functions and multi-module programs are still being brought across.)

### Compiler internals

- **MIR is now the only runtime middle-end.** The VM compiles exclusively through Core MIR (`src/ir/mir/`); the old ~2200-line HIR tree-walking compiler is gone — a function that can't lower to MIR is a hard error, not a silent second path. wasm-gc and wasip2 emit from MIR too (resolved-HIR fallback for shapes they don't yet cover) and run the shared `ir::mir::optimize()` passes, so one optimizer improves every backend at once.
- **Owned `Vector`/`Map` are mutated in place.** The MIR last-use pass updates a uniquely-held collection without copying it first, turning build-and-fill loops from O(n²) into O(n) — roughly 20× faster on the VM and 26× on native (`--target rust`) versus 0.23; wasm-gc is unchanged (it leans on the engine's GC).
- **`match` on `Int.div`/`Int.mod`'s `Result` lowers on every backend.** The directly-consumed (boxed) form now builds the `Result<Int, String>` on wasm-gc and exports faithfully to Lean/Dafny (guarding the zero divisor so the `Err` arm is reachable), so the Ok/Err idiom is no longer VM-only.
- **A constant divisor compiles `Int.div`/`Int.mod` down to bare division.** The MIR const-folder rewrites `Int.div(a, k)` for a literal `k ∉ {0, -1}` to `Result.Ok(a div_euclid k)`, then folds the consumer (`withDefault` / `match`) over the now-literal constructor — so `match Int.div(a, 10) { Ok / Err }` and `Result.withDefault(Int.div(a, 10), d)` lower to a plain Euclidean division on every backend. The explicit `Result`-returning function disappears when it provably can't fail.
- **wasm-gc modules are byte-reproducible across builds.** Carrier type slots are now registered in sorted order instead of `HashMap`-iteration order.

## 0.23.0 "Shape" — 2026-05-30

> _What `aver shape` sees, `aver proof` now uses._

### Breaking

- **All commands now use the same module-root default: explicit `--module-root` wins, otherwise current working directory.** Previously `aver run`, `aver run --wasm-gc`, and `aver run --wasip2` walked the entry file's parent chain to find a directory where every `depends [...]` resolved, while `aver verify` / `aver check` / `aver context` / `aver compile` already used cwd. After this change, every command treats the same arguments the same way. **Migration:** if you relied on `aver run projects/foo/main.av` from the repo root finding `projects/foo/depends/*`, add `--module-root projects/foo` (or `cd` into that directory).

### `aver shape`

- **MVP — architectural smell radar (CLI + LSP).** Per-fn archetypes (14 labels), `ModuleShape` 5-dim vector with derived `Kind` (`ServiceClient`, `Orchestration`, `SmartConstructor`, `DataModule`, `PureHelpers`, `Library`, `EffectfulLibrary`, `EffectfulShell`), and a histogram-based Layer guess (`Domain` / `Parse` / `Command` / `AiStrategy` / `RenderUi` / `Infra`) with confidence and runners-up. `aver shape <dir>` walks corpora; `--lint` checks `aver.toml`'s `[[shape.expected]]`. LSP surfaces the verdict via CodeLens, hover, and document symbol.
- **Module patterns are now first-class typed facts.** Five shapes by name: `RefinementSmartConstructor`, `WrapperOverRecursion`, `ResultPipelineChain`, `RendererFormatter`, `MatchDispatcherFold`. Surfaced in a new `Module patterns:` section in the CLI output and as a `patterns` array in `--json`. Same data feeds the LSP and `proof_lower`.
- **Output is flat + colored** — section headers at column 0, ANSI palette (bold headers, cyan Kind / Layer, yellow counts, magenta pattern variants). Auto-disables on non-TTY pipes; `NO_COLOR` respected.

### Proof export

- **Three new `ProofStrategy` variants close universal laws on Lean and Dafny.** Each consumes a `ModulePattern`:
  - `WrapperOverRecursion` — monoidal-accumulator wrapper (`sum(xs) == sumDirect(xs)`). Demo: `examples/data/sum_acc.av`.
  - `ResultPipelineChain` — `?`-chain ≡ nested `match Result.Err -> Err`. Demo: `examples/core/result_chain.av`.
  - `MatchDispatcherFold` — two list folds equal by structural induction. Demo: `examples/data/list_length_fold.av`.

### Tooling

- **`aver proof --check` runs the backend verifier and gates regressions.** `--error-budget=N` (Dafny) and `--sorry-budget=N` (Lean) tolerate up to N residual failures so CI can pin a budget; `--check-json` emits `{backend, errors|sorries, budget, passed}` for external harnesses. Exit codes: 0 within budget, 1 over, 2 on harness failure.
- **Playground Audit panel gains a Shape section** — Kind · Layer summary; expand to see module-shape vector, histogram, recognized patterns, and per-fn archetypes. Same payload as `aver shape --json`; no new toolbar button.
- **`aver verify --hostile`: opaque runtime handles can be fabricated inside verify-trace context** for system-handle effects (today: `Tcp.Connection`), so Oracle stubs can feed a deterministic conn into the SUT without round-tripping through `Tcp.connect`. Opt-in per type — user-defined opaques (`Refinement.Natural`, …) stay protected. `examples/services/redis.av` exercises this via `verify ping/set/get trace` blocks.

## 0.22.1 — 2026-05-29

### Hardening

- **`aver verify` works on multi-module programs.** 0.22.0 shipped with a regression: running `aver verify <entry>` on any program that declared `depends [...]` failed with `"missing VM symbol for exposed function Foo.Bar.baz"`. Every multi-module example under `examples/refinement/` (including the refinement-via-opaque flagship that Lift introduced), `examples/apps/notepad/`, and `projects/payment_ops/` hit it; `aver run`, `aver compile`, and `aver verify --wasm-gc` were unaffected. The VM verify path now loads dep modules the same way the other paths already do (both the disk-loader CLI shape and the pre-loaded playground/LSP shape).

## 0.22.0 "Lift" — 2026-05-28

> _Aver source stays ordinary. The proof export lifts it to the backend's native mathematical shape — refinements become subtypes, mutual recursion becomes a structural block, every verify-law passes through one classifier, and Dafny closes more obligations on its own._

### Refinement recovery

- **Aver recovers proof-language refinements from ordinary validated code.** A single-field `Int` record with a validating smart constructor (e.g. `Natural { v: Int }` with `v >= 0`) now exports as a native subtype on Lean (`{ v : Int // P v }`) and subset type on Dafny (`type X = v: int | P v witness W`); the predicate travels with the type. A public function that guards `n >= 0` before calling a private worker similarly refines that worker's domain in the export. Universal laws like `add_commutative(a: Natural, b: Natural)` close in the backend's natural proof shape, no per-law tactic plumbing. Conservative and source-compatible — no new Aver annotation, and `when` clauses stronger than the recovered invariant are kept as theorem premises rather than silently dropped. Recovered refinements survive module boundaries.

### Mutual recursion in proof export

- **Mutual-recursion SCCs over `List` / `Vector` / `String` parameters export natively.** Lean emits a single `mutual ... termination_by ... end` block keyed off the structural measure; Dafny emits a `decreases <measure>, <rank>` tuple per member. The fuel-bounded helper-and-wrapper indirection is gone for these groups. Bounded-∀ universal laws over the SCC verify as real proofs (BigInt's `add_commutative` moves from `assume {:axiom}` on trust to a real theorem). Sample assertions no longer exhaust Lean's synth budget on compound predicates.

### Law strategy substrate

- **Both proof backends read every verify-law strategy from one classifier.** Each `verify <fn> law` lowers to one of fourteen algebraic shapes (commutative / associative / identity / induction / library axiom / map update / linear arithmetic / four spec-equivalence flavours / linear recurrence) before Lean or Dafny emits anything. Visible via `aver compile --emit-ir-after law_lower`. Effectful impl-vs-spec laws classify on the canonical post-Oracle-Lift shape; `fib(n) == fibSpec(n)` closes as a real proof on both backends (Nat-helper bridge + worker-shift lemma) where prior releases emitted `sorry` / empty-body.

### Dafny verifier improvements

- **`dafny verify` closes 25 more proofs across the flagship suite (160 errors → 135).** Smarter `infer_decreases` picks the actually-moving recursion measure across self-call sites (catches `repeat(char_, n - 1)` and `scanExpTail(s, pos + 1, start)`). List-induction hints case-split `|xs| == 0` and recurse on `xs[1..]`, detecting recursive fns nested under `Map.*` / `Option.*` helpers. `examples/data/map.av` verifies clean; `fibonacci`, `rle`, `quicksort`, `json`, `grok_s_language` all improve without regression.
- **`String.slice` lowers via a clamp-to-empty helper instead of raw `s[from..to]`.** Aver's runtime semantics — negative or out-of-range indices collapse to an empty slice — now travel into the Dafny export. `examples/data/date.av` verifies clean (the `parseIntSlice(s, from, to)` shape no longer leaves uncloseable range obligations on every caller); `examples/data/json.av` closes 24 more proofs at the same time (113 → 89 errors), entirely from the parser's slice-heavy lookahead. Source-compatible — no Aver-side change, the helper is emitted only when `String.slice` actually appears.
- **Float `/` lowers via a `FloatDiv` helper that mirrors Aver's IEEE-754 semantics.** Aver float division never crashes — `1.0/0.0` is `Infinity`, `0.0/0.0` is `NaN`. Dafny's exact-rational `real`, in contrast, makes `a / b` impose `b != 0` on every caller. The new helper returns a defined value (`0.0`) when the divisor is zero, so callers like `goldenApprox(n) = Float.fromInt(fib(n + 1)) / Float.fromInt(fib(n))` no longer need to prove `fib(n) ≥ 1` just to compute the ratio. `examples/data/fibonacci.av` verifies clean.
- **Negative-domain guard recognition broader.** Recursive `Int` functions whose author writes `match n <= 0 { true -> base; false -> recur(n - 1) }` (or the equivalent `match n < 1 { ... }`) are now treated as self-guarded by `infer_decreases`, so the lowerer emits `decreases if n >= 0 then n else 0` without a `requires n >= 0` precondition the user never wrote. `examples/data/rle.av`'s `repeat(char_, n)` shape closes (the `expandRun(run)` caller no longer has to prove `run.count ≥ 0`).

### Tooling

- **`tests/proof_spec` gates `dafny verify` on the IR-clean examples and tracks per-example `sorry` and Dafny-error budgets across both backends.** Lean: three examples carry honest `sorry` budgets (`json.av` 13 sampled-domain laws, `rle.av` 2, `quicksort.av` 2). Dafny: the flagship examples whose proofs Dafny still can't auto-discharge — `fibonacci` (1), `rle` (4), `quicksort` (5), `date` (2), `json` (113) — are now gated by an error-count budget instead of being silently unverified. Drift either way fails the test, so a new shape regressing or an old gap closing both surface in CI. `aver compile --emit-ir-after={refinement_lower,contract_lower,law_lower}` exposes the three new proof-lower stages — when a law falls through to `sorry`/empty-body, `law_lower` shows whether the lowerer pinned a strategy or fell back to `BackendDispatch`.

### Backend foundation

- **Every backend (Rust, Lean, Dafny, wasm-gc) now consumes one typed view of the program.** Pre-Lift each backend carried its own string-keyed fn-signature side-channel (`ctx.fn_sigs`) and re-derived types by parsing source-annotation strings on the fly during emit; identity decisions (which `Shape` does this constructor refer to? which dep module's `Box` is at this slot? is this callee pure?) lived in fragile bare-name lookups that could silently drift between backends. Lift consolidates the codegen substrate around one resolver-produced view of the post-name-resolution program — typed `(name, Type)` parameter lists, typed return types, opaque `FnId` / `TypeId` / `CtorId` identity, and a `.ty()` slot on every reachable body expression — and the bare-string fn-signature cache is gone. A guardrail test (`tests/identity_guardrails.rs`) blocks any new backend code from regressing to the old patterns. The most visible payoff: two dep modules can each declare `record Box { value: Int }` and compile cleanly on `aver compile --target wasm-gc` (pre-Lift the post-flatten type registry collided them under one slot and `Left.Box(value = 5).value + Right.Box(value = 10).value` failed wasm validation), and `==` / `!=` on those records dispatches to the right per-type equality helper rather than silently picking the last writer. Single-declarer dep types stay on the legacy bare-key path with no behavior change; the next major IR step (MIR / Core IR for per-expression effects + ownership + cross-scope optimisation) lowers from this substrate, separate epic.
- **`String.fromFloat` on `aver compile --target wasm-gc` now matches VM and wasip2 to the last shortest-roundtrip digit.** The wasm-gc WAT helper capped the fractional-digit loop at 15 to avoid an `i64.trunc_f64_s` trap on large-magnitude inputs, but the cap also chopped one digit off values in the `[0.1, 10)` range where the multiplication doesn't overflow — `goldenApprox(n) = Float.fromInt(fib(n + 1)) / Float.fromInt(fib(n))` from `examples/data/fibonacci.av` printed `1.618181818181818` on wasm-gc vs `1.6181818181818182` on VM. The cap is now 17 (the IEEE 754 f64 maximum); the overflow guard above it still bails before the trap on large magnitudes, so behaviour for values outside `[0.1, 10)` is unchanged.
- **`Console.print` / `Console.error` / `Console.warn` on `aver compile --target wasip2` now append the trailing newline that VM and `--target wasm-gc` already shipped.** Pre-Lift wasip2's lowering wrote only the string bytes to the wasi output stream, so a sequence of `Console.print("a"); Console.print("b")` came out as `ab` on `aver run --wasip2` and `a\nb\n` everywhere else — multi-line programs on wasip2 produced one jammed line. The `println!` semantic now lives in a dedicated `__rt_println_to_lm` bridge helper called by all three Console.* methods; other consumers of the bridge (`Disk.writeText`, `Http.*` marshalling) stay on the no-newline `__rt_string_to_lm` and are unaffected.

### Examples

- **New `examples/refinement/`** collects the canonical refinement-via-opaque demos: `Natural`, `Positive`, `IntRange`, `NonNegFloat`, `Email`, `BigInt`. Each exercises a different point in the design space.

### Hardening

- **Functions with wildcard `_` parameters (`fn f(_: Int)`) compile and run cleanly on every backend.** Pre-Lift the resolver short-circuited `_` and never claimed a slot for it, so `local_count` stayed at zero while callsites still pushed one value per source-level param — `aver verify` on a program like `fn ignore(_: Int) -> Int = 42` then `ignore(7) => 42` deterministically panicked inside the VM dispatch loop. The resolver now allocates a slot for every param regardless of name; wildcard params still skip the scope map so the body cannot read them, but the frame layout matches what callsites push. Found by `fuzz_verify_runner` AFL nightly.

## 0.21.1 — 2026-05-21

### Verify
- **`aver verify --hostile` now exercises a third axis: execution order.** Every `verify <fn> law` case whose fn contains an `(a, b)!` independent-product gets a twin run in which the branches execute right-to-left, with each result placed back into its source position. A pure law claims its independent products commute, so the twin's tuple must match the forward run; a mismatch surfaces as `verify-hostile-mismatch` with origin `+reverse-eval`. Catches the class of bug where the runtime, the stub map, or a compiler optimisation has snuck a hidden ordering dependency into code Aver was treating as order-invariant. The most common shape it catches: a law that pins a trace event by flat `trace.event(k)` (which indexes the global emission sequence, so reverse-eval moves it) instead of structural `trace.group(g).branch(b).event(k)` (which addresses by source position and stays order-invariant). Same `--hostile` flag — no new CLI surface, no source-language change. New worked example at `examples/formal/hostile_order_axis.av`; the diagnostic's repair message names the right rewrite when it fires.

## 0.21.0 "Iron" — 2026-05-20

> _Iron in the frame — the type checker stops lying to itself about negation, recursion, and identity. A new fuzz harness shakes the rest of the toolchain until things fall out._

### Type checker
- **Duplicate `fn` names in one module are a type error.** Pre-Iron the second definition silently replaced the first — a typo could swap a function's body with no signal.
- **Polymorphic recursion that would need `T := F<...T...>` is rejected.** A shape like `fn nest(v: A) -> Unit; nest([v])` now surfaces as a normal type-incompatibility error instead of silently typechecking with a circular binding that later confused backends.
- **Two same-named types from different modules don't conflate.** A project importing `A.Shape` (`Circle | Square`) and `B.Shape` (`Triangle | Hexagon`) used to silently accept a `B.Shape` value where `A.Shape` was expected. Now rejected with `expected A.Shape, got B.Shape`; the legitimate same-name-same-type case still type-checks.
- **One source error stops at one diagnostic.** `let z = add(unknownFn(1), unknownFn(2))` reported the two real errors plus two follow-on `expected Int, got Invalid` cascades; the recovery sentinel is now treated as a wildcard so callers see only the originating diagnostic.
- **Unreachable `match` arms are rejected.** A second `_ -> ...` (or a repeated `0 -> ...`) used to compile silently — the second arm could never fire because Aver match is first-arm-wins, but nothing flagged it. The type checker now reports `Unreachable match arm: pattern X is already covered by an earlier arm at line N`, naming the covering arm so you can pick which one to delete. Catches duplicate wildcards, identical literal patterns, identical constructor patterns, and structurally-equivalent tuples.

### Parser
- **Deeply-nested expressions return a parse error instead of crashing `aver`.** Sources with 2500+ nested `(...)`, `Option.Some(...)`, `{...}`, chained `-`, chained `?`, or deeply-nested match patterns used to abort the process with `fatal runtime error: stack overflow`. The parser now caps recursion at 64 levels and surfaces `Expression too deeply nested` as an ordinary `parse-error`.

### Runtime
- **`-0.0` keeps its sign bit on every backend.** Pre-Iron `-x` desugared to `0 - x`, and `0.0 - 0.0` collapses to `+0.0` under IEEE 754. Unary minus is now a first-class AST node and every backend negates at the float bit level, so `-0.0` round-trips through compile + eval + replay.
- **`-x` on tight numeric loops is faster.** Typed unary-minus dispatch on the VM joins the typed-arith pipeline (so `-x` runs at the same shape as `+`/`*`/`/` for known-`Int` / known-`Float` operands).
- **Hand-edited compiler inputs surface as compile errors, not panics.** Sources that registered two meanings under one symbol (duplicate variant constructor with conflicting payload, constant rebound under a function name, empty namespace path) used to panic the VM compiler; they now surface as `Compile error: VM symbol 'X' already exists as ...` with a source span.

### Replay
- **`aver replay` accepts every recording shape `aver run --record` writes.** Three places where replay refused or panicked on legitimate input got fixed: recordings touching `Vector` values, single-string-key `Map` recordings (collided with the marker shape), and any JSON containing multi-byte UTF-8 in an unexpected position (the JSON keyword parser sliced `&str` and panicked on `byte index N is not a char boundary`).

### wasm-gc backend
- **`String.fromFloat(x)` doesn't trap at `1e4`–`1e18` magnitudes.** The shortest-roundtrip loop overflowed `i64.trunc_f64_s` and the module crashed with `wasm trap: integer overflow`. The loop now bails one iteration before overflow, accepting fewer fractional digits at very large magnitudes instead of a crash.
- **`Result<Unit, X>` and `Tuple<Unit, X>` compile.** The carrier-equality codegen path had no `Unit` arm and rejected typecheck-clean programs that used these shapes (e.g. `fn animate() -> Result<Unit, String>`).
- **`Console.print` from in-process embedders is captured correctly.** When the host called wasm-gc-compiled code inside a thread-local stdout capture, `Console.print` wrote directly to fd 1 and bypassed the buffer — programs looked silent from the host's view even though they did print. The wasm-gc Console import now routes through the same capture path the VM uses.

### Verify
- **`aver verify` won't hang on a non-terminating case.** A `verify` block whose function had no terminating recursive base case (an easy shape to write by accident, and the one the fuzz harness reproduces every nightly) used to spin in the VM forever. The runtime now caps per-case work at 10M VM opcodes (mirrored as a wasmtime fuel budget on `aver verify --wasm-gc`); over-budget cases surface as `RuntimeError: VM step limit exceeded`, not a hang.

### Tooling
- **Iron ships a multi-target fuzz harness.** Eight AFL++ targets run nightly against `main`: frontend (lex + parse + typecheck), codegen on three backends (wasm-gc / wasip2 / Rust), VM-vs-wasm-gc parity (in-process, ~300 execs/s), verify runner, replay record→replay roundtrip. Plus a custom AST-aware mutator that produces typecheck-clean inputs reaching the codegen + verify layers byte-havoc rarely touches. Every "the parser used to crash" / "the backend rejected a typecheck-clean program" bullet above was found by this harness during Iron development. PR-side cost is zero — fuzz only runs nightly + on manual `workflow_dispatch`.
- **`aver verify` runs on `--wasm-gc`.** Cross-target check — catches divergence between VM and wasm-gc on equality. Pure value-givens and value-hostile cases supported; trace projections, classified-effect Oracle stubs, and `BranchPath` cases reject upfront with an actionable pointer back to the VM verify path.
- **Snapshot regression suites per backend.** `examples/` corpus walks under wasm-gc + wasip2 codegen on every PR (≈0.1s each), Rust codegen + Lean + Dafny build under nightly. A compiler refactor that breaks codegen on a vetted example fails a named test instead of an obscure end-to-end break.

## 0.20.0 "Pulse" — 2026-05-18

> _The same Aver source that opened HTTP both ways in 0.19 now opens raw TCP — connect, send, receive, ping. One pool, one handle shape, every backend the same._

### Added
- **TCP client on `--target wasip2`.** All six methods — `Tcp.connect`, `Tcp.writeLine`, `Tcp.readLine`, `Tcp.send`, `Tcp.close`, `Tcp.ping` — compile and run as components against `wasi:sockets/*`. Long-lived `connect`/`writeLine`/`readLine`/`close` share a 256-slot pool keyed by `"tcp-N"` (same id shape every other backend uses, so cross-backend code stays portable); `send` and `ping` are ephemeral (no pool slot), so a program holding 256 live connections can still issue either. Run with `wasmtime run -W gc=y -W tail-call=y -S inherit-network=y -S allow-ip-name-lookup=y -S tcp=y`, or via `aver run --wasip2`. See [`docs/wasip2.md`](docs/wasip2.md) for the per-method status table.

## 0.19.0 "Echo" (2026-05-13)

> _Aver speaks HTTP both ways now — same source can call out as a client and answer back as a server, the same `.component.wasm` runs on Cranelift and V8._

### Added
- **HTTP client on `--target wasip2`.** All six methods — `Http.get`, `Http.head`, `Http.delete`, `Http.post`, `Http.put`, `Http.patch` — now compile and run as components. Response headers surface as `Map<String, List<String>>`; multi-value headers (e.g. `Set-Cookie`) keep server emit order. Failure messages name the wasi:http error variant (`http: connection-refused`, `http: DNS-timeout`, …) instead of a generic string.
- **`HttpServer.listen` on `--target wasip2 --world wasi:http/proxy`.** Write a handler `fn(HttpRequest) -> HttpResponse`, name it via `--handler <fn>` at compile time (same flag the wasm-gc + Cloudflare path uses), compile to a `.component.wasm`, and serve it with any wasi-http host — `wasmtime serve --addr=:N`, Spin, NGINX Unit, wasmCloud. Request method / path / query / body / headers surface inside the handler as ordinary Aver values; the response's status code, body bytes, and `Map<String, List<String>>` headers round-trip to the client. The same source keeps working under `aver run` because `HttpServer.listen(port, handler)` in `main` is still a real call under VM — it just lowers to a no-op when the wasip2 proxy codegen takes over (which reads the handler identity from the flag, not from `main`). `HttpServer.listenWith` (per-instance context handler) stays deferred for one more iteration.

## 0.18.0 "Span" (2026-05-09)

> _Cross the Component Model boundary the same way Aver crosses the source/wasm one — typed effects in, canonical-ABI imports out._

### Added
- **`--target wasip2`.** WASI 0.2 / Component Model output. Wraps a wasm-gc core module via `wit-component`, lowers Aver effects directly to canonical-ABI WASI imports (no preview-1 adapter, no `--bridge` shim). Emits `.component.wasm` + sibling `.wit`. Effect surface: `Console.{print,error,warn,readLine}`, `Args.get`, `Env.get`, `Time.{unixMs,now,sleep}`, `Random.{int,float}`, all 7 `Disk.*` methods (`exists`/`readText`/`writeText`/`appendText`/`delete`/`deleteDir`/`makeDir`/`listDir`). `Env.set` and `Terminal.*` reject at compile time as structurally absent from WASI 0.2; `Http.*` / `Tcp.*` / `HttpServer.*` deferred to 0.19+. See [`docs/wasip2.md`](docs/wasip2.md).
- **`aver run --wasip2`** — embedded wasmtime + `wasmtime-wasi` runner. CWD preopened as `.` for filesystem effects (matches VM target's path resolution semantics).
- **`tests/wasip2_stress.rs`** — six regression tests covering 100× write+read+delete (resource leak), 50KB write/read roundtrip (chunked-write boundary), 5KB Console.print (chunked-write on stdout side), 200-entry listDir, Random distribution sanity, Time.sleep precision.

### Removed (breaking)
- **`--target wasm` deleted.** The pre-2024 NaN-boxed wasm32 backend (`src/codegen/wasm/`, ~9.5 kLoC) is gone, plus the `wasm-legacy` Cargo feature, the `--bridge {wasip1,fetch,none}` flag, the `aver wasm-runtime` subcommand, and the legacy bundling code in `src/main/commands.rs`. Modern hosts run `--target wasm-gc`; standalone runtimes use `--target wasip2`. `--target edge-wasm` went too — it depended on the deleted `codegen::wasm::emit_wasm_with_adapter`.
- **`BenchTarget::WasmLocal` removed** — bench targets are now `vm` / `wasm-gc` / `wasm-gc-v8` / `rust`.

### Internal
- **`module.rs` / `builtins.rs` / `types.rs` split.** `wasm_gc/module.rs` 5984 → 4197 (extracted `wasip2_helpers.rs`); `body/builtins.rs` 2351 → 1740 (extracted `body/builtins_wasip2.rs`); `wasm_gc/types.rs` 2778 → 1934 (extracted `types_discovery.rs` for `collect_*` AST walkers). Pure code movement, no behaviour change.

## 0.17.3 (2026-05-08)

### Fixed
- **`aver proof` BranchPath prelude gating.** Files with classified effects (`Time.unixMs`, `Random.int`, `Disk.readText`) emit Oracle subtype predicates that reference `BranchPath` (`abbrev TimeUnixMsOracle := BranchPath → Int → Int` in Lean, `predicate IsTimeUnixMsNonneg(f: (BranchPath, int) -> int)` in Dafny). The `union_body` substring check that decides whether to ship the `BranchPath` structure / datatype in the prelude ran BEFORE the subtype block was appended — `AverCommon.lean` and `common.dfy` ended up missing the declaration and the build failed with `unknown identifier 'BranchPath'` (Lean) / `Type or type parameter is not declared in this scope: BranchPath` (Dafny). Fold the subtype block into `union_body` before computing `needed_helpers`. Same one-line reorder in both backends.

### Removed (breaking)
- **`aver run --verify` removed.** Run and verify are two different commands; bundling them forced a guard for every flag combination (`--verify --record`, `--verify --expr`, `--verify --wasm-gc` not yet wired). Callers compose: `aver run x.av && aver verify x.av`. Pre-1.0; no users to deprecate against.

### Added
- **`aver verify --wasm-gc`** runs verify cases via the wasm-gc backend instead of the VM. Cross-target check — catches divergence between VM and wasm-gc codegen on equality. Synthesizes a `__verify_X_check() -> Bool` helper per case and lets wasm-gc lower `==` natively via the per-type eq_helpers registry; the host decodes a single i32 per case. Cross-module `depends [...]` supported. Failure diagnostics render the actual runtime value for primitive return types (Int/Float/Bool/Str) via synthesized `__verify_X_repr() -> String` helpers + the `__rt_string_to_lm` decode bridge. Trace projections (`.trace.*`), classified-effect Oracle stubs (`given X: Time = stub`), and case bodies mentioning `BranchPath` are rejected upfront with an actionable pointer back to VM verify — those depend on namespace-value dispatch and runtime override that the wasm-gc backend doesn't have yet.
- **`aver bench --target wasm-gc-v8`** alongside `wasm-gc-wasmtime` — runs the wasm-gc bytes under V8 (Node 22+ via `tools/wasm-gc-bench-v8.mjs`) so alloc-heavy workloads aren't pinned to wasmtime's GC engine speed.
- **`tests/cross_backend_stress.rs::cross_vector_aliasing_pin_*`** — pins the `Vector.set` aliasing corruption repro across all four backends (VM, legacy wasm, wasm-gc, self-host). Future `ir::alias` relaxation must keep this green or it ships unsound.

## 0.17.2 (2026-05-07)

### Changed
- **VM ~1.46× faster (geo-mean).** Hoist bytecode pointer out of the dispatch loop, fuse `match n { LIT -> … }` into one `MATCH_INT_LITERAL`, typed arith + compare for `Int`/`Float` operands, builtin invoke takes `symbol_id` directly (no per-call hash lookup), profile flag cached. countdown ▼-71%, newtype ▼-42%, match_dispatch ▼-39%, fractal_seahorse ▼-12%.

### Fixed
- **`--target wasm-gc --handler X` (and `--preset cloudflare`).** Three compounding wasm-gc bugs broke every handler-mode compile since 0.16: missing `caller_fn_idx` in handler synthesis, duplicate user/builtin record entries in the rec group, and a data-count snapshot taken before the handler wrapper registered its caller_fn name. `tools/edge` deploys cleanly again.
- **Cloudflare worker stubs** match the wasm-gc ABI (trailing `_caller` i32, `console_*` stubs).

### Added
- **`aver bench --target wasm-gc`** alongside `vm` / `wasm-local` / `rust`.

## 0.17.1 (2026-05-07)

### Removed (breaking)
- **Paren-tuple types `(A, B)` no longer valid.** Type position uses `Tuple<A, B>` only; value literals (`(1, 2)`) and patterns (`(a, b) -> …`) unchanged. Old form errors with `paren-tuple types removed — use 'Tuple<A, B>'`. Migrated ~30 files across `examples/`, `self_hosted/`, `docs/`.

### Fixed
- **Vector / Map aliasing — `Vector.set` / `Map.set` no longer rewrite shared rows.** New `ir::alias` pass flags every slot reachable from `Vector.get` / `Map.get` / `Vector.new(_, compound)` (or `Vector<…>` / `Map<…>` param) on `FnResolution.aliased_slots`. VM skips the `last_use` `mem::take` path on flagged slots; wasm-gc skips clone-on-write when the slot is non-aliased + last-use, falls back to `array.copy` + `array.set` on a fresh array otherwise. Plus wasm-gc now registers nested `Vector<Vector<T>>` from `Vector.new` call sites. Legacy `--target wasm`, `--self-host`, Rust codegen unaffected.
- **`Int.mod` Euclidean across every backend.** Result in `[0, |b|)` regardless of signs — `Int.mod(-7, 3) = 2`. Rust codegen already had it; VM + wasm-gc (per-instantiation `__int_mod_euclid` helper) + legacy wasm (inline `rem_euclid` sequence) now match. Legacy `--target wasm` still traps on `b == 0` (pre-existing); VM / wasm-gc / Rust return `Result.Err("division by zero")`. Fused `Result.withDefault(Int.mod(a, b), default)` sidesteps the trap on every backend.

### Diagnostics
- **`Invalid indentation level` includes a hint** about the most common cause (wrapped fn signatures, multi-line argument lists — both unsupported) and points at the fix (one-line declaration or named helper).
- **Skill / `llms.txt` clarified** for three high-bias-from-other-languages mistakes: `()` is not a Unit literal (write `Unit`), `?` is Result-only (use `Option.withDefault` for Option), `Int.mod` is Euclidean and returns `Result<Int, String>`. Plus `Tuple<A, B>` is the type spelling, paren stays for value/pattern. Skill is the source of truth for `tools/website/llms.txt` (regenerated via `tools/website/build_llms.sh`).

## 0.17.0 "Purge" (2026-05-06)

> _What was never used is no longer in the way._

### Removed
- `Map.empty()` — use `{}`. `Int.parse`, `Float.parse`, `Int.rem` — unreachable aliases. `Int.toString`, `Float.toString` — use `String.fromInt` / `String.fromFloat`. `Float.toInt`, `Int.toFloat` — use `Int.fromFloat` / `Float.fromInt`. `String.concat(a, b)` — use `a + b`. Convention: `Target.fromSource` for conversions, literals for literals, operators for composition, interpolation `"{x}"` for rendering.

### Renamed
- `Vector.toList(v)` → `List.fromVector(v)`.

### Added
- Structural eq + commutative hash for `Map<K, V>` under wasm-gc. Insertion-order-invariant, matches the VM. Works as map K / V, as record/sum field, and as list/vector element.
- caller_fn i32-table replaces per-fn String-ref globals; one `global.get` per call, zero alloc on the hot path. Synthetic `__entry__()` carries `--expr` recordings end-to-end with no JS-side argument encoder.

## 0.16.2 — 2026-05-06

> _Record/replay correctness across all three backends — and a tidier wasm-gc imports tree along the way._

### Added
- **wasm-gc independent products (`?!` / `!`) record/replay parity.** Codegen now emits `enter_group` / `set_branch(i)` / `exit_group` host calls around independent-product literals, so contained effects pick up the same `(group_id, branch_path, effect_occurrence)` tuple the VM annotates. Cross-backend replay (VM → wasm-gc and wasm-gc → VM) round-trips cleanly on `?!` programs. Previously wasm-gc traces were flat and group-tagged VM recordings broke under wasm-gc replay.
- **Self-host real output value comparison.** Self-host's CLI plumbing (`runFromFileWithRest` / `runCliFile` / `runGuestCliProgram` / `finishCliRun`) now propagates the user `main()`'s `Val` up to the wrapping replay scope instead of dropping it to Unit, and the replay-template runtime emits a `__aver_return__:` stdout marker for the host to parse. `aver replay --self-host` now reports a real `MATCH` / `DIFFERS` instead of always claiming MATCH.
- **Playground record/replay runs natively under V8 wasm-gc.** Trace capture and replay used to bounce through the VM-in-wasm32 bridge; the playground now compiles user source to wasm-gc bytes and drives `--record` / `--replay` on a WebWorker via a JS-side `EffectReplayState` mirror of the CLI host. Trace JSON is byte-compatible with `aver run --record`, so a downloaded `.replay.json` from the playground replays under the CLI replayer (and vice versa). Independent-product (`?!`) markers are wired so cross-backend traces match end-to-end. `--expr` per-fn recordings (`add(7, 35)`) ride the same path: the compiler injects a synthetic `__entry__()` fn that wraps the call with literal args, `_start` is wired through it, and the recording's `entry_fn` reflects the user-facing target — no JS-side argument encoder, no VM-in-wasm32 fallback.
- **`caller_fn` stamped on every recorded effect under wasm-gc.** Trace events now carry the originating Aver fn name (`"caller_fn": "renderRoom"` instead of the universal `"main"`); the playground trace panel and CLI dumps show real per-function labels. One shared global per effect-emitting fn, init at instantiation — hot path is a single `global.get` per call, zero alloc.
- **Playground compiler trimmed of dead VM-in-wasm32 paths.** Two cuts: the legacy NaN-boxed wasm32 emitter (`codegen::wasm`, gated behind a new `wasm-legacy` Cargo feature that the CLI's `wasm` feature pulls in but `playground` does not — so `--target wasm` / `--bridge {wasip1,fetch}` keep working on the CLI), and the unused `aver_run_record` / `aver_run_record_entry` / `aver_replay_run` wasm-bindgen bindings plus their `run_record_project*` / `replay_run_project` Rust hosts (record/replay now runs natively under V8 wasm-gc on a WebWorker — see the playground bullet above). `aver_bg.wasm` shrinks 4811 KiB → 4556 KiB after `wasm-opt -Oz` (-255 KiB / -5.3% on first-load).

### Changed
- **`run_wasm_gc/imports.rs` split into 13 per-domain submodules** (`args.rs` / `console.rs` / `disk.rs` / `env.rs` / `factories.rs` / `groups.rs` / `http.rs` / `lm.rs` / `numeric.rs` / `replay_glue.rs` / `tcp.rs` / `terminal.rs` / `time.rs`). The 1711-line dispatch monolith is now a 101-line chain that hands off to per-namespace `dispatch(...)` functions; new effects live next to their decoders and factories.

## 0.16.1 — wasm-gc record/replay parity (2026-05-05)

### Added
- **`aver run --wasm-gc --record <dir>`** + **`aver replay <recording.json> --wasm-gc`** — wasm-gc joins the VM and self-host as a first-class record/replay backend. Replay short-circuits real I/O on every effect, compares the entry-fn return against the recorded `output` with the same `MATCH` / `DIFFERS` shape the VM replayer prints. Recordings are interchangeable across all three backends — a trace written by any one of them replays cleanly under any of the three.
- **`aver run --wasm-gc -e 'fn(args)' --record`** — record a specific function call instead of `main`, parity with the VM `--expr` flow. Literal args (Int / Float / Bool / String / Unit) are encoded into `recording.input` and re-fed at replay; the recorded `output` carries the actual decoded return value.

## 0.16.0 "Anneal" — 2026-05-05

> _Heat-treat the internals — every backend now agrees what `Console.print` is, what's a type, and that engine GC beats hand-rolling._

### Breaking changes
- **`Console.print` / `Console.error` / `Console.warn` / `Terminal.print` / `Terminal.setColor` now take `String`.** The previous polymorphic signature (a `Printable` type variable that accepted any value and let the runtime stringify per-type) is gone. Stringification is the caller's job — use interpolation `"{x}"` for primitives, write a per-type render helper for compound shapes. Programs that called `Console.print(record)` / `Console.print(some_list)` will fail typecheck with `Argument 1 of 'Console.print': expected String, got <Type>` and the message points at the call site to migrate. Every backend (VM, legacy wasm, wasm-gc, Rust, self-host) loses its per-type format dispatch; replay/record stores plain string bytes per call; the effect ABI becomes trivial across hosts.

### Added
- **`--target wasm-gc`** — native WebAssembly GC + tail-call output, the recommended WASM target. No NaN-boxing, no boundary GC framing, no inline runtime. Per-instantiation monomorphisation for `Vector<T>`, `List<T>`, `Map<K, V>`, `Option<T>`, `Result<T, E>`, `Tuple<A, B>` (incl. nested paren tuples like `(Int, (Int, Int))`). Full Aver type parity: tuples, cross-collection nesting, sum-type and record map keys, all 12 `Terminal.*` effects, multi-binding variant patterns, `?` / `?!` / Result-Unit / Tuple-Unit shapes, generic constructors driven by bidirectional inference. Wasm-gc wins V8 microbenches on alloc / recursion / collection workloads (vector_ops 269×, map_build 5.67×, record_access 3.37×); `--optimize size` wins 19-32 % across the example games.
- **`aver run --wasm-gc`** — embedded wasmtime executor with engine GC + tail calls. Full effect surface wired against `aver_rt::*`: Args, Console (incl. `readLine`), Time, Random, Float math, Terminal, Disk (read/write/append/exists/delete/listDir/makeDir), Env, Tcp (connect/writeLine/readLine/close/send/ping), Http (get/head/delete/post/put/patch). Multi-module entries auto-resolve their module root from the entry file's `depends [...]`, so `aver run --wasm-gc projects/payment_ops/main.av` works without `cd` or `--module-root`. Output matches the VM byte-for-byte modulo time/randomness on every audited core / data / games / services / apps / modules / playground sample.
- **Browser playground migrated to `--target wasm-gc`.** Engine GC replaces the `aver_runtime.wasm` sidecar; binaries are self-contained. Modern-browser baseline (Chrome 119+ / Firefox 120+ / Safari 18.2+).
- **`--target wasm-gc --optimize size`** — wasm-opt pipeline passes `--enable-gc --enable-reference-types`, factory exports + LM transport survive `wasm-metadce`, per-program binaries shrink 19-32 %.
- **`--preset cloudflare` migrated to wasm-gc.** Expands to `--target wasm-gc --pack cloudflare` and requires `--handler <fn>`. Smaller wasm + smaller `worker.js` than the legacy `--target wasm` bundle, no `wasm-merge` dependency.

### Changed
- **Soundier internals across the type system.** `Type::Unknown` / `Type::Any` removed from the static type enum: generic builtin positions use named `Type::Var(K|V|T|E)` resolved per call site by `match_expected_type` substitution; error recovery uses `Type::Invalid` (never compatible with concrete types). Bidirectional inference drives expected types into generic constructors and collection literals — `Map.empty`, `Option.None`, `Result.Ok/Err`, empty list literals all type without backend recovery shims, including in tail-call argument position. `Char.fromCode` accepts the full Unicode range (Doom Braille, etc.).
- **Resolver is arm-scoped.** Pattern bindings live per arm (`MatchArm.binding_slots`) instead of a function-flat name → slot map. Two arms can share a binding name with different field types — `recordedAt(event: TaskEvent)` reuses `at` across `TaskCreated.at: String` / `CommentAdded` (where `at` projects from a record), `serializeTaskEvent` reuses `deadline` across `TaskCreated.deadline: Option<String>` and `DeadlineSet.deadline: String` — and each arm's slot is allocated independently. The resolver runs as a single-pass walk with a scope stack so ident lookup respects shadowing.
- **Slot types lifted into IR** as `FnResolution.local_slot_types` — every backend that needs typed locals consumes the same table instead of re-walking patterns. Sibling sumtypes sharing a bare variant name (e.g. `Query.ProviderSummary` vs `QueryOutput.ProviderSummary` in payment_ops) are disambiguated by `(parent, bare)` keys throughout resolver and wasm-gc registry. Multi-arm tuple-of-Constructors matches and chained-Attr value access (`Domain.Types.TaskStatus.Blocked`) flatten through one shared rewrite path used by both VM and wasm-gc.

### Coverage
- Of the 71 single-file or entry-point examples + project mains, 68 run identically to the VM under `aver run --wasm-gc` (every `examples/{core, data, games, formal, wasm, services, apps, modules}` and `projects/*` plus 13 of 14 playground samples). The remaining 3 are intentionally bad code that *show* compile errors (`examples/diagnostics/{lint_demo, test_errors}.av`, `tools/website/playground/sources/examples/effect-violation.av`).

### Removed
- **Per-version runtime artifacts (`tools/website/runtime/`, `averlang.dev/runtime/`).** Wasm-gc binaries are self-contained — there's no shared `aver_runtime.wasm` to fetch and cache, so the versioned CDN tree, the `latest/` pointer, and the `release.py` builder that emitted them all go. The legacy `--target wasm` bundle inlines its own runtime via `wasm-merge` and doesn't rely on the CDN either.
- **`--bridge` rejected under `--target wasm-gc`.** The legacy `--bridge fetch | wasip1 | none` axis is bound to the legacy NaN-boxed backend. Mixing it with wasm-gc was previously silently ignored — now the compiler errors with a hint pointing at the correct shape: `--handler <fn>` (and `--preset cloudflare --handler <fn>`) for HTTP, the planned `--target wasip2` for standalone-WASI deployments. wasm-gc skips preview 1 by design — porting the legacy `aver_to_wasi.wasm` shim to GC string types would re-implement an ABI we want to leave behind.

### Roadmap
- **`--target wasip2`** (Component Model output, `wasi:http/proxy` + `wasi:filesystem` + `wasi:sockets`) is the modern wasm-gc companion for standalone-runtime deployments. Lands in 0.17. Once shipped, `Http.*` / `HttpServer.listen` / `Disk.*` / `Tcp.*` all become ✅ on the standalone-WASI side instead of stubbing.

## 0.15.2 "Traversal" — observability lands, regression gate closes (unreleased)

> _Every pipeline pass exposes its decisions through one typed shape; bench reports gain the metrics that make regressions catchable; CI gates them per-host without per-runner branching._

### Added
- **`aver compile --explain-passes [--json]`** — typed per-pass diagnostic report (TCO conversions, interpolations lowered, fusion sites + sinks fired, slots resolved, alloc/recursion facts). JSON shape is one `{stage, data}` block per pass with stage-specific fields; `schema_version: 1` pins the contract for CI gates (`tests/explain_passes_spec.rs`).
- **`aver bench` populates `compiler_visible_allocs`** (IR-level via `NeutralAllocPolicy`, same across all three targets) and **`response_bytes`** (`vm`: rendered return value via `aver_display`; `wasm-local`: `fd_write` iovec sum; `rust`: subprocess stdout). `--compare` gates both as exact-match alongside p50/p95.
- **`aver bench --baseline-dir DIR`** auto-picks `<host.os>-<host.arch>-<backend.name>.json`; silent skip when no match. Directory mode supports `--save-baseline` (NDJSON, same shape `--baseline-dir` reads) — `bench/baselines/macos-aarch64-vm.json` ships as the reference.
- **GitHub Actions `Bench Gate` job** runs `aver bench bench/scenarios/ --target=vm --baseline-dir bench/baselines/ --fail-on-regression` on every PR; results upload as 30-day artifact.

### Changed
- **`PipelineResult.buffer_build`** is now a typed `BufferBuildPassReport` (sinks, synthesized fns, per-sink rewrite counts) instead of an opaque `(usize, usize)` tuple.
- **`aver check`** no longer prints the `↻ N buffer-build sink(s) […]` summary — same data with richer detail is now in `aver compile --explain-passes`.

## 0.15.1 "Traversal" — pipeline foundation + observability (2026-05-01)

> _The compiler grew a real pipeline. Every IR transform is a named ordered stage, every consumer reads derived facts from one place, every backend's relationship to the IR is a documented contract._

### Added
- **Ordered 7-stage pipeline** (`tco → typecheck → interp_lower → buffer_build → resolve → last_use → analyze`) as the single source of truth, with per-stage public functions and per-stage `PipelineConfig` flags.
- **`Analyze` stage** centralises five ad-hoc IR analyses (alloc info, mutual-TCO membership, recursive fns, callsite counts, body classification) into one `PipelineResult.analysis` codegen reads from.
- **`LastUse` stage** split out of the resolver — `<name:last>` markers in IR dump show which slots VM/Rust/WASM MOVE instead of COPY.
- **`aver compile --emit-ir-after=PASS`** prints the IR snapshot after any pipeline stage; `diff -u` between two stages shows exactly what each pass rewrote.
- **`aver bench` scenario harness** with three targets (`vm` / `wasm-local` / `rust`), TOML manifests, NDJSON for batch runs, `--save-baseline` / `--compare` / `--fail-on-regression`, and `backend` + `host` identity blocks in every report. Thirteen scenarios shared with `cargo bench` via `include_str!`. See [docs/bench.md](docs/bench.md).

### Fixed
- **Proof export traversal leak.** Dep modules ignored the proof-export flag and ran deforestation anyway, leaking synthesized `<fn>__buffered` variants into Lean/Dafny output.
- **Playground bypassed deforestation.** Browser-compiled WASM/Rust artifacts were measurably slower than equivalent CLI compiles. All playground compile / replay / record paths now use the canonical pipeline.

### Changed
- **`PipelineConfig` is per-stage booleans.** No bundled `apply_traversal_lowering` knob; `stop_after` removed. Caller-level proof-vs-runtime distinction translates to per-stage flags at the CLI boundary.
- **`pipeline::*` is the single entry point** for IR transforms and analyses — direct calls to `tco::transform_program` / `resolver::resolve_program` / `run_type_check_full` etc. are gone from production and tests.
- **Codegen consumers read derived facts from `PipelineResult.analysis` / `CodegenContext`** instead of recomputing. Aver's module DAG makes per-module unions sound (cross-module SCCs are mathematically impossible — pinned in `src/ir/analyze.rs` doc and memory).
- **VM compile API consolidated.** All three entry points take `analysis: Option<&AnalysisResult>` as a peer parameter; no `_and_analysis` variant.

### Pipeline contract (new)
- **WASM and Rust codegen no longer handle `Expr::InterpolatedStr`** — `interp_lower` is now a mandatory predecessor stage. ~100 lines of dead code + helper enums deleted, replaced with `unreachable!()` + contract comments.
- **VM keeps its `compile_interpolated_str`** — the REPL is the only legitimate consumer of pre-lower IR.

## 0.15.0 "Traversal" (2026-04-30)

> _The compiler now eliminates intermediate list traversals it can prove are consumed once — `String.join` builders, interpolation chains, and external-reverse pipelines fuse to direct buffer writes across all three execution backends._

### Added
- **Buffer-build deforestation across VM, WASM, and Rust.** `String.join(<recursive_prepend_builder>(args, []), sep)` no longer materialises the intermediate `List<String>`. The IR-level pass detects the canonical sink shape (`match cond { true -> List.reverse(acc); false -> recurse(... List.prepend(elem, acc)) }`), synthesizes a `<sink>__buffered` variant that threads a mutable `Buffer` through the tail-call chain, and rewrites matching call sites to `__buf_finalize(<sink>__buffered(args.., __buf_new(8192), sep))`. Each backend then lowers the four `__buf_*` intrinsics: VM dispatches to dedicated bytecode opcodes (`BUFFER_NEW`/`APPEND_STR`/`APPEND_SEP_UNLESS_FIRST`/`FINALIZE`) backed by a `Vec<Option<String>>` pool on the VM struct; Rust uses `String::with_capacity` + `push_str` (with `Buffer = String` aliased in `aver-rt`); WASM uses an `OBJ_BUFFER` heap object and `rt_buffer_*` runtime helpers. Local benches: 8× speedup on Rust (`buildLines(500_000, []) → String.join`), 2.7× on VM, 24% on WASM under workerd.
- **String interpolation lowers through the same buffer pipeline.** `"a${x}b${y}c"` no longer chains `str_concat` calls — every interpolation goes through a new IR pass (`src/ir/interp_lower.rs`) that desugars to `__buf_finalize(__buf_append(... __buf_new(N), __to_str(x)))`. Coercion of non-string parts uses a new `__to_str` intrinsic (VM: `CONCAT` against empty, Rust: `aver_rt::aver_display`, WASM: existing `emit_value_to_str` host bridge). Universal speedup — every Aver program with f-strings benefits, not just the niche of recursive `String.join` builders. Old O(N²) chained-concat shape (each step allocates a string of cumulative length) becomes O(N + total_len) buffer-write.
- **External-reverse builder shape.** The buffer-build detector now also matches sinks shaped `match list { [] -> acc; [head, ..rest] -> recurse(rest, List.prepend(elem, acc)) }` — common in payment_ops / workflow_engine codebases under the `*Into` naming convention (`serializeEntriesInto`, `filterSubjectInto`, `renderEventItemsInto`). Call sites spelled `String.join(List.reverse(<sink>(args, [])), sep)` fuse against these. Per-shape kind alignment + acc-position empty-list precondition checked the same way in detection and rewrite, so `aver check` can't over-report sites the rewrite would refuse.
- **Antipattern lints (`aver check` traversal warnings).** Three patterns the deforestation pass deliberately *doesn't* fuse get surfaced as warnings instead — the fuse-vs-warn split says: silently fuse only when Aver has no idiomatic alternative; warn when it does. Flagged: `Vector.fromList(<sink>(args, []))` (suggest `Vector.new(N, default)` + `Option.withDefault(Vector.set(v, i, x), v)` owned-mutate fast path), `Map.fromList(<sink>(args, []))` (suggest `Map.empty()` + `Map.set` chain), and standalone `List.reverse(<sink>(args, []))` whose result isn't fed straight into a `String.join` (the fusion path covers that case; the lint catches sites where the wrapper is something else like `renderLines` and fusion can't chain through). Acc-position-empty-list precondition checked the same way the rewrite checks it, so the warning never offers broken advice.

### Fixed
- **Dead HAMT pointer walk in WASM boundary GC.** The HAMT runtime was replaced by the flat `OBJ_MAP` (kind=12) earlier, but the kind=13/15 pointer-walks for `HAMT_NODE`/`HAMT_COLLISION` stayed in `collect_end` and `retain_i32`. They never fired on main because no live object used those kinds — but Phase 2c reuses kind=13 for `OBJ_BUFFER`, and the stale walk was treating buffer payload bytes as inner pointers, calling `rt_retain_i32`/`rt_rebase_i32` on garbage. On wasmtime the pseudo-pointers happened to land outside the collect range and were silently passed through; on Cloudflare Workers v8 the layout differed and they corrupted memory mid-collect, surfacing as `aver_http_handle threw: RuntimeError: memory access out of bounds`. Removing the dead walks fixes it.
- **`Result.withDefault` / `Option.withDefault` / `Option.toResult` first-arg validation.** All three combinators were registered with `Type::Unknown` parameters in `src/types/checker/builtins.rs`, so `Result.withDefault(Vector.get(v, i), 0)` (Vector.get returns `Option<T>`, not `Result<T, E>`) compiled cleanly through `aver check` and silently returned the default at runtime — every lookup folded to the fallback value with no error surfaced. The special-case handlers in `infer/expr.rs` already had the right return-type inference but skipped argument validation; they now emit a real type error when the first argument is not the expected wrapper. `Type::Unknown` still flows through as escape hatch for genuinely polymorphic returns.
- **`HttpServer.listenWith` context-handler type linkage.** The second argument (user-defined context) and the handler's first parameter must share the same type — in the `weather.av` style, both are `WeatherContext`. The builtin sig leaves context as `Type::Unknown` (Aver builtins don't carry parametric polymorphism), so the linkage is enforced as a cross-arg check after the standard sig-based validation: extract the handler's `Type::Fn` first param, compare with the inferred type of the context arg, emit a type error on mismatch. Same logic covers `SelfHostRuntime.httpServerListenWith`.
- **`SelfHostRuntime.httpServerListen{,With}` handler typing.** Both aliases declared their handler parameter as `Type::Unknown`, so callers could pass an `Int` where a `Fn(HttpRequest) -> HttpResponse` was expected and the compile would proceed past type-check. Tightened to `http_handler()` / `http_handler_with_context()` to mirror the public `HttpServer.listen{,With}` shape.

## 0.14.2 (2026-04-29)

### Added
- **Pure no-alloc fast path on both WASM and VM.** New shared `compute_alloc_info` analyzer (`src/ir/alloc_info.rs`) parametrised by a backend-specific `AllocPolicy` trait classifies every user fn as allocating or not, walking the call graph to fixpoint. The WASM emitter then skips the prologue `heap_ptr` save and epilogue `rt_truncate` for no-alloc bodies; the mutual-TCO trampoline drops its watermark check, per-iter heap_ptr save, and adaptive compaction when every member is no-alloc. The VM compiler tags qualifying chunks so `TAIL_CALL_KNOWN` / `TAIL_CALL_SELF` skip `finalize_frame_locals_for_tail_call`, `CALL_KNOWN` parks dummy zero marks, and `RETURN` short-circuits the standard fast-return path. Mandelbrot bench (160×96 × 100 reps × 80 iter, mutual-TCO `mandelStep ↔ mandelIter` with cardioid + period-2 bulb pre-tests) runs ~4× faster on WASM and ~22% faster on VM. Closes the bulk of the float-tight-loop gap with peer typed-wasm + Cranelift toolchains (down from ~3× slower to ~1.07× slower, apples-to-apples).
- **`HttpRequest.query: String` field** for the fetch bridge. `req.query` returns the URL search string without the leading `?` (previously dropped silently — `req.body` worked but `req.query` was simply absent from the builtin record). Plumbed through `builtin_records.rs`, `abi.rs` (`Request.query` → `request_query`), the fetch-bridge field-emit map, the cloudflare `worker.js` template, and the type-checker's net-request field list. Enables URL-driven server-side state without an extra parser layer.
- **Friendly compile-time error for HTTP types under non-fetch bridges.** `aver compile … --target wasm` (no bridge) on a program declaring `fn handler(req: HttpRequest) -> HttpResponse` previously produced invalid bytecode (`type mismatch: expected i64, found i32` deep in the record-shape codegen). The emitter now rejects HttpRequest/HttpResponse parameters/returns under any non-fetch adapter before emit, naming the offending fn and pointing at `--preset cloudflare` / `--bridge fetch`.
- **`/fractal` on `edge.averlang.dev` becomes interactive.** Replaces the previous static Braille pair with a single-panel 200×120 half-block Mandelbrot driven by URL query params (`cx`, `cy`, `w`). Per-pixel 16-step colour palette (each cell `<i c=XX>▀</i>` packs top/bot palette indices into one 2-hex attribute, ~38% smaller HTML than per-cell two-class form). Pan / zoom / reset buttons + six landmark presets (seahorse, mini-Mandelbrot, double spiral, elephant valley, tendrils, eastern bulb) all encode their target view in the URL — every navigation fires a fresh handler invocation with `cache-control: no-store`, no precomputed images. Iter cap auto-scales (100 / 150 / 250) so deep zooms stay sharp at modest extra compute. Production warm TTFB ~35-40 ms (keep-alive); first hit ~70 ms with TLS handshake.

### Changed
- **WASM mutual-TCO trampoline collapses to a flat shared slot row when every member has an identical typed signature and is no-alloc** (`mandelStep ↔ mandelIter` style). Per-tail-call args reshape skips its `eval → tmp → target` double-copy under those conditions and writes args directly into target slots in reverse stack order. Eliminates ~10 `local.set` per iter from the trampoline hot loop.

### Docs
- `docs/wasm.md` — reframed `aver.toml` policy as a deliberate host concern, not a missing feature. New "Policy is the Host's Job" section cites wasmtime/WASI `--allow-net`, Cloudflare Workers `services`/`fetch` bindings, browser CSP `connect-src`, and Fastly Compute backend whitelists as the actual enforcement layers; build-time concerns (effect declarations, deterministic mocks, independence invariants) stay with us.

### Tooling
- `tools/release.py` — GitHub release titles now carry the codename for thematic releases (`Aver 0.14.0 "Edge"`); patches stay plain. Cascade-bump logic tightened to fire only for genuine publish-blockers (`aver-rt → aver-memory`); `aver-lsp` no longer auto-bumps just because `aver-lang` does.

## 0.14.1 (2026-04-29)

### Removed
- **`Header` record.** 0.14 standardised HTTP headers as `Map<String, List<String>>` across every backend; the per-entry `Header { name, value }` record left over from the old `List<Header>` shape was unreachable from any built-in HTTP type and is now retired. User code that constructed `Header` literals must now build a `Map<String, List<String>>` directly.

### Fixed
- **`Http.post` / `Http.put` / `Http.patch` oracle signature** now reports the headers parameter as `Map<String, List<String>>` instead of the stale `List<Header>` from the pre-0.14 shape. Verify-mode mocks and effect classification see the correct type.
- **Self-host codegen template** (`--with-self-host-support`) emitted `headers: AverList<Header>` for HTTP request/response handling. Now emits `headers: HttpHeaders` (= `AverMap<AverStr, AverList<AverStr>>`), matching the runtime shape every other backend already used.

### Docs
- `docs/services.md`: `Http.*` signatures and `HttpRequest` / `HttpResponse` records updated to the `Map<String, List<String>>` headers shape.

## 0.14.0 "Edge" (2026-04-29)

> _Aver's WASM backend becomes a deployable edge target: small user modules, a shared runtime, and explicit host bridges._

### Added
- **`--target edge-wasm`** emits a thin `user.wasm` that imports a separately hosted `aver_runtime`, so browser and edge deployments can cache the runtime once.
- **Host bridges:** `--bridge fetch` for JS/Workers-style hosts and `--bridge wasip1` for standalone WASI preview 1 execution.
- **Cloudflare Workers pack output (`--preset cloudflare`)** — drops `worker.js` (ES-module bootstrap that wires `aver/*` host imports against `console.*` / `Date.now()` / `Math.random()` / Fetch + JSPI for `Http.*`) and `wrangler.toml` next to a single bundled `user.wasm`. Cloudflare Workers reject `WebAssembly.instantiate(bytes, …)` from runtime-fetched bytes, so the preset uses `--target wasm` (runtime inlined via `wasm-merge`) for static-import shape; `--target edge-wasm` stays for browsers / Deno / Bun where the runtime CDN at `averlang.dev/runtime/` works. Runtime artifacts, checksums, and manifest are published independently under `/runtime/`.
- **WASM host coverage for `Env.*`, `Console.warn`, `Http.*`, request/response headers, and multi-value header flow.**
- **WASM `Map` runtime** — flat hashtable with structural hashing/equality across all hashable key types (Int, Float, Bool, String, heap), O(1) `Map.len`, owned-mutate fast path on `Map.set` when last-use analysis proves sole ownership. `map build 5k` bench: WASM 963 µs vs VM 1.33 ms. Replaces the prior placeholder linked-list shape that was O(N²) on TCO build patterns.
- **`--optimize size|speed`** for the WASM optimization pipeline.
- **Runtime artifacts at `/runtime/`**: `aver_runtime.wasm`, `aver_to_wasi.wasm`, WAT companions, checksums, versioned URLs, and `latest/`.

### Changed
- **HTTP headers are `Map<String, List<String>>`** across request/response records and `Http.post` / `Http.put` / `Http.patch`.
- **WASM map ABI is polymorphic**, with key kind and value pointer flags passed explicitly.
- **WAT is the source of truth for the standalone runtime**, and emit-time WASM validation is part of the compile path.
- **WASM `Vector.set` is O(1) in TCO build loops** via owned-mutate dispatch — fused `Option.withDefault(Vector.set(v, i, x), v)` where `v` is a last-use slot lowers to an inline bounds check + `i64.store`. Same trick the VM uses; matches its perf profile on `vector get/set 5k`.

### Removed
- **`--adapter` → `--bridge`** and **`--wasm-opt oz|o3` → `--optimize size|speed`**.
- **`--target wat` and `--target wasm+wat`**; use standard WASM tooling for WAT output.

### Fixed
- WASM `Map<Int|Float|Bool, V>` validation and lookup issues.
- VM map structural equality for heap keys.
- Cloudflare/fetch bridge response headers no longer get dropped.

### Known limitations
- `Http.*` under `--bridge wasip1` returns a transport error; real WASI HTTP belongs in a later Component Model target.

## 0.13.0 "Limit" (2026-04-27)

> _Pure core. Explicit shell. Auditable boundary. Aver learns to say no — at the module's edge, to extra calls, to hostile worlds._

### Added
- **`aver verify --hostile` / `aver audit --hostile`** — runs every `verify <fn> law <name>` block under an adversarial expansion of its `given` clauses. Typed value domains (`given x: Int = [3]`) get augmented with the per-type boundary set (`0`, `1`, `-1`, `i64::MIN`, `i64::MAX`; for `Float` add `±Inf` and `NaN`; for `Str` add empty / NUL-embedded / multi-byte / 1024-char). On top of that, classified effects get multiplied by adversarial profiles (frozen / fast-forward / backward clocks, always-min / always-max / alternating random, network-down responses, empty / always-error filesystem, …) — `given` for an effect is just the worlds *you* listed, hostile mode adds more on top, and `law` form already quantifies universally over every stub. Failures that surface only here mean the law isn't universal — either weaken it with `when <precondition>` to scope it to the worlds where it actually holds, downgrade from `law` to `verify <fn>` cases-form when the claim is really stub-specific, or accept the profile as a real production world the impl should handle.
- **Module-level effect boundary** — `effects [...]` declaration on the module header. Every function's `! [Effect]` must be covered by the module's declared surface, enforced at type-check. A namespace entry like `Disk` admits any `Disk.*` method; method-level `Disk.readText` admits only that one. Modules with functions but no `effects [...]` get a soft warning nudging them to declare the boundary; you opt in when you want the enforcement.
- **`when` over oracle stubs (oracle assumptions).** In `verify <fn> trace law <name>`, the `when` predicate may now reference an effect-given oracle directly: `when clock(BranchPath.Root, 1) > clock(BranchPath.Root, 0)` declares "this law assumes a monotonic clock". Hostile profiles that violate the assumption are skipped; profiles that respect it run normally. Read-your-writes, conservation, idempotent fetch, protocol order — invariants you used to leave in comments now live in source. Skipped count is shown alongside passed / failed; if every adversarial profile is skipped, you get a vacuous-under-hostile warning so the law isn't silently uncovered.
- **`.trace.count(method)`** — quantitative trace assertion. Returns the number of trace events whose method matches the argument (`Console.print`, `Http.get(...)`, etc.). Complements `.contains` (boolean any-match) with the count form so laws can pin "this fn calls the API exactly once" or "no extra disk reads under hostile profiles".
- **`verify-hostile-mismatch` diagnostic slug** — distinct from the regular `verify-mismatch`, so CI gates can route declared-world regressions and adversarial-world surfacings to separate channels: `jq '.diagnostics[] | select(.slug == "verify-hostile-mismatch")'`. Each carries `from_hostile: true` and an `origin` field with the profile label (`hostile effect profile: Time.unixMs/saturated`, `hostile boundary expansion`).
- **Oracle invariants in proof export — documented and enforced.** The trust header in every `aver proof --backend lean` / `--backend dafny` artifact spells out the per-effect bounds the runtime guarantees: `Random.int` stays in `[min, max]`, `Random.float` in `[0.0, 1.0]`, `Time.unixMs ≥ 0`, `Disk.exists` is total. Lifted theorems quantify over subtype-encoded oracle types (`RandomIntInBounds`, `RandomFloatInUnit`, `TimeUnixMsNonneg`) — the bound is a constrained quantifier at the lemma level, not a free side-condition you have to discharge separately.
- **Playground hostile toggle** — checkbox next to the Audit button. When checked, audit runs the hostile pipeline; the Verify panel and the structured JSON output carry the same dual-run breakdown (declared vs hostile, with the per-block scorecard).
- **Showcase example: hostile clock.** `examples/formal/hostile_clock.av` (and the Oracle dropdown in the playground): a deadline check that passes under real time and breaks under the saturated-clock profile — concrete demo of what `--hostile` finds that plain `verify` misses.
- **Dual-run breakdown in TTY and JSON.** Per-block summary line shows `(1/1 declared, 11/35 hostile, 3 skipped by `when`)` so you can see at a glance whether a regression is in the declared world or only surfaces under adversarial profiles. JSON output adds four counters on `verify_summary.blocks[]` — `declared_passed`, `declared_failed`, `hostile_passed`, `hostile_failed` — for tooling that wants to split "law regression" from "hostile coverage gap".
- **Cartesian cap.** Hostile expansion stops at 10,000 cases per block (same ceiling the parser uses for declared `given` ranges). Over-budget blocks get a clear error pointing at the law and listing the projected size; tighten the `given`, add a `when`, or run that block without `--hostile`.

## 0.12.0 "Atlas" (2026-04-25)

> _Multi-module Aver projects export to Lean and Dafny end-to-end. Generated proofs shrank by ~85% on pure-math examples. All 42 canonical examples now pass both proof backends._

### Added
- **Multi-module proof export.** Projects with `depends [...]` produce one `.lean` / `.dfy` file per module, plus a shared `AverCommon` library carrying only the helpers your code actually references. Submodule paths like `Models.User` land at `Models/User.{lean,dfy}`. Works on rogue, doom, tetris, checkers, notepad — every multi-module example in the repo, including those mixing same-named fns across modules and modules sharing a name with a record.
- **`?` operator now works in pure proof export.** Previously `cmd = parseCommand(x)?; <rest>` produced uncompilable proof code; now correctly lowers to `match … { Result.Ok(cmd) -> <rest>; Result.Err(e) -> Result.Err(e) }` before reaching Lean / Dafny. Fixes mission_control.
- **Concrete `Float` evaluation in Lean proofs.** `Float.floor`, `Float.round`, `Float.ceil`, `Float.toInt` now match the runtime exactly (IEEE 754 + saturating cast, including NaN → 0 and ±∞ → i64 bounds). `verify` cases over Float values get real `native_decide` evaluation in Lean instead of opaque stubs. Edge-case behaviour is asserted by `native_decide` proofs that re-run on every Lake build.
- **`Float.sin`, `Float.cos`, `Float.atan2`** are now proof-exportable across both backends, not just runtime. doom's raycaster compiles to Lean / Dafny.
- **`Terminal.size` is now a verifiable effect.** Oracle signature `() -> Terminal.Size`, same shape as `Args.get` / `Env.get`. Example: `examples/formal/terminal_size_snapshot.av`.
- **Playground download menu.** The ⬇ button now expands into Aver source / WASM binary / Rust project / Lean 4 proof / Dafny proof — your in-browser source compiles and ships as `.av` / `.wasm` / `.zip`.

### Changed
- **Generated proof files shrank dramatically.** Lean and Dafny now emit only the helpers, records, and trust-assumption headers your code actually references — no more 1500-line preamble for a 20-line pure-math file. Calculator and pure-laws examples shrank ~85-90% in Lean, ~70-90% in Dafny; effectful examples 25-40%.
- **Shared backend infrastructure.** Built-in records, helpers, and per-module emission logic live in single shared modules consumed by Lean / Dafny / WASM. Adding a new built-in record shape is one line in `codegen::builtin_records`.
- **`aver context` output is denser (schema_version: 7, breaking).** Markdown signatures now use the Aver source form `name(args) -> Ret ! [Effects]` instead of a separate `effects:` line. JSON `records.fields` and `types.variants` are pre-formatted strings (`["id: String", ...]`, `["IngestWebhooks(String, String)", ...]`) instead of nested objects, and arrays are rendered inline (`[a, b, c]`) while objects stay multi-line. Net effect on the showcase projects: workflow_engine fits 78/131 elements at 24kb (was 47/131); payment_ops fits 98/98 at 32kb with room to spare. Anything parsing CONTEXT.json should switch from `fields[i].name` / `fields[i].type` access to `split(": ")` on the string.

## 0.11.0 "Oracle" (2026-04-24)

> _Effectful functions get verified now — bind an oracle with `given`, check the trace, or export the universal law to Lean & Dafny. Dafny caught up with Lean across every recursive shape._

### Added
- **Oracle law and trace docs/example** — `docs/oracle.md` now separates proof-oriented `verify <fn> law` over explicit oracles from cases-form `verify <fn> trace` for `.result` / `.trace.*` assertions. Added `examples/formal/oracle_trace.av` as the short runnable example.
- **Broader Oracle effect classification** — Oracle now covers CLI input (`Console.readLine`), disk operation/result effects, one-shot TCP (`Tcp.send` / `Tcp.ping`), `Time.sleep`, and terminal trace/input calls that do not depend on modal terminal state.
- **Dafny proof backend reaches feature parity with Lean on recursion.** Shared recursion classifier (`codegen::recursion::detect`) + AST transform now feed both backends. Dafny emits mutual-recursion SCCs as fuel-guarded `function fn__fuel(fuel: nat, …) decreases fuel { … }` pairs with plan-specific metrics, parallel to Lean's `def fn__fuel (fuel : Nat) …`. Shapes that admit no total default or use `?` that doesn't lower fall back to `function {:axiom}` — the Dafny analogue of Lean's `partial def`. Lemmas over opaque fns short-circuit to `assume {:axiom} <ensures>;`, the Dafny analogue of Lean's `sorry`. Across the 23 canonical examples: 12 are clean on both backends (full proof), 9 have matching proof gaps (Dafny axiom/assume vs. Lean sorry), 2 are pre-existing codegen gaps orthogonal to the recursion story.

### Changed
- **Effectful verification story** — README, language guide, and proof-backend docs now distinguish Oracle verification for classified effects from record/replay for ambient state, persistent protocols, terminal modes, and server callbacks.
- **`codegen::recursion` module** — `RecursionPlan` enum, `ProofModeIssue`, recursion classifier, and `rewrite_recursive_calls_{body,expr}` AST transform pulled out of `codegen::lean` into the shared module. Lean and Dafny both consume it. `RecursionPlan::IntAscending` now holds the bound as an Aver AST node (`Spanned<Expr>`) instead of a Lean-rendered string — each backend renders it in its own idiom.

## 0.10.1 (2026-04-23)

### Added
- **`aver run --expr '<call>'` / `--input-file PATH`** — record or run any function, not just `main`. Pass a call like `aver run src/tax.av -e 'loadTaxRate("PL")' --record dir/` and the recording's `entry_fn` and `input` are populated from the call; `aver replay` picks it up unchanged. Repeat `-e` to batch. Supports literal, list/tuple, and ADT-constructor arguments (`Result.Ok(5)`, `Shape.Circle(1.0)`, nested). Function calls / arithmetic / variables in arg position stay out of scope — wrap them in a helper function and call that instead. The same capability is exposed in the playground's Trace panel.

## 0.10.0 "Telltale" (2026-04-21)

> _Tooling now shows its work — parse errors point at the exact token with a repair, audit runs three axes in one shot, and every program can be recorded & replayed in the browser._

### Added
- **`aver audit`** — one command that runs static checks, verify blocks, and format compliance together. CI-friendly exit code, `--json` for pipelines.
- **`aver format --check` / `aver format --check --json`** — non-mutating format verification. Every rewrite reports a structured `FormatViolation` with a stable `rule` slug: `tab-indent`, `bad-function-header`, `effects-unsorted`, `effects-reshape`, `verify-misplaced`, `excess-blank`, `module-intent-reshape`, `decision-inline`, `trailing-whitespace`, `missing-final-newline`. Agents and linters can key off specific rules instead of free-text diffs.
- **Naming convention checker** — flags non-camelCase functions / fields and non-PascalCase types / modules / variants as stable diagnostics (`bad-fn-name`, `bad-type-name`, `bad-module-name`, `bad-variant-name`, `bad-field-name`). Runs as part of `aver check` and `aver audit`.
- **Canonical diagnostic bundle across every CLI command** — `aver check --json`, `aver verify --json`, `aver why --json`, `aver audit --json`, `aver format --check --json` all emit `AnalysisReport` NDJSON now. One schema, `schema_version: 1`, documented in `docs/diagnostics-schema.md`.
- **Reworked [browser playground](https://averlang.dev/playground/)** — multi-file editor, interactive record & replay, full parity with CLI audit / why / context / format. Go play.

### Changed
- **Format engine rewritten around structured violations** — the formatter used to report "needs format" as opaque before/after diffs. Every normalization pass now tracks per-line rule violations with original source-line numbers, so `--check` output points at the exact line that needs the specific fix instead of dumping a reformatted file. `needs-format` stays as the aggregate marker; each `FormatViolation` rides alongside with its rule slug.
- **Effects list in `aver context`** — `signature` is now params + return type only; effects live on the sibling `effects` array. Lets renderers show them without duplicating `! [...]` on screen.

### Fixed
- **Parse errors landed on line 1:1 with no hint** — the formatter stripped the real span and emitted a single red line that didn't tell you where or why. Now: real line/col pulled from the parser, source snippet with `^^^` caret under the offending token (clamped to the last char for EOL errors like Unterminated string), and repair hints for common shapes (`Expected '[' after '!'` → `! [Console.print, ...]`, missing `module <Name>`, map `=>` syntax, tuple-needs-2-elements, …).
- **`Time.sleep` on wasm32-unknown-unknown panicked** ("can't sleep") — the browser runtime now makes it a no-op; native builds keep real blocking sleep.

## 0.9.7 (2026-04-16)

### Changed
- **Pre-compiled self-host** — `--self-host` no longer generates Rust code and runs `cargo build` at runtime. The self-host interpreter is compiled as a `[[bin]]` target alongside `aver`, so `cargo install aver-lang` provides both binaries out of the box. No Rust toolchain needed at runtime.
- **Cargo package cleanup** — published crate excludes `self_hosted/`, `examples/`, `tools/`, `editors/`, etc. Only `src/` and essentials ship. Self-host generated code lives in `src/self_host/`.

### Added
- **Release script** (`tools/release.py`) — automates version bumps, self-host regeneration, playground rebuild, crates.io publish, and GitHub release creation.

### Fixed
- **`--self-host` crash from crates.io install** — the runtime codegen tried to read `aver-rt/Cargo.toml` relative to the installed crate source, which doesn't exist. Eliminated by removing runtime codegen entirely.
- **Rust codegen: TCO invariant hoisting of variant constructors** — enum variant constructors (e.g., `Val::ValStr`) were hoisted as loop invariants, causing move errors in generated Rust. Now excluded alongside builtins.

### Removed
- Runtime codegen pipeline for self-host (`build_self_host_binary`, fingerprinting, `cargo build --offline` invocation).

## 0.9.6 (2026-04-16)

### Performance
- **VECTOR_SET_OR_KEEP in-place mutation** — fused `Option.withDefault(Vector.set(v, i, val), v)` always has sole ownership; the opcode now mutates the vector directly at its arena slot. Vector get/set 5k: 17ms → 816µs (20× faster).
- **Skip promotion rewrite for bulk types without young refs** — vectors/maps/tuples of inline ints skip O(n) per-element rewrite during young→yard promotion. Vector get/set 5k: 17ms → 12.6ms (−26%).
- **VM arena in-place map rewrite** — map promotion rewrites NanValue pairs in-place via `Rc::make_mut` instead of rebuilding the HashMap. Map build 5k: 24ms → 1.3ms (18.9×). COW ops inherit source allocation space, skipping redundant promotion.

### Changed
- **Unified symbol resolution for uppercase dotted paths** — `classify_leaf_op` now classifies uppercase `Expr::Attr` paths (Option.None, variant constructors, module function refs) via three new `LeafOp` variants instead of returning `None`. Eliminates duplicate resolution logic from Rust and WASM backends. WASM backend now routes `Expr::Attr` through the shared IR layer.

### Added
- **WASM `Option.toResult` builtin** — `Some(v) → Ok(v)`, `None → Err(err_value)`.

### Fixed
- **WASM cross-module variant constructor resolution** — hierarchical module paths like `Domain.Types.TaskStatus.Blocked` now resolve correctly (previously only single-level bases worked).
- **Rust codegen: TCO invariant hoisting of builtin callees** — `List.prepend` and similar builtin namespace refs were hoisted as standalone value expressions, generating invalid Rust (`List.prepend`). Now excluded from hoisting since they're compile-time constants.

### Removed
- Dead `entry_has_young_refs` function.

## 0.9.5 (2026-04-15)

### Changed
- **Shared symbol layer** — module visibility, type registration, and symbol resolution now go through a single shared layer. All backends consume the same `ModuleExports` and `SymbolRegistry` instead of building their own views.
- **Unified module loader** — `load_module_tree()` replaces independent loaders in the VM compiler and type checker. Proper circular-import detection and module-name validation everywhere.

### Fixed
- **WASM codegen wrong types for private module helpers** — dependency modules with private functions returning non-Int types (e.g. `padTwo` returning `String`) caused invalid WASM. Codegen now has full signatures for all emitted functions.
- **Rust codegen missing `Arc::new()` for cross-module recursive types** — self-host binary failed to compile from a clean cache. Constructor boxed-position lookup now handles qualified names.

### Removed
- Suffix-matching heuristic in VM type resolution
- Dual-key registration in type checker (replaced by alias-based lookup)
- Checker's `ModuleSigCache` and cycle-detection stack (handled by shared loader)

## 0.9.4 (2026-04-15)

### Fixed
- **Rust codegen crash with forwarded arguments** — `aver compile` and `aver run --self-host` failed on programs with forwarded local variables after the unified resolver change in 0.9.3.

## 0.9.3 (2026-04-15)

### Performance
- **Map/Vector in-place mutation everywhere** — collections are now mutated in-place whenever the variable is at its last use, not just inside tail-call loops. VM map build 50K entries: **39s → 0.9s** (43× faster). Sequential `Map.set` chains without recursion also benefit.
- **WASM handles large collections** — map/list build with 50K+ entries no longer crashes. Previously overflowed at ~5450 entries.

### Changed
- **Unified variable resolution** — all backends (VM, Rust codegen, WASM, Lean, Dafny) now share a single resolver and liveness analysis. ~675 lines of duplicated ownership logic removed.
- **`rebuild_playground.py`** — now also rebuilds the in-browser compiler (`aver_bg.wasm`), not just game modules.

### Fixed
- **Map.set not using fast path with computed keys** — `Map.set(m, Int.toString(n), n)` was incorrectly falling back to the slow clone path. Now correctly detected as in-place-safe.

## 0.9.2 (2026-04-13)

### Fixed
- **WASM mutual TCO memory leak** — dead temporaries (HUD strings, record updates) accumulated across iterations in mutual tail-call loops because nested function calls masked garbage from the yard heuristic. Replaced per-iteration skip threshold with watermark-based adaptive compaction (triggers when garbage exceeds 16KB since last collection). Game of Life editor loop now stable at ~15KB instead of growing to 10+ MB.

### Added
- **`$heap_ptr` WASM export** — modules export the bump allocator position for host-side memory inspection.
- **Playground memory display** — live heap usage shown in status bar, throttled to 500ms updates.
- **Playground rogue touch controls** — added descend stairs (>) button.
- **`llms.txt` improvements** — expanded namespace signatures, explicit "do NOT exist" operator section, verify block rules, qualified constructor examples. Driven by vera-bench LLM evaluation data.

## 0.9.1 (2026-04-10)

### Added
- **Aver Playground** — write and run Aver in the browser at [averlang.dev](https://averlang.dev). In-browser compiler, 7 playable games, source viewer, syntax highlighting.

### Fixed
- **WASM variant equality** — nullary variants (`Color.White == Color.White`) now compare correctly.
- **WASM variant display** — shows type names instead of `Variant#0`.
- **WASM `Console.readLine`** — properly wrapped in `Result.Ok`, blocking via SharedArrayBuffer.

## 0.9.0 (2026-04-09)

### Changed
- **VM is the default backend** — `aver run`, `verify`, `replay`, and `repl` now use the bytecode VM directly. The `--vm` flag is no longer needed and has been removed.

### Removed
- **Tree-walking interpreter** — ~7500 lines removed. The VM covers all use cases the interpreter handled.

### Added
- **WASM backend** — `aver compile --target wasm` and `aver run --wasm`. Own `aver/*` import ABI with `--adapter wasi` for standalone wasmtime. Works with built-in host, browser JS shim, or custom host.
- **Browser WASM runner** — `tools/wasm-runner/` with terminal canvas rendering and keyboard input.
- **`aver-memory` crate** — standalone NaN-boxed value representation and arena allocator.

## 0.8.2 (2026-04-03)

### Added
- **Root-parallel checkers AI** — the checkers example now uses independent products to score root moves in parallel.

### Fixed
- **Self-host stability** — running `self_hosted/main.av` through the host interpreter and host VM is much more stable. Fixes cover qualified module calls, constructor pattern matching, resolver slot binding, and VM runtime aliases.
- **VM parallel map imports** — child VMs in independent products no longer silently drop map contents, fixing incorrect fallback behaviour in self-host runs.
- **Codegen: record field access** — accessing a field of a borrowed record parameter in return position now emits the required `.clone()`.
- **Codegen: memoized recursive functions** — call sites for auto-memoized recursive functions now correctly pass arguments by reference, matching the generated function signature.
- **Codegen: independent products with cancel mode** — `?!` expressions no longer produce invalid `let`-in-expression Rust code when cancel mode is active.
- **VM: no-main programs** — the VM now finishes silently when a program has no `fn main`, consistent with the interpreter.

## 0.8.1 (2026-04-03)

### Added
- **`examples/apps/status_board.av`** — an offline terminal dashboard that makes independent-product pipeline overlap visible in the VM and compiled Rust backends.

### Fixed
- **VM parallel child contexts** — child VMs now rebase `MATCH_DISPATCH` / `MATCH_DISPATCH_CONST` inline `NanValue` payloads when building the static arena for independent products. This fixes crashes when a branch returns or matches on heap-backed constant strings.
- **VM regressions for child arenas** — added coverage for direct child-VM calls, nested string interpolation, and full `CALL_PAR` execution so this class of arena-rebasing bugs stays closed.

## 0.8.0 (2026-04-02)

### Added
- **Independent products (`?!` / `!`)** — a tuple followed by `!` is a product of independent computations. `?!` adds Result unwrapping — all must succeed or the first error propagates. Independence is structural: tuple elements cannot reference each other. Composes recursively, giving fan-out parallelism and pipeline overlap with no new language concepts. No async, no futures, no channels — just products and independence. See [docs/independence.md](docs/independence.md).
- **Parallel execution** — compiled Aver programs run `?!` / `!` elements on separate threads. Two HTTP calls that each take 2 seconds complete in ~3 seconds, not 4. Recursive `?!` over a list fans out the entire tree.
- **Replay groups** — effects inside a `?!` / `!` product are order-independent in replay. Reordering independent code does not break recorded sessions.
- **`aver check` independence hazards** — branch-pair warnings for likely unsafe overlaps in independent products, including `Console`/`Terminal`, `Tcp`, `HttpServer`, and mutating `Disk` / `Http` / `Env` effects. Warnings can be suppressed via `[[check.suppress]]` with a mandatory reason.
- **`List.take` / `List.drop`** — list windowing helpers for bounded fan-out and batching patterns.
- **`aver why`** — justification coverage tracer. Scores every function as justified, partial, or unjustified based on description, verify blocks, and coverage. `--verbose` and `--json` output modes.
- **`[[check.suppress]]` in `aver.toml`** — suppress specific warnings with a mandatory `reason`.

### Changed
- **Thread-safe runtime** — all runtime types use atomic reference counting, enabling parallel execution of independent products.
- **Structured replay diagnostics** — per-error-type diagnostics with source locations. Args diff hint shown even without `--check-args`.
- **Independent-product replay matching** — replay now keys grouped effects by `branch_path + effect_occurrence + effect_type + effect_args`, so nested and repeated effects replay deterministically across reordering.
- **VM independent products** — `CALL_PAR` now carries callable values rather than only statically resolved function ids, so aliases like `f = foo; (f(x), f(y))!` work. Branch VMs import a thin static arena plus per-branch inputs/outputs instead of cloning the whole parent heap.
- **Compiled Rust cancellation** — generated Rust now uses cooperative cancel checkpoints for `?!` in `mode = "cancel"` and keeps branch closures borrow-checker-safe.

## 0.7.3 (2026-03-30)

### Added
- **`aver verify --json`** — structured NDJSON output: `block-result` per verify block, `diagnostic` per failure, `summary` at end.
- **`aver verify --verbose`** — failure diagnostics with source snippets and full fields (given/law context for specs).
- **`aver replay --json`** — NDJSON output: `replay-result` per recording, `summary` at end.
- **Structured verify diagnostics** — verify failures use the same diagnostic system as `aver check`: `fail[verify-mismatch]`, `fail[verify-runtime-error]`, `fail[verify-unexpected-err]` with `at:`, `block:`, `case:`, `expected:`/`actual:`, source snippets with carets. Normal mode caps to 3 diagnostics per block.

### Changed
- **Inline variants (TAG 14)** — single-field variants whose payload is a small int (±268M), bool, unit, or none are now NaN-boxed inline (8 bytes, zero arena allocation). Pattern matching and field extraction skip arena indirection entirely.
- **Unified NDJSON format** — `check`, `verify`, and `replay` all emit `{"schema_version":1,"kind":"..."}` envelope with summary events.
- **Verify output redesign** — per-file grouping, one-line block summaries with failure type breakdown, streaming output. Skipped files (type errors) show count + hint about `--module-root`.
- **Codegen: let-destructuring** — single-arm irrefutable matches (`match x: (a, b) -> expr`) now emit `let (a, b) = x; expr` instead of a full match block.

### Fixed
- `aver check --json` no longer emits human-readable lines mixed with JSON.
- `aver replay` no longer duplicates "Replay:" prefix in error messages.
- `aver run --verify` now shows file path and source snippets in failure diagnostics.

## 0.7.2 (2026-03-29)

### Added
- **Structured error messages** — `aver check` shows source snippets, repair suggestions, and semantic error categories (`type-mismatch`, `unused-binding`, `missing-verify`). Use `--verbose` for full context on warnings.
- **Unused binding warnings** — `aver check` warns on bindings that are defined but never used. Prefix with `_` to silence.
- **`aver check --json`** — structured JSON output for editor and CI integrations.
- **`Map<T, Unit>` as set** — Lean codegen emits `Finset T`, Dafny emits `set<T>`. See [docs](docs/language.md#sets).
- **Common Pushback FAQ** — [docs/pushback.md](docs/pushback.md) covers frequent questions and objections about the language.

### Changed
- **Faster compiled code** — generated Rust is significantly faster across all benchmarks: pattern matching -66%, maps -13%, records -14%, vectors -19%. The self-hosted interpreter is 7-25% faster depending on workload. Fused IR ops (`IntModOrDefault`, `ListIndexGet`) eliminate intermediate allocations; codegen now skips unnecessary clones on Copy fields, drops `&` on numeric arithmetic, and matches borrowed params without cloning the subject.
- **LSP** — Vector namespace completions, updated List members, `exposes opaque` support in document symbols.
- **Editor highlighting** — VSCode and Sublime grammars updated with all current namespaces and keywords.
- Aver formatter keeps medium effect lists on one line when they fit.

### Fixed
- `Console.error`/`Console.warn` in self-hosted now route to stderr.
- `--with-self-host-support` enforces guest-entry contract.

## 0.7.1 (2026-03-27)

### Changed
- `aver run --self-host` now caches its generated helper per installed Aver/self-host build instead of per guest `module_root`, so switching projects no longer forces a rebuild.
- Self-hosted guest `aver.toml` policy is now loaded at runtime from the guest module root and starts only at the guest boundary, matching scoped replay behavior.
- Cold `--self-host` runs now print short progress messages while Aver generates and builds the cached helper.
- `aver compile` now exposes runtime policy mode explicitly via `--policy embed|runtime`; plain codegen defaults to `embed`, while `--with-replay` defaults to `runtime`.

### Fixed
- `aver run --self-host` no longer misclassifies qualified user module calls like `Map.generateMap` or `Time.foo` as builtins just because they share a builtin namespace prefix. Self-hosted module programs such as `examples/games/rogue` now execute correctly again.

## 0.7.0 (2026-03-26)

**Breaking:** `List.get` and `List.append` removed. Use `Vector` for indexed access.

### Added
- **`Vector<T>`** — indexed sequence with O(1) get/set. API: `Vector.new`, `Vector.get`, `Vector.set`, `Vector.len`, `Vector.fromList`, `Vector.toList`. `Vector.set` returns `Option<Vector<T>>`.
- **Mutual TCO** in codegen — mutually recursive functions compiled to trampoline dispatch loops.
- **Namespace effect shorthand** — `! [Disk]` covers all `Disk.*` effects.
- **Self-host CLI path** — `aver run --self-host` and `aver replay --self-host` now run through the Aver-in-Aver interpreter compiled to a cached Rust binary.
- **Scoped generated replay runtime** — `aver compile --with-replay --guest-entry <fn>` emits replay support that starts record/replay and `aver.toml` policy at an explicit guest boundary instead of the process boundary.

### Changed
- **`List` is now purely recursive** — `prepend`, `head`, `tail`, `concat`, `reverse`, `contains`, `find`, `any`, `zip`. No indexed access.
- Idiomatic pattern: build with `List.prepend` → `List.reverse` → `Vector.fromList` (zero-copy on Flat lists).
- Compiled projects use LTO + `codegen-units = 1` for faster release builds.
- Self-hosted interpreter ~1.5× faster (COW maps, `Rc<str>` strings, Vector env).
- Installed `aver` now bundles the `self_hosted/` sources directly, so `aver run --self-host` bootstraps its cached helper binary automatically without a separate self-host install step.
- Generated Rust projects now target Rust 2024.
- `aver check` no longer suggests granular namespace effects on wrappers that also require the broad namespace transitively through a callee.
- `benches/comparison_bench.rs` now measures the real `aver run --self-host` CLI path instead of a stale standalone `aver-self` binary from `$HOME/.cargo/bin`.

### Removed
- `List.get`, `List.append`.

## 0.6.1

Highlights:
- VM is 25–54% faster across benchmarks; interpreter-to-VM speed ratios improved from 5–7× to 7–13×.
- Added `aver run --profile` for opcode/function-level VM profiling.
- Fixed several VM correctness and memory issues, including match fallthrough, deep-list return overflow, and request-local stable-space retention.

### Added
- `aver run --profile` — VM execution profile with opcode counts, function stats, and opcode-pair analysis.
- Game of Life example (`examples/games/life.av`) with terminal visualization and FPS counter.
- Self-hosted interpreter project in Aver (`self_hosted/`).

### Changed
- Added specialized VM handling for common unwrap/default, boolean branch, and fused-load patterns.
- Added frameless calls for small leaf functions to reduce hot-path call overhead.
- Bool `match` on `true/false` now compiles to a direct conditional branch.
- Refined VM value layout to reduce wrapper overhead and speed up dispatch.
- `Terminal.size` now returns a record with `width`/`height` fields instead of a tuple. Generated Rust requires `aver-rt >= 0.3.1`.
- `aver context --json` now uses `serde` serialization.
- Lean proof export now emits universal theorems with `sorry` when auto-proof fails.

### Fixed
- Exhaustiveness checker hang on recursive sum types.
- `MATCH_DISPATCH_CONST` fallthrough causing infinite recursion in patterns like `fib(n)`.
- Arena stack overflow on deep list returns.
- `HttpServer` callback stable-space retention across requests.
- Lean export reserved-word conflict for `toString`.

## 0.6.0

### Added
- **Bytecode VM** — `aver run --vm` compiles Aver to a stack-based bytecode VM with NaN-boxed values, region-based arena memory (young/yard/handoff/stable), dedicated list opcodes, structural persistent lists, and thin-function fast return paths. 5-9x faster than the tree-walking interpreter on compute-heavy workloads.
- **Terminal service** — `Terminal.*` namespace (12 methods) for raw-mode terminal I/O via crossterm: cursor control, colored output, non-blocking key input, screen management. Behind `terminal` cargo feature (enabled by default).
- **Terminal guard** — `aver run` installs a drop guard that restores terminal state (cursor, colors, raw mode) on exit, panic, or runtime error.
- **Bool namespace** — `Bool.or`, `Bool.and`, `Bool.not` pure builtins for logical combinators.

### Changed
- `aver-rt::AverList` now packs repeated `append` chains into segmented chunk spines, improving list-heavy workloads in both the interpreter and generated Rust.

## 0.5.5

### Added
- **Opaque types** — `exposes opaque [TypeName]` in module declarations. Types listed as opaque are visible in signatures but cannot be constructed, field-accessed, or pattern-matched from outside the defining module. Enforced at compile time by the typechecker. See `docs/language.md` for usage.
- `aver context --focus <symbol>` builds context around a specific function's dependency cone (callees, types, verify blocks, decisions)
- Priority scoring for `aver context` budget allocation: elements with more verify coverage, spec references, and focus relevance are included first
- Type-aware verify sample selection: scorer uses fn return type to pick diverse cases (Ok + Err for Result fns, true + false for Bool, per-constructor for sum types)
- Granular verify coverage warnings: checker now reports missing Result Ok/Err, Option Some/None, Bool true/false, and sum type variant cases

### Changed
- `src/checker.rs` split into `src/checker/` module (coverage, verify, intent, law)

## 0.5.4

### Added
- **Dafny verification backend** — `aver proof --backend dafny` generates a `.dfy` file with Z3-powered automated proofs for `verify law` blocks; complements Lean's `native_decide` / tactic approach
- **Random service** — `Random.int(min, max)` and `Random.float()` with `! [Random]` effect, backed by `aver_rt::random` (OS entropy via `rand` crate behind feature flag)
- **Shared `Builtin` enum** — `codegen/builtins.rs` defines all pure Aver builtins (~80 variants); adding a new builtin forces all backends to handle it via exhaustive match
- **Shared codegen utilities** — `codegen/common.rs` now provides `escape_string_literal`, `split_type_params`, `escape_reserved_word`, `parse_type_annotation`, and `to_lower_first` used by all three backends
- `docs/dafny.md` documenting the Dafny backend, its two-layer contract (sample assertions + universal lemma), and Lean vs Dafny comparison
- Wumpus example (`examples/apps/wumpus.av`) — Hunt the Wumpus with dodecahedron topology, effectful random, full verify coverage (30/30)

### Changed
- `aver proof` now accepts `--backend lean|dafny` (default: `lean`)
- **Lean auto-proof simplified** — removed brittle indirect-recursion and recursive map-presence strategies; the backend now honestly rejects patterns it can't prove instead of generating fragile proofs. Helper-law dependency hints guide users toward layered verification.
- Dafny emits `verify law` as both capped sample assertions (max 5) and a universal `lemma`; `verify` cases are not emitted (Z3 can't compute deep recursion)
- All three codegen backends (Lean, Dafny, Rust) now share reserved-word escaping, string literal escaping, and type annotation parsing via `codegen/common.rs`
- Rust codegen now gates `aver-rt` features (`http`, `random`) based on which services the program actually uses

## 0.5.3

### Added
- `projects/payment_ops` as a medium-size dirty-backoffice showcase covering provider normalization, replay, settlement reconciliation, manual-review cases, and audit trail

### Fixed
- Rust codegen ownership for list / tuple / map literals and record updates, so valid Aver programs no longer emit generated Rust that fails with move errors in these patterns

## 0.5.2

### Added
- `workflow_codegen_bench` for repeatable end-to-end comparisons between interpreter, VM, and generated Rust on `projects/workflow_engine`

### Changed
- interpreter function bodies now lower to shared `ExprId`-based runtime nodes, so the evaluation hot path no longer carries cloned AST fragments through continuations
- `aver check` now tells users with non-tail-recursive functions to either rewrite them into tail recursion or make them a spec, and canonical spec functions no longer emit that warning
- generated Rust now pins `aver-rt = "=0.2.1"` so current codegen matches the shared runtime features it emits

### Fixed
- Rust codegen regressions around nested builtin-argument liveness, same-arity mutual tail calls, and memoized recursive named types, restoring generated builds for examples such as `grok_s_language`, `red_black_tree`, and `mysql`


## 0.5.1

### Added
- native LSP document formatting via the shared Aver formatter
- richer `aver-lsp` editor UX: effect-aware completion, verify/decision code lenses, contract-first hover, and document symbols with nested `verify`
- publishable `aver-lsp` crate metadata and docs for installing the language server separately from `aver-lang`

### Changed
- editor install docs now target `cargo install aver-lsp` plus editor extension installation, with local source-build setup kept as a development path

## 0.5.0

### Added
- `Args.get()` as an explicit runtime service for CLI arguments (`List<String>`)
- round-trip law coverage for naturally invertible examples, including `json`, `grok_s_language`, and `notepad/store`
- `aver context --budget` with `kb` / `mb` suffixes for prompt-sized exports
- `aver context` selection metadata in JSON and in the `--output` summary, including included depth and next-depth size
- modular Rust code generation that emits `src/aver_generated/...` instead of flattening all Aver code into one giant `main.rs`
- directory inputs for `aver check` and `aver verify`, so one command can walk a whole example or project tree
- `projects/workflow_engine` as a serious medium-sized Aver application core, covering projects, tasks, workflow rules, audit trail, notifications, and CLI/query flows
- `aver check` warnings for recursive functions that still contain non-tail recursive callsites after TCO, with accumulator-style guidance
- iterative interpreter expression evaluation backed by a heap continuation stack instead of the Rust call stack

### Changed
- **Breaking:** effect aliases (`effects X = [...]`) were removed; declare concrete method effects directly in `! [...]`
- **Breaking:** broad namespace declarations such as `! [Http]` no longer satisfy child effects like `Http.get`
- `aver verify` now checks only declared `left => right` examples; coverage-style diagnostics moved to `aver check`
- `aver check` now reports coverage hints as warnings and no longer exits non-zero because of warnings alone
- `aver context` now defaults to `--depth auto --budget 10kb` instead of walking dependencies without a budget
- `aver context --json` stays human/LLM-oriented: compact signatures, short verify strings, omitted empty sections, and skipped long verify cases
- examples were reorganized into `core/`, `data/`, `formal/`, `modules/`, `services/`, and `apps/` under a shared `--module-root examples`, while standalone showcase apps now live under `projects/`

### Fixed
- `aver verify --deps` now verifies transitive dependencies
- exposed sum types and constructors now resolve correctly across module boundaries
- fully-qualified constructor patterns now work consistently in parsing, typechecking, exhaustiveness, and runtime matching
- `Result<Unit, String>` now accepts `Unit` cleanly and renders `Unit` consistently
- `unused exposes` diagnostics now resolve real symbol usage from AST and point at the module's `exposes` line
- Rust codegen now resolves module-qualified Aver calls/types without flattening sibling modules into one ambiguous Rust namespace
- Rust codegen now routes `Args.get()` through `aver-rt`
- deep `AverList` teardown and `append -> match` / `tail` paths in `aver-rt`, removing shared stack-overflow cliffs for both the interpreter and generated Rust
- `String.slice` semantics are now shared between the interpreter and `aver-rt`, including negative-index clamping

## 0.4.0

### Added
- `aver proof` as a dedicated Lean proof-export command
- `aver --version`
- docs for `Unit`, `main` returning `Result<Unit, String>`, and the `HttpServer.listen` / `listenWith` callback model

### Changed
- **Breaking:** `aver compile` now targets Rust only
- **Breaking:** Lean export moved from `aver compile -t lean` to `aver proof`
- **Breaking:** Lean CLI flags were renamed from `--lean-verify` to `--verify-mode`
- **Breaking:** match patterns now reject positional record destructuring such as `User(name, age)`; bind the record and use field access instead
- **Breaking:** constructor patterns must now be qualified (`Shape.Circle`, `Result.Ok`, `Option.None`) instead of bare `Circle` / `Some` / `None`
- CLI/docs were split around two separate backend intents: deployment (`compile`) and proof export (`proof`)

### Fixed
- Lean proof export now respects qualified cross-module calls such as `Examples.Json.toString` during function emission ordering
- Lean prelude now injects built-in `Header`, `HttpRequest`, `HttpResponse`, and `Tcp.Connection` support when generated code references those runtime types
- Lean `List.get` now preserves Aver's `Int` index semantics, including negative indices returning `Option.None`
- `examples/notepad/routes.av` proof export now builds successfully under Lean with `aver proof --verify-mode auto`
- parser/typechecker/interpreter specs were aligned with the qualified-constructor pattern rules and explicit record binding model

## 0.3.0

### Added
- `aver-rt` as a shared Rust runtime crate for transpiled projects and interpreter adapters
- `aver check --deps` to run contract checks for transitive `depends [...]` modules
- deterministic replay now walks nested recording directories
- recursion-first list runtime based on persistent `AverList`

### Changed
- **Breaking:** function bodies now use indentation only; `fn ... = expr` shorthand was removed
- **Breaking:** `|>` pipe operator was removed
- **Breaking:** `List` was simplified to a recursion-first API: `len`, `get`, `prepend`, `append`, `concat`, `reverse`, `contains`, `zip`
- **Breaking:** `List.push`, `List.head`, `List.tail`, `List.map`, `List.filter`, `List.fold`, `List.find`, `List.any`, and `List.flatMap` were removed
- **Breaking:** removed `aver decisions`; decision export now lives under `aver context --decisions-only`
- Rust transpilation now depends on the published `aver-rt` crate by default, with optional `AVER_RUNTIME_PATH` override for local runtime hacking
- `aver check` contract diagnostics now always include line numbers
- Decision `impacts` now accepts both validated symbols and semantic strings
- `input`, `expect`, `case`, `where`, `effect`, `service`, `needs` are no longer reserved keywords
- README and docs were restructured around quickstart, AI-first positioning, and the current CLI/runtime model

### Fixed
- old `= expr` syntax now fails consistently in parser and formatter with an actionable migration error
- `decisions/*.av` updated to conform to strict impacts validation and namespaced console usage
- renamed `examples/type_errors.av` to `examples/test_errors.av` with expanded checker diagnostics coverage

## 0.2.3

### Added
- `verify ... law ...` blocks with typed `given` domains (`a..b` ranges and explicit lists)
- Lean emission for `verify law`: named law theorems, sample theorems, and universal theorem skeletons
- Lean verify modes in CLI: `--lean-verify auto|sorry|theorem-skeleton`
- `--lean-proof-mode` fail-fast gate for proof-unsafe Lean transpilation paths
- Deeper match exhaustiveness analysis for nested and recursive patterns

### Changed
- `aver check` now treats missing verify on pure non-trivial functions as an error
- `verify law` skips regular case-level target-call heuristics used by `verify` case blocks
- Decision block fields (`date`, `author`, `reason`, `chosen`, `impacts`, etc.) are contextual (no longer globally reserved keywords)
- File-based commands require exactly one `module` declaration as the first top-level item

### Fixed
- Exhaustiveness checker stack overflow on recursive sum types with 2+ variants
- Empty `verify` blocks are rejected explicitly
- Rust codegen now fails fast on unresolved unknown types instead of panicking
- Lean codegen/parser ordering and mutual-recursion proof-mode integration issues
- Multiple Clippy-level borrow/style issues in builtin dispatcher paths

## 0.2.2

### Added
- Lean transpilation target in CLI: `aver compile -t lean`
- Lean codegen backend module structure (`src/codegen/lean/*`)
- Transpilation docs for Lean target in README and `docs/transpilation.md`

### Fixed
- Shared deterministic function ordering via call-graph SCC topo order (callee-before-caller) for codegen backends
- Lean forward-reference failures in emitted code (e.g. helper emitted after use)
- Lean prelude: avoid reserved keyword `from` in generated `String.slice`
- Lean `AverMap.set` now preserves key order when updating existing keys

## 0.2.1

### Added
- HttpServer: skip real TCP server in `--record` mode (return Unit immediately)
- Example recordings: console_demo, disk_demo, http_demo, notepad
- Recording snapshots persist on every effect (long-running processes safe)
- Module sum types exported via `exposes`
- Map literal syntax in examples

### Fixed
- Silent body discard when `=` appears after bindings in fn body
- Architecture decision output formatting
- README: clarify no Int/Float promotion, document missing Disk methods

## 0.2.0

### Added
- **Aver-to-Rust transpiler** (`aver transpile`) with full service support, module inlining, last-use analysis, and copy-type elision
- **LSP server** and **VSCode extension** with diagnostics, hover, go-to-definition, and module dependency caching
- **Deterministic replay** (`--record` / `aver replay`) — record effectful runs, replay without I/O
- **Static match exhaustiveness checking** at compile time
- **Compile-time variable resolution** — `Ident` → `Resolved(slot)` for O(1) lookup in fn bodies
- **Auto-memoization** of pure recursive functions with memo-safe arguments
- **Tail-call optimization** — self and mutual recursion without stack overflow
- **Typed bindings** — `name: Type = expr` with type checker validation
- **Tuple values** and tuple destructuring in match patterns
- **Map type** with `Map.get`, `Map.set`, `Map.keys`, `Map.values`, `Map.has`, `Map.remove`, `Map.size`
- **Map literal syntax** — `{"key" => value, ...}`
- **Record update syntax** — `Type.update(base, field = newVal)`
- **Multiline expressions** inside `()`, `[]`, `{}` delimiters
- **`Char` namespace** — `Char.toCode`, `Char.fromCode` for Unicode operations
- **`Byte` namespace** — `Byte.toHex`, `Byte.fromHex`
- **`String` additions** — `String.charAt`, `String.toLower`, `String.toUpper`
- **`List` additions** — `List.find`, `List.any`, `List.contains`, `List.zip`, `List.flatMap`
- **`Result.withDefault`**, **`Option.withDefault`**, **`Option.toResult`** combinators
- **Generic type inference** for Option/Result combinators
- **JSON parser** (`examples/json.av`) — full RFC 8259 with `\uXXXX` surrogate pairs, control char validation
- **Persistent Tcp connections** — `Tcp.connect`/`writeLine`/`readLine`/`close` with opaque `Tcp.Connection`
- **Redis RESP client** (`examples/redis.av`)
- **HttpServer service** — `HttpServer.listen` and `HttpServer.listenWith` (explicit context parameter)
- **Weather microservice example** — HttpServer + Http + Redis cache
- **Notepad REST API example** — multi-module CRUD app with Disk persistence
- **Agent challenge infrastructure** — prepare.sh, evaluate.sh, 3 challenges for AI agent testing
- **Interactive REPL** (`aver repl`) — stateful, multi-line, type-checked
- **`aver context`** — project context export for LLM consumption (Markdown + `--json`)
- **`aver decisions`** — generated architecture decision docs
- **`aver check --strict`** mode
- Human-readable parser error messages via `TokenKind` Display
- Editor support: VSCode extension + Sublime Text syntax highlighting
- Prepared for crates.io publication as `aver-lang`

### Changed
- **Breaking:** `List.get`, `List.head`, `List.tail` now return `Option` (was raw value / error)
- **Breaking:** `String.length` renamed to `String.len`
- **Breaking:** `val`/`var` keywords removed — all bindings are `name = expr`, always immutable
- **Breaking:** flat builtins removed (`print`, `len`, `map`, `filter`, `fold`, `str`, `int`, `abs`, etc.) — use namespaced equivalents (`Console.print`, `List.len`, `List.map`, ...)
- **Breaking:** `Ok`/`Err`/`Some`/`None` keywords removed — use `Result.Ok`, `Result.Err`, `Option.Some`, `Option.None`
- **Breaking:** `Any` removed from surface syntax — `Type::Unknown` is internal only
- **Breaking:** colon-only type annotations (`x: Int` not `x Int`)
- Renamed `Network` service to `Http`
- Env uses `Rc<Value>` with slot-based frames for resolved functions
- Closures removed — functions see globals at call time, not capture time
- Pipeline: parse → TCO transform → typecheck → resolve → interpret
- Verify warnings only for pure non-trivial functions
- File size warning raised to 250 lines

### Fixed
- Constructor rules enforced: named fields required for records, positional for sum variants
- Empty list binding rejection without type annotation
- Pipe RHS parsing tightened
- Tuple memo hashing
- `{{ }}` brace escapes in string highlighting
- Module function scope and memo collisions
- Match arm body error message after unexpected newline
- LSP UTF-16 position handling

## 0.1.0

Initial release of the Aver language interpreter.

### Core language
- Significant indentation (Python-like)
- Immutable bindings (`name = expr`)
- Functions with descriptions (`?`), effect declarations (`! [Effect]`), and type annotations
- `match` as the only branching construct (no `if`/`else` by design)
- No loops — `List.map`/`filter`/`fold` for iteration
- String interpolation with `{expr}`
- `|>` pipe operator
- `?` error propagation operator

### Type system
- Static type checker with named types, generics, `Result<T, E>`, `Option<T>`
- User-defined sum types and records
- Function types with effect annotations (`Fn(A) -> B ! [Effect]`)
- List pattern matching (`[]`, `[h, ..t]`)

### Built-in namespaces
- `Int`, `Float`, `String`, `List` — pure operations
- `Console` — print, error, warn, readLine (`! [Console]`)
- `Http` — GET, HEAD, DELETE, POST, PUT, PATCH (`! [Http]`)
- `Disk` — readText, writeText, appendText, exists, delete, deleteDir, listDir, makeDir (`! [Disk]`)
- `Tcp` — send, ping (`! [Tcp]`)

### Module system
- `module` blocks with `intent`, `depends`, `exposes`
- Dot-path imports (`depends [Examples.Foo]`)
- Named effect sets (`effects AppIO = [Console, Disk]`)

### Tooling
- `aver run` — execute programs
- `aver verify` — run verify blocks as tests
- `aver check` — static analysis (types, effects, style)
- `verify` blocks — declarative equality-based test cases
- `decision` blocks — architectural decisions as code
