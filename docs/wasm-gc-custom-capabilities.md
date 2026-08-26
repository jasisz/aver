# Custom capabilities on `--target wasm-gc`

`aver compile app.av --target wasm-gc` turns every reached program-defined
capability into a deterministic raw WebAssembly-GC import interface. It does
not embed a provider implementation. A browser, Node, Deno, Bun, Worker, or
other JavaScript host supplies the imports when it instantiates the module.

`aver capabilities app.av` reports this as
`host-bound[wasm-gc-import-required]`: the target defines an ABI but never
chooses an implementation. An external embedder supplies the imports itself.
When `[providers]` in `aver.toml` selects a Rust `ProviderBinding`,
`aver run --wasm-gc` builds the same cached host used by the VM and adapts that
binding through the generated ABI. The binding remains target-neutral; there
is no wasm-specific provider declaration.

The embedded adapter covers the complete provider vocabulary, including
compound values, arbitrary `Int`, bulk `Bytes`, represented records/sums,
opaque provider resources, and record/replay. Compiler-shipped standard
capabilities still use specialised `aver/*` imports; an explicit standard
provider override is rejected as
`capability-provider-runner-adapter-unavailable` until those older adapters
are replaceable too. That diagnostic is a runner limitation, not a claim that
the wasm-gc target lacks the capability.

## Import identity

Reaching one operation imports the capability's complete, sorted contract:

```text
module = aver:user/cap-n<module UTF-8 as lowercase hex>-c<contract hash hex>
field  = op-n<operation UTF-8 as lowercase hex>
```

For example, `Clock.now` under contract hash `sha256:abc…` becomes:

```text
aver:user/cap-n436c6f636b-cabc… / op-n6e6f77
```

The leading `n` makes the hex encoding a valid identifier and the encoding is
injective. The 64-digit SHA-256 contract hash is part of the import module
because it identifies the exact operation names and boundary types the host
must implement. Changing any contract member therefore changes the link name
and fails closed instead of binding an old adapter accidentally. `model_hash`
is audit/replay identity, not transport identity, so changing only hostile
profiles or verification laws does not rename the runtime ABI.

Every imported function takes its declared parameters in source order, then a
final `i32 caller_fn` used for diagnostics. `Unit` contributes no WebAssembly
value. A non-`Unit` return is the single function result.

## Value representation

The custom boundary uses the same native wasm-gc representation as the guest;
there is no JSON transport codec and no narrowing of Aver values. The embedded
runner lifts these values directly into transport-neutral `ProviderValue`s:

| Aver type | Raw wasm-gc boundary |
|---|---|
| `Bool` | `i32` (`0` or `1`) |
| `Float` | `f64` |
| `Int` | `$AverInt` GC reference, preserving `Int = ℤ` |
| `String` | GC byte-array reference |
| `Unit` | no value |
| `resource R` | nullable `externref`; the JavaScript value is the provider token |
| `Result`, `Option`, tuple, record | typed GC struct reference |
| sum type | nominal root reference carrying a typed variant struct |
| `List` | nullable typed cons reference |
| `Vector` | typed mutable GC array reference |
| `Map` | Aver's typed deterministic map reference |
| proof-packed `List<Int>` refinement | typed GC array reference; record factories/projectors bridge its declared carrier |

Boundary records and sums keep their nominal representation when scalar
wrapper erasure would otherwise change their ABI. A proof-packed structural
refinement is the deliberate exception: its `make` and field inspector aliases
point at the generated pack/unpack helpers, so the declared record API stays
stable while byte-heavy providers keep the array representation end to end.

## Host bridge exports

JavaScript can retain wasm-gc references but cannot directly execute
`struct.new` or `array.new`. The compiler therefore exports constructors and
inspectors for every compound boundary type. Names are deterministic:

```text
__cap_abi_n<exact Aver type spelling as UTF-8 hex>_<operation>
```

Examples include:

- `...Result..._{ok,err,tag,ok_value,err_value}`
- `...Option..._{some,none,tag,value}`
- `...List..._{cons,nil,is_empty,head,tail}`
- `...Vector..._{new,len,get,set}`
- `...Map..._{empty,set,get,len,keys}`
- record `{make,field_n<field-name-hex>}`
- sum `{kind,variant_n<variant-name-hex>_make,...}`

Sum `kind` values are zero-based variant declaration order. `Unit` payloads
consume no host parameter and produce no host result; their structural slot is
filled internally, including inside `Option`, `List`, and `Vector`.

`Int` additionally exports `from_i64`, `to_i64_checked`, `from_decimal`, and
`to_decimal`. Hosts should use the decimal pair for arbitrary JavaScript
`BigInt`; it round-trips the whole mathematical integer and returns Aver's
ordinary `Result<Int, String>` on parse. Strings cross JavaScript through the
existing `memory`, `__rt_string_from_lm`, and `__rt_string_to_lm` bridge.

`Bytes` has the equivalent bulk bridge. A host writes octets at `memory[0..n]`
and calls `__rt_bytes_from_lm(n)`; `__rt_bytes_to_lm(bytes)` copies the other
direction and returns the written length. When `Result<Bytes, String>` is
reachable, `__rt_result_bytes_string_ok_from_lm(n)` combines the inbound copy
with its `Result.Ok` wrapper. These exports keep the same ABI whether the
compiler proof-packed `Bytes` into a GC byte array or packing is disabled in an
internal differential-test build, so hosts never need a helper call per octet.

An import function may close over an instance variable assigned immediately
after `WebAssembly.instantiate`. Calls happen when an exported Aver entry point
runs, so the provider can then call the instance's factories and return their
GC references. Resource tokens can simply be JavaScript objects passed as
`externref`.

## Certificates

Artifact certificates accept the exact custom namespace above as an opaque
host capability. The verifier still re-reads the real import section, requires
the manifest's ordered pairs to match it byte-for-byte, checks the namespace
grammar and contract-hash width, and proves certified pure closures do not gain
an implementation for those operations. Provider code is never included in or
credited by the certificate.

The wasip2 route remains separate: it lowers the supported canonical subset to
WIT and the Component Model. Raw wasm-gc deliberately uses native GC values so
it can carry Aver's complete provider-value vocabulary today.
