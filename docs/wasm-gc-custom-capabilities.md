# Custom capabilities on `--target wasm-gc`

`aver compile app.av --target wasm-gc` turns every reached program-defined
capability into a deterministic raw WebAssembly-GC import interface. It does
not embed a provider implementation. A browser, Node, Deno, Bun, Worker, or
other JavaScript host supplies the imports when it instantiates the module.

`aver capabilities app.av` reports this as
`host-bound[wasm-gc-import-required]`. The target defines an ABI and never
picks an implementation; an external embedder supplies the imports itself.
When `[providers]` in `aver.toml` selects a Rust `ProviderBinding`,
`aver run --wasm-gc` builds the same cached host the VM uses and adapts that
binding through the generated ABI. The binding stays target-neutral. There is
no wasm-specific provider declaration.

The embedded adapter covers the whole provider vocabulary, including compound
values, arbitrary `Int`, bulk `Bytes`, represented records/sums, opaque
provider resources, and record/replay. Compiler-shipped standard capabilities
still use specialised `aver/*` imports. An explicit standard provider override
is rejected as `capability-provider-runner-adapter-unavailable` until those
older adapters can be replaced too. That diagnostic is a limitation of the
runner. It does not mean the wasm-gc target lacks the capability.

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

The leading `n` makes the hex encoding a valid identifier, and the encoding is
injective. The 64-digit SHA-256 contract hash is part of the import module
because it identifies the exact operation names and boundary types the host
must implement. Changing any contract member changes the link name, so linking
fails closed and never binds an old adapter by accident. `model_hash` is
audit/replay identity and plays no part in transport identity, so changing only
hostile profiles or verification laws does not rename the runtime ABI.

Every imported function takes its declared parameters in source order, then a
final `i32 caller_fn` used for diagnostics. `Unit` contributes no WebAssembly
value. A non-`Unit` return is the single function result.

## Value representation

The custom boundary uses the same native wasm-gc representation as the guest.
There is no JSON transport codec, and Aver values are not narrowed. The
embedded runner lifts these values directly into transport-neutral
`ProviderValue`s:

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

Boundary records and sums keep their nominal representation where erasing a
scalar wrapper would otherwise change their ABI. A proof-packed structural
refinement is the one deliberate exception. Its `make` and field inspector
aliases point at the generated pack/unpack helpers, so the declared record API
stays stable while byte-heavy providers keep the array representation end to
end.

## Host bridge exports

JavaScript can hold wasm-gc references but cannot run `struct.new` or
`array.new` itself. So the compiler exports constructors and inspectors for
every compound boundary type. Names are deterministic:

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

Sum `kind` values follow variant declaration order, starting at zero. `Unit`
payloads take no host parameter and produce no host result. Their structural
slot is filled internally, including inside `Option`, `List`, and `Vector`.

`Int` also exports `from_i64`, `to_i64_checked`, `from_decimal`, and
`to_decimal`. Hosts should use the decimal pair for arbitrary JavaScript
`BigInt`. It round-trips the whole mathematical integer, and parsing returns
Aver's ordinary `Result<Int, String>`. Strings cross to and from JavaScript
through the existing `memory`, `__rt_string_from_lm`, and `__rt_string_to_lm`
bridge.

`Bytes` has a matching bulk bridge. A host writes octets at `memory[0..n]` and
calls `__rt_bytes_from_lm(n)`. `__rt_bytes_to_lm(bytes)` copies the other way
and returns the written length. When `Result<Bytes, String>` is reachable,
`__rt_result_bytes_string_ok_from_lm(n)` does the inbound copy and the
`Result.Ok` wrap in one call. These exports keep the same ABI whether the
compiler proof-packed `Bytes` into a GC byte array or packing is disabled in an
internal differential-test build, so hosts never need one helper call per
octet.

An import function may close over an instance variable assigned right after
`WebAssembly.instantiate`. Calls happen when an exported Aver entry point
runs, so by then the provider can call the instance's factories and return
their GC references. Resource tokens can be plain JavaScript objects passed as
`externref`.

## Certificates

Artifact certificates accept the exact custom namespace above as an opaque
host capability. The verifier still re-reads the real import section, requires
the manifest's ordered pairs to match it byte for byte, checks the namespace
grammar and contract-hash width, and proves that certified pure closures do not
gain an implementation for those operations. Provider code is never included
in the certificate or credited by it.

The wasip2 route is separate: it lowers the supported canonical subset to WIT
and the Component Model. Raw wasm-gc uses native GC values on purpose, so it
can carry Aver's complete provider-value vocabulary today.

## Program-bound Work jobs

Work-shaped capabilities use the versioned `aver:work/v1` scheduling imports
and worker exports emitted by the compiler. Their task and result transport
uses the same typed construction and inspection helpers, and owned values are
copied between instances. See [Parallel Work on wasm-gc](wasm-work.md).
