# Aver — Standard Library Namespaces

All functions live in namespaces — no flat builtins (decision: `FullNamespaceEverywhere`).

## Aver source modules

Standard modules ship as ordinary Aver source embedded in the compiler. Import
them explicitly with `depends`; they do not depend on the current directory or
`--module-root`, and project files cannot shadow their reserved names.

### `Bytes` and `Crypto.Digest32`

```aver
module Packet
    depends [Bytes, Crypto.Digest32]

fn validate(payload: List<Int>) -> Result<Bytes, String>
    Bytes.fromList(payload)
```

`Bytes` is an opaque refinement over `List<Int>` whose values are all in
`0..=255`. `Digest32`, imported from `Crypto.Digest32`, is a nested refinement
requiring exactly 32 bytes.
Both remain ordinary Aver types and retain their invariants in Lean and Dafny
proof export.

`Bytes.fromList` written against a list literal whose every element is an
integer literal in `0..=255` cannot fail, so it types as plain `Bytes` — no
`?`, no `match`:

```aver
payload = Bytes.fromList([249, 190, 180, 217])   -- : Bytes
Tcp.sendBytes("127.0.0.1", 9, payload)
```

Anything else keeps `Result<Bytes, String>`: a variable, a computed list, a
computed element, or a literal outside `0..=255`. See
[language.md](language.md#operators) for the exact boundary.

| Function | Signature | Notes |
|---|---|---|
| `Bytes.fromList` | `List<Int> -> Result<Bytes, String>` | Validates every octet; `Result.Err` names the offending value and its index. An all-literal in-range list argument discharges to plain `Bytes` — see below |
| `Bytes.octets` | `Bytes -> List<Int>` | Exposes validated values |
| `Bytes.empty` | `() -> Bytes` | Empty byte sequence |
| `Bytes.len` | `Bytes -> Int` | Number of octets |
| `Bytes.concat` | `(Bytes, Bytes) -> Bytes` | Concatenates without revalidation |
| `Bytes.take` | `(Bytes, Int) -> Bytes` | Prefix of at most `count` octets |
| `Bytes.drop` | `(Bytes, Int) -> Bytes` | Octets after `count` positions |
| `Bytes.fromHex` | `String -> Result<Bytes, String>` | Even length, case-insensitive, no `0x` prefix |
| `Bytes.toHex` | `Bytes -> String` | Total, lowercase output |
| `Crypto.Digest32.fromBytes` | `Bytes -> Result<Digest32, String>` | Requires exactly 32 bytes |
| `Crypto.Digest32.bytes` | `Digest32 -> Bytes` | Forgets only the length refinement |
| `Crypto.Digest32.fromHex` | `String -> Result<Digest32, String>` | Hex decode plus exact-length validation |
| `Crypto.Digest32.toHex` | `Digest32 -> String` | Always 64 lowercase characters |

## Pure namespaces (no effects)

### `Bool` namespace

Source: `src/types/bool.rs`

| Function | Signature | Notes |
|---|---|---|
| `Bool.or` | `(Bool, Bool) -> Bool` | Logical OR |
| `Bool.and` | `(Bool, Bool) -> Bool` | Logical AND |
| `Bool.not` | `Bool -> Bool` | Logical NOT |

### `List` namespace

Source: `src/types/list.rs`

List is a recursive structure — use it for sequential processing with `prepend`, `take`, `drop`, and `match [h, ..t]`. For indexed access, use `Vector`.

| Function | Signature | Notes |
|---|---|---|
| `List.len` | `List<T> -> Int` | |
| `List.prepend` | `(T, List<T>) -> List<T>` | O(1) prepend |
| `List.take` | `(List<T>, Int) -> List<T>` | First `n` elements; negative `n` yields `[]` |
| `List.drop` | `(List<T>, Int) -> List<T>` | All but first `n` elements; negative `n` keeps the original list |
| `List.concat` | `(List<T>, List<T>) -> List<T>` | Concatenates two lists |
| `List.reverse` | `List<T> -> List<T>` | Returns a reversed copy |
| `List.contains` | `(List<T>, T) -> Bool` | Membership check via `==` |
| `List.zip` | `(List<A>, List<B>) -> List<(A, B)>` | Pairs elements, truncates to shorter list |

### `Vector` namespace

Source: `src/types/vector.rs`

Vector is a persistent indexed sequence — use it for grids, buffers, lookup tables, and anywhere you need O(1) access by index. Backed by `Rc<Vec<T>>` with copy-on-write: `set` mutates in place when the vector has a single owner, clones otherwise.

| Function | Signature | Notes |
|---|---|---|
| `Vector.new` | `(Int, T) -> Result<Vector<T>, String>` | Rejects sizes outside `0..=1_048_576`; a syntactic literal in that range discharges to plain `Vector<T>` |
| `Vector.get` | `(Vector<T>, Int) -> Option<T>` | O(1) indexed access |
| `Vector.set` | `(Vector<T>, Int, T) -> Option<Vector<T>>` | O(1) COW update; `None` if out of bounds |
| `Vector.len` | `Vector<T> -> Int` | |
| `Vector.fromList` | `List<T> -> Vector<T>` | Convert list to vector |
| `List.fromVector` | `Vector<T> -> List<T>` | Convert vector to list |

The `Vector.new` ceiling is one mebielement on every backend. It is an element budget, not a byte estimate: Aver has no portable storage layout for an arbitrary `T`, while the operation has the same observable cost shape everywhere—one slot and one clone per element. The lower bound also sits safely below wasm GC's `u32` array-addressability ceiling; that representation detail is no longer mistaken for a safe allocation policy.

### `Result` namespace

Source: `src/types/result.rs` + constructors in `src/vm/runtime.rs`.

| Function | Signature | Notes |
|---|---|---|
| `Result.Ok` | `T -> Result<T, E>` | Constructor |
| `Result.Err` | `E -> Result<T, E>` | Constructor |
| `Result.withDefault` | `(Result<T, E>, T) -> T` | Unwrap Ok or return default |

### `Option` namespace

Source: `src/types/option.rs` + constructors in `src/vm/runtime.rs`.

| Function | Signature | Notes |
|---|---|---|
| `Option.Some` | `T -> Option<T>` | Constructor |
| `Option.None` | `Option<T>` | Value (not a function) |
| `Option.withDefault` | `(Option<T>, T) -> T` | Unwrap Some or return default |
| `Result.fromOption` | `(Option<T>, E) -> Result<T, E>` | Convert Option to Result |

### `Int` namespace

Source: `src/types/int.rs`

| Function | Signature |
|---|---|
| `Int.fromString` | `String -> Result<Int, String>` |
| `Int.fromFloat` | `Float -> Int` |
| `String.fromInt` | `Int -> String` |
| `Float.fromInt` | `Int -> Float` |
| `Int.abs` | `Int -> Int` |
| `Int.min` | `(Int, Int) -> Int` |
| `Int.max` | `(Int, Int) -> Int` |
| `Int.mod` | `(Int, Int) -> Result<Int, String>` | A syntactic nonzero literal divisor discharges to plain `Int` |
| `Int.div` | `(Int, Int) -> Result<Int, String>` | A syntactic nonzero literal divisor discharges to plain `Int` |

### `Bits` namespace

Source: `src/types/bits.rs`

`Bits` is a **namespace, not a type**. Its arguments and its results are ordinary mathematical `Int` values; the namespace only fixes how those integers are *read* for the duration of one call. There is no bit-vector, no machine word, no `Word32`/`Word64`, and no persistent width — `Bits.and(6, 3)` takes two `Int`s and gives back an `Int`.

The reading is **infinite two's complement**: a non-negative integer has infinitely many leading zeroes, a negative one infinitely many leading ones, and `and` / `or` / `xor` / `not` operate pointwise on those infinite sequences. That is what makes them total on ℤ without a width to complement against, and it gives `Bits.and(-1, x) == x`, `Bits.or(-1, x) == -1`, `Bits.xor(-1, x) == Bits.not(x)` and `Bits.not(x) == -x - 1`.

Fixed-width behaviour is always **requested explicitly**, through `Bits.low`. Arithmetic on `Int` itself still never overflows and never wraps: `Bits.shiftLeft(1, 100)` is `1267650600228229401496703205376`, not `0`.

| Function | Signature |
|---|---|
| `Bits.and` | `(Int, Int) -> Int` |
| `Bits.or` | `(Int, Int) -> Int` |
| `Bits.xor` | `(Int, Int) -> Int` |
| `Bits.not` | `Int -> Int` |
| `Bits.shiftLeft` | `(Int, Int) -> Result<Int, String>` |
| `Bits.shiftRight` | `(Int, Int) -> Result<Int, String>` |
| `Bits.low` | `(Int, Int) -> Result<Int, String>` |

For a non-negative `n` and `width`:

- `Bits.shiftLeft(x, n)` is `x * 2^n`
- `Bits.shiftRight(x, n)` is `floor(x / 2^n)` — an **arithmetic** right shift, so `Bits.shiftRight(-3, 1) == -2`
- `Bits.low(x, width)` is `x mod 2^width` — the non-negative value of the lowest `width` bits, so `Bits.low(257, 8) == 1`, `Bits.low(-1, 8) == 255`, and `Bits.low(x, 0) == 0`

A negative shift count or width is `Result.Err` — never a panic, never a silent direction flip, never a clamp. The **16,777,216-bit materialization bound applies only where a result can grow**: always to `shiftLeft`, and to `low` when `x` is negative (extracting a finite low-bit value from infinite leading ones). Positive `low` returns `x` directly once `width` reaches its existing bit length. `shiftRight` never grows or materializes a count-sized value; an arbitrarily large non-negative count reaches `0` for non-negative `x` or `-1` for negative `x` in constant space.

Like `Int.div` / `Int.mod`, syntax can discharge the error. A bounded non-negative literal discharges `shiftLeft` and `low`; **any** non-negative literal discharges `shiftRight`. Thus `Bits.low(x, 32)` and `Bits.shiftRight(x, 100000000000000000000)` type as plain `Int`, while dynamic counts keep `Result<Int, String>` because they may still be negative.

Prefer `Bits.low` over a magic mask — it states the protocol invariant instead of implying it:

```aver
top = Bits.shiftRight(checksum, 25)
shifted = Bits.shiftLeft(Bits.low(checksum, 25), 5)
mixed = Bits.xor(shifted, value)
```

### `Float` namespace

Source: `src/types/float.rs`

| Function | Signature |
|---|---|
| `Float.fromString` | `String -> Result<Float, String>` |
| `Float.fromInt` | `Int -> Float` |
| `String.fromFloat` | `Float -> String` |
| `Float.abs` | `Float -> Float` |
| `Float.floor` | `Float -> Int` |
| `Float.ceil` | `Float -> Int` |
| `Float.round` | `Float -> Int` |
| `Float.min` | `(Float, Float) -> Float` |
| `Float.max` | `(Float, Float) -> Float` |
| `Float.sin` | `Float -> Float` | sine (radians) |
| `Float.cos` | `Float -> Float` | cosine (radians) |
| `Float.sqrt` | `Float -> Float` | square root |
| `Float.pow` | `(Float, Float) -> Float` | exponentiation |
| `Float.atan2` | `(Float, Float) -> Float` | two-argument arctangent |
| `Float.pi` | `() -> Float` | π constant |

### `String` namespace

Source: `src/types/string.rs`

| Function | Signature | Notes |
|---|---|---|
| `String.len` | `String -> Int` | Number of characters (Unicode scalar values), on every backend |
| `String.byteLength` | `String -> Int` | UTF-8 byte count |
| `String.toUtf8` | `String -> Bytes` | Total UTF-8 encoding; one linear byte copy |
| `String.fromUtf8` | `Bytes -> Result<String, String>` | One linear validation/decode; `Result.Err("invalid UTF-8")` for malformed input |
| `String.charAt` | `(String, Int) -> Option<String>` | Character at character index, or `Option.None` on out-of-bounds |
| `String.startsWith` | `(String, String) -> Bool` | |
| `String.endsWith` | `(String, String) -> Bool` | |
| `String.contains` | `(String, String) -> Bool` | |
| `String.slice` | `(String, Int, Int) -> String` | Character indices; out-of-range ends clamp |
| `String.trim` | `String -> String` | |
| `String.split` | `(String, String) -> List<String>` | |
| `String.replace` | `(String, String, String) -> String` | |
| `String.join` | `(List<String>, String) -> String` | |
| `String.chars` | `String -> List<String>` | Splits into characters (Unicode scalar values) |
| `String.fromInt` | `Int -> String` | |
| `String.fromFloat` | `Float -> String` | |
| `String.fromBool` | `Bool -> String` | |
| `String.toLower` | `String -> String` | Unicode-aware lowercase |
| `String.toUpper` | `String -> String` | Unicode-aware uppercase |

Repeated `String.charAt` / `String.slice` access through a recursive call cone
shares one hidden codepoint-to-UTF-8 boundary index. When a `charAt` result is
used only to dispatch on the character—directly or through recognised pure
helpers—the runtime reads its Unicode scalar without constructing the surface
`Option<String>`. Public positions and return types are unchanged; any String
use the compiler cannot eliminate stays on the general indexed path.

### `Map` namespace

Source: `src/types/map.rs`

| Function | Signature | Notes |
|---|---|---|
| `{}` (literal) | — | The empty map; type from context (annotation or expected type). No `Map.empty()` builtin since 0.17 — symmetric with `[]` for List. |
| `Map.fromList` | `List<(K, V)> -> Map<K, V>` | The key type must order; `Float`, `Map` and `Vector` cannot key a map |
| `Map.set` | `(Map<K, V>, K, V) -> Map<K, V>` | Returns new map with key set |
| `Map.get` | `(Map<K, V>, K) -> Option<V>` | |
| `Map.has` | `(Map<K, V>, K) -> Bool` | |
| `Map.remove` | `(Map<K, V>, K) -> Map<K, V>` | Returns new map without key |
| `Map.keys` | `Map<K, V> -> List<K>` | |
| `Map.values` | `Map<K, V> -> List<V>` | |
| `Map.entries` | `Map<K, V> -> List<(K, V)>` | |
| `Map.len` | `Map<K, V> -> Int` | |

**Sets**: `Map<T, Unit>` is the Aver way to have a set — see [language.md](language.md#sets) for usage and codegen lowering.

### Unicode code points

Source: `src/types/code_point.rs`. Aver has no surface `Char` type or
namespace; code-point operations belong to `String` and use `Int` scalar
values explicitly.

| Function | Signature | Notes |
|---|---|---|
| `String.firstCodePoint` | `String -> Option<Int>` | First Unicode scalar value, or `Option.None` for empty text |
| `String.fromCodePoint` | `Int -> Option<String>` | Code point to 1-char string, `Option.None` for surrogates/invalid |

### `Crypto` namespace

Source: `src/types/crypto.rs`; byte and digest types come from the embedded
`Bytes` and `Crypto.Digest32` Aver modules.

| Function | Signature | Notes |
|---|---|---|
| `Crypto.sha256` | `Bytes -> Digest32` | Pure, total SHA-256 over validated bytes. |

Import both nominal types with `depends [Bytes, Crypto.Digest32]`. Hashing is
deterministic and total over `Bytes`, so it requires neither an effect declaration
nor a `Result`:

```aver
fn doubleSha(bytes: Bytes) -> Digest32
    first = Crypto.sha256(bytes)
    Crypto.sha256(Crypto.Digest32.bytes(first))
```
## Effectful namespaces

**Namespace effect shorthand**: declaring `! [ServiceName]` covers all methods of that service. For example, `! [Disk]` covers the complete text, binary, metadata, and directory API listed below. You can still use granular declarations like `! [Disk.readBytesAt]` when you want to be precise. `aver check` suggests narrowing when a shorthand could be more specific.

The namespaces below are supplied by Aver's standard host runtime. A project can describe an additional host boundary as a [capability module](language.md#capability-modules): its operations participate in the same effect, Oracle, hostile-testing, proof-trust, provider, and replay machinery. Rust embedders can install one typed in-process `ProviderBinding` unchanged in either the VM or a generated Rust artifact. On wasip2, complete custom contracts containing only `Unit`, `Bool`, `Float`, and `String` cross as host-bound generated WIT imports; the component host, not Aver, supplies their implementation. Bare wasm-gc custom adapters remain unavailable and fail closed with a target-specific `unsupported(reason)` row. `aver capabilities FILE` shows the complete VM/Rust/wasm-gc/wasip2 matrix. `recorded` or `suppressed` replay can run without a live provider.

### `Args` namespace — use `! [Args.get]`

Source: `src/services/args.rs`

| Function | Signature | Notes |
|---|---|---|
| `Args.get` | `() -> List<String>` | Command-line arguments passed after `--` |

Usage: `aver run file.av -- arg1 arg2 arg3`

```aver
fn main() -> Unit
    ! [Args.get, Console.print]
    args = Args.get()
    Console.print(args)
```

### `Console` namespace — use `! [Console.print]`, `! [Console.error]`, etc.

Source: `src/services/console.rs`

| Function | Signature |
|---|---|
| `Console.print` | `T -> Unit` |
| `Console.error` | `T -> Unit` (writes to stderr) |
| `Console.warn` | `T -> Unit` (writes to stderr) |
| `Console.readLine` | `() -> Result<String, String>` |

### `Http` namespace — use granular effects (`! [Http.get]`, `! [Http.post]`, etc.)

Source: `src/services/http.rs`

| Function | Signature | Notes |
|---|---|---|
| `Http.get` | `String -> Result<Http.Response, String>` | |
| `Http.head` | `String -> Result<Http.Response, String>` | headers only, `body` is `""`; requests the identity encoding so `content-length` describes the resource |
| `Http.delete` | `String -> Result<Http.Response, String>` | |
| `Http.post` | `(String, String, String, Map<String, List<String>>) -> Result<Http.Response, String>` | url, body, content-type, headers |
| `Http.put` | `(String, String, String, Map<String, List<String>>) -> Result<Http.Response, String>` | |
| `Http.patch` | `(String, String, String, Map<String, List<String>>) -> Result<Http.Response, String>` | |

`Http.Response` record: `{ status: Int, body: String, headers: Map<String, List<String>> }`. Headers are a multimap — a single name can carry multiple values (Set-Cookie, Vary, …).

A response that by definition carries no body — any `Http.head` response, a `204 No Content`, a `304 Not Modified` — arrives with `body: ""` and its header fields intact. Every other response is read to the end, so a server that closes before sending the `Content-Length` it announced is an error, not a short body.

### Incoming HTTP — `HttpWire`, `HttpServer`, and `--handler`

Sources: `stdlib/http_wire.av`, `stdlib/http_server.av`

Incoming HTTP is not a provider callback. Native programs build it from two
ordinary Aver layers:

| Layer | Surface | Responsibility |
|---|---|---|
| `HttpWire` | `frameRequest`, `renderResponse`, `requestCloses`, `responseCloses` | Pure HTTP/1.1 framing over `Bytes` |
| `HttpServer` | `listen`, `serve` | Listener, poll loop, sessions, pipelining, writes, and cooperative shutdown over `Tcp` + `Process` |

```aver
module Hello
    depends [HttpServer]
    effects [Tcp, Process]

fn hello(req: HttpRequest) -> Http.Response
    Http.Response(status = 200, body = "hello {req.path}\n", headers = {})

fn main() -> Result<Unit, String>
    ! [Tcp.listen, Tcp.poll, Tcp.accept, Tcp.readSome, Tcp.writeBytes,
       Tcp.close, Tcp.closeListener, Process.stopRequested]
    HttpServer.listen(8080, hello)
```

`HttpServer.listen` has signature
`(Int, Fn(HttpRequest) -> Http.Response ! [_]) -> Result<Unit, String>`.
The `[_]` forwards the concrete named handler's effects to the call site: an
effectful handler still requires those exact effects in `main`; no ambient or
hidden grant is introduced.

`HttpRequest` is
`{ method: String, path: String, query: String, body: String, headers: Map<String, List<String>> }`.
`Http.Response` is
`{ status: Int, body: String, headers: Map<String, List<String>> }`.
Incoming header names are normalised to lowercase and repeated fields retain
wire order. The current pure framer deliberately supports bounded,
content-length HTTP/1.1 with UTF-8 request bodies; transfer encoding and
`Expect` are rejected before a body wait.

Fetch-style deployments do not run `HttpServer`: the host already owns the
listener and invokes one request handler. Select the same handler explicitly
with `--handler <fn>` (for example `aver compile app.av --preset cloudflare
--handler handler`, or the `wasi:http/proxy` world). This boundary stays a
simple `Fn(HttpRequest) -> Http.Response`; there is no synthetic listener call
in `main` and no provider-owned request token.

### `Disk` namespace — use granular effects (`! [Disk.readText]`, `! [Disk.writeText]`, etc.)

Contract source: `stdlib/capabilities/disk.av`. Native VM and generated Rust share the `aver-rt` Disk provider; wasm-gc keeps the existing `aver.disk_*` host imports and wasip2 keeps its WASI filesystem lowering. Signatures, Oracle classification, hostile profiles, replay semantics, and target accounting all derive from the same contract and model hashes. `aver.toml` path policy is enforced before the provider boundary on native targets.

| Function | Signature | Notes |
|---|---|---|
| `Disk.readText` | `String -> Result<String, String>` | |
| `Disk.writeText` | `(String, String) -> Result<Unit, String>` | path, content |
| `Disk.appendText` | `(String, String) -> Result<Unit, String>` | |
| `Disk.readBytes` | `String -> Result<Bytes, String>` | Reads the whole file as exact octets |
| `Disk.readBytesAt` | `(String, Int, Int) -> Result<Bytes, String>` | path, offset, maximum length; EOF returns a shorter `Bytes` value |
| `Disk.writeBytes` | `(String, Bytes) -> Result<Unit, String>` | Replaces the file with exact octets |
| `Disk.appendBytes` | `(String, Bytes) -> Result<Unit, String>` | Appends exact octets |
| `Disk.size` | `String -> Result<Int, String>` | File length in bytes |
| `Disk.exists` | `String -> Bool` | |
| `Disk.delete` | `String -> Result<Unit, String>` | Files only |
| `Disk.deleteDir` | `String -> Result<Unit, String>` | Recursive |
| `Disk.listDir` | `String -> Result<List<String>, String>` | |
| `Disk.makeDir` | `String -> Result<Unit, String>` | Creates parents |

`Disk.readBytesAt` is a single positional effect. It reads at most the requested length, returns `Ok(Bytes.fromList([]))` when the offset is at or beyond EOF, and rejects negative offsets or lengths. Reading a whole file remains a separate `Disk.readBytes` effect, so callers do not need the racy `size`-then-`readBytesAt` sequence.

### `Tcp` namespace — use granular effects (`! [Tcp.send]`, `! [Tcp.ping]`, etc.)

Contract source: `stdlib/capabilities/tcp.av`. Native VM and generated Rust
share the `aver-rt` Tcp provider; wasm-gc and wasip2 bind their existing host
lowerings to the same exact contract. Signatures, resource ownership,
Oracle classification, hostile profiles, replay semantics, and target
accounting derive from that source contract.

**One-shot (stateless):**

| Function | Signature |
|---|---|
| `Tcp.send` | `(String, Int, String) -> Result<String, String>` |
| `Tcp.sendBytes` | `(String, Int, Bytes) -> Result<Bytes, String>` |
| `Tcp.ping` | `(String, Int) -> Result<Unit, String>` |

**Persistent connections and readiness resources:**

| Function | Signature | Notes |
|---|---|---|
| `Tcp.connect` | `(String, Int) -> Result<Tcp.Connection, String>` | Provider-owned resource. Socket establishment has a 5-second default deadline on native and the in-process wasm-gc host; configure it with `[effects.Tcp].connect_timeout_secs`. wasip2 timing is host-controlled. |
| `Tcp.beginConnect` | `(String, Int) -> Result<Tcp.Dial, String>` | Starts a non-blocking outbound attempt. A `Dial` cannot be read or written. |
| `Tcp.dialled` | `Tcp.Dial -> Result<Option<Tcp.Connection>, String>` | `None` means still in flight (including a false wake), `Some` promotes the dial to a usable connection, and `Err` means refusal or deadline expiry. |
| `Tcp.listen` | `(Int, Int) -> Result<Tcp.Listener, String>` | Binds a port with the requested positive backlog. A listener cannot be read or written. |
| `Tcp.accept` | `Tcp.Listener -> Result<Option<Tcp.Connection>, String>` | Accepts at most one queued client without blocking; `None` is a legal false wake or an empty backlog. |
| `Tcp.peerAddress` | `Tcp.Connection -> Result<String, String>` | Returns the remote endpoint, including brackets around an IPv6 address. |
| `Tcp.writeLine` | `(Tcp.Connection, String) -> Result<Unit, String>` | Appends `\r\n` on the wire. |
| `Tcp.writeBytes` | `(Tcp.Connection, Bytes) -> Result<Unit, String>` | Exact bytes; nothing appended, nothing encoded. |
| `Tcp.readLine` | `Tcp.Connection -> Result<String, String>` | Strips the trailing `\r\n`; `Ok("")` on a clean EOF before any byte. |
| `Tcp.readBytes` | `(Tcp.Connection, Int) -> Result<Bytes, String>` | Reads exactly N bytes, no decoding. Short read is an error. |
| `Tcp.readSome` | `(Tcp.Connection, Int) -> Result<Bytes, String>` | Reads 1–N bytes without waiting to fill N; empty `Bytes` means clean EOF. |
| `Tcp.poll` | `(Map<Int, Tcp.Socket>, Int) -> Result<List<Int>, String>` | One wait over connected peers, in-flight dials, and listeners. Returns sorted caller IDs; `[]` means timeout. |
| `Tcp.close` | `Tcp.Connection -> Result<Unit, String>` | `Err("tcp: unknown connection ...")` on a double-close. |
| `Tcp.closeDial` | `Tcp.Dial -> Result<Unit, String>` | Cancels an in-flight attempt and invalidates the handle. |
| `Tcp.closeListener` | `Tcp.Listener -> Result<Unit, String>` | Releases the bound port; accepted connections remain live. |

`Tcp.Connection`, `Tcp.Dial`, and `Tcp.Listener` are distinct capability
**resources**: only the provider can mint them, while construction, field
reads, equality, hashing, and pattern matches are rejected by the type checker.
The distinction is typestate: `writeBytes` is not merely likely to fail on a
dial or listener; those calls do not typecheck. `Tcp.Socket` is an ordinary
represented sum used to keep all three states in one caller-owned map:

```aver
type Socket
    Listening(Tcp.Listener)
    Dialing(Tcp.Dial)
    Connected(Tcp.Connection)
```

The sum removes the old cross-map invariant. One key names exactly one socket
state, and exhaustive matching says what may happen next: `Listening` can be
accepted, `Dialing` can become `Connected`, and `Connected` can be read or
written. Native VM and generated Rust carry provider host tokens inside the
resource payloads; backend-specific socket tables and handles remain
implementation details.

Persistent session I/O has deliberately **no read or write deadline**. A
session operation may therefore wait indefinitely until it completes, reaches
EOF, or gets an actual I/O error. Once `readBytes`, `readSome`, `readLine`,
`writeBytes`, or `writeLine` begins touching the socket, any error removes that
connection from the provider pool. A failed exact read may already have
consumed part of a frame, and a failed write may already have sent part of its
payload; allowing a retry on the same handle would silently desynchronise the
protocol. Argument validation happens first, so a negative or oversized
`readBytes` count or invalid `readSome` maximum does not poison the connection.
Timeout errors are rendered without platform errno.

Socket establishment and one-shot request calls retain deployment defaults:

```toml
[effects.Tcp]
connect_timeout_secs = 5
request_idle_timeout_secs = 30
max_connections = 256
```

`request_idle_timeout_secs` applies only to blocking operations within
`Tcp.send` and `Tcp.sendBytes`, not to their total wall-clock duration and never
to persistent sessions. `max_connections` is shared by established outbound
and accepted connections plus in-flight dials; listeners and one-shot
`send`/`sendBytes`/`ping` calls do not occupy the pool. `accept` checks the pool
before removing a client from the OS backlog, so a full process does not
silently discard the pending connection. All three settings must be positive
integers. Effect
sections reject unknown or misplaced keys, so a typo cannot silently select a
default. Native VM, generated Rust, and the in-process wasm-gc host honour the
settings. wasip2 currently uses host-controlled WASI socket timing and a fixed
256-slot connected-socket pool; it emits a warning when a required operation
depends on a Tcp deployment setting that target cannot honour.

`Tcp.send` is stateless and ephemeral — it opens a fresh socket, writes the request bytes raw (no `\r\n` append), `shutdown(Write)` to signal end-of-request, then reads the peer's response until EOF, capped at 10 MiB. It does **not** touch the persistent-connection pool, so a program already holding its configured maximum of live session/dial handles can still issue `Tcp.send` to another peer. Stream errors (`stream-error.last-operation-failed`) surface as `Result.Err("tcp: stream error")`; a clean half-close (`stream-error.closed`) returns whatever the peer flushed.

`Tcp.sendBytes` is the byte-clean form of `Tcp.send`: same socket behaviour, but
the payload and response stay `Bytes` and no UTF-8 encoding or decoding
happens in either direction. Prefer it for any binary protocol. `Tcp.send`
decodes the response with `String::from_utf8_lossy`, which replaces every
non-UTF-8 sequence with U+FFFD — silently, irreversibly, and starting at the
first offending byte — so it is only safe for protocols whose responses are
valid UTF-8 text. Construct payloads with `Bytes.fromList` or `Bytes.fromHex`;
invalid octets are rejected at that refinement boundary before TCP is called.

`Tcp.readBytes` is the byte-clean form of `Tcp.readLine`, and the only way to
read a fixed number of bytes off a persistent connection. `readLine` frames on
`\n` — wrong for length-prefixed protocols, whose payloads carry `0x0A` at
arbitrary offsets — and goes through `BufRead::read_line`, which rejects
non-UTF-8 input outright. `readBytes` does neither: it reads exactly the
requested count and decodes nothing.

`Tcp.poll` is the one event-loop wait. The caller owns the `Int` keys in its
`Map<Int, Tcp.Socket>`, so the same keys can index protocol metadata without
making provider resources comparable. The standard provider watches all three
states with one poller and returns every readiness event it observes as a
sorted, duplicate-free subset of the supplied keys. An unknown or stale
resource makes the whole call `Err` rather than disappearing from the result.
The requested timeout is clipped to the nearest dial deadline, so an expiring
attempt cannot remain asleep behind a longer idle timeout.

A `Connected` key is ready for buffered input, stream readability, EOF, or an
observable error. A `Dialing` key is ready when establishment settles or its
deadline expires. A `Listening` key is ready when a client can be accepted.
Readiness is still a hint: false wakes are legal, so `dialled` and `accept` may
return `Ok(None)`, and the following operation can fail. Completeness is a
provider/runtime obligation over hidden host readiness, not a fabricated pure
Oracle law; the standard implementation tests that simultaneous connection,
dial, and listener events are all returned.

`readSome(connection, maxBytes)` performs one bounded read and returns as soon
as any bytes are available instead of waiting to fill `maxBytes`. `maxBytes`
must be positive and is capped at 10 MiB. Without a preceding `poll`, it may
wait indefinitely for the first byte. After `poll` reports the caller's peer
ID, a single-reader loop can call `readSome` to make progress without falling
back to exact-count blocking. Empty `Bytes` is reserved for clean EOF.

The returned payload is nominal `Bytes`; use `Bytes.octets` only when ordinary
list operations are needed.

A short read is an error rather than a truncated success, because fewer bytes
than a length prefix promised means the peer went away mid-message. The count is
capped at 10 MiB; a negative, oversized, or `i64`-overflowing count returns
`Result.Err` rather than trapping. `Tcp.readLine` is unchanged and remains the
right choice for line-oriented text protocols.

`Tcp.writeBytes` is the byte-clean form of `Tcp.writeLine`. `writeLine` appends
`\r\n` unconditionally — two bytes that desynchronise a length-prefixed stream —
and its `String` argument is UTF-8, so a codepoint above `0x7F` is re-encoded
into a multi-byte sequence: the single byte `0xF9` cannot be put on the wire at
all. `writeBytes` writes the nominal `Bytes` payload exactly as given. Build it
with `Bytes.fromList` or `Bytes.fromHex`; an invalid octet returns `Result.Err`
at that refinement boundary before any wire I/O, so a bad payload never
half-writes. An empty payload is a no-op. `Tcp.writeLine` is unchanged and
remains right for line-oriented text.

### `Random` namespace — use granular effects (`! [Random.int]`, `! [Random.float]`)

Contract source: `stdlib/capabilities/random.av`. Native VM and generated Rust share the `aver-rt` Random provider; wasm-gc keeps the existing `aver.random_*` imports and wasip2 keeps its WASI random lowering. Signatures, Oracle classification, hostile profiles, replay semantics, and target accounting all derive from the same contract and model hashes.

| Function | Signature | Notes |
|---|---|---|
| `Random.int` | `(Int, Int) -> Result<Int, String>` | Random integer in [min, max] inclusive; valid host-range literal bounds discharge the wrapper while the effect still runs |
| `Random.float` | `() -> Float` | Random float in [0.0, 1.0) |

Literal discharge is fail-closed: it removes user-side `Result` ceremony, not the provider contract. If a provider or Oracle stub returns `Err` for proven-valid literal bounds, execution faults as a contract violation. The compiler never substitutes `min`, `0`, or another apparently valid random sample.

### `Process` namespace — use `! [Process.stopRequested]`

Contract source: `stdlib/capabilities/process.av`. Native VM, generated Rust,
and the embedded wasm-gc wasmtime host install one process-wide SIGINT/SIGTERM
flag. Browser and Worker wasm-gc hosts satisfy `aver.process_stop_requested`
with `false`, because they have no process signal. WASI 0.2 has no corresponding
signal binding, so wasip2 rejects this effect at compile time and points to
`--target wasm-gc`.

| Function | Signature | Notes |
|---|---|---|
| `Process.stopRequested` | `() -> Bool` | Cooperative stop observation; once one call returns `true`, every later call on the same branch returns `true` |

The native handler only changes the flag from `false` to `true`; the flag is
never reset. Oracle hostile profiles obey the same cross-call law, and Lean
and Dafny receive it as a capability invariant. Poll at a cleanup-safe point:
the operation does not interrupt a blocking effect or run a shutdown hook.
See `examples/formal/process_stop_requested.av` for a recursive loop checked
against `stopAfterThree`.

### `Time` namespace — use granular effects (`! [Time.now]`, `! [Time.unixMs]`, `! [Time.sleep]`)

Contract source: `stdlib/capabilities/time.av`. Native VM and generated Rust share the `aver-rt` Time adapter; wasm-gc uses the existing `aver.time_*` imports and wasip2 uses WASI clocks/poll. All four bindings are checked/accounted against the same contract and model hashes.

| Function | Signature | Notes |
|---|---|---|
| `Time.now` | `() -> String` | Current UTC timestamp string (`...Z`) |
| `Time.unixMs` | `() -> Int` | Unix epoch milliseconds |
| `Time.sleep` | `Int -> Result<Unit, String>` | Rejects negative/out-of-host-range dynamic durations; a valid literal discharges the wrapper while the sleep still runs |

As with `Random.int`, a provider `Err` after literal discharge is a contract violation and faults; discharge does not turn a failed sleep into `Unit`.

### `Terminal` namespace — use granular effects (`! [Terminal.clear]`, `! [Terminal.readKey]`, etc.)

Source: `src/services/terminal.rs` (requires `terminal` feature, enabled by default)

| Function | Signature | Notes |
|---|---|---|
| `Terminal.enableRawMode` | `() -> Result<Unit, String>` | Enter raw mode (no line buffering, no echo) |
| `Terminal.disableRawMode` | `() -> Result<Unit, String>` | Leave raw mode |
| `Terminal.clear` | `() -> Result<Unit, String>` | Clear entire screen |
| `Terminal.moveTo` | `(Int, Int) -> Result<Unit, String>` | Move cursor to column x, row y; terminal I/O can still fail even for literal coordinates |
| `Terminal.print` | `String -> Result<Unit, String>` | Print at cursor position (no newline) |
| `Terminal.setColor` | `String -> Result<Unit, String>` | Set foreground: "red"/"green"/"yellow"/"blue"/"white"/"cyan"/"magenta"/"black" |
| `Terminal.resetColor` | `() -> Result<Unit, String>` | Reset colors to default |
| `Terminal.readKey` | `() -> Result<Option<String>, String>` | Non-blocking poll: `Ok(Some(key))`, `Ok(None)` when idle, or `Err` when the host input fails |
| `Terminal.size` | `() -> Result<Terminal.Size, String>` | Returns `Terminal.Size { width: Int, height: Int }`; querying the host terminal can fail |
| `Terminal.hideCursor` | `() -> Result<Unit, String>` | Hide cursor |
| `Terminal.showCursor` | `() -> Result<Unit, String>` | Show cursor |
| `Terminal.flush` | `() -> Result<Unit, String>` | Flush stdout |

All terminal operations expose the same adapter boundary: a broken output stream, failed mode change, or input error is a `Result.Err` that the program may propagate or handle. Their outcomes are recorded for deterministic replay; there is no split where `moveTo` is fallible but an adjacent `print` silently faults outside the language value.

Terminal guard: `aver run` installs a drop guard that restores the terminal (show cursor, reset colors, disable raw mode) even on panic or runtime error.

### `Env` namespace — use granular effects (`! [Env.get]`, `! [Env.set]`)

Source: `src/services/env.rs`

| Function | Signature | Notes |
|---|---|---|
| `Env.get` | `String -> Option<String>` | Returns `Option.None` for missing/unreadable variable |
| `Env.set` | `(String, String) -> Result<Unit, String>` | Invalid key/value format and host write failures stay catchable |

Runtime policy (`aver.toml`) can restrict allowed keys:

```toml
[effects.Env]
keys = ["APP_*", "PUBLIC_*"]
```
