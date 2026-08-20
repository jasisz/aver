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
| `Bytes.toList` | `Bytes -> List<Int>` | Exposes validated values |
| `Bytes.fromHex` | `String -> Result<Bytes, String>` | Even length, case-insensitive, no `0x` prefix |
| `Bytes.toHex` | `Bytes -> String` | Total, lowercase output |
| `Crypto.Digest32.fromBytes` | `Bytes -> Result<Digest32, String>` | Requires exactly 32 bytes |
| `Crypto.Digest32.toBytes` | `Digest32 -> Bytes` | Forgets only the length refinement |
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
| `Vector.new` | `(Int, T) -> Vector<T>` | Create vector of N elements, all set to default |
| `Vector.get` | `(Vector<T>, Int) -> Option<T>` | O(1) indexed access |
| `Vector.set` | `(Vector<T>, Int, T) -> Option<Vector<T>>` | O(1) COW update; `None` if out of bounds |
| `Vector.len` | `Vector<T> -> Int` | |
| `Vector.fromList` | `List<T> -> Vector<T>` | Convert list to vector |
| `List.fromVector` | `Vector<T> -> List<T>` | Convert vector to list |

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
| `Option.toResult` | `(Option<T>, E) -> Result<T, E>` | Convert Option to Result |

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
| `Int.mod` | `(Int, Int) -> Result<Int, String>` |
| `Int.div` | `(Int, Int) -> Result<Int, String>` |

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

A negative shift count or width is `Result.Err` — never a panic, never a silent direction flip, never a clamp. Like `Int.div` / `Int.mod`, a **syntactic non-negative integer literal** count discharges that error at compile time, so `Bits.low(x, 32)` types as plain `Int` while `Bits.low(x, width)` keeps `Result<Int, String>`.

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

### `Char` namespace

Source: `src/types/char.rs` — not a type, operates on `String`/`Int`.

| Function | Signature | Notes |
|---|---|---|
| `Char.toCode` | `String -> Int` | Unicode scalar value of first char |
| `Char.fromCode` | `Int -> Option<String>` | Code point to 1-char string, `Option.None` for surrogates/invalid |

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
    Crypto.sha256(Crypto.Digest32.toBytes(first))
```
## Effectful namespaces

**Namespace effect shorthand**: declaring `! [ServiceName]` covers all methods of that service. For example, `! [Disk]` is equivalent to `! [Disk.readText, Disk.writeText, Disk.appendText, Disk.exists, Disk.delete, Disk.deleteDir, Disk.listDir, Disk.makeDir]`. You can still use granular declarations like `! [Disk.readText]` when you want to be precise. `aver check` suggests narrowing when a shorthand could be more specific.

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
| `Http.get` | `String -> Result<HttpResponse, String>` | |
| `Http.head` | `String -> Result<HttpResponse, String>` | |
| `Http.delete` | `String -> Result<HttpResponse, String>` | |
| `Http.post` | `(String, String, String, Map<String, List<String>>) -> Result<HttpResponse, String>` | url, body, content-type, headers |
| `Http.put` | `(String, String, String, Map<String, List<String>>) -> Result<HttpResponse, String>` | |
| `Http.patch` | `(String, String, String, Map<String, List<String>>) -> Result<HttpResponse, String>` | |

`HttpResponse` record: `{ status: Int, body: String, headers: Map<String, List<String>> }`. Headers are a multimap — a single name can carry multiple values (Set-Cookie, Vary, …).

### `HttpServer` namespace — use `! [HttpServer.listen]` or `! [HttpServer.listenWith]`

Source: `src/services/http_server.rs`

| Function | Signature |
|---|---|
| `HttpServer.listen` | `(Int, Fn(HttpRequest) -> HttpResponse ! [...method-level effects...]) -> Unit` |
| `HttpServer.listenWith` | `(Int, T, Fn(T, HttpRequest) -> HttpResponse ! [...method-level effects...]) -> Unit` |

`HttpServer.listen` and `HttpServer.listenWith` accept top-level function values. `listenWith` is the preferred form when a handler needs configuration, connections, or other app state, because the context stays explicit instead of being hidden in closure capture.

This is the main intended use of function values in Aver: named handlers and callbacks with explicit types and explicit effects. Most user code still stays first-order.

The handler itself still uses exact method-level effects such as `Http.get`, `Tcp.readLine`, or `Console.print`. The server call does not widen those into namespace-level grants.

`HttpRequest` record: `{ method: String, path: String, body: String, headers: Map<String, List<String>> }`.
`HttpResponse` record: `{ status: Int, body: String, headers: Map<String, List<String>> }`.
Header keys are case-insensitive by convention (the runtime normalises incoming names to lowercase; outgoing should match).

The caller declares only `HttpServer.listen` / `HttpServer.listenWith`. The handler carries its own `! [...]` declaration; its effects are checked on the handler function itself rather than copied onto the caller.

### `Disk` namespace — use granular effects (`! [Disk.readText]`, `! [Disk.writeText]`, etc.)

Contract source: `stdlib/capabilities/disk.av`. Native VM and generated Rust share the `aver-rt` Disk provider; wasm-gc keeps the existing `aver.disk_*` host imports and wasip2 keeps its WASI filesystem lowering. Signatures, Oracle classification, hostile profiles, replay semantics, and target accounting all derive from the same contract and model hashes. `aver.toml` path policy is enforced before the provider boundary on native targets.

| Function | Signature | Notes |
|---|---|---|
| `Disk.readText` | `String -> Result<String, String>` | |
| `Disk.writeText` | `(String, String) -> Result<Unit, String>` | path, content |
| `Disk.appendText` | `(String, String) -> Result<Unit, String>` | |
| `Disk.exists` | `String -> Bool` | |
| `Disk.delete` | `String -> Result<Unit, String>` | Files only |
| `Disk.deleteDir` | `String -> Result<Unit, String>` | Recursive |
| `Disk.listDir` | `String -> Result<List<String>, String>` | |
| `Disk.makeDir` | `String -> Result<Unit, String>` | Creates parents |

### `Tcp` namespace — use granular effects (`! [Tcp.send]`, `! [Tcp.ping]`, etc.)

Source: `src/services/tcp.rs`

**One-shot (stateless):**

| Function | Signature |
|---|---|
| `Tcp.send` | `(String, Int, String) -> Result<String, String>` |
| `Tcp.sendBytes` | `(String, Int, Bytes) -> Result<Bytes, String>` |
| `Tcp.ping` | `(String, Int) -> Result<Unit, String>` |

**Persistent connections:**

| Function | Signature | Notes |
|---|---|---|
| `Tcp.connect` | `(String, Int) -> Result<Tcp.Connection, String>` | Opaque handle — see below. |
| `Tcp.writeLine` | `(Tcp.Connection, String) -> Result<Unit, String>` | Appends `\r\n` on the wire. |
| `Tcp.writeBytes` | `(Tcp.Connection, Bytes) -> Result<Unit, String>` | Exact bytes; nothing appended, nothing encoded. |
| `Tcp.readLine` | `Tcp.Connection -> Result<String, String>` | Strips the trailing `\r\n`; `Ok("")` on a clean EOF before any byte. |
| `Tcp.readBytes` | `(Tcp.Connection, Int) -> Result<Bytes, String>` | Reads exactly N bytes, no decoding. Short read is an error. |
| `Tcp.close` | `Tcp.Connection -> Result<Unit, String>` | `Err("tcp: unknown connection ...")` on a double-close. |

`Tcp.Connection` is **opaque** from the surface: construction is reserved to `Tcp.connect` and field reads / pattern matches are rejected by the type checker. The handle is purely an identity token — the caller has nothing to inspect inside it. The underlying socket lives in a thread-local `HashMap` (VM / self-host / wasm-gc-bridge, keyed by `AtomicU64` "tcp-N") or a 256-slot wasm-gc array (`--target wasip2`, slot allocated via first-free scan + monotonic counter generation). Either way, manually forging an id is impossible: the type checker rejects the constructor.

`Tcp.send` is stateless and ephemeral — it opens a fresh socket, writes the request bytes raw (no `\r\n` append), `shutdown(Write)` to signal end-of-request, then reads the peer's response until EOF, capped at 10 MiB. It does **not** touch the persistent-connection pool, so a program holding 256 live `Tcp.connect` handles can still issue `Tcp.send` to another peer. Stream errors (`stream-error.last-operation-failed`) surface as `Result.Err("tcp: stream error")`; a clean half-close (`stream-error.closed`) returns whatever the peer flushed.

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

The returned payload is nominal `Bytes`; use `Bytes.toList` only when ordinary
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
| `Random.int` | `(Int, Int) -> Int` | Random integer in [min, max] inclusive |
| `Random.float` | `() -> Float` | Random float in [0.0, 1.0) |

### `Time` namespace — use granular effects (`! [Time.now]`, `! [Time.unixMs]`, `! [Time.sleep]`)

Contract source: `stdlib/capabilities/time.av`. Native VM and generated Rust share the `aver-rt` Time adapter; wasm-gc uses the existing `aver.time_*` imports and wasip2 uses WASI clocks/poll. All four bindings are checked/accounted against the same contract and model hashes.

| Function | Signature | Notes |
|---|---|---|
| `Time.now` | `() -> String` | Current UTC timestamp string (`...Z`) |
| `Time.unixMs` | `() -> Int` | Unix epoch milliseconds |
| `Time.sleep` | `Int -> Unit` | Sleeps current thread for ms, runtime error on negative |

### `Terminal` namespace — use granular effects (`! [Terminal.clear]`, `! [Terminal.readKey]`, etc.)

Source: `src/services/terminal.rs` (requires `terminal` feature, enabled by default)

| Function | Signature | Notes |
|---|---|---|
| `Terminal.enableRawMode` | `() -> Unit` | Enter raw mode (no line buffering, no echo) |
| `Terminal.disableRawMode` | `() -> Unit` | Leave raw mode |
| `Terminal.clear` | `() -> Unit` | Clear entire screen |
| `Terminal.moveTo` | `(Int, Int) -> Unit` | Move cursor to column x, row y |
| `Terminal.print` | `a -> Unit` | Print at cursor position (no newline) |
| `Terminal.setColor` | `String -> Unit` | Set foreground: "red"/"green"/"yellow"/"blue"/"white"/"cyan"/"magenta"/"black" |
| `Terminal.resetColor` | `() -> Unit` | Reset colors to default |
| `Terminal.readKey` | `() -> Option<String>` | Non-blocking poll: "up"/"down"/"left"/"right"/"esc"/"q"/char or None |
| `Terminal.size` | `() -> Terminal.Size` | Returns `Terminal.Size { width: Int, height: Int }` |
| `Terminal.hideCursor` | `() -> Unit` | Hide cursor |
| `Terminal.showCursor` | `() -> Unit` | Show cursor |
| `Terminal.flush` | `() -> Unit` | Flush stdout |

Terminal guard: `aver run` installs a drop guard that restores the terminal (show cursor, reset colors, disable raw mode) even on panic or runtime error.

### `Env` namespace — use granular effects (`! [Env.get]`, `! [Env.set]`)

Source: `src/services/env.rs`

| Function | Signature | Notes |
|---|---|---|
| `Env.get` | `String -> Option<String>` | Returns `Option.None` for missing/unreadable variable |
| `Env.set` | `(String, String) -> Unit` | Runtime error on invalid key/value format |

Runtime policy (`aver.toml`) can restrict allowed keys:

```toml
[effects.Env]
keys = ["APP_*", "PUBLIC_*"]
```
