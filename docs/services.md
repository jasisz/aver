# Aver — Standard Library Namespaces

Every function lives in a namespace. There are no flat builtins (decision: `FullNamespaceEverywhere`).

## Aver source modules

Standard modules ship as ordinary Aver source embedded in the compiler. Import them explicitly with `depends`. They do not depend on the current directory or `--module-root`, and project files cannot shadow their reserved names.

### `Bytes` and `Crypto.Digest32`

```aver
module Packet
    depends [Bytes, Crypto.Digest32]

fn validate(payload: List<Int>) -> Result<Bytes, String>
    Bytes.fromList(payload)
```

`Bytes` is an opaque refinement over `List<Int>` whose values are all in `0..=255`. `Digest32`, imported from `Crypto.Digest32`, is a nested refinement that requires exactly 32 bytes. Both are ordinary Aver types and keep their invariants in Lean and Dafny proof export.

`Bytes.fromList` cannot fail when its argument is a list literal whose every element is an integer literal in `0..=255`. Such a call types as plain `Bytes`, with no `?` and no `match`:

```aver
payload = Bytes.fromList([249, 190, 180, 217])   -- : Bytes
Tcp.sendBytes("127.0.0.1", 9, payload)
```

Anything else keeps `Result<Bytes, String>`: a variable, a computed list, a computed element, or a literal outside `0..=255`. [language.md](language.md#operators) gives the exact boundary.

| Function | Signature | Notes |
|---|---|---|
| `Bytes.fromList` | `List<Int> -> Result<Bytes, String>` | Validates every octet; `Result.Err` names the offending value and its index. An all-literal in-range list argument discharges to plain `Bytes` (described above) |
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

List is a recursive structure. Use it for sequential processing with `prepend`, `take`, `drop` and `match [h, ..t]`. For indexed access, use `Vector`.

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

Vector is a persistent indexed sequence. Use it for grids, buffers, lookup tables and anywhere else you need O(1) access by index. It is backed by `Rc<Vec<T>>` with copy-on-write: `set` mutates in place when the vector has a single owner and clones otherwise.

| Function | Signature | Notes |
|---|---|---|
| `Vector.new` | `(Int, T) -> Result<Vector<T>, String>` | Rejects sizes outside `0..=1_048_576`; a syntactic literal in that range discharges to plain `Vector<T>` |
| `Vector.get` | `(Vector<T>, Int) -> Option<T>` | O(1) indexed access |
| `Vector.set` | `(Vector<T>, Int, T) -> Option<Vector<T>>` | O(1) COW update; `None` if out of bounds |
| `Vector.len` | `Vector<T> -> Int` | |
| `Vector.fromList` | `List<T> -> Vector<T>` | Convert list to vector |
| `List.fromVector` | `Vector<T> -> List<T>` | Convert vector to list |

The `Vector.new` ceiling is one mebielement on every backend. It counts elements, not bytes. Aver has no portable storage layout for an arbitrary `T`, but the operation has the same observable cost shape everywhere: one slot and one clone per element. This lower limit also sits safely below the `u32` array-addressability ceiling of wasm GC, so that representation detail is no longer mistaken for a safe allocation policy.

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

| Function | Signature | Notes |
|---|---|---|
| `Int.fromString` | `String -> Result<Int, String>` | |
| `Int.fromFloat` | `Float -> Int` | |
| `String.fromInt` | `Int -> String` | |
| `Float.fromInt` | `Int -> Float` | |
| `Int.abs` | `Int -> Int` | |
| `Int.min` | `(Int, Int) -> Int` | |
| `Int.max` | `(Int, Int) -> Int` | |
| `Int.mod` | `(Int, Int) -> Result<Int, String>` | A syntactic nonzero literal divisor discharges to plain `Int` |
| `Int.div` | `(Int, Int) -> Result<Int, String>` | A syntactic nonzero literal divisor discharges to plain `Int` |
| `Int.toBigEndian` | `(Int, Int) -> Result<Bytes, String>` | Unsigned `(value, width)` encoding |
| `Int.toLittleEndian` | `(Int, Int) -> Result<Bytes, String>` | Unsigned `(value, width)` encoding |
| `Int.fromBigEndian` | `Bytes -> Int` | Total unsigned decoding |
| `Int.fromLittleEndian` | `Bytes -> Int` | Total unsigned decoding |

The endian encoders produce exactly `width` octets, padded with zeroes on the most-significant side. They reject a negative value, a value that does not fit, and a width outside `0..=1048576`. When both arguments are literals, the `Result` discharges to plain `Bytes` only if the compiler can prove those conditions. Dynamic calls keep `Result<Bytes, String>`. Width zero encodes only zero, as `Bytes.empty()`, and both decoders read `Bytes.empty()` as zero. Signed fixed-width intent stays explicit: encode `Bits.low(value, 8 * width)`.

### `Bits` namespace

Source: `src/types/bits.rs`

`Bits` is a **namespace, not a type**. Its arguments and results are ordinary mathematical `Int` values. The namespace only fixes how those integers are *read* for the duration of one call. There is no bit-vector, no machine word, no `Word32`/`Word64` and no persistent width: `Bits.and(6, 3)` takes two `Int`s and returns an `Int`.

The reading is **infinite two's complement**. A non-negative integer has infinitely many leading zeroes, a negative one has infinitely many leading ones, and `and` / `or` / `xor` / `not` work pointwise on those infinite sequences. That makes them total on ℤ without a width to complement against. It also gives `Bits.and(-1, x) == x`, `Bits.or(-1, x) == -1`, `Bits.xor(-1, x) == Bits.not(x)` and `Bits.not(x) == -x - 1`.

Fixed-width behaviour is always **requested explicitly**, through `Bits.low`. Arithmetic on `Int` itself still never overflows or wraps: `Bits.shiftLeft(1, 100)` is `1267650600228229401496703205376`, not `0`.

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
- `Bits.shiftRight(x, n)` is `floor(x / 2^n)`, an **arithmetic** right shift, so `Bits.shiftRight(-3, 1) == -2`
- `Bits.low(x, width)` is `x mod 2^width`, the non-negative value of the lowest `width` bits, so `Bits.low(257, 8) == 1`, `Bits.low(-1, 8) == 255`, and `Bits.low(x, 0) == 0`

A negative shift count or width is `Result.Err`. It never panics, never silently flips direction and never clamps. The **16,777,216-bit materialization bound applies only where a result can grow**: always to `shiftLeft`, and to `low` when `x` is negative (extracting a finite low-bit value from infinite leading ones). Positive `low` returns `x` directly once `width` reaches its existing bit length. `shiftRight` never grows or materializes a count-sized value. An arbitrarily large non-negative count reaches `0` for non-negative `x`, or `-1` for negative `x`, in constant space.

As with `Int.div` / `Int.mod`, syntax can discharge the error. A bounded non-negative literal discharges `shiftLeft` and `low`, and **any** non-negative literal discharges `shiftRight`. So `Bits.low(x, 32)` and `Bits.shiftRight(x, 100000000000000000000)` type as plain `Int`. Dynamic counts keep `Result<Int, String>` because they may still be negative.

Prefer `Bits.low` over a magic mask. It states the protocol invariant instead of implying it:

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
| `String.trim` | `String -> String` | Trims Unicode `White_Space` from both ends (same set as Rust `str::trim`) |
| `String.split` | `(String, String) -> List<String>` | |
| `String.replace` | `(String, String, String) -> String` | |
| `String.join` | `(List<String>, String) -> String` | |
| `String.chars` | `String -> List<String>` | Splits into characters (Unicode scalar values) |
| `String.fromInt` | `Int -> String` | |
| `String.fromFloat` | `Float -> String` | |
| `String.fromBool` | `Bool -> String` | |
| `String.toLower` | `String -> String` | Unicode-aware lowercase |
| `String.toUpper` | `String -> String` | Unicode-aware uppercase |

Repeated `String.charAt` / `String.slice` access through a recursive call cone shares one hidden codepoint-to-UTF-8 boundary index. When a `charAt` result is used only to dispatch on the character, directly or through recognised pure helpers, the runtime reads its Unicode scalar without building the surface `Option<String>`. Public positions and return types do not change. Any String use the compiler cannot eliminate stays on the general indexed path.

### `Map` namespace

Source: `src/types/map.rs`

| Function | Signature | Notes |
|---|---|---|
| `{}` (literal) | — | The empty map. Its type comes from context (annotation or expected type). There has been no `Map.empty()` builtin since 0.17, matching `[]` for List. |
| `Map.fromList` | `List<(K, V)> -> Map<K, V>` | The key type must order; `Float`, `Map` and `Vector` cannot key a map |
| `Map.set` | `(Map<K, V>, K, V) -> Map<K, V>` | Returns new map with key set |
| `Map.get` | `(Map<K, V>, K) -> Option<V>` | |
| `Map.has` | `(Map<K, V>, K) -> Bool` | |
| `Map.remove` | `(Map<K, V>, K) -> Map<K, V>` | Returns new map without key |
| `Map.keys` | `Map<K, V> -> List<K>` | |
| `Map.values` | `Map<K, V> -> List<V>` | |
| `Map.entries` | `Map<K, V> -> List<(K, V)>` | |
| `Map.len` | `Map<K, V> -> Int` | |

**Sets**: Aver represents a set as `Map<T, Unit>`. See [language.md](language.md#sets) for usage and codegen lowering.

### Unicode code points

Source: `src/types/code_point.rs`. Aver has no surface `Char` type or namespace. Code-point operations belong to `String` and use `Int` scalar values explicitly.

| Function | Signature | Notes |
|---|---|---|
| `String.firstCodePoint` | `String -> Option<Int>` | First Unicode scalar value, or `Option.None` for empty text |
| `String.fromCodePoint` | `Int -> Option<String>` | Code point to 1-char string, `Option.None` for surrogates/invalid |

### `Crypto` namespace

Source: `src/types/crypto.rs`. The byte and digest types come from the embedded `Bytes` and `Crypto.Digest32` Aver modules.

| Function | Signature | Notes |
|---|---|---|
| `Crypto.sha256` | `Bytes -> Digest32` | Pure, total SHA-256 over validated bytes. |

Import both nominal types with `depends [Bytes, Crypto.Digest32]`. Hashing is deterministic and total over `Bytes`, so it needs neither an effect declaration nor a `Result`:

```aver
fn doubleSha(bytes: Bytes) -> Digest32
    first = Crypto.sha256(bytes)
    Crypto.sha256(Crypto.Digest32.bytes(first))
```
## Effectful namespaces

**Namespace effect shorthand**: declaring `! [ServiceName]` covers every method of that service. For example, `! [Disk]` covers the whole text, binary, metadata and directory API listed below. Granular declarations such as `! [Disk.readBytesAt]` still work when you want to be precise, and `aver check` suggests narrowing when a shorthand could be more specific.

Aver's standard host runtime supplies the namespaces below. A project can describe an additional host boundary as a [capability module](language.md#capability-modules). Its operations go through the same effect, Oracle, hostile-testing, proof-trust, provider and replay machinery. Rust embedders can install one typed in-process `ProviderBinding` unchanged in the VM, in a generated Rust artifact, or in the cached host behind `aver run/replay --wasm-gc`. The wasm-gc adapter covers the whole provider vocabulary through a raw ABI derived from the contract. On wasip2, complete custom contracts that contain only `Unit`, `Bool`, `Float` and `String` cross as host-bound generated WIT imports, and the component host supplies their implementation (Aver does not). `aver capabilities FILE` shows the full VM/Rust/wasm-gc/wasip2 matrix. `recorded` or `suppressed` replay can run without a live provider.

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

`Http.Response` record: `{ status: Int, body: String, headers: Map<String, List<String>> }`. Headers are a multimap: one name can carry several values (Set-Cookie, Vary, …).

A response that by definition has no body (any `Http.head` response, a `204 No Content`, a `304 Not Modified`) arrives with `body: ""` and its header fields intact. Every other response is read to the end. A server that closes before sending the `Content-Length` it announced therefore produces an error, not a short body.

### Incoming HTTP — `HttpWire`, `HttpServer`, and `--handler`

Sources: `stdlib/http_wire.av`, `stdlib/http_server.av`

Incoming HTTP does not use a provider callback. Native programs build it from two ordinary Aver layers:

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

`HttpServer.listen` has the signature `(Int, Fn(HttpRequest) -> Http.Response ! [_]) -> Result<Unit, String>`. The `[_]` forwards the effects of the concrete named handler to the call site. An effectful handler still requires those exact effects in `main`, and no ambient or hidden grant is introduced.

`HttpRequest` is `{ method: String, path: String, query: String, body: String, headers: Map<String, List<String>> }`. `Http.Response` is `{ status: Int, body: String, headers: Map<String, List<String>> }`. Incoming header names are normalised to lowercase, and repeated fields keep wire order. By design, the current pure framer supports bounded, content-length HTTP/1.1 with UTF-8 request bodies. Transfer encoding and `Expect` are rejected before a body wait.

Fetch-style deployments do not run `HttpServer`, because the host already owns the listener and invokes one request handler. Select the same handler explicitly with `--handler <fn>` (for example `aver compile app.av --preset cloudflare --handler handler`, or the `wasi:http/proxy` world). This boundary stays a plain `Fn(HttpRequest) -> Http.Response`. There is no synthetic listener call in `main` and no provider-owned request token.

### `Disk` namespace — use granular effects (`! [Disk.readText]`, `! [Disk.writeText]`, etc.)

Contract source: `stdlib/capabilities/disk.av`. The native VM and generated Rust share the `aver-rt` Disk provider. wasm-gc keeps the existing `aver.disk_*` host imports, and wasip2 keeps its WASI filesystem lowering. Signatures, Oracle classification, hostile profiles, replay semantics and target accounting all derive from the same contract and model hashes. On native targets, `aver.toml` path policy is enforced before the provider boundary.

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
| `Disk.sync` | `String -> Result<Unit, String>` | Flushes a file or a directory to stable storage |

`Disk.sync(path)` returns only after the named path's bytes AND metadata are on stable storage. It is `fsync`, not a buffer flush. The path may be a file or a directory, and the difference matters. On POSIX, syncing a file does not make its own directory entry durable, so the crash-safe recipe for a newly created file is `Disk.sync(file)` followed by `Disk.sync(parentDirectory)`. One sync after a batch of appends costs one flush. One sync per append costs one flush each.

Write the same two calls on every platform. Windows has no call that flushes a directory. A directory handle needs `FILE_FLAG_BACKUP_SEMANTICS` to open at all, and `FlushFileBuffers` refuses it for lack of write access. So on Windows, `Disk.sync` on a directory returns `Ok` without touching the disk. The guarantee still holds: NTFS journals the metadata the second call asks about, so the durability the program asked for is in place when the call returns. Syncing a file is a real flush on Windows, as everywhere else.

`Disk.readBytesAt` is a single positional effect. It reads at most the requested length, returns `Ok(Bytes.fromList([]))` when the offset is at or past EOF, and rejects negative offsets or lengths. Reading a whole file stays a separate `Disk.readBytes` effect, so callers do not need the racy `size`-then-`readBytesAt` sequence.

### `Tcp` namespace — use granular effects (`! [Tcp.send]`, `! [Tcp.ping]`, etc.)

Contract source: `stdlib/capabilities/tcp.av`. The native VM and generated Rust share the `aver-rt` Tcp provider. wasm-gc and wasip2 bind their existing host lowerings to the same exact contract. Signatures, resource ownership, Oracle classification, hostile profiles, replay semantics and target accounting derive from that source contract.

`--target wasip2` binds thirteen of the twenty operations: `connect`, `writeLine`, `writeBytes`, `writeNow`, `readLine`, `readBytes`, `readSome`, `readNow`, `poll`, `close`, and the three one-shot calls. The seven dial and listener operations (`beginConnect`, `dialled`, `listen`, `accept`, `peerAddress`, `closeDial`, `closeListener`) are refused at compile time on that target with `capability-target-unsupported`; see [`docs/wasip2.md`](wasip2.md). Every other shipped target binds all twenty.

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
| `Tcp.beginConnect` | `(String, Int) -> Result<Tcp.Dial, String>` | Starts a non-blocking outbound attempt. A `Dial` cannot be read or written. Not on wasip2. |
| `Tcp.dialled` | `Tcp.Dial -> Result<Option<Tcp.Connection>, String>` | `None` means still in flight (including a false wake), `Some` promotes the dial to a usable connection, and `Err` means refusal or deadline expiry. Not on wasip2. |
| `Tcp.listen` | `(Int, Int) -> Result<Tcp.Listener, String>` | Binds a port with the requested positive backlog. A listener cannot be read or written. Not on wasip2. |
| `Tcp.accept` | `Tcp.Listener -> Result<Option<Tcp.Connection>, String>` | Accepts at most one queued client without blocking; `None` is a legal false wake or an empty backlog. Not on wasip2. |
| `Tcp.peerAddress` | `Tcp.Connection -> Result<String, String>` | Returns the remote endpoint, including brackets around an IPv6 address. Not on wasip2. |
| `Tcp.writeLine` | `(Tcp.Connection, String) -> Result<Unit, String>` | Appends `\r\n` on the wire. |
| `Tcp.writeBytes` | `(Tcp.Connection, Bytes) -> Result<Unit, String>` | Exact bytes; nothing appended, nothing encoded. |
| `Tcp.writeNow` | `(Tcp.Connection, Bytes) -> Result<Int, String>` | Never blocks. Returns how many payload bytes the socket accepted this call, from 0 to the payload length; 0 for a non-empty payload means the socket would block, and a partial count is normal. |
| `Tcp.readLine` | `Tcp.Connection -> Result<String, String>` | Strips the trailing `\r\n`; `Ok("")` on a clean EOF before any byte. |
| `Tcp.readBytes` | `(Tcp.Connection, Int) -> Result<Bytes, String>` | Reads exactly N bytes, no decoding. Short read is an error. |
| `Tcp.readSome` | `(Tcp.Connection, Int) -> Result<Bytes, String>` | Reads 1–N bytes without waiting to fill N; empty `Bytes` means clean EOF. |
| `Tcp.readNow` | `(Tcp.Connection, Int) -> Result<Option<Bytes>, String>` | Never blocks. `None` means nothing is available right now; `Some(empty)` means clean EOF, as `readSome` reports it; `Some(bytes)` is one chunk of at most N bytes. |
| `Tcp.poll` | `(Map<Int, Tcp.Socket>, Int) -> Result<List<Int>, String>` | One wait over readable connections (`Connected`), writable connections (`Sending`), in-flight dials, and listeners. Returns sorted caller IDs; `[]` means timeout. A socket this runtime no longer knows is reported ready rather than failing the poll. On wasip2 only the two connection states, because that target mints no dial or listener, and there an unknown connection still fails the poll. |
| `Tcp.close` | `Tcp.Connection -> Result<Unit, String>` | `Err("tcp: unknown connection ...")` on a double-close. |
| `Tcp.closeDial` | `Tcp.Dial -> Result<Unit, String>` | Cancels an in-flight attempt and invalidates the handle. Not on wasip2. |
| `Tcp.closeListener` | `Tcp.Listener -> Result<Unit, String>` | Releases the bound port; accepted connections remain live. Not on wasip2. |

`Tcp.Connection`, `Tcp.Dial` and `Tcp.Listener` are distinct capability **resources**. Only the provider can mint them, and the type checker rejects construction, field reads, equality, hashing and pattern matches on them. The distinction is enforced by typestate: `writeBytes` on a dial or listener does not typecheck at all. `Tcp.Socket` is an ordinary represented sum that keeps every polled state in one caller-owned map:

```aver
type Socket
    Listening(Tcp.Listener)
    Dialing(Tcp.Dial)
    Connected(Tcp.Connection)
    Sending(Tcp.Connection)
```

The sum removes the old invariant that had to hold across several maps. One key names exactly one socket state, and exhaustive matching says what may happen next: `Listening` can be accepted, `Dialing` can become `Connected`, `Connected` can be read, and `Sending` can be written. `Connected` and `Sending` wrap the same resource. They are two readiness interests on one connection, so a caller who wants both directions registers the connection under two keys. The native VM and generated Rust carry provider host tokens inside the resource payloads. Backend-specific socket tables and handles stay implementation details.

Persistent session I/O deliberately has **no read or write deadline**. A blocking session operation may wait indefinitely until it completes, reaches EOF or gets an actual I/O error. `readNow` and `writeNow` are the two operations that never wait. Once `readBytes`, `readSome`, `readNow`, `readLine`, `writeBytes`, `writeNow` or `writeLine` begins touching the socket, any error removes that connection from the provider pool. A failed exact read may already have consumed part of a frame, and a failed write may already have sent part of its payload, so a retry on the same handle would silently desynchronise the protocol. Argument validation happens first, so a negative or oversized `readBytes` count or an invalid `readSome` maximum does not poison the connection. Timeout errors are rendered without platform errno.

Socket establishment and one-shot request calls keep deployment defaults:

```toml
[effects.Tcp]
connect_timeout_secs = 5
request_idle_timeout_secs = 30
max_connections = 256
```

`request_idle_timeout_secs` applies only to blocking operations inside `Tcp.send` and `Tcp.sendBytes`. It does not limit their total wall-clock duration and never applies to persistent sessions. `max_connections` is shared by established outbound connections, accepted connections and in-flight dials. Listeners and one-shot `send`/`sendBytes`/`ping` calls do not occupy the pool. `accept` checks the pool before removing a client from the OS backlog, so a full process does not silently discard the pending connection. All three settings must be positive integers. Effect sections reject unknown or misplaced keys, so a typo cannot silently select a default. The native VM, generated Rust and the in-process wasm-gc host honour the settings. wasip2 currently uses host-controlled WASI socket timing and a fixed 256-slot connected-socket pool, and it emits a warning when a required operation depends on a Tcp deployment setting that target cannot honour.

`Tcp.send` is stateless and ephemeral. It opens a fresh socket, writes the request bytes raw (no `\r\n` appended), calls `shutdown(Write)` to signal the end of the request, then reads the peer's response until EOF, capped at 10 MiB. It does **not** touch the persistent-connection pool, so a program that already holds its configured maximum of live session/dial handles can still issue `Tcp.send` to another peer. Stream errors (`stream-error.last-operation-failed`) surface as `Result.Err("tcp: stream error")`. A clean half-close (`stream-error.closed`) returns whatever the peer flushed.

`Tcp.sendBytes` is the byte-clean form of `Tcp.send`. The socket behaviour is the same, but the payload and response stay `Bytes` and no UTF-8 encoding or decoding happens in either direction. Prefer it for any binary protocol. `Tcp.send` decodes the response with `String::from_utf8_lossy`, which replaces every non-UTF-8 sequence with U+FFFD, silently and irreversibly, starting at the first offending byte. It is only safe for protocols whose responses are valid UTF-8 text. Build payloads with `Bytes.fromList` or `Bytes.fromHex`. Invalid octets are rejected at that refinement boundary before TCP is called.

`Tcp.readBytes` is the byte-clean form of `Tcp.readLine`, and the only way to read a fixed number of bytes off a persistent connection. `readLine` frames on `\n`, which is wrong for length-prefixed protocols whose payloads carry `0x0A` at arbitrary offsets. It also goes through `BufRead::read_line`, which rejects non-UTF-8 input outright. `readBytes` does neither. It reads exactly the requested count and decodes nothing.

`Tcp.poll` is the one event-loop wait. The caller owns the `Int` keys in its `Map<Int, Tcp.Socket>`, so the same keys can index protocol metadata without making provider resources comparable. The standard provider watches all four states with one poller and returns every readiness event it sees as a sorted, duplicate-free subset of the supplied keys. An unknown or stale resource makes the whole call `Err` instead of quietly dropping out of the result. The requested timeout is clipped to the nearest dial deadline, so an expiring attempt cannot stay asleep behind a longer idle timeout.

Whatever runs between two polls is one turn, and every peer waits for it. `aver check` flags a turn that hands control to an effectful loop as `warning[serve-path]`, and `aver verify` measures turns against `[verify] turn-budget`.

A `Connected` key is ready for buffered input, stream readability, EOF or an observable error. A `Sending` key is ready when the next `writeNow` on that connection will accept at least one byte or fail. A `Dialing` key is ready when establishment settles or its deadline expires. A `Listening` key is ready when a client can be accepted. Readiness is still only a hint. False wakes are legal, so `dialled` and `accept` may return `Ok(None)`, `readNow` may return `Ok(None)`, `writeNow` may accept nothing, and the next operation can fail. Completeness is an obligation on the provider and runtime over hidden host readiness. It is not written as a made-up pure Oracle law. The standard implementation tests that simultaneous connection, dial and listener events are all returned.

`readSome(connection, maxBytes)` does one bounded read and returns as soon as any bytes are available, without waiting to fill `maxBytes`. `maxBytes` must be positive and is capped at 10 MiB. Without a preceding `poll`, it may wait indefinitely for the first byte. After `poll` reports the caller's peer ID, a single-reader loop can call `readSome` to make progress without falling back to exact-count blocking. Empty `Bytes` is reserved for clean EOF.

The returned payload is nominal `Bytes`. Use `Bytes.octets` only when you need ordinary list operations.

`readNow(connection, maxBytes)` is the non-blocking form of `readSome`. It does the same single bounded read but returns immediately. `Ok(None)` means nothing is available right now, which is the case where `readSome` would have waited. `Ok(Some(empty))` is clean EOF, exactly as `readSome` reports it, and `Ok(Some(bytes))` is one chunk of at most `maxBytes`. `Err` is a transport failure and poisons the connection. Bytes left buffered by an earlier `readLine` count as available. The argument rules are the same as for `readSome`: `maxBytes` must be positive and is capped at 10 MiB, and an invalid maximum is a catchable error that leaves the handle live.

A short read is an error and never a truncated success: fewer bytes than a length prefix promised means the peer went away mid-message. The count is capped at 10 MiB. A negative, oversized or `i64`-overflowing count returns `Result.Err` instead of trapping. `Tcp.readLine` is unchanged and is still the right choice for line-oriented text protocols.

`Tcp.writeBytes` is the byte-clean form of `Tcp.writeLine`. `writeLine` always appends `\r\n`, two bytes that desynchronise a length-prefixed stream. Its `String` argument is UTF-8, so a codepoint above `0x7F` is re-encoded into a multi-byte sequence, and the single byte `0xF9` cannot be put on the wire at all. `writeBytes` writes the nominal `Bytes` payload exactly as given. Build it with `Bytes.fromList` or `Bytes.fromHex`. An invalid octet returns `Result.Err` at that refinement boundary before any wire I/O, so a bad payload never half-writes. An empty payload is a no-op. `Tcp.writeLine` is unchanged and is still right for line-oriented text.

`Tcp.writeNow` is the non-blocking form of `writeBytes`. It writes as many payload bytes as the socket accepts right now and returns that count, from 0 to the payload length. 0 for a non-empty payload means the socket would block. A partial count is normal: the caller keeps the remainder (`Bytes.drop`) for a later call, usually after `poll` reports the connection's `Sending` key. An empty payload returns 0 without touching the socket. `Err` is a transport failure and poisons the connection, so a refused or partial write is never an error. The native provider switches the stream to non-blocking mode for that one call and restores it afterwards, so the blocking operations keep their contracts on the same connection.

A program that writes with `writeNow` carries the queue itself. The shape that works is a bounded outbox per connection: the payload at the head, an exact count of how many of its bytes have already gone, and the rest of the queue behind it. The turn that a `Sending` key reports writes from that count and advances it by what `writeNow` accepted. The connection is registered under its `Sending` key only while bytes remain, so a connection with nothing to send is never woken for writability. When the outbox is full, the slow peer pays for it: drop the peer, or drop what it asked for, instead of letting one connection's backlog grow without bound.

The easy mistake is a bulk producer, where one request is answered by many large payloads. Enqueueing all of them as soon as the request arrives fills the outbox from the program's own side. A healthy peer is then dropped for an overflow it did not cause: it was reading as fast as the wire allowed, and the program produced faster. Keep the cheap thing instead, which is the list of what was asked for, and render the next payload only once the outbox has fallen below a watermark. "Common patterns" in the bundled language guide has a sketch.

### Jobs — `Work` and `Wait`

Contract sources: `stdlib/capabilities/work.av` and `stdlib/capabilities/wait.av`. A job is long pure work that a program starts off the turn, so the turn can go back to serving peers, and collects in a later turn. The program never sees a thread. It holds a handle, the schedule stays data, and `Wait.poll` is the one wait of a turn.

| Function | Signature | Notes |
|---|---|---|
| `Work.cancel` | `(Work.Job) -> Unit` | Takes a queued job out of the queue so it never starts, stops a running job at the runtime's next cancellation check, drops a finished job's result, and changes nothing for a job already cancelled or taken. It answers nothing and refuses nothing. A job that finished before the cancel shows up in `take`, so that is where a hostile world lives. |
| `Wait.poll` | `(Map<K, Wait.Item>, Int) -> Result<List<K>, String>` | One wait over sockets and jobs together. `K` is any type a map accepts as a key. Returns the ready keys in the map's key order; `[]` means the timeout passed or, in a program that watches for one, a stop request arrived. A socket the runtime no longer knows (closed, or dropped after an I/O error) is reported ready, never an `Err` of the whole wait. A timeout beyond a century is capped at a century, because past that it means "no deadline within reach", which is the wait such a number asked for. |

A wait is keyed by whatever the program keys its map by. `Tcp.poll` keys by whole numbers because the sockets it watches are all one kind of thing. A wait is the one place where several parts of a program say at once what they are waiting for, so the key is the program's own way of saying which part a readiness belongs to:

```aver
type Watch
    Peer(Int)
    Write(Int)
    Board(Int)
    Listener
    Job

fn interests(eye: Eye, job: Work.Job) -> Map<Watch, Wait.Item>
    Map.set(boardItems(eye.board, peerItems(eye.peers)), Watch.Job, Wait.Item.Job(job))
```

`Int` is one such key, and every program written before the key became a choice keeps working unchanged. The hostile profiles `aver verify --hostile` answers `Wait.poll` with are written for `Int` keys (`Map<Int, Wait.Item>`), although the operation is generic in `K`. A wait keyed by a type of the program's own is verified with the declared answers and stubs, not with those profiles, and no law can yet put a running job into a verified wait. A key type of the program's own has two benefits. When two owners collide, the compiler sees a duplicate constructor, where an `Int` key would give an arithmetic accident that nothing catches. And the ready keys can be matched on and passed down instead of decoded back out of an integer. A map orders a variant by its constructor name and then its payload, and the ready keys come back in exactly that order, so the answer is as ordered as the `Int` answer was. Every rule for map keys applies here too: a `Float`, a `Map`, a `Vector` or a record with a `Float` field cannot key a wait, and neither can a capability resource, whose identity is deliberately not observable.

The key type of a wait is the key type of the map it was handed, and the type checker settles it. A wait set written at the call is keyed by what it holds. One built by another function and passed in is keyed by what that function returns. Nothing needs an annotation.

One program uses one wait key type. A turn has one wait, the key is how the program says what it is waiting for, and the keys leave every backend through one set of helpers built from that type. A program that keys one wait one way and another wait another way is refused. The fix is to name both kinds as constructors of one type. A wait whose key type nothing settles is refused for the same reason, and the fix is to bind the map to a name with its type written down. `aver check` reads one file at a time, so a program whose modules disagree about the key is refused at the compile door, which reads the entry module and the modules it depends on together. `--target wasip2` keys a wait by `Int`. It carries the wait through canonical ABI imports instead of the helpers an external host walks, and a wait keyed by another type is refused for that target.

A wait set written empty at the call is a turn with nothing to watch. It holds nothing, so it names no key of its own and is read as keyed by `Int`, which is what such a call has always meant. In a program that keys its waits by a type of its own, that reads as a second key type and the program is refused by name. Write the type on that one set:

```aver
idle: Map<Watch, Wait.Item> = {}
ready = Wait.poll(idle, 10)?
```

`Work.Job` is a provider-owned resource, exactly like `Tcp.Connection`. A program can hold it, pass it and put it in a wait set, but cannot construct it, read it, compare it or use it as a `Map` key. `Wait.Item` is the sum that lets one wait set hold both kinds of thing: `Socket(Tcp.Socket)` for everything `Tcp.poll` watches, and `Job(Work.Job)` for a running job. A `Socket` item follows the `Tcp.poll` readiness rule exactly. A `Job` key is ready once its job has finished or was cancelled. False-positive readiness is legal on both, so the next operation the caller runs still has to handle "nothing yet". A socket or a job the runtime no longer knows is reported ready too: a connection the program closed, or one the runtime dropped after an I/O error, while some request still waits on it. The operation the caller runs on it next answers with the real error, so one stale key never fails the whole wait, and a generated turn, which propagates an `Err` of its wait, is not ended by it. On `wasip2`, whose connected sockets are lowered separately, an unknown connection still fails the wait.

A module that answers a capability says why it has no answer yet with `Run.Wake`, from the standard module `Run`; see [Capabilities the program answers](#capabilities-the-program-answers--answers-runwake-and-the-generated-loop) below. `Wait` itself holds only `Item` and `poll`.

The stdlib deliberately owns only the handle and the cancel. What a job takes and what it returns is the program's own business, so a *job kind* is an ordinary program capability of **Work shape**: `kind = capability`, `depends [Work]`, and exactly two operations:

```
operation begin(task: T) -> Result<Work.Job, String>
operation take(job: Work.Job) -> Result<Option<R>, String>
```

`T` and `R` are ordinary program data (records, sums, lists, maps, scalars), with no function types and no capability resources inside either. Parameter names are free. The operation names, their arity and their result shapes are fixed. A capability that names `Work.Job` at its boundary without this shape is `error[work-shape]`.

A job kind may declare `T` and `R` itself, and it may also name data types of the modules it lists in `depends`. It is the only kind of capability allowed to do that. An ordinary capability must not depend on the program it serves, so it stays closed over its own declarations, and naming another module's type there is still refused. The difference is who answers. A job is answered by a function of the same program, so no host package has to track a layout that lives somewhere else. The job kind and the module whose type it names always ship together.

```
module DecodeJob
    kind = capability
    semantics = effectful
    depends [Work, Ledger]
    exposes [begin, take]

operation begin(task: Ledger.Request) -> Result<Work.Job, String>
operation take(job: Work.Job) -> Result<Option<List<Ledger.Tx>>, String>
```

The cost is visible on purpose. The layouts of `Ledger.Request` and `Ledger.Tx`, and of every type they reach, are inside this job kind's `contract_hash`. Adding a field to `Ledger.Tx` moves that hash. That invalidates recordings made before the edit and means a deployment pack must be rebuilt, the same as adding a field to a record declared inside the job kind's own module. `aver capabilities` prints the one hash as before. What it covers is now written in the descriptor as one layout row per named type, under the name of the module that declares it, so a capability-local `Tx` and a dependency's `Ledger.Tx` stay two identities.

The owner has to be spelled out. A bare name in a capability's operation still means that capability's own module, so nothing picks this up by accident. Two things must hold for the name: the module is in `depends`, and the module exposes the type. `depends` says which module a job kind may reach into, and `exposes` says what it finds there. These are the same two gates an ordinary fn passes when it names another module's type, so a record its author did not expose never ends up inside a published `contract_hash`.

Types reached *through* a named type need no further declaration. They come with the layout and are hashed with it. Each is bound under the name of the module that declares it, however the module holding the field wrote it. `Ledger.Tx` may write `info: Meta.Info` or, when it depends on `Meta`, just `info: Info`. Either way the layout enters the hash as `Meta::Info` and crosses the boundary at run time as `Meta.Info`. That keeps a capability-local `Info` and a dependency's `Meta.Info` as two identities on every backend.

What a job may not carry has not changed, and it is now also checked through dependency types. A named dependency type that holds a capability resource anywhere inside it (a connection, a dial, a listener, a job handle, or a `Tcp.Socket`, which is a sum of those) is refused at compile time, and the refusal names the field and the type:

```
operation 'Infra.BlockJobs.begin' parameter 0 names dependency type 'Infra.Tending.Kept', whose field 'sockets' has type 'Tcp.Socket', which holds capability resource 'Tcp.Listener'; a job carries its task and its reply off the turn as plain data, so no type on a job boundary may hold a resource
```

A named type the program does not declare is refused the same way, because `contract_hash` can only bind a layout it can read. A name the owning module declares as a `resource` is refused as a resource, because a resource is a provider's handle and has no layout on purpose.

A deployment pack carries the modules whose types a packed job kind named, next to the job kind's own contract source, so its host recomputes the same hash without the project tree.

Who runs the job is a deployment choice and not part of the contract, so it is a binding in `aver.toml`:

```toml
[[providers.bindings]]
capability = "Validation"
work = "Node.validate"
```

`work` names one module-qualified function of the same program, `validate(task: T) -> R`, with no effect list. A job runs off the turn, so the function that runs it must be pure. Its parameter and result must be exactly the `begin` task type and the `take` payload type: the same type of the same module, and a record that only shares the name does not count. `work` cannot be combined with the `crate`/`package`/`factory` binding a native Rust provider uses, because a job kind is always answered by the program and never by a host package. One capability takes one binding. The function belongs to a module the program depends on, not to the entry module the command was pointed at. A job reaches its function through the module that owns it, and the entry unit is not one of those. Each of these is `error[work-binding]`: a Work-shaped capability with no binding, or a binding that names a function that does not exist, a function of the entry module, a function with effects, or a function with the wrong types. `aver run`, `aver check` and `aver verify` report it at the program door instead of silently changing what the program means. A capability module checked on its own is not a program yet, so it needs no binding.

False-positive readiness is why a `take` can answer `Ok(None)` after a wait already reported the job. A reported key means "ask", not "it is done". Whoever asked keeps the handle and asks again in a later turn; a handle that is dropped is a job that keeps running with nobody left to collect it. A `take` that answers `Err` is the other end of that. A job that was cancelled, whose body stopped, or whose id the engine has forgotten will never produce a payload, so the handle is finished with. What that means is up to the program: an answer module that began the job answers the request with the error, or records it, and the run continues. A wait is not ended by a job outside its own set either. The engine's signal is per engine, so the wait checks its whole set again, sockets included, and goes back to sleep until something of its own is ready or the timeout runs out. A wait over sockets and jobs therefore still reports a socket that became ready after a job outside the set woke it.

The table answers "already taken" and "job cancelled" from a bounded number of dead slots: at most 4096, oldest forgotten first. A slot is dead once its answer was collected or its job was cancelled. A program that collects or cancels more jobs than that over its life gets `Err("work: unknown job")` for a handle whose slot has been forgotten, from whichever job kind it asks. The alternative would be a store of tombstones that grows for as long as the program runs. A job that is queued, running, or finished and never collected or cancelled keeps its slot, because its answer is still to come or still there for whoever holds the handle. Which job kind began a job is kept in the job's own slot, so the answer "not started by job kind 'K'" lives exactly as long as the slot does, and nothing a job kind keeps grows with the number of jobs it starts.

The bytecode VM runs jobs. `begin` returns the handle at once and starts the bound function on its own thread, or queues it when the job limit is reached. `take` answers `Ok(None)` while the job is queued or runs and `Ok(Some(result))` once it has finished. `Work.cancel` stops it, and `Wait.poll` returns as soon as a socket is ready, a job settles, the timeout elapses, or a stop request arrives in a program that watches for one. `begin` never blocks the turn and never refuses at the limit. A second `take` of the same job answers `Err("work: job already taken")`, and taking a cancelled job answers `Err("work: job cancelled")`. A handle belongs to the job kind that started it. Because `Work.Job` is one type, a handle from one job kind type-checks as an argument to another kind's `take`. The runtime answers that with `Err("work: this job was not started by job kind 'K'")` instead of handing over an answer the program never asked for. When the program ends, jobs still running are cancelled and the runtime waits for them only briefly.

How many jobs may run at once is a deployment choice, so it also lives in the manifest. It is how much of the host the program uses and nothing the program can observe. Without it, a program gets the host's own available parallelism:

```toml
[work]
max-jobs = 4
```

`max-jobs` must be a positive integer. Zero would mean a program that can never start a job, and it is refused when `aver.toml` is read.

`begin` at the limit never refuses and never waits. It queues the job and answers its handle at once. A queued job starts, in the order it was begun, as soon as a running body stops; until then `take` answers `Ok(None)` and a wait does not report it ready. So the same program gives the same answers on a host with one core and on a host with sixty-four, a recording made on one replays on the other, and the limit decides only how many bodies run at the same moment. The queue has no bound of its own: a program that begins jobs faster than they finish holds every task it began in memory, exactly as it would hold them in a list of its own.

Cancelling follows the queue. A queued job that is cancelled leaves the queue and never starts. A running job that is cancelled keeps its place under the limit until its body stops, and that stop is what lets the next queued job start. How long that takes depends on the backend. The VM's interpreter stops the body at its next cancellation check, wasm-gc traps it at the next epoch check (at most about 10 ms), the JavaScript adapter terminates its Worker at once, and generated Rust checks no flag, so there the body runs to completion and the jobs queued behind it wait for it. On `wasip2` there is nothing to size, because a job runs inline at `begin` and the key is ignored.

Jobs run in parallel on the VM, under `--target rust`, on the wasm-gc native runner and Wasmtime packs, and in the JavaScript adapter's Workers. On `wasip2` they do not: each one runs to completion inside `begin`.

Recording a turn records `begin` with its task and the handle it minted, `take` with the answer it gave, and `poll` with the keys it reported, the same way `Tcp.dial` records a `Dial`. A wait key records as the value it is: a whole number records as a number, and a key of the program's own type records the way every other value of that type records. On wasm-gc a wait records its set in the map's key order. An earlier release recorded that set in bucket order there, so a wasm-gc recording of a wait made before this release can mismatch on that argument and has to be made again. One backend has a limit here. On wasm-gc the host receives its keys as bare references and can read a whole number out of one but nothing else, so recording a wait keyed by another type is refused there by name instead of being written down as a guess. Record such a program on the bytecode VM or under `--target rust`, where the key crosses as a value of a type both ends know. Replay hands the program the recorded answers back in the turns they were recorded in, because a faster or slower machine must not move a result into a different turn. It also runs the bound function again beside them. A job is pure, so recomputing it is a cheap and useful check. When a recorded `take` said `Some(v)` and the recomputation produces a different value, replay stops and names the job kind, the job and both values. A job whose recording ends before anything took it is cancelled when the recording ends. Replay starts every recorded job whatever `max-jobs` the replaying host runs with, because a job begun at the limit is queued, so a recording made with more jobs running at once than the replaying host allows still replays.

How long replay waits for a recomputation differs by backend. The VM waits up to 5 seconds for the recomputed job and lets the recorded answer stand when it has not finished by then, so a slow recomputation is not a divergence. The Wasmtime host waits up to 30 seconds and fails the replay when the recomputation has not finished. Generated Rust does not recompute at all and serves the recorded answers. Only a recomputation that finished with a different value is a divergence on every backend that recomputes.

#### On the Rust backend

`aver compile --target rust` accepts a program with a job kind, and the binary it builds behaves like the VM on the same program. The job engine is the one in `aver-rt`. `begin` starts a thread, `take` gives the same four answers in the same words, `Wait.poll` watches sockets through the same reactor and jobs through the same engine, and `[work] max-jobs` reaches the binary from the manifest at compile time. A manifest that names no limit leaves the running host's own parallelism, read where the binary runs and not where it was built. The bound function is compiled into the same crate, so a job is one call and needs no second interpreter.

There are two differences from the VM. First, `Work.cancel` drops the job's answer and sets its cancellation flag, but generated Rust has no cancellation check, so the thread runs to completion in the background. On the Rust backend, cancelling detaches the work instead of stopping it, and the process exits without waiting for it. A cancelled body keeps its place under `[work] max-jobs` until it ends, so a job begun right after a `Work.cancel` at a limit of one is queued until the cancelled body finishes. Its answers are the same as on the VM; only when it starts differs. Second, replaying a recording serves the recorded answers without recomputing the job beside them, so for now the divergence check is done on the VM and the Wasmtime host.

A recording made on the VM replays on the built binary, and a recording the binary makes replays on the VM. The binary writes the source it was compiled from into the recording's header: the module root, made absolute at compile time, and the program file relative to that root. It can therefore run from any directory on the machine it was built on, and `aver replay` still finds the program it replays. `AVER_REPLAY_PROGRAM_FILE` and `AVER_REPLAY_MODULE_ROOT` override both at run time.

#### On wasm-gc and wasip2

`aver run --wasm-gc` and `--target wasm-gc --pack wasmtime` run jobs on host threads with separate Wasm instances and GC heaps. The compiled module is shared, and completed instances go back to a bounded pool. `begin` returns immediately, `take` may answer `Ok(None)`, and `[work] max-jobs` bounds how many bodies run at once, with the same queue as the VM. `Work.cancel` interrupts a running body at an epoch check. `Wait.poll` is the same wait loop the VM and generated Rust run, from `aver-rt`: it watches sockets and job completions together, and an empty set waits for the requested timeout. Taken, cancelled, forgotten and foreign-kind handles get the same answers as on the VM.

Raw wasm-gc exposes the versioned `aver:work/v1` ABI. The supplied JavaScript adapter uses Web Workers (or Node worker threads) and drives a generated coordinator through its post-wait step, returning to the event loop between turns. The adapter does not read `[work] max-jobs` from the manifest, because the module carries no limit: its limit is the `maxJobs` option of `createWorkHost`, and without it the lower of the host's `hardwareConcurrency` and 8. It queues a job begun at that limit exactly as the native hosts do, and it frees a cancelled job's place at once, because terminating the Worker stops the body there and then. Task and result data are copied between instances with the existing typed capability helpers. GC references stay local, and live transport preserves full `Int`. See [Parallel Work on wasm-gc](wasm-work.md) for embedding, imports, cancellation and deployment.

The native Wasmtime host records the same operation shapes as the VM, keeps recorded handle tokens and readiness during replay, and recomputes completed job results for comparison. Live transport does not depend on the recording codec, which keeps its existing i64 and finite-float limits. The JS adapter provides live execution.

`wasip2` keeps the current inline implementation. `begin` computes the result immediately, `take` can collect it at once, cancellation drops an uncollected answer, and every job key is ready immediately. Three consequences are observable and differ from every other backend: `take` never answers `Ok(None)`, so that branch of a program never runs there; a job whose body never ends blocks the turn for good; and a job whose body fails stops the whole component, where the other backends answer `take` with the error. `[work] max-jobs` still produces `warning[work-max-jobs-ignored]` on this target, and the warning says the same three things. The component has no `aver:work/v1` imports, and `--record` is still refused. A generated WASI loop keeps its signal stop flag false and ends through its policy or normal exhaustion. WASI 0.2 does not bind `Process.stopRequested`, and explicit calls are still rejected.

### Capabilities the program answers — `answers`, `Run.Wake` and the generated loop

A job kind is answered by the runtime running a pure function off the turn. A program can also answer a capability of its own *inside* the turn, from one of its own modules. The module says so in its header:

```aver
module Sockets
    intent = "The answers this program gives to Wire."
    depends [Wire, Tcp]
    answers [Wire]
```

`answers [Wire]` names the capabilities this module answers, as the module writes them in its own `depends`. It answers **every** operation of each one. The module has one function per operation, `op(state: S, a1: T1, …) -> Tuple<S, Result<R, Run.Wake>>`, with the same state `S` across every operation of every capability it answers, and a pure `fresh() -> S`, the state before anything has happened. `R` is the operation's own result. `Result.Ok(v)` answers the request now. `Result.Err(wake)` answers nothing yet and says when to ask again. Getting any of that wrong is `error[answer-binding]`, and the message prints the signature wanted.

An answer module is a module of the program like any other: some module of the program lists it in `depends`, usually the entry. A program whose modules never reach it has no module answering the capability. Only a capability this program declares may be answered: `Console`, `Disk`, `Tcp`, `Time`, `Wait`, `Work` and the rest are answered by the runtime's own providers, and `answers [Tcp]` is `error[answer-binding]`. A capability is answered by exactly one of three things: a host package, a pure function of the program through the job engine, or a module of the program. Answering it makes every call to its operations a request, which is legal only inside a function whose effect list names `yield`. See [Yielding functions](language.md#yielding-functions) for what the compiler generates from that, and [The coordinator](language.md#the-coordinator) for the loop that answers it.

`Run.Wake` has two constructors:

```aver
type Wake
    Until(List<Wait.Item>, Option<Int>)
    Settled(Option<Int>)
```

`Until(items, deadline)` asks again in a turn whose wait reported one of the items, or once `deadline` milliseconds have passed, whichever comes first. `Until([], Option.Some(ms))` is a plain deadline, and `Until([], Option.Some(0))` asks again on the next turn. `Until([], Option.None)` is never asked again. `Settled(deadline)` asks again once the same answer module has answered any request with something other than `Settled`, or once the deadline has passed. It is how one process waits for another to move the module's state: a delivery, a take, a post. An `Err` of either kind keeps the state the answer module returned, so an `Err` is where a module records its own progress.

A job is the answer to a request. An answer module begins the job itself, keeps its handle in its state, and parks the request on it with `Result.Err(Run.Wake.Until([Wait.Item.Job(job)], Option.None))`. The turn watches that job in its one wait and asks the request again when the job settles, and the module then takes it and answers `Ok`. The answer module performs only `begin` and `take`, which return at once, so `answer-shape` does not warn about it. When the run ends, the generated loop cancels every job a parked request is still waiting on, so no job outlives its run. `tests/fixtures/run_job_request` is a worked example.

Earlier releases bound an answer module in `aver.toml` with `answer = "Module"`, moved jobs through `task`, `started` and `landed` keys, and asked for the loop with a `[run]` table. A manifest that still carries any of them is refused with the repair. The manifest keeps only deployment: the `work` function of each job kind, `[work] max-jobs`, and host packages.

The entry module's own `effects [...]` is widened by what is generated into it: `Process.stopRequested`, `Time.unixMs`, `Wait.poll`, what the answer modules perform, and `Work.cancel` when the program binds a job kind. The module boundary has to hold the generated loop as well as the processes the program wrote. `Time.unixMs` is there because the turn reads the clock once, after its one wait has returned and before it serves. A deadline is measured against that one reading, so a deadline that fell due while the turn was asleep is askable in that turn and not the next one. The next turn's wait is measured against it too. A slot keeps the deadline it asked for next to the reading it falls due at, so the turn never waits longer than the shortest request in the program, even when the clock steps backwards. A recording replays the reading.

### `Random` namespace — use granular effects (`! [Random.int]`, `! [Random.float]`)

Contract source: `stdlib/capabilities/random.av`. The native VM and generated Rust share the `aver-rt` Random provider. wasm-gc keeps the existing `aver.random_*` imports, and wasip2 keeps its WASI random lowering. Signatures, Oracle classification, hostile profiles, replay semantics and target accounting all derive from the same contract and model hashes.

| Function | Signature | Notes |
|---|---|---|
| `Random.int` | `(Int, Int) -> Result<Int, String>` | Random integer in [min, max] inclusive; valid host-range literal bounds discharge the wrapper while the effect still runs |
| `Random.float` | `() -> Float` | Random float in [0.0, 1.0) |

Literal discharge is fail-closed. It removes the `Result` handling on the user's side, but the provider contract still applies. If a provider or Oracle stub returns `Err` for literal bounds proven valid, execution faults as a contract violation. The compiler never substitutes `min`, `0` or any other sample that merely looks valid.

### `Process` namespace — use `! [Process.stopRequested]`

Contract source: `stdlib/capabilities/process.av`. The native VM, generated Rust and the embedded wasm-gc wasmtime host install one process-wide SIGINT/SIGTERM flag. Browser and Worker wasm-gc hosts have no process signal, so they satisfy `aver.process_stop_requested` with `false`. WASI 0.2 has no matching signal binding, so wasip2 rejects this effect at compile time and points to `--target wasm-gc`.

| Function | Signature | Notes |
|---|---|---|
| `Process.stopRequested` | `() -> Bool` | Cooperative stop observation; once one call returns `true`, every later call on the same branch returns `true` |

The native handler only changes the flag from `false` to `true` and never resets it. Oracle hostile profiles obey the same law across calls, and Lean and Dafny receive it as a capability invariant. Poll at a point where cleanup is safe: the operation does not interrupt a blocking effect or run a shutdown hook. The handler is installed by the first `Process.stopRequested` call; before that, SIGINT and SIGTERM end the process the usual way. Once it is installed, a `Wait.poll` on the VM, in generated Rust and on the wasm-gc native host notices a stop request within about 100 ms and returns the keys that are ready, possibly none, instead of sleeping out its timeout. A generated loop observes the flag at the start of its next turn, so a run parked on a long deadline stops within that moment too. The JavaScript adapter's `stop()` ends a wait at once. `examples/formal/process_stop_requested.av` has a recursive loop checked against `stopAfterThree`.

The first call takes SIGINT and SIGTERM away from their default action for the rest of the process. From then on the two signals raise the flag and end nothing. The program ends when it returns, so a program that stops polling the flag holds its terminal until SIGKILL. Ctrl-C therefore gives the shell prompt back only once the program has answered the request. This applies wherever that handler is installed: the native VM, generated Rust, the embedded wasm-gc wasmtime host, and the cached provider host that runs the programs of a project with `[providers]`.

### `Time` namespace — use granular effects (`! [Time.now]`, `! [Time.unixMs]`, `! [Time.sleep]`)

Contract source: `stdlib/capabilities/time.av`. The native VM and generated Rust share the `aver-rt` Time adapter. wasm-gc uses the existing `aver.time_*` imports, and wasip2 uses WASI clocks/poll. All four bindings are checked and accounted against the same contract and model hashes.

| Function | Signature | Notes |
|---|---|---|
| `Time.now` | `() -> String` | Current UTC timestamp string (`...Z`) |
| `Time.unixMs` | `() -> Int` | Unix epoch milliseconds |
| `Time.sleep` | `Int -> Result<Unit, String>` | Rejects negative/out-of-host-range dynamic durations; a valid literal discharges the wrapper while the sleep still runs |

As with `Random.int`, a provider `Err` after literal discharge is a contract violation and faults. Discharge does not turn a failed sleep into `Unit`.

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

All terminal operations share the same adapter boundary. A broken output stream, a failed mode change or an input error is a `Result.Err` that the program may propagate or handle. Their outcomes are recorded for deterministic replay. There is no split where `moveTo` is fallible but an adjacent `print` silently faults outside the language value.

Terminal guard: `aver run` installs a drop guard that restores the terminal (show cursor, reset colors, disable raw mode) even on panic or runtime error.

### `Env` namespace — use granular effects (`! [Env.get]`, `! [Env.set]`)

Source: `src/services/env.rs`

| Function | Signature | Notes |
|---|---|---|
| `Env.get` | `String -> Option<String>` | Returns `Option.None` for missing/unreadable variable |
| `Env.set` | `(String, String) -> Result<Unit, String>` | Invalid key/value format and host write failures stay catchable |

Runtime policy (`aver.toml`) can restrict which keys are allowed:

```toml
[effects.Env]
keys = ["APP_*", "PUBLIC_*"]
```
