# Aver — Standard Library Namespaces

All functions live in namespaces — no flat builtins (decision: `FullNamespaceEverywhere`).

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
| `Vector.toList` | `Vector<T> -> List<T>` | Convert vector to list |

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
| `Int.toString` | `Int -> String` |
| `Int.toFloat` | `Int -> Float` |
| `Int.abs` | `Int -> Int` |
| `Int.min` | `(Int, Int) -> Int` |
| `Int.max` | `(Int, Int) -> Int` |
| `Int.mod` | `(Int, Int) -> Result<Int, String>` |

### `Float` namespace

Source: `src/types/float.rs`

| Function | Signature |
|---|---|
| `Float.fromString` | `String -> Result<Float, String>` |
| `Float.fromInt` | `Int -> Float` |
| `Float.toString` | `Float -> String` |
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
| `String.len` | `String -> Int` | |
| `String.byteLength` | `String -> Int` | |
| `String.charAt` | `(String, Int) -> Option<String>` | Single char or `Option.None` on out-of-bounds |
| `String.startsWith` | `(String, String) -> Bool` | |
| `String.endsWith` | `(String, String) -> Bool` | |
| `String.contains` | `(String, String) -> Bool` | |
| `String.slice` | `(String, Int, Int) -> String` | |
| `String.trim` | `String -> String` | |
| `String.split` | `(String, String) -> List<String>` | |
| `String.replace` | `(String, String, String) -> String` | |
| `String.join` | `(List<String>, String) -> String` | |
| `String.chars` | `String -> List<String>` | |
| `String.fromInt` | `Int -> String` | |
| `String.fromFloat` | `Float -> String` | |
| `String.fromBool` | `Bool -> String` | |
| `String.toLower` | `String -> String` | Unicode-aware lowercase |
| `String.toUpper` | `String -> String` | Unicode-aware uppercase |

### `Map` namespace

Source: `src/types/map.rs`

| Function | Signature | Notes |
|---|---|---|
| `Map.empty` | `() -> Map<K, V>` | |
| `Map.fromList` | `List<(K, V)> -> Map<K, V>` | Keys must be hashable (Int, Float, String, Bool) |
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

### `Byte` namespace

Source: `src/types/byte.rs` — not a type, operates on `Int`/`String`.

| Function | Signature | Notes |
|---|---|---|
| `Byte.toHex` | `Int -> Result<String, String>` | Always 2-char lowercase hex |
| `Byte.fromHex` | `String -> Result<Int, String>` | Exactly 2 hex chars required |

## Effectful namespaces

**Namespace effect shorthand**: declaring `! [ServiceName]` covers all methods of that service. For example, `! [Disk]` is equivalent to `! [Disk.readText, Disk.writeText, Disk.appendText, Disk.exists, Disk.delete, Disk.deleteDir, Disk.listDir, Disk.makeDir]`. You can still use granular declarations like `! [Disk.readText]` when you want to be precise. `aver check` suggests narrowing when a shorthand could be more specific.

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

Source: `src/services/disk.rs`

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
| `Tcp.ping` | `(String, Int) -> Result<Unit, String>` |

**Persistent connections:**

| Function | Signature | Notes |
|---|---|---|
| `Tcp.connect` | `(String, Int) -> Result<Tcp.Connection, String>` | Returns record `{id, host, port}` |
| `Tcp.writeLine` | `(Tcp.Connection, String) -> Result<Unit, String>` | |
| `Tcp.readLine` | `Tcp.Connection -> Result<String, String>` | |
| `Tcp.close` | `Tcp.Connection -> Result<Unit, String>` | |

`Tcp.Connection` is a record with fields `id: String`, `host: String`, `port: Int`. It can also be constructed directly via `Tcp.Connection(id = ..., host = ..., port = ...)`. The actual socket lives in a thread-local `HashMap` keyed by the `id` field. Connection IDs are generated by `AtomicU64` ("tcp-1", "tcp-2", ...).

### `Random` namespace — use granular effects (`! [Random.int]`, `! [Random.float]`)

Source: `src/services/random.rs` (backed by `aver_rt::random`)

| Function | Signature | Notes |
|---|---|---|
| `Random.int` | `(Int, Int) -> Int` | Random integer in [min, max] inclusive |
| `Random.float` | `() -> Float` | Random float in [0.0, 1.0) |

### `Time` namespace — use granular effects (`! [Time.now]`, `! [Time.unixMs]`, `! [Time.sleep]`)

Source: `src/services/time.rs`

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
