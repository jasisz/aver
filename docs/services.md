# Aver — Standard Library Namespaces

All functions live in namespaces — no flat builtins (decision: `FullNamespaceEverywhere`).

## Pure namespaces (no effects)

### `List` namespace

Source: `src/types/list.rs` (pure helpers) + `src/interpreter/builtins.rs` (`map`/`filter`/`fold`/`find`/`any`/`flatMap` — need closure calls).

| Function | Signature | Notes |
|---|---|---|
| `List.len` | `List<T> -> Int` | |
| `List.get` | `(List<T>, Int) -> Option<T>` | Returns `Option.None` on out-of-bounds |
| `List.push` | `(List<T>, T) -> List<T>` | Appends element, returns new list |
| `List.head` | `List<T> -> Option<T>` | Returns `Option.None` on empty |
| `List.tail` | `List<T> -> Option<List<T>>` | Returns `Option.None` on empty |
| `List.map` | `(List<T>, Fn(T) -> U) -> List<U>` | |
| `List.filter` | `(List<T>, Fn(T) -> Bool) -> List<T>` | |
| `List.fold` | `(List<T>, U, Fn(U, T) -> U) -> U` | |
| `List.find` | `(List<T>, Fn(T) -> Bool) -> Option<T>` | First matching element |
| `List.any` | `(List<T>, Fn(T) -> Bool) -> Bool` | True if any element matches |
| `List.zip` | `(List<A>, List<B>) -> List<(A, B)>` | Pairs elements, truncates to shorter list |
| `List.flatMap` | `(List<T>, Fn(T) -> List<U>) -> List<U>` | Map then flatten |

### `Result` namespace

Source: `src/types/result.rs` + constructors in `src/interpreter/core.rs`.

| Function | Signature | Notes |
|---|---|---|
| `Result.Ok` | `T -> Result<T, E>` | Constructor |
| `Result.Err` | `E -> Result<T, E>` | Constructor |
| `Result.withDefault` | `(Result<T, E>, T) -> T` | Unwrap Ok or return default |

### `Option` namespace

Source: `src/types/option.rs` + constructors in `src/interpreter/core.rs`.

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

### `Console` namespace — `! [Console]`

Source: `src/services/console.rs`

| Function | Signature |
|---|---|
| `Console.print` | `T -> Unit` |
| `Console.error` | `T -> Unit` (writes to stderr) |
| `Console.warn` | `T -> Unit` (writes to stderr) |
| `Console.readLine` | `String -> Result<String, String>` |

### `Http` namespace — `! [Http]`

Source: `src/services/http.rs`

| Function | Signature | Notes |
|---|---|---|
| `Http.get` | `String -> Result<HttpResponse, String>` | |
| `Http.head` | `String -> Result<HttpResponse, String>` | |
| `Http.delete` | `String -> Result<HttpResponse, String>` | |
| `Http.post` | `(String, String, String, List<Header>) -> Result<HttpResponse, String>` | url, body, content-type, headers |
| `Http.put` | `(String, String, String, List<Header>) -> Result<HttpResponse, String>` | |
| `Http.patch` | `(String, String, String, List<Header>) -> Result<HttpResponse, String>` | |

`HttpResponse` record: `{ status: Int, body: String, headers: List<Header> }`.
`Header` record: `{ name: String, value: String }`.

### `HttpServer` namespace — `! [HttpServer]`

Source: `src/services/http_server.rs`

| Function | Signature |
|---|---|
| `HttpServer.listen` | `(Int, Fn(Request) -> Response ! [Http], context: T) -> Unit` |

### `Disk` namespace — `! [Disk]`

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

### `Tcp` namespace — `! [Tcp]`

Source: `src/services/tcp.rs`

**One-shot (stateless):**

| Function | Signature |
|---|---|
| `Tcp.send` | `(String, Int, String) -> Result<String, String>` |
| `Tcp.ping` | `(String, Int) -> Result<String, String>` |

**Persistent connections:**

| Function | Signature | Notes |
|---|---|---|
| `Tcp.connect` | `(String, Int) -> Result<Tcp.Connection, String>` | Returns opaque record `{id, host, port}` |
| `Tcp.writeLine` | `(Tcp.Connection, String) -> Result<Unit, String>` | |
| `Tcp.readLine` | `Tcp.Connection -> Result<String, String>` | |
| `Tcp.close` | `Tcp.Connection -> Result<Unit, String>` | |

`Tcp.Connection` is an opaque record with fields `id: String`, `host: String`, `port: Int`. The actual socket lives in a thread-local `HashMap` keyed by the `id` field. Connection IDs are generated by `AtomicU64` ("tcp-1", "tcp-2", ...).
