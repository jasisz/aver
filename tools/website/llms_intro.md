# Aver

> Aver is a statically typed language for a world where code is cheap to generate and expensive to trust. It is optimized for the person who reviews the code.

- Website: [averlang.dev](https://averlang.dev)
- Playground: [averlang.dev/playground](https://averlang.dev/playground/)
- GitHub: [jasisz/aver](https://github.com/jasisz/aver)
- crates.io: [aver-lang](https://crates.io/crates/aver-lang)
- License: MIT
- Written in: Rust
- Backends: bytecode VM, Rust codegen, WASM, Lean 4 proof export

## Before you write Aver

If you read only one section, read this one.

- Files end with `.av`, not `.aver`
- Each file starts with exactly one `module <Name>` declaration
- Module metadata uses `intent =` and `exposes [...]`
- Bindings are `name = expr` or `name: Type = expr`. There is no `let`, `val` or `var`
- Constructors are always qualified: `Result.Ok`, `Result.Err`, `Option.Some`, `Option.None`
- There is no `if` / `else`; use `match`
- **Match arm bodies must start on the same line as `->`. This is the most common error.** Move longer logic into a helper function
- Functions do not have type parameters. Write `fn sum(xs: List<Int>) -> Int`, never `fn sum<T>(xs: List<T>) -> T`
- Effects are explicit: `! [Console.print]`, `! [Http.get]`
- Pure functions get `verify` blocks placed right next to them
- Classified effectful fns (Random, Disk, Http, Tcp one-shot, Time, Console.readLine, Terminal non-modal, all output) get `verify fn trace` with `given` stubs. Unclassified ambient, session and modal flows go through record/replay
- Parse integers with `Int.fromString`, which returns `Result<Int, String>`
- Convert unsigned integers to fixed-width protocol bytes with `Int.toBigEndian(value, width)` or `Int.toLittleEndian(value, width)`; both return `Result<Bytes, String>` unless both literal arguments prove the call cannot fail. Decode total `Bytes` values with `Int.fromBigEndian` / `Int.fromLittleEndian`
- `Console.readLine()` returns `Result<String, String>`, not a plain `String`

## Minimal correct file

```aver
module Hello
    intent = "Tiny intro module."
    exposes [greet, main]
    effects [Console.print]

fn greet(name: String) -> String
    ? "Builds a greeting."
    "Hello, {name}!"

verify greet
    greet("Aver") => "Hello, Aver!"

fn main() -> Unit
    ? "Entry point."
    ! [Console.print]
    Console.print(greet("world"))
```

The basic pattern:

- a pure function
- a `verify` block directly below it
- an effectful `main`
- a module-level `effects [...]` boundary that names every effect any function in the module uses (since 0.13)

