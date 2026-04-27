# Aver

> Aver is a statically typed language optimized for a world where code is cheap to generate and expensive to trust. The optimization target is the reviewer, not the generator.

- Website: [averlang.dev](https://averlang.dev)
- Playground: [averlang.dev/playground](https://averlang.dev/playground/)
- GitHub: [jasisz/aver](https://github.com/jasisz/aver)
- crates.io: [aver-lang](https://crates.io/crates/aver-lang)
- License: MIT
- Written in: Rust
- Backends: bytecode VM, Rust codegen, WASM, Lean 4 proof export, Dafny verification

## Before you write Aver

If you only remember one section, make it this one:

- Files end with `.av`, not `.aver`
- Each file starts with exactly one `module <Name>` declaration
- Module metadata uses `intent =` and `exposes [...]`
- Bindings are `name = expr` or `name: Type = expr` — no `let`, `val`, or `var`
- Constructors are always qualified: `Result.Ok`, `Result.Err`, `Option.Some`, `Option.None`
- There is no `if` / `else`; use `match`
- **Match arm bodies must start on the same line as `->` — this is the most common error.** For complex logic, extract a helper function
- Functions do not have type parameters — write `fn sum(xs: List<Int>) -> Int`, not `fn sum<T>(xs: List<T>) -> T`
- Effects are explicit: `! [Console.print]`, `! [Http.get]`
- Pure functions get colocated `verify` blocks
- Classified effectful fns (Random, Disk, Http, Tcp one-shot, Time, Console.readLine, Terminal non-modal, all output) get `verify fn trace` with `given` stubs; unclassified ambient / session / modal flows go through record/replay
- Parse integers with `Int.fromString`, which returns `Result<Int, String>`
- `Console.readLine()` returns `Result<String, String>`, not plain `String`

## Minimal correct file

```aver
module Hello
    intent = "Tiny intro module."
    exposes [greet, main]

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

This shows the core pattern:

- a pure function
- a `verify` block directly below it
- an effectful `main`

