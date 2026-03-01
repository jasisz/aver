# Plan: Transpilacja Aver → Rust

## Context

Aver to interpretowany język — tree-walking evaluator. Transpilacja do Rust daje:
- **Natywną wydajność** (kompilowany binary zamiast interpretera)
- **Deployment** (single binary, `cargo build --release`)
- **Ekosystem Rust** (potencjalny dostęp do crates)

Komenda: `aver compile file.av -o output_dir/` → generuje projekt Cargo gotowy do `cargo build && cargo run`.

Output transpilowanego programu musi być identyczny z `aver run` — format wyświetlania wartości (Display) zgodny z Aver (`Result.Ok(5)`, `Option.Some("x")`, `Shape.Circle(3.14)`), nie z natywnym Rust (`Ok(5)`).

## Architektura

Pipeline: `parse → tco_transform → typecheck → **codegen**` (nowy krok zamiast resolve+interpret)

### Multi-target design

Aver jest wystarczająco prosty (brak closures, brak mutacji, czyste typy), żeby transpilować do wielu języków. Architektura codegen oparta o trait `CodegenBackend`:

```rust
pub trait CodegenBackend {
    fn emit_type(&self, ty: &Type) -> String;
    fn emit_expr(&self, expr: &Expr, ctx: &CodegenContext) -> String;
    fn emit_pattern(&self, pat: &Pattern, ctx: &CodegenContext) -> String;
    fn emit_stmt(&self, stmt: &Stmt, ctx: &CodegenContext) -> String;
    fn emit_fn(&self, fd: &FnDef, ctx: &CodegenContext) -> String;
    fn emit_type_def(&self, td: &TypeDef, ctx: &CodegenContext) -> String;
    fn emit_builtin_call(&self, name: &str, args: &[Expr], ctx: &CodegenContext) -> Option<String>;
    fn emit_project(&self, ctx: &CodegenContext) -> ProjectOutput;
}
```

Rust jest pierwszym backendem. Dodanie JS/Python/Go w przyszłości = nowy impl tego traitu.

```
src/codegen/
  mod.rs          — trait CodegenBackend + CodegenContext + compile_program()
  context.rs      — CodegenContext: type defs, fn sigs, memo fns, module tree
  rust/           — Rust backend (pierwszy)
    mod.rs        — RustBackend impl CodegenBackend
    types.rs      — Type → Rust type string
    expr.rs       — Expr → Rust expression string
    pattern.rs    — Pattern → Rust pattern string
    toplevel.rs   — TopLevel → Rust items (fn, struct, enum, #[test])
    builtins.rs   — tabela mapowania ~80 builtin functions na Rust equivalents
    runtime.rs    — generowanie inline `mod aver_rt { ... }` z helperami
    project.rs    — generowanie Cargo.toml + main.rs
```

Modyfikowane pliki:
- `src/main/cli.rs` — nowy wariant `Compile` w Commands enum
- `src/main/commands.rs` — nowa funkcja `cmd_compile`
- `src/main.rs` — dispatch Compile
- `src/lib.rs` — `pub mod codegen`

## Mapowanie typów

| Aver | Rust |
|------|------|
| Int | i64 |
| Float | f64 |
| String | String |
| Bool | bool |
| Unit | () |
| Result<T, E> | Result<T, E> |
| Option<T> | Option<T> |
| List<T> | Vec<T> |
| Tuple(A, B) | (A, B) |
| Map<K, V> | HashMap<K, V> |
| Named("Shape") | Shape (enum) |
| Named("User") | User (struct) |
| Fn(A) -> B | fn(A) -> B |

## Mapowanie wyrażeń (kluczowe)

| Aver | Rust |
|------|------|
| `42` | `42i64` |
| `"hello {name}"` | `format!("hello {}", name)` |
| `[1, 2, 3]` | `vec![1i64, 2i64, 3i64]` |
| `a \|> f` | `f(a)` |
| `expr?` | `expr?` |
| `Result.Ok(v)` | `Ok(v)` |
| `Shape.Circle(r)` | `Shape::Circle(r)` |
| `User(name: "A", age: 30)` | `User { name: "A".to_string(), age: 30i64 }` |
| `match x: ...` | `match x { ... }` |
| `Console.print(x)` | `println!("{}", x)` |
| `List.map(xs, f)` | `xs.iter().map(\|x\| f(x.clone())).collect()` |
| `Disk.readText(p)` | `std::fs::read_to_string(&p).map_err(\|e\| e.to_string())` |

## Mapowanie pattern matching

| Aver Pattern | Rust Pattern |
|------|------|
| `_` | `_` |
| `42` | `42i64` |
| `"hello"` | `"hello"` (na `.as_str()`) |
| `x` (binding) | `x` |
| `Result.Ok(v)` | `Ok(v)` |
| `Option.None` | `None` |
| `Shape.Circle(r)` | `Shape::Circle(r)` |
| `[]` | `[]` (na `.as_slice()`) |
| `[h, ..t]` | `[h, t @ ..]` (na `.as_slice()`) |
| `(a, b)` | `(a, b)` |
| `User(name, age)` | `User { name, age }` |

## Mapowanie top-level

- **TypeDef::Sum** → `#[derive(Clone, Debug, PartialEq)] enum Name { ... }`
- **TypeDef::Product** → `#[derive(Clone, Debug, PartialEq)] struct Name { ... }`
- **FnDef** → `fn snake_name(params) -> RetType { body }`
  - `? "description"` → `/// description` doc comment
  - `! [Effect]` → ignorowane (statyczna weryfikacja na etapie Aver)
- **Verify** → `#[cfg(test)] mod tests { #[test] fn test_name() { assert_eq!(...) } }`
- **Module** → komentarz doc na początku pliku
- **Decision** → komentarz `// Decision: ...`
- **Top-level Stmt** → przeniesione do początku `fn main()`

## Runtime helpers (`mod aver_rt`)

Generowany inline w `main.rs`. Zawiera:
- `read_line() -> Result<String, String>` — Console.readLine
- `string_slice(s, from, to) -> String` — code-point based slice
- `list_dir(path) -> Result<Vec<String>, String>` — Disk.listDir
- `append_text(path, content) -> Result<(), String>` — Disk.appendText
- `aver_display(val) -> String` — formatowanie zgodne z Aver (`Result.Ok(5)`, nie `Ok(5)`)
- Custom `Display` impl dla generowanych struct/enum w formacie Aver

Brak zewnętrznych dependencies dla MVP (poza std). Http → dodanie `ureq` do Cargo.toml gdy wykryty.

## TCO — self-recursion → loop

```rust
// Aver: fn fib(n, a, b) = match n: 0 -> a | _ -> fib(n-1, b, a+b)
fn fib(mut n: i64, mut a: i64, mut b: i64) -> i64 {
    loop {
        match n {
            0 => return a,
            _ => { let (n_, a_, b_) = (n - 1, b, a + b); n = n_; a = a_; b = b_; }
        }
    }
}
```

Mutual TCO: poza scope MVP (rzadkie w Aver).

## Auto-memoizacja

Funkcje kwalifikujące się (pure + recursive + memo-safe params) dostają `thread_local!` cache:

```rust
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static FIB_CACHE: RefCell<HashMap<i64, i64>> = RefCell::new(HashMap::new());
}

fn fib(n: i64) -> i64 {
    FIB_CACHE.with(|cache| {
        if let Some(&r) = cache.borrow().get(&n) { return r; }
        let result = match n { 0 => 0, 1 => 1, _ => fib(n - 1) + fib(n - 2) };
        cache.borrow_mut().insert(n, result.clone());
        result
    })
}
```

Dane z `TypeCheckResult`: `memo_safe_types` + `fn_sigs` → `compute_memo_fns()` daje `HashSet<String>` eligible functions. Codegen sprawdza czy fn name jest w secie i generuje cache wrapper.

Multi-param: klucz cache to tuple `(param1, param2, ...)`. Cap 4096 entries jak w interpreterze.

## Output

`aver compile file.av -o dir/` generuje:
```
dir/
  Cargo.toml      — [package] name, edition = "2024"
  src/
    main.rs        — mod aver_rt + types + functions + main + #[cfg(test)]
```

### Rekurencyjne dependencies

`depends [Examples.Redis]` → resolve `examples/redis.av` → jeśli ten ma `depends [Examples.Foo]` → resolve `examples/foo.av` → itd. Wszystkie moduły inlined jako `mod module_name { pub fn ... }`. Circular imports → hard error z łańcuchem (`A → B → A`).

## Krok po kroku (kolejność implementacji)

### Krok 0 (FIRST — zmiana w core języka): Usunąć implicit Int→Float widening
- `src/types/mod.rs` — `Type::Int.compatible(&Type::Float)` → `false`
- `src/interpreter/eval.rs` — usunąć implicit widening w `eval_binop` (Int+Float→Float)
- Wymusza explicit `Int.toFloat(n)` zamiast niejawnej konwersji
- Zaktualizować przykłady `.av` i testy jeśli coś się psuje
- **Robimy to PRZED transpilacją** — upraszcza cały codegen

### Krok 1: Szkielet codegen + CLI
- `src/codegen/mod.rs` — `compile_to_rust(items, tc_result) -> RustProject`
- `src/codegen/project.rs` — `RustProject { cargo_toml, main_rs }`, generowanie Cargo.toml
- `src/main/cli.rs` — wariant `Compile { file, output, module_root, name }`
- `src/main/commands.rs` — `cmd_compile`: parse → tco → typecheck → codegen → write files
- `src/lib.rs` — `pub mod codegen`
- Test: `aver compile examples/hello.av -o /tmp/test-out` generuje puste ale poprawne Cargo project

### Krok 2: Typy i definicje typów
- `src/codegen/types.rs` — `aver_type_to_rust(&Type) -> String`
- `src/codegen/toplevel.rs` — generowanie struct (Product) i enum (Sum) z `#[derive]`
- Test: TypeDef w wygenerowanym kodzie

### Krok 3: Wyrażenia (core)
- `src/codegen/expr.rs` — `emit_expr(&Expr, &Ctx) -> String`
  - Literal, Ident, BinOp, FnCall (proste), Attr, List, Tuple
- `src/codegen/stmt.rs` — `emit_stmt(&Stmt, &Ctx) -> String`
  - Binding → `let name = expr;`, Expr → `expr;`
- Test: proste funkcje się kompilują

### Krok 4: Pattern matching + Match
- `src/codegen/pattern.rs` — `emit_pattern(&Pattern) -> String`
  - Wildcard, Literal, Ident, Constructor (Result/Option/user types)
- Wyrażenie Match w expr.rs
- Detekcja String/List subjects → `.as_str()` / `.as_slice()`
- Test: `calculator.av`, `shapes.av`

### Krok 5: Builtiny (Console + Result/Option + String interp)
- `src/codegen/builtins.rs` — tabela mapowania FnCall na Rust
  - Console.print → println!, Result.Ok → Ok(), Option.withDefault → .unwrap_or()
- InterpolatedStr → format!()
- Pipe → f(a)
- ErrorProp → expr?
- `src/codegen/runtime.rs` — `mod aver_rt` z helperami
- Test: `hello.av` kompiluje się i daje poprawny output

### Krok 6: Pełne namespace builtiny
- Int.*, Float.*, String.*, List.* (map/filter/fold → iteratory), Map.*
- Char.*, Byte.*, Result.*, Option.*
- Test: `lists.av`, `fibonacci.av`, `temperature.av`

### Krok 7: Zaawansowane features
- RecordCreate/RecordUpdate → struct literal / `..base` syntax
- Tuple patterns
- Cons/EmptyList patterns → slice patterns
- TCO self-recursion → loop rewrite
- Auto-memoizacja → `thread_local!` cache dla eligible functions
- Verify blocks → `#[cfg(test)]`
- Test: `user_record.av` + verify jako `cargo test`, `fib(30)` instant z memo

### Krok 8: Moduły i serwisy
- `depends [X]` → inline dependent module code jako `mod x { pub fn ... }`
- Disk.* → std::fs calls
- Http.* → ureq dependency (opcjonalnie)
- Test: `app.av` (cross-module)

## Znane wyzwania

1. ~~**Int + Float widening**~~: **Usunięte** — Aver wymaga explicit `Int.toFloat()`. Upraszcza transpiler.
2. **Clone-heavy code**: immutability = dużo `.clone()`. Poprawne ale nie optymalne.
3. **String w match**: Rust wymaga `.as_str()` do matchowania String na literały
4. **Map<Float, V>**: f64 nie impl Hash. → błąd transpilacji (jasny komunikat)
5. **Top-level Stmt**: brak `let` poza fn w Rust → przenieść do `main()` lub `const`/`static`
6. **Display dla custom types**: potrzebne do Console.print → derive Debug + custom Display impl

## Weryfikacja

1. `cargo build` — codegen się kompiluje
2. `cargo test` — istniejące testy przechodzą
3. `aver compile examples/hello.av -o /tmp/hello-rs && cd /tmp/hello-rs && cargo run` — output identyczny z `aver run examples/hello.av`
4. `aver compile examples/calculator.av -o /tmp/calc-rs && cd /tmp/calc-rs && cargo test` — verify blocks przechodzą jako cargo test
5. Porównanie output dla 5+ przykładów: hello, calculator, shapes, fibonacci, lists
