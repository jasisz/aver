# Aver — Extending the Language

## How to add a new keyword

1. Add a variant to `TokenKind` in `src/lexer.rs`
2. Add a match arm in the `keyword()` function in `src/lexer.rs`
3. Add the corresponding AST node(s) to `src/ast.rs` if needed
4. Add a `parse_*` method in the appropriate `src/parser/*.rs` submodule and call it from `parse_top_level()` in `src/parser/module.rs`
5. Resolve it in `src/ir/hir/resolve.rs`, lower it in `src/ir/mir/lower.rs`, and emit opcodes for it in `src/vm/compiler/mir.rs`; the other backends (`src/codegen/rust/`, `src/codegen/wasm_gc/`, `src/codegen/lean/`, `src/codegen/dafny/`) need their own handling

**Concrete example — adding `maintain` (goal-based looping):**

Aver has no `for`/`while`. Future iteration uses goal-based constructs. Here is the extension pattern for a `maintain` keyword:

```
// lexer.rs — add:
Maintain,

// keyword() — add:
"maintain" => Some(TokenKind::Maintain),

// ast.rs — add to Expr:
Maintain(Box<Expr>, Box<Expr>),  // condition, body block

// parser/expr.rs — add method:
fn parse_maintain(&mut self) -> Result<Expr, ParseError> {
    self.expect_exact(&TokenKind::Maintain)?;
    let cond = self.parse_expr()?;
    self.expect_exact(&TokenKind::Colon)?;
    // parse indented body block
}

// ir/hir + ir/mir/lower.rs — add the mirrored node and its lowering

// vm/compiler/mir.rs — add to the MIR walker:
// Emit opcodes for maintain: compile condition, emit conditional jump,
// compile body, emit jump back to condition check
```

## How to add a new namespace function

All functions live in namespaces (e.g., `Int.abs`, `List.len`, `Console.print`). To add a new function to an existing namespace:

Only the `NanValue` half is executed. The `Value`-typed `register()` / `call()` functions still present in the namespace files are legacy, are not reached by any backend, and must not be extended; removing them is a separate change.

1. Add the implementation in the namespace's file (e.g., `src/types/int.rs` for pure, `src/services/console.rs` for effectful) as `<op>_nv`, and add its arm to `call_nv()`:
   ```rust
   // in call_nv():
   "Int.yourMethod" => Some(your_method_nv(args, arena)),

   fn your_method_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
       // validate args, return Ok(NanValue::...)
   }
   ```
2. Add the row to the `vm_builtins!` table in `src/vm/builtin.rs` and the matching arm in `VmBuiltin::invoke_nv`. The table generates `VmBuiltin::ALL`, which `bootstrap_core_symbols` in `src/vm/compiler/mod.rs` reads for namespace membership.
3. Add the type signature in `src/types/checker/builtins.rs` in the corresponding sigs section.
4. For a pure function, add the `codegen_builtins!` row in `src/codegen/builtins.rs`; the exhaustive matches then force the Lean (`src/codegen/lean/builtins.rs`) and Dafny (`src/codegen/dafny/expr.rs`) arms. Effectful operations have no row there (`recognize_builtin` returns `None` for services).
5. For a pure function, add the Rust arm in `src/codegen/rust/from_mir.rs` and the wasm-gc lowering in `src/codegen/wasm_gc` (`builtins/mod.rs` plus `body/from_mir/builtins.rs`). An effectful operation instead gets its Rust emission in `src/codegen/rust/builtins.rs`, an `EffectName` row in `src/codegen/wasm_gc/effects.rs` (plus its wasip2 lowering when the target supports it), and the matching `(module, field)` entry in `WASM_GC_CAPABILITIES` in `aver-cert/src/format.rs` — a test in `effects.rs` fails until both lists agree.
6. Document the function in `docs/services.md`.

To create a new pure namespace, follow the pattern in `src/types/char.rs` or `src/types/int.rs`: implement `register_nv()`, `effects()`, and `call_nv()`, add `pub mod` in `src/types/mod.rs`, add the namespace name to `is_builtin_namespace` in `src/ir/calls.rs` (the HIR resolver reads that list; without it a dotted call never resolves as a builtin), and add the builtin rows above. For effectful namespaces, use `src/services/` instead.

## How to add a new expression type

1. Add a variant to `Expr` in `src/ast.rs` and an arm in `src/codegen/expr_walk.rs`, the single exhaustive child-walk over `Expr` (a new variant fails the build there by design)
2. Parse it in `src/parser/expr.rs` (typically in `parse_atom` or a new precedence level)
3. Mirror it in the HIR (`src/ir/hir/`), lower it in `src/ir/mir/lower.rs`, and emit opcodes for it in `src/vm/compiler/mir.rs`
4. If it should appear in verify cases, update `expr_to_str` in `src/checker/verify.rs`
