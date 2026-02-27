# Aver — Extending the Language

## How to add a new keyword

1. Add a variant to `TokenKind` in `src/lexer.rs`
2. Add a match arm in the `keyword()` function in `src/lexer.rs`
3. Add the corresponding AST node(s) to `src/ast.rs` if needed
4. Add a `parse_*` method in the appropriate `src/parser/*.rs` submodule and call it from `parse_top_level()` in `src/parser/module.rs`
5. Add execution logic in the appropriate `src/interpreter/*.rs` submodule (`eval.rs` for expressions, `exec.rs` for statements)

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

// interpreter/eval.rs — add to eval_expr():
Expr::Maintain(cond, body) => {
    loop {
        match self.eval_expr(cond)? {
            Value::Bool(false) => break,
            Value::Bool(true)  => { self.eval_expr(body)?; }
            _ => return Err(RuntimeError::Error("maintain condition must be Bool".into())),
        }
    }
    Ok(Value::Unit)
}
```

## How to add a new namespace function

All functions live in namespaces (e.g., `Int.abs`, `List.len`, `Console.print`). To add a new function to an existing namespace:

1. Add the implementation in the namespace's file (e.g., `src/types/int.rs` for pure, `src/services/console.rs` for effectful) inside `call()`:
   ```rust
   "Int.yourMethod" => {
       // validate args, return Ok(Value::...)
   }
   ```
2. Register the member name in `register()` so it appears in the namespace's `members` map.
3. Add the type signature in `src/types/checker/builtins.rs` in the corresponding sigs section.

To create a new pure namespace, follow the pattern in `src/types/char.rs` or `src/types/int.rs`: implement `register()`, `effects()`, and `call()`, add `pub mod` in `src/types/mod.rs`, wire dispatch in `src/interpreter/builtins.rs` and effects in `src/interpreter/effects.rs`. For effectful namespaces, use `src/services/` instead.

## How to add a new expression type

1. Add a variant to `Expr` in `src/ast.rs`
2. Parse it in `src/parser/expr.rs` (typically in `parse_atom` or a new precedence level)
3. Evaluate it in `src/interpreter/eval.rs` inside `eval_expr`'s `match expr` block
4. If it should appear in verify cases, update `expr_to_str` in `src/checker.rs`
