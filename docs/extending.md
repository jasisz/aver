# Aver — Extending the Language

## How to add a new keyword

1. Add a variant to `TokenKind` in `src/lexer.rs`
2. Add a match arm in the `keyword()` function in `src/lexer.rs`
3. Add the corresponding AST node(s) to `src/ast.rs` if needed
4. Add a `parse_*` method in `src/parser.rs` and call it from `parse_top_level()` or the appropriate sub-parser
5. Add execution logic in `src/interpreter.rs` (`eval_expr`, `exec_stmt`, or a new method)

**Concrete example — adding `maintain` (goal-based looping):**

Aver has no `for`/`while`. Future iteration uses goal-based constructs. Here is the extension pattern for a `maintain` keyword:

```
// lexer.rs — add:
Maintain,

// keyword() — add:
"maintain" => Some(TokenKind::Maintain),

// ast.rs — add to Expr:
Maintain(Box<Expr>, Box<Expr>),  // condition, body block

// parser.rs — add method:
fn parse_maintain(&mut self) -> Result<Expr, ParseError> {
    self.expect_exact(&TokenKind::Maintain)?;
    let cond = self.parse_expr()?;
    self.expect_exact(&TokenKind::Colon)?;
    // parse indented body block
}

// interpreter.rs — add to eval_expr():
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

1. Add the implementation in the namespace's service file (e.g., `src/services/int_helpers.rs`) inside `call()`:
   ```rust
   "Int.yourMethod" => {
       // validate args, return Ok(Value::...)
   }
   ```
2. Register the member name in `register()` so it appears in the namespace's `members` map.
3. Add the type signature in `src/typechecker.rs` in the corresponding `service_sigs` section.

To create a new namespace, follow the pattern in any `src/services/*_helpers.rs` file: implement `register()`, `effects()`, and `call()`, add `pub mod` in `services/mod.rs`, wire it up in `interpreter/mod.rs`.

## How to add a new expression type

1. Add a variant to `Expr` in `ast.rs`
2. Parse it in `parser.rs` (typically in `parse_atom` or a new precedence level)
3. Evaluate it in `interpreter.rs` inside `eval_expr`'s `match expr` block
4. If it should appear in verify cases, update `expr_to_str` in `checker.rs`
