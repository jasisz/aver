# Aver — Key Data Types

## Compiler types

| Type | Location | Description |
|---|---|---|
| `TokenKind` | `lexer.rs` | Every possible token: literals, keywords, operators, structural (`INDENT`/`DEDENT`/`NEWLINE`/`EOF`) |
| `Token` | `lexer.rs` | `TokenKind` + source position (`line`, `col`) |
| `LexerError` | `lexer.rs` | Carry `msg`, `line`, `col`; formatted as `"Lexer error [L:C]: msg"` |
| `ParseError` | `parser.rs` | `msg`, `line`, `col`; formatted as `"Parse error [L:C]: msg"` |

## AST types

| Type | Location | Description |
|---|---|---|
| `Literal` | `ast.rs` | `Int(i64)`, `Float(f64)`, `Str(String)`, `Bool(bool)` |
| `BinOp` | `ast.rs` | Arithmetic and comparison operators as enum variants |
| `Pattern` | `ast.rs` | Match arm pattern: `Wildcard`, `Literal`, `Ident`, `EmptyList`, `Cons`, `Constructor` |
| `StrPart` | `ast.rs` | Piece of an interpolated string: `Literal(String)` or `Expr(String)` (raw source) |
| `Expr` | `ast.rs` | Every expression form: `Literal`, `Ident`, `Attr`, `FnCall`, `BinOp`, `Match`, `Pipe`, `Constructor`, `ErrorProp`, `InterpolatedStr`, `List(Vec<Expr>)`, `RecordCreate { type_name, fields }` |
| `Stmt` | `ast.rs` | `Val(name, expr)`, `Var(name, expr, reason)`, `Assign(name, expr)`, `Expr(expr)` |
| `FnBody` | `ast.rs` | `Expr(Expr)` for `= expr` shorthand, or `Block(Vec<Stmt>)` |
| `FnDef` | `ast.rs` | Name, params, return type, effects, optional description, body |
| `Module` | `ast.rs` | Name, depends, exposes, intent string |
| `VerifyBlock` | `ast.rs` | Function name + list of `(left_expr, right_expr)` equality cases |
| `DecisionBlock` | `ast.rs` | Name, date, reason, chosen, rejected list, impacts list, optional author |
| `TopLevel` | `ast.rs` | Top-level item: `Module`, `FnDef`, `Verify`, `Decision`, `Stmt`, `TypeDef` |
| `TypeDef` | `ast.rs` | `Sum { name, variants }` or `Product { name, fields }` |

## Runtime types

| Type | Location | Description |
|---|---|---|
| `Value` | `value.rs` | Runtime value: `Int`, `Float`, `Str`, `Bool`, `Unit`, `Ok(Box<Value>)`, `Err(Box<Value>)`, `Some(Box<Value>)`, `None`, `List(Vec<Value>)`, `Fn{..}`, `Builtin(String)`, `Variant { type_name, variant, fields }`, `Record { type_name, fields }`, `Namespace { name, members }` |
| `Env` | `value.rs` | `Vec<HashMap<String, Value>>` — scope stack, innermost last |
| `RuntimeError` | `value.rs` | `Error(String)` or `ErrProp(Box<Value>)` (internal `?` signal) |
