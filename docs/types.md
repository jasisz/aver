# Aver — Key Data Types

## Compiler types

| Type | Location | Description |
|---|---|---|
| `TokenKind` | `src/lexer.rs` | Every possible token: literals, keywords, operators, structural (`INDENT`/`DEDENT`/`NEWLINE`/`EOF`) |
| `Token` | `src/lexer.rs` | `TokenKind` + source position (`line`, `col`) |
| `LexerError` | `src/lexer.rs` | Carry `msg`, `line`, `col`; formatted as `"Lexer error [L:C]: msg"` |
| `ParseError` | `src/parser/core.rs` | `msg`, `line`, `col`; formatted as `"Parse error [L:C]: msg"` |

## AST types

| Type | Location | Description |
|---|---|---|
| `Literal` | `src/ast.rs` | `Int(i64)`, `Float(f64)`, `Str(String)`, `Bool(bool)` |
| `BinOp` | `src/ast.rs` | Arithmetic and comparison operators as enum variants |
| `Pattern` | `src/ast.rs` | Match arm pattern: `Wildcard`, `Literal`, `Ident`, `EmptyList`, `Cons`, `Constructor` |
| `StrPart` | `src/ast.rs` | Piece of an interpolated string: `Literal(String)` or `Parsed(Box<Expr>)` |
| `Expr` | `src/ast.rs` | Every expression form: `Literal`, `Ident`, `Resolved(u16)`, `Attr`, `FnCall`, `BinOp`, `Match`, `Pipe`, `Constructor`, `ErrorProp`, `InterpolatedStr`, `List(Vec<Expr>)`, `Tuple(Vec<Expr>)`, `MapLiteral(Vec<(Expr, Expr)>)`, `RecordCreate { type_name, fields }`, `RecordUpdate { type_name, base, updates }`, `TailCall(Box<(String, Vec<Expr>)>)` |
| `Stmt` | `src/ast.rs` | `Binding(name, Option<type_ann>, expr)`, `Expr(expr)` |
| `FnBody` | `src/ast.rs` | `Expr(Expr)` for `= expr` shorthand, or `Block(Vec<Stmt>)` |
| `FnDef` | `src/ast.rs` | Name, params, return type, effects, optional description, body |
| `Module` | `src/ast.rs` | Name, depends, exposes, intent string |
| `VerifyBlock` | `src/ast.rs` | Function name + list of `(left_expr, right_expr)` equality cases |
| `DecisionBlock` | `src/ast.rs` | Name, date, reason, chosen, rejected list, impacts list, optional author |
| `TopLevel` | `src/ast.rs` | Top-level item: `Module`, `FnDef`, `Verify`, `Decision`, `Stmt`, `TypeDef`, `EffectSet` |
| `TypeDef` | `src/ast.rs` | `Sum { name, variants: Vec<TypeVariant> }` or `Product { name, fields: Vec<(String, String)> }` |

## Runtime types

| Type | Location | Description |
|---|---|---|
| `Value` | `src/value.rs` | Runtime value: `Int`, `Float`, `Str`, `Bool`, `Unit`, `Ok(Box<Value>)`, `Err(Box<Value>)`, `Some(Box<Value>)`, `None`, `List(Vec<Value>)`, `Fn{..}`, `Builtin(String)`, `Variant { type_name, variant, fields }`, `Record { type_name, fields }`, `Namespace { name, members }` |
| `Env` | `src/value.rs` | `Vec<EnvFrame>` — scope stack (`Owned(HashMap)` or `Slots(Vec<Rc<Value>>)`), innermost last |
| `RuntimeError` | `src/value.rs` | Error enum: `Error(String)`, `ErrProp(Box<Value>)`, `TailCall(Box<(String, Vec<Value>)>)`, `ReplayMismatch(String)`, `ReplayExhausted` |
