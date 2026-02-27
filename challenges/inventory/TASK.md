# Challenge: Inventory Management System

## Goal

Write an Aver program (`solution.av`) that models a simple product inventory.

## Domain

A warehouse tracks products. Each product has a **name** (String), **price** (Float), and **quantity** (Int). The system must support these operations:

1. **Add a product** to the inventory
2. **Update quantity** of an existing product (by name)
3. **Find a product** by name
4. **Calculate total inventory value** (sum of price × quantity for all products)

## Requirements

### Data model
- Inventory is a `List` of products
- Use Aver's `record` for the product type
- Use `Result` for operations that can fail (product not found, duplicate name, etc.)

### Functions to implement
- `addProduct(inventory, name, price, quantity)` → returns updated inventory or error (duplicate name)
- `updateQuantity(inventory, name, newQuantity)` → returns updated inventory or error (not found)
- `findProduct(inventory, name)` → returns the product or error (not found)
- `totalValue(inventory)` → returns Float

### Conventions
- Read `README.md` and `docs/` for the language specification and API
- Look at `examples/calculator.av` as a reference for style and structure
- Every function must have a `? "..."` description
- Use `verify` blocks to prove your functions work
- Use at least one `decision` block to justify your data model choice
- Use a `module` block with `intent`
- **No `if`/`else`** — use `match` for all branching
- **No loops** — use `List.map`, `List.filter`, `List.fold` for iteration
- All constructors are namespaced: `Result.Ok(x)`, `Result.Err(msg)`

### Main function
- `fn main()` with `! [Console]` that demonstrates all four operations
- Print results using `Console.print`

## Verification

Your solution must pass:
```bash
cargo run -- check challenges/inventory/solution.av
cargo run -- verify challenges/inventory/solution.av
cargo run -- run challenges/inventory/solution.av
```

## What success looks like

A single `solution.av` file that:
- Compiles without type errors (`check`)
- All verify cases pass (`verify`)
- Runs and prints meaningful output (`run`)
- Follows Aver conventions (namespaced calls, Result for errors, match for branching, descriptions on functions)
- Contains at least one `decision` block
- Contains at least 8 verify cases covering happy paths and error cases
