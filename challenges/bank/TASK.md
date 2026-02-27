# Challenge: Bank Account System

## Goal

Write an Aver program (`solution.av`) that models a simple banking system.

## Domain

A bank manages accounts. Each account has an **owner** (String), **balance** (Float), and **id** (Int). The system must support these operations:

1. **Create an account** with an initial balance
2. **Deposit** money into an account (by id)
3. **Withdraw** money from an account (by id, fails if insufficient funds)
4. **Transfer** money between two accounts (by ids, fails if insufficient funds or same account)
5. **Find account** by id
6. **Total assets** — sum of all balances across accounts

## Requirements

### Data model
- Bank is a `List` of accounts
- Use Aver's `record` for the account type
- Use `Result` for operations that can fail (insufficient funds, account not found, etc.)

### Functions to implement
- `createAccount(bank, owner, initialBalance)` → returns updated bank with new account (auto-assign id)
- `deposit(bank, id, amount)` → returns updated bank or error (not found, negative amount)
- `withdraw(bank, id, amount)` → returns updated bank or error (not found, insufficient funds)
- `transfer(bank, fromId, toId, amount)` → returns updated bank or error
- `findAccount(bank, id)` → returns the account or error (not found)
- `totalAssets(bank)` → returns Float

### Conventions
- Read `README.md` and `docs/` for the language specification and API
- Look at `examples/calculator.av` as a reference for style and structure
- Every function must have a `? "..."` description
- Use `verify` blocks to prove your functions work
- Use at least one `decision` block to justify a design choice
- Use a `module` block with `intent`
- **No `if`/`else`** — use `match` for all branching
- **No loops** — use `List.map`, `List.filter`, `List.fold` for iteration
- All constructors are namespaced: `Result.Ok(x)`, `Result.Err(msg)`

### Main function
- `fn main()` with `! [Console]` that demonstrates all operations
- Print results using `Console.print`

## Verification

Your solution must pass:
```bash
aver check challenges/bank/solution.av
aver verify challenges/bank/solution.av
aver run challenges/bank/solution.av
```

## What success looks like

A single `solution.av` file that:
- Compiles without type errors (`check`)
- All verify cases pass (`verify`)
- Runs and prints meaningful output (`run`)
- Follows Aver conventions (namespaced calls, Result for errors, match for branching, descriptions on functions)
- Contains at least one `decision` block
- Contains at least 10 verify cases covering happy paths and error cases
