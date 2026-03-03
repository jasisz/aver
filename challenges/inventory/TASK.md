# Challenge: Inventory Management System

## Goal

Build a product inventory system for a small warehouse — **twice**.

1. First, implement it in a language of your choice (`solution_compare.*`) — write it naturally, using whatever patterns you normally would
2. Then, learn Aver and implement it again (`solution.av`) — write idiomatic Aver, not a line-by-line translation

## What the system should do

The warehouse tracks products. Each product has a name, unit price, and quantity in stock.

Your program should support:

1. **Add** a new product to inventory (reject duplicates)
2. **Find** a product by name
3. **Update** the stock quantity of a product
4. **Remove** a product from inventory
5. **Total value** — sum of (price × quantity) across all products
6. **Low stock report** — find all products below a given quantity threshold
7. **Apply discount** — reduce the price of every product above a given price by a percentage

Think about: what data structures best fit this domain? How should errors be represented? What edge cases exist?

## Main function

In both versions, write a `main` that demonstrates all operations with sample data and prints results.

## Verification

In the Aver version, include `verify` blocks that prove your functions work — cover both happy paths and error cases (not found, duplicates, empty inventory, edge cases).

In the comparison version, write equivalent tests using the language's native test framework.

Your Aver solution must pass:
```
aver check challenges/inventory/solution.av
aver verify challenges/inventory/solution.av
aver run challenges/inventory/solution.av
```

## Comparison notes

After implementing both versions, write `notes.md` with your comparison:

- Which language made the problem easier to express? Why?
- Where did Aver's constraints (no if/else, no loops, no mutation) help or hurt?
- How did error handling compare (Result/match vs exceptions/try-catch/etc.)?
- How did testability compare (verify blocks vs your language's test framework)?
- What would you steal from Aver for your chosen language, and vice versa?
- Which version do you prefer reading? Which do you prefer writing?
- Lines of code comparison — is one significantly shorter?

Be honest and specific. We want genuine comparison, not flattery.

## Getting started

1. Read `README.md` — the complete language reference
2. Read `docs/services.md` — full API for all built-in namespaces
3. Study `examples/calculator.av` — conventions and style
