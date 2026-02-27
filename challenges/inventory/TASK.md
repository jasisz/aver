# Challenge: Inventory Management System

## Goal

Build an Aver program (`solution.av`) that models a product inventory for a small warehouse.

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

Write `fn main()` that demonstrates all operations with sample data and prints results.

## Verification

Include `verify` blocks that prove your functions work — cover both happy paths and error cases (not found, duplicates, empty inventory, edge cases).

Your solution must pass:
```
aver check challenges/inventory/solution.av
aver verify challenges/inventory/solution.av
aver run challenges/inventory/solution.av
```

## Getting started

1. Read `README.md` — the complete language reference
2. Read `docs/services.md` — full API for all built-in namespaces
3. Study `examples/calculator.av` — conventions and style
