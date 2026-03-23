# Self-Hosted

Mini-interpreter of an Aver subset, written in Aver.

Demonstrates self-hosting: Aver executing Aver source code on its own VM.

## Supported subset

- Int, String, Bool literals
- Arithmetic: `+`, `-`, `*`, `/`
- Comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Bindings: `x = 5`
- Function definitions with multi-statement bodies
- Function calls: `add(1, 2)`
- Match expressions with literal, bool, wildcard, variable, and list patterns
- List literals: `[1, 2, 3]`, `[]`
- List pattern matching: `[]`, `[h, ..t]`
- Recursion (e.g. fibonacci, list sum)

## How to run

```bash
# Run a file
aver run projects/self_hosted/main.av --module-root projects/self_hosted -- examples/fib.av

# Built-in demos (no file argument)
aver run projects/self_hosted/main.av --module-root projects/self_hosted

# Run on VM
aver run --vm projects/self_hosted/main.av --module-root projects/self_hosted -- examples/fib.av
```
