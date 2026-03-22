# Self-Hosted

Mini-interpreter of an Aver subset, written in Aver.

Demonstrates self-hosting: Aver executing Aver source code on its own VM.

## Supported subset

- Int literals and arithmetic (`+`, `-`, `*`)
- String literals
- Bindings (`x = 5`)
- Function definitions (`fn add(a, b) ...`)
- Function calls (`add(1, 2)`)
- Match expressions with literal and wildcard patterns
- Recursion (e.g. fibonacci)

## How to run

```bash
aver run projects/self_hosted/main.av --vm
```
