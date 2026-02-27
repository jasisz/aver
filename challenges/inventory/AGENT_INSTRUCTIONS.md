# Agent Challenge: Inventory Management

You are an AI agent being tested on your ability to write a program in **Aver**, a programming language you have not seen before.

## Rules

1. **Read `README.md`** in the project root — it explains the language, its philosophy, and its features
2. **Read the docs** in `docs/` — especially `docs/services.md` for the full API reference
3. **Read `examples/calculator.av`** as a style and convention reference
4. **Read `challenges/inventory/TASK.md`** for the task requirements
5. **Write your solution** to `challenges/inventory/solution.av`
6. **Verify it works** by running:
   ```bash
   aver check challenges/inventory/solution.av
   aver verify challenges/inventory/solution.av
   aver run challenges/inventory/solution.av
   ```
7. **Do not ask questions** — everything you need is in the README, docs, and the example
8. **Do not read other example files** — only `examples/calculator.av` is allowed as reference
9. **Do not modify any existing files** — only create `challenges/inventory/solution.av`

## Evaluation

When you're done, run:
```bash
bash challenges/inventory/evaluate.sh
```

This will check your solution against all criteria. Your goal is `CHALLENGE PASSED`.

## What's being measured

- Can you learn a new language from its README, docs, and one example?
- Do you follow the language's conventions without being told twice?
- Do you write idiomatic code (match over if, Result over exceptions, namespaced calls)?
- Do you use the language's unique features (verify blocks, decision blocks, ? descriptions)?
- How many iterations did you need before green?

Good luck.
