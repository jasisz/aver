# Agent Challenge: Inventory Management

You are an AI agent being tested on your ability to learn a new programming language and compare it with one you already know.

## Rules

1. **Read `README.md`** in the project root — it explains Aver, its philosophy, and its features
2. **Read the docs** in `docs/` — especially `docs/services.md` for the full API reference
3. **Read `examples/calculator.av`** as a style and convention reference
4. **Read `challenges/inventory/TASK.md`** for the task requirements
5. **Implement the solution twice:**
   - `challenges/inventory/solution.av` — in Aver
   - `challenges/inventory/solution_compare.*` — in a language of your choice
6. **Write `challenges/inventory/notes.md`** — an honest comparison of both implementations
7. **Verify the Aver version** by running:
   ```bash
   aver check challenges/inventory/solution.av
   aver verify challenges/inventory/solution.av
   aver run challenges/inventory/solution.av
   ```
8. **Do not ask questions** — everything you need is in the README, docs, and the example
9. **Do not read other example files** — only `examples/calculator.av` is allowed as reference
10. **Do not modify any existing files** — only create your solution files and notes

## Evaluation

When you're done, run:
```bash
bash challenges/inventory/evaluate.sh
```

This will check your Aver solution against all criteria. Your goal is `CHALLENGE PASSED`.

## What's being measured

- Can you learn a new language from its README, docs, and one example?
- Can you implement the same problem in two languages and compare them thoughtfully?
- Do you write idiomatic code in both languages (not a literal translation)?
- Are your comparison notes honest, specific, and backed by code examples?
- How many iterations did you need before green?

Good luck.
