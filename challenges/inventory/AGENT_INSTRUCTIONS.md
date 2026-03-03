# Agent Challenge: Inventory Management

You are an AI agent being tested on your ability to learn a new programming language and compare it with one you already know.

## Rules

Do these steps **in order**:

### Phase 1 — Solve it in your language first
1. **Read `challenges/inventory/TASK.md`** for the task requirements
2. **Pick a language** you're comfortable with
3. **Write `challenges/inventory/solution_compare.*`** — implement it naturally, using whatever patterns and idioms you normally would. Include tests.

### Phase 2 — Learn Aver and solve it again
4. **Read `README.md`** in the project root — it explains Aver, its philosophy, and its features
5. **Read the docs** in `docs/` — especially `docs/services.md` for the full API reference
6. **Read `examples/calculator.av`** as a style and convention reference
7. **Write `challenges/inventory/solution.av`** — do NOT translate line-by-line, write idiomatic Aver
8. **Verify the Aver version** by running:
   ```bash
   aver check challenges/inventory/solution.av
   aver verify challenges/inventory/solution.av
   aver run challenges/inventory/solution.av
   ```

### Phase 3 — Compare
9. **Write `challenges/inventory/notes.md`** — an honest comparison of both implementations
10. **Do not ask questions** — everything you need is in the README, docs, and the example
11. **Do not read other example files** — only `examples/calculator.av` is allowed as reference
12. **Do not modify any existing files** — only create your solution files and notes
13. **IMPORTANT:** finish Phase 1 completely before starting Phase 2

## Evaluation

When you're done, run:
```bash
bash challenges/inventory/evaluate.sh
```

This will check your Aver solution against all criteria. Your goal is `CHALLENGE PASSED`.

## What's being measured

- Can you solve the problem naturally in a language you know?
- Can you then learn a new language and solve the same problem idiomatically?
- Did your Aver solution look different from your first one, or was it a copy-paste translation?
- Are your comparison notes honest, specific, and backed by code examples?
- How many iterations did you need before green?

Good luck.
