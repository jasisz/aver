# Agent Challenge Prompt

Copy-paste this to the agent. Replace `{{CHALLENGE}}` with the challenge directory name (e.g. `inventory`).

---

You are an AI agent being tested on your ability to learn a new programming language and compare it with one you already know.

Your task (do these steps IN ORDER):

PHASE 1 — Solve it in your language first:
1. Read `challenges/{{CHALLENGE}}/TASK.md` — task requirements
2. Pick a language you're comfortable with and implement the solution:
   `challenges/{{CHALLENGE}}/solution_compare.*` (e.g. `.py`, `.ts`, `.rs`)
   Write it naturally — use whatever patterns, idioms, and structures you normally would.
   Include tests using the language's native test framework.

PHASE 2 — Now learn Aver and solve it again:
3. Read `README.md` — the complete language reference
4. Read `docs/services.md` — full API reference
5. Read `examples/calculator.av` — style reference
6. Implement the same solution in Aver: `challenges/{{CHALLENGE}}/solution.av`
   Do NOT just translate your first solution line-by-line — write idiomatic Aver.
7. Verify it works:
   ```
   cargo run -- check challenges/{{CHALLENGE}}/solution.av
   cargo run -- verify challenges/{{CHALLENGE}}/solution.av
   cargo run -- run challenges/{{CHALLENGE}}/solution.av
   ```
8. Write `challenges/{{CHALLENGE}}/notes.md` — an honest comparison of both implementations
9. When done, run: `bash challenges/{{CHALLENGE}}/evaluate.sh`

Rules:
- Do not ask questions — everything you need is in the docs
- Do not read other example files — only `examples/calculator.av`
- Do not modify any existing files — only create your solution files and notes
- IMPORTANT: finish Phase 1 completely before starting Phase 2
- Your goal is `CHALLENGE PASSED` for the Aver version, plus a thoughtful comparison
