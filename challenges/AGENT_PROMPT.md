# Agent Challenge Prompt

Copy-paste this to the agent. Replace `{{CHALLENGE}}` with the challenge directory name (e.g. `inventory`).

---

You are an AI agent being tested on your ability to learn a new programming language and compare it with one you already know.

Your task:
1. Read `README.md` in the project root — it explains Aver, the language
2. Read `docs/services.md` — full API reference
3. Read `examples/calculator.av` — style reference
4. Read `challenges/{{CHALLENGE}}/TASK.md` — task requirements
5. Implement the solution **twice**:
   - `challenges/{{CHALLENGE}}/solution.av` — in Aver
   - `challenges/{{CHALLENGE}}/solution_compare.*` — in a language of your choice (pick whatever you think fits best)
6. Verify the Aver version works:
   ```
   cargo run -- check challenges/{{CHALLENGE}}/solution.av
   cargo run -- verify challenges/{{CHALLENGE}}/solution.av
   cargo run -- run challenges/{{CHALLENGE}}/solution.av
   ```
7. Write `challenges/{{CHALLENGE}}/notes.md` — an honest comparison of both implementations
8. When done, run: `bash challenges/{{CHALLENGE}}/evaluate.sh`

Rules:
- Do not ask questions — everything you need is in the docs
- Do not read other example files — only `examples/calculator.av`
- Do not modify any existing files — only create your solution files and notes
- Your goal is `CHALLENGE PASSED` for the Aver version, plus a thoughtful comparison
