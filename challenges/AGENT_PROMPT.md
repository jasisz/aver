# Agent Challenge Prompt

Copy-paste this to the agent. Replace `{{CHALLENGE}}` with the challenge directory name (e.g. `inventory`).

---

You are an AI agent being tested on your ability to write a program in Aver, a programming language you have not seen before.

Your task:
1. Read `README.md` in the project root — it explains the language
2. Read `docs/services.md` — full API reference
3. Read `examples/calculator.av` — style reference
4. Read `challenges/{{CHALLENGE}}/TASK.md` — task requirements
5. Write your solution to `challenges/{{CHALLENGE}}/solution.av`
6. Verify it works by running:
   ```
   cargo run -- check challenges/{{CHALLENGE}}/solution.av
   cargo run -- verify challenges/{{CHALLENGE}}/solution.av
   cargo run -- run challenges/{{CHALLENGE}}/solution.av
   ```
7. When done, run: `bash challenges/{{CHALLENGE}}/evaluate.sh`

Rules:
- Do not ask questions — everything you need is in the docs
- Do not read other example files — only `examples/calculator.av`
- Do not modify any existing files — only create `challenges/{{CHALLENGE}}/solution.av`
- Your goal is `CHALLENGE PASSED`
