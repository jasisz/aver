# Agent Assessment

Questions and criteria to evaluate the agent after it completes (or fails) a challenge.

## Timing

| Metric | How to measure |
|---|---|
| Wall time | Start → `CHALLENGE PASSED` (or give up) |
| Iterations | How many edit-run cycles before green |
| First working version | Time to first `cargo run -- run` success |

## Post-challenge questions (ask the agent)

### 1. Language comprehension
- What is Aver's branching construct? (expected: `match`, no `if`/`else`)
- How do you handle errors in Aver? (expected: `Result<T,E>` with `Result.Ok`/`Result.Err`, `?` operator)
- How does iteration work? (expected: `List.map`/`filter`/`fold`, no loops)

### 2. API discovery
- Where did you find the function signatures? (expected: README and/or `docs/services.md`)
- Name 3 List functions you used. (check if they used correct namespace calls)
- What does `List.get` return? (expected: `Option<T>` — tests if agent read updated docs)

### 3. Debugging process
- What errors did you hit? How did you fix them?
- Did you use `cargo run -- check` before `verify`? (good practice)
- How many times did you re-read the docs?

### 4. Code quality (review `solution.av`)
- Does every function have a `?` description?
- Are there `verify` blocks with edge cases (empty list, not found, duplicates)?
- Is there a `decision` block with meaningful reasoning?
- Is the code readable without comments (self-documenting names)?

## Scoring rubric

| Category | Weight | Criteria |
|---|---|---|
| Correctness | 40% | evaluate.sh passes (13/13) |
| Idiomatic code | 25% | match-only, namespaced calls, Result/Option, no workarounds |
| Docs usage | 15% | Read README + docs/services.md, not trial-and-error |
| Verify quality | 10% | Edge cases covered, not just happy path |
| Speed | 10% | Under 10 min = excellent, 10-20 = good, 20+ = slow |

## Red flags (automatic deductions)
- Used `if`/`else` (didn't read docs)
- Flat builtins like `print()` or `len()` (didn't read namespace docs)
- `List.get` returning `Result` (read stale docs or guessed)
- Asked clarifying questions (instructions say don't ask)
- Modified existing files
