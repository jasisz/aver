# Agent Assessment

How to evaluate the agent after it completes a challenge.

## Timing

| Metric | How to measure |
|---|---|
| Wall time | Start → `CHALLENGE PASSED` (or give up) |
| Iterations | How many edit-run cycles before green |
| First working version | Time to first `aver run` success |
| Comparison language | Which language did the agent pick? |

## Deliverables checklist

- [ ] `solution.av` — passes evaluate.sh (13/13)
- [ ] `solution_compare.*` — equivalent implementation in chosen language
- [ ] `notes.md` — honest comparison of both implementations

## Evaluating notes.md

The notes are the most important output. Good notes should cover:

### Comparison depth
- Does it go beyond surface syntax ("Aver uses match, Python uses if")?
- Does it discuss how constraints shaped the solution design?
- Does it identify concrete trade-offs, not just list differences?

### Honesty
- Does it acknowledge where Aver is worse, not just better?
- Does it give specific examples from the code, not generic statements?
- Does it avoid sycophantic praise ("Aver's elegant design...")?

### Key questions to look for in notes

| Question | What a good answer looks like |
|---|---|
| Which was easier to write? | Specific — "the Aver version needed 3 helper functions for what Python does in a list comprehension" |
| Which is easier to read? | References actual code — "the match chain in `withdraw` reads like a spec, but the Python version is more scannable" |
| Where did no-mutation help? | Concrete — "I never had a bug from accidental state change" or "it made the transaction log trivial" |
| Where did no-mutation hurt? | Concrete — "updating one field in a record required rebuilding the entire structure" |
| verify vs tests? | Compared the experience — setup, discoverability, expressiveness, not just syntax |
| What would you steal? | Something specific and non-obvious, not "I'd add match to Python" |

### Red flags in notes
- Generic praise without code references
- No mention of Aver's weaknesses
- Comparison is only about syntax, not about how it felt to solve the problem
- Notes read like marketing copy
- "Both languages have their strengths" without saying which strengths

## Scoring rubric

| Category | Weight | Criteria |
|---|---|---|
| Aver correctness | 30% | evaluate.sh passes (13/13) |
| Comparison implementation | 15% | Equivalent solution exists, runs, handles same cases |
| Idiomatic Aver | 15% | match-only, namespaced calls, Result/Option, verify blocks |
| Idiomatic comparison | 10% | Uses the chosen language's idioms, not a literal translation |
| Notes quality | 30% | Honest, specific, comparative (see criteria above) |

## Post-challenge questions (optional, ask the agent)

1. Why did you pick [language X] for comparison?
2. If you had to maintain one of these for a year, which would you choose?
3. What surprised you most about Aver?
4. What was the hardest part of the Aver implementation?
5. Would you use Aver for a real project? Under what conditions?
