# Common Pushback

Questions, objections, and honest answers.

---

## Is Aver a functional language?

Not in the usual sense.

It borrows functional discipline — immutability, purity, pattern matching, recursion — but leaves out much of the abstraction machinery associated with modern FP: closures, generics, monads. Higher-order functions exist but only for callbacks (`Fn(A) -> B` parameters), not as a general composition tool.

Aver keeps the constraints and leaves out many of the compression tools.

Aver is closer to "pure and constrained" than to modern abstraction-heavy functional programming.

---

## Could this just be Rust macros?

No, because then the reviewer would need to understand Rust macros.

The whole point is that the artifact is readable without knowing the implementation language. Aver reads the same whether it runs through the VM, self-hosted path, or native Rust codegen. A Rust macro DSL would make readability depend on a layer of Rust that is already hard to review.

---

## Why would AI need a *new* language? It already writes Python fine.

AI writes Python fluently. That's the problem.

Python lets you do anything: mutate state, throw exceptions, capture closures, hide side effects. AI will happily use all of it. The result is code that works but is full of implicit behavior — hard to verify, hard to audit, hard to review with confidence.

Aver doesn't make AI's job easier. It makes the reviewer's job possible.

---

## Can AI even write in a language it wasn't trained on?

Yes — at least in our experience, surprisingly quickly.

The surface syntax is deliberately familiar: significant indentation, named functions, pattern matching. What's unusual is mostly what is absent: no mutation, no null, no exceptions, no loops.

Models do not seem to struggle much with unfamiliar syntax. They struggle more with ambiguity and implicit behavior. Aver is deliberately designed to have less of both.

---

## How do verification, effect replay, proofs, and codegen fit together?

They're concentric layers, not a sequential pipeline.

- **Verify blocks** live in source, run every time. This is your daily workflow.
- **Record/replay for effects** lets you capture and re-run effectful behavior without mocking infrastructure.
- **Lean/Dafny proof generation** is a layer you add when you want a harder guarantee. Not every function needs it. Not every project needs it.
- **Rust codegen** is always there. Your code runs native.

You use the depth you need, not the whole stack every time.

---

## Where do the guarantees live?

In the artifact itself. Intent descriptions, effect declarations, verify blocks, decision records — all in the same file as the code. Not in external specs, not in test harnesses, not in CI configs.

If the guarantees live outside the artifact, you're just making software easier to produce and harder to trust.

---

## Is this a toy language?

No, but it is still young.

The same source file runs unchanged through three execution modes: bytecode VM, self-hosted (an Aver interpreter written in Aver, compiled to Rust), and native Rust codegen. The self-hosted pipeline alone is several thousand lines of Aver — lexer, parser, evaluator — running through the full stack.

The language has its own LSP and generates proof obligations for Lean and Dafny.

It is early. But it is not a toy. Aver works at the scale it has been tested at, and that scale is growing with every release.

---

## Why no closures? Why no generics?

Because Aver is optimized for review, not authoring power.

Closures can hide captured context. Generics add another layer of abstraction. That is often worth it for the author. Aver is simply biased toward paying less of that cost during review.

This is not because abstraction is bad. It is because abstraction shifts more reconstruction work onto the reviewer.

Aver is intentionally biased toward explicit code over compressed code.

---

## What Aver is not, and never will be

Aver will never be a general-purpose language. That is a strategy, not an apology.

Capture and closures, mutation, and exceptions are out — not "not yet," but permanently. Each one breaks a property the proof engine relies on, so no version of Aver adds them back.

The line is not drawn by taste; it is drawn by a razor. A feature is admitted only if its elaboration preserves all four of:

- **purity** — no hidden state, so equational reasoning stays valid
- **first-order-ness after elaboration** — no captured environments surviving into the runtime, so signatures stay first-order and enumerable
- **structural termination** — recursion that provably shrinks, so there is no fuel or partiality to reason about
- **monomorphic proof obligations** — no proof that has to be quantified over unknown type shapes

Run candidate features through that razor and the answers fall out mechanically. Function references as callback parameters (`Fn(A) -> B`) pass all four and exist today. Generics via monomorphization would pass — after monomorphization the obligations look exactly like today's — so the razor leaves that door open, even though the review-cost bias above keeps it shut for now. Captured closures fail purity *and* first-order-ness, so they never enter.

This is not a corner we got stuck in. Verified kernels wrapped in thin, unverified shells is the common shape of serious formal-methods deployments. Aver is built to be the kernel and to make the shell obvious, not to swallow the whole program.

---

## Why not just use Lean or Coq directly?

Lean and Coq are proof assistants. Writing a web server in Lean is technically possible but practically miserable.

Aver is a *programming* language that generates proof obligations for Lean. You write normal-looking code. Aver extracts the verification parts and sends them to Lean. You don't write tactics. You don't fight the elaborator. The proof assistant does what it's good at — checking proofs — without being your programming environment.

There is a deeper structural difference, and it grows more important as models get better at Lean tactics. With an LLM writing Lean against Mathlib, you prove theorems about a *model* of your program — a hand-maintained Lean transcription of what the code is supposed to do. Keeping that model in step with the code that actually ships is manual work, and the correspondence rots a little with every commit. In Aver there is no separately maintained model to rot: the spec, the proof obligations, and the executable come from one artifact, and the obligations are regenerated mechanically from that source on every build. The remaining trusted link is the statement translator — one mechanical component, [documented plainly in docs/lean.md](lean.md), not a transcription someone keeps in step by hand. Better tactic-writing narrows the gap in *proving*; it does little for the gap in *correspondence*, and correspondence is the part that quietly breaks.

---

## Do you need a package manager?

Not yet.

Aver is still small enough that a traditional package ecosystem is not the first bottleneck. More importantly, a lot of dependency usage in current ecosystems is a human shortcut for "I don't want to write this."

In an AI-assisted workflow, that tradeoff is no longer obviously the same.

If we ever absolutely have to add one, it's called Morphine.

---

## "AI-optimized" just means "easy for models to emit," right?

No. "AI-optimized" should mean auditable by default, not just easy to emit.

Strict types help. Explicit effects help. Clean dependency graphs help. But if the code doesn't carry its own intent and constraints, we're just making software easier to produce and harder to trust.

The optimization target is the reviewer, not the generator.

---

## What's the difference between Aver and [Bend / NanoLang / other AI-first language]?

Many projects in this space optimize for *how code executes* — parallel runtimes, novel compilation targets, agent-friendly toolchains.

Aver optimizes for *what the code communicates*. Intent, effects, constraints, and verification are part of the language, not external tooling. The question isn't "can the agent write it faster" but "can the reviewer trust it without having written it."

---

## What are Aver's limits?

Aver assumes the AI is cooperative — that it writes in good faith and needs structure, not supervision.

Verify blocks check the properties you actually encode. They do not check whether what it declares is what you actually need. Decision blocks record tradeoffs, but they cannot enforce that the right tradeoffs were considered.

Aver protects against bugs, ambiguity, and implicit behavior. It does not protect against wrong intent — yours or the AI's. If you formalize the wrong thing precisely, it will pass every check.

The honest version: Aver makes review possible. It does not make review unnecessary.

---

## What does Aver assume about the future role of humans?

Aver assumes that someone reviews the code. It does not assume that someone is human.

The language is designed so that intent, effects, constraints, and checks are legible in the artifact itself. That helps a human reviewer, but it also helps an AI reviewer. Explicit structure is easier to verify than implicit behavior, regardless of who or what is doing the verification.

If the future is human review, Aver makes it more possible. If the future is AI reviewing AI, Aver makes that review cheaper and more reliable.

The bet is not on humans. It is on legibility.

---

## What is Aver actually optimizing for?

Aver is optimized for a world where code is cheap to generate and expensive to trust.

Its goal is not to make generation easier. It is to keep intent, effects, constraints, and checks legible enough that someone who did not write the code can still review it with confidence.
