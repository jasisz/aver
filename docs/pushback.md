# Common Pushback

Questions and objections we hear, with our answers.

---

## Is Aver a functional language?

Not in the usual sense.

It takes the discipline of functional programming: immutability, purity, pattern matching and recursion. It leaves out much of the abstraction machinery of modern FP, such as closures, generics and monads. Higher-order functions exist only for callbacks (`Fn(A) -> B` parameters). They are not a general tool for composition.

So Aver keeps the constraints and drops many of the tools for compressing code. It is closer to "pure and constrained" than to abstraction-heavy functional programming.

---

## Could this just be Rust macros?

No. Then the reviewer would need to understand Rust macros.

The artifact has to be readable without knowing the implementation language. Aver reads the same whether it runs through the VM, the self-hosted path or native Rust codegen. With a Rust macro DSL, readability would depend on a layer of Rust that is already hard to review.

---

## Why would AI need a *new* language? It already writes Python fine.

AI writes Python fluently, and that is the problem.

Python lets you mutate state, throw exceptions, capture closures and hide side effects. AI will use all of it. The code works, but it is full of implicit behavior, which makes it hard to verify, audit or review with confidence.

Aver does not make the AI's job easier. It makes the reviewer's job possible.

---

## Can AI even write in a language it wasn't trained on?

Yes. In our experience it picks it up surprisingly quickly.

The surface syntax is familiar on purpose: significant indentation, named functions, pattern matching. Most of what is unusual is what's missing. There is no mutation, no null, no exceptions and no loops.

Models do not seem to struggle much with unfamiliar syntax. They struggle more with ambiguity and implicit behavior, and Aver is designed to have less of both.

---

## How do verification, effect replay, proofs, and codegen fit together?

They are layers around the same code. You do not run them in sequence as a pipeline.

- **Verify blocks** live in source and run every time. This is the daily workflow.
- **Record/replay for effects** captures effectful behavior and re-runs it without mocking infrastructure.
- **Lean proof generation** is a layer you add when you want a stronger guarantee. Not every function needs it, and not every project does.
- **Rust codegen** is always available, so your code runs native.

You go as deep as you need. You do not have to use the whole stack every time.

---

## Where do the guarantees live?

In the artifact. Intent descriptions, effect declarations, verify blocks and decision records sit in the same file as the code. They are not kept in external specs, test harnesses or CI configs.

If the guarantees live outside the artifact, software gets easier to produce and harder to trust.

---

## Is this a toy language?

No, but it is young.

The same source file runs unchanged in three execution modes: a bytecode VM, self-hosted (an Aver interpreter written in Aver and compiled to Rust), and native Rust codegen. The self-hosted pipeline alone is several thousand lines of Aver (lexer, parser, evaluator) running through the full stack.

The language has its own LSP and generates proof obligations for Lean.

It is early. Aver works at the scale it has been tested at, and that scale grows with every release.

---

## Why no closures? Why no generics?

Aver is optimized for review. Power for the author comes second.

Closures can hide captured context, and generics add another layer of abstraction. For the author that is often worth it. Aver leans toward keeping the cost for the reviewer low.

Abstraction is not bad. It moves more reconstruction work onto the reviewer, so Aver prefers explicit code over compressed code.

---

## What Aver is not, and never will be

Aver will never be a general-purpose language. That is a deliberate strategy.

Capture and closures, mutation and exceptions are permanently out, not deferred to a later version. Each one breaks a property the proof engine relies on, so no version of Aver will add them back.

Where the line falls is decided by a rule. A feature gets in only if its elaboration preserves all four of these:

- **purity**: no hidden state, so equational reasoning stays valid
- **first-order-ness after elaboration**: no captured environments survive into the runtime, so signatures stay first-order and enumerable
- **structural termination**: recursion that provably shrinks, so there is no fuel or partiality to reason about
- **monomorphic proof obligations**: no proof has to be quantified over unknown type shapes

Apply the rule to a candidate feature and the answer follows mechanically. Function references as callback parameters (`Fn(A) -> B`) pass all four and exist today. Generics through monomorphization would pass, because after monomorphization the obligations look exactly like today's. The rule leaves that option open, even though the review-cost bias above keeps generics out for now. Captured closures fail purity *and* first-order-ness, so they never get in.

We did not end up here by accident. Serious formal-methods deployments usually look like this: verified kernels inside thin, unverified shells. Aver is built to be the kernel and to make the shell obvious. It does not try to take over the whole program.

---

## Why not just use Lean or Coq directly?

Lean and Coq are proof assistants. You could write a web server in Lean, but it would be miserable in practice.

Aver is a *programming* language that generates proof obligations for Lean. You write normal-looking code, and Aver extracts the verification parts and sends them to Lean. You do not write tactics or fight the elaborator. The proof assistant checks proofs, which is what it is good at, and you do not have to program inside it.

There is also a structural difference, and it matters more as models get better at Lean tactics. When an LLM writes Lean against Mathlib, you prove theorems about a *model* of your program: a hand-maintained Lean transcription of what the code is supposed to do. Someone has to keep that model in step with the code that ships, by hand, and the match drifts a little with every commit. Aver has no separate model to drift. The spec, the proof obligations and the executable come from one artifact, and every build regenerates the obligations mechanically from that source. The remaining trusted link is the statement translator. It is one mechanical component, [documented plainly in docs/lean.md](lean.md), and nobody has to keep it in step by hand. Better tactic-writing closes the gap in *proving*. It does little for the gap in *correspondence*, and correspondence is the part that breaks without anyone noticing.

---

## Do you need a package manager?

Not yet.

Aver is still small enough that a package ecosystem is not the first bottleneck. Also, much dependency use in current ecosystems is a human shortcut for "I don't want to write this." In an AI-assisted workflow, that tradeoff is no longer clearly the same.

If we ever absolutely have to add one, it's called Morphine.

---

## "AI-optimized" just means "easy for models to emit," right?

No. "AI-optimized" should mean auditable by default. Being easy to emit is not enough.

Strict types help. Explicit effects help. Clean dependency graphs help. But if the code does not carry its own intent and constraints, software only gets easier to produce and harder to trust.

Aver optimizes for the reviewer. The generator comes second.

---

## What's the difference between Aver and [Bend / NanoLang / other AI-first language]?

Many projects in this space optimize *how code executes*: parallel runtimes, new compilation targets, toolchains that suit agents.

Aver optimizes *what the code communicates*. Intent, effects, constraints and verification are part of the language, so no external tooling is needed for them. The question Aver asks is whether the reviewer can trust the code without having written it. How fast the agent can write it matters less.

---

## What are Aver's limits?

Aver assumes the AI is cooperative: it writes in good faith and needs structure rather than supervision.

Verify blocks check the properties you actually encode. They do not check whether what the code declares is what you need. Decision blocks record tradeoffs, but they cannot make sure the right tradeoffs were considered.

Aver protects against bugs, ambiguity and implicit behavior. It does not protect against wrong intent, yours or the AI's. If you formalize the wrong thing precisely, it passes every check.

In short, Aver makes review possible. It does not make review unnecessary.

---

## What does Aver assume about the future role of humans?

Aver assumes that someone reviews the code. It does not assume that the reviewer is human.

The language keeps intent, effects, constraints and checks readable in the artifact itself. That helps a human reviewer and an AI reviewer alike. Explicit structure is easier to verify than implicit behavior, whoever or whatever does the verifying.

If review stays human, Aver makes it more feasible. If AI ends up reviewing AI, Aver makes that review cheaper and more reliable.

What Aver depends on is legibility, whoever the reviewer turns out to be.

---

## What is Aver actually optimizing for?

Aver is built for a world where code is cheap to generate and expensive to trust.

Its goal is to keep intent, effects, constraints and checks readable enough that someone who did not write the code can still review it with confidence. Making generation easier is not the goal.
