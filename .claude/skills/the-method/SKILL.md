---
description: The Method — close an OPEN Aver proof law by having an agent propose auxiliary helper lemmas, test them with `aver proof --discover`/`--check`, and refine until the Lean kernel / Z3 certifies the law. Works on any Aver project; the agent proposes, the judge decides.
allowed-tools: Bash, Read, Write, Workflow
---

# The Method

A reusable loop for closing an OPEN Aver `verify ... law` in **any** Aver project. An agent
PROPOSES auxiliary helper lemmas, `aver` TESTS them, and the Lean kernel / Z3 JUDGES whether the
target law now closes — looping until it closes or a budget runs out. The LLM proposer is the only
unbounded source of new lemmas; the judge keeps it sound.

## When to use
- A `verify ... law` is open in Lean (often already provable by Z3/Dafny — then this kernel-
  certifies it in Lean).
- A goal that needs an auxiliary lemma the auto-prover can't find on its own (a missing
  homomorphism / associativity / distributivity / an equation relating subterms of the goal).

## Pre-check — run BEFORE spending tokens (the frontier is predictable)

The Method closes CONTENT gaps: a missing TRUE STATEMENT expressible in the law surface. It is
measurably the wrong hammer for FORM gaps (engine/executor procedures) — a past probe burned
~1.3M tokens on goals that were never content-shaped. Classify first:

0. **Samples first.** If the target (or a proposed helper) is false-on-samples, the STATEMENT is
   wrong — fix the claim or the decomposition; there is nothing to prove yet.
1. **The truth-value test.** Isolate the missing piece and ask: *does it have a truth value?*
   - YES, and statable as an Aver law (bare givens + `when`-guards + program fns) → The Method.
   - NO — it is a "how" (normalize, pick an induction scheme, split a match, chain citations) →
     FORM gap: stop; file an engine gap instead of probing.
   - YES but not statable (meta: Bool↔Prop bridging, translation artifacts) → executor bridge,
     engine work.
   - CAN'T TELL → you have not isolated the missing piece; decompose/minimize further before
     probing. Composite identities often decompose into statable leaves — try the decomposition
     BEFORE classifying the whole goal as unreachable (the trunc-sticky wall fell exactly this way).
2. **Driver check (syntactic, zero tokens).** A composite scrutinee (`match f(x)`),
   Nat-predecessor arithmetic, or higher-order (map/filter) in the goal's cone is a known
   executor-gap class; The Method will not fix it.
3. **Closing-algebra check (per backend).** Int `+` closes on both backends; nonlinear Int `·`:
   Lean has the order primitive, Z3 is native; user-ADT multiplication/distributivity on Dafny is
   turbo-hard — do not grind it (route Lean-only with per-backend credit).

## Stop-losses (how to read repeated verdicts)

- Helpers repeatedly **false-on-samples** → the target likely needs an INVENTED intermediate
  quantity (a generalization) — escalate to design; more rounds will not help.
- Helpers **true-on-samples but 0 closures for 2 consecutive rounds** → it was a FORM gap after
  all: STOP the loop and record the residual as an engine gap. Do not raise `attempts` past this
  signal.

## Usage
```
/the-method <task.av> [<task2.av> ...]
```
Run from your Aver project root. Paths are relative to that root, or absolute. The loop auto-detects
the `aver` binary (`./target/release/aver`, `./target/debug/aver`, or `aver` on PATH; it will build
it if missing). You can also invoke the engine directly:
```
Workflow({ scriptPath: "<this-skill-dir>/the-method-loop.js", args: { tasks: ["path/to/task.av"], attempts: 4 } })
```

## How it runs
The conjecturer and the prover are SEPARATE, capability-fenced agents — "the agent proposes, the
kernel decides" is enforced structurally, not merely asked. One independent chain per task, run in
parallel; within a chain, up to `attempts` propose→test rounds:
1. **Conjecturer** (`the-method-proposer`, **Read+Glob only** — no toolchain, no Bash, cannot open
   any generated `.lean`/`.dfy`): reads the target (and one `decomposed/` example, if present) and
   proposes 1–3 true, general helper laws aimed at the open goal — never a restatement.
2. **Runner** (`the-method-runner`): mechanically splices the laws into a `/tmp` copy — **before**
   the target `verify ... law` (order + rendering matter) — then runs a **three-stage sieve**,
   cheapest first: (a) `aver check` (parse/typecheck — a malformed law caught + fed straight back);
   (b) **`aver verify`** (bounded sample eval, no Lean — an INDEPENDENT Aver-semantics check: a law
   FALSE on its samples, e.g. a `Nat`-returning fn bridged to an `Int`-returning builtin, is
   rejected here as `false-on-samples`, before any Lean — this catches the class the kernel proof
   can wrongly accept because its own bounded check shares the Lean translation); (c) only if verify
   is clean, `aver proof --check --check-json --backend lean`. A closure requires **both**
   `verifyClean` AND `universal:true`/`sorries:0`. The runner returns **only the Aver-level verdict**
   (per-law `lawStatus`: proven / sample-only / open / false-on-samples) — the Lean residual never
   crosses back. **No `--discover`**: The Method measures the LLM-proposed laws + our auto-prover,
   not the enumerative recognizer; a closure is self-contained, and the conjecturer must supply every
   law (including bridges to builtins like `List.concat`/`List.reverse`).
3. On failure the conjecturer refines against that Aver-level verdict; on `"universal":true` with
   `"sorries":0` the chain closes.

Because the conjecturer physically cannot see the proof, it cannot drift from conjecturing into
tactic/prover-internals debugging (the measured dominant cost sink). The `model` override sets the
conjecturer; the runner and verify gate stay on the session model by default. (The runner is
mechanical but must read the verdict ACCURATELY — the loop only verifies a self-reported close, so a
runner that under-reports silently drops a real win; a measured run showed a `haiku` runner doing
exactly that, for marginal cost saving. Pass `runnerModel`/`verifierModel` to override, accepting
that risk.)

Then a **Verify** phase (`the-method-verifier`) independently re-checks each claimed closure from
scratch (a fresh dir) — a self-reported closure is not trusted on its own — and persists the
verified decomposition to `decomposed/`. Only verified closures are returned.

## Output
Per task: `closed`, `verified`, `attempts`, `helperLaws` ({name, source}), `summary`; plus a
`verifiedClosed` count and a `winners` list ([{task, helperLaws}]) ready to save as `decomposed/`
entries.

## Do no harm
Keep a proposed lemma set only if the augmented task still closes — never let a committed lemma
regress a proof that worked without it.

## Safety
The conjecturer and runner are READ-ONLY on the project — all proof work happens on `/tmp` scratch
copies. The ONLY sanctioned project-file write is the verify gate persisting a re-confirmed win to
`decomposed/`. The loop never runs state-changing `git` commands and never modifies your source.
