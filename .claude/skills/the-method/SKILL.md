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
   the target `verify ... law` (order + rendering matter) — runs a cheap `aver check` pre-flight
   (parse/typecheck; a malformed law is caught here and fed straight back), then
   `aver proof <scratch> --check --check-json --backend lean`, and returns **ONLY the Aver-level
   verdict** (`universal`/`sorries`, which helper failed its bounded check). The Lean residual never
   crosses back to the conjecturer. **No `--discover`**: The Method measures the LLM-proposed laws +
   our auto-prover, not the built-in enumerative recognizer — so a closure is self-contained (the
   inline laws alone), and the conjecturer must supply every law the proof needs (including bridges
   to builtins like `List.concat`/`List.len`/`List.reverse`).
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
