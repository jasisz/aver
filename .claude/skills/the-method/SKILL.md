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
One autonomous agent per task (in parallel). Each agent:
1. Reads the target task; if the project has example decompositions (e.g. a `decomposed/`
   directory of solved tasks), learns the exact splice form from one of them.
2. Proposes 1–3 true, general helper laws aimed at the open goal (not a restatement of it).
3. Splices them into a `/tmp` copy — **before** the target `verify ... law` (order matters; rendering
   matters too) — and runs `aver proof <scratch> --discover -o <dir>` then
   `aver proof <scratch> --check --check-json --backend lean -o <dir>`; success ⟺ `"universal":true`.
4. Refines on failure, up to `attempts` tries.

Then a **Verify** phase independently re-checks each claimed closure from scratch (a separate agent,
a fresh dir) — a self-reported closure is not trusted on its own. Only verified closures are
returned.

## Output
Per task: `closed`, `verified`, `attempts`, `helperLaws` ({name, source}), `summary`; plus a
`verifiedClosed` count and a `winners` list ([{task, helperLaws}]) ready to save as `decomposed/`
entries.

## Do no harm
Keep a proposed lemma set only if the augmented task still closes — never let a committed lemma
regress a proof that worked without it.

## Safety
READ-ONLY on the project. All edits happen on `/tmp` scratch copies. The loop never runs
state-changing `git` commands and never modifies project files.
