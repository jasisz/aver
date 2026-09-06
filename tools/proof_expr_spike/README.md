# Lean Expr proof-construction spike (#1293)

**Conclusion: narrow, not a backend replacement.** Direct Expr construction works for
the tested proof skeletons and can attach failures to Aver reason/branch/fact paths.
It neither proves more laws nor shows a meaningful speed advantage in this corpus.
Keep this as an isolated experiment; a production change would need evidence that
reusing such a bridge actually simplifies the current exporter.

## Reproduce

From this repository, with Rust and elan installed:

```sh
cargo build --locked
python3 tools/proof_expr_spike/run.py \
  --aver target/debug/aver --out /tmp/aver-expr-spike
```

The recorded run used compiler source at
`db6fb85b615cfc04c84cb67fb99b1c56624f8b59`, Aver 0.30.0-dev, and
`leanprover/lean4:v4.32.2` on arm64 macOS. The experiment adds no production
compiler changes. The helper crate has its own lockfile; the runner records the
repository revision, executable version, Lean version and resource budget.
Use `--target-dir` to reuse a Cargo build directory, or `--runs N` for more than
three repetitions. Running the experiment needs the usual Cargo/elan cache access.

The negative examples are intentional: the production proof command exits 1,
as does hostile verification. The runner accepts those exits only as part of
checking the expected results. A successful complete experiment exits 0.

## What is actually compared

1. **Current exporter reference:** compile the Aver corpus with `aver proof
   --check-json`; keep its definitions, original law statements, obligations and
   manifest.
2. **Controlled text lane:** render a small proof plan as Lean tactics.
3. **Expr lane:** construct introductions, case splits, List recursion, ordered
   Bool conjunction steps and final lambda/application composition through
   Lean's Meta APIs.

The two measured lanes use the same checked statement types, function cone,
structural plan and leaf search: `simp_all; done`, otherwise `omega`, otherwise
`grind`. Failed simplification rolls back before trying the next alternative.
Each stage has 200,000 heartbeats. Both lanes use the same Expr composition of
the completed obligations into the original law; this shared cost is included.
Thus the measured difference is the **stage skeleton**, not a claim to bypass all
Lean parsing. Leaf tactics are still parsed/elaborated in both lanes.

The Rust helper reads the actual Aver AST using the existing parser, checker and
expression walker. It discovers reachable definitions and recognizes a reason
whose body begins with a match on its first parameter. Direct recursive List
reasons select List induction; the remaining parameters stay quantified in the
induction hypothesis. Other shapes have only the plain leaf path. This is a
deliberately small subset, with explicit `using []`, not a new public proof IR.

The bridge imports the reference module **for definitions and statement types**.
It does not copy or cite its law proofs. A transitive dependency audit rejects
baseline law/reason declarations and other newly generated stage/law declarations.
Composition uses the freshly built proof values themselves.

## Results

| Corpus law | Current export | Controlled text | Expr |
| --- | --- | --- | --- |
| Positive chain, two ordered reasons under a guard | universal | universal | universal |
| Option case analysis | universal | universal | universal |
| List induction with changing integer accumulators | universal | universal | universal |
| False reason for a true identity law | rejected reason | rejected reason | rejected reason |
| Broken recursive reason, true original identity | rejected reason | rejected reason | rejected reason |

Declared verification samples pass: 56 executed cases, 24 skipped by `when`.
Hostile verification finds 33 failures in the two intentionally wrong reasons.
Both measured proof paths reject them independently of sampled execution.
All accepted proofs depend only on `propext`, `Classical.choice`, and `Quot.sound`.

Two boundary checks also pass: the kernel rejects a mismatched `True.intro`
proof value, and the dependency audit rejects citing the baseline theorem.
The recursion hypothesis comes from `List.rec`; it is not an axiom or an
assumption that the recursive reason succeeds.

Recorded medians over three repetitions, summing the three successful laws and
their obligations:

| Measurement | Controlled text | Expr |
| --- | ---: | ---: |
| Construct proof values | 81.35 ms | 82.45 ms |
| Synchronous kernel checking | 18.80 ms | 17.99 ms |
| Pretty-printed final terms, total | 8,422 bytes | 6,787 bytes |
| Raw Expr representation, total (median) | 1,237,571 bytes | 1,047,766 bytes |

These are tiny, local measurements, **not evidence of a performance improvement**.
They exclude parsing/exporting definitions, imports, dependency audits, artifact
printing and subprocess startup. Each lane runs in a fresh Lean process, with
alternating launch order. An earlier same-process attempt favored whichever lane
ran second during kernel checking; those contaminated timings are excluded.
The production exporter reference uses a different induction/search strategy, so
its end-to-end time is not compared with these in-process stage timings.

Representation byte counts are debugging-output sizes, not heap sizes or proof
complexity. Different tactic and Meta API bookkeeping produces slightly different
terms even for the same structural plan. Smaller here does not mean more readable.

## Diagnostics and artifacts

`plan.txt` describes the source steps; `plan.json` is their machine-readable form.
Each measured run writes `results.json`, pretty `.term.txt` and raw `.expr.txt`
proof views. The full original export and runtime verification logs are retained.
The compact recorded evidence is in [results](results/); rerun to generate full
raw terms instead of committing megabytes of machine output.

The negative induction reports:

```text
advance.brokenInduction.because1@57/branch2@72/fact2: grind failed
```

This locates the failing Aver obligation and constructor branch, then the second
conjunct. The remaining diagnostic is still Lean's goal/context, and the text
lane reports the obligation with its Lean tactic failure. Translating all leaf
diagnostics to Aver is **not implemented**. The pretty printer abbreviates some
proof internals; the raw Expr dump remains available when those details matter.

## Findings that constrain a follow-up

- Lean tactic error recovery can insert `sorry`. The bridge disables recovery
  and additionally checks the transitive axiom dependencies.
- `Lean.addDecl` schedules kernel checking asynchronously. Measuring its return
  alone did not measure validation. The bridge calls the synchronous kernel
  boundary and waits for its result, then registers the already checked theorem.
- Destructive simplification can erase a proved fact using an induction hypothesis
  before another tactic needs that fact. Expr does not solve this search problem.
  Shared rollback between leaf alternatives restores the needed context.
- Direct term construction removes proof-skeleton string assembly, while retaining
  Lean's elaboration/metaprogramming environment. It is not Rust sending untyped
  lambda expressions to a standalone kernel.
- Ordinary Aver reasons remain the user-facing proof description. Raw kernel terms
  are a debug artifact; they are not a nicer replacement for generated Lean source.

A sensible next experiment would replace one existing fragile skeleton renderer
with this style and count the code deleted, while retaining the source-plan view.
Do not introduce a second complete production backend based on these results.
