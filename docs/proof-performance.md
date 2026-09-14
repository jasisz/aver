# Measuring Lean proof performance

`tools/proof_bench.py` runs the full Lean proof CLI serially against three source
fixtures: a single-list conditional, K5 rounding, and the K5 kernel. Every run
uses a fresh output directory with no generated Lake cache. The installed Lean
toolchain is shared; this is not a toolchain download benchmark. The tool requires
Python 3.11+ and a POSIX host.

Freeze the two Aver binaries before rebuilding, then run from the same checkout:

```bash
python3 tools/proof_bench.py --aver /tmp/aver-before --out /tmp/proof-before
python3 tools/proof_bench.py --aver /tmp/aver-after --out /tmp/proof-after
```

The default is three repetitions per fixture. `--case k5-round --repeat 1` selects
a quick comparison. Output directories must be new. `--timeout` limits the whole
invocation and terminates its process group, retaining incomplete results.

Each run retains the generated project, Aver output, every Lake invocation's
arguments, elapsed seconds, stdout and stderr, the check summary, and the full
proof manifest. `report.json` includes compiler and tracked Aver source hashes
and rejects a run whose inputs changed while measuring. Inspect the generated
`lean-toolchain` and recorded `lake --version` output for the actual checker.
Compare medians **and the complete manifests**: a faster run with fewer universal
laws, missing obligations, or different axiom dependencies is not equivalent.
A zero benchmark exit status only means the requested checks completed; it does
not assert that every law is universal. K5 Kernel intentionally retains four
bounded laws.

## Reusing successful speculative proofs

The Lean exporter first tries eligible laws universally, then re-emits the
project with unsuccessful candidates restored to bounded statements. Previously,
even successful candidates changed text: their unreachable diagnostic fallback
`(trace "AVERSPEC_SORRY:…"; sorry)` became `sorry`. Lake consequently rebuilt
those modules and their dependents.

Successful candidates now retain the same fallback text. Lake can reuse a
module only when its source and dependencies remain unchanged. A demoted law
still rebuilds its module and dependents. The committed build, final build and
axiom audit all remain in place. The diagnostic runs only if proof search reaches
`sorry`; it does not establish a theorem or grant universal credit. The audit
continues to reject `sorryAx` and non-whitelisted axioms.

This saves repeated elaboration where candidates close. It does not accelerate
the initial search or eliminate rebuilds downstream of a demoted dependency.

## Compose explanations before splitting cases

The final implication of a law with `because` receives all earlier explanations
as hypotheses. The Lean backend first attempts to compose these facts with its
existing solver. If that attempt cannot close the goal, it runs the original
`fun_cases` strategy and solves the resulting branches.

This ordering avoids expanding a product of cases when the implication already
follows from the explanations. The earlier attempt contains no `sorry` fallback;
only the final reporting branch may record an open obligation. Statements,
individual explanation checks, citations and checker budgets are unchanged.
