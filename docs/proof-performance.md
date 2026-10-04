# Measuring Lean proof performance

`tools/proof_bench.py` runs the full Lean proof CLI serially against three source
fixtures: a single-list conditional, K5 rounding, and the K5 kernel. Each run
gets a fresh output directory with no generated Lake cache. The installed Lean
toolchain is shared, so the benchmark does not measure toolchain downloads. The
tool needs Python 3.11+ and a POSIX host.

Freeze the two Aver binaries before rebuilding, then run both from the same
checkout:

```bash
python3 tools/proof_bench.py --aver /tmp/aver-before --out /tmp/proof-before
python3 tools/proof_bench.py --aver /tmp/aver-after --out /tmp/proof-after
```

By default each fixture runs three times. For a quick comparison, use
`--case k5-round --repeat 1`. Output directories must be new. `--timeout` caps
the whole invocation; when it fires, the tool kills the process group and keeps
the results collected so far.

Each run keeps the generated project, the Aver output, and for every Lake
invocation its arguments, elapsed seconds, stdout and stderr. It also keeps the
check summary and the full proof manifest. `report.json` records the compiler
hash and the hashes of the tracked Aver sources, and rejects a run whose inputs
changed during measurement. To see which checker actually ran, look at the
generated `lean-toolchain` and the recorded `lake --version` output.
Compare medians **and the complete manifests**. A faster run that has fewer
universal laws, missing obligations or different axiom dependencies is a
different result. A zero exit status from the benchmark
means the requested checks finished. It says nothing about whether every law is
universal. K5 Kernel keeps four bounded laws on purpose.

## One build for guarded laws

Up to 0.30 the Lean exporter ran an extra `lake build` before the real one: it
stated every speculative guarded law universally, learned which ones closed, and
re-emitted the rest over their sample domains only. On a large module that probe
cost as much as the check itself. The exporter now states every law for every
input and builds once. A guarded law whose speculative proof does not close is
declined by `--check` instead of being restated, so the probe has nothing left to
decide.

## Compose explanations before splitting cases

For a law with `because`, the final implication gets every earlier explanation
as a hypothesis. The Lean backend first tries to combine these facts with its
existing solver. If that fails to close the goal, it falls back to the original
`fun_cases` strategy and solves the resulting branches.

When the implication already follows from the explanations, this order avoids
expanding a product of cases. The first attempt has no `sorry` fallback. Only
the final reporting branch can record an open obligation. Statements, the checks
of individual explanations, citations and checker budgets stay the same.
