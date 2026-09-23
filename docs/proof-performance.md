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

## Reusing successful speculative proofs

The Lean exporter first tries eligible laws universally. It then re-emits the
project with the failed candidates put back as bounded statements. Until this
change, the successful candidates changed text too: their unreachable diagnostic
fallback `(trace "AVERSPEC_SORRY:…"; sorry)` was rewritten to `sorry`. Lake saw
new source and rebuilt those modules and everything depending on them.

Successful candidates now keep the same fallback text. Lake reuses a module only
when its source and dependencies are unchanged, so a demoted law still rebuilds
its module and dependents. The committed build, the final build and the axiom
audit all still run. The diagnostic fires only if proof search reaches `sorry`.
It proves nothing and gives no universal credit. The audit still rejects
`sorryAx` and any axiom outside the whitelist.

The saving is repeated elaboration for candidates that close. The initial search
is no faster, and modules downstream of a demoted dependency still rebuild.

## Compose explanations before splitting cases

For a law with `because`, the final implication gets every earlier explanation
as a hypothesis. The Lean backend first tries to combine these facts with its
existing solver. If that fails to close the goal, it falls back to the original
`fun_cases` strategy and solves the resulting branches.

When the implication already follows from the explanations, this order avoids
expanding a product of cases. The first attempt has no `sorry` fallback. Only
the final reporting branch can record an open obligation. Statements, the checks
of individual explanations, citations and checker budgets stay the same.
