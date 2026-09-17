# Waterfall on Aver exports

An isolated proof-search experiment, run on 2026-09-17. The original experiment
added no compiler option or production dependency.

The follow-up implementation is now available as
`aver proof --waterfall PATH --check`: see [the integration guide](../../docs/waterfall.md).
The experiments below retain their original source revisions and measurements.

The follow-up on the real btc-listener proof corpus is documented in [BTC.md](BTC.md).

## Finding

Waterfall is worth trying as an optional discovery/fallback tool for recursive
laws. It closes an accumulator theorem over Aver-exported trees that Aver's
current proof portfolio leaves open. Its generated script also checks without
waterfall, including a separate `leanchecker --fresh` replay.

It does not replace the existing portfolio: the default search misses several
integer-division/accumulator laws that Aver already proves. The committed policy
also misses some laws that the backtracking policy proves.

## Results

Counts below mean axiom-audited universal proofs, not successful samples.

| Case | Laws | Aver baseline | Waterfall search | Waterfall committed |
| --- | ---: | ---: | ---: | ---: |
| Existing list fold | 3 | 3 | 3 | 2 |
| Existing reverse algebra | 3 | 3 | 3 | 3 |
| Existing floor-division digit collector | 7 | 7 | 5 | 3 |
| Existing decimal roundtrip | 3 | 3 | 1 | 1 |
| New tree traversal | 2 | 0 | 1 | 1 |
| **True laws** | **18** | **16** | **13** | **10** |
| False accumulator controls | 2 | 0 | 0 | 0 |

The additional tree demonstration proves **both** tree laws when the second
can cite the first. This is separate from the independent-attempt matrix.
On this selected set, retaining baseline successes and adding waterfall yields
one additional proof without hints, or two with that explicit helper lemma.
All existing positive fixtures were already proven by Aver; the new coverage
demonstration is the advertised tree example translated through Aver.

Full outcomes, hashes, timings, and axiom lists are in [results.json](results.json).
[Evidence logs](evidence/) retain the tree baseline, suggestions, replay, and
negative controls. The fresh-checker log is empty on success; its exit code and
duration are recorded in `results.json` (`tree_demo.fresh`).

## Pinned inputs

- Aver base: `1ef21e7e38192134bce899d4c87a3d66b968258c`.
- [Waterfall](https://github.com/samth/waterfall) commit:
  `e04ea93b678c831c404067dd86573b821bc528e4`.
- Lean: `leanprover/lean4:v4.34.0` for both the Aver export and waterfall.
- Upstream pins 4.33.1, but its library and `Tutorial/Examples.lean` both checked
  on 4.34.0 with no source changes in this experiment. This is not a run of its
  entire test suite.
- Each isolated trial: one worker, effort 1,000, `maxHeartbeats` 1,000,000,
  30-second wall timeout. Both `.search` and `.committed` are measured.

## Method

1. Build Aver from the pinned tree and export each fixture with
   `aver proof --check --check-json`.
2. Read baseline verdicts and transitive axiom dependencies from
   `proof_manifest.json`. A completed `lake build` alone is not proof credit.
3. For each law, keep the exported definitions unchanged and copy the exact
   emitted theorem statement into a separate Lean file. Replace its proof with
   waterfall. Definitions remain in the same module, enabling automatic
   discovery. There are no hand-selected definition hints in the matrix.
4. Exclude ALL baseline law proofs and sampled theorems from that file. The
   original module is not imported, so library retrieval cannot rediscover
   Aver's proof of the target. Earlier helper laws are also excluded: these
   are independent proof attempts, not a complete integration with `using`.
5. Require a successful Lean exit and a `#print axioms` result contained in
   `{propext, Classical.choice, Quot.sound}`. Missing audit output, `sorryAx`,
   `Lean.ofReduceBool`, resource exhaustion, or timeout never count as success.

The runner rejects fixtures with `because`: proving only their final equation
would not establish their explanation obligations. Bounded statements are
reported as skipped instead of being counted as universal proofs.

This is a small, deliberately selected feasibility corpus, not a representative
benchmark. The tree case is a new Aver adaptation of waterfall's advertised
accumulator traversal example; the other five files are existing Aver fixtures.
Timing includes Lean startup and elaboration of definitions, and the machine
was shared with other work. The runs establish proof outcomes, not speedups.

## Search once, retain the proof

`tree.av` expresses the optimized and specification traversals in Aver.
The accumulator theorem quantifies over **every** accumulator. Waterfall closes
it without hints; both default and committed search succeed. The empty-list
specialization is not found independently within the matrix budget.

In a second experiment, `tree_search.lean.inc` supplies the already proved
accumulator theorem to the specialization. Both close, and `waterfall?` prints
ordinary Lean scripts. `tree_replay.lean.inc` contains those checked scripts,
with only the editor's `[apply]` decoration removed. It uses functional induction
over `elements` followed by simplification; the specialization uses the helper.

`tree_demo.py` checks that the snippets have the exact exported statements,
prepends the original emitted definitions, and audits both theorem roots.
Replay runs with no waterfall import or waterfall search path, then checks the
stored module with `leanchecker --fresh TreeReplay`. Both theorem roots depend
only on `propext` and `Quot.sound`. The generated script retains harmless unused
simp-argument warnings so the recorded suggestion remains recognizable.

## Reproduce

Run from this worktree. All shell commands use the repository's RTK wrapper.
The required Lean toolchain must already be installed.

```sh
rtk cargo build --locked --bin aver --target-dir /private/tmp/aver-waterfall-target
rtk git clone https://github.com/samth/waterfall.git /private/tmp/aver-waterfall-upstream
rtk git -C /private/tmp/aver-waterfall-upstream checkout e04ea93b678c831c404067dd86573b821bc528e4

rtk proxy python3 tools/waterfall/run.py \
  --aver /private/tmp/aver-waterfall-target/debug/aver \
  --waterfall /private/tmp/aver-waterfall-upstream \
  --out /private/tmp/waterfall-new-run

rtk proxy python3 tools/waterfall/tree_demo.py \
  --export /private/tmp/waterfall-new-run/tree/baseline \
  --waterfall /private/tmp/aver-waterfall-upstream
```

Use a new output directory for each run. `--case tree` selects the minimal
demonstration; repeated `--case` and `--mode` select subsets. Each run leaves
generated Lean files, baseline manifests, per-trial logs, and `report.json`.
To check only the retained tree proofs, omit `--waterfall` from `tree_demo.py`.

The original measurement was split into two nonoverlapping runs:
`/private/tmp/waterfall-pilot` (`list_fold`, `tree`) and
`/private/tmp/waterfall-corpus` (the other four cases).

## Integration recommendation

Keep the existing proof portfolio. The promising next experiment is a bounded,
opt-in search after it fails, passing only admissible helper laws and respecting
`using`/`because` obligations. A discovery command could retain the checked
replacement script, making subsequent checks independent of search.

Search budget, reproducibility, output-script caching, and axiom audits must be
explicit. This experiment does not establish artifact-byte certification or
concurrent-program correctness; it checks the exported source-level theorems.
