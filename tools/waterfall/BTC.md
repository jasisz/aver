# Waterfall on btc-listener

Measured on 2026-09-17, continuing the [initial experiment](README.md).
The source is the project's `feat/follow-work-wait` snapshot
`ba4303a5a4c247f39b5257f0ca43d694340386d6`, which contains the Script proof
corpus. The separate, older local btc-listener checkout did not contain it.
Source functions and source laws were not edited.

Aver remains `1ef21e7e38192134bce899d4c87a3d66b968258c`, waterfall remains
`e04ea93b678c831c404067dd86573b821bc528e4`, and all checks use Lean 4.34.0.
The freshly built Aver binary is the same one as in the initial experiment.

## Findings

1. **The interpreter baseline passes:** 117 universal laws, 3 bounded laws,
   0 open laws, 0 build errors, 134 explicitly declined claims. Waterfall is
   being compared with a working, already substantial proof portfolio.
2. **One of the three bounded laws gets a symbolic proof:**
   `Domain.StackItem.isMinimalPush.directPushIsMinimalUnlessSmallNumber`
   succeeds with `.committed`. Its `0 <= b <= 255` source guard remains.
   The existing samples already enumerate all 256 bytes, so this replaces
   enumeration with a universal-form proof; it does not extend the byte domain.
   Default search fails at the tested budget.
3. **Five Segment claims admit universal proofs:** `place.staysUnderCap`,
   `place.recordEndsAtUsed`, `place.consecutiveRecordsTile`,
   `agreesWithDisk.anyOtherSizeRefuses`, and `place.agreesWithTheReader`.
   Their source guards remain, but record fields, lengths, and deltas are no
   longer restricted to the emitted sample lists.
4. **Citations matter on the real parser:** with the already audited local
   `prependReversed.reverseOnto` lemma named by the source's `using`, waterfall
   proves `intoBytes.accumulates`, its `because1` obligation, and its implication.
   Without that lemma, the main claim and reason fail. The implication alone
   succeeding is not credited as a completed guided law.
5. **Six retained proofs check without waterfall:** five Segment claims and the
   minimal-push claim. Both stored modules also pass `leanchecker --fresh`.

### The Segment baseline needs a qualification

The complete Segment export **fails**, with one default-200,000-heartbeat
timeout in `headerFor.readsBack.implication` (`Segment.lean:944` in this export).
The speculative probe also fails and retains bounded statements. Consequently
its whole-module manifest has no audited law results. The five successes above
are independently checked stronger propositions from that emitted export, **not
five measured per-law regressions recovered from a passing baseline**.

Waterfall does not resolve `headerFor.readsBack` or its implication in 30 seconds,
even when supplied with the exact source citation
`Domain.Message.littleEndian_law_fourBytesReadBack`. It also does not prove
`nameOf.sortsWithSegment`. The full Segment module is therefore still unproved.

The retained Segment scripts use normalization and `grind`, without induction.
This points to a useful improvement in proof discovery/portfolio routing; it
does not demonstrate new inference power over the tactics already in Lean.

## Measurements

Each search has effort 1,000, `maxHeartbeats` 1,000,000, one waterfall worker,
and a 30-second process timeout. These are **not** the baseline's default
heartbeat settings, so the counts are a feasibility study, not an equal-budget
benchmark. Some processes ran concurrently and timings include definition
elaboration and Lean startup.

The initial `.search` pass covers **25 distinct propositions**: 18 law statements
and 7 explanation/implication obligations. It accepts 15. Guided-law roots are
reported separately from the obligations needed to justify their explanations.

| Initial search selection | Accepted / attempted | Notes |
| --- | ---: | --- |
| Segment | 6 / 9 | Five generalized claims plus the existing disk-size identity; header root and implication fail |
| Message | 3 / 3 | Accumulator prefix, fixed width, and reader's final digit |
| ScriptParse | 2 / 5 | Reverse-onto and serializer implication; the serializer reason remains open |
| StackItem | 4 / 7 | Two final claims and two implications; their two reasons remain open; minimal push fails |
| ScriptState | 0 / 1 | Universal-form rearrangement-depth claim times out |

Follow-ups are separate measurements:

- Serializer with its source-selected local lemma: **3 / 3**, including the reason.
- Segment header with its source-selected imported lemma: **0 / 2**.
- `.committed` on each of the three bounded interpreter claims: **1 / 3**.
  Minimal push succeeds; direct-push truncation and rearrangement depth do not.
- `waterfall?` reproduces the six selected discoveries for script extraction.

Full statements, removed sample-membership premises, baseline entries, axiom
lists, and individual outcomes are in [btc-results.json](btc-results.json).
Evidence logs are in [btc-evidence](btc-evidence/). All accepted roots use only
`propext`, `Classical.choice`, and `Quot.sound`. No sampled/native-evaluation
axiom receives universal credit.

## Isolation and statement handling

`btc_trials.py` retains the export's unchanged definition prefix and its imports,
but removes every local law and sample theorem. It never imports the target
module's baseline `.olean`. Imported dependencies remain available, including
their audited laws; this differs from an empty-library benchmark.

For `bounded-domain` targets it removes only the leading, compiler-generated
sample-membership premises, one per quantified variable. The original source
guards and conclusion are unchanged. For a partitioned theorem it requires
all partitions to yield the same proposition after that removal. Both the
original statement and the stronger candidate are retained in the report.

Local helper use is explicit with `--local-lemma`: the baseline must mark the
helper universal with allowed axioms; its unchanged proof is copied into the
trial. The helper cannot be the target. The final root's axiom audit is transitive.
The serializer uses precisely the `prependReversed.reverseOnto` citation already
present in its Aver source. Other guided examples are not credited just because
waterfall can bypass the explanation and prove their final equality directly.

## Reproduce

Use a clean btc-listener checkout at the source SHA above, represented here by
`/private/tmp/btc-waterfall-source`. Build Aver and waterfall as in the initial
README. Then, from the btc-listener snapshot:

```sh
rtk /private/tmp/aver-waterfall-target/debug/aver proof domain/interp.av \
  --module-root . --check --check-json --declined-budget 134 \
  -o /private/tmp/waterfall-btc-interp
rtk /private/tmp/aver-waterfall-target/debug/aver proof domain/segment.av \
  --module-root . --check --check-json --declined-budget 134 \
  -o /private/tmp/waterfall-btc-segment
```

The second command is expected to fail as documented above; its imported
dependencies build and can be used by isolated trials. From the Aver worktree:

```sh
rtk proxy python3 tools/waterfall/btc_trials.py \
  --export /private/tmp/waterfall-btc-interp \
  --source /private/tmp/btc-waterfall-source \
  --module Domain.StackItem \
  --waterfall /private/tmp/aver-waterfall-upstream \
  --out /private/tmp/btc-minimal-new \
  --law Domain.StackItem.isMinimalPush.directPushIsMinimalUnlessSmallNumber \
  --mode committed --suggest

rtk proxy python3 tools/waterfall/btc_trials.py \
  --export /private/tmp/waterfall-btc-interp \
  --source /private/tmp/btc-waterfall-source \
  --module Domain.ScriptParse \
  --waterfall /private/tmp/aver-waterfall-upstream \
  --out /private/tmp/btc-serializer-new \
  --law Domain.ScriptParse.intoBytes.accumulates \
  --law Domain.ScriptParse.intoBytes.accumulates.because1 \
  --law Domain.ScriptParse.intoBytes.accumulates.implication \
  --local-lemma prependReversed_law_reverseOnto
```

Replay the retained scripts with **no waterfall dependency or search path**:

```sh
rtk proxy python3 tools/waterfall/btc_replay.py \
  --export /private/tmp/waterfall-btc-segment --module Segment \
  --snippet tools/waterfall/btc_segment_replay.lean.inc
rtk proxy python3 tools/waterfall/btc_replay.py \
  --export /private/tmp/waterfall-btc-interp --module Domain.StackItem \
  --snippet tools/waterfall/btc_minimal_replay.lean.inc
```

Both replay commands compare the retained statements against the exported guarded
claims, audit each root, and run a separate fresh kernel check. They do not
modify the original module, the source project, its proof gate, or its manifest.

## Recommendation

Keep waterfall as an optional, bounded discovery/fallback step and give it the
source's admitted citations. It can find short reusable scripts and remove some
reliance on enumeration. It is not a drop-in replacement for Aver's existing
automation, and success on an implication must never hide a failed `because`.
These are source-model proofs; no result here certifies Bitcoin consensus,
provider cryptography, or the compiled artifact bytes.
