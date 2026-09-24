# Dafny / Lean per-law parity

Where does the Dafny backend stand against Lean on every law we have, which laws does Z3 close that Lean does not, and is the Dafny backend worth what it costs? Measured on GitHub Actions on 2026-09-24, Aver at `origin/main` 79e5bf62 (plus these experiment files), Lean 4.34.0 (the toolchain the exports pin), Dafny 4.11.0.

## How it was measured

`.github/workflows/experiment-dafny-parity.yml` builds `aver` once, then runs one job per target and backend.

- Lean: `aver proof <entry> --check-json` with unlimited sorry and declined budgets, then the per-law `proof_manifest.json` (the ratchet's tiers). If the Lean build fails hard, no law is audited at all, so `strip_laws.py` removes the law that owns the error and checks again (the "isolation pass"). The removed law keeps its first-pass verdict.
- Dafny: `aver proof <entry> --backend dafny` (export only), then `dafny verify --verify-included-files` with the same 30 s limit `aver proof --check` uses, plus a per-implementation log. A law counts as Dafny-universal only if its lemma and all of its step lemmas verified, it has a verification outcome, no `assume {:axiom}` is in its cone, and no lemma or function it depends on (transitively) failed. That last rule matters: Dafny verifies each lemma modularly, so a lemma can pass while the lemma it cites fails. 44 laws whose own lemmas Dafny reported as passing are not credited because of it.
- `analyze.py` turns the raw outputs into `parity.json` / `parity.md` (artifact `parity-report`, and the job summary). The tables below come from the final run's `results-*` artifacts, run through the committed `analyze.py`.

Targets: `projects/k5_fdiv` through `entries/k5_lawsentry.av` (a leaf module that depends on all 23 law-carrying modules), `payment_ops` and `workflow_engine` (their one law module, `infra/codec.av`), `durable_promise` (`domain/promise.av`), and btc-listener. For btc-listener, `domain/interp.av` and `domain/laws.av` at `main` (c5541f8), at PR #361's head (5698c8e, fork jasisz/btc-listener), and at the `proof-entry-laws` branch (c63816b, main plus the commit that adds `laws.av`). `laws.av` exists only on that branch, so it is copied onto `main` and the PR head. There is also a probe module, `entries/btc_parityprobe.av`, which states the bit-mask laws issues #352/#353/#356 ask for (they are not in the repository yet). Robin's pins differ from ours (main pins 600b3551, the PR head 77ff2e66). Every btc entry type-checks and exports with our main, with no source changes except the one below.

Runs:
- final measurement: https://github.com/jasisz/aver/actions/runs/36008231040
- the same corpus, earlier iteration (identical per-law results): https://github.com/jasisz/aver/actions/runs/36002661502
- CI cost (Proof lanes with and without Dafny): https://github.com/jasisz/aver/actions/runs/36002670569
- forall-citation what-if on K5 (`k5cite`): https://github.com/jasisz/aver/actions/runs/36005390749
- 120 s time-limit what-if (`k5slow`, `btcslow`): https://github.com/jasisz/aver/actions/runs/36011387143

## Results

| target | laws | Lean universal | Dafny universal | both | Lean only | Dafny only | neither |
|---|---:|---:|---:|---:|---:|---:|---:|
| k5_fdiv | 175 | 158 | 106 | 102 | 56 | 4 | 13 |
| btc main, interp.av | 122 | 117 | 80 | 78 | 39 | 2 | 3 |
| btc main, laws.av | 65 | 56 | 47 | 44 | 12 | 3 | 6 |
| btc bit-mask probe (+ its cone) | 55 | 45 | 35 | 33 | 12 | 2 | 8 |
| btc proof-entry-laws, laws.av | 62 | 56 | 47 | 44 | 12 | 3 | 3 |
| btc PR #361 head, interp.av | 122 | 117 | 80 | 78 | 39 | 2 | 3 |
| btc PR #361 head, laws.av | 65 | 56 | 47 | 44 | 12 | 3 | 6 |
| durable_promise | 8 | 8 | 8 | 8 | 0 | 0 | 0 |
| payment_ops | 1 | 0 | 0 | 0 | 0 | 0 | 1 |
| workflow_engine | 1 | 0 | 0 | 0 | 0 | 0 | 1 |
| **corpus at main, unique laws** | **330** | **294** | **208** | **199** | **95** | **9** | **27** |

"Universal" means every input, with nothing trusted. Lean tiers come from the manifest. Bounded (native_decide over the given domain), sorry, declined and failed laws are all "not universal". The btc rows overlap (interp and laws both reach ScriptParse), so the last row counts each law once. It covers K5, btc main (interp, laws and probe) and the three small projects. main and PR #361 give identical per-law verdicts. `proof-entry-laws` differs only because the #349 reshape removed `Segment.headerFor.readsBack` and needed one rename (below).

Per backend: Lean closes 294/330 (89%) and Dafny 208/330 (63%). Dafny-only is 9 laws (2.7%).

### A Lean finding on the way

In 3 of the 5 entries, one law's theorem ended in a hard error instead of a `sorry`: a failed `rewrite` in K5 `Table.seedBelow.intervalErrorBound`, a heartbeat timeout in btc `Segment.headerFor.readsBack`, and `unsolved goals` in probe `bip341Set`. Each one failed `lake build`, so the ratchet audited nothing and the speculative-universal probe kept every candidate bounded. On K5 that one law hides 48 universal laws: 110 without the isolation pass, 158 with it. The Lean emitter should fail closed per law (a `sorry` or a bounded fallback), not per build. Otherwise one bad law makes the Lean numbers look worse than Dafny's.

### Dafny-only: Z3 closes, Lean does not (9)

| law | Lean | why Lean misses it | Lean-side fix |
|---|---|---|---|
| `Recip.reciprocalBound.lemma_8_1_1` (K5 8.1.1) | sorry | nonlinear rational inequality chain | none in core; Mathlib `nlinarith` (behind `--allow-mathlib`) is untested |
| `Remainder.oppositeSignSumBounded.guarded` | sorry | nonlinear sign reasoning | same |
| `Remainder.signsDifferent_8_2_2.branchNonpositive` | sorry | nonlinear sign reasoning | same |
| `FloorLaws.squareLeSumOfSquares.amgmSquareBound` | bounded | AM-GM square bound, nonlinear | same |
| btc `Segment.place.staysUnderCap` | bounded | linear arithmetic over record fields, universal strategy not found | strategy gap (omega-shaped) |
| btc `Watchdog.unheard.aPoolWithSomebodyKeepsTheRule` | bounded | Option equality over a guarded Int | strategy gap |
| btc `ScriptParse.parse.directPushRunsPastTheEnd` | bounded | `"{n}"` interpolation in the claim | strategy gap |
| btc `StackItem.isMinimalPush.directPushIsMinimalUnlessSmallNumber` | bounded | case analysis | strategy gap |
| probe `bip341Set.isValidHashType` | hard error | `Int` literal `match` versus a two-range predicate; the tactic falls through to `unsolved goals` and **breaks the whole build** | `split <;> omega` |

Four of the nine are nonlinear arithmetic in K5, where Z3's nonlinear solver has no counterpart in core Lean. The other five are Lean strategy gaps that Lean's own decision procedures cover.

### Neither backend (27), classified

| law | Lean | Dafny | class | note |
|---|---|---|---|---|
| probe `lowFive.isModThirtyTwo` | sorry | postcondition | **cheap, both** | `whatif/bits_bridge.dfy` closes it with one bridge lemma; `whatif/BitsBridge.lean` closes it with core `Nat.and_two_pow_sub_one_eq_mod` + `omega` |
| probe `lowFiveBase.agreesWithBaseOf` | sorry | postcondition | **cheap, both** | the same lemma, cited once |
| probe `anyoneCanPayMask.agreesWithIsAnyoneCanPay` | sorry | postcondition | **cheap, both** | single-bit bridge (Dafny what-if verified; Lean core has `Nat.testBit_eq_decide_div_mod_eq`) |
| probe `mantissaNegative.isBitTwentyThree` (#356 sign bit) | sorry | postcondition | **cheap, both** | single-bit bridge |
| probe `csvDisabled.isBitThirtyOne` (#352) | sorry | postcondition | **cheap, both** | single-bit bridge |
| btc `Chainwork.negative.isTheMantissaTopBit` (already in the repo) | sorry | postcondition | **cheap, both** | single-bit bridge, verified in the what-if |
| probe `csvCompared.isTypeBitAndCount` (#352 mask 0x0040FFFF) | sorry | postcondition | cheap-to-moderate | needs one more lemma: AND with a sum of disjoint masks splits |
| btc `Chainwork.ofBits.neverNegative` | sorry | postcondition | cheap, both | needs `powerOf(b, e, acc) >= 0`, one inductive helper law |
| btc `ScriptState.rearranged.staysWithinDeclaredDepth` | bounded | assertion | Dafny export gap + guidance | the `itemAt`/`hereOrDeeper` mutual recursion fails Dafny termination (below) |
| btc `Segment.headerFor.readsBack` | heartbeat timeout, **breaks the whole build** | declined: `?` outside the guided fragment | Dafny export gap; Lean timeout | Robin demoted it to cases in #349 for the Lean timeout |
| btc `Transaction.decode.refusesAFlagByteThatIsNotOne` | declined (fuel cone) | timeout at 30 s | Lean export gap; Dafny time | still times out at 120 s |
| btc `Transaction.decode.refusesAllEmptyWitnesses` | declined (fuel cone) | timeout at 30 s | same | still times out at 120 s |
| btc `Segment.nameOf.sortsWithSegment` | bounded | postcondition | hard | string order of zero-padded decimals against Int order |
| K5 `Round.truncErrorBound.strictBound` | bounded | timeout | turbo-hard | the squared-denominator rational order through the pow2 homomorphism (the documented K5 wall) |
| K5 `Round.awayErrorBound.strictBound` | bounded | timeout | turbo-hard | same |
| K5 `Round.stickyErrorBound.strictBound` | bounded | postcondition | turbo-hard | same |
| K5 `Round.truncErrorSameSign.signCondition` | bounded | timeout | turbo-hard | same family |
| K5 `FracRound.truncFracErrorBound.strictBound` | bounded | postcondition | turbo-hard | same |
| K5 `FracRound.awayFracErrorBound.strictBound` | bounded | postcondition | turbo-hard | same |
| K5 `Kernel.divide.theorem_2` | sorry | timeout | hard | rests on the open §8/§9 lemmas |
| K5 `Recip.nrRoundedStepError.oneStepBound` | bounded | timeout | nonlinear, Z3 time | the 120 s run had not finished after 75 min |
| K5 `Remainder.reciprocalMagnitudeBound_8_2_4.reciprocalMagnitudeComposition` | sorry | timeout | nonlinear, Z3 time | the 120 s run had not finished after 75 min |
| K5 `Remainder.productMagnitudeBound.guarded` | sorry | timeout | nonlinear, Z3 time | the 120 s run had not finished after 75 min |
| K5 `Remainder.signsDifferent_8_2_2.branchPositive` | sorry | postcondition | nonlinear | its sibling `branchNonpositive` is Dafny-only |
| K5 `Table.seedBelow.intervalErrorBound` | rewrite error, **breaks the whole build** | timeout | nonlinear, Z3 time | the 120 s run had not finished after 75 min |
| K5 `Table.bucketErrorOk.bucketMonotonicity` | bounded | postcondition | export gap, both | `countEpsilonOkFrom` is outside the proof subset |
| `Codec.unescapeField.escapedRoundtrip` (payment_ops, workflow_engine) | sorry | postcondition | turbo-hard | an escape/unescape round trip over String replace |

Robin's bit-mask laws are not a Dafny advantage. Both backends fail them today for the same reason: neither prelude relates `Bits.and` with a literal mask to `Int.div`/`Int.mod`. One small bridge per backend closes them, and Lean's is cheaper because core already has the Nat lemma.

`Segment.headerFor.readsBack` is neither: Lean runs out of heartbeats, and Dafny declines `?` in guided laws.

The laws that end in `sorry` on Lean (16 unique) split into: seven bit-mask laws (six cheap on both, plus `csvCompared`), `Chainwork.ofBits.neverNegative` (cheap on both), three K5 nonlinear laws that Dafny closes (Dafny-only above), three K5 nonlinear laws plus `divide.theorem_2` that neither closes, and the codec round trip.

### Lean-only (95), where Dafny loses them

- K5 (56): 18 time out at 30 s, 13 fail on their own, and 25 are blocked by a failed supplier. Most of the supplier blocks go back to five `pow2Signed*` lemmas and to `fracExpoFromMagnitude`. The `pow2Signed*` lemmas fail because the exporter cites `pow2.positive` through a `forall` that Z3 does not instantiate (below).
- btc interp (39: 19 supplier-blocked, 16 own failures, 4 timeouts): StackItem (23; `placed.placementReadsBack` times out and `withSign.preservesLowByte` fails a step, which blocks the byte-range family), ScriptParse guided laws (12; the `bytesOf.prepend` implication and the `parsedBytesMatch.*` steps fail, and reverse/concat reasoning over sequences is the likely cause), and ScriptState (4; the `itemAt`/`hereOrDeeper` mutual recursion fails Dafny's termination check).
- In addition, 9 of the 39 generated modules of the Script-engine cone make Dafny run out of memory at a 12 GB cap, in its front end (`System.OutOfMemoryException` in `Node.Visit`, before Z3). None of those 9 modules carries a law, so no verdict is lost. Verifying the whole cone in one `dafny verify` got the runner killed twice, so the interp targets run one module at a time.

## Top cheap wins on the Dafny backend, by laws gained per effort

1. **Cite at terms, not through `forall`** (measured). The exporter emits `forall k: int ensures (pow2(k) >= 1) == true { pow2_positive(k); }`, which Z3 does not instantiate at `pow2(0 - k)` once `pow2Signed` unfolds under `{:fuel pow2, 5}`. Rewriting 9 such lines as direct calls (`whatif/patch_forall_citations.py`) took K5 Dafny from 106 to **113** universal: 3 laws fixed directly and 4 unblocked through the supplier cone. Doing it in codegen (instantiate at the argument terms of the unfolded claim) also reaches the record-parameter lemmas the text patch skips.
2. **Escape the generated `fuel` parameter** (measured). A user parameter named `fuel` collides with the fuel lowering's own `fuel`: `Duplicate parameter name: fuel`, and Dafny rejects the whole file. On btc `proof-entry-laws` that was 0/62 → **47/62** once the parameter was renamed. The fix is a one-line name escape.
3. **Bit-mask bridge lemmas** (measured as a what-if): `Bits.and(a, 2^k - 1) == a mod 2^k` and `Bits.and(a, 2^k) == 2^k ⇔ (a div 2^k) mod 2 == 1` for every Int `a`. They are proved by induction in `whatif/bits_bridge.dfy` (33 verified, 0 errors) and close **6 laws**: 5 from the probe plus `Chainwork.negative.isTheMantissaTopBit`. The exporter also has to state the literal power (`BitsPow2(23) == 8388608`), because Z3 will not unfold it 23 times. The Lean analogue (`whatif/BitsBridge.lean`, core only) closes `lowFive` over every Int, so this is a win on **both** backends, not a Dafny advantage.
4. **Mutual-recursion measure for `itemAt`/`hereOrDeeper`**: 1 termination failure blocks **5** btc ScriptState laws (4 Lean-only, 1 neither). The measure needs `|items|` with a rank for the `hereOrDeeper` hop. Not measured.
5. **pow2 addition lemma** (`pow2(m + n) == pow2(m) * pow2(n)`, one induction): `pow2SignedHomomorphism` still fails after (1) and blocks about 7 K5 Kernel/TruncScale laws through the supplier cone. Estimated, not measured.
6. **A longer time limit does not help.** At 120 s instead of 30 s, btc laws.av gives exactly the same per-law verdicts (the same 3 timeouts, 518 verified / 21 errors). The K5 run at 120 s had not finished after 75 minutes, against 9 minutes at 30 s. Timeouts need guidance, not time.
7. **Sequence reverse/concat step lemmas** for the ScriptParse guided laws: about 5 root failures, 12 laws. Probably cheap, not measured.
8. **`?` in guided laws** (Result propagation): unlocks `Segment.headerFor.readsBack`. Moderate.
9. **AND with disjoint masks splits**: one more bridge lemma for `csvCompared` (#352's 0x0040FFFF). Small.
10. **Front-end memory**: lemmas carry 60+ `{:fuel f, 5}` attributes each, and 9 btc modules exhaust 12 GB in Dafny's front end. Cutting fuel to the functions a lemma actually needs is the first thing to try. It costs no law today, but it blocks the whole-cone check.

Items 1–5 are small codegen changes. Most of the laws they recover (1, 2, 4, 5) are already Lean-universal, so they add a second witness, not new coverage. Only (3) adds coverage, and it adds it on Lean as well.

## What Dafny costs today

| item | size |
|---|---|
| `src/codegen/dafny/` | 13,467 lines of Rust (Lean backend: 56,483; all codegen: 193,429) |
| outside it | 50 other `src` files mention Dafny (97 lines in `src/main/commands.rs` alone), plus `src/main/proof_explain/dafny.rs` (235) |
| tests | 157 test functions with `dafny` in the name; `tests/proof_spec/dafny_inline.rs` + `dafny_guidance.rs` (724 lines); Dafny fixtures 2,166 lines; `tools/dafny_guidance_spike.py` (327) |
| docs | `docs/dafny.md` (237) + `docs/dafny-guidance-spike.md` (480); 14 docs mention Dafny |
| CI | Proof lanes, same commit, with Dafny installed vs not: 2,113 s vs 1,582 s of runner time per Proof run (**+531 s, +34%**); slowest shard 661 s vs 457 s (+3.4 min wall). Proof runs on every PR, every main push and nightly |
| churn | since 2026-06-24: 66 of 802 commits touched `src/codegen/dafny` (+8,512 / −1,180 lines); 49 commit subjects mention Dafny |
| bugs found by this run | duplicate `fuel` parameter (whole file rejected); `forall` citations Z3 does not instantiate; front-end out-of-memory on the Script-engine cone; mutual termination measure (`itemAt`) |

## What Dafny gives us

- **Coverage**: 9 laws (2.7%) that Lean does not close. Four are nonlinear arithmetic, and one of them is the K5 8.1.1 capstone, which the comment on `reciprocalBound` in `recip.av` already credits to Z3. Five are Lean strategy gaps that `omega`/`split` should close.
- **A second witness** on 199 laws both close. Its independence is limited. Both exports share the same front end, ProofIR, proof search and citation plans, so a lowering bug in the shared part passes both. What is independent is the prover, the prelude definitions and the final translation step.
- **Counterexample models** for `--explain`. Not wired up (the "Lodówka G2" item in memory); no current user.

## Should Dafny-only laws count in the proof gate or the certificate?

- **Certificate**: no. The certificate's claim is a Lean kernel check over the exact artifact bytes (`leanchecker --fresh`, axiom audit). A Z3 verdict has no proof object, the Aver→Dafny translation and the Dafny prelude are unverified, `{:axiom}` functions and fuel encodings are trusted, and the result depends on the Z3/Dafny versions and on time limits (#342 was a platform drift of this kind). Putting a Dafny-only law in the certificate would lower its whole trust floor to "Dafny + Boogie + Z3 + our exporter".
- **Proof gate / manifest**: acceptable as a separately named tier, for example `smt`, ranked above `bounded` and below `universal`, and never added to the kernel-universal count. Conditions: it is pinned to a Dafny version, strict per law (the lemma, its steps, and every supplier and function in its cone verified; no `assume {:axiom}`), and ratcheted like the others. The analyzer here implements exactly that rule, and 44 laws whose own lemmas Dafny accepted did not qualify under it.

## Verdict

**Keep Dafny minimal; do not invest.** Freeze feature work on the Dafny backend, and move its Proof arms from per-PR to nightly, which saves about 531 runner-seconds per PR. Apply the two one-line fixes (the `fuel` escape, citation at terms) only if the backend stays. Remove it once Lean covers the four nonlinear K5 laws.

The numbers behind it: Dafny adds 9 laws out of 330 (2.7%). Five of those are Lean strategy gaps Lean can close itself, and four are nonlinear arithmetic. For those 13.5k lines of code, a third more Proof CI time and about 8% of recent commits, the only unique coverage is the nonlinear K5 lemmas. The cheap wins mostly re-prove laws Lean already has. Robin's bit-mask laws are closed more cheaply on Lean (core `Nat.and_two_pow_sub_one_eq_mod`, `Nat.testBit_eq_decide_div_mod_eq`, `omega`) than on Dafny.

**Remove** is the right call as soon as one measurement comes back: run the four Dafny-only K5 laws (8.1.1, `oppositeSignSumBounded`, `signsDifferent_8_2_2.branchNonpositive`, `amgmSquareBound`) with `aver proof --allow-mathlib` and see whether Mathlib's `nlinarith`/`positivity` close them. If they do, Dafny's unique coverage is zero. At that point the second-witness value alone (not independent at the front end) does not pay for the backend, and an independent external kernel (see the `experiments/external-kernels` branch) is the better place to buy independence.

**Invest** would only make sense if we decided to lean on SMT for nonlinear arithmetic as a product feature. Even then the investment would go into an SMT tier for Lean-shaped obligations (Z3 through a Lean tactic, or `grind`/`omega` extensions), not into a second full language backend.
