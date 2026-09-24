# Core Lean (no Mathlib) against the Dafny-only laws

Can core Lean 4.34 (grind, omega, simp, split, induction, core library lemmas) plus a small lemma kit for Aver's own prelude close the laws that only Dafny closed in `experiments/dafny-parity`, and the bit-mask laws neither backend closed? Measured on GitHub Actions on 2026-09-24 with Aver `main` e00dd27c, Lean 4.34.0, btc-listener c5541f8.

## How

`.github/workflows/experiment-core-lean.yml` builds `aver` from `main` and exports four entries (K5 `LawsEntry`, btc `laws.av`, the btc bit-mask probe, btc `interp.av`). `strip_proofs.py` keeps every exported theorem's statement, replaces its proof with `sorry` and drops every `example`, and the model is built with `lake build`. `run_attempts.py` then reads each law's statement from the export (for a bounded-domain law it drops the sampled-domain premises, so the statement proved is the universal `∀ givens, when = true -> claim`), writes one standalone file per law (`laws/*.lean`: import the module, the kit, then the proof variants) and compiles it. A variant counts only with no error and axioms within propext / Classical.choice / Quot.sound, so no `sorry` and no `native_decide`. `core_*` variants may not cite the kit.

Runs: models and main's own verdicts https://github.com/jasisz/aver/actions/runs/36029013997; final attempts (reusing those models via `models_run`) https://github.com/jasisz/aver/actions/runs/36033642251; intermediate https://github.com/jasisz/aver/actions/runs/36032406989.

## Results: 18 of 18 closed

| law | parity run | closed by | how |
|---|---|---|---|
| K5 `reciprocalBound.lemma_8_1_1` | Dafny only | core | the exporter's own fraction-order chain with the subject name fixed (see below); also kit `cross_lt_trans` |
| K5 `oppositeSignSumBounded.guarded` | Dafny only | core | `Int.mul_pos`, `Int.mul_pos_of_neg_of_neg` as hypotheses, then `grind`; also kit `mul_sign` + `grind` |
| K5 `signsDifferent_8_2_2.branchNonpositive` | Dafny only | core | `Int.mul_nonpos_of_nonpos_of_nonneg` as a hypothesis, then `grind`; also kit `mul_sign` + `grind` |
| K5 `squareLeSumOfSquares.amgmSquareBound` | Dafny only | core | sign split + `Int.mul_nonneg` / `Int.mul_nonneg_of_nonpos_of_nonpos`, `omega`; also kit `sq_nonneg` |
| btc `Segment.place.staysUnderCap` | Dafny only | core | `grind [place, rolls, rolled, appended, headerBytes, capBytes]` |
| btc `Watchdog.unheard.aPoolWithSomebodyKeepsTheRule` | Dafny only | core | `grind [unheard, unheardFor]` |
| btc `ScriptParse.parse.directPushRunsPastTheEnd` | Dafny only | core | `simp` through `parse/from'/nextOp/pushOf/taken` with `isPushData n` and `pushWidth n = 0` (by `split <;> omega`) |
| btc `StackItem.isMinimalPush.directPushIsMinimalUnlessSmallNumber` | Dafny only | core | `simp` unfolding the cone, then `grind` |
| probe `bip341Set.isValidHashType` | Dafny only | core | `unfold; split <;> simp_all <;> omega` |
| probe `lowFive.isModThirtyTwo` | neither | core | core `Nat.and_two_pow_sub_one_eq_mod` + `omega`; also kit |
| probe `lowFiveBase.agreesWithBaseOf` | neither | core | the same, then `Int.emod_emod` |
| probe `anyoneCanPayMask.agreesWithIsAnyoneCanPay` | neither | kit | `and_of_neg`/`and_of_nonneg`, `nat_land_bit` |
| probe `mantissaNegative.isBitTwentyThree` | neither | kit | same |
| probe `csvDisabled.isBitThirtyOne` | neither | kit | same |
| btc `Chainwork.negative.isTheMantissaTopBit` | neither | kit | same |
| probe `csvCompared.isTypeBitAndCount` (0x0040FFFF) | neither | kit | `nat_land_split` at bit 16, then `nat_land_bit`, `nat_land_low` |
| btc `Chainwork.ofBits.neverNegative` | neither | core | two induction helpers (`powerOf ≥ 0`, `doubled ≥ 0`), `Int.ediv_nonneg` |
| K5 `signsDifferent_8_2_2.branchPositive` (extra) | neither | core | `Int.mul_nonpos_of_nonneg_of_nonpos` as a hypothesis, then `grind` |

Main itself still leaves all of these open: the K5, btc laws and probe exports each fail `lake build` (`build_errors: 1`, so nothing is audited), and in the interp export both btc laws there stay `bounded`.

## Kit

`Kit.lean`, 137 lines (121 non-blank, non-comment), 9 lemmas, all proved in core with axioms within propext / Classical.choice / Quot.sound. Only the five bit-mask lemmas are needed; the nonlinear ones are conveniences, since each nonlinear law also closes by citing a core `Int.mul_*` lemma.

## Verdict

Yes. Core Lean closes all nine laws only Dafny closed, with no kit at all: four need the right core `Int.mul_*` sign lemma cited as a hypothesis before `grind`/`omega` (8.1.1 needs only the exporter's existing chain with a name fixed), five need a better tactic choice (`grind` over the unfolded cone, `split <;> omega`). The five single-bit and composite mask laws that neither backend closes need the five bit-mask kit lemmas.
