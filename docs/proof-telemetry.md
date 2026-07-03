# Proof Telemetry Report

Date: 2026-07-03
Repo: `/private/tmp/lumen-rs-proof-telemetry-work`
Branch: `proof-telemetry-report`
Input commit: `466da4c3f0f6`
Tool runtime: 30.84s

## How To Run
Run from the repository root after building the local `aver` binary:

```bash
RUSTC_WRAPPER= cargo build --bin aver
python3 tools/proof_telemetry.py --aver-bin target/debug/aver --output /tmp/proof-telemetry.md
```

Add `--lean-tags-for <path>` to collect optional no-lake Lean class tags. The committed report uses `examples/formal` and `projects/k5_fdiv/domain/round.av`; full K5 Lean tag export is intentionally not part of the default fast path.

Fast smoke check:

```bash
python3 tools/proof_telemetry.py --no-current >/tmp/proof-telemetry-smoke.md
python3 -m py_compile tools/proof_telemetry.py
```

## Summary
| metric | value |
| --- | --- |
| HEAD proof_manifest.json files | 0 |
| manifest-history commits with files | 0 |
| manifest-history paths | 0 |
| manifest law records | 0 |
| manifest records with strategy field | 0 |
| current source files requested | 35 |
| current source files with law rows | 26 |
| current distinct law strategy rows | 158 |
| current concrete strategy pins | 94 |
| current BackendDispatch pins | 64 |
| ProofStrategy variants total | 31 |
| ProofStrategy variants fired | 15 |
| ProofStrategy variants never fired | 16 |
| current Lean class tags collected | 80 |
| hand/manual proof sidecars | 4 |

## Data Recoverability
No committed `proof_manifest.json` files were found in `git log --all` for this clone.
The current manifest schema observable in the repo records `law`, `backend`, `tier`, `axioms`, `theorem`, optional `open_goal`, and optional `credit`; it does not record `ProofStrategy` today.
Today's strategy attribution is recoverable without lake through `aver compile <file> --emit-ir-after=law_lower`.
Today's Lean class tags were collected by transpiling only, via `aver proof <file> -o <tmp>` without `--check`; no `lake` command was run.
Lean tag requested roots: `examples/formal`, `projects/k5_fdiv/domain/round.av`
Lean tag transpile runtime: 27.94s

## Reuse Curve From Manifest History
No strategy-attributed reuse curve is recoverable from committed manifest history.
| committed manifest records | records with strategy | curve rows |
| --- | --- | --- |
| 0 | 0 | 0 |

## Today's Trigger Count Ranking
This is a no-lake classifier ranking: it counts distinct laws pinned to each `ProofStrategy` today. It is not a kernel-credit table.
| strategy | distinct laws | examples |
| --- | --- | --- |
| BackendDispatch | 64 | Domain.Exponent.sumExponentLower_9_3_1.section_9_3_1, Domain.Exponent.sumExponentUpper_9_3_2.section_9_3_2, Domain.Fprep.fpMulValue.denotesProduct |
| LinearArithmetic | 44 | AffineWrapper.boundedScale.returnsCore, Domain.Estimate.mulLeTrans.guarded, Domain.Exponent.divideExponentNoOverflow.section_9_3_no_overflow |
| RingIdentity | 12 | Domain.Fprep.fpScaleKeepsSignificand.significandInvariant, Domain.Fprep.fpScaleShiftsExponent.exponentShift, Domain.Rational.minus.equalsPlusNegate |
| FloorDivWindow | 8 | Domain.Fprep.pow2.homomorphism, Domain.Fprep.pow2.positive, Domain.Round.floorDiv.dividesPow2Multiple |
| NonlinearNonneg | 6 | Domain.Estimate.mulLeMonoRight.guarded, Domain.Estimate.mulNonneg.guarded, Domain.Estimate.nrContraction.guarded |
| SimpOverPreludeLemmas | 6 | Domain.Estimate.nrNewErrNum.nrErrorSquares, Domain.Fprep.pow2SignedDenomPositive.positive, Domain.Fprep.pow2SignedHomomorphism.signedHomomorphism |
| IdentityElement | 5 | LawAuto.add.identityZero, LawAuto.add.identityZeroLeft, LawAuto.mul.identityOne |
| Commutative | 3 | LawAuto.add.commutative, LawAuto.add.commutativeSwapSides, LawAuto.mul.commutative |
| Associative | 2 | LawAuto.add.associative, LawAuto.mul.associative |
| MapUpdatePostcondition | 2 | LawAuto.incCount.existingKeyIncrements, LawAuto.incCount.keyPresent |
| SpecEquivalence | 2 | SpecLaws.absVal.absValSpec, SpecLaws.clampNonNegative.clampNonNegativeSpec |
| EffectfulSpecEquivalence | 1 | OracleIndependentProducts.pickPair.branchPathLaw |
| Induction | 1 | LengthHomomorphism.len.homomorphism |
| Reflexive | 1 | LawAuto.id.reflexive |
| UnaryEqualsBinary | 1 | LawAuto.addOne.identityViaAdd |

## Zero-Trigger Strategies
ProofStrategy inventory from `src/ir/proof_ir.rs`: 31 strategies total, 15 fired, 16 never fired on this corpus.
| strategy | distinct laws |
| --- | --- |
| SimpOverLemmas | 0 |
| AntiCommutative | 0 |
| LibraryAxiom | 0 |
| MapKeyTrackedIncrement | 0 |
| SpecEquivalenceSimpNormalized | 0 |
| LinearIntSpecEquivalence | 0 |
| LinearRecurrence2SpecEquivalence | 0 |
| BoundedUniversal | 0 |
| ResultPipelineChain | 0 |
| WrapperOverRecursion | 0 |
| TailRecFixedBaseFold | 0 |
| EnumConstantFold | 0 |
| FiniteDomainCases | 0 |
| IntDecimalRoundtrip | 0 |
| StringEscapeRoundtrip | 0 |
| Sorry | 0 |

## Trigger Count 1 Rows
| strategy | law | source |
| --- | --- | --- |
| EffectfulSpecEquivalence | OracleIndependentProducts.pickPair.branchPathLaw | examples/formal/oracle_independent_products.av |
| Induction | LengthHomomorphism.len.homomorphism | examples/formal/length_homomorphism.av |
| Reflexive | LawAuto.id.reflexive | examples/formal/law_auto.av |
| UnaryEqualsBinary | LawAuto.addOne.identityViaAdd | examples/formal/law_auto.av |

## Mechanism Detail
| mechanism | distinct laws |
| --- | --- |
| BackendDispatch | 64 |
| LinearArithmetic | 44 |
| RingIdentity | 12 |
| NonlinearNonneg | 6 |
| SimpOverPreludeLemmas | 6 |
| FloorDivWindow.PowSumSplit | 3 |
| Commutative.Add | 2 |
| FloorDivWindow.FloorPow2Window | 2 |
| FloorDivWindow.PowPositive | 2 |
| IdentityElement.Add | 2 |
| IdentityElement.Mul | 2 |
| MapUpdatePostcondition | 2 |
| SpecEquivalence | 2 |
| Associative.Add | 1 |
| Associative.Mul | 1 |
| Commutative.Mul | 1 |
| EffectfulSpecEquivalence | 1 |
| FloorDivWindow.FloorPow2Cancel | 1 |
| IdentityElement.Sub | 1 |
| Induction | 1 |
| Reflexive | 1 |
| UnaryEqualsBinary | 1 |

## Current Emitted Class Counts
These counts come from `-- aver:law-class` markers in committed Lean plus requested local no-lake proof export. They are statement classes, not `lake` credit.
| module | universal | bounded | total tags |
| --- | --- | --- | --- |
| AffineWrapper | 1 | 0 | 1 |
| Domain.Fprep | 8 | 0 | 8 |
| Domain.Rational | 5 | 0 | 5 |
| EmptyMapFacts | 3 | 0 | 3 |
| IntAbsLaws | 3 | 0 | 3 |
| IntComparisonLaws | 3 | 0 | 3 |
| LawAuto | 15 | 0 | 15 |
| LengthHomomorphism | 3 | 0 | 3 |
| LogLineLength | 1 | 0 | 1 |
| MapSetNonEmpty | 1 | 0 | 1 |
| OracleIndependentProducts | 1 | 0 | 1 |
| OracleTrace | 1 | 0 | 1 |
| RecursiveMonotone | 3 | 0 | 3 |
| Round | 24 | 0 | 24 |
| SpecLaws | 2 | 0 | 2 |
| StringConcatMonoid | 3 | 0 | 3 |
| TerminalSizeSnapshot | 1 | 0 | 1 |
| ValidatedWrapper | 1 | 0 | 1 |
| tests.fixtures.large_domain_law.baseline | 1 | 0 | 1 |

## Tier Evolution Per Module
No per-module tier evolution is recoverable because no committed manifest history was found.
| module-history rows | universal | bounded |
| --- | --- | --- |
| 0 | 0 | 0 |

## Hand-Credit And Manual Laws
| law | kind | path |
| --- | --- | --- |
| safeSum.commutative | manual sidecar | examples/refinement/natural/proof/SafeSumCommutative.lean |
| sigDouble.doubleIsTwice | hand-credit | projects/k5_fdiv/proofs/dafny/sigDouble__doubleIsTwice.dfy |
| truncSig.truncStickyInt | hand-credit | projects/k5_fdiv/proofs/lean/truncSig__truncStickyInt.lean |
| truncStickyComposes.composesThroughSticky | hand-credit | projects/k5_fdiv/proofs/lean/truncStickyComposes__composesThroughSticky.lean |

## Source Coverage
| source | strategy rows |
| --- | --- |
| examples/formal/affine_wrapper_law.av | 1 |
| examples/formal/empty_map_facts.av | 3 |
| examples/formal/hostile_order_axis.av | 2 |
| examples/formal/int_abs_laws.av | 3 |
| examples/formal/int_comparison_laws.av | 3 |
| examples/formal/law_auto.av | 15 |
| examples/formal/length_homomorphism.av | 3 |
| examples/formal/log_line_length.av | 1 |
| examples/formal/map_set_nonempty.av | 1 |
| examples/formal/oracle_independent_products.av | 1 |
| examples/formal/oracle_trace.av | 1 |
| examples/formal/randomness_paradox.av | 1 |
| examples/formal/recursive_monotone.av | 3 |
| examples/formal/spec_laws.av | 2 |
| examples/formal/string_concat_monoid.av | 3 |
| examples/formal/terminal_size_snapshot.av | 1 |
| examples/formal/validated_wrapper_law.av | 1 |
| projects/k5_fdiv/domain/estimate.av | 8 |
| projects/k5_fdiv/domain/exponent.av | 56 |
| projects/k5_fdiv/domain/floorlaws.av | 1 |
| projects/k5_fdiv/domain/fracround.av | 28 |
| projects/k5_fdiv/domain/recip.av | 2 |
| projects/k5_fdiv/domain/remainder.av | 8 |
| projects/k5_fdiv/domain/sticky_int.av | 4 |
| projects/k5_fdiv/domain/table.av | 4 |
| projects/k5_fdiv/leafprobe.av | 2 |

## Check And Test
Commands used for this report:

```bash
RUSTC_WRAPPER= cargo build --bin aver
python3 tools/proof_telemetry.py --no-current >/tmp/proof-telemetry-smoke.md
python3 tools/proof_telemetry.py --aver-bin target/debug/aver --lean-tags-for examples/formal --lean-tags-for projects/k5_fdiv/domain/round.av --output docs/proof-telemetry.md
python3 -m py_compile tools/proof_telemetry.py
```

The tool never invokes `lake`; local Lean export is transpile-only.

## Deviations And Gaps
- The brief expected committed `proof_manifest.json` history. This clone has 0 such files across `git log --all`, so historical reuse and tier-evolution tables cannot be populated honestly.
- Manifest strategy field: persist per-law `ProofStrategy` in `proof_manifest.json` so historical reuse curves are first-class.
- Manifest history absence: commit proof manifests when proof credit matters; without them, history tables honestly stay empty.
