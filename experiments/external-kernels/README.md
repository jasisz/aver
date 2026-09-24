# External kernels on certificate packages

A measurement, not part of the verifier. The question: could the final replay of `aver-cert verify` (today `leanchecker --fresh CheckerWitness`, from the same Lean distribution that elaborated the package) be done by an independent kernel, and what would it cost?

The workflow `.github/workflows/experiment-external-kernels.yml` runs on pushes to `experiments/external-kernels`. For each package it:

1. produces the package with `aver compile --target wasm-gc --certify`:
   - `certkit-clockrange`: `tools/certkit/fixtures/clockrange.av`;
   - `k5-main`: `projects/k5_fdiv/main.av`;
   - `k5-laws`: a generated leaf `LawsEntry` that depends on every module under `projects/k5_fdiv/domain` and exports one trivial function;
2. runs `aver-cert check` and `aver-cert verify` with `AVER_CERT_TIMINGS=1`. The verify run's final replay goes through the developer override `AVER_CERT_PARALLEL_REPLAY`, pointed at a script that copies the staged build directory away and then runs exactly `leanchecker --fresh CheckerWitness`, so the verdict is still the stock one;
3. runs stock `aver-cert verify` on the artifact with one code-section byte flipped and the manifest hash re-pinned, so the rejection has to come from Lean;
4. in the kept build, times `lake env leanchecker --fresh CheckerWitness` again, exports the whole `CheckerWitness` closure with `lean4export` (tag `v4.34.0`), and replays the export through
   - the arena's "official" checker (lean4export's parser, then `Environment.replay` in the stock kernel),
   - nanoda (arena configuration: all axioms admitted; and a strict one admitting only `propext`, `Classical.choice`, `Quot.sound`),
   - sokonanoda,
   - the MathGraph checker;
5. rewrites the export so `AverCert.ArtifactBytes.modBytes` denotes the byte-flipped module (exactly one 1024-byte numeral changes; the proofs stay as built) and replays that through the same kernels (`tamper.py`);
6. re-elaborates every module of the verify build with `-Dprofiler=true -DElab.async=false` to split their time into kernel (`type checking`) and elaboration.

Kernel revisions and build recipes follow the Lean Kernel Arena's `checkers/*.yaml` (revisions in `build-tools.sh`). Two deviations: no `-C target-cpu=native` and no profile-guided build for sokonanoda and MathGraph, since the binaries are built on one runner and run on another. The arena's "official" checker is built against the same lean4export revision as the exporter.

Results are uploaded as workflow artifacts (`results-<package>`, `summary`) and printed to the job summary.

## Results (run 36000339454, 2026-09-24, GitHub `ubuntu-latest`, 4 cores)

https://github.com/jasisz/aver/actions/runs/36000339454 (the job summary carries the full tables). Wall seconds; peak memory in parentheses. "Tampered" is the export with one artifact byte flipped; "root" is the export restricted to `AverCertChecker.checked` and its dependencies.

| kernel | clockrange | clockrange tampered | k5-main | k5-main tampered | k5-laws | k5-laws tampered |
|---|---|---|---|---|---|---|
| `leanchecker --fresh` (stock) | 82 s (1.0 GiB) accepted | n/a | 88 s (1.0 GiB) accepted | n/a | 166 s (5.7 GiB) accepted | n/a |
| official kernel from export | 102 s accepted | rejected | 107 s accepted | rejected | 198 s (5.4 GiB) accepted | rejected |
| nanoda | 25 s (1.0 GiB) accepted | rejected | 40 s (3.0 GiB) accepted | rejected | 68 s (4.6 GiB) accepted | rejected |
| sokonanoda | 12 s (1.9 GiB) accepted | rejected | 13 s (2.2 GiB) accepted | rejected | 45 s (11.5 GiB) accepted | rejected |
| MathGraph | 13 s (2.0 GiB) accepted | rejected | 15 s (2.3 GiB) accepted | rejected | 50 s (11.5 GiB) accepted | rejected |
| export (`lean4export`) | 25 s, 484 MB | | 25 s, 486 MB | | 21 s, 529 MB | |
| root only: official / nanoda / nanoda strict / sokonanoda / MathGraph | 23 / 6 / 6 / 7 / 8 s | | 28 / 9 / 8 / 7 / 7 s | | 77 / 45 / 45 / 34 / 35 s | |

The stock verifier, run on the same flipped artifact with the manifest hash re-pinned (the offset is chosen so that the module still validates), declines at the Lean build: a `decide +kernel` byte fact evaluates to `false`. Every kernel rejects the flipped export too; the official kernel names the declaration (`AverCert.Artifact.plans_chunk_192._proof_1_1` for k5-laws, an arithmetic helper body for the other two).

Verify breakdown: clockrange 168 s = build 82 + witness 1 + audit 4 + replay 81; k5-main 198 s = 92 + 1 + 17 + 88; k5-laws 1026 s = 673 + 19 + 171 + 162.

Kernel versus elaboration (Lean profiler, every module of the verify build re-elaborated with `-DElab.async=false`; "type checking" is the kernel): clockrange 12 % of 87 s, k5-main 16 % of 97 s, k5-laws 15 % of 659 s. The wall modules are 5 % kernel; the `Artifact*` byte-fact modules are 90 to 100 % kernel but small (k5-laws: about 70 s in total, `ArtifactClosure` alone 29 s); the source-model and bridge modules are 0 to 20 % kernel and dominate k5-laws (`BridgeSteps8` 278 s, of which `simp` 152 s and elaboration 102 s, kernel 1 s). The wall's `ArithTemplateDerisk` spends 10 to 20 s in typeclass inference on every verify.

Notes:

- Nat literals: the checker renders the artifact as 1024-byte numerals joined by `<<<` and `|||`; the exports carry 386 to 659 `natVal` literals, the largest 2466 to 3165 decimal digits. All kernels evaluate them through their Nat extensions (`Nat.shiftLeft`, `Nat.lor` on big integers); none fell back to unary arithmetic.
- The big `decide +kernel` facts replay in every kernel. The external kernels are 2 to 7 times faster than `leanchecker --fresh` on the full closure.
- The full closure is mostly the Lean distribution: the official kernel replays 78,810 declarations for clockrange (85,497 for k5-laws), against 9,210 (9,510) in the root closure. The root-restricted replay is about 4 times cheaper for the small packages (official kernel 102 s to 23 s, nanoda 25 s to 6 s) and 1.3 to 2.6 times cheaper for k5-laws, where the package's own proofs weigh more.
- nanoda with only `propext`, `Classical.choice` and `Quot.sound` permitted fails on the full closure (Init uses `Lean.trustCompiler`) but accepts every root closure, so it doubles as an independent axiom check of the accepted root.
- nanoda, sokonanoda and MathGraph report a rejection by panicking (exit 101 or 1), not with a verdict line. sokonanoda and MathGraph peak at 11.5 GiB on k5-laws, close to the 16 GiB of a hosted runner; nanoda stays under 5 GiB.
- `lean4export` loads the same `.olean` files through Lean's own loader, so an external kernel is independent in type checking only, not in reading the build.
- Timings vary about 1.5x between hosted runners (the first run, 35993987305, measured k5-laws verify at 1574 s).

Recommendation: restructuring toward "elaborate, then a fast kernel" is not worth it, because the kernel is 12 to 16 % of the build and elaboration (`simp`, `grind`, typeclass inference) is the rest. An independent kernel as an additional final replay is cheap and closes the "one kernel" gap in the trust boundary: nanoda on a root-restricted export costs about 10 s (export) plus 6 to 45 s, and also checks the axioms. Keep `leanchecker --fresh` beside it rather than replacing it. The larger saving is to restrict both replays to the closure of the checked root and the pins, instead of every declaration in Init and Std.
