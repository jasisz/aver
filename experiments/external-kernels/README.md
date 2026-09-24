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
6. re-elaborates the eight slowest modules of the verify build with `-Dprofiler=true -DElab.async=false` to split their time into kernel (`type checking`) and elaboration.

Kernel revisions and build recipes follow the Lean Kernel Arena's `checkers/*.yaml` (revisions in `build-tools.sh`). Two deviations: no `-C target-cpu=native` and no profile-guided build for sokonanoda and MathGraph, since the binaries are built on one runner and run on another. The arena's "official" checker is built against the same lean4export revision as the exporter.

Results are uploaded as workflow artifacts (`results-<package>`, `summary`) and printed to the job summary.
