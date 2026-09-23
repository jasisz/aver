# Rust iteration build measurements

This note records the measurements behind the generated `iteration` profile
and the decision to keep one generated crate for now. It is a benchmark
record. Other machines will not necessarily reproduce the same wall-clock
numbers.

## Setup

- measured: 2026-08-28
- host: Apple M2 Pro, 12 cores, 32 GB, arm64 macOS 26.6
- toolchain: rustc/cargo 1.95.0
- Aver base: `361e034765c6b257ad54853bb6659ebb451495c0`, plus the change described here
- program: `n1bor/btc-listener` at
  `a28f19de1d85d506cc75b0a52a9466b2e9607af8` (140 modules)
- native providers: the pinned Primitives and RocksDB providers; only their
  machine-specific `aver-rt` path was redirected to this checkout

All commands used the same generated source tree and separate Cargo target
directories. Times are elapsed wall time from `/usr/bin/time -p`. The leaf
edit changed one help string in `App.Usage`. The wide edit changed one string
in `Domain.Transaction`, which much of the program imports. Neither edit
changed the Rust type interface.

## Large-project build times

| State | `cargo check --release` | `cargo build --profile iteration` | `cargo build --release` |
|---|---:|---:|---:|
| clean target, including RocksDB | 86.79 s | 227.68 s | 300.14 s |
| no generated change | 0.24 s | 0.19 s | 0.21 s |
| leaf-module body edit | 5.30 s | 3.80 s | 212.78 s |
| widely depended-on body edit | 6.56 s | 11.61 s | 210.46 s |

Before write-if-changed materialisation, a no-op `aver compile` rewrote every
generated source file, and the `cargo check --release` after it took 2.71 s.
With unchanged mtimes it takes 0.24 s. Aver's own parse/typecheck/codegen pass
still takes about 7–9 seconds on this program. This change does not try to
speed up that stage.

Compiling the native RocksDB provider dominates the clean numbers. For
choosing a profile, the edit numbers are the ones to compare. The iteration
profile turns a runnable leaf edit from 213 seconds into 3.8 seconds, and a
wide edit into 11.6 seconds. Final release keeps its existing optimisation.
The recorded wide release run also restored the leaf string to its baseline.
Both changes take the same whole-crate LTO path, and its 210.46-second result
matches the 212.78 seconds of the isolated leaf edit.

## Choosing optimisation level 1

Runtime was measured separately, using the repository's Map-build benchmark
shape enlarged to two million inserts. The first run, with a cold filesystem,
was discarded. The table reports the median elapsed time of the next four
runs.

| Profile | Clean small-project build | Runtime |
|---|---:|---:|
| iteration candidate, `opt-level = 0` | 4.44 s | 1.98 s |
| chosen iteration, `opt-level = 1` | 7.52 s | 0.67 s |
| final release, `opt-level = 3`, LTO, one codegen unit | 10.34 s | 0.65 s |

On this workload, level 0 saves about three seconds on a clean small build and
runs about three times slower. Level 1 stays within roughly 3% of the
final-release runtime and keeps the incremental link on the large project
fast. That measurement is why the profile uses `opt-level = 1`; the value was
not copied from a Cargo default.

## Crate-boundary decision

Keep one generated crate for now. Write-if-changed plus Rust incremental
compilation already gives a 3.8-second leaf rebuild and an 11.6-second
rebuild after an edit in a widely depended-on module. Splitting 140 Aver
modules into crates would add public-boundary and link overhead and make
cross-module optimisation less direct, and this data shows no measured win
for it.

Revisit grouped or per-module crates only with a benchmark that beats these
numbers and also records clean build time and representative runtime.
Final-release LTO is expensive on purpose and is not part of the iteration
workflow.
