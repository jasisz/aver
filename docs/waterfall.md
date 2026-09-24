# Optional waterfall proof discovery

`aver proof --waterfall PATH --check` runs extra proof search after Aver’s
existing automation has finished. `PATH` is a locally built
[waterfall](https://github.com/samth/waterfall) checkout. The option is off by
default and works with the Lean backend in `auto` verify mode. It cannot be
combined with `--allow-mathlib`.

```sh
aver proof program.av -o out/proof --check --waterfall /path/to/waterfall

# Limit discovery to a law identity from proof_manifest.json.
# A root law selection also includes all its because/implication obligations.
aver proof program.av -o out/proof --check-json \
  --waterfall /path/to/waterfall --waterfall-law function.lawName
```

## Installation

The integration was tested with waterfall revision
`e04ea93b678c831c404067dd86573b821bc528e4` and the Lean 4.34.0 project that
Aver generates. Build waterfall with the **same toolchain as the exported
`lean-toolchain`**:

```sh
git clone https://github.com/samth/waterfall.git /path/to/waterfall
git -C /path/to/waterfall checkout e04ea93b678c831c404067dd86573b821bc528e4
cd /path/to/waterfall
lake +leanprover/lean4:v4.34.0 build
```

Aver does not fetch packages and does not modify the checkout. Its compiled
import directory is passed only to search subprocesses. Generated proofs and
their normal Lake build do not depend on waterfall.

## What is checked

1. The exporter emits candidate statements owned by the compiler, plus the
   names of admissible helpers. An explicit `using` keeps its existing
   citation rules, including `using []`. Claims declined at provider
   boundaries or at unproved fuel boundaries stay declined.
2. Existing universal proofs first get a transitive axiom audit. For a bounded
   law, the compiler builds a universal candidate with its normal statement
   builder. It drops the sample-membership premises and keeps `when` guards
   and refinement binders.
3. Unresolved candidates try `waterfall?` in search mode and then in committed
   mode. Each attempt uses one worker, effort 1,000 and 1,000,000 Lean
   heartbeats. `--waterfall-effort N` changes the effort.
   `--waterfall-timeout N` changes the default 30-second limit for each
   baseline, search or replay subprocess. Dependency builds have their own
   five-minute limit. All of these limit a single attempt and do not bound the
   total time for a project; use `--waterfall-law` to restrict the work.
4. A suggested ordinary Lean script is replayed without importing waterfall.
   It replaces the old proof only if Lean exits successfully and an explicit
   audit shows at most `propext`, `Classical.choice` and `Quot.sound`. Missing
   diagnostics, a timeout, `sorryAx` or `Lean.ofReduceBool` all count as
   failure.
5. The full generated project then goes through the existing build, law and
   obligation audits, budgets and manifest gate. Each `because` obligation
   stays separate, and the root proof still composes them.

A failed search keeps the original proof or bounded statement. A broken
dependency build blocks discovery in its consumers. A hard error in an earlier
declaration can also stop an isolated attempt from reaching its target. This
first integration does not repair arbitrary errors in an export.

## Retained output

- Generated `.lean` files contain the accepted ordinary scripts.
- `proof_waterfall_cache.json` keeps scripts keyed by the preceding emitted
  context, the exact candidate statement and the helper hints. Keep it next to
  the export to reuse discoveries after regeneration. Cached proposals are
  rechecked on every use, including against the current imported dependencies,
  so a cache entry never grants proof credit on its own. If every needed
  script replays, the original waterfall checkout does not have to be present.
- `proof_waterfall.json` reports `existing-proof`, `discovered`, `replayed`,
  `unresolved` or `dependency-build-failed` for the selected candidates.
  Discovery results are preliminary. The `proof_manifest.json` from the final
  check decides the proof status of the whole project.
- `proof_waterfall.log` records the Lean commands that were tried and their
  diagnostics.

Search and replay use the definitions and preceding declarations of the
emitted module. They never import the target module that contains the theorem
itself. Larger modules batch their initial baseline audit. Changed suppliers
are rebuilt before their consumers are searched. A failed dependency build
cannot supply stale oleans.

## Validation

The integrated CLI closes the tree accumulator example, and existing
automation then closes its traversal corollary: two universal laws, no
`sorry`. Regeneration replays the retained script without the waterfall
checkout. The regression canary also corrupts a cache entry with `sorry` and
checks that a false `because` still fails, even when its final implication is
easy.

On btc-listener `ba4303a5a4c247f39b5257f0ca43d694340386d6`, the integrated CLI
finds a committed-mode script for
`isMinimalPush.directPushIsMinimalUnlessSmallNumber` in `domain/stackitem.av`.
The full export checks with **51 universal laws, zero bounded laws, zero
sorries, and zero build errors**. The original byte guard is still there. The
script removes enumeration without widening the intended byte domain.

After building waterfall, run the integration canary explicitly:

```sh
AVER_WATERFALL_DIR=/path/to/waterfall \
  cargo test --test proof_spec waterfall -- --ignored
```

Earlier standalone experiments and their limitations are described in
[tools/waterfall/README.md](../tools/waterfall/README.md) and
[the btc-listener report](../tools/waterfall/BTC.md).
