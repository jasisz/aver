# Optional waterfall proof discovery

`aver proof --waterfall PATH --check` tries additional proof search after Aver’s
existing automation. `PATH` is a locally built [waterfall](https://github.com/samth/waterfall)
checkout. The option is off by default and supports the Lean backend in `auto`
verify mode. It cannot be combined with `--allow-mathlib`.

```sh
aver proof program.av -o out/proof --check --waterfall /path/to/waterfall

# Limit discovery to a law identity from proof_manifest.json.
# A root law selection also includes all its because/implication obligations.
aver proof program.av -o out/proof --check-json \
  --waterfall /path/to/waterfall --waterfall-law function.lawName
```

## Installation

The integration was tested with waterfall revision
`e04ea93b678c831c404067dd86573b821bc528e4` and Aver’s generated Lean 4.34.0 project.
Build waterfall using the **same toolchain as the exported `lean-toolchain`**:

```sh
git clone https://github.com/samth/waterfall.git /path/to/waterfall
git -C /path/to/waterfall checkout e04ea93b678c831c404067dd86573b821bc528e4
cd /path/to/waterfall
lake +leanprover/lean4:v4.34.0 build
```

Aver does not fetch packages or change the checkout. Only search subprocesses
receive its compiled import directory. Generated proofs and their normal Lake
build have no waterfall dependency.

## What is checked

1. The exporter emits compiler-owned candidate statements and admissible helper
   names. Explicit `using` retains its existing citation rules, including `using []`.
   Claims declined at provider or unproved fuel boundaries remain declined.
2. Existing universal proofs get a transitive axiom audit first. For a bounded
   law, the compiler builds a universal candidate using its normal statement
   builder: it omits sample-membership premises and retains `when` guards and
   refinement binders.
3. Unresolved candidates try `waterfall?` in search mode, then committed mode.
   Each attempt uses one worker, effort 1,000 and 1,000,000 Lean heartbeats.
   `--waterfall-effort N` changes effort; `--waterfall-timeout N` changes the
   default 30-second limit per baseline, search, or replay subprocess. Dependency
   builds have a separate five-minute limit. These are per-attempt limits, not a
   bound on total project time; `--waterfall-law` can restrict the work.
4. A suggested ordinary Lean script is replayed without importing waterfall.
   Only a successful Lean exit and an explicit audit containing at most
   `propext`, `Classical.choice`, and `Quot.sound` allow replacement. Missing
   diagnostics, timeout, `sorryAx`, and `Lean.ofReduceBool` earn no credit.
5. The full generated project passes through the existing build, law and
   obligation audits, budgets, and manifest gate. Each `because` obligation
   remains separate; the root proof still composes those obligations.

Failed search retains the original proof or bounded statement. A broken
dependency build prevents discovery in its consumers. A hard error in an earlier
declaration can also prevent an isolated attempt from reaching its target; this
initial integration does not repair arbitrary errors in an export.

## Retained output

- Generated `.lean` files contain the accepted ordinary scripts.
- `proof_waterfall_cache.json` retains scripts keyed by the preceding emitted
  context, exact candidate statement, and helper hints. Keep it beside the export
  to reuse discoveries after regeneration. Cache proposals are rechecked on every
  use, including against the current imported dependencies; a cache entry grants
  no proof credit by itself. If every needed script replays, the original
  waterfall checkout need not be present.
- `proof_waterfall.json` reports `existing-proof`, `discovered`, `replayed`,
  `unresolved`, or `dependency-build-failed` for selected candidates. Discovery
  results are preliminary: `proof_manifest.json` from the final check determines
  whole-project proof status.
- `proof_waterfall.log` records attempted Lean commands and their diagnostics.

Search and replay use the definitions and preceding declarations of the emitted
module, never an import of the target module containing its own theorem. Larger
modules batch their initial baseline audit. Changed suppliers rebuild before
their consumers are searched. Failed dependency builds cannot supply stale oleans.

## Validation

The integrated CLI closes the tree accumulator example, after which existing
automation closes its traversal corollary: two universal laws, no `sorry`.
Regeneration replays the retained script with the waterfall checkout absent.
The regression canary also corrupts a cache entry with `sorry` and checks that a
false `because` remains failed even when its final implication is easy.

On btc-listener `ba4303a5a4c247f39b5257f0ca43d694340386d6`, the integrated CLI
finds a committed-mode script for
`isMinimalPush.directPushIsMinimalUnlessSmallNumber` in `domain/stackitem.av`.
The full export checks with **51 universal laws, zero bounded laws, zero sorries,
and zero build errors**. The original byte guard remains; this removes enumeration
without expanding the intended byte domain.

Run the integration canary explicitly after building waterfall:

```sh
AVER_WATERFALL_DIR=/path/to/waterfall \
  cargo test --test proof_spec waterfall -- --ignored
```

Earlier standalone experiments and their limitations are in
[tools/waterfall/README.md](../tools/waterfall/README.md) and
[the btc-listener report](../tools/waterfall/BTC.md).
