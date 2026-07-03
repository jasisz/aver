# durable_promise

The core of a durable-promise state machine, modeled in pure Aver and shipped with a
machine-checked certificate: every safety law below is proven **universal on the Lean 4
kernel**, and the same model runs.

## What is modeled

A durable promise is a cell that starts `Pending` and moves **exactly once** to a settled
state. It is the smallest honest slice of a durable-execution promise machine — the state
ADT and its pure transitions, with no effects, no server, no persistence layer.

```
type PromiseState
    Pending
    Resolved(Int)      // carries the resolved value
    Rejected(Int)      // carries the rejection error
    TimedOut

record Promise
    state: PromiseState
    timeoutAt: Int      // absolute deadline
```

Time never enters as an ambient clock — only as a `now: Int` argument, the same discipline
the reference spec uses.

Three pure transitions plus a projection:

- `observe(p, now)` — the timeout **projection**. A `Pending` promise whose deadline has
  passed (`now >= timeoutAt`) is *viewed* as `TimedOut`; everything else is returned
  unchanged. This is the read-time view applied *before* any writer acts, so observation
  happens before persistence.
- `resolve(p, now, value)` / `reject(p, now, error)` — settle a still-`Pending` promise.
  Both project the timeout first (call `observe`) and only then write, which is what makes a
  settled promise absorbing and an expired one impossible to resurrect.

The value slot is modeled the way the existing Aver corpus models payloads: it lives inside
the state variant (`Resolved(Int)` / `Rejected(Int)`), so `Pending` and `TimedOut` carry no
value by construction rather than by a nullable field.

## What is PROVEN vs sample-verified

**All 8 law theorems are proven universal on the Lean kernel** (`aver proof --backend lean
--check` reports `"universal":true, "sorries":0, "universal_laws":8, "bounded_laws":0`).
Nothing here is sample-only. Every theorem's `#print axioms` set is exactly the three
standard Lean foundational axioms — no `sorryAx`:

```
{ Classical.choice, Quot.sound, propext }
```

The 8 theorems cover the 5 safety themes:

| # | Theme | Law(s) | Lean statement (∀-closed) |
|---|-------|--------|---------------------------|
| 1 | Settled is absorbing | `observe.settledIsIdentity`, `resolve.settledIsIdentity`, `reject.settledIsIdentity` | `isSettled p = true → op(p, …) = p` |
| 2 | No double-settle | `resolve.alwaysSettles` (helper), `resolve.noDoubleSettle` | `resolve (resolve p n1 v1) n2 v2 = resolve p n1 v1` |
| 3 | Projection is idempotent | `observe.idempotent` | `observe (observe p now) now = observe p now` |
| 4 | Timeout is monotone | `observe.timeoutMonotone` | `isTimedOut (observe p now1) && now2 >= now1 → isTimedOut (observe p now2)` |
| 5 | No resurrection | `resolve.noResurrection` | `isTimedOut (observe p now) → resolve (observe p now) now v = observe p now` |

The `noDoubleSettle` law states more than "the value is preserved": it proves the *entire*
promise (state and value) is unchanged by a second settle — so the first value is kept, as a
corollary. The `alwaysSettles` helper (`resolve` never leaves a promise `Pending`) is the
lemma that makes it go through. `noResurrection` is discharged by the kernel citing
`observe.idempotent` automatically (`grind [observe_law_idempotent]`) — laws-as-lemmas
composition, no manual wiring.

The `verify` blocks in `domain/promise.av` are a separate, independent check: they evaluate
each law on concrete samples with Aver's own evaluator (no Lean), which catches a false claim
before any proof is attempted. They are a cross-check, not the proof — the proof is the
universal `∀` theorem.

### How each theorem closes (from the generated Lean)

The three unconditional laws close with a direct `grind` over the definitions; the five
`when`-guarded laws close with `simp only [<defs>, Bool.and_eq_true, decide_eq_true_eq] <;>
grind`. The emitter wraps each guarded proof in a `first | <tactic> | … | sorry` portfolio
whose final `sorry` is a never-taken floor: if any theorem had actually closed through it, its
axiom set would contain `sorryAx` and the manifest tier would not be `universal`. It does not —
`proof_manifest.json` records all 8 as `"tier":"universal"` with the clean 3-axiom set.

## Reproduce

From the repository root (build the repo binary first: `cargo build --bin aver`):

```
# 1. Sample-check every function and law (Aver evaluator, no Lean):
aver verify projects/durable_promise/domain/promise.av

# 2. Export to Lean 4 and run the kernel check (rm the output dir first):
rm -rf out
aver proof projects/durable_promise/domain/promise.av --backend lean --check --check-json

#    Expected tail:
#    {"backend":"lean", … ,"passed":true,"sorries":0,"universal":true,"universal_laws":8}
#    Per-law axioms + tiers are written to out/proof_manifest.json.

# 3. Run the demo:
aver run projects/durable_promise/main.av --module-root projects/durable_promise
```

Demo output:

```
start          : Pending(deadline=10)
resolve @5     : Resolved(42)
resolve @6     : Resolved(42)  (first value kept)
observe @12    : TimedOut(deadline=10)  (timeout projected)
resolve @12    : TimedOut(deadline=10)  (no resurrection)
```

## Files

- `domain/promise.av` — the state machine: ADT, transitions, and the 8 law theorems.
- `main.av` — the runnable demo above.
