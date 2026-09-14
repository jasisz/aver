# Knowledge that grows consistently

`examples/formal/knowledge.av` separates admission, merging and queries. Its
eleven laws are universally checked by Lean; no law relies on a finite sample
domain or a `sorry`. Run the example's cases and proofs with:

```sh
aver verify examples/formal/knowledge.av
aver proof examples/formal/knowledge.av --backend lean --check -o out/knowledge
```

Admission reads only the contribution. Headers and bodies carry the SHA-256
key of their bytes; `headerOf.admitted` and `bodyOf.admitted` prove that the
constructors produce admitted contributions. These theorems do not assert
that SHA-256 is injective. The merge laws explicitly require `agrees(a, b)`:
contributions to the same slot carry the same value. Applying this model to
content hashes requires the application's cryptographic assumption; applying
it to verdicts requires a deterministic local producer. The example does not
enforce that producer boundary with a capability or a `Work` binding yet.

The merge algebra is stated over contributions and their application to a
state, rather than a binary merge of two Knowledge snapshots:

| Law | What it establishes |
| --- | --- |
| `merge.commutative` | Two admitted, agreeing contributions can exchange order. |
| `merge.idempotent` | Applying an admitted contribution twice equals applying it once. |
| `mergeAll.appends` | Applying concatenated batches equals applying the batches in succession, for arbitrary lists. This gives batch grouping independence. |
| `mergeAll.orderFree` | The two-contribution batch result follows from commutativity. |
| `mergeAll.swapAdjacent` | The swap remains valid inside arbitrary prefixes and suffixes. |
| `mergeAll.duplicateAdjacent` | An adjacent duplicate can be removed inside arbitrary prefixes and suffixes. |

Repeated adjacent swaps and duplicate removals explain why agreeing
contributions can be reordered and deduplicated. The exported laws above
are the local transformation rules; there is not yet a separate exported
theorem quantified over arbitrary list permutations.

`body.stable` and `verdict.stable` prove that a known answer survives any
admitted contribution consistent with the current state. An unknown answer
may become known. The time at which that happens remains part of coordinator
policy and replay. `count.monotone` proves that body count cannot decrease;
count remains a changing snapshot, without a stable-answer guarantee.

Verdict keys encode the length of the block key before concatenating the block
and context keys. Thus `("ab", "c")` and `("a", "bc")` occupy different slots;
a regression case exercises both in one state.

The Lean map helper proves distinct-key update commutation for `Int`, `String`
and `Bool`, with explicit proofs of the ordering properties it uses. No lawful
order instance is supplied for the model's fallback comparator. The theorem
also handles unsorted or duplicate-key lists quantified by the Lean model,
even though an Aver program cannot construct those map representations.

Coordinator integration remains a separate step: bind the provider, admit
peer contributions before merging, accept verdicts only from local `Work`,
and exercise parallel contribution processing and stable queries with `!`.
The proofs here do not make control decisions or arbitrary snapshots commute.
