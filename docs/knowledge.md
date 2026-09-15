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

The companion [Schedules.lean](../examples/formal/knowledge_proof/Schedules.lean)
composes these exported laws into an arbitrary-schedule theorem. It imports
the freshly generated `Knowledge` module and uses its actual `mergeAll`.
It supplies no alternate implementation of merging.

`KnowledgeSchedules.any_schedule` quantifies over any two finite lists of
batches, without a bound on their lengths. If both contain the same set of
contributions, every contribution is admitted, and all contributions agree
pairwise, both schedules produce equal Knowledge from the same initial state.
Order, batch boundaries, empty batches and duplicate counts may all differ.
The membership premise permits different multiplicities, so the result is
stronger than a permutation theorem. The proof uses induction and the exported merge
laws, with only `propext`, `Classical.choice` and `Quot.sound` as axioms.

Check it after exporting the model above:

```sh
cp examples/formal/knowledge_proof/Schedules.lean out/knowledge/Schedules.lean
(cd out/knowledge && lake env lean Schedules.lean)
```

This is a reviewed companion Lean proof, checked in the Proof suite against a
fresh Aver export. The ordinary Aver proof manifest still reports the eleven
source laws; the test separately audits the companion's five public theorems.

`body.stable` and `verdict.stable` prove that a known answer survives any
admitted contribution consistent with the current state. An unknown answer
may become known. The time at which that happens remains part of coordinator
policy and replay. `count.monotone` proves that body count cannot decrease;
count remains a changing snapshot, without a stable-answer guarantee.

The companion also lifts body and verdict stability to every finite sequence
of consistent, admitted updates. `ConsistentRun` states the condition at each
actual intermediate state, rather than assuming that admission alone makes a
contribution consistent with an arbitrary initial state. The theorem applies
to every finite prefix of a continuing run. It does not assert when an unknown
answer becomes available or that an unfair schedule eventually serves it.

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

## Generated coordinator laws

The coordinator generator emits proof obligations for its own pure transitions:
stale answers leave slots alone and increment the dropped counter, `Later`
preserves the request and instance, fresh instances increase, and a full job
table offers no new task. These laws already quantify over all states and
arguments satisfying their premises when the proof report says `universal`.
The `given` values also support sampled VM verification; they are not the
domain restriction of a successful universal proof.

Generation alone is not proof. `aver proof --check` runs the checker, and the
report distinguishes universal proofs from bounded or failed obligations.
Some generated laws cite laws supplied by the program's answer module, such
as `aStartedTaskIsNotAskedAgain`; that dependency must itself prove universally.
The examples' green proofs do not establish a theorem about every possible
program the generator might receive.

The remaining coordinator proof work is to compose its transition invariants
over arbitrary finite admissible histories and prove that the generated code
preserves the direct-style request trace. `any_schedule` above covers Knowledge
contributions; it does not claim either of those whole-coordinator results.
Automatic schedule enumeration is optional execution testing, not a substitute
or a prerequisite for these universal proofs.
