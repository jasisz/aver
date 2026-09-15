# Knowledge that grows consistently

`examples/knowledge/knowledge.av` separates admission, merging and queries. Its
twenty laws are universally checked by Lean; no law relies on a finite sample
domain or a `sorry`. Run the example's cases and proofs with:

```sh
aver verify examples/knowledge/knowledge.av
aver proof examples/knowledge/knowledge.av --backend lean --check -o out/knowledge
```

Admission reads only the contribution. Headers and bodies carry the SHA-256
key of their bytes; `headerOf.admitted` and `bodyOf.admitted` prove that the
constructors produce admitted contributions. These theorems do not assert
that SHA-256 is injective. The merge laws explicitly require `agrees(a, b)`:
contributions to the same slot carry the same value. Applying this model to
content hashes requires the application's cryptographic assumption; applying
it to verdicts requires a deterministic local producer. The runnable provider below enforces the peer/local-worker split; its
query laws retain the explicit consistency premise.

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

The ordinary Aver law `runBatches.anySchedule` quantifies over any two finite lists of
batches, without a bound on their lengths. If both contain the same set of
contributions, every contribution is admitted, and all contributions agree
pairwise, both schedules produce equal Knowledge from the same initial state.
Order, batch boundaries, empty batches and duplicate counts may all differ.
Its executable `covered(xs, ys)` premise says that each entry of `xs` occurs
in `ys`, is admitted and agrees with every entry of `ys`. Coverage in both
directions expresses equal membership and agreement; it permits different
multiplicities. `allAdmitted` makes the admission requirement explicit for
both flattened schedules. These predicates inspect contributions, without
assuming equality of the resulting states.

All the laws live in [knowledge.av](../examples/knowledge/knowledge.av).
`mergeAll.moveOne`, `absorbMember`, `absorbCovered` and `commuteCovered` compose
the local merge laws into `sameContributions`; `runBatches.flatten` lifts the
result to arbitrary batches. The source uses ordinary `given`, `when`,
`because` and `using` clauses. The commands above generate and check the entire
proof and report all twenty laws in the normal manifest, using only
`propext`, `Classical.choice` and `Quot.sound` as axioms.

The Lean exporter first tries to compose the cited facts with recursive
functions left opaque. When a list induction is needed, it keeps guards in
the induction motive, generalizes the other state inputs, and uses checked
recursive equations with the cited laws to prove the base and step. It also
tries generalizing another list when that list is a changing accumulator.
This strategy is independent of the example's names and types; it is tested
on a separate meter model and a growing list accumulator. A failed step stays
an open obligation, including when all supplied samples pass. This is a
bounded proof search for universal theorems, not a bounded schedule domain.

`body.stable` and `verdict.stable` prove that a known answer survives any
admitted contribution consistent with the current state. An unknown answer
may become known. The time at which that happens remains part of coordinator
policy and replay. `count.monotone` proves that body count cannot decrease;
count remains a changing snapshot, without a stable-answer guarantee.

`body.stableRun` and `verdict.stableRun` lift stability to every finite sequence
of consistent, admitted updates. `consistentRun` states the condition at each
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

## A running provider and coordinator

The model now lives beside its consumer in `examples/knowledge/`. Run the
whole program, including its generated coordinator, from that project root.
Its proof checks all 62 laws universally with no bounded or open obligations:

```sh
cd examples/knowledge
aver check main.av
aver verify main.av
aver run main.av
aver proof main.av --backend lean --check --sorry-budget 0 -o /tmp/knowledge-proof
aver run main.av --record /tmp/knowledge-recording
aver replay /tmp/knowledge-recording --test --diff
```

`Content` declares the peer and reader operations; `aver.toml` binds them to
`Stored`. A peer offers two hash/body pairs. `Stored.offer` checks both hashes
with `(admitted(first), admitted(second))!`, before changing any state. A bad
hash rejects the entire batch. Accepted bodies enter the **same** `Knowledge`
model proved above, then join the ordered work queue. Reversing an admitted,
agreeing pair preserves Knowledge; the queue's order is deliberately separate.
Duplicate queue entries are all removed when a task starts.

The `Validation` job kind binds `Stored.validate`, a pure illustrative rule:
nonempty bodies are valid in the fixed `nonempty-v1` context. This is not
Bitcoin block validation. `Work.take` delivers the body hash and Boolean
result to `Stored.validated`, which translates it to the model's verdict.
There is no peer operation that accepts a verdict. Worker failure records a
control error and adds no Knowledge. Capability boundary layouts remain local
to their contracts; the internal model's nominal types do not cross the job
ABI implicitly.

`Stored.read` uses `!` for the body and verdict lookups. It answers
`Now(Ok(snapshot))` only once both are known, otherwise `Later(NextTurn)` or a
worker error. Its stable-answer law applies to the actual answer function:
under admitted, consistent growth, a successful `Now` retains its value.
`read.stableHistory` applies this to every finite consistent update sequence,
with no bound on its length.
`observed` projects exactly that successful `Now` payload; it returns `None`
for `Later`, `Then` and errors, so the premise does not compare wake handles.
Errors and the time an answer becomes known have no stability claim.

The example has two producers and a reader. It rejects a forged hash, processes
duplicates through local Work, and obtains the same settled answer after another
delivery. Native VM, generated Rust and wasm integration tests execute this
program; the VM recording is replayed with recomputation of local work.

The independent products run inside the answer functions. The generated
coordinator still orders requests, queue updates and result visibility; this
example does not add parallel yielding calls or make control decisions commute.

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

The generator also emits `__HistoryEvent`, `__historyStep`,
`__historyAdmissible`, and `__historyRun` into this program. Events use its
concrete request, answer-state and job-result types. The fold calls the same
pure transitions as the live coordinator: parking, saving an answer, settling
an instance, seating a worker, reporting its result, and cancellation. Clock,
stop and returned answer-state observations are explicit events. These helpers
are proof/verification code; normal execution does not record or allocate a
history.

The three generated `__historyRun` laws quantify over arbitrary finite lists:

- `noNewProcesses`: after initial seating, the slot count never increases.
- `jobsStayWithinLimit`: a run initially within `max-jobs` remains within it,
  counting all job kinds in the same table.
- `retiredInstanceNeverReturns`: once an instance is retired (its process is
  gone or its number has advanced), it stays retired. The per-process
  `answeringRetiresTheInstance` law establishes this premise after accepting
  a current answer, including completion that removes the slot.

Each statement applies to any finite prefix, with no length bound. The
admissibility predicate checks each event against the state its predecessors
left: starts require positive room; settlements require a nonnegative instance
and a seated slot if that instance is current. It does **not** assume the three
conclusions. The history model deliberately allows more observations than the
live driver, including arbitrary returned answer states, arbitrary tasks and
spurious job reports. Thus these structural properties need no honesty or
consistency premise about a provider. Semantic properties of its answers still
need the provider's own laws, such as `Stored.read.stableHistory`.

The remaining proof work is to show that lowering and the effectful driver
preserve the direct-style request trace. These history laws prove the composed
pure transitions; they do not establish that correspondence or termination.
`runBatches.anySchedule` separately covers Knowledge contributions. Automatic
schedule enumeration remains optional execution testing.
