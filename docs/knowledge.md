# Knowledge that grows consistently

`examples/knowledge/knowledge.av` keeps admission, merging and queries apart.
Lean checks all twenty of its laws universally. No law depends on a finite
sample domain or on a `sorry`. Run the example's cases and proofs with:

```sh
aver verify examples/knowledge/knowledge.av
aver proof examples/knowledge/knowledge.av --backend lean --check -o out/knowledge
```

Admission looks only at the contribution. Headers and bodies carry the SHA-256
key of their bytes, and `headerOf.admitted` and `bodyOf.admitted` prove that
the constructors produce admitted contributions. These theorems do not claim
that SHA-256 is injective. The merge laws require `agrees(a, b)` explicitly:
contributions to the same slot carry the same value. To apply this model to
content hashes, the application needs its cryptographic assumption. To apply
it to verdicts, it needs a deterministic local producer. The runnable provider
below enforces the split between peers and local workers, and its query laws
keep the explicit consistency premise.

The merge algebra is stated over contributions and how they apply to a state.
There is no binary merge of two Knowledge snapshots:

| Law | What it establishes |
| --- | --- |
| `merge.commutative` | Two admitted, agreeing contributions can exchange order. |
| `merge.idempotent` | Applying an admitted contribution twice equals applying it once. |
| `mergeAll.appends` | Applying concatenated batches equals applying the batches one after another, for arbitrary lists. So how contributions are grouped into batches does not matter. |
| `mergeAll.orderFree` | The two-contribution batch result follows from commutativity. |
| `mergeAll.swapAdjacent` | The swap stays valid inside arbitrary prefixes and suffixes. |
| `mergeAll.duplicateAdjacent` | An adjacent duplicate can be removed inside arbitrary prefixes and suffixes. |

The ordinary Aver law `runBatches.anySchedule` quantifies over any two finite
lists of batches, with no bound on their lengths. Suppose both hold the same
set of contributions, every contribution is admitted, and all contributions
agree pairwise. Then both schedules produce equal Knowledge from the same
initial state. Order, batch boundaries, empty batches and duplicate counts may
all differ. Its executable premise `covered(xs, ys)` says that each entry of
`xs` occurs in `ys`, is admitted, and agrees with every entry of `ys`.
Coverage in both directions gives equal membership and agreement, and still
allows different multiplicities. `allAdmitted` states the admission
requirement explicitly for both flattened schedules. These predicates look at
contributions only. They do not assume that the resulting states are equal.

All the laws are in [knowledge.av](../examples/knowledge/knowledge.av).
`mergeAll.moveOne`, `absorbMember`, `absorbCovered` and `commuteCovered` build
`sameContributions` out of the local merge laws, and `runBatches.flatten`
lifts that result to arbitrary batches. The source uses ordinary `given`,
`when`, `because` and `using` clauses. The commands above generate and check
the whole proof and report all twenty laws in the normal manifest. The only
axioms used are `propext`, `Classical.choice` and `Quot.sound`.

The Lean exporter first tries to compose the cited facts while keeping
recursive functions opaque. When it needs a list induction, it keeps guards in
the induction motive, generalizes the other state inputs, and proves the base
and step from checked recursive equations and the cited laws. When another
list is a changing accumulator, it also tries generalizing that list. None of
this depends on the example's names or types. It is tested on a separate meter
model and on a growing list accumulator. A step that fails stays an open
obligation, even when every supplied sample passes. The proof search is
bounded. The theorems are universal, and the schedule domain is not bounded.

`body.stable` and `verdict.stable` prove that a known answer survives any
admitted contribution that is consistent with the current state. An unknown
answer may become known. When that happens is left to coordinator policy and
replay. `count.monotone` proves that the body count cannot decrease. The count
is still a changing snapshot and has no stable-answer guarantee.

`body.stableRun` and `verdict.stableRun` extend stability to every finite
sequence of consistent, admitted updates. `consistentRun` states the condition
at each actual intermediate state. It does not assume that admission alone
makes a contribution consistent with an arbitrary initial state. The theorem
applies to every finite prefix of a run that keeps going. It says nothing
about when an unknown answer becomes available, or whether an unfair schedule
ever serves it.

Verdict keys encode the length of the block key before concatenating the block
and context keys. So `("ab", "c")` and `("a", "bc")` land in different slots,
and a regression case puts both into one state.

The Lean map helper proves that updates at distinct keys commute for `Int`,
`String` and `Bool`, with explicit proofs of the ordering properties it relies
on. The model's fallback comparator gets no lawful order instance. The theorem
also covers the unsorted or duplicate-key lists that the Lean model quantifies
over, even though an Aver program cannot build those map representations.

## A running provider and coordinator

The model now sits next to its consumer in `examples/knowledge/`. Run the
whole program, generated coordinator included, from that project root. Its
proof checks all 62 laws universally, with no bounded or open obligations:

```sh
cd examples/knowledge
aver check main.av
aver verify main.av
aver run main.av
aver proof main.av --backend lean --check --sorry-budget 0 -o /tmp/knowledge-proof
aver run main.av --record /tmp/knowledge-recording
aver replay /tmp/knowledge-recording --test --diff
```

`Content` declares the peer and reader operations, and `aver.toml` binds them
to `Stored`. A peer offers two hash/body pairs. `Stored.offer` checks both
hashes with `(admitted(first), admitted(second))!` before it changes any
state. One bad hash rejects the whole batch. Accepted bodies go into the
**same** `Knowledge` model proved above and then join the ordered work queue.
Reversing an admitted, agreeing pair leaves Knowledge unchanged; the queue's
order is kept separate on purpose. When a task starts, all duplicate queue
entries are removed.

The `Validation` job kind binds `Stored.validate`, a pure rule for
illustration: a nonempty body is valid in the fixed `nonempty-v1` context. It
is not Bitcoin block validation. `Work.take` delivers the body hash and the
Boolean result to `Stored.validated`, which turns it into the model's verdict.
No peer operation accepts a verdict. A worker failure records a control error
and adds nothing to Knowledge. Capability boundary layouts stay local to their
contracts, and the internal model's nominal types never cross the job ABI
implicitly.

`Stored.read` uses `!` for the body and verdict lookups. It answers
`Now(Ok(snapshot))` only once both are known. Otherwise it answers
`Later(NextTurn)` or a worker error. Its stable-answer law is about the actual
answer function: under admitted, consistent growth, a successful `Now` keeps
its value. `read.stableHistory` applies this to every finite consistent update
sequence, of any length. `observed` projects exactly that successful `Now`
payload and returns `None` for `Later`, `Then` and errors, so the premise
never compares wake handles. Errors, and the moment an answer becomes known,
carry no stability claim.

The example has two producers and a reader. It rejects a forged hash, handles
duplicates through local Work, and gets the same settled answer after another
delivery. Integration tests run this program on the native VM, as generated
Rust and as wasm. The VM recording is replayed with local work recomputed.

The independent products run inside the answer functions. The generated
coordinator still orders requests, queue updates and result visibility. This
example does not add parallel yielding calls, and it does not make control
decisions commute.

## Generated coordinator laws

The coordinator generator emits proof obligations for its own pure
transitions. A stale answer leaves slots alone and increments the dropped
counter. `Later` preserves the request and the instance. Fresh instances
increase. A full job table offers no new task. When the proof report says
`universal`, these laws already quantify over every state and argument that
satisfies their premises. The `given` values also feed sampled VM
verification; they do not restrict the domain of a successful universal proof.

Generating the laws proves nothing by itself. `aver proof --check` runs the
checker, and the report separates universal proofs from bounded or failed
obligations. Some generated laws cite laws from the program's answer module,
such as `aStartedTaskIsNotAskedAgain`, and that dependency must also prove
universally. Green proofs on the examples do not give a theorem about every
program the generator might be handed.

The generator also emits `__HistoryEvent`, `__historyStep`,
`__historyAdmissible`, and `__historyRun` into this program. Events use the
program's concrete request, answer-state and job-result types. The fold calls
the same pure transitions as the live coordinator: parking, saving an answer,
settling an instance, seating a worker, reporting its result, and
cancellation. Observations of the clock, of stop and of returned answer states
are explicit events. These helpers exist for proof and verification. Normal
execution does not record or allocate a history.

The three generated `__historyRun` laws quantify over arbitrary finite lists:

- `noNewProcesses`: after initial seating, the slot count never increases.
- `jobsStayWithinLimit`: a run that starts within `max-jobs` stays within it,
  counting all job kinds in the same table.
- `retiredInstanceNeverReturns`: once an instance is retired (its process is
  gone or its number has moved on), it stays retired. The per-process
  `answeringRetiresTheInstance` law establishes this premise after a current
  answer is accepted, including a completion that removes the slot.

Each statement holds for any finite prefix, of any length. The admissibility
predicate checks each event against the state left by the events before it. A
start needs positive room. A settlement needs a nonnegative instance, and a
seated slot if that instance is current. It does **not** assume the three
conclusions. The history model deliberately allows more observations than the
live driver can produce, including arbitrary returned answer states, arbitrary
tasks and spurious job reports. That is why these structural properties need
no premise about a provider being honest or consistent. Semantic properties of
a provider's answers still need the provider's own laws, such as
`Stored.read.stableHistory`.

The [source request-trace observers](yield-request-traces.md) check a
supported subset of lowering independently. Recursive helper composition,
tail-entry alignment and correspondence with the effectful driver are still
open. These history laws prove the composed pure transitions. They do not
establish that correspondence, or termination. `runBatches.anySchedule` covers
Knowledge contributions separately. Automatic schedule enumeration remains an
optional execution test.
