# Source request-trace correspondence

The compiler can build two pure, concrete Aver observers for a yielding
function: one from its retained direct-style source and one from the actual
lowered `Start` and `Answer` functions. Equality of their observations checks
ordered requests, arguments, answers, cursor state, pending requests and
completion values. Equal return values alone do not establish correspondence.

This is the compiler validation surface for #1376. It is separate from the
coordinator's finite-history invariants and from provider answer-stability
laws. Recursive helpers compose through explicit helper and splice laws,
including imports whose owning module supplies its source correspondence and
cursor contract. Imported segments with in-place effects still need an
owning-module observation interface; the compiler rejects those model requests
explicitly. The existing refusal to export stubbed direct process
cases as proofs remains in place.

## Two independent inputs to the model generator

`Module.yield_sources` retains the original stamped definitions before lowering,
including private helpers and their source lines. A source observer visits
these definitions; it does not reconstruct source behavior from machine states,
liveness or generated continuation bodies. Imported observers stay in their
owning module, where their private functions and layouts remain available.

The protocol observer folds the actual generated entry points. When a local
segment contains an in-place capability call, a separate traversal instruments
that generated segment with the same answer-tape semantics. This observes code
on each side independently; both observers share the meaning of consuming an
answer. A lowering error can therefore change the protocol observation without
changing the retained source observation.

## Finite semantic inputs

Each process gets its own monomorphic input, query, event and result types.
For a process named `count` with a `Pool.claim` request, the internal names
include `__CountTraceInput`, `__CountTraceEvent`, `__countSourceTrace` and
`__countProtocolTrace`. These reserved names are used by compiler fixtures;
this does not add a user-facing trace DSL or generic type facility.

| Input | Meaning |
| --- | --- |
| `AnswerClaim(answer)` | Answer the pending `Pool.claim` with its exact result type. |
| `AnswerHostTimeUnixMs(answer)` | Supply an in-place clock observation. |
| `Advance` | Permit a yielding tail call to resume: self-entry or entry into another yielding helper. |
| `Foreign` | Internal adapter marker for an input kind unavailable in a child. |

Inputs are semantic answers and resumptions, not a shared interpreter instruction
budget. Pure evaluation consumes nothing. An internal yield consumes `Advance`
but emits no operation event and consumes no answer. Requests emit an ordered
`ObservedKind(position, arguments..., answer)` event when answered.

Tail positions come from the retained source: the last expression of a yielding
function, through its `match` arms. Arguments run before the boundary. A call
whose value feeds another expression has no entry pause; its helper still keeps
its own internal tail boundaries. Inlining a finite helper preserves that scope.

For example, `parent(id) = helper(id)` suspends before entering `helper`.
An empty tape reports `pending = Yield`; `[Advance]` enters the helper and stops
at its first unanswered request. The advance preserves `position` and `events`,
and increases only `consumed`. A wrong-kind answer at this boundary is rejected
without consumption. A chain of two tail entries needs two advances. This is the
alignment checked against the real protocol; no instruction-count fuel is added.

A result records:

- `remaining`: the exact unconsumed input suffix;
- `position`: the dynamic executed-answer position across all operation kinds;
- `consumed`: answers plus resumption permissions consumed;
- `events`: observations in execution order;
- `value`: `Some(result)` on completion, otherwise `None`;
- `pending`: the unanswered operation and its arguments, or an internal yield;
- `valid`: false when the next input has the wrong kind.

An empty tape stops at the next pending query. A wrong-kind token remains in the
suffix. Early completion also preserves the unused suffix. Helper calls return
updated cursors; branches that do not execute do not advance them. Import
adapters map concrete input and event types by operation identity and recover
the caller's suffix from the child's consumed-count delta.

## Universal checking

A compiler fixture states an ordinary Aver law comparing source and protocol
observers, quantified over the input parameters and `List<...TraceInput>`.
The generator adds a stronger ordinary law for `SourceTraceFrom` and
`ProtocolTraceFrom`, with arbitrary initial position, events and consumed
count. The initial-state law explicitly cites that auxiliary obligation.
Distinct cursor parameters keep answer and resumption counters independent in
functional induction.

The normal Lean exporter checks these laws. Recursive root processes use
functional induction over the checked list measure, with arbitrary changed
arguments and accumulated observations. Finite helper chains can expose a
constructor prefix before simplification; their remaining list tail is still
universally quantified. The examples in `given` provide executable regression
cases, not the proof domain. Every successful law must appear as `universal`
in the manifest and pass the existing axiom whitelist; no handwritten Lean
companion, added axiom or larger proof budget is involved.

The regression fixture covers repeated requests, branches, mixed result types,
Unit answers, private helpers, early `Result.Err`, self yields and in-place
clock reads. A second fixture covers repeated calls through imported private
helpers. Tail-entry fixtures additionally check local and imported pauses, a
helper used in a non-tail expression, branches, and same-named private helpers
in different modules. False laws that count a pause as an answer or hide its
consumption stay unproved even when the result and operation events are unchanged. Negative controls drop, duplicate and reorder events, change arguments
or answers, and reset positions while preserving the function's return value.

## Local recursive helper composition

A caller can observe a private recursive helper, resume after its completion,
and call it again with the updated cursor. The generator observes the helper's
actual protocol using the caller's concrete input and event types; operation
identity determines the mapping. It records the actual nested-call routers
when lowering instead of inferring them from generated names.

Two ordinary Aver laws provide the composition:

- The helper correspondence law equates its retained-source observation with
  its protocol observation for arbitrary arguments, input tape and cursor.
- Each call site's splice law equates observing the nested protocol directly
  with observing the child and then continuing the parent. It quantifies over
  every child outcome, including suspended states, and every captured argument.

The parent's correspondence obligation explicitly cites these laws. A splice
preserves the complete observation: events, remaining input, both counters,
completion value, pending request and validity. Local helpers return the exact
remaining tape, so local composition needs no imported consumed-count/drop
adapter. A pending or invalid child suspends the parent with that child's cursor.

Lean proves each splice by functional induction over the child's checked input
list measure. It composes those facts at call boundaries before unfolding the
parent's finite continuation. Repeated calls therefore reuse the child theorem
instead of repeating induction over its history. A finite prefix of in-place
observations is split only when it prevents applying the cited equation.

Recursive fixtures check non-tail calls, tail entry, repeated calls, nonzero
initial cursors, nominal arguments, Unit answers, early errors and in-place
clock observations. False laws that reset cursor fields remain unproved even
when completion values are unchanged. These are proof obligations for arbitrary
finite tapes, not a fixed collection of generated schedules.

## Composition across imports

An imported helper uses its own concrete input and event types. The adapter
maps operations by contract identity; a token that the child cannot accept maps
to `Foreign`. It reconstructs the caller's remaining input with `List.drop`
using the child's consumed-count delta, preserving the original token types.

Every explicitly requested source observer gains an ordinary cursor law over
**all outcomes**, including every waiting state, and arbitrary tapes and initial
cursors. If `used = observed.consumed - consumed`, that law establishes:

- `0 <= used <= List.len(inputs)`;
- `observed.remaining == List.drop(inputs, used)`.

The owning module exports the law subject and its strengthened correspondence
subject. Import metadata carries their identities, not a trusted assertion that
they have been proved. A recursive imported observer without these contracts is
rejected explicitly. A cited law with an unproved obligation still fails the
normal proof check and axiom audit.

The caller generates a mapping law equating its observation of the child's
actual protocol with the adapted owning-module observation. This law quantifies
over both modules' event prefixes, the complete child outcome and every input.
Ordinary append and singleton laws summarize event conversion. Two explicit
`because` steps then apply the owning-module correspondence and the checked
mapping. The caller's existing splice laws use the resulting child summary.

Private recursive helpers remain in their original module. A finite exported
wrapper around such a helper carries the same contracts. Fixtures check repeated
imports, tail entry, private recursion, arbitrary initial cursors, Unit answers,
early errors and tokens belonging only to the caller. The latter must remain
unchanged when the child rejects them.

Proof search keeps a summarized adapter opaque until its theorem rewrites the
call. Between splices, a constructor-specific equation unfolds a completed
continuation while preserving the next unknown outcome for its own theorem.
No extra axiom, proof budget, generated scenario bound or handwritten Lean file
is required.

## Boundary of the claim

This proves equality of the two generated Aver observations for the supported
source shapes. It assumes each in-place capability returns the value supplied
by the shared typed tape. It does not prove a provider implementation, a backend's
runtime conformance, scheduling fairness, liveness, termination of an unbounded
run, or equivalence with the coordinator's host loop. Runtime tests on VM and
WASM are separate checks of the executable observers.

Model requests currently reject recursive imported helpers without owning-module
contracts, effectful independent products, indirect effectful calls and imported segments
whose in-place effects cannot be observed in their owning module. Pure source
computations retain the ordinary exporter's recursion and proof requirements.
Combining a local recursive source cone with an imported nested call requires
the imported helper's owning-module contracts even when that helper is finite.
A generated model is not itself universal credit: an unproved equality remains
an open obligation. #1376 stays open for imported in-place effects and the
broader source/driver correspondence.

The owning module records whether its source observer or any reachable helper
is recursive. A finite caller of a recursive helper therefore retains this marker. An internal
`Yield` kind is not itself evidence of recursion: a finite tail-entry chain also
has that kind. Importing finite observers therefore retains those boundaries;
recursive imported subtraces still require the pending composition theorem.
