# Source request-trace correspondence

The compiler can build two pure, concrete Aver observers for a yielding
function: one from its retained direct-style source and one from the actual
lowered `Start` and `Answer` functions. Equality of their observations checks
ordered requests, arguments, answers, cursor state, pending requests and
completion values. Equal return values alone do not establish correspondence.

This is the compiler validation surface for #1376. It is separate from the
coordinator's finite-history invariants and from provider answer-stability
laws. Recursive helper composition and imported segments with in-place
effects still need compositional theorems; the compiler rejects those model
requests explicitly. The existing refusal to export stubbed direct process
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
| `Advance` | Permit a source self-tail-call to resume across an internal yield. |
| `Foreign` | Internal adapter marker for an input kind unavailable in a child. |

Inputs are semantic answers and resumptions, not a shared interpreter instruction
budget. Pure evaluation consumes nothing. An internal yield consumes `Advance`
but emits no operation event and consumes no answer. Requests emit an ordered
`ObservedKind(position, arguments..., answer)` event when answered.

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
helpers. Negative controls drop, duplicate and reorder events, change arguments
or answers, and reset positions while preserving the function's return value.

## Boundary of the claim

This proves equality of the two generated Aver observations for the supported
source shapes. It assumes each in-place capability returns the value supplied
by the shared typed tape. It does not prove a provider implementation, a backend's
runtime conformance, scheduling fairness, liveness, termination of an unbounded
run, or equivalence with the coordinator's host loop. Runtime tests on VM and
WASM are separate checks of the executable observers.

Model requests currently reject tail entry into another yielding helper (which
needs alignment of the inserted internal yield), recursive local/imported helper subtraces,
effectful independent products, indirect effectful calls and imported segments
whose in-place effects cannot be observed in their owning module. Pure source
computations retain the ordinary exporter's recursion and proof requirements.
A generated model is not itself universal credit: an unproved equality remains
an open obligation. #1376 stays open for recursive helper splice invariants and
compositional dependencies across those boundaries.
