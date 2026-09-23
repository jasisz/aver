# Source request-trace correspondence

For a yielding function, the compiler can build two pure, concrete Aver
observers. One comes from the function's retained direct-style source, the
other from the lowered `Start` and `Answer` functions that are actually
generated. When their observations are equal, the ordered requests, arguments,
answers, cursor state, pending requests and completion values all match. Equal
return values alone do not show correspondence.

This is how the compiler validates #1376. It is separate from the
coordinator's finite-history invariants and from provider answer-stability
laws. Recursive helpers compose through explicit helper and splice laws. That
includes imports, where the owning module supplies the source correspondence
and cursor contract. In-place effects in imported segments use pure
observations exported by their owning module. Stubbed direct process cases are
still refused as proofs, as before.

## Two independent inputs to the model generator

`Module.yield_sources` keeps the original stamped definitions from before
lowering, including private helpers and their source lines. A source observer
visits these definitions. It does not reconstruct source behavior from machine
states, liveness or generated continuation bodies. Imported observers stay in
their owning module, where that module's private functions and layouts are
still available.

The protocol observer folds the generated entry points themselves. When a
segment contains an in-place capability call, a separate traversal instruments
that generated segment with the same answer-tape semantics. Each side's code
is observed on its own, and the two observers share only the meaning of
consuming an answer. So a lowering error can change the protocol observation
while the retained source observation stays the same.

## Finite semantic inputs

Each process gets its own monomorphic input, query, event and result types.
For a process named `count` with a `Pool.claim` request, the internal names
include `__CountTraceInput`, `__CountTraceEvent`, `__countSourceTrace` and
`__countProtocolTrace`. Compiler fixtures use these reserved names. They do
not add a trace DSL for users or a generic type facility.

| Input | Meaning |
| --- | --- |
| `AnswerClaim(answer)` | Answer the pending `Pool.claim` with its exact result type. |
| `AnswerHostTimeUnixMs(answer)` | Supply an in-place clock observation. |
| `Advance` | Allow a yielding tail call to resume: self-entry or entry into another yielding helper. |
| `Foreign` | Internal adapter marker for an input kind that a child does not have. |

Inputs are semantic answers and permissions to resume. They are not a shared
instruction budget for an interpreter. Pure evaluation consumes nothing. An
internal yield consumes `Advance`, emits no operation event and consumes no
answer. A request emits an ordered
`ObservedKind(position, arguments..., answer)` event when it is answered.

Tail positions come from the retained source: the last expression of a
yielding function, followed through its `match` arms. Arguments are evaluated
before the boundary. A call whose value feeds into another expression has no
entry pause, though the helper keeps its own internal tail boundaries.
Inlining a finite helper keeps that scope.

For example, `parent(id) = helper(id)` suspends before entering `helper`. An
empty tape reports `pending = Yield`. `[Advance]` enters the helper and stops
at its first unanswered request. The advance leaves `position` and `events`
unchanged and increases only `consumed`. An answer of the wrong kind at this
boundary is rejected and not consumed. A chain of two tail entries needs two
advances. This alignment is what gets checked against the real protocol, and
no instruction-count fuel is added.

A result records:

- `remaining`: the exact unconsumed input suffix;
- `position`: the dynamic executed-answer position across all operation kinds;
- `consumed`: answers plus resumption permissions consumed;
- `events`: observations in execution order;
- `value`: `Some(result)` on completion, otherwise `None`;
- `pending`: the unanswered operation and its arguments, or an internal yield;
- `valid`: false when the next input has the wrong kind.

An empty tape stops at the next pending query. A token of the wrong kind stays
in the suffix. Early completion also keeps the unused suffix. Helper calls
return updated cursors, and branches that do not execute do not advance them.
Import adapters map concrete input and event types by operation identity and
recover the caller's suffix from the change in the child's consumed count.

## Universal checking

A compiler fixture states an ordinary Aver law that compares the source and
protocol observers, quantified over the input parameters and
`List<...TraceInput>`. The generator adds a stronger ordinary law for
`SourceTraceFrom` and `ProtocolTraceFrom`, with an arbitrary initial position,
events and consumed count. The initial-state law cites that auxiliary
obligation explicitly. Separate cursor parameters keep the answer and
resumption counters independent under functional induction.

The normal Lean exporter checks these laws. Recursive root processes use
functional induction over the checked list measure, with arbitrary changed
arguments and accumulated observations. Finite helper chains can expose a
constructor prefix before simplification, and the rest of the list is still
universally quantified. The examples in `given` are executable regression
cases; they do not define the proof domain. Every successful law must show up
as `universal` in the manifest and pass the existing axiom whitelist. No
handwritten Lean companion, extra axiom or larger proof budget is involved.

The regression fixture covers repeated requests, branches, mixed result types,
Unit answers, private helpers, early `Result.Err`, self yields and in-place
clock reads. A second fixture covers repeated calls through imported private
helpers. Tail-entry fixtures also check local and imported pauses, a helper
used in a non-tail expression, branches, and private helpers with the same
name in different modules. False laws that count a pause as an answer, or hide
what it consumed, stay unproved even when the result and the operation events
are unchanged. Negative controls drop, duplicate and reorder events, change
arguments or answers, and reset positions, all while keeping the function's
return value the same.

## Local recursive helper composition

A caller can observe a private recursive helper, resume after it completes,
and call it again with the updated cursor. The generator observes the helper's
real protocol using the caller's concrete input and event types, with the
mapping determined by operation identity. It records the nested-call routers
during lowering and does not infer them from generated names.

Two ordinary Aver laws provide the composition:

- The helper correspondence law equates the helper's retained-source
  observation with its protocol observation, for arbitrary arguments, input
  tape and cursor.
- Each call site's splice law equates observing the nested protocol directly
  with observing the child and then continuing the parent. It quantifies over
  every child outcome, suspended states included, and over every captured
  argument.

The parent's correspondence obligation cites these laws explicitly. A splice
keeps the whole observation: events, remaining input, both counters,
completion value, pending request and validity. Local helpers return the exact
remaining tape, so local composition needs no imported consumed-count/drop
adapter. A pending or invalid child suspends the parent with the child's
cursor.

Lean proves each splice by functional induction over the child's checked input
list measure. It composes those facts at call boundaries before unfolding the
parent's finite continuation, so repeated calls reuse the child's theorem
instead of repeating induction over its history. A finite prefix of in-place
observations is split off only when it would otherwise block applying the
cited equation.

Recursive fixtures check non-tail calls, tail entry, repeated calls, nonzero
initial cursors, nominal arguments, Unit answers, early errors and in-place
clock observations. False laws that reset cursor fields stay unproved even
when completion values are unchanged. These are proof obligations over
arbitrary finite tapes. They are not a fixed collection of generated
schedules.

## Composition across imports

An imported helper uses its own concrete input and event types. The adapter
maps operations by contract identity, and a token the child cannot accept maps
to `Foreign`. The adapter rebuilds the caller's remaining input with
`List.drop`, using the change in the child's consumed count, so the original
token types are kept.

Every source observer that is explicitly requested gets an ordinary cursor law
over **all outcomes**, including every waiting state, for arbitrary tapes and
initial cursors. With `used = observed.consumed - consumed`, that law
establishes:

- `0 <= used <= List.len(inputs)`;
- `observed.remaining == List.drop(inputs, used)`.

The owning module exports the subject of this law and the subject of its
strengthened correspondence law. Import metadata carries their identities. It
does not carry a trusted claim that they have been proved. A recursive
imported observer without these contracts is rejected explicitly. A cited law
with an unproved obligation still fails the normal proof check and the axiom
audit.

The caller generates a mapping law that equates its observation of the child's
actual protocol with the adapted observation from the owning module. This law
quantifies over the event prefixes of both modules, the complete child outcome
and every input. Ordinary append and singleton laws summarize event
conversion. Two explicit `because` steps then apply the owning module's
correspondence and the checked mapping, and the caller's existing splice laws
use the resulting child summary.

Private recursive helpers stay in their original module. A finite exported
wrapper around such a helper carries the same contracts. Fixtures check
repeated imports, tail entry, private recursion, arbitrary initial cursors,
Unit answers, early errors and tokens that belong only to the caller. Those
tokens must stay unchanged when the child rejects them.

An effectful imported `Start` or `Answer` has its own observer, owned by its
module and generated from that actual segment. It can stop on an in-place
operation before it produces a protocol outcome. Its result carries the same
cursor fields and an optional outcome, and the caller adapts the tape and
observations before continuing its real router. This covers effects before the
first request, effects after an answer, and early errors. The retained-source
observer stays independent of these segment bodies.

Every module with a requested trace also checks an interface for each observed
segment, in the module where the segment is defined. `segmentCursor` states
that the observation leaves the tape at the suffix its own cursor reports.
`eventsPrefix` states that its incoming event history is only a prefix. `step`
states that one protocol step equals that observation followed by the generic
continuation (for the start segment, that the protocol trace from a cursor
does). The owner publishes a sample of each protocol state an importer has to
quantify over. A module that lifts an imported observation also checks the
lifted cursor, citing the owner's law. Each statement is about a wrapper and
not about the observation itself, so no observation owns a law and its body
keeps plain matchers.

A caller reads this interface in place of an imported observer's body. This
holds for every proof rung a request-trace law can take, not only for some of
them. Consider a function owned by another module that builds a trace result
record (a record whose fields are the observation cursor): an observation, a
protocol observer, a segment adapter, a lift or a source trace. If some law of
its module reaches it, a caller's proof never unfolds it. It goes into no simp
set, no `grind` list and no `eq_def` rewrite. A rung knows about it exactly
what the laws cited by the caller's law say.

The boundary is decided by shape and by the owner's laws, never by a name. A
router, join or answer function of another module returns an outcome or a
scalar and opens like any pure helper. A finite import whose module states no
law about it is inlined as before, because there is no interface to read
instead. The wrappers an interface law is stated with (the cursor predicate,
the prefixed form, the step continuation) belong to that law's vocabulary. A
rung that reads the citation opens them by the name the citation gives, and
the observation behind them stays closed. There is exactly one equation from
another module that a rung ever applies: the owner's protocol observer,
stepped once per token inside the proof of the mapping law, where the owner
observes no segment and so publishes no step. The routers that this step
exposes are pure and open like any helper.

A caller's mapping law cites the cursor, prefix and step of every observation
it drives through. Its source correspondence crosses one call site at a time.
At each site, the segment observation and the drive result are named, the
splice law folds the protocol side into the site's splice wrapper, and the
wrapper is then opened, so both sides continue as a match on the same named
result. Its cursor contract cites the lifted cursor laws and uses functional
induction on its own protocol observer. It names each observation, each token
read after one and each recursive result, so that the induction hypothesis and
the cited bounds talk about the same variables. What is left is linear cursor
arithmetic and a chain of `drop`s. Between local splices, a
constructor-specific equation still unfolds a completed continuation and keeps
the next unknown outcome for its own theorem. No extra axiom, proof budget,
generated scenario bound or handwritten Lean file is needed.

## Boundary of the claim

This proves that the two generated Aver observations are equal for the
supported source shapes. It assumes that each in-place capability returns the
value supplied by the shared typed tape. It does not prove a provider
implementation, a backend's runtime conformance, scheduling fairness,
liveness, termination of an unbounded run, or equivalence with the
coordinator's host loop. Runtime tests on VM and WASM check the executable
observers separately.

Model requests currently reject recursive imported helpers without
owning-module contracts, effectful independent products, indirect effectful
calls, and imported segments whose in-place effects cannot be observed in
their owning module. Pure source computations keep the ordinary exporter's
recursion and proof requirements. Combining a local recursive source cone with
an imported nested call requires the imported helper's owning-module
contracts, even when that helper is finite. A generated model does not earn
universal credit by itself: an unproved equality stays an open obligation.
Issue #1376 stays open for the broader correspondence between source and
driver.

The owning module records whether its source observer, or any helper it can
reach, is recursive, so a finite caller of a recursive helper keeps this
marker. An internal `Yield` kind is not evidence of recursion on its own,
because a finite tail-entry chain has that kind too. Importing finite
observers therefore keeps those boundaries, and recursive imported subtraces
use the checked composition contracts above.
