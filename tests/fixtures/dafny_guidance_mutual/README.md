# Native mutual recursion with a growing accumulator

`positive.av` alternates between scan and accept states. The scan peels its input
list; accept may prepend to a separate accumulator. The guided laws state the
exact source transitions: consuming the head enters accept, and accepting a
nonnegative head returns to scan with that head prepended to the accumulator.
Only the remaining input belongs in the termination measure; counting the
growing accumulator would reject a terminating source program.

`imported/main.av` uses identical processing functions from another module.
Both positive sources require universal law and step credit, without a fuel-bound
model or an assumed termination fact. These one-step laws validate the native
mutual termination feature; they do not claim a general mutual induction tactic.

`false_reason.av` has a true final equality but a false intermediate assertion
that every accepted head is zero. Its finite samples pass; universal proof must
fail. `nonshrinking.av` passes the whole input unchanged around its cycle, while
growing the accumulator. It must remain declined and is never executed by the
finite-sample test.
