# Structured guided proofs

These are identical-source Lean/Dafny controls, not a whole-project BTC result.

`btc_stackitem_slice.av` copies these complete declarations verbatim from
`domain/stackitem.av` at BTC PR #338 commit
`a6870c9de7280593d3e5a5928ceb62610a2ba316`: `bigEndian`,
`largerPrefixReason`, `validBytes`, `positiveReadReason`, and the laws
`bigEndian.largerPrefixStaysLarger` and `bigEndian.positivePrefixStaysPositive`.
Only the module header changes; unrelated functions, imports, sample verifies,
and laws are omitted. The law bodies, guards, sample domains and reasons are
unchanged. These two laws have a closed local dependency cone. The original
BTC checkout is not modified. Passing this projection establishes portability
of this source slice; it does not establish that all of StackItem passes.

`adt_positive.av` covers a named sum type with a list payload, lists of that
sum type, recursive Bool reasons and selected local citations.
`result_positive.av` covers Result equality and pattern matching, including
an error payload. These are generic backend regression fixtures, not BTC laws.

The negative controls deliberately pass their finite sample domains:
`missing_guard.av` drops the necessary order guard from the accumulator claim;
`recursive_false_reason.av` has a false recursive reason and a true final goal;
`failed_citation.av` cites a false theorem about a structured value. Both proof
backends must reject each complete file with no unsupported-subset decline,
placeholder axioms, or timeouts. A Dafny caller verified against the contract
of a failed supplier receives no standalone proof credit.
