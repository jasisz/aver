# Primitive string rendering

`positive.av` checks exact decimal and boolean spellings, unchanged Unicode and
escaped String contents, and equivalence between interpolation and named
`String.fromInt` / `String.fromBool` conversions. Decimal examples include zero,
negative numbers and integers beyond machine width. Five positive laws carry
explicit reasoning; finite samples alone never establish the unrestricted ones.

`false_reason.av` has true final equalities but false intermediate display claims.
Its listed samples pass. Actual proof checking must reject both universal claims,
which prevents a constant-empty/zero/true rendering fallback from earning credit.

`unsupported_float.av` must remain declined for universal guidance. Dafny's real
numbers do not give an exact IEEE-754 formatting model. Structured interpolation
has no implicit renderer in Aver and is already a source type error; callers must
write named functions returning String instead.

## Structural text operations

`text_positive.av` gives eight guided laws for empty needles, reflexive substring
search, non-ASCII character indexing, empty-delimiter splitting, empty-source
splitting, replacing a whole nonempty string, and Unicode trim boundaries.
`text_false.av` has two false intermediate claims whose finite samples pass.
`text_samples.av` separately pins nonoverlapping and empty-delimiter behavior.

The exact helper block is separate from uninterpreted Unicode case/byte-length
operations and the legacy UTF-8 axiom. Ordinary character, split, join and trim
proofs must not import that unrelated axiom. The whitespace predicate is the
25-scalar Unicode White_Space set used by Rust `str::trim`; zero-width space and
BOM are deliberately excluded. Dafny uses its default Unicode scalar char mode.

`plain.av` exercises the same primitive rendering without guided
proof syntax. Its ordinary templates include direct given interpolation, named
conversion results, and scoped Result match binders. The source typechecker
must stamp these templates independently of the literal-expanded sample cases.
