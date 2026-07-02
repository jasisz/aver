// HAND PROOF (labeled, kernel-checked by `dafny verify`) for sigDouble.doubleIsTwice.
// The symmetric Dafny splice demo: this body is spliced between the lemma's `{`
// and `}` and re-checked every build. Z3 also closes this from an empty body, so
// this is the trivial per-backend witness that the Dafny splice + `hand` credit
// works -- a wrong body (see the staleness test) fails `dafny verify` loudly and
// is denied universal credit.
assert sigDouble(x) == x + x;
assert x + x == 2 * x;
