# Imported and arithmetic guidance controls

The three positive source graphs contain eleven laws and twenty-two guided
obligations. `imports_positive` gives local and imported records/sums the same
names and follows imported calls and guarded citations. `transitive_consumer`
puts the guided consumer inside an imported module; its supplier's statement
mentions a third module. `arithmetic_positive.av` covers signed Euclidean
division, dynamic and zero divisors, and quotient recursion with a growing
accumulator. Both backends must verify the complete generated projects.

`imported_false` cites a false supplier from a true caller. `arithmetic_false.av`
contains a false recursive reason with passing samples. Both backends must fail
actual proof checking, without substituting a decline or checker error.

Additional integration controls cover an unsupported body hidden behind a
transitive Int/Bool signature and explicit citations of ordinary laws without
guidance. A selected ordinary supplier must be checked universally under its
original guard. False suppliers and missing guards fail; implicit selection
remains declined. The ordinary positive supplier is a Dafny capability test:
Lean currently classifies that ordinary guarded statement as bounded.

Run `cargo test --test proof_spec dafny_import_div` with Lean and Dafny installed.
The five shared positive/negative cases also belong to
`tools/dafny_guidance_spike.py`; imported cases use explicit module roots and
fingerprint all their Aver inputs.
