# Source recursion in the proof export

`proof_spec::source_recursion` sends these Aver sources to the Lean export.
Positive runs require all laws to verify, no declined claims or build errors,
and universal credit with only the approved kernel axioms.

- `../guarded_countdown_digits.av`: four laws for a decreasing width and growing
  digit accumulator, including a cited accumulator-prefix lemma.
- `list_fold.av`: three laws for a fold whose accumulator changes; the append law
  carries an extra given that is not a source function parameter. A length-only
  empty list also exercises the absence of an inferred element type.
- `imported/main.av`: the same append argument through an imported supplier,
  beside an unrelated local function with the supplier's bare name.
- `sequence_growth.av`: three guarded base equations and executable examples for
  list padding with one or two elements and string padding with a nonempty literal. The kernel checks the termination
  measure, including negative bounds and overshoot.
- `false_accumulator.av`: the zero-valued samples pass, but the universal law
  drops its accumulator. The checker must reject it and its consumer.

ProofIR tests also reject non-growing steps, moving bounds, and invalid guards,
and check fresh projection names and both recursive accumulator instances.
These fixtures test compiler behavior; they do not establish full BTC coverage.
