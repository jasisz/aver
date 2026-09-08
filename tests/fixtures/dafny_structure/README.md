# Dafny source structure controls

These fixtures exercise translation of pure Aver source, independently of the
larger K5/BTC proof obligations. Run `cargo test --test proof_spec dafny_structure`
with Dafny installed; the harness uses the strict whole-file gate including imports.

| Source group | Positive laws | What it checks |
|---|---:|---|
| containers | 15 | Lists, vectors, extensional maps, Unit, Results, exact integer builtins |
| unit_maps | 3 | Unit-variable values, empty maps and pair-list construction |
| callbacks + imported callbacks | 3 | Checked named functions, invocation and nominal module identity |
| strings/positive | 5 | Exact Int/Bool/String interpolation and named display |
| strings/plain | 7 | Ordinary law templates, given types and scoped match binders |
| strings/text_positive | 8 | Defined Unicode sequence operations and boundary behavior |
| propagation_positive | 5 | Nested `?`, `?!`, records, lists, tuples, updates, branches and first error |
| refinement + refinement_update | 2 | Checked subset invariants and replacement carriers |
| bytes | 4 | Canonical Bytes, literal discharge, concat and endian Result boundaries |

The 52 positive laws include 45 guided laws and seven ordinary laws. Eight
negative files contain ten source laws: false intermediate reasons or invalid
subset constructors/updates. Their finite samples pass; actual Dafny verification
must fail without refusals, axioms, omissions or timeouts excusing the failure.
Float, opaque predicate dependencies and hidden higher-order recursion have
separate refusal controls. Never execute `callbacks_recursive.av`: it is an
export-only control with intentional nontermination.

VM differential tests in `src/codegen/dafny/propagation.rs` compare original and
normalized bodies, including evaluation order, branch selection, discard bindings
and collisions between legal source names and generated Dafny names. Proof
checking additionally validates the generated functions and helper contracts.

This is structural coverage of the pure proof fragment. Exact IEEE Float,
UTF-8/case/parsing helpers, sorted map iteration, arbitrary function-valued givens
and unclassified host effects still need their own semantic models or contracts.
No fixture result grants independent credit to a law in a failing BTC/K5 module.
