# Provenance — TIP-derived proof tasks

The `.av` files in this directory are translations into Aver of inductive
proof problems from **TIP — Tons of Inductive Problems**.

- Upstream: https://github.com/tip-org/benchmarks (commit `bace5b2`)
- Source files: `benchmarks/isaplanner/*.smt2` (and `benchmarks/prod/*`)
- Upstream license: **BSD-3-Clause**, Copyright (c) 2015-2016, Dan Rosén and
  contributors. The full upstream license text is retained verbatim in
  `LICENSE.TIP` in this directory.

**This does NOT relicense anything.** The lumen-rs repository as a whole remains
under the **MIT License** (see the top-level `LICENSE`). The `.av` files in this
directory are DERIVED works (translations / modifications) of BSD-3-Clause
content; `LICENSE.TIP` is the upstream license retained *for attribution only*,
as BSD-3-Clause clause 1 requires for redistributions of (derived) source — it
is not a license grant over this directory or the repo. BSD-3-Clause and MIT are
compatible permissive licenses, so including these derived files in an MIT repo
is fine provided the upstream notice/conditions/disclaimer are retained, which
`LICENSE.TIP` does.

The original problems are from "Case-Analysis for Rippling and Inductive Proof"
(Johansson, Dixon, Bundy, ITP 2010) and the IsaPlanner / CLAM evaluations, as
cited per-file in the upstream `.smt2` headers and each translated file's intent.

Translation conventions: Peano `Nat` → an Aver `type Nat { Z | S(Nat) }`;
polymorphic `list a` monomorphized to `List<Int>`; `++` → `List.concat`;
`nil` → `[]`; `cons h t` → `[h, ..t]`; the `(prove (forall ...))` goal →
a `verify <fn> law <name>` block with sampled `given`s.
