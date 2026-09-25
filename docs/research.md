# Research Context

This related-work list is kept narrow on purpose. None of these papers is an
implementation dependency, and Aver does not claim novelty for typed effects,
handlers, capabilities, traces or proof assistants themselves. They are the
research closest to Aver's design choices.

## Typed Effects

- John M. Lucassen and David K. Gifford, **Polymorphic Effect Systems** (POPL
  1988). Effects are tracked statically as part of a type-like description of
  a computation, including scheduling constraints for parallel programs.
  Aver's method-level `! [Effect.method]` signatures are much simpler, but
  they come from this line of work.
  https://doi.org/10.1145/73560.73564

- Daan Leijen, **Koka: Programming with Row-Polymorphic Effect Types** (MSFP
  2014). Koka is the closest practical reference for a language where effects
  are visible in function types. For now Aver deliberately avoids row
  polymorphism and effect inference, so effects stay explicit and concrete.
  https://doi.org/10.4204/EPTCS.153.8

## Effects, Handlers, and Oracles

- Gordon Plotkin and Matija Pretnar, **Handling Algebraic Effects** (LMCS
  2013). Algebraic handlers interpret effectful computations. Aver's Oracle is
  a closed, auditable subset of that idea and not a general handler system:
  classified built-in effects become explicit stubs or trace events.
  https://doi.org/10.2168/LMCS-9(4:23)2013

- Li-yao Xia, Yannick Zakowski, Paul He, Chung-Kil Hur, Gregory Malecha,
  Benjamin C. Pierce, and Steve Zdancewic, **Interaction Trees: Representing
  Recursive and Impure Programs in Coq** (POPL 2020). Interaction trees model
  impure recursive programs inside a proof assistant as uninterpreted events
  plus interpreters. Aver takes a narrower route. It lifts classified effects
  to explicit oracle parameters and structured traces for Lean export.
  https://doi.org/10.1145/3371119

## Capabilities and External Resources

- Jonathan Immanuel Brachthäuser, Philipp Schuster, Edward Lee, and Aleksander
  Boruch-Gruszecki, **Effects, Capabilities, and Boxes: From Scope-Based
  Reasoning to Type-Based Reasoning and Back** (OOPSLA 2022). This bears
  directly on reasoning about external resources through capabilities and
  effects. Aver uses named effects instead of capability capture checking, but
  both deal with the same problem.
  https://doi.org/10.1145/3527320

- Aleksander Boruch-Gruszecki, Martin Odersky, Edward Lee, Ondrej Lhotak, and
  Jonathan Immanuel Brachthäuser, **Capturing Types** (TOPLAS 2023). Capture
  tracking gives a type-level account of free variables and scoped
  capabilities, including effect polymorphism. Aver's current design is less
  expressive and more explicit, but its effect contracts address related
  concerns about reasoning over resources.
  https://doi.org/10.1145/3618003

## Independent Products and Replay

- I. J. J. Aalbersberg and Grzegorz Rozenberg, **Theory of Traces**
  (Theoretical Computer Science, 1988). Trace theory reconciles sequential
  observations with nonsequential causality in concurrent systems. Aver's `!`
  / `?!` model does not implement trace theory, but its branch paths and
  replay matching rest on the same idea: keep the stable structure and let
  independent actions reorder.
  https://doi.org/10.1016/0304-3975(88)90051-5

## Proof Targets

- Leonardo de Moura, Soonho Kong, Jeremy Avigad, Floris van Doorn, and Jakob
  von Raumer, **The Lean Theorem Prover (System Description)** (CADE 2015).
  Lean is the target Aver exports proofs to for kernel checking.
  https://doi.org/10.1007/978-3-319-21401-6_26

## Proof Method Lineage

- Matt Kaufmann, Panagiotis Manolios, and J Strother Moore, **Computer-Aided
  Reasoning: An Approach** (Kluwer Academic Publishers, 2000). This is where
  "The Method" comes from: you steer a deterministic prover by stating lemmas
  in the same executable language as the code, and never by writing tactics.
  The book also carries three decades of rewrite-rule discipline (rule
  orientation, loop-stoppers, free variables in hypotheses). Aver's law-driven
  auto-prover arrived at the same approach independently, and the ACL2
  literature is the best guide to how it fails. The trust model differs.
  Aver's proofs end up as kernel-checked Lean certificates instead of relying
  on a trusted prover, and every lemma runs as a test before it becomes a
  theorem.
  https://doi.org/10.1007/978-1-4615-4449-4

- Warren A. Hunt Jr., Matt Kaufmann, J Strother Moore, and Anna Slobodová,
  **Industrial Hardware and Software Verification with ACL2** (Philosophical
  Transactions of the Royal Society A 375, 2017). The industrial record of the
  lemma-driven method, starting with the AMD K5 floating-point division proof.
  Aver does not target hardware. What it shares with this work is the cost
  structure of a workflow where source-language lemmas steer the prover.
  https://doi.org/10.1098/rsta.2015.0399

## Integer Range Analysis

- Raphael Ernani Rodrigues, Victor Hugo Sperle Campos, and Fernando Magno
  Quintão Pereira, **A Fast and Low-Overhead Technique to Secure Programs
  Against Integer Overflows** (CGO 2013). A sparse range analysis proves that
  most integer operations stay within machine-word bounds, so the few that
  remain can be guarded cheaply. Aver's unboxing analysis also proves that an
  integer stays in i64 range, but it uses the result to choose a
  representation instead of inserting runtime overflow checks: a provably
  bounded `Int` lowers to a native `i64` instead of the default
  arbitrary-precision carrier. Aver has no loops (only tail recursion), so the
  paper's loop-header "future bounds" do not carry over directly. Aver bounds
  a counter with a recognizer for bounded tail recursion over its own interval
  domain. It is fail-closed: a value it cannot prove bounded stays
  arbitrary-precision.
  https://doi.org/10.1109/CGO.2013.6494996

## Not Currently Claimed

Aver is also broadly adjacent to the literature on monads, property-based
testing, model checking and deterministic replay. Those areas matter, but work
is listed above only when the connection is direct enough to explain part of
Aver's current design.
