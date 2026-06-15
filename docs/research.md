# Research Context

This is a deliberately narrow related-work list. These papers are not
implementation dependencies and Aver does not claim novelty for typed effects,
handlers, capabilities, traces, or proof assistants themselves. They are the
closest research context for Aver's design choices.

## Typed Effects

- John M. Lucassen and David K. Gifford, **Polymorphic Effect Systems**
  (POPL 1988). Effects are tracked statically as part of a type-like
  description of computation, including scheduling constraints for parallel
  programs. Aver's method-level `! [Effect.method]` signatures are much simpler,
  but they sit in this lineage.
  https://doi.org/10.1145/73560.73564

- Daan Leijen, **Koka: Programming with Row-Polymorphic Effect Types**
  (MSFP 2014). Koka is the closest practical reference for a language where
  effects are visible in function types. Aver intentionally avoids row
  polymorphism and effect inference for now; effects stay explicit and concrete.
  https://doi.org/10.4204/EPTCS.153.8

## Effects, Handlers, and Oracles

- Gordon Plotkin and Matija Pretnar, **Handling Algebraic Effects**
  (LMCS 2013). Algebraic handlers interpret effectful computations. Aver's
  Oracle is not a general handler system; it is a closed, auditable subset where
  classified built-in effects become explicit stubs or trace events.
  https://doi.org/10.2168/LMCS-9(4:23)2013

- Li-yao Xia, Yannick Zakowski, Paul He, Chung-Kil Hur, Gregory Malecha,
  Benjamin C. Pierce, and Steve Zdancewic, **Interaction Trees: Representing
  Recursive and Impure Programs in Coq** (POPL 2020). Interaction trees model
  impure recursive programs as uninterpreted events plus interpreters inside a
  proof assistant. Aver takes a narrower route: classified effects are lifted to
  explicit oracle parameters and structured traces for Lean/Dafny export.
  https://doi.org/10.1145/3371119

## Capabilities and External Resources

- Jonathan Immanuel Brachthäuser, Philipp Schuster, Edward Lee, and Aleksander
  Boruch-Gruszecki, **Effects, Capabilities, and Boxes: From Scope-Based
  Reasoning to Type-Based Reasoning and Back** (OOPSLA 2022). This is directly
  relevant to reasoning about external resources through capabilities and
  effects. Aver uses named effects rather than capability capture checking, but
  the problem boundary is shared.
  https://doi.org/10.1145/3527320

- Aleksander Boruch-Gruszecki, Martin Odersky, Edward Lee, Ondrej Lhotak, and
  Jonathan Immanuel Brachthäuser, **Capturing Types** (TOPLAS 2023). Capture
  tracking gives a type-level account of free variables and scoped
  capabilities, including effect polymorphism. Aver's current design is less
  expressive and more explicit, but its effect contracts address related
  resource-reasoning concerns.
  https://doi.org/10.1145/3618003

## Independent Products and Replay

- I. J. J. Aalbersberg and Grzegorz Rozenberg, **Theory of Traces**
  (Theoretical Computer Science, 1988). Trace theory reconciles sequential
  observations with nonsequential causality in concurrent systems. Aver's
  `!` / `?!` model is not a trace-theory implementation, but its branch paths
  and replay matching use the same core idea: preserve stable structure while
  allowing independent actions to reorder.
  https://doi.org/10.1016/0304-3975(88)90051-5

## Proof Targets

- Leonardo de Moura, Soonho Kong, Jeremy Avigad, Floris van Doorn, and Jakob
  von Raumer, **The Lean Theorem Prover (System Description)** (CADE 2015).
  Lean is Aver's kernel-checked proof-export target.
  https://doi.org/10.1007/978-3-319-21401-6_26

- K. Rustan M. Leino, **Dafny: An Automatic Program Verifier for Functional
  Correctness** (LPAR 2010). Dafny is Aver's SMT-backed automated verification
  target for `aver proof --backend dafny`.
  https://doi.org/10.1007/978-3-642-17511-4_20

## Proof Method Lineage

- Matt Kaufmann, Panagiotis Manolios, and J Strother Moore, **Computer-Aided
  Reasoning: An Approach** (Kluwer Academic Publishers, 2000). The home of
  "The Method": steering a deterministic prover by stating lemmas in the same
  executable language as the code — never by writing tactics — plus three
  decades of rewrite-rule discipline (rule orientation, loop-stoppers, free
  variables in hypotheses). Aver's law-driven auto-prover independently
  reconverged on this terrain; the ACL2 literature is the closest map of its
  failure modes. Aver differs in trust model: proofs land as kernel-checked
  Lean certificates rather than relying on a trusted prover, and every lemma
  is a runnable test before it is a theorem.
  https://doi.org/10.1007/978-1-4615-4449-4

- Warren A. Hunt Jr., Matt Kaufmann, J Strother Moore, and Anna Slobodová,
  **Industrial Hardware and Software Verification with ACL2** (Philosophical
  Transactions of the Royal Society A 375, 2017). The industrial track record
  of the lemma-driven method, from the AMD K5 floating-point division proof
  onward. Aver does not target hardware; the shared surface is the workflow
  economics of a prover steered by source-language lemmas.
  https://doi.org/10.1098/rsta.2015.0399

## Integer Range Analysis

- Raphael Ernani Rodrigues, Victor Hugo Sperle Campos, and Fernando Magno
  Quintão Pereira, **A Fast and Low-Overhead Technique to Secure Programs
  Against Integer Overflows** (CGO 2013). A sparse range analysis proves the
  large majority of integer operations stay within machine-word bounds, so the
  few that remain can be guarded cheaply. Aver's unboxing analysis shares the
  goal of proving an integer stays in i64 range, but uses it for representation
  selection — a provably-bounded `Int` lowers to a native `i64` instead of the
  default arbitrary-precision carrier — rather than to insert runtime overflow
  checks. Aver has no loops (tail recursion only), so the paper's loop-header
  "future bounds" do not apply directly; Aver bounds a counter through a
  bounded-tail-recursion recognizer over its own interval domain, and is
  fail-closed (an unproven value stays arbitrary-precision).
  https://doi.org/10.1109/CGO.2013.6494996

## Not Currently Claimed

Aver is also broadly adjacent to monads, property-based testing, model checking,
and deterministic replay literature. Those areas matter, but they are not listed
above unless the connection is direct enough to explain a current Aver design
surface.
