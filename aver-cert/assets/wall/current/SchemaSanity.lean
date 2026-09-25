/-
  Non-vacuity for the acceptance-side schema.

  `CertPreludeSanity` deliberately imports only `CertPrelude` — the decoder
  differential stages it as a standalone three-file package — so anything that
  needs `Schema` lives here instead: the carrier-specification witness, a
  witness that the named host contracts are jointly satisfiable, and S-6
  negatives (ill-typed plans are rejected by the typing the whole statement
  rests on). Like that file, this one is a repo-side
  gate: it is a root of the wall's own lakefile and is NOT in `wall.rs`'s
  `SOURCES` or `PRISTINE_ROOTS`, so it is built by `lake build` in this
  directory and never shipped inside a certificate package.
-/
import SchemaCore

/-! ## Non-vacuity of `CarrierSpec` (`Schema.CarrierSpec`)

`CarrierSpec` is the abstract representation the whole certificate is
quantified over, and `Canon` — the runtime's normal form — was added to it as
a bare predicate with two axioms (`canonSmall`, `canonBig`). An abstract
structure with contradictory fields would make every obligation vacuously
true, so the structure needs a WITNESS: one concrete inhabitant, built here
and checked by the kernel.

The witness models a carrier by its `$small` field on BOTH shapes: a `Small`
word is `carrierSmall C n`, and a limb-carrying word denotes the integer in its
own `$small` field with a sign field that agrees with that integer's sign. That
is not the layout `wat/normalize.wat` uses (the real runtime reads a Big's value
out of its limbs), and it does not need to be — `CarrierSpec` constrains a Big
word only up to the sign facts `bigElim` states, so any model satisfying those
facts proves the field set is jointly satisfiable.

`Canon` here holds EXACTLY where the runtime's normal form does: for a small
carrier whose value is inside the i64 band, and for a limb-carrying word whose
value is OUTSIDE that band and whose sign field is `-1` or `1`. So the witness
also shows the two canonicity axioms do not collapse `Canon` to `True` (an
in-band limb-carrying word is not canonical) or to `False` (both shapes have
canonical inhabitants). -/

namespace AverCert.Schema

open CertPrelude

/-- Representation of the sanity carrier: a small carrier for any integer, or a
    limb-carrying word carrying that integer in its `$small` field, a non-zero
    value, and a sign field agreeing with the value's sign. -/
def sanityRepr (C : Nat) (n : Int) (w : WVal) : Prop :=
  w = carrierSmall C n ∨
    ∃ lty les sg, w = .structv C [.i64v n, .arr lty les, .i32v sg] ∧
      ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0

/-- Normal form of the sanity carrier: an in-band small carrier, or a
    limb-carrying word whose value is out of band with a sign of `-1` or `1`. -/
def sanityCanon (C : Nat) (w : WVal) : Prop :=
  (∃ k : Int, w = carrierSmall C k ∧ -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63) ∨
    (∃ s lty les sg, w = .structv C [.i64v s, .arr lty les, .i32v sg] ∧
      ¬(-(2 ^ 63 : Int) ≤ s ∧ s < 2 ^ 63) ∧ (sg = -1 ∨ sg = 1))

/-- A concrete inhabitant of `CarrierSpec C`: the six fields are jointly
    satisfiable, so no obligation is vacuously true through an empty
    representation type. -/
def sanityCarrierSpec (C : Nat) : CarrierSpec C where
  Repr := sanityRepr C
  Canon := sanityCanon C
  car := by
    intro n v h
    rcases h with rfl | ⟨lty, les, sg, rfl, -, -⟩
    · exact Or.inl ⟨n, 0, rfl⟩
    · exact Or.inr ⟨n, lty, les, sg, rfl⟩
  smallIntro := by
    intro k
    exact Or.inl rfl
  smallElim := by
    intro n s sg h
    rcases h with h | ⟨lty, les, sg', h, -, -⟩
    · simp [carrierSmall] at h
      exact h.1
    · simp at h
  bigElim := by
    intro n s lty les sg h
    rcases h with h | ⟨lty', les', sg', h, hsign, hne⟩
    · simp [carrierSmall] at h
    · simp only [WVal.structv.injEq, List.cons.injEq, WVal.i64v.injEq,
        WVal.arr.injEq, WVal.i32v.injEq] at h
      obtain ⟨-, -, -, hsg, -⟩ := h
      subst hsg
      exact ⟨hsign, hne⟩
  canonSmall := by
    intro k
    constructor
    · rintro (⟨k', h, hlo, hhi⟩ | ⟨s, lty, les, sg, h, -, -⟩)
      · simp only [carrierSmall, WVal.structv.injEq, List.cons.injEq,
          WVal.i64v.injEq] at h
        exact h.2.1 ▸ ⟨hlo, hhi⟩
      · simp [carrierSmall] at h
    · rintro ⟨hlo, hhi⟩
      exact Or.inl ⟨k, rfl, hlo, hhi⟩
  canonBig := by
    intro n s lty les sg hrepr hcanon
    -- The represented value sits in the `$small` field on both shapes, so a
    -- limb-carrying word that represents `n` carries `n` there.
    have hsn : s = n := by
      rcases hrepr with h | ⟨lty', les', sg', h, -, -⟩
      · simp [carrierSmall] at h
      · simp only [WVal.structv.injEq, List.cons.injEq, WVal.i64v.injEq] at h
        exact h.2.1
    subst hsn
    rcases hcanon with ⟨k, h, -, -⟩ | ⟨s', lty', les', sg', h, hband, hsg⟩
    · simp [carrierSmall] at h
    · simp only [WVal.structv.injEq, List.cons.injEq, WVal.i64v.injEq,
        WVal.arr.injEq, WVal.i32v.injEq] at h
      obtain ⟨-, hs, -, hsgEq, -⟩ := h
      subst hs
      subst hsgEq
      exact ⟨hband, by rcases hsg with rfl | rfl <;> simp⟩

#print axioms sanityCarrierSpec

/-- `Canon` is inhabited on the SMALL shape. -/
example (C : Nat) (k : Int) (hlo : -(2 ^ 63 : Int) ≤ k) (hhi : k < 2 ^ 63) :
    sanityCanon C (carrierSmall C k) :=
  Or.inl ⟨k, rfl, hlo, hhi⟩

/-- `Canon` is inhabited on the LIMB-CARRYING shape. -/
example (C lty : Nat) (les : List WVal) (s : Int)
    (hband : ¬(-(2 ^ 63 : Int) ≤ s ∧ s < 2 ^ 63)) :
    sanityCanon C (.structv C [.i64v s, .arr lty les, .i32v 1]) :=
  Or.inr ⟨s, lty, les, 1, rfl, hband, Or.inr rfl⟩

/-- `Canon` is NOT `True`: a limb-carrying word whose value is inside the i64
    band is outside the normal form, which is exactly the pair
    (`Small k`, `Big k`) that would make the two STRUCTURAL helpers disagree. -/
example (C lty : Nat) (les : List WVal) (s sg : Int)
    (hlo : -(2 ^ 63 : Int) ≤ s) (hhi : s < 2 ^ 63) :
    ¬ sanityCanon C (.structv C [.i64v s, .arr lty les, .i32v sg]) := by
  rintro (⟨k, h, -, -⟩ | ⟨s', lty', les', sg', h, hband, -⟩)
  · simp [carrierSmall] at h
  · simp only [WVal.structv.injEq, List.cons.injEq, WVal.i64v.injEq,
      WVal.arr.injEq, WVal.i32v.injEq] at h
    obtain ⟨-, hs, -, -, -⟩ := h
    subst hs
    exact hband ⟨hlo, hhi⟩

/-- `Canon` is NOT `True` for the sign field either: a zero sign is never in
    the normal form, which is what `canonBig` promises the comparison helper. -/
example (C lty : Nat) (les : List WVal) (s : Int) :
    ¬ sanityCanon C (.structv C [.i64v s, .arr lty les, .i32v 0]) := by
  rintro (⟨k, h, -, -⟩ | ⟨s', lty', les', sg', h, -, hsg⟩)
  · simp [carrierSmall] at h
  · simp only [WVal.structv.injEq, List.cons.injEq, WVal.i64v.injEq,
      WVal.arr.injEq, WVal.i32v.injEq] at h
    obtain ⟨-, -, -, hsgEq, -⟩ := h
    subst hsgEq
    rcases hsg with h | h <;> simp at h

/-! ## The named host contracts are jointly satisfiable

Trap-only helpers satisfy every contract of `HostContracts` (a helper that
returns nothing makes its premise vacuous), so the premise surface of
`Obligation.holds` is never contradictory: the obligation is not vacuously
true through its contracts. -/

def trapHost : HostFns :=
  { add := fun _ => none, sub := fun _ => none, mul := fun _ => none, cmp := fun _ => none,
    eq := fun _ => none, stringEq := fun _ => none, stringConcat := fun _ _ => none,
    toIndex := fun _ => none, divmod := fun _ => none }

example (C : Nat) : HostContracts (sanityCarrierSpec C) trapHost where
  add := by intro _ _ _ _ _ _ _ h; cases h
  sub := by intro _ _ _ _ _ _ _ h; cases h
  mul := by intro _ _ _ _ _ _ _ h; cases h
  cmp := by intro _ _ _ _ _ _ _ _ _ h; cases h
  eq := by intro _ _ _ _ _ _ _ _ _ h; cases h
  stringEq := by intro _ _ _ h; cases h
  stringConcat := by intro _ _ _ h; cases h
  divmod := by intro _ _ _ _ _ _ _ _ _ _ _ _ h; cases h
  toIndex := by intro _ _ _ _ h; cases h

/-! ## S-6: typing is load-bearing, and ill-typed plans decline -/

open AverCert.Grammar in
def sanityM : MCtx :=
  { carrier := 2, box := 7, add := 8, sub := 9, mul := 10, neg := 11, cmp := 12, eq := 13,
    structOf := fun _ => 0, recFields := fun _ => none, sigs := fun _ => none }

open AverCert.Grammar in
/-- A well-typed plan: `n + 1` at `Int -> Int`. -/
example : planTyped sanityM
    { sig := ⟨[.int], .int⟩, nslots := 1, locals := [.int],
      body := .binOp .add (.local 0) (.literal (.int 1)) } = true := by decide

open AverCert.Grammar in
/-- `if n then 1 else 0` over an Int condition declines. -/
example : planTyped sanityM
    { sig := ⟨[.int], .int⟩, nslots := 1, locals := [.int],
      body := .ifThenElse (.local 0) (.literal (.int 1)) (.literal (.int 0)) } = false := by
  decide

open AverCert.Grammar in
/-- A declared result type the body does not have declines. -/
example : planTyped sanityM
    { sig := ⟨[.int], .bool⟩, nslots := 1, locals := [.int],
      body := .binOp .add (.local 0) (.literal (.int 1)) } = false := by decide

open AverCert.Grammar in
/-- A call to a function with no plan (no signature) declines. -/
example : planTyped sanityM
    { sig := ⟨[.int], .int⟩, nslots := 1, locals := [.int],
      body := .call (.fn 3) [.local 0] } = false := by decide

open AverCert.Grammar in
/-- An Int literal outside the i64 band declines. -/
example : planTyped sanityM
    { sig := ⟨[], .int⟩, nslots := 0, locals := [.int],
      body := .literal (.int (2 ^ 63)) } = false := by decide

end AverCert.Schema
