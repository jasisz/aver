/-!
What-if, Lean side: does core Lean (no Mathlib) close the low-mask law
`Bits.and(h, 31) == Int.mod(h, 32)` over every Int once the exporter cites
`Nat.and_two_pow_sub_one_eq_mod`? `AverBits.and` is copied verbatim from the
Lean prelude in src/codegen/lean/prelude.rs.
-/
namespace AverBits

def mag (x : Int) : Nat :=
  if x < 0 then (-x - 1).toNat else x.toNat

def and (a b : Int) : Int :=
  let x := mag a
  let y := mag b
  if a < 0 then
    if b < 0 then -((Nat.lor x y : Int)) - 1 else ((y - Nat.land x y : Nat) : Int)
  else
    if b < 0 then ((x - Nat.land x y : Nat) : Int) else ((Nat.land x y : Nat) : Int)

end AverBits

theorem land31 (x : Nat) : Nat.land x 31 = x % 32 :=
  Nat.and_two_pow_sub_one_eq_mod x 5

theorem lowFive_isModThirtyTwo (h : Int) : AverBits.and h 31 = h % 32 := by
  unfold AverBits.and AverBits.mag
  by_cases hh : h < 0
  · simp only [hh, if_true, show ¬((31 : Int) < 0) by decide, if_false,
      show (31 : Int).toNat = 31 from rfl, land31]
    omega
  · simp only [hh, if_false, show ¬((31 : Int) < 0) by decide,
      show (31 : Int).toNat = 31 from rfl, land31]
    omega
