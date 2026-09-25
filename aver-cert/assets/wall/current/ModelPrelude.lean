/- ModelPrelude — the checker-owned pieces of the certificate source model.

   A certificate package ships the `aver proof` model of the certified module
   as untrusted DATA, and the checker's token gate refuses every construct in
   package text that can change how later text elaborates: `syntax`,
   `macro_rules`, attributes (`@[simp]`). Two prelude pieces the model needs
   are exactly such constructs, so they live HERE, in the wall, where the
   checker owns the text and pins it by the wall id:

   * `AverBits` — the model of Aver's `Bits.*` builtins over `Int`, with its
     four definitional equations and the complement involution as `@[simp]`
     lemmas (the attribute is what lets the law proofs' `simp` reach them);
   * `aver_sq_nonneg` and the `aver_int_order` tactic — the nonlinear
     order-decision step the law proofs close product inequalities with.
     `aver_int_order` is a `syntax`/`macro_rules` pair, and it is recursive,
     so it cannot be expanded into the proofs that use it.

   The text below is byte-identical to the compiler's prelude constants
   (`LEAN_PRELUDE_AVER_BITS`, `LEAN_PRELUDE_NONLINEAR_NONNEG` in
   `src/codegen/lean/prelude.rs`; a compiler test pins the two together).
   A model file imports this module only when it uses one of the pieces. -/

namespace AverBits

/-- Magnitude of the infinite two's-complement reading of `x`. -/
def mag (x : Int) : Nat :=
  if x < 0 then (-x - 1).toNat else x.toNat

/-- Pointwise conjunction. Both non-negative: plain `Nat` conjunction. One
    negative: `other AND NOT negative`, i.e. `other - (other AND negative)`.
    Both negative: every result bit is `NOT (a OR b)`, so the answer is
    negative with complement magnitude `a OR b`. -/
def and (a b : Int) : Int :=
  let x := mag a
  let y := mag b
  if a < 0 then
    if b < 0 then -((Nat.lor x y : Int)) - 1 else ((y - Nat.land x y : Nat) : Int)
  else
    if b < 0 then ((x - Nat.land x y : Nat) : Int) else ((Nat.land x y : Nat) : Int)

/-- Pointwise disjunction, by the same case split. -/
def or (a b : Int) : Int :=
  let x := mag a
  let y := mag b
  if a < 0 then
    if b < 0 then -((Nat.land x y : Int)) - 1 else -((x - Nat.land x y : Nat) : Int) - 1
  else
    if b < 0 then -((y - Nat.land x y : Nat) : Int) - 1 else ((Nat.lor x y : Nat) : Int)

/-- Pointwise exclusive-or. The magnitudes always xor; only the SIGN of the
    result depends on whether the two sign tails differ. -/
def xor (a b : Int) : Int :=
  let x := mag a
  let y := mag b
  if a < 0 then
    if b < 0 then ((Nat.xor x y : Nat) : Int) else -((Nat.xor x y : Nat) : Int) - 1
  else
    if b < 0 then -((Nat.xor x y : Nat) : Int) - 1 else ((Nat.xor x y : Nat) : Int)

/-- Pointwise complement, which over `Int` is exactly `-x - 1`. -/
def not (a : Int) : Int := -a - 1

def shiftLeft (x n : Int) : Int := x * 2 ^ n.toNat
def shiftRight (x n : Int) : Int := x / 2 ^ n.toNat
def low (x w : Int) : Int := x % 2 ^ w.toNat

/-- The four definitional equations, as `simp` lemmas. Without them a law
    like `Bits.not x = -x - 1` is true by `rfl` yet invisible to the tactic
    portfolio, which unfolds the USER's function and then stalls on an
    opaque-looking `AverBits.*` head. With them, an arithmetic law about the
    bit-level view reduces to an ordinary `Int` goal that `simp` / `omega` /
    `grind` already close. They fire only on `AverBits.*` terms, so no proof
    that never mentions `Bits` is affected. -/
@[simp] theorem not_eq (x : Int) : not x = -x - 1 := rfl
@[simp] theorem shiftLeft_eq (x n : Int) : shiftLeft x n = x * 2 ^ n.toNat := rfl
@[simp] theorem shiftRight_eq (x n : Int) : shiftRight x n = x / 2 ^ n.toNat := rfl
@[simp] theorem low_eq (x w : Int) : low x w = x % 2 ^ w.toNat := rfl

/-- Complementing twice is the identity — stated in the form `simp` actually
    reaches. `not_eq` rewrites innermost-first, so a `not (not x)` goal has
    already become `-(-x - 1) - 1 = x` by the time any lemma about `not`
    could fire; matching THAT shape is what makes the involution close
    without widening the tactic portfolio. The rewrite is terminating and
    matches only this exact term. -/
@[simp] theorem neg_complement_involution (x : Int) : -(-x - 1) - 1 = x := by
  omega

/-! Masks. A law that masks with a literal (`Bits.and(x, 128)`) is read through
    `Nat`: split on the sign of `x`, rewrite `and` with `and_of_nonneg` or
    `and_of_neg`, then take the mask apart into single bits (`nat_land_bit`)
    and low runs (`nat_land_low`), splitting a composite mask at a run
    boundary with `nat_land_split`. What is left is `/` and `%` by literals,
    which `omega` decides. Core Lean only. -/

/-- A nonnegative number masked by a nonnegative literal mask is the `Nat`
    conjunction of the two. -/
theorem and_of_nonneg (a m : Int) (M : Nat) (hM : m = M) (ha : 0 ≤ a) :
    AverBits.and a m = ((a.toNat &&& M : Nat) : Int) := by
  subst hM
  have hm : ¬ ((M : Int) < 0) := by omega
  have hna : ¬ (a < 0) := by omega
  simp only [AverBits.and, AverBits.mag, hm, hna, ite_false, Int.toNat_natCast, Nat.land_eq]

/-- A negative number masked by a nonnegative literal mask: the mask minus the
    mask bits the complement `-a - 1` carries. -/
theorem and_of_neg (a m : Int) (M : Nat) (hM : m = M) (ha : a < 0) :
    AverBits.and a m = ((M - ((-a - 1).toNat &&& M) : Nat) : Int) := by
  subst hM
  have hm : ¬ ((M : Int) < 0) := by omega
  simp only [AverBits.and, AverBits.mag, hm, ha, ite_true, ite_false, Int.toNat_natCast, Nat.land_eq]

/-- A low mask `2^k - 1` keeps the remainder by `2^k`. -/
theorem nat_land_low (x m M k : Nat) (hM : m + 1 = M) (hk : M = 2 ^ k) : x &&& m = x % M := by
  subst hk
  have e : m = 2 ^ k - 1 := by omega
  rw [e, Nat.and_two_pow_sub_one_eq_mod]

/-- A single-bit mask `2^k` keeps that bit of the quotient. -/
theorem nat_land_bit (x m k : Nat) (hm : m = 2 ^ k) : x &&& m = m * (x / m % 2) := by
  subst hm
  apply Nat.eq_of_testBit_eq
  intro i
  rw [Nat.testBit_and, Nat.testBit_two_pow]
  rcases Nat.mod_two_eq_zero_or_one (x / 2 ^ k) with h | h
  · rw [h, Nat.mul_zero, Nat.zero_testBit]
    by_cases hk : k = i
    · subst hk
      rw [Nat.testBit_eq_decide_div_mod_eq, h]
      simp
    · simp [hk]
  · rw [h, Nat.mul_one, Nat.testBit_two_pow]
    by_cases hk : k = i
    · subst hk
      rw [Nat.testBit_eq_decide_div_mod_eq, h]
      simp
    · simp [hk]

/-- A mask `2^j * hi + lo` with `lo < 2^j` splits at bit `j`: the high part
    masks the quotient, the low part the remainder. Applied repeatedly it takes
    any literal mask apart into single bits and low runs. -/
theorem nat_land_split (x m P j hi lo : Nat) (hP : P = 2 ^ j) (hm : m = P * hi + lo)
    (hlo : lo < P) : x &&& m = P * ((x / P) &&& hi) + ((x % P) &&& lo) := by
  subst hP
  subst hm
  have hlt : (x % 2 ^ j) &&& lo < 2 ^ j := Nat.and_lt_two_pow _ hlo
  apply Nat.eq_of_testBit_eq
  intro i
  rw [Nat.testBit_and, Nat.testBit_two_pow_mul_add _ hlo, Nat.testBit_two_pow_mul_add _ hlt]
  by_cases hij : i < j
  · simp [hij, Nat.testBit_and, Nat.testBit_mod_two_pow]
  · simp [hij, Nat.testBit_and, Nat.testBit_div_two_pow,
      Nat.sub_add_cancel (Nat.le_of_not_lt hij)]

end AverBits

/-- A square is never negative — the sign-split base case the product
closer bottoms out on (`Int.mul_self_nonneg` is absent from core Int). -/
theorem aver_sq_nonneg (t : Int) : 0 ≤ t * t := by
  rcases Int.le_total 0 t with h | h
  · exact Int.mul_nonneg h h
  · have h2 : 0 ≤ -t := by omega
    have := Int.mul_nonneg h2 h2
    rwa [Int.neg_mul_neg] at this

/-- Generic nonneg/order decision step for nonlinear Int products: the
`omega`-analog for the products-and-squares fragment. Recurse on a product
with `Int.mul_nonneg` (nonneg goal `0 ≤ a*b`), `Int.mul_pos` (strict goal
`0 < a*b`, the value-magnitude positivity the rounding sign condition needs),
or `Int.mul_le_mul` (product ≤ product),
close a product order whose two sides share their right factor (`a*c ≤ b*c`
from `a ≤ b`, `0 ≤ c`) with `Int.mul_le_mul_of_nonneg_right`, bottom squares
out on `aver_sq_nonneg`, split a conjunctive premise, and discharge the linear
leaves with `omega`. The `mul_pos` rung sits right after `mul_nonneg` (their
conclusions `0 < _` / `0 ≤ _` never unify, so neither shadows the other). The
`mul_le_mul_of_nonneg_right` rung sits BEFORE
`mul_le_mul`, and that order is load-bearing for performance: `mul_le_mul`
would also unify with `a*c ≤ b*c` (taking `d := c`) but spawns a `0 ≤ b` leaf
that is NOT derivable when the law carries no `0 ≤ a` guard. Trying
`mul_le_mul_of_nonneg_right` first closes such a goal directly from `a ≤ b` /
`0 ≤ c` and never spawns `0 ≤ b`; on the squared shapes (`e*e ≤ b*b`, the
contraction's `s²` bound) its shared-right-factor unification fails fast (the
two right factors differ), so `mul_le_mul` still takes them — and any genuine
`0 ≤ b` leaf there is closed by the early `omega` rung from that family's
`0 ≤ e ≤ b` guards. The `mul_le_mul` arm is NOT heartbeat-capped: a
deterministic `whnf` timeout is a HARD, uncatchable failure of a `first`
portfolio at the tactic level — it aborts `lake build` rather than falling
through to the next `first` alternative. `set_option maxHeartbeats … in` only
takes effect at the COMMAND level, never inside a `first | …` tactic
alternative (measured 2026-07-02 across three controlled builds under Lean
4.31: the inline wrapper changed nothing). So this timeout class is not
containable here. What actually keeps this arm from diverging in practice is
the narrower conjunction split below (keyed to the named `h_when` guard rather
than an anonymous `_ ∧ _` match, so it no longer feeds spurious metavariable
products into the product rungs), not any cap. When a timeout does occur its
class is surfaced truthfully by the `--check-json` `build_errors` field; the
named follow-up is driver-level re-emission of the offending law WITHOUT this
arm (a tactic-level cap cannot do it).

The MULTIPLY-BY-POSITIVE rungs (`mul_lt_mul_of_pos_left` / `_right` for a strict
product order `m*a < m*b` / `a*m < b*m`, and `mul_le_mul_of_nonneg_left` for the
nonstrict `m*a ≤ m*b`) sit LAST, after the `<=`-conclusion rungs. They are the
generic non-recursive composition step `omega`/`grind` cannot do — multiplying an
inequality `a < b` by a positive factor `m` — and close any goal already in the
multiplied form `m*a < m*b` from `a < b` (`assumption`) and `0 < m` (the
`mul_pos` recursion on the positive factor). The rational-floor truncation-error
bound (Lemma 7.2.2) ring-bridges its goal into exactly that shape and hands it to
this rung; the same rung is the general non-recursive `mulLeTrans`/`fpMulValue`
composition step. Placed last so their strict (`<`) conclusion never shadows a
`<=`/`0 <=`/`0 <` goal the earlier rungs own (a strict-conclusion lemma cannot
unify with a non-strict goal, but keeping them last also keeps the common
nonneg/positivity search shallow and the output byte-identical for corpora that
never hit a multiplied-form goal).

The final arm splits a named guard conjunction and recurses. It reads the
hypothesis LITERALLY named `h_when` — the order-law emitters
(`law_auto/inequality.rs`, `law_auto/induction/floor_bound.rs`) intro the guard
under exactly that name and `simp … at h_when ⊢` — takes `And.left`/`And.right`,
and recurses. This is a NAMING CONTRACT: any new order-law emitter that renames
the guard makes this arm silently no-op (no `h_when` in context), and the goal
falls to `sorry`. It also peels ONE level only (measured): a right-nested guard
of three-plus conjuncts (`A ∧ (B ∧ C)`) yields `h_when_left := A` /
`h_when_right := B ∧ C`, leaving the inner conjunction bundled. -/
syntax "aver_int_order" : tactic
macro_rules
  | `(tactic| aver_int_order) => `(tactic|
      first
        | assumption
        | omega
        | exact aver_sq_nonneg _
        | (apply Int.mul_nonneg <;> aver_int_order)
        | (apply Int.mul_pos <;> aver_int_order)
        | (apply Int.mul_le_mul_of_nonneg_right <;> aver_int_order)
        | (apply Int.mul_le_mul <;> aver_int_order)
        | (apply Int.mul_lt_mul_of_pos_left <;> aver_int_order)
        | (apply Int.mul_lt_mul_of_pos_right <;> aver_int_order)
        | (apply Int.mul_le_mul_of_nonneg_left <;> aver_int_order)
        | (have h_when_left := And.left h_when
           have h_when_right := And.right h_when
           clear h_when
           aver_int_order))
