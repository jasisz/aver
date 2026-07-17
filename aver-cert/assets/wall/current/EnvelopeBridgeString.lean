/-
The positive semantic bridge for the lower-pinned string-concat result type.

`StringResultDerisk` established the FORGE-death direction: a `resultTy` pointed
at a wrong-element type entry is refuted at the pin, and a declared wrong
element cannot even state the pin (`lower` fails closed). This module supplies
the complementary POSITIVE direction: the `resultTy` pin FORCES the byte entry
at `resultTy` to the plan-declared canonical i8 array, and — composed with the
fact that the concat model's output element tag IS the free `resultTy` — pins
the string-concat face's result element type to the plan-declared one, so the
output-tag model equals the plan.

The string-result `EnvelopeFact` is the third instance of the shared
`EnvelopeLowering` abstraction (after the ADT rec-group and the ADT
signatures), reused verbatim: `Req`, `lower`, `EnvelopeFact.pinnedAt`,
`bytesPinnedAt`, `typeCursorAt`. There is no byte -> structure decoder in the
meaning path — only the shared cursor navigation.

Self-verified `#print axioms` at the foot of the file: `[propext]`.
-/
import EnvelopeLowering
import StringSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering.StringBridge

open AverCert.Schema
open CertPrelude
open AverCert.EnvelopeLowering

/-! ## §1 The string-result `EnvelopeFact` (third instance, same abstraction) -/

structure StringResultEnvelope where
  elem : Nat
deriving Repr, DecidableEq

/-- The string result array element is canonically i8 (`0x78`). Fail-closed on
    any other element. -/
def checkStringResultEnvelope (e : StringResultEnvelope) : Bool := e.elem == 0x78

/-- LOWER: the declared result array type's type-section entry, `(array elem mut)`. -/
def lowerStringResultTy (e : StringResultEnvelope) : Option (List Nat) :=
  if checkStringResultEnvelope e then some [0x5e, e.elem, 0x01] else none

/-- The string-result `EnvelopeFact`, pinned at the helper's declared result
    type index `resultTy`. -/
def stringResultTyFact : EnvelopeFact := ⟨StringResultEnvelope, lowerStringResultTy⟩

/-! ## §2 The positive lemmas -/

/-- The `resultTy` pin FORCES the declared element to the canonical i8 and the
    byte entry at `resultTy` to `(array i8 mut)`. This is the positive analogue
    of `EnvelopeBridge.env_intDispatch_bridge`: the pin leaves the producer zero
    choice on the result element type. -/
theorem stringResult_pin_forces_i8
    (env : StringResultEnvelope) (modBytes modLen resultTy : Nat)
    (hpin : stringResultTyFact.pinnedAt modBytes modLen resultTy env) :
    env.elem = 0x78 ∧
      bytesPinnedAt modBytes modLen resultTy [0x5e, 0x78, 0x01] := by
  obtain ⟨bs, hlow, hbp⟩ := hpin
  -- `stringResultTyFact.lower ≡ lowerStringResultTy`, check-gated
  have hlow' : lowerStringResultTy env = some bs := hlow
  unfold lowerStringResultTy at hlow'
  by_cases hc : checkStringResultEnvelope env = true
  · rw [if_pos hc] at hlow'
    have helem : env.elem = 0x78 := by
      unfold checkStringResultEnvelope at hc
      simpa using hc
    have hbs : bs = [0x5e, 0x78, 0x01] := by
      rw [helem] at hlow'
      exact (Option.some.inj hlow').symm
    subst hbs
    exact ⟨helem, hbp⟩
  · rw [if_neg (by simpa using hc)] at hlow'
    exact absurd hlow' (by simp)

/-- The concat model's output element tag IS the free `resultTy` (the bug site):
    the container element tag is discarded, the OUTPUT is `.arr resultTy _`. -/
theorem model_output_tag_is_resultTy
    (resultTy containerTy : Nat) (plan : StringConcatRawPlan)
    (input : WVal) (bytes : List WVal)
    (hcat : stringConcatParts (StringSoundness.evalConcatParts resultTy plan input)
      = some bytes) :
    StringSoundness.evalStringConcat resultTy containerTy plan input
      = .arr resultTy bytes := by
  simp only [StringSoundness.evalStringConcat, stringConcatW, hcat]
  rfl

/-- The positive string-concat bridge: on the declared envelope, the `resultTy`
    pin forces the model output element type to the plan-declared canonical i8
    array, and the concat model output is exactly `.arr resultTy bytes` (the tag
    now byte-pinned). This is the residual `stringConcatSemanticBridge` fact
    that acceptance-soundness needs: the output-tag model equals the plan. -/
theorem env_stringConcat_bridge
    (env : StringResultEnvelope) (modBytes modLen resultTy containerTy : Nat)
    (plan : StringConcatRawPlan) (input : WVal) (bytes : List WVal)
    (hpin : stringResultTyFact.pinnedAt modBytes modLen resultTy env)
    (hcat : stringConcatParts (StringSoundness.evalConcatParts resultTy plan input)
      = some bytes) :
    StringSoundness.evalStringConcat resultTy containerTy plan input
        = .arr resultTy bytes ∧
      env.elem = 0x78 ∧
      bytesPinnedAt modBytes modLen resultTy [0x5e, 0x78, 0x01] := by
  obtain ⟨helem, hbp⟩ := stringResult_pin_forces_i8 env modBytes modLen resultTy hpin
  exact ⟨model_output_tag_is_resultTy resultTy containerTy plan input bytes hcat,
    helem, hbp⟩

#print axioms stringResult_pin_forces_i8
#print axioms model_output_tag_is_resultTy
#print axioms env_stringConcat_bridge

end AverCert.EnvelopeLowering.StringBridge
