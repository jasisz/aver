/-
The positive semantic bridge for the lower-pinned ADT read face.

`EnvelopeLowering` establishes the FORGE-death direction: any accepted claim on
a fixed module has the honest envelope, and every meaning field (`domRepr`,
`model`) is pinned to a plan-computed term with zero producer choice. This
module supplies the complementary POSITIVE direction that the acceptance
soundness assembly needs: on the DECLARED representation, the byte-origin
`EvalCascade` relates every represented input to exactly the plan-computed
`envStructModel`.

Together these turn `AcceptanceSoundness.intDispatchSemanticBridge` from a
producer-emitted premise into a THEOREM of the lower-pinned face: the model is
`envStructModel env plan.body`, the domain representation is `envDomRepr env`,
both are byte-pinned by the envelope lowering, and the bridge below proves they
agree with the wall's `EvalCascade` — with no byte -> structure decoder.

Self-verified `#print axioms` at the foot of the file: `[propext]`.
-/
import EnvelopeLowering
import IntDispatchSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering

open AverCert.Schema
open CertPrelude

/-- The load-bearing positive bridge: on any represented value of the DECLARED
    ADT, the byte-origin `EvalCascade` relates it to exactly the plan-computed
    `cascadeEval` result. The declared/plan agreement (`cascadeInEnv`) rules out
    the payloadless-projection case, and the represented-input structure
    (`EnvValidChild`, carried by `AdtVal`) supplies the field witness. -/
theorem cascade_bridge (env : AdtEnvelope) (body : IntDispatchCascade)
    (hcasc : cascadeInEnv env body = true)
    (S : CarrierSpec (carrierIdxE env)) (x : AdtVal env) (vs : List WVal)
    (hdom : envDomRepr env S x vs) :
    ∃ tag fields,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S body tag fields
        ((cascadeEval body x.1.1 x.1.2).getD 0) := by
  induction body with
  | default k =>
      refine ⟨x.1.1, ?_⟩
      unfold envDomRepr at hdom
      cases hpay : x.1.2 with
      | some m =>
          rw [hpay] at hdom
          obtain ⟨v, hvs, _hrepr⟩ := hdom
          exact ⟨[v], hvs, by
            simp only [cascadeEval, Option.getD]
            exact IntDispatchSoundness.EvalCascade.default k x.1.1 [v]⟩
      | none =>
          rw [hpay] at hdom
          exact ⟨[], hdom, by
            simp only [cascadeEval, Option.getD]
            exact IntDispatchSoundness.EvalCascade.default k x.1.1 []⟩
  | test tyIdx leaf rest ih =>
      simp only [cascadeInEnv, Bool.and_eq_true, decide_eq_true_eq] at hcasc
      obtain ⟨hpayTy, hrest⟩ := hcasc
      by_cases htag : x.1.1 = tyIdx
      · have hchild := x.2
        unfold EnvValidChild at hchild
        rw [htag] at hchild
        rcases hchild with ⟨_, m, hm⟩ | ⟨hfalse, _⟩
        · unfold envDomRepr at hdom
          rw [hm] at hdom
          obtain ⟨v, hvs, hrepr⟩ := hdom
          refine ⟨tyIdx, [v], by rw [← htag]; exact hvs, ?_⟩
          have hce : (cascadeEval (.test tyIdx leaf rest) x.1.1 x.1.2).getD 0
              = IntDispatchSoundness.evalLeaf leaf m := by
            simp [cascadeEval, htag, hm]
          rw [hce]
          exact IntDispatchSoundness.EvalCascade.hit tyIdx leaf rest [v] m v rfl hrepr
        · rw [hpayTy] at hfalse
          exact absurd hfalse (by decide)
      · obtain ⟨tag, fields, hvs, hev⟩ := ih hrest
        refine ⟨tag, fields, hvs, ?_⟩
        have htageq : tag = x.1.1 := by
          unfold envDomRepr at hdom
          cases hpay : x.1.2 with
          | some m =>
              rw [hpay] at hdom; obtain ⟨v, hvs2, _⟩ := hdom
              rw [hvs2] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
          | none =>
              rw [hpay] at hdom
              rw [hdom] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
        have hne : x.1.1 ≠ tyIdx := htag
        have hce : (cascadeEval (.test tyIdx leaf rest) x.1.1 x.1.2).getD 0
            = (cascadeEval rest x.1.1 x.1.2).getD 0 := by
          simp only [cascadeEval, if_neg hne]
        rw [hce]
        subst htageq
        exact IntDispatchSoundness.EvalCascade.miss tyIdx x.1.1 leaf rest fields _ hne hev

/-- The bridge in the exact shape the acceptance-soundness assembly consumes:
    with the plan-computed model `envStructModel` and the standard `intRepr`
    codomain, the residual `intDispatchSemanticBridge` body holds outright.
    `accept_sound` obtains this after transporting `o.domRepr`/`o.model`/
    `o.codRepr` along the face's `HEq` pins. -/
theorem env_intDispatch_bridge (env : AdtEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : cascadeInEnv env plan.body = true)
    (S : CarrierSpec (carrierIdxE env)) (x : AdtVal env) (vs : List WVal)
    (hdom : envDomRepr env S x vs) :
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
      ∀ w, S.Repr n w →
        intRepr S (envStructModel env plan.body x) w := by
  obtain ⟨tag, fields, hvs, hev⟩ := cascade_bridge env plan.body hcasc S x vs hdom
  refine ⟨tag, fields, (cascadeEval plan.body x.1.1 x.1.2).getD 0, hvs, hev, ?_⟩
  intro w hw
  simpa [intRepr, envStructModel] using hw

#print axioms cascade_bridge
#print axioms env_intDispatch_bridge

end AverCert.EnvelopeLowering
