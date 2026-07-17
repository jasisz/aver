/-
The positive semantic bridge for the lower-pinned ADT constructor face.

`EnvelopeLowering` supplies the constructor face `AdtCtorFaceLower` together
with the FORGE-death direction (a fabricated payload map is unstatable, the
signature pin forces `root` and the carrier index). This module supplies the
complementary POSITIVE direction the acceptance-soundness assembly needs — the
constructor analogue of `EnvelopeBridge.env_intDispatch_bridge`: on the
DECLARED single-Int-argument representation, the lower-pinned constructor face
forces the obligation model to equal the plan-computed `envCtorModel`, and the
constructed `WVal` represents that model value.

The constructor face profile is `plan.arity = 1`, `plan.fields = [.local 0]`
(the unary Int-payload constructor the wall lowers). The residual body of
`AcceptanceSoundness.constructSemanticBridge` — argument arity plus the fact
that the exact constructed struct represents `model x` — then holds outright.

Self-verified `#print axioms` at the foot of the file: `[propext]`.
-/
import EnvelopeLowering
import ConstructVerbatimSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering

open AverCert.Schema
open CertPrelude

/-- The load-bearing positive constructor bridge, in the exact shape the
    acceptance-soundness assembly (`constructSemanticBridge`) consumes for the
    unary Int-payload constructor profile. On the declared Int-argument
    representation `intArgDomRepr`, the plan-computed constructor model
    `envCtorModel env structIdx hpay` is represented by exactly the struct the
    canonical lowering builds: `structv structIdx [v]`, where `v` represents the
    argument. There is no byte -> structure decoder — `model`, `codRepr`, and
    `domRepr` are all wall terms over the plan envelope. -/
theorem env_construct_bridge (env : AdtEnvelope) (structIdx : Nat)
    (hpay : ctorPayload? env structIdx = some true)
    (plan : ConstructRawPlan)
    (harity : plan.arity = 1) (hfields : plan.fields = [.local 0])
    (S : CarrierSpec (carrierIdxE env)) (x : Int) (args : List WVal)
    (hdom : intArgDomRepr (carrierIdxE env) S x args) :
    args.length = plan.arity ∧
    envCodRepr env S (envCtorModel env structIdx hpay x)
      (.structv structIdx
        (ConstructVerbatimSoundness.constructModelFields
          (args ++ List.replicate 1 .null) plan.fields)) := by
  obtain ⟨v, hargs, hrepr⟩ := hdom
  subst hargs
  rw [harity, hfields]
  refine ⟨rfl, ?_⟩
  -- constructModelFields ([v] ++ [.null]) [.local 0] = [v]
  have hflds : ConstructVerbatimSoundness.constructModelFields
      ([v] ++ List.replicate 1 (.null : WVal)) [.local 0] = [v] := by
    simp [ConstructVerbatimSoundness.constructModelFields,
      ConstructVerbatimSoundness.constructModelField]
  rw [hflds]
  -- envCodRepr on envCtorModel: the model tag is structIdx, payload some x
  show envCodRepr env S (envCtorModel env structIdx hpay x) (.structv structIdx [v])
  unfold envCodRepr envCtorModel
  exact ⟨v, rfl, hrepr⟩

#print axioms env_construct_bridge

end AverCert.EnvelopeLowering
