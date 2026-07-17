/-
The HEq transport that carries `EnvelopeBridge.env_intDispatch_bridge` from its
canonical statement over `(AdtVal env, envDomRepr env, envStructModel …,
intRepr …)` onto the acceptance-soundness obligation fields
`o.Dom / o.domRepr / o.model / o.codRepr`.

`AdtIntFaceLower` states the face over the abstract `Obligation` `o`, exposing
only `HEq` pins between `o`'s dependent fields and the plan-computed terms
(`o.carrier = carrierIdxE env` is a bare `Eq`, the rest are `HEq` because the
field types themselves depend on `o.carrier` / `o.Dom` / `o.Cod`). This module
proves that the dependent-cast plumbing CLOSES: casting the bridge along those
pins yields the residual body of `AcceptanceSoundness.intDispatchSemanticBridge`
over the obligation fields, with no `sorry`.

The transport lemma is stated over the field values as ordinary universally
quantified variables (not `o.field` projections) so that `subst` applies once
each pin is turned into an `Eq`; the obligation-level corollary then applies it
to `o.carrier`, `o.Dom`, … directly. It also confirms the policy question:
adding `o.policy = .simulatesModel` to the face makes the FULL semantic bridge
(policy conjunct included) derivable.

Self-verified `#print axioms` at the foot of the file: `[propext]`.
-/
import EnvelopeBridge

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering

open AverCert.Schema
open CertPrelude

/-- The dependent-cast core: with the field values supplied as free variables
    and the face's pins as `Eq`/`HEq`, the residual `intDispatchSemanticBridge`
    body over those fields is exactly `env_intDispatch_bridge`. The proof is a
    chain of `subst`s — the `HEq`s become `Eq`s once `carrier`, `Dom`, `Cod`
    are substituted, and no cast residue survives. -/
theorem env_intDispatch_bridge_transport
    (env : AdtEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : cascadeInEnv env plan.body = true)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = carrierIdxE env)
    (hDom : HEq Dom (AdtVal env))
    (hCod : HEq Cod Int)
    (hdomRepr : HEq domRepr (envDomRepr env))
    (hcodRepr : HEq codRepr (@AverCert.Schema.intRepr (carrierIdxE env)))
    (hmodel : HEq model (envStructModel env plan.body)) :
    ∀ (S : CarrierSpec carrier) (x : Dom) (vs : List WVal),
      domRepr S x vs →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
        ∀ w, S.Repr n w → codRepr S (model x) w := by
  subst hcar
  have hDomEq : Dom = AdtVal env := eq_of_heq hDom
  subst hDomEq
  have hCodEq : Cod = Int := eq_of_heq hCod
  subst hCodEq
  have hdomReprEq : domRepr = envDomRepr env := eq_of_heq hdomRepr
  subst hdomReprEq
  have hcodReprEq : codRepr = @AverCert.Schema.intRepr (carrierIdxE env) :=
    eq_of_heq hcodRepr
  subst hcodReprEq
  have hmodelEq : model = envStructModel env plan.body := eq_of_heq hmodel
  subst hmodelEq
  exact env_intDispatch_bridge env plan hcasc

/-- The obligation-level corollary: the face's pins (over the real `Obligation`
    projections) plus `o.policy = .simulatesModel` yield the FULL residual
    `intDispatchSemanticBridge` body — the policy conjunct AND the represented
    model agreement. This answers the policy question precisely: acceptance
    (`intDispatchPlanAccepted`) admits both `simulatesModel` and
    `simulatesModelTotally`, so the policy conjunct is NOT already forced; the
    single added face conjunct `o.policy = .simulatesModel` closes the gap and
    makes the bridge fully derivable. -/
theorem face_gives_intDispatch_bridge
    (env : AdtEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : cascadeInEnv env plan.body = true)
    (o : Obligation)
    (hcar : o.carrier = carrierIdxE env)
    (hDom : HEq o.Dom (AdtVal env))
    (hCod : HEq o.Cod Int)
    (hdomRepr : HEq o.domRepr (envDomRepr env))
    (hcodRepr : HEq o.codRepr (@AverCert.Schema.intRepr (carrierIdxE env)))
    (hmodel : HEq o.model (envStructModel env plan.body))
    (hpolicy : o.policy = .simulatesModel) :
    o.policy = .simulatesModel ∧
    ∀ (S : CarrierSpec o.carrier) (x : o.Dom) (vs : List WVal),
      o.domRepr S x vs →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
        ∀ w, S.Repr n w → o.codRepr S (o.model x) w := by
  refine ⟨hpolicy, ?_⟩
  exact env_intDispatch_bridge_transport env plan hcasc
    o.carrier o.Dom o.Cod o.domRepr o.codRepr o.model
    hcar hDom hCod hdomRepr hcodRepr hmodel

#print axioms env_intDispatch_bridge_transport
#print axioms face_gives_intDispatch_bridge

end AverCert.EnvelopeLowering
