/-
The HEq transport that carries the declared-index envelope bridges
(`DeclaredIndexEnvelope.env_declaredIntDispatch_bridge` and
`env_declaredConstruct_bridge`) from their canonical statements over
`(DAdtVal env, dEnvDomRepr env, dEnvStructModel …, intRepr …)` /
`(Int, intArgDomRepr …, dEnvCtorModel …, dEnvCodRepr …)` onto the
acceptance-soundness obligation fields `o.Dom / o.domRepr / o.model / o.codRepr`.

Same discipline as `EnvelopeAcceptTransport`: the field values are supplied as
ordinary universally quantified variables (not `o.field` projections) so that
`subst` applies once each pin is turned into an `Eq`; the `HEq`s become `Eq`s
via `eq_of_heq` after `o.carrier` / `o.Dom` / `o.Cod` are substituted, and no
cast residue survives. The obligation-level corollaries then apply the cores to
`o.carrier`, `o.Dom`, … directly, producing exactly the residual bodies of
`AcceptanceSoundness.intDispatchSemanticBridge` and `constructSemanticBridge`.

The `o.policy = .simulatesModel` conjunct is carried by the face itself
(`DIdxIntReadFace` supplies it as a separate hypothesis; `DIdxCtorFace` includes
it), which is what makes the FULL semantic bridge — policy conjunct included —
derivable.

Self-verified `#print axioms` at the foot of the file: `[propext]`.
-/
import DeclaredIndexEnvelope

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.DeclaredIndexEnvelope

open AverCert.Schema
open CertPrelude

/-! ## §1 The Int-dispatch transport -/

/-- The dependent-cast core for the Int-read column: with the field values as
    free variables and the face's pins as `Eq`/`HEq`, the residual
    `intDispatchSemanticBridge` body over those fields is exactly
    `env_declaredIntDispatch_bridge`. -/
theorem env_declaredIntDispatch_bridge_transport
    (env : DIdxEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : dCascadeInEnv env plan.body = true)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = env.carrier)
    (hDom : HEq Dom (DAdtVal env))
    (hCod : HEq Cod Int)
    (hdomRepr : HEq domRepr (dEnvDomRepr env))
    (hcodRepr : HEq codRepr (@AverCert.Schema.intRepr env.carrier))
    (hmodel : HEq model (dEnvStructModel env plan.body)) :
    ∀ (S : CarrierSpec carrier) (x : Dom) (vs : List WVal),
      domRepr S x vs →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
        ∀ w, S.Repr n w → codRepr S (model x) w := by
  subst hcar
  have hDomEq : Dom = DAdtVal env := eq_of_heq hDom
  subst hDomEq
  have hCodEq : Cod = Int := eq_of_heq hCod
  subst hCodEq
  have hdomReprEq : domRepr = dEnvDomRepr env := eq_of_heq hdomRepr
  subst hdomReprEq
  have hcodReprEq : codRepr = @AverCert.Schema.intRepr env.carrier :=
    eq_of_heq hcodRepr
  subst hcodReprEq
  have hmodelEq : model = dEnvStructModel env plan.body := eq_of_heq hmodel
  subst hmodelEq
  intro S x vs hdom
  exact env_declaredIntDispatch_bridge env plan hcasc S x vs hdom

/-- Obligation-level corollary for the Int-read column: the declared-index face
    (`DIdxIntReadFace`) plus `o.policy = .simulatesModel` yields the FULL
    residual `intDispatchSemanticBridge` body over the real `Obligation`
    projections — the policy conjunct AND the represented model agreement. -/
theorem face_gives_declaredIntDispatch_bridge
    (modBytes modLen : Nat)
    (env : DIdxEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : DIdxIntReadFace modBytes modLen env plan o)
    (hpolicy : o.policy = .simulatesModel) :
    o.policy = .simulatesModel ∧
    ∀ (S : CarrierSpec o.carrier) (x : o.Dom) (vs : List WVal),
      o.domRepr S x vs →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
        ∀ w, S.Repr n w → o.codRepr S (o.model x) w := by
  obtain ⟨-, hcasc, -, hcar, hDom, hCod, hdomRepr, hcodRepr, hmodel⟩ := hface
  refine ⟨hpolicy, ?_⟩
  exact env_declaredIntDispatch_bridge_transport env plan hcasc
    o.carrier o.Dom o.Cod o.domRepr o.codRepr o.model
    hcar hDom hCod hdomRepr hcodRepr hmodel

/-! ## §2 The constructor transport -/

/-- The dependent-cast core for the constructor column: with the field values as
    free variables and the face's pins as `Eq`/`HEq`, the residual
    `constructSemanticBridge` body over those fields is exactly
    `env_declaredConstruct_bridge`. -/
theorem env_declaredConstruct_bridge_transport
    (env : DIdxEnvelope) (structIdx : Nat)
    (hhit : dCtorShape? env structIdx = some .hit)
    (plan : ConstructRawPlan)
    (harity : plan.arity = 1) (hfields : plan.fields = [.local 0])
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = env.carrier)
    (hDom : HEq Dom Int)
    (hCod : HEq Cod (DAdtVal env))
    (hdomRepr : HEq domRepr (AverCert.EnvelopeLowering.intArgDomRepr env.carrier))
    (hcodRepr : HEq codRepr (dEnvCodRepr env))
    (hmodel : HEq model (dEnvCtorModel env structIdx hhit)) :
    ∀ (S : CarrierSpec carrier) (x : Dom) (args : List WVal),
      domRepr S x args →
      args.length = plan.arity ∧
      codRepr S (model x)
        (.structv structIdx
          (ConstructVerbatimSoundness.constructModelFields
            (args ++ List.replicate 1 .null) plan.fields)) := by
  subst hcar
  have hDomEq : Dom = Int := eq_of_heq hDom
  subst hDomEq
  have hCodEq : Cod = DAdtVal env := eq_of_heq hCod
  subst hCodEq
  have hdomReprEq : domRepr = AverCert.EnvelopeLowering.intArgDomRepr env.carrier :=
    eq_of_heq hdomRepr
  subst hdomReprEq
  have hcodReprEq : codRepr = dEnvCodRepr env := eq_of_heq hcodRepr
  subst hcodReprEq
  have hmodelEq : model = dEnvCtorModel env structIdx hhit := eq_of_heq hmodel
  subst hmodelEq
  intro S x args hdom
  exact env_declaredConstruct_bridge env structIdx hhit plan harity hfields
    S x args hdom

/-- Obligation-level corollary for the constructor column: the declared-index
    constructor face (`DIdxCtorFace`, whose `o.policy = .simulatesModel` conjunct
    is included) yields the FULL residual `constructSemanticBridge` body over the
    real `Obligation` projections. -/
theorem face_gives_declaredConstruct_bridge
    (modBytes modLen : Nat)
    (env : DIdxEnvelope) (structIdx : Nat)
    (hhit : dCtorShape? env structIdx = some .hit)
    (plan : ConstructRawPlan) (o : Obligation)
    (hface : DIdxCtorFace modBytes modLen env structIdx hhit plan o) :
    o.policy = .simulatesModel ∧
    ∀ (S : CarrierSpec o.carrier) (x : o.Dom) (args : List WVal),
      o.domRepr S x args →
      args.length = plan.arity ∧
      o.codRepr S (o.model x)
        (.structv structIdx
          (ConstructVerbatimSoundness.constructModelFields
            (args ++ List.replicate 1 .null) plan.fields)) := by
  obtain ⟨-, harity, hfields, -, hpolicy, hcar, hDom, hCod, hdomRepr,
    hcodRepr, hmodel⟩ := hface
  refine ⟨hpolicy, ?_⟩
  exact env_declaredConstruct_bridge_transport env structIdx hhit plan harity hfields
    o.carrier o.Dom o.Cod o.domRepr o.codRepr o.model
    hcar hDom hCod hdomRepr hcodRepr hmodel

#print axioms env_declaredIntDispatch_bridge_transport
#print axioms face_gives_declaredIntDispatch_bridge
#print axioms env_declaredConstruct_bridge_transport
#print axioms face_gives_declaredConstruct_bridge

end AverCert.DeclaredIndexEnvelope
