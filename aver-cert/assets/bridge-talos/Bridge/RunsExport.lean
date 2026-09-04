import Interpreter.Wasm.Host.Run
import Batteries.Tactic.OpenPrivate
import Bridge.Accepted

set_option autoImplicit false

/-!
# The composition sentence at Talos's export boundary (brief §9 (4))

Talos's own statement shape for a parameterized exported call is
`RunsExportWith env m op call post` (Host/Run.lean): `startExportConfig? env m
op call = some config` — the export named `op` exists, the call carries
exactly the declared number of arguments and each matches its declared
parameter type — and `TerminatesWith config …` from there. The wall-facing
theorem `wFuncN_terminatesWith` (Bridge.lean) is stated over the configuration
`Config.lean` builds; this file shows that configuration is what
`startExportConfig?` builds for the synthetic module's one export
(`startExportConfig?_synth`) and states the composition sentence over an
accepted record projection-compute claim in Talos's vocabulary
(`recordCompute_runsExport`).

`env` in `startExportConfig?` is a Talos `HostEnv α`; the adapter
(`Adapter.lean`) is one, so nothing is wrapped. `call.initial` is the initial
wasm store — it has to carry the argument records, so `ExportCall.ofHost`
(the module's empty initial store) does not apply here; `call.arguments` is
in Talos's operand-stack order (top first), which is why the relation to the
wall's argument list reads `Rs call.initial.gcHeap call.arguments.reverse vs`.

The two argument guards of `startExportConfig?` are private to Host/Run.lean;
`open private` (Batteries) names them so they can be discharged from the
sorts and the value relation: a sorted, related argument of any profile sort
matches the parameter type `valueTypeOf` assigns to that sort.

`RunsExportWith` rather than `RunsExportWithOutcome`: the wall's successful
run yields an actual `.done` trace, and `Step` is deterministic
(`RunsExportWithOutcome.deterministic`), so a trap is not merely unobserved
but impossible on the same call.
-/

namespace Bridge
open CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.PlanLower AverCert.StandardFace
  RecordComputeBridge
open AverCert.Schema (CarrierSpec)
open Wasm Wasm.SmallStep

open private exportValueMatches exportArgumentsMatch from Interpreter.Wasm.Host.Run

/-! ## The synthetic module at the export boundary -/

theorem synthModule_findExport (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (body : Program) :
    (synthModule env paramSorts result nlocals body).findExport exportName =
      some env.imports.length := by
  simp [Module.findExport, synthModule]

theorem synthModule_funcSig? (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (body : Program) :
    (synthModule env paramSorts result nlocals body).funcSig? env.imports.length =
      some { params := paramSorts.map valueTypeOf, results := [valueTypeOf result] } := by
  simp [Module.funcSig?, synthModule, synthFunction]

/-- A sorted argument related to a Talos value matches the parameter type of
    its sort. -/
theorem exportValueMatches_of_R {α : Type} {env : TranslateEnv} {S : CarrierSpec env.carrier}
    (m : Module) (st : Store α) {t : STy} {w : WVal} {v : Value}
    (hs : HasSort env S w t) (hR : R st.gcHeap v w) :
    exportValueMatches m st v (valueTypeOf t) = true := by
  cases t with
  | i32 =>
    obtain ⟨n, rfl⟩ := HasSort_i32 hs
    obtain ⟨u, rfl, -⟩ := R_i32v hR
    rfl
  | i64 =>
    obtain ⟨n, rfl⟩ := HasSort_i64 hs
    obtain ⟨u, rfl, -⟩ := R_i64v hR
    rfl
  | i64b =>
    obtain ⟨n, rfl, -⟩ := HasSort_i64b hs
    obtain ⟨u, rfl, -⟩ := R_i64v hR
    rfl
  | f64 =>
    obtain ⟨b, rfl⟩ := HasSort_f64 hs
    obtain rfl := R_f64v hR
    rfl
  | ref =>
    rcases HasSort_ref hs with rfl | ⟨t, fs, rfl⟩ | ⟨t, es, rfl⟩
    · obtain rfl := R_null hR
      rfl
    · obtain ⟨a, vs, rfl, -, -⟩ := R_structv hR
      rfl
    · obtain ⟨a, vs, rfl, -, -⟩ := R_arr hR
      rfl
  | car =>
    rcases HasSort_car_shape hs with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩ <;>
    · obtain ⟨a, vs, rfl, -, -⟩ := R_structv hR
      rfl

theorem exportValuesMatch_of_Rs {α : Type} {env : TranslateEnv} {S : CarrierSpec env.carrier}
    (m : Module) (st : Store α) :
    ∀ {ts : List STy} {ws : List WVal} {vs : List Value}, Sorted env S ws ts → Rs st.gcHeap vs ws →
      ((vs.zip (ts.map valueTypeOf)).all fun p => exportValueMatches m st p.1 p.2) = true
  | [], [], [], _, _ => rfl
  | t :: ts, w :: ws, v :: vs, hs, hR => by
      simp only [Sorted] at hs
      simp only [Rs] at hR
      simp [exportValueMatches_of_R m st hs.1 hR.1, exportValuesMatch_of_Rs m st hs.2 hR.2]
  | [], [], _ :: _, _, hR => by simp [Rs] at hR
  | [], _ :: _, _, hs, _ => by simp [Sorted] at hs
  | _ :: _, [], _, hs, _ => by simp [Sorted] at hs
  | _ :: _, _ :: _, [], _, hR => by simp [Rs] at hR

/-- Talos's export boundary, on sorted and related arguments, enters exactly
    the configuration `wFuncN_terminatesWith` is stated over. -/
theorem startExportConfig?_synth {α : Type} [Inhabited α] (env : TranslateEnv)
    (S : CarrierSpec env.carrier) (paramSorts : List STy) (result : STy) (nlocals : Nat)
    (body : Program) (hostEnv : HostEnv α) (call : ExportCall α) (vs : List WVal)
    (hvs : Sorted env S vs paramSorts) (hargs : Rs call.initial.gcHeap call.arguments.reverse vs) :
    startExportConfig? hostEnv (synthModule env paramSorts result nlocals body) exportName call =
      some (initialConfig (synthModule env paramSorts result nlocals body) hostEnv
        (synthFunction env paramSorts result nlocals body) call.initial call.arguments.reverse) := by
  have hlen : call.arguments.length = paramSorts.length := by
    have h₁ := Rs_length hargs
    have h₂ := Sorted_length hvs
    simp only [List.length_reverse] at h₁
    exact h₁.trans h₂
  have hmatch : exportArgumentsMatch (synthModule env paramSorts result nlocals body) call
      { params := paramSorts.map valueTypeOf, results := [valueTypeOf result] } = true := by
    unfold exportArgumentsMatch
    exact exportValuesMatch_of_Rs _ _ hvs hargs
  have hinit := initConfig_synth env paramSorts result nlocals body hostEnv call.initial
    call.arguments.reverse (by simpa using hlen)
  simp only [List.reverse_reverse, synthInstance] at hinit
  simp [startExportConfig?, synthModule_findExport, synthModule_funcSig?, hlen, hmatch, hinit, guard, Except.toOption]

/-! ## The composition sentence -/

/-- Brief §9 (4): over the declared envelope of an accepted record
    projection-compute claim, the export of the synthetic module
    `synthModule (envOfClaim …) … (translation of the lowered body)`, called
    under the adapter host with arguments related to the wall's, RUNS in
    Talos's sense (`RunsExportWith`: the boundary admits the call and the run
    terminates normally) and returns one value related to what the wall's
    `wFuncN` computed at the export, under the verbatim contracts.
    `envOfClaim` consumes only the claim's role table, its carrier index and
    the pinned record declaration. -/
theorem recordCompute_runsExport (α : Type) [Inhabited α]
    (roles : CertDecode.AddSub.Roles) (ht : List (HostRole × Nat)) (C structIdx : Nat)
    (fields : List TypeDecl) (plan : ExprFragmentRawPlan) (body : List WInstr)
    -- the wall's conjuncts (declared data)
    (hbound : hostTableBound roles ht = true)
    (hok : (plan.body.nodes.all fun n => recordComputeNodeOk ht n.kind) = true)
    (hidx : ((plan.body.nodes.filterMap fun n => fragNodeStructIdx? n.kind).all (· == structIdx)) = true)
    (htyped : planTypedB structIdx (tyOfPlan plan) plan.params plan.body.nodes = true)
    (hparams : (plan.params.all (· == .adtRef)) = true)
    (hfields : (fields.all fun f => match f with | .intCarrier => true | _ => false) = true)
    (hdecl : checkRecordDecl (.record structIdx fields) = true)
    (hlow : lowerExprFragmentBody C plan = some body)
    -- the three extra hypotheses (`Accepted.lean`, header)
    (hi32 : ∀ n ∈ plan.body.nodes, ∀ v, n.kind = .constI32 v → i32Band v)
    (harity : ∀ n ∈ plan.body.nodes,
      (∀ args, n.kind = .structNew structIdx args → args.length = fields.length) ∧
      (∀ field value, n.kind = .structGetUser structIdx field value → field < fields.length))
    (hne : structIdx ≠ C)
    -- the wall's contracts, the representation, the run
    (S : CarrierSpec C) (add sub mul cmp eq : List WVal → Option WVal)
    (hc : ComputeContracts S add sub mul cmp eq) (hmw : CarrierMachine S)
    (code : CodeTbl) (self fuel : Nat)
    (hself : code self = some ⟨plan.params.length, 1, body⟩)
    (vs : List WVal)
    (hvs : Sorted (envOfClaim ht C [.record structIdx fields]) S vs (plan.params.map sortOfFragTy))
    (call : ExportCall α) (hargs : Rs call.initial.gcHeap call.arguments.reverse vs)
    (w : WVal)
    (hrun : wFuncN code (recordComputeSlots C add sub mul cmp eq ht) fuel self vs = some w) :
    let env := envOfClaim ht C [.record structIdx fields]
    let host := recordComputeSlots C add sub mul cmp eq ht
    ∃ (body' : Program) (t : STy),
      translateList env body = some body' ∧ SubSort t (sortOfFragTy plan.result) ∧
      RunsExportWith (adapterEnv α env host) (synthModule env (plan.params.map sortOfFragTy) t 1 body')
        exportName call (fun ret => ∃ v, ret.values = [v] ∧ R ret.final.gcHeap v w) := by
  intro env host
  have hchk : checkBlockFuel AverCert.PlanCheck.maxFuel plan.params plan.body = true := by
    simp only [lowerExprFragmentBody] at hlow
    split at hlow
    · rename_i h
      simp only [checkExprFragmentRawPlan, Bool.and_eq_true] at h
      exact h.1.2
    · simp at hlow
  have hprof := planInProfile_of_recordCompute roles ht C structIdx fields plan hbound hok hidx htyped
    hparams hchk hfields hdecl hne hi32 harity
  obtain ⟨t, hsub, hty, body', htr⟩ := coverage_envOfClaim ht C [.record structIdx fields] plan hprof
    body hlow
  have hnd : hostTableIndicesDistinct ht = true := by
    simp only [hostTableBound, Bool.and_eq_true] at hbound; exact hbound.1
  refine ⟨body', t, htr, hsub, _,
    startExportConfig?_synth env S _ t 1 body' (adapterEnv α env host) call vs hvs hargs, ?_⟩
  exact wFuncN_terminatesWith env S (envOfClaim_carrier ht C _) host (adapterEnv α env host)
    (HostSimulation_recordCompute α ht C _ S add sub mul cmp eq hc hmw hnd)
    (HostSorts_of_contracts ht C _ S add sub mul cmp eq hc hnd)
    code self fuel ⟨plan.params.length, 1, body⟩ hself (plan.params.map sortOfFragTy) t body'
    (by simpa [Γof] using hty) htr vs hvs call.initial call.arguments.reverse hargs w hrun

end Bridge
