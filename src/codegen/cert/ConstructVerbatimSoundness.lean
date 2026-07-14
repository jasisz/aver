import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower
import PlanBytes

set_option maxRecDepth 100000
set_option maxHeartbeats 1000000

namespace ConstructVerbatimSoundness
open CertPrelude AverCert.Schema

def outValue : Option Out → Option WVal
  | some (.ok _ [value]) => some value
  | some (.ret value) => some value
  | _ => none

/-! ## Constructor family -/

def constructModelField (locals : List WVal) : ConstructField → WVal
  | .local index => locals.getD index .null
  | .null => .null

def constructModelFields (locals : List WVal) : List ConstructField → List WVal
  | [] => []
  | field :: rest =>
      constructModelField locals field :: constructModelFields locals rest

@[simp] theorem constructModelFields_length
    (locals : List WVal) (fields : List ConstructField) :
    (constructModelFields locals fields).length = fields.length := by
  induction fields <;> simp [constructModelFields, *]

@[simp] theorem popArgs_reverse (values : List WVal) :
    popArgs values.length values.reverse = some (values, []) := by
  unfold popArgs
  rw [if_neg (by simp)]
  have hlen : values.length = values.reverse.length := by simp
  rw [hlen, List.take_length, List.drop_length]
  simp

def FieldsReadable (locals : List WVal) (fields : List ConstructField) : Prop :=
  ∀ i, ConstructField.local i ∈ fields → ∃ v, locals[i]? = some v

theorem fieldsReadable_tail
    {locals : List WVal} {field : ConstructField} {fields : List ConstructField}
    (h : FieldsReadable locals (field :: fields)) : FieldsReadable locals fields := by
  intro i hi
  exact h i (by simp [hi])

/-- The constructor-family simulation lemma.  Its `rest` quantifier is the same
    compositional device used by the straight-line soundness proof. -/
theorem simNodes_construct
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (structIdx : Nat) (locals : List WVal) :
    ∀ fields, FieldsReadable locals fields → ∀ rest stack,
      wRunF host ar callee
          (AverCert.PlanLower.lowerConstructFields structIdx fields ++ rest) locals stack =
        wRunF host ar callee rest locals
          ((constructModelFields locals fields).reverse ++ stack) := by
  intro fields
  induction fields with
  | nil =>
      intro _ rest stack
      rfl
  | cons field fields ih =>
      intro hread rest stack
      cases field with
      | «local» i =>
          obtain ⟨v, hv⟩ := hread i (by simp)
          have htail := fieldsReadable_tail hread
          simpa [AverCert.PlanLower.lowerConstructFields,
            AverCert.PlanLower.lowerConstructField, constructModelFields,
            constructModelField, List.getD, hv, wRunF, List.reverse_cons,
            List.append_assoc] using ih htail rest (v :: stack)
      | null =>
          have htail := fieldsReadable_tail hread
          simpa [AverCert.PlanLower.lowerConstructFields,
            AverCert.PlanLower.lowerConstructField, constructModelFields,
            constructModelField, wRunF, List.reverse_cons, List.append_assoc]
            using ih htail rest (WVal.null :: stack)

theorem constructFieldsOk_local_lt
    (arity i : Nat) : ∀ fields,
      AverCert.PlanCheck.constructFieldsOk arity fields = true →
      ConstructField.local i ∈ fields → i < arity := by
  intro fields
  induction fields with
  | nil => simp
  | cons field fields ih =>
      cases field with
      | «local» j =>
          intro hok hmem
          simp [AverCert.PlanCheck.constructFieldsOk,
            AverCert.PlanCheck.constructFieldOk] at hok
          simp at hmem
          rcases hmem with rfl | hmem
          · exact hok.1
          · exact ih hok.2 hmem
      | null =>
          intro hok hmem
          simp [AverCert.PlanCheck.constructFieldsOk,
            AverCert.PlanCheck.constructFieldOk] at hok
          simp at hmem
          exact ih hok hmem

theorem accepted_fields_readable
    (plan : ConstructRawPlan) (nlocals : Nat) (args : List WVal)
    (hcheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    (hlen : args.length = plan.arity) :
    FieldsReadable (args ++ List.replicate nlocals .null) plan.fields := by
  have hfields : AverCert.PlanCheck.constructFieldsOk plan.arity plan.fields = true := by
    have hall := hcheck
    simp [AverCert.PlanCheck.checkConstructRawPlan] at hall
    exact hall.1.2
  intro i hi
  have hlt : i < plan.arity :=
    constructFieldsOk_local_lt plan.arity i plan.fields hfields hi
  have hargs : i < args.length := by simpa [hlen] using hlt
  refine ⟨args[i], ?_⟩
  rw [List.getElem?_append_left hargs]
  exact List.getElem?_eq_getElem hargs

/-- Any checker-accepted constructor plan, when tied to its audited lowering,
    constructs exactly the field list described by the plan. -/
theorem generic_construct_certified
    (structIdx : Nat) (plan : ConstructRawPlan)
    (code : CodeTbl) (host : HostTbl) (self nlocals : Nat)
    (hcheck : AverCert.PlanCheck.checkConstructRawPlan plan = true)
    (instrs : List WInstr)
    (hlow : AverCert.PlanLower.lowerConstructBody structIdx plan = some instrs)
    (hself : code self = some
      { arity := plan.arity, nlocals := nlocals, body := instrs })
    (args : List WVal) (hlen : args.length = plan.arity) :
    wFuncN code host 1 self args =
      some (.structv structIdx
        (constructModelFields (args ++ List.replicate nlocals .null) plan.fields)) := by
  have hcanon : instrs =
      AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
        [.structNew structIdx plan.fields.length] := by
    simp [AverCert.PlanLower.lowerConstructBody, hcheck] at hlow
    exact hlow.symm
  subst instrs
  simp only [wFuncN]
  rw [hself]
  simp only [initLocals]
  change outValue (wRunF host (fun g => (code g).map (·.arity))
    (fun _ _ => none)
    (AverCert.PlanLower.lowerConstructFields structIdx plan.fields ++
      [.structNew structIdx plan.fields.length])
    (args ++ List.replicate nlocals .null) []) = _
  have hsim := simNodes_construct host (fun g => (code g).map (·.arity))
    (fun _ _ => none)
    structIdx (args ++ List.replicate nlocals .null) plan.fields
    (accepted_fields_readable plan nlocals args hcheck hlen)
    [.structNew structIdx plan.fields.length] []
  rw [hsim]
  simp only [List.append_nil]
  rw [show plan.fields.length =
    (constructModelFields (args ++ List.replicate nlocals .null) plan.fields).length by simp]
  simp only [wRunF, popArgs_reverse]
  rfl

/-! ## Verbatim family -/

def verbatimLeafModel (scrutinee : WVal) : VerbatimLeaf → Option WVal
  | .project tyIdx field =>
      match scrutinee with
      | .structv ty fields => if ty = tyIdx then fields[field]? else none
      | _ => none
  | .arrayNewData arrTy _ bytes =>
      some (.arr arrTy (bytes.map (fun b => .i32v (Int.ofNat b))))
  | .refNull => some .null
  | .f64Bits bits => some (.f64v (UInt64.ofNat bits))

def verbatimDispatchModel (scrutinee : WVal) : VerbatimDispatch → Option WVal
  | .leaf leaf => verbatimLeafModel scrutinee leaf
  | .test tyIdx hit rest =>
      match scrutinee with
      | .structv ty _ =>
          if ty = tyIdx then verbatimLeafModel scrutinee hit
          else verbatimDispatchModel scrutinee rest
      | .arr ty _ =>
          if ty = tyIdx then verbatimLeafModel scrutinee hit
          else verbatimDispatchModel scrutinee rest
      | _ => verbatimDispatchModel scrutinee rest

def verbatimModel (plan : VerbatimRawPlan) (scrutinee : WVal) : WVal :=
  (verbatimDispatchModel scrutinee plan.body).getD .null

@[simp] theorem finishRun_nil
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (r : Option Out) :
    (match r with
     | some (.ok locals stack) =>
         wRunF host ar callee [] locals stack
     | some (.ret value) => some (.ret value)
     | none => none) = r := by
  cases r with
  | none => rfl
  | some out => cases out <;> simp [wRunF]

@[simp] theorem outValue_finishRun_nil
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (r : Option Out) :
    outValue
      (match r with
       | some (.ok locals stack) => wRunF host ar callee [] locals stack
       | some (.ret value) => some (.ret value)
       | none => none) = outValue r := by
  rw [finishRun_nil]

@[simp] theorem outValue_passthrough (r : Option Out) :
    outValue
      (match r with
       | some (.ok locals stack) => some (.ok locals stack)
       | some (.ret value) => some (.ret value)
       | none => none) = outValue r := by
  cases r with
  | none => rfl
  | some out => cases out <;> rfl

theorem unwrap_passthrough {r : Option Out} {w : WVal}
    (h : outValue
      (match r with
       | some (.ok locals stack) => some (.ok locals stack)
       | some (.ret value) => some (.ret value)
       | none => none) = some w) : outValue r = some w := by
  cases r with
  | none => simpa [outValue] using h
  | some out => cases out <;> simpa [outValue] using h

/-- A verbatim leaf started with an empty operand stack returns the value
    described by that leaf whenever it does not trap. -/
theorem simLeaf_verbatim
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (S F : Nat) (locals : List WVal) (scrutinee : WVal) :
    ∀ leaf w,
      locals[S]? = some scrutinee →
      F < locals.length →
      outValue (wRunF host ar callee
          (AverCert.PlanLower.lowerLeaf S F leaf)
          locals []) = some w →
      w = (verbatimLeafModel scrutinee leaf).getD .null := by
  intro leaf w hslot hF hrun
  cases leaf with
  | project ty field =>
      cases scrutinee with
      | structv actual fields =>
          by_cases hty : actual = ty
          · subst actual
            simp [AverCert.PlanLower.lowerLeaf, verbatimLeafModel,
              wRunF, hslot] at hrun ⊢
            split at hrun
            · rename_i value hfield
              have hset : (locals.set F value)[F]? = some value :=
                List.getElem?_set_self hF
              simp [outValue, hfield, hset] at hrun ⊢
              exact hrun.symm
            · contradiction
          · simp [AverCert.PlanLower.lowerLeaf, verbatimLeafModel,
              outValue, wRunF, hslot, hty] at hrun
      | i32v n => simp [AverCert.PlanLower.lowerLeaf, outValue, wRunF, hslot] at hrun
      | i64v n => simp [AverCert.PlanLower.lowerLeaf, outValue, wRunF, hslot] at hrun
      | f64v bits => simp [AverCert.PlanLower.lowerLeaf, outValue, wRunF, hslot] at hrun
      | arr ty elems => simp [AverCert.PlanLower.lowerLeaf, outValue, wRunF, hslot] at hrun
      | null => simp [AverCert.PlanLower.lowerLeaf, outValue, wRunF, hslot] at hrun
  | arrayNewData arrTy dataIdx bytes =>
      simp [AverCert.PlanLower.lowerLeaf, verbatimLeafModel,
        outValue, wRunF, Function.comp_def] at hrun ⊢
      exact hrun.symm
  | refNull =>
      simp [AverCert.PlanLower.lowerLeaf, verbatimLeafModel, outValue, wRunF] at hrun ⊢
      exact hrun.symm
  | f64Bits bits =>
      simp [AverCert.PlanLower.lowerLeaf, verbatimLeafModel, outValue, wRunF] at hrun ⊢
      exact hrun.symm

/-- Simulation for the non-first tail of a dispatch cascade. -/
theorem simNodes_verbatim_tail
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (S F : Nat) (locals : List WVal) (scrutinee : WVal)
    (hslot : locals[S]? = some scrutinee) (hF : F < locals.length) :
    ∀ dispatch w,
      outValue (wRunF host ar callee
          (AverCert.PlanLower.lowerDispatch S F false dispatch)
          locals []) = some w →
      w = (verbatimDispatchModel scrutinee dispatch).getD .null := by
  intro dispatch
  induction dispatch with
  | leaf leaf =>
      intro w hrun
      exact simLeaf_verbatim host ar callee S F locals scrutinee
        leaf w hslot hF (by simpa [AverCert.PlanLower.lowerDispatch] using hrun)
  | test ty hit rest ih =>
      intro w hrun
      cases scrutinee with
      | structv actual fields =>
          by_cases hty : actual = ty
          · subst ty
            simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, hslot, b32] at hrun ⊢
            exact simLeaf_verbatim host ar callee S F locals
              (.structv actual fields) hit w hslot hF
                (unwrap_passthrough hrun)
          · simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, hslot, b32, hty] at hrun ⊢
            exact ih w (unwrap_passthrough hrun)
      | arr actual elems =>
          by_cases hty : actual = ty
          · subst ty
            simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, hslot, b32] at hrun ⊢
            exact simLeaf_verbatim host ar callee S F locals
              (.arr actual elems) hit w hslot hF
                (unwrap_passthrough hrun)
          · simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, hslot, b32, hty] at hrun ⊢
            exact ih w (unwrap_passthrough hrun)
      | i32v n =>
          simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
            outValue, wRunF, hslot] at hrun
      | i64v n =>
          simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
            outValue, wRunF, hslot] at hrun
      | f64v bits =>
          simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
            outValue, wRunF, hslot] at hrun
      | null =>
          simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
            outValue, wRunF, hslot, b32] at hrun ⊢
          exact ih w (unwrap_passthrough hrun)

/-- Generic dispatch simulation.  It is deliberately a partial-correctness
    statement: failed casts/projections trap, exactly as `wFuncN` does. -/
theorem simNodes_verbatim
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (S F : Nat) (locals : List WVal) (scrutinee : WVal)
    (hslot : locals[S]? = some scrutinee) (hF : F < locals.length) :
    ∀ ty hit rest w,
      outValue (wRunF host ar callee
          (AverCert.PlanLower.lowerDispatch S F true (.test ty hit rest))
          locals [scrutinee]) = some w →
      w = (verbatimDispatchModel scrutinee (.test ty hit rest)).getD .null := by
  intro ty hit rest w hrun
  cases scrutinee with
      | structv actual fields =>
          by_cases hty : actual = ty
          · subst ty
            simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, b32] at hrun ⊢
            exact simLeaf_verbatim host ar callee S F locals
              (.structv actual fields) hit w hslot hF
                (unwrap_passthrough hrun)
          · simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, b32, hty] at hrun ⊢
            exact simNodes_verbatim_tail host ar callee S F locals
              (.structv actual fields) hslot hF rest w
                (unwrap_passthrough hrun)
      | arr actual elems =>
          by_cases hty : actual = ty
          · subst ty
            simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, b32] at hrun ⊢
            exact simLeaf_verbatim host ar callee S F locals
              (.arr actual elems) hit w hslot hF
                (unwrap_passthrough hrun)
          · simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
              wRunF, b32, hty] at hrun ⊢
            exact simNodes_verbatim_tail host ar callee S F locals
              (.arr actual elems) hslot hF rest w
                (unwrap_passthrough hrun)
      | i32v n => simp [AverCert.PlanLower.lowerDispatch, outValue, wRunF] at hrun
      | i64v n => simp [AverCert.PlanLower.lowerDispatch, outValue, wRunF] at hrun
      | f64v bits => simp [AverCert.PlanLower.lowerDispatch, outValue, wRunF] at hrun
      | null =>
          simp [AverCert.PlanLower.lowerDispatch, verbatimDispatchModel,
            outValue, wRunF, b32] at hrun ⊢
          exact simNodes_verbatim_tail host ar callee S F locals
            .null hslot hF rest w (unwrap_passthrough hrun)

/-- Any checker-accepted verbatim plan tied to its audited lowering simulates
    the plan-derived `WVal → WVal` model on every successful execution. -/
theorem generic_verbatim_shape_certified
    (plan : VerbatimRawPlan) (code : CodeTbl) (host : HostTbl)
    (self nlocals : Nat)
    (hcheck : AverCert.PlanCheck.checkVerbatimRawPlan plan = true)
    (hroot : ∃ ty hit rest, plan.body = .test ty hit rest)
    (hscratch : plan.scrutineeLocal < 1 + nlocals)
    (hfieldScratch : plan.fieldLocal < 1 + nlocals)
    (hself : code self = some
      { arity := 1, nlocals := nlocals,
        body := AverCert.PlanLower.lowerVerbatimBody plan }) :
    ∀ fuel v w,
      wFuncN code host (fuel + 1) self [v] = some w →
      w = verbatimModel plan v := by
  intro fuel v w hrun
  rcases hroot with ⟨ty, hit, rest, hroot⟩
  simp only [wFuncN] at hrun
  rw [hself] at hrun
  let updated :=
    ([v] ++ List.replicate nlocals WVal.null).set plan.scrutineeLocal v
  have hslot :
      updated[plan.scrutineeLocal]? = some v := by
    dsimp [updated]
    apply List.getElem?_set_self
    simpa [Nat.add_comm] using hscratch
  have hslotRaw := hslot
  dsimp [updated] at hslotRaw
  have hF : plan.fieldLocal < updated.length := by
    simp [updated]
    simpa [Nat.add_comm] using hfieldScratch
  change outValue (wRunF host (fun g => (code g).map (·.arity))
    (fun g as => wFuncN code host fuel g as)
    (AverCert.PlanLower.lowerVerbatimBody plan)
    ([v] ++ List.replicate nlocals .null) []) = some w at hrun
  dsimp [AverCert.PlanLower.lowerVerbatimBody] at hrun
  simp only [wRunF, hslotRaw] at hrun
  have hsim := simNodes_verbatim host (fun g => (code g).map (·.arity))
    (fun g as => wFuncN code host fuel g as)
    plan.scrutineeLocal plan.fieldLocal
    updated v hslot hF ty hit rest w
  have hrun' : outValue (wRunF host (fun g => (code g).map (·.arity))
      (fun g as => wFuncN code host fuel g as)
      (AverCert.PlanLower.lowerDispatch plan.scrutineeLocal plan.fieldLocal true
        (.test ty hit rest)) updated [v]) = some w := by
    simpa [hroot, updated, wRunF, hslotRaw] using hrun
  have hmodel := hsim hrun'
  simpa [verbatimModel, hroot] using hmodel

/-- Checked generic certificate for the full admitted verbatim family. -/
theorem generic_verbatim_certified
    (plan : VerbatimRawPlan) (code : CodeTbl) (host : HostTbl)
    (self nlocals : Nat)
    (hcheck : AverCert.PlanCheck.checkVerbatimPlan nlocals plan = true)
    (hself : code self = some
      { arity := 1, nlocals := nlocals,
        body := AverCert.PlanLower.lowerVerbatimBody plan }) :
    ∀ fuel v w,
      wFuncN code host (fuel + 1) self [v] = some w →
      w = verbatimModel plan v := by
  cases hbody : plan.body with
  | leaf leaf =>
      simp [AverCert.PlanCheck.checkVerbatimPlan, hbody] at hcheck
  | test ty hit rest =>
      have hall := hcheck
      simp [AverCert.PlanCheck.checkVerbatimPlan, hbody] at hall
      exact generic_verbatim_shape_certified plan code host self nlocals
        hall.1.1 ⟨ty, hit, rest, hbody⟩ hall.1.2 hall.2 hself

end ConstructVerbatimSoundness
