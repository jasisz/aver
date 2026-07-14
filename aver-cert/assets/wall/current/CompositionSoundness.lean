/- Generic soundness for composition-family call chains.

   A composition root is a unary chain of user calls.  The byte-bound plan
   resolves each callee name through the kernel function table.  Soundness of
   each member is supplied as a hypothesis with exactly the generated member
   theorem's face; this file proves only the straight-line call glue. -/
import AcceptedArtifactCore

set_option maxRecDepth 1000000
set_option linter.unusedSimpArgs false

namespace CompositionSoundness
open CertPrelude AverCert.Schema AverCert.PlanLower
open AverCert.AcceptedArtifact
open AverCert

/-- Source-model composition in the same left-to-right order as
    `lowerCompositionCalls`. -/
def evalCompositionCalls (models : String → Int → Int) :
    List String → Int → Int
  | [], x => x
  | name :: rest, x => evalCompositionCalls models rest (models name x)

/-- The exact semantic face emitted today for every member theorem. -/
def MemberCertified {C : Nat} (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl) (idx : Nat) (model : Int → Int) : Prop :=
  ∀ (fuel : Nat) (x : Int) (v w : WVal), S.Repr x v →
    wFuncN code host fuel idx [v] = some w → S.Repr (model x) w

/-- A member theorem plus the byte-derived routing facts needed by the call
    interpreter.  `member`/`member_lookup` align this hypothesis with the
    `CompositionMemberClaim` table consumed by artifact acceptance. -/
structure MemberFact {C : Nat} (S : CarrierSpec C)
    (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat))
    (code : CodeTbl) (host : HostTbl)
    (models : String → Int → Int) (name : String) where
  member : CompositionMemberClaim
  member_lookup : compositionMemberForName name members = some member
  idx : Nat
  target : compositionFuncIdx? funcTable name = some idx
  host_absent : host idx = none
  body : List WInstr
  code_entry : code idx = some {
    arity := 1
    nlocals := compositionNLocals member.plan
    body := body
  }
  certified : MemberCertified S code host idx (models name)

/-- The result projection used by `wFuncN` after interpreting a body. -/
def finishRun : Option Out → Option WVal
  | some (.ok _ [v]) => some v
  | some (.ret v) => some v
  | _ => none

/-- Generic straight-line simulation for the `.call` list produced by the
    audited composition lowerer. -/
theorem simCompositionCalls {C : Nat} (S : CarrierSpec C)
    (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat))
    (code : CodeTbl) (host : HostTbl) (models : String → Int → Int)
    (fuel : Nat) (locals : List WVal) :
    ∀ (callees : List String) (instrs : List WInstr) (x : Int) (v w : WVal),
      lowerCompositionCalls funcTable callees = some instrs →
      (∀ name, name ∈ callees → MemberFact S members funcTable code host models name) →
      S.Repr x v →
      finishRun (wRunF host (fun g => (code g).map (·.arity))
        (fun g args => wFuncN code host fuel g args)
        instrs locals [v]) = some w →
      S.Repr (evalCompositionCalls models callees x) w := by
  intro callees
  induction callees with
  | nil =>
      intro instrs x v w hlow _facts hv hrun
      simp only [lowerCompositionCalls, Option.some.injEq] at hlow
      subst instrs
      simp only [wRunF, finishRun, Option.some.injEq] at hrun
      subst w
      simpa [evalCompositionCalls] using hv
  | cons name rest ih =>
      intro instrs x v w hlow facts hv hrun
      have fact := facts name (by simp)
      simp only [lowerCompositionCalls] at hlow
      rw [fact.target] at hlow
      cases htail : lowerCompositionCalls funcTable rest with
      | none => simp [htail] at hlow
      | some tail =>
          rw [htail] at hlow
          simp only [Option.some.injEq] at hlow
          subst instrs
          cases hcall : wFuncN code host fuel fact.idx [v] with
          | none =>
              simp [wRunF, finishRun, fact.host_absent, fact.code_entry, popArgs, hcall] at hrun
          | some mid =>
              have hrun' : finishRun (
                  wRunF host (fun g => (code g).map (·.arity))
                    (fun g args => wFuncN code host fuel g args)
                    tail locals [mid]) = some w := by
                simpa [wRunF, fact.host_absent, fact.code_entry, popArgs, hcall]
                  using hrun
              have hmid : S.Repr (models name x) mid :=
                fact.certified fuel x v mid hv hcall
              apply ih tail (models name x) mid w htail
              · intro callee hmem
                exact facts callee (by simp [hmem])
              · exact hmid
              · exact hrun'

/-- ONE generic theorem for a composition root.  Member semantics are cited,
    never re-proved; `hcheck` and `hlow` bind the glue to the admitted plan and
    canonical lowering. -/
theorem generic_composition_certified {C : Nat} (S : CarrierSpec C)
    (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat))
    (code : CodeTbl) (host : HostTbl)
    (models : String → Int → Int)
    (rootModel : Int → Int) (self : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : CompositionRawPlan) (callees : List String)
    (hshape : plan.shape = .chain callees)
    (hcheck : AverCert.PlanCheck.checkCompositionRawPlan plan = true)
    (body : List WInstr)
    (hlow : lowerCompositionBody hostTable funcTable plan = some body)
    (hself : code self = some {
      arity := 1, nlocals := compositionNLocals plan, body := body })
    (facts : ∀ name, name ∈ callees →
      MemberFact S members funcTable code host models name)
    (hmodel : ∀ x, rootModel x = evalCompositionCalls models callees x) :
    MemberCertified S code host self rootModel := by
  intro fuel x v w hv hrun
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ fuel =>
      unfold lowerCompositionBody at hlow
      rw [if_pos hcheck] at hlow
      simp only [hshape] at hlow
      cases hcalls : lowerCompositionCalls funcTable callees with
      | none => simp [hcalls] at hlow
      | some callInstrs =>
          rw [hcalls] at hlow
          simp only [Option.some.injEq] at hlow
          subst body
          simp only [wFuncN, hself] at hrun
          change finishRun (wRunF host (fun g => (code g).map (·.arity))
            (fun g args => wFuncN code host fuel g args)
            (.localGet 0 :: callInstrs) (initLocals {
              arity := 1
              nlocals := compositionNLocals plan
              body := .localGet 0 :: callInstrs
            } [v]) []) = some w at hrun
          simp only [wRunF, initLocals] at hrun
          have hrun' : finishRun (
              wRunF host (fun g => (code g).map (·.arity))
                (fun g args => wFuncN code host fuel g args)
                callInstrs (initLocals {
                  arity := 1
                  nlocals := compositionNLocals plan
                  body := .localGet 0 :: callInstrs
                } [v]) [v]) = some w := by
            exact hrun
          rw [hmodel]
          exact simCompositionCalls S members funcTable code host models fuel _
            callees callInstrs x v w hcalls facts hv hrun'


end CompositionSoundness
