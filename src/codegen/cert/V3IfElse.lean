import CertPrelude
import SchemaCore

namespace V3ExprFragmentIfElse
open CertPrelude AverCert.Schema

def StackOK {C : Nat} (S : CarrierSpec C) (n : Int) (base : List WVal) :
    Option Out → Prop
  | some (.ok _ (w :: rest)) => rest = base ∧ S.Repr n w
  | _ => False

def BlockOK {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (n : Int) (base : List WVal) (instrs : List WInstr)
    (locals : List WVal) (stack : List WVal) : Prop :=
  ∀ out, wRunF host ar callee instrs locals stack = some out →
    StackOK S n base (some out)

/-- The family-dispatch branch arm, lifted unchanged to the expr-fragment
    spike lane.  Either recursively simulated nested block preserves the same
    `StackOK` boundary through the audited `.ifElse` instruction. -/
theorem blockOK_ifElse {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (n : Int) (base : List WVal) (thenB elseB : List WInstr)
    (locals : List WVal) (stack : List WVal) (cond : Bool)
    (hbranch : BlockOK S host ar callee n base
      (if cond then thenB else elseB) locals stack) :
    BlockOK S host ar callee n base [.ifElse thenB elseB]
      locals (b32 cond :: stack) := by
  cases cond with
  | false =>
      intro out hrun
      cases hb : wRunF host ar callee elseB locals stack with
      | none => simp [wRunF, b32, hb] at hrun
      | some branchOut =>
          cases branchOut <;> simp [wRunF, b32, hb] at hrun
          all_goals subst out; exact hbranch _ (by simpa using hb)
  | true =>
      intro out hrun
      cases hb : wRunF host ar callee thenB locals stack with
      | none => simp [wRunF, b32, hb] at hrun
      | some branchOut =>
          cases branchOut <;> simp [wRunF, b32, hb] at hrun
          all_goals subst out; exact hbranch _ (by simpa using hb)

#print axioms blockOK_ifElse

end V3ExprFragmentIfElse
