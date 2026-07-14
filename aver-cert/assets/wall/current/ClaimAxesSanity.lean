import ClaimAxes

namespace AverCert.ClaimAxesSanity

open AverCert.Schema
open AverCert.ClaimAxes
open CertPrelude

def dummyObligation
    (policy : Policy)
    (termination? : Option TerminationWitness)
    (role : TotalityRole) : Obligation :=
  { export_ := "probe"
    policy := policy
    termination? := termination?
    totalityRole := role
    carrier := 0
    code := fun _ => none
    host := fun _ _ _ _ _ _ => none
    self := 0
    Dom := Unit
    Cod := Unit
    domRepr := fun _ _ _ => True
    codRepr := fun _ _ _ => True
    model := id }

example :
    (total .mul).Matches
      (dummyObligation .simulatesModelTotally (some canonicalTermination) .mul) := by
  simp [AxisSpec.Matches, total, dummyObligation]

example : ¬
    (total .mul).Matches
      (dummyObligation .simulatesModelTotally
        (some { measure := .intNatAbs 1, descent := -1 }) .mul) := by
  simp [AxisSpec.Matches, total, canonicalTermination, dummyObligation]

example : ¬
    partialAxis.Matches
      (dummyObligation .simulatesModelTotally (some canonicalTermination) .addSub) := by
  simp [AxisSpec.Matches, partialAxis, dummyObligation]

def dispatchUse : ContractUse :=
  useIntDispatchCascade
    (.test 7 (.hostOp .add 2 false)
      (.test 8 (.hostOp .sub (-3) true) (.default 0)))

example : dispatchUse.contracts =
    [boxContract, addContract, subContract] := by
  rfl

def totalMulUse : ContractUse :=
  { box := true, sub := true, mul := true
    addTotal := true, subTotal := true, mulTotal := true }

example : totalMulUse.contracts =
    [boxContract, subContract, mulContract,
      addTotalContract, subTotalContract, mulTotalContract] := by
  rfl

example : ({ stringConcat := true, add := true } : ContractUse).contracts =
    [addContract, stringConcatContract] := by
  rfl

end AverCert.ClaimAxesSanity
