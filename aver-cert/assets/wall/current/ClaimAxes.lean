/-
Canonical claim axes derived inside Lean.

Policy, termination evidence, totality role, and disclosed runtime contracts
are outputs of the checked family plans. They are not producer-selected inputs.
-/
import AcceptedArtifactCore

namespace AverCert.ClaimAxes

open AverCert.Schema
open AverCert.AcceptedArtifact

def canonicalTermination : TerminationWitness :=
  { measure := .intNatAbs 0, descent := -1 }

structure AxisSpec where
  policy : Policy
  termination? : Option TerminationWitness
  totalityRole : TotalityRole

def partialAxis : AxisSpec :=
  { policy := .simulatesModel, termination? := none, totalityRole := .addSub }

def total (role : TotalityRole) : AxisSpec :=
  { policy := .simulatesModelTotally
    termination? := some canonicalTermination
    totalityRole := role }

def AxisSpec.matches (spec : AxisSpec) (obligation : Obligation) : Bool :=
  obligation.policy == spec.policy &&
  obligation.termination? == spec.termination? &&
  obligation.totalityRole == spec.totalityRole

/-- Classify the byte-bound recursion grammar first; compare the obligation's
    claimed role only after classification. The additive unary and accumulator
    shapes are disjoint from the unary multiplication shape. -/
def classifyRecursionPlanShape
    (self : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : RecursionRawPlan) : Option TotalityRole :=
  AverCert.PlanCheck.classifyRecursionPlanShape self hostTable plan

def recursionAxis (manifest : Manifest) (claim : RecursionClaim) : Option AxisSpec := do
  let plan ← recursionPlanForExport claim.exportName manifest.recursionPlans
  let role ← classifyRecursionPlanShape claim.obligation.self claim.hostTable plan
  pure (total role)

def mutualAxis (manifest : Manifest) (claim : MutualRecursionClaim) : Option AxisSpec := do
  let _ ← mutualPlanForExport claim.exportName manifest.mutualPlans
  pure (total .addSub)

def allMatch (axis : Claim → Option AxisSpec)
    (obligation : Claim → Obligation) : List Claim → Bool
  | [] => true
  | claim :: rest =>
      match axis claim with
      | some spec =>
          spec.matches (obligation claim) && allMatch axis obligation rest
      | none => false

def checkedAxes (artifact : ArtifactData) : Bool :=
  allMatch (fun _ : SymFragmentClaim => some partialAxis) (fun c => c.obligation)
      artifact.symFragmentClaims &&
  allMatch (fun _ : StringEqClaim => some partialAxis) (fun c => c.obligation)
      artifact.stringEqClaims &&
  allMatch (fun _ : StringConcatClaim => some partialAxis) (fun c => c.obligation)
      artifact.stringConcatClaims &&
  allMatch (fun _ : ConstructClaim => some partialAxis) (fun c => c.obligation)
      artifact.constructClaims &&
  allMatch (recursionAxis artifact.manifest) (fun c => c.obligation)
      artifact.recursionClaims &&
  allMatch (mutualAxis artifact.manifest) (fun c => c.obligation)
      artifact.mutualRecursionClaims &&
  allMatch (fun _ : VerbatimClaim => some partialAxis) (fun c => c.obligation)
      artifact.verbatimClaims &&
  allMatch (fun _ : IntDispatchClaim => some partialAxis) (fun c => c.obligation)
      artifact.intDispatchClaims &&
  allMatch (fun _ : FieldProjectionClaim => some partialAxis) (fun c => c.obligation)
      artifact.fieldProjectionClaims &&
  allMatch (fun _ : CompositionClaim => some partialAxis) (fun c => c.obligation)
      artifact.compositionClaims

structure ContractUse where
  box : Bool := false
  add : Bool := false
  sub : Bool := false
  mul : Bool := false
  stringEq : Bool := false
  stringConcat : Bool := false
  toIndex : Bool := false
  cmp : Bool := false
  eq : Bool := false
  addTotal : Bool := false
  subTotal : Bool := false
  mulTotal : Bool := false
deriving Repr, DecidableEq

def ContractUse.merge (left right : ContractUse) : ContractUse :=
  { box := left.box || right.box
    add := left.add || right.add
    sub := left.sub || right.sub
    mul := left.mul || right.mul
    stringEq := left.stringEq || right.stringEq
    stringConcat := left.stringConcat || right.stringConcat
    toIndex := left.toIndex || right.toIndex
    cmp := left.cmp || right.cmp
    eq := left.eq || right.eq
    addTotal := left.addTotal || right.addTotal
    subTotal := left.subTotal || right.subTotal
    mulTotal := left.mulTotal || right.mulTotal }

def useHostRole : HostRole → ContractUse
  | .box => { box := true }
  | .add => { add := true }
  | .sub => { sub := true }
  | .mul => { mul := true }
  | .toIndex => { toIndex := true }
  | .cmp => { cmp := true }
  | .eq => { eq := true }

def useFragBlockFuel : Nat → FragBlock → ContractUse
  | 0, _ => {}
  | fuel + 1, block =>
      block.nodes.foldl (fun used node =>
        let here := match node.kind with
          | .hostCall role _ _ => useHostRole role
          | .ifElse _ thenBlock elseBlock =>
              (useFragBlockFuel fuel thenBlock).merge
                (useFragBlockFuel fuel elseBlock)
          -- The fused vector read calls both the to-index and box helpers.
          | .vectorGetOrDefault _ _ _ _ => { box := true, toIndex := true }
          | _ => {}
        used.merge here) {}

def useFragBlock (block : FragBlock) : ContractUse :=
  useFragBlockFuel AverCert.PlanLower.maxFuel block

def useSymFragment (claim : SymFragmentClaim) : Option ContractUse := do
  let plan ← AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
    claim.hostTable claim.structTable claim.plan
  pure (useFragBlock plan.body)

def useRecursion (manifest : Manifest) (claim : RecursionClaim) : Option ContractUse := do
  let plan ← recursionPlanForExport claim.exportName manifest.recursionPlans
  let role ← classifyRecursionPlanShape claim.obligation.self claim.hostTable plan
  match role with
  | .addSub =>
      pure { box := true, add := true, sub := true
             addTotal := true, subTotal := true }
  | .mul =>
      pure { box := true, sub := true, mul := true
             addTotal := true, subTotal := true, mulTotal := true }

def useMutual (manifest : Manifest) (claim : MutualRecursionClaim) : Option ContractUse := do
  let _ ← mutualPlanForExport claim.exportName manifest.mutualPlans
  pure { box := true, sub := true, addTotal := true, subTotal := true }

def useIntDispatchLeaf : IntDispatchLeaf → ContractUse
  | .proj => {}
  | .hostOp .add _ _ => { box := true, add := true }
  | .hostOp .sub _ _ => { box := true, sub := true }
  | .const _ => { box := true }

def useIntDispatchCascade : IntDispatchCascade → ContractUse
  | .default _ => { box := true }
  | .test _ hit rest =>
      (useIntDispatchLeaf hit).merge (useIntDispatchCascade rest)

def useIntDispatch (manifest : Manifest) (claim : IntDispatchClaim) : Option ContractUse := do
  let plan ← intDispatchPlanForExport claim.exportName manifest.intDispatchPlans
  pure (useIntDispatchCascade plan.body)

def usesOf (use : Claim → Option ContractUse) : List Claim → Option ContractUse
  | [] => some {}
  | claim :: rest => do
      let head ← use claim
      let tail ← usesOf use rest
      pure (head.merge tail)

def requiredContractUse (artifact : ArtifactData) : Option ContractUse := do
  let sym ← usesOf useSymFragment artifact.symFragmentClaims
  let stringEq ← usesOf (fun _ : StringEqClaim =>
    some { stringEq := true }) artifact.stringEqClaims
  let stringConcat ← usesOf (fun _ : StringConcatClaim =>
    some { stringConcat := true }) artifact.stringConcatClaims
  let construct ← usesOf (fun _ : ConstructClaim => some {}) artifact.constructClaims
  let recursion ← usesOf (useRecursion artifact.manifest) artifact.recursionClaims
  let mutualUse ← usesOf (useMutual artifact.manifest) artifact.mutualRecursionClaims
  let verbatim ← usesOf (fun _ : VerbatimClaim => some {}) artifact.verbatimClaims
  let intDispatch ← usesOf (useIntDispatch artifact.manifest) artifact.intDispatchClaims
  let projection ← usesOf (fun _ : FieldProjectionClaim => some {})
    artifact.fieldProjectionClaims
  -- Every accepted composition closure contains a `selfSum` leaf and its
  -- canonical host table contains exactly the add role.
  let composition ← usesOf (fun _ : CompositionClaim =>
    some { add := true }) artifact.compositionClaims
  let used := sym.merge stringEq
  let used := used.merge stringConcat
  let used := used.merge construct
  let used := used.merge recursion
  let used := used.merge mutualUse
  let used := used.merge verbatim
  let used := used.merge intDispatch
  let used := used.merge projection
  pure (used.merge composition)

def boxContract : String :=
  "__rt_aint_from_i64 (box i64 -> carrier)"
def addContract : String :=
  "Int.add (carrier add = exact integer addition on represented values)"
def subContract : String :=
  "Int.sub (carrier sub = exact integer subtraction on represented values)"
def mulContract : String :=
  "Int.mul (carrier mul = exact integer multiplication on represented values)"
def stringEqContract : String :=
  "String.eq (WVal byte-array equality; non-arrays compare false)"
def stringConcatContract : String :=
  "String.concat (container-of-string-arrays -> byte-concatenated array)"
def toIndexContract : String :=
  "__aint_to_index (carrier -> i32 array index; [0, 2^31) passes, else -1)"
def cmpContract : String :=
  "__aint_cmp (carrier pair -> i32 three-way sign: -1 less, 0 equal, 1 greater)"
def eqContract : String :=
  "__aint_eq (carrier pair -> i32 boolean: 1 when the represented integers are equal, else 0)"
def addTotalContract : String :=
  "Int.add (carrier add = exact integer addition on represented values); total on represented values"
def subTotalContract : String :=
  "Int.sub (carrier sub = exact integer subtraction on represented values); total on represented values"
def mulTotalContract : String :=
  "Int.mul (carrier mul = exact integer multiplication on represented values); total on represented values"

def ContractUse.contracts (use : ContractUse) : List String :=
  (if use.box then [boxContract] else []) ++
  (if use.add then [addContract] else []) ++
  (if use.sub then [subContract] else []) ++
  (if use.mul then [mulContract] else []) ++
  (if use.stringEq then [stringEqContract] else []) ++
  (if use.stringConcat then [stringConcatContract] else []) ++
  (if use.toIndex then [toIndexContract] else []) ++
  (if use.cmp then [cmpContract] else []) ++
  (if use.eq then [eqContract] else []) ++
  (if use.addTotal then [addTotalContract] else []) ++
  (if use.subTotal then [subTotalContract] else []) ++
  (if use.mulTotal then [mulTotalContract] else [])

def requiredContracts (artifact : ArtifactData) : Option (List String) := do
  let use ← requiredContractUse artifact
  pure use.contracts

def contractsMatch (artifact : ArtifactData) : Bool :=
  requiredContracts artifact == some artifact.manifest.subject.contracts

/-- All producer-selectable claim metadata that is instead canonicalized by
    the checked family and plan. -/
def checked (artifact : ArtifactData) : Bool :=
  checkedAxes artifact && contractsMatch artifact

end AverCert.ClaimAxes
