-- Lean-side canonical lowering from `expr-fragment-v1` plans to the measured
-- `CertPrelude.WInstr` tree.
--
-- This still targets the semantic instruction tree, not raw Wasm code-entry
-- bytes. v1 raw-byte equality remains verifier-side Rust TCB; this module moves
-- the plan-to-semantics lowering rule into hash-pinned Lean code.
import CertPrelude
import PlanCheck

namespace AverCert.PlanLower
open AverCert.Schema
open CertPrelude

def primInstr : FragPrim → WInstr
  | .f64Add => .f64Add
  | .f64Mul => .f64Mul
  | .f64Le => .f64Le
  | .i64Eq => .i64Eq
  | .i64LeS => .i64LeS
  | .i64LtS => .i64LtS
  | .i64GeS => .i64GeS
  | .i32LtS => .i32LtS
  | .i32GtS => .i32GtS

def popExpected : List Nat → Nat → Option (List Nat)
  | got :: rest, expected => if got = expected then some rest else none
  | [], _ => none

def popExpectedAll : List Nat → List Nat → Option (List Nat)
  | stack, [] => some stack
  | stack, expected :: rest =>
      match popExpected stack expected with
      | some stack' => popExpectedAll stack' rest
      | none => none

/-- Fuel cap for recursive lowering through nested `if` blocks. Exceeding it is
    a fail-closed unsupported fragment, matching `PlanCheck.maxFuel`. -/
def maxFuel : Nat := 10000

mutual
  def lowerNodesFuel :
      Nat → Nat → List FragNode → List Nat → Option (List WInstr × List Nat)
    | 0, _, _, _ => none
    | _fuel + 1, _carrier, [], stack => some ([], stack)
    | fuel + 1, carrier, node :: rest, stack =>
        let lowered? : Option (List WInstr × List Nat) :=
          match node.kind with
          | .local index =>
              some ([.localGet index], node.id :: stack)
          | .constBool value =>
              some ([.i32Const (if value then 1 else 0)], node.id :: stack)
          | .constI64 value =>
              some ([.i64Const value], node.id :: stack)
          | .constI32 value =>
              some ([.i32Const value], node.id :: stack)
          | .constF64Bits bits =>
              some ([.f64Const (UInt64.ofNat bits)], node.id :: stack)
          | .structGet field receiver =>
              match popExpected stack receiver with
              | some stack' => some ([.structGet carrier field], node.id :: stack')
              | none => none
          | .structGetUser tyIdx field value =>
              match popExpected stack value with
              | some stack' => some ([.structGet tyIdx field], node.id :: stack')
              | none => none
          | .refIsNull value =>
              match popExpected stack value with
              | some stack' => some ([.refIsNull], node.id :: stack')
              | none => none
          | .prim op args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some ([primInstr op], node.id :: stack')
              | none => none
          | .hostCall _role funcIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some ([.call funcIdx], node.id :: stack')
              | none => none
          | .selfCall tail funcIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' =>
                  some ([if tail then .returnCall funcIdx else .call funcIdx],
                    node.id :: stack')
              | none => none
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some [] =>
                  match lowerBlockFuel fuel carrier thenBlock,
                        lowerBlockFuel fuel carrier elseBlock with
                  | some thenInstrs, some elseInstrs =>
                      some ([.ifElse thenInstrs elseInstrs], [node.id])
                  | _, _ => none
              | _ => none
        match lowered? with
        | some (instrs, stack') =>
            match lowerNodesFuel fuel carrier rest stack' with
            | some (restInstrs, finalStack) => some (instrs ++ restInstrs, finalStack)
            | none => none
        | none => none

  def lowerBlockFuel : Nat → Nat → FragBlock → Option (List WInstr)
    | 0, _, _ => none
    | fuel + 1, carrier, block =>
        match lowerNodesFuel fuel carrier block.nodes [] with
        | some (instrs, [result]) =>
            if result = block.result then some instrs else none
        | _ => none
end

def lowerBlock (carrier : Nat) (block : FragBlock) : Option (List WInstr) :=
  lowerBlockFuel maxFuel carrier block

def lowerExprFragmentBody (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List WInstr) :=
  if AverCert.PlanCheck.checkExprFragmentRawPlan plan then
    lowerBlock carrier plan.body
  else
    none

def lowerRecursionBody (carrier : Nat) (plan : RecursionRawPlan) :
    Option (List WInstr) :=
  if AverCert.PlanCheck.checkRecursionRawPlan plan then
    lowerBlock carrier plan.body
  else
    none

def lowerMutualBody (carrier : Nat) (plan : MutualRawPlan) :
    Option (List WInstr) :=
  if AverCert.PlanCheck.checkMutualRawPlan plan then
    lowerBlock carrier plan.body
  else
    none

/-! ### Verbatim `ref.test`-dispatch WInstr lowering (mirrors `{name}Code`). -/

def lowerLeaf (S F : Nat) : VerbatimLeaf → List WInstr
  | .project tyIdx field =>
      [.localGet S, .refCast tyIdx, .structGet tyIdx field, .localSet F, .localGet F]
  | .arrayNewData arrTy _dataIdx bytes =>
      [.i32Const 0, .i32Const (Int.ofNat bytes.length), .arrayNewData arrTy bytes]
  | .refNull => [.refNull]
  | .f64Bits bits => [.f64Const (UInt64.ofNat bits)]

def lowerDispatch (S F : Nat) (first : Bool) : VerbatimDispatch → List WInstr
  | .leaf l => lowerLeaf S F l
  | .test tyIdx hit rest =>
      (if first then [] else [.localGet S]) ++
      [.refTest tyIdx, .ifElse (lowerLeaf S F hit) (lowerDispatch S F false rest)]

def lowerVerbatimBody (plan : VerbatimRawPlan) : List WInstr :=
  [.localGet 0, .localSet plan.scrutineeLocal, .localGet plan.scrutineeLocal] ++
    lowerDispatch plan.scrutineeLocal plan.fieldLocal true plan.body

/-! ### Int-face `ref.test`-dispatch WInstr lowering (mirrors `{name}Code`).

The scrutinee/field scratch locals are a fixed function of the arm count: arm
`i` (0-based, in dispatch order) spills its projected payload to local `i+1`,
the scrutinee is spilled to local `armCount + 1`. The box/add/sub function
indices come from the byte-derived host-role table PARAMETER — a role the table
lacks fail-closes the lowering. -/

/-- One hit arm: the payload projection spilled through this arm's scratch
    local `F`, then the leaf's own tail. -/
def lowerIntDispatchArm
    (hostTable : List (HostRole × Nat)) (S F tyIdx : Nat) :
    IntDispatchLeaf → Option (List WInstr)
  | .proj =>
      some [.localGet S, .refCast tyIdx, .structGet tyIdx 0, .localSet F, .localGet F]
  | .hostOp role k constFirst =>
      match AverCert.PlanCheck.hostRoleIdx? hostTable .box,
            AverCert.PlanCheck.hostRoleIdx? hostTable
              (AverCert.PlanCheck.intDispatchRoleHostRole role) with
      | some boxIdx, some hostIdx =>
          some ([.localGet S, .refCast tyIdx, .structGet tyIdx 0, .localSet F] ++
            (if constFirst then
              [.i64Const k, .call boxIdx, .localGet F, .call hostIdx]
            else
              [.localGet F, .i64Const k, .call boxIdx, .call hostIdx]))
      | _, _ => none

def lowerIntDispatchCascade
    (hostTable : List (HostRole × Nat)) (S : Nat) :
    Nat → Bool → IntDispatchCascade → Option (List WInstr)
  | _pos, _first, .default k =>
      match AverCert.PlanCheck.hostRoleIdx? hostTable .box with
      | some boxIdx => some [.i64Const k, .call boxIdx]
      | none => none
  | pos, first, .test tyIdx hit rest =>
      match lowerIntDispatchArm hostTable S (pos + 1) tyIdx hit,
            lowerIntDispatchCascade hostTable S (pos + 1) false rest with
      | some hitInstrs, some restInstrs =>
          some ((if first then [] else [.localGet S]) ++
            [.refTest tyIdx, .ifElse hitInstrs restInstrs])
      | _, _ => none

def lowerIntDispatchBody
    (hostTable : List (HostRole × Nat))
    (plan : IntDispatchRawPlan) : Option (List WInstr) :=
  let S := AverCert.PlanCheck.intDispatchArmCount plan.body + 1
  match lowerIntDispatchCascade hostTable S 0 true plan.body with
  | some cascade => some ([.localGet 0, .localSet S, .localGet S] ++ cascade)
  | none => none

def lowerStringConcatChunk (resultTy : Nat) (chunk : StringConcatChunk) :
    List WInstr :=
  [.i32Const 0, .i32Const (Int.ofNat chunk.bytes.length),
    .arrayNewData resultTy chunk.bytes]

def lowerStringConcatChunks (resultTy : Nat) :
    List StringConcatChunk → List WInstr
  | [] => []
  | chunk :: rest =>
      lowerStringConcatChunk resultTy chunk ++
        lowerStringConcatChunks resultTy rest

def lowerStringConcatBody
    (resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List WInstr) :=
  if AverCert.PlanCheck.checkStringConcatRawPlan plan then
    some (
      lowerStringConcatChunks resultTy plan.prefixes ++
      [.localGet 0] ++
      lowerStringConcatChunks resultTy plan.suffixes ++
      [.arrayNewFixed containerTy (plan.prefixes.length + 1 + plan.suffixes.length),
        .call concatFuncIdx]
    )
  else
    none

def lowerStringEqChunk (stringTy : Nat) (chunk : StringEqChunk) :
    List WInstr :=
  [.i32Const 0, .i32Const (Int.ofNat chunk.bytes.length),
    .arrayNewData stringTy chunk.bytes]

def lowerStringEqResult (stringTy : Nat) : StringEqResult → List WInstr
  | .input => [.localGet 0]
  | .literal chunk => lowerStringEqChunk stringTy chunk

def lowerStringEqBody
    (stringTy stringEqFuncIdx : Nat)
    (plan : StringEqRawPlan) : Option (List WInstr) :=
  if AverCert.PlanCheck.checkStringEqRawPlan plan then
    some (
      [.localGet 0, .localSet 1, .localGet 1, .refCast stringTy] ++
      lowerStringEqChunk stringTy plan.needle ++
      [.call stringEqFuncIdx,
        .ifElse
          (lowerStringEqResult stringTy plan.hit)
          (lowerStringEqResult stringTy plan.default)]
    )
  else
    none

def lowerConstructField : ConstructField → WInstr
  | .local index => .localGet index
  | .null => .refNull

def lowerConstructFields : List ConstructField → List WInstr
  | [] => []
  | field :: rest => lowerConstructField field :: lowerConstructFields rest

def lowerConstructBody (plan : ConstructRawPlan) : Option (List WInstr) :=
  if AverCert.PlanCheck.checkConstructRawPlan plan then
    some (lowerConstructFields plan.fields ++ [.structNew plan.structIdx plan.fields.length])
  else
    none

/-! ### `composition-plan-v1` lowering

Function indices are resolved through `funcTable`, which the acceptance
predicate computes from Wasm export bindings. The plan itself names exports.
-/

def compositionFuncIdx? (funcTable : List (String × Nat)) (name : String) : Option Nat :=
  match funcTable.find? (fun entry => entry.1 == name) with
  | some entry => some entry.2
  | none => none

def lowerCompositionCalls
    (funcTable : List (String × Nat)) : List String → Option (List WInstr)
  | [] => some []
  | callee :: rest =>
      match compositionFuncIdx? funcTable callee,
            lowerCompositionCalls funcTable rest with
      | some idx, some tail => some (.call idx :: tail)
      | _, _ => none

def lowerCompositionBody
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (plan : CompositionRawPlan) : Option (List WInstr) :=
  if AverCert.PlanCheck.checkCompositionRawPlan plan then
    match plan.shape with
    | .selfSum =>
        match AverCert.PlanCheck.hostRoleIdx? hostTable .add with
        | some addIdx => some [.localGet 0, .localGet 0, .call addIdx]
        | none => none
    | .chain callees =>
        match lowerCompositionCalls funcTable callees with
        | some calls => some (.localGet 0 :: calls)
        | none => none
  else
    none

end AverCert.PlanLower
