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

end AverCert.PlanLower
