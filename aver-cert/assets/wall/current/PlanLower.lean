-- Lean-side canonical lowering from checked plans to the measured
-- `CertPrelude.WInstr` tree. `PlanBytes` lowers that tree to exact code-entry
-- bytes, and `AcceptedArtifact` binds it to the decoded artifact.
import CertPrelude
import PlanCheck

namespace AverCert.PlanLower
open AverCert.Schema
open CertPrelude

def lowerFieldProjectionBody
    (structIdx fieldCount : Nat) (plan : FieldProjectionRawPlan) :
    Option (List WInstr) :=
  if AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan then
    some [.localGet 0, .localSet 2, .localGet 2, .refCast structIdx,
          .structGet structIdx plan.fieldIdx, .localSet 1, .localGet 1]
  else none

def primInstr : FragPrim → WInstr
  | .f64Add => .f64Add
  | .f64Mul => .f64Mul
  | .f64Le => .f64Le
  | .f64Ge => .f64Ge
  | .f64Lt => .f64Lt
  | .f64Gt => .f64Gt
  | .f64Eq => .f64Eq
  | .i64Eq => .i64Eq
  | .i64LeS => .i64LeS
  | .i64LtS => .i64LtS
  | .i64GeS => .i64GeS
  | .i64GtS => .i64GtS
  | .i32Eq => .i32Eq
  | .i32LtS => .i32LtS
  | .i32GtS => .i32GtS
  | .i32GeS => .i32GeS
  | .i32And => .i32And

def popExpected : List Nat → Nat → Option (List Nat)
  | got :: rest, expected => if got = expected then some rest else none
  | [], _ => none

def popExpectedAll : List Nat → List Nat → Option (List Nat)
  | stack, [] => some stack
  | stack, expected :: rest =>
      match popExpected stack expected with
      | some stack' => popExpectedAll stack' rest
      | none => none

/-- Semantic lowering uses the checker's one canonical recursive-plan budget. -/
abbrev maxFuel : Nat := AverCert.PlanCheck.maxFuel

/-- The fused `Option.withDefault(Vector.get(vec, idx), d)` template exactly as
    the wasm-gc emitter produces it (`from_mir/builtins.rs`): extract the index
    through `__aint_to_index`, test `idx >= 0 (signed) AND idx < len
    (unsigned)`, read the element on hit, box the literal default on miss.
    Holes: `toIndexIdx` (the `__aint_to_index` function index), `boxIdx` (the
    `__rt_aint_from_i64` function index), `arrTy` (the vector's array type
    index), `d` (the literal default). Locals pinned: vec = 0, idx = 1. -/
def vectorGetOrDefaultTemplate
    (toIndexIdx boxIdx arrTy : Nat) (d : Int) : List WInstr :=
  [ .localGet 1, .call toIndexIdx, .i32Const 0, .i32GeS,
    .localGet 1, .call toIndexIdx,
    .localGet 0, .arrayLen, .i32LtU,
    .i32And,
    .ifElse
      [.localGet 0, .localGet 1, .call toIndexIdx, .arrayGet arrTy]
      [.i64Const d, .call boxIdx] ]

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
          | .structNew tyIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' =>
                  some ([.structNew tyIdx args.length], node.id :: stack')
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
          -- Values already on the symbolic stack stay beneath the branch,
          -- exactly as the wasm `if` leaves the remaining operand stack in
          -- place (`InterpreterSequencing.wRunF_frame`).
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some stack' =>
                  match lowerBlockFuel fuel carrier thenBlock,
                        lowerBlockFuel fuel carrier elseBlock with
                  | some thenInstrs, some elseInstrs =>
                      some ([.ifElse thenInstrs elseInstrs], node.id :: stack')
                  | _, _ => none
              | none => none
          | .vectorGetOrDefault arrTy toIndexIdx boxIdx default =>
              -- Monolithic template over pinned locals 0/1; it consumes no
              -- stack operands, so it is canonical only as the sole value.
              match stack with
              | [] =>
                  some (vectorGetOrDefaultTemplate toIndexIdx boxIdx arrTy default,
                    [node.id])
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

The scrutinee/field scratch locals are a fixed function of the payload-BINDING
arm count: the `j`-th binding (`proj`/`hostOp`) arm (0-based, in dispatch order)
spills its projected payload to local `j+1`, the scrutinee is spilled to local
`bindArmCount + 1`; a `const` arm reads no field and spills no local. The
box/add/sub function indices come from the byte-derived host-role table
PARAMETER — a role the table lacks fail-closes the lowering. -/

/-- One hit arm: the payload projection spilled through this arm's scratch
    local `F`, then the leaf's own tail. -/
def lowerIntDispatchArm
    (hostTable : List (HostRole × Nat)) (S F tyIdx : Nat) :
    IntDispatchLeaf → Option (List WInstr)
  | .const _ => none
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
  | pos, first, .test tyIdx (.const k) rest =>
      -- A const (nullary) arm reads no field and spills no local: its if-branch
      -- is the terminal-style `i64.const k; call box` (NO projection prefix), and
      -- the binding position `pos` does not advance.
      match AverCert.PlanCheck.hostRoleIdx? hostTable .box,
            lowerIntDispatchCascade hostTable S pos false rest with
      | some boxIdx, some restInstrs =>
          some ((if first then [] else [.localGet S]) ++
            [.refTest tyIdx, .ifElse [.i64Const k, .call boxIdx] restInstrs])
      | _, _ => none
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
  let S := AverCert.PlanCheck.bindArmCount plan.body + 1
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

def lowerConstructField (_structIdx : Nat) : ConstructField → WInstr
  | .local index => .localGet index
  | .null => .refNull

def lowerConstructFields (structIdx : Nat) : List ConstructField → List WInstr
  | [] => []
  | field :: rest => lowerConstructField structIdx field :: lowerConstructFields structIdx rest

def lowerConstructBody (structIdx : Nat) (plan : ConstructRawPlan) : Option (List WInstr) :=
  if AverCert.PlanCheck.checkConstructRawPlan plan then
    some (lowerConstructFields structIdx plan.fields ++ [.structNew structIdx plan.fields.length])
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
