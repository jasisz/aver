-- Lean-side canonical byte lowering from `expr-fragment-v1` raw plans to the
-- exact Wasm code-entry byte sequence used by the current cert island.
--
-- This is still not a full Wasm module parser. It is the plan-first byte
-- encoder for one checked profile: local declarations + expression body +
-- body-size prefix.
import PlanCheck

namespace AverCert.PlanBytes
open AverCert.Schema

def ulebFuel : Nat → Nat → Option (List Nat)
  | 0, _ => none
  | fuel + 1, value =>
      let byte := value % 128
      let rest := value / 128
      if rest = 0 then
        some [byte]
      else
        match ulebFuel fuel rest with
        | some bytes => some ((byte + 128) :: bytes)
        | none => none

def uleb32 (value : Nat) : Option (List Nat) :=
  if value < 4294967296 then ulebFuel 5 value else none

def slebFuel : Nat → Int → Option (List Nat)
  | 0, _ => none
  | fuel + 1, value =>
      let byte := Int.toNat (value % 128)
      let rest := value / 128
      let signSet := 64 ≤ byte
      let done := (rest = 0 ∧ !signSet) ∨ (rest = -1 ∧ signSet)
      let outByte := if done then byte else byte + 128
      if done then
        some [outByte]
      else
        match slebFuel fuel rest with
        | some bytes => some (outByte :: bytes)
        | none => none

def inI32Range (value : Int) : Bool :=
  if (-2147483648 : Int) ≤ value then
    if value ≤ 2147483647 then true else false
  else
    false

def inI64Range (value : Int) : Bool :=
  if (-9223372036854775808 : Int) ≤ value then
    if value ≤ 9223372036854775807 then true else false
  else
    false

def sleb32 (value : Int) : Option (List Nat) :=
  if inI32Range value then slebFuel 5 value else none

def sleb64 (value : Int) : Option (List Nat) :=
  if inI64Range value then slebFuel 10 value else none

/-- Concrete heap-type indices (inside a reftype `0x63/0x64 <ht>`, a block type,
    or a `ref.cast`/`ref.test`/`ref.null` immediate) are encoded as SIGNED s33
    LEB128 per the Wasm spec, not unsigned: index 64 is `c0 00`, never `40`.
    Indices below 64 coincide with the unsigned encoding. Instruction TYPE
    indices (`struct.get`, `array.new_data`, …) stay unsigned u32. -/
def s33HeapIdx (idx : Nat) : Option (List Nat) :=
  if idx < 4294967296 then slebFuel 6 (Int.ofNat idx) else none

def f64Bytes (bits : Nat) : Option (List Nat) :=
  if bits < 18446744073709551616 then
    some [
      (bits / (2 ^ 0)) % 256,
      (bits / (2 ^ 8)) % 256,
      (bits / (2 ^ 16)) % 256,
      (bits / (2 ^ 24)) % 256,
      (bits / (2 ^ 32)) % 256,
      (bits / (2 ^ 40)) % 256,
      (bits / (2 ^ 48)) % 256,
      (bits / (2 ^ 56)) % 256
    ]
  else
    none

/-- Block-type bytes for an `if (result …)`. Scalar results are their value
    type byte; an Int-carrier result is the ref-null heap type `63 <carrier>`
    (the value-if of a fuel-recursion body). `carrier` supplies that index. -/
def blockTypeBytes (carrier : Nat) : FragTy → Option (List Nat)
  | .boolI32 => some [0x7f]
  | .rawI32 => some [0x7f]
  | .i64 => some [0x7e]
  | .f64 => some [0x7c]
  | .intCarrier => (s33HeapIdx carrier).map (fun c => [0x63] ++ c)
  | .ref => none
  | .adtRef => none

def primBytes : FragPrim → List Nat
  | .f64Add => [0xa0]
  | .f64Mul => [0xa2]
  | .f64Le => [0x65]
  | .i64Eq => [0x51]
  | .i64LtS => [0x53]
  | .i64LeS => [0x57]
  | .i64GeS => [0x59]
  | .i32LtS => [0x48]
  | .i32GtS => [0x4a]

def popExpected : List Nat → Nat → Option (List Nat)
  | got :: rest, expected => if got = expected then some rest else none
  | [], _ => none

def popExpectedAll : List Nat → List Nat → Option (List Nat)
  | stack, [] => some stack
  | stack, expected :: rest =>
      match popExpected stack expected with
      | some stack' => popExpectedAll stack' rest
      | none => none

/-- Fuel cap for recursive byte lowering through nested `if` blocks. -/
def maxFuel : Nat := 10000

mutual
  def lowerNodesBytesFuel :
      Nat → Nat → List FragNode → List Nat → Option (List Nat × List Nat)
    | 0, _, _, _ => none
    | _fuel + 1, _carrier, [], stack => some ([], stack)
    | fuel + 1, carrier, node :: rest, stack =>
        let lowered? : Option (List Nat × List Nat) :=
          match node.kind with
          | .local index =>
              match uleb32 index with
              | some indexBytes => some ([0x20] ++ indexBytes, node.id :: stack)
              | none => none
          | .constBool value =>
              match sleb32 (if value then 1 else 0) with
              | some valueBytes => some ([0x41] ++ valueBytes, node.id :: stack)
              | none => none
          | .constI64 value =>
              match sleb64 value with
              | some valueBytes => some ([0x42] ++ valueBytes, node.id :: stack)
              | none => none
          | .constI32 value =>
              match sleb32 value with
              | some valueBytes => some ([0x41] ++ valueBytes, node.id :: stack)
              | none => none
          | .constF64Bits bits =>
              match f64Bytes bits with
              | some valueBytes => some ([0x44] ++ valueBytes, node.id :: stack)
              | none => none
          | .structGet field receiver =>
              match popExpected stack receiver, uleb32 0x02, uleb32 carrier, uleb32 field with
              | some stack', some opBytes, some carrierBytes, some fieldBytes =>
                  some ([0xfb] ++ opBytes ++ carrierBytes ++ fieldBytes, node.id :: stack')
              | _, _, _, _ => none
          | .structGetUser tyIdx field value =>
              match popExpected stack value, uleb32 0x02, uleb32 tyIdx, uleb32 field with
              | some stack', some opBytes, some tyBytes, some fieldBytes =>
                  some ([0xfb] ++ opBytes ++ tyBytes ++ fieldBytes, node.id :: stack')
              | _, _, _, _ => none
          | .refIsNull value =>
              match popExpected stack value with
              | some stack' => some ([0xd1], node.id :: stack')
              | none => none
          | .prim op args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some (primBytes op, node.id :: stack')
              | none => none
          | .hostCall _role funcIdx args =>
              match popExpectedAll stack args.reverse, uleb32 funcIdx with
              | some stack', some idxBytes => some ([0x10] ++ idxBytes, node.id :: stack')
              | _, _ => none
          | .selfCall tail funcIdx args =>
              match popExpectedAll stack args.reverse, uleb32 funcIdx with
              | some stack', some idxBytes =>
                  some ((if tail then [0x12] else [0x10]) ++ idxBytes, node.id :: stack')
              | _, _ => none
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some [] =>
                  match blockTypeBytes carrier node.ty,
                        lowerBlockBytesFuel fuel carrier thenBlock,
                        lowerBlockBytesFuel fuel carrier elseBlock with
                  | some blockTy, some thenBytes, some elseBytes =>
                      some ([0x04] ++ blockTy ++ thenBytes ++ [0x05] ++ elseBytes ++ [0x0b],
                        [node.id])
                  | _, _, _ => none
              | _ => none
        match lowered? with
        | some (bytes, stack') =>
            match lowerNodesBytesFuel fuel carrier rest stack' with
            | some (restBytes, finalStack) => some (bytes ++ restBytes, finalStack)
            | none => none
        | none => none

  def lowerBlockBytesFuel : Nat → Nat → FragBlock → Option (List Nat)
    | 0, _, _ => none
    | fuel + 1, carrier, block =>
        match lowerNodesBytesFuel fuel carrier block.nodes [] with
        | some (bytes, [result]) =>
            if result = block.result then some bytes else none
        | _ => none
end

def lowerBlockBytes (carrier : Nat) (block : FragBlock) : Option (List Nat) :=
  lowerBlockBytesFuel maxFuel carrier block

def lowerExprFragmentExprBytes (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List Nat) :=
  if AverCert.PlanCheck.checkExprFragmentRawPlan plan then
    match lowerBlockBytes carrier plan.body with
    | some bytes => some (bytes ++ [0x0b])
    | none => none
  else
    none

def lowerExprFragmentBodyBytes (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerExprFragmentExprBytes carrier plan with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

def lowerExprFragmentCodeEntry (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List Nat) :=
  match lowerExprFragmentBodyBytes carrier plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

def lowerRecursionExprBytes (carrier : Nat) (plan : RecursionRawPlan) :
    Option (List Nat) :=
  if AverCert.PlanCheck.checkRecursionRawPlan plan then
    match lowerBlockBytes carrier plan.body with
    | some bytes => some (bytes ++ [0x0b])
    | none => none
  else
    none

def lowerRecursionBodyBytes (carrier : Nat) (plan : RecursionRawPlan) :
    Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerRecursionExprBytes carrier plan with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

def lowerRecursionCodeEntry (carrier : Nat) (plan : RecursionRawPlan) :
    Option (List Nat) :=
  match lowerRecursionBodyBytes carrier plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

def lowerMutualExprBytes (carrier : Nat) (plan : MutualRawPlan) :
    Option (List Nat) :=
  if AverCert.PlanCheck.checkMutualRawPlan plan then
    match lowerBlockBytes carrier plan.body with
    | some bytes => some (bytes ++ [0x0b])
    | none => none
  else
    none

def lowerMutualBodyBytes (carrier : Nat) (plan : MutualRawPlan) :
    Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerMutualExprBytes carrier plan with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

def lowerMutualCodeEntry (carrier : Nat) (plan : MutualRawPlan) :
    Option (List Nat) :=
  match lowerMutualBodyBytes carrier plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

/-! ### Verbatim `ref.test`-dispatch byte lowering (exact code-entry bytes).

`ref.test`/`ref.cast`/`ref.null`/block-type heap indices are s33 SIGNED;
`struct.get`/`array.new_data` type/field/data indices are uleb32. -/

def lowerLeafBytes (S F resultHeapTy : Nat) : VerbatimLeaf → Option (List Nat)
  | .project tyIdx field =>
      match uleb32 S, s33HeapIdx tyIdx, uleb32 tyIdx, uleb32 field, uleb32 F with
      | some sB, some castTy, some getTy, some fieldB, some fB =>
          some ([0x20] ++ sB ++ [0xfb, 0x16] ++ castTy ++
                [0xfb, 0x02] ++ getTy ++ fieldB ++ [0x21] ++ fB ++ [0x20] ++ fB)
      | _, _, _, _, _ => none
  | .arrayNewData arrTy dataIdx bytes =>
      match sleb32 0, sleb32 (Int.ofNat bytes.length), uleb32 arrTy, uleb32 dataIdx with
      | some off, some len, some arrTyB, some dataIdxB =>
          some ([0x41] ++ off ++ [0x41] ++ len ++ [0xfb, 0x09] ++ arrTyB ++ dataIdxB)
      | _, _, _, _ => none
  | .refNull =>
      match s33HeapIdx resultHeapTy with
      | some ht => some ([0xd0] ++ ht)
      | none => none
  | .f64Bits bits =>
      match f64Bytes bits with
      | some fb => some ([0x44] ++ fb)
      | none => none

def lowerDispatchBytes (S F resultHeapTy : Nat) (first : Bool) :
    VerbatimDispatch → Option (List Nat)
  | .leaf l => lowerLeafBytes S F resultHeapTy l
  | .test tyIdx hit rest =>
      match (if first then some ([] : List Nat)
             else (uleb32 S).map (fun b => [0x20] ++ b)),
            s33HeapIdx tyIdx, s33HeapIdx resultHeapTy,
            lowerLeafBytes S F resultHeapTy hit,
            lowerDispatchBytes S F resultHeapTy false rest with
      | some reload, some testTy, some blockTy, some hitBytes, some restBytes =>
          some (reload ++ [0xfb, 0x14] ++ testTy ++ [0x04, 0x63] ++ blockTy ++
                hitBytes ++ [0x05] ++ restBytes ++ [0x0b])
      | _, _, _, _, _ => none

def lowerVerbatimExprBytes (plan : VerbatimRawPlan) : Option (List Nat) :=
  match uleb32 plan.scrutineeLocal,
        lowerDispatchBytes plan.scrutineeLocal plan.fieldLocal plan.resultHeapTy true plan.body with
  | some sB, some dispatchBytes =>
      some ([0x20, 0x00] ++ [0x21] ++ sB ++ [0x20] ++ sB ++ dispatchBytes ++ [0x0b])
  | _, _ => none

/-- Local declarations. A projecting (widened-match) body declares the field
    scratch local (of the result heap type) first, then the eqref scrutinee, then
    the always-present unused Int-carrier scratch; a non-projecting (variant
    dispatch) body declares only the scrutinee and the carrier scratch. -/
def lowerVerbatimLocalsBytes (carrier resultHeapTy : Nat) (hasProj : Bool) :
    Option (List Nat) :=
  match s33HeapIdx carrier with
  | some carrierB =>
      if hasProj then
        match s33HeapIdx resultHeapTy with
        | some rhtB =>
            some ([0x03] ++ [0x01, 0x63] ++ rhtB ++ [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
        | none => none
      else
        some ([0x02] ++ [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
  | none => none

def lowerVerbatimBodyBytes (carrier : Nat) (plan : VerbatimRawPlan) : Option (List Nat) :=
  match lowerVerbatimLocalsBytes carrier plan.resultHeapTy
          (AverCert.PlanCheck.dispatchHasProjection plan.body),
        lowerVerbatimExprBytes plan with
  | some localsBytes, some exprBytes => some (localsBytes ++ exprBytes)
  | _, _ => none

def lowerVerbatimCodeEntry (carrier : Nat) (plan : VerbatimRawPlan) : Option (List Nat) :=
  match lowerVerbatimBodyBytes carrier plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

def lowerStringConcatChunkBytes
    (resultTy : Nat) (chunk : StringConcatChunk) : Option (List Nat) :=
  match sleb32 0,
        sleb32 (Int.ofNat chunk.bytes.length),
        uleb32 0x09,
        uleb32 resultTy,
        uleb32 chunk.dataIdx with
  | some offsetBytes, some lenBytes, some opBytes, some resultTyBytes, some dataIdxBytes =>
      some (
        [0x41] ++ offsetBytes ++
        [0x41] ++ lenBytes ++
        [0xfb] ++ opBytes ++ resultTyBytes ++ dataIdxBytes
      )
  | _, _, _, _, _ => none

def lowerStringConcatChunksBytes (resultTy : Nat) :
    List StringConcatChunk → Option (List Nat)
  | [] => some []
  | chunk :: rest =>
      match lowerStringConcatChunkBytes resultTy chunk,
            lowerStringConcatChunksBytes resultTy rest with
      | some chunkBytes, some restBytes => some (chunkBytes ++ restBytes)
      | _, _ => none

def lowerStringConcatExprBytes
    (resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkStringConcatRawPlan plan then
    match lowerStringConcatChunksBytes resultTy plan.prefixes,
          uleb32 0,
          lowerStringConcatChunksBytes resultTy plan.suffixes,
          uleb32 0x08,
          uleb32 containerTy,
          uleb32 (plan.prefixes.length + 1 + plan.suffixes.length),
          uleb32 concatFuncIdx with
    | some prefixBytes, some localIdxBytes, some suffixBytes,
      some arrayNewFixedOpBytes, some containerTyBytes, some partCountBytes,
      some concatFuncIdxBytes =>
        some (
          prefixBytes ++
          [0x20] ++ localIdxBytes ++
          suffixBytes ++
          [0xfb] ++ arrayNewFixedOpBytes ++ containerTyBytes ++ partCountBytes ++
          [0x10] ++ concatFuncIdxBytes ++
          [0x0b]
        )
    | _, _, _, _, _, _, _ => none
  else
    none

def lowerStringConcatBodyBytes
    (carrier resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerStringConcatExprBytes resultTy containerTy concatFuncIdx plan with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

def lowerStringConcatCodeEntry
    (carrier resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List Nat) :=
  match lowerStringConcatBodyBytes carrier resultTy containerTy concatFuncIdx plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

def lowerStringEqChunkBytes
    (stringTy : Nat)
    (chunk : StringEqChunk) : Option (List Nat) :=
  match sleb32 0, sleb32 (Int.ofNat chunk.bytes.length),
        uleb32 0x09, uleb32 stringTy, uleb32 chunk.dataIdx with
  | some offsetBytes, some lenBytes, some arrayNewDataOpBytes,
    some stringTyBytes, some dataIdxBytes =>
      some (
        [0x41] ++ offsetBytes ++
        [0x41] ++ lenBytes ++
        [0xfb] ++ arrayNewDataOpBytes ++ stringTyBytes ++ dataIdxBytes
      )
  | _, _, _, _, _ => none

def lowerStringEqResultBytes
    (stringTy : Nat) : StringEqResult → Option (List Nat)
  | .input =>
      match uleb32 0 with
      | some inputIdxBytes => some ([0x20] ++ inputIdxBytes)
      | none => none
  | .literal chunk => lowerStringEqChunkBytes stringTy chunk

def lowerStringEqExprBytes
    (stringTy stringEqFuncIdx : Nat)
    (plan : StringEqRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkStringEqRawPlan plan then
    match uleb32 0, uleb32 1, uleb32 1, uleb32 0x17,
          s33HeapIdx stringTy, lowerStringEqChunkBytes stringTy plan.needle,
          uleb32 stringEqFuncIdx, s33HeapIdx stringTy,
          lowerStringEqResultBytes stringTy plan.hit,
          lowerStringEqResultBytes stringTy plan.default with
    | some inputIdxBytes, some scratchIdxBytes, some _localOneBytes,
      some refCastOpBytes, some stringTyBytes, some needleBytes,
      some stringEqFuncIdxBytes, some blockTypeBytes, some hitBytes,
      some defaultBytes =>
        some (
          [0x20] ++ inputIdxBytes ++
          [0x21] ++ scratchIdxBytes ++
          [0x20] ++ scratchIdxBytes ++
          [0xfb] ++ refCastOpBytes ++ stringTyBytes ++
          needleBytes ++
          [0x10] ++ stringEqFuncIdxBytes ++
          [0x04, 0x63] ++ blockTypeBytes ++
          hitBytes ++
          [0x05] ++
          defaultBytes ++
          [0x0b, 0x0b]
        )
    | _, _, _, _, _, _, _, _, _, _ => none
  else
    none

def lowerStringEqBodyBytes
    (carrier stringTy stringEqFuncIdx : Nat)
    (plan : StringEqRawPlan) : Option (List Nat) :=
  match uleb32 2, uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerStringEqExprBytes stringTy stringEqFuncIdx plan with
  | some localDeclCount, some localCount, some carrierLocalCount,
    some carrierBytes, some exprBytes =>
      some (
        localDeclCount ++
        localCount ++ [0x6d] ++
        carrierLocalCount ++ [0x63] ++ carrierBytes ++
        exprBytes
      )
  | _, _, _, _, _ => none

def lowerStringEqCodeEntry
    (carrier stringTy stringEqFuncIdx : Nat)
    (plan : StringEqRawPlan) : Option (List Nat) :=
  match lowerStringEqBodyBytes carrier stringTy stringEqFuncIdx plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

def lowerConstructFieldBytes : ConstructField → Option (List Nat)
  | .local index =>
      match uleb32 index with
      | some indexBytes => some ([0x20] ++ indexBytes)
      | none => none
  | .null => none

def lowerConstructFieldsBytes : List ConstructField → Option (List Nat)
  | [] => some []
  | field :: rest =>
      match lowerConstructFieldBytes field, lowerConstructFieldsBytes rest with
      | some fieldBytes, some restBytes => some (fieldBytes ++ restBytes)
      | _, _ => none

def lowerConstructExprBytes (plan : ConstructRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkConstructRawPlan plan then
    match lowerConstructFieldsBytes plan.fields,
          uleb32 0x00,
          uleb32 plan.structIdx with
    | some fieldBytes, some structNewOpBytes, some structIdxBytes =>
        some (fieldBytes ++ [0xfb] ++ structNewOpBytes ++ structIdxBytes ++ [0x0b])
    | _, _, _ => none
  else
    none

def lowerConstructBodyBytes
    (carrier : Nat)
    (plan : ConstructRawPlan) : Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier,
        lowerConstructExprBytes plan with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

def lowerConstructCodeEntry
    (carrier : Nat)
    (plan : ConstructRawPlan) : Option (List Nat) :=
  match lowerConstructBodyBytes carrier plan with
  | some body =>
      match uleb32 body.length with
      | some lenBytes => some (lenBytes ++ body)
      | none => none
  | none => none

end AverCert.PlanBytes
