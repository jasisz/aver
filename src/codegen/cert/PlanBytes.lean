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

def blockTypeBytes : FragTy → Option (List Nat)
  | .boolI32 => some [0x7f]
  | .rawI32 => some [0x7f]
  | .i64 => some [0x7e]
  | .f64 => some [0x7c]
  | .intCarrier => none
  | .ref => none

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
          | .refIsNull value =>
              match popExpected stack value with
              | some stack' => some ([0xd1], node.id :: stack')
              | none => none
          | .prim op args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some (primBytes op, node.id :: stack')
              | none => none
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some [] =>
                  match blockTypeBytes node.ty,
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
  match uleb32 1, uleb32 1, uleb32 carrier,
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
  match uleb32 1, uleb32 1, uleb32 carrier,
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

end AverCert.PlanBytes
