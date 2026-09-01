-- Lean-side canonical byte lowering from `expr-fragment-v1` raw plans to the
-- exact Wasm code-entry byte sequence used by the current cert island.
--
-- This is still not a full Wasm module parser. It is the plan-first byte
-- encoder for one checked profile: local declarations + expression body +
-- body-size prefix.
import PlanLower

namespace AverCert.PlanBytes
open AverCert.Schema

/-- Canonical unsigned LEB128 of a u32 index, or `none` outside the range.
    The bytes come from the shared total encoder (`CertPrelude.uleb32Bytes`,
    exact below `2 ^ 35`); the guard keeps this plan-side wrapper fail-closed
    at the u32 boundary the wasm binary format admits. -/
def uleb32 (value : Nat) : Option (List Nat) :=
  if value < 4294967296 then some (CertPrelude.uleb32Bytes value) else none

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
    indices (`struct.get`, `array.new_data`, …) stay unsigned u32. The bytes
    come from the shared total encoder (`CertPrelude.s33Bytes`, exact below
    `2 ^ 41`); the guard keeps this plan-side wrapper fail-closed at the u32
    boundary. -/
def s33HeapIdx (idx : Nat) : Option (List Nat) :=
  if idx < 4294967296 then some (CertPrelude.s33Bytes idx) else none

/-- Prefix a successfully lowered function body with its canonical u32 byte
    length, producing the exact byte sequence stored as one Wasm code entry. -/
private def codeEntryBytes : Option (List Nat) → Option (List Nat)
  | some body =>
      match uleb32 body.length with
      | some lengthBytes => some (lengthBytes ++ body)
      | none => none
  | none => none

/-- Encode the local declaration shared by plan families that reserve exactly
    one nullable carrier-reference local, then append the lowered expression. -/
private def singleCarrierLocalBodyBytes
    (carrier : Nat) (exprBytes? : Option (List Nat)) : Option (List Nat) :=
  match uleb32 1, uleb32 1, s33HeapIdx carrier, exprBytes? with
  | some localDeclCount, some localCount, some carrierBytes, some exprBytes =>
      some (localDeclCount ++ localCount ++ [0x63] ++ carrierBytes ++ exprBytes)
  | _, _, _, _ => none

/-- Encode an EMPTY local-declaration vector, then append the lowered
    expression. A module that emits no Int carrier struct has no carrier type to
    name in a locals prelude, and the emitter reserves no scratch slot there, so
    the code entry opens with a zero declaration-group count and goes straight
    into the expression. -/
private def noLocalBodyBytes (exprBytes? : Option (List Nat)) : Option (List Nat) :=
  match uleb32 0, exprBytes? with
  | some localDeclCount, some exprBytes => some (localDeclCount ++ exprBytes)
  | _, _ => none

/-- Locals prelude selected by the module's byte-derived CARRIER STATE.

    This is deliberately a function of `carrier?` and nothing else: a plan cannot
    reach the shorter prelude by describing itself differently, because the only
    input that selects it is the carrier state, and every acceptance predicate
    that calls this passes a `carrier?` pinned by
    `CertDecode.carrierState modBytes modLen`. `some` and `none` are therefore
    two states of the MODULE, decided by its type section, not two options open
    to the producer. -/
private def carrierLocalsBodyBytes
    (carrier? : Option Nat) (exprBytes? : Option (List Nat)) : Option (List Nat) :=
  match carrier? with
  | some carrier => singleCarrierLocalBodyBytes carrier exprBytes?
  | none => noLocalBodyBytes exprBytes?

def fieldProjectionResultTyBytes : FieldProjectionResultTy → Option (List Nat)
  | .eqref => some [0x6d]
  | .nullableRef idx => (s33HeapIdx idx).map (fun b => [0x63] ++ b)

def lowerFieldProjectionCodeEntry
    (carrier structIdx fieldCount : Nat)
    (resultTy : FieldProjectionResultTy)
    (plan : FieldProjectionRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan then
    match fieldProjectionResultTyBytes resultTy,
          s33HeapIdx carrier, s33HeapIdx structIdx,
          uleb32 structIdx, uleb32 plan.fieldIdx with
    | some resultTyB, some carrierB, some castTy, some getTy, some fieldB =>
        let body := [0x03, 0x01] ++ resultTyB ++
          [0x01, 0x6d, 0x01, 0x63] ++ carrierB ++
          [0x20, 0x00, 0x21, 0x02, 0x20, 0x02, 0xfb, 0x16] ++ castTy ++
          [0xfb, 0x02] ++ getTy ++ fieldB ++
          [0x21, 0x01, 0x20, 0x01, 0x0b]
        codeEntryBytes (some body)
    | _, _, _, _, _ => none
  else none

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
  | .f64Ge => [0x66]
  | .f64Lt => [0x63]
  | .f64Gt => [0x64]
  | .f64Eq => [0x61]
  | .i64Eq => [0x51]
  | .i64LtS => [0x53]
  | .i64LeS => [0x57]
  | .i64GeS => [0x59]
  | .i64GtS => [0x55]
  | .i32Eq => [0x46]
  | .i32LtS => [0x48]
  | .i32GtS => [0x4a]
  | .i32GeS => [0x4e]
  | .i32And => [0x71]

/-- Byte twin of `PlanLower.intSignCmpBigArm`. -/
def intSignCmpBigArmBytes (carrier scratch : Nat) : SymIntCmp → Option (List Nat)
  | .eq => some [0x41, 0x00]
  | op =>
      match uleb32 scratch, uleb32 0x02, uleb32 carrier, uleb32 2 with
      | some scratchB, some getOp, some carrierB, some fieldB =>
          some ([0x20] ++ scratchB ++ [0xfb] ++ getOp ++ carrierB ++ fieldB ++
            [0x41, 0x00] ++
            primBytes (match op with
              | .lt | .le => FragPrim.i32LtS
              | _ => FragPrim.i32GtS))
      | _, _, _, _ => none

/-- Byte twin of `PlanLower.intSignCmpTemplate`: `local.set` (`0x21`), the
    `limbs = null` test, and an `i32` block-typed `if` whose arms are the byte
    twins of the two `WInstr` arms. -/
def intSignCmpTemplateBytes (carrier scratch : Nat) (op : SymIntCmp) (k : Int) :
    Option (List Nat) :=
  match uleb32 scratch, uleb32 0x02, uleb32 carrier, uleb32 1, uleb32 0,
        sleb64 k, intSignCmpBigArmBytes carrier scratch op with
  | some scratchB, some getOp, some carrierB, some limbsField, some smallField,
      some kB, some bigBytes =>
      some ([0x21] ++ scratchB ++
        [0x20] ++ scratchB ++ [0xfb] ++ getOp ++ carrierB ++ limbsField ++
        [0xd1] ++
        [0x04, 0x7f] ++
        ([0x20] ++ scratchB ++ [0xfb] ++ getOp ++ carrierB ++ smallField ++
          [0x42] ++ kB ++
          primBytes (AverCert.PlanLower.intSignCmpSmallPrim op)) ++
        [0x05] ++ bigBytes ++ [0x0b])
  | _, _, _, _, _, _, _ => none

/-- Byte and semantic lowering share one symbolic-stack discipline and one
    fail-closed recursion budget.  The aliases retain the public API while
    making divergence between the two lowerers impossible here. -/
abbrev popExpected := AverCert.PlanLower.popExpected
abbrev popExpectedAll := AverCert.PlanLower.popExpectedAll
abbrev maxFuel : Nat := AverCert.PlanLower.maxFuel

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
          | .structNew tyIdx args =>
              match popExpectedAll stack args.reverse, uleb32 0x00, uleb32 tyIdx with
              | some stack', some opBytes, some tyBytes =>
                  some ([0xfb] ++ opBytes ++ tyBytes, node.id :: stack')
              | _, _, _ => none
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
          -- Byte twin of `PlanLower`'s arm: operands already on the symbolic
          -- stack stay beneath the emitted `if` block.
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some stack' =>
                  match blockTypeBytes carrier node.ty,
                        lowerBlockBytesFuel fuel carrier thenBlock,
                        lowerBlockBytesFuel fuel carrier elseBlock with
                  | some blockTy, some thenBytes, some elseBytes =>
                      some ([0x04] ++ blockTy ++ thenBytes ++ [0x05] ++ elseBytes ++ [0x0b],
                        node.id :: stack')
                  | _, _, _ => none
              | none => none
          | .vectorGetOrDefault arrTy toIndexIdx boxIdx default =>
              -- Byte twin of `PlanLower.vectorGetOrDefaultTemplate`, including
              -- the `(ref null carrier)` if block type the emitter declares.
              match stack with
              | [] =>
                  match uleb32 toIndexIdx, uleb32 boxIdx, uleb32 arrTy,
                        s33HeapIdx carrier, sleb64 default with
                  | some toIndexB, some boxB, some arrB, some carrierB, some dB =>
                      some (
                        [0x20, 0x01, 0x10] ++ toIndexB ++ [0x41, 0x00, 0x4e] ++
                        [0x20, 0x01, 0x10] ++ toIndexB ++
                        [0x20, 0x00, 0xfb, 0x0f, 0x49, 0x71] ++
                        [0x04, 0x63] ++ carrierB ++
                        [0x20, 0x00, 0x20, 0x01, 0x10] ++ toIndexB ++
                        [0xfb, 0x0b] ++ arrB ++
                        [0x05, 0x42] ++ dB ++ [0x10] ++ boxB ++ [0x0b],
                        [node.id])
                  | _, _, _, _, _ => none
              | _ => none
          | .intSignCmp op k scratch value =>
              match popExpected stack value,
                    intSignCmpTemplateBytes carrier scratch op k with
              | some stack', some bytes => some (bytes, node.id :: stack')
              | _, _ => none
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
  singleCarrierLocalBodyBytes carrier (lowerExprFragmentExprBytes carrier plan)

def lowerExprFragmentCodeEntry (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List Nat) :=
  codeEntryBytes (lowerExprFragmentBodyBytes carrier plan)

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
  singleCarrierLocalBodyBytes carrier (lowerRecursionExprBytes carrier plan)

def lowerRecursionCodeEntry (carrier : Nat) (plan : RecursionRawPlan) :
    Option (List Nat) :=
  codeEntryBytes (lowerRecursionBodyBytes carrier plan)

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
  singleCarrierLocalBodyBytes carrier (lowerMutualExprBytes carrier plan)

def lowerMutualCodeEntry (carrier : Nat) (plan : MutualRawPlan) :
    Option (List Nat) :=
  codeEntryBytes (lowerMutualBodyBytes carrier plan)

/-! ### Verbatim `ref.test`-dispatch byte lowering (exact code-entry bytes).

`ref.test`/`ref.cast`/`ref.null`/block-type heap indices are s33 SIGNED;
`struct.get`/`array.new_data` type/field/data indices are uleb32. -/

def lowerLeafBytes (S F : Nat) (resultSig : VerbatimResultSig) :
    VerbatimLeaf → Option (List Nat)
  | .project tyIdx field =>
      match uleb32 S, s33HeapIdx tyIdx, uleb32 tyIdx, uleb32 field, uleb32 F with
      | some sB, some castTy, some getTy, some fieldB, some fB =>
          some ([0x20] ++ sB ++ [0xfb, 0x16] ++ castTy ++
                [0xfb, 0x02] ++ getTy ++ fieldB ++ [0x21] ++ fB ++ [0x20] ++ fB)
      | _, _, _, _, _ => none
  | .arrayNewData arrTy dataIdx bytes =>
      match resultSig with
      | .refNull _ =>
          match sleb32 0, sleb32 (Int.ofNat bytes.length), uleb32 arrTy, uleb32 dataIdx with
          | some off, some len, some arrTyB, some dataIdxB =>
              some ([0x41] ++ off ++ [0x41] ++ len ++ [0xfb, 0x09] ++ arrTyB ++ dataIdxB)
          | _, _, _, _ => none
      | .f64Scalar => none
  | .refNull =>
      match resultSig with
      | .refNull heapTy =>
          match s33HeapIdx heapTy with
          | some ht => some ([0xd0] ++ ht)
          | none => none
      | .f64Scalar => none
  | .f64Bits bits =>
      match resultSig with
      | .f64Scalar =>
          match f64Bytes bits with
          | some fb => some ([0x44] ++ fb)
          | none => none
      | .refNull _ => none

def lowerDispatchBytes (S F : Nat) (resultSig : VerbatimResultSig) (first : Bool) :
    VerbatimDispatch → Option (List Nat)
  | .leaf l => lowerLeafBytes S F resultSig l
  | .test tyIdx hit rest =>
      match (if first then some ([] : List Nat)
             else (uleb32 S).map (fun b => [0x20] ++ b)),
            s33HeapIdx tyIdx,
            (match resultSig with
             | .refNull heapTy => (s33HeapIdx heapTy).map (fun b => [0x63] ++ b)
             | .f64Scalar => some [0x7c]),
            lowerLeafBytes S F resultSig hit,
            lowerDispatchBytes S F resultSig false rest with
      | some reload, some testTy, some blockTy, some hitBytes, some restBytes =>
          some (reload ++ [0xfb, 0x14] ++ testTy ++ [0x04] ++ blockTy ++
                hitBytes ++ [0x05] ++ restBytes ++ [0x0b])
      | _, _, _, _, _ => none

def lowerVerbatimExprBytes (plan : VerbatimRawPlan) : Option (List Nat) :=
  match uleb32 plan.scrutineeLocal,
        lowerDispatchBytes plan.scrutineeLocal plan.fieldLocal plan.resultSig true plan.body with
  | some sB, some dispatchBytes =>
      some ([0x20, 0x00] ++ [0x21] ++ sB ++ [0x20] ++ sB ++ dispatchBytes ++ [0x0b])
  | _, _ => none

/-- Local declarations. A projecting (widened-match) body declares the field
    scratch local (of the declared ref or f64 result type) first, then the eqref scrutinee, then
    the always-present unused Int-carrier scratch; a non-projecting (variant
    dispatch) body declares only the scrutinee and the carrier scratch. -/
def lowerVerbatimLocalsBytes (carrier : Nat) (resultSig : VerbatimResultSig) (hasProj : Bool) :
    Option (List Nat) :=
  match s33HeapIdx carrier with
  | some carrierB =>
      if hasProj then
        match resultSig with
        | .refNull heapTy =>
            match s33HeapIdx heapTy with
            | some rhtB =>
                some ([0x03] ++ [0x01, 0x63] ++ rhtB ++ [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
            | none => none
        | .f64Scalar =>
            some ([0x03] ++ [0x01, 0x7c] ++ [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
      else
        some ([0x02] ++ [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
  | none => none

def lowerVerbatimBodyBytes (carrier : Nat) (plan : VerbatimRawPlan) : Option (List Nat) :=
  match lowerVerbatimLocalsBytes carrier plan.resultSig
          (AverCert.PlanCheck.dispatchHasProjection plan.body),
        lowerVerbatimExprBytes plan with
  | some localsBytes, some exprBytes => some (localsBytes ++ exprBytes)
  | _, _ => none

def lowerVerbatimCodeEntry (carrier : Nat) (plan : VerbatimRawPlan) : Option (List Nat) :=
  codeEntryBytes (lowerVerbatimBodyBytes carrier plan)

/-! ### Int-face `ref.test`-dispatch byte lowering (exact code-entry bytes).

`ref.test`/`ref.cast`/block-type heap indices are s33 SIGNED; `struct.get` type
and field indices are uleb32; arm/default constants are sleb64. The scratch
locals mirror `PlanLower`: arm `i` spills to local `i+1`, the scrutinee is local
`armCount + 1`, and one trailing unused carrier scratch local is always
declared. The box/add/sub call target indices come from the byte-derived
host-role table PARAMETER — a role the table lacks fail-closes the lowering, so
the plan cannot name a function index. -/

/-- The shared arm prefix: project the tested variant's field 0 out of the
    spilled scrutinee and spill it to this arm's scratch local
    (`local.get S; ref.cast t; struct.get t 0; local.set F`). -/
def intDispatchProjBytes (S F tyIdx : Nat) : Option (List Nat) :=
  match uleb32 S, s33HeapIdx tyIdx, uleb32 tyIdx, uleb32 F with
  | some sB, some castTy, some getTy, some fB =>
      some ([0x20] ++ sB ++ [0xfb, 0x16] ++ castTy ++
            [0xfb, 0x02] ++ getTy ++ [0x00] ++ [0x21] ++ fB)
  | _, _, _, _ => none

def lowerIntDispatchArmBytes
    (hostTable : List (HostRole × Nat)) (S F tyIdx : Nat) :
    IntDispatchLeaf → Option (List Nat)
  | .const _ => none
  | .proj =>
      match intDispatchProjBytes S F tyIdx, uleb32 F with
      | some proj, some fB => some (proj ++ [0x20] ++ fB)
      | _, _ => none
  | .hostOp role k constFirst =>
      match intDispatchProjBytes S F tyIdx, uleb32 F, sleb64 k,
            (AverCert.PlanCheck.hostRoleIdx? hostTable .box).bind uleb32,
            (AverCert.PlanCheck.hostRoleIdx? hostTable
              (AverCert.PlanCheck.intDispatchRoleHostRole role)).bind uleb32 with
      | some proj, some fB, some kB, some boxB, some hostB =>
          some (proj ++
            (if constFirst then
              [0x42] ++ kB ++ [0x10] ++ boxB ++ [0x20] ++ fB ++ [0x10] ++ hostB
            else
              [0x20] ++ fB ++ [0x42] ++ kB ++ [0x10] ++ boxB ++ [0x10] ++ hostB))
      | _, _, _, _, _ => none

def intDispatchDefaultBytes
    (hostTable : List (HostRole × Nat)) (k : Int) : Option (List Nat) :=
  match sleb64 k, (AverCert.PlanCheck.hostRoleIdx? hostTable .box).bind uleb32 with
  | some kB, some boxB => some ([0x42] ++ kB ++ [0x10] ++ boxB)
  | _, _ => none

def lowerIntDispatchCascadeBytes
    (hostTable : List (HostRole × Nat)) (carrier S : Nat) :
    Nat → Bool → IntDispatchCascade → Option (List Nat)
  | _pos, _first, .default k => intDispatchDefaultBytes hostTable k
  | pos, first, .test tyIdx (.const k) rest =>
      -- A const (nullary) arm: the hit block is the boxed constant with NO
      -- projection prefix (byte-identical to a default), and the binding
      -- position `pos` does not advance (no per-arm payload spill).
      match (if first then some ([] : List Nat)
             else (uleb32 S).map (fun b => [0x20] ++ b)),
            s33HeapIdx tyIdx, s33HeapIdx carrier,
            intDispatchDefaultBytes hostTable k,
            lowerIntDispatchCascadeBytes hostTable carrier S pos false rest with
      | some reload, some testTy, some blockTy, some hitBytes, some restBytes =>
          some (reload ++ [0xfb, 0x14] ++ testTy ++ [0x04, 0x63] ++ blockTy ++
                hitBytes ++ [0x05] ++ restBytes ++ [0x0b])
      | _, _, _, _, _ => none
  | pos, first, .test tyIdx hit rest =>
      match (if first then some ([] : List Nat)
             else (uleb32 S).map (fun b => [0x20] ++ b)),
            s33HeapIdx tyIdx, s33HeapIdx carrier,
            lowerIntDispatchArmBytes hostTable S (pos + 1) tyIdx hit,
            lowerIntDispatchCascadeBytes hostTable carrier S (pos + 1) false rest with
      | some reload, some testTy, some blockTy, some hitBytes, some restBytes =>
          some (reload ++ [0xfb, 0x14] ++ testTy ++ [0x04, 0x63] ++ blockTy ++
                hitBytes ++ [0x05] ++ restBytes ++ [0x0b])
      | _, _, _, _, _ => none

def lowerIntDispatchExprBytes
    (hostTable : List (HostRole × Nat)) (carrier S : Nat)
    (body : IntDispatchCascade) : Option (List Nat) :=
  match uleb32 S, lowerIntDispatchCascadeBytes hostTable carrier S 0 true body with
  | some sB, some cascadeBytes =>
      some ([0x20, 0x00] ++ [0x21] ++ sB ++ [0x20] ++ sB ++ cascadeBytes ++ [0x0b])
  | _, _ => none

/-- Local declarations: `bindArmCount` single-local carrier-ref groups (the
    per-binding-arm payload spills), one eqref group (the scrutinee), and one
    trailing unused carrier scratch group — `bindArmCount + 2` groups in all. A
    const (nullary) arm declares no spill group. -/
def intDispatchArmLocalGroups (carrierB : List Nat) : Nat → List Nat
  | 0 => []
  | n + 1 => [0x01, 0x63] ++ carrierB ++ intDispatchArmLocalGroups carrierB n

def lowerIntDispatchLocalsBytes (carrier armCount : Nat) : Option (List Nat) :=
  match uleb32 (armCount + 2), s33HeapIdx carrier with
  | some countB, some carrierB =>
      some (countB ++ intDispatchArmLocalGroups carrierB armCount ++
            [0x01, 0x6d] ++ [0x01, 0x63] ++ carrierB)
  | _, _ => none

def lowerIntDispatchBodyBytes
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (plan : IntDispatchRawPlan) : Option (List Nat) :=
  let armCount := AverCert.PlanCheck.bindArmCount plan.body
  match lowerIntDispatchLocalsBytes carrier armCount,
        lowerIntDispatchExprBytes hostTable carrier (armCount + 1) plan.body with
  | some localsBytes, some exprBytes => some (localsBytes ++ exprBytes)
  | _, _ => none

def lowerIntDispatchCodeEntry
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (plan : IntDispatchRawPlan) : Option (List Nat) :=
  codeEntryBytes (lowerIntDispatchBodyBytes carrier hostTable plan)

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

/-- The `string-concat-v1` code entry, in both carrier states of the module.
    The expression is identical either way — concatenation never touches the
    carrier — and only the locals prelude differs, so the carrier state is an
    environmental fact about the module rather than anything the concatenation
    means. `carrier?` is the pinned `CertDecode.carrierState` of the same bytes;
    see `carrierLocalsBodyBytes`. -/
def lowerStringConcatBodyBytes
    (carrier? : Option Nat) (resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List Nat) :=
  carrierLocalsBodyBytes carrier?
    (lowerStringConcatExprBytes resultTy containerTy concatFuncIdx plan)

def lowerStringConcatCodeEntry
    (carrier? : Option Nat) (resultTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan) : Option (List Nat) :=
  codeEntryBytes
    (lowerStringConcatBodyBytes carrier? resultTy containerTy concatFuncIdx plan)

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
  codeEntryBytes (lowerStringEqBodyBytes carrier stringTy stringEqFuncIdx plan)

def lowerConstructFieldBytes (structIdx : Nat) : ConstructField → Option (List Nat)
  | .local index =>
      match uleb32 index with
      | some indexBytes => some ([0x20] ++ indexBytes)
      | none => none
  | .null =>
      match s33HeapIdx structIdx with
      | some idxBytes => some ([0xd0] ++ idxBytes)
      | none => none

def lowerConstructFieldsBytes (structIdx : Nat) : List ConstructField → Option (List Nat)
  | [] => some []
  | field :: rest =>
      match lowerConstructFieldBytes structIdx field, lowerConstructFieldsBytes structIdx rest with
      | some fieldBytes, some restBytes => some (fieldBytes ++ restBytes)
      | _, _ => none

def lowerConstructExprBytes (structIdx : Nat) (plan : ConstructRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkConstructRawPlan plan then
    match lowerConstructFieldsBytes structIdx plan.fields,
          uleb32 0x00,
          uleb32 structIdx with
    | some fieldBytes, some structNewOpBytes, some structIdxBytes =>
        some (fieldBytes ++ [0xfb] ++ structNewOpBytes ++ structIdxBytes ++ [0x0b])
    | _, _, _ => none
  else
    none

def lowerConstructBodyBytes
    (carrier : Nat)
    (structIdx : Nat)
    (plan : ConstructRawPlan) : Option (List Nat) :=
  singleCarrierLocalBodyBytes carrier (lowerConstructExprBytes structIdx plan)

def lowerConstructCodeEntry
    (carrier : Nat)
    (structIdx : Nat)
    (plan : ConstructRawPlan) : Option (List Nat) :=
  codeEntryBytes (lowerConstructBodyBytes carrier structIdx plan)

/-! ### `composition-plan-v1` exact byte lowering -/

def compositionFuncIdx? (funcTable : List (String × Nat)) (name : String) : Option Nat :=
  match funcTable.find? (fun entry => entry.1 == name) with
  | some entry => some entry.2
  | none => none

def lowerCompositionCallBytes
    (funcTable : List (String × Nat)) : List String → Option (List Nat)
  | [] => some []
  | callee :: rest =>
      match (compositionFuncIdx? funcTable callee).bind uleb32,
            lowerCompositionCallBytes funcTable rest with
      | some idx, some tail => some ([0x10] ++ idx ++ tail)
      | _, _ => none

def lowerCompositionExprBytes
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (plan : CompositionRawPlan) : Option (List Nat) :=
  if AverCert.PlanCheck.checkCompositionRawPlan plan then
    match uleb32 0 with
    | some zero =>
        match plan.shape with
        | .selfSum =>
            match (AverCert.PlanCheck.hostRoleIdx? hostTable .add).bind uleb32 with
            | some addIdx =>
                some ([0x20] ++ zero ++ [0x20] ++ zero ++ [0x10] ++ addIdx ++ [0x0b])
            | none => none
        | .chain callees =>
            match lowerCompositionCallBytes funcTable callees with
            | some calls => some ([0x20] ++ zero ++ calls ++ [0x0b])
            | none => none
    | none => none
  else
    none

def lowerCompositionBodyBytes
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (plan : CompositionRawPlan) : Option (List Nat) :=
  singleCarrierLocalBodyBytes carrier
    (lowerCompositionExprBytes hostTable funcTable plan)

def lowerCompositionCodeEntry
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (plan : CompositionRawPlan) : Option (List Nat) :=
  codeEntryBytes (lowerCompositionBodyBytes carrier hostTable funcTable plan)

end AverCert.PlanBytes
