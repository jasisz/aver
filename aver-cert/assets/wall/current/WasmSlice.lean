-- Lean-side exact Wasm policy checks for certificate artifacts.
--
-- This is intentionally not a full WebAssembly validator. `CertDecode` owns
-- the shared strict section/type grammar; this layer projects that typed IR and
-- exact raw code/data slices into the shapes admitted by certified plans. Its
-- closure scanner additionally rejects every instruction channel outside the
-- certificate profile.
import SchemaCore
import CertDecode
import Std.Data.TreeSet

namespace AverCert.WasmSlice

abbrev ByteSeq := List Nat

structure FuncBinding where
  funcIdx : Nat
  typeIdx : Nat
  codeEntry : ByteSeq

/-! ### Whole-module byte cursor

The Wasm module is one little-endian `Nat` plus its explicit byte length.  This
is the same byte model and the same shift/mask discipline as
`tools/certkit/prelude/CertDecode.lean`: reading uses `&&& 0xff`, advancing uses
`>>> 8`, and skipping a framed payload uses one `>>> (8 * size)`.  The length is
soundness-relevant because high trailing `0x00` bytes are not represented by the
numeral.

Only the closure scanner uses the local list cursor. Export/import names, data
payloads, and code entries cross to `ByteSeq`; selected types stay in the typed
`CertDecode.TypeEntry` IR. -/

@[inline] def readNatUleb32 := CertDecode.readU

def takeN : Nat → ByteSeq → Option (ByteSeq × ByteSeq)
  | 0, xs => some ([], xs)
  | n + 1, x :: xs =>
      match takeN n xs with
      | some (taken, rest) => some (x :: taken, rest)
      | none => none
  | _ + 1, [] => none

def readUlebFuel : Nat → Nat → Nat → ByteSeq → Option (Nat × ByteSeq)
  | 0, _, _, _ => none
  | _fuel + 1, _shift, _acc, [] => none
  | fuel + 1, shift, acc, b :: rest =>
      if b < 256 then
        let low := b % 128
        let acc' := acc + low * (2 ^ shift)
        if b < 128 then
          some (acc', rest)
        else
          readUlebFuel fuel (shift + 7) acc' rest
      else
        none

def readUleb32 (bytes : ByteSeq) : Option (Nat × ByteSeq) :=
  readUlebFuel 5 0 0 bytes

/-- Signed LEB128 reader (s33 heap-type indices in value types). -/
def readSlebFuel : Nat → Nat → Int → ByteSeq → Option (Int × ByteSeq)
  | 0, _, _, _ => none
  | _fuel + 1, _shift, _acc, [] => none
  | fuel + 1, shift, acc, b :: rest =>
      if b < 256 then
        let low := b % 128
        let acc' := acc + Int.ofNat (low * (2 ^ shift))
        if b < 128 then
          let signed := if 64 ≤ low then acc' - Int.ofNat (2 ^ (shift + 7)) else acc'
          some (signed, rest)
        else
          readSlebFuel fuel (shift + 7) acc' rest
      else
        none

/-- Read a spec-bounded s33: at most five bytes and in the signed 33-bit range
    `-(2^32) ≤ v < 2^32`; the range check closes the final byte's unused bits. -/
def readS33 (bytes : ByteSeq) : Option (Int × ByteSeq) :=
  match readSlebFuel 5 0 0 bytes with
  | some (v, rest) =>
      if -(Int.ofNat (2 ^ 32)) ≤ v ∧ v < Int.ofNat (2 ^ 32) then
        some (v, rest)
      else
        none
  | none => none

/-! ### Type-section navigation

Just enough of the wasm-gc type grammar to bind a function-section type index
to the canonical certified signature `[(ref null C)^n] → [(ref null C)]`. Like
the rest of this slicer it is deliberately not a validator: every unrecognised
byte fail-closes to `none`/`false`. -/

/-- Skip one value type: numeric types (`0x7b`–`0x7f`) and abstract heap-type
    shorthands (`0x69`–`0x73`) are single bytes; a concrete reference type is
    `0x63`/`0x64` followed by an s33 heap-type index. -/
def skipValType : ByteSeq → Option ByteSeq
  | b :: rest =>
      if b = 0x63 ∨ b = 0x64 then
        match readS33 rest with
        | some (_, rest') => some rest'
        | none => none
      else if (0x7b ≤ b ∧ b ≤ 0x7f) ∨ (0x69 ≤ b ∧ b ≤ 0x73) then
        some rest
      else
        none
  | [] => none

def skipValTypesFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, 0, bytes => some bytes
  | fuel + 1, n + 1, bytes =>
      match skipValType bytes with
      | some rest => skipValTypesFuel fuel n rest
      | none => none

def skipUlebsFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, 0, bytes => some bytes
  | fuel + 1, n + 1, bytes =>
      match readUleb32 bytes with
      | some (_, rest) => skipUlebsFuel fuel n rest
      | none => none

/-- The exact nullable concrete reference admitted by certified signatures. -/
def nullableRefType (typeIdx : Nat) : CertDecode.ValType :=
  .ref 0x63 (Int.ofNat typeIdx)

def checkCanonicalFuncType (arity carrier : Nat)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .funcType params results =>
      decide (params = List.replicate arity (nullableRefType carrier)) &&
        decide (results = [nullableRefType carrier])
  | _, _ => false

/-- Whether the module's type section is well-formed AND its entry `typeIdx`
    satisfies `check`. The whole rectype vector is parsed and required to consume
    the section payload EXACTLY (no trailing bytes past the last rectype), so a
    valid target entry followed by garbage is rejected, not silently accepted. -/
def typeSectionMatches (check : CertDecode.TypeEntry → Bool)
    (modBytes modLen typeIdx : Nat) : Bool :=
  match CertDecode.decodeTypes modBytes modLen with
  | some info =>
      match info.entryIndex[typeIdx]? with
      | some entry => check entry
      | none => false
  | none => false

/-- Whether the module's type-section entry `typeIdx` is exactly the canonical
    certified function type `[(ref null carrier)^arity] → [(ref null carrier)]`.
    This binds a claimed function binding's declared signature to the plan's
    params/result without trusting either, and (via `typeSectionMatches`) requires
    the whole type section to be well-formed and exactly consumed. -/
def funcTypeMatches (modBytes modLen typeIdx arity carrier : Nat) : Bool :=
  typeSectionMatches (checkCanonicalFuncType arity carrier) modBytes modLen typeIdx

def fragValTypeMatches (carrier : Nat) :
    AverCert.Schema.FragTy → CertDecode.ValType → Bool
  | .f64, .numeric 0x7c => true
  | .boolI32, .numeric 0x7f => true
  | .i64, .numeric 0x7e => true
  | .rawI32, .numeric 0x7f => true
  | .intCarrier, actual => actual == nullableRefType carrier
  | .ref, .ref 0x63 heap => decide (0 ≤ heap)
  | .adtRef, .ref 0x63 heap =>
      decide (0 ≤ heap) && decide (heap ≠ Int.ofNat carrier)
  | _, _ => false

def fragParamsMatch (carrier : Nat) :
    List AverCert.Schema.FragTy → List CertDecode.ValType → Bool
  | [], [] => true
  | expected :: expectedRest, actual :: actualRest =>
      fragValTypeMatches carrier expected actual &&
        fragParamsMatch carrier expectedRest actualRest
  | _, _ => false

/-- Exact scalar/reference shape of an expression-fragment function. Unlike a
    code entry, the type-section binding distinguishes e.g. `f64 → i32` from
    `i32 → f64`; reference-shaped ADT projections are tightened nominally by
    `exprFragmentNominalTypesMatch` below. -/
def checkExprFragmentFuncType
    (carrier : Nat)
    (params : List AverCert.Schema.FragTy)
    (result : AverCert.Schema.FragTy)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .funcType actualParams [actualResult] =>
      fragParamsMatch carrier params actualParams &&
        fragValTypeMatches carrier result actualResult
  | _, _ => false

def exprFragmentFuncTypeMatches
    (modBytes modLen typeIdx carrier : Nat)
    (params : List AverCert.Schema.FragTy)
    (result : AverCert.Schema.FragTy) : Bool :=
  typeSectionMatches (checkExprFragmentFuncType carrier params result)
    modBytes modLen typeIdx

/-- One of the two admitted opaque-ADT fragment shapes is a direct field
    projection (the other, tag dispatch, is checked below). Its function
    parameter must name that exact struct, and its result must equal the
    selected field's decoded storage type. This simultaneously proves that
    the struct and field exist in the artifact's type section. -/
def checkExprProjectionTypes
    (carrier structIdx fieldIdx : Nat)
    (funcEntry structEntry : CertDecode.TypeEntry) : Bool :=
  if structIdx == carrier then false else
    match funcEntry.form, funcEntry.composite,
        structEntry.form, structEntry.composite with
    | .plain, .funcType [param] [result], .plain, .structType fields =>
        fields.length == 2 && decide (param = nullableRefType structIdx) &&
          match fields[fieldIdx]? with
          | some { storage := .val fieldType, .. } => decide (result = fieldType)
          | _ => false
    | _, _, _, _ => false

def exprProjectionTypesMatch
    (modBytes modLen typeIdx carrier structIdx fieldIdx : Nat) : Bool :=
  match CertDecode.decodeTypes modBytes modLen with
  | some info =>
      match info.entryIndex[typeIdx]?, info.entryIndex[structIdx]? with
      | some funcEntry, some structEntry =>
          checkExprProjectionTypes carrier structIdx fieldIdx funcEntry structEntry
      | _, _ => false
  | none => false

def exprProjectionFace? (plan : AverCert.Schema.ExprFragmentRawPlan) :
    Option (Nat × Nat) :=
  if plan.params = [.adtRef] && plan.result = .adtRef &&
      plan.body.result = 1 then
    match plan.body.nodes with
    | [n0, n1] =>
        match n0.kind, n1.kind with
        | .local 0, .structGetUser structIdx fieldIdx 0 =>
            if fieldIdx ≤ 1 && n0.ty = .adtRef && n1.ty = .adtRef then
              some (structIdx, fieldIdx)
            else none
        | _, _ => none
    | _ => none
  else none

/-- Byte-level recognizer of the tag-dispatch shape (mirror of the representation
    plan; lives here because WasmSlice cannot import StandardFace). Returns the
    scrutinee struct index. -/
def exprTagDispatchStructIdx? (plan : AverCert.Schema.ExprFragmentRawPlan) : Option Nat :=
  if plan.params = [.adtRef] && plan.result = .intCarrier && plan.body.result = 4 then
    match plan.body.nodes with
    | [n0, n1, n2, n3, n4] =>
        match n0.kind, n1.kind, n2.kind, n3.kind, n4.kind with
        | .local 0, .structGetUser structIdx 0 0, .constI32 _, .prim .i32Eq [1, 2], .ifElse 3 _ _ =>
            if n0.ty = .adtRef && n1.ty = .rawI32 && n2.ty = .rawI32 &&
                n3.ty = .boolI32 && n4.ty = .intCarrier
            then some structIdx else none
        | _, _, _, _, _ => none
    | _ => none
  else none

/-- The scrutinee struct's field 0 must be the i32 operational tag. Guard
    structIdx≠carrier (the Int carrier is never the scrutinee). -/
def checkTagDispatchTypes (carrier structIdx : Nat) (structEntry : CertDecode.TypeEntry) : Bool :=
  if structIdx == carrier then false else
  match structEntry.form, structEntry.composite with
  | .plain, .structType fields =>
      match (fields[0]? : Option CertDecode.FieldType) with
      | some { storage := .val (.numeric 0x7f), .. } => true
      | _ => false
  | _, _ => false

def exprTagDispatchTypesMatch (modBytes modLen carrier structIdx : Nat) : Bool :=
  match CertDecode.decodeTypes modBytes modLen with
  | some info =>
      match info.entryIndex[structIdx]? with
      | some structEntry => checkTagDispatchTypes carrier structIdx structEntry
      | none => false
  | none => false

/-- Byte-level recognizer of the monolithic fused vector-read shape (mirror of
    the representation plan). Returns the vector's array type index. -/
def exprVectorGetOrDefaultArrTy?
    (plan : AverCert.Schema.ExprFragmentRawPlan) : Option Nat :=
  if plan.params = [.adtRef, .intCarrier] && plan.result = .intCarrier &&
      plan.body.result = 0 then
    match plan.body.nodes with
    | [n0] =>
        match n0.kind with
        | .vectorGetOrDefault arrTy _toIndexIdx _boxIdx _default =>
            if n0.ty = .intCarrier then some arrTy else none
        | _ => none
    | _ => none
  else none

/-- The fused vector read is nominally bound on both ends: the function must
    take exactly the declared vector array plus one Int carrier and return the
    carrier, and the declared array type's element storage must be the nullable
    carrier reference — so the elements a `domRepr` state carries really are
    Int-carrier representations. The carrier is never the array itself. -/
def checkVectorGetTypes
    (carrier arrTy : Nat)
    (funcEntry arrEntry : CertDecode.TypeEntry) : Bool :=
  if arrTy == carrier then false else
    match funcEntry.form, funcEntry.composite,
        arrEntry.form, arrEntry.composite with
    | .plain, .funcType [vecParam, idxParam] [result], .plain, .arrayType field =>
        decide (vecParam = nullableRefType arrTy) &&
          decide (idxParam = nullableRefType carrier) &&
          decide (result = nullableRefType carrier) &&
          (match field.storage with
           | .val elemTy => decide (elemTy = nullableRefType carrier)
           | .packed _ => false)
    | _, _, _, _ => false

def exprVectorGetTypesMatch
    (modBytes modLen typeIdx carrier arrTy : Nat) : Bool :=
  match CertDecode.decodeTypes modBytes modLen with
  | some info =>
      match info.entryIndex[typeIdx]?, info.entryIndex[arrTy]? with
      | some funcEntry, some arrEntry =>
          checkVectorGetTypes carrier arrTy funcEntry arrEntry
      | _, _ => false
  | none => false

/-- Opaque references fail closed unless the plan has one of the three admitted
    faces: the field-projection face (whose nominal signature and field type are
    decoded above), the tag-dispatch face (whose i32 tag field is decoded
    below), or the fused vector-read face (whose array element type is decoded
    above). -/
def exprFragmentNominalTypesMatch
    (modBytes modLen typeIdx carrier : Nat)
    (plan : AverCert.Schema.ExprFragmentRawPlan) : Bool :=
  if plan.params.contains .adtRef || plan.result = .adtRef then
    match exprProjectionFace? plan with
    | some (structIdx, fieldIdx) =>
        exprProjectionTypesMatch
          modBytes modLen typeIdx carrier structIdx fieldIdx
    | none =>
        match exprTagDispatchStructIdx? plan with
        | some structIdx => exprTagDispatchTypesMatch modBytes modLen carrier structIdx
        | none =>
            match exprVectorGetOrDefaultArrTy? plan with
            | some arrTy => exprVectorGetTypesMatch modBytes modLen typeIdx carrier arrTy
            | none => false
  else true

def isNonnegativeNullableRef : CertDecode.ValType → Bool
  | .ref 0x63 heap => decide (0 ≤ heap)
  | _ => false

def verbatimResultTypeMatches
    (expected : AverCert.Schema.VerbatimResultSig) : CertDecode.ValType → Bool
  | .ref 0x63 heap =>
      match expected with
      | .refNull expectedHeap => heap == Int.ofNat expectedHeap
      | .f64Scalar => false
  | .numeric 0x7c => expected == .f64Scalar
  | _ => false

/-- Exact plain certified verbatim signature: one nullable concrete nominal
    root parameter and exactly one result selected by `resultSig`. -/
def checkVerbatimFuncType (resultSig : AverCert.Schema.VerbatimResultSig)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .funcType [root] [result] =>
      isNonnegativeNullableRef root && verbatimResultTypeMatches resultSig result
  | _, _ => false

/-- Whether the module's byte-derived type-section entry `typeIdx` exactly
    matches the verbatim plan's declared result-signature variant. -/
def verbatimFuncTypeMatches (modBytes modLen typeIdx : Nat)
    (resultSig : AverCert.Schema.VerbatimResultSig) : Bool :=
  typeSectionMatches (checkVerbatimFuncType resultSig) modBytes modLen typeIdx

/-! ### Bare field-projection type binding -/

def projectionResultTypeMatches
    (expected : AverCert.Schema.FieldProjectionResultTy) : CertDecode.ValType → Bool
  | .abstract 0x6d => expected == .eqref
  | .ref 0x63 heap =>
      match expected with
      | .nullableRef expectedIdx => heap == Int.ofNat expectedIdx
      | .eqref => false
  | _ => false

def hasValStorage : CertDecode.FieldType → Bool
  | ⟨.val _, _⟩ => true
  | _ => false

def projectionFieldMatches
    (expected : AverCert.Schema.FieldProjectionResultTy)
    (field : CertDecode.FieldType) : Bool :=
  match field.storage with
  | .val actual => projectionResultTypeMatches expected actual
  | .packed _ => false

def checkProjectionStructType
    (fieldCount fieldIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .structType fields =>
      fields.length == fieldCount && fields.all hasValStorage &&
        match fields[fieldIdx]? with
        | some field => projectionFieldMatches resultTy field
        | none => false
  | _, _ => false

def projectionStructTypeMatches
    (modBytes modLen structIdx fieldCount fieldIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches
    (checkProjectionStructType fieldCount fieldIdx resultTy)
    modBytes modLen structIdx

def checkProjectionFuncType
    (structIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .funcType [param] [result] =>
      decide (param = nullableRefType structIdx) &&
        projectionResultTypeMatches resultTy result
  | _, _ => false

def projectionFuncTypeMatches
    (modBytes modLen typeIdx structIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches (checkProjectionFuncType structIdx resultTy) modBytes modLen typeIdx

/-! ### List-constructor type binding -/

def constructValType
    (expected : AverCert.Schema.ConstructValType) : CertDecode.ValType :=
  match expected with
  | .i32 => .numeric 0x7f
  | .i64 => .numeric 0x7e
  | .f64 => .numeric 0x7c
  | .eqref => .abstract 0x6d
  | .nullableRef typeIdx => nullableRefType typeIdx

def immutableConstructFieldMatches
    (expected : AverCert.Schema.ConstructValType)
    (field : CertDecode.FieldType) : Bool :=
  field.mutability == 0 &&
    match field.storage with
    | .val actual => decide (actual = constructValType expected)
    | .packed _ => false

def checkListConstructStructType
    (structIdx : Nat) (elemTy : AverCert.Schema.ConstructValType)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .structType [head, tail] =>
      immutableConstructFieldMatches elemTy head &&
        immutableConstructFieldMatches (.nullableRef structIdx) tail
  | _, _ => false

def listConstructStructTypeMatches
    (modBytes modLen structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructStructType structIdx elemTy) modBytes modLen structIdx

def checkListConstructFuncType
    (arity structIdx : Nat) (elemTy : AverCert.Schema.ConstructValType)
    (entry : CertDecode.TypeEntry) : Bool :=
  match entry.form, entry.composite with
  | .plain, .funcType params results =>
      let head := constructValType elemTy
      let tail := nullableRefType structIdx
      let paramsMatch :=
        if arity = 1 then decide (params = [head])
        else if arity = 2 then decide (params = [head, tail])
        else false
      paramsMatch && decide (results = [tail])
  | _, _ => false

def listConstructFuncTypeMatches
    (modBytes modLen typeIdx arity structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructFuncType arity structIdx elemTy)
    modBytes modLen typeIdx

/-- Exact selected passive data payload.  The full declared vector must parse
    and exhaust the section payload. -/
def dataSegmentBytes (modBytes modLen dataIdx : Nat) : Option ByteSeq :=
  match CertDecode.decodeData modBytes modLen with
  | some segments => segments[dataIdx]?
  | none => none

def importedFuncCount (modBytes modLen : Nat) : Option Nat :=
  CertDecode.funcImportBase modBytes modLen

/-! ### Whole-module interface enumeration

The canonical decoders retain the names that the older binding slicer
intentionally discarded. They consume each section exactly and return `none`
on malformed or unsupported descriptors, so the artifact envelope fails closed. -/

abbrev ExportEntry := CertDecode.ExportEntry

def enumImportNames (modBytes modLen : Nat) :
    Option (List (ByteSeq × ByteSeq)) :=
  (CertDecode.decodeRawImports modBytes modLen).map
    (fun imports => imports.map (fun entry => (entry.moduleName, entry.fieldName)))

def enumExports (modBytes modLen : Nat) : Option (List ExportEntry) :=
  CertDecode.decodeRawExports modBytes modLen

/-- `some none` means no start section; `some (some idx)` is the exact function
    index encoded by section 8.  A present payload must contain one u32 and no
    trailing bytes. -/
def startFuncIndex (modBytes modLen : Nat) : Option (Option Nat) :=
  CertDecode.decodeStart modBytes modLen

def findExportFuncIndex (targetName : ByteSeq) : List ExportEntry → Option Nat
  | [] => none
  | entry :: rest =>
      if entry.kind = 0x00 ∧ entry.name = targetName then some entry.idx
      else findExportFuncIndex targetName rest

def exportFuncIndex (modBytes modLen : Nat) (targetName : ByteSeq) : Option Nat :=
  match CertDecode.decodeRawExports modBytes modLen with
  | some entries => findExportFuncIndex targetName entries
  | none => none

def codeEntryByCodeIndex (modBytes modLen codeIdx : Nat) : Option ByteSeq :=
  match CertDecode.codeLocs modBytes modLen with
  | some locations =>
      match locations[codeIdx]? with
      | some location => some (CertDecode.takeBytes location.entryLen location.entryN)
      | none => none
  | none => none

def typeIndexByCodeIndex (modBytes modLen codeIdx : Nat) : Option Nat :=
  match CertDecode.decodeFuncTypes modBytes modLen with
  | some typeIndices => typeIndices[codeIdx]?
  | none => none

def codeIndexByFuncIndex (modBytes modLen funcIdx : Nat) : Option Nat :=
  match importedFuncCount modBytes modLen with
  | some imported =>
      if imported ≤ funcIdx then
        some (funcIdx - imported)
      else
        none
  | none => none

def codeEntryByFuncIndex (modBytes modLen funcIdx : Nat) : Option ByteSeq :=
  match codeIndexByFuncIndex modBytes modLen funcIdx with
  | some codeIdx => codeEntryByCodeIndex modBytes modLen codeIdx
  | none => none

def funcBindingByFuncIndex (modBytes modLen funcIdx : Nat) : Option FuncBinding :=
  match codeIndexByFuncIndex modBytes modLen funcIdx with
  | some codeIdx =>
      match typeIndexByCodeIndex modBytes modLen codeIdx,
            codeEntryByCodeIndex modBytes modLen codeIdx with
      | some typeIdx, some codeEntry =>
          some { funcIdx := funcIdx, typeIdx := typeIdx,
                 codeEntry := codeEntry }
      | _, _ => none
  | none => none

def codeEntryForExport (modBytes modLen : Nat) (targetName : ByteSeq) : Option ByteSeq :=
  match exportFuncIndex modBytes modLen targetName with
  | some funcIdx => codeEntryByFuncIndex modBytes modLen funcIdx
  | none => none

def funcBindingForExport (modBytes modLen : Nat) (targetName : ByteSeq) : Option FuncBinding :=
  match exportFuncIndex modBytes modLen targetName with
  | some funcIdx => funcBindingByFuncIndex modBytes modLen funcIdx
  | none => none

/-- Bind an export only when its selected code entry exactly matches the expected bytes. -/
def exactFuncBindingForExport
    (modBytes modLen : Nat) (targetName expectedCode : ByteSeq) : Option FuncBinding :=
  (funcBindingForExport modBytes modLen targetName).filter
    (fun binding => binding.codeEntry = expectedCode)

/-! ### Certified direct-call closure and rejected-channel scan

The scanner deliberately recognizes only immediate layouts needed by the
validated wasm-gc certified profile.  Structured control is linear in the code
bytes (`block`/`loop`/`if`, branches, `else`, `end`), so instructions in every
nested body are visited.  Direct `call`/`return_call` targets are collected;
dynamic calls, globals, tables, linear memory, atomics, SIMD, segment drops and
every unknown prefix fail closed.  Generated witnesses disclose a raised
`maxRecDepth`; this is an elaborator reduction limit only and does not change
the proposition or admitted axioms. -/

def skipSignedLebBytes : Nat → ByteSeq → Option ByteSeq
  | 0, _ => none
  | _fuel + 1, [] => none
  | fuel + 1, b :: rest =>
      if b < 256 then
        if b < 128 then some rest else skipSignedLebBytes fuel rest
      else none

def skipBlockType : ByteSeq → Option ByteSeq
  | [] => none
  | b :: rest =>
      if b = 0x40 ∨ (0x7b ≤ b ∧ b ≤ 0x7f) then some rest
      else if b = 0x63 ∨ b = 0x64 then skipSignedLebBytes 5 rest
      else skipSignedLebBytes 5 (b :: rest)

def skipLocalGroups : Nat → ByteSeq → Option ByteSeq
  | 0, bytes => some bytes
  | groups + 1, bytes =>
      match readUleb32 bytes with
      | some (_, afterCount) =>
          match skipValType afterCount with
          | some rest => skipLocalGroups groups rest
          | none => none
      | none => none

def skipTwoUlebs (bytes : ByteSeq) : Option ByteSeq :=
  match readUleb32 bytes with
  | some (_, rest) =>
      match readUleb32 rest with
      | some (_, tail) => some tail
      | none => none
  | none => none

def skipGcImmediate (sub : Nat) (bytes : ByteSeq) : Option ByteSeq :=
  if sub = 0x00 ∨ sub = 0x01 ∨ sub = 0x06 ∨ sub = 0x07 ∨
      sub = 0x0b ∨ sub = 0x0c ∨ sub = 0x0d ∨ sub = 0x0e ∨ sub = 0x10 then
    match readUleb32 bytes with
    | some (_, rest) => some rest
    | none => none
  else if sub = 0x02 ∨ sub = 0x03 ∨ sub = 0x04 ∨ sub = 0x05 ∨
      sub = 0x08 ∨ sub = 0x09 ∨ sub = 0x0a ∨ sub = 0x11 ∨
      sub = 0x12 ∨ sub = 0x13 then
    skipTwoUlebs bytes
  else if sub = 0x0f ∨ sub = 0x1a ∨ sub = 0x1b ∨
      sub = 0x1c ∨ sub = 0x1d ∨ sub = 0x1e then
    some bytes
  else if sub = 0x14 ∨ sub = 0x15 ∨ sub = 0x16 ∨ sub = 0x17 then
    skipSignedLebBytes 5 bytes
  else none

/-- Full-body instruction scan for direct callees.  Rejected opcodes return
    `none`, which also makes reachable imports fail when closure expansion asks
    `codeEntryByFuncIndex` for a body that does not exist. -/
def scanClosureInstrs : Nat → ByteSeq → Option (List Nat)
  | 0, _ => none
  | _fuel + 1, [] => some []
  | fuel + 1, op :: bytes =>
      let next (rest : ByteSeq) := scanClosureInstrs fuel rest
      let oneUleb := fun () =>
        match readUleb32 bytes with
        | some (_, rest) => next rest
        | none => none
      if op = 0x10 ∨ op = 0x12 then
        match readUleb32 bytes with
        | some (callee, rest) =>
            (next rest).map (fun calls => callee :: calls)
        | none => none
      else if op = 0x02 ∨ op = 0x03 ∨ op = 0x04 then
        match skipBlockType bytes with
        | some rest => next rest
        | none => none
      else if op = 0x0c ∨ op = 0x0d ∨ op = 0x20 ∨ op = 0x21 ∨ op = 0x22 then
        oneUleb ()
      else if op = 0x0e then
        match readUleb32 bytes with
        | some (count, rest) =>
            match skipUlebsFuel (count + 2) (count + 1) rest with
            | some tail => next tail
            | none => none
        | none => none
      else if op = 0x1c then
        match readUleb32 bytes with
        | some (count, rest) =>
            match skipValTypesFuel (count + 1) count rest with
            | some tail => next tail
            | none => none
        | none => none
      else if op = 0x41 ∨ op = 0xd0 then
        match skipSignedLebBytes 5 bytes with
        | some rest => next rest
        | none => none
      else if op = 0x42 then
        match skipSignedLebBytes 10 bytes with
        | some rest => next rest
        | none => none
      else if op = 0x43 then
        match takeN 4 bytes with
        | some (_, rest) => next rest
        | none => none
      else if op = 0x44 then
        match takeN 8 bytes with
        | some (_, rest) => next rest
        | none => none
      else if op = 0xfb then
        match readUleb32 bytes with
        | some (sub, rest) =>
            match skipGcImmediate sub rest with
            | some tail => next tail
            | none => none
        | none => none
      else if op = 0xfc then
        match readUleb32 bytes with
        | some (sub, rest) => if sub ≤ 7 then next rest else none
        | none => none
      else if op = 0x00 ∨ op = 0x01 ∨ op = 0x05 ∨ op = 0x0b ∨
          op = 0x0f ∨ op = 0x1a ∨ op = 0x1b ∨ op = 0xd1 ∨ op = 0xd3 ∨
          (0x45 ≤ op ∧ op ≤ 0xc4) then
        next bytes
      else none

def scanClosureCodeEntry (entry : ByteSeq) : Option (List Nat) :=
  match readUleb32 entry with
  | some (_, body) =>
      match readUleb32 body with
      | some (localGroups, afterGroupCount) =>
          match skipLocalGroups localGroups afterGroupCount with
          | some instrs => scanClosureInstrs (instrs.length + 1) instrs
          | none => none
      | none => none
  | none => none

def scanClosureBody (modBytes modLen funcIdx : Nat) : Option (List Nat) :=
  match codeEntryByFuncIndex modBytes modLen funcIdx with
  | some entry => scanClosureCodeEntry entry
  | none => none

def natMem (x : Nat) : List Nat → Bool
  | [] => false
  | y :: ys => x == y || natMem x ys

/-- Shared balanced index layer for audited set-shaped checks. -/
def orderedSet [Ord α] (xs : List α) : Std.TreeSet α :=
  xs.foldl (fun set value => set.insert value) Std.TreeSet.empty

def indexedNodup [Ord α] (xs : List α) : Bool :=
  let set := orderedSet xs
  set.size == xs.length

def indexedSubset [Ord α] (xs ys : List α) : Bool :=
  let set := orderedSet ys
  xs.all set.contains

def indexedSetEq [Ord α] (xs ys : List α) : Bool :=
  indexedSubset xs ys && indexedSubset ys xs

def uniqueMap [Ord Key] : List (Key × Value) → Option (Std.TreeMap Key Value)
  | [] => some Std.TreeMap.empty
  | (key, value) :: rest =>
      match uniqueMap rest with
      | none => none
      | some index =>
          if index.contains key then none else some (index.insert key value)

def natSetEq (xs ys : List Nat) : Bool :=
  indexedSetEq xs ys

def natListNodup (xs : List Nat) : Bool := indexedNodup xs

/-- Fuel-bounded transitive direct-call closure, using the spike-proven
    worklist/seen fold over the big-Nat module representation. -/
def closureFold (modBytes modLen : Nat) :
    Nat → List Nat → List Nat → Option (List Nat)
  | 0, [], seen => some seen
  | 0, _ :: _, _ => none
  | _fuel + 1, [], seen => some seen
  | fuel + 1, func :: work, seen =>
      if natMem func seen then
        closureFold modBytes modLen fuel work seen
      else
        match scanClosureBody modBytes modLen func with
        | some callees =>
            closureFold modBytes modLen fuel (callees ++ work) (func :: seen)
        | none => none

def memoryLimitsUnshared : Nat → Nat → Nat → Option (Nat × Nat)
  | 0, bytes, len => some (bytes, len)
  | count + 1, bytes, len =>
      match readNatUleb32 bytes len with
      | some (flags, afterFlags, afterFlagsLen) =>
          if flags < 16 ∧ (flags &&& 0x02) = 0 then
            match readNatUleb32 afterFlags afterFlagsLen with
            | some (_, afterMin, afterMinLen) =>
                let afterMax? :=
                  if (flags &&& 0x01) != 0 then readNatUleb32 afterMin afterMinLen
                  else some (0, afterMin, afterMinLen)
                match afterMax? with
                | some (_, afterMax, afterMaxLen) =>
                    if (flags &&& 0x08) != 0 then
                      match readNatUleb32 afterMax afterMaxLen with
                      | some (_, tail, tailLen) =>
                          memoryLimitsUnshared count tail tailLen
                      | none => none
                    else memoryLimitsUnshared count afterMax afterMaxLen
                | none => none
            | none => none
          else none
      | none => none

/-- Shared linear memory is a module-level hidden channel even if the current
    bodies contain no atomic opcode, so the strict profile rejects the
    declaration itself. -/
def noSharedMemory (modBytes modLen : Nat) : Bool :=
  match CertDecode.modulePayload 0x05 modBytes modLen with
  | none => true
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          match memoryLimitsUnshared count rest restLen with
          | some (_, 0) => true
          | _ => false
      | none => false

end AverCert.WasmSlice
