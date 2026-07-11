-- Lean-side relevant Wasm byte slicer for certificate artifacts.
--
-- This is intentionally not a full WebAssembly validator. It parses only the
-- module facts needed to bind an exported function name to its raw function
-- binding: header, import function count, function section type index, export
-- section and code section.
-- Unsupported import kinds decline.
import SchemaCore

namespace AverCert.WasmSlice

abbrev ByteSeq := List Nat

structure FuncBinding where
  funcIdx : Nat
  codeIdx : Nat
  typeIdx : Nat
  codeEntry : ByteSeq

def takeN : Nat → ByteSeq → Option (ByteSeq × ByteSeq)
  | 0, xs => some ([], xs)
  | n + 1, x :: xs =>
      match takeN n xs with
      | some (taken, rest) => some (x :: taken, rest)
      | none => none
  | _ + 1, [] => none

def readByte : ByteSeq → Option (Nat × ByteSeq)
  | b :: rest => if b < 256 then some (b, rest) else none
  | [] => none

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

def readNameBytes (bytes : ByteSeq) : Option (ByteSeq × ByteSeq) :=
  match readUleb32 bytes with
  | some (len, rest) => takeN len rest
  | none => none

def stripHeader : ByteSeq → Option ByteSeq
  | 0x00 :: 0x61 :: 0x73 :: 0x6d :: 0x01 :: 0x00 :: 0x00 :: 0x00 :: rest =>
      some rest
  | _ => none

def findSectionPayloadFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, _target, [] => none
  | fuel + 1, target, id :: rest =>
      match readUleb32 rest with
      | some (size, afterSize) =>
          match takeN size afterSize with
          | some (payload, remaining) =>
              if id = target then
                some payload
              else
                findSectionPayloadFuel fuel target remaining
          | none => none
      | none => none

def findSectionPayload (target : Nat) (wasmBytes : ByteSeq) : Option ByteSeq :=
  match stripHeader wasmBytes with
  | some sections => findSectionPayloadFuel (sections.length + 1) target sections
  | none => none

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

def readS33 (bytes : ByteSeq) : Option (Int × ByteSeq) :=
  readSlebFuel 6 0 0 bytes

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

/-- Skip one field type: a storage type (value type, or packed `0x78`/`0x77`)
    followed by a mutability byte. -/
def skipFieldType (bytes : ByteSeq) : Option ByteSeq :=
  let storage? : Option ByteSeq :=
    match bytes with
    | 0x78 :: rest => some rest
    | 0x77 :: rest => some rest
    | _ => skipValType bytes
  match storage? with
  | some (m :: rest) => if m = 0x00 ∨ m = 0x01 then some rest else none
  | _ => none

def skipFieldTypesFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, 0, bytes => some bytes
  | fuel + 1, n + 1, bytes =>
      match skipFieldType bytes with
      | some rest => skipFieldTypesFuel fuel n rest
      | none => none

/-- Skip one composite type: `0x60` func (params vec + results vec), `0x5f`
    struct (field vec), `0x5e` array (one field). -/
def skipCompType (bytes : ByteSeq) : Option ByteSeq :=
  match bytes with
  | 0x60 :: rest =>
      match readUleb32 rest with
      | some (np, r1) =>
          match skipValTypesFuel (np + 1) np r1 with
          | some r2 =>
              match readUleb32 r2 with
              | some (nr, r3) => skipValTypesFuel (nr + 1) nr r3
              | none => none
          | none => none
      | none => none
  | 0x5f :: rest =>
      match readUleb32 rest with
      | some (nf, r1) => skipFieldTypesFuel (nf + 1) nf r1
      | none => none
  | 0x5e :: rest => skipFieldType rest
  | _ => none

def skipUlebsFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, 0, bytes => some bytes
  | fuel + 1, n + 1, bytes =>
      match readUleb32 bytes with
      | some (_, rest) => skipUlebsFuel fuel n rest
      | none => none

/-- Skip one subtype: an optional `0x50`/`0x4f` prefix carrying a supertype
    index vector, then the composite type. -/
def skipSubType (bytes : ByteSeq) : Option ByteSeq :=
  match bytes with
  | 0x50 :: rest | 0x4f :: rest =>
      match readUleb32 rest with
      | some (ns, r1) =>
          match skipUlebsFuel (ns + 1) ns r1 with
          | some r2 => skipCompType r2
          | none => none
      | none => none
  | _ => skipCompType bytes

def skipSubTypesFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, 0, bytes => some bytes
  | fuel + 1, n + 1, bytes =>
      match skipSubType bytes with
      | some rest => skipSubTypesFuel fuel n rest
      | none => none

/-- Whether `n` value types, each exactly `(ref null carrier)`, sit at the head
    of `bytes`; returns the remainder. -/
def readCarrierRefsFuel : Nat → Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _, _ => none
  | _fuel + 1, 0, _carrier, bytes => some bytes
  | fuel + 1, n + 1, carrier, bytes =>
      match bytes with
      | 0x63 :: rest =>
          match readS33 rest with
          | some (idx, rest') =>
              if idx = Int.ofNat carrier then
                readCarrierRefsFuel fuel n carrier rest'
              else
                none
          | none => none
      | _ => none

/-- Whether the subtype at the head of `bytes` is EXACTLY the plain (final,
    supertype-free) canonical certified function type
    `[(ref null carrier)^arity] → [(ref null carrier)]`. Subtype prefixes
    fail-close: the emitter never produces them for certified functions. -/
def checkCanonicalFuncType (arity carrier : Nat) (bytes : ByteSeq) : Bool :=
  match bytes with
  | 0x60 :: rest =>
      match readUleb32 rest with
      | some (np, r1) =>
          if np = arity then
            match readCarrierRefsFuel (np + 1) np carrier r1 with
            | some r2 =>
                match readUleb32 r2 with
                | some (nr, r3) =>
                    if nr = 1 then
                      match readCarrierRefsFuel 2 1 carrier r3 with
                      | some _ => true
                      | none => false
                    else
                      false
                | none => false
            | none => false
          else
            false
      | none => false
  | _ => false

/-- Walk exactly `n` consecutive subtypes at cumulative type index `base`,
    applying `check` to the one at absolute index `target`. Returns the remaining
    bytes and whether the target (if it fell in this run) passed `check`. Every
    subtype is fully skipped (`skipSubType` fail-closes on malformed), so the run
    is exact — a garbage subtype anywhere in the run declines. -/
def walkSubtypesFuel (check : ByteSeq → Bool) (target : Nat) :
    Nat → Nat → Nat → ByteSeq → Bool → Option (ByteSeq × Bool)
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _base, bytes, acc => some (bytes, acc)
  | fuel + 1, n + 1, base, bytes, acc =>
      let acc := if base = target then check bytes else acc
      match skipSubType bytes with
      | some rest => walkSubtypesFuel check target fuel n (base + 1) rest acc
      | none => none

/-- Walk the type section's ENTIRE rectype vector (all `remaining` rec groups —
    an explicit `0x4e cnt` group defines `cnt` consecutive type indices, a bare
    subtype defines one), applying `check` to the subtype at absolute index
    `target`. Every rec group is fully parsed (not just up to `target`), so the
    remainder returned is the tail after the last declared rectype; the caller
    requires it to be EMPTY, rejecting trailing bytes / a count mismatch. -/
def walkRecTypesFuel (check : ByteSeq → Bool) (target : Nat) :
    Nat → Nat → Nat → ByteSeq → Bool → Option (ByteSeq × Bool)
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _base, bytes, acc => some (bytes, acc)
  | fuel + 1, rem + 1, base, bytes, acc =>
      match bytes with
      | 0x4e :: rest =>
          match readUleb32 rest with
          | some (cnt, r1) =>
              match walkSubtypesFuel check target (cnt + 1) cnt base r1 acc with
              | some (r2, acc') => walkRecTypesFuel check target fuel rem (base + cnt) r2 acc'
              | none => none
          | none => none
      | _ =>
          match walkSubtypesFuel check target 2 1 base bytes acc with
          | some (r2, acc') => walkRecTypesFuel check target fuel rem (base + 1) r2 acc'
          | none => none

/-- Whether the module's type section is well-formed AND its entry `typeIdx`
    satisfies `check`. The whole rectype vector is parsed and required to consume
    the section payload EXACTLY (no trailing bytes past the last rectype), so a
    valid target entry followed by garbage is rejected, not silently accepted. -/
def typeSectionMatches (check : ByteSeq → Bool)
    (wasmBytes : ByteSeq) (typeIdx : Nat) : Bool :=
  match findSectionPayload 0x01 wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          match walkRecTypesFuel check typeIdx (count + 1) count 0 rest false with
          | some ([], acc) => acc
          | _ => false
      | none => false
  | none => false

/-- Whether the module's type-section entry `typeIdx` is exactly the canonical
    certified function type `[(ref null carrier)^arity] → [(ref null carrier)]`.
    This binds a claimed function binding's declared signature to the plan's
    params/result without trusting either, and (via `typeSectionMatches`) requires
    the whole type section to be well-formed and exactly consumed. -/
def funcTypeMatches (wasmBytes : ByteSeq) (typeIdx arity carrier : Nat) : Bool :=
  typeSectionMatches (checkCanonicalFuncType arity carrier) wasmBytes typeIdx

/-- Whether the head of `bytes` is EXACTLY the certified verbatim dispatch
    function type for `resultSig`: `[eqref] → [(ref null heapTy)]` for a
    `.refNull heapTy` plan, or `[eqref] → [f64]` for `.f64Scalar`. The result
    bytes are parsed from the type section and must match the plan variant
    exactly; accepting f64 never loosens the nullable-reference branch. -/
def checkVerbatimFuncType (resultSig : AverCert.Schema.VerbatimResultSig)
    (bytes : ByteSeq) : Bool :=
  match bytes with
  | 0x60 :: 0x01 :: 0x6d :: rest =>
      match readUleb32 rest with
      | some (nr, r1) =>
          if nr = 1 then
            match resultSig, r1 with
            | .refNull resultHeapTy, 0x63 :: r2 =>
                match readS33 r2 with
                | some (idx, _) => idx == Int.ofNat resultHeapTy
                | none => false
            | .f64Scalar, 0x7c :: _ => true
            | _, _ => false
          else
            false
      | none => false
  | _ => false

/-- Whether the module's byte-derived type-section entry `typeIdx` exactly
    matches the verbatim plan's declared result-signature variant. -/
def verbatimFuncTypeMatches (wasmBytes : ByteSeq) (typeIdx : Nat)
    (resultSig : AverCert.Schema.VerbatimResultSig) : Bool :=
  typeSectionMatches (checkVerbatimFuncType resultSig) wasmBytes typeIdx

/-! ### Bare field-projection type binding -/

def readProjectionResultTy
    (expected : AverCert.Schema.FieldProjectionResultTy) : ByteSeq → Option ByteSeq
  | 0x6d :: rest =>
      if expected = .eqref then some rest else none
  | 0x63 :: rest =>
      match expected, readS33 rest with
      | .nullableRef expectedIdx, some (actualIdx, tail) =>
          if actualIdx = Int.ofNat expectedIdx then some tail else none
      | _, _ => none
  | _ => none

def readProjectionField
    (selected current : Nat)
    (expected : AverCert.Schema.FieldProjectionResultTy)
    (bytes : ByteSeq) : Option ByteSeq :=
  let storage? :=
    if current = selected then readProjectionResultTy expected bytes
    else skipValType bytes
  match storage? with
  | some (mutability :: rest) =>
      if mutability = 0x00 ∨ mutability = 0x01 then some rest else none
  | _ => none

def readProjectionFieldsFuel
    (selected : Nat) (expected : AverCert.Schema.FieldProjectionResultTy) :
    Nat → Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _, _ => none
  | _fuel + 1, 0, _current, bytes => some bytes
  | fuel + 1, remaining + 1, current, bytes =>
      match readProjectionField selected current expected bytes with
      | some rest => readProjectionFieldsFuel selected expected fuel remaining (current + 1) rest
      | none => none

def checkProjectionStructType
    (fieldCount fieldIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : ByteSeq → Bool
  | 0x5f :: rest =>
      match readUleb32 rest with
      | some (actualCount, fields) =>
          actualCount = fieldCount && fieldIdx < actualCount &&
            (readProjectionFieldsFuel fieldIdx resultTy
              (actualCount + 1) actualCount 0 fields).isSome
      | none => false
  | _ => false

def projectionStructTypeMatches
    (wasmBytes : ByteSeq) (structIdx fieldCount fieldIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches
    (checkProjectionStructType fieldCount fieldIdx resultTy)
    wasmBytes structIdx

def checkProjectionFuncType
    (structIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : ByteSeq → Bool
  | 0x60 :: rest =>
      match readUleb32 rest with
      | some (1, 0x63 :: paramTail) =>
          match readS33 paramTail with
          | some (paramIdx, resultCountBytes) =>
              if paramIdx = Int.ofNat structIdx then
                match readUleb32 resultCountBytes with
                | some (1, resultBytes) => (readProjectionResultTy resultTy resultBytes).isSome
                | _ => false
              else false
          | none => false
      | _ => false
  | _ => false

def projectionFuncTypeMatches
    (wasmBytes : ByteSeq) (typeIdx structIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches (checkProjectionFuncType structIdx resultTy) wasmBytes typeIdx

/-! ### List-constructor type binding -/

def readConstructValType
    (expected : AverCert.Schema.ConstructValType) : ByteSeq → Option ByteSeq
  | 0x7f :: rest => if expected = .i32 then some rest else none
  | 0x7e :: rest => if expected = .i64 then some rest else none
  | 0x7c :: rest => if expected = .f64 then some rest else none
  | 0x6d :: rest => if expected = .eqref then some rest else none
  | 0x63 :: rest =>
      match expected, readS33 rest with
      | .nullableRef expectedIdx, some (actualIdx, tail) =>
          if actualIdx = Int.ofNat expectedIdx then some tail else none
      | _, _ => none
  | _ => none

def readImmutableConstructField
    (expected : AverCert.Schema.ConstructValType) (bytes : ByteSeq) : Option ByteSeq :=
  match readConstructValType expected bytes with
  | some (0x00 :: rest) => some rest
  | _ => none

def checkListConstructStructType
    (structIdx : Nat) (elemTy : AverCert.Schema.ConstructValType) : ByteSeq → Bool
  | 0x5f :: rest =>
      match readUleb32 rest with
      | some (2, fields) =>
          match readImmutableConstructField elemTy fields with
          | some tailField =>
              (readImmutableConstructField (.nullableRef structIdx) tailField).isSome
          | none => false
      | _ => false
  | _ => false

def listConstructStructTypeMatches
    (wasmBytes : ByteSeq) (structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructStructType structIdx elemTy) wasmBytes structIdx

def checkListConstructFuncType
    (arity structIdx : Nat) (elemTy : AverCert.Schema.ConstructValType) : ByteSeq → Bool
  | 0x60 :: rest =>
      match readUleb32 rest with
      | some (actualArity, params) =>
          if actualArity = arity then
            match readConstructValType elemTy params with
            | some afterHead =>
                let afterParams? :=
                  if arity = 1 then some afterHead
                  else if arity = 2 then
                    readConstructValType (.nullableRef structIdx) afterHead
                  else none
                match afterParams? with
                | some resultCountBytes =>
                    match readUleb32 resultCountBytes with
                    | some (1, resultBytes) =>
                        (readConstructValType (.nullableRef structIdx) resultBytes).isSome
                    | _ => false
                | none => false
            | none => false
          else false
      | none => false
  | _ => false

def listConstructFuncTypeMatches
    (wasmBytes : ByteSeq) (typeIdx arity structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructFuncType arity structIdx elemTy) wasmBytes typeIdx

/-! ### Passive data-section navigation

`array.new_data` encodes only the referenced segment's INDEX and the copied
LENGTH into the code entry — never the segment's byte CONTENTS, which live in the
data section (id 11). The verbatim string-literal leaves therefore need the exact
segment bytes recovered here to bind a plan's claimed payload to the module. The
wasm-gc backend emits every data segment passively (`0x01 <vec byte>`); an active
flag (`0x00`/`0x02`) is a shape this slicer never needs to cross, so it
fail-closes. -/

/-- Read one passive data segment (`0x01 <vec byte>`), returning its byte
    contents and the remaining bytes; any other flag fail-closes. -/
def readPassiveDataSegment : ByteSeq → Option (ByteSeq × ByteSeq)
  | 0x01 :: rest =>
      match readUleb32 rest with
      | some (len, afterLen) => takeN len afterLen
      | none => none
  | _ => none

/-- Walk ALL `remaining` passive data segments, recording the contents of the one
    at absolute index `target`. Returns the tail after the last segment and the
    recorded contents; the caller requires the tail to be EMPTY, so a segment
    followed by garbage or a count that overshoots the payload declines. -/
def walkDataSegmentsFuel (target : Nat) :
    Nat → Nat → Nat → ByteSeq → Option ByteSeq → Option (ByteSeq × Option ByteSeq)
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _cur, bytes, found => some (bytes, found)
  | fuel + 1, rem + 1, cur, bytes, found =>
      match readPassiveDataSegment bytes with
      | some (contents, rest) =>
          let found := if cur = target then some contents else found
          walkDataSegmentsFuel target fuel rem (cur + 1) rest found
      | none => none

/-- The exact byte contents of passive data segment `dataIdx`, recovered from the
    module's data section (id 11). The WHOLE segment vector is parsed and required
    to consume the section payload EXACTLY (no trailing bytes), so a valid target
    segment followed by garbage — or a declared count that does not match the
    bytes — declines. `none` if the section is absent, `dataIdx` is out of range,
    or any segment on the way is not passive. -/
def dataSegmentBytes (wasmBytes : ByteSeq) (dataIdx : Nat) : Option ByteSeq :=
  match findSectionPayload 0x0b wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          if dataIdx < count then
            match walkDataSegmentsFuel dataIdx (count + 1) count 0 rest none with
            | some ([], found) => found
            | _ => none
          else none
      | none => none
  | none => none

def readImportFuncFlag (bytes : ByteSeq) : Option (Bool × ByteSeq) :=
  match readNameBytes bytes with
  | some (_, rest1) =>
      match readNameBytes rest1 with
      | some (_, rest2) =>
          match readByte rest2 with
          | some (kind, rest3) =>
              if kind = 0x00 then
                match readUleb32 rest3 with
                | some (_, rest4) => some (true, rest4)
                | none => none
              else
                none
          | none => none
      | none => none
  | none => none

def countFuncImportsFuel : Nat → Nat → ByteSeq → Nat → Option Nat
  | 0, _, _, _ => none
  | _fuel + 1, 0, _bytes, acc => some acc
  | fuel + 1, count + 1, bytes, acc =>
      match readImportFuncFlag bytes with
      | some (isFunc, rest) =>
          countFuncImportsFuel fuel count rest (if isFunc then acc + 1 else acc)
      | none => none

def importedFuncCount (wasmBytes : ByteSeq) : Option Nat :=
  match findSectionPayload 0x02 wasmBytes with
  | none => some 0
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) => countFuncImportsFuel (count + 1) count rest 0
      | none => none

def readExportEntry (bytes : ByteSeq) :
    Option (ByteSeq × Nat × Nat × ByteSeq) :=
  match readNameBytes bytes with
  | some (name, rest1) =>
      match readByte rest1 with
      | some (kind, rest2) =>
          match readUleb32 rest2 with
          | some (idx, rest3) => some (name, kind, idx, rest3)
          | none => none
      | none => none
  | none => none

def findExportFuncIndexFuel : Nat → Nat → ByteSeq → ByteSeq → Option Nat
  | 0, _, _, _ => none
  | _fuel + 1, 0, _bytes, _targetName => none
  | fuel + 1, count + 1, bytes, targetName =>
      match readExportEntry bytes with
      | some (name, kind, idx, rest) =>
          if kind = 0x00 then
            if name = targetName then
              some idx
            else
              findExportFuncIndexFuel fuel count rest targetName
          else
            findExportFuncIndexFuel fuel count rest targetName
      | none => none

def exportFuncIndex (wasmBytes targetName : ByteSeq) : Option Nat :=
  match findSectionPayload 0x07 wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          findExportFuncIndexFuel (count + 1) count rest targetName
      | none => none
  | none => none

def readCodeEntry (bytes : ByteSeq) : Option (ByteSeq × ByteSeq) :=
  match readUleb32 bytes with
  | some (size, afterSize) =>
      let prefixLen := bytes.length - afterSize.length
      let lenBytes := bytes.take prefixLen
      match takeN size afterSize with
      | some (body, rest) => some (lenBytes ++ body, rest)
      | none => none
  | none => none

def codeEntryByCodeIndexFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | _fuel + 1, _idx, [] => none
  | fuel + 1, idx, bytes =>
      match readCodeEntry bytes with
      | some (entry, rest) =>
          if idx = 0 then
            some entry
          else
            codeEntryByCodeIndexFuel fuel (idx - 1) rest
      | none => none

def codeEntryByCodeIndex (wasmBytes : ByteSeq) (codeIdx : Nat) : Option ByteSeq :=
  match findSectionPayload 0x0a wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          if codeIdx < count then
            codeEntryByCodeIndexFuel (count + 1) codeIdx rest
          else
            none
      | none => none
  | none => none

def typeIndexByCodeIndexFuel : Nat → Nat → Nat → ByteSeq → Option Nat
  | 0, _, _, _ => none
  | _fuel + 1, _idx, 0, _bytes => none
  | fuel + 1, idx, count + 1, bytes =>
      match readUleb32 bytes with
      | some (typeIdx, rest) =>
          if idx = 0 then
            some typeIdx
          else
            typeIndexByCodeIndexFuel fuel (idx - 1) count rest
      | none => none

def typeIndexByCodeIndex (wasmBytes : ByteSeq) (codeIdx : Nat) : Option Nat :=
  match findSectionPayload 0x03 wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          if codeIdx < count then
            typeIndexByCodeIndexFuel (count + 1) codeIdx count rest
          else
            none
      | none => none
  | none => none

def codeEntryByFuncIndex (wasmBytes : ByteSeq) (funcIdx : Nat) : Option ByteSeq :=
  match importedFuncCount wasmBytes with
  | some imported =>
      if imported ≤ funcIdx then
        codeEntryByCodeIndex wasmBytes (funcIdx - imported)
      else
        none
  | none => none

def funcBindingByFuncIndex (wasmBytes : ByteSeq) (funcIdx : Nat) : Option FuncBinding :=
  match importedFuncCount wasmBytes with
  | some imported =>
      if imported ≤ funcIdx then
        let codeIdx := funcIdx - imported
        match typeIndexByCodeIndex wasmBytes codeIdx,
              codeEntryByCodeIndex wasmBytes codeIdx with
        | some typeIdx, some codeEntry =>
            some { funcIdx := funcIdx, codeIdx := codeIdx, typeIdx := typeIdx, codeEntry := codeEntry }
        | _, _ => none
      else
        none
  | none => none

def codeEntryForExport (wasmBytes targetName : ByteSeq) : Option ByteSeq :=
  match exportFuncIndex wasmBytes targetName with
  | some funcIdx => codeEntryByFuncIndex wasmBytes funcIdx
  | none => none

def funcBindingForExport (wasmBytes targetName : ByteSeq) : Option FuncBinding :=
  match exportFuncIndex wasmBytes targetName with
  | some funcIdx => funcBindingByFuncIndex wasmBytes funcIdx
  | none => none

end AverCert.WasmSlice
