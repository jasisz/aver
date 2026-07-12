-- Lean-side relevant Wasm byte slicer for certificate artifacts.
--
-- This is intentionally not a full WebAssembly validator. It parses only the
-- module facts needed to bind an exported function name to its raw function
-- binding: header, import function count, function section type index, export
-- section and code section.
-- Unsupported import kinds decline.
import SchemaCore
import CertDecode
import Std.Data.TreeSet

namespace AverCert.WasmSlice

abbrev ByteSeq := List Nat

structure FuncBinding where
  funcIdx : Nat
  codeIdx : Nat
  typeIdx : Nat
  codeEntry : ByteSeq

/-! ### Whole-module byte cursor

The Wasm module is one little-endian `Nat` plus its explicit byte length.  This
is the same byte model and the same shift/mask discipline as
`tools/certkit/prelude/CertDecode.lean`: reading uses `&&& 0xff`, advancing uses
`>>> 8`, and skipping a framed payload uses one `>>> (8 * size)`.  The length is
soundness-relevant because high trailing `0x00` bytes are not represented by the
numeral.

Only whole-module navigation uses this cursor.  Export names, selected type
entries, data payloads, and code entries cross back to `ByteSeq`; those objects
are small and remain the byte-exact certificate surface. -/

abbrev takeNatBytes := CertDecode.takeBytes

@[inline] def readNatUleb32 := CertDecode.readU

def readNatS33 (bytes len : Nat) : Option (Int × Nat × Nat) :=
  match CertDecode.sleb 5 0 0 bytes len with
  | some (v, bytes', len') =>
      if -(Int.ofNat (2 ^ 32)) ≤ v ∧ v < Int.ofNat (2 ^ 32) then
        some (v, bytes', len')
      else
        none
  | none => none

/-- Validate and remove the exact eight-byte Wasm header. -/
def stripHeader (modBytes modLen : Nat) : Option (Nat × Nat) :=
  if 8 ≤ modLen ∧ (modBytes &&& 0xffffffffffffffff) = 0x000000016d736100 then
    some (modBytes >>> 64, modLen - 8)
  else
    none

def findSectionPayload (target modBytes modLen : Nat) : Option (Nat × Nat) :=
  match CertDecode.moduleView modBytes modLen with
  | some view => view.payload target
  | none => none

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

/-! The matching whole-module walkers use the shared big-`Nat` cursor.  They
parse every declared subtype, but materialize only the selected subtype as a
small `ByteSeq` for the existing signature checker. -/

def skipValTypeNat (bytes len : Nat) : Option (Nat × Nat) :=
  if len = 0 then none else
    let b := bytes &&& 0xff
    let rest := bytes >>> 8
    let restLen := len - 1
    if b = 0x63 ∨ b = 0x64 then
      match readNatS33 rest restLen with
      | some (_, rest', restLen') => some (rest', restLen')
      | none => none
    else if (0x7b ≤ b ∧ b ≤ 0x7f) ∨ (0x69 ≤ b ∧ b ≤ 0x73) then
      some (rest, restLen)
    else
      none

def skipValTypesNatFuel : Nat → Nat → Nat → Nat → Option (Nat × Nat)
  | 0, _, _, _ => none
  | _fuel + 1, 0, bytes, len => some (bytes, len)
  | fuel + 1, count + 1, bytes, len =>
      match skipValTypeNat bytes len with
      | some (rest, restLen) => skipValTypesNatFuel fuel count rest restLen
      | none => none

def skipFieldTypeNat (bytes len : Nat) : Option (Nat × Nat) :=
  if len = 0 then none else
    let b := bytes &&& 0xff
    let storage? :=
      if b = 0x78 ∨ b = 0x77 then some (bytes >>> 8, len - 1)
      else skipValTypeNat bytes len
    match storage? with
    | some (afterStorage, storageLen) =>
        if storageLen = 0 then none else
          let mutability := afterStorage &&& 0xff
          if mutability = 0x00 ∨ mutability = 0x01 then
            some (afterStorage >>> 8, storageLen - 1)
          else
            none
    | none => none

def skipFieldTypesNatFuel : Nat → Nat → Nat → Nat → Option (Nat × Nat)
  | 0, _, _, _ => none
  | _fuel + 1, 0, bytes, len => some (bytes, len)
  | fuel + 1, count + 1, bytes, len =>
      match skipFieldTypeNat bytes len with
      | some (rest, restLen) => skipFieldTypesNatFuel fuel count rest restLen
      | none => none

def skipCompTypeNat (bytes len : Nat) : Option (Nat × Nat) :=
  if len = 0 then none else
    let tag := bytes &&& 0xff
    let rest := bytes >>> 8
    let restLen := len - 1
    if tag = 0x60 then
      match readNatUleb32 rest restLen with
      | some (np, params, paramsLen) =>
          match skipValTypesNatFuel (np + 1) np params paramsLen with
          | some (afterParams, afterParamsLen) =>
              match readNatUleb32 afterParams afterParamsLen with
              | some (nr, results, resultsLen) =>
                  skipValTypesNatFuel (nr + 1) nr results resultsLen
              | none => none
          | none => none
      | none => none
    else if tag = 0x5f then
      match readNatUleb32 rest restLen with
      | some (nf, fields, fieldsLen) =>
          skipFieldTypesNatFuel (nf + 1) nf fields fieldsLen
      | none => none
    else if tag = 0x5e then
      skipFieldTypeNat rest restLen
    else
      none

def skipNatUlebsFuel : Nat → Nat → Nat → Nat → Option (Nat × Nat)
  | 0, _, _, _ => none
  | _fuel + 1, 0, bytes, len => some (bytes, len)
  | fuel + 1, count + 1, bytes, len =>
      match readNatUleb32 bytes len with
      | some (_, rest, restLen) => skipNatUlebsFuel fuel count rest restLen
      | none => none

def skipSubTypeNat (bytes len : Nat) : Option (Nat × Nat) :=
  if len = 0 then none else
    let b := bytes &&& 0xff
    if b = 0x50 ∨ b = 0x4f then
      match readNatUleb32 (bytes >>> 8) (len - 1) with
      | some (ns, supertypes, supertypesLen) =>
          match skipNatUlebsFuel (ns + 1) ns supertypes supertypesLen with
          | some (comp, compLen) => skipCompTypeNat comp compLen
          | none => none
      | none => none
    else
      skipCompTypeNat bytes len

def walkSubtypesNatFuel (check : ByteSeq → Bool) (target : Nat) :
    Nat → Nat → Nat → Nat → Nat → Bool → Option (Nat × Nat × Bool)
  | 0, _, _, _, _, _ => none
  | _fuel + 1, 0, _base, bytes, len, acc => some (bytes, len, acc)
  | fuel + 1, count + 1, base, bytes, len, acc =>
      match skipSubTypeNat bytes len with
      | some (rest, restLen) =>
          let consumed := len - restLen
          let acc' := if base = target then check (takeNatBytes consumed bytes) else acc
          walkSubtypesNatFuel check target fuel count (base + 1) rest restLen acc'
      | none => none

def walkRecTypesNatFuel (check : ByteSeq → Bool) (target : Nat) :
    Nat → Nat → Nat → Nat → Nat → Bool → Option (Nat × Nat × Bool)
  | 0, _, _, _, _, _ => none
  | _fuel + 1, 0, _base, bytes, len, acc => some (bytes, len, acc)
  | fuel + 1, remaining + 1, base, bytes, len, acc =>
      if len = 0 then none else
        let b := bytes &&& 0xff
        if b = 0x4e then
          match readNatUleb32 (bytes >>> 8) (len - 1) with
          | some (count, subtypes, subtypesLen) =>
              match walkSubtypesNatFuel check target (count + 1) count base
                  subtypes subtypesLen acc with
              | some (rest, restLen, acc') =>
                  walkRecTypesNatFuel check target fuel remaining (base + count)
                    rest restLen acc'
              | none => none
          | none => none
        else
          match walkSubtypesNatFuel check target 2 1 base bytes len acc with
          | some (rest, restLen, acc') =>
              walkRecTypesNatFuel check target fuel remaining (base + 1)
                rest restLen acc'
          | none => none

/-- Whether the module's type section is well-formed AND its entry `typeIdx`
    satisfies `check`. The whole rectype vector is parsed and required to consume
    the section payload EXACTLY (no trailing bytes past the last rectype), so a
    valid target entry followed by garbage is rejected, not silently accepted. -/
def typeSectionMatches (check : ByteSeq → Bool)
    (modBytes modLen typeIdx : Nat) : Bool :=
  match findSectionPayload 0x01 modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          match walkRecTypesNatFuel check typeIdx (count + 1) count 0
              rest restLen false with
          | some (_, 0, acc) => acc
          | _ => false
      | none => false
  | none => false

/-- Whether the module's type-section entry `typeIdx` is exactly the canonical
    certified function type `[(ref null carrier)^arity] → [(ref null carrier)]`.
    This binds a claimed function binding's declared signature to the plan's
    params/result without trusting either, and (via `typeSectionMatches`) requires
    the whole type section to be well-formed and exactly consumed. -/
def funcTypeMatches (modBytes modLen typeIdx arity carrier : Nat) : Bool :=
  typeSectionMatches (checkCanonicalFuncType arity carrier) modBytes modLen typeIdx

/-- Whether the head of `bytes` is EXACTLY the certified verbatim dispatch
    function type for `resultSig`: one nullable concrete nominal-root parameter
    followed by `[(ref null heapTy)]` for a `.refNull heapTy` plan, or `[f64]`
    for `.f64Scalar`. The result
    bytes are parsed from the type section and must match the plan variant
    exactly; accepting f64 never loosens the nullable-reference branch. The
    parameter's s33 heap type must be non-negative: a negative s33 after `0x63`
    encodes an abstract heap type (e.g. long-form `eqref` as `0x63 0x6D`), not
    a concrete nominal root, so it fail-closes. -/
def checkVerbatimFuncType (resultSig : AverCert.Schema.VerbatimResultSig)
    (bytes : ByteSeq) : Bool :=
  match bytes with
  | 0x60 :: 0x01 :: 0x63 :: rootBytes =>
      match readS33 rootBytes with
      | some (rootIdx, rest) =>
          if rootIdx < 0 then false else
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
      | none => false
  | _ => false

/-- Whether the module's byte-derived type-section entry `typeIdx` exactly
    matches the verbatim plan's declared result-signature variant. -/
def verbatimFuncTypeMatches (modBytes modLen typeIdx : Nat)
    (resultSig : AverCert.Schema.VerbatimResultSig) : Bool :=
  typeSectionMatches (checkVerbatimFuncType resultSig) modBytes modLen typeIdx

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
    (modBytes modLen structIdx fieldCount fieldIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches
    (checkProjectionStructType fieldCount fieldIdx resultTy)
    modBytes modLen structIdx

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
    (modBytes modLen typeIdx structIdx : Nat)
    (resultTy : AverCert.Schema.FieldProjectionResultTy) : Bool :=
  typeSectionMatches (checkProjectionFuncType structIdx resultTy) modBytes modLen typeIdx

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
    (modBytes modLen structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructStructType structIdx elemTy) modBytes modLen structIdx

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
    (modBytes modLen typeIdx arity structIdx : Nat)
    (elemTy : AverCert.Schema.ConstructValType) : Bool :=
  typeSectionMatches (checkListConstructFuncType arity structIdx elemTy)
    modBytes modLen typeIdx

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
def walkDataSegmentsNatFuel (target : Nat) :
    Nat → Nat → Nat → Nat → Nat → Option ByteSeq → Option ByteSeq
  | 0, _, _, _, _, _ => none
  | _fuel + 1, 0, _current, _bytes, len, found =>
      if len = 0 then found else none
  | fuel + 1, remaining + 1, current, bytes, len, found =>
      if len = 0 ∨ (bytes &&& 0xff) != 0x01 then none else
        match readNatUleb32 (bytes >>> 8) (len - 1) with
        | none => none
        | some (size, contents, contentsLen) =>
            if size ≤ contentsLen then
              let found' :=
                if current = target then some (takeNatBytes size contents) else found
              walkDataSegmentsNatFuel target fuel remaining (current + 1)
                (contents >>> (8 * size)) (contentsLen - size) found'
            else
              none

/-- Exact selected passive data payload.  The full declared vector must parse
    and exhaust the section payload. -/
def dataSegmentBytes (modBytes modLen dataIdx : Nat) : Option ByteSeq :=
  match findSectionPayload 0x0b modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          if dataIdx < count then
            walkDataSegmentsNatFuel dataIdx (count + 1) count 0 rest restLen none
          else
            none
      | none => none
  | none => none

def skipNatName (bytes len : Nat) : Option (Nat × Nat) :=
  match readNatUleb32 bytes len with
  | some (nameLen, name, nameAndRestLen) =>
      if nameLen ≤ nameAndRestLen then
        some (name >>> (8 * nameLen), nameAndRestLen - nameLen)
      else
        none
  | none => none

def readImportFuncNat (bytes len : Nat) : Option (Nat × Nat) :=
  match skipNatName bytes len with
  | some (afterModule, afterModuleLen) =>
      match skipNatName afterModule afterModuleLen with
      | some (afterName, afterNameLen) =>
          if afterNameLen = 0 ∨ (afterName &&& 0xff) != 0x00 then none else
            match readNatUleb32 (afterName >>> 8) (afterNameLen - 1) with
            | some (_, rest, restLen) => some (rest, restLen)
            | none => none
      | none => none
  | none => none

def countFuncImportsNatFuel : Nat → Nat → Nat → Nat → Nat → Option Nat
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _bytes, len, count =>
      if len = 0 then some count else none
  | fuel + 1, remaining + 1, bytes, len, count =>
      match readImportFuncNat bytes len with
      | some (rest, restLen) =>
          countFuncImportsNatFuel fuel remaining rest restLen (count + 1)
      | none => none

def importedFuncCount (modBytes modLen : Nat) : Option Nat :=
  match findSectionPayload 0x02 modBytes modLen with
  | none => some 0
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          countFuncImportsNatFuel (count + 1) count rest restLen 0
      | none => none

/-! ### Whole-module interface enumeration

These folds retain the names that the older binding slicer intentionally
discarded.  They consume each section exactly and return `none` on malformed or
unsupported descriptors, so the artifact envelope fails closed. -/

structure ExportEntry where
  name : ByteSeq
  kind : Nat
  idx  : Nat
deriving Repr, DecidableEq

def readNatName (bytes len : Nat) : Option (ByteSeq × Nat × Nat) :=
  match readNatUleb32 bytes len with
  | some (nameLen, nameBytes, nameAndRestLen) =>
      if nameLen ≤ nameAndRestLen then
        some (takeNatBytes nameLen nameBytes,
          nameBytes >>> (8 * nameLen), nameAndRestLen - nameLen)
      else none
  | none => none

def skipLimitsNat (bytes len : Nat) : Option (Nat × Nat) :=
  match readNatUleb32 bytes len with
  | some (flags, afterFlags, afterFlagsLen) =>
      if flags < 16 then
        match readNatUleb32 afterFlags afterFlagsLen with
        | some (_, afterMin, afterMinLen) =>
            let afterMax? :=
              if (flags &&& 0x01) != 0 then readNatUleb32 afterMin afterMinLen
              else some (0, afterMin, afterMinLen)
            match afterMax? with
            | some (_, afterMax, afterMaxLen) =>
                if (flags &&& 0x08) != 0 then
                  match readNatUleb32 afterMax afterMaxLen with
                  | some (_, rest, restLen) => some (rest, restLen)
                  | none => none
                else some (afterMax, afterMaxLen)
            | none => none
        | none => none
      else none
  | none => none

def skipImportDescNat (bytes len : Nat) : Option (Nat × Nat) :=
  if len = 0 then none else
    let kind := bytes &&& 0xff
    let rest := bytes >>> 8
    let restLen := len - 1
    if kind = 0x00 then
      match readNatUleb32 rest restLen with
      | some (_, tail, tailLen) => some (tail, tailLen)
      | none => none
    else if kind = 0x01 then
      match skipValTypeNat rest restLen with
      | some (limits, limitsLen) => skipLimitsNat limits limitsLen
      | none => none
    else if kind = 0x02 then
      skipLimitsNat rest restLen
    else if kind = 0x03 then
      match skipValTypeNat rest restLen with
      | some (mutability, mutabilityLen) =>
          if mutabilityLen = 0 then none else
            let m := mutability &&& 0xff
            if m = 0 ∨ m = 1 then some (mutability >>> 8, mutabilityLen - 1)
            else none
      | none => none
    else if kind = 0x04 then
      if restLen = 0 ∨ (rest &&& 0xff) != 0 then none else
        match readNatUleb32 (rest >>> 8) (restLen - 1) with
        | some (_, tail, tailLen) => some (tail, tailLen)
        | none => none
    else none

def readImportNamePairNat (bytes len : Nat) :
    Option ((ByteSeq × ByteSeq) × Nat × Nat) :=
  match readNatName bytes len with
  | some (moduleName, afterModule, afterModuleLen) =>
      match readNatName afterModule afterModuleLen with
      | some (fieldName, descriptor, descriptorLen) =>
          match skipImportDescNat descriptor descriptorLen with
          | some (rest, restLen) => some ((moduleName, fieldName), rest, restLen)
          | none => none
      | none => none
  | none => none

def enumImportNamesNatFuel :
    Nat → Nat → Nat → Nat → List (ByteSeq × ByteSeq) →
      Option (List (ByteSeq × ByteSeq))
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _bytes, len, acc =>
      if len = 0 then some acc.reverse else none
  | fuel + 1, remaining + 1, bytes, len, acc =>
      match readImportNamePairNat bytes len with
      | some (pair, rest, restLen) =>
          enumImportNamesNatFuel fuel remaining rest restLen (pair :: acc)
      | none => none

def enumImportNames (modBytes modLen : Nat) :
    Option (List (ByteSeq × ByteSeq)) :=
  match findSectionPayload 0x02 modBytes modLen with
  | none => some []
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          enumImportNamesNatFuel (count + 1) count rest restLen []
      | none => none

def readExportEntryNat (bytes len : Nat) :
    Option (ByteSeq × Nat × Nat × Nat × Nat) :=
  match readNatUleb32 bytes len with
  | some (nameLen, nameBytes, nameAndRestLen) =>
      if nameLen ≤ nameAndRestLen then
        let name := takeNatBytes nameLen nameBytes
        let afterName := nameBytes >>> (8 * nameLen)
        let afterNameLen := nameAndRestLen - nameLen
        if afterNameLen = 0 then none else
          let kind := afterName &&& 0xff
          match readNatUleb32 (afterName >>> 8) (afterNameLen - 1) with
          | some (idx, rest, restLen) => some (name, kind, idx, rest, restLen)
          | none => none
      else
        none
  | none => none

def enumExportsNatFuel :
    Nat → Nat → Nat → Nat → List ExportEntry → Option (List ExportEntry)
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _bytes, len, acc =>
      if len = 0 then some acc.reverse else none
  | fuel + 1, remaining + 1, bytes, len, acc =>
      match readExportEntryNat bytes len with
      | some (name, kind, idx, rest, restLen) =>
          enumExportsNatFuel fuel remaining rest restLen
            ({ name := name, kind := kind, idx := idx } :: acc)
      | none => none

def enumExports (modBytes modLen : Nat) : Option (List ExportEntry) :=
  match findSectionPayload 0x07 modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          enumExportsNatFuel (count + 1) count rest restLen []
      | none => none
  | none => none

/-- `some none` means no start section; `some (some idx)` is the exact function
    index encoded by section 8.  A present payload must contain one u32 and no
    trailing bytes. -/
def startFuncIndex (modBytes modLen : Nat) : Option (Option Nat) :=
  match findSectionPayload 0x08 modBytes modLen with
  | none => some none
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (idx, _, 0) => some (some idx)
      | _ => none

def findExportFuncIndexNatFuel (targetName : ByteSeq) :
    Nat → Nat → Nat → Nat → Option Nat → Option Nat
  | 0, _, _, _, _ => none
  | _fuel + 1, 0, _bytes, len, found =>
      if len = 0 then found else none
  | fuel + 1, remaining + 1, bytes, len, found =>
      match readExportEntryNat bytes len with
      | some (name, kind, idx, rest, restLen) =>
          let found' :=
            if kind = 0x00 ∧ name = targetName ∧ found.isNone then some idx else found
          findExportFuncIndexNatFuel targetName fuel remaining rest restLen found'
      | none => none

def exportFuncIndex (modBytes modLen : Nat) (targetName : ByteSeq) : Option Nat :=
  match findSectionPayload 0x07 modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          findExportFuncIndexNatFuel targetName (count + 1) count rest restLen none
      | none => none
  | none => none

def codeEntryByCodeIndexNatFuel (target : Nat) :
    Nat → Nat → Nat → Nat → Nat → Option ByteSeq → Option ByteSeq
  | 0, _, _, _, _, _ => none
  | _fuel + 1, 0, _current, _bytes, len, found =>
      if len = 0 then found else none
  | fuel + 1, remaining + 1, current, bytes, len, found =>
      match readNatUleb32 bytes len with
      | some (size, body, bodyAndRestLen) =>
          if size ≤ bodyAndRestLen then
            let prefixLen := len - bodyAndRestLen
            let found' :=
              if current = target then some (takeNatBytes (prefixLen + size) bytes) else found
            codeEntryByCodeIndexNatFuel target fuel remaining (current + 1)
              (body >>> (8 * size)) (bodyAndRestLen - size) found'
          else
            none
      | none => none

def codeEntryByCodeIndex (modBytes modLen codeIdx : Nat) : Option ByteSeq :=
  match findSectionPayload 0x0a modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          if codeIdx < count then
            codeEntryByCodeIndexNatFuel codeIdx (count + 1) count 0 rest restLen none
          else
            none
      | none => none
  | none => none

def typeIndexByCodeIndexNatFuel (target : Nat) :
    Nat → Nat → Nat → Nat → Nat → Option Nat → Option Nat
  | 0, _, _, _, _, _ => none
  | _fuel + 1, 0, _current, _bytes, len, found =>
      if len = 0 then found else none
  | fuel + 1, remaining + 1, current, bytes, len, found =>
      match readNatUleb32 bytes len with
      | some (typeIdx, rest, restLen) =>
          let found' := if current = target then some typeIdx else found
          typeIndexByCodeIndexNatFuel target fuel remaining (current + 1)
            rest restLen found'
      | none => none

def typeIndexByCodeIndex (modBytes modLen codeIdx : Nat) : Option Nat :=
  match findSectionPayload 0x03 modBytes modLen with
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          if codeIdx < count then
            typeIndexByCodeIndexNatFuel codeIdx (count + 1) count 0 rest restLen none
          else
            none
      | none => none
  | none => none

def codeEntryByFuncIndex (modBytes modLen funcIdx : Nat) : Option ByteSeq :=
  match importedFuncCount modBytes modLen with
  | some imported =>
      if imported ≤ funcIdx then
        codeEntryByCodeIndex modBytes modLen (funcIdx - imported)
      else
        none
  | none => none

def funcBindingByFuncIndex (modBytes modLen funcIdx : Nat) : Option FuncBinding :=
  match importedFuncCount modBytes modLen with
  | some imported =>
      if imported ≤ funcIdx then
        let codeIdx := funcIdx - imported
        match typeIndexByCodeIndex modBytes modLen codeIdx,
              codeEntryByCodeIndex modBytes modLen codeIdx with
        | some typeIdx, some codeEntry =>
            some { funcIdx := funcIdx, codeIdx := codeIdx,
                   typeIdx := typeIdx, codeEntry := codeEntry }
        | _, _ => none
      else
        none
  | none => none

def codeEntryForExport (modBytes modLen : Nat) (targetName : ByteSeq) : Option ByteSeq :=
  match exportFuncIndex modBytes modLen targetName with
  | some funcIdx => codeEntryByFuncIndex modBytes modLen funcIdx
  | none => none

def funcBindingForExport (modBytes modLen : Nat) (targetName : ByteSeq) : Option FuncBinding :=
  match exportFuncIndex modBytes modLen targetName with
  | some funcIdx => funcBindingByFuncIndex modBytes modLen funcIdx
  | none => none

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

def natSubset : List Nat → List Nat → Bool
  | xs, ys => indexedSubset xs ys

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
  match findSectionPayload 0x05 modBytes modLen with
  | none => true
  | some (payload, payloadLen) =>
      match readNatUleb32 payload payloadLen with
      | some (count, rest, restLen) =>
          match memoryLimitsUnshared count rest restLen with
          | some (_, 0) => true
          | _ => false
      | none => false

end AverCert.WasmSlice
