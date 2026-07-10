-- Lean-side relevant Wasm byte slicer for certificate artifacts.
--
-- This is intentionally not a full WebAssembly validator. It parses only the
-- module facts needed to bind an exported function name to its raw function
-- binding: header, import function count, function section type index, export
-- section and code section.
-- Unsupported import kinds decline.

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

/-- Walk the type section's rectype vector to the `target`-th TYPE INDEX
    (an explicit rec group defines several consecutive indices) and validate that
    entry with the supplied leaf `check`. Generic over the leaf predicate so the
    canonical carrier-signature check and the verbatim `eqref → ref` signature
    check share the same rec-group navigation. -/
def walkTypeEntriesFuel (check : ByteSeq → Bool) :
    Nat → Nat → Nat → ByteSeq → Bool
  | 0, _, _, _ => false
  | _fuel + 1, 0, _target, _bytes => false
  | fuel + 1, remaining + 1, target, bytes =>
      match bytes with
      | 0x4e :: rest =>
          match readUleb32 rest with
          | some (cnt, r1) =>
              if target < cnt then
                match skipSubTypesFuel (target + 1) target r1 with
                | some r2 => check r2
                | none => false
              else
                match skipSubTypesFuel (cnt + 1) cnt r1 with
                | some r2 =>
                    walkTypeEntriesFuel check fuel remaining (target - cnt) r2
                | none => false
          | none => false
      | _ =>
          if target = 0 then
            check bytes
          else
            match skipSubType bytes with
            | some rest => walkTypeEntriesFuel check fuel remaining (target - 1) rest
            | none => false

/-- Whether the module's type-section entry `typeIdx` is exactly the canonical
    certified function type `[(ref null carrier)^arity] → [(ref null carrier)]`.
    This binds a claimed function binding's declared signature to the plan's
    params/result without trusting either. -/
def funcTypeMatches (wasmBytes : ByteSeq) (typeIdx arity carrier : Nat) : Bool :=
  match findSectionPayload 0x01 wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          walkTypeEntriesFuel (checkCanonicalFuncType arity carrier) (count + 1) count typeIdx rest
      | none => false
  | none => false

/-- Whether the head of `bytes` is EXACTLY the certified verbatim dispatch
    function type `[eqref] → [(ref null resultHeapTy)]` (or its non-null variant):
    one abstract `eq` reference parameter (`0x6d`) and one concrete reference
    result (`0x63`/`0x64` followed by the s33 heap index `resultHeapTy`). Subtype
    prefixes and every other shape fail-close. This pins a verbatim binding's
    declared signature to UNARY arity, the `eqref` domain the dispatch reads, and
    the plan's result heap type — none of which the code-entry bytes encode (a
    second parameter leaves the code entry byte-identical). -/
def checkVerbatimFuncType (resultHeapTy : Nat) (bytes : ByteSeq) : Bool :=
  match bytes with
  | 0x60 :: 0x01 :: 0x6d :: rest =>
      match readUleb32 rest with
      | some (nr, r1) =>
          if nr = 1 then
            match r1 with
            | b :: r2 =>
                if b = 0x63 ∨ b = 0x64 then
                  match readS33 r2 with
                  | some (idx, _) => idx == Int.ofNat resultHeapTy
                  | none => false
                else
                  false
            | [] => false
          else
            false
      | none => false
  | _ => false

/-- Whether the module's type-section entry `typeIdx` is exactly the certified
    verbatim dispatch signature `[eqref] → [(ref null resultHeapTy)]`. Binds a
    claimed verbatim binding's declared signature to unary arity, the `eqref`
    domain and the plan's result heap type without trusting any of them. -/
def verbatimFuncTypeMatches (wasmBytes : ByteSeq) (typeIdx resultHeapTy : Nat) : Bool :=
  match findSectionPayload 0x01 wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          walkTypeEntriesFuel (checkVerbatimFuncType resultHeapTy) (count + 1) count typeIdx rest
      | none => false
  | none => false

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

def dataSegmentBytesFuel : Nat → Nat → ByteSeq → Option ByteSeq
  | 0, _, _ => none
  | fuel + 1, idx, bytes =>
      match readPassiveDataSegment bytes with
      | some (contents, rest) =>
          if idx = 0 then some contents
          else dataSegmentBytesFuel fuel (idx - 1) rest
      | none => none

/-- The exact byte contents of passive data segment `dataIdx`, recovered from the
    module's data section (id 11). `none` if the section is absent, `dataIdx` is
    out of range, or any segment on the way is not passive. -/
def dataSegmentBytes (wasmBytes : ByteSeq) (dataIdx : Nat) : Option ByteSeq :=
  match findSectionPayload 0x0b wasmBytes with
  | some payload =>
      match readUleb32 payload with
      | some (count, rest) =>
          if dataIdx < count then dataSegmentBytesFuel (count + 1) dataIdx rest
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
