/-
  CertDecode — checker-owned, in-kernel decoder for the AverUserProfile/v1
  certificate profile. Given a wasm-gc module as a little-endian big-`Nat`
  plus a byte length, it decodes the profile-relevant sections into PURE DATA
  (`rfl`-comparable): a typed type-section IR, raw import/export indices, the
  Int carrier type index, exact data/code locations, and per-function `WCode`
  (arity from the type section, nlocals from the locals declaration, body as
  `List CertPrelude.WInstr` — the full 39-opcode fragment, if/else/end folded
  into `WInstr.ifElse`).

  Differential fixtures pin this decoder against the producer decoder and the
  standalone certkit reference implementation.

  Construction constraints (from the C2 kill-fast probes):
  * All recursion is STRUCTURAL or on an explicit fuel `Nat`. There is no
    well-founded recursion and no `partial def`, so every `decode…` reduces in
    the kernel and a certificate witness closes by `rfl` (axioms `[propext]`).
  * The byte representation is one little-endian big-`Nat` + a length. Reading a
    byte is `n &&& 0xff`, advancing is `n >>> 8`, and skipping a section body of
    `size` bytes is a single GMP-accelerated shift `n >>> (8 * size)`.

  Scope is PROFILE-ONLY and REJECT-BY-DEFAULT. The type-section subset decoded
  is exactly what the Aver wasm-gc compiler emits and the fixtures exercise:
  rec groups, optional `sub`/`sub final` prefixes, `func`/`struct`/`array`
  composite types, numeric valtypes (0x7b–0x7f), reference valtypes
  (0x63/0x64 + heaptype), and the abstract-heap valtype shorthands (0x6a–0x73,
  e.g. `eqref`). Anything outside this subset (a valtype/opcode the emitter does
  not produce, an overlong LEB, a truncated section) decodes to `none` — never a
  garbage success. `moduleFramingValid` separately guards the entire module;
  prefix-preserving section projections remain useful for isolated negative
  tests, while whole-artifact admission requires strict global framing.
-/
import CertPrelude
open CertPrelude

namespace CertDecode

/-- ASCII/latin1 name bytes → `String` (defeq to the string literal under `rfl`,
    so the certified export names are anchored, not weakened to byte lists). -/
def mkName (ns : List Nat) : String := String.ofList (ns.map (fun n => Char.ofNat n))

/-- Peel `count` bytes off the little-endian big-`Nat` into a `List Nat`. -/
def takeBytes : Nat → Nat → List Nat
  | 0,   _ => []
  | k+1, n => (n &&& 0xff) :: takeBytes k (n >>> 8)

/- ===================================================================== -/
/-  LEB128                                                                -/
/- ===================================================================== -/

/-- Canonical unsigned LEB128 over `(n, len)`; overlong trailing-zero → none. -/
def uleb : Nat → Nat → Nat → Nat → Nat → Option (Nat × Nat × Nat)
  | 0,      _,   _,     _, _   => none
  | fuel+1, acc, shift, n, len =>
      if len == 0 then none else
        let b   := n &&& 0xff
        let n'  := n >>> 8
        let acc' := acc + ((b &&& 0x7f) <<< shift)
        if b < 128 then
          if (shift != 0) && (b == 0) then none else some (acc', n', len-1)
        else uleb fuel acc' (shift+7) n' (len-1)

@[inline] def readU (n len : Nat) : Option (Nat × Nat × Nat) := uleb 5 0 0 n len

/-- Signed LEB128 (i64/i32 const immediates, s33 heaptypes / typeidx). -/
def sleb : Nat → Int → Nat → Nat → Nat → Option (Int × Nat × Nat)
  | 0,      _,   _,     _, _   => none
  | fuel+1, acc, shift, n, len =>
      if len == 0 then none else
        let b   := n &&& 0xff
        let n'  := n >>> 8
        let acc' := acc + (Int.ofNat (b &&& 0x7f)) * ((2 : Int) ^ shift)
        if b < 128 then
          let signed := if (b &&& 0x40) != 0 then acc' - ((2 : Int) ^ (shift+7)) else acc'
          some (signed, n', len-1)
        else sleb fuel acc' (shift+7) n' (len-1)

@[inline] def readS (n len : Nat) : Option (Int × Nat × Nat) := sleb 10 0 0 n len

/-- Spec-bounded signed heap type: at most five bytes and in s33 range. -/
def readS33 (n len : Nat) : Option (Int × Nat × Nat) :=
  match sleb 5 0 0 n len with
  | some (v, n1, len1) =>
      if -(Int.ofNat (2 ^ 32)) ≤ v ∧ v < Int.ofNat (2 ^ 32) then
        some (v, n1, len1)
      else none
  | none => none

/- ===================================================================== -/
/-  Section walk                                                          -/
/- ===================================================================== -/

/-- Isolate exactly `len` low bytes. Besides enforcing the declared section
    boundary, this keeps every subsequent byte shift proportional to the
    payload being decoded instead of to the remaining module suffix. -/
def isolateBytes (n len : Nat) : Nat :=
  n &&& ((1 <<< (8 * len)) - 1)

/-- A bounded section table. Payloads are stored as exact low-byte slices, so
    no consumer retains or shifts the enclosing module suffix. -/
structure ModuleView where
  sections : List (Nat × Nat × Nat)

def sectionTable : Nat → Nat → Nat → Option (List (Nat × Nat × Nat))
  | 0,      _, _   => none
  | fuel+1, n, len =>
      if len == 0 then some [] else
        let id := n &&& 0xff
        let n1 := n >>> 8
        match readU n1 (len - 1) with
        | none => some []
        | some (size, n2, len2) =>
            if size ≤ len2 then
              match sectionTable fuel (n2 >>> (8 * size)) (len2 - size) with
              | none => none
              | some rest => some ((id, isolateBytes n2 size, size) :: rest)
            else some []

def ModuleView.payload (view : ModuleView) (target : Nat) : Option (Nat × Nat) :=
  (view.sections.find? (fun entry => entry.1 == target)).map
    (fun entry => (entry.2.1, entry.2.2))

/-- Validate the header and materialize the bounded section-table prefix in one
    pass. A malformed frame terminates the table: already-complete earlier
    sections remain independently decodable, while that section and every
    later section are absent. The separate whole-module framing guard still
    rejects malformed modules globally. -/
def moduleView (modBytes modLen : Nat) : Option ModuleView :=
  if 8 ≤ modLen ∧ (modBytes &&& 0xffffffffffffffff) = 0x000000016d736100 then
    (sectionTable 64 (modBytes >>> 64) (modLen - 8)).map (fun sections => ⟨sections⟩)
  else none

/-- Strict whole-file framing, intentionally separate from prefix-decoding
    `moduleView`: every canonical section-size LEB must be in bounds and the
    declared module length must be exhausted within 64 section frames. -/
def sectionFramingValid : Nat → Nat → Nat → Bool
  | 0,      _, len => len == 0
  | fuel+1, n, len =>
      if len == 0 then true else
        match readU (n >>> 8) (len - 1) with
        | none => false
        | some (size, payload, payloadAndRestLen) =>
            size ≤ payloadAndRestLen &&
              sectionFramingValid fuel (payload >>> (8 * size))
                (payloadAndRestLen - size)

def moduleFramingValid (modBytes modLen : Nat) : Bool :=
  8 ≤ modLen &&
    (modBytes &&& 0xffffffffffffffff) == 0x000000016d736100 &&
    sectionFramingValid 64 (modBytes >>> 64) (modLen - 8)

/-- Header-validated exact payload of the first section with id `target`. All
    section decoders share this one framing implementation. -/
def modulePayload (target modBytes modLen : Nat) : Option (Nat × Nat) :=
  (moduleView modBytes modLen).bind (fun view => view.payload target)

/- ===================================================================== -/
/-  Type section: one exact typed IR shared by every checker projection     -/
/- ===================================================================== -/

/-- Exact wasm value-type form admitted by this certificate profile. Tags are
    retained because nullability (`0x63`/`0x64`) and abstract shorthands are
    byte-significant even when a downstream projection ignores the detail. -/
inductive ValType
  | numeric (tag : Nat)
  | abstract (tag : Nat)
  | ref (tag : Nat) (heap : Int)
deriving Repr, DecidableEq

inductive StorageType
  | packed (tag : Nat)
  | val (type : ValType)
deriving Repr, DecidableEq

structure FieldType where
  storage : StorageType
  mutability : Nat
deriving Repr, DecidableEq

inductive CompositeType
  | funcType (params results : List ValType)
  | structType (fields : List FieldType)
  | arrayType (field : FieldType)
deriving Repr, DecidableEq

inductive SubtypeForm
  | plain
  | sub (supertypes : List Nat)
  | subFinal (supertypes : List Nat)
deriving Repr, DecidableEq

/-- One flattened subtype in absolute type-index order. -/
structure TypeEntry where
  form : SubtypeForm
  composite : CompositeType
deriving Repr, DecidableEq

/-- One strict type-section pass plus compatibility summaries used by WCode. -/
structure TypeInfo where
  nfields : List Nat
  arityIndex : Array Nat
  nfieldIndex : Array Nat
  carrier : Option Nat
  entries : List TypeEntry
  entryIndex : Array TypeEntry

def readValType (n len : Nat) : Option (ValType × Nat × Nat) :=
  if len == 0 then none else
    let tag := n &&& 0xff
    let n1 := n >>> 8
    let len1 := len - 1
    if tag == 0x7f || tag == 0x7e || tag == 0x7d || tag == 0x7c || tag == 0x7b then
      some (.numeric tag, n1, len1)
    else if tag == 0x63 || tag == 0x64 then
      match readS33 n1 len1 with
      | some (heap, n2, len2) => some (.ref tag heap, n2, len2)
      | none => none
    else if decide (0x6a ≤ tag ∧ tag ≤ 0x73) then
      some (.abstract tag, n1, len1)
    else none

def readValTypes : Nat → Nat → Nat → Option (List ValType × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readValType n len with
      | none => none
      | some (type, n1, len1) =>
          match readValTypes k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (type :: rest, n2, len2)

def readStorageType (n len : Nat) : Option (StorageType × Nat × Nat) :=
  if len == 0 then none else
    let tag := n &&& 0xff
    if tag == 0x78 || tag == 0x77 then some (.packed tag, n >>> 8, len - 1)
    else (readValType n len).map (fun p => (.val p.1, p.2.1, p.2.2))

def readField (n len : Nat) : Option (FieldType × Nat × Nat) :=
  match readStorageType n len with
  | none => none
  | some (storage, n1, len1) =>
      if len1 == 0 then none else
        let mutability := n1 &&& 0xff
        if mutability == 0 || mutability == 1 then
          some (⟨storage, mutability⟩, n1 >>> 8, len1 - 1)
        else none

def readFields : Nat → Nat → Nat → Option (List FieldType × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readField n len with
      | none => none
      | some (field, n1, len1) =>
          match readFields k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (field :: rest, n2, len2)

def readUlebs : Nat → Nat → Nat → Option (List Nat × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readU n len with
      | none => none
      | some (value, n1, len1) =>
          match readUlebs k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (value :: rest, n2, len2)

/-- Cursor-only compatibility projection used by instruction scanners. -/
def skipUlebs (k n len : Nat) : Option (Nat × Nat) :=
  (readUlebs k n len).map (fun p => (p.2.1, p.2.2))

def readCompositeType (n len : Nat) : Option (CompositeType × Nat × Nat) :=
  if len == 0 then none else
    let tag := n &&& 0xff
    let n1 := n >>> 8
    let len1 := len - 1
    if tag == 0x60 then
      match readU n1 len1 with
      | none => none
      | some (np, n2, len2) =>
          match readValTypes np n2 len2 with
          | none => none
          | some (params, n3, len3) =>
              match readU n3 len3 with
              | none => none
              | some (nr, n4, len4) =>
                  match readValTypes nr n4 len4 with
                  | some (results, n5, len5) => some (.funcType params results, n5, len5)
                  | none => none
    else if tag == 0x5f then
      match readU n1 len1 with
      | none => none
      | some (nf, n2, len2) =>
          match readFields nf n2 len2 with
          | some (fields, n3, len3) => some (.structType fields, n3, len3)
          | none => none
    else if tag == 0x5e then
      match readField n1 len1 with
      | some (field, n2, len2) => some (.arrayType field, n2, len2)
      | none => none
    else none

def readSubtypeForm (n len : Nat) : Option (SubtypeForm × Nat × Nat) :=
  if len == 0 then none else
    let tag := n &&& 0xff
    if tag == 0x50 || tag == 0x4f then
      match readU (n >>> 8) (len - 1) with
      | none => none
      | some (count, n1, len1) =>
          match readUlebs count n1 len1 with
          | none => none
          | some (supertypes, n2, len2) =>
              if tag == 0x50 then some (.sub supertypes, n2, len2)
              else some (.subFinal supertypes, n2, len2)
    else some (.plain, n, len)

def readTypeEntry (n len : Nat) : Option (TypeEntry × Nat × Nat) :=
  match readSubtypeForm n len with
  | none => none
  | some (form, n1, len1) =>
      match readCompositeType n1 len1 with
      | none => none
      | some (composite, n2, len2) =>
          some (⟨form, composite⟩, n2, len2)

def readTypeEntries : Nat → Nat → Nat → Option (List TypeEntry × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readTypeEntry n len with
      | none => none
      | some (entry, n1, len1) =>
          match readTypeEntries k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (entry :: rest, n2, len2)

/-- Decode the declared rectype vector, flattening explicit rec groups into one
    absolute type-index order. This is the only type grammar parser. -/
def decRecVec : Nat → Nat → Nat → Option (List TypeEntry × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      if len == 0 then none else
        if (n &&& 0xff) == 0x4e then
          match readU (n >>> 8) (len - 1) with
          | none => none
          | some (count, n1, len1) =>
              match readTypeEntries count n1 len1 with
              | none => none
              | some (group, n2, len2) =>
                  match decRecVec k n2 len2 with
                  | none => none
                  | some (rest, n3, len3) => some (group ++ rest, n3, len3)
        else
          match readTypeEntry n len with
          | none => none
          | some (entry, n1, len1) =>
              match decRecVec k n1 len1 with
              | none => none
              | some (rest, n2, len2) => some (entry :: rest, n2, len2)

def StorageType.tag : StorageType → Nat
  | .packed tag => tag
  | .val (.numeric tag) => tag
  | .val (.abstract tag) => tag
  | .val (.ref tag _) => tag

def TypeEntry.arity (entry : TypeEntry) : Nat :=
  match entry.composite with
  | .funcType params _ => params.length
  | _ => 0

def TypeEntry.fieldCount (entry : TypeEntry) : Nat :=
  match entry.composite with
  | .structType fields => fields.length
  | _ => 0

def TypeEntry.isCarrier (entry : TypeEntry) : Bool :=
  match entry.composite with
  | .structType fields =>
      fields.length == 3 && (fields[0]?).map (fun f => f.storage.tag) == some 0x7e &&
        (fields[2]?).map (fun f => f.storage.tag) == some 0x7f
  | _ => false

def firstCarrier : Nat → List TypeEntry → Option Nat
  | _,   [] => none
  | idx, entry :: rest =>
      if entry.isCarrier then some idx else firstCarrier (idx + 1) rest

def decodeTypes (n len : Nat) : Option TypeInfo :=
  match modulePayload 1 n len with
  | none => none
  | some (tN, tLen) =>
      match readU tN tLen with
      | none => none
      | some (count, n1, len1) =>
          match decRecVec count n1 len1 with
          | some (entries, _, 0) =>
              let arities := entries.map TypeEntry.arity
              let nfields := entries.map TypeEntry.fieldCount
              some { nfields := nfields
                   , arityIndex := arities.toArray
                   , nfieldIndex := nfields.toArray
                   , carrier := firstCarrier 0 entries
                   , entries := entries
                   , entryIndex := entries.toArray }
          | _ => none

/-- The Int carrier struct type index. -/
def decodeCarrier (n len : Nat) : Option Nat :=
  match decodeTypes n len with
  | some ti => ti.carrier
  | none => none

/-- Strict THREE-state Int-carrier decode, the presence counterpart of
    `decodeCarrier`:

    * `none` — the type section does not decode at all. No declaration is
      admissible; absence is never certified from bytes that cannot be read.
    * `some none` — the type section decodes strictly and holds no Int-carrier
      struct. This is the byte-proved carrierless state.
    * `some (some idx)` — the type section decodes strictly and the first
      Int-carrier struct sits at type index `idx`.

    `decodeCarrier` collapses the first two states onto `none`, so a conjunct
    written against it can only ever pin a PRESENT carrier: it fails closed on a
    carrierless module and on an unreadable one alike, and can distinguish
    neither. Every acceptance conjunct that must ADMIT a carrierless module
    reads this decoder instead, which keeps the unreadable case rejected. -/
def carrierState (n len : Nat) : Option (Option Nat) :=
  match decodeTypes n len with
  | some ti => some ti.carrier
  | none => none

/-- The decoded struct-field count at one type index. Non-struct type entries
    have count zero; an out-of-range index or malformed type section is
    rejected. -/
def decodeStructFieldCount (n len typeidx : Nat) : Option Nat :=
  match decodeTypes n len with
  | some ti => ti.nfieldIndex[typeidx]?
  | none => none

/- ===================================================================== -/
/-  Function / import / export / data sections                            -/
/- ===================================================================== -/

/-- Read `k` type indices (function section entries). -/
def decFuncVec : Nat → Nat → Nat → Option (List Nat × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readU n len with
      | none => none
      | some (t, n1, len1) =>
          match decFuncVec k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (t :: rest, n2, len2)

/-- Per-defined-function type index (function section). -/
def decodeFuncTypes (n len : Nat) : Option (List Nat) :=
  match modulePayload 3 n len with
  | none => some []
  | some (fN, fLen) =>
      match readU fN fLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decFuncVec cnt n1 len1 with
          | none => none
          | some (ts, _, 0) => some ts
          | some _ => none

/-! The raw module-indexing records below retain descriptor kinds and byte-exact
    names. Profile-specific APIs are projections of this one strict parse. -/

structure ImportEntry where
  moduleName : List Nat
  fieldName : List Nat
  kind : Nat
  funcTypeIdx : Option Nat
deriving Repr, DecidableEq

structure ExportEntry where
  name : List Nat
  kind : Nat
  idx : Nat
deriving Repr, DecidableEq

/-- A bounded raw wasm name. -/
def readName (n len : Nat) : Option (List Nat × Nat × Nat) :=
  match readU n len with
  | none => none
  | some (nameLen, bytes, bytesLen) =>
      if nameLen ≤ bytesLen then
        some (takeBytes nameLen bytes, bytes >>> (8 * nameLen), bytesLen - nameLen)
      else none

/-- Skip exactly one import-descriptor value type. Import descriptors admit
    the `0x69` abstract shorthand in addition to the profile type-section IR. -/
def skipImportValType (n len : Nat) : Option (Nat × Nat) :=
  if len != 0 && (n &&& 0xff) == 0x69 then some (n >>> 8, len - 1)
  else (readValType n len).map (fun p => (p.2.1, p.2.2))

/-- Skip one table/memory limits descriptor. This is the exact descriptor
    subset admitted by the certificate wasm slicer. -/
def skipLimits (n len : Nat) : Option (Nat × Nat) :=
  match readU n len with
  | none => none
  | some (flags, n1, len1) =>
      if flags < 16 then
        match readU n1 len1 with
        | none => none
        | some (_, n2, len2) =>
            let afterMax :=
              if (flags &&& 0x01) != 0 then readU n2 len2 else some (0, n2, len2)
            match afterMax with
            | none => none
            | some (_, n3, len3) =>
                if (flags &&& 0x08) != 0 then
                  match readU n3 len3 with
                  | some (_, n4, len4) => some (n4, len4)
                  | none => none
                else some (n3, len3)
      else none

/-- Parse one import descriptor. Function imports retain their type index;
    table, memory, global, and tag imports are validated but store `none`. -/
def readImportDesc (n len : Nat) : Option (Nat × Option Nat × Nat × Nat) :=
  if len == 0 then none else
    let kind := n &&& 0xff
    let rest := n >>> 8
    let restLen := len - 1
    if kind == 0x00 then
      match readU rest restLen with
      | some (typeIdx, tail, tailLen) => some (kind, some typeIdx, tail, tailLen)
      | none => none
    else if kind == 0x01 then
      match skipImportValType rest restLen with
      | some (limits, limitsLen) =>
          (skipLimits limits limitsLen).map (fun p => (kind, none, p.1, p.2))
      | none => none
    else if kind == 0x02 then
      (skipLimits rest restLen).map (fun p => (kind, none, p.1, p.2))
    else if kind == 0x03 then
      match skipImportValType rest restLen with
      | some (mutability, mutabilityLen) =>
          if mutabilityLen == 0 then none else
            let m := mutability &&& 0xff
            if m == 0 || m == 1 then
              some (kind, none, mutability >>> 8, mutabilityLen - 1)
            else none
      | none => none
    else if kind == 0x04 then
      if restLen == 0 || (rest &&& 0xff) != 0 then none else
        match readU (rest >>> 8) (restLen - 1) with
        | some (_, tail, tailLen) => some (kind, none, tail, tailLen)
        | none => none
    else none

def readImportEntry (n len : Nat) : Option (ImportEntry × Nat × Nat) :=
  match readName n len with
  | none => none
  | some (moduleName, n1, len1) =>
      match readName n1 len1 with
      | none => none
      | some (fieldName, n2, len2) =>
          match readImportDesc n2 len2 with
          | none => none
          | some (kind, typeIdx, n3, len3) =>
              some (⟨moduleName, fieldName, kind, typeIdx⟩, n3, len3)

def decRawImportVec : Nat → Nat → Nat → Option (List ImportEntry × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readImportEntry n len with
      | none => none
      | some (entry, n1, len1) =>
          match decRawImportVec k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (entry :: rest, n2, len2)

/-- Byte-exact import table. The declared vector must exhaust section 2. -/
def decodeRawImports (n len : Nat) : Option (List ImportEntry) :=
  match modulePayload 2 n len with
  | none => some []
  | some (iN, iLen) =>
      match readU iN iLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decRawImportVec cnt n1 len1 with
          | some (entries, _, 0) => some entries
          | _ => none

def functionImports : List ImportEntry → Option (List (String × String))
  | [] => some []
  | entry :: rest =>
      if entry.kind == 0 && entry.funcTypeIdx.isSome then
        (functionImports rest).map
          (fun tail => (mkName entry.moduleName, mkName entry.fieldName) :: tail)
      else none

/-- Imported function names. Any valid non-function import still declines this
    profile projection, preserving the original fail-closed contract. -/
def decodeImports (n len : Nat) : Option (List (String × String)) :=
  (decodeRawImports n len).bind functionImports

/-- Number of imported functions = the base offset for defined function indices. -/
def funcImportBase (n len : Nat) : Option Nat :=
  (decodeImports n len).map List.length

def readExportEntry (n len : Nat) : Option (ExportEntry × Nat × Nat) :=
  match readName n len with
  | none => none
  | some (name, n1, len1) =>
      if len1 == 0 then none else
        let kind := n1 &&& 0xff
        if kind > 4 then none else
          match readU (n1 >>> 8) (len1 - 1) with
          | none => none
          | some (idx, n2, len2) => some (⟨name, kind, idx⟩, n2, len2)

def decRawExportVec : Nat → Nat → Nat → Option (List ExportEntry × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readExportEntry n len with
      | none => none
      | some (entry, n1, len1) =>
          match decRawExportVec k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (entry :: rest, n2, len2)

/-- Byte-exact export table. The declared vector must exhaust section 7. -/
def decodeRawExports (n len : Nat) : Option (List ExportEntry) :=
  match modulePayload 7 n len with
  | none => none
  | some (eN, eLen) =>
      match readU eN eLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decRawExportVec cnt n1 len1 with
          | some (entries, _, 0) => some entries
          | _ => none

def functionExports : List ExportEntry → List (String × Nat)
  | [] => []
  | entry :: rest =>
      if entry.kind == 0 then (mkName entry.name, entry.idx) :: functionExports rest
      else functionExports rest

/-- Function-export compatibility projection of `decodeRawExports`. -/
def decodeExports (n len : Nat) : Option (List (String × Nat)) :=
  (decodeRawExports n len).map functionExports

/-- `some none` means section 8 is absent; a present section must contain one
    canonical u32 and no trailing bytes. -/
def decodeStart (n len : Nat) : Option (Option Nat) :=
  match modulePayload 8 n len with
  | none => some none
  | some (sN, sLen) =>
      match readU sN sLen with
      | some (idx, _, 0) => some (some idx)
      | _ => none

/-- Read `k` (passive) data segments as byte lists. Non-passive flags are
    outside the profile → reject (fail-closed). -/
def decDataVec : Nat → Nat → Nat → Option (List (List Nat) × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      if len == 0 then none else
        let flag := n &&& 0xff
        let n1 := n >>> 8
        let len1 := len - 1
        if flag == 0x01 then
          match readU n1 len1 with              -- byte count
          | none => none
          | some (bc, n2, len2) =>
              if bc ≤ len2 then
                let bytes := takeBytes bc n2
                let n3 := n2 >>> (8*bc)
                let len3 := len2 - bc
                match decDataVec k n3 len3 with
                | none => none
                | some (rest, n4, len4) => some (bytes :: rest, n4, len4)
              else none
        else none

/-- Data segment payloads by segment index. Absent data section → no segments. -/
def decodeData (n len : Nat) : Option (List (List Nat)) :=
  match modulePayload 11 n len with
  | none => some []
  | some (dN, dLen) =>
      match readU dN dLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decDataVec cnt n1 len1 with
          | none => none
          | some (segs, _, 0) => some segs
          | some _ => none

/- ===================================================================== -/
/-  Code section: locals, instructions, nested if/else                    -/
/- ===================================================================== -/

/-- Skip `g` local-declaration groups; return the total declared local count
    (handles wasm-gc ref valtypes 0x63/0x64 followed by a heaptype). -/
def decLocals : Nat → Nat → Nat → Option (Nat × Nat × Nat)
  | 0,   n, len => some (0, n, len)
  | g+1, n, len =>
      match readU n len with                      -- group count
      | none => none
      | some (c, n1, len1) =>
          if len1 == 0 then none else
            let t := n1 &&& 0xff
            let n2 := n1 >>> 8
            if t == 0x63 || t == 0x64 then
              match readS n2 (len1-1) with          -- heaptype
              | none => none
              | some (_, n3, len3) =>
                  match decLocals g n3 len3 with
                  | none => none
                  | some (m, n4, len4) => some (c + m, n4, len4)
            else
              match decLocals g n2 (len1-1) with
              | none => none
              | some (m, n4, len4) => some (c + m, n4, len4)

/-- Consume an `if` blocktype (empty / numeric / ref+heaptype / typeidx s33). -/
def skipBlockType (n len : Nat) : Option (Nat × Nat) :=
  if len == 0 then none else
    let b0 := n &&& 0xff
    if b0 == 0x40 then some (n >>> 8, len - 1)
    else if b0 == 0x7f || b0 == 0x7e || b0 == 0x7d || b0 == 0x7c || b0 == 0x7b then
      some (n >>> 8, len - 1)
    else if b0 == 0x63 || b0 == 0x64 then
      match readS (n >>> 8) (len - 1) with
      | none => none
      | some (_, n1, len1) => some (n1, len1)
    else
      match readS n len with                       -- typeidx s33 blocktype
      | none => none
      | some (_, n1, len1) => some (n1, len1)

/-- Decode one non-structured instruction. `pending` holds i32 const values
    most-recent-first, so `array.new_data` resolves its (offset, length). -/
def decInstr (nfields : List Nat) (segs : List (List Nat)) (pending : List Int)
    (op n len : Nat) : Option (WInstr × List Int × Nat × Nat) :=
  if op == 0x20 then (readU n len).map (fun p => (WInstr.localGet p.1, pending, p.2.1, p.2.2))
  else if op == 0x21 then (readU n len).map (fun p => (WInstr.localSet p.1, pending, p.2.1, p.2.2))
  else if op == 0x42 then (readS n len).map (fun p => (WInstr.i64Const p.1, pending, p.2.1, p.2.2))
  else if op == 0x41 then (readS n len).map (fun p => (WInstr.i32Const p.1, p.1 :: pending, p.2.1, p.2.2))
  else if op == 0x44 then
    if len < 8 then none
    else some (WInstr.f64Const (UInt64.ofNat (n &&& 0xffffffffffffffff)), pending, n >>> 64, len - 8)
  else if op == 0xd0 then (readS n len).map (fun p => (WInstr.refNull, pending, p.2.1, p.2.2))
  else if op == 0xd1 then some (WInstr.refIsNull, pending, n, len)
  else if op == 0x10 then (readU n len).map (fun p => (WInstr.call p.1, pending, p.2.1, p.2.2))
  else if op == 0x12 then (readU n len).map (fun p => (WInstr.returnCall p.1, pending, p.2.1, p.2.2))
  else if op == 0x0f then some (WInstr.ret, pending, n, len)
  -- integer / float scalar ops (single byte, no immediate)
  else if op == 0x50 then some (WInstr.i64Eqz, pending, n, len)
  else if op == 0x51 then some (WInstr.i64Eq, pending, n, len)
  else if op == 0x57 then some (WInstr.i64LeS, pending, n, len)
  else if op == 0x53 then some (WInstr.i64LtS, pending, n, len)
  else if op == 0x59 then some (WInstr.i64GeS, pending, n, len)
  else if op == 0x55 then some (WInstr.i64GtS, pending, n, len)
  else if op == 0x46 then some (WInstr.i32Eq, pending, n, len)
  else if op == 0x71 then some (WInstr.i32And, pending, n, len)
  else if op == 0x48 then some (WInstr.i32LtS, pending, n, len)
  else if op == 0x4c then some (WInstr.i32LeS, pending, n, len)
  else if op == 0x4a then some (WInstr.i32GtS, pending, n, len)
  else if op == 0xa0 then some (WInstr.f64Add, pending, n, len)
  else if op == 0xa1 then some (WInstr.f64Sub, pending, n, len)
  else if op == 0xa2 then some (WInstr.f64Mul, pending, n, len)
  else if op == 0xa3 then some (WInstr.f64Div, pending, n, len)
  else if op == 0x61 then some (WInstr.f64Eq, pending, n, len)
  else if op == 0x63 then some (WInstr.f64Lt, pending, n, len)
  else if op == 0x65 then some (WInstr.f64Le, pending, n, len)
  else if op == 0x66 then some (WInstr.f64Ge, pending, n, len)
  else if op == 0x64 then some (WInstr.f64Gt, pending, n, len)
  -- wasm-gc prefixed opcodes (0xfb sub)
  else if op == 0xfb then
    match readU n len with
    | none => none
    | some (sub, n1, len1) =>
        if sub == 0x00 then                          -- struct.new
          (readU n1 len1).map (fun q =>
            (WInstr.structNew q.1 ((nfields[q.1]?).getD 0), pending, q.2.1, q.2.2))
        else if sub == 0x02 then                     -- struct.get
          match readU n1 len1 with
          | none => none
          | some (ty, n2, len2) =>
              (readU n2 len2).map (fun f => (WInstr.structGet ty f.1, pending, f.2.1, f.2.2))
        else if sub == 0x08 then                     -- array.new_fixed
          match readU n1 len1 with
          | none => none
          | some (ty, n2, len2) =>
              (readU n2 len2).map (fun m => (WInstr.arrayNewFixed ty m.1, pending, m.2.1, m.2.2))
        else if sub == 0x09 then                     -- array.new_data
          match readU n1 len1 with
          | none => none
          | some (ty, n2, len2) =>
              match readU n2 len2 with
              | none => none
              | some (seg, n3, len3) =>
                  let length := (pending.headD 0).toNat
                  let offset := ((pending.drop 1).headD 0).toNat
                  let payload := (segs[seg]?).getD []
                  let chunk := (payload.drop offset).take length
                  some (WInstr.arrayNewData ty chunk, pending, n3, len3)
        else if sub == 0x14 || sub == 0x15 then      -- ref.test
          (readS n1 len1).map (fun h => (WInstr.refTest h.1.toNat, pending, h.2.1, h.2.2))
        else if sub == 0x16 || sub == 0x17 then      -- ref.cast
          (readS n1 len1).map (fun h => (WInstr.refCast h.1.toNat, pending, h.2.1, h.2.2))
        else none
  else none

/-- Decode a block of instructions. Returns the folded instructions, the
    threaded `pending` list, the new `(n, len)`, and a terminator tag
    (0 = `end` 0x0b, 1 = `else` 0x05), both consumed. Fuel bounds the number of
    instructions (body byte size is a safe bound). -/
def decBlock (nfields : List Nat) (segs : List (List Nat)) :
    Nat → List Int → Nat → Nat → Option (List WInstr × List Int × Nat × Nat × Nat)
  | 0,      _,       _, _   => none
  | fuel+1, pending, n, len =>
      if len == 0 then none else
        let op := n &&& 0xff
        let n1 := n >>> 8
        let len1 := len - 1
        if op == 0x0b then some ([], pending, n1, len1, 0)
        else if op == 0x05 then some ([], pending, n1, len1, 1)
        else if op == 0x04 then
          match skipBlockType n1 len1 with
          | none => none
          | some (n2, len2) =>
              match decBlock nfields segs fuel pending n2 len2 with
              | none => none
              | some (thenB, pend1, n3, len3, t1) =>
                  if t1 == 1 then
                    match decBlock nfields segs fuel pend1 n3 len3 with
                    | none => none
                    | some (elseB, pend2, n4, len4, t2) =>
                        if t2 == 0 then
                          match decBlock nfields segs fuel pend2 n4 len4 with
                          | none => none
                          | some (rest, pend3, n5, len5, t3) =>
                              some (WInstr.ifElse thenB elseB :: rest, pend3, n5, len5, t3)
                        else none
                  else if t1 == 0 then
                    match decBlock nfields segs fuel pend1 n3 len3 with
                    | none => none
                    | some (rest, pend3, n5, len5, t3) =>
                        some (WInstr.ifElse thenB [] :: rest, pend3, n5, len5, t3)
                  else none
        else
          match decInstr nfields segs pending op n1 len1 with
          | none => none
          | some (ins, pending', n2, len2) =>
              match decBlock nfields segs fuel pending' n2 len2 with
              | none => none
              | some (rest, pend3, n3, len3, t3) => some (ins :: rest, pend3, n3, len3, t3)

/- ===================================================================== -/
/-  Per-function obligation decode                                        -/
/- ===================================================================== -/

structure CodeLoc where
  nlocals : Nat
  bodyN : Nat
  bodyLen : Nat
  /-- Exact raw code entry, including its size-LEB prefix. -/
  entryN : Nat
  entryLen : Nat

/-- One bounded code-section pass. Every body is isolated from both the next
    code entry and the enclosing section, and its final list position is its
    defined-function index. -/
def decCodeLocs : Nat → Nat → Nat → Option (List CodeLoc)
  | 0,   _, len => if len == 0 then some [] else none
  | k+1, n, len =>
      match readU n len with
      | none => none
      | some (esz, bN, bLen) =>
          if esz ≤ bLen then
            let entryBodyN := isolateBytes bN esz
            let sizePrefixLen := len - bLen
            let wholeEntryLen := sizePrefixLen + esz
            let wholeEntryN := isolateBytes n wholeEntryLen
            match readU entryBodyN esz with
            | none => none
            | some (ng, gN, gLen) =>
                match decLocals ng gN gLen with
                | none => none
                | some (nloc, bodyN, bodyLen) =>
                    match decCodeLocs k (bN >>> (8 * esz)) (bLen - esz) with
                    | none => none
                    | some rest =>
                        some (⟨nloc, isolateBytes bodyN bodyLen, bodyLen,
                          wholeEntryN, wholeEntryLen⟩ :: rest)
          else none

def codeLocs (n len : Nat) : Option (Array CodeLoc) :=
  match modulePayload 10 n len with
  | none => none
  | some (codeN, codeLen) =>
      match readU codeN codeLen with
      | none => none
      | some (nf, r0, l0) => (decCodeLocs nf r0 l0).map List.toArray

/-- Full `WCode` (arity from the type section, nlocals from the locals decl,
    body from the code section) for the module function `funcidx`. -/
def decodeCode (n len funcidx : Nat) : Option WCode :=
  match decodeTypes n len with
  | none => none
  | some ti =>
    match decodeData n len with
    | none => none
    | some segs =>
      match funcImportBase n len with
      | none => none
      | some nimp =>
        match decodeFuncTypes n len with
        | none => none
        | some ftys =>
          match ftys[funcidx - nimp]? with
          | none => none
          | some tyidx =>
            match ti.arityIndex[tyidx]? with
            | none => none
            | some ar =>
              match codeLocs n len with
              | none => none
              | some locs =>
                match locs[funcidx - nimp]? with
                | none => none
                | some loc =>
                  match decBlock ti.nfields segs loc.bodyLen [] loc.bodyN loc.bodyLen with
                  | none => none
                  | some (instrs, _, _, restLen, term) =>
                      if term == 0 && restLen == 0 then
                        some ⟨ar, loc.nlocals, instrs⟩
                      else none

/- ===================================================================== -/
/-  Plan-first add/mul/sub/box host-role table                           -/
/- ===================================================================== -/

namespace AddSub


def boxIdx (n len : Nat) : Option Nat :=
  match decodeExports n len with
  | none => none
  | some es => (es.find? (fun e => e.1 == "__rt_aint_from_i64")).map Prod.snd

/-- The `__aint_to_index` helper role, bound exactly like `box`: by its named
    runtime export. The helper's semantics stay an assumed runtime contract
    (`toIndexW` in the obligation denotation); the byte-derived index only
    prevents a claim from wiring the contract to an arbitrary function. -/
def toIndexIdx (n len : Nat) : Option Nat :=
  match decodeExports n len with
  | none => none
  | some es => (es.find? (fun e => e.1 == "__aint_to_index")).map Prod.snd

structure Roles where
  box : Option Nat
  add : Option Nat
  mul : Option Nat
  sub : Option Nat
  toIndex : Option Nat
  deriving DecidableEq, Repr

/-- Byte-derived proof that the module carries no Int-carrier box helper: the
    export section decodes strictly and no function export is named
    `__rt_aint_from_i64` — the same name the disassembler binds as the `box`
    role. A malformed or unreadable export section never certifies absence. -/
def carrierHelperAbsent (n len : Nat) : Bool :=
  match decodeExports n len with
  | none => false
  | some es => es.all (fun e => e.1 != "__rt_aint_from_i64")

end AddSub

/- ===================================================================== -/
/-  Byte-exact String.eq/String.concat host-role table                    -/
/- ===================================================================== -/

namespace StringHost

/-- F5-relevant value-type detail: i32, a concrete reference target, or a
    lumped kind that cannot satisfy either string-helper signature. -/
inductive Ty
  | i32
  | scalar
  | ref (idx : Nat)
  | abstractHeap
  deriving DecidableEq, Repr, BEq

abbrev Sig := List Ty × List Ty

def projectVal : ValType → Ty
  | .numeric tag => if tag == 0x7f then .i32 else .scalar
  | .ref _ heap => if heap ≥ 0 then .ref heap.toNat else .abstractHeap
  | .abstract _ => .abstractHeap

def decodeTypeSig (entry : TypeEntry) : Option Sig :=
  match entry.composite with
  | .funcType params results => some (params.map projectVal, results.map projectVal)
  | _ => none

/-- Exactly the packed-i8 array shape used as String byte storage. As before,
    either validated field mutability projects to the same array index. -/
def stringBytesIndex (tidx : Nat) (entry : TypeEntry) : Option Nat :=
  match entry.composite with
  | .arrayType ⟨.packed tag, _⟩ => if tag == 0x78 then some tidx else none
  | _ => none

@[inline] def consOpt (o : Option Nat) (l : List Nat) : List Nat :=
  match o with | some x => x :: l | none => l

def decodeTypeEntries : Nat → List TypeEntry → List (Option Sig) × List Nat
  | _,    [] => ([], [])
  | tidx, entry :: rest =>
      let decoded := decodeTypeEntries (tidx + 1) rest
      (decodeTypeSig entry :: decoded.1,
        consOpt (stringBytesIndex tidx entry) decoded.2)

/-- Function signatures and string-byte-array indices from one type pass. -/
def decodeTypeSigs (n len : Nat) :
    Option (Array (Option Sig) × List Nat) :=
  (CertDecode.decodeTypes n len).map (fun info =>
    let decoded := decodeTypeEntries 0 info.entries
    (decoded.1.toArray, decoded.2))

/-- Exact admitted host-operation vocabulary. Non-mapped but boundary-known
    opcodes are `other`, making exact template comparison decline. -/
inductive Op
  | localGet (i : Nat)
  | localSet (i : Nat)
  | i32Const (v : Int)
  | arrayLen
  | arrayGetU (t : Nat)
  | arrayGet (t : Nat)
  | arrayNewDefault (t : Nat)
  | arrayCopy (dst src : Nat)
  | i32Ne
  | i32GeU
  | i32Add
  | hIf
  | hBlock
  | hLoop
  | br (d : Nat)
  | brIf (d : Nat)
  | hReturn
  | hEnd
  | other
  deriving DecidableEq, Repr, BEq

/-- Decode one instruction and skip every immediate exactly. Unknown encodings
    fail closed; known non-template instructions become `other`. -/
def decodeOne (n len : Nat) : Option (Op × Nat × Nat) :=
  let op := n &&& 0xff
  let n1 := n >>> 8
  let len1 := len - 1
  if op == 0x20 then
    (CertDecode.readU n1 len1).map (fun p => (Op.localGet p.1, p.2.1, p.2.2))
  else if op == 0x21 then
    (CertDecode.readU n1 len1).map (fun p => (Op.localSet p.1, p.2.1, p.2.2))
  else if op == 0x41 then
    (CertDecode.readS n1 len1).map (fun p => (Op.i32Const p.1, p.2.1, p.2.2))
  else if op == 0x47 then some (Op.i32Ne, n1, len1)
  else if op == 0x4f then some (Op.i32GeU, n1, len1)
  else if op == 0x6a then some (Op.i32Add, n1, len1)
  else if op == 0x02 then
    (CertDecode.skipBlockType n1 len1).map (fun p => (Op.hBlock, p.1, p.2))
  else if op == 0x03 then
    (CertDecode.skipBlockType n1 len1).map (fun p => (Op.hLoop, p.1, p.2))
  else if op == 0x04 then
    (CertDecode.skipBlockType n1 len1).map (fun p => (Op.hIf, p.1, p.2))
  else if op == 0x0c then
    (CertDecode.readU n1 len1).map (fun p => (Op.br p.1, p.2.1, p.2.2))
  else if op == 0x0d then
    (CertDecode.readU n1 len1).map (fun p => (Op.brIf p.1, p.2.1, p.2.2))
  else if op == 0x0f then some (Op.hReturn, n1, len1)
  else if op == 0x0b then some (Op.hEnd, n1, len1)
  else if op == 0x00 || op == 0x01 || op == 0x05 || op == 0x1a ||
          op == 0x1b || op == 0xd1 then some (Op.other, n1, len1)
  else if decide (0x45 ≤ op ∧ op ≤ 0xc4) then some (Op.other, n1, len1)
  else if op == 0x22 || op == 0x23 || op == 0x24 || op == 0x10 || op == 0x12 then
    (CertDecode.readU n1 len1).map (fun p => (Op.other, p.2.1, p.2.2))
  else if op == 0x0e then
    match CertDecode.readU n1 len1 with
    | none => none
    | some (cnt, n2, len2) =>
        (CertDecode.skipUlebs (cnt+1) n2 len2).map (fun p => (Op.other, p.1, p.2))
  else if op == 0x11 then
    match CertDecode.readU n1 len1 with
    | none => none
    | some (_, n2, len2) =>
        (CertDecode.readU n2 len2).map (fun p => (Op.other, p.2.1, p.2.2))
  else if op == 0x42 then
    (CertDecode.readS n1 len1).map (fun p => (Op.other, p.2.1, p.2.2))
  else if op == 0x43 then
    if len1 < 4 then none else some (Op.other, n1 >>> 32, len1 - 4)
  else if op == 0x44 then
    if len1 < 8 then none else some (Op.other, n1 >>> 64, len1 - 8)
  else if op == 0xd0 then
    (CertDecode.readS n1 len1).map (fun p => (Op.other, p.2.1, p.2.2))
  else if op == 0xd2 then
    (CertDecode.readU n1 len1).map (fun p => (Op.other, p.2.1, p.2.2))
  else if op == 0xfb then
    match CertDecode.readU n1 len1 with
    | none => none
    | some (sub, n2, len2) =>
        if sub == 0x0f then some (Op.arrayLen, n2, len2)
        else if sub == 0x0b then
          (CertDecode.readU n2 len2).map (fun p => (Op.arrayGet p.1, p.2.1, p.2.2))
        else if sub == 0x0d then
          (CertDecode.readU n2 len2).map (fun p => (Op.arrayGetU p.1, p.2.1, p.2.2))
        else if sub == 0x07 then
          (CertDecode.readU n2 len2).map
            (fun p => (Op.arrayNewDefault p.1, p.2.1, p.2.2))
        else if sub == 0x11 then
          match CertDecode.readU n2 len2 with
          | none => none
          | some (dst, n3, len3) =>
              (CertDecode.readU n3 len3).map
                (fun p => (Op.arrayCopy dst p.1, p.2.1, p.2.2))
        else if sub == 0x00 || sub == 0x01 || sub == 0x06 ||
                sub == 0x0c || sub == 0x0e then
          (CertDecode.readU n2 len2).map (fun p => (Op.other, p.2.1, p.2.2))
        else if sub == 0x02 || sub == 0x05 || sub == 0x08 || sub == 0x09 then
          match CertDecode.readU n2 len2 with
          | none => none
          | some (_, n3, len3) =>
              (CertDecode.readU n3 len3).map (fun p => (Op.other, p.2.1, p.2.2))
        else if sub == 0x14 || sub == 0x15 || sub == 0x16 || sub == 0x17 then
          (CertDecode.readS n2 len2).map (fun p => (Op.other, p.2.1, p.2.2))
        else none
  else none

def scan : Nat → Nat → Nat → Option (List Op)
  | 0,      _, _   => none
  | fuel+1, n, len =>
      if len == 0 then some [] else
        match decodeOne n len with
        | none => none
        | some (op, n1, len1) =>
            match scan fuel n1 len1 with
            | none => none
            | some rest => some (op :: rest)

def eqTemplate (t : Nat) : List Op :=
  [.localGet 0, .arrayLen, .localGet 1, .arrayLen, .i32Ne, .hIf,
   .i32Const 0, .hReturn, .hEnd, .localGet 0, .arrayLen, .localSet 2,
   .i32Const 0, .localSet 3, .hBlock, .hLoop, .localGet 3, .localGet 2,
   .i32GeU, .brIf 1, .localGet 0, .localGet 3, .arrayGetU t, .localGet 1,
   .localGet 3, .arrayGetU t, .i32Ne, .hIf, .i32Const 0, .hReturn, .hEnd,
   .localGet 3, .i32Const 1, .i32Add, .localSet 3, .br 0, .hEnd, .hEnd,
   .i32Const 1, .hEnd]

def concatTemplate (c b : Nat) : List Op :=
  [.localGet 0, .arrayLen, .localSet 3, .i32Const 0, .localSet 1,
   .i32Const 0, .localSet 2, .hBlock, .hLoop, .localGet 2, .localGet 3,
   .i32GeU, .brIf 1, .localGet 1, .localGet 0, .localGet 2, .arrayGet c,
   .arrayLen, .i32Add, .localSet 1, .localGet 2, .i32Const 1, .i32Add,
   .localSet 2, .br 0, .hEnd, .hEnd, .localGet 1, .arrayNewDefault b,
   .localSet 6, .i32Const 0, .localSet 7, .i32Const 0, .localSet 2,
   .hBlock, .hLoop, .localGet 2, .localGet 3, .i32GeU, .brIf 1,
   .localGet 0, .localGet 2, .arrayGet c, .localSet 4, .localGet 4,
   .arrayLen, .localSet 5, .localGet 6, .localGet 7, .localGet 4,
   .i32Const 0, .localGet 5, .arrayCopy b b, .localGet 7, .localGet 5,
   .i32Add, .localSet 7, .localGet 2, .i32Const 1, .i32Add, .localSet 2,
   .br 0, .hEnd, .hEnd, .localGet 6, .hEnd]

inductive Role | eq | concat
  deriving DecidableEq, Repr, BEq

def eqCandidate : Option Sig → Nat → Option Nat
  | some (params, results), nloc =>
      match params, results with
      | [Ty.ref lhs, Ty.ref rhs], (Ty.i32 :: _) =>
          if lhs == rhs && nloc == 2 then some lhs else none
      | _, _ => none
  | none, _ => none

def concatCandidate : Option Sig → Nat → Option (Nat × Nat)
  | some (params, results), nloc =>
      match params, results with
      | [Ty.ref c], (Ty.ref b :: _) =>
          if nloc == 7 then some (c, b) else none
      | _, _ => none
  | none, _ => none

def classifyOne (sbat : List Nat) (sig : Option Sig)
    (nloc bodyN bodyLen : Nat) : Option Role :=
  match eqCandidate sig nloc with
  | some lhs =>
      if sbat.contains lhs then
        match scan (bodyLen+1) bodyN bodyLen with
        | some ops => if ops == eqTemplate lhs then some Role.eq else none
        | none => none
      else none
  | none =>
      match concatCandidate sig nloc with
      | some (c, b) =>
          if sbat.contains b then
            match scan (bodyLen+1) bodyN bodyLen with
            | some ops => if ops == concatTemplate c b then some Role.concat else none
            | none => none
          else none
      | none => none

def bodyLocs (n len : Nat) : Option (List (Nat × Nat × Nat)) :=
  (CertDecode.codeLocs n len).map (fun locs =>
    locs.toList.map (fun loc => (loc.nlocals, loc.bodyN, loc.bodyLen)))

/-- Classify each function independently. This intentionally has no uniqueness
    or cross-function decline: two exact eq helpers produce two entries. -/
def classify (nimp : Nat) (sbat : List Nat) (tsigs : Array (Option Sig)) :
    Nat → List Nat → List (Nat × Nat × Nat) → List (Nat × Role)
  | _,       [],        _  => []
  | _,       _ :: _,    [] => []
  | def_idx, ty :: tys, (nloc, bodyN, bodyLen) :: locs =>
      let sig := (tsigs[ty]?).getD none
      match classifyOne sbat sig nloc bodyN bodyLen with
      | some role =>
          (nimp + def_idx, role) :: classify nimp sbat tsigs (def_idx+1) tys locs
      | none => classify nimp sbat tsigs (def_idx+1) tys locs

/-- Decode-once F5 result: every String.eq/String.concat `(funcIdx, role)` in
    defined-function order. -/
def roleTable (n len : Nat) : Option (List (Nat × Role)) :=
  match decodeTypeSigs n len, CertDecode.decodeFuncTypes n len,
        CertDecode.funcImportBase n len, bodyLocs n len with
  | some (tsigs, sbat), some ftys, some nimp, some locs =>
      some (classify nimp sbat tsigs 0 ftys locs)
  | _, _, _, _ => none

end StringHost

end CertDecode
