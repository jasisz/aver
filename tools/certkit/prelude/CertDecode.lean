/-
  CertDecode — checker-owned, in-kernel decoder for the AverUserProfile/v0
  certificate profile. Given a wasm-gc module as a little-endian big-`Nat`
  plus a byte length, it decodes the profile-relevant sections into PURE DATA
  (`rfl`-comparable): the export map, the imported (module, name) list, the Int
  carrier type index, and per-function `WCode` (arity from the type section,
  nlocals from the locals declaration, body as `List CertPrelude.WInstr` — the
  full 39-opcode fragment, if/else/end folded into `WInstr.ifElse`).

  The result semantics mirror `src/codegen/cert/mod.rs::rederive_obligations`
  and `tools/certkit/extract.py` so all three decoders agree term for term on
  every certkit fixture (three-way differential).

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
  garbage success. Module admission is out of scope (that is a later item on the
  same decoder); the certificate claim is about obligations, and skipped
  sections are covered by the checker-side module hash.
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

/- ===================================================================== -/
/-  Section walk                                                          -/
/- ===================================================================== -/

/-- Whole-file section id sequence (skip is one GMP shift per section). Not on
    the targeted decode paths below, but pinned per fixture by the decoder
    differential (`tests/cert_decode_spec.rs`) so a malformed section frame
    anywhere in the module — including regions the targeted decoders skip —
    fails the kernel witness. -/
def walkIds : Nat → Nat → Nat → Option (List Nat)
  | 0,      _, _   => none
  | fuel+1, n, len =>
      if len == 0 then some [] else
        let id  := n &&& 0xff
        let n1  := n >>> 8
        match readU n1 (len-1) with
        | none => none
        | some (size, n2, len2) =>
            match walkIds fuel (n2 >>> (8*size)) (len2 - size) with
            | none => none
            | some tl => some (id :: tl)

/-- Body suffix `(n, len)` of the first section with id = target, else none. -/
def findSec (target : Nat) : Nat → Nat → Nat → Option (Nat × Nat)
  | 0,      _, _   => none
  | fuel+1, n, len =>
      if len == 0 then none else
        let id  := n &&& 0xff
        let n1  := n >>> 8
        match readU n1 (len-1) with
        | none => none
        | some (size, n2, len2) =>
            if id == target then some (n2, len2)
            else findSec target fuel (n2 >>> (8*size)) (len2 - size)

/- ===================================================================== -/
/-  Type section (arity per type index, struct field counts, carrier)     -/
/- ===================================================================== -/

/-- Per-type-index arities (0 for non-func), struct field counts (0 for
    non-struct), and the first Int-carrier struct index. -/
structure TypeInfo where
  arities : List Nat
  nfields : List Nat
  carrier : Option Nat

/-- Consume one valtype, returning its leading byte (for carrier detection). -/
def readValType (n len : Nat) : Option (Nat × Nat × Nat) :=
  if len == 0 then none else
    let v := n &&& 0xff
    let n1 := n >>> 8
    let len1 := len - 1
    if v == 0x7f || v == 0x7e || v == 0x7d || v == 0x7c || v == 0x7b then some (v, n1, len1)
    else if v == 0x63 || v == 0x64 then
      match readS n1 len1 with            -- heaptype (s33)
      | none => none
      | some (_, n2, len2) => some (v, n2, len2)
    else if decide (0x6a ≤ v ∧ v ≤ 0x73) then some (v, n1, len1)  -- abstract-heap shorthand
    else none

/-- Consume one storage type (packed i8/i16, or a valtype). -/
def readStorageType (n len : Nat) : Option (Nat × Nat × Nat) :=
  if len == 0 then none else
    let v := n &&& 0xff
    if v == 0x78 || v == 0x77 then some (v, n >>> 8, len - 1)
    else readValType n len

/-- Consume one struct/array field (storage type + mut byte); return leading
    byte of the storage type. -/
def readField (n len : Nat) : Option (Nat × Nat × Nat) :=
  match readStorageType n len with
  | none => none
  | some (sfb, n1, len1) =>
      if len1 == 0 then none else some (sfb, n1 >>> 8, len1 - 1)   -- drop mut byte

/-- Read `k` fields, collecting their leading storage bytes. -/
def readFields : Nat → Nat → Nat → Option (List Nat × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readField n len with
      | none => none
      | some (sfb, n1, len1) =>
          match readFields k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (sfb :: rest, n2, len2)

/-- Skip `p` valtypes (func params/results). -/
def skipValTypes : Nat → Nat → Nat → Option (Nat × Nat)
  | 0,   n, len => some (n, len)
  | p+1, n, len =>
      match readValType n len with
      | none => none
      | some (_, n1, len1) => skipValTypes p n1 len1

/-- Skip `s` unsigned LEB values (e.g. a vec of supertype type indices). -/
def skipUlebs : Nat → Nat → Nat → Option (Nat × Nat)
  | 0,   n, len => some (n, len)
  | s+1, n, len =>
      match readU n len with
      | none => none
      | some (_, n1, len1) => skipUlebs s n1 len1

/-- Decode one composite type at absolute type index `tidx`. -/
def decComptype (tidx n len : Nat) : Option (Nat × Nat × Option Nat × Nat × Nat) :=
  if len == 0 then none else
    let c := n &&& 0xff
    let n1 := n >>> 8
    let len1 := len - 1
    if c == 0x60 then            -- func
      match readU n1 len1 with
      | none => none
      | some (np, n2, len2) =>
          match skipValTypes np n2 len2 with
          | none => none
          | some (n3, len3) =>
              match readU n3 len3 with
              | none => none
              | some (nr, n4, len4) =>
                  match skipValTypes nr n4 len4 with
                  | none => none
                  | some (n5, len5) => some (np, 0, none, n5, len5)
    else if c == 0x5f then       -- struct
      match readU n1 len1 with
      | none => none
      | some (nf, n2, len2) =>
          match readFields nf n2 len2 with
          | none => none
          | some (fbs, n3, len3) =>
              let isCarrier :=
                nf == 3 && (fbs[0]?).getD 0 == 0x7e && (fbs[2]?).getD 0 == 0x7f
              some (0, nf, (if isCarrier then some tidx else none), n3, len3)
    else if c == 0x5e then       -- array
      match readField n1 len1 with
      | none => none
      | some (_, n2, len2) => some (0, 0, none, n2, len2)
    else none

/-- Decode one subtype at absolute type index `tidx`: its func arity (0 if not
    func), struct field count (0 if not struct), and whether it is the carrier.
    Handles an optional `sub` / `sub final` prefix (0x50 / 0x4f). -/
def decOneSubtype (tidx n len : Nat) : Option (Nat × Nat × Option Nat × Nat × Nat) :=
  if len == 0 then none else
    let b0 := n &&& 0xff
    if b0 == 0x50 || b0 == 0x4f then
      match readU (n >>> 8) (len - 1) with        -- vec of supertype typeidx
      | none => none
      | some (nsup, n1, len1) =>
          match skipUlebs nsup n1 len1 with
          | none => none
          | some (n2, len2) => decComptype tidx n2 len2
    else decComptype tidx n len

/-- Decode `k` subtypes in a rec group, threading the absolute type index. -/
def decSubtypes : Nat → Nat → Nat → Nat → Option (List Nat × List Nat × Option Nat × Nat × Nat)
  | 0,   _,    n, len => some ([], [], none, n, len)
  | k+1, tidx, n, len =>
      match decOneSubtype tidx n len with
      | none => none
      | some (ar, nf, car, n1, len1) =>
          match decSubtypes k (tidx+1) n1 len1 with
          | none => none
          | some (ars, nfs, car2, n2, len2) =>
              some (ar :: ars, nf :: nfs, car.orElse (fun _ => car2), n2, len2)

/-- Decode `entries` rectype entries (each a rec group or a bare subtype). -/
def decRecVec : Nat → Nat → Nat → Nat → Option (List Nat × List Nat × Option Nat × Nat × Nat)
  | 0,   _,    n, len => some ([], [], none, n, len)
  | e+1, tidx, n, len =>
      if len == 0 then none else
        let b0 := n &&& 0xff
        if b0 == 0x4e then                 -- explicit rec group
          match readU (n >>> 8) (len - 1) with
          | none => none
          | some (sc, n1, len1) =>
              match decSubtypes sc tidx n1 len1 with
              | none => none
              | some (ars, nfs, car, n2, len2) =>
                  match decRecVec e (tidx + sc) n2 len2 with
                  | none => none
                  | some (ars2, nfs2, car2, n3, len3) =>
                      some (ars ++ ars2, nfs ++ nfs2, car.orElse (fun _ => car2), n3, len3)
        else                               -- single implicit rec group
          match decOneSubtype tidx n len with
          | none => none
          | some (ar, nf, car, n1, len1) =>
              match decRecVec e (tidx+1) n1 len1 with
              | none => none
              | some (ars2, nfs2, car2, n2, len2) =>
                  some (ar :: ars2, nf :: nfs2, car.orElse (fun _ => car2), n2, len2)

def decodeTypes (n len : Nat) : Option TypeInfo :=
  match findSec 1 64 (n >>> 64) (len - 8) with
  | none => none
  | some (tN, tLen) =>
      match readU tN tLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decRecVec cnt 0 n1 len1 with
          | none => none
          | some (ars, nfs, car, _, _) => some ⟨ars, nfs, car⟩

/-- The Int carrier struct type index. -/
def decodeCarrier (n len : Nat) : Option Nat :=
  match decodeTypes n len with
  | some ti => ti.carrier
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
  match findSec 3 64 (n >>> 64) (len - 8) with
  | none => some []
  | some (fN, fLen) =>
      match readU fN fLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decFuncVec cnt n1 len1 with
          | none => none
          | some (ts, _, _) => some ts

/-- Read `k` imports, keeping the (module, name) of each imported FUNCTION.
    Non-function import kinds are outside the profile → reject (fail-closed). -/
def decImportVec : Nat → Nat → Nat → Option (List (String × String) × Nat × Nat)
  | 0,   n, len => some ([], n, len)
  | k+1, n, len =>
      match readU n len with                     -- module name length
      | none => none
      | some (ml, n1, len1) =>
          let modName := mkName (takeBytes ml n1)
          let n2 := n1 >>> (8*ml)
          let len2 := len1 - ml
          match readU n2 len2 with                -- import name length
          | none => none
          | some (nl, n3, len3) =>
              let nm := mkName (takeBytes nl n3)
              let n4 := n3 >>> (8*nl)
              let len4 := len3 - nl
              if len4 == 0 then none else
                let kind := n4 &&& 0xff
                let n5 := n4 >>> 8
                let len5 := len4 - 1
                if kind == 0 then                 -- func import: typeidx uleb
                  match readU n5 len5 with
                  | none => none
                  | some (_, n6, len6) =>
                      match decImportVec k n6 len6 with
                      | none => none
                      | some (rest, n7, len7) => some ((modName, nm) :: rest, n7, len7)
                else none

/-- Imported (module, name) list. Absent import section → no imports. -/
def decodeImports (n len : Nat) : Option (List (String × String)) :=
  match findSec 2 64 (n >>> 64) (len - 8) with
  | none => some []
  | some (iN, iLen) =>
      match readU iN iLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decImportVec cnt n1 len1 with
          | none => none
          | some (imps, _, _) => some imps

/-- Number of imported functions = the base offset for defined function indices. -/
def funcImportBase (n len : Nat) : Option Nat :=
  match decodeImports n len with
  | some imps => some imps.length
  | none => none

/-- Export section: keep function exports (kind 0) as (name, funcidx). -/
def decEntries : Nat → Nat → Nat → Option (List (String × Nat))
  | 0,     _, _   => some []
  | cnt+1, n, len =>
      match readU n len with
      | none => none
      | some (nlen, n1, len1) =>
          let name := takeBytes nlen n1
          let n2   := n1 >>> (8*nlen)
          let len2 := len1 - nlen
          if len2 == 0 then none else
            let kind := n2 &&& 0xff
            let n3   := n2 >>> 8
            match readU n3 (len2-1) with
            | none => none
            | some (idx, n4, len4) =>
                match decEntries cnt n4 len4 with
                | none => none
                | some tl => if kind == 0 then some ((mkName name, idx) :: tl) else some tl

def decodeExports (n len : Nat) : Option (List (String × Nat)) :=
  match findSec 7 64 (n >>> 64) (len - 8) with
  | none => none
  | some (eN, eLen) =>
      match readU eN eLen with
      | none => none
      | some (cnt, n1, len1) => decEntries cnt n1 len1

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
              let bytes := takeBytes bc n2
              let n3 := n2 >>> (8*bc)
              let len3 := len2 - bc
              match decDataVec k n3 len3 with
              | none => none
              | some (rest, n4, len4) => some (bytes :: rest, n4, len4)
        else none

/-- Data segment payloads by segment index. Absent data section → no segments. -/
def decodeData (n len : Nat) : Option (List (List Nat)) :=
  match findSec 11 64 (n >>> 64) (len - 8) with
  | none => some []
  | some (dN, dLen) =>
      match readU dN dLen with
      | none => none
      | some (cnt, n1, len1) =>
          match decDataVec cnt n1 len1 with
          | none => none
          | some (segs, _, _) => some segs

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

/-- Decode a raw straight/nested body Nat (little-endian, ending in `end`).
    Used by the coverage matrix's synthetic single-body probes. -/
def decodeBodyBytes (nfields : List Nat) (segs : List (List Nat)) (fuel n len : Nat) :
    Option (List WInstr) :=
  match decBlock nfields segs fuel [] n len with
  | none => none
  | some (instrs, _, _, _, term) => if term == 0 then some instrs else none

/- ===================================================================== -/
/-  Per-function obligation decode                                        -/
/- ===================================================================== -/

/-- Skip `k` code-section entries (each: size prefix + `size` body bytes). -/
def skipEntries : Nat → Nat → Nat → Option (Nat × Nat)
  | 0,   n, len => some (n, len)
  | k+1, n, len =>
      match readU n len with
      | none => none
      | some (sz, n1, len1) => skipEntries k (n1 >>> (8*sz)) (len1 - sz)

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
            match ti.arities[tyidx]? with
            | none => none
            | some ar =>
              match findSec 10 64 (n >>> 64) (len - 8) with
              | none => none
              | some (codeN, codeLen) =>
                match readU codeN codeLen with          -- nfuncs
                | none => none
                | some (_nf, r0, l0) =>
                  match skipEntries (funcidx - nimp) r0 l0 with
                  | none => none
                  | some (eN, eLen) =>
                    match readU eN eLen with            -- this entry's size
                    | none => none
                    | some (esz, bN, bLen) =>
                      match readU bN bLen with          -- nloc groups
                      | none => none
                      | some (ng, gN, gLen) =>
                        match decLocals ng gN gLen with
                        | none => none
                        | some (nloc, bodyN, bodyLen) =>
                          match decBlock ti.nfields segs esz [] bodyN bodyLen with
                          | none => none
                          | some (instrs, _, _, _, term) =>
                              if term == 0 then some ⟨ar, nloc, instrs⟩ else none

/-- The decoded body (`List WInstr`) of `funcidx`. -/
def decodeBody (n len funcidx : Nat) : Option (List WInstr) :=
  (decodeCode n len funcidx).map (·.body)

/-- The decoded arity of `funcidx` (from the type section). -/
def decodeArity (n len funcidx : Nat) : Option Nat :=
  (decodeCode n len funcidx).map (·.arity)

end CertDecode
