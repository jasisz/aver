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
