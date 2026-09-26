/-
  ScaleSections (cert-scale spike): a section read as declared blocks of
  entries, each block from its own chunk window.

  A block is a run of consecutive entries starting at a declared module
  offset. `blockOk` reads the block's bytes once through `window` (only the
  chunks the block touches) and decodes every entry on its own window inside
  the block (`ByteWindow.seqWin`). The blocks tile the section when the
  declared starts chain (`tiled`): block `b + 1` starts where block `b`
  ends, the first at the section's first entry, the last ends at the
  section's end. Each block is its own declaration, so the kernel's caches
  never hold more than one block.
-/
import ScaleBytes

namespace AverCert.ScaleSections
open CertDecode AverCert.ByteWindow AverCert.ScaleBytes

/-- Every entry of a block decodes and fills its window exactly. -/
def blockOk {α : Type} (r : Nat → Nat → Option (α × Nat × Nat)) (cs : List Nat)
    (start : Nat) (ls : List Nat) : Bool :=
  let P := window 1024 cs start ls.sum
  (seqWin P ls).all (fun w => (whole r w).isSome)

/-- The declared blocks `(start, lengths)` tile `[first, last)`. -/
def tiled : Nat → Nat → List (Nat × List Nat) → Bool
  | first, last, [] => first == last
  | first, last, (s, ls) :: bs => s == first && tiled (s + ls.sum) last bs

/-- The size LEB of a section header at `hdr`: its id byte and its payload
    `(start, size)`, read from a 6-byte window. -/
def sectionAt (cs : List Nat) (hdr : Nat) : Option (Nat × Nat × Nat) :=
  let h := window 1024 cs hdr 6
  match readU (h >>> 8) 5 with
  | some (size, _, rest) => some (h &&& 0xff, hdr + 1 + (5 - rest), size)
  | none => none

/-- Module framing over declared section headers: the magic and version,
    then every header at its declared offset, each payload ending where the
    next header starts, the last at the module's end. -/
def framingOk (cs : List Nat) (modLen : Nat) : Nat → List Nat → Bool
  | pos, [] => pos == modLen
  | pos, hdr :: hs =>
      hdr == pos &&
      match sectionAt cs hdr with
      | some (_, start, size) => framingOk cs modLen (start + size) hs
      | none => false

def headerOk (cs : List Nat) : Bool := window 1024 cs 0 8 == 0x000000016d736100

/-- A vector count at `pos`: the count and where the entries start. -/
def countAt (cs : List Nat) (pos : Nat) : Option (Nat × Nat) :=
  match readU (window 1024 cs pos 5) 5 with
  | some (cnt, _, rest) => some (cnt, pos + (5 - rest))
  | none => none

/-! ### Closure scan per function -/

open AverCert.DeclaredLayout in
/-- The call targets of defined function `k`, scanned on its own window. -/
def calleesAt (cs : List Nat) (L : Layout) (f : Nat) : Option (List Nat) :=
  let k := f - L.imports
  AverCert.WasmSlice.scanClosureCodeEntry (takeBytes (L.len k) (window 1024 cs (L.off k) (L.len k)))

/-- Each admitted function's declared callee list is what its bytes call. -/
def calleesOk (cs : List Nat) (L : AverCert.DeclaredLayout.Layout) :
    List (Nat × List Nat) → Bool
  | [] => true
  | (f, cl) :: rest => calleesAt cs L f == some cl && calleesOk cs L rest

/-! ### Export accounting per block -/

/-- A name's numeric key: its packed bytes and its length (injective). -/
def nameKey (bs : List Nat) : Nat := pack bs * 65536 + bs.length

/-- Every export entry of a block has the declared name key, kind and index. -/
def exportBlock (cs : List Nat) (start : Nat) (ls : List Nat)
    (ks : List (Nat × Nat × Nat)) : Bool :=
  let ws := seqWin (window 1024 cs start ls.sum) ls
  ws.length == ks.length && (ws.zip ks).all fun p =>
    match whole readExportEntry p.1 with
    | some e => nameKey e.name == p.2.1 && e.kind == p.2.2.1 && e.idx == p.2.2.2
    | none => false

/-- A run of manifest names has the given keys. -/
def namesBlock (names : List String) (keys : List Nat) : Bool :=
  names.map (fun s => nameKey (s.toList.map Char.toNat)) == keys

end AverCert.ScaleSections
