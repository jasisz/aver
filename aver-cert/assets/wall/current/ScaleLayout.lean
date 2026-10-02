-- Declared framing, code tiling and packed plan checks over chunk windows.
import DeclaredLayout
import ClaimAxes
import ScaleBytes

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleLayout
open CertDecode AverCert.ByteWindow AverCert.ScaleBytes AverCert.DeclaredLayout
open AverCert.Schema AverCert.AcceptedArtifact AverCert.TypeTable AverCert.Grammar

/-! ### What a LEB read consumes

A LEB reader leaves the numeral shifted by exactly the bytes it read. -/

theorem uleb_shift : ∀ (fuel acc sh n len x n' len' : Nat),
    uleb fuel acc sh n len = some (x, n', len') → len' < len ∧ n' = n >>> (8 * (len - len'))
  | 0, _, _, _, _, _, _, _, h => by simp [uleb] at h
  | fuel + 1, acc, sh, n, len, x, n', len', h => by
      unfold uleb at h
      by_cases hl : len = 0
      · simp [hl] at h
      simp only [hl, beq_iff_eq, ite_false] at h
      split at h
      · split at h
        · cases h
        · simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨-, rfl, rfl⟩ := h
          refine ⟨by omega, ?_⟩
          rw [show len - (len - 1) = 1 by omega]
      · obtain ⟨hlt, heq⟩ := uleb_shift fuel _ _ _ _ _ _ _ h
        refine ⟨by omega, ?_⟩
        rw [heq, ← Nat.shiftRight_add]
        congr 1
        omega

theorem readU_shift {n len x n' len' : Nat} (h : readU n len = some (x, n', len')) :
    len' < len ∧ n' = n >>> (8 * (len - len')) :=
  uleb_shift 5 0 0 n len x n' len' h

/-- A LEB read of the `l` bytes at `off` (at most the bytes left before
    `stop`) is the read from `off` on: the same value, and the numeral and
    length after it. -/
theorem readU_window {n off l stop x w' rest : Nat} (hl : off + l ≤ stop)
    (h : readU (slice n off l) l = some (x, w', rest)) :
    rest ≤ l ∧ readU (n >>> (8 * off)) (stop - off) =
      some (x, n >>> (8 * (off + (l - rest))), stop - (off + (l - rest))) := by
  obtain ⟨hw', hle, hext⟩ := readU_ext (slice_lt n off l) h
  obtain ⟨hlt, hsh⟩ := readU_shift h
  refine ⟨hle, ?_⟩
  have hs := hext (n >>> (8 * (off + l))) (stop - off - l)
  rw [← shr_split, show l + (stop - off - l) = stop - off by omega] at hs
  rw [hs, hsh, slice_shr n off l (l - rest) (Nat.sub_le _ _),
    show l - (l - rest) = rest by omega]
  have hsplit := shr_split n (off + (l - rest)) rest
  rw [show off + (l - rest) + rest = off + l by omega] at hsplit
  rw [← hsplit, show rest + (stop - off - l) = stop - (off + (l - rest)) by omega]

/-- The same read inside a slice that holds the window. -/
theorem readU_window_slice {n off l S x w' rest : Nat} (hl : l ≤ S)
    (h : readU (slice n off l) l = some (x, w', rest)) :
    rest ≤ l ∧ readU (slice n off S) S =
      some (x, slice n (off + (l - rest)) (S - (l - rest)), S - (l - rest)) := by
  obtain ⟨hw', hle, hext⟩ := readU_ext (slice_lt n off l) h
  obtain ⟨hlt, hsh⟩ := readU_shift h
  refine ⟨hle, ?_⟩
  have hs := hext (slice n (off + l) (S - l)) (S - l)
  rw [← slice_split, show l + (S - l) = S by omega] at hs
  rw [hs, hsh, slice_shr n off l (l - rest) (Nat.sub_le _ _),
    show l - (l - rest) = rest by omega]
  have hsplit := slice_split n (off + (l - rest)) rest (S - l)
  rw [show off + (l - rest) + rest = off + l by omega] at hsplit
  rw [← hsplit, show rest + (S - l) = S - (l - rest) by omega]

/-! ### Declared module framing

The producer declares the offset of every section header. `headersAt` reads
each header on a window at its declared offset: its id byte and its size
LEB, which fix where its payload starts and where the next header must be.
The chain must start after the magic and version and end exactly at the
module's length, so no header can be declared inside a payload or past the
end, and no section is skipped. `sectionTable_of_headers` shows that the
declared headers are the ones `CertDecode.sectionTable` walks. -/

/-- The section header at `pos`: its id, its payload's start and its size. -/
def headerAt (cs : List Nat) (len pos : Nat) : Option (Nat × Nat × Nat) :=
  if pos < len then
    match readU (window 1024 cs (pos + 1) (min 5 (len - (pos + 1)))) (min 5 (len - (pos + 1))) with
    | some (size, _, rest) =>
        if pos + 1 + (min 5 (len - (pos + 1)) - rest) + size ≤ len then
          some (window 1024 cs pos 1, pos + 1 + (min 5 (len - (pos + 1)) - rest), size)
        else none
    | none => none
  else none

/-- The declared headers, each at the end of the previous payload, the last
    payload ending at `len`. -/
def headersAt (cs : List Nat) (len : Nat) : Nat → List Nat → Option (List (Nat × Nat × Nat))
  | pos, [] => if pos == len then some [] else none
  | pos, h :: hs =>
      if h == pos then
        match headerAt cs len pos with
        | some (id, start, size) =>
            (headersAt cs len (start + size) hs).map (fun rest => (id, start, size) :: rest)
        | none => none
      else none

/-- The module's framing over its declared headers. -/
def framingOk (cs : List Nat) (len : Nat) (hs : List Nat) : Bool :=
  decide (8 ≤ len) && window 1024 cs 0 8 == 0x000000016d736100 && decide (hs.length < 64) &&
    (headersAt cs len 8 hs).isSome

theorem headerAt_spec {cs : List Nat} {n len pos id start size : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : headerAt cs len pos = some (id, start, size)) :
    pos < len ∧ start + size ≤ len ∧ (n >>> (8 * pos)) &&& 0xff = id ∧
      readU ((n >>> (8 * pos)) >>> 8) (len - pos - 1) =
        some (size, n >>> (8 * start), len - start) := by
  unfold headerAt at h
  split at h
  · rename_i hpos
    split at h
    · rename_i size' w' rest hU
      split at h
      · rename_i hle
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        rw [window_eq (by decide) hfit, hn] at hU
        obtain ⟨hrest, hread⟩ := readU_window (stop := len) (by omega) hU
        refine ⟨hpos, hle, ?_, ?_⟩
        · rw [window_eq (by decide) hfit, hn, land_ff]; rfl
        · rw [← Nat.shiftRight_add, show 8 * pos + 8 = 8 * (pos + 1) by omega,
            show len - pos - 1 = len - (pos + 1) by omega, hread]
      · cases h
    · cases h
  · cases h

theorem headersAt_nil {cs : List Nat} {len pos : Nat} {S : List (Nat × Nat × Nat)}
    (h : headersAt cs len pos [] = some S) : pos = len ∧ S = [] := by
  simp only [headersAt] at h
  split at h
  · rename_i he; simp only [beq_iff_eq] at he; simp only [Option.some.injEq] at h
    exact ⟨he, h.symm⟩
  · cases h

theorem headersAt_cons {cs : List Nat} {len pos hd : Nat} {hs : List Nat}
    {S : List (Nat × Nat × Nat)} (h : headersAt cs len pos (hd :: hs) = some S) :
    ∃ id start size S', headerAt cs len pos = some (id, start, size) ∧
      headersAt cs len (start + size) hs = some S' ∧ S = (id, start, size) :: S' := by
  simp only [headersAt] at h
  split at h
  · split at h
    · rename_i id start size hH
      cases hr : headersAt cs len (start + size) hs with
      | none => simp [hr] at h
      | some S' =>
          simp only [hr, Option.map_some, Option.some.injEq] at h
          exact ⟨id, start, size, S', hH, hr, h.symm⟩
    · cases h
  · cases h

/-- The declared headers are the sections `sectionTable` walks. -/
theorem sectionTable_of_headers {cs : List Nat} {n len : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) :
    ∀ (hs : List Nat) (pos fuel : Nat) (S : List (Nat × Nat × Nat)), hs.length < fuel →
      headersAt cs len pos hs = some S →
      sectionTable fuel (n >>> (8 * pos)) (len - pos) =
        some (S.map fun e => (e.1, slice n e.2.1 e.2.2, e.2.2))
  | [], pos, fuel, S, hf, h => by
      obtain ⟨rfl, rfl⟩ := headersAt_nil h
      obtain ⟨f, rfl⟩ : ∃ f, fuel = f + 1 := ⟨fuel - 1, by simp at hf; omega⟩
      simp [sectionTable]
  | _ :: hs, pos, fuel, S, hf, h => by
      obtain ⟨id, start, size, S', hH, hrest, rfl⟩ := headersAt_cons h
      obtain ⟨hpos, hend, hid, hread⟩ := headerAt_spec hn hfit hH
      obtain ⟨f, rfl⟩ : ∃ f, fuel = f + 1 := ⟨fuel - 1, by simp at hf; omega⟩
      have ih := sectionTable_of_headers hn hfit hs (start + size) f S' (by simp at hf; omega) hrest
      have hlen : (len - pos == 0) = false := by simp; omega
      have hsz : size ≤ len - start := by omega
      simp only [sectionTable, hlen, Bool.false_eq_true, ↓reduceIte, hread, hid, hsz]
      rw [← Nat.shiftRight_add, ← Nat.mul_add,
        show len - start - size = len - (start + size) by omega, ih, List.map_cons,
        slice_eq_isolate]

/-- The declared headers pass `sectionFramingValid`. -/
theorem sectionFramingValid_of_headers {cs : List Nat} {n len : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) :
    ∀ (hs : List Nat) (pos fuel : Nat) (S : List (Nat × Nat × Nat)), hs.length ≤ fuel →
      headersAt cs len pos hs = some S →
      sectionFramingValid fuel (n >>> (8 * pos)) (len - pos) = true
  | [], pos, fuel, S, _, h => by
      obtain ⟨rfl, rfl⟩ := headersAt_nil h
      cases fuel <;> simp [sectionFramingValid]
  | _ :: hs, pos, fuel, S, hf, h => by
      obtain ⟨id, start, size, S', hH, hrest, rfl⟩ := headersAt_cons h
      obtain ⟨hpos, hend, hid, hread⟩ := headerAt_spec hn hfit hH
      obtain ⟨f, rfl⟩ : ∃ f, fuel = f + 1 := ⟨fuel - 1, by simp at hf; omega⟩
      have ih := sectionFramingValid_of_headers hn hfit hs (start + size) f S'
        (by simp at hf; omega) hrest
      have hlen : (len - pos == 0) = false := by simp; omega
      simp only [sectionFramingValid, hlen, Bool.false_eq_true, ↓reduceIte, hread,
        Bool.and_eq_true, decide_eq_true_eq]
      rw [← Nat.shiftRight_add, ← Nat.mul_add, show len - start - size = len - (start + size) by omega]
      exact ⟨by omega, ih⟩

theorem magic_of_window {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) (h : window 1024 cs 0 8 = 0x000000016d736100) :
    (n &&& 0xffffffffffffffff) = 0x000000016d736100 := by
  rw [window_eq (by decide) hfit, hn] at h
  rw [show (0xffffffffffffffff : Nat) = 2 ^ 64 - 1 from rfl, Nat.and_two_pow_sub_one_eq_mod]
  simpa [slice] using h

/-- Declared framing gives the wall's framing check. -/
theorem moduleFramingValid_of_framing {cs : List Nat} {n len : Nat} {hs : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true) (h : framingOk cs len hs = true) :
    moduleFramingValid n len = true := by
  simp only [framingOk, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, Option.isSome_iff_exists] at h
  obtain ⟨⟨⟨h8, hmagic⟩, hcount⟩, S, hS⟩ := h
  have hv := sectionFramingValid_of_headers hn hfit hs 8 64 S (by omega) hS
  simp only [moduleFramingValid, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq]
  refine ⟨⟨h8, magic_of_window hn hfit hmagic⟩, ?_⟩
  simpa using hv

/-- Declared framing gives every section payload the decoders read. -/
theorem modulePayload_of_framing {cs : List Nat} {n len : Nat} {hs : List Nat}
    {S : List (Nat × Nat × Nat)} (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : framingOk cs len hs = true) (hS : headersAt cs len 8 hs = some S) (t : Nat) :
    modulePayload t n len =
      (S.find? (fun e => e.1 == t)).map (fun e => (slice n e.2.1 e.2.2, e.2.2)) := by
  simp only [framingOk, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at h
  obtain ⟨⟨⟨h8, hmagic⟩, hcount⟩, -⟩ := h
  have ht := sectionTable_of_headers hn hfit hs 8 64 S (by omega) hS
  simp only [modulePayload, moduleView, h8, magic_of_window hn hfit hmagic, and_self, ↓reduceIte]
  rw [show (64 : Nat) = 8 * 8 from rfl, ht]
  simp only [Option.map_some, Option.bind_some, ModuleView.payload, List.find?_map]
  rw [Option.map_map]
  rfl

/-! ### Code section tiling

The producer declares every code entry's offset and length (the packed
tables of the layout). `codeTiled` reads the code section's count on a
window at its payload's start and requires the declared entries to tile the
payload: the first at the first entry, each at the end of the one before,
the last ending at the payload's end. Each entry is decoded on its own
window (`codeEntryOk`), a block of entries per declaration. Together they
give the code section as the decoder reads it (`codeLocs_of_tiled`), and so
the declared layout (`layoutConfirmed_of_tiled`), with no declaration
reading more than one block of entries. -/

/-- The declared entries `k, k + 1, …` (`r` of them) start at `pos`, each at
    the end of the one before, and end at `stop`. -/
def tiled (L : Layout) : Nat → Nat → Nat → Nat → Bool
  | _, 0, pos, stop => pos == stop
  | k, r + 1, pos, stop => L.off k == pos && tiled L (k + 1) r (pos + L.len k) stop

/-- The code section's framing, its count, and its tiling. -/
def codeTiled (cs : List Nat) (len : Nat) (hs : List Nat) (L : Layout) : Bool :=
  framingOk cs len hs &&
  match headersAt cs len 8 hs with
  | some S =>
      match S.find? (fun e => e.1 == 10) with
      | some (_, start, size) =>
          match readU (window 1024 cs start (min 5 size)) (min 5 size) with
          | some (cnt, _, rest) =>
              cnt == L.count && tiled L 0 L.count (start + (min 5 size - rest)) (start + size)
          | none => false
      | none => false
  | none => false

/-- Code entry `k` decodes on its declared window and fills it exactly. -/
def codeEntryOk (cs : List Nat) (L : Layout) (k : Nat) : Bool :=
  (whole readCodeEntry (window 1024 cs (L.off k) (L.len k), L.len k)).isSome

/-- The code section as the declared windows, read lazily. -/
def codeLocsL (cs : List Nat) (L : Layout) : Array CodeLoc :=
  ((List.range L.count).map fun k => locOf (window 1024 cs (L.off k) (L.len k), L.len k)).toArray

/-- `f` holds at `k, k + 1, …, k + m - 1`. -/
def allRange (f : Nat → Bool) : Nat → Nat → Bool
  | _, 0 => true
  | k, m + 1 => f k && allRange f (k + 1) m

theorem allRange_spec {f : Nat → Bool} :
    ∀ {k m : Nat}, allRange f k m = true → ∀ j, k ≤ j → j < k + m → f j = true
  | _, 0, _, j, h1, h2 => by omega
  | k, m + 1, h, j, h1, h2 => by
      simp only [allRange, Bool.and_eq_true] at h
      by_cases hj : j = k
      · subst hj; exact h.1
      · exact allRange_spec h.2 j (by omega) (by omega)

theorem allRange_zero (f : Nat → Bool) (k : Nat) : allRange f k 0 = true := rfl

/-- Two consecutive ranges. -/
theorem allRange_join {f : Nat → Bool} :
    ∀ {k a b : Nat}, allRange f k a = true → allRange f (k + a) b = true →
      allRange f k (a + b) = true
  | _, 0, _, _, h2 => by simpa using h2
  | k, a + 1, b, h1, h2 => by
      simp only [allRange, Bool.and_eq_true] at h1
      rw [show a + 1 + b = (a + b) + 1 by omega]
      simp only [allRange, Bool.and_eq_true]
      exact ⟨h1.1, allRange_join h1.2 (by rw [show k + 1 + a = k + (a + 1) by omega]; exact h2)⟩

theorem tiled_spec {L : Layout} {n stop : Nat} :
    ∀ (r k pos : Nat), tiled L k r pos stop = true →
      pos + ((List.range' k r).map L.len).sum = stop ∧
      ∀ T, ((List.range' k r).map L.len).sum ≤ T →
        seqWin (slice n pos T) ((List.range' k r).map L.len) =
          (List.range' k r).map (fun j => (slice n (L.off j) (L.len j), L.len j))
  | 0, k, pos, h => by
      simp only [tiled, beq_iff_eq] at h
      refine ⟨by simp [h], fun T _ => by simp [seqWin]⟩
  | r + 1, k, pos, h => by
      simp only [tiled, Bool.and_eq_true, beq_iff_eq] at h
      obtain ⟨hoff, hrest⟩ := h
      obtain ⟨hsum, hwin⟩ := tiled_spec r (k + 1) (pos + L.len k) hrest
      simp only [List.range'_succ, List.map_cons, List.sum_cons]
      refine ⟨by omega, fun T hT => ?_⟩
      simp only [seqWin, List.cons.injEq]
      refine ⟨by rw [slice_mod n pos _ T (by omega), hoff], ?_⟩
      rw [slice_shr n pos T _ (by omega)]
      exact hwin _ (by omega)

/-- A `mapM` that succeeds everywhere with a known value. -/
theorem mapM_of_forall {α β : Type} {g : α → Option β} {f : α → β} :
    ∀ (l : List α), (∀ x ∈ l, g x = some (f x)) → l.mapM g = some (l.map f)
  | [], _ => rfl
  | x :: xs, h => by
      simp only [List.mapM_cons, h x List.mem_cons_self, Option.bind_eq_bind, Option.bind_some,
        mapM_of_forall xs (fun y hy => h y (List.mem_cons_of_mem _ hy)), Option.pure_def,
        List.map_cons]

theorem codeTiled_spec {cs : List Nat} {n len : Nat} {hs : List Nat} {L : Layout}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true) (h : codeTiled cs len hs L = true) :
    ∃ start size k, modulePayload 10 n len = some (slice n start size, size) ∧ k ≤ size ∧
      readU (slice n start size) size =
        some (L.count, slice n (start + k) (size - k), size - k) ∧
      tiled L 0 L.count (start + k) (start + size) = true := by
  unfold codeTiled at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨hfr, hm⟩ := h
  split at hm
  · rename_i S hS
    split at hm
    · rename_i id start size hfind
      split at hm
      · rename_i cnt w' rest hU
        simp only [Bool.and_eq_true, beq_iff_eq] at hm
        obtain ⟨rfl, htile⟩ := hm
        have hpay := modulePayload_of_framing hn hfit hfr hS 10
        rw [hfind] at hpay
        rw [window_eq (by decide) hfit, hn] at hU
        obtain ⟨hrest, hread⟩ := readU_window_slice (S := size) (by omega) hU
        have hk : min 5 size - rest ≤ size := by omega
        exact ⟨start, size, min 5 size - rest, hpay, hk, hread, htile⟩
      · cases hm
    · cases hm
  · cases hm

/-- The declared code tiling, with every entry decoded on its window, is the
    code section the decoder reads. -/
theorem codeLocs_of_tiled {cs : List Nat} {n len : Nat} {hs : List Nat} {L : Layout}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true) (h : codeTiled cs len hs L = true)
    (he : allRange (codeEntryOk cs L) 0 L.count = true) :
    codeLocs n len = some (codeLocsL cs L) := by
  obtain ⟨start, size, k, hpay, hk, hread, htile⟩ := codeTiled_spec hn hfit h
  obtain ⟨hsum, hwin⟩ := tiled_spec (n := n) L.count 0 (start + k) htile
  have hsum' : ((List.range' 0 L.count).map L.len).sum = size - k := by omega
  have hw := hwin (size - k) (by omega)
  -- Every declared window decodes to its lazy reading.
  have hlocs : ((List.range' 0 L.count).map (fun j => (slice n (L.off j) (L.len j), L.len j))).mapM
      (whole readCodeEntry) =
      some (((List.range' 0 L.count).map (fun j => (slice n (L.off j) (L.len j), L.len j))).map
        locOf) := by
    apply mapM_of_forall
    intro w hw
    obtain ⟨j, hj, rfl⟩ := List.mem_map.mp hw
    rw [List.mem_range'] at hj
    have hok := allRange_spec he j (by omega) (by omega)
    unfold codeEntryOk at hok
    rw [window_eq (by decide) hfit, hn] at hok
    obtain ⟨loc, hloc⟩ := Option.isSome_iff_exists.mp hok
    rw [hloc, locOf_eq (slice_lt _ _ _) hloc]
  rw [← hw] at hlocs
  have hv := vec_of_windows readCodeEntry_ext ((List.range' 0 L.count).map L.len)
    (slice n (start + k) (size - k)) (size - k) _ (by omega) hlocs
  simp only [List.length_map, List.length_range', hsum', Nat.sub_self] at hv
  unfold codeLocs
  rw [hpay]
  simp only [hread, decCodeLocs_eq_vec, hv, Option.bind_some, BEq.rfl, ↓reduceIte, Option.map_some,
    Option.some.injEq]
  unfold codeLocsL
  rw [hw, List.map_map, List.range_eq_range']
  congr 2
  funext j
  simp only [Function.comp_apply, window_eq (by decide) hfit, hn]

/-- The function types of the declared layout, in order. -/
def layoutTys (L : Layout) : List Nat := (List.range L.count).map L.ty

/-- The import count and the function section, against the declared layout. -/
def funcsOk (n len : Nat) (L : Layout) : Bool :=
  funcImportBase n len == some L.imports && decodeFuncTypes n len == some (layoutTys L)

theorem funcImportBase_of_funcsOk {n len : Nat} {L : Layout} (h : funcsOk n len L = true) :
    funcImportBase n len = some L.imports := by
  simp only [funcsOk, Bool.and_eq_true, beq_iff_eq] at h
  exact h.1

theorem decodeFuncTypes_of_funcsOk {n len : Nat} {L : Layout} (h : funcsOk n len L = true) :
    decodeFuncTypes n len = some (layoutTys L) := by
  simp only [funcsOk, Bool.and_eq_true, beq_iff_eq] at h
  exact h.2

theorem locsMatch_of_tiled {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) (L : Layout) :
    ∀ (r k : Nat), locsMatch n L k ((List.range' k r).map L.ty)
      ((List.range' k r).map fun j => locOf (window 1024 cs (L.off j) (L.len j), L.len j)) = true
  | 0, k => by simp [locsMatch]
  | r + 1, k => by
      simp only [List.range'_succ, List.map_cons, locsMatch, BEq.rfl, Bool.true_and,
        Bool.and_eq_true, beq_iff_eq]
      refine ⟨⟨rfl, ?_⟩, locsMatch_of_tiled hn hfit L r (k + 1)⟩
      simp only [locOf, window_eq (by decide) hfit, hn, Layout.entryN, slice_eq_isolate]

/-- The declared layout from its tiling and the function section: what
    `layoutConfirmed` decides by decoding the code section whole. -/
theorem layoutConfirmed_of_tiled {cs : List Nat} {n len : Nat} {hs : List Nat} {L : Layout}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true) (h : codeTiled cs len hs L = true)
    (he : allRange (codeEntryOk cs L) 0 L.count = true) (hf : funcsOk n len L = true) :
    layoutConfirmed n len L = true := by
  simp only [funcsOk, Bool.and_eq_true, beq_iff_eq] at hf
  obtain ⟨himp, hfts⟩ := hf
  unfold layoutConfirmed
  rw [himp, hfts, codeLocs_of_tiled hn hfit h he]
  simp only [BEq.rfl, Bool.true_and, layoutTys, List.length_map, List.length_range, codeLocsL,
    List.toList_toArray, List.range_eq_range', List.length_range']
  exact locsMatch_of_tiled hn hfit L L.count 0

/-! ### Packed plan checks

A planned function's code entry is compared as one numeral: the lowering,
packed a limb at a time, against the chunk window of its declared entry.
The window reads only the chunks the entry touches, so each plan is its own
small declaration. `entryFast_of_packed` shows the packed check implies the
plan check the layout lemmas already turn into `entryAccepted`. -/

/-- `DeclaredLayout.entryFast` without its export conjunct, with the code
    entry compared as a packed window. -/
def entryPacked (cs : List Nat) (L : Layout) (fts : List FnType) (M : MCtx) (fns : List FnEntry)
    (e : FnEntry) (d : FnDecl) : Bool :=
  planTyped M e.plan && callsOrdered fns e &&
  decide (L.imports ≤ e.funcIdx) && decide (e.funcIdx - L.imports < L.count) &&
  match codeEntryBytes M e.plan, e.plan.sig.params.mapM (valTyD M), valTyD M e.plan.sig.ret with
  | some bytes, some ps, some r =>
      bytesOk bytes && bytes.length == L.len (e.funcIdx - L.imports) &&
      window 1024 cs (L.off (e.funcIdx - L.imports)) bytes.length ==
        packBy 32 bytes.length bytes &&
      fts[d.sigPos]? == some (L.ty (e.funcIdx - L.imports), ps, [r])
  | _, _, _ => false

/-- The export conjunct of `entryFast`. -/
def exportOk (E : List AverCert.WasmSlice.ExportEntry) (e : FnEntry) (d : FnDecl) : Bool :=
  !e.exported || E[d.exportPos]? == some ⟨d.name.map Char.toNat, 0, e.funcIdx⟩

theorem entryFast_of_packed {cs : List Nat} {n : Nat} {L : Layout} {fts : List FnType}
    {E : List AverCert.WasmSlice.ExportEntry} {M : MCtx} {fns : List FnEntry} {e : FnEntry}
    {d : FnDecl} (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hp : entryPacked cs L fts M fns e d = true) (hx : exportOk E e d = true) :
    entryFast n L fts E M fns e d = true := by
  unfold entryPacked at hp
  unfold exportOk at hx
  unfold entryFast
  simp only [Bool.and_eq_true] at hp ⊢
  obtain ⟨⟨⟨⟨htyped, hcalls⟩, hlo⟩, hhi⟩, hm⟩ := hp
  refine ⟨⟨⟨⟨htyped, hcalls⟩, hlo⟩, hhi⟩, ?_⟩
  split at hm
  · rename_i bytes ps r hb hps hr
    simp only [hb, hps, hr]
    simp only [Bool.and_eq_true, beq_iff_eq] at hm
    obtain ⟨⟨⟨hok, hlen⟩, hwin⟩, hsig⟩ := hm
    rw [window_eq (by decide) hfit, hn, packBy_eq (by decide) _ _ (Nat.le_refl _)] at hwin
    have hentry : L.entry n (e.funcIdx - L.imports) = bytes := by
      unfold Layout.entry Layout.entryN
      rw [← hlen]
      exact takeBytes_slice hok hwin
    simp only [hentry, BEq.rfl, hsig, Bool.true_and, hx, Bool.and_self]
  · cases hm

/-- The helper roles a plan's lowering calls, as bits: `box`, `add`, `sub`,
    `mul`, String equality, concatenation, `toIndex`, `cmp`, `eq`, `divmod`. -/
def roleCalls (M : MCtx) (e : FnEntry) : List Bool :=
  let calls := AverCert.ClaimAxes.wCallsL (fnCode M e.plan).body
  [calls.contains M.box, calls.contains M.add, calls.contains M.sub, calls.contains M.mul,
    calls.contains M.streq, calls.contains M.concat, calls.contains M.toIndex,
    calls.contains M.cmp, calls.contains M.eq, calls.contains M.divmod]

def bitsOf : List Bool → Nat
  | [] => 0
  | b :: bs => (if b then 1 else 0) + 2 * bitsOf bs

def bitAt (r i : Nat) : Bool := r / 2 ^ i % 2 == 1

theorem bitAt_bitsOf : ∀ (bs : List Bool) (i : Nat), bitAt (bitsOf bs) i = bs.getD i false
  | [], i => by simp [bitAt, bitsOf]
  | b :: bs, 0 => by cases b <;> simp [bitAt, bitsOf, Nat.add_mul_mod_self_left]
  | b :: bs, i + 1 => by
      rw [List.getD_cons_succ, ← bitAt_bitsOf bs i]
      simp only [bitAt]
      rw [Nat.pow_succ, Nat.mul_comm, ← Nat.div_div_eq_div_mul]
      have h2 : bitsOf (b :: bs) / 2 = bitsOf bs := by
        simp only [bitsOf]; cases b <;> simp <;> omega
      rw [h2]

def roleBits (M : MCtx) (e : FnEntry) : Nat := bitsOf (roleCalls M e)

/-! ### Export entries on windows

The producer declares where the export section's first entry starts (`e0`),
every entry's length (the export cut `ls`), and the bitmap of every entry's
start relative to `e0` (`startBits`, checked once against the cut).
`exportsHead` reads the section's count on a window at its payload's start
and requires the count to be the number of declared lengths, the first
entry to start at `e0` and the lengths to fill the payload. A plan then
names its export entry by module offset and length: the offset's bit in the
bitmap makes it an entry start, and the entry decoded on its own window,
filling it exactly, is the section's entry there (`exportSite_mem`). No
check walks the export list. -/

/-- The export section's count and first entry, against the declared cut. -/
def exportsHead (cs : List Nat) (len : Nat) (hs : List Nat) (e0 : Nat) (ls : List Nat) : Bool :=
  framingOk cs len hs &&
  match headersAt cs len 8 hs with
  | some S =>
      match S.find? (fun e => e.1 == 7) with
      | some (_, start, size) =>
          match readU (window 1024 cs start (min 5 size)) (min 5 size) with
          | some (cnt, _, rest) =>
              cnt == ls.length && e0 == start + (min 5 size - rest) &&
                ls.sum == size - (min 5 size - rest)
          | none => false
      | none => false
  | none => false

/-- The starts of consecutive entries of the given lengths, from `s`, as the
    bits of one numeral. -/
def startBits : Nat → List Nat → Nat
  | _, [] => 0
  | s, l :: ls => (1 <<< s) ||| startBits (s + l) ls

theorem startBits_spec : ∀ (ls : List Nat) (s t : Nat), (startBits s ls).testBit t = true →
    ∃ p, p < ls.length ∧ t = s + (ls.take p).sum
  | [], _, t, h => by simp [startBits] at h
  | l :: ls, s, t, h => by
      simp only [startBits, Nat.testBit_or, Bool.or_eq_true, Nat.one_shiftLeft,
        Nat.testBit_two_pow] at h
      rcases h with h | h
      · refine ⟨0, by simp, ?_⟩
        simp only [decide_eq_true_eq] at h
        simp [h]
      · obtain ⟨p, hp, rfl⟩ := startBits_spec ls (s + l) t h
        exact ⟨p + 1, by simp; omega, by simp [List.take_succ_cons, Nat.add_assoc]⟩

/-- The windows of consecutive entries in a slice, by position. -/
theorem seqWin_slice_get {n : Nat} :
    ∀ (ls : List Nat) (a T p : Nat), ls.sum ≤ T →
      (seqWin (slice n a T) ls)[p]? = ls[p]?.map fun l => (slice n (a + (ls.take p).sum) l, l)
  | [], _, _, p, _ => by simp [seqWin]
  | l :: ls, a, T, 0, h => by
      simp only [List.sum_cons] at h
      simp [seqWin, slice_mod n a l T (by omega)]
  | l :: ls, a, T, p + 1, h => by
      simp only [List.sum_cons] at h
      simp only [seqWin, List.getElem?_cons_succ, List.take_succ_cons, List.sum_cons]
      rw [slice_shr n a T l (by omega), seqWin_slice_get ls (a + l) (T - l) p (by omega),
        Nat.add_assoc]

/-- A checked `mapM`, read at one position. -/
theorem mapM_get {α β : Type} {f : α → Option β} :
    ∀ {l : List α} {E : List β}, l.mapM f = some E → ∀ p : Nat, E[p]? = (l[p]?).bind f
  | [], E, h, p => by
      simp only [List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h; simp
  | x :: xs, E, h, p => by
      simp only [List.mapM_cons, Option.bind_eq_bind, Option.pure_def] at h
      cases hx : f x with
      | none => simp [hx] at h
      | some y =>
          cases hxs : xs.mapM f with
          | none => simp [hx, hxs] at h
          | some ys =>
              simp only [hx, hxs, Option.bind_some, Option.some.injEq] at h
              subst h
              cases p with
              | zero => simp [hx]
              | succ p => simpa using mapM_get hxs p

theorem exportsHead_spec {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 : Nat} {ls : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : exportsHead cs len hs e0 ls = true) :
    ∃ start size k, modulePayload 7 n len = some (slice n start size, size) ∧
      readU (slice n start size) size =
        some (ls.length, slice n e0 (size - k), size - k) ∧ ls.sum = size - k := by
  unfold exportsHead at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨hfr, hm⟩ := h
  split at hm
  · rename_i S hS
    split at hm
    · rename_i id start size hfind
      split at hm
      · rename_i cnt w' rest hU
        simp only [Bool.and_eq_true, beq_iff_eq] at hm
        obtain ⟨⟨rfl, rfl⟩, hsum⟩ := hm
        have hpay := modulePayload_of_framing hn hfit hfr hS 7
        rw [hfind] at hpay
        rw [window_eq (by decide) hfit, hn] at hU
        obtain ⟨-, hread⟩ := readU_window_slice (S := size) (by omega) hU
        exact ⟨start, size, min 5 size - rest, hpay, hread, hsum⟩
      · cases hm
    · cases hm
  · cases hm

/-- With a confirmed export cut and head, export entry `p` is the entry its
    window decodes to. -/
theorem exportWindow_eq {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 : Nat}
    {ls : List Nat} {E : List AverCert.WasmSlice.ExportEntry} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) (h : exportsHead cs len hs e0 ls = true)
    (hE : decodeRawExportsCut n len ls = some E) (p : Nat) :
    E[p]? = (ls[p]?).bind fun l => whole readExportEntry (slice n (e0 + (ls.take p).sum) l, l) := by
  obtain ⟨start, size, k, hpay, hread, hsum⟩ := exportsHead_spec hn hfit h
  unfold decodeRawExportsCut at hE
  rw [hpay] at hE
  simp only [hread, BEq.rfl, hsum, Bool.and_self, ↓reduceIte] at hE
  rw [cutWin_eq] at hE
  rw [mapM_get hE p, seqWin_slice_get ls e0 (size - k) p (by omega)]
  cases ls[p]? <;> rfl

theorem mapM_isSome {α β : Type} {f : α → Option β} :
    ∀ {l : List α} {E : List β}, l.mapM f = some E → ∀ x ∈ l, (f x).isSome = true
  | [], _, _, _, hx => by cases hx
  | x :: xs, E, h, y, hy => by
      simp only [List.mapM_cons, Option.bind_eq_bind, Option.pure_def] at h
      cases hx : f x with
      | none => simp [hx] at h
      | some z =>
          cases hxs : xs.mapM f with
          | none => simp [hx, hxs] at h
          | some zs =>
              cases hy with
              | head => simp [hx]
              | tail _ hmem => exact mapM_isSome hxs y hmem

/-- Every window of a confirmed export cut decodes. -/
theorem exportWindow_some {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 : Nat}
    {ls : List Nat} {E : List AverCert.WasmSlice.ExportEntry} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) (h : exportsHead cs len hs e0 ls = true)
    (hE : decodeRawExportsCut n len ls = some E) (p : Nat) (hp : p < ls.length) :
    (whole readExportEntry (slice n (e0 + (ls.take p).sum) ls[p], ls[p])).isSome = true := by
  obtain ⟨start, size, k, hpay, hread, hsum⟩ := exportsHead_spec hn hfit h
  unfold decodeRawExportsCut at hE
  rw [hpay] at hE
  simp only [hread, BEq.rfl, hsum, Bool.and_self, ↓reduceIte] at hE
  rw [cutWin_eq] at hE
  have hget := seqWin_slice_get (n := n) ls e0 (size - k) p (by omega)
  rw [List.getElem?_eq_getElem hp, Option.map_some] at hget
  exact mapM_isSome hE _ (List.mem_of_getElem? hget)

/-- Two windows at one offset that a reader consumes exactly read the same
    entry. -/
theorem whole_unique {α : Type} {r : Nat → Nat → Option (α × Nat × Nat)} (hr : Ext r)
    {n a l₁ l₂ : Nat} {x y : α} (h₁ : whole r (slice n a l₁, l₁) = some x)
    (h₂ : whole r (slice n a l₂, l₂) = some y) : x = y := by
  -- The shorter window, extended to the longer one, leaves its excess unread.
  have key : ∀ {l₁ l₂ : Nat} {x y : α}, l₁ ≤ l₂ → whole r (slice n a l₁, l₁) = some x →
      whole r (slice n a l₂, l₂) = some y → x = y := by
    intro l₁ l₂ x y hle h₁ h₂
    unfold whole at h₁ h₂
    split at h₁
    · rename_i x' w' hw
      simp only [Option.some.injEq] at h₁
      subst h₁
      obtain ⟨-, -, hext⟩ := hr (slice_lt n a l₁) hw
      have hs := hext (slice n (a + l₁) (l₂ - l₁)) (l₂ - l₁)
      rw [← slice_split, show l₁ + (l₂ - l₁) = l₂ by omega] at hs
      simp only [hs, Nat.zero_add] at h₂
      split at h₂
      · rename_i x'' w'' heq
        simp only [Option.some.injEq, Prod.mk.injEq] at heq h₂
        exact heq.1.trans h₂
      · cases h₂
    · cases h₁
  by_cases hle : l₁ ≤ l₂
  · exact key hle h₁ h₂
  · exact (key (by omega) h₂ h₁).symm

/-- A plan's export site: the entry at module offset `off`, of length `l`,
    starts an entry of the section and is the function export of `name` at
    `fi`. -/
def exportSite (cs : List Nat) (e0 B off l : Nat) (name : List Nat) (fi : Nat) : Bool :=
  decide (e0 ≤ off) && B.testBit (off - e0) &&
    whole readExportEntry (window 1024 cs off l, l) == some ⟨name, 0, fi⟩

theorem exportSite_mem {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 B : Nat}
    {ls : List Nat} {E : List AverCert.WasmSlice.ExportEntry} {off l fi : Nat}
    {name : List Nat} (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : exportsHead cs len hs e0 ls = true) (hE : decodeRawExportsCut n len ls = some E)
    (hB : startBits 0 ls = B) (hsite : exportSite cs e0 B off l name fi = true) :
    ∃ p : Nat, E[p]? = some (⟨name, 0, fi⟩ : AverCert.WasmSlice.ExportEntry) := by
  simp only [exportSite, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at hsite
  obtain ⟨⟨hle, hbit⟩, hw⟩ := hsite
  rw [← hB] at hbit
  obtain ⟨p, hp, hoff⟩ := startBits_spec ls 0 (off - e0) hbit
  refine ⟨p, ?_⟩
  rw [exportWindow_eq hn hfit h hE p, List.getElem?_eq_getElem hp, Option.bind_some]
  rw [window_eq (by decide) hfit, hn] at hw
  have hat : e0 + (ls.take p).sum = off := by omega
  rw [hat]
  cases hq : whole readExportEntry (slice n off ls[p], ls[p]) with
  | none =>
      have := exportWindow_some hn hfit h hE p hp
      rw [hat, hq] at this
      cases this
  | some y => rw [whole_unique readExportEntry_ext hq hw]

/-! ### The plans, one declaration each -/

/-- One plan entry: its packed check, its export site, and its role bits. -/
def planOne (cs : List Nat) (L : Layout) (fts : List FnType) (M : MCtx) (fns : List FnEntry)
    (e0 B : Nat) (e : FnEntry) (d : FnDecl) (r : Nat) (site : Nat × Nat) : Bool :=
  entryPacked cs L fts M fns e d &&
  (!e.exported || exportSite cs e0 B site.1 site.2 (d.name.map Char.toNat) e.funcIdx) &&
  roleBits M e == r

/-- `planOne` along four lists in step, which must end together. -/
def plansFrom (cs : List Nat) (L : Layout) (fts : List FnType) (M : MCtx) (fns : List FnEntry)
    (e0 B : Nat) : List FnEntry → List FnDecl → List Nat → List (Nat × Nat) → Bool
  | [], [], [], [] => true
  | e :: es, d :: ds, r :: rs, x :: xs =>
      planOne cs L fts M fns e0 B e d r x && plansFrom cs L fts M fns e0 B es ds rs xs
  | _, _, _, _ => false

theorem plansFrom_nil (cs : List Nat) (L : Layout) (fts : List FnType) (M : MCtx)
    (fns : List FnEntry) (e0 B : Nat) : plansFrom cs L fts M fns e0 B [] [] [] [] = true :=
  rfl

theorem plansFrom_cons {cs : List Nat} {L : Layout} {fts : List FnType} {M : MCtx}
    {fns : List FnEntry} {e0 B : Nat} {e : FnEntry} {d : FnDecl} {r : Nat} {x : Nat × Nat}
    {es : List FnEntry} {ds : List FnDecl} {rs : List Nat} {xs : List (Nat × Nat)}
    (h : planOne cs L fts M fns e0 B e d r x = true)
    (t : plansFrom cs L fts M fns e0 B es ds rs xs = true) :
    plansFrom cs L fts M fns e0 B (e :: es) (d :: ds) (r :: rs) (x :: xs) = true := by
  simp only [plansFrom, h, t, Bool.and_self]

theorem plansFrom_append {cs : List Nat} {L : Layout} {fts : List FnType} {M : MCtx}
    {fns : List FnEntry} {e0 B : Nat} :
    ∀ {es₁ : List FnEntry} {ds₁ : List FnDecl} {rs₁ : List Nat} {xs₁ : List (Nat × Nat)}
      {es₂ : List FnEntry} {ds₂ : List FnDecl} {rs₂ : List Nat} {xs₂ : List (Nat × Nat)},
      plansFrom cs L fts M fns e0 B es₁ ds₁ rs₁ xs₁ = true →
      plansFrom cs L fts M fns e0 B es₂ ds₂ rs₂ xs₂ = true →
      plansFrom cs L fts M fns e0 B (es₁ ++ es₂) (ds₁ ++ ds₂) (rs₁ ++ rs₂) (xs₁ ++ xs₂) = true
  | [], [], [], [], _, _, _, _, _, h2 => h2
  | e :: es, d :: ds, r :: rs, x :: xs, _, _, _, _, h1, h2 => by
      simp only [plansFrom, Bool.and_eq_true] at h1
      simp only [List.cons_append, plansFrom, h1.1, plansFrom_append h1.2 h2, Bool.and_self]
  | [], [], [], _ :: _, _, _, _, _, h1, _ => by simp [plansFrom] at h1
  | [], [], _ :: _, _, _, _, _, _, h1, _ => by cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h1
  | [], _ :: _, _, _, _, _, _, _, h1, _ => by
      cases ‹List Nat› <;> cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h1
  | _ :: _, [], _, _, _, _, _, _, h1, _ => by
      cases ‹List Nat› <;> cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h1
  | _ :: _, _ :: _, [], _, _, _, _, _, h1, _ => by
      cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h1
  | _ :: _, _ :: _, _ :: _, [], _, _, _, _, h1, _ => by simp [plansFrom] at h1

/-- One block of plans, `m` from position `k`, and all the plans after it,
    give all the plans from `k`. -/
theorem plansFrom_block {cs : List Nat} {L : Layout} {fts : List FnType} {M : MCtx}
    {fns : List FnEntry} {e0 B : Nat} {es : List FnEntry} {ds : List FnDecl}
    {rs : List Nat} {xs : List (Nat × Nat)} {k m : Nat}
    (h1 : plansFrom cs L fts M fns e0 B ((es.drop k).take m) ((ds.drop k).take m)
      ((rs.drop k).take m) ((xs.drop k).take m) = true)
    (h2 : plansFrom cs L fts M fns e0 B (es.drop (k + m)) (ds.drop (k + m)) (rs.drop (k + m))
      (xs.drop (k + m)) = true) :
    plansFrom cs L fts M fns e0 B (es.drop k) (ds.drop k) (rs.drop k) (xs.drop k) = true := by
  rw [← List.drop_drop] at h2
  rw [← List.take_append_drop m (es.drop k), ← List.take_append_drop m (ds.drop k),
    ← List.take_append_drop m (rs.drop k), ← List.take_append_drop m (xs.drop k)]
  exact plansFrom_append h1 (by simpa [List.drop_drop, Nat.add_comm] using h2)

theorem plansFrom_spec {cs : List Nat} {n len : Nat} {hs : List Nat} {L : Layout}
    {fts : List FnType} {M : MCtx} {fns : List FnEntry} {e0 B : Nat} {ls : List Nat}
    {E : List AverCert.WasmSlice.ExportEntry}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hL : layoutConfirmed n len L = true) (hT : fnTypesConfirmed n len fts = true)
    (hH : exportsHead cs len hs e0 ls = true) (hcut : decodeRawExportsCut n len ls = some E)
    (hB : startBits 0 ls = B) (hnd : (E.map (·.name)).Nodup) :
    ∀ (es : List FnEntry) (ds : List FnDecl) (rs : List Nat) (xs : List (Nat × Nat)),
      es.map (·.name) = ds.map (fun d => String.ofList d.name) →
      plansFrom cs L fts M fns e0 B es ds rs xs = true →
      es.all (entryAccepted n len M fns) = true ∧ es.map (roleBits M) = rs
  | [], [], [], [], _, _ => ⟨rfl, rfl⟩
  | e :: es, d :: ds, r :: rs, x :: xs, hnm, h => by
      simp only [List.map_cons, List.cons.injEq] at hnm
      simp only [plansFrom, planOne, Bool.and_eq_true, beq_iff_eq] at h
      obtain ⟨⟨⟨hp, hx⟩, hr⟩, hrest⟩ := h
      obtain ⟨ih1, ih2⟩ :=
        plansFrom_spec hn hfit hL hT hH hcut hB hnd es ds rs xs hnm.2 hrest
      have hname : stringBytes e.name = d.name.map Char.toNat := by
        rw [hnm.1, stringBytes_ofList]
      have hdec := decodeRawExports_of_cut hcut
      -- The export entry, at the position the site names.
      have hexp : ∃ p, exportOk E e { d with exportPos := p } = true := by
        cases hex : e.exported
        · exact ⟨0, by simp [exportOk, hex]⟩
        · simp only [hex, Bool.not_true, Bool.false_or] at hx
          obtain ⟨p, hpE⟩ := exportSite_mem hn hfit hH hcut hB hx
          exact ⟨p, by simp [exportOk, hpE]⟩
      obtain ⟨p, hx'⟩ := hexp
      -- The packed check reads the declaration's name and function type
      -- position, never its export position.
      have hp' : entryPacked cs L fts M fns e { d with exportPos := p } = true := hp
      have hname' : stringBytes e.name = ({ d with exportPos := p } : FnDecl).name.map
          Char.toNat := hname
      refine ⟨?_, by simp [hr, ih2]⟩
      simp only [List.all_cons, ih1, Bool.and_true]
      exact entryAccepted_of_fast (d := { d with exportPos := p }) hL hT hdec hnd hname'
        (entryFast_of_packed hn hfit hp' hx')
  | [], [], [], _ :: _, _, h => by simp [plansFrom] at h
  | [], [], _ :: _, _, _, h => by cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h
  | [], _ :: _, _, _, hnm, _ => by simp at hnm
  | _ :: _, [], _, _, hnm, _ => by simp at hnm
  | _ :: _, _ :: _, [], _, _, h => by cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h
  | _ :: _, _ :: _, _ :: _, [], _, h => by simp [plansFrom] at h

/-- The per-plan checks, the export head and cut, and the confirmed layout
    give every plan's `entryAccepted` and its role bits. -/
theorem entries_of_packed {cs : List Nat} {n len : Nat} {hs : List Nat} {L : Layout}
    {fts : List FnType} {M : MCtx} {fns : List FnEntry} {ds : List FnDecl} {rs : List Nat}
    {xs : List (Nat × Nat)} {e0 B : Nat} {ls : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hL : layoutConfirmed n len L = true) (hT : fnTypesConfirmed n len fts = true)
    (hX : exportNamesDistinct n len = true)
    (hnames : fns.map (·.name) = ds.map (fun d => String.ofList d.name))
    (hcut : (decodeRawExportsCut n len ls).isSome = true)
    (hH : exportsHead cs len hs e0 ls = true) (hB : startBits 0 ls = B)
    (hall : plansFrom cs L fts M fns e0 B fns ds rs xs = true) :
    fns.all (entryAccepted n len M fns) = true ∧ fns.map (roleBits M) = rs := by
  obtain ⟨E, hE⟩ := Option.isSome_iff_exists.mp hcut
  unfold exportNamesDistinct at hX
  rw [decodeRawExports_of_cut hE] at hX
  exact plansFrom_spec hn hfit hL hT hH hE hB (byteSeqListNodup_nodup hX) fns ds rs xs hnames hall

/-! ### Claim axes from the per-plan role bits

`ClaimAxes.contractUse` lowers every plan again to find the helpers it
calls. Each plan's declaration already lowers it, so it also pins the plan's
role bits (`planOne`), and the contracts are read from the bits. -/

/-- The contract helpers the declared List and `Bytes` helpers call, the rest
    of `ClaimAxes.usedCalls`. -/
def helperCalls (M : MCtx) : List Nat :=
  (M.listHelpers.map fun x => _root_.AverCert.ListHelpers.innerCalls M x.2.1 x.1).flatten ++
    (M.bytesHelpers.map fun x => _root_.AverCert.BytesHelpers.bytesInnerCalls M x.1).flatten

/-- `ClaimAxes.contractUse` from the plans' role bits and the List helpers'
    own calls. -/
def useOfBits (M : MCtx) (rs : List Nat) (total totalMul : Bool) :
    AverCert.ClaimAxes.ContractUse :=
  let hc := helperCalls M
  { box := rs.any (bitAt · 0) || hc.contains M.box,
    add := rs.any (bitAt · 1) || hc.contains M.add,
    sub := rs.any (bitAt · 2) || hc.contains M.sub,
    mul := rs.any (bitAt · 3) || hc.contains M.mul,
    stringEq := rs.any (bitAt · 4) || hc.contains M.streq,
    stringConcat := rs.any (bitAt · 5) || hc.contains M.concat,
    toIndex := rs.any (bitAt · 6) || hc.contains M.toIndex,
    cmp := rs.any (bitAt · 7) || hc.contains M.cmp,
    eq := rs.any (bitAt · 8) || hc.contains M.eq,
    divmod := rs.any (bitAt · 9) || hc.contains M.divmod,
    addTotal := total, subTotal := total, mulTotal := totalMul }

/-- `ClaimAxes.checked` with the plans' helper calls read from the role bits. -/
def checkedBits (artifact : ArtifactData) (rs : List Nat) : Bool :=
  let m := artifact.manifest
  let M := mctxOf m.subject m.types m.fnPlans
  let total := m.obligations.any fun o => o.policy == .simulatesModelTotally
  let totalMul := m.obligations.any fun o =>
    o.policy == .simulatesModelTotally && o.totalityRole == .mul
  rs.length == m.fnPlans.length &&
    (useOfBits M rs total totalMul).contracts == m.subject.contracts

theorem contains_flatten (a : Nat) : ∀ xs : List (List Nat),
    xs.flatten.contains a = xs.any (·.contains a)
  | [] => rfl
  | x :: xs => by
      simp only [List.flatten_cons, List.contains_append, List.any_cons, contains_flatten a xs]

/-- One role's bit, over every plan, is whether some plan's lowering calls the
    role's helper. -/
theorem any_bit {M : MCtx} {fns : List FnEntry} {i a : Nat}
    (hi : ∀ e : FnEntry, (roleCalls M e).getD i false =
      (AverCert.ClaimAxes.wCallsL (fnCode M e.plan).body).contains a) :
    (fns.map (roleBits M)).any (bitAt · i) =
      (fns.map fun e => AverCert.ClaimAxes.wCallsL (fnCode M e.plan).body).flatten.contains a := by
  rw [contains_flatten, List.any_map, List.any_map]
  refine congrArg (List.any fns) (funext fun e => ?_)
  simp only [Function.comp_def, roleBits, bitAt_bitsOf, hi]

/-- The per-plan role bits give the claim axes. -/
theorem checked_of_bits {artifact : ArtifactData} {rs : List Nat}
    (hbits : artifact.manifest.fnPlans.map (roleBits (mctxOf artifact.manifest.subject
      artifact.manifest.types artifact.manifest.fnPlans)) = rs)
    (h : checkedBits artifact rs = true) : AverCert.ClaimAxes.checked artifact = true := by
  simp only [checkedBits, Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨-, h⟩ := h
  unfold AverCert.ClaimAxes.checked AverCert.ClaimAxes.contractsMatch
    AverCert.ClaimAxes.requiredContracts
  rw [beq_iff_eq, ← h]
  congr 1
  subst hbits
  simp only [AverCert.ClaimAxes.contractUse, AverCert.ClaimAxes.usedCalls, useOfBits,
    helperCalls, List.contains_append, Bool.or_assoc]
  congr 1 <;>
    (rw [any_bit]; intro e; simp only [roleCalls, List.getD_cons_zero, List.getD_cons_succ])

end AverCert.ScaleLayout
