-- The certified closure from declared callee lists, each checked on its
-- function's chunk window.
import ScaleTypes

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleClosure
open CertDecode AverCert.ScaleBytes AverCert.ScaleLayout AverCert.DeclaredLayout
open AverCert.SortedKeys AverCert.AcceptedArtifact

/-! ### Declared callee lists

`closureIsolation` folds a work list from the certified roots: each function
not seen before has its code entry scanned for direct calls
(`WasmSlice.scanClosureCodeEntry`), and its callees go in front of the work
list. The producer declares, in the order the fold first meets them, every
function the fold scans with its callee list. Each declared list is checked
against its function's scan on the function's own chunk window
(`calleesOk`, a block of functions per declaration), and the fold is run over
the declared lists (`foldSeq`), which requires each newly met function to be
the next one declared and every declaration to be used. `foldB_of_seq` shows
that the fold over the declared lists returns what the fold over the scans
returns, so a wrong or missing list, a list for another function, or a
declaration the fold never meets makes a check fail. -/

/-- The code entry of function index `f` on its chunk window. -/
def entryW (cs : List Nat) (L : Layout) (f : Nat) : Option AverCert.WasmSlice.ByteSeq :=
  if L.imports ≤ f ∧ f - L.imports < L.count then
    some (takeBytes (L.len (f - L.imports))
      (window 1024 cs (L.off (f - L.imports)) (L.len (f - L.imports))))
  else none

theorem entryW_eq {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) (L : Layout) (f : Nat) :
    entryW cs L f = L.entryAt n f := by
  unfold entryW Layout.entryAt Layout.entry Layout.entryN
  rw [window_eq (by decide) hfit, hn, slice_eq_isolate]

/-- Every declared function's scan, on its window, is its declared list. -/
def calleesOk (cs : List Nat) (L : Layout) (ds : List (Nat × List Nat)) : Bool :=
  ds.all fun d => (entryW cs L d.1).bind AverCert.WasmSlice.scanClosureCodeEntry == some d.2

theorem calleesOk_append {cs : List Nat} {L : Layout} {d1 d2 : List (Nat × List Nat)}
    (h1 : calleesOk cs L d1 = true) (h2 : calleesOk cs L d2 = true) :
    calleesOk cs L (d1 ++ d2) = true := by
  unfold calleesOk at *
  rw [List.all_append, h1, h2]; rfl

theorem calleesOk_spec {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {L : Layout} {ds : List (Nat × List Nat)}
    (h : calleesOk cs L ds = true) :
    ∀ d ∈ ds, (L.entryAt n d.1).bind AverCert.WasmSlice.scanClosureCodeEntry = some d.2 := by
  intro d hd
  unfold calleesOk at h
  rw [List.all_eq_true] at h
  have := h d hd
  rw [beq_iff_eq, entryW_eq hn hfit] at this
  exact this

/-- `SortedKeys.closureFoldB` over declared callee lists, consumed in the
    order the fold meets new functions; every declaration must be used. -/
def foldSeq : Nat → List Nat → List Nat → Nat → List (Nat × List Nat) → Option (List Nat)
  | 0, [], seen, _, ds => if ds.isEmpty then some seen else none
  | 0, _ :: _, _, _, _ => none
  | _ + 1, [], seen, _, ds => if ds.isEmpty then some seen else none
  | fuel + 1, func :: work, seen, bits, ds =>
      if bits.testBit func then foldSeq fuel work seen bits ds
      else
        match ds with
        | (g, callees) :: ds' =>
            if g == func then
              foldSeq fuel (callees ++ work) (func :: seen) (bits ||| (1 <<< func)) ds'
            else none
        | [] => none

/-- The fold over the declared lists is the fold over the scans, when every
    declared list is its function's scan. -/
theorem foldB_of_seq (look : Nat → Option AverCert.WasmSlice.ByteSeq) :
    ∀ (fuel : Nat) (work seen : List Nat) (bits : Nat) (ds : List (Nat × List Nat))
      (actual : List Nat),
      foldSeq fuel work seen bits ds = some actual →
      (∀ d ∈ ds, (look d.1).bind AverCert.WasmSlice.scanClosureCodeEntry = some d.2) →
      closureFoldB look fuel work seen bits = some actual
  | 0, [], seen, bits, ds, actual, h, _ => by
      simp only [foldSeq] at h
      split at h
      · exact h
      · cases h
  | 0, _ :: _, _, _, _, _, h, _ => by simp [foldSeq] at h
  | _ + 1, [], seen, bits, ds, actual, h, _ => by
      simp only [foldSeq] at h
      split at h
      · exact h
      · cases h
  | fuel + 1, func :: work, seen, bits, ds, actual, h, hds => by
      simp only [foldSeq] at h
      simp only [closureFoldB]
      split
      · rename_i hb
        simp only [hb, ↓reduceIte] at h
        exact foldB_of_seq look fuel work seen bits ds actual h hds
      · rename_i hb
        simp only [hb, Bool.false_eq_true, ↓reduceIte] at h
        match ds, h, hds with
        | (g, callees) :: ds', h, hds =>
            simp only at h
            split at h
            · rename_i hg
              rw [beq_iff_eq] at hg
              subst hg
              have hscan := hds (g, callees) List.mem_cons_self
              simp only at hscan
              rw [hscan]
              exact foldB_of_seq look fuel _ _ _ ds' actual h
                (fun d hd => hds d (List.mem_cons_of_mem _ hd))
            · cases h
        | [], h, _ => cases h

/-- A set of function indices as the bits of one numeral. -/
def natBits : List Nat → Nat
  | [] => 0
  | x :: xs => natBits xs ||| (1 <<< x)

theorem natBits_testBit : ∀ (xs : List Nat) (y : Nat), (natBits xs).testBit y = true ↔ y ∈ xs
  | [], y => by simp [natBits]
  | x :: xs, y => by
      rw [natBits, Nat.testBit_or, Nat.shiftLeft_eq, Nat.one_mul, Nat.testBit_two_pow,
        Bool.or_eq_true, natBits_testBit xs y, List.mem_cons, decide_eq_true_eq]
      constructor
      · rintro (h | rfl)
        · exact Or.inr h
        · exact Or.inl rfl
      · rintro (rfl | h)
        · exact Or.inr rfl
        · exact Or.inl h

/-- No root is a helper, read on the helpers' bits. -/
theorem rootsApart_of_bits {roots helpers : List Nat}
    (h : roots.all (fun r => !(natBits helpers).testBit r) = true) :
    roots.all (fun root => !AverCert.WasmSlice.natMem root helpers) = true := by
  rw [List.all_eq_true] at h ⊢
  intro r hr
  have := h r hr
  simp only [Bool.not_eq_true', Bool.not_eq_eq_eq_not, Bool.not_true] at this ⊢
  cases hm : AverCert.WasmSlice.natMem r helpers
  · rfl
  · rw [natMem_iff] at hm
    have := (natBits_testBit helpers r).mpr hm
    simp_all

/-- `SortedKeys.closureIsolationS` with the fold over declared callee lists. -/
def closureIsolationD (artifact : ArtifactData) (ds : List (Nat × List Nat)) : Bool :=
  let claim := artifact.closureClaim
  let certified := artifact.manifest.obligations.map (fun obligation => obligation.self)
  strictly (sortedOr claim.roots) &&
  strictly (sortedOr claim.helpers) &&
  strictly (sortedOr claim.admitted) &&
  setEqSorted claim.roots certified &&
  claim.roots.all (fun r => !(natBits claim.helpers).testBit r) &&
  setEqSorted claim.admitted (claim.roots ++ claim.helpers) &&
  AverCert.WasmSlice.noSharedMemory artifact.modBytes artifact.modLen &&
  match foldSeq artifact.closureFuel claim.roots [] 0 ds with
  | some actual => setEqSorted actual claim.admitted
  | none => false

theorem closureIsolationS_of_D {artifact : ArtifactData} {L : Layout}
    {ds : List (Nat × List Nat)}
    (hds : ∀ d ∈ ds, (L.entryAt artifact.modBytes d.1).bind
      AverCert.WasmSlice.scanClosureCodeEntry = some d.2)
    (h : closureIsolationD artifact ds = true) : closureIsolationS artifact L = true := by
  unfold closureIsolationD at h
  unfold closureIsolationS
  simp only [Bool.and_eq_true] at h ⊢
  obtain ⟨⟨⟨⟨h1, h5⟩, h6⟩, h7⟩, h8⟩ := h
  refine ⟨⟨⟨⟨h1, rootsApart_of_bits h5⟩, h6⟩, h7⟩, ?_⟩
  split at h8
  · rename_i actual hact
    rw [foldB_of_seq _ _ _ _ _ _ actual hact hds]
    exact h8
  · cases h8

/-- The closure isolation from the declared callee lists, each checked on its
    function's chunk window, and the fold over them. -/
theorem closureIsolation_of_callees {artifact : ArtifactData} {cs : List Nat} {L : Layout}
    {ds : List (Nat × List Nat)}
    (hn : join 1024 cs = artifact.modBytes) (hfit : chunksFit 1024 cs = true)
    (hL : layoutConfirmed artifact.modBytes artifact.modLen L = true)
    (hc : calleesOk cs L ds = true) (h : closureIsolationD artifact ds = true) :
    closureIsolation artifact = true :=
  closureIsolation_of_layout hL
    (closureIsolationL_of_S (closureIsolationS_of_D (calleesOk_spec hn hfit hc) h))

end AverCert.ScaleClosure
