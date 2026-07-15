/-
ACCEPTANCE-SOUNDNESS CORE.

This file assembles the structural heart against the REAL audited types:
`holdsCore_of_claims` reduces `HoldsCore m` (∀ manifest obligation, it holds)
to the per-claim obligations holding, via the coverage bijection
(`manifestObligationsClaimed` + `fragmentClaimObligationsInManifest` +
export-name uniqueness). Each family's generic soundness theorem discharges
its slice of the per-claim hypothesis; the capstone wires all ten families.
-/
import AcceptedArtifactCore
import ExprFragmentSoundness

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact

namespace AcceptanceSoundness

/-- Per-obligation denotation selected by policy — the body of `HoldsCore`. -/
def obligationHolds (o : Obligation) : Prop :=
  match o.policy with
  | .simulatesModel => o.holds
  | .simulatesModelTotally => o.holdsTotal

/-- `HoldsCore` restated pointwise over the manifest obligation list. -/
theorem holdsCore_iff (m : Manifest) :
    HoldsCore m ↔ ∀ o ∈ m.obligations, obligationHolds o := by
  constructor <;> intro h o ho <;> exact h o ho

/-- Membership reflection for the audited `foldl insert` set index. -/
private theorem mem_foldl_insert (xs : List String) (s : Std.TreeSet String)
    (x : String)
    (h : x ∈ xs.foldl (fun set value => set.insert value) s) :
    x ∈ s ∨ x ∈ xs := by
  induction xs generalizing s with
  | nil => exact Or.inl h
  | cons y ys ih =>
      have h' := ih (s := s.insert y) h
      rcases h' with hset | hys
      · rw [Std.TreeSet.mem_insert] at hset
        simp only [Std.LawfulEqCmp.compare_eq_iff_eq] at hset
        rcases hset with hyx | hs
        · exact Or.inr (by simp [hyx])
        · exact Or.inl hs
      · exact Or.inr (by simp [hys])

private theorem mem_of_orderedSet_contains (xs : List String) (x : String)
    (h : (AverCert.WasmSlice.orderedSet xs).contains x = true) : x ∈ xs := by
  have hm := Std.TreeSet.contains_iff_mem.mp h
  rcases mem_foldl_insert xs Std.TreeSet.empty x hm with hempty | hxs
  · exact (Std.TreeSet.not_mem_emptyc hempty).elim
  · exact hxs

/-- A successful `uniqueMap` contains the key of every source entry. -/
private theorem uniqueMap_mem_key {β : Type u} (entries : List (String × β))
    (index : Std.TreeMap String β)
    (hu : AverCert.WasmSlice.uniqueMap entries = some index)
    (entry : String × β) (he : entry ∈ entries) : entry.1 ∈ index := by
  induction entries generalizing index with
  | nil => simp at he
  | cons head tail ih =>
      cases htail : AverCert.WasmSlice.uniqueMap tail with
      | none => simp [AverCert.WasmSlice.uniqueMap, htail] at hu
      | some tailIndex =>
          by_cases hc : tailIndex.contains head.1 = true
          · simp [AverCert.WasmSlice.uniqueMap, htail, hc] at hu
          · simp only [AverCert.WasmSlice.uniqueMap, htail, hc,
              Bool.false_eq_true, if_false, Option.some.injEq] at hu
            subst index
            simp only [List.mem_cons] at he
            rcases he with rfl | he
            · exact Std.TreeMap.mem_insert_self
            · exact Std.TreeMap.mem_insert.mpr (Or.inr (ih tailIndex htail he))

/-- Unique export keys make `find?` return any listed obligation at its key. -/
private theorem find?_eq_some_of_uniqueMap {α : Type u} (xs : List α)
    (key : α → String)
    (hunique : (AverCert.WasmSlice.uniqueMap
      (xs.map fun x => (key x, x))).isSome = true)
    (x : α) (hx : x ∈ xs) :
    xs.find? (fun y => key y = key x) = some x := by
  induction xs with
  | nil => simp at hx
  | cons head tail ih =>
      cases htail : AverCert.WasmSlice.uniqueMap
          (tail.map fun y => (key y, y)) with
      | none => simp [AverCert.WasmSlice.uniqueMap, htail] at hunique
      | some tailIndex =>
          have hnot : tailIndex.contains (key head) = false := by
            cases hc : tailIndex.contains (key head) with
            | false => rfl
            | true =>
                simp [AverCert.WasmSlice.uniqueMap, htail, hc] at hunique
          simp only [List.mem_cons] at hx
          rcases hx with rfl | hx
          · simp
          · have hne : key head ≠ key x := by
              intro heq
              have hmem : (key x, x) ∈ tail.map (fun y => (key y, y)) := by
                exact List.mem_map.mpr ⟨x, hx, rfl⟩
              have hkey : key x ∈ tailIndex :=
                uniqueMap_mem_key _ _ htail _ hmem
              have hcontains : tailIndex.contains (key x) = true :=
                Std.TreeMap.mem_iff_contains.mp hkey
              rw [← heq, hnot] at hcontains
              contradiction
            have htailUnique : (AverCert.WasmSlice.uniqueMap
                (tail.map fun y => (key y, y))).isSome = true := by
              simp [htail]
            simpa [hne] using ih htailUnique hx

/-- The recursive claims-in-manifest predicate exposes the equation for any
listed claim obligation. -/
private theorem find?_eq_claim_of_mem (manifest claims : List Obligation)
    (h : claimObligationsInManifest manifest claims)
    (o : Obligation) (ho : o ∈ claims) :
    manifest.find? (fun candidate => candidate.export_ = o.export_) = some o := by
  induction claims with
  | nil => simp at ho
  | cons head tail ih =>
      simp only [claimObligationsInManifest] at h
      rcases h with ⟨hhead, htail⟩
      simp only [List.mem_cons] at ho
      rcases ho with rfl | ho
      · exact hhead
      · exact ih htail ho

/--
STRUCTURAL HEART of the master. If every manifest obligation is claimed
(`manifestObligationsClaimed`), every claim's obligation is found-in-manifest
and equal (`fragmentClaimObligationsInManifest`), obligation export names are
unique, and every CLAIMED obligation holds, then `HoldsCore` holds for the
whole manifest.

The bijection: a manifest obligation `o` has its export in the claimed set
(hCover); some claim carries that export; that claim's obligation is found in
the manifest by export and equals what `find?` returns; uniqueness of export
names forces that found obligation to be `o` itself; hence `o` is (equal to) a
claimed obligation and `hClaims` applies.
-/
theorem holdsCore_of_claims
    (artifact : ArtifactData)
    (hCover : manifestObligationsClaimed artifact = true)
    (hInManifest : fragmentClaimObligationsInManifest artifact)
    (hUnique : manifestObligationExportsUnique artifact = true)
    (hClaims : ∀ o ∈ claimObligations artifact, obligationHolds o) :
    HoldsCore artifact.manifest := by
  rw [holdsCore_iff]
  intro o ho
  -- hCover: o.export_ is in the claimed-export set.
  have hclaimed : (AverCert.WasmSlice.orderedSet
      (claimObligationExports artifact)).contains o.export_ = true := by
    have := hCover
    simp only [manifestObligationsClaimed] at this
    exact (List.all_eq_true.mp this) o ho
  -- The claimed exports are exactly the export_ of claimObligations.
  have hexp : o.export_ ∈ (claimObligations artifact).map (·.export_) := by
    have hraw := mem_of_orderedSet_contains _ _ hclaimed
    simpa only [claimObligationExports, claimObligations, List.map_append,
      List.map_map, Function.comp_def] using hraw
  -- Some claim obligation shares o's export; fragmentClaimObligationsInManifest
  -- + uniqueness pin it to o; then hClaims closes.
  rcases List.mem_map.mp hexp with ⟨claimed, hclaimedMem, hExport⟩
  have hClaimFind := find?_eq_claim_of_mem
    artifact.manifest.obligations (claimObligations artifact)
    hInManifest claimed hclaimedMem
  rw [hExport] at hClaimFind
  have hManifestFind : artifact.manifest.obligations.find?
      (fun candidate => candidate.export_ = o.export_) = some o := by
    apply find?_eq_some_of_uniqueMap artifact.manifest.obligations
      (fun obligation : Obligation => obligation.export_)
    · simpa only [manifestObligationExportsUnique] using hUnique
    · exact ho
  rw [hManifestFind] at hClaimFind
  have hClaimedEq : claimed = o := (Option.some.inj hClaimFind).symm
  simpa only [hClaimedEq] using hClaims claimed hclaimedMem

/--
MASTER TARGET (the capstone). The byte-acceptance conjuncts of the production
`accepted` predicate, MINUS the asserted `Holds`, plus obligation coverage,
imply `Holds` — i.e. `Holds` is derivable, not asserted.

Stated here as the goal; its proof is `holdsCore_of_claims` (structural heart,
above) composed with the ten per-family discharges of `hClaims` from
`acceptedFragments` + each family's generic soundness theorem, and the hash
conjunct from the artifact-hash pin. See VERDICT.md for the enumerated residual.
-/
def holdsAtHash (wasmSha256 : String) (m : Manifest) : Prop :=
  m.subject.artifactHash = wasmSha256 ∧ HoldsCore m

/-- Artifact-independent form of the production target.  Supplying the
artifact's audited wasm hash recovers the thin `Schema.Holds` shim without
importing generated `Module.lean`. -/
def masterTarget (wasmSha256 : String) (artifact : ArtifactData) : Prop :=
  subjectMatchesArtifactRoot artifact →
  fragmentClaimObligationsInManifest artifact →
  claimsMatchManifest artifact →
  decodedNonExprFacts artifact →
  acceptedFragments artifact →
  manifestObligationsClaimed artifact = true →
  holdsAtHash wasmSha256 artifact.manifest

end AcceptanceSoundness
