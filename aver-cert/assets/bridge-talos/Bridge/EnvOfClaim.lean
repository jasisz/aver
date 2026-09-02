import Bridge.Config

/-!
# Spike (c): `envOfClaim` is a projection of the declared envelope

The translation environment is built from three pieces of DECLARED claim data
and nothing else — no artifact byte is read here:

* `hostTable : List (HostRole × Nat)` — the claim's host-role table (the same
  list `PlanCheck.encodeSymRawPlan…` resolves `hostCall` roles with, bound to
  the byte-derived role table by `StandardFace.hostTableBound`);
* `carrier : Nat` — the claim's Int carrier struct index (bound to the decoded
  `CertDecode.carrierState`);
* the certified record declarations `TypeDecl` of the certificate's
  `typedecl-v1` claims (bound to the artifact's type section by
  `WasmSlice.typeSectionMatches (lowerTypeDecl carrier … decl = some ·)`,
  StandardFace.lean:816/1114).

Two lemmas make the projection precise:

* **host half** — every import of the synthetic module is an entry of the
  host-role table, with the role's fixed signature, and conversely the index
  the wall's own `hostRoleIdx?` resolves a role to is that import (given
  distinct indices in the table);
* **struct half** — the environment's field sorts for a certified record are
  the sort projection of exactly the type-section entry `lowerTypeDecl`
  produces for it, i.e. of the entry the wall pins by equality.

What the declared envelope does NOT carry (found while writing this file, see
README "Step 7"): a symbolic-fragment claim names user structs only by
`structTable : List (String × Nat)` (name → index). The FIELD LAYOUT of a
struct index the plan constructs or projects comes from a separate
`typedecl-v1` record claim of the same certificate; a struct index cited by a
compute plan but by no record claim has no declared layout at all, and
`envOfClaim` then has no entry for it (`translate` refuses its `structNew`).
-/

namespace Bridge
open CertPrelude AverCert.Schema AverCert.Schema.Lowering AverCert.PlanCheck

/-! ## Struct half -/

/-- Sort of a wall-lowered scalar field storage (`lowerScalarStorage`):
    `ref null C` holds a carrier (a canonical one, by the compute face's domain
    representation), `i32` (`0x7f`) and `f64` (`0x7c`) are numeric. -/
def sortOfStorage : CertDecode.FieldType → Option STy
  | ⟨.val (.ref 0x63 _), _⟩ => some .car
  | ⟨.val (.numeric 0x7f), _⟩ => some .i32
  | ⟨.val (.numeric 0x7c), _⟩ => some .f64
  | _ => none

theorem sortOfStorage_lowerScalarStorage (C : Nat) (f : TypeDecl) :
    (lowerScalarStorage C f).bind sortOfStorage = scalarSort f := by
  cases f <;> rfl

theorem mapM_lower_of_scalarSort (C : Nat) :
    ∀ (fields : List TypeDecl) (sorts : List STy),
      fields.mapM scalarSort = some sorts →
      ∃ fts : List CertDecode.FieldType,
        fields.mapM (lowerScalarStorage C) = some fts ∧
        fts.mapM sortOfStorage = some sorts ∧ fts.length = fields.length
  | [], sorts, h => by
      simp only [List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h
      exact ⟨[], rfl, rfl, rfl⟩
  | f :: fields, sorts, h => by
      simp only [List.mapM_cons, Option.pure_def, Option.bind_eq_bind] at h
      match hf : scalarSort f with
      | none => simp [hf] at h
      | some t =>
        simp only [hf, Option.bind_some] at h
        match hrest : fields.mapM scalarSort with
        | none => simp [hrest] at h
        | some ts =>
          simp only [hrest, Option.bind_some, Option.some.injEq] at h
          subst h
          obtain ⟨fts, hl, hs, hlen⟩ := mapM_lower_of_scalarSort C fields ts hrest
          have hpt := sortOfStorage_lowerScalarStorage C f
          rw [hf] at hpt
          match hlf : lowerScalarStorage C f with
          | none => simp [hlf] at hpt
          | some ft =>
            rw [hlf] at hpt
            simp only [Option.bind_some] at hpt
            refine ⟨ft :: fts, ?_, ?_, by simp [hlen]⟩
            · simp [List.mapM_cons, hlf, hl]
            · simp [List.mapM_cons, hpt, hs]

/-- Composition, struct half. -/
theorem declEntry?_lowerTypeDecl (C : Nat) (decl : TypeDecl) (idx : Nat) (sorts : List STy)
    (h : declEntry? decl = some (idx, sorts)) :
    ∃ (fields : List TypeDecl) (fts : List CertDecode.FieldType),
      decl = .record idx fields ∧ checkRecordDecl decl = true ∧
      lowerTypeDecl C 1 decl = some ⟨.plain, .structType fts⟩ ∧
      fts.mapM sortOfStorage = some sorts ∧ fts.length = fields.length := by
  match decl, h with
  | .record idx' fields, h =>
    simp only [declEntry?] at h
    split at h
    · rename_i hchk
      match hm : fields.mapM scalarSort with
      | none => simp [hm] at h
      | some ts =>
        simp only [hm, Option.map_some, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        obtain ⟨fts, hl, hs, hlen⟩ := mapM_lower_of_scalarSort C fields ts hm
        exact ⟨fields, fts, rfl, hchk, by simp [lowerTypeDecl, hl], hs, hlen⟩
    · simp at h
  | .intCarrier, h => simp [declEntry?] at h
  | .boolScalar, h => simp [declEntry?] at h
  | .floatScalar, h => simp [declEntry?] at h
  | .variant _ _ _, h => simp [declEntry?] at h

/-- The carrier entry of `envOfClaim` is the fixed `{i64, ref, i32}` layout. -/
theorem envOfClaim_carrier (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl) :
    structSorts? (envOfClaim ht C decls).structs C = some [.i64, .ref, .i32] := by
  simp [envOfClaim, structSorts?]

/-- Every other struct entry of `envOfClaim` comes from a certified record
    declaration in the list (first match). -/
theorem envOfClaim_struct_of_decl (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl)
    (ty : Nat) (fs : List STy) (hne : ty ≠ C)
    (h : structSorts? (envOfClaim ht C decls).structs ty = some fs) :
    ∃ decl ∈ decls, declEntry? decl = some (ty, fs) := by
  simp only [envOfClaim, structSorts?, if_neg (Ne.symm hne)] at h
  induction decls with
  | nil => simp [structSorts?] at h
  | cons d ds ih =>
    simp only [List.filterMap_cons] at h
    split at h
    · exact (ih h).imp fun d' hd' => ⟨List.mem_cons_of_mem _ hd'.1, hd'.2⟩
    · rename_i i fs' hd
      simp only [structSorts?] at h
      split at h
      · rename_i heq
        subst heq
        simp only [Option.some.injEq] at h
        subst h
        exact ⟨d, List.mem_cons_self, hd⟩
      · exact (ih h).imp fun d' hd' => ⟨List.mem_cons_of_mem _ hd'.1, hd'.2⟩

/-! ## Host half -/

theorem envOfClaim_import_role (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl)
    (f i : Nat) (sig : ImportSig)
    (h : slotLookup? (envOfClaim ht C decls).imports f = some (i, sig)) :
    ∃ role, ht[i]? = some (role, f) ∧ sig = ⟨f, (roleSig role).1, (roleSig role).2⟩ := by
  obtain ⟨hget, hslot⟩ := slotLookup?_getElem h
  simp only [envOfClaim, List.getElem?_map] at hget
  match hht : ht[i]? with
  | none => simp [hht] at hget
  | some (role, f') =>
    simp only [hht, Option.map_some, Option.some.injEq] at hget
    subst hget
    simp only at hslot
    subst hslot
    exact ⟨role, rfl, rfl⟩

theorem hostRoleIdx?_mem :
    ∀ (ht : List (HostRole × Nat)) (role : HostRole) (f : Nat),
      hostRoleIdx? ht role = some f → (role, f) ∈ ht
  | [], _, _, h => by simp [hostRoleIdx?] at h
  | (r, idx) :: rest, role, f, h => by
      simp only [hostRoleIdx?] at h
      split at h
      · rename_i heq
        subst heq
        simp only [Option.some.injEq] at h
        subst h
        exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (hostRoleIdx?_mem rest role f h)

/-- Composition, host half (converse): the index the wall resolves a role to
    is an import of the synthetic module with that role's signature. Needs
    distinct indices in the table (two roles bound to one function would make
    the first-match lookups disagree). -/
theorem hostRoleIdx?_slotLookup (C : Nat) (decls : List TypeDecl) (role : HostRole) :
    ∀ (ht : List (HostRole × Nat)) (f : Nat),
      (ht.map Prod.snd).Nodup → hostRoleIdx? ht role = some f →
      ∃ i, slotLookup? (envOfClaim ht C decls).imports f =
          some (i, ⟨f, (roleSig role).1, (roleSig role).2⟩) ∧ ht[i]? = some (role, f)
  | [], _, _, h => by simp [hostRoleIdx?] at h
  | (r, idx) :: rest, f, hnd, h => by
      simp only [List.map_cons, List.nodup_cons] at hnd
      simp only [hostRoleIdx?] at h
      split at h
      · rename_i heq
        subst heq
        simp only [Option.some.injEq] at h
        subst h
        exact ⟨0, by simp [envOfClaim, slotLookup?], rfl⟩
      · obtain ⟨i, hi, hget⟩ := hostRoleIdx?_slotLookup C decls role rest f hnd.2 h
        have hmem : f ∈ rest.map Prod.snd :=
          List.mem_map.mpr ⟨(role, f), hostRoleIdx?_mem rest role f h, rfl⟩
        have hne : idx ≠ f := fun heq => hnd.1 (heq ▸ hmem)
        refine ⟨i + 1, ?_, by simpa using hget⟩
        simp only [envOfClaim, List.map_cons, slotLookup?, hne, if_false]
        simp only [envOfClaim] at hi
        simp [hi]

/-! ## The k5 instance (declared data of `Domain_Rational_*`, `scratchpad/k5b/cert`) -/

def k5HostTable : List (HostRole × Nat) :=
  [(.box, 23), (.add, 24), (.mul, 26), (.sub, 25), (.toIndex, 35), (.cmp, 30), (.eq, 31)]

def k5FractionDecl : TypeDecl := .record 0 [.intCarrier, .intCarrier]

def k5Env : TranslateEnv := envOfClaim k5HostTable 3 [k5FractionDecl]

example : k5Env.structs = [(3, [.i64, .ref, .i32]), (0, [.car, .car])] := by decide
example : k5Env.imports.map (·.slot) = [23, 24, 26, 25, 35, 30, 31] := by decide
example : slotLookup? k5Env.imports 24 = some (1, ⟨24, [.car, .car], .car⟩) := by decide
example : lowerTypeDecl 3 1 k5FractionDecl =
    some ⟨.plain, .structType [⟨.val (.ref 0x63 3), 0⟩, ⟨.val (.ref 0x63 3), 0⟩]⟩ := by decide

end Bridge
