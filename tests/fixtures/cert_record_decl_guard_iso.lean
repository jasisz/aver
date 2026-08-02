import StandardFace

open CertPrelude AverCert AverCert.Schema AverCert.AcceptedArtifact
set_option maxRecDepth 300000

-- TEMPLATE, not standalone Lean. `record_type_declaration_pin_is_isolated_and_
-- weaken_confirmed` in `tests/cert_whole_module_guard_iso.rs` substitutes every
-- percent-delimited placeholder: the module byte numerals come from the REAL
-- compiled `person.wasm` and its byte-tampered/synthetic variants, the layout
-- indices are read back out of that artifact (never written down as literals),
-- and the three weakened copies are cut from the LIVE materialized
-- `StandardFace.lean` with exactly-once surgery on the conjunct being tested.

-- REAL compiled person.wasm plus its byte-tampered variants.
def personBytes : Nat := %personBytes%
def personLen : Nat := %personLen%
def subBytes : Nat := %subBytes%
def subLen : Nat := %subLen%
def dupBytes : Nat := %dupBytes%
def dupLen : Nat := %dupLen%
def pseudoHonestBytes : Nat := %pseudoHonestBytes%
def pseudoHonestLen : Nat := %pseudoHonestLen%
def pseudoHostileBytes : Nat := %pseudoHostileBytes%
def pseudoHostileLen : Nat := %pseudoHostileLen%

def readMemberName : AverCert.WasmSlice.ByteSeq :=
  %readMemberName%

def personFields : List TypeDecl := [.intCarrier, .boolScalar]
def permFields : List TypeDecl := [.boolScalar, .intCarrier]
def extFields : List TypeDecl := [.intCarrier, .boolScalar, .boolScalar]

def personEntry : CertDecode.TypeEntry :=
  ⟨.plain, .structType [⟨.val (.ref 0x63 (Int.ofNat %carrier%)), 0⟩,
                       ⟨.val (.numeric 0x7f), 0⟩]⟩
def subEntry : CertDecode.TypeEntry :=
  ⟨.sub [], .structType [⟨.val (.ref 0x63 (Int.ofNat %carrier%)), 0⟩,
                        ⟨.val (.numeric 0x7f), 0⟩]⟩
def pseudoPersonEntry : CertDecode.TypeEntry :=
  ⟨.plain, .structType [⟨.val (.ref 0x63 (Int.ofNat 1)), 0⟩,
                       ⟨.val (.numeric 0x7f), 0⟩]⟩

/-- The record plan reading `field` of struct `si` as `ty`. -/
def projPlan (si field : Nat) (ty : FragTy) : ExprFragmentRawPlan :=
  ⟨"expr-fragment-v1", [.adtRef], ty,
    ⟨[⟨0, .adtRef, .local 0⟩, ⟨1, ty, .structGetUser si field 0⟩], 1⟩⟩

def honestPlan : ExprFragmentRawPlan := projPlan %structIdx% 1 .boolI32
def permPlan : ExprFragmentRawPlan := projPlan %structIdx% 0 .boolI32
def extPlan : ExprFragmentRawPlan := projPlan %structIdx% 1 .boolI32
def dupPlan : ExprFragmentRawPlan := projPlan %dupIdx% 1 .boolI32
def pseudoPlan : ExprFragmentRawPlan := projPlan 2 1 .boolI32

def honestSym : SymRawPlan :=
  ⟨"sym-fragment-v1", [.named "Person"], .bool,
    ⟨[⟨0, .named "Person", .param 0⟩,
      ⟨1, .bool, .projectField "Person" 1 .bool 0⟩], 1⟩⟩

/-- Face-level obligation over declaration fields `fs`, projected `field`, at
    struct `si` and carrier `c`. -/
def probeOb (c si : Nat) (fs : List TypeDecl) (field : Nat)
    (hf : field < fs.length) : Obligation :=
  { export_ := "readMember", policy := .simulatesModel, carrier := c,
    code := fun _ => none, host := AverCert.StandardFace.emptyHost, self := 0,
    Dom := RecordFields fs, Cod := RecordVal (fs[field]'hf),
    domRepr := AverCert.StandardFace.recordParamDomRepr c si fs,
    codRepr := AverCert.StandardFace.recordParamCodRepr c fs field hf,
    model := AverCert.StandardFace.recordParamModel fs field hf }

def probeClaim (c si : Nat) (fs : List TypeDecl) (field : Nat)
    (hf : field < fs.length) : SymFragmentClaim :=
  { exportNameBytes := readMemberName, exportName := "readMember", carrier := c,
    hostTable := [], structTable := [("Person", si)], plan := honestSym,
    obligation := probeOb c si fs field hf }

def honestClaim : SymFragmentClaim := probeClaim %carrier% %structIdx% personFields 1 (by decide)
def permClaim : SymFragmentClaim := probeClaim %carrier% %structIdx% permFields 0 (by decide)
def extClaim : SymFragmentClaim := probeClaim %carrier% %structIdx% extFields 1 (by decide)
def dupClaim : SymFragmentClaim := probeClaim %carrier% %dupIdx% personFields 1 (by decide)
def pseudoHostileClaim : SymFragmentClaim := probeClaim 1 2 personFields 1 (by decide)
def pseudoHonestClaim : SymFragmentClaim := probeClaim 0 2 personFields 1 (by decide)

-- ==== Conjunct-level byte ground truth ====

-- Entry-equality pins for the decoded entries the elimination lemma consumes.
example : AverCert.WasmSlice.typeSectionMatches (fun x => x == personEntry)
    personBytes personLen %structIdx% = true := by native_decide
example : AverCert.WasmSlice.typeSectionMatches (fun x => x == subEntry)
    subBytes subLen %structIdx% = true := by native_decide
example : AverCert.WasmSlice.typeSectionMatches (fun x => x == personEntry)
    dupBytes dupLen %dupIdx% = true := by native_decide
example : AverCert.WasmSlice.typeSectionMatches (fun x => x == pseudoPersonEntry)
    pseudoHostileBytes pseudoHostileLen 2 = true := by native_decide

-- The tampered/synthetic modules keep the surrounding byte facts the honest
-- module has: the carrier state and the param binding still decode.
example : CertDecode.carrierState personBytes personLen = some (some %carrier%) := by native_decide
example : CertDecode.carrierState subBytes subLen = some (some %carrier%) := by native_decide
example : CertDecode.carrierState dupBytes dupLen = some (some %carrier%) := by native_decide
example : CertDecode.carrierState pseudoHostileBytes pseudoHostileLen
    = some (some 0) := by native_decide
example : CertDecode.carrierState pseudoHonestBytes pseudoHonestLen
    = some (some 0) := by native_decide
example : AverCert.WasmSlice.recordParamFuncTypeMatches personBytes personLen
    readMemberName %structIdx% = true := by native_decide
example : AverCert.WasmSlice.recordParamFuncTypeMatches subBytes subLen
    readMemberName %structIdx% = true := by native_decide
example : AverCert.WasmSlice.recordParamFuncTypeMatches dupBytes dupLen
    readMemberName %structIdx% = true := by native_decide
example : AverCert.WasmSlice.recordParamFuncTypeMatches dupBytes dupLen
    readMemberName %dupIdx% = false := by native_decide
example : AverCert.WasmSlice.recordParamFuncTypeMatches pseudoHostileBytes
    pseudoHostileLen readMemberName 2 = true := by native_decide

-- The dup entry satisfies the equality pin at the duplicated index; the sub
-- entry fails it; the permuted/extra declarations fail it on the honest bytes.
example : AverCert.WasmSlice.typeSectionMatches
    (fun e => decide (lowerTypeDecl %carrier% lowerTypeDeclFuel (.record %dupIdx% personFields) = some e))
    dupBytes dupLen %dupIdx% = true := by native_decide
example : AverCert.WasmSlice.typeSectionMatches
    (fun e => decide (lowerTypeDecl %carrier% lowerTypeDeclFuel (.record %structIdx% personFields) = some e))
    subBytes subLen %structIdx% = false := by native_decide
example : AverCert.WasmSlice.typeSectionMatches
    (fun e => decide (lowerTypeDecl %carrier% lowerTypeDeclFuel (.record %structIdx% permFields) = some e))
    personBytes personLen %structIdx% = false := by native_decide
example : AverCert.WasmSlice.typeSectionMatches
    (fun e => decide (lowerTypeDecl %carrier% lowerTypeDeclFuel (.record %structIdx% extFields) = some e))
    personBytes personLen %structIdx% = false := by native_decide

-- ==== The weakened copies (live-source cuts) ====

namespace AverCert.StandardFace

/-! Literal copy of the live face, weakened by EXACTLY the type-section
    equality pin. -/
%weakPin%

/-! Literal copy of the live face, weakened by EXACTLY the param binding. -/
%weakParam%

/-! Literal copy of the live face, weakened by EXACTLY the carrier binding. -/
%weakCarrier%

end AverCert.StandardFace

-- ==== Honest control: the REAL face accepts, and so does every weakened copy ====

theorem honestFace :
    AverCert.StandardFace.recordParamDeclaredFace personBytes personLen
      honestClaim honestPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% personFields, 0, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

example : AverCert.StandardFace.weakPinRecordParamDeclaredFace personBytes personLen
    honestClaim honestPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% personFields, 0, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩
example : AverCert.StandardFace.weakParamRecordParamDeclaredFace personBytes personLen
    honestClaim honestPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% personFields, 0, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩
example : AverCert.StandardFace.weakCarrierRecordParamDeclaredFace personBytes personLen
    honestClaim honestPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% personFields, 0, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

-- ==== (a) The 0A doppelganger: entry declared `.sub` ====

theorem subRejected :
    ¬ AverCert.StandardFace.recordParamDeclaredFace subBytes subLen
      honestClaim honestPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -, -,
    hPin, -, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? honestPlan = some (%structIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ subEntry subBytes subLen %structIdx%
    hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  have := AverCert.Schema.lowerTypeDecl_plain _ _ _ _ hp
  simp [subEntry] at this

-- weakened by EXACTLY the pin: accepted (every remaining conjunct holds).
example : AverCert.StandardFace.weakPinRecordParamDeclaredFace subBytes subLen
    honestClaim honestPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% personFields, 0, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

-- EXCLUSIVE: the copies weakened by the OTHER conjuncts still reject it.
theorem subRejectedWeakParam :
    ¬ AverCert.StandardFace.weakParamRecordParamDeclaredFace subBytes subLen
      honestClaim honestPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -, -,
    hPin, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? honestPlan = some (%structIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ subEntry subBytes subLen %structIdx%
    hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  have := AverCert.Schema.lowerTypeDecl_plain _ _ _ _ hp
  simp [subEntry] at this

theorem subRejectedWeakCarrier :
    ¬ AverCert.StandardFace.weakCarrierRecordParamDeclaredFace subBytes subLen
      honestClaim honestPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -,
    hPin, -, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? honestPlan = some (%structIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ subEntry subBytes subLen %structIdx%
    hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  have := AverCert.Schema.lowerTypeDecl_plain _ _ _ _ hp
  simp [subEntry] at this

-- ==== (b) Permuted declaration over the HONEST bytes ====

theorem permRejected :
    ¬ AverCert.StandardFace.recordParamDeclaredFace personBytes personLen
      permClaim permPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, hScalar, -,
    hPin, -, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? permPlan = some (%structIdx%, 0) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  subst hfi
  -- The plan claims a Boolean field 0, so the declaration's field 0 is
  -- `.boolScalar`...
  have hbool : fields[0]'hfield = .boolScalar :=
    AverCert.Schema.scalarLeafFragTy?_boolI32 _ hScalar
  -- ...but the pin forces the declaration to lower to the REAL entry, whose
  -- field 0 is the carrier reference, which only `.intCarrier` lowers to.
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ personEntry
    personBytes personLen %structIdx% hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  obtain ⟨idx, flds, fts, hd, he, hmap⟩ :=
    AverCert.Schema.lowerTypeDecl_recordFields _ _ _ _ hp
  rw [hdecl] at hd
  injection hd with hidx hflds
  subst hflds
  have hcomp := congrArg CertDecode.TypeEntry.composite he
  have hlist : [(⟨.val (.ref 0x63 (Int.ofNat %carrier%)), 0⟩ : CertDecode.FieldType),
      ⟨.val (.numeric 0x7f), 0⟩] = fts := by injection hcomp
  subst hlist
  obtain ⟨f0, hf0, hlow⟩ := AverCert.Schema.mapM_getElem?_inv
    (lowerScalarStorage %carrier%) fields _ 0 _ hmap (by rfl)
  have hint : f0 = .intCarrier :=
    AverCert.Schema.lowerScalarStorage_ref_intCarrier 3 f0 _ _ hlow
  have hgot : fields[0]? = some (fields[0]'hfield) := List.getElem?_eq_getElem hfield
  rw [hgot] at hf0
  injection hf0 with hf0
  rw [hbool] at hf0
  rw [hint] at hf0
  simp at hf0

-- weakened by EXACTLY the pin: the permuted meaning is accepted.
example : AverCert.StandardFace.weakPinRecordParamDeclaredFace personBytes personLen
    permClaim permPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% permFields, 0, 0, permFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

-- EXCLUSIVE: the other weakened copies still reject the permuted meaning.
theorem permRejectedWeakParam :
    ¬ AverCert.StandardFace.weakParamRecordParamDeclaredFace personBytes personLen
      permClaim permPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, hScalar, -,
    hPin, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? permPlan = some (%structIdx%, 0) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  subst hfi
  have hbool : fields[0]'hfield = .boolScalar :=
    AverCert.Schema.scalarLeafFragTy?_boolI32 _ hScalar
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ personEntry
    personBytes personLen %structIdx% hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  obtain ⟨idx, flds, fts, hd, he, hmap⟩ :=
    AverCert.Schema.lowerTypeDecl_recordFields _ _ _ _ hp
  rw [hdecl] at hd
  injection hd with hidx hflds
  subst hflds
  have hcomp := congrArg CertDecode.TypeEntry.composite he
  have hlist : [(⟨.val (.ref 0x63 (Int.ofNat %carrier%)), 0⟩ : CertDecode.FieldType),
      ⟨.val (.numeric 0x7f), 0⟩] = fts := by injection hcomp
  subst hlist
  obtain ⟨f0, hf0, hlow⟩ := AverCert.Schema.mapM_getElem?_inv
    (lowerScalarStorage %carrier%) fields _ 0 _ hmap (by rfl)
  have hint : f0 = .intCarrier :=
    AverCert.Schema.lowerScalarStorage_ref_intCarrier 3 f0 _ _ hlow
  have hgot : fields[0]? = some (fields[0]'hfield) := List.getElem?_eq_getElem hfield
  rw [hgot] at hf0
  injection hf0 with hf0
  rw [hbool] at hf0
  rw [hint] at hf0
  simp at hf0

theorem permRejectedWeakCarrier :
    ¬ AverCert.StandardFace.weakCarrierRecordParamDeclaredFace personBytes personLen
      permClaim permPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, hScalar,
    hPin, -, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? permPlan = some (%structIdx%, 0) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  subst hfi
  have hbool : fields[0]'hfield = .boolScalar :=
    AverCert.Schema.scalarLeafFragTy?_boolI32 _ hScalar
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ personEntry
    personBytes personLen %structIdx% hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  obtain ⟨idx, flds, fts, hd, he, hmap⟩ :=
    AverCert.Schema.lowerTypeDecl_recordFields _ _ _ _ hp
  rw [hdecl] at hd
  injection hd with hidx hflds
  subst hflds
  have hcomp := congrArg CertDecode.TypeEntry.composite he
  have hlist : [(⟨.val (.ref 0x63 (Int.ofNat %carrier%)), 0⟩ : CertDecode.FieldType),
      ⟨.val (.numeric 0x7f), 0⟩] = fts := by injection hcomp
  subst hlist
  obtain ⟨f0, hf0, hlow⟩ := AverCert.Schema.mapM_getElem?_inv
    (lowerScalarStorage %carrier%) fields _ 0 _ hmap (by rfl)
  have hint : f0 = .intCarrier :=
    AverCert.Schema.lowerScalarStorage_ref_intCarrier 3 f0 _ _ hlow
  have hgot : fields[0]? = some (fields[0]'hfield) := List.getElem?_eq_getElem hfield
  rw [hgot] at hf0
  injection hf0 with hf0
  rw [hbool] at hf0
  rw [hint] at hf0
  simp at hf0

-- Extra-field declaration: the pin is its sole byte-side rejector (conjunct
-- ground truth above); with the pin cut, the three-field meaning is accepted
-- over the two-field module.
example : AverCert.StandardFace.weakPinRecordParamDeclaredFace personBytes personLen
    extClaim extPlan :=
  ⟨rfl, rfl, rfl, .record %structIdx% extFields, 0, 1, extFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

-- ==== (c) Param binding: duplicated entry at a fresh index ====

theorem dupRejected :
    ¬ AverCert.StandardFace.recordParamDeclaredFace dupBytes dupLen
      dupClaim dupPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -, -, -,
    hParam, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? dupPlan = some (%dupIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  exact absurd hParam (by native_decide)

example : AverCert.StandardFace.weakParamRecordParamDeclaredFace dupBytes dupLen
    dupClaim dupPlan :=
  ⟨rfl, rfl, rfl, .record %dupIdx% personFields, %dupIdx%, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

theorem dupRejectedWeakPin :
    ¬ AverCert.StandardFace.weakPinRecordParamDeclaredFace dupBytes dupLen
      dupClaim dupPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -, -,
    hParam, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? dupPlan = some (%dupIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  exact absurd hParam (by native_decide)

theorem dupRejectedWeakCarrier :
    ¬ AverCert.StandardFace.weakCarrierRecordParamDeclaredFace dupBytes dupLen
      dupClaim dupPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -,
    -, hParam, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? dupPlan = some (%dupIdx%, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  exact absurd hParam (by native_decide)

-- ==== (d) Carrier binding: Int field referencing a carrier doppelganger ====

theorem pseudoRejected :
    ¬ AverCert.StandardFace.recordParamDeclaredFace pseudoHostileBytes
      pseudoHostileLen pseudoHostileClaim pseudoPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -,
    hCarrierBind, hPin, -, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? pseudoPlan = some (2, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  -- The pin forces an Int-carrier field in the declaration (field 0 of the
  -- pinned entry is a concrete reference)...
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ pseudoPersonEntry
    pseudoHostileBytes pseudoHostileLen 2 hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  obtain ⟨idx, flds, fts, hd, he, hmap⟩ :=
    AverCert.Schema.lowerTypeDecl_recordFields _ _ _ _ hp
  have hcomp := congrArg CertDecode.TypeEntry.composite he
  have hlist : [(⟨.val (.ref 0x63 (Int.ofNat 1)), 0⟩ : CertDecode.FieldType),
      ⟨.val (.numeric 0x7f), 0⟩] = fts := by injection hcomp
  subst hlist
  obtain ⟨f0, hf0, hlow⟩ := AverCert.Schema.mapM_getElem?_inv
    (lowerScalarStorage 1) flds _ 0 _ hmap (by rfl)
  have hint : f0 = .intCarrier :=
    AverCert.Schema.lowerScalarStorage_ref_intCarrier 1 f0 _ _ hlow
  rw [hint] at hf0
  have hmention : typeDeclMentionsIntCarrier decl = true := by
    rw [hd]
    simp only [AverCert.Schema.typeDeclMentionsIntCarrier]
    exact AverCert.Schema.typeDeclsMention_of_getElem? flds 0 hf0
  -- ...so the byte-derived carrier binding applies, and the claimed
  -- doppelganger index contradicts the decoded carrier state.
  have hcb := hCarrierBind hmention
  rw [show pseudoHostileClaim.carrier = 1 from rfl] at hcb
  exact absurd hcb (by native_decide)

example : AverCert.StandardFace.weakCarrierRecordParamDeclaredFace
    pseudoHostileBytes pseudoHostileLen pseudoHostileClaim pseudoPlan :=
  ⟨rfl, rfl, rfl, .record 2 personFields, 2, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

-- EXCLUSIVE: the param-weakened copy keeps BOTH the pin and the carrier
-- binding, so it still rejects the doppelganger claim by the same forcing.
-- (The pin-weakened copy's rejection of this claim is by UNPROVABILITY of the
-- meaning pins rather than by a decidable conjunct: with the pin cut, no
-- byte fact forces the declaration's field list, so neither acceptance nor a
-- refutation of it can be exhibited — the conditional carrier binding is
-- attributable against the carrier-weakened copy and the real face only.)
theorem pseudoRejectedWeakParam :
    ¬ AverCert.StandardFace.weakParamRecordParamDeclaredFace pseudoHostileBytes
      pseudoHostileLen pseudoHostileClaim pseudoPlan := by
  intro h
  obtain ⟨-, -, -, decl, structIdx, field, fields, hfield, hdecl, hRec, -, -,
    hCarrierBind, hPin, -, -, -, -, -⟩ := h
  have hconc : AverCert.WasmSlice.exprRecordProjFace? pseudoPlan = some (2, 1) := rfl
  rw [hconc] at hRec
  injection hRec with hpair
  injection hpair with hsi hfi
  subst hsi
  have hp := AverCert.WasmSlice.typeSectionMatches_elim _ pseudoPersonEntry
    pseudoHostileBytes pseudoHostileLen 2 hPin (by native_decide)
  simp only [decide_eq_true_eq] at hp
  obtain ⟨idx, flds, fts, hd, he, hmap⟩ :=
    AverCert.Schema.lowerTypeDecl_recordFields _ _ _ _ hp
  have hcomp := congrArg CertDecode.TypeEntry.composite he
  have hlist : [(⟨.val (.ref 0x63 (Int.ofNat 1)), 0⟩ : CertDecode.FieldType),
      ⟨.val (.numeric 0x7f), 0⟩] = fts := by injection hcomp
  subst hlist
  obtain ⟨f0, hf0, hlow⟩ := AverCert.Schema.mapM_getElem?_inv
    (lowerScalarStorage 1) flds _ 0 _ hmap (by rfl)
  have hint : f0 = .intCarrier :=
    AverCert.Schema.lowerScalarStorage_ref_intCarrier 1 f0 _ _ hlow
  rw [hint] at hf0
  have hmention : typeDeclMentionsIntCarrier decl = true := by
    rw [hd]
    simp only [AverCert.Schema.typeDeclMentionsIntCarrier]
    exact AverCert.Schema.typeDeclsMention_of_getElem? flds 0 hf0
  have hcb := hCarrierBind hmention
  rw [show pseudoHostileClaim.carrier = 1 from rfl] at hcb
  exact absurd hcb (by native_decide)

-- Honest pseudo twin: the real face accepts the claim naming the REAL carrier.
theorem pseudoHonestFace :
    AverCert.StandardFace.recordParamDeclaredFace pseudoHonestBytes
      pseudoHonestLen pseudoHonestClaim pseudoPlan :=
  ⟨rfl, rfl, rfl, .record 2 personFields, 2, 1, personFields, by decide,
   rfl, rfl, rfl, rfl,
   fun _ => by native_decide, by native_decide, by native_decide,
   HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩
