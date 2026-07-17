/-
Soundness witnesses for `DeclaredIndexEnvelope`, checked against the REAL bytes of
`aver compile --target wasm-gc --certify` on:
  examples/data/json.av            : the `Json` ADT (String/Int/Float/Bool/List/
                                     Map/Null) sharing one rec group with
                                     `ParseResult`, `List`, `Map`, Int carrier.
  tools/certkit/fixtures/cert_goals.av : a second, richer multi-ADT module.

The position pin is now a SINGLE byte-slice equality. `declaredConcat typePrefix
env` is one ordered byte list: an opaque DECLARED prefix (`jsonPrefix` / `cgPrefix`
— the rec-group header and every entry before the ADT's constructors, given as
literal bytes) followed by the ADT constructor bodies SYNTHESIZED from meaning.
`concatPinnedAt` confirms it against the located type-section start by
`takeBytes … = declaredConcat`. There is NO `readTypeEntry`/`entryStep`/
`entryCursorAt` walk: the only byte navigation is `modulePayload 1` + `readU`
(the fixed type-section-framing locate).

Because the one equality fixes every declared byte, every position follows by
construction: `Json`'s constructors sit at flattened indices 1..7 (hit `JsonInt`
at 2, carrier 21) and `cert_goals`'s named ADT at root 4 (hit at 5, carrier 23)
purely because the declared concat equals the real prefix at those offsets. Every
forge — including one that REORDERS a constructor to a wrong position, or points
the carrier at a wrong index — makes the declared concat differ from the real
bytes, so the equality fails.
-/
import DeclaredIndexEnvelope
import JsonBytes
import CertGoalsBytes

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.DeclaredIndexEnvelope.Tests

open AverCert.Schema
open CertPrelude
open AverCert.WidenedEnvelope (WCtor WPayload)
open AverCert.DeclaredIndexEnvelope

/-- Concrete `CarrierSpec` at any index (small-Int representation), reused by the
    meaning-column forges. -/
def dSmallSpec (C : Nat) : CarrierSpec C where
  Repr := fun n v => v = carrierSmall C n
  car := fun n v h => Or.inl ⟨n, 0, h⟩
  smallIntro := fun _ => rfl
  smallElim := fun n s sg h => by simp [carrierSmall] at h; exact h.1
  bigElim := fun n s lty les sg h => by simp [carrierSmall] at h

/-! ## §1 json.av — the honest `jsonInt` declared envelope, REAL bytes

`type Json = JsonString(String) | JsonInt(Int) | JsonFloat(Float) | JsonBool(Bool)
| JsonList(List<Json>) | JsonObject(Map<String,Json>) | JsonNull`. The hit
constructor `JsonInt` sits at flattened type index 2 with carrier 21; the non-hit
constructors sit at 1,3,4,5,6,7 with their own declared targets. -/

def jsonEnv : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 19⟩, ⟨2, .hit, 21⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

def jsonPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 2 .proj (.default 0) }

/-- The opaque DECLARED type-section prefix that precedes `Json`'s constructors:
    the shared rec-group header (`0x4e 0x2b`, 43 members) and flattened entry 0.
    Literal bytes — no parser produced them. -/
def jsonPrefix : List Nat := [0x4e, 0x2b, 0x50, 0x00, 0x5f, 0x00]

/-- The located type-section entry cursor for the json module (framing locate
    only: `modulePayload 1` + `readU`). -/
def jTypeCur : Nat × Nat :=
  (typeSectionCursor JsonBytes.modBytes JsonBytes.modLen).getD (0, 0)

/-- THE PIN: one byte-slice equality confirms the whole declared type-section
    prefix — the opaque header/entry-0 bytes AND the seven synthesized `Json`
    constructor bodies, contiguous in source order — against the real json bytes.
    Every constructor's flattened index and byte offset is thereby fixed by
    construction; no entry is walked. -/
theorem json_concat_pin :
    concatPinnedAt JsonBytes.modBytes JsonBytes.modLen
      (declaredConcat jsonPrefix jsonEnv) :=
  ⟨jTypeCur, rfl, by decide +kernel, by decide +kernel⟩

/-- The honest `jsonInt` obligation. `code`/`host`/`self` are irrelevant to the
    type-section face and left trivial; the meaning columns read only the hit. -/
def oJsonHonest : Obligation where
  export_ := "jsonInt"
  policy := .simulatesModel
  carrier := 21
  code := fun _ => none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := DAdtVal jsonEnv
  Cod := Int
  domRepr := dEnvDomRepr jsonEnv
  codRepr := @intRepr 21
  model := dEnvStructModel jsonEnv jsonPlan.body

/-- HONEST PASS: the declared-index face closes on the REAL json bytes,
    NON-VACUOUS end to end (the single concat pin holds, and the model reads only
    the hit Int payload). -/
theorem json_honest_passes :
    DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonEnv jsonPlan
      oJsonHonest := by
  refine ⟨by decide, by decide, json_concat_pin, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The positive bridge specializes to the json envelope: every represented `Json`
    input relates to the `jsonInt` model, whatever its non-hit payload shape. -/
theorem json_bridge_applies
    (S : CarrierSpec jsonEnv.carrier) (x : DAdtVal jsonEnv) (vs : List WVal)
    (hdom : dEnvDomRepr jsonEnv S x vs) :
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S jsonPlan.body tag fields n ∧
      ∀ w, S.Repr n w → intRepr S (dEnvStructModel jsonEnv jsonPlan.body x) w :=
  env_declaredIntDispatch_bridge jsonEnv jsonPlan (by decide) S x vs hdom

/-! ## §2 The forges die ON REAL BYTES -/

/-- (a) An arbitrary constant model the pins do not force: DIES (meaning column). -/
def oJsonForgeConst : Obligation := { oJsonHonest with model := fun _ => (17 : Int) }

theorem forge_const_model_dies :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonEnv jsonPlan
        oJsonForgeConst := by
  intro hface
  obtain ⟨-, -, -, -, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => (17 : Int)) = dEnvStructModel jsonEnv jsonPlan.body :=
    eq_of_heq hmodel
  have h := congrFun heq ⟨(2, .int 5), Or.inl ⟨by decide, 5, rfl⟩⟩
  exact absurd h (by decide)

/-- (b) A vacuous `domRepr := False` (the vacuity smuggle): DIES (meaning column). -/
def oJsonForgeDomFalse : Obligation := { oJsonHonest with domRepr := fun _ _ _ => False }

theorem forge_vacuous_domRepr_dies :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonEnv jsonPlan
        oJsonForgeDomFalse := by
  intro hface
  obtain ⟨-, -, -, -, -, -, hdomR, -, -⟩ := hface
  have heq : (fun _ _ _ => False) = dEnvDomRepr jsonEnv := eq_of_heq hdomR
  have h := congrFun (congrFun (congrFun heq (dSmallSpec 21))
    ⟨(2, .int 0), Or.inl ⟨by decide, 0, rfl⟩⟩) [.structv 2 [carrierSmall 21 0]]
  exact Eq.mp h.symm ⟨carrierSmall 21 0, rfl, rfl⟩

/-- (c) THE KEY forge — DECLARE A WRONG CARRIER INDEX. The forger points the hit
    constructor `JsonInt` (and the declared carrier) at type index 20 instead of
    21. The profile checker still passes (hit target = carrier = 20), but the hit
    body byte inside the declared concat is then `20`, while the real bytes at
    that offset are `21`: the single concat equality fails. The carrier index is
    confirmed by byte equality, not chosen. -/
def jsonForgeCarrier : DIdxEnvelope :=
  ⟨0, 20, [⟨1, .strBox, 19⟩, ⟨2, .hit, 20⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonForgeCarrier
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-- (c') DECLARE A WRONG HIT POSITION. The forger REORDERS the constructors so the
    hit `JsonInt` body lands at the real `JsonFloat` slot (flattened index 3) to
    read a Float as an Int. Because the declared concat lays the bodies out in
    source order, the hit body now sits at the offset where the real bytes are the
    Float struct (`struct{f64}`), so the concat equality fails. Position is fixed
    by construction — a forger cannot shift a body without breaking the pin. -/
def jsonForgeHitPos : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 19⟩, ⟨2, .floatBox, 0⟩, ⟨3, .hit, 21⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_hit_position_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonForgeHitPos
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-- (c'') DECLARE A WRONG NON-HIT TARGET. The forger points `JsonString`'s payload
    at the string array 18 instead of 19. That target byte lives inside the
    declared concat's first constructor body, so the concat differs from the real
    bytes — every declared payload target, not just the carrier, is pinned. -/
def jsonForgeStrArr : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 18⟩, ⟨2, .hit, 21⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_strarray_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonForgeStrArr
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §3 cert_goals.av — a multi-ADT witness at a NON-ZERO named root

The named ADT lives at root 4 in a shared rec group of 33 types (carrier 23, string
array 21); the OTHER ADTs (roots 0, 8, 11, 15) coexist in the same group and are
simply NOT referenced. The declared prefix subsumes the entire first ADT (as
opaque bytes), so the named ADT's constructors sit at their real positions 5,6,7. -/

def cgEnv : DIdxEnvelope :=
  ⟨4, 23, [⟨5, .hit, 23⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

def cgPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 5 .proj (.default 0) }

/-- The opaque DECLARED prefix for cert_goals: the rec-group header and every
    flattened entry (0..4) before the named ADT's constructors, as literal bytes. -/
def cgPrefix : List Nat :=
  [0x4e, 0x21, 0x50, 0x00, 0x5f, 0x00, 0x4f, 0x01, 0x00, 0x5f, 0x01, 0x63, 0x17,
   0x00, 0x4f, 0x01, 0x00, 0x5f, 0x01, 0x63, 0x17, 0x00, 0x4f, 0x01, 0x00, 0x5f,
   0x00, 0x50, 0x00, 0x5f, 0x00]

def cgTypeCur : Nat × Nat :=
  (typeSectionCursor CertGoalsBytes.modBytes CertGoalsBytes.modLen).getD (0, 0)

theorem cg_concat_pin :
    concatPinnedAt CertGoalsBytes.modBytes CertGoalsBytes.modLen
      (declaredConcat cgPrefix cgEnv) :=
  ⟨cgTypeCur, rfl, by decide +kernel, by decide +kernel⟩

def oCgHonest : Obligation where
  export_ := "evalOp"
  policy := .simulatesModel
  carrier := 23
  code := fun _ => none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := DAdtVal cgEnv
  Cod := Int
  domRepr := dEnvDomRepr cgEnv
  codRepr := @intRepr 23
  model := dEnvStructModel cgEnv cgPlan.body

/-- HONEST PASS on the multi-ADT module: the named-root ADT closes at its real
    non-zero position, the first ADT subsumed into the opaque prefix. -/
theorem cg_honest_passes :
    DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen cgPrefix cgEnv
      cgPlan oCgHonest := by
  refine ⟨by decide, by decide, cg_concat_pin, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The wrong-carrier forge dies on the multi-ADT module too: declaring the named
    ADT's carrier at 22 (the limb array) instead of 23 makes the hit body byte in
    the declared concat `22`, while the real bytes at position 5 are `23`. -/
def cgForgeCarrier : DIdxEnvelope :=
  ⟨4, 22, [⟨5, .hit, 22⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

theorem cg_forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen cgPrefix
        cgForgeCarrier plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hjc : typeSectionCursor CertGoalsBytes.modBytes CertGoalsBytes.modLen
      = some cgTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §4 The declared-index CONSTRUCTOR column, REAL bytes

The reverse arrow: build the `JsonInt` hit constructor at its declared index 2
from a single Int argument. The same single concat pin closes on the real json
bytes, so the constructed struct's body is byte-pinned at its declared offset; a
forge that REORDERS the built (result) constructor to a wrong index dies at the
pin. -/

def jsonCtorPlan : ConstructRawPlan :=
  { profile := "construct-v1", arity := 1, fields := [.local 0] }

/-- The honest `JsonInt` constructor obligation: domain a single `Int`, codomain
    the declared `Json` value, model the declared hit constructor at index 2. -/
def oJsonCtorHonest : Obligation where
  export_ := "jsonMkInt"
  policy := .simulatesModel
  carrier := 21
  code := fun _ => none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := Int
  Cod := DAdtVal jsonEnv
  domRepr := AverCert.EnvelopeLowering.intArgDomRepr 21
  codRepr := dEnvCodRepr jsonEnv
  model := dEnvCtorModel jsonEnv 2 (by decide)

/-- HONEST PASS: the declared-index constructor face closes on the REAL json
    bytes — the single concat pin holds and the model builds the hit constructor
    at its declared index. -/
theorem json_ctor_honest_passes :
    DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonEnv 2 (by decide)
      jsonCtorPlan oJsonCtorHonest := by
  refine ⟨by decide, rfl, rfl, json_concat_pin, rfl, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The positive constructor bridge specializes to the json envelope: the single
    Int argument's canonical representation is carried into the exact struct the
    lowering builds at declared index 2. -/
theorem json_ctor_bridge_applies
    (S : CarrierSpec jsonEnv.carrier) (x : Int) (args : List WVal)
    (hdom : AverCert.EnvelopeLowering.intArgDomRepr jsonEnv.carrier S x args) :
    args.length = jsonCtorPlan.arity ∧
    dEnvCodRepr jsonEnv S (dEnvCtorModel jsonEnv 2 (by decide) x)
      (.structv 2
        (ConstructVerbatimSoundness.constructModelFields
          (args ++ List.replicate 1 .null) jsonCtorPlan.fields)) :=
  env_declaredConstruct_bridge jsonEnv 2 (by decide) jsonCtorPlan rfl rfl S x args hdom

/-- WRONG RESULT TAG: the forger REORDERS so the built hit constructor's body
    lands at the real `JsonFloat` slot (index 3), claiming an Int result while
    `struct.new` would target a Float struct. The concat pin refutes it against
    the real bytes (`struct{f64}`), exactly as in the read column. -/
theorem forge_ctor_wrong_result_tag_dies
    (structIdx : Nat) (hhit : dCtorShape? jsonForgeHitPos structIdx = some .hit)
    (plan : ConstructRawPlan) (o : Obligation) :
    ¬ DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen jsonPrefix jsonForgeHitPos
        structIdx hhit plan o := by
  intro hface
  obtain ⟨-, -, -, hpin, -, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §5 Axiom scrub (kernel-checked witness inventory) -/

#print axioms json_concat_pin
#print axioms json_honest_passes
#print axioms json_bridge_applies
#print axioms forge_const_model_dies
#print axioms forge_vacuous_domRepr_dies
#print axioms forge_wrong_carrier_index_dies
#print axioms forge_wrong_hit_position_dies
#print axioms forge_wrong_strarray_index_dies
#print axioms cg_concat_pin
#print axioms cg_honest_passes
#print axioms cg_forge_wrong_carrier_index_dies
#print axioms json_ctor_honest_passes
#print axioms json_ctor_bridge_applies
#print axioms forge_ctor_wrong_result_tag_dies

end AverCert.DeclaredIndexEnvelope.Tests
