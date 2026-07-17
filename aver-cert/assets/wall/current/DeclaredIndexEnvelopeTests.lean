/-
Soundness witnesses for `DeclaredIndexEnvelope`, checked against the REAL bytes of
`aver compile --target wasm-gc --certify` on:
  examples/data/json.av            : the `Json` ADT (String/Int/Float/Bool/List/
                                     Map/Null) sharing one rec group with
                                     `ParseResult`, `List`, `Map`, Int carrier.
  tools/certkit/fixtures/cert_goals.av : a second, richer multi-ADT module.

The position pin is now the SINGLE unified walk (`dWalkPinned`): from the located
type-section start it navigates each entry by its real byte length (counting to
assign flattened indices) and, at every declared-constructor index, confirms the
entry bytes EQUAL that constructor's declared template. Position (counting) and
content (equality) come from ONE traversal — the former split of a whole-prefix
concat equality plus a separate index-count walk is gone, and no `typePrefix`
literal is supplied (`[]`): the prefix is navigated, not declared.

`Json`'s constructors are confirmed at flattened indices 1..7 (hit `JsonInt` at 2,
carrier 21) and `cert_goals`'s named ADT at root 4 (hit at 5, carrier 23), each
because the real entry at the counted index equals the declared template. Every
forge — wrong carrier/target, wrong hit position, or an idx-LABEL SWAP invisible
to a body-only concat — makes the walk's equality fail at the counted index.
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

/-- The located type-section entry cursor for the json module (framing locate
    only: `modulePayload 1` + `readU`). -/
def jTypeCur : Nat × Nat :=
  (typeSectionCursor JsonBytes.modBytes JsonBytes.modLen).getD (0, 0)

/-- THE PIN: the SINGLE unified walk over the real json type section succeeds. It
    navigates the rec-group header and flattened entry 0, then confirms each of
    the seven `Json` constructor bodies EQUAL to its declared template at the
    counted flattened index (1..7). Position (by counting) and content (by
    equality) come from ONE traversal — no separate prefix concat, no separate
    alignment walk. The prefix is NAVIGATED, not declared, so no `typePrefix`
    literal is supplied. -/
theorem json_walk_pin :
    dWalkPinned JsonBytes.modBytes JsonBytes.modLen jsonEnv :=
  ⟨jTypeCur, rfl, by decide +kernel⟩

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
    DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonEnv jsonPlan
      oJsonHonest := by
  refine ⟨by decide, by decide, json_walk_pin, rfl,
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
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonEnv jsonPlan
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
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonEnv jsonPlan
        oJsonForgeDomFalse := by
  intro hface
  obtain ⟨-, -, -, -, -, -, hdomR, -, -⟩ := hface
  have heq : (fun _ _ _ => False) = dEnvDomRepr jsonEnv := eq_of_heq hdomR
  have h := congrFun (congrFun (congrFun heq (dSmallSpec 21))
    ⟨(2, .int 0), Or.inl ⟨by decide, 0, rfl⟩⟩) [.structv 2 [carrierSmall 21 0]]
  exact Eq.mp h.symm ⟨carrierSmall 21 0, rfl, rfl⟩

/-- (c) THE KEY forge — DECLARE A WRONG CARRIER INDEX. The forger points the hit
    constructor `JsonInt` (and the declared carrier) at type index 20 instead of
    21. The profile checker still passes (hit target = carrier = 20), but the walk
    matches the hit template `…63 20 00` at flattened index 2, while the real
    bytes there are `…63 21 00`: the equality in the walk fails. The carrier index
    is confirmed by byte equality, not chosen. -/
def jsonForgeCarrier : DIdxEnvelope :=
  ⟨0, 20, [⟨1, .strBox, 19⟩, ⟨2, .hit, 20⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonForgeCarrier
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-- (c') DECLARE A WRONG HIT POSITION. The forger REORDERS the constructors so the
    hit `JsonInt` body is labelled at the real `JsonFloat` slot (flattened index 3)
    to read a Float as an Int. The walk reaches index 3 by counting, matches the
    hit template there, and the real bytes are the Float struct (`struct{f64}`),
    so the equality fails. Position is counted, not assumed. -/
def jsonForgeHitPos : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 19⟩, ⟨2, .floatBox, 0⟩, ⟨3, .hit, 21⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_hit_position_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonForgeHitPos
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-- (c'') DECLARE A WRONG NON-HIT TARGET. The forger points `JsonString`'s payload
    at the string array 18 instead of 19. The walk matches `JsonString`'s template
    at index 1, whose target byte is now `18` while the real bytes carry `19`, so
    the equality fails — every declared payload target, not just the carrier, is
    confirmed by the walk. -/
def jsonForgeStrArr : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 18⟩, ⟨2, .hit, 21⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_strarray_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonForgeStrArr
        plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-- (c''') THE idx-LABEL SWAP — the decoupling forge that motivated this pin. The
    forger swaps the `idx` LABELS of the hit `JsonInt` (real index 2) and the
    `JsonFloat` (real index 3), leaving list order — hence every body byte —
    untouched, so a body-only concat pin cannot see the swap. The dispatch tests
    type index 3 (the real `JsonFloat`, `struct{f64}`). The UNIFIED walk kills it
    on the REAL json bytes: counting reaches index 3, where the hit template
    `…63 21 00` is matched against the real Float entry `…5f 01 7c 00`, and the
    equality fails. (It already fails one entry earlier — the swapped `floatBox`
    label at index 2 mismatches the real `JsonInt` — either way the walk is
    `false`.) The kill is on the actual module bytes, not a structural label rule. -/
def jsonForgeIdxSwap : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 19⟩, ⟨3, .hit, 21⟩, ⟨2, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

def jsonForgeIdxSwapPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 3 .proj (.default 0) }

/-- The forged env still passes the profile checker and the cascade (index 3
    resolves to hit), so ONLY the walk pin is doing the killing. -/
theorem idx_swap_checks_still_pass :
    checkDIdxEnvelope jsonForgeIdxSwap = true ∧
    dCascadeInEnv jsonForgeIdxSwap jsonForgeIdxSwapPlan.body = true ∧
    dCtorShape? jsonForgeIdxSwap 3 = some .hit := by
  refine ⟨by decide, by decide, by decide⟩

theorem forge_idx_label_swap_dies (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen [] jsonForgeIdxSwap
        jsonForgeIdxSwapPlan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-! ## §3 cert_goals.av — a multi-ADT witness at a NON-ZERO named root

The named ADT lives at root 4 in a shared rec group of 33 types (carrier 23, string
array 21); the OTHER ADTs (roots 0, 8, 11, 15) coexist in the same group and are
simply NOT referenced. The walk NAVIGATES the rec-group header and the four
flattened entries (0..4) of the first ADT, then confirms the named ADT's three
constructors at their real positions 5,6,7 — no declared prefix literal needed. -/

def cgEnv : DIdxEnvelope :=
  ⟨4, 23, [⟨5, .hit, 23⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

def cgPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 5 .proj (.default 0) }

def cgTypeCur : Nat × Nat :=
  (typeSectionCursor CertGoalsBytes.modBytes CertGoalsBytes.modLen).getD (0, 0)

theorem cg_walk_pin :
    dWalkPinned CertGoalsBytes.modBytes CertGoalsBytes.modLen cgEnv :=
  ⟨cgTypeCur, rfl, by decide +kernel⟩

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
    DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen [] cgEnv
      cgPlan oCgHonest := by
  refine ⟨by decide, by decide, cg_walk_pin, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The wrong-carrier forge dies on the multi-ADT module too: declaring the named
    ADT's carrier at 22 (the limb array) instead of 23 makes the walk match the
    hit template `…63 22 00` at index 5, while the real bytes there carry `23`. -/
def cgForgeCarrier : DIdxEnvelope :=
  ⟨4, 22, [⟨5, .hit, 22⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

theorem cg_forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen []
        cgForgeCarrier plan o := by
  intro hface
  obtain ⟨-, -, hpin, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor CertGoalsBytes.modBytes CertGoalsBytes.modLen
      = some cgTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-! ## §4 The declared-index CONSTRUCTOR column, REAL bytes

The reverse arrow: build the `JsonInt` hit constructor at its declared index 2
from a single Int argument. The same single walk pin closes on the real json
bytes, so the constructed struct's body is confirmed at its declared index by the
walk; a forge that mislabels the built (result) constructor to a wrong index dies
at the walk. -/

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
    DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen [] jsonEnv 2 (by decide)
      jsonCtorPlan oJsonCtorHonest := by
  refine ⟨by decide, rfl, rfl, json_walk_pin, rfl, rfl,
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

/-- WRONG RESULT TAG: the forger mislabels so the built hit constructor's body is
    claimed at the real `JsonFloat` slot (index 3), claiming an Int result while
    `struct.new` would target a Float struct. The walk refutes it against the real
    bytes (`struct{f64}`) at the counted index 3, exactly as in the read column. -/
theorem forge_ctor_wrong_result_tag_dies
    (structIdx : Nat) (hhit : dCtorShape? jsonForgeHitPos structIdx = some .hit)
    (plan : ConstructRawPlan) (o : Obligation) :
    ¬ DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen [] jsonForgeHitPos
        structIdx hhit plan o := by
  intro hface
  obtain ⟨-, -, -, hpin, -, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, hwalk⟩ := hpin
  have hjc : typeSectionCursor JsonBytes.modBytes JsonBytes.modLen = some jTypeCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd hwalk (by decide +kernel)

/-! ## §4.5 NON-CONTIGUOUS layout — the generality the concat lacked

A hand-assembled minimal wasm-gc module whose rec group interleaves an UNRELATED
entry between two declared constructors: flattened index 0 is a prelude
`struct{}`, index 1 the hit constructor, index 2 an unrelated `array i32`, index 3
a `strBox` constructor. The two declared constructors (indices 1 and 3) are NOT a
contiguous block. The unified walk CLOSES: it navigates the group header, entry 0
and the array (index 2) by their real byte lengths, and confirms the hit and
strBox templates at the counted indices 1 and 3. A contiguous `declaredConcat`
(`prefix ++ bodies.flatten`) cannot express this — its bodies are adjacent, but
the real bytes put the array between them — so the old concat pin FAILS on the
very layout the walk confirms. -/

/-- Little-endian byte list → big-`Nat` (the certificate byte representation). -/
def natFromBytesLE : List Nat → Nat
  | [] => 0
  | b :: rest => b + 256 * natFromBytesLE rest

/-- The synthetic module: `\0asm` + version, then one type section whose rec group
    is `[struct{} ; sub_final[0] struct{ref carrier} ; array i32 ;
    sub_final[0] struct{ref strArr}]`. The array at flattened index 2 sits BETWEEN
    the two declared constructors. -/
def ncBytes : List Nat :=
  [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,      -- magic + version
   0x01, 0x1a,                                          -- type section id, size 26
   0x01,                                                -- type vector count
   0x4e, 0x04,                                          -- rec group header, 4 members
   0x50, 0x00, 0x5f, 0x00,                              -- idx 0: sub struct{}
   0x4f, 0x01, 0x00, 0x5f, 0x01, 0x63, 0x09, 0x00,      -- idx 1: hit  → carrier 9
   0x5e, 0x7f, 0x00,                                    -- idx 2: UNRELATED array i32
   0x4f, 0x01, 0x00, 0x5f, 0x01, 0x63, 0x07, 0x00]      -- idx 3: strBox → strArr 7

def ncModBytes : Nat := natFromBytesLE ncBytes
def ncModLen : Nat := ncBytes.length

/-- Constructors declared at NON-contiguous indices 1 and 3. -/
def ncEnv : DIdxEnvelope :=
  ⟨0, 9, [⟨1, .hit, 9⟩, ⟨3, .strBox, 7⟩]⟩

def ncTypeCur : Nat × Nat :=
  (typeSectionCursor ncModBytes ncModLen).getD (0, 0)

/-- THE PIN CLOSES on the non-contiguous layout: one walk navigates the header,
    entry 0 and the interleaved array, and confirms the hit (index 1) and strBox
    (index 3) templates at their counted indices. -/
theorem nc_walk_pin : dWalkPinned ncModBytes ncModLen ncEnv :=
  ⟨ncTypeCur, rfl, by decide +kernel⟩

/-- The full Int-read face closes on the non-contiguous module. -/
def ncPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 1 .proj (.default 0) }

def oNc : Obligation where
  export_ := "ncEval"
  policy := .simulatesModel
  carrier := 9
  code := fun _ => none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := DAdtVal ncEnv
  Cod := Int
  domRepr := dEnvDomRepr ncEnv
  codRepr := @intRepr 9
  model := dEnvStructModel ncEnv ncPlan.body

theorem nc_honest_passes :
    DIdxIntReadFace ncModBytes ncModLen [] ncEnv ncPlan oNc := by
  refine ⟨by decide, by decide, nc_walk_pin, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The contiguous concat pin FAILS on the same module even with the correct
    prefix: `declaredConcat` lays the hit and strBox bodies adjacently, but the
    real bytes interpose the array entry, so the byte-slice equality differs. This
    is exactly the generality the single walk adds. -/
theorem nc_contiguous_concat_fails :
    ¬ concatPinnedAt ncModBytes ncModLen
        (declaredConcat [0x4e, 0x04, 0x50, 0x00, 0x5f, 0x00] ncEnv) := by
  intro hpin
  obtain ⟨cur, hcur, -, htake⟩ := hpin
  have hnc : typeSectionCursor ncModBytes ncModLen = some ncTypeCur := rfl
  rw [hnc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §5 Axiom scrub (kernel-checked witness inventory) -/

#print axioms json_walk_pin
#print axioms json_honest_passes
#print axioms json_bridge_applies
#print axioms forge_const_model_dies
#print axioms forge_vacuous_domRepr_dies
#print axioms forge_wrong_carrier_index_dies
#print axioms forge_wrong_hit_position_dies
#print axioms forge_wrong_strarray_index_dies
#print axioms idx_swap_checks_still_pass
#print axioms forge_idx_label_swap_dies
#print axioms cg_walk_pin
#print axioms cg_honest_passes
#print axioms cg_forge_wrong_carrier_index_dies
#print axioms nc_walk_pin
#print axioms nc_honest_passes
#print axioms nc_contiguous_concat_fails
#print axioms json_ctor_honest_passes
#print axioms json_ctor_bridge_applies
#print axioms forge_ctor_wrong_result_tag_dies

end AverCert.DeclaredIndexEnvelope.Tests
