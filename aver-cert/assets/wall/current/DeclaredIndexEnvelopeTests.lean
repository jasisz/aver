/-
Soundness witnesses for `DeclaredIndexEnvelope`, checked against the REAL bytes of
`aver compile --target wasm-gc --certify` on:
  examples/data/json.av            : the `Json` ADT (String/Int/Float/Bool/List/
                                     Map/Null) sharing one rec group with
                                     `ParseResult`, `List`, `Map`, Int carrier.
  tools/certkit/fixtures/cert_goals.av : a second, richer multi-ADT module.

None of the declared indices are the arithmetic `root + len + 3`: `Json`'s carrier
is at type index 21, its string array at 19, its `List` cons at 27, its `Map`
struct at 38, and `cert_goals`'s named ADT sits at root 4 with carrier 23. Each is
DECLARED and pinned at its real position; the honest face closes; every forge —
including one that DECLARES A WRONG INDEX — dies against the real bytes.
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

def jStrCur   : Nat × Nat := (entryCursorAt JsonBytes.modBytes JsonBytes.modLen 1).getD (0, 0)
def jHitCur   : Nat × Nat := (entryCursorAt JsonBytes.modBytes JsonBytes.modLen 2).getD (0, 0)
def jFloatCur : Nat × Nat := (entryCursorAt JsonBytes.modBytes JsonBytes.modLen 3).getD (0, 0)

/-- Each `Json` constructor's synthesized struct-body is pinned at its DECLARED
    flattened index in the REAL json module. This is the whole de-risk: the pins
    close INTO the shared rec group at compiler-assigned positions. -/
theorem json_ctor_pins :
    ∀ c ∈ jsonEnv.ctors,
      entryPinnedAt JsonBytes.modBytes JsonBytes.modLen c.idx (dCtorBody 0 c) := by
  intro c hc
  simp only [jsonEnv, List.mem_cons, List.mem_singleton, List.not_mem_nil, or_false] at hc
  rcases hc with rfl | rfl | rfl | rfl | rfl | rfl | rfl
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 1).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 2).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 3).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 4).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 5).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 6).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt JsonBytes.modBytes JsonBytes.modLen 7).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩

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
    NON-VACUOUS end to end (all seven constructor pins hold at their real
    positions, and the model reads only the hit Int payload). -/
theorem json_honest_passes :
    DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonEnv jsonPlan oJsonHonest := by
  refine ⟨by decide, by decide, json_ctor_pins, rfl,
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

/-- (a) An arbitrary constant model the pins do not force: DIES. -/
def oJsonForgeConst : Obligation := { oJsonHonest with model := fun _ => (17 : Int) }

theorem forge_const_model_dies :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonEnv jsonPlan
        oJsonForgeConst := by
  intro hface
  obtain ⟨-, -, -, -, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => (17 : Int)) = dEnvStructModel jsonEnv jsonPlan.body :=
    eq_of_heq hmodel
  have h := congrFun heq ⟨(2, .int 5), Or.inl ⟨by decide, 5, rfl⟩⟩
  exact absurd h (by decide)

/-- (b) A vacuous `domRepr := False` (the vacuity smuggle): DIES. -/
def oJsonForgeDomFalse : Obligation := { oJsonHonest with domRepr := fun _ _ _ => False }

theorem forge_vacuous_domRepr_dies :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonEnv jsonPlan
        oJsonForgeDomFalse := by
  intro hface
  obtain ⟨-, -, -, -, -, -, hdomR, -, -⟩ := hface
  have heq : (fun _ _ _ => False) = dEnvDomRepr jsonEnv := eq_of_heq hdomR
  have h := congrFun (congrFun (congrFun heq (dSmallSpec 21))
    ⟨(2, .int 0), Or.inl ⟨by decide, 0, rfl⟩⟩) [.structv 2 [carrierSmall 21 0]]
  exact Eq.mp h.symm ⟨carrierSmall 21 0, rfl, rfl⟩

/-- (c) THE KEY forge — DECLARE A WRONG CARRIER INDEX. The forger points the hit
    constructor `JsonInt` (and the declared carrier) at type index 20 instead of
    21. The profile checker still passes (hit target = carrier = 20), but the pin
    at the hit constructor's real position 2 refutes it: the real byte there is 21,
    not 20. This is the exists-not-constrains guarantee — the carrier index is
    nailed by a byte pin, not chosen. -/
def jsonForgeCarrier : DIdxEnvelope :=
  ⟨0, 20, [⟨1, .strBox, 19⟩, ⟨2, .hit, 20⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonForgeCarrier plan o := by
  intro hface
  obtain ⟨-, -, hpins, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpins ⟨2, .hit, 20⟩ (by decide)
  have hjc : entryCursorAt JsonBytes.modBytes JsonBytes.modLen 2 = some jHitCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-- (c') DECLARE A WRONG HIT POSITION. The forger declares the hit constructor at
    index 3 (the real `JsonFloat` slot) to read a Float as an Int. The pin at
    index 3 refutes the hit body: the real bytes there are `struct{f64}`. -/
def jsonForgeHitPos : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 19⟩, ⟨3, .hit, 21⟩, ⟨2, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_hit_position_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonForgeHitPos plan o := by
  intro hface
  obtain ⟨-, -, hpins, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpins ⟨3, .hit, 21⟩ (by decide)
  have hjc : entryCursorAt JsonBytes.modBytes JsonBytes.modLen 3 = some jFloatCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-- (c'') DECLARE A WRONG NON-HIT TARGET. The forger points `JsonString`'s payload
    at the string array 18 instead of 19. The pin at index 1 refutes it — every
    declared payload target, not just the carrier, is nailed by a byte pin. -/
def jsonForgeStrArr : DIdxEnvelope :=
  ⟨0, 21, [⟨1, .strBox, 18⟩, ⟨2, .hit, 21⟩, ⟨3, .floatBox, 0⟩, ⟨4, .boolBox, 0⟩,
           ⟨5, .listBox, 27⟩, ⟨6, .mapBox, 38⟩, ⟨7, .unit, 0⟩]⟩

theorem forge_wrong_strarray_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace JsonBytes.modBytes JsonBytes.modLen jsonForgeStrArr plan o := by
  intro hface
  obtain ⟨-, -, hpins, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpins ⟨1, .strBox, 18⟩ (by decide)
  have hjc : entryCursorAt JsonBytes.modBytes JsonBytes.modLen 1 = some jStrCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §3 cert_goals.av — a multi-ADT witness at a NON-ZERO named root

The named ADT lives at root 4 in a shared rec group of 33 types (carrier 23, string
array 21); the OTHER ADTs (roots 0, 8, 11, 15) coexist in the same group and are
simply NOT referenced. The cursor navigates past the entire first ADT to reach the
named one at its real position. -/

def cgEnv : DIdxEnvelope :=
  ⟨4, 23, [⟨5, .hit, 23⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

def cgPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 5 .proj (.default 0) }

theorem cg_ctor_pins :
    ∀ c ∈ cgEnv.ctors,
      entryPinnedAt CertGoalsBytes.modBytes CertGoalsBytes.modLen c.idx
        (dCtorBody 4 c) := by
  intro c hc
  simp only [cgEnv, List.mem_cons, List.mem_singleton, List.not_mem_nil, or_false] at hc
  rcases hc with rfl | rfl | rfl
  · exact ⟨(entryCursorAt CertGoalsBytes.modBytes CertGoalsBytes.modLen 5).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt CertGoalsBytes.modBytes CertGoalsBytes.modLen 6).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩
  · exact ⟨(entryCursorAt CertGoalsBytes.modBytes CertGoalsBytes.modLen 7).getD (0, 0),
      rfl, by decide +kernel, by decide +kernel⟩

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
    non-zero position, the other ADTs unreferenced. -/
theorem cg_honest_passes :
    DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen cgEnv cgPlan
      oCgHonest := by
  refine ⟨by decide, by decide, cg_ctor_pins, rfl,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- The wrong-carrier forge dies on the multi-ADT module too: declaring the named
    ADT's carrier at 22 (the limb array) instead of 23 refutes the hit pin at its
    real position 5. -/
def cgForgeCarrier : DIdxEnvelope :=
  ⟨4, 22, [⟨5, .hit, 22⟩, ⟨6, .strBox, 21⟩, ⟨7, .unit, 0⟩]⟩

def cgHitCur : Nat × Nat :=
  (entryCursorAt CertGoalsBytes.modBytes CertGoalsBytes.modLen 5).getD (0, 0)

theorem cg_forge_wrong_carrier_index_dies
    (plan : IntDispatchRawPlan) (o : Obligation) :
    ¬ DIdxIntReadFace CertGoalsBytes.modBytes CertGoalsBytes.modLen cgForgeCarrier
        plan o := by
  intro hface
  obtain ⟨-, -, hpins, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpins ⟨5, .hit, 22⟩ (by decide)
  have hjc : entryCursorAt CertGoalsBytes.modBytes CertGoalsBytes.modLen 5
      = some cgHitCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §4 The declared-index CONSTRUCTOR column, REAL bytes

The reverse arrow: build the `JsonInt` hit constructor at its declared index 2
from a single Int argument. The same seven-constructor pin column closes on the
real json bytes, so the constructed struct's type index is byte-pinned; a forge
that declares the built (result) constructor at a WRONG index dies at the pin. -/

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
    bytes — the hit constructor pins hold at their real positions and the model
    builds the hit constructor at its declared index. -/
theorem json_ctor_honest_passes :
    DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen jsonEnv 2 (by decide)
      jsonCtorPlan oJsonCtorHonest := by
  refine ⟨by decide, rfl, rfl, json_ctor_pins, rfl, rfl,
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

/-- WRONG RESULT TAG: the forger declares the built hit constructor at index 3
    (the real `JsonFloat` slot) so `struct.new` would target a Float struct while
    claiming an Int result. The pin at index 3 refutes the hit body against the
    real bytes (`struct{f64}`), exactly as in the read column. -/
theorem forge_ctor_wrong_result_tag_dies
    (structIdx : Nat) (hhit : dCtorShape? jsonForgeHitPos structIdx = some .hit)
    (plan : ConstructRawPlan) (o : Obligation) :
    ¬ DIdxCtorFace JsonBytes.modBytes JsonBytes.modLen jsonForgeHitPos structIdx
        hhit plan o := by
  intro hface
  obtain ⟨-, -, -, hpins, -, -, -, -, -, -, -⟩ := hface
  obtain ⟨cur, hcur, -, htake⟩ := hpins ⟨3, .hit, 21⟩ (by decide)
  have hjc : entryCursorAt JsonBytes.modBytes JsonBytes.modLen 3 = some jFloatCur := rfl
  rw [hjc] at hcur
  obtain rfl := Option.some.inj hcur
  exact absurd htake (by decide +kernel)

/-! ## §5 Axiom scrub (kernel-checked witness inventory) -/

#print axioms json_ctor_pins
#print axioms json_honest_passes
#print axioms json_bridge_applies
#print axioms forge_const_model_dies
#print axioms forge_vacuous_domRepr_dies
#print axioms forge_wrong_carrier_index_dies
#print axioms forge_wrong_hit_position_dies
#print axioms forge_wrong_strarray_index_dies
#print axioms cg_ctor_pins
#print axioms cg_honest_passes
#print axioms cg_forge_wrong_carrier_index_dies
#print axioms json_ctor_honest_passes
#print axioms json_ctor_bridge_applies
#print axioms forge_ctor_wrong_result_tag_dies

end AverCert.DeclaredIndexEnvelope.Tests
