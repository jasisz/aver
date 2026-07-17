/-
Widened envelope lowering.

`EnvelopeLowering` handles the NARROW ADT profile: every constructor carries at
most one Int payload (`ctors : List Bool`), effectively one ADT per module. The
survey found honest certificates OUTSIDE that profile — a widened Int match over
an ADT whose non-hit constructors carry String / Float / Bool / List / Map /
multi-field payloads (`examples/data/json.av` `jsonInt`), a `Box.StrBox(String)`
match, and modules holding several ADTs.

This module WIDENS the declared envelope so it lowers the REAL rec-group bytes of
such a type — whatever its constructors actually are — while the meaning MODEL
still reads ONLY the hit constructor (its Int payload) and returns a widen
default for every non-hit constructor. That is exactly why tolerating non-hit
payload shapes is SOUND: the model provably never reads a non-hit payload.

DESIGN — bounded shape vocabulary, not unbounded bytes.
Each non-hit constructor is drawn from a FINITE shape vocabulary
(`WCtor`) whose byte lowering has a KNOWN length. This is deliberate: an
`unbounded` raw-byte payload would make the rec-group byte length a function of
attacker-chosen payload content, which reopens a substring-coincidence forge — a
malicious envelope could pad an early constructor so that the fixed hit-encoding
lands on an unrelated, coincidentally matching stretch of the module. With every
shape a KNOWN length the rec-group prefix pin covers exactly the declared bytes
and constructor forcing stays a finite decision. The vocabulary covers every
shape the survey needs; adding a shape is a one-line extension, not a redesign.

There is NO byte -> structure decoder: `wEnvDomRepr` / `wEnvStructModel` are
computed FROM THE PLAN, and the only residue pulled up from the bytes is the
compiler-assigned root type INDEX.
-/
import EnvelopeLowering
import IntDispatchSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.WidenedEnvelope

open AverCert.Schema
open CertPrelude
open AverCert.EnvelopeLowering
  (preludeTail bytesPinnedAt typeCursorAt EnvelopeFact cascadeEval takeBytes_take)

/-! ## §1 The widened declared envelope

`WCtor` is the finite shape vocabulary. `hit` is the ONLY shape the meaning model
reads: it lowers to the canonical single-field `struct { (ref null carrier) }`,
the Int box exposed at read offset 0. Every other shape is a non-hit payload the
model ignores; its bytes are pinned only to keep the rec group honest. -/

inductive WCtor
  | hit            -- Int box, readable:  struct { (ref null carrier) }
  | unit           -- no payload:          struct { }
  | strBox         -- String payload:      struct { (ref null strArray) }
  | floatBox       -- Float payload:       struct { f64 }
  | boolBox        -- Bool payload:        struct { i32 }
  | listBox        -- List payload:        struct { (ref null listArray) }
  | mapBox         -- Map payload:         struct { (ref map) }
deriving Repr, DecidableEq

structure WAdtEnvelope where
  root : Nat
  ctors : List WCtor
deriving Repr, DecidableEq

def wCarrierIdx (env : WAdtEnvelope) : Nat := env.root + env.ctors.length + 3
def wLimbIdx (env : WAdtEnvelope) : Nat := env.root + env.ctors.length + 2
/-- Derived string byte-array index (first prelude entry). -/
def wStrArrIdx (env : WAdtEnvelope) : Nat := env.root + env.ctors.length + 1

/-- Profile checker, fail-closed. Same single-byte index regime as the narrow
    profile; the widening is entirely in the constructor shape vocabulary. -/
def checkWidenedEnvelope (env : WAdtEnvelope) : Bool :=
  decide (1 ≤ env.ctors.length) && decide (wCarrierIdx env < 64)

/-- Declared shape of the constructor with tag `tag` (fail-closed). -/
def wCtorShape? (env : WAdtEnvelope) (tag : Nat) : Option WCtor :=
  if env.root + 1 ≤ tag then env.ctors[tag - (env.root + 1)]? else none

/-! ## §2 Canonical byte LOWERING of the declared widened envelope

Each constructor is ONE `(sub_final [root] struct{…})` rec-group entry, so the
carrier index `root + len + 3` is unchanged by the payload shapes. The body of
each shape (everything after the `0x4f 0x01 root` subtype header) has a KNOWN
length; `strArray`/`listArray`/`map` payload fields reference derived indices. -/

/-- Struct-body bytes of a constructor shape (after the `0x4f 0x01 root` head).
    `C` is the carrier index, `S` the derived string-array index. -/
def wCtorBody (C S : Nat) : WCtor → List Nat
  | .hit        => [0x5f, 0x01, 0x63, C, 0x00]
  | .unit       => [0x5f, 0x00]
  | .strBox     => [0x5f, 0x01, 0x63, S, 0x00]
  | .floatBox   => [0x5f, 0x01, 0x7c, 0x00]
  | .boolBox    => [0x5f, 0x01, 0x7f, 0x00]
  | .listBox    => [0x5f, 0x01, 0x64, S, 0x00]
  | .mapBox     => [0x5f, 0x01, 0x64, C, 0x00]

def wLowerCtorEntry (root C S : Nat) (c : WCtor) : List Nat :=
  0x4f :: 0x01 :: root :: wCtorBody C S c

def wLowerCtorEntries (root C S : Nat) : List WCtor → List Nat
  | [] => []
  | c :: rest => wLowerCtorEntry root C S c ++ wLowerCtorEntries root C S rest

/-- LOWER for the whole declared rec group, header included. -/
def wLowerAdtRecGroup (env : WAdtEnvelope) : Option (List Nat) :=
  if checkWidenedEnvelope env = true then
    some ([0x4e, env.ctors.length + 4, 0x50, 0x00, 0x5f, 0x00] ++
      wLowerCtorEntries env.root (wCarrierIdx env) (wStrArrIdx env) env.ctors ++
      preludeTail (wLimbIdx env))
  else none

/-- LOWER for the dispatch export's function-signature type entry (byte-identical
    to the narrow one: it does not mention constructor shapes). -/
def wLowerDispatchSig (env : WAdtEnvelope) : Option (List Nat) :=
  if checkWidenedEnvelope env = true then
    some [0x60, 0x01, 0x63, env.root, 0x01, 0x63, wCarrierIdx env]
  else none

/-- LOWER for a constructor export's signature. -/
def wLowerCtorSig (env : WAdtEnvelope) : Option (List Nat) :=
  if checkWidenedEnvelope env = true then
    some [0x60, 0x01, 0x63, wCarrierIdx env, 0x01, 0x63, env.root]
  else none

def wAdtRecGroupFact : EnvelopeFact := ⟨WAdtEnvelope, wLowerAdtRecGroup⟩
def wAdtDispatchSigFact : EnvelopeFact := ⟨WAdtEnvelope, wLowerDispatchSig⟩
def wAdtCtorSigFact : EnvelopeFact := ⟨WAdtEnvelope, wLowerCtorSig⟩

/-! ## §3 Wall-owned meaning terms over the PLAN (no decode output)

The widened value carries an Int payload for a `hit` constructor and an OPAQUE
`List WVal` for any non-hit constructor. The model reads only the Int
(`WPayload.toInt?`); a non-hit payload maps to `none`, which — because the
cascade only ever projects at a tested tag and `wCascadeInEnv` forces every
tested tag to be `hit` — never reaches a projection. -/

inductive WPayload
  | int (n : Int)
  | opaqueFields (fields : List WVal)

def WPayload.toInt? : WPayload → Option Int
  | .int n => some n
  | .opaqueFields _ => none

/-- A value of the DECLARED widened ADT: a `hit` tag carries an Int, any other
    declared tag carries opaque wasm fields. Vacuity is structurally impossible
    for a declared tag. -/
def WEnvValidChild (env : WAdtEnvelope) (tag : Nat) (p : WPayload) : Prop :=
  (wCtorShape? env tag = some .hit ∧ ∃ n, p = .int n) ∨
  (∃ c, wCtorShape? env tag = some c ∧ c ≠ .hit ∧ ∃ fs, p = .opaqueFields fs)

def WAdtVal (env : WAdtEnvelope) : Type :=
  { q : Nat × WPayload // WEnvValidChild env q.1 q.2 }

/-- Canonical domain representation from the plan structure. The `hit` case pins
    the single Int-box field through the carrier `Repr`; a non-hit value pins its
    opaque fields verbatim (the model does not constrain them). -/
def wEnvDomRepr (env : WAdtEnvelope) :
    CarrierSpec (wCarrierIdx env) → WAdtVal env → List WVal → Prop :=
  fun S x vs =>
    match x.1.2 with
    | .int n => ∃ v, vs = [.structv x.1.1 [v]] ∧ S.Repr n v
    | .opaqueFields fs => vs = [.structv x.1.1 fs]

/-- structModelFromPlan, widened: reads only the hit constructor's Int payload
    (via `toInt?`), else the cascade default. -/
def wEnvStructModel (env : WAdtEnvelope) (body : IntDispatchCascade) :
    WAdtVal env → Int :=
  fun x => (cascadeEval body x.1.1 x.1.2.toInt?).getD 0

/-- Plan-internal consistency: every tested tag is a DECLARED `hit` constructor
    (byte-pinned by the rec-group lowering), so a projected field is always the
    readable Int box. -/
def wCascadeInEnv (env : WAdtEnvelope) : IntDispatchCascade → Bool
  | .default _ => true
  | .test tyIdx _ rest =>
      decide (wCtorShape? env tyIdx = some .hit) && wCascadeInEnv env rest

/-! ## §4 The widened lower-pinned Int-read face -/

def WAdtIntFaceLower
    (modBytes modLen : Nat)
    (exportNameBytes : List Nat) (exportName : String)
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation) : Prop :=
  AverCert.AcceptedArtifact.intDispatchPlanAccepted
    modBytes modLen exportNameBytes exportName carrier hostTable plan o ∧
  checkWidenedEnvelope env = true ∧
  wCascadeInEnv env plan.body = true ∧
  carrier = wCarrierIdx env ∧
  wAdtRecGroupFact.pinnedAt modBytes modLen env.root env ∧
  (∃ binding,
    AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes
      = some binding ∧
    wAdtDispatchSigFact.pinnedAt modBytes modLen binding.typeIdx env) ∧
  o.carrier = wCarrierIdx env ∧
  HEq o.Dom (WAdtVal env) ∧
  HEq o.Cod Int ∧
  HEq o.domRepr (wEnvDomRepr env) ∧
  HEq o.codRepr (@AverCert.Schema.intRepr (wCarrierIdx env)) ∧
  HEq o.model (wEnvStructModel env plan.body)

/-! ### The widened constructor face (unary Int-payload constructor) -/

/-- Single-Int-argument constructor domain representation. -/
def wIntArgDomRepr (C : Nat) : CarrierSpec C → Int → List WVal → Prop :=
  fun S n vs => ∃ v, vs = [v] ∧ S.Repr n v

/-- Constructor codomain: same relation as `wEnvDomRepr` on a single result. -/
def wEnvCodRepr (env : WAdtEnvelope) :
    CarrierSpec (wCarrierIdx env) → WAdtVal env → WVal → Prop :=
  fun S y w =>
    match y.1.2 with
    | .int n => ∃ v, w = .structv y.1.1 [v] ∧ S.Repr n v
    | .opaqueFields fs => w = .structv y.1.1 fs

/-- Constructor model from the plan: build the DECLARED `hit` constructor `tag`
    with the Int argument as its Int-box payload. The `hit` fact is DEMANDED. -/
def wEnvCtorModel (env : WAdtEnvelope) (tag : Nat)
    (h : wCtorShape? env tag = some .hit) : Int → WAdtVal env :=
  fun n => ⟨(tag, .int n), Or.inl ⟨h, n, rfl⟩⟩

def WAdtCtorFaceLower
    (modBytes modLen : Nat)
    (exportNameBytes : List Nat) (exportName : String)
    (carrier structIdx fieldCount : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (env : WAdtEnvelope) (plan : ConstructRawPlan)
    (o : Obligation) : Prop :=
  AverCert.AcceptedArtifact.constructPlanAccepted
    modBytes modLen exportNameBytes exportName carrier structIdx fieldCount
    elemTy symPlan plan o ∧
  plan.arity = 1 ∧ plan.fields = [.local 0] ∧
  checkWidenedEnvelope env = true ∧
  carrier = wCarrierIdx env ∧
  wAdtRecGroupFact.pinnedAt modBytes modLen env.root env ∧
  (∃ binding,
    AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes
      = some binding ∧
    wAdtCtorSigFact.pinnedAt modBytes modLen binding.typeIdx env) ∧
  o.carrier = wCarrierIdx env ∧
  ∃ hpay : wCtorShape? env structIdx = some .hit,
    HEq o.Dom Int ∧
    HEq o.Cod (WAdtVal env) ∧
    HEq o.domRepr (wIntArgDomRepr (wCarrierIdx env)) ∧
    HEq o.codRepr (wEnvCodRepr env) ∧
    HEq o.model (wEnvCtorModel env structIdx hpay)

/-! ## §5 GOAL 2 — the positive widened bridge

Mirrors `EnvelopeLowering.cascade_bridge` / `env_intDispatch_bridge`, but over the
WIDENED representation: on any represented widened value the byte-origin
`EvalCascade` relates it to exactly the plan-computed `cascadeEval` result,
REGARDLESS of the non-hit payload shapes. The hit branch uses that a tested tag
is `hit` (so the value is `.int`, and its field is the Int box); the miss/default
branches never read a payload. -/

theorem wCascade_bridge (env : WAdtEnvelope) (body : IntDispatchCascade)
    (hcasc : wCascadeInEnv env body = true)
    (S : CarrierSpec (wCarrierIdx env)) (x : WAdtVal env) (vs : List WVal)
    (hdom : wEnvDomRepr env S x vs) :
    ∃ tag fields,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S body tag fields
        ((cascadeEval body x.1.1 x.1.2.toInt?).getD 0) := by
  induction body with
  | default k =>
      refine ⟨x.1.1, ?_⟩
      unfold wEnvDomRepr at hdom
      cases hpay : x.1.2 with
      | int m =>
          rw [hpay] at hdom
          obtain ⟨v, hvs, _hrepr⟩ := hdom
          refine ⟨[v], hvs, ?_⟩
          simp only [cascadeEval, Option.getD]
          exact IntDispatchSoundness.EvalCascade.default k x.1.1 [v]
      | opaqueFields fs =>
          rw [hpay] at hdom
          refine ⟨fs, hdom, ?_⟩
          simp only [cascadeEval, Option.getD]
          exact IntDispatchSoundness.EvalCascade.default k x.1.1 fs
  | test tyIdx leaf rest ih =>
      simp only [wCascadeInEnv, Bool.and_eq_true, decide_eq_true_eq] at hcasc
      obtain ⟨hpayTy, hrest⟩ := hcasc
      by_cases htag : x.1.1 = tyIdx
      · -- tested tag: the value must be a `hit` payload (`.int`).
        have hchild := x.2
        unfold WEnvValidChild at hchild
        rw [htag] at hchild
        rcases hchild with ⟨_, m, hm⟩ | ⟨c, hcshape, hcne, _⟩
        · unfold wEnvDomRepr at hdom
          rw [hm] at hdom
          obtain ⟨v, hvs, hrepr⟩ := hdom
          refine ⟨tyIdx, [v], by rw [← htag]; exact hvs, ?_⟩
          have hce : (cascadeEval (.test tyIdx leaf rest) x.1.1 x.1.2.toInt?).getD 0
              = IntDispatchSoundness.evalLeaf leaf m := by
            rw [hm]; simp [cascadeEval, htag, WPayload.toInt?]
          rw [hce]
          exact IntDispatchSoundness.EvalCascade.hit tyIdx leaf rest [v] m v rfl hrepr
        · -- a non-hit shape at a tested tag contradicts `wCascadeInEnv`.
          rw [hpayTy] at hcshape
          exact absurd (Option.some.inj hcshape).symm hcne
      · obtain ⟨tag, fields, hvs, hev⟩ := ih hrest
        refine ⟨tag, fields, hvs, ?_⟩
        have htageq : tag = x.1.1 := by
          unfold wEnvDomRepr at hdom
          cases hpay : x.1.2 with
          | int m =>
              rw [hpay] at hdom; obtain ⟨v, hvs2, _⟩ := hdom
              rw [hvs2] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
          | opaqueFields fs =>
              rw [hpay] at hdom
              rw [hdom] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
        have hne : x.1.1 ≠ tyIdx := htag
        have hce : (cascadeEval (.test tyIdx leaf rest) x.1.1 x.1.2.toInt?).getD 0
            = (cascadeEval rest x.1.1 x.1.2.toInt?).getD 0 := by
          simp only [cascadeEval, if_neg hne]
        rw [hce]
        subst htageq
        exact IntDispatchSoundness.EvalCascade.miss tyIdx x.1.1 leaf rest fields _ hne hev

/-- GOAL 2, final shape: the widened bridge in the form the acceptance-soundness
    assembly consumes. The model is `wEnvStructModel env plan.body`, the domain
    representation `wEnvDomRepr env`, and the residual bridge holds outright — for
    every represented input, whatever its non-hit payload shape. -/
theorem env_widenedIntDispatch_bridge (env : WAdtEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : wCascadeInEnv env plan.body = true)
    (S : CarrierSpec (wCarrierIdx env)) (x : WAdtVal env) (vs : List WVal)
    (hdom : wEnvDomRepr env S x vs) :
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
      ∀ w, S.Repr n w →
        intRepr S (wEnvStructModel env plan.body x) w := by
  obtain ⟨tag, fields, hvs, hev⟩ := wCascade_bridge env plan.body hcasc S x vs hdom
  refine ⟨tag, fields, (cascadeEval plan.body x.1.1 x.1.2.toInt?).getD 0, hvs, hev, ?_⟩
  intro w hw
  simpa [intRepr, wEnvStructModel] using hw

#print axioms wCascade_bridge
#print axioms env_widenedIntDispatch_bridge

end AverCert.WidenedEnvelope
