/-
Envelope lowering: the plan declares the meaning envelope it operates on
(the ADT it reads or builds, and — as future columns — a string result type,
or the absence of an effect), a canonical wall LOWER emits the exact bytes
that envelope must occupy, and the face pins `lower(envelope) = bytes`.

This is the same trick the wall already uses for function BODIES
(`PlanBytes.lower… = codeEntry`), lifted to the type envelope. There is NO
byte -> structure decoder in the meaning path: `domRepr`/`codRepr`/`model`
are wall terms computed FROM THE PLAN, never from a decode output. The only
value pulled up from the bytes is the compiler-assigned base type INDEX (a
bare opaque `Nat`), and even the byte OFFSET of a pinned entry is derived
from that index by cursor-only navigation that discards everything it reads.

## The general abstraction

`EnvelopeFact` is a plan-declared requirement together with its canonical
lowering to bytes; `EnvelopeFact.pinnedAt` is the pin `lower(req) = bytes at
target`. The ADT rec-group and the ADT function-signature entry are the FIRST
two instances. Further meaning envelopes plug in as additional `EnvelopeFact`
values without reworking this abstraction:

  * `stringConcat.resultTy` — an `EnvelopeFact` whose `Req` is the declared
    result element type and whose `lower` emits its type-section entry;
  * effect-absence ("no SSTORE", "no call") — an `EnvelopeFact` whose pin,
    combined with a once-per-lowering "no constructor emits the opcode"
    lemma (see `lowerConstructBody_no_call` below for the shape), turns an
    absence claim into a corollary of `lower(plan) = bytes` with no scan.
-/
import AcceptedArtifactCore
import IntDispatchSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering

open AverCert.Schema
open CertPrelude

/-! ## §0 Cursor-only navigation from a type INDEX to its byte position

This is NOT a decoder: every value `readTypeEntry`/`readTypeEntries` parses is
DISCARDED — only the cursor `(n, len)` advances. It answers one question: "at
which byte does the rectype whose first flattened type index is `target`
begin?" — the exact analogue of `CertDecode.codeLocs` locating code entries
for `funcBindingForExport`. A `target` that does not begin a rectype fails
closed. It is shared by every type-section `EnvelopeFact`. -/

def rectypeCursorAt : Nat → Nat → Nat → Nat → Nat → Option (Nat × Nat)
  | 0, _, _, _, _ => none
  | fuel + 1, target, flatBase, n, len =>
      if flatBase = target then some (n, len)
      else if len == 0 then none
      else if (n &&& 0xff) == 0x4e then
        match CertDecode.readU (n >>> 8) (len - 1) with
        | none => none
        | some (count, n1, len1) =>
            match CertDecode.readTypeEntries count n1 len1 with
            | none => none
            | some (_, n2, len2) =>
                if target < flatBase + count then none
                else rectypeCursorAt fuel target (flatBase + count) n2 len2
      else
        match CertDecode.readTypeEntry n len with
        | none => none
        | some (_, n1, len1) => rectypeCursorAt fuel target (flatBase + 1) n1 len1

def typeCursorAt (modBytes modLen target : Nat) : Option (Nat × Nat) :=
  match CertDecode.modulePayload 1 modBytes modLen with
  | none => none
  | some (tN, tLen) =>
      match CertDecode.readU tN tLen with
      | none => none
      | some (_count, n1, len1) => rectypeCursorAt (tLen + 1) target 0 n1 len1

/-- The module's type-section bytes at (the position of) type index `target`
    are EXACTLY `expected`. The length guard keeps the read inside the type
    section. This is the byte-level pin shared by all type-section facts. -/
def bytesPinnedAt (modBytes modLen target : Nat) (expected : List Nat) : Prop :=
  ∃ cur : Nat × Nat,
    typeCursorAt modBytes modLen target = some cur ∧
    expected.length ≤ cur.2 ∧
    CertDecode.takeBytes expected.length cur.1 = expected

/-! ## §1 The general envelope fact -/

/-- A plan-declared meaning envelope together with its canonical LOWER to the
    exact bytes it must occupy. `Req` is whatever the plan declares (an ADT
    profile, a result type, an effect profile); `lower` is the wall's
    structural encoder for that declaration, fail-closed. The pin is taken at
    a `target` type index that is supplied at the pin site (the ADT root, or a
    function-signature `typeIdx` read off the export binding). -/
structure EnvelopeFact where
  Req : Type
  lower : Req → Option (List Nat)

/-- THE PIN, one shape for every envelope column: the module's bytes at
    `target` are exactly `lower req`. -/
@[reducible] def EnvelopeFact.pinnedAt
    (F : EnvelopeFact) (modBytes modLen target : Nat) (req : F.Req) : Prop :=
  ∃ bs, F.lower req = some bs ∧ bytesPinnedAt modBytes modLen target bs

/-! ## §2 The ADT envelope (the first instance)

`root` is the compiler-assigned base type index — THE opaque residue pulled up
from the bytes. Everything else is DECLARED structure: `ctors[i] = true` iff
source constructor `i` carries the one Int payload. Constructor `i` lowers to
type index `root + 1 + i`; the prelude tail (string array, limb array, Int
carrier) follows the constructors inside the same rec group, so the carrier
index is DERIVED, never decoded. -/

structure AdtEnvelope where
  root : Nat
  ctors : List Bool
deriving Repr, DecidableEq

def carrierIdxE (env : AdtEnvelope) : Nat := env.root + env.ctors.length + 3
def limbIdxE (env : AdtEnvelope) : Nat := env.root + env.ctors.length + 2

/-- Profile checker, fail-closed. The `< 64` bound keeps every index in the
    single-byte LEB/s33 regime (covers the current compiler's certified
    profile; lifting it is a routine uleb exercise, not a design change). -/
def checkAdtEnvelope (env : AdtEnvelope) : Bool :=
  decide (1 ≤ env.ctors.length) && decide (carrierIdxE env < 64)

/-! ## §3 Canonical byte LOWERING of the declared ADT envelope (meaning -> bytes)

Mirrors `src/codegen/wasm_gc/module.rs`: one rec group
  `4e <n+4>  (sub [] struct{})  (sub_final [root] struct{(ref null C)}|{})*
             (array i8 mut) (array i64 mut)
             (struct {i64 mut, (ref null limbs) mut, i32 mut})`
exactly as `PlanBytes` lowers bodies: structural recursion over the plan,
emitting bytes. -/

def lowerCtorEntry (root C : Nat) : Bool → List Nat
  | true => [0x4f, 0x01, root, 0x5f, 0x01, 0x63, C, 0x00]
  | false => [0x4f, 0x01, root, 0x5f, 0x00]

def lowerCtorEntries (root C : Nat) : List Bool → List Nat
  | [] => []
  | b :: rest => lowerCtorEntry root C b ++ lowerCtorEntries root C rest

/-- The canonical wasm-gc prelude tail the compiler appends inside the ADT's
    rec group: string byte-array, bignum limb array, Int carrier struct. -/
def preludeTail (limb : Nat) : List Nat :=
  [0x5e, 0x78, 0x01,
   0x5e, 0x7e, 0x01,
   0x5f, 0x03, 0x7e, 0x01, 0x63, limb, 0x01, 0x7f, 0x01]

/-- LOWER for the whole declared rec group, header included. -/
def lowerAdtRecGroup (env : AdtEnvelope) : Option (List Nat) :=
  if checkAdtEnvelope env = true then
    some ([0x4e, env.ctors.length + 4, 0x50, 0x00, 0x5f, 0x00] ++
      lowerCtorEntries env.root (carrierIdxE env) env.ctors ++
      preludeTail (limbIdxE env))
  else none

/-- LOWER for the dispatch export's function-signature type entry:
    `(ref null root) -> (ref null carrier)`. -/
def lowerDispatchSig (env : AdtEnvelope) : Option (List Nat) :=
  if checkAdtEnvelope env = true then
    some [0x60, 0x01, 0x63, env.root, 0x01, 0x63, carrierIdxE env]
  else none

/-- LOWER for a constructor export's signature: `(ref null carrier) -> (ref null root)`. -/
def lowerCtorSig (env : AdtEnvelope) : Option (List Nat) :=
  if checkAdtEnvelope env = true then
    some [0x60, 0x01, 0x63, carrierIdxE env, 0x01, 0x63, env.root]
  else none

/-! ### The ADT `EnvelopeFact` instances -/

/-- The ADT rec-group fact: declared `AdtEnvelope`, lowered to the whole
    type-section rec group, pinned at `env.root`. -/
def adtRecGroupFact : EnvelopeFact := ⟨AdtEnvelope, lowerAdtRecGroup⟩

/-- The ADT dispatch-signature fact, pinned at the export binding's `typeIdx`. -/
def adtDispatchSigFact : EnvelopeFact := ⟨AdtEnvelope, lowerDispatchSig⟩

/-- The ADT constructor-signature fact, pinned at the export binding's `typeIdx`. -/
def adtCtorSigFact : EnvelopeFact := ⟨AdtEnvelope, lowerCtorSig⟩

/-! ## §4 Wall-owned meaning terms over the PLAN structure (no decode output)

`Dom`, `domRepr`, `codRepr`, `model` are computed from the envelope + the
checked cascade. No `AdtTS`, no `TypeInfo`, no view function. -/

/-- Declared payload discipline of the tag `root+1+i |-> ctors[i]`. -/
def ctorPayload? (env : AdtEnvelope) (tag : Nat) : Option Bool :=
  if env.root + 1 ≤ tag then env.ctors[tag - (env.root + 1)]? else none

def EnvValidChild (env : AdtEnvelope) (tag : Nat) (payload : Option Int) : Prop :=
  (ctorPayload? env tag = some true ∧ ∃ n, payload = some n) ∨
  (ctorPayload? env tag = some false ∧ payload = none)

/-- A value of the DECLARED ADT: a constructor tag plus its Int payload
    exactly when the plan declares one. -/
def AdtVal (env : AdtEnvelope) : Type :=
  { p : Nat × Option Int // EnvValidChild env p.1 p.2 }

/-- Canonical domain representation from the plan structure: total in `x`
    (vacuity structurally impossible), zero producer choice. -/
def envDomRepr (env : AdtEnvelope) :
    CarrierSpec (carrierIdxE env) → AdtVal env → List WVal → Prop :=
  fun S x vs =>
    match x.1.2 with
    | some n => ∃ v, vs = [.structv x.1.1 [v]] ∧ S.Repr n v
    | none => vs = [.structv x.1.1 []]

/-- Same relation as a single-result representation (constructor codomain). -/
def envCodRepr (env : AdtEnvelope) :
    CarrierSpec (carrierIdxE env) → AdtVal env → WVal → Prop :=
  fun S y w =>
    match y.1.2 with
    | some n => ∃ v, w = .structv y.1.1 [v] ∧ S.Repr n v
    | none => w = .structv y.1.1 []

/-- Canonical Int argument representation (constructor domain). -/
def intArgDomRepr (C : Nat) : CarrierSpec C → Int → List WVal → Prop :=
  fun S n vs => ∃ v, vs = [v] ∧ S.Repr n v

/-- Total functional cascade evaluation (leaves reuse the real
    `IntDispatchSoundness.evalLeaf`). -/
def cascadeEval : IntDispatchCascade → Nat → Option Int → Option Int
  | .default k, _, _ => some k
  | .test tyIdx leaf rest, tag, payload =>
      if tag = tyIdx then payload.map (IntDispatchSoundness.evalLeaf leaf)
      else cascadeEval rest tag payload

/-- structModelFromPlan: THE model, computed from the checked plan. -/
def envStructModel (env : AdtEnvelope) (body : IntDispatchCascade) :
    AdtVal env → Int :=
  fun x => (cascadeEval body x.1.1 x.1.2).getD 0

/-- Plan-INTERNAL consistency (a pure plan <-> plan check): every tag the
    cascade tests and projects must be a DECLARED payload constructor of the
    envelope. Both plan components are separately byte-pinned by lowering, so
    this closes the triangle cascade <-> envelope <-> bytes with no decode. -/
def cascadeInEnv (env : AdtEnvelope) : IntDispatchCascade → Bool
  | .default _ => true
  | .test tyIdx _ rest =>
      decide (ctorPayload? env tyIdx = some true) && cascadeInEnv env rest

/-- Constructor model from the plan: build the DECLARED payload constructor
    `tag` with the Int argument as payload. The payload fact is DEMANDED. -/
def envCtorModel (env : AdtEnvelope) (tag : Nat)
    (h : ctorPayload? env tag = some true) : Int → AdtVal env :=
  fun n => ⟨(tag, some n), Or.inl ⟨h, n, rfl⟩⟩

/-! ## §5 The lower-pinned ADT faces

FREE-PARAMETER AUDIT — complete input list of `AdtIntFaceLower`:
  modBytes/modLen   — the artifact bytes (sha256-pinned upstream);
  exportNameBytes/exportName — the claimed export, bound by
                      `funcBindingForExport` (fail-closed);
  carrier           — claim data, pinned to `carrierIdxE env` (derived);
  hostTable         — claim data, byte-checked through the code-entry gate;
  env, plan         — PLAN data. Each is pinned meaning -> bytes:
                      `lowerIntDispatchCodeEntry plan = codeEntry` (existing),
                      `adtRecGroupFact.pinnedAt … env.root env`,
                      `adtDispatchSigFact.pinnedAt … binding.typeIdx env`;
                      `cascadeInEnv` ties them to each other;
  o                 — the obligation; Dom/Cod/domRepr/codRepr/model are all
                      pinned to terms COMPUTED from env/plan.
NOT inputs: any decoded type structure, any view function, any byte offset
(derived from `env.root` by navigation). A fabricated type structure is not
merely refuted — it is UNSTATABLE (there is no such input). -/

def AdtIntFaceLower
    (modBytes modLen : Nat)
    (exportNameBytes : List Nat) (exportName : String)
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation) : Prop :=
  AverCert.AcceptedArtifact.intDispatchPlanAccepted
    modBytes modLen exportNameBytes exportName carrier hostTable plan o ∧
  checkAdtEnvelope env = true ∧
  cascadeInEnv env plan.body = true ∧
  carrier = carrierIdxE env ∧
  adtRecGroupFact.pinnedAt modBytes modLen env.root env ∧
  (∃ binding,
    AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes
      = some binding ∧
    adtDispatchSigFact.pinnedAt modBytes modLen binding.typeIdx env) ∧
  o.carrier = carrierIdxE env ∧
  HEq o.Dom (AdtVal env) ∧
  HEq o.Cod Int ∧
  HEq o.domRepr (envDomRepr env) ∧
  HEq o.codRepr (@AverCert.Schema.intRepr (carrierIdxE env)) ∧
  HEq o.model (envStructModel env plan.body)

/-- Constructor face (unary Int-payload constructor profile). `structIdx` is
    pinned twice: by the exact code entry (`struct.new structIdx` immediate)
    and by the DECLARED payload fact `ctorPayload? env structIdx = some true`,
    whose envelope is byte-pinned by lowering. -/
def AdtCtorFaceLower
    (modBytes modLen : Nat)
    (exportNameBytes : List Nat) (exportName : String)
    (carrier structIdx fieldCount : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (env : AdtEnvelope) (plan : ConstructRawPlan)
    (o : Obligation) : Prop :=
  AverCert.AcceptedArtifact.constructPlanAccepted
    modBytes modLen exportNameBytes exportName carrier structIdx fieldCount
    elemTy symPlan plan o ∧
  plan.arity = 1 ∧ plan.fields = [.local 0] ∧
  checkAdtEnvelope env = true ∧
  carrier = carrierIdxE env ∧
  adtRecGroupFact.pinnedAt modBytes modLen env.root env ∧
  (∃ binding,
    AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes
      = some binding ∧
    adtCtorSigFact.pinnedAt modBytes modLen binding.typeIdx env) ∧
  o.carrier = carrierIdxE env ∧
  ∃ hpay : ctorPayload? env structIdx = some true,
    HEq o.Dom Int ∧
    HEq o.Cod (AdtVal env) ∧
    HEq o.domRepr (intArgDomRepr (carrierIdxE env)) ∧
    HEq o.codRepr (envCodRepr env) ∧
    HEq o.model (envCtorModel env structIdx hpay)

/-! ## §6 Generic forcing lemmas (fixture-independent)

The envelope IS an input, but the two lower-pins leave a forger ZERO choice on
a given module: the signature pin forces `root` and the carrier index (hence
the constructor count), and the rec-group pin then decides the payload map.
These are the encoder-injectivity facts the fixture uniqueness theorems use;
they involve no bytes of their own. -/

theorem takeBytes_take :
    ∀ (k m n : Nat), k ≤ m →
      CertDecode.takeBytes k n = (CertDecode.takeBytes m n).take k := by
  intro k
  induction k with
  | zero => intro m n _; simp [CertDecode.takeBytes]
  | succ k ih =>
      intro m n h
      obtain ⟨m', rfl⟩ : ∃ m', m = m' + 1 := ⟨m - 1, by omega⟩
      simp only [CertDecode.takeBytes, List.take_succ_cons]
      exact congrArg _ (ih m' (n >>> 8) (by omega))

theorem sig_forces_dispatch (env : AdtEnvelope) (sig : List Nat)
    (hlow : lowerDispatchSig env = some sig)
    (heq : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05]) :
    env.root = 0 ∧ env.ctors.length = 2 := by
  unfold lowerDispatchSig at hlow
  by_cases hc : checkAdtEnvelope env = true
  · rw [if_pos hc] at hlow
    have h := (Option.some.inj hlow).trans heq
    have hroot : env.root = 0 := congrArg (fun l => l.getD 3 0) h
    have hcar : carrierIdxE env = 5 := congrArg (fun l => l.getD 6 0) h
    refine ⟨hroot, ?_⟩
    unfold carrierIdxE at hcar
    omega
  · rw [if_neg hc] at hlow; exact absurd hlow (by simp)

theorem sig_forces_ctor (env : AdtEnvelope) (sig : List Nat)
    (hlow : lowerCtorSig env = some sig)
    (heq : sig = [0x60, 0x01, 0x63, 0x06, 0x01, 0x63, 0x00]) :
    env.root = 0 ∧ env.ctors.length = 3 := by
  unfold lowerCtorSig at hlow
  by_cases hc : checkAdtEnvelope env = true
  · rw [if_pos hc] at hlow
    have h := (Option.some.inj hlow).trans heq
    have hcar : carrierIdxE env = 6 := congrArg (fun l => l.getD 3 0) h
    have hroot : env.root = 0 := congrArg (fun l => l.getD 6 0) h
    refine ⟨hroot, ?_⟩
    unfold carrierIdxE at hcar
    omega
  · rw [if_neg hc] at hlow; exact absurd hlow (by simp)

/-! ## §7 Effect-absence, the shape of the future effect column

An absence claim ("no call", "no SSTORE") is a corollary of `lower(plan) =
bytes` plus "the plan has no such node": prove once, per lowering, that no
constructor emits the opcode — by structural induction on the plan, with NO
byte scan. The byte-level statement transports along the pin because the byte
encoder is the same structural recursion (`PlanBytes` mirrors `PlanLower` node
by node). Below: the REAL wall constructor lowering provably emits no `call`. -/

theorem lowerConstructFields_no_call (structIdx : Nat)
    (fields : List ConstructField) :
    ∀ i ∈ AverCert.PlanLower.lowerConstructFields structIdx fields,
      ∀ k, i ≠ .call k := by
  induction fields with
  | nil => intro i hi; simp [AverCert.PlanLower.lowerConstructFields] at hi
  | cons f rest ih =>
      intro i hi k
      simp only [AverCert.PlanLower.lowerConstructFields, List.mem_cons] at hi
      rcases hi with hi | hi
      · subst hi; cases f <;> simp [AverCert.PlanLower.lowerConstructField]
      · exact ih i hi k

/-- Absence-of-effect corollary, REAL lowering: a lowered constructor body
    contains no `call` — proved from the PLAN, not from the bytes. -/
theorem lowerConstructBody_no_call (structIdx : Nat) (plan : ConstructRawPlan)
    (body : List WInstr)
    (hlow : AverCert.PlanLower.lowerConstructBody structIdx plan = some body) :
    ∀ i ∈ body, ∀ k, i ≠ .call k := by
  unfold AverCert.PlanLower.lowerConstructBody at hlow
  by_cases hc : AverCert.PlanCheck.checkConstructRawPlan plan = true
  · rw [if_pos hc] at hlow
    intro i hi k
    rw [← Option.some.inj hlow] at hi
    rcases List.mem_append.mp hi with hi | hi
    · exact lowerConstructFields_no_call structIdx plan.fields i hi k
    · simp at hi; subst hi; simp
  · rw [if_neg hc] at hlow; exact absurd hlow (by simp)

end AverCert.EnvelopeLowering
