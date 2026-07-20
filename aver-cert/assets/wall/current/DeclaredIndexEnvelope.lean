/-
Declared-index envelope lowering (real shared-rec-group modules).

`EnvelopeLowering` / `WidenedEnvelope` derive every non-root type index
ARITHMETICALLY: `carrier = root + ctors.length + 3`, string array `root + len + 1`,
and so on, and model each ADT as living in its OWN rec group. That is exactly the
shape the SYNTHETIC `synthMod` test modules had. It is WRONG for the modules the
real compiler emits: `aver compile --target wasm-gc` puts ALL ADTs of a module
PLUS the shared `List`/`Map` cons structs and the Int carrier into ONE rec group,
with compiler-assigned type indices and source-order constructor positions. In
`examples/data/json.av` the `Json` ADT's Int carrier sits at type index 21, its
string byte-array at 19, its `List` cons at 27, its `Map` struct at 38 — none of
which equals `root + len + 3 = 10` (index 10 is in fact the *other* ADT's
`ParseResult.Err`).

This module stops COMPUTING those indices. It DECLARES them — the carrier index,
each constructor's flattened type index, and each non-Int payload target index —
as OPAQUE `Nat` fields pulled up from the bytes, and pins each constructor's
synthesized struct-body AT ITS DECLARED INDEX in the REAL module. The struct-body
bytes are still SYNTHESIZED from the plan (they already byte-match the compiler);
the only residue lifted from the bytes are those opaque indices. Source order and
multiple-ADTs-per-module are then non-problems: each constructor is pinned wherever
it actually sits, navigated to by a cursor that descends into the shared rec group
and DISCARDS every value it parses.

There is NO byte -> structure decoder: `dEnvDomRepr` / `dEnvStructModel` are
computed FROM THE PLAN; the cursor only advances, never yields a parsed type.
-/
import WidenedEnvelope
import IntDispatchSoundness
import ConstructVerbatimSoundness

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.DeclaredIndexEnvelope

open AverCert.Schema
open CertPrelude
open AverCert.WidenedEnvelope (WCtor WPayload)
open AverCert.EnvelopeLowering (cascadeEval takeBytes_take)

/-! ## §0 The type-section START cursor, and the single concat pin

The previous revision navigated to each constructor's flattened index with a
fuel loop (`entryStep`) that called `CertDecode.readTypeEntry` on every preceding
entry to COUNT positions. That put a per-entry byte-structure parser in the
trusted confirmation path: a `readTypeEntry` length mistake would land the cursor
at a wrong position and the pin would confirm a wrong body at a wrong index.

That parser is now GONE. The only byte navigation that remains is locating the
type section's entry region — `modulePayload 1` (section framing) followed by
`readU` (skip the vector count LEB). Neither descends into the entry bytes; both
are fixed section-framing locates. Positions are no longer walked out of the
bytes: they are DECLARED (see `declaredConcat`) and CONFIRMED by one byte-slice
equality against the located start. -/

/-- Cursor at the FIRST type-section entry: the section payload located by
    `modulePayload 1`, advanced past the vector count `readU` reads. No entry is
    parsed; this is the type-section-framing locate and nothing more. -/
def typeSectionCursor (modBytes modLen : Nat) : Option (Nat × Nat) :=
  match CertDecode.modulePayload 1 modBytes modLen with
  | none => none
  | some (tN, tLen) =>
      match CertDecode.readU tN tLen with
      | none => none
      | some (_count, n1, len1) => some (n1, len1)

/-- The module's type-section entry bytes, taken from the located start, are
    EXACTLY `expected`. A single byte-slice equality over the whole declared
    prefix of the type section — not a per-entry walk. Because `expected` is a
    concatenation of DECLARED chunks (§2 `declaredConcat`), this one equality
    fixes every declared byte, hence every declared position, by construction. -/
def concatPinnedAt (modBytes modLen : Nat) (expected : List Nat) : Prop :=
  ∃ cur : Nat × Nat,
    typeSectionCursor modBytes modLen = some cur ∧
    expected.length ≤ cur.2 ∧
    CertDecode.takeBytes expected.length cur.1 = expected

/-! ## §1 The declared-index envelope

Every index is DECLARED, not computed. `DCtor.idx` is the constructor's flattened
type index (its source-order position in the shared group); `DCtor.target` is the
declared payload TARGET index the constructor's ref field points at (the carrier
for `hit`, the string array for `strBox`, the `List` cons for `listBox`, the `Map`
struct for `mapBox`; unused by the payloadless / scalar shapes). `root` and
`carrier` are the two remaining declared indices. -/

structure DCtor where
  idx    : Nat
  shape  : WCtor
  target : Nat
deriving Repr, DecidableEq

structure DIdxEnvelope where
  root    : Nat
  carrier : Nat
  ctors   : List DCtor
deriving Repr, DecidableEq

/-! ## §2 Canonical byte LOWERING of one declared constructor (meaning -> bytes)

Mirrors `src/codegen/wasm_gc/module.rs`: each constructor is one
`(sub_final [root] struct{…})` entry. The struct body is a function of the shape
and the declared target index. The ref shapes (`hit`/`strBox`/`listBox`/`mapBox`)
share the `struct{(ref null target)}` body and differ ONLY in which declared index
`target` names; the scalar/unit shapes ignore `target`. -/

def dCtorBody (root : Nat) (c : DCtor) : List Nat :=
  0x4f :: 0x01 :: root ::
    (match c.shape with
     | .hit      => [0x5f, 0x01, 0x63, c.target, 0x00]
     | .strBox   => [0x5f, 0x01, 0x63, c.target, 0x00]
     | .listBox  => [0x5f, 0x01, 0x63, c.target, 0x00]
     | .mapBox   => [0x5f, 0x01, 0x63, c.target, 0x00]
     | .floatBox => [0x5f, 0x01, 0x7c, 0x00]
     | .boolBox  => [0x5f, 0x01, 0x7f, 0x00]
     | .unit     => [0x5f, 0x00])

/-- The DECLARED type-section prefix as one ordered byte list: an opaque declared
    chunk (`typePrefix` — the rec-group header and every entry BEFORE the ADT's
    constructors: other ADTs, `List`/`Map` cons structs, the prelude, function
    signatures, all named as literal bytes), followed by the ADT's constructor
    entries SYNTHESIZED from meaning in source order (`dCtorBody`). Every position
    is fixed by construction: constructor `k`'s flattened index is the number of
    entries before it, and its byte offset is `typePrefix.length` plus the summed
    lengths of the earlier constructor bodies. No byte is read to compute either. -/
def declaredConcat (typePrefix : List Nat) (env : DIdxEnvelope) : List Nat :=
  typePrefix ++ (env.ctors.map (dCtorBody env.root)).flatten

/-- Declared shape at a flattened index (fail-closed): the constructor whose
    declared `idx` equals `tag`. On its own the label lookup would be FREE —
    `dCtorBody` never reads `c.idx` and `declaredConcat` lays bodies by list
    order, so permuting labels is invisible to a body-only byte pin. The unified
    walk (§2c) is what makes the label DERIVED from byte position: the ctor at
    counted position `tag` is the one whose body the walk fixes at that position
    by equality, so `dCtorShape? env tag` can only resolve `tag` to a shape the
    real entry at type index `tag` actually carries. -/
def dCtorShape? (env : DIdxEnvelope) (tag : Nat) : Option WCtor :=
  (env.ctors.find? (fun c => decide (c.idx = tag))).map (fun c => c.shape)

/-! ## §2c The UNIFIED walk-match pin (position by counting, content by equality)

ONE single-pass traversal of the type section replaces the former split of a
whole-prefix concat equality plus a separate entry-count walk. Entry by entry,
starting at flattened index 0, the walk:

* skips a rec-group header (`0x4e` + single-byte member count) WITHOUT counting
  — group boundaries do not contribute a flattened index;
* at a position where a constructor is DECLARED (`idx = current count`), confirms
  the entry bytes EQUAL that constructor's declared template `dCtorBody` and
  advances by the TEMPLATE's length — the length comes from the DECLARATION, and
  the bytes are matched by equality, never read for meaning;
* at every other (undeclared) position, NAVIGATES one entry by its REAL byte
  length via the WebAssembly type-entry grammar and counts it — the navigator
  yields only the advanced cursor (a position), never a surfaced shape.

Both jobs — assigning each entry its flattened index by counting, and confirming
each declared constructor's body at its declared index by equality — are done in
this ONE traversal. The only primitives are length-advance (`bEntryStep`,
`takeBytes`-length) and equality (`takeBytes … = template`); there is no
shape-to-meaning decoder. It also drops the contiguity assumption of
`declaredConcat` (`prefix ++ bodies.flatten`): declared constructors may be
separated by unrelated navigated entries and still be pinned at their true
indices, because position is counted, not assumed. A relabelled (idx-swapped)
constructor fails because the walk matches its template at the position its label
names, and the real bytes there are a different entry — the equality breaks. -/

/-- Peel one byte off the little-endian cursor `(n, len)`; fail-closed at the
    end of the bounded region. -/
@[inline] def dpByte : Nat × Nat → Option (Nat × (Nat × Nat))
  | (n, len) => if len = 0 then none else some (n &&& 0xff, (n >>> 8, len - 1))

/-- Navigate one single-byte (`< 0x80`) LEB index over the cursor. -/
def bIdxByte : Nat × Nat → Option (Nat × Nat)
  | (n, len) => if len = 0 then none
      else if (n &&& 0xff) < 0x80 then some (n >>> 8, len - 1) else none

/-- Navigate one value/storage type: a one-byte code (numeric, packed, or
    abstract-ref shorthand `0x65..0x7f`) or a `(ref …)`/`(ref null …)` prefix
    (`0x63`/`0x64`) followed by a single-byte heap type. Fail-closed otherwise. -/
def bValStep : Nat × Nat → Option (Nat × Nat)
  | (n, len) => if len = 0 then none else
      let b := n &&& 0xff
      if b = 0x63 ∨ b = 0x64 then bIdxByte (n >>> 8, len - 1)
      else if 0x65 ≤ b ∧ b ≤ 0x7f then some (n >>> 8, len - 1)
      else none

/-- Navigate one field type: a storage type then a mutability byte. -/
def bFieldStep (c : Nat × Nat) : Option (Nat × Nat) :=
  match bValStep c with
  | some (n, len) => if len = 0 then none else
      let m := n &&& 0xff
      if m = 0x00 ∨ m = 0x01 then some (n >>> 8, len - 1) else none
  | none => none

/-- Navigate `k` items with `step`. -/
def bSteps (step : Nat × Nat → Option (Nat × Nat)) : Nat → Nat × Nat → Option (Nat × Nat)
  | 0,     c => some c
  | k + 1, c => (step c).bind (bSteps step k)

/-- Navigate one composite type: `struct` (`0x5f` + field vector), `array`
    (`0x5e` + one field), or `func` (`0x60` + param vector + result vector).
    Vector counts must be single-byte LEBs; fail-closed otherwise. -/
def bCompStep : Nat × Nat → Option (Nat × Nat)
  | (n, len) => if len = 0 then none else
      let b := n &&& 0xff
      let c1 := (n >>> 8, len - 1)
      if b = 0x5f then
        match dpByte c1 with
        | some (nf, c2) => if nf < 0x80 then bSteps bFieldStep nf c2 else none
        | none => none
      else if b = 0x5e then bFieldStep c1
      else if b = 0x60 then
        match dpByte c1 with
        | some (np, c2) =>
            if np < 0x80 then
              match bSteps bValStep np c2 with
              | some c3 =>
                  match dpByte c3 with
                  | some (nr, c4) => if nr < 0x80 then bSteps bValStep nr c4 else none
                  | none => none
              | none => none
            else none
        | none => none
      else none

/-- Navigate one complete type entry: a `sub` / `sub final` header
    (`0x50`/`0x4f` + single-byte supertype vector) followed by a composite type,
    or a bare composite type. Returns only the advanced cursor. -/
def bEntryStep : Nat × Nat → Option (Nat × Nat)
  | (n, len) => if len = 0 then none else
      let b := n &&& 0xff
      let c1 := (n >>> 8, len - 1)
      if b = 0x50 ∨ b = 0x4f then
        match dpByte c1 with
        | some (m, c2) => if m < 0x80 then (bSteps bIdxByte m c2).bind bCompStep else none
        | none => none
      else bCompStep (n, len)

/-- The single-pass walk-match. `fuel` bounds the traversal, `idx` is the running
    flattened index, `(n, len)` the cursor. Succeeds once every declared
    constructor has been passed (each matched at its own index along the way). -/
def dWalkFuel (root : Nat) (ctors : List DCtor) : Nat → Nat → Nat × Nat → Bool
  | 0,        _,   _        => false
  | fuel + 1, idx, (n, len) =>
      if ctors.all (fun c => decide (c.idx < idx)) then true
      else if len = 0 then false
      else
        let b := n &&& 0xff
        if b = 0x4e then
          -- rec-group header: consume `0x4e` + single-byte member count, no count
          if decide (2 ≤ len) && decide (((n >>> 8) &&& 0xff) < 0x80) then
            dWalkFuel root ctors fuel idx (n >>> 16, len - 2)
          else false
        else
          match ctors.find? (fun c => decide (c.idx = idx)) with
          | some c =>
              let tmpl := dCtorBody root c
              if decide (tmpl.length ≤ len) && (CertDecode.takeBytes tmpl.length n == tmpl) then
                dWalkFuel root ctors fuel (idx + 1) (n >>> (8 * tmpl.length), len - tmpl.length)
              else false
          | none =>
              match bEntryStep (n, len) with
              | some c' => dWalkFuel root ctors fuel (idx + 1) c'
              | none => false

/-- THE PIN: from the located type-section start, the unified walk succeeds. One
    traversal both counts positions and confirms every declared constructor's
    body at its declared index by equality — replacing `concatPinnedAt
    (declaredConcat …)` and any separate index-alignment walk. -/
def dWalkPinned (modBytes modLen : Nat) (env : DIdxEnvelope) : Prop :=
  ∃ cur : Nat × Nat,
    typeSectionCursor modBytes modLen = some cur ∧
    dWalkFuel env.root env.ctors (cur.2 + 1) 0 cur = true

/-- Profile checker, fail-closed. Single-byte index regime (`< 64`, covering the
    real fixtures whose largest declared index is the `Map` struct at 38), at
    least one constructor, and — crucially — the declared `carrier` is the target
    of every `hit` constructor, so the Int-box read index and the pinned hit body
    name the SAME index. -/
def checkDIdxEnvelope (env : DIdxEnvelope) : Bool :=
  decide (1 ≤ env.ctors.length) &&
  decide (env.carrier < 64) && decide (env.root < 64) &&
  env.ctors.all (fun c =>
    decide (c.idx < 64) && decide (c.target < 64) &&
    (if c.shape == WCtor.hit then decide (c.target = env.carrier) else true))

/-! ## §3 Wall-owned meaning terms over the PLAN (no decode output)

Identical discipline to `WidenedEnvelope`, but the carrier index is the DECLARED
`env.carrier`, not `wCarrierIdx`. A payload-BINDING (`proj`/`hostOp`) arm reads the
`hit` constructor's Int payload; a `const` arm reads NO field and returns its
constant regardless of the (possibly absent) payload, so it is sound at any
declared constructor — including a nullary (`unit`) one. `dCascadeEval` is the
declared-face model that gives a matched `const` arm its constant directly; a
non-hit payload under a BINDING arm is excluded by `dCascadeInEnv`. -/

def DEnvValidChild (env : DIdxEnvelope) (tag : Nat) (p : WPayload) : Prop :=
  (dCtorShape? env tag = some .hit ∧ ∃ n, p = .int n) ∨
  (∃ c, dCtorShape? env tag = some c ∧ c ≠ .hit ∧ ∃ fs, p = .opaqueFields fs)

def DAdtVal (env : DIdxEnvelope) : Type :=
  { q : Nat × WPayload // DEnvValidChild env q.1 q.2 }

def dEnvDomRepr (env : DIdxEnvelope) :
    CarrierSpec env.carrier → DAdtVal env → List WVal → Prop :=
  fun S x vs =>
    match x.1.2 with
    | .int n => ∃ v, vs = [.structv x.1.1 [v]] ∧ S.Repr n v
    | .opaqueFields fs => vs = [.structv x.1.1 fs]

/-- Declared-face cascade model. Like `EnvelopeLowering.cascadeEval` but a matched
    `const` arm returns its constant WITHOUT consulting the payload: without this,
    a nullary value (`payload = none`) would send every arm through `none.map _`
    to the wrong `getD 0 = 0`. Kept separate from the shared `cascadeEval` so the
    narrow/widened bridges (which never carry a `const` arm) are untouched. -/
def dCascadeEval : IntDispatchCascade → Nat → Option Int → Option Int
  | .default k, _, _ => some k
  | .test tyIdx leaf rest, tag, payload =>
      if tag = tyIdx then
        (match leaf with
         | .const k => some k
         | _ => payload.map (IntDispatchSoundness.evalLeaf leaf))
      else dCascadeEval rest tag payload

def dEnvStructModel (env : DIdxEnvelope) (body : IntDispatchCascade) :
    DAdtVal env → Int :=
  fun x => (dCascadeEval body x.1.1 x.1.2.toInt?).getD 0

/-- Plan-INTERNAL declared-envelope consistency, LEAF-AWARE. A payload-BINDING
    (`proj`/`hostOp`) arm projects the tested variant's field, so its tag must be a
    declared `hit` (Int-payload) constructor. A `const` arm reads no field, so its
    tag need only be a DECLARED constructor of any shape (a nullary `unit` is
    fine). A uniform `= some .hit` would wrongly reject the nullary const arms; a
    uniform `.isSome` would be UNSOUND for a binding arm (it could project an
    opaque non-Int payload). -/
def dCascadeInEnv (env : DIdxEnvelope) : IntDispatchCascade → Bool
  | .default _ => true
  | .test tyIdx leaf rest =>
      (match leaf with
       | .const _ => (dCtorShape? env tyIdx).isSome
       | .proj => decide (dCtorShape? env tyIdx = some .hit)
       | .hostOp _ _ _ => decide (dCtorShape? env tyIdx = some .hit)) &&
      dCascadeInEnv env rest

/-! ## §4 The declared-index Int-read face

The pin is now ONE byte-slice equality over the whole declared type-section
prefix (`concatPinnedAt … (declaredConcat typePrefix env)`), replacing the former
per-constructor walked pin. Every declared byte — `env.root`, each `c.target`, and
the exact source-order placement of every synthesized constructor body — sits
inside that single `expected` list, so the equality against the real bytes fixes
all of them at once. `env.carrier` equals every hit `c.target` by the checker,
and each hit `c.target` byte lives inside the pinned concat, so the carrier index
is confirmed by byte equality, not chosen. No `readTypeEntry` walk participates. -/

def DIdxIntReadFace
    (modBytes modLen : Nat) (_typePrefix : List Nat)
    (env : DIdxEnvelope) (plan : IntDispatchRawPlan) (o : Obligation) : Prop :=
  checkDIdxEnvelope env = true ∧
  dCascadeInEnv env plan.body = true ∧
  dWalkPinned modBytes modLen env ∧
  o.carrier = env.carrier ∧
  HEq o.Dom (DAdtVal env) ∧
  HEq o.Cod Int ∧
  HEq o.domRepr (dEnvDomRepr env) ∧
  HEq o.codRepr (@AverCert.Schema.intRepr env.carrier) ∧
  HEq o.model (dEnvStructModel env plan.body)

/-! ## §5 The positive bridge (BINDING arms read hit, CONST arms read nothing)

On any represented declared value the byte-origin `EvalCascade` relates it to
exactly the plan-computed `dCascadeEval` result. A matched `const` arm relates by
`constHit` (reads no field, sound for a nullary value); a matched BINDING arm
relates by `hit` (its tag is `hit`, so the value carries the readable Int payload);
a non-matched tag recurses through `miss` regardless of leaf shape. Mirrors
`WidenedEnvelope.wCascade_bridge` over the declared carrier, extended with `const`. -/

theorem dCascade_bridge (env : DIdxEnvelope) (body : IntDispatchCascade)
    (hcasc : dCascadeInEnv env body = true)
    (S : CarrierSpec env.carrier) (x : DAdtVal env) (vs : List WVal)
    (hdom : dEnvDomRepr env S x vs) :
    ∃ tag fields,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S body tag fields
        ((dCascadeEval body x.1.1 x.1.2.toInt?).getD 0) := by
  induction body with
  | default k =>
      refine ⟨x.1.1, ?_⟩
      unfold dEnvDomRepr at hdom
      cases hpay : x.1.2 with
      | int m =>
          rw [hpay] at hdom
          obtain ⟨v, hvs, _hrepr⟩ := hdom
          refine ⟨[v], hvs, ?_⟩
          simp only [dCascadeEval, Option.getD]
          exact IntDispatchSoundness.EvalCascade.default k x.1.1 [v]
      | opaqueFields fs =>
          rw [hpay] at hdom
          refine ⟨fs, hdom, ?_⟩
          simp only [dCascadeEval, Option.getD]
          exact IntDispatchSoundness.EvalCascade.default k x.1.1 fs
  | test tyIdx leaf rest ih =>
      simp only [dCascadeInEnv, Bool.and_eq_true] at hcasc
      obtain ⟨hleaf, hrest⟩ := hcasc
      by_cases htag : x.1.1 = tyIdx
      · -- matched tag: split on the leaf shape
        cases leaf with
        | const kk =>
            -- const arm: reads no field, sound at any declared shape
            unfold dEnvDomRepr at hdom
            cases hpay : x.1.2 with
            | int m =>
                rw [hpay] at hdom
                obtain ⟨v, hvs, _hrepr⟩ := hdom
                refine ⟨tyIdx, [v], by rw [← htag]; exact hvs, ?_⟩
                have hce : (dCascadeEval (.test tyIdx (.const kk) rest)
                    x.1.1 (WPayload.int m).toInt?).getD 0 = kk := by
                  rw [htag]; simp [dCascadeEval]
                rw [hce]
                exact IntDispatchSoundness.EvalCascade.constHit tyIdx kk rest [v]
            | opaqueFields fs =>
                rw [hpay] at hdom
                refine ⟨tyIdx, fs, by rw [← htag]; exact hdom, ?_⟩
                have hce : (dCascadeEval (.test tyIdx (.const kk) rest)
                    x.1.1 (WPayload.opaqueFields fs).toInt?).getD 0 = kk := by
                  rw [htag]; simp [dCascadeEval]
                rw [hce]
                exact IntDispatchSoundness.EvalCascade.constHit tyIdx kk rest fs
        | proj =>
            have hpayTy : dCtorShape? env tyIdx = some .hit := by
              simpa using hleaf
            have hchild := x.2
            unfold DEnvValidChild at hchild
            rw [htag] at hchild
            rcases hchild with ⟨_, m, hm⟩ | ⟨c, hcshape, hcne, _⟩
            · unfold dEnvDomRepr at hdom
              rw [hm] at hdom
              obtain ⟨v, hvs, hrepr⟩ := hdom
              refine ⟨tyIdx, [v], by rw [← htag]; exact hvs, ?_⟩
              have hce : (dCascadeEval (.test tyIdx .proj rest)
                  x.1.1 x.1.2.toInt?).getD 0
                  = IntDispatchSoundness.evalLeaf .proj m := by
                rw [hm]; simp [dCascadeEval, htag, WPayload.toInt?]
              rw [hce]
              exact IntDispatchSoundness.EvalCascade.hit tyIdx .proj rest [v] m v rfl hrepr
            · rw [hpayTy] at hcshape
              exact absurd (Option.some.inj hcshape).symm hcne
        | hostOp role kk cf =>
            have hpayTy : dCtorShape? env tyIdx = some .hit := by
              simpa using hleaf
            have hchild := x.2
            unfold DEnvValidChild at hchild
            rw [htag] at hchild
            rcases hchild with ⟨_, m, hm⟩ | ⟨c, hcshape, hcne, _⟩
            · unfold dEnvDomRepr at hdom
              rw [hm] at hdom
              obtain ⟨v, hvs, hrepr⟩ := hdom
              refine ⟨tyIdx, [v], by rw [← htag]; exact hvs, ?_⟩
              have hce : (dCascadeEval (.test tyIdx (.hostOp role kk cf) rest)
                  x.1.1 x.1.2.toInt?).getD 0
                  = IntDispatchSoundness.evalLeaf (.hostOp role kk cf) m := by
                rw [hm]; simp [dCascadeEval, htag, WPayload.toInt?]
              rw [hce]
              exact IntDispatchSoundness.EvalCascade.hit tyIdx (.hostOp role kk cf)
                rest [v] m v rfl hrepr
            · rw [hpayTy] at hcshape
              exact absurd (Option.some.inj hcshape).symm hcne
      · -- non-matched tag: recurse, leaf shape irrelevant
        obtain ⟨tag, fields, hvs, hev⟩ := ih hrest
        refine ⟨tag, fields, hvs, ?_⟩
        have htageq : tag = x.1.1 := by
          unfold dEnvDomRepr at hdom
          cases hpay : x.1.2 with
          | int m =>
              rw [hpay] at hdom; obtain ⟨v, hvs2, _⟩ := hdom
              rw [hvs2] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
          | opaqueFields fs =>
              rw [hpay] at hdom
              rw [hdom] at hvs; injection hvs with hh; injection hh with h1 _; exact h1.symm
        have hne : x.1.1 ≠ tyIdx := htag
        have hce : (dCascadeEval (.test tyIdx leaf rest) x.1.1 x.1.2.toInt?).getD 0
            = (dCascadeEval rest x.1.1 x.1.2.toInt?).getD 0 := by
          simp only [dCascadeEval, if_neg hne]
        rw [hce]
        subst htageq
        exact IntDispatchSoundness.EvalCascade.miss tyIdx x.1.1 leaf rest fields _ hne hev

/-- The bridge in assembly shape: every represented declared input relates to the
    declared-carrier Int model, whatever its non-hit payload. -/
theorem env_declaredIntDispatch_bridge (env : DIdxEnvelope) (plan : IntDispatchRawPlan)
    (hcasc : dCascadeInEnv env plan.body = true)
    (S : CarrierSpec env.carrier) (x : DAdtVal env) (vs : List WVal)
    (hdom : dEnvDomRepr env S x vs) :
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S plan.body tag fields n ∧
      ∀ w, S.Repr n w →
        intRepr S (dEnvStructModel env plan.body x) w := by
  obtain ⟨tag, fields, hvs, hev⟩ := dCascade_bridge env plan.body hcasc S x vs hdom
  refine ⟨tag, fields, (dCascadeEval plan.body x.1.1 x.1.2.toInt?).getD 0, hvs, hev, ?_⟩
  intro w hw
  simpa [intRepr, dEnvStructModel] using hw

#print axioms dCascade_bridge
#print axioms env_declaredIntDispatch_bridge

/-! ## §6 The declared-index CONSTRUCTOR column (meaning -> bytes, reverse arrow)

The Int-read column reads the hit constructor's Int payload OUT of a declared
value; the constructor column builds one IN. The domain is a single `Int`
argument; the model is the declared hit constructor at its DECLARED index
carrying that Int as its Int-box payload; the codomain relation is the
single-result view of `dEnvDomRepr`. No new byte pin is needed beyond the same
single `concatPinnedAt` equality the read face already carries — the constructed
struct's body sits at its declared offset inside the pinned concat, so a forger
cannot build at a fabricated position without breaking the byte equality. -/

/-- Single-result codomain view of `dEnvDomRepr`: the declared value `y` is
    represented by exactly one `WVal`. Reads the same payload discipline. -/
def dEnvCodRepr (env : DIdxEnvelope) :
    CarrierSpec env.carrier → DAdtVal env → WVal → Prop :=
  fun S y w =>
    match y.1.2 with
    | .int n => ∃ v, w = .structv y.1.1 [v] ∧ S.Repr n v
    | .opaqueFields fs => w = .structv y.1.1 fs

/-- Constructor model from the plan: build the DECLARED hit constructor
    `structIdx` with the Int argument as its readable Int-box payload. The hit
    fact is DEMANDED so the value is well-formed (`DEnvValidChild`). -/
def dEnvCtorModel (env : DIdxEnvelope) (structIdx : Nat)
    (h : dCtorShape? env structIdx = some .hit) : Int → DAdtVal env :=
  fun n => ⟨(structIdx, .int n), Or.inl ⟨h, n, rfl⟩⟩

/-- The declared-index constructor face. Mirrors `DIdxIntReadFace` with the
    domain/codomain arrow reversed: the constructed result is a `DAdtVal` at the
    declared hit position, the plan builds that hit constructor with the Int
    argument as its Int-box payload, the ctor entries are pinned at their
    declared indices, and `o.policy = .simulatesModel` is asserted so the full
    semantic bridge (policy conjunct included) is derivable. -/
def DIdxCtorFace
    (modBytes modLen : Nat) (_typePrefix : List Nat)
    (env : DIdxEnvelope) (structIdx : Nat)
    (hhit : dCtorShape? env structIdx = some .hit)
    (plan : ConstructRawPlan) (o : Obligation) : Prop :=
  checkDIdxEnvelope env = true ∧
  plan.arity = 1 ∧ plan.fields = [.local 0] ∧
  dWalkPinned modBytes modLen env ∧
  o.policy = .simulatesModel ∧
  o.carrier = env.carrier ∧
  HEq o.Dom Int ∧
  HEq o.Cod (DAdtVal env) ∧
  HEq o.domRepr (AverCert.EnvelopeLowering.intArgDomRepr env.carrier) ∧
  HEq o.codRepr (dEnvCodRepr env) ∧
  HEq o.model (dEnvCtorModel env structIdx hhit)

/-- The positive constructor bridge, in the exact shape the acceptance-soundness
    assembly (`constructSemanticBridge`) consumes for the unary Int-payload
    constructor profile. On the declared Int-argument representation, the
    plan-computed constructor model `dEnvCtorModel env structIdx hhit x` is
    represented by exactly the struct the canonical lowering builds:
    `structv structIdx [v]`, where `v` represents the argument. `model`,
    `codRepr`, and `domRepr` are all wall terms over the plan envelope; there is
    no byte -> structure decoder. -/
theorem env_declaredConstruct_bridge (env : DIdxEnvelope) (structIdx : Nat)
    (hhit : dCtorShape? env structIdx = some .hit)
    (plan : ConstructRawPlan)
    (harity : plan.arity = 1) (hfields : plan.fields = [.local 0])
    (S : CarrierSpec env.carrier) (x : Int) (args : List WVal)
    (hdom : AverCert.EnvelopeLowering.intArgDomRepr env.carrier S x args) :
    args.length = plan.arity ∧
    dEnvCodRepr env S (dEnvCtorModel env structIdx hhit x)
      (.structv structIdx
        (ConstructVerbatimSoundness.constructModelFields
          (args ++ List.replicate 1 .null) plan.fields)) := by
  obtain ⟨v, hargs, hrepr⟩ := hdom
  subst hargs
  rw [harity, hfields]
  refine ⟨rfl, ?_⟩
  have hflds : ConstructVerbatimSoundness.constructModelFields
      ([v] ++ List.replicate 1 (.null : WVal)) [.local 0] = [v] := by
    simp [ConstructVerbatimSoundness.constructModelFields,
      ConstructVerbatimSoundness.constructModelField]
  rw [hflds]
  show dEnvCodRepr env S (dEnvCtorModel env structIdx hhit x)
    (.structv structIdx [v])
  unfold dEnvCodRepr dEnvCtorModel
  exact ⟨v, rfl, hrepr⟩

#print axioms dEnvCodRepr
#print axioms env_declaredConstruct_bridge

end AverCert.DeclaredIndexEnvelope
