import IntDispatchSoundness

set_option maxRecDepth 100000
set_option maxHeartbeats 2000000
set_option linter.unusedVariables false

/-!
# FoxCount beachhead — enum→Int constant-leaf on the int-dispatch cascade

Kill-fast for Leg A. `foxCount(c : Chute) -> Int` maps all four NULLARY variants
to `1`. The wasm-gc emitter lowers it to a `ref.test` cascade whose arms are
`i64.const 1; call box` with NO payload projection (a nullary constructor has no
field to read). This fixture models the REAL emitted code (verified against the
compiled `examples/games/eggcatch/core.av`: two locals — an `eqref` scrutinee at
local 1 and one unused carrier scratch — the scrutinee spilled to local 1, three
`ref.test` arms plus a boxed-constant default) and proves the constant-leaf
cascade simulates to the represented constant `1`, kernel-clean.

The NEW machinery mirrored here (to be transcribed into the wall):
* `LeafC.const k` — a leaf that returns `k` WITHOUT reading a field;
* `EvalCascadeC.constHit` — the byte-origin semantics of a matched const arm,
  which reads NO field (no `hfield`/`hrepr`), so it is sound for a nullary value;
* `evalCascadeC_constHit_inv` — the matched-const inversion;
* `simConstArm` — the const arm's `i64.const k; box` if-branch is `BlockOK` via
  `S.smallIntro k` (exactly the terminal-default closer, wrapped in the `ref.test`
  `if`); and
* `simCascadeC` / `foxCount_certified` — the whole const cascade simulates with
  the scrutinee at local `bindArmCount + 1` (here `0 + 1 = 1`), matching the real
  local layout.

Reuses the wall primitives `StackOK`, `BlockOK`, `blockOK_ifElse`, `boxRef`,
`popArgs_one`, `wRunF`, `carrierSmall`, `CarrierSpec.smallIntro`.
-/

namespace FoxCountBeachhead

open CertPrelude
open AverCert.Schema
open IntDispatchSoundness

/-! ## §1 The extended leaf and the byte-origin cascade semantics -/

/-- Local mirror of the extended `IntDispatchLeaf` (`+ const`). -/
inductive LeafC where
  | proj
  | hostOp (role : AverCert.Schema.IntDispatchRole) (k : Int) (constFirst : Bool)
  | const (k : Int)

/-- Local mirror of `evalLeaf` extended with `.const k, _ => k`. -/
def evalLeafC : LeafC → Int → Int
  | .proj, x => x
  | .hostOp .add k true, x => k + x
  | .hostOp .add k false, x => x + k
  | .hostOp .sub k true, x => k - x
  | .hostOp .sub k false, x => x - k
  | .const k, _ => k

/-- Local mirror of `IntDispatchCascade` over `LeafC`. -/
inductive CascadeC where
  | default (k : Int)
  | test (tyIdx : Nat) (hit : LeafC) (rest : CascadeC)

/-- Local mirror of `EvalCascade` with the NEW `constHit` rule. `constHit` reads
    NO field (no `hfield`/`hrepr`): a matched const arm returns `k` regardless of
    the payload, which is exactly what makes it sound for a nullary constructor. -/
inductive EvalCascadeC {C : Nat} (S : CarrierSpec C) :
    CascadeC → Nat → List WVal → Int → Prop where
  | default (k : Int) (tag : Nat) (fields : List WVal) :
      EvalCascadeC S (.default k) tag fields k
  | hit (tyIdx : Nat) (leaf : LeafC) (rest : CascadeC)
      (fields : List WVal) (x : Int) (v : WVal)
      (hfield : fields[0]? = some v) (hrepr : S.Repr x v) :
      EvalCascadeC S (.test tyIdx leaf rest) tyIdx fields (evalLeafC leaf x)
  | constHit (tyIdx : Nat) (k : Int) (rest : CascadeC) (fields : List WVal) :
      EvalCascadeC S (.test tyIdx (.const k) rest) tyIdx fields k
  | miss (tyIdx tag : Nat) (leaf : LeafC) (rest : CascadeC)
      (fields : List WVal) (n : Int) (hne : tag ≠ tyIdx)
      (hrest : EvalCascadeC S rest tag fields n) :
      EvalCascadeC S (.test tyIdx leaf rest) tag fields n

/-- Matched-const inversion (analogue of `evalCascade_hit_inv`): a matched const
    arm forces the result to the constant, whether it was witnessed by `constHit`
    or by a `hit` with a `.const` leaf. -/
theorem evalCascadeC_constHit_inv {C : Nat} {S : CarrierSpec C}
    {tyIdx : Nat} {k : Int} {rest : CascadeC} {fields : List WVal} {n : Int}
    (h : EvalCascadeC S (.test tyIdx (.const k) rest) tyIdx fields n) :
    n = k := by
  cases h with
  | hit _ _ _ _ x v hfield hrepr => rfl
  | constHit => rfl
  | miss _ _ _ _ _ _ hne _ => exact absurd rfl hne

/-- Miss inversion (analogue of `evalCascade_miss_inv`), including the new
    `constHit` case. -/
theorem evalCascadeC_miss_inv {C : Nat} {S : CarrierSpec C}
    {tyIdx tag : Nat} {leaf : LeafC} {rest : CascadeC}
    {fields : List WVal} {n : Int} (hne : tag ≠ tyIdx)
    (h : EvalCascadeC S (.test tyIdx leaf rest) tag fields n) :
    EvalCascadeC S rest tag fields n := by
  cases h with
  | hit _ _ _ _ _ _ _ _ => exact absurd rfl hne
  | constHit => exact absurd rfl hne
  | miss _ _ _ _ _ _ _ hrest => exact hrest

/-! ## §2 The const-arm lowering (default-style body, no projection) -/

/-- The lowering of an all-const cascade: the scrutinee lives at local `S`, each
    const arm's if-branch is the terminal-style `i64.const k; call box` (NO
    projection prefix), and non-first arms reload the scrutinee. -/
def lowerCascadeC (boxIdx S : Nat) : Bool → CascadeC → Option (List WInstr)
  | _first, .default k => some [.i64Const k, .call boxIdx]
  | first, .test tyIdx (.const k) rest =>
      match lowerCascadeC boxIdx S false rest with
      | some restI =>
          some ((if first then [] else [.localGet S]) ++
            [.refTest tyIdx, .ifElse [.i64Const k, .call boxIdx] restI])
      | none => none
  | _first, .test _ _ _ => none

/-! ## §3 The const arm simulates (the terminal-default closer, in an `if`) -/

/-- `simConstArm`: a const arm's if-branch `[i64.const k, call box]` leaves the
    represented constant above the unchanged stack — EXACTLY the terminal-default
    proof (`S.smallIntro k`), now reused inside a `ref.test` `if`. Reads no local
    and no field. -/
theorem simConstArm {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (boxIdx : Nat) (hbox : host boxIdx = some (1, boxRef C))
    (k : Int) (base locals : List WVal) :
    BlockOK S host ar callee k base [.i64Const k, .call boxIdx] locals base := by
  simpa [BlockOK, StackOK, wRunF, hbox, boxRef, popArgs_one]
    using S.smallIntro k

/-! ## §4 The whole const cascade simulates -/

theorem simCascadeC {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (boxIdx : Nat) (hbox : host boxIdx = some (1, boxRef C))
    (scrut : Nat) (locals : List WVal) (tag : Nat) (fields : List WVal)
    (hslot : locals[scrut]? = some (.structv tag fields)) :
    ∀ body first n instrs base,
      EvalCascadeC S body tag fields n →
      (match body with | .default _ => first = false | .test _ _ _ => True) →
      lowerCascadeC boxIdx scrut first body = some instrs →
      BlockOK S host ar callee n base instrs locals
        (if first then .structv tag fields :: base else base) := by
  intro body
  induction body with
  | default k =>
      intro first n instrs base hsem hfirst hlow
      cases hsem
      have hfirst' : first = false := hfirst
      subst first
      simp only [lowerCascadeC, Option.some.injEq] at hlow
      subst instrs
      simpa using simConstArm S host ar callee boxIdx hbox k base locals
  | test tyIdx leaf rest ih =>
      intro first n instrs base hsem _hfirst hlow
      cases leaf with
      | proj => simp [lowerCascadeC] at hlow
      | hostOp role kk cf => simp [lowerCascadeC] at hlow
      | const k =>
          cases hr : lowerCascadeC boxIdx scrut false rest with
          | none => simp [lowerCascadeC, hr] at hlow
          | some restI =>
              simp only [lowerCascadeC, hr, Option.some.injEq] at hlow
              subst instrs
              by_cases htag : tag = tyIdx
              · subst tag
                have hn : n = k := evalCascadeC_constHit_inv hsem
                subst n
                have harm : BlockOK S host ar callee k base
                    [.i64Const k, .call boxIdx] locals base :=
                  simConstArm S host ar callee boxIdx hbox k base locals
                have hif := blockOK_ifElse S host ar callee k base
                  [.i64Const k, .call boxIdx] restI locals base true
                  (by simpa using harm)
                cases first <;>
                  simpa [BlockOK, StackOK, wRunF, hslot, b32] using hif
              · have hrest := evalCascadeC_miss_inv htag hsem
                have htail := ih false n restI base hrest (by cases rest <;> simp) hr
                have hif := blockOK_ifElse S host ar callee n base
                  [.i64Const k, .call boxIdx] restI locals base false
                  (by simpa using htail)
                cases first <;>
                  simp [BlockOK, StackOK, wRunF, hslot, b32, htag] at hif ⊢ <;>
                  exact hif

/-! ## §5 The concrete foxCount export certificate

Carrier `C`, box helper `boxIdx`, the three tested Chute variants `lu ld ru`
(RightDown is the untested default). `S = 1` is `bindArmCount + 1 = 0 + 1`: an
all-const cascade spills NO per-arm payload local, so the scrutinee scratch is
local 1 and there are exactly two declared locals — the real emitted layout. -/

/-- The plan cascade of `foxCount`: three const arms returning `1`, default `1`. -/
def foxBody : CascadeC :=
  .test 1 (.const 1) (.test 2 (.const 1) (.test 3 (.const 1) (.default 1)))

/-- The lowered `ref.test` cascade (no `local.get 0/set 1` prelude). Exactly the
    real emitted cascade tree, with NO payload projection anywhere. -/
def foxCascadeInstrs (boxIdx : Nat) : List WInstr :=
  [.refTest 1, .ifElse [.i64Const 1, .call boxIdx]
    [.localGet 1, .refTest 2, .ifElse [.i64Const 1, .call boxIdx]
      [.localGet 1, .refTest 3, .ifElse [.i64Const 1, .call boxIdx]
        [.i64Const 1, .call boxIdx]]]]

/-- The full lowered body, scrutinee spilled to local 1. -/
def foxInstrs (boxIdx : Nat) : List WInstr :=
  [.localGet 0, .localSet 1, .localGet 1] ++ foxCascadeInstrs boxIdx

/-- The lowering of `foxBody` at scrutinee-local 1 is exactly `foxCascadeInstrs`
    — every arm is a boxed constant, NO field projection. -/
theorem lowerCascadeC_fox (boxIdx : Nat) :
    lowerCascadeC boxIdx 1 true foxBody = some (foxCascadeInstrs boxIdx) := by
  rfl

/-- THE SOUNDNESS-CRITICAL PIECE: on any represented declared input the emitted
    `foxCount` body runs to a value representing the plan model. Mirrors
    `generic_int_dispatch_certified` at the all-const (`bindArmCount = 0`) layout:
    scrutinee at local 1, two declared locals, no per-arm payload locals. -/
theorem foxCount_certified {C : Nat} (S : CarrierSpec C)
    (code : CodeTbl) (host : HostTbl) (self boxIdx : Nat)
    (hbox : host boxIdx = some (1, boxRef C))
    (hself : code self = some {
      arity := 1, nlocals := 2, body := foxInstrs boxIdx }) :
    ∀ fuel tag fields n w,
      EvalCascadeC S foxBody tag fields n →
      wFuncN code host (fuel + 1) self [.structv tag fields] = some w →
      S.Repr n w := by
  intro fuel tag fields n w hsem hrun
  simp only [wFuncN, hself] at hrun
  let locals : List WVal := [WVal.structv tag fields, WVal.null, WVal.null]
  let updated := locals.set 1 (WVal.structv tag fields)
  have hslt : (1 : Nat) < locals.length := by simp [locals]
  have hslot : updated[1]? = some (.structv tag fields) :=
    List.getElem?_set_self hslt
  have hsim := simCascadeC S host (fun g => (code g).map (·.arity))
    (fun g args => wFuncN code host fuel g args)
    boxIdx hbox 1 updated tag fields hslot foxBody true n
    (foxCascadeInstrs boxIdx) [] hsem trivial (lowerCascadeC_fox boxIdx)
  change
    (match wRunF host (fun g => (code g).map (·.arity))
      (fun g args => wFuncN code host fuel g args)
      ([.localGet 0, .localSet 1, .localGet 1] ++ foxCascadeInstrs boxIdx)
      locals [] with
    | some (.ok _ [value]) => some value
    | some (.ret value) => some value
    | _ => none) = some w at hrun
  simp only [List.cons_append, List.nil_append, wRunF] at hrun
  have hzero : locals[0]? = some (WVal.structv tag fields) := by simp [locals]
  have hslotRaw : (locals.set 1 (WVal.structv tag fields))[1]? =
      some (WVal.structv tag fields) :=
    List.getElem?_set_self hslt
  simp only [hzero, hslotRaw] at hrun
  change
    (match wRunF host (fun g => (code g).map (·.arity))
      (fun g args => wFuncN code host fuel g args)
      (foxCascadeInstrs boxIdx) updated [WVal.structv tag fields] with
    | some (.ok _ [value]) => some value
    | some (.ret value) => some value
    | _ => none) = some w at hrun
  cases hr : wRunF host (fun g => (code g).map (·.arity))
      (fun g args => wFuncN code host fuel g args)
      (foxCascadeInstrs boxIdx) updated [.structv tag fields] with
  | none => simp [hr] at hrun
  | some out =>
      cases out with
      | ret value =>
          simp [hr] at hrun
          have hfalse := hsim (.ret value) (by simpa using hr)
          simp [StackOK] at hfalse
      | ok finalLocals stack =>
          cases stack with
          | nil => simp [hr] at hrun
          | cons value rest =>
              cases rest with
              | nil =>
                  simp [hr] at hrun
                  subst w
                  have hok := hsim (.ok finalLocals [value]) (by simpa using hr)
                  exact hok.2
              | cons x xs => simp [hr] at hrun

/-! ## §6 The face-bridge side: model and `EvalCascade` witness (reads no field)

Mirrors `cascadeEval`'s NEW const-path and the const case of `dCascade_bridge`:
the model of a matched const arm is `k` REGARDLESS of the (absent) payload, and
the byte-origin `EvalCascade` witness for a nullary value is `constHit`, which
reads no field. A nullary Chute value carries `payload = none` (no Int box). -/

/-- Local mirror of `cascadeEval` with the NEW const-path: a matched `.const k`
    returns `some k` without consulting the payload. -/
def cascadeEvalC : CascadeC → Nat → Option Int → Option Int
  | .default k, _, _ => some k
  | .test tyIdx leaf rest, tag, payload =>
      if tag = tyIdx then
        (match leaf with | .const k => some k | _ => payload.map (evalLeafC leaf))
      else cascadeEvalC rest tag payload

/-- All four Chute variants (nullary, `payload = none`) model to `1` — the three
    tested tags via the const-path, `RightDown` (tag 4) via the default. If the
    const-path were absent, `none.map _ = none` would send every arm to `getD 0 =
    0` (the wrong model): this is exactly the payload-blind reason a nullary enum
    needs the new rule rather than a reused payload-reading `hit`. -/
theorem foxModel_all_one :
    (cascadeEvalC foxBody 1 none).getD 0 = 1 ∧
    (cascadeEvalC foxBody 2 none).getD 0 = 1 ∧
    (cascadeEvalC foxBody 3 none).getD 0 = 1 ∧
    (cascadeEvalC foxBody 4 none).getD 0 = 1 := by
  refine ⟨rfl, rfl, rfl, rfl⟩

/-- The face-bridge (declared-value → `EvalCascade`) side, const arms only: every
    represented nullary declared Chute value relates by `EvalCascadeC` to exactly
    the plan-computed model, with NO field read. Mirrors `dCascade_bridge`'s const
    branch: matched tags use `constHit`, non-matched tags recurse through `miss`. -/
theorem foxCount_bridge {C : Nat} (S : CarrierSpec C)
    (tag : Nat) (fields : List WVal)
    (htag : tag = 1 ∨ tag = 2 ∨ tag = 3 ∨ tag = 4) :
    EvalCascadeC S foxBody tag fields ((cascadeEvalC foxBody tag none).getD 0) := by
  rcases htag with h | h | h | h <;> subst tag
  · exact EvalCascadeC.constHit 1 1 _ fields
  · exact EvalCascadeC.miss 1 2 _ _ fields 1 (by decide)
      (EvalCascadeC.constHit 2 1 _ fields)
  · exact EvalCascadeC.miss 1 3 _ _ fields 1 (by decide)
      (EvalCascadeC.miss 2 3 _ _ fields 1 (by decide)
        (EvalCascadeC.constHit 3 1 _ fields))
  · exact EvalCascadeC.miss 1 4 _ _ fields 1 (by decide)
      (EvalCascadeC.miss 2 4 _ _ fields 1 (by decide)
        (EvalCascadeC.miss 3 4 _ _ fields 1 (by decide)
          (EvalCascadeC.default 1 4 fields)))

#print axioms simConstArm
#print axioms simCascadeC
#print axioms evalCascadeC_constHit_inv
#print axioms foxCount_certified
#print axioms foxModel_all_one
#print axioms foxCount_bridge

end FoxCountBeachhead
