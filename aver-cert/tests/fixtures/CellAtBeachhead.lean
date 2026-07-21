import SchemaCore
import StandardFace

set_option linter.unusedSimpArgs false
set_option linter.unusedVariables false
set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

open AverCert
open AverCert.Schema
open CertPrelude

/-!
# Array-read beachhead — fused `Option.withDefault(Vector.get(vec, idx), d)`

Target witness: `cellAt(grid: Vector<Int>, idx: Int) -> Int =
Option.withDefault(Vector.get(grid, idx), 0)` (`examples/games/life.av`).

The wasm-gc emitter fuses this call pair into one fixed bounds-checked
`array.get` template (`emit_mir_option_with_default` +
`emit_index_i32`/`emit_index_nonneg`): extract the index through the
`__aint_to_index` host helper, test `idx >= 0 (signed) AND idx < len
(unsigned)`, read the element on hit, box the literal default on miss. This
fixture pins that template as ONE monolithic shape with four holes — the
`__aint_to_index` function index, the box function index, the array type
index, and the literal default — plus the pinned local order (vec = local 0,
idx = local 1), and proves in the kernel that any instantiation computes
exactly the model `if 0 <= i and i < len v then v[i] else d`.

The `__aint_to_index` contract (the sixth host role, from
`builtins/bignum.rs` + `wat/to_index.wat`, confirmed against the WAT source):
for a represented integer `n`, the helper returns `n` when `0 <= n < 2^31`
and the out-of-bounds sentinel `-1` otherwise (a negative or `>= 2^31` small
value, and every big value, all collapse to `-1`). It is stated RELATIONALLY
over the representation (like the add/sub/mul host contracts), never as a
concrete function: the certificate assumes only this law of the wired helper.
The helper is pure, so calling it three times on the same local yields the
same value for free — the contract pins the result value, not an effect.

SOUNDNESS-CRITICAL BOUND: the representation relation requires
`elems.length < 2^31`. Without it, a state with a `>= 2^31`-element array
would "represent" a vector for which the model reads `v[i]` at
`i in [2^31, len)` while the machine's `to_index` collapses `i` to the `-1`
sentinel and returns the default. The bound lives INSIDE `domRepr` — a state
carrying a larger array simply represents no `(v, i)` at all — never as an
asserted premise. It is also true of the actual runtime: no engine array
spans `2^31` entries (`wat/to_index.wat` comment), so real states always
satisfy it.
-/

namespace CellAtDev

/-- `__aint_to_index` result on a represented integer: pass an index in
    `[0, 2^31)` through, collapse everything else to the OOB sentinel `-1`. -/
def toIndexModel (n : Int) : Int :=
  if 0 ≤ n ∧ n < 2147483648 then n else -1

/-- The fused `Option.withDefault(Vector.get(vec, idx), d)` template. Holes:
    `toIndexIdx` (the `__aint_to_index` function index), `boxIdx` (the
    `__rt_aint_from_i64` function index), `arrTy` (the vector's array type
    index), `d` (the literal default). Locals pinned: vec = 0, idx = 1. -/
def cellAtTemplate (toIndexIdx boxIdx arrTy : Nat) (d : Int) : List WInstr :=
  [ .localGet 1, .call toIndexIdx, .i32Const 0, .i32GeS,
    .localGet 1, .call toIndexIdx,
    .localGet 0, .arrayLen, .i32LtU,
    .i32And,
    .ifElse
      [.localGet 0, .localGet 1, .call toIndexIdx, .arrayGet arrTy]
      [.i64Const d, .call boxIdx] ]

/-- Host table of the template: the abstract `__aint_to_index` helper (bound
    only by its relational contract) and the concrete box reference face. -/
def cellAtHost (carrier toIndexIdx boxIdx : Nat)
    (toIndex : List WVal → Option WVal) : HostTbl :=
  fun fn =>
    if fn = toIndexIdx then some (1, toIndex)
    else if fn = boxIdx then some (1, boxRef carrier) else none

/-- Code table: the template is the single self function (index 1). -/
def cellAtCode (toIndexIdx boxIdx arrTy : Nat) (d : Int) : CodeTbl :=
  fun fn =>
    if fn = 1 then
      some { arity := 2, nlocals := 0,
             body := cellAtTemplate toIndexIdx boxIdx arrTy d }
    else none

/-- Domain representation of the fused-get face: the machine state is exactly
    `[vector array, boxed index]`, the array has one represented element per
    model element, and — soundness-critical, see the header — fewer than
    `2^31` elements. All witnesses (`elems`, `wi`, the per-element carriers)
    live INSIDE the relation. -/
def vecDomRepr (carrier arrTy : Nat) (S : CarrierSpec carrier)
    (p : List Int × Int) (vs : List WVal) : Prop :=
  ∃ elems wi,
    vs = [.arr arrTy elems, wi] ∧
    elems.length = p.1.length ∧
    elems.length < 2147483648 ∧
    (∀ k, k < p.1.length → ∃ w, elems[k]? = some w ∧ intRepr S (p.1[k]!) w) ∧
    intRepr S p.2 wi

/-- The source model: in-bounds read, else the literal default. -/
def vecModel (d : Int) (p : List Int × Int) : Int :=
  if 0 ≤ p.2 ∧ p.2 < (p.1.length : Int) then p.1[p.2.toNat]! else d

/-- The complete face of the fused `Vector.get`-or-default shape, mirroring
    `TagDispatchFace`: the four template holes are the face data; the domain,
    codomain, representation relations, and model are fixed by the family. -/
structure VectorGetOrDefaultFace where
  toIndexIdx : Nat
  boxIdx     : Nat
  arrTy      : Nat
  d          : Int
deriving Repr, DecidableEq

/-- THE TEMPLATE-IMPLIES-MODEL THEOREM, generic over every hole: under any
    representation `S` and any `toIndex` helper obeying the relational
    `__aint_to_index` contract, running the fused template on a represented
    `(v, i)` yields a represented `vecModel d (v, i)`. Partial correctness —
    vacuous on trap or fuel exhaustion, like `Obligation.holds`. -/
theorem cellAtTemplate_simulates_model
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexModel n))
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hRun : wFuncN (cellAtCode toIndexIdx boxIdx arrTy d)
      (cellAtHost carrier toIndexIdx boxIdx toIndex) fuel 1 vs = some w) :
    intRepr S (vecModel d (v, i)) w := by
  obtain ⟨elems, wi, rfl, hlen0, hbound, hall0, hwi0⟩ := hDom
  -- Re-state the relation components with `(v, i).fst/.snd` projected away so
  -- `omega` sees one atom per length.
  have hlen : elems.length = v.length := hlen0
  have hall : ∀ k, k < v.length → ∃ w, elems[k]? = some w ∧
      intRepr S (v[k]!) w := hall0
  have hwi : intRepr S i wi := hwi0
  have hbox : ¬(boxIdx = toIndexIdx) := fun h => hIdx h.symm
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      cases htix : toIndex [wi] with
      | none =>
          simp [wFuncN, cellAtCode, cellAtTemplate, cellAtHost, initLocals,
            wRunF, popArgs, htix] at hRun
      | some r =>
          have hr := hToIndex i wi r hwi htix
          subst hr
          by_cases hin : 0 ≤ i ∧ i < (v.length : Int)
          · -- In bounds: the hit arm reads the represented element.
            have hlt31 : i < 2147483648 := by omega
            have ht : toIndexModel i = i := by
              simp [toIndexModel, hin.1, hlt31]
            rw [ht] at htix
            have hkn : i.toNat < v.length := by omega
            obtain ⟨wv, hw, hwr⟩ := hall i.toNat hkn
            have hidx : i.toNat < elems.length := by omega
            have hwv : elems[i.toNat] = wv := by
              simpa [List.getElem?_eq_getElem hidx] using hw
            have hemodI : i.emod 4294967296 = i := by
              show i % 4294967296 = i
              omega
            have hemodL : ((elems.length : Int)).emod 4294967296 =
                (elems.length : Int) := by
              show (elems.length : Int) % 4294967296 = (elems.length : Int)
              omega
            have hilen : i < (elems.length : Int) := by omega
            simp [wFuncN, cellAtCode, cellAtTemplate, cellAtHost, initLocals,
              wRunF, popArgs, b32, htix, ht, hin.1, hemodI, hemodL, hilen,
              hbox, hw] at hRun
            subst hRun
            simpa [vecModel, hin, hwv] using hwr
          · -- Out of bounds: the miss arm boxes the literal default.
            have hmodel : vecModel d (v, i) = d := by
              simp only [vecModel]
              exact if_neg hin
            rw [hmodel]
            by_cases hsmall : 0 ≤ i ∧ i < 2147483648
            · -- The extracted index is `i` itself but fails the unsigned
              -- length test (`i >= len`).
              have ht : toIndexModel i = i := by simp [toIndexModel, hsmall]
              rw [ht] at htix
              have hemodI : i.emod 4294967296 = i := by
                show i % 4294967296 = i
                omega
              have hemodL : ((elems.length : Int)).emod 4294967296 =
                  (elems.length : Int) := by
                show (elems.length : Int) % 4294967296 = (elems.length : Int)
                omega
              have hnlt : ¬(i < (elems.length : Int)) := by omega
              simp [wFuncN, cellAtCode, cellAtTemplate, cellAtHost, initLocals,
                wRunF, popArgs, b32, htix, ht, hsmall.1, hemodI, hemodL, hnlt,
                hbox, boxRef] at hRun
              subst hRun
              exact S.smallIntro d
            · -- The helper collapses the index to the sentinel `-1`, which
              -- fails the signed lower-bound test.
              have ht : toIndexModel i = -1 := by simp [toIndexModel, hsmall]
              rw [ht] at htix
              simp [wFuncN, cellAtCode, cellAtTemplate, cellAtHost, initLocals,
                wRunF, popArgs, b32, htix, ht, hbox, boxRef] at hRun
              subst hRun
              exact S.smallIntro d

/-- LAW 1 (`cellAt` in-bounds read): on `0 <= i < len v` the fused template
    returns the represented `v[i]`. -/
theorem cellAt_in_bounds_read
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexModel n))
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hin : 0 ≤ i ∧ i < (v.length : Int))
    (hRun : wFuncN (cellAtCode toIndexIdx boxIdx arrTy d)
      (cellAtHost carrier toIndexIdx boxIdx toIndex) fuel 1 vs = some w) :
    intRepr S (v[i.toNat]!) w := by
  have h := cellAtTemplate_simulates_model carrier toIndexIdx boxIdx arrTy d
    hIdx S toIndex hToIndex fuel v i vs w hDom hRun
  simpa [vecModel, hin] using h

/-- LAW 2 (`cellAt` out-of-bounds default): outside `[0, len v)` the fused
    template returns the represented literal default. -/
theorem cellAt_out_of_bounds_default
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexModel n))
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hout : ¬(0 ≤ i ∧ i < (v.length : Int)))
    (hRun : wFuncN (cellAtCode toIndexIdx boxIdx arrTy d)
      (cellAtHost carrier toIndexIdx boxIdx toIndex) fuel 1 vs = some w) :
    intRepr S d w := by
  have h := cellAtTemplate_simulates_model carrier toIndexIdx boxIdx arrTy d
    hIdx S toIndex hToIndex fuel v i vs w hDom hRun
  simpa [vecModel, hout] using h

/-! ### Anti-vacuity guards

The theorems above are partial-correctness statements, vacuous if the
template always trapped. These executable runs pin concrete instantiations
of every hole (carrier 3, `to_index` 7, box 6, array type 4, default 0) on
the executable `__aint_to_index` reference face and check — decoded to `Int`
through `carrierToInt` and evaluated by `native_decide`, the established
`CertPreludeSanity` guard idiom, OUTSIDE the proof budget — that the template
actually produces the modelled results: in-bounds read, negative index,
past-end index, `>= 2^31` index, and a big (limb-carrying) index. -/

/-- Executable reference face of the `__aint_to_index` contract: a small
    carrier extracts through `toIndexModel`; any big (limb-carrying) carrier
    is the sentinel. -/
def toIndexRef : List WVal → Option WVal
  | [.structv _ [.i64v s, .null, .i32v _]] => some (.i32v (toIndexModel s))
  | [.structv _ [.i64v _, .arr _ _, .i32v _]] => some (.i32v (-1))
  | _ => none

def demoVec : WVal :=
  .arr 4 [carrierSmall 3 10, carrierSmall 3 20, carrierSmall 3 30]

def demoRun (idx : Int) : Option WVal :=
  wFuncN (cellAtCode 7 6 4 0) (cellAtHost 3 7 6 toIndexRef) 1 1
    [demoVec, carrierSmall 3 idx]

example : (demoRun 1).bind carrierToInt = some 20 := by native_decide
example : (demoRun (-1)).bind carrierToInt = some 0 := by native_decide
example : (demoRun 3).bind carrierToInt = some 0 := by native_decide
example : (demoRun 2147483648).bind carrierToInt = some 0 := by native_decide
example :
    (wFuncN (cellAtCode 7 6 4 0) (cellAtHost 3 7 6 toIndexRef) 1 1
      [demoVec, .structv 3 [.i64v 0, .arr 5 [.i64v 1], .i32v 0]]).bind
      carrierToInt = some 0 := by native_decide

#print axioms cellAtTemplate_simulates_model
#print axioms cellAt_in_bounds_read
#print axioms cellAt_out_of_bounds_default

end CellAtDev
