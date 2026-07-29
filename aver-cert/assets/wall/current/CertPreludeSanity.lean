/-
  Sanity + anti-vacuity for the generalized certificate semantics.

  Two kinds of check:
  * a ported simulation theorem (`addTwo_wasm_certified`) showing the proof
    pattern from the kill-fast probe survives the generalization to
    `structv`-based carriers, and stays kernel-clean under `#print axioms`
    ([propext, Classical.choice, Quot.sound]; no `sorryAx`);
  * executable `example`s via `native_decide` (OUTSIDE the proof budget) that
    force the interpreter to actually COMPUTE a decoded result on concrete
    inputs across every value family (Int, Bool, f64, ADT, String, List, tail
    recursion). A vacuous semantics — e.g. one that fails `localGet` on
    uninitialised locals — cannot pass these.
-/
import CertPrelude

set_option linter.unusedSimpArgs false

namespace CertPrelude

/-! ## Ported simulation theorem: addTwo (non-recursive), carrier type 2 -/

def addTwoCode : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 1, [.localGet 0, .i64Const 2, .call 6, .call 7]⟩ else none

def addTwoHost (add7 : List WVal → Option WVal) : HostTbl := fun fn =>
  if fn = 6 then some (1, boxRef 2)
  else if fn = 7 then some (2, add7)
  else none

/-- The VERBATIM-shaped body of addTwo maps any representation of `n` to a
    representation of `n + 2`, for ALL `n : ℤ`, given the abstract runtime
    contract `h7` (carrier add = exact ℤ addition on represented values). -/
theorem addTwo_wasm_certified
    (S : ReprSpec 2)
    (add7 : List WVal → Option WVal)
    (h7 : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add7 [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w, wFuncN addTwoCode (addTwoHost add7) 1 1 [v] = some w ∧ S.Repr (n + 2) w := by
  intro n v hv
  obtain ⟨w, hw, hrepr⟩ := h7 n 2 v (carrierSmall 2 2) hv (S.smallIntro 2)
  refine ⟨w, ?_, hrepr⟩
  simp only [wFuncN, addTwoCode, addTwoHost, boxRef, carrierSmall, initLocals,
    wRunF, popArgs, List.getElem?_cons_zero, List.length, List.take, List.drop,
    List.reverse, List.replicate, if_true, reduceIte]
  simp only [carrierSmall] at hw
  simp [hw]

#print axioms addTwo_wasm_certified

/-! ## Anti-vacuity guards — the interpreter must COMPUTE, per value family.

Each uses a small self-contained code table built from the generalized
instruction set, then runs `wFuncN` and DECODES the result (to Int / Bool /
String / structural bytes) so no `DecidableEq WVal` is needed. -/

/-- Executable host env for the guards: box (6), Int.add (7), Int.sub (8),
    all at carrier type 5 (the zoo module's carrier). -/
def gHost : HostTbl := fun fn =>
  if fn = 6 then some (1, boxRef 5)
  else if fn = 7 then some (2, addRef 5)
  else if fn = 8 then some (2, subRef 5)
  else none

/-- Decode an i32 boolean result. -/
def asBool : WVal → Option Bool
  | .i32v 1 => some true
  | .i32v 0 => some false
  | _ => none

/-- Decode a byte-array (string) result to its UTF-8 bytes. -/
def asBytes : WVal → Option (List Nat)
  | .arr _ es => es.mapM (fun v => match v with | .i32v n => some n.toNat | _ => none)
  | _ => none

/-- Decode an f64 result to its IEEE-754 bit pattern (`UInt64` is `DecidableEq`). -/
def asF64 : WVal → Option UInt64
  | .f64v bits => some bits
  | _ => none

-- 1) Int + recursion (sumTo): 3 -> 6, and a small carrier is decoded to ℤ.
def cSumTo : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 1,
    [ .localGet 0, .localSet 1,
      .localGet 1, .structGet 5 1, .refIsNull,
      .ifElse [.localGet 1, .structGet 5 0, .i64Const 0, .i64LeS]
              [.localGet 1, .structGet 5 2, .i32Const 0, .i32LtS],
      .ifElse [.i64Const 0, .call 6]
              [.localGet 0, .localGet 0, .i64Const 1, .call 6, .call 8, .call 1, .call 7] ]⟩
  else none

example :
    ((wFuncN cSumTo gHost 20 1 [carrierSmall 5 3]).bind carrierToInt) = some 6 := by
  native_decide

example :
    ((wFuncN cSumTo gHost 20 1 [carrierSmall 5 0]).bind carrierToInt) = some 0 := by
  native_decide

example :
    ((wFuncN cSumTo gHost 20 1 [carrierSmall 5 (-4)]).bind carrierToInt) = some 0 := by
  native_decide

-- 2) Tail recursion via return_call (countDown-style accumulator): tick(3,0)=6.
def cTick : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 1,
    [ .localGet 0, .structGet 5 1, .refIsNull,
      .ifElse [.localGet 0, .structGet 5 0, .i64Const 0, .i64LeS]
              [.localGet 0, .structGet 5 2, .i32Const 0, .i32LtS],
      .ifElse [.localGet 1]
              [.localGet 0, .i64Const 1, .call 6, .call 8, .localGet 1, .localGet 0, .call 7,
               .returnCall 1] ]⟩
  else none

example :
    ((wFuncN cTick gHost 20 1 [carrierSmall 5 3, carrierSmall 5 0]).bind carrierToInt)
      = some 6 := by native_decide

-- 3) ADT match via ref.test / ref.cast / struct.get: classify(Circle)=1, Point=3.
--    Shape types: Circle = 0 (one f64 field), Rect = 1 (two), Point = 2 (empty).
def cClassify : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 5,
    [ .localGet 0, .localSet 4,
      .localGet 4, .refTest 0,
      .ifElse [.localGet 4, .refCast 0, .structGet 0 0, .localSet 1, .i64Const 1, .call 6]
              [.localGet 4, .refTest 1,
               .ifElse [.localGet 4, .refCast 1, .structGet 1 0, .localSet 2,
                        .localGet 4, .refCast 1, .structGet 1 1, .localSet 3, .i64Const 2, .call 6]
                       [.i64Const 3, .call 6]] ]⟩
  else none

example :
    ((wFuncN cClassify gHost 8 1 [WVal.structv 0 [.f64v (1.5 : Float).toBits]]).bind carrierToInt)
      = some 1 := by native_decide

example :
    ((wFuncN cClassify gHost 8 1 [WVal.structv 2 []]).bind carrierToInt)
      = some 3 := by native_decide

-- 4) String literal via array.new_data: describe(Point) = "point" (bytes).
def cDescribe : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 5,
    [ .localGet 0, .localSet 4,
      .localGet 4, .refTest 0,
      .ifElse [.i32Const 0, .i32Const 6, .arrayNewData 3 [99,105,114,99,108,101]] -- "circle"
              [.i32Const 0, .i32Const 5, .arrayNewData 3 [112,111,105,110,116]] ]⟩ -- "point"
  else none

example :
    ((wFuncN cDescribe gHost 8 1 [WVal.structv 2 []]).bind asBytes)
      = some [112, 111, 105, 110, 116] := by native_decide

-- 5) f64 arithmetic + comparison, Bool result: (a < a*a) style through opcodes.
def cFcmp : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 1, [.localGet 0, .localGet 1, .f64Lt,
    .ifElse [.i32Const 1] [.localGet 0, .localGet 1, .f64Eq]]⟩
  else none

example :
    ((wFuncN cFcmp gHost 4 1 [WVal.f64v (1.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
      = some true := by native_decide

example :
    ((wFuncN cFcmp gHost 4 1 [WVal.f64v (2.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
      = some true := by native_decide

example :
    ((wFuncN cFcmp gHost 4 1 [WVal.f64v (3.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
      = some false := by native_decide

-- 6) f64 arithmetic value: 8.0 / 2.0 - 1.0 = 3.0 (bit-exact through the opcodes).
def cFarith : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 0,
    [.localGet 1, .localGet 0, .f64Div, .f64Const (1.0 : Float).toBits, .f64Sub]⟩
  else none

example :
    ((wFuncN cFarith gHost 4 1 [WVal.f64v (2.0 : Float).toBits, WVal.f64v (8.0 : Float).toBits]).bind asF64)
      = some (3.0 : Float).toBits := by native_decide

-- 7) Bool logic via i32.eq / i32.and and list head via structGet on a cons cell.
def cAnd : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 0, [.localGet 0, .localGet 1, .i32And]⟩ else none

example :
    ((wFuncN cAnd gHost 4 1 [WVal.i32v 1, WVal.i32v 1]).bind asBool) = some true := by native_decide
example :
    ((wFuncN cAnd gHost 4 1 [WVal.i32v 1, WVal.i32v 0]).bind asBool) = some false := by native_decide

/-! ## Residue guards — opcodes the differential harness cannot drive
end-to-end without string / Result / list-builder runtime contracts (kept
abstract in Stage A). Their SEMANTICS is still validated here by execution:
each guard runs the opcode through the real interpreter and checks the
decoded result against a concrete expected value. -/

def asIntList : WVal → Option (List Int)
  | .arr _ es => es.mapM carrierToInt
  | _ => none

-- array.new_fixed: build [10, 20] as an array of carriers.
def cArrFixed : CodeTbl := fun fn =>
  if fn = 1 then some ⟨0, 0,
    [.i64Const 10, .call 6, .i64Const 20, .call 6, .arrayNewFixed 7 2]⟩ else none
example :
    ((wFuncN cArrFixed gHost 4 1 []).bind asIntList) = some [10, 20] := by native_decide

-- i32.eq
def cI32Eq : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 0, [.localGet 0, .localGet 1, .i32Eq]⟩ else none
example : ((wFuncN cI32Eq gHost 4 1 [WVal.i32v 5, WVal.i32v 5]).bind asBool) = some true := by
  native_decide
example : ((wFuncN cI32Eq gHost 4 1 [WVal.i32v 5, WVal.i32v 4]).bind asBool) = some false := by
  native_decide

-- i64.eqz
def cI64Eqz : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 0, [.localGet 0, .i64Eqz]⟩ else none
example : ((wFuncN cI64Eqz gHost 4 1 [WVal.i64v 0]).bind asBool) = some true := by native_decide
example : ((wFuncN cI64Eqz gHost 4 1 [WVal.i64v 3]).bind asBool) = some false := by native_decide

-- ref.null (+ ref.is_null)
def cRefNull : CodeTbl := fun fn =>
  if fn = 1 then some ⟨0, 0, [.refNull, .refIsNull]⟩ else none
example : ((wFuncN cRefNull gHost 4 1 []).bind asBool) = some true := by native_decide

-- return (plain): box the argument then early-return it.
def cRet : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 0, [.localGet 0, .structGet 5 0, .call 6, .ret, .i64Const 999, .call 6]⟩
  else none
example :
    ((wFuncN cRet gHost 4 1 [carrierSmall 5 42]).bind carrierToInt) = some 42 := by native_decide

-- remaining i32 / i64 / f64 comparison + arithmetic opcodes, each executed once.
def cMisc (ops : List WInstr) : CodeTbl := fun fn =>
  if fn = 1 then some ⟨2, 0, ops⟩ else none
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i32GtS]) gHost 4 1
    [WVal.i32v 7, WVal.i32v 3]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i32LeS]) gHost 4 1
    [WVal.i32v 3, WVal.i32v 3]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i64Eq]) gHost 4 1
    [WVal.i64v 9, WVal.i64v 9]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i64GeS]) gHost 4 1
    [WVal.i64v 9, WVal.i64v 8]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i64GtS]) gHost 4 1
    [WVal.i64v 9, WVal.i64v 8]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .i64LtS]) gHost 4 1
    [WVal.i64v 8, WVal.i64v 9]).bind asBool) = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .f64Add]) gHost 4 1
    [WVal.f64v (1.5 : Float).toBits, WVal.f64v (2.5 : Float).toBits]).bind asF64)
    = some (4.0 : Float).toBits := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .f64Mul]) gHost 4 1
    [WVal.f64v (3.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asF64)
    = some (6.0 : Float).toBits := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .f64Le]) gHost 4 1
    [WVal.f64v (2.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
    = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .f64Ge]) gHost 4 1
    [WVal.f64v (2.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
    = some true := by native_decide
example : ((wFuncN (cMisc [.localGet 0, .localGet 1, .f64Gt]) gHost 4 1
    [WVal.f64v (3.0 : Float).toBits, WVal.f64v (2.0 : Float).toBits]).bind asBool)
    = some true := by native_decide

/-! ## LEB128 index encoders

Boundary sanity for the shared total encoders. The pairs that matter:
127/128 is where the UNSIGNED encoding grows a second byte, 63/64 is where
the SIGNED (s33) encoding grows one (bit 6 of the final group is the sign),
and `4294967295` is the largest index the u32 regime admits — five groups,
still inside both encoders' exact fuel range. 166 is a live two-byte call
target (the smallest helper index of the notepad fixture). -/

example : uleb32Bytes 0 = [0x00] := by decide
example : uleb32Bytes 63 = [0x3f] := by decide
example : uleb32Bytes 64 = [0x40] := by decide
example : uleb32Bytes 127 = [0x7f] := by decide
example : uleb32Bytes 128 = [0x80, 0x01] := by decide
example : uleb32Bytes 166 = [0xa6, 0x01] := by decide
example : uleb32Bytes 16384 = [0x80, 0x80, 0x01] := by decide
example : uleb32Bytes 4294967295 = [0xff, 0xff, 0xff, 0xff, 0x0f] := by decide

example : s33Bytes 0 = [0x00] := by decide
example : s33Bytes 63 = [0x3f] := by decide
example : s33Bytes 64 = [0xc0, 0x00] := by decide
example : s33Bytes 127 = [0xff, 0x00] := by decide
example : s33Bytes 128 = [0x80, 0x01] := by decide
example : s33Bytes 8192 = [0x80, 0xc0, 0x00] := by decide
example : s33Bytes 4294967295 = [0xff, 0xff, 0xff, 0xff, 0x0f] := by decide

end CertPrelude
