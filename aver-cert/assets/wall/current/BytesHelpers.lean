/- BytesHelpers — the packed `Bytes` runtime helpers, pinned and proved.

   A `Bytes` value is the standard library's refinement `record Bytes
   { values: List<Int> }` whose every element the compiler proves to be an
   octet (`src/codegen/proof_lower/packed_sequence.rs`). On wasm-gc it is not
   a record but a packed `(array (mut i8))` of its octets, and the compiler
   emits five helpers per packed type (`src/codegen/wasm_gc/packed_sequences.rs`):

   * `pack` — `Bytes(values = xs)`: count the cons cells of an `List<Int>`,
     allocate that many bytes, and store each element's low 8 bits (the Int
     goes through `__aint_to_i64_checked`, which traps on a Big carrier, and
     `i32.wrap_i64`);
   * `unpack` — `bytes.values`: cons every byte, zero-extended and boxed,
     onto a list, from the last byte to the first;
   * `concat` / `take` / `drop` — `List.concat` / `take` / `drop` over the
     carrier projections the record construction consumes: one `array.copy`
     (two for `concat`) into a fresh array of the result length, the count
     clamped to the length.

   This file gives each helper, and `__aint_to_i64_checked`, its TEMPLATE:
   the instruction tree the emitter writes, as a function of the declared
   indices (the `List<Int>` cons struct `L`, the byte array type `B`, the Int
   carrier `C`, the checked conversion and the box helper). The acceptance
   pins every declared helper's code entry to its template's bytes, exactly as
   `ListHelpers` pins the List helpers. Nothing about a `Bytes` helper is a
   runtime contract: the template RUNS with `ListHelpers.hRun` (whose array
   instructions `newBytes`, `getU`, `setAt` and `copyTo` model the packed
   `i8` array), and the theorems below prove what each run computes, for
   example `catSemB_eq : catSemB B [bytesW B xs, bytesW B ys] = some (bytesW B
   (xs ++ ys))` below `2 ^ 31` bytes.

   The `i32` arithmetic of the model is exact inside the signed `i32` range
   and stuck outside it, so a result of `2 ^ 31` bytes or more is not
   modelled (the run is `none`, which only makes an obligation vacuous on
   that input; no engine allocates such an array). -/
import ListHelpers

namespace AverCert.BytesHelpers
open CertPrelude AverCert.Grammar AverCert.ListHelpers

/-! ## Code with grouped locals

The packed helpers declare their locals in groups (`Function::new([(2,
i32), (1, ref)])`), unlike the List helpers' one group per local. -/

structure BCode where
  params : Nat
  groups : List (Nat × LTy)
  body : List HI

/-- The declared locals, one per slot. -/
def BCode.locals (c : BCode) : List LTy :=
  c.groups.flatMap fun g => List.replicate g.1 g.2

def BCode.toH (c : BCode) : HCode :=
  { params := c.params, locals := c.locals, body := c.body }

/-- The locals vector: one `(n, type)` group per declared group. -/
def groupsBytes (M : MCtx) : List (Nat × LTy) → Option (List Nat)
  | [] => some []
  | (n, t) :: gs =>
      match uleb32 n, t.bytes M, groupsBytes M gs with
      | some a, some b, some c => some (a ++ b ++ c)
      | _, _, _ => none

/-- A helper's code entry without its size prefix (what
    `AcceptedArtifact.bodyBytesAtFuncIndex` reads). -/
def bBodyBytes (M : MCtx) (c : BCode) : Option (List Nat) :=
  match uleb32 c.groups.length, groupsBytes M c.groups, encHL M c.body with
  | some n, some g, some b => some (n ++ g ++ b ++ [0x0b])
  | _, _, _ => none

def bCall (host : HostTbl) (c : BCode) (fuel : Nat) (args : List WVal) : Option WVal :=
  hCall host c.toH fuel args

/-! ## The templates -/

def i32c (k : Int) : HI := .b (.op (.i32Const k))
def alen : HI := .b (.op .arrayLen)
def callH (f : Nat) : HI := .b (.op (.call f))

/-- `pack`'s first pass (`emit_pack`): count the cons cells in an `i32`. -/
def packCount (L : Nat) : List HI :=
  [lg 1, isNull, .brIf 1, lg 2, i32c 1, .i32Add, ls 2, lg 1, tl L, ls 1, .br 0]

/-- `pack`'s second pass: store each head's low byte at the running index. -/
def packFill (L B chk : Nat) : List HI :=
  [lg 1, isNull, .brIf 1,
   .setAt 3 B [lg 4, lg 1, hd L, callH chk, .wrapI64],
   lg 4, i32c 1, .i32Add, ls 4, lg 1, tl L, ls 1, .br 0]

def packCode (L B chk : Nat) : BCode :=
  { params := 1, groups := [(1, .ref L), (1, .i32), (1, .ref B), (1, .i32)],
    body := [lg 0, ls 1, i32c 0, ls 2, .block [.loop (packCount L)],
      lg 2, .newBytes B, ls 3,
      lg 0, ls 1, i32c 0, ls 4, .block [.loop (packFill L B chk)],
      lg 3] }

/-- `unpack`'s loop (`emit_unpack`): from the last byte down, cons the boxed
    byte onto the accumulator. -/
def unpackBody (L B box : Nat) : List HI :=
  [lg 2, .b (.op .i32Eqz), .brIf 1, lg 2, i32c 1, .i32Sub, ls 2,
   lg 0, lg 2, .getU B, .extU, callH box, lg 1, cons L, ls 1, .br 0]

def unpackCode (L B box : Nat) : BCode :=
  { params := 1, groups := [(1, .ref L), (1, .i32)],
    body := [.b (.nullOf L), ls 1, lg 0, alen, ls 2, .block [.loop (unpackBody L B box)], lg 1] }

/-- `concat` (`emit_concat`): both arrays, copied into one fresh array. -/
def catCodeB (B : Nat) : BCode :=
  { params := 2, groups := [(2, .i32), (1, .ref B)],
    body := [lg 0, alen, ls 2, lg 1, alen, ls 3, lg 2, lg 3, .i32Add, .newBytes B, ls 4,
      .copyTo 4 B [i32c 0, lg 0, i32c 0, lg 2],
      .copyTo 4 B [lg 2, lg 1, i32c 0, lg 3],
      lg 4] }

/-- The count clamp of `take` / `drop`: a positive count below the length
    (unsigned) is the count, a larger one the length; local 3 stays `0`
    otherwise. -/
def clampB : List HI :=
  [lg 1, i64c 0, .b (.op .i64GtS),
   .ifThen [lg 1, lg 2, .extU, .i64LtU, .ifElseI32 [lg 1, .wrapI64] [lg 2], ls 3]]

/-- `take` (`emit_take`): the first `min count len` bytes. -/
def takeCodeB (B : Nat) : BCode :=
  { params := 2, groups := [(2, .i32), (1, .ref B)],
    body := [lg 0, alen, ls 2, i32c 0, ls 3] ++ clampB ++
      [lg 3, .newBytes B, ls 4, .copyTo 4 B [i32c 0, lg 0, i32c 0, lg 3], lg 4] }

/-- `drop` (`emit_drop`): the bytes after the first `min count len`. -/
def dropCodeB (B : Nat) : BCode :=
  { params := 2, groups := [(3, .i32), (1, .ref B)],
    body := [lg 0, alen, ls 2, i32c 0, ls 3] ++ clampB ++
      [lg 2, lg 3, .i32Sub, ls 4, lg 4, .newBytes B, ls 5,
       .copyTo 5 B [i32c 0, lg 0, lg 3, lg 4], lg 5] }

/-- `__aint_to_i64_checked` (`to_i64_checked.wat`) over the carrier struct
    `C`: a Small carrier's `i64`; a Big one traps. -/
def chkCode (C : Nat) : BCode :=
  { params := 1, groups := [],
    body := [lg 0, .b (.op (.structGet C 1)), isNull,
      .ifElseI64 [lg 0, .b (.op (.structGet C 0))] [.unreachable]] }

/-! ## Meaning: each helper is its template, run -/

/-- The wasm image of a byte sequence: the packed array of its bytes. -/
def bytesW (B : Nat) (bs : List Nat) : WVal := .arr B (bs.map fun (b : Nat) => .i32v (b : Int))

/-- The run's fuel over an array argument: enough for `unpack`'s loop. -/
def arrFuel : List WVal → Nat
  | .arr _ es :: _ => es.length + 64
  | _ => 0

def chkSem (C : Nat) (args : List WVal) : Option WVal :=
  bCall noHost (chkCode C) 16 args

def packSem (L B C chk : Nat) (args : List WVal) : Option WVal :=
  bCall (oneHost chk 1 (chkSem C)) (packCode L B chk) (hFuel args) args

def unpackSem (L B C box : Nat) (args : List WVal) : Option WVal :=
  bCall (oneHost box 1 (boxRef C)) (unpackCode L B box) (arrFuel args) args

def catSemB (B : Nat) (args : List WVal) : Option WVal :=
  bCall noHost (catCodeB B) 64 args

def takeSemB (B : Nat) (args : List WVal) : Option WVal :=
  bCall noHost (takeCodeB B) 64 args

def dropSemB (B : Nat) (args : List WVal) : Option WVal :=
  bCall noHost (dropCodeB B) 64 args

/-! ## Control steps of the array instructions -/

section Steps
variable (host : HostTbl)

theorem hRun_i32Add (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i32Add :: rest) l (.i32v y :: .i32v x :: st) =
      if inI32 (x + y) then hRun host k rest l (.i32v (x + y) :: st) else none := by
  simp only [hRun]

theorem hRun_i32Sub (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i32Sub :: rest) l (.i32v y :: .i32v x :: st) =
      if inI32 (x - y) then hRun host k rest l (.i32v (x - y) :: st) else none := by
  simp only [hRun]

theorem hRun_wrapI64 (k : Nat) (rest : List HI) (l : List WVal) (x : Int) (st : List WVal) :
    hRun host (k + 1) (.wrapI64 :: rest) l (.i64v x :: st) =
      hRun host k rest l (.i32v (wrapI32 x) :: st) := by
  simp only [hRun]

theorem hRun_i64LtU (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i64LtU :: rest) l (.i64v y :: .i64v x :: st) =
      if inI64 x && inI64 y then hRun host k rest l (b32 (ltU64 x y) :: st)
      else none := by
  simp only [hRun]

theorem hRun_unreachable (k : Nat) (rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.unreachable :: rest) l st = none := by
  simp only [hRun]

theorem hRun_ifElseI32 (k : Nat) (tB eB rest : List HI) (l : List WVal) (c : Int)
    (st : List WVal) :
    hRun host (k + 1) (.ifElseI32 tB eB :: rest) l (.i32v c :: st) =
      match hRun host k (if c = 0 then eB else tB) l st with
      | some (.ok l' st'') => hRun host k rest l' st''
      | some (.ret v) => some (.ret v)
      | _ => none := by
  simp only [hRun]; rfl

theorem hRun_newBytes (k : Nat) (ty : Nat) (rest : List HI) (l : List WVal) (n : Int)
    (st : List WVal) :
    hRun host (k + 1) (.newBytes ty :: rest) l (.i32v n :: st) =
      if 0 ≤ n then hRun host k rest l (.arr ty (List.replicate n.toNat (.i32v 0)) :: st)
      else none := by
  simp only [hRun]

theorem hRun_getU (k ty : Nat) (rest : List HI) (l : List WVal) (i : Int) (t : Nat)
    (es : List WVal) (st : List WVal) :
    hRun host (k + 1) (.getU ty :: rest) l (.i32v i :: .arr t es :: st) =
      if t = ty ∧ 0 ≤ i then
        match es[i.toNat]? with
        | some (.i32v v) => hRun host k rest l (.i32v (v % 256) :: st)
        | _ => none
      else none := by
  simp only [hRun]; rfl

theorem hRun_setAt (k j ty : Nat) (args rest : List HI) (l st : List WVal) (es : List WVal)
    (hj : l[j]? = some (.arr ty es)) :
    hRun host (k + 1) (.setAt j ty args :: rest) l st =
      match hRun host k args l st with
      | some (.ok l' (.i32v v :: .i32v i :: st')) =>
          if 0 ≤ i ∧ i < es.length then
            hRun host k rest (l'.set j (.arr ty (es.set i.toNat (.i32v (v % 256))))) st'
          else none
      | _ => none := by
  simp only [hRun, hj, ↓reduceIte]; rfl

theorem hRun_copyTo (k j ty : Nat) (args rest : List HI) (l st : List WVal) (dst : List WVal)
    (hj : l[j]? = some (.arr ty dst)) :
    hRun host (k + 1) (.copyTo j ty args :: rest) l st =
      match hRun host k args l st with
      | some (.ok l' (.i32v n :: .i32v so :: .arr t2 src :: .i32v d :: st')) =>
          if t2 = ty ∧ 0 ≤ n ∧ 0 ≤ so ∧ 0 ≤ d ∧ so + n ≤ src.length ∧ d + n ≤ dst.length then
            hRun host k rest (l'.set j (.arr ty (copyInto dst src d.toNat so.toNat n.toNat))) st'
          else none
      | _ => none := by
  simp only [hRun, hj, ↓reduceIte]; rfl

end Steps

/-! ## Arithmetic facts -/

theorem inI32_iff (x : Int) : inI32 x = true ↔ -2147483648 ≤ x ∧ x < 2147483648 := by
  cases x <;> simp only [inI32, Nat.blt_eq, Int.ofNat_eq_natCast, Int.negSucc_eq] <;> omega

theorem inI32_of {x : Int} (h0 : -2147483648 ≤ x) (h1 : x < 2147483648) : inI32 x = true :=
  (inI32_iff x).2 ⟨h0, h1⟩

theorem inI32_false {x : Int} (h : ¬ (-2147483648 ≤ x ∧ x < 2147483648)) : inI32 x = false := by
  cases hx : inI32 x
  · rfl
  · exact absurd ((inI32_iff x).1 hx) h

theorem inI64_iff (x : Int) :
    inI64 x = true ↔ -9223372036854775808 ≤ x ∧ x < 9223372036854775808 := by
  cases x <;> simp only [inI64, Nat.blt_eq, Int.ofNat_eq_natCast, Int.negSucc_eq] <;> omega

theorem inI64_of {x : Int} (h0 : -9223372036854775808 ≤ x) (h1 : x < 9223372036854775808) :
    inI64 x = true :=
  (inI64_iff x).2 ⟨h0, h1⟩

theorem i32Word_of {x : Int} (h0 : -2147483648 ≤ x) (h1 : x < 4294967296) : i32Word x = true := by
  cases x <;> simp only [Int.ofNat_eq_natCast, Int.negSucc_eq] at h0 h1 <;>
    simp only [i32Word, Nat.blt_eq] <;> omega

theorem toU32_of_nonneg {x : Int} (h : 0 ≤ x) : toU32 x = x := by
  cases x with
  | ofNat n => rfl
  | negSucc n => omega

theorem ltU64_of_nonneg {x y : Int} (hx : 0 ≤ x) (hy : 0 ≤ y) : ltU64 x y = decide (x < y) := by
  cases x with
  | negSucc a => omega
  | ofNat a =>
      cases y with
      | negSucc b => omega
      | ofNat b =>
          simp only [ltU64]
          rw [Bool.eq_iff_iff]
          simp only [Nat.blt_eq, Int.ofNat_eq_natCast, decide_eq_true_eq]
          omega

theorem wrapI32_mod (x : Int) : wrapI32 x % 256 = x % 256 := by
  unfold wrapI32; omega

theorem wrapI32_id {x : Int} (h0 : -2147483648 ≤ x) (h1 : x < 2147483648) : wrapI32 x = x := by
  unfold wrapI32; omega

/-! ## `__aint_to_i64_checked` -/

theorem chkSem_small (C : Nat) (s sg : Int) :
    chkSem C [.structv C [.i64v s, .null, .i32v sg]] = some (.i64v s) := by
  simp [chkSem, bCall, hCall, chkCode, BCode.toH, BCode.locals, hRun_b, hRun_ifElse, step1,
    eraseI, wRunF, lg, isNull, b32]

theorem chkSem_big (C : Nat) (s : Int) (lty : Nat) (les : List WVal) (sg : Int) :
    chkSem C [.structv C [.i64v s, .arr lty les, .i32v sg]] = none := by
  simp [chkSem, bCall, hCall, chkCode, BCode.toH, BCode.locals, hRun_b, hRun_ifElse, step1,
    eraseI, wRunF, lg, isNull, b32, hRun_unreachable]

/-- On a represented Int, the checked conversion returns the Int itself
    (a Small carrier) or fails (a Big one traps). -/
theorem chkSem_spec {C : Nat} (S : AverCert.Schema.CarrierSpec C) (n : Int) (w r : WVal)
    (hw : S.Repr n w) (h : chkSem C [w] = some r) : r = .i64v n := by
  rcases S.car n w hw with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · rw [chkSem_small] at h
    rw [S.smallElim n s sg hw] at h
    exact (Option.some.inj h).symm
  · rw [chkSem_big] at h
    cases h

/-! ## `concat` -/

theorem catSemB_run (B : Nat) (xs ys : List WVal) :
    catSemB B [.arr B xs, .arr B ys] =
      if xs.length + ys.length < 2147483648 then some (.arr B (xs ++ ys)) else none := by
  by_cases h : xs.length + ys.length < 2147483648
  · have hi : inI32 ((xs.length : Int) + ys.length) = true := inI32_of (by omega) (by omega)
    have e : ((xs.length : Int) + ys.length).toNat = xs.length + ys.length := by omega
    have e3 : (0 : Int) ≤ xs.length + ys.length := by omega
    have e4 : (xs.length : Int) ≤ xs.length + ys.length := by omega
    simp [h, e, e3, e4, catSemB, bCall, hCall, catCodeB, BCode.toH, BCode.locals, hRun_b, step1,
      eraseI, wRunF, lg, ls, alen, i32c, hRun_i32Add, hi, hRun_newBytes, hRun_copyTo, copyInto,
      LTy.dflt]
  · have hi : inI32 ((xs.length : Int) + ys.length) = false := inI32_false (by omega)
    simp [h, catSemB, bCall, hCall, catCodeB, BCode.toH, BCode.locals, hRun_b, step1, eraseI,
      wRunF, lg, ls, alen, i32c, hRun_i32Add, hi]

/-! ## `take` and `drop` -/

theorem hRun_extU (host : HostTbl) (k : Nat) (rest : List HI) (a : Int) (l st : List WVal)
    (h0 : 0 ≤ a) (h1 : a < 4294967296) :
    hRun host (k + 1) (.extU :: rest) l (.i32v a :: st) = hRun host k rest l (.i64v a :: st) := by
  have hw : i32Word a = true := i32Word_of (by omega) h1
  simp only [hRun, hw, ↓reduceIte, toU32_of_nonneg h0]

/-- The count clamp over a length below `2 ^ 31`: local 3 becomes
    `min (max c 0) len`. -/
theorem clampB_run (host : HostTbl) (k : Nat) (rest : List HI) (a : WVal) (os : List WVal)
    (c : Int) (n : Nat) (hn : n < 2147483648) (hc' : 0 < c → inI64 c = true) (st : List WVal) :
    hRun host (k + 20) (clampB ++ rest) (a :: .i64v c :: .i32v n :: .i32v 0 :: os) st =
      hRun host (k + 16) rest (a :: .i64v c :: .i32v n :: .i32v (min (max c 0) n) :: os) st := by
  have hlen : inI64 (n : Int) = true := inI64_of (by omega) (by omega)
  have hx1 : (n : Int) < 4294967296 := by omega
  have hx0 : (0 : Int) ≤ n := by omega
  by_cases hpos : 0 < c
  · have hc := hc' hpos
    have hmax : max c 0 = c := by omega
    by_cases hlt : c < n
    · have hu : ltU64 c n = true := by rw [ltU64_of_nonneg (by omega) hx0]; simp [hlt]
      have hw : wrapI32 c = c := wrapI32_id (by omega) (by omega)
      have hmin : min c (n : Int) = c := by omega
      simp [clampB, hRun_b, step1, eraseI, wRunF, lg, ls, i64c, hRun_extU, hRun_ifThen,
        hRun_i64LtU, hRun_ifElseI32, hRun_wrapI64, hpos, hc, hlen, hx0, hx1, hu, hw, b32, hmax,
        hmin]
    · have hu : ltU64 c n = false := by rw [ltU64_of_nonneg (by omega) hx0]; simp [hlt]
      have hmin : min c (n : Int) = n := by omega
      simp [clampB, hRun_b, step1, eraseI, wRunF, lg, ls, i64c, hRun_extU, hRun_ifThen,
        hRun_i64LtU, hRun_ifElseI32, hpos, hc, hlen, hx0, hx1, hu, b32, hmax, hmin]
  · have hmax : max c 0 = 0 := by omega
    have hmin : min (0 : Int) n = 0 := by omega
    simp [clampB, hRun_b, step1, eraseI, wRunF, lg, i64c, hRun_ifThen, hpos, b32, hmax, hmin]

/-- A positive count outside the `i64` range: `i64.lt_u` is stuck. -/
theorem clampB_none (host : HostTbl) (k : Nat) (rest : List HI) (a : WVal) (os : List WVal)
    (c : Int) (n : Nat) (hn : n < 2147483648) (hpos : 0 < c) (hc : inI64 c = false)
    (st : List WVal) :
    hRun host (k + 20) (clampB ++ rest) (a :: .i64v c :: .i32v n :: .i32v 0 :: os) st = none := by
  have hlen : inI64 (n : Int) = true := inI64_of (by omega) (by omega)
  have hx1 : (n : Int) < 4294967296 := by omega
  have hx0 : (0 : Int) ≤ n := by omega
  simp [clampB, hRun_b, step1, eraseI, wRunF, lg, ls, i64c, hRun_extU, hRun_ifThen,
    hRun_i64LtU, hpos, hc, hlen, hx0, hx1, b32]

theorem takeSemB_run (B : Nat) (xs : List WVal) (c : Int) (hl : xs.length < 2147483648)
    (hc : 0 < c → inI64 c = true) :
    takeSemB B [.arr B xs, .i64v c] = some (.arr B (xs.take c.toNat)) := by
  unfold takeSemB bCall hCall
  have hpre : hRun noHost 64 (takeCodeB B).toH.body
      ([.arr B xs, .i64v c] ++ (takeCodeB B).toH.locals.map LTy.dflt) [] =
      hRun noHost (39 + 20) (clampB ++ [lg 3, .newBytes B, ls 4,
        .copyTo 4 B [i32c 0, lg 0, i32c 0, lg 3], lg 4])
        [.arr B xs, .i64v c, .i32v xs.length, .i32v 0, .null] [] := by
    simp [takeCodeB, BCode.toH, BCode.locals, LTy.dflt, hRun_b, step1, eraseI, wRunF, lg, ls,
      alen, i32c]
  have hm : (min (max c 0) (xs.length : Int)).toNat = min c.toNat xs.length := by omega
  have h0 : (0 : Int) ≤ min (max c 0) (xs.length : Int) := by omega
  have h1 : min (max c 0) (xs.length : Int) ≤ xs.length := by omega
  have ht : xs.take (min c.toNat xs.length) = xs.take c.toNat := take_min_length xs c.toNat
  have h2 : min (max c 0) (xs.length : Int) ≤ ((min c.toNat xs.length : Nat) : Int) := by omega
  simp only [BCode.toH, takeCodeB, List.length_cons, List.length_nil] at hpre ⊢
  simp only [↓reduceIte]
  rw [hpre, clampB_run noHost 39 _ _ [.null] c xs.length hl hc]
  simp [hRun_b, step1, eraseI, wRunF, lg, ls, i32c, hRun_newBytes, hRun_copyTo, copyInto, h0,
    h1, hm, ht, h2]

theorem dropSemB_run (B : Nat) (xs : List WVal) (c : Int) (hl : xs.length < 2147483648)
    (hc : 0 < c → inI64 c = true) :
    dropSemB B [.arr B xs, .i64v c] = some (.arr B (xs.drop c.toNat)) := by
  unfold dropSemB bCall hCall
  have hpre : hRun noHost 64 (dropCodeB B).toH.body
      ([.arr B xs, .i64v c] ++ (dropCodeB B).toH.locals.map LTy.dflt) [] =
      hRun noHost (39 + 20) (clampB ++ [lg 2, lg 3, .i32Sub, ls 4, lg 4, .newBytes B, ls 5,
        .copyTo 5 B [i32c 0, lg 0, lg 3, lg 4], lg 5])
        [.arr B xs, .i64v c, .i32v xs.length, .i32v 0, .i32v 0, .null] [] := by
    simp [dropCodeB, BCode.toH, BCode.locals, LTy.dflt, hRun_b, step1, eraseI, wRunF, lg, ls,
      alen, i32c]
  have hm : (min (max c 0) (xs.length : Int)).toNat = min c.toNat xs.length := by omega
  have h0 : (0 : Int) ≤ min (max c 0) (xs.length : Int) := by omega
  have hsub : inI32 ((xs.length : Int) - min (max c 0) (xs.length : Int)) = true :=
    inI32_of (by omega) (by omega)
  have h3 : (0 : Int) ≤ (xs.length : Int) - min (max c 0) (xs.length : Int) := by omega
  have h4 : min (max c 0) (xs.length : Int) + ((xs.length : Int) - min (max c 0) xs.length) ≤
      xs.length := by omega
  have h5 : ((xs.length : Int) - min (max c 0) (xs.length : Int)).toNat =
      xs.length - min c.toNat xs.length := by omega
  have hd : xs.drop (min c.toNat xs.length) = xs.drop c.toNat := drop_min_length xs c.toNat
  have h6 : min (max c 0) (xs.length : Int) ≤ xs.length := by omega
  have h7 : (xs.length : Int) - min (max c 0) (xs.length : Int) ≤
      ((xs.length - min c.toNat xs.length : Nat) : Int) := by omega
  have h8 : (xs.drop c.toNat).take (xs.length - min c.toNat xs.length) = xs.drop c.toNat := by
    apply List.take_of_length_le; simp; omega
  simp only [BCode.toH, dropCodeB, List.length_cons, List.length_nil] at hpre ⊢
  simp only [↓reduceIte]
  rw [hpre, clampB_run noHost 39 _ _ [.i32v 0, .null] c xs.length hl hc]
  simp [hRun_b, step1, eraseI, wRunF, lg, ls, i32c, hRun_i32Sub, hsub, hRun_newBytes,
    hRun_copyTo, copyInto, h0, h4, h5, hm, hd, h6, h7, h8]

theorem takeSemB_spec (B : Nat) (xs : List WVal) (c : Int) (hl : xs.length < 2147483648)
    (v : WVal) (h : takeSemB B [.arr B xs, .i64v c] = some v) : v = .arr B (xs.take c.toNat) := by
  by_cases hbad : 0 < c ∧ inI64 c = false
  · unfold takeSemB bCall hCall at h
    have hpre : hRun noHost 64 (takeCodeB B).toH.body
        ([.arr B xs, .i64v c] ++ (takeCodeB B).toH.locals.map LTy.dflt) [] =
        hRun noHost (39 + 20) (clampB ++ [lg 3, .newBytes B, ls 4,
          .copyTo 4 B [i32c 0, lg 0, i32c 0, lg 3], lg 4])
          [.arr B xs, .i64v c, .i32v xs.length, .i32v 0, .null] [] := by
      simp [takeCodeB, BCode.toH, BCode.locals, LTy.dflt, hRun_b, step1, eraseI, wRunF, lg, ls,
        alen, i32c]
    simp only [BCode.toH, takeCodeB, List.length_cons, List.length_nil] at hpre h
    simp only [↓reduceIte] at h
    rw [hpre, clampB_none noHost 39 _ _ [.null] c xs.length hl hbad.1 hbad.2] at h
    cases h
  · have hc : 0 < c → inI64 c = true := by
      intro hp; cases hi : inI64 c
      · exact absurd ⟨hp, hi⟩ hbad
      · rfl
    rw [takeSemB_run B xs c hl hc] at h
    exact (Option.some.inj h).symm

theorem dropSemB_spec (B : Nat) (xs : List WVal) (c : Int) (hl : xs.length < 2147483648)
    (v : WVal) (h : dropSemB B [.arr B xs, .i64v c] = some v) : v = .arr B (xs.drop c.toNat) := by
  by_cases hbad : 0 < c ∧ inI64 c = false
  · unfold dropSemB bCall hCall at h
    have hpre : hRun noHost 64 (dropCodeB B).toH.body
        ([.arr B xs, .i64v c] ++ (dropCodeB B).toH.locals.map LTy.dflt) [] =
        hRun noHost (39 + 20) (clampB ++ [lg 2, lg 3, .i32Sub, ls 4, lg 4, .newBytes B, ls 5,
          .copyTo 5 B [i32c 0, lg 0, lg 3, lg 4], lg 5])
          [.arr B xs, .i64v c, .i32v xs.length, .i32v 0, .i32v 0, .null] [] := by
      simp [dropCodeB, BCode.toH, BCode.locals, LTy.dflt, hRun_b, step1, eraseI, wRunF, lg, ls,
        alen, i32c]
    simp only [BCode.toH, dropCodeB, List.length_cons, List.length_nil] at hpre h
    simp only [↓reduceIte] at h
    rw [hpre, clampB_none noHost 39 _ _ [.i32v 0, .null] c xs.length hl hbad.1 hbad.2] at h
    cases h
  · have hc : 0 < c → inI64 c = true := by
      intro hp; cases hi : inI64 c
      · exact absurd ⟨hp, hi⟩ hbad
      · rfl
    rw [dropSemB_run B xs c hl hc] at h
    exact (Option.some.inj h).symm

theorem catSemB_spec (B : Nat) (xs ys : List WVal) (v : WVal)
    (h : catSemB B [.arr B xs, .arr B ys] = some v) :
    v = .arr B (xs ++ ys) ∧ xs.length + ys.length < 2147483648 := by
  rw [catSemB_run] at h
  by_cases hl : xs.length + ys.length < 2147483648
  · simp only [hl, ↓reduceIte, Option.some.injEq] at h
    exact ⟨h.symm, hl⟩
  · simp [hl] at h

/-! ## `unpack` -/

theorem hRun_callH (host : HostTbl) (k f : Nat) (g : List WVal → Option WVal) (rest : List HI)
    (l : List WVal) (w r : WVal) (st : List WVal) (hh : host f = some (1, g))
    (hg : g [w] = some r) :
    hRun host (k + 1) (callH f :: rest) l (w :: st) = hRun host k rest l (r :: st) := by
  simp [callH, hRun_b, step1, eraseI, wRunF, hh, hg, popArgs]

theorem bytesW_get (bs : List Nat) (j : Nat) (hj : j < bs.length) :
    (bs.map fun (b : Nat) => WVal.i32v (b : Int))[j]? = some (.i32v (bs[j] : Int)) := by
  simp [List.getElem?_map, List.getElem?_eq_getElem hj]

theorem unpackBody_step (host : HostTbl) (L B box C : Nat) (hb : host box = some (1, boxRef C))
    (bs : List Nat) (hbs : ∀ b ∈ bs, b < 256) (hlen : bs.length < 2147483648) (j : Nat)
    (hj : j < bs.length) (acc : List WVal) (n : Nat) (hn : 20 ≤ n) (st : List WVal) :
    hRun host n (unpackBody L B box) [bytesW B bs, wList L acc, .i32v ((j + 1 : Nat) : Int)] st =
      some (.br 0 [bytesW B bs, wList L (carrierSmall C bs[j] :: acc), .i32v (j : Int)] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have hb256 : bs[j] < 256 := hbs _ (List.getElem_mem hj)
  have hg : boxRef C [.i64v (bs[j] : Int)] = some (carrierSmall C bs[j]) := rfl
  have hm : ((bs[j] : Nat) : Int) % 256 = bs[j] := by omega
  have hget := bytesW_get bs j hj
  have hz : ¬ ((j : Int) + 1 = 0) := by omega
  have hi : inI32 (j : Int) = true := inI32_of (by omega) (by omega)
  have hb2 : ((bs[j] : Nat) : Int) < 4294967296 := by omega
  simp only [unpackBody, bytesW]
  simp [hRun_b, step1, eraseI, wRunF, lg, ls, i32c, hRun_brIf, hRun_i32Sub, hz, hi,
    hRun_getU, hget, hm, hRun_extU, hb2, hRun_callH host _ box _ _ _ _ _ _ hb hg, cons, hRun_br,
    b32, popArgs_two', wList]

theorem unpackBody_zero (host : HostTbl) (L B box : Nat) (a acc : WVal) (n : Nat) (hn : 4 ≤ n)
    (st : List WVal) :
    hRun host n (unpackBody L B box) [a, acc, .i32v 0] st = some (.br 1 [a, acc, .i32v 0] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [unpackBody, hRun_b, step1, eraseI, wRunF, lg, hRun_brIf, b32]

/-- The boxed bytes, as `unpack` conses them. -/
def smallW (C : Nat) (bs : List Nat) : List WVal := bs.map fun (b : Nat) => carrierSmall C (b : Int)

theorem unpackLoop (host : HostTbl) (L B box C : Nat) (hb : host box = some (1, boxRef C))
    (bs : List Nat) (hbs : ∀ b ∈ bs, b < 256) (hlen : bs.length < 2147483648) (st : List WVal) :
    ∀ (i : Nat) (acc : List WVal) (n : Nat), i ≤ bs.length → i + 21 ≤ n →
    hRun host n [.loop (unpackBody L B box)] [bytesW B bs, wList L acc, .i32v (i : Int)] st =
      some (.br 0 [bytesW B bs, wList L (smallW C (bs.take i) ++ acc), .i32v 0] st)
  | 0, acc, n, _, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop]
      rw [show ((0 : Nat) : Int) = 0 from rfl, unpackBody_zero host L B box _ _ k (by omega)]
      simp [smallW]
  | j + 1, acc, n, hj, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, unpackBody_step host L B box C hb bs hbs hlen j (by omega) acc k (by omega)]
      simp only
      have ih := unpackLoop host L B box C hb bs hbs hlen st j (carrierSmall C bs[j] :: acc) k
        (by omega) (by omega)
      simp only [wList] at ih ⊢
      rw [ih]
      have htake : bs.take (j + 1) = bs.take j ++ [bs[j]] := by
        rw [List.take_add_one, List.getElem?_eq_getElem (by omega)]; rfl
      rw [htake]
      simp only [smallW, List.map_append, List.map_cons, List.map_nil, List.append_assoc,
        List.singleton_append]

theorem unpackSem_eq (L B C box : Nat) (bs : List Nat) (hbs : ∀ b ∈ bs, b < 256)
    (hlen : bs.length < 2147483648) :
    unpackSem L B C box [bytesW B bs] = some (wList L (smallW C bs)) := by
  unfold unpackSem bCall hCall
  have hb : oneHost box 1 (boxRef C) box = some (1, boxRef C) := by simp [oneHost]
  have h0 := unpackLoop (oneHost box 1 (boxRef C)) L B box C hb bs hbs hlen [] bs.length []
    (bs.length + 58) (Nat.le_refl _) (by omega)
  simp only [wList, List.take_length, List.append_nil, bytesW] at h0
  simp [unpackCode, BCode.toH, BCode.locals, LTy.dflt, arrFuel, bytesW, hRun_b, hRun_block,
    step1, eraseI, wRunF, lg, ls, alen, h0]

/-! ## `pack` -/

theorem packCount_nil (host : HostTbl) (L n : Nat) (hn : 3 ≤ n) (a c o i : WVal)
    (st : List WVal) :
    hRun host n (packCount L) [a, .null, c, o, i] st = some (.br 1 [a, .null, c, o, i] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [packCount, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem packCount_cons (host : HostTbl) (L n : Nat) (hn : 12 ≤ n) (a x t o i : WVal) (c : Int)
    (st : List WVal) :
    hRun host n (packCount L) [a, .structv L [x, t], .i32v c, o, i] st =
      if inI32 (c + 1) then some (.br 0 [a, t, .i32v (c + 1), o, i] st) else none := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  by_cases h : inI32 (c + 1) = true
  · simp [packCount, hRun_b, hRun_brIf, hRun_i32Add, hRun_br, step1, eraseI, wRunF, lg, ls,
      isNull, tl, i32c, b32, h]
  · simp at h
    simp [packCount, hRun_b, hRun_brIf, hRun_i32Add, step1, eraseI, wRunF, lg, ls, isNull,
      i32c, b32, h]

theorem countLoop (host : HostTbl) (L : Nat) (a o i : WVal) (st : List WVal) :
    ∀ (ys : List WVal) (c : Int) (n : Nat), 0 ≤ c → c < 2147483648 → ys.length + 13 ≤ n →
    hRun host n [.loop (packCount L)] [a, wList L ys, .i32v c, o, i] st =
      if c + ys.length < 2147483648 then
        some (.br 0 [a, .null, .i32v (c + ys.length), o, i] st)
      else none
  | [], c, n, h0, h1, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, packCount_nil host L k (by simp at hn; omega)]
      simp [h1]
  | y :: ys, c, n, h0, h1, hn => by
      simp only [List.length_cons] at hn
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, packCount_cons host L k (by omega)]
      by_cases hc : c + 1 < 2147483648
      · have hi : inI32 (c + 1) = true := inI32_of (by omega) hc
        simp only [hi, ↓reduceIte]
        rw [countLoop host L a o i st ys (c + 1) k (by omega) hc (by omega)]
        simp only [List.length_cons]
        rw [show c + 1 + (ys.length : Int) = c + ((ys.length + 1 : Nat) : Int) by omega]
      · have hi : inI32 (c + 1) = false := inI32_false (by omega)
        have hc' : ¬ (c + ((ys.length + 1 : Nat) : Int) < 2147483648) := by omega
        simp only [hi, List.length_cons, hc']
        simp

/-- What `pack`'s second pass stores for each cell: the low byte of the
    checked conversion's `i64`, or nothing at the first cell it fails on. -/
def fillRes (g : List WVal → Option WVal) : List WVal → Option (List WVal)
  | [] => some []
  | w :: ws =>
      match g [w] with
      | some (.i64v x) => (fillRes g ws).map (.i32v (wrapI32 x % 256) :: ·)
      | _ => none

theorem packFill_nil (host : HostTbl) (L B chk n : Nat) (hn : 3 ≤ n) (a c o i : WVal)
    (st : List WVal) :
    hRun host n (packFill L B chk) [a, .null, c, o, i] st =
      some (.br 1 [a, .null, c, o, i] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [packFill, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem packFill_cons (host : HostTbl) (L B chk : Nat) (g : List WVal → Option WVal)
    (hh : host chk = some (1, g)) (n : Nat) (hn : 20 ≤ n) (a w t c : WVal) (es : List WVal)
    (i : Nat) (hi : i < es.length) (hi2 : i + 1 < 2147483648) (st : List WVal) :
    hRun host n (packFill L B chk) [a, .structv L [w, t], c, .arr B es, .i32v (i : Int)] st =
      match g [w] with
      | some (.i64v x) =>
          some (.br 0 [a, t, c, .arr B (es.set i (.i32v (wrapI32 x % 256))),
            .i32v ((i + 1 : Nat) : Int)] st)
      | _ => none := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have h3 : [a, .structv L [w, t], c, .arr B es, .i32v (i : Int)][3]? = some (.arr B es) := rfl
  have hadd : inI32 ((i : Int) + 1) = true := inI32_of (by omega) (by omega)
  have hlt : (i : Int) < es.length := by omega
  simp only [packFill]
  rw [show k + 20 = (k + 16) + 1 + 1 + 1 + 1 by omega]
  simp only [hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32, List.getElem?_cons_succ,
    List.getElem?_cons_zero]
  simp only [Bool.false_eq_true, ↓reduceIte]
  rw [hRun_setAt host _ 3 B _ _ _ _ es h3]
  cases hg : g [w] with
  | none =>
      simp [hRun_b, step1, eraseI, wRunF, hd, callH, hh, hg, popArgs]
  | some r =>
      cases r with
      | i64v x =>
          simp [hRun_b, step1, eraseI, wRunF, ls, hd, tl, callH, hh, hg, popArgs,
            hRun_wrapI64, i32c, hRun_i32Add, hadd, hlt, hRun_br]
      | _ =>
          simp [hRun_b, step1, eraseI, wRunF, hd, callH, hh, hg, popArgs, hRun]

theorem set_mid {α : Type} (pre rest : List α) (z v : α) :
    (pre ++ z :: rest).set pre.length v = pre ++ v :: rest := by
  induction pre with
  | nil => rfl
  | cons x xs ih => simp [ih]

theorem fillLoop (host : HostTbl) (L B chk : Nat) (g : List WVal → Option WVal)
    (hh : host chk = some (1, g)) (a c : WVal) (st : List WVal) :
    ∀ (ws pre : List WVal) (n : Nat), pre.length + ws.length < 2147483648 →
    ws.length + 21 ≤ n →
    hRun host n [.loop (packFill L B chk)]
        [a, wList L ws, c, .arr B (pre ++ List.replicate ws.length (.i32v 0)),
          .i32v (pre.length : Int)] st =
      (fillRes g ws).map (fun bs =>
        .br 0 [a, .null, c, .arr B (pre ++ bs), .i32v ((pre.length + ws.length : Nat) : Int)] st)
  | [], pre, n, _, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, packFill_nil host L B chk k (by simp at hn; omega)]
      simp [fillRes]
  | w :: ws, pre, n, hl, hn => by
      simp only [List.length_cons] at hl hn
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, List.length_cons, List.replicate_succ,
        packFill_cons host L B chk g hh k (by omega) a w _ c _ pre.length (by simp) (by omega)]
      cases hg : g [w] with
      | none => simp [fillRes, hg]
      | some r =>
          cases r with
          | i64v x =>
              simp only
              rw [set_mid]
              have ih := fillLoop host L B chk g hh a c st ws (pre ++ [.i32v (wrapI32 x % 256)]) k
                (by simp; omega) (by omega)
              simp only [List.append_assoc, List.singleton_append, List.length_append,
                List.length_singleton] at ih
              rw [ih]
              simp only [fillRes, hg, Option.map_map]
              cases fillRes g ws with
              | none => rfl
              | some bs =>
                  simp only [Option.map_some, Function.comp_apply]
                  rw [show pre.length + 1 + ws.length = pre.length + (ws.length + 1) by omega]
          | _ => simp [fillRes, hg]

theorem packSem_run (L B C chk : Nat) (ws : List WVal) :
    packSem L B C chk [wList L ws] =
      if ws.length < 2147483648 then (fillRes (chkSem C) ws).map (.arr B ·) else none := by
  unfold packSem bCall hCall
  rw [hFuel_wList]
  have hh : oneHost chk 1 (chkSem C) chk = some (1, chkSem C) := by simp [oneHost]
  have hc := countLoop (oneHost chk 1 (chkSem C)) L (wList L ws) .null (.i32v 0) []
    ws 0 (ws.length + 59) (by omega) (by omega) (by omega)
  simp only [Int.zero_add] at hc
  by_cases h63 : ws.length < 9223372036854775808
  · simp only [h63, ↓reduceIte]
    by_cases h31 : ws.length < 2147483648
    · have h31' : (ws.length : Int) < 2147483648 := by omega
      have hf := fillLoop (oneHost chk 1 (chkSem C)) L B chk (chkSem C) hh (wList L ws)
        (.i32v (ws.length : Int)) [] ws [] (ws.length + 51) (by simp; omega) (by omega)
      simp only [h31', ↓reduceIte] at hc
      simp only [List.nil_append, List.length_nil, Nat.zero_add, Int.natCast_zero] at hf
      have hz : (0 : Int) ≤ ws.length := by omega
      have hr : (ws.length : Int).toNat = ws.length := by omega
      simp [h31, packCode, BCode.toH, BCode.locals, LTy.dflt, hRun_b, hRun_block, step1, eraseI,
        wRunF, lg, ls, i32c, hc, hRun_newBytes, hz, hr, hf]
      cases fillRes (chkSem C) ws <;> rfl
    · have h31' : ¬ (ws.length : Int) < 2147483648 := by omega
      simp only [h31', ↓reduceIte] at hc
      simp [h31, packCode, BCode.toH, BCode.locals, LTy.dflt, hRun_b, hRun_block, step1, eraseI,
        wRunF, lg, ls, i32c, hc]
  · have h31 : ¬ ws.length < 2147483648 := by omega
    simp [h63, h31]

theorem byteOf_lt (n : Int) : byteOf n < 256 := by unfold byteOf; omega

theorem i32v_byteOf (n : Int) : WVal.i32v ((byteOf n : Nat) : Int) = .i32v (n % 256) := by
  unfold byteOf; congr 1; omega

/-- Words related pointwise to Ints. -/
def RelI (R : Int → WVal → Prop) : List Int → List WVal → Prop
  | [], [] => True
  | n :: ns, w :: ws => R n w ∧ RelI R ns ws
  | _, _ => False

theorem fillRes_spec {C : Nat} (S : AverCert.Schema.CarrierSpec C) :
    ∀ (ns : List Int) (ws : List WVal) (bs : List WVal), RelI S.Repr ns ws →
    fillRes (chkSem C) ws = some bs → bs = (ns.map byteOf).map fun (b : Nat) => .i32v (b : Int)
  | [], [], bs, _, h => by simp [fillRes] at h; simp [h]
  | n :: ns, w :: ws, bs, hr, h => by
      simp only [fillRes] at h
      cases hc : chkSem C [w] with
      | none => simp [hc] at h
      | some r =>
          have hr' := chkSem_spec S n w r hr.1 hc
          subst hr'
          simp only [hc, Option.map_eq_some_iff] at h
          obtain ⟨bs', hbs', rfl⟩ := h
          rw [fillRes_spec S ns ws bs' hr.2 hbs', wrapI32_mod]
          simp [i32v_byteOf]
  | [], _ :: _, _, hr, _ => by simp [RelI] at hr
  | _ :: _, [], _, hr, _ => by simp [RelI] at hr

theorem relI_length {R : Int → WVal → Prop} :
    ∀ {ns : List Int} {ws : List WVal}, RelI R ns ws → ns.length = ws.length
  | [], [], _ => rfl
  | _ :: _, _ :: _, h => by simp [relI_length h.2]
  | [], _ :: _, h => by simp [RelI] at h
  | _ :: _, [], h => by simp [RelI] at h

/-- What a run of `pack` computes over represented Ints: the low bytes, below
    `2 ^ 31` of them. -/
theorem packSem_spec {C : Nat} (S : AverCert.Schema.CarrierSpec C) (L B chk : Nat)
    (ns : List Int) (ws : List WVal) (hr : RelI S.Repr ns ws) (v : WVal)
    (h : packSem L B C chk [wList L ws] = some v) :
    v = bytesW B (ns.map byteOf) ∧ ns.length < 2147483648 := by
  rw [packSem_run] at h
  have hl := relI_length hr
  by_cases h31 : ws.length < 2147483648
  · simp only [h31, ↓reduceIte, Option.map_eq_some_iff] at h
    obtain ⟨bs, hbs, rfl⟩ := h
    rw [fillRes_spec S ns ws bs hr hbs]
    exact ⟨rfl, by omega⟩
  · simp [h31] at h

theorem fillRes_small (C : Nat) :
    ∀ ns : List Int, fillRes (chkSem C) (ns.map (carrierSmall C)) =
      some ((ns.map byteOf).map fun (b : Nat) => .i32v (b : Int))
  | [] => rfl
  | n :: ns => by
      simp only [List.map, fillRes, carrierSmall, chkSem_small, fillRes_small C ns, Option.map_some,
        wrapI32_mod, i32v_byteOf]

/-- `pack` returns on every list of Small carriers below `2 ^ 31` cells. -/
theorem packSem_small (L B C chk : Nat) (ns : List Int) (hl : ns.length < 2147483648) :
    packSem L B C chk [wList L (ns.map (carrierSmall C))] = some (bytesW B (ns.map byteOf)) := by
  rw [packSem_run]
  simp [hl, fillRes_small, bytesW]

/-! ## The helpers of a module -/

/-- The template of the `Bytes` helper of role `r`, over the module's
    declared indices: the `List<Int>` cons struct, the packed array, the
    checked conversion (`pack`) and the box helper (`unpack`). -/
def bytesCode (M : MCtx) : BytesRole → BCode
  | .pack => packCode (M.listStruct .int) M.bytesArr M.toI64Chk
  | .unpack => unpackCode (M.listStruct .int) M.bytesArr M.box
  | .concat => catCodeB M.bytesArr
  | .take => takeCodeB M.bytesArr
  | .drop => dropCodeB M.bytesArr

/-- The meaning of the `Bytes` helper of role `r`: its template, run. `pack`
    calls the pinned checked conversion, `unpack` the wall's box. -/
def bytesSem (M : MCtx) : BytesRole → List WVal → Option WVal
  | .pack => packSem (M.listStruct .int) M.bytesArr M.carrier M.toI64Chk
  | .unpack => unpackSem (M.listStruct .int) M.bytesArr M.carrier M.box
  | .concat => catSemB M.bytesArr
  | .take => takeSemB M.bytesArr
  | .drop => dropSemB M.bytesArr

/-- The contract helpers a `Bytes` helper's template calls: `unpack` boxes
    each byte with `__aint_from_i64` (the box contract). -/
def bytesInnerCalls (M : MCtx) : BytesRole → List Nat
  | .unpack => [M.box]
  | _ => []

/-! ## The typing of a `Bytes` builtin -/

theorem bytesHelper_some {M : MCtx} {r : BytesRole} (h : (M.bytesHelper r).isSome = true) :
    ∃ f, M.bytesHelper r = some f := Option.isSome_iff_exists.mp h

set_option linter.unusedSimpArgs false in
theorem builtinTy_bytesOfList {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesOfList ts = some T) :
    ∃ f, ts = [.list .int] ∧ T = .bytes ∧ M.bytesHelper .pack = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, rest⟩⟩
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    cases t <;> simp only [builtinTy] at h <;> try cases h
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := bytesHelper_some hs
      exact ⟨f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_bytesValues {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesValues ts = some T) :
    ∃ f, ts = [.bytes] ∧ T = .list .int ∧ M.bytesHelper .unpack = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, rest⟩⟩
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := bytesHelper_some hs
      exact ⟨f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_bytesLen {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesLen ts = some T) : ts = [.bytes] ∧ T = .int := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, rest⟩⟩
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    exact ⟨rfl, rfl⟩
  · simp [builtinTy] at h

theorem builtinTy_bytesConcat {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesConcat ts = some T) :
    ∃ f, ts = [.bytes, .bytes] ∧ T = .bytes ∧ M.bytesHelper .concat = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := bytesHelper_some hs
      exact ⟨f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_bytesTake {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesTake ts = some T) :
    ∃ f, ts = [.bytes, .int] ∧ T = .bytes ∧ M.bytesHelper .take = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := bytesHelper_some hs
      exact ⟨f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_bytesDrop {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .bytesDrop ts = some T) :
    ∃ f, ts = [.bytes, .int] ∧ T = .bytes ∧ M.bytesHelper .drop = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := bytesHelper_some hs
      exact ⟨f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

end AverCert.BytesHelpers
