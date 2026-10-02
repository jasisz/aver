/- StringHelpers — the `String.fromInt` runtime helper, pinned and proved.

   An `Int` part of a string interpolation is a call of the runtime helper
   `String.fromInt` (`src/codegen/wasm_gc/builtins/wat/string_from_aint.wat`,
   emitted by `bignum::emit_string_from_aint`), which writes the decimal
   digits of its `Int` argument into a fresh `$string`. A Small carrier (its
   `i64` field, no magnitude array) is formatted with two loops over the
   `i64`: one counts the digits, one stores them from the last to the first,
   after a leading `-` for a negative number. A Big carrier (a magnitude
   array) is divided by ten limb by limb.

   This file gives the helper its TEMPLATE over the Int carrier `C`, its
   magnitude array `G` and `$string` `S`, with its locals in the emitter's
   groups; the acceptance pins the declared helper's code entry and function
   type to it, as it pins the List and `Bytes` helpers. Nothing about the
   helper is a runtime contract. The Small branch RUNS with `ListHelpers.hRun`,
   and `fromIntSem_spec` proves what it returns: the decimal bytes
   `Grammar.decBytes n` of the Int it represents. The Big branch is pinned by
   its bytes but not run (`HI.dead`): formatting a Big Int is not modelled,
   which only makes an obligation vacuous on such an input. -/
import BytesHelpers

namespace AverCert.StringHelpers
open CertPrelude AverCert.Grammar AverCert.ListHelpers AverCert.BytesHelpers

/-! ## The template -/

def eqz64 : HI := .b (.op .i64Eqz)
def ltS64 : HI := .b (.op .i64LtS)
def ltS32 : HI := .b (.op .i32LtS)
def sget (C f : Nat) : HI := .b (.op (.structGet C f))
def aget (G : Nat) : HI := .b (.op (.arrayGet G))
/-- The Big branch's instructions the wall only encodes (`HI.rawOp`). -/
def geU32 : HI := .rawOp [0x4f]
def mul32 : HI := .rawOp [0x6c]
def shl64 : HI := .rawOp [0x86]
def and64 : HI := .rawOp [0x83]
def or64 : HI := .rawOp [0x84]

/-- The count loop: one more digit (local 4) per division of `copy`
    (local 3) by ten, until it is zero. -/
def countB : List HI :=
  [lg 3, eqz64, .brIf 1, lg 4, i32c 1, .i32Add, ls 4, lg 3, i64c 10, .i64DivU, ls 3, .br 0]

/-- The fill loop: from position `i` (local 6) down to `neg` (local 7), the
    low digit of `copy` (local 3), into the array in local 8. -/
def fillB (S : Nat) : List HI :=
  [lg 6, lg 7, ltS32, .brIf 1,
   .setAt 8 S [lg 6, i32c 48, lg 3, i64c 10, .i64RemU, .wrapI64, .i32Add],
   lg 3, i64c 10, .i64DivU, ls 3, lg 6, i32c 1, .i32Sub, ls 6, .br 0]

/-- A nonzero Small Int (local 1): its sign (local 7), its magnitude
    (local 2, read unsigned), the digit count, the array and the fill, then a
    `-` at the front of a negative number. -/
def nonzeroB (S : Nat) : List HI :=
  [lg 1, i64c 0, ltS64, .ifElseI32 [i32c 1] [i32c 0], ls 7,
   lg 7, .ifElseI64 [i64c 0, lg 1, .i64Sub] [lg 1], ls 2,
   lg 2, ls 3, i32c 0, ls 4, .block [.loop countB],
   lg 4, lg 7, .i32Add, ls 5, lg 5, .newBytes S, ls 8,
   lg 5, i32c 1, .i32Sub, ls 6, lg 2, ls 3, .block [.loop (fillB S)],
   lg 7, .ifThen [.setAt 8 S [i32c 0, i32c 45]], lg 8]

/-- The Small branch: `"0"` for zero, else `nonzeroB`. -/
def smallB (C S : Nat) : List HI :=
  [lg 0, sget C 0, ls 1, lg 1, eqz64, .ifElseRef S [i32c 48, i32c 1, .newFill S] (nonzeroB S)]

/-- The Big branch, by its instructions: copy the magnitude, divide it by ten
    limb by limb while it is nonzero, collecting the digits, then write the
    sign and the digits reversed. Pinned, not run. -/
def bigB (C G S : Nat) : List HI :=
  [lg 0, sget C 2, i32c 0, ltS32, .ifElseI32 [i32c 1] [i32c 0], ls 7,
   lg 0, sget C 1, alen, ls 10,
   lg 10, .newBytes G, ls 9,
   i32c 0, ls 11,
   .block [.loop [lg 11, lg 10, geU32, .brIf 1,
     .setAt 9 G [lg 11, lg 0, sget C 1, lg 11, aget G],
     lg 11, i32c 1, .i32Add, ls 11, .br 0]],
   lg 10, i32c 10, mul32, .newBytes S, ls 16,
   i32c 0, ls 17,
   .block [.loop [i64c 0, ls 13, lg 10, ls 11,
     .block [.loop [lg 11, .b (.op .i32Eqz), .brIf 1, lg 11, i32c 1, .i32Sub, ls 11,
       lg 13, i64c 32, shl64, lg 9, lg 11, aget G, i64c 4294967295, and64, or64, ls 12,
       lg 12, i64c 10, .i64DivU, ls 14, lg 12, i64c 10, .i64RemU, ls 13,
       .setAt 9 G [lg 11, lg 14], .br 0]],
     .setAt 16 S [lg 17, i32c 48, lg 13, .wrapI64, .i32Add],
     lg 17, i32c 1, .i32Add, ls 17, i32c 1, ls 15, i32c 0, ls 11,
     .block [.loop [lg 11, lg 10, geU32, .brIf 1, lg 9, lg 11, aget G, i64c 0, .b (.op .i64Ne),
       .ifThen [i32c 0, ls 15, .br 2], lg 11, i32c 1, .i32Add, ls 11, .br 0]],
     lg 15, .brIf 1, .br 0]],
   lg 17, lg 7, .i32Add, ls 5, lg 5, .newBytes S, ls 8,
   lg 7, .ifThen [.setAt 8 S [i32c 0, i32c 45]],
   i32c 0, ls 6,
   .block [.loop [lg 6, lg 17, geU32, .brIf 1,
     .setAt 8 S [lg 7, lg 6, .i32Add, lg 16, lg 17, i32c 1, .i32Sub, lg 6, .i32Sub, .getU S],
     lg 6, i32c 1, .i32Add, ls 6, .br 0]],
   lg 8]

/-- `String.fromInt` over the carrier `C`, its magnitude array `G` and
    `$string` `S`: the Small branch, or the Big one (not run). The locals are
    the wat's, in its groups: `n abs copy : i64`, `digits total i neg : i32`,
    `arr : $string`, `magc : G`, `len li : i32`, `cur rem q : i64`,
    `allzero : i32`, `digbuf : $string`, `dcount : i32`. -/
def fromIntCode (C G S : Nat) : BCode :=
  { params := 1,
    groups := [(3, .i64), (4, .i32), (1, .ref S), (1, .ref G), (2, .i32), (3, .i64), (1, .i32),
      (1, .ref S), (1, .i32)],
    body := [lg 0, sget C 1, isNull, .ifElseRef S (smallB C S) [.dead (bigB C G S)]] }

/-- The helper's meaning: its template, run. It calls nothing. -/
def fromIntSem (C G S : Nat) (args : List WVal) : Option WVal :=
  bCall noHost (fromIntCode C G S) 256 args

/-! ## Control steps of the new instructions -/

section Steps
variable (host : HostTbl)

theorem hRun_ifElseRef (k ht : Nat) (tB eB rest : List HI) (l : List WVal) (c : Int)
    (st : List WVal) :
    hRun host (k + 1) (.ifElseRef ht tB eB :: rest) l (.i32v c :: st) =
      match hRun host k (if c = 0 then eB else tB) l st with
      | some (.ok l' st'') => hRun host k rest l' st''
      | some (.ret v) => some (.ret v)
      | _ => none := by
  simp only [hRun]; rfl

theorem hRun_newFill (k ty : Nat) (rest : List HI) (l : List WVal) (n v : Int)
    (st : List WVal) :
    hRun host (k + 1) (.newFill ty :: rest) l (.i32v n :: .i32v v :: st) =
      if 0 ≤ n then hRun host k rest l (.arr ty (List.replicate n.toNat (.i32v (v % 256))) :: st)
      else none := by
  simp only [hRun]

theorem hRun_i64Sub (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i64Sub :: rest) l (.i64v y :: .i64v x :: st) =
      if inI64 x && inI64 y then hRun host k rest l (.i64v (wrapI64 (x - y)) :: st)
      else none := by
  simp only [hRun]

theorem hRun_i64DivU (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i64DivU :: rest) l (.i64v y :: .i64v x :: st) =
      if inI64 x && inI64 y && y != 0 then
        hRun host k rest l (.i64v (ofU64 (toU64 x / toU64 y)) :: st)
      else none := by
  simp only [hRun]

theorem hRun_i64RemU (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i64RemU :: rest) l (.i64v y :: .i64v x :: st) =
      if inI64 x && inI64 y && y != 0 then
        hRun host k rest l (.i64v (ofU64 (toU64 x % toU64 y)) :: st)
      else none := by
  simp only [hRun]

theorem hRun_dead (k : Nat) (body rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.dead body :: rest) l st = none := by
  simp only [hRun]

end Steps

/-! Sanity: the new instructions on concrete operands. `i64.div_u` reads a
negative word unsigned; `i64.sub` wraps and is stuck outside the `i64` range;
`array.new` stores the low byte; `dead` and `rawOp` are stuck. -/

example : hRun noHost 2 [.i64DivU] [] [.i64v 10, .i64v (-1)] =
    some (.ok [] [.i64v 1844674407370955161]) := rfl

example : hRun noHost 2 [.i64RemU] [] [.i64v 0, .i64v 7] = none := by decide

example : hRun noHost 2 [.i64Sub] [] [.i64v 1, .i64v (-9223372036854775808)] =
    some (.ok [] [.i64v 9223372036854775807]) := rfl

example : hRun noHost 2 [.i64Sub] [] [.i64v 1, .i64v 9223372036854775808] = none := by decide

example : hRun noHost 2 [.newFill 0] [] [.i32v 2, .i32v 300] =
    some (.ok [] [.arr 0 [.i32v 44, .i32v 44]]) := rfl

example : hRun noHost 2 [.dead [.ret]] [] [.i32v 0] = none := by decide

example : hRun noHost 2 [.rawOp [0x6c]] [] [.i32v 2, .i32v 3] = none := by decide

/-! ## Unsigned 64-bit words -/

theorem ofU64_small {u : Int} (h0 : 0 ≤ u) (h1 : u < 9223372036854775808) : ofU64 u = u := by
  unfold ofU64; simp [h1]

theorem inI64_ofU64 {u : Int} (h0 : 0 ≤ u) (h1 : u < 18446744073709551616) :
    inI64 (ofU64 u) = true := by
  apply (inI64_iff _).2
  unfold ofU64; split <;> omega

theorem toU64_ofU64 {u : Int} (h0 : 0 ≤ u) (h1 : u < 18446744073709551616) :
    toU64 (ofU64 u) = u := by
  unfold toU64 ofU64; split <;> split <;> omega

theorem ofU64_eq_zero {u : Int} (h0 : 0 ≤ u) (h1 : u < 18446744073709551616) :
    ofU64 u = 0 ↔ u = 0 := by
  unfold ofU64; split <;> omega

theorem toU64_ten : toU64 10 = 10 := by decide

/-! ## Decimal digits -/

theorem digitsRev_zero : digitsRev 0 = [] := by
  rw [digitsRev]; simp

theorem digitsRev_pos {m : Nat} (h : m ≠ 0) :
    digitsRev m = (48 + m % 10) :: digitsRev (m / 10) := by
  rw [digitsRev]; simp [h]

theorem digitsRev_length_le : ∀ (k m : Nat), m < 10 ^ k → (digitsRev m).length ≤ k
  | 0, m, h => by
      have : m = 0 := by simpa using h
      subst this; simp [digitsRev_zero]
  | k + 1, m, h => by
      by_cases hm : m = 0
      · subst hm; simp [digitsRev_zero]
      · rw [digitsRev_pos hm, List.length_cons]
        have : m / 10 < 10 ^ k := by
          rw [Nat.pow_succ] at h
          exact Nat.div_lt_of_lt_mul (by simpa [Nat.mul_comm] using h)
        have := digitsRev_length_le k (m / 10) this
        omega

theorem digitsRev_length_pos {m : Nat} (h : m ≠ 0) : 0 < (digitsRev m).length := by
  rw [digitsRev_pos h]; simp

/-- At most twenty digits below `2 ^ 64`. -/
theorem digitsRev_length_20 {m : Nat} (h : m < 18446744073709551616) :
    (digitsRev m).length ≤ 20 :=
  digitsRev_length_le 20 m (Nat.lt_of_lt_of_le h (by decide))

/-! ## The count loop -/

section Loops
variable (host : HostTbl)

theorem countB_zero (n : Nat) (hn : 3 ≤ n) (l st : List WVal)
    (h3 : l[3]? = some (.i64v 0)) :
    hRun host n countB l st = some (.br 1 l st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [countB, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, eqz64, b32, h3]

theorem countB_step (n : Nat) (hn : 12 ≤ n) (l st : List WVal) (c : Nat) (d : Int)
    (hc0 : c ≠ 0) (hc : c < 18446744073709551616) (hd : inI32 (d + 1) = true)
    (h3 : l[3]? = some (.i64v (ofU64 c))) (h4 : l[4]? = some (.i32v d)) :
    hRun host n countB l st =
      some (.br 0 ((l.set 4 (.i32v (d + 1))).set 3 (.i64v (ofU64 ((c / 10 : Nat) : Int)))) st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have hz : ofU64 (c : Int) ≠ 0 := fun h => hc0 (by
    have := (ofU64_eq_zero (u := (c : Int)) (by omega) (by omega)).1 h; omega)
  have hi : inI64 (ofU64 (c : Int)) = true := inI64_ofU64 (by omega) (by omega)
  have hu : toU64 (ofU64 (c : Int)) = c := toU64_ofU64 (by omega) (by omega)
  have h10 : inI64 (10 : Int) = true := by decide
  have hdiv : ((c / 10 : Nat) : Int) = (c : Int) / 10 := by omega
  rw [hdiv]
  simp [countB, hRun_b, hRun_brIf, hRun_i32Add, hRun_i64DivU, hRun_br, step1, eraseI, wRunF, lg,
    ls, i32c, i64c, eqz64, b32, h3, h4, hz, hd, hi, hu, h10, toU64_ten, List.getElem?_set_ne]

theorem countLoopM (st : List WVal) :
    ∀ (m c : Nat) (l : List WVal) (d : Int), c < m →
    c < 18446744073709551616 → 0 ≤ d → d + (digitsRev c).length < 2147483648 →
    l[3]? = some (.i64v (ofU64 c)) → l[4]? = some (.i32v d) →
    ∃ l', (∀ n, (digitsRev c).length + 14 ≤ n →
        hRun host n [.loop countB] l st = some (.br 0 l' st)) ∧
      l'[3]? = some (.i64v 0) ∧ l'[4]? = some (.i32v (d + (digitsRev c).length)) ∧
      l'.length = l.length ∧ ∀ j, j ≠ 3 → j ≠ 4 → l'[j]? = l[j]?
  | 0, c, _, _, hm, _, _, _, _, _ => absurd hm (Nat.not_lt_zero c)
  | m + 1, c, l, d, hm, hc, hd, hlen, h3, h4 => by
    by_cases hc0 : c = 0
    · subst hc0
      have h3' : l[3]? = some (.i64v 0) := by simpa [ofU64] using h3
      refine ⟨l, fun n hn => ?_, h3', by simpa [digitsRev_zero] using h4, rfl, fun _ _ _ => rfl⟩
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, countB_zero host k (by simp [digitsRev_zero] at hn; omega) l st h3']
    · have hlen' : (digitsRev c).length = (digitsRev (c / 10)).length + 1 := by
        rw [digitsRev_pos hc0]; simp
      have hd1 : inI32 (d + 1) = true := inI32_of (by omega) (by omega)
      have hl3 : 3 < l.length := by
        rcases Nat.lt_or_ge 3 l.length with h | h
        · exact h
        · rw [List.getElem?_eq_none (by omega)] at h3; cases h3
      have hl4 : 4 < l.length := by
        rcases Nat.lt_or_ge 4 l.length with h | h
        · exact h
        · rw [List.getElem?_eq_none (by omega)] at h4; cases h4
      obtain ⟨l', hrun, h3', h4', hlen'', hrest⟩ :=
        countLoopM st m (c / 10) ((l.set 4 (.i32v (d + 1))).set 3
          (.i64v (ofU64 ((c / 10 : Nat) : Int)))) (d + 1) (by omega) (by omega) (by omega)
          (by omega) (by simp [hl3]) (by simp [hl4])
      refine ⟨l', fun n hn => ?_, h3', ?_, ?_, ?_⟩
      · obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
        rw [hRun_loop, countB_step host k (by omega) l st c d hc0 hc hd1 h3 h4]
        exact hrun k (by omega)
      · rw [h4', hlen']; congr 2; push_cast; omega
      · simp [hlen'']
      · intro j hj3 hj4
        rw [hrest j hj3 hj4]
        simp [List.getElem?_set_ne (Ne.symm hj3), List.getElem?_set_ne (Ne.symm hj4)]

theorem countLoop (st : List WVal) (c : Nat) (l : List WVal) (d : Int)
    (hc : c < 18446744073709551616) (hd : 0 ≤ d) (hlen : d + (digitsRev c).length < 2147483648)
    (h3 : l[3]? = some (.i64v (ofU64 c))) (h4 : l[4]? = some (.i32v d)) :
    ∃ l', (∀ n, (digitsRev c).length + 14 ≤ n →
        hRun host n [.loop countB] l st = some (.br 0 l' st)) ∧
      l'[3]? = some (.i64v 0) ∧ l'[4]? = some (.i32v (d + (digitsRev c).length)) ∧
      l'.length = l.length ∧ ∀ j, j ≠ 3 → j ≠ 4 → l'[j]? = l[j]? :=
  countLoopM host st (c + 1) c l d (by omega) hc hd hlen h3 h4

/-! ## The fill loop -/

theorem set_take {α : Type} : ∀ (l : List α) (i n : Nat) (a : α), n ≤ i →
    (l.set i a).take n = l.take n
  | [], _, _, _, _ => by simp
  | _ :: _, _, 0, _, _ => by simp
  | x :: xs, i + 1, n + 1, a, h => by
      simp only [List.set_cons_succ, List.take_succ_cons]
      rw [set_take xs i n a (by omega)]
  | _ :: _, 0, _ + 1, _, h => by omega

theorem set_drop {α : Type} : ∀ (l : List α) (i : Nat) (a : α), i < l.length →
    (l.set i a).drop i = a :: l.drop (i + 1)
  | [], _, _, h => by simp at h
  | x :: xs, 0, a, _ => by simp
  | x :: xs, i + 1, a, h => by
      simp only [List.set_cons_succ, List.drop_succ_cons]
      exact set_drop xs i a (by simp at h; omega)

theorem fillB_exit (S n : Nat) (hn : 4 ≤ n) (l st : List WVal) (j ng : Int)
    (h6 : l[6]? = some (.i32v j)) (h7 : l[7]? = some (.i32v ng)) (hlt : j < ng) :
    hRun host n (fillB S) l st = some (.br 1 l st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [fillB, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, ltS32, b32, h6, h7, hlt]

theorem fillB_step (S n : Nat) (hn : 20 ≤ n) (l st : List WVal) (c : Nat) (j ng : Int)
    (A : List WVal) (hc : c < 18446744073709551616) (hj : ng ≤ j) (hng : 0 ≤ ng)
    (hjA : j < A.length) (hj31 : j < 2147483647)
    (h3 : l[3]? = some (.i64v (ofU64 c))) (h6 : l[6]? = some (.i32v j))
    (h7 : l[7]? = some (.i32v ng)) (h8 : l[8]? = some (.arr S A)) :
    hRun host n (fillB S) l st =
      some (.br 0 (((l.set 8 (.arr S (A.set j.toNat (.i32v ((48 + c % 10 : Nat) : Int))))).set 3
        (.i64v (ofU64 ((c / 10 : Nat) : Int)))).set 6 (.i32v (j - 1))) st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have hi : inI64 (ofU64 (c : Int)) = true := inI64_ofU64 (by omega) (by omega)
  have hu : toU64 (ofU64 (c : Int)) = c := toU64_ofU64 (by omega) (by omega)
  have h10 : inI64 (10 : Int) = true := by decide
  have hdiv : ((c / 10 : Nat) : Int) = (c : Int) / 10 := by omega
  have hmod : ofU64 ((c : Int) % 10) = (c : Int) % 10 := ofU64_small (by omega) (by omega)
  have hw : wrapI32 ((c : Int) % 10) = (c : Int) % 10 := wrapI32_id (by omega) (by omega)
  have ha : inI32 (48 + (c : Int) % 10) = true := inI32_of (by omega) (by omega)
  have hs : inI32 (j - 1) = true := inI32_of (by omega) (by omega)
  have hv : (48 + (c : Int) % 10) % 256 = ((48 + c % 10 : Nat) : Int) := by omega
  have hnot : ¬ j < ng := by omega
  rw [hdiv]
  simp [fillB, hRun_b, hRun_brIf, hRun_i32Add, hRun_i32Sub, hRun_i64DivU, hRun_i64RemU,
    hRun_wrapI64, hRun_br, step1, eraseI, wRunF, lg, ls, i32c, i64c, ltS32, b32, h3, h6, h7, h8,
    hnot, hi, hu, h10, toU64_ten, hmod, hw, ha, hs, hv, hRun_setAt host _ 8 S _ _ _ _ A h8,
    List.getElem?_set_ne, show (0 : Int) ≤ j by omega, hjA]

theorem fillLoopM (S : Nat) (st : List WVal) :
    ∀ (m c : Nat) (l : List WVal) (j ng : Int) (A : List WVal), c < m →
    c < 18446744073709551616 → 0 ≤ ng → ((digitsRev c).length : Int) = j + 1 - ng →
    j + 1 ≤ A.length → j < 2147483647 →
    l[3]? = some (.i64v (ofU64 c)) → l[6]? = some (.i32v j) → l[7]? = some (.i32v ng) →
    l[8]? = some (.arr S A) →
    ∃ l', (∀ n, (digitsRev c).length + 21 ≤ n →
        hRun host n [.loop (fillB S)] l st = some (.br 0 l' st)) ∧
      l'[8]? = some (.arr S (A.take ng.toNat ++
        (digitsRev c).reverse.map (fun b => .i32v (b : Int)) ++ A.drop (j + 1).toNat)) ∧
      l'[7]? = some (.i32v ng) ∧ l'.length = l.length
  | 0, c, _, _, _, _, hm, _, _, _, _, _, _, _, _, _ => absurd hm (Nat.not_lt_zero c)
  | m + 1, c, l, j, ng, A, hm, hc, hng, hlen, hjA, hj31, h3, h6, h7, h8 => by
    by_cases hc0 : c = 0
    · subst hc0
      rw [digitsRev_zero] at hlen
      simp only [List.length_nil] at hlen
      refine ⟨l, fun n hn => ?_, ?_, h7, rfl⟩
      · obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
        rw [hRun_loop, fillB_exit host S k (by simp [digitsRev_zero] at hn; omega) l st j ng h6 h7
          (by omega)]
      · rw [h8, digitsRev_zero]
        simp only [List.reverse_nil, List.map_nil, List.append_nil]
        rw [show (j + 1).toNat = ng.toNat by omega, List.take_append_drop]
    · have hlen' : (digitsRev c).length = (digitsRev (c / 10)).length + 1 := by
        rw [digitsRev_pos hc0]; simp
      have hl8 : 8 < l.length := by
        rcases Nat.lt_or_ge 8 l.length with h | h
        · exact h
        · rw [List.getElem?_eq_none (by omega)] at h8; cases h8
      have hjn : ng ≤ j := by omega
      obtain ⟨l', hrun, h8', h7', hlen''⟩ :=
        fillLoopM S st m (c / 10) (((l.set 8 (.arr S (A.set j.toNat
          (.i32v ((48 + c % 10 : Nat) : Int))))).set 3 (.i64v (ofU64 ((c / 10 : Nat) : Int)))).set 6
          (.i32v (j - 1))) (j - 1) ng (A.set j.toNat (.i32v ((48 + c % 10 : Nat) : Int)))
          (by omega) (by omega) hng (by omega) (by simp; omega) (by omega)
          (by simp [List.getElem?_set, show 3 < l.length by omega])
          (by simp [List.getElem?_set, show 6 < l.length by omega])
          (by rw [List.getElem?_set_ne (by omega), List.getElem?_set_ne (by omega),
                List.getElem?_set_ne (by omega)]; exact h7)
          (by simp [List.getElem?_set, hl8])
      refine ⟨l', fun n hn => ?_, ?_, h7', by simp [hlen'']⟩
      · obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
        rw [hRun_loop, fillB_step host S k (by omega) l st c j ng A hc hjn hng (by omega) hj31
          h3 h6 h7 h8]
        exact hrun k (by omega)
      · rw [h8', digitsRev_pos hc0]
        congr 2
        rw [set_take _ _ _ _ (by omega), show (j - 1 + 1).toNat = j.toNat by omega,
          set_drop _ _ _ (by omega)]
        simp only [List.reverse_cons, List.map_append, List.map_cons, List.map_nil,
          List.append_assoc, List.singleton_append]
        rw [show j.toNat + 1 = (j + 1).toNat by omega]

theorem fillLoop (S : Nat) (st : List WVal) (c : Nat) (l : List WVal) (j ng : Int)
    (A : List WVal) (hc : c < 18446744073709551616) (hng : 0 ≤ ng)
    (hlen : ((digitsRev c).length : Int) = j + 1 - ng) (hjA : j + 1 ≤ A.length)
    (hj31 : j < 2147483647)
    (h3 : l[3]? = some (.i64v (ofU64 c))) (h6 : l[6]? = some (.i32v j))
    (h7 : l[7]? = some (.i32v ng)) (h8 : l[8]? = some (.arr S A)) :
    ∃ l', (∀ n, (digitsRev c).length + 21 ≤ n →
        hRun host n [.loop (fillB S)] l st = some (.br 0 l' st)) ∧
      l'[8]? = some (.arr S (A.take ng.toNat ++
        (digitsRev c).reverse.map (fun b => .i32v (b : Int)) ++ A.drop (j + 1).toNat)) ∧
      l'[7]? = some (.i32v ng) ∧ l'.length = l.length :=
  fillLoopM host S st (c + 1) c l j ng A (by omega) hc hng hlen hjA hj31 h3 h6 h7 h8

end Loops

/-! ## The Small branch -/

/-- The magnitude a nonzero Small Int's `abs` local holds: its absolute value,
    as an `i64` word read unsigned (`-2^63` is its own two's complement). -/
theorem abs_word {n : Int} (h0 : -9223372036854775808 ≤ n) (h1 : n < 9223372036854775808) :
    (if n < 0 then wrapI64 (0 - n) else n) = ofU64 (n.natAbs : Int) := by
  unfold wrapI64 ofU64
  split <;> split <;> omega

/-- The decimal bytes of a nonzero Int, as the `$string` elements. -/
def decW (n : Int) : List WVal := (decBytes n).map fun (b : Nat) => .i32v (b : Int)

theorem nonzeroB_run (S : Nat) (fuel : Nat) (hf : 120 ≤ fuel) (l st : List WVal) (n : Int)
    (hn0 : n ≠ 0) (h0 : -9223372036854775808 ≤ n) (h1 : n < 9223372036854775808)
    (hl : 17 < l.length) (h1l : l[1]? = some (.i64v n)) :
    ∃ l', hRun noHost fuel (nonzeroB S) l st = some (.ok l' (.arr S (decW n) :: st)) := by
  obtain ⟨k, rfl⟩ := fuel_split hf
  have hab := abs_word h0 h1
  have hm : (n.natAbs : Nat) < 18446744073709551616 := by omega
  have hlen20 := digitsRev_length_20 hm
  have hpos := digitsRev_length_pos (show n.natAbs ≠ 0 by omega)
  have hi1 : inI64 n = true := inI64_of (by omega) (by omega)
  have hi0 : inI64 (0 : Int) = true := by decide
  have hl2 : 2 < l.length := by omega
  have hl3 : 3 < l.length := by omega
  have hl4 : 4 < l.length := by omega
  have hl5 : 5 < l.length := by omega
  have hl6 : 6 < l.length := by omega
  have hl7 : 7 < l.length := by omega
  have hl8 : 8 < l.length := by omega
  by_cases hneg : n < 0
  · have hab' : wrapI64 (0 - n) = ofU64 (n.natAbs : Int) := by simpa [hneg] using hab
    simp [nonzeroB, hRun_b, step1, eraseI, wRunF, lg, ls, i64c, i32c, ltS64, b32, h1l,
      hneg, hRun_ifElseI32, hRun_ifElse, hRun_i64Sub, hi1, hi0, hab', List.getElem?_set_ne,
      List.getElem?_set_self, hl2, hl3, hl4, hl7]
    have hab'' : wrapI64 (-n) = ofU64 (n.natAbs : Int) := by simpa using hab'
    rw [hab'']
    obtain ⟨l2, hc2, h23, h24, hlen2, hrest2⟩ := countLoop noHost st n.natAbs
      ((((l.set 7 (WVal.i32v 1)).set 2 (WVal.i64v (ofU64 (n.natAbs : Int)))).set 3
        (WVal.i64v (ofU64 (n.natAbs : Int)))).set 4 (WVal.i32v 0)) 0 hm (Int.le_refl 0) (by omega)
      (by simp [List.getElem?_set, hl3]) (by simp [List.getElem?_set, hl4])
    rw [hRun_block, hc2 (k + 107) (by omega)]
    have h22 : l2[2]? = some (.i64v (ofU64 (n.natAbs : Int))) := by
      rw [hrest2 2 (by omega) (by omega)]; simp [List.getElem?_set, hl2]
    have h27 : l2[7]? = some (.i32v 1) := by
      rw [hrest2 7 (by omega) (by omega)]; simp [List.getElem?_set, hl7]
    have hl2' : 17 < l2.length := by simp [hlen2]; omega
    have hT : inI32 (0 + ((digitsRev n.natAbs).length : Int) + 1) = true :=
      inI32_of (by omega) (by omega)
    have hT1 : inI32 (0 + ((digitsRev n.natAbs).length : Int) + 1 - 1) = true :=
      inI32_of (by omega) (by omega)
    have h25 : 5 < l2.length := by omega
    have h26 : 6 < l2.length := by omega
    have h28 : 8 < l2.length := by omega
    have h23' : 3 < l2.length := by omega
    simp [hRun_b, step1, eraseI, wRunF, lg, ls, i32c, h24, h27, h22, hRun_i32Add, hRun_i32Sub,
      hRun_newBytes, hT, hT1, List.getElem?_set_ne, List.getElem?_set_self, h25, h26, h28]
    refine ⟨inI32_of (by omega) (by omega), by omega, inI32_of (by omega) (by omega), ?_⟩
    obtain ⟨l4, hf4, h48, h47, hlen4⟩ := fillLoop noHost S st n.natAbs
      ((((l2.set 5 (WVal.i32v (((digitsRev n.natAbs).length : Int) + 1))).set 8
        (WVal.arr S (List.replicate ((digitsRev n.natAbs).length + 1) (WVal.i32v 0)))).set 6
        (WVal.i32v ((digitsRev n.natAbs).length : Int))).set 3 (WVal.i64v (ofU64 (n.natAbs : Int))))
      ((digitsRev n.natAbs).length : Int) 1
      (List.replicate ((digitsRev n.natAbs).length + 1) (WVal.i32v 0)) hm (by omega) (by omega)
      (by simp) (by omega) (by simp [List.getElem?_set, h23'])
      (by simp [List.getElem?_set, h26]) (by simp [List.getElem?_set_ne, h27])
      (by simp [List.getElem?_set, h28])
    have hl48 : 8 < l4.length := by simp [hlen4]; omega
    rw [hRun_block, hf4 (k + 93) (by omega)]
    simp only [List.take_replicate, List.drop_replicate, Nat.sub_self, List.replicate_zero,
      List.append_nil] at h48
    simp [hRun_b, hRun_ifThen, step1, eraseI, wRunF, lg, i32c, h47,
      hRun_setAt noHost _ 8 S _ _ _ _ _ h48, List.getElem?_set_self, hl48]
    simp [decW, decBytes, hn0, hneg, List.map_reverse]
  · have hab' : n = ofU64 (n.natAbs : Int) := by simpa [hneg] using hab
    simp [nonzeroB, hRun_b, step1, eraseI, wRunF, lg, ls, i64c, i32c, ltS64, b32, h1l,
      hneg, hRun_ifElseI32, hRun_ifElse, List.getElem?_set_ne,
      List.getElem?_set_self, hl2, hl3, hl4, hl7]
    obtain ⟨l2, hc2, h23, h24, hlen2, hrest2⟩ := countLoop noHost st n.natAbs
      ((((l.set 7 (WVal.i32v 0)).set 2 (WVal.i64v n)).set 3 (WVal.i64v n)).set 4 (WVal.i32v 0))
      0 hm (Int.le_refl 0) (by omega)
      (by simp [List.getElem?_set, hl3]; exact hab') (by simp [List.getElem?_set, hl4])
    rw [hRun_block, hc2 (k + 107) (by omega)]
    have h22 : l2[2]? = some (.i64v n) := by
      rw [hrest2 2 (by omega) (by omega)]; simp [List.getElem?_set, hl2]
    have h27 : l2[7]? = some (.i32v 0) := by
      rw [hrest2 7 (by omega) (by omega)]; simp [List.getElem?_set, hl7]
    have h25 : 5 < l2.length := by simp [hlen2]; omega
    have h26 : 6 < l2.length := by simp [hlen2]; omega
    have h28 : 8 < l2.length := by simp [hlen2]; omega
    have h23' : 3 < l2.length := by simp [hlen2]; omega
    simp [hRun_b, step1, eraseI, wRunF, lg, ls, i32c, h24, h27, h22, hRun_i32Add, hRun_i32Sub,
      hRun_newBytes, List.getElem?_set_ne, List.getElem?_set_self, h25, h26, h28]
    refine ⟨inI32_of (by omega) (by omega), inI32_of (by omega) (by omega), ?_⟩
    obtain ⟨l4, hf4, h48, h47, hlen4⟩ := fillLoop noHost S st n.natAbs
      ((((l2.set 5 (WVal.i32v ((digitsRev n.natAbs).length : Int))).set 8
        (WVal.arr S (List.replicate (digitsRev n.natAbs).length (WVal.i32v 0)))).set 6
        (WVal.i32v (((digitsRev n.natAbs).length : Int) - 1))).set 3 (WVal.i64v n))
      (((digitsRev n.natAbs).length : Int) - 1) 0
      (List.replicate (digitsRev n.natAbs).length (WVal.i32v 0)) hm (by omega) (by omega)
      (by simp) (by omega) (by simp [List.getElem?_set, h23']; exact hab')
      (by simp [List.getElem?_set, h26]) (by simp [List.getElem?_set_ne, h27])
      (by simp [List.getElem?_set, h28])
    rw [hRun_block, hf4 (k + 93) (by omega)]
    have hd : (((digitsRev n.natAbs).length : Int) - 1 + 1).toNat = (digitsRev n.natAbs).length := by
      omega
    simp only [hd, List.take_replicate, List.drop_replicate, Nat.sub_self, List.replicate_zero,
      List.append_nil, Int.toNat_zero, Nat.min_zero, List.nil_append] at h48
    simp [hRun_b, hRun_ifThen, step1, eraseI, wRunF, lg, h47, h48]
    simp [decW, decBytes, hn0, hneg, List.map_reverse]


/-! ## The helper -/

/-- The helper's locals at entry: the argument, then the declared locals at
    their defaults. -/
theorem fromInt_locals (C G S : Nat) (w : WVal) :
    [w] ++ (fromIntCode C G S).toH.locals.map LTy.dflt =
      [w, .i64v 0, .i64v 0, .i64v 0, .i32v 0, .i32v 0, .i32v 0, .i32v 0, .null, .null, .i32v 0,
        .i32v 0, .i64v 0, .i64v 0, .i64v 0, .i32v 0, .null, .i32v 0] := by
  simp [fromIntCode, BCode.toH, BCode.locals, LTy.dflt]

/-- A Small carrier in the `i64` range: the decimal bytes of its Int. -/
theorem fromIntSem_small (C G S : Nat) (n sg : Int) (h0 : -9223372036854775808 ≤ n)
    (h1 : n < 9223372036854775808) :
    fromIntSem C G S [.structv C [.i64v n, .null, .i32v sg]] = some (.arr S (decW n)) := by
  unfold fromIntSem bCall hCall
  rw [fromInt_locals]
  by_cases hn0 : n = 0
  · subst hn0
    simp [fromIntCode, BCode.toH, smallB, hRun_b, hRun_ifElseRef, hRun_newFill, step1, eraseI,
      wRunF, lg, ls, i32c, sget, eqz64, isNull, b32, decW, decBytes]
  · simp [fromIntCode, BCode.toH, smallB, hRun_b, hRun_ifElseRef, step1, eraseI, wRunF, lg, ls,
      sget, eqz64, isNull, b32, hn0]
    obtain ⟨l', hl'⟩ := nonzeroB_run S 246 (by omega)
      [.structv C [.i64v n, .null, .i32v sg], .i64v n, .i64v 0, .i64v 0, .i32v 0, .i32v 0,
        .i32v 0, .i32v 0, .null, .null, .i32v 0, .i32v 0, .i64v 0, .i64v 0, .i64v 0, .i32v 0,
        .null, .i32v 0] [] n hn0 h0 h1 (by simp) (by simp)
    rw [hl']

/-- A Small carrier outside the `i64` range is not a word the module holds;
    the run is stuck on it. -/
theorem fromIntSem_wide (C G S : Nat) (n sg : Int)
    (h : ¬ (-9223372036854775808 ≤ n ∧ n < 9223372036854775808)) :
    fromIntSem C G S [.structv C [.i64v n, .null, .i32v sg]] = none := by
  unfold fromIntSem bCall hCall
  rw [fromInt_locals]
  have hn0 : n ≠ 0 := by omega
  have hi : inI64 n = false := by
    cases hb : inI64 n
    · rfl
    · exact absurd ((inI64_iff n).1 hb) h
  by_cases hneg : n < 0
  · simp [fromIntCode, BCode.toH, smallB, nonzeroB, hRun_b, hRun_ifElseRef, hRun_ifElseI32,
      hRun_ifElse, hRun_i64Sub, step1, eraseI, wRunF, lg, ls, i64c, i32c, sget, eqz64, ltS64,
      isNull, b32, hn0, hneg, hi]
  · simp [fromIntCode, BCode.toH, smallB, nonzeroB, hRun_b, hRun_ifElseRef, hRun_ifElseI32,
      hRun_ifElse, hRun_block, hRun_loop, countB, hRun_brIf, hRun_i32Add, hRun_i64DivU, step1,
      eraseI, wRunF, lg, ls, i64c, i32c, sget, eqz64, ltS64, isNull, b32, hn0, hneg, hi]

/-- A Big carrier: the Big branch is not run. -/
theorem fromIntSem_big (C G S : Nat) (s : Int) (lty : Nat) (les : List WVal) (sg : Int) :
    fromIntSem C G S [.structv C [.i64v s, .arr lty les, .i32v sg]] = none := by
  unfold fromIntSem bCall hCall
  rw [fromInt_locals]
  simp [fromIntCode, BCode.toH, hRun_b, hRun_ifElseRef, hRun_dead, step1, eraseI, wRunF, lg,
    sget, isNull, b32]

/-- What the helper returns on a represented Int: its decimal bytes. A Big
    Int, and a Small word outside the `i64` range, make it return nothing. -/
theorem fromIntSem_spec {C : Nat} (Sp : AverCert.Schema.CarrierSpec C) (G S : Nat) (n : Int)
    (w v : WVal) (hw : Sp.Repr n w) (h : fromIntSem C G S [w] = some v) :
    v = .arr S ((decBytes n).map fun (b : Nat) => .i32v (b : Int)) := by
  rcases Sp.car n w hw with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · have hs := Sp.smallElim n s sg hw
    subst hs
    by_cases hr : -9223372036854775808 ≤ s ∧ s < 9223372036854775808
    · rw [fromIntSem_small C G S s sg hr.1 hr.2] at h
      exact (Option.some.inj h).symm
    · rw [fromIntSem_wide C G S s sg hr] at h
      cases h
  · rw [fromIntSem_big] at h
    cases h

/-! Sanity: the run on concrete words. -/

example : fromIntSem 2 1 0 [.structv 2 [.i64v (-907), .null, .i32v 0]] =
    some (.arr 0 [.i32v 45, .i32v 57, .i32v 48, .i32v 55]) := by
  rw [fromIntSem_small 2 1 0 (-907) 0 (by omega) (by omega)]
  simp [decW, decBytes, digitsRev]

example : fromIntSem 2 1 0 [.structv 2 [.i64v 0, .null, .i32v 0]] = some (.arr 0 [.i32v 48]) := by
  rw [fromIntSem_small 2 1 0 0 0 (by omega) (by omega)]
  simp [decW, decBytes]


end AverCert.StringHelpers
