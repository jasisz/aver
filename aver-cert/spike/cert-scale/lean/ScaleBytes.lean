/-
  ScaleBytes (cert-scale spike): the module bytes as a list of fixed-width
  chunks, read through windows, and code entries compared as packed numerals.

  Nothing here changes what acceptance says. `join w cs` IS the module numeral
  (the checker renders it that way); every lemma below shows that a cheap
  reading equals the reading the wall's decoders already use.
-/
import DeclaredLayout
import ByteWindow

namespace AverCert.ScaleBytes
open CertDecode

/-! ### Chunked module bytes -/

/-- Little-endian join of `w`-byte chunks: chunk 0 in the lowest bytes. -/
def join (w : Nat) : List Nat → Nat
  | [] => 0
  | c :: cs => c + 2 ^ (8 * w) * join w cs

theorem pow_mul_succ (w n : Nat) : 2 ^ (8 * w * (n + 1)) = 2 ^ (8 * w) * 2 ^ (8 * w * n) := by
  rw [Nat.mul_succ, Nat.pow_add, Nat.mul_comm]

theorem join_append (w : Nat) : ∀ (xs ys : List Nat),
    join w (xs ++ ys) = join w xs + 2 ^ (8 * w * xs.length) * join w ys
  | [], ys => by simp [join]
  | x :: xs, ys => by
      simp only [List.cons_append, join, List.length_cons]
      rw [join_append w xs ys, pow_mul_succ, Nat.mul_add, Nat.add_assoc, Nat.mul_assoc (2 ^ (8 * w))]

/-- Every chunk fits its width. -/
def chunksFit (w : Nat) (cs : List Nat) : Bool := cs.all (· < 2 ^ (8 * w))

theorem join_lt (w : Nat) : ∀ (cs : List Nat), chunksFit w cs = true →
    join w cs < 2 ^ (8 * w * cs.length)
  | [], _ => by simp [join]
  | c :: cs, h => by
      simp only [chunksFit, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
      have ih := join_lt w cs h.2
      simp only [join, List.length_cons, pow_mul_succ]
      -- c + B * j < B * P  from  c < B,  j < P  (so j + 1 ≤ P)
      have hj : join w cs + 1 ≤ 2 ^ (8 * w * cs.length) := ih
      calc c + 2 ^ (8 * w) * join w cs
          < 2 ^ (8 * w) + 2 ^ (8 * w) * join w cs := Nat.add_lt_add_right h.1 _
        _ = 2 ^ (8 * w) * (join w cs + 1) := by rw [Nat.mul_add, Nat.mul_one, Nat.add_comm]
        _ ≤ 2 ^ (8 * w) * 2 ^ (8 * w * cs.length) := Nat.mul_le_mul_left _ hj

theorem chunksFit_take {w : Nat} {cs : List Nat} (h : chunksFit w cs = true) (i : Nat) :
    chunksFit w (cs.take i) = true := by
  simp only [chunksFit, List.all_eq_true, decide_eq_true_eq] at h ⊢
  intro x hx; exact h x (List.mem_of_mem_take hx)

/-- The `len` bytes at byte offset `off`. -/
def slice (n off len : Nat) : Nat := (n >>> (8 * off)) % 2 ^ (8 * len)

theorem shr_add_mul {a b k : Nat} (ha : a < 2 ^ k) (s : Nat) :
    (a + 2 ^ k * b) >>> (k + s) = b >>> s := by
  rw [Nat.shiftRight_eq_div_pow, Nat.shiftRight_eq_div_pow, Nat.pow_add, ← Nat.div_div_eq_div_mul,
    Nat.add_mul_div_left _ _ (Nat.two_pow_pos k), Nat.div_eq_of_lt ha, Nat.zero_add]

theorem mod_add_mul {a b k m : Nat} (hm : m ≤ k) :
    (a + 2 ^ k * b) % 2 ^ m = a % 2 ^ m := by
  obtain ⟨d, rfl⟩ := Nat.exists_eq_add_of_le hm
  rw [Nat.pow_add, Nat.mul_assoc, Nat.add_mul_mod_self_left]

theorem shr_mod {x s m : Nat} : (x % 2 ^ (s + m)) >>> s % 2 ^ m = x >>> s % 2 ^ m := by
  rw [Nat.shiftRight_eq_div_pow, Nat.shiftRight_eq_div_pow, Nat.pow_add, Nat.mod_mul_right_div_self,
    Nat.mod_mod_of_dvd _ (Nat.dvd_refl _)]

/-- The window of whole chunks that covers `[off, off + len)`. -/
def window (w : Nat) (cs : List Nat) (off len : Nat) : Nat :=
  let i := off / w
  let r := off % w
  slice (join w ((cs.drop i).take ((r + len + w - 1) / w))) r len

theorem slice_drop {w : Nat} {cs : List Nat} (hfit : chunksFit w cs = true) (i r len : Nat)
    (hi : i ≤ cs.length) :
    slice (join w cs) (w * i + r) len = slice (join w (cs.drop i)) r len := by
  conv => lhs; rw [← List.take_append_drop i cs]
  rw [join_append]
  have hlt := join_lt w (cs.take i) (chunksFit_take hfit i)
  have hl : (cs.take i).length = i := by simp [List.length_take, Nat.min_eq_left hi]
  rw [hl] at hlt ⊢
  unfold slice
  rw [show 8 * (w * i + r) = 8 * w * i + 8 * r by rw [Nat.mul_add, Nat.mul_assoc],
    shr_add_mul hlt]

theorem slice_take {w : Nat} (cs : List Nat) (off len j : Nat) (h : off + len ≤ w * j) :
    slice (join w cs) off len = slice (join w (cs.take j)) off len := by
  by_cases hj : j ≤ cs.length
  · conv => lhs; rw [← List.take_append_drop j cs]
    rw [join_append]
    have hl : (cs.take j).length = j := by simp [List.length_take, Nat.min_eq_left hj]
    rw [hl]
    unfold slice
    have hm : 8 * off + 8 * len ≤ 8 * w * j := by
      rw [Nat.mul_assoc, ← Nat.mul_add]; exact Nat.mul_le_mul_left 8 h
    rw [← shr_mod (s := 8 * off) (m := 8 * len), ← shr_mod (x := join w (cs.take j)) (s := 8 * off)
      (m := 8 * len), mod_add_mul hm]
  · rw [List.take_of_length_le (by omega)]

theorem window_eq {w : Nat} {cs : List Nat} (hw : 0 < w) (hfit : chunksFit w cs = true)
    (off len : Nat) (hin : off / w ≤ cs.length) :
    window w cs off len = slice (join w cs) off len := by
  unfold window
  simp only
  have hoff : off = w * (off / w) + off % w := (Nat.div_add_mod off w).symm
  conv => rhs; rw [hoff]
  rw [slice_drop hfit _ _ _ hin]
  symm
  apply slice_take
  -- off % w + len ≤ w * ((off % w + len + w - 1) / w)
  have := Nat.lt_mul_div_succ (off % w + len + w - 1) hw
  have h2 := Nat.div_mul_le_self (off % w + len + w - 1) w
  -- ceiling division bound
  have key : off % w + len ≤ w * ((off % w + len + w - 1) / w) := by
    have := Nat.div_add_mod (off % w + len + w - 1) w
    have hm := Nat.mod_lt (off % w + len + w - 1) hw
    omega
  exact key

/-! ### Packed byte comparison -/

/-- Little-endian numeral of a byte list. -/
def pack : List Nat → Nat
  | [] => 0
  | b :: bs => b + 256 * pack bs

def bytesOk (bs : List Nat) : Bool := bs.all (· < 256)

theorem takeBytes_pack : ∀ (bs : List Nat), bytesOk bs = true →
    takeBytes bs.length (pack bs) = bs
  | [], _ => rfl
  | b :: bs, h => by
      simp only [bytesOk, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
      have ih := takeBytes_pack bs (by simpa [bytesOk] using h.2)
      simp only [List.length_cons, takeBytes, pack]
      have h1 : (b + 256 * pack bs) &&& 0xff = b := by
        rw [AverCert.ByteWindow.land_ff, Nat.add_mul_mod_self_left, Nat.mod_eq_of_lt h.1]
      have h2 : (b + 256 * pack bs) >>> 8 = pack bs := by
        rw [Nat.shiftRight_eq_div_pow, show (2 : Nat) ^ 8 = 256 from rfl,
          Nat.add_mul_div_left _ _ (by decide), Nat.div_eq_of_lt h.1, Nat.zero_add]
      rw [h1, h2, ih]

theorem pack_lt : ∀ (bs : List Nat), bytesOk bs = true → pack bs < 2 ^ (8 * bs.length)
  | [], _ => by simp [pack]
  | b :: bs, h => by
      simp only [bytesOk, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
      have ih := pack_lt bs (by simpa [bytesOk] using h.2)
      simp only [pack, List.length_cons]
      rw [show 8 * (bs.length + 1) = 8 * bs.length + 8 by omega, Nat.pow_add]
      have : pack bs + 1 ≤ 2 ^ (8 * bs.length) := ih
      calc b + 256 * pack bs < 256 + 256 * pack bs := Nat.add_lt_add_right h.1 _
        _ = 256 * (pack bs + 1) := by rw [Nat.mul_add, Nat.mul_one, Nat.add_comm]
        _ ≤ 256 * 2 ^ (8 * bs.length) := Nat.mul_le_mul_left _ this
        _ = 2 ^ (8 * bs.length) * 2 ^ 8 := by rw [Nat.mul_comm]

/-- The layout's code entry `k` is `bs` when the packed window equals `pack bs`. -/
theorem entry_of_packed {n : Nat} {L : AverCert.DeclaredLayout.Layout} {k : Nat} {bs : List Nat}
    (hok : bytesOk bs = true) (hlen : bs.length = L.len k)
    (heq : slice n (L.off k) (L.len k) = pack bs) : L.entry n k = bs := by
  unfold AverCert.DeclaredLayout.Layout.entry AverCert.DeclaredLayout.Layout.entryN
  rw [AverCert.ByteWindow.isolateBytes_eq]
  have : n >>> (8 * L.off k) % 2 ^ (8 * L.len k) = pack bs := heq
  rw [this, ← hlen, takeBytes_pack bs hok]

/-! ### The per-function code check -/

open AverCert.Grammar AverCert.DeclaredLayout in
/-- Planned function `f` with plan `p`: its lowering, packed, is the chunk
    window of its declared code entry. Reads only the chunks the entry
    touches, whatever the module's size. -/
def codePacked (cs : List Nat) (L : Layout) (M : MCtx) (f : Nat) (p : FnPlan) : Bool :=
  decide (L.imports ≤ f) && decide (f - L.imports < L.count) &&
  decide (L.off (f - L.imports) / 1024 ≤ cs.length) &&
  match codeEntryBytes M p with
  | some bs => bytesOk bs && bs.length == L.len (f - L.imports) &&
      window 1024 cs (L.off (f - L.imports)) (L.len (f - L.imports)) == pack bs
  | none => false

open AverCert.Grammar AverCert.DeclaredLayout in
/-- The code-entry conjunct of today's `entryFast`, for comparison. -/
def codeOld (n : Nat) (L : Layout) (M : MCtx) (f : Nat) (p : FnPlan) : Bool :=
  decide (L.imports ≤ f) && decide (f - L.imports < L.count) &&
  match codeEntryBytes M p with
  | some bs => L.entry n (f - L.imports) == bs
  | none => false

open AverCert.Grammar AverCert.DeclaredLayout in
/-- The packed check implies the check today's wall decides. -/
theorem codeOld_of_packed {cs : List Nat} {L : Layout} {M : MCtx} {f : Nat} {p : FnPlan}
    (hfit : chunksFit 1024 cs = true) (h : codePacked cs L M f p = true) :
    codeOld (join 1024 cs) L M f p = true := by
  unfold codePacked at h
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨hlo, hhi⟩, hin⟩, hm⟩ := h
  rw [codeOld, decide_eq_true hlo, decide_eq_true hhi, Bool.true_and, Bool.true_and]
  split at hm
  · rename_i bs hbs
    simp only [Bool.and_eq_true, beq_iff_eq] at hm
    obtain ⟨⟨hok, hlen⟩, hwin⟩ := hm
    rw [window_eq (by decide) hfit _ _ hin] at hwin
    simp [entry_of_packed hok hlen hwin]
  · cases hm

end AverCert.ScaleBytes
