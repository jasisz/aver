-- The module bytes as fixed-width chunks, read through windows.
import ByteWindow

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleBytes
open CertDecode AverCert.ByteWindow

/-! ### Chunked bytes

The checker renders the module as a list of `1024`-byte chunks, chunk 0 the
lowest bytes. `join` is the numeral they denote; a check that reads one entry
reads it through `window`, which joins only the chunks the entry touches, so
its cost does not depend on the size of the module. `window_eq` shows that
the window is the same slice of the whole numeral. -/

/-- Little-endian join of `w`-byte chunks: chunk 0 in the lowest bytes. -/
def join (w : Nat) : List Nat → Nat
  | [] => 0
  | c :: cs => c + (join w cs <<< (8 * w))

theorem join_cons (w c : Nat) (cs : List Nat) : join w (c :: cs) = c + 2 ^ (8 * w) * join w cs := by
  rw [join, Nat.shiftLeft_eq, Nat.mul_comm]

theorem pow_mul_succ (w n : Nat) : 2 ^ (8 * w * (n + 1)) = 2 ^ (8 * w) * 2 ^ (8 * w * n) := by
  rw [Nat.mul_succ, Nat.pow_add, Nat.mul_comm]

theorem join_append (w : Nat) : ∀ (xs ys : List Nat),
    join w (xs ++ ys) = join w xs + 2 ^ (8 * w * xs.length) * join w ys
  | [], ys => by simp [join]
  | x :: xs, ys => by
      rw [List.cons_append, join_cons, join_cons, join_append w xs ys, List.length_cons,
        pow_mul_succ, Nat.mul_add, Nat.add_assoc, Nat.mul_assoc (2 ^ (8 * w))]

/-- Every chunk fits its width. -/
def chunksFit (w : Nat) (cs : List Nat) : Bool := cs.all (· < 2 ^ (8 * w))

theorem join_lt (w : Nat) : ∀ (cs : List Nat), chunksFit w cs = true →
    join w cs < 2 ^ (8 * w * cs.length)
  | [], _ => by simp [join]
  | c :: cs, h => by
      simp only [chunksFit, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
      have ih := join_lt w cs h.2
      rw [join_cons, List.length_cons, pow_mul_succ]
      have hj : join w cs + 1 ≤ 2 ^ (8 * w * cs.length) := ih
      calc c + 2 ^ (8 * w) * join w cs
          < 2 ^ (8 * w) + 2 ^ (8 * w) * join w cs := Nat.add_lt_add_right h.1 _
        _ = 2 ^ (8 * w) * (join w cs + 1) := by rw [Nat.mul_add, Nat.mul_one, Nat.add_comm]
        _ ≤ 2 ^ (8 * w) * 2 ^ (8 * w * cs.length) := Nat.mul_le_mul_left _ hj

theorem chunksFit_take {w : Nat} {cs : List Nat} (h : chunksFit w cs = true) (i : Nat) :
    chunksFit w (cs.take i) = true := by
  simp only [chunksFit, List.all_eq_true, decide_eq_true_eq] at h ⊢
  intro x hx; exact h x (List.mem_of_mem_take hx)

/-- A join balanced by halves, `join` in value (`joinTree_eq`). The kernel's
    intermediate numerals then add up to the module's size once per level,
    instead of once per chunk. -/
def joinTree (w : Nat) : Nat → List Nat → Nat
  | 0, cs => join w cs
  | d + 1, cs =>
      if cs.length / 2 == 0 then join w cs
      else joinTree w d (cs.take (cs.length / 2)) +
        (joinTree w d (cs.drop (cs.length / 2)) <<< (8 * w * (cs.length / 2)))

theorem joinTree_eq (w : Nat) : ∀ (d : Nat) (cs : List Nat), joinTree w d cs = join w cs
  | 0, _ => rfl
  | d + 1, cs => by
      unfold joinTree
      split
      · rfl
      · rename_i hh
        have hlen : (cs.take (cs.length / 2)).length = cs.length / 2 := by
          simp only [List.length_take]; omega
        rw [joinTree_eq w d, joinTree_eq w d, Nat.shiftLeft_eq, Nat.mul_comm _ (2 ^ _)]
        conv => rhs; rw [← List.take_append_drop (cs.length / 2) cs]
        rw [join_append, hlen]

/-! ### Slices and windows -/

/-- The `len` bytes at byte offset `off`. -/
def slice (n off len : Nat) : Nat := (n >>> (8 * off)) % 2 ^ (8 * len)

theorem slice_lt (n off len : Nat) : slice n off len < 2 ^ (8 * len) :=
  Nat.mod_lt _ (Nat.two_pow_pos _)

theorem slice_eq_isolate (n off len : Nat) :
    slice n off len = isolateBytes (n >>> (8 * off)) len := by
  rw [isolateBytes_eq]; rfl

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

/-- The numeral from byte `off` on: the slice of its first `l` bytes, then
    the rest. -/
theorem shr_split (n off l : Nat) :
    n >>> (8 * off) = slice n off l + 2 ^ (8 * l) * (n >>> (8 * (off + l))) := by
  unfold slice
  rw [Nat.mul_add, Nat.shiftRight_add]
  exact (split_pow _ l).symm

/-- A slice of `l + m` bytes is its first `l` bytes, then the next `m`. -/
theorem slice_split (n a l m : Nat) :
    slice n a (l + m) = slice n a l + 2 ^ (8 * l) * slice n (a + l) m := by
  unfold slice
  have h := split_pow ((n >>> (8 * a)) % 2 ^ (8 * (l + m))) l
  rw [mod_mod_pow (by omega), mod_shr_pow (by omega), Nat.add_sub_cancel_left,
    ← Nat.shiftRight_add, ← Nat.mul_add] at h
  exact h.symm

/-- The rest of a slice after its first `k` bytes. -/
theorem slice_shr (n a l k : Nat) (h : k ≤ l) :
    slice n a l >>> (8 * k) = slice n (a + k) (l - k) := by
  unfold slice
  rw [mod_shr_pow h, ← Nat.shiftRight_add, ← Nat.mul_add]

/-- The first `l` bytes of a slice. -/
theorem slice_mod (n a l L : Nat) (h : l ≤ L) :
    slice n a L % 2 ^ (8 * l) = slice n a l := by
  unfold slice; exact mod_mod_pow h

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

/-- The window of whole chunks that covers `[off, off + len)`. -/
def window (w : Nat) (cs : List Nat) (off len : Nat) : Nat :=
  slice (join w ((cs.drop (off / w)).take ((off % w + len + w - 1) / w))) (off % w) len

/-- A window is the slice of the whole join, wherever it lies: past the last
    chunk both are zero. -/
theorem window_eq {w : Nat} {cs : List Nat} (hw : 0 < w) (hfit : chunksFit w cs = true)
    (off len : Nat) : window w cs off len = slice (join w cs) off len := by
  unfold window
  have hoff : off = w * (off / w) + off % w := (Nat.div_add_mod off w).symm
  by_cases hin : off / w ≤ cs.length
  · conv => rhs; rw [hoff]
    rw [slice_drop hfit _ _ _ hin]
    symm
    apply slice_take
    have := Nat.div_add_mod (off % w + len + w - 1) w
    have hm := Nat.mod_lt (off % w + len + w - 1) hw
    omega
  · rw [List.drop_of_length_le (by omega)]
    have hz : slice (join w ([] : List Nat)) (off % w) len = 0 := by
      simp [slice, join]
    simp only [List.take_nil] at hz ⊢
    rw [hz]
    have hlt := join_lt w cs hfit
    have hle : 8 * w * cs.length ≤ 8 * off := by
      have : w * cs.length ≤ off := by
        have := Nat.div_mul_le_self off w
        have h2 : w * cs.length ≤ w * (off / w) := Nat.mul_le_mul_left w (by omega)
        rw [Nat.mul_comm] at this; omega
      rw [Nat.mul_assoc]; exact Nat.mul_le_mul_left 8 this
    unfold slice
    rw [Nat.shiftRight_eq_div_pow,
      Nat.div_eq_of_lt (Nat.lt_of_lt_of_le hlt (Nat.pow_le_pow_right (by decide) hle)),
      Nat.zero_mod]

/-! ### Packed byte lists -/

/-- Little-endian numeral of a byte list. -/
def pack : List Nat → Nat
  | [] => 0
  | b :: bs => b + 256 * pack bs

def bytesOk (bs : List Nat) : Bool := bs.all (· < 256)

theorem pack_append : ∀ (xs ys : List Nat),
    pack (xs ++ ys) = pack xs + 2 ^ (8 * xs.length) * pack ys
  | [], ys => by simp [pack]
  | x :: xs, ys => by
      simp only [List.cons_append, pack, List.length_cons, pack_append xs ys]
      rw [show 8 * (xs.length + 1) = 8 + 8 * xs.length by omega, Nat.pow_add, Nat.mul_add,
        ← Nat.mul_assoc, Nat.add_assoc]

/-- `pack` a limb of `k` bytes at a time: every limb is packed alone, and the
    limbs are joined, so the kernel multiplies one numeral per limb instead of
    one per byte (`packBy_eq`). The fuel bounds the number of limbs. -/
def packBy (k : Nat) : Nat → List Nat → Nat
  | 0, _ => 0
  | f + 1, bs =>
      match bs with
      | [] => 0
      | _ :: _ => pack (bs.take k) + (packBy k f (bs.drop k) <<< (8 * k))

theorem packBy_eq {k : Nat} (hk : 0 < k) : ∀ (f : Nat) (bs : List Nat), bs.length ≤ f →
    packBy k f bs = pack bs
  | 0, [], _ => rfl
  | 0, _ :: _, h => by simp at h
  | f + 1, [], _ => rfl
  | f + 1, b :: bs, h => by
      simp only [packBy]
      rw [packBy_eq hk f _ (by simp only [List.length_drop, List.length_cons] at h ⊢; omega),
        Nat.shiftLeft_eq, Nat.mul_comm]
      by_cases hl : k ≤ (b :: bs).length
      · have ht : ((b :: bs).take k).length = k := by simp only [List.length_take]; omega
        conv => rhs; rw [← List.take_append_drop k (b :: bs)]
        rw [pack_append, ht]
      · rw [List.drop_of_length_le (by omega), List.take_of_length_le (by omega)]
        simp [pack]

theorem takeBytes_pack : ∀ (bs : List Nat), bytesOk bs = true →
    takeBytes bs.length (pack bs) = bs
  | [], _ => rfl
  | b :: bs, h => by
      simp only [bytesOk, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
      have ih := takeBytes_pack bs (by simpa [bytesOk] using h.2)
      simp only [List.length_cons, takeBytes, pack]
      have h1 : (b + 256 * pack bs) &&& 0xff = b := by
        rw [land_ff, Nat.add_mul_mod_self_left, Nat.mod_eq_of_lt h.1]
      have h2 : (b + 256 * pack bs) >>> 8 = pack bs := by
        rw [Nat.shiftRight_eq_div_pow, show (2 : Nat) ^ 8 = 256 from rfl,
          Nat.add_mul_div_left _ _ (by decide), Nat.div_eq_of_lt h.1, Nat.zero_add]
      rw [h1, h2, ih]

/-- The bytes of a slice, when it is the packed form of a byte list of its
    length. -/
theorem takeBytes_slice {n off : Nat} {bs : List Nat} (hok : bytesOk bs = true)
    (heq : slice n off bs.length = pack bs) :
    takeBytes bs.length (isolateBytes (n >>> (8 * off)) bs.length) = bs := by
  rw [← slice_eq_isolate, heq, takeBytes_pack bs hok]

end AverCert.ScaleBytes
