import Interpreter.Wasm.SmallStep
import Bridge.AverMin

/-!
# Representation relation between Talos values and the wall's structural `WVal`

Talos: `Value.anyref (some (.struct addr))` is an address into
`Store.gcHeap : List GcObject`; `GcObject.struct typeIdx fields`.
The wall: `WVal.structv tyIdx fields` is the object itself, structurally.

`R heap v w` unfolds the heap along the structure of `w`. Defined by
structural recursion on the (nested) `WVal`; the heap is a parameter, so heap
extension needs an explicit monotonicity lemma (`R_append` / `R_prefix`).
Talos's heap is append-only for the profile (no `struct.set`), which is what
makes the relation stable across a run.

Beyond the value relation (carried over from the probe) this file adds:
* `RLocals` — the LOCALS relation: Talos keeps `params`/`locals` separately,
  the wall keeps one flat list (`initLocals` = args ++ null padding);
* `Locals.get_eq` / `Locals.set?_eq` — the two facts that let a flat index
  drive Talos's split lookup;
* `STy`/`HasSort`/`Sorted` — the value sorts the typing judgment of
  `Translate.lean` speaks about (numeric vs reference); the wall's `wRunF` is
  untyped and Talos is not, so every bridge lemma is stated on sorted stacks.
-/

namespace Bridge
open Wasm CertPrelude

mutual
def R (heap : List GcObject) : Value → WVal → Prop
  | .i64 u, .i64v k => k = u.toInt64.toInt
  | .i32 u, .i32v k => k = u.toInt32.toInt
  | .f64 b, .f64v b' => b = b'
  | .anyref none, .null => True
  | .anyref (some (.struct a)), .structv t fs =>
      ∃ vs, heap[a]? = some (.struct t vs) ∧ Rs heap vs fs
  | .anyref (some (.array a)), .arr t es =>
      ∃ vs, heap[a]? = some (.array t vs) ∧ Rs heap vs es
  | _, _ => False

def Rs (heap : List GcObject) : List Value → List WVal → Prop
  | [], [] => True
  | v :: vs, w :: ws => R heap v w ∧ Rs heap vs ws
  | _, _ => False
end

/-! ## Structural facts about `R`/`Rs` -/

theorem Rs_nil {heap : List GcObject} : Rs heap [] [] := by simp [Rs]

theorem Rs_cons {heap : List GcObject} {v : Value} {vs : List Value} {w : WVal} {ws : List WVal}
    (h : R heap v w) (hs : Rs heap vs ws) : Rs heap (v :: vs) (w :: ws) := by
  simp only [Rs]; exact ⟨h, hs⟩

theorem Rs_length {heap : List GcObject} :
    ∀ {vs : List Value} {ws : List WVal}, Rs heap vs ws → vs.length = ws.length
  | [], [], _ => rfl
  | _ :: vs, _ :: ws, h => by
      simp only [Rs] at h
      simp [Rs_length h.2]
  | [], _ :: _, h => by simp [Rs] at h
  | _ :: _, [], h => by simp [Rs] at h

theorem Rs_getElem? {heap : List GcObject} :
    ∀ {vs : List Value} {ws : List WVal} (i : Nat) (w : WVal),
      Rs heap vs ws → ws[i]? = some w → ∃ v, vs[i]? = some v ∧ R heap v w
  | [], [], i, w, _, hw => by simp at hw
  | v :: vs, w' :: ws, 0, w, h, hw => by
      simp only [Rs] at h
      simp only [List.getElem?_cons_zero, Option.some.injEq] at hw
      exact ⟨v, rfl, hw ▸ h.1⟩
  | _ :: vs, _ :: ws, i + 1, w, h, hw => by
      simp only [Rs] at h
      simp only [List.getElem?_cons_succ] at hw ⊢
      exact Rs_getElem? i w h.2 hw
  | [], _ :: _, _, _, h, _ => by simp [Rs] at h
  | _ :: _, [], _, _, h, _ => by simp [Rs] at h

theorem Rs_take {heap : List GcObject} :
    ∀ (n : Nat) {vs : List Value} {ws : List WVal}, Rs heap vs ws → Rs heap (vs.take n) (ws.take n)
  | 0, _, _, _ => by simp [Rs]
  | _ + 1, [], [], _ => by simp [Rs]
  | n + 1, _ :: vs, _ :: ws, h => by
      simp only [Rs] at h
      simp only [List.take_succ_cons, Rs]
      exact ⟨h.1, Rs_take n h.2⟩
  | _ + 1, [], _ :: _, h => by simp [Rs] at h
  | _ + 1, _ :: _, [], h => by simp [Rs] at h

theorem Rs_drop {heap : List GcObject} :
    ∀ (n : Nat) {vs : List Value} {ws : List WVal}, Rs heap vs ws → Rs heap (vs.drop n) (ws.drop n)
  | 0, _, _, h => h
  | _ + 1, [], [], _ => by simp [Rs]
  | n + 1, _ :: vs, _ :: ws, h => by
      simp only [Rs] at h
      simp only [List.drop_succ_cons]
      exact Rs_drop n h.2
  | _ + 1, [], _ :: _, h => by simp [Rs] at h
  | _ + 1, _ :: _, [], h => by simp [Rs] at h

theorem Rs_append_list {heap : List GcObject} :
    ∀ {vs₁ vs₂ : List Value} {ws₁ ws₂ : List WVal},
      Rs heap vs₁ ws₁ → Rs heap vs₂ ws₂ → Rs heap (vs₁ ++ vs₂) (ws₁ ++ ws₂)
  | [], _, [], _, _, h₂ => h₂
  | _ :: vs₁, _, _ :: ws₁, _, h₁, h₂ => by
      simp only [Rs] at h₁
      simp only [List.cons_append, Rs]
      exact ⟨h₁.1, Rs_append_list h₁.2 h₂⟩
  | [], _, _ :: _, _, h₁, _ => by simp [Rs] at h₁
  | _ :: _, _, [], _, h₁, _ => by simp [Rs] at h₁

theorem Rs_reverse {heap : List GcObject} :
    ∀ {vs : List Value} {ws : List WVal}, Rs heap vs ws → Rs heap vs.reverse ws.reverse
  | [], [], _ => by simp [Rs]
  | v :: vs, w :: ws, h => by
      simp only [Rs] at h
      simp only [List.reverse_cons]
      exact Rs_append_list (Rs_reverse h.2) (by simp [Rs, h.1])
  | [], _ :: _, h => by simp [Rs] at h
  | _ :: _, [], h => by simp [Rs] at h

/-- Splitting a related pair of stacks at the same length. -/
theorem Rs_append_inv {heap : List GcObject} :
    ∀ {vs : List Value} {ws₁ ws₂ : List WVal},
      Rs heap vs (ws₁ ++ ws₂) →
      ∃ vs₁ vs₂, vs = vs₁ ++ vs₂ ∧ Rs heap vs₁ ws₁ ∧ Rs heap vs₂ ws₂
  | vs, [], ws₂, h => ⟨[], vs, rfl, Rs_nil, h⟩
  | [], w :: ws₁, ws₂, h => by simp [Rs] at h
  | v :: vs, w :: ws₁, ws₂, h => by
      simp only [List.cons_append, Rs] at h
      obtain ⟨vs₁, vs₂, rfl, h₁, h₂⟩ := Rs_append_inv h.2
      exact ⟨v :: vs₁, vs₂, rfl, Rs_cons h.1 h₁, h₂⟩

theorem Rs_set {heap : List GcObject} :
    ∀ {vs : List Value} {ws : List WVal} (i : Nat) {v : Value} {w : WVal},
      Rs heap vs ws → R heap v w → Rs heap (vs.set i v) (ws.set i w)
  | [], [], _, _, _, h, _ => h
  | _ :: vs, _ :: ws, 0, v, w, h, hv => by
      simp only [Rs] at h
      simp only [List.set_cons_zero, Rs]
      exact ⟨hv, h.2⟩
  | _ :: vs, _ :: ws, i + 1, v, w, h, hv => by
      simp only [Rs] at h
      simp only [List.set_cons_succ, Rs]
      exact ⟨h.1, Rs_set i h.2 hv⟩
  | [], _ :: _, _, _, _, h, _ => by simp [Rs] at h
  | _ :: _, [], _, _, _, h, _ => by simp [Rs] at h

/-! Heap extension preserves the relation (the heap is append-only in Talos). -/
mutual
theorem R_append (heap more : List GcObject) :
    ∀ (v : Value) (w : WVal), R heap v w → R (heap ++ more) v w
  | .i64 _, .i64v _, h => h
  | .i32 _, .i32v _, h => h
  | .f64 _, .f64v _, h => h
  | .anyref none, .null, h => h
  | .anyref (some (.struct a)), .structv t fs, h => by
      simp only [R] at h ⊢
      obtain ⟨vs, hget, hvs⟩ := h
      refine ⟨vs, ?_, Rs_append heap more vs fs hvs⟩
      rw [List.getElem?_append_left (List.getElem?_eq_some_iff.mp hget).1]
      exact hget
  | .anyref (some (.array a)), .arr t es, h => by
      simp only [R] at h ⊢
      obtain ⟨vs, hget, hvs⟩ := h
      refine ⟨vs, ?_, Rs_append heap more vs es hvs⟩
      rw [List.getElem?_append_left (List.getElem?_eq_some_iff.mp hget).1]
      exact hget
  | .i64 _, .i32v _, h => by simp [R] at h
  | .i64 _, .f64v _, h => by simp [R] at h
  | .i64 _, .structv _ _, h => by simp [R] at h
  | .i64 _, .arr _ _, h => by simp [R] at h
  | .i64 _, .null, h => by simp [R] at h
  | .i32 _, .i64v _, h => by simp [R] at h
  | .i32 _, .f64v _, h => by simp [R] at h
  | .i32 _, .structv _ _, h => by simp [R] at h
  | .i32 _, .arr _ _, h => by simp [R] at h
  | .i32 _, .null, h => by simp [R] at h
  | .f32 _, _, h => by simp [R] at h
  | .f64 _, .i64v _, h => by simp [R] at h
  | .f64 _, .i32v _, h => by simp [R] at h
  | .f64 _, .structv _ _, h => by simp [R] at h
  | .f64 _, .arr _ _, h => by simp [R] at h
  | .f64 _, .null, h => by simp [R] at h
  | .funcref _, _, h => by simp [R] at h
  | .externref _, _, h => by simp [R] at h
  | .v128 _, _, h => by simp [R] at h
  | .exnref _, _, h => by simp [R] at h
  | .anyref none, .i64v _, h => by simp [R] at h
  | .anyref none, .i32v _, h => by simp [R] at h
  | .anyref none, .f64v _, h => by simp [R] at h
  | .anyref none, .structv _ _, h => by simp [R] at h
  | .anyref none, .arr _ _, h => by simp [R] at h
  | .anyref (some (.i31 _)), _, h => by simp [R] at h
  | .anyref (some (.host _)), _, h => by simp [R] at h
  | .anyref (some (.struct _)), .i64v _, h => by simp [R] at h
  | .anyref (some (.struct _)), .i32v _, h => by simp [R] at h
  | .anyref (some (.struct _)), .f64v _, h => by simp [R] at h
  | .anyref (some (.struct _)), .arr _ _, h => by simp [R] at h
  | .anyref (some (.struct _)), .null, h => by simp [R] at h
  | .anyref (some (.array _)), .i64v _, h => by simp [R] at h
  | .anyref (some (.array _)), .i32v _, h => by simp [R] at h
  | .anyref (some (.array _)), .f64v _, h => by simp [R] at h
  | .anyref (some (.array _)), .structv _ _, h => by simp [R] at h
  | .anyref (some (.array _)), .null, h => by simp [R] at h

theorem Rs_append (heap more : List GcObject) :
    ∀ (vs : List Value) (ws : List WVal), Rs heap vs ws → Rs (heap ++ more) vs ws
  | [], [], h => h
  | v :: vs, w :: ws, h => by
      simp only [Rs] at h ⊢
      exact ⟨R_append heap more v w h.1, Rs_append heap more vs ws h.2⟩
  | [], _ :: _, h => by simp [Rs] at h
  | _ :: _, [], h => by simp [Rs] at h
end

theorem R_prefix {heap heap' : List GcObject} (h : heap <+: heap') {v : Value} {w : WVal}
    (hR : R heap v w) : R heap' v w := by
  obtain ⟨more, rfl⟩ := h
  exact R_append heap more v w hR

theorem Rs_prefix {heap heap' : List GcObject} (h : heap <+: heap') {vs : List Value}
    {ws : List WVal} (hR : Rs heap vs ws) : Rs heap' vs ws := by
  obtain ⟨more, rfl⟩ := h
  exact Rs_append heap more vs ws hR

/-- Inversion: a Talos value related to a wall struct is a live struct reference
    whose heap object has the same type index and related fields. -/
theorem R_structv {heap : List GcObject} {v : Value} {t : Nat} {fs : List WVal}
    (h : R heap v (.structv t fs)) :
    ∃ a vs, v = .anyref (some (.struct a)) ∧ heap[a]? = some (.struct t vs) ∧ Rs heap vs fs := by
  cases v with
  | anyref r =>
    cases r with
    | none => simp [R] at h
    | some r =>
      cases r with
      | struct a =>
        simp only [R] at h
        obtain ⟨vs, hget, hvs⟩ := h
        exact ⟨a, vs, rfl, hget, hvs⟩
      | i31 _ => simp [R] at h
      | array _ => simp [R] at h
      | host _ => simp [R] at h
  | i32 _ => simp [R] at h
  | i64 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | f64 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

theorem R_arr {heap : List GcObject} {v : Value} {t : Nat} {es : List WVal}
    (h : R heap v (.arr t es)) :
    ∃ a vs, v = .anyref (some (.array a)) ∧ heap[a]? = some (.array t vs) ∧ Rs heap vs es := by
  cases v with
  | anyref r =>
    cases r with
    | none => simp [R] at h
    | some r =>
      cases r with
      | array a =>
        simp only [R] at h
        obtain ⟨vs, hget, hvs⟩ := h
        exact ⟨a, vs, rfl, hget, hvs⟩
      | i31 _ => simp [R] at h
      | struct _ => simp [R] at h
      | host _ => simp [R] at h
  | i32 _ => simp [R] at h
  | i64 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | f64 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

theorem R_null {heap : List GcObject} {v : Value} (h : R heap v .null) : v = .anyref none := by
  cases v with
  | anyref r =>
    cases r with
    | none => rfl
    | some r => cases r <;> simp [R] at h
  | i32 _ => simp [R] at h
  | i64 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | f64 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

theorem R_i64v {heap : List GcObject} {v : Value} {k : Int} (h : R heap v (.i64v k)) :
    ∃ u : UInt64, v = .i64 u ∧ k = u.toInt64.toInt := by
  cases v with
  | i64 u => exact ⟨u, rfl, h⟩
  | anyref r =>
    cases r with
    | none => simp [R] at h
    | some r => cases r <;> simp [R] at h
  | i32 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | f64 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

theorem R_i32v {heap : List GcObject} {v : Value} {k : Int} (h : R heap v (.i32v k)) :
    ∃ u : UInt32, v = .i32 u ∧ k = u.toInt32.toInt := by
  cases v with
  | i32 u => exact ⟨u, rfl, h⟩
  | anyref r =>
    cases r with
    | none => simp [R] at h
    | some r => cases r <;> simp [R] at h
  | i64 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | f64 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

/-- The wall's `b32` is represented by Talos's `.i32 (if p then 1 else 0)`. -/
theorem R_b32 {heap : List GcObject} (p q : Prop) [Decidable p] [Decidable q] (hpq : p ↔ q) :
    R heap (.i32 (if q then 1 else 0)) (b32 (decide p)) := by
  by_cases hp : p
  · have hq : q := hpq.mp hp
    simp [b32, hp, hq, R]
  · have hq : ¬ q := fun h => hp (hpq.mpr h)
    simp [b32, hp, hq, R]

/-! ## Locals

Talos splits a frame into `params`/`locals`/`values`; the wall keeps one flat
locals list (`initLocals c args = args ++ replicate c.nlocals null`). The
relation is pointwise on the concatenation. Talos's default locals are the
zero of their declared type; the configuration (`Config.lean`) declares every
non-parameter local as a nullable reference, whose zero is `.anyref none`,
the one Talos value `.null` relates to. -/

def RLocals (heap : List GcObject) (L : Locals) (flat : List WVal) : Prop :=
  Rs heap (L.params ++ L.locals) flat

theorem Locals.get_eq (L : Locals) (i : Nat) : L.get i = (L.params ++ L.locals)[i]? := by
  simp only [Locals.get]
  by_cases h1 : i < L.params.length
  · rw [if_pos h1, List.getElem?_append_left h1]
  · rw [if_neg h1]
    by_cases h2 : i < L.params.length + L.locals.length
    · rw [if_pos h2, List.getElem?_append_right (Nat.le_of_not_lt h1)]
    · rw [if_neg h2, eq_comm, List.getElem?_eq_none_iff]
      simp only [List.length_append]
      omega

theorem Locals.set?_eq (L : Locals) (i : Nat) (v : Value)
    (hi : i < (L.params ++ L.locals).length) :
    ∃ L' : Locals, L.set? i v = some L' ∧
      L'.params ++ L'.locals = (L.params ++ L.locals).set i v ∧ L'.values = L.values := by
  simp only [Locals.set?]
  by_cases h1 : i < L.params.length
  · refine ⟨{ L with params := L.params.set i v }, by rw [if_pos h1], ?_, rfl⟩
    rw [List.set_append_left _ _ h1]
  · rw [if_neg h1]
    have h2 : i < L.params.length + L.locals.length := by
      simpa [List.length_append] using hi
    refine ⟨{ L with locals := L.locals.set (i - L.params.length) v }, by rw [if_pos h2], ?_, rfl⟩
    rw [List.set_append_right _ _ (Nat.le_of_not_lt h1)]

/-! ## Value sorts

The wall's interpreter is untyped: `refIsNull` accepts any non-null value,
`localSet` past the end is a silent no-op, and an `if` branch may consume the
stack beneath it. Talos refuses each of those. The bridge therefore states
its lemmas on SORTED stacks: a sort is the coarse wasm value class the
profile distinguishes. The sort relation itself is indexed by the declared
struct table and lives in `Env.lean`. -/

inductive STy where
  | i32
  | i64
  /-- An `i64` known to be a machine-band LITERAL (`i64Const`): the one
      argument shape under which the wall's `boxRef` returns a canonical
      carrier (`CarrierSpec.canonSmall`). -/
  | i64b
  | f64
  /-- Any reference: `null`, an array, or a struct of a DECLARED type whose
      fields are sorted by the declared table. -/
  | ref
  /-- A represented, canonical Int carrier (`CarrierSpec.Repr` and `Canon`):
      the operand shape the wall's host contracts speak about. -/
  | car
deriving DecidableEq, Repr

/-- Sort inclusion: a band literal is an `i64`, a canonical carrier is a
    reference (its fields are sorted by the carrier's declared layout). -/
def SubSort (t' t : STy) : Prop := t' = t ∨ (t' = .i64b ∧ t = .i64) ∨ (t' = .car ∧ t = .ref)

instance : DecidableRel SubSort := fun _ _ => by unfold SubSort; infer_instance

theorem SubSort.refl (t : STy) : SubSort t t := Or.inl rfl

/-- The two `i64` sorts (both comparison operands). -/
def IsI64 (t : STy) : Prop := t = .i64 ∨ t = .i64b

/-- The two reference sorts (both `structGet` receivers). -/
def IsRef (t : STy) : Prop := t = .ref ∨ t = .car

end Bridge
