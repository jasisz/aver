import Bridge.Rel

/-!
# Translation environment

`translate` (Translate.lean) is total on the profile only relative to an
environment: which `call` immediates are host slots (and become IMPORTS of the
synthetic Talos module), and which struct type indices exist with how many
fields of which sort. Both are DECLARED data of the claim — the host-role
table and the certified type declarations — never something read off the
artifact bytes; `envOfClaim` is that projection and `EnvOfClaim.lean` is the
lemma about it.

Talos's `Step` semantics is type-blind for the constructs the profile uses
(`iff` consults only the two arities, `structNew` only the field COUNT and
packedness, `call` to an import only the import's arity), so the sorts here
serve the TYPING judgment of `Translate.lean`, not Talos's own validator.
-/

namespace Bridge
open Wasm CertPrelude
open AverCert.Schema (CarrierSpec)

/-- One import of the synthetic module: the wall's function index the plan
    calls (`call slot`), and the sorts of its arguments and result. -/
structure ImportSig where
  slot : Nat
  params : List STy
  result : STy
deriving Repr, DecidableEq

structure TranslateEnv where
  /-- Position = import index of the synthetic module. -/
  imports : List ImportSig
  /-- Struct type index → field sorts (position irrelevant). -/
  structs : List (Nat × List STy)
  /-- The Int carrier struct index (a struct of sorts `[i64, ref, i32]`). -/
  carrier : Nat
deriving Repr

/-- Resolve a wall function index to its import index and signature. -/
def slotLookup? : List ImportSig → Nat → Option (Nat × ImportSig)
  | [], _ => none
  | s :: rest, f =>
      if s.slot = f then some (0, s)
      else (slotLookup? rest f).map fun p => (p.1 + 1, p.2)

theorem slotLookup?_getElem :
    ∀ {l : List ImportSig} {f i : Nat} {s : ImportSig},
      slotLookup? l f = some (i, s) → l[i]? = some s ∧ s.slot = f
  | [], _, _, _, h => by simp [slotLookup?] at h
  | s' :: rest, f, i, s, h => by
      simp only [slotLookup?] at h
      split at h
      · rename_i heq
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact ⟨rfl, heq⟩
      · match hrec : slotLookup? rest f with
        | none => simp [hrec] at h
        | some (i', s'') =>
          simp only [hrec, Option.map_some, Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          obtain ⟨h1, h2⟩ := slotLookup?_getElem hrec
          exact ⟨by simpa using h1, h2⟩

/-- Field sorts of a declared struct type index. -/
def structSorts? : List (Nat × List STy) → Nat → Option (List STy)
  | [], _ => none
  | (i, fs) :: rest, ty => if i = ty then some fs else structSorts? rest ty

/-- Largest declared struct index (the synthetic type table must reach it). -/
def structsBound : List (Nat × List STy) → Nat
  | [] => 0
  | (i, _) :: rest => max (i + 1) (structsBound rest)

theorem structSorts?_lt_bound :
    ∀ {l : List (Nat × List STy)} {ty : Nat} {fs : List STy},
      structSorts? l ty = some fs → ty < structsBound l
  | [], _, _, h => by simp [structSorts?] at h
  | (i, fs') :: rest, ty, fs, h => by
      simp only [structSorts?] at h
      split at h
      · rename_i heq; subst heq; simp only [structsBound]; omega
      · have := structSorts?_lt_bound h
        simp only [structsBound]
        omega

/-! ## Machine bands -/

def i32Band (n : Int) : Prop := -2147483648 ≤ n ∧ n < 2147483648
def i64Band (n : Int) : Prop := -9223372036854775808 ≤ n ∧ n < 9223372036854775808

instance : DecidablePred i32Band := fun n => by unfold i32Band; infer_instance
instance : DecidablePred i64Band := fun n => by unfold i64Band; infer_instance

theorem i64Band_iff (n : Int) : i64Band n ↔ (-(2 ^ 63 : Int) ≤ n ∧ n < 2 ^ 63) := by
  have h63 : (2 : Int) ^ 63 = 9223372036854775808 := by decide
  rw [h63]
  exact Iff.rfl

/-! ## Value sorts, relative to the declared struct table and a carrier specification

`HasSort env S w t`: `w` is a wall value of sort `t`. A `.ref` struct must be
an instance of a DECLARED struct type with fields of the declared sorts (that
is what lets `structGet` be typed by the table); a `.car` value is a
REPRESENTED, CANONICAL Int carrier under the wall's `CarrierSpec S` — the
operand shape `Obligation.holds`' contracts speak about (the arguments of
`add`/`sub`/`mul`/`cmp`/`eq`); an `.i64b` is a band literal (the argument
shape under which `boxRef` returns a canonical carrier). Arrays are opaque
references (the profile never reads into one). -/

mutual
def HasSort (env : TranslateEnv) (S : CarrierSpec env.carrier) : WVal → STy → Prop
  | .i32v _, .i32 => True
  | .i64v _, .i64 => True
  | .i64v n, .i64b => i64Band n
  | .f64v _, .f64 => True
  | .structv t fs, .ref => ∃ ts, structSorts? env.structs t = some ts ∧ Sorted env S fs ts
  | .arr _ _, .ref => True
  | .null, .ref => True
  | .structv t fs, .car => ∃ n, S.Repr n (.structv t fs) ∧ S.Canon (.structv t fs)
  | _, _ => False

def Sorted (env : TranslateEnv) (S : CarrierSpec env.carrier) : List WVal → List STy → Prop
  | [], [] => True
  | w :: ws, t :: ts => HasSort env S w t ∧ Sorted env S ws ts
  | _, _ => False
end

/-- The carrier's declared layout, as every wall face assumes it. -/
def CarrierDeclared (env : TranslateEnv) : Prop :=
  structSorts? env.structs env.carrier = some [.i64, .ref, .i32]

variable {env : TranslateEnv} {S : CarrierSpec env.carrier}

theorem Sorted_nil : Sorted env S [] [] := by simp [Sorted]

theorem Sorted_cons {w : WVal} {ws : List WVal} {t : STy} {ts : List STy}
    (h : HasSort env S w t) (hs : Sorted env S ws ts) : Sorted env S (w :: ws) (t :: ts) := by
  simp only [Sorted]; exact ⟨h, hs⟩

theorem Sorted_length : ∀ {ws : List WVal} {ts : List STy}, Sorted env S ws ts → ws.length = ts.length
  | [], [], _ => rfl
  | _ :: ws, _ :: ts, h => by
      simp only [Sorted] at h
      simp [Sorted_length h.2]
  | [], _ :: _, h => by simp [Sorted] at h
  | _ :: _, [], h => by simp [Sorted] at h

theorem Sorted_getElem? :
    ∀ {ws : List WVal} {ts : List STy} (i : Nat) (t : STy),
      Sorted env S ws ts → ts[i]? = some t → ∃ w, ws[i]? = some w ∧ HasSort env S w t
  | [], [], i, t, _, ht => by simp at ht
  | w :: ws, t' :: ts, 0, t, h, ht => by
      simp only [Sorted] at h
      simp only [List.getElem?_cons_zero, Option.some.injEq] at ht
      exact ⟨w, rfl, ht ▸ h.1⟩
  | _ :: ws, _ :: ts, i + 1, t, h, ht => by
      simp only [Sorted] at h
      simp only [List.getElem?_cons_succ] at ht ⊢
      exact Sorted_getElem? i t h.2 ht
  | [], _ :: _, _, _, h, _ => by simp [Sorted] at h
  | _ :: _, [], _, _, h, _ => by simp [Sorted] at h

theorem Sorted_set :
    ∀ {ws : List WVal} {ts : List STy} (i : Nat) {w : WVal} {t : STy},
      Sorted env S ws ts → ts[i]? = some t → HasSort env S w t → Sorted env S (ws.set i w) ts
  | [], [], _, _, _, h, _, _ => h
  | _ :: ws, t' :: ts, 0, w, t, h, ht, hw => by
      simp only [Sorted] at h
      simp only [List.getElem?_cons_zero, Option.some.injEq] at ht
      subst ht
      simp only [List.set_cons_zero, Sorted]
      exact ⟨hw, h.2⟩
  | _ :: ws, _ :: ts, i + 1, w, t, h, ht, hw => by
      simp only [Sorted] at h
      simp only [List.getElem?_cons_succ] at ht
      simp only [List.set_cons_succ, Sorted]
      exact ⟨h.1, Sorted_set i h.2 ht hw⟩
  | [], _ :: _, _, _, _, h, _, _ => by simp [Sorted] at h
  | _ :: _, [], _, _, _, h, _, _ => by simp [Sorted] at h

theorem Sorted_append :
    ∀ {ws₁ ws₂ : List WVal} {ts₁ ts₂ : List STy},
      Sorted env S ws₁ ts₁ → Sorted env S ws₂ ts₂ → Sorted env S (ws₁ ++ ws₂) (ts₁ ++ ts₂)
  | [], _, [], _, _, h₂ => h₂
  | _ :: ws₁, _, _ :: ts₁, _, h₁, h₂ => by
      simp only [Sorted] at h₁
      simp only [List.cons_append, Sorted]
      exact ⟨h₁.1, Sorted_append h₁.2 h₂⟩
  | [], _, _ :: _, _, h₁, _ => by simp [Sorted] at h₁
  | _ :: _, _, [], _, h₁, _ => by simp [Sorted] at h₁

theorem Sorted_append_inv :
    ∀ {ws : List WVal} {ts₁ ts₂ : List STy},
      Sorted env S ws (ts₁ ++ ts₂) →
        ∃ ws₁ ws₂, ws = ws₁ ++ ws₂ ∧ Sorted env S ws₁ ts₁ ∧ Sorted env S ws₂ ts₂
  | ws, [], ts₂, h => ⟨[], ws, rfl, Sorted_nil, h⟩
  | [], _ :: _, _, h => by simp [Sorted] at h
  | w :: ws, t :: ts₁, ts₂, h => by
      simp only [List.cons_append, Sorted] at h
      obtain ⟨ws₁, ws₂, rfl, h₁, h₂⟩ := Sorted_append_inv h.2
      exact ⟨w :: ws₁, ws₂, rfl, Sorted_cons h.1 h₁, h₂⟩

theorem Sorted_reverse :
    ∀ {ws : List WVal} {ts : List STy}, Sorted env S ws ts → Sorted env S ws.reverse ts.reverse
  | [], [], _ => Sorted_nil
  | w :: ws, t :: ts, h => by
      simp only [Sorted] at h
      simp only [List.reverse_cons]
      exact Sorted_append (Sorted_reverse h.2) (Sorted_cons h.1 Sorted_nil)
  | [], _ :: _, h => by simp [Sorted] at h
  | _ :: _, [], h => by simp [Sorted] at h

theorem Sorted_singleton_inv {ws : List WVal} {t : STy} (h : Sorted env S ws [t]) :
    ∃ w, ws = [w] ∧ HasSort env S w t := by
  match ws, h with
  | [w], h => exact ⟨w, rfl, by simpa [Sorted] using h⟩
  | [], h => simp [Sorted] at h
  | _ :: _ :: _, h => simp [Sorted] at h

theorem Sorted_pair_inv {ws : List WVal} {t₁ t₂ : STy} (h : Sorted env S ws [t₁, t₂]) :
    ∃ w₁ w₂, ws = [w₁, w₂] ∧ HasSort env S w₁ t₁ ∧ HasSort env S w₂ t₂ := by
  match ws, h with
  | [w₁, w₂], h => exact ⟨w₁, w₂, rfl, by simpa [Sorted] using h⟩
  | [], h => simp [Sorted] at h
  | [_], h => simp [Sorted] at h
  | _ :: _ :: _ :: _, h => simp [Sorted] at h

theorem Sorted_replicate_null (n : Nat) :
    Sorted env S (List.replicate n .null) (List.replicate n .ref) := by
  induction n with
  | zero => exact Sorted_nil
  | succ n ih => simpa [List.replicate_succ, Sorted, HasSort] using ih

/-- A value of reference sort is one of the three reference shapes. -/
theorem HasSort_ref {w : WVal} (h : HasSort env S w .ref) :
    w = .null ∨ (∃ t fs, w = .structv t fs) ∨ (∃ t es, w = .arr t es) := by
  cases w with
  | null => exact Or.inl rfl
  | structv t fs => exact Or.inr (Or.inl ⟨t, fs, rfl⟩)
  | arr t es => exact Or.inr (Or.inr ⟨t, es, rfl⟩)
  | i32v _ => simp [HasSort] at h
  | i64v _ => simp [HasSort] at h
  | f64v _ => simp [HasSort] at h

theorem HasSort_structv {t : Nat} {fs : List WVal} (h : HasSort env S (.structv t fs) .ref) :
    ∃ ts, structSorts? env.structs t = some ts ∧ Sorted env S fs ts := by
  simpa [HasSort] using h

theorem HasSort_i32 {w : WVal} (h : HasSort env S w .i32) : ∃ n, w = .i32v n := by
  cases w <;> simp [HasSort] at h ⊢

theorem HasSort_i64 {w : WVal} (h : HasSort env S w .i64) : ∃ n, w = .i64v n := by
  cases w <;> simp [HasSort] at h ⊢

theorem HasSort_i64b {w : WVal} (h : HasSort env S w .i64b) : ∃ n, w = .i64v n ∧ i64Band n := by
  cases w <;> simp [HasSort] at h ⊢
  exact h

theorem HasSort_isI64 {t : STy} {w : WVal} (ht : IsI64 t) (h : HasSort env S w t) :
    ∃ n, w = .i64v n := by
  rcases ht with rfl | rfl
  · exact HasSort_i64 h
  · obtain ⟨n, rfl, -⟩ := HasSort_i64b h
    exact ⟨n, rfl⟩

theorem HasSort_b32 (p : Bool) : HasSort env S (b32 p) .i32 := by
  cases p <;> simp [b32, HasSort]

theorem HasSort_i64b_of_band {n : Int} (hn : i64Band n) : HasSort env S (.i64v n) .i64b := by
  simpa [HasSort] using hn

/-- A canonical carrier: represented and canonical, hence (by `CarrierSpec.car`)
    a struct at the carrier index in one of the two carrier shapes. -/
theorem HasSort_car {w : WVal} (h : HasSort env S w .car) : ∃ n, S.Repr n w ∧ S.Canon w := by
  cases w <;> simp [HasSort] at h ⊢
  exact h

theorem HasSort_of_canonRepr {w : WVal} {n : Int} (hR : S.Repr n w) (hC : S.Canon w) :
    HasSort env S w .car := by
  rcases S.car n w hR with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩ <;>
    exact ⟨n, hR, hC⟩

theorem HasSort_car_shape {w : WVal} (h : HasSort env S w .car) :
    (∃ s sg, w = .structv env.carrier [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, w = .structv env.carrier [.i64v s, .arr lty les, .i32v sg]) := by
  obtain ⟨n, hR, -⟩ := HasSort_car h
  exact S.car n w hR

/-- Under the carrier's declared layout, a canonical carrier is also a
    `.ref`: its fields are sorted `[i64, ref, i32]`. -/
theorem HasSort_car_ref (hcar : CarrierDeclared env) {w : WVal} (h : HasSort env S w .car) :
    HasSort env S w .ref := by
  rcases HasSort_car_shape h with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩ <;>
    exact ⟨_, hcar, by simp [Sorted, HasSort]⟩

theorem HasSort_sub (hcar : CarrierDeclared env) {t' t : STy} {w : WVal} (hs : SubSort t' t)
    (h : HasSort env S w t') : HasSort env S w t := by
  rcases hs with rfl | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
  · exact h
  · obtain ⟨n, rfl, -⟩ := HasSort_i64b h
    simp [HasSort]
  · exact HasSort_car_ref hcar h

theorem HasSort_isRef (hcar : CarrierDeclared env) {t : STy} {w : WVal} (ht : IsRef t)
    (h : HasSort env S w t) : HasSort env S w .ref := by
  rcases ht with rfl | rfl
  · exact h
  · exact HasSort_car_ref hcar h

/-! ## `envOfClaim` — the projection of the declared envelope

The claim carries `hostTable : List (HostRole × Nat)` (role → resolved wasm
function index) and, for user records, the certified `TypeDecl` (index +
source-order scalar fields, `checkRecordDecl`). The Int carrier is the fixed
`{i64 small, ref null limbs, i32 sign}` layout every wall face assumes
(`CertPrelude.carrierSmall`, `CarrierSpec.car`). -/

open AverCert.Schema in
/-- Argument/result sorts fixed by a host role (`PlanCheck` types the roles
    exactly like this: box `i64 → carrier`, add/sub/mul `carrier² → carrier`,
    cmp/eq `carrier² → i32`, toIndex `carrier → i32`), refined to what the
    wall's contracts need: carrier operands are canonical carriers (`.car`),
    the `box` operand is a band literal (`.i64b`). -/
def roleSig : HostRole → List STy × STy
  | .box => ([.i64b], .car)
  | .add => ([.car, .car], .car)
  | .mul => ([.car, .car], .car)
  | .sub => ([.car, .car], .car)
  | .toIndex => ([.car], .i32)
  | .cmp => ([.car, .car], .i32)
  | .eq => ([.car, .car], .i32)

open AverCert.Schema in
/-- Sort of a stage-1 scalar record field. -/
def scalarSort : TypeDecl → Option STy
  | .intCarrier => some .car
  | .boolScalar => some .i32
  | .floatScalar => some .f64
  | .record _ _ => none
  | .variant _ _ _ => none

open AverCert.Schema in
/-- A certified record declaration as a struct-table entry (fail-closed on
    anything `checkRecordDecl` rejects). -/
def declEntry? : TypeDecl → Option (Nat × List STy)
  | .record idx fields =>
      if checkRecordDecl (.record idx fields) then
        (fields.mapM scalarSort).map fun sorts => (idx, sorts)
      else none
  | _ => none

open AverCert.Schema in
def envOfClaim (hostTable : List (HostRole × Nat)) (carrier : Nat)
    (decls : List TypeDecl) : TranslateEnv :=
  { imports := hostTable.map fun p => { slot := p.2, params := (roleSig p.1).1, result := (roleSig p.1).2 }
    structs := (carrier, [.i64, .ref, .i32]) :: decls.filterMap declEntry?
    carrier }

end Bridge
