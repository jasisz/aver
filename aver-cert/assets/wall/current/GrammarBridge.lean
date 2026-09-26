/- GrammarBridge — plan-equals-source bridges on the one grammar.

   A certified export's obligation is stated over its plan: the model is the
   plan's fuel-indexed meaning (`AcceptedArtifact.modelOf`). A BRIDGE says
   that this model computes a named source function `f`, read through
   source-value encoders. The checker renders every bridge statement from
   structure (`aver-cert/src/bridge_statement.rs`) in one of two kinds, both
   defined here:

   * `Exact` — for a plan whose call closure has no recursion: above some
     fuel, the model at every encoded argument list returns exactly the
     encoded source result.
   * `Adequate` — for any plan, recursive or not: every result the model
     returns (at any fuel) on an encoded argument list is the encoded source
     result. Together with the obligation's `holds` this is the L1 meaning
     "whatever the bytes return on represented source arguments represents
     `f` of those arguments" (`adequate_transfer`). It says nothing about
     termination: a model that never returns satisfies it vacuously, and a
     bridge is never read as a totality claim.

   Both kinds also state that every encoded argument list inhabits the plan's
   parameter types, so the obligation's `holds` applies to it.

   The proof engines are generic in the plan. The producer supplies a source
   IMAGE table `I` (per bridged function, the encoded source result at the
   argument lists its decoder recognises) and ONE step lemma per function
   (`Step`): the plan body, with every call answered by the callees' images,
   returns the function's own image. That lemma unfolds the source function
   once; it never inducts. `bridge_of_step` turns the step lemmas of a call
   closure into adequacy at every fuel by one fuel induction for the whole
   closure (self and mutual recursion alike), using that evaluation is
   monotone in the callee table (`eval_mono`). `exact_of_step` turns them
   into exact answers above a declared call depth, for a closure without
   recursion. -/
import AcceptedArtifactCore

namespace AverCert.GrammarBridge
open AverCert.Schema AverCert.Grammar AverCert.TypeTable AverCert.AcceptedArtifact CertPrelude

/-! ## The statement vocabulary -/

/-- Export `name`'s obligation: the first obligation of the manifest carrying
    that export name (`HoldsCore` covers every member, so the choice among
    duplicates is immaterial). -/
def exportObligation (m : Manifest) (name : String) : Option Obligation :=
  m.obligations.find? (fun o => o.export_ == name)

theorem exportObligation_mem {m : Manifest} {name : String} {o : Obligation}
    (h : exportObligation m name = some o) : o ∈ m.obligations :=
  List.mem_of_find?_eq_some h

/-! Selecting an export's obligation without evaluating String equality.

The kernel has no fast path for String literals: deciding `a == b` rebuilds
both UTF-8 byte arrays, in time quadratic in their length. Deciding
`exportObligation` directly therefore compares the wanted name with every
earlier export name. The lemmas below select it from pairwise-distinct names
instead: the names are shown distinct ONCE per package, as character lists
(a literal is definitionally `String.ofList` of its characters, which the
kernel checks without building bytes), and each export's obligation then
follows from membership and one literal-to-literal name equality. -/

theorem find?_export_of_nodup {os : List Obligation} {name : String} {o : Obligation}
    (hnd : (os.map (·.export_)).Nodup) (hmem : o ∈ os) (hname : o.export_ = name) :
    os.find? (fun o => o.export_ == name) = some o := by
  induction os with
  | nil => cases hmem
  | cons a rest ih =>
      rw [List.map_cons, List.nodup_cons] at hnd
      rcases List.mem_cons.mp hmem with rfl | hrest
      · simp [hname]
      · have hne : ¬ a.export_ = name := fun h =>
          hnd.1 (List.mem_map.mpr ⟨o, hrest, hname.trans h.symm⟩)
        simp [hne, ih hnd.2 hrest]

/-- One number per code-point list: base `2^21` digits `c + 1`. A decided
    `Nodup` over these numbers compares one numeral per pair; it needs no
    injectivity, since distinct images already have distinct preimages. -/
def natOfCodes : List Nat → Nat
  | [] => 0
  | c :: cs => (c + 1) + 2097152 * natOfCodes cs

/-- Pairwise-distinct names, from pairwise-distinct character lists, decided
    on one number per list. -/
theorem names_nodup_of_chars {names : List String} (cs : List (List Char))
    (h : names = cs.map String.ofList)
    (hnd : (cs.map (fun c => natOfCodes (c.map Char.toNat))).Nodup) : names.Nodup := by
  subst h
  have hcs : cs.Nodup :=
    List.Pairwise.of_map (fun c => natOfCodes (c.map Char.toNat))
      (fun a b hab heq => hab (heq ▸ rfl)) hnd
  exact List.Pairwise.map String.ofList (fun a b hab heq =>
    hab (by simpa [String.toList_ofList] using congrArg String.toList heq)) hcs

/-- The obligation of a planned, exported entry, when the manifest's export
    names are pairwise distinct. -/
theorem exportObligation_of_entry {m : Manifest} {s : Subject} {tt : TypeTable}
    {fns : List FnEntry} (hm : m.obligations = obligationsOf s tt fns)
    (hnd : (m.obligations.map (·.export_)).Nodup) {e : FnEntry} (he : e ∈ fns)
    (hex : e.exported = true) :
    exportObligation m e.name = some (obligationOf s tt fns e) := by
  unfold exportObligation
  refine find?_export_of_nodup hnd ?_ rfl
  rw [hm]
  exact List.mem_map.mpr ⟨e, List.mem_filter.mpr ⟨he, hex⟩, rfl⟩

/-- An argument list inhabits the obligation's parameter types. -/
def ArgsTyped (o : Obligation) (args : List SVal) : Prop :=
  HasTyL o.layout args o.sig.params

/-- The adequate kind, over one argument binder `α` (a rendered statement
    spells the same proposition with one binder per parameter). -/
def Adequate (m : Manifest) (name : String) {α : Type} (args : α → List SVal)
    (res : α → SVal) : Prop :=
  ∃ o, exportObligation m name = some o ∧ (∀ x, ArgsTyped o (args x)) ∧
    ∀ fuel x v, o.model fuel (args x) = some v → v = res x

/-- The exact kind. -/
def Exact (m : Manifest) (name : String) {α : Type} (args : α → List SVal)
    (res : α → SVal) : Prop :=
  ∃ o, exportObligation m name = some o ∧ (∀ x, ArgsTyped o (args x)) ∧
    ∃ k, ∀ fuel, k ≤ fuel → ∀ x, o.model fuel (args x) = some (res x)

/-! ## String values

A String is represented by its UTF-8 bytes (`SVal.s`); `strBytes` is that
encoding, the one the plan's String literals, concatenation and equality
read. It is injective, so `decodeStr` — the String whose encoding a value is,
by choice — inverts it exactly on encoded values. A bridge matches a String
argument as a whole value and decodes it with `decodeStr`. -/

/-- The UTF-8 bytes of a String. -/
def strBytes (s : String) : List Nat := s.toByteArray.data.toList.map UInt8.toNat

theorem strBytes_inj {a b : String} (h : strBytes a = strBytes b) : a = b := by
  unfold strBytes at h
  have hl : a.toByteArray.data.toList = b.toByteArray.data.toList := by
    have := congrArg (List.map UInt8.ofNat) h
    simpa [List.map_map, Function.comp_def, UInt8.ofNat_toNat] using this
  have hd : a.toByteArray.data = b.toByteArray.data := Array.ext' hl
  have hb : a.toByteArray = b.toByteArray := by
    cases ha : a.toByteArray
    cases hb' : b.toByteArray
    rw [ha, hb'] at hd
    simp only [ByteArray.mk.injEq]
    exact hd
  exact String.toByteArray_inj.mp hb

theorem strBytes_append (a b : String) : strBytes (a ++ b) = strBytes a ++ strBytes b := by
  unfold strBytes
  rw [String.toByteArray_append, ByteArray.data_append, Array.toList_append, List.map_append]

/-- The same, for a model that spells concatenation as `+` through an
    `HAdd String String String` instance whose operation is `String.append`. -/
theorem strBytes_hadd (a b : String) :
    strBytes (@HAdd.hAdd String String String ⟨String.append⟩ a b) = strBytes a ++ strBytes b :=
  strBytes_append a b

/-- A model's String interpolation renders a String part through `toString`,
    which is the identity on Strings. -/
theorem strBytes_toString (s : String) : strBytes (toString s) = strBytes s := rfl

theorem string_eq_iff (a b : String) : a = b ↔ strBytes a = strBytes b :=
  ⟨fun h => h ▸ rfl, strBytes_inj⟩

theorem string_beq (a b : String) : (a == b) = (strBytes a == strBytes b) := by
  rw [Bool.eq_iff_iff, beq_iff_eq, beq_iff_eq]
  exact string_eq_iff a b

/-- The String a value encodes, if any. -/
noncomputable def decodeStr (v : SVal) : Option String := by
  classical
  exact if h : ∃ s, SVal.s (strBytes s) = v then some (Classical.choose h) else none

theorem decodeStr_eq_some {v : SVal} {s : String} :
    decodeStr v = some s ↔ v = SVal.s (strBytes s) := by
  classical
  unfold decodeStr
  split
  · rename_i hx
    constructor
    · intro h
      have hc := Classical.choose_spec hx
      simp only [Option.some.injEq] at h
      rw [← hc, h]
    · intro h
      have hc : SVal.s (strBytes (Classical.choose hx)) = SVal.s (strBytes s) :=
        (Classical.choose_spec hx).trans h
      simp only [SVal.s.injEq] at hc
      simp only [Option.some.injEq]
      exact strBytes_inj hc
  · rename_i hx
    constructor
    · intro h; cases h
    · intro h; exact absurd ⟨s, h.symm⟩ hx

theorem decodeStr_strBytes (s : String) : decodeStr (SVal.s (strBytes s)) = some s :=
  decodeStr_eq_some.mpr rfl

/-! ## What an adequate bridge means for the bytes -/

theorem holds_of_mem {m : Manifest} (hm : HoldsCore m) {o : Obligation}
    (ho : o ∈ m.obligations) : o.holds := by
  have h := hm o ho
  cases hp : o.policy <;> rw [hp] at h
  · exact h
  · exact h.1

/-- The bytes compute the source function: under the named runtime
    contracts, a run of the export's emitted function on a represented
    encoded argument list that returns, returns a represented, well-typed
    image of the source result. -/
theorem adequate_transfer {m : Manifest} (hm : HoldsCore m) {name : String} {α : Type}
    {args : α → List SVal} {res : α → SVal} (hb : Adequate m name args res) :
    ∃ o, exportObligation m name = some o ∧
      ∀ (S : CarrierSpec o.carrier) (h : HostFns), HostContracts S h →
        ∀ fuel x ws r, SReprL S o.layout (args x) ws →
          wFuncN o.code (o.host h) fuel o.self ws = some r →
          SRepr S o.layout (res x) r ∧ HasTy o.layout (res x) o.sig.ret := by
  obtain ⟨o, ho, hty, hadq⟩ := hb
  refine ⟨o, ho, ?_⟩
  intro S h hc fuel x ws r hrep hrun
  obtain ⟨sv, hmod, hrep', hty'⟩ :=
    holds_of_mem hm (exportObligation_mem ho) S h hc fuel (args x) ws r (hty x) hrep hrun
  have := hadq fuel x sv hmod
  subst this
  exact ⟨hrep', hty'⟩

/-! ## The plan model, one step at a time -/

theorem modelOf_zero (fns : List FnEntry) (g : Nat) (a : List SVal) :
    modelOf fns 0 g a = none := by
  simp only [modelOf, groupModel]
  split <;> rfl

theorem modelOf_succ (fns : List FnEntry) (k g : Nat) (a : List SVal) {p : FnPlan}
    (h : planOf fns g = some p) :
    modelOf fns (k + 1) g a = eval (modelOf fns k) (argsEnv a) p.body := by
  simp only [modelOf, groupModel, h]

/-! ## Callee tables and monotonicity -/

/-- A callee table: the meaning of each function index at an argument list. -/
abbrev Table := Nat → List SVal → Option SVal

/-- `G` extends `F`: every answer of `F` is an answer of `G`. -/
def Le (F G : Table) : Prop := ∀ g a v, F g a = some v → G g a = some v

mutual
theorem eval_mono {F G : Nat → List SVal → Option SVal} (hle : Le F G) :
    ∀ (env : Nat → Option SVal) (e : Expr) (v : SVal), eval F env e = some v → eval G env e = some v
  | env, .literal l, v, h => by cases l <;> simpa [eval] using h
  | env, .local i, v, h => by simpa [eval] using h
  | env, .let_ b x body, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i x' hx
        rw [eval_mono hle env x x' hx]
        exact eval_mono hle _ body v h
      · cases h
  | env, .call (.fn f) args, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs
        rw [evalArgs_mono hle env args vs hvs]
        exact hle f vs v h
      · cases h
  | env, .call (.builtin b) args, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs
        rw [evalArgs_mono hle env args vs hvs]
        exact h
      · cases h
  | env, .call (.intrinsic i) args, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs
        rw [evalArgs_mono hle env args vs hvs]
        exact h
      · cases h
  | env, .call (.lazy b) [o, d], v, h => by
      simp only [eval] at h ⊢
      split at h <;> rename_i heq <;>
        first
        | (rw [eval_mono hle env o _ heq]; first | exact h | exact eval_mono hle env d v h)
        | cases h
  | env, .call (.lazy b) [], v, h => by simp [eval] at h
  | env, .call (.lazy b) [_], v, h => by simp [eval] at h
  | env, .call (.lazy b) (_ :: _ :: _ :: _), v, h => by simp [eval] at h
  | env, .tailCall f args, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs
        rw [evalArgs_mono hle env args vs hvs]
        exact hle f vs v h
      · cases h
  | env, .binOp op l r, v, h => by
      simp only [eval] at h ⊢
      split at h <;> rename_i hl hr <;>
        first
        | (rw [eval_mono hle env l _ hl, eval_mono hle env r _ hr]; exact h)
        | cases h
  | env, .neg e, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i x hx; rw [eval_mono hle env e _ hx]; exact h
      · cases h
  | env, .ifThenElse c t e, v, h => by
      simp only [eval] at h ⊢
      split at h <;> rename_i hc <;>
        first
        | (rw [eval_mono hle env c _ hc]; first | exact eval_mono hle env t v h | exact eval_mono hle env e v h)
        | cases h
  | env, .recordCreate tid fs, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs; rw [evalArgs_mono hle env fs vs hvs]; exact h
      · cases h
  | env, .project tid i base, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i t fs hb; rw [eval_mono hle env base _ hb]; exact h
      · cases h
  | env, .match_ s arms, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i x hx; rw [eval_mono hle env s x hx]; exact evalArms_mono hle env x arms v h
      · cases h
  | env, .construct c ty args, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs; rw [evalArgs_mono hle env args vs hvs]; exact h
      · cases h
  | env, .interp parts, v, h => by
      simp only [eval] at h ⊢
      split at h
      · rename_i vs hvs; rw [evalArgs_mono hle env parts vs hvs]; exact h
      · cases h
  | env, .list t items, v, h => by
      simp only [eval] at h ⊢
      by_cases he : items.isEmpty = true
      · simp only [he, ↓reduceIte] at h ⊢
        exact h
      · simp only [he, Bool.false_eq_true, ↓reduceIte] at h ⊢
        split at h
        · rename_i vs hvs; rw [evalArgs_mono hle env items vs hvs]; exact h
        · cases h
theorem evalArgs_mono {F G : Nat → List SVal → Option SVal} (hle : Le F G) :
    ∀ (env : Nat → Option SVal) (es : List Expr) (vs : List SVal),
      evalArgs F env es = some vs → evalArgs G env es = some vs
  | env, [], vs, h => by simpa [evalArgs] using h
  | env, e :: es, vs, h => by
      simp only [evalArgs] at h ⊢
      split at h
      · rename_i x xs hx hxs
        rw [eval_mono hle env e x hx, evalArgs_mono hle env es xs hxs]; exact h
      · cases h
theorem evalArms_mono {F G : Nat → List SVal → Option SVal} (hle : Le F G) :
    ∀ (env : Nat → Option SVal) (sv : SVal) (arms : Arms) (v : SVal),
      evalArms F env sv arms = some v → evalArms G env sv arms = some v
  | env, sv, .nil, v, h => by simp [evalArms] at h
  | env, sv, .cons p b rest, v, h => by
      simp only [evalArms] at h ⊢
      split at h
      · split at h
        · exact eval_mono hle _ b v h
        · cases h
      · exact evalArms_mono hle env sv rest v h
end


/-! ## Source images and one-step obligations

A source IMAGE `I` gives, for each bridged function index, the encoded
source result at the argument lists it recognises (`none` elsewhere); the
producer builds it from pattern-matching decoders and the transpiled source
functions. `over I Cs F` answers a call to a function of `Cs` with its image
where the image is defined, and with `F` everywhere else. -/

def over (I : Table) (Cs : List Nat) (F : Table) : Table := fun g a =>
  if g ∈ Cs then
    match I g a with
    | some w => some w
    | none => F g a
  else F g a

theorem over_of_image {I : Table} {Cs : List Nat} {F : Table} {g : Nat} {a : List SVal} {w : SVal}
    (hg : g ∈ Cs) (hw : I g a = some w) : over I Cs F g a = some w := by
  simp [over, hg, hw]

/-- The one-step obligation of function `g` over its direct callees `Cs`:
    whatever the other functions answer, its plan body, with every call to
    `Cs` answered by the image, returns `g`'s image wherever that image is
    defined. This is the only per-function proof a bridge needs; it unfolds
    the source function once and never inducts. -/
def Step (fns : List FnEntry) (I : Table) (Cs : List Nat) (g : Nat) : Prop :=
  ∃ p, planOf fns g = some p ∧
    ∀ (F : Table) (a : List SVal) (w : SVal), I g a = some w →
      eval (over I Cs F) (argsEnv a) p.body = some w

/-- Every answer of `F` at an index of `D` agrees with the image where the
    image is defined. -/
def SoundOn (I : Table) (D : List Nat) (F : Table) : Prop :=
  ∀ g ∈ D, ∀ a v w, F g a = some v → I g a = some w → v = w

/-- At an index of `D`, `F` answers the image wherever the image is defined. -/
def ExactOn (I : Table) (D : List Nat) (F : Table) : Prop :=
  ∀ g ∈ D, ∀ a w, I g a = some w → F g a = some w

theorem le_over {I : Table} {Cs : List Nat} {F : Table} (h : SoundOn I Cs F) :
    Le F (over I Cs F) := by
  intro g a v hv
  unfold over
  split
  · rename_i hg
    cases hI : I g a with
    | none => simpa using hv
    | some w =>
        have := h g hg a v w hv hI
        subst this
        rfl
  · exact hv

theorem over_eq {I : Table} {Cs : List Nat} {F : Table} (h : ExactOn I Cs F) :
    over I Cs F = F := by
  funext g a
  unfold over
  split
  · rename_i hg
    cases hI : I g a with
    | none => rfl
    | some w => exact (h g hg a w hI).symm
  · rfl

/-- `bridge_of_step`, adequate form: one-step obligations for every function
    of `D` (each over callees inside `D`) make the plan model sound for the
    image at every fuel, for all of `D` at once. One fuel induction covers
    self recursion and mutual recursion alike. -/
theorem bridge_of_step (fns : List FnEntry) (I : Table) (D : List Nat)
    (hstep : ∀ g ∈ D, ∃ Cs, (∀ c ∈ Cs, c ∈ D) ∧ Step fns I Cs g) :
    ∀ fuel, SoundOn I D (modelOf fns fuel) := by
  intro fuel
  induction fuel with
  | zero =>
      intro g _ a v w h
      rw [modelOf_zero] at h
      cases h
  | succ k ih =>
      intro g hg a v w h hw
      obtain ⟨Cs, hcs, p, hp, hs⟩ := hstep g hg
      rw [modelOf_succ fns k g a hp] at h
      have hsound : SoundOn I Cs (modelOf fns k) := fun c hc => ih c (hcs c hc)
      have hmono := eval_mono (le_over hsound) (argsEnv a) p.body v h
      rw [hs (modelOf fns k) a w hw] at hmono
      exact (Option.some.inj hmono).symm

/-- `bridge_of_step`, exact form: over an acyclic call order (a declared
    `depth` that every call strictly decreases), the plan model at any fuel
    above a function's depth answers exactly its image. -/
theorem exact_of_step (fns : List FnEntry) (I : Table) (D : List Nat) (depth : Nat → Nat)
    (hstep : ∀ g ∈ D, ∃ Cs, (∀ c ∈ Cs, c ∈ D ∧ depth c < depth g) ∧ Step fns I Cs g) :
    ∀ fuel, ∀ g ∈ D, depth g < fuel → ∀ a w, I g a = some w → modelOf fns fuel g a = some w := by
  intro fuel
  induction fuel with
  | zero => intro g _ h; omega
  | succ k ih =>
      intro g hg hd a w hw
      obtain ⟨Cs, hcs, p, hp, hs⟩ := hstep g hg
      rw [modelOf_succ fns k g a hp]
      have hexact : ExactOn I Cs (modelOf fns k) := by
        intro c hc b u hu
        obtain ⟨hcD, hlt⟩ := hcs c hc
        exact ih c hcD (by omega) b u hu
      rw [← over_eq hexact]
      exact hs (modelOf fns k) a w hw

end AverCert.GrammarBridge
