/-
ACCEPTANCE-SOUNDNESS CORE.

The list and lookup facts the discharge needs, stated over the real audited
definitions: a planned function is found at its own index when indices are
unique; the host table resolves every role to its own function and every
planned index to nothing; the one group model over all plans agrees with a
group's model over the rest; and the two facts `plansAccepted` supplies to the
proof (distinct indices, typed plans).
-/
import AcceptedArtifactCore

open AverCert
open AverCert.Schema
open AverCert.Grammar
open AverCert.TypeTable
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

/-- Per-obligation denotation selected by policy: the body of `HoldsCore`. -/
def obligationHolds (o : Obligation) : Prop :=
  match o.policy with
  | .simulatesModel => o.holds
  | .simulatesModelTotally => o.holdsTotal

theorem holdsCore_iff (m : Manifest) :
    HoldsCore m ↔ ∀ o ∈ m.obligations, obligationHolds o := by
  constructor <;> intro h o ho <;> exact h o ho

/-! ### Unique keys -/

theorem find?_key_nodup {α : Type} (key : α → Nat) :
    ∀ {l : List α}, (l.map key).Nodup → ∀ {x : α}, x ∈ l →
      l.find? (fun y => key y == key x) = some x
  | [], _, _, hx => by simp at hx
  | a :: t, hnd, x, hx => by
      simp only [List.map_cons, List.nodup_cons] at hnd
      rcases List.mem_cons.mp hx with rfl | hxt
      · simp
      · have hne : key a ≠ key x := by
          intro h
          exact hnd.1 (by rw [h]; exact List.mem_map_of_mem hxt)
        have hb : (key a == key x) = false := by simpa using hne
        rw [List.find?_cons, hb]
        exact find?_key_nodup key hnd.2 hxt

theorem entryOf_mem {fns : List FnEntry} (hnd : (fns.map (·.funcIdx)).Nodup)
    {e : FnEntry} (he : e ∈ fns) : entryOf fns e.funcIdx = some e := by
  show fns.find? (fun y => y.funcIdx == e.funcIdx) = some e
  exact find?_key_nodup (fun y => y.funcIdx) hnd he

theorem planOf_mem {fns : List FnEntry} (hnd : (fns.map (·.funcIdx)).Nodup)
    {e : FnEntry} (he : e ∈ fns) : planOf fns e.funcIdx = some e.plan := by
  simp [planOf, entryOf_mem hnd he]

theorem planOf_some {fns : List FnEntry} {f : Nat} {p : FnPlan} (h : planOf fns f = some p) :
    ∃ e ∈ fns, e.funcIdx = f ∧ e.plan = p := by
  unfold planOf entryOf at h
  cases hf : fns.find? (·.funcIdx == f) with
  | none => rw [hf] at h; cases h
  | some e =>
      rw [hf] at h
      simp only [Option.map_some, Option.some.injEq] at h
      have hk := List.find?_some hf
      simp only [beq_iff_eq] at hk
      exact ⟨e, List.mem_of_find?_eq_some hf, hk, h⟩

/-! ### The host table -/

theorem lookup_of_nodup {β : Type} :
    ∀ {l : List (Nat × β)}, (l.map (·.1)).Nodup → ∀ {k : Nat} {v : β}, (k, v) ∈ l →
      l.lookup k = some v
  | [], _, _, _, h => by simp at h
  | (a, b) :: t, hnd, k, v, h => by
      simp only [List.map_cons, List.nodup_cons] at hnd
      rcases List.mem_cons.mp h with he | ht
      · cases he
        simp [List.lookup]
      · have hne : k ≠ a := by
          intro hk
          subst hk
          exact hnd.1 (List.mem_map_of_mem (f := (·.1)) ht)
        have hb : (k == a) = false := by simpa using hne
        simp only [List.lookup, hb]
        exact lookup_of_nodup hnd.2 ht

theorem lookup_none {β : Type} :
    ∀ {l : List (Nat × β)} {k : Nat}, k ∉ l.map (·.1) → l.lookup k = none
  | [], _, _ => rfl
  | (a, b) :: t, k, h => by
      simp only [List.map_cons, List.mem_cons, not_or] at h
      have hb : (k == a) = false := by simpa using h.1
      simp only [List.lookup, hb]
      exact lookup_none h.2

theorem hostAssoc_keys (M : MCtx) (h : HostFns) :
    (hostAssoc M h).map (·.1) = roleIndices M := rfl

/-- Under distinct role indices, the host table resolves every role to its own
    function; under disjointness, every planned index to nothing. -/
theorem host_facts {M : MCtx} {fns : List FnEntry} (h : HostFns)
    (hd : (roleIndices M ++ fns.map (·.funcIdx)).Nodup) :
    hostOf M h M.box = some (1, boxRef M.carrier) ∧
    hostOf M h M.add = some (2, h.add) ∧
    hostOf M h M.sub = some (2, h.sub) ∧
    hostOf M h M.mul = some (2, h.mul) ∧
    hostOf M h M.neg = some (1, fun _ => none) ∧
    hostOf M h M.cmp = some (2, h.cmp) ∧
    hostOf M h M.eq = some (2, h.eq) ∧
    hostOf M h M.concat = some (1, h.stringConcat M.str) ∧
    hostOf M h M.streq = some (2, h.stringEq) ∧
    hostOf M h M.toIndex = some (1, h.toIndex) ∧
    hostOf M h M.divmod = some (3, h.divmod) ∧
    ∀ e ∈ fns, hostOf M h e.funcIdx = none := by
  have hkeys : ((hostAssoc M h).map (·.1)).Nodup := by
    rw [hostAssoc_keys]
    exact (List.nodup_append.mp hd).1
  have L : ∀ {k v}, (k, v) ∈ hostAssoc M h → hostOf M h k = some v :=
    fun hm => lookup_of_nodup hkeys hm
  refine ⟨L (by simp [hostAssoc]), L (by simp [hostAssoc]), L (by simp [hostAssoc]),
    L (by simp [hostAssoc]), L (by simp [hostAssoc]), L (by simp [hostAssoc]),
    L (by simp [hostAssoc]), L (by simp [hostAssoc]), L (by simp [hostAssoc]),
    L (by simp [hostAssoc]), L (by simp [hostAssoc]), ?_⟩
  intro e he
  apply lookup_none
  rw [hostAssoc_keys]
  intro hr
  have hdis := (List.nodup_append.mp hd).2.2
  exact hdis _ hr _ (List.mem_map_of_mem he) rfl

/-! ### One model over all plans -/

/-- A group's model over the one model of all plans IS that model, when the
    group's plans are plans of the artifact. -/
theorem groupModel_restrict (P G : Nat → Option FnPlan)
    (hGP : ∀ f p, G f = some p → P f = some p) :
    ∀ k f args, groupModel (groupModel (fun _ _ _ => none) P) G k f args =
      groupModel (fun _ _ _ => none) P k f args := by
  intro k
  induction k with
  | zero =>
      intro f args
      cases hG : G f with
      | none => simp [groupModel, hG]
      | some p => simp [groupModel, hG, hGP f p hG]
  | succ k ih =>
      intro f args
      have hfun : groupModel (groupModel (fun _ _ _ => none) P) G k =
          groupModel (fun _ _ _ => none) P k := funext fun g => funext fun a => ih g a
      cases hG : G f with
      | none => simp [groupModel, hG]
      | some p => simp [groupModel, hG, hGP f p hG, hfun]

/-! ### What `plansAccepted` supplies to the proof -/

structure PlanFacts (s : Subject) (tt : TypeTable) (fns : List FnEntry) : Prop where
  distinct : (roleIndices (mctxOf s tt fns) ++ fns.map (·.funcIdx)).Nodup
  typed : ∀ e ∈ fns, planTyped (mctxOf s tt fns) e.plan = true
  cons : ∀ t f, (mctxOf s tt fns).listCons t = some f →
    ∃ p, planOf fns f = some p ∧ isConsPlan p = true

theorem PlanFacts.nodup {s : Subject} {tt : TypeTable} {fns : List FnEntry}
    (hf : PlanFacts s tt fns) : (fns.map (·.funcIdx)).Nodup :=
  (List.nodup_append.mp hf.distinct).2.1

theorem planFacts_of_accepted (artifact : ArtifactData) (h : plansAccepted artifact = true) :
    PlanFacts artifact.manifest.subject artifact.manifest.types artifact.manifest.fnPlans := by
  simp only [plansAccepted, consPinned, Bool.and_eq_true, List.all_eq_true] at h
  obtain ⟨⟨⟨⟨⟨⟨hd, he⟩, _⟩, _⟩, _⟩, _⟩, hc⟩ := h
  refine ⟨of_decide_eq_true hd, fun e hm => ?_, fun t f hf => ?_⟩
  · have := he e hm
    simp only [entryAccepted, Bool.and_eq_true] at this
    exact this.1.1
  · simp only [mctxOf, Option.map_eq_some_iff] at hf
    obtain ⟨x, hx, rfl⟩ := hf
    have hmem := hc x (List.mem_of_find?_eq_some hx)
    revert hmem
    split
    · rename_i p hp
      intro hmem
      exact ⟨p, hp, hmem⟩
    · intro hmem
      cases hmem

end AcceptanceSoundness
