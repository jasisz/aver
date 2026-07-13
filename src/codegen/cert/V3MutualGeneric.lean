/- V3 MUTUAL-RECURSION FAMILY — a k-generic conjunction layer over the
   completed unary recursion/fuel skeleton.

   A member is a byte-bound unary countdown body.  Its step tail-calls the
   byte-bound member at `cross`.  `Fin k` makes the proof motive the finite
   conjunction of all k members; the sole fuel induction cites the matching
   conjunct at fuel-1. -/
import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower
import AcceptedArtifactCore

set_option linter.unusedSimpArgs false
set_option linter.unusedVariables false
set_option maxRecDepth 1000000

namespace V3Mutual
open CertPrelude AverCert AverCert.Schema AverCert.PlanLower

structure MemberU (k : Nat) where
  self : Nat
  base : Int
  cross : Fin k
deriving Repr, DecidableEq

/-! The mutual fuel twin.  This is `evalRecUFuel` with the recursive member
    selected by the byte-derived cross edge. -/

def evalMutualUFuel {k : Nat} (members : Fin k → MemberU k) :
    Nat → Fin k → Int → Int
  | 0, _, _ => 0
  | fuel + 1, i, n =>
      if n ≤ 0 then (members i).base
      else evalMutualUFuel members fuel (members i).cross (n - 1)

def evalMutualU {k : Nat} (members : Fin k → MemberU k)
    (i : Fin k) (n : Int) : Int :=
  evalMutualUFuel members (n.natAbs + 1) i n

/-- One cap induction proves fuel irrelevance for every member simultaneously.
    The `∀ i : Fin k` result is the k-generic finite conjunction. -/
theorem evalMutualU_fuel_irrel {k : Nat} (members : Fin k → MemberU k) :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      ∀ i : Fin k,
        evalMutualUFuel members k1 i n = evalMutualUFuel members k2 i n := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _ i; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2 i
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [evalMutualUFuel, hn]
      · have hstep : (n - 1).natAbs < t := by
          have h1n : (1 : Int) ≤ n := by omega
          have h2n : (n - 1).natAbs = n.natAbs - 1 := by omega
          omega
        simp only [evalMutualUFuel]
        rw [if_neg hn, if_neg hn]
        exact ih m1 m2 (n - 1) hstep (by omega) (by omega) (members i).cross

theorem evalMutualU_fuel_stable {k : Nat} (members : Fin k → MemberU k)
    (fuel : Nat) (i : Fin k) (n : Int) (h : n.natAbs < fuel) :
    evalMutualUFuel members fuel i n = evalMutualU members i n :=
  evalMutualU_fuel_irrel members (n.natAbs + fuel + 1) fuel (n.natAbs + 1)
    n (by omega) h (by omega) i

theorem evalMutualU_base {k : Nat} (members : Fin k → MemberU k)
    (i : Fin k) (n : Int) (hn : n ≤ 0) :
    evalMutualU members i n = (members i).base := by
  have h0 : evalMutualU members i n =
      evalMutualUFuel members (n.natAbs + 1) i n := rfl
  rw [h0]
  simp [evalMutualUFuel, hn]

theorem evalMutualU_step {k : Nat} (members : Fin k → MemberU k)
    (i : Fin k) (n : Int) (hn : ¬ n ≤ 0) :
    evalMutualU members i n = evalMutualU members (members i).cross (n - 1) := by
  have h0 : evalMutualU members i n =
      evalMutualUFuel members (n.natAbs + 1) i n := rfl
  rw [h0]
  simp only [evalMutualUFuel]
  rw [if_neg hn]
  exact evalMutualU_fuel_stable members n.natAbs (members i).cross (n - 1) (by omega)

/-! Canonical lowering of one accepted mutual-countdown member. -/

def signSmallInstrs (C : Nat) : List WInstr :=
  [.localGet 0, .structGet C 0, .i64Const 0, .i64LeS]

def signBigInstrs (C : Nat) : List WInstr :=
  [.localGet 0, .structGet C 2, .i32Const 0, .i32LtS]

def mutualInstrs {k : Nat} (C boxIdx subIdx : Nat) (members : Fin k → MemberU k)
    (i : Fin k) : List WInstr :=
  [.localGet 0, .structGet C 1, .refIsNull,
   .ifElse (signSmallInstrs C) (signBigInstrs C),
   .ifElse [.i64Const (members i).base, .call boxIdx]
     [.localGet 0, .i64Const 1, .call boxIdx, .call subIdx,
      .returnCall (members (members i).cross).self]]

/-- The plan/byte admission package.  SCC closure is deliberately a hypothesis:
    production's `mutualMembersFormClosedSccs` already proves it in-kernel.
    `lowered` is the required equality from each checked, byte-bound plan to the
    canonical member body; changing a cross target breaks this equality. -/
structure AdmittedScc (k C boxIdx subIdx : Nat) where
  members : Fin k → MemberU k
  plans : Fin k → MutualRawPlan
  rawEdges : List (Nat × Nat × List Nat)
  edgesBound : rawEdges = List.ofFn (fun i =>
    ((members i).self, (members (members i).cross).self,
      List.ofFn (fun j => (members j).self)))
  closed : AverCert.AcceptedArtifact.mutualMembersFormClosedSccs rawEdges = true
  checked : ∀ i, AverCert.PlanCheck.checkMutualRawPlan (plans i) = true
  shaped : ∀ i,
    AverCert.PlanCheck.checkMutualPlanShape
      (List.ofFn (fun j => (members j).self))
      [(.box, boxIdx), (.sub, subIdx)] (plans i) = true
  lowered : ∀ i,
    lowerMutualBody C (plans i) = some (mutualInstrs C boxIdx subIdx members i)

/-! One conjunction-over-fuel simulation theorem. -/

set_option trace.profiler true in
/-- Every admitted k-member SCC simulates its mutual fuel-twin.  There is one
    induction over fuel and one finite-conjunction motive (`∀ i : Fin k`).
    In the cross-call arm the recursive fact is exactly
    `ih (members i).cross ...`. -/
theorem mutual_generic_certified
    (k C boxIdx subIdx : Nat)
    (scc : AdmittedScc k C boxIdx subIdx)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ x : Int, Repr x (carrierSmall C x))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (sub : List WVal → Option WVal)
    (hBox : host boxIdx = some (1, boxRef C))
    (hSubHost : host subIdx = some (2, sub))
    (hMemberHost : ∀ i, host (scc.members i).self = none)
    (hCode : ∀ i, code (scc.members i).self =
      some ⟨1, 1, mutualInstrs C boxIdx subIdx scc.members i⟩)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (i : Fin k) (n : Int) (v w : WVal), Repr n v →
      wFuncN code host fuel (scc.members i).self [v] = some w →
      Repr (evalMutualU scc.members i n) w := by
  intro fuel
  induction fuel with
  | zero =>
      intro i n v w hv hrun
      simp [wFuncN] at hrun
  | succ fuel ih =>
      intro i n v w hv hrun
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, hCode i, hBox, hMemberHost i,
            mutualInstrs, signSmallInstrs, signBigInstrs,
            boxRef, b32, popArgs, initLocals, hle] at hrun
          rw [evalMutualU_base scc.members i s hle, ← hrun]
          exact hsmall_intro (scc.members i).base
        · simp [wFuncN, wRunF, hCode i, hCode (scc.members i).cross,
            hBox, hSubHost, hMemberHost i,
            mutualInstrs, signSmallInstrs, signBigInstrs,
            boxRef, b32, popArgs, initLocals, hle] at hrun
          rcases hsub : sub
              [.structv C [.i64v s, .null, .i32v sg], carrierSmall C 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN code host fuel
                (scc.members (scc.members i).cross).self [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr := ih (scc.members i).cross (s - 1) vd vr hrd hrec
              rw [evalMutualU_step scc.members i s hle]
              rw [Option.some.injEq] at hrun
              rw [← hrun]
              exact hrr
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, hCode i, hBox, hMemberHost i,
              mutualInstrs, signSmallInstrs, signBigInstrs,
              boxRef, b32, popArgs, initLocals, hlt] at hrun
          rw [evalMutualU_base scc.members i n hn0, ← hrun]
          exact hsmall_intro (scc.members i).base
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, hCode i, hCode (scc.members i).cross,
              hBox, hSubHost, hMemberHost i,
              mutualInstrs, signSmallInstrs, signBigInstrs,
              boxRef, b32, popArgs, initLocals, hlt] at hrun
          rcases hsub : sub
              [.structv C [.i64v s, .arr lty les, .i32v sg], carrierSmall C 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (n - 1) vd :=
              hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN code host fuel
                (scc.members (scc.members i).cross).self [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr := ih (scc.members i).cross (n - 1) vd vr hrd hrec
              rw [evalMutualU_step scc.members i n hn0]
              rw [Option.some.injEq] at hrun
              rw [← hrun]
              exact hrr

/-- Fuel-parametric progress for every member of an admitted countdown SCC.
    The single induction keeps all members in its motive, so a cross call uses
    the totality fact for exactly the byte-derived successor member. -/
theorem mutual_generic_certified_total_aux
    (k C boxIdx subIdx : Nat)
    (scc : AdmittedScc k C boxIdx subIdx)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ x : Int, Repr x (carrierSmall C x))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (sub : List WVal → Option WVal)
    (hBox : host boxIdx = some (1, boxRef C))
    (hSubHost : host subIdx = some (2, sub))
    (hMemberHost : ∀ i, host (scc.members i).self = none)
    (hCode : ∀ i, code (scc.members i).self =
      some ⟨1, 1, mutualInstrs C boxIdx subIdx scc.members i⟩)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, sub [va, vb] = some w) :
    ∀ fuel i n v, Repr n v → n.natAbs < fuel →
      ∃ w, wFuncN code host fuel (scc.members i).self [v] = some w ∧
        Repr (evalMutualU scc.members i n) w := by
  intro fuel
  induction fuel with
  | zero =>
      intro i n v hv hlt
      omega
  | succ fuel ih =>
      intro i n v hv hlt
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · refine ⟨carrierSmall C (scc.members i).base, ?_, ?_⟩
          · simp [wFuncN, wRunF, hCode i, hBox, hMemberHost i,
              mutualInstrs, signSmallInstrs, signBigInstrs,
              boxRef, b32, popArgs, initLocals, hle]
          · rw [evalMutualU_base scc.members i s hle]
            exact hsmall_intro (scc.members i).base
        · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall C 1)
            hv (hsmall_intro 1)
          have hrd : Repr (s - 1) vd :=
            hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
          obtain ⟨vr, hrec, hrr⟩ :=
            ih (scc.members i).cross (s - 1) vd hrd (by omega)
          refine ⟨vr, ?_, ?_⟩
          · simp [wFuncN, wRunF, hCode i, hCode (scc.members i).cross,
              hBox, hSubHost, hMemberHost i, mutualInstrs,
              signSmallInstrs, signBigInstrs, boxRef, b32, popArgs,
              initLocals, hle, hsub, hrec]
          · rw [evalMutualU_step scc.members i s hle]
            exact hrr
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by
            have := hsign.mp hlt
            omega
          refine ⟨carrierSmall C (scc.members i).base, ?_, ?_⟩
          · simp [wFuncN, wRunF, hCode i, hBox, hMemberHost i,
              mutualInstrs, signSmallInstrs, signBigInstrs,
              boxRef, b32, popArgs, initLocals, hlt]
          · rw [evalMutualU_base scc.members i n hn0]
            exact hsmall_intro (scc.members i).base
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall C 1)
            hv (hsmall_intro 1)
          have hrd : Repr (n - 1) vd :=
            hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
          obtain ⟨vr, hrec, hrr⟩ :=
            ih (scc.members i).cross (n - 1) vd hrd (by omega)
          refine ⟨vr, ?_, ?_⟩
          · simp [wFuncN, wRunF, hCode i, hCode (scc.members i).cross,
              hBox, hSubHost, hMemberHost i, mutualInstrs,
              signSmallInstrs, signBigInstrs, boxRef, b32, popArgs,
              initLocals, hlt, hsub, hrec]
          · rw [evalMutualU_step scc.members i n hn0]
            exact hrr

/-- Bounded-total correctness at the standard `Int.natAbs + 1` fuel for the
    whole admitted SCC conjunction. -/
theorem mutual_generic_certified_total
    (k C boxIdx subIdx : Nat)
    (scc : AdmittedScc k C boxIdx subIdx)
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ x : Int, Repr x (carrierSmall C x))
    (hsmall_elim : ∀ n s sg,
      Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
        ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (code : CodeTbl) (host : HostTbl)
    (sub : List WVal → Option WVal)
    (hBox : host boxIdx = some (1, boxRef C))
    (hSubHost : host subIdx = some (2, sub))
    (hMemberHost : ∀ i, host (scc.members i).self = none)
    (hCode : ∀ i, code (scc.members i).self =
      some ⟨1, 1, mutualInstrs C boxIdx subIdx scc.members i⟩)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb →
      sub [va, vb] = some w → Repr (a - b) w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb →
      ∃ w, sub [va, vb] = some w) :
    ∀ i n v, Repr n v →
      ∃ w, wFuncN code host (n.natAbs + 1) (scc.members i).self [v] = some w ∧
        Repr (evalMutualU scc.members i n) w := by
  intro i n v hv
  apply mutual_generic_certified_total_aux k C boxIdx subIdx scc Repr hcar
    hsmall_intro hsmall_elim hbig code host sub hBox hSubHost hMemberHost
    hCode hSub hSubTot
  · exact hv
  · omega

#print axioms evalMutualU_fuel_irrel
#print axioms evalMutualU_fuel_stable
#print axioms evalMutualU_base
#print axioms evalMutualU_step
#print axioms mutual_generic_certified
#print axioms mutual_generic_certified_total_aux
#print axioms mutual_generic_certified_total

end V3Mutual
