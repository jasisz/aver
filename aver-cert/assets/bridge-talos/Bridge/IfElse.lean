import Bridge.Config

/-!
# Spike (b): `ifElse`

Talos rules used (SmallStep.lean at the pinned commit):
* `Step.iff` (3499–3516): pops the `i32` condition, selects the body by
  `condition ≠ 0`, pushes a control frame `{ kind := .block, paramArity,
  resultArity, body := selected, continuation := code, belowStack :=
  values.drop paramArity }` and continues in the selected body with the WHOLE
  remaining stack (the values beneath stay visible to the body);
* `Step.exitControl` (3261–3270): with the body exhausted (`code = []`) and a
  non-throwing frame on top, replaces the stack by `values.take
  frame.resultArity ++ frame.belowStack` and continues in `frame.continuation`.

The wall's `wRunF` runs the branch on the flat stack beneath the condition and
keeps whatever the branch leaves. The two agree exactly when the branch pushes
ONE value and leaves the rest untouched — the `if` typing rule (`HasTy.ifElse`
types each branch from the empty stack; `typed_run` turns it into the
`BranchPushesOne` hypothesis below). The block "type" is inert: `Step.iff`
reads only the two arities, which the translation fixes to `0`/`1`.

Nesting is handled by the shape of the hypotheses: `BranchSim` is exactly the
conclusion of the whole-list bridge lemma (`Bridge.lean`), so an inner
`ifElse` inside a branch is discharged by the same lemma one level down. The
control stack `controls` is universally quantified, which is what makes the
frame pushed here invisible to the branch.
-/

namespace Bridge
open Wasm Wasm.SmallStep CertPrelude

/-- The induction-hypothesis shape for a branch body at the entry state
    `(locA, st₀)`: a successful wall run of `bodyA` pushes one value and is
    matched, from any related Talos thread, by a Talos run of `body'` that
    consumes exactly `body'` and lands in a related state, extending the heap.
    This is the conclusion of the whole-list lemma `bridge_run` (Bridge.lean)
    instantiated at the empty typed prefix. -/
def BranchSimAt {α : Type} (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostEnv : HostEnv α) (m : Module) (bodyA : List WInstr) (body' : Program)
    (locA st₀ : List WVal) : Prop :=
  ∀ (locA' stA' : List WVal),
    wRunF host ar callee bodyA locA st₀ = some (.ok locA' stA') →
    ∀ (cont : Program) (L : Locals) (wasm : Store α) (arity : Nat) (remainder : List Value)
      (controls : List ControlFrame) (calls : List CallFrame),
      RLocals wasm.gcHeap L locA → Rs wasm.gcHeap L.values st₀ →
      ∃ v trace L' wasm',
        stA' = v :: st₀ ∧
        Steps ⟨.running ⟨L, body' ++ cont, arity, remainder, controls, calls⟩,
              ⟨synthRuntime m hostEnv, wasm⟩⟩ trace
          ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩, ⟨synthRuntime m hostEnv, wasm'⟩⟩
        ∧ wasm.gcHeap <+: wasm'.gcHeap
        ∧ RLocals wasm'.gcHeap L' locA'
        ∧ Rs wasm'.gcHeap L'.values stA'

theorem UInt32.toInt32_toInt_eq_zero (u : UInt32) : u.toInt32.toInt = 0 ↔ u = 0 := by
  rw [← Int32.toInt_zero, Int32.toInt_inj]
  constructor
  · intro h
    have := congrArg Int32.toUInt32 h
    rw [show (0 : Int32).toUInt32 = 0 from by decide] at this
    exact this
  · rintro rfl
    rfl

/-- The `if` control frame the translation's `iff 0 1` pushes. -/
def ifFrame (selected cont : Program) (below : List Value) : ControlFrame :=
  { kind := .block, paramArity := 0, resultArity := 1, body := selected,
    continuation := cont, belowStack := below }

theorem ifFrame_not_throwing (selected cont : Program) (below : List Value) :
    (ifFrame selected cont below).kind.isThrowing = false := rfl

/-- One selected branch, from `Step.iff` through `exitControl`. -/
theorem bridge_branch {α : Type}
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostEnv : HostEnv α) (m : Module)
    (bodyA : List WInstr) (body' : Program)
    (L : Locals) (wasm : Store α) (cont : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame)
    (locA st₀ locA' stA' : List WVal)
    (hsim : BranchSimAt host ar callee hostEnv m bodyA body' locA st₀)
    (hL : RLocals wasm.gcHeap L locA) (hS : Rs wasm.gcHeap L.values st₀)
    (hbr : wRunF host ar callee bodyA locA st₀ = some (.ok locA' stA')) :
    ∃ trace L' wasm',
      Steps ⟨.running ⟨L, body', arity, remainder, ifFrame body' cont L.values :: controls, calls⟩,
            ⟨synthRuntime m hostEnv, wasm⟩⟩ trace
        ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩, ⟨synthRuntime m hostEnv, wasm'⟩⟩
      ∧ wasm.gcHeap <+: wasm'.gcHeap
      ∧ RLocals wasm'.gcHeap L' locA'
      ∧ Rs wasm'.gcHeap L'.values stA' := by
  obtain ⟨v, trace, L₁, wasm', rfl, hsteps, hpre, hL', hS'⟩ :=
    hsim locA' stA' hbr [] L wasm arity remainder
      (ifFrame body' cont L.values :: controls) calls hL hS
  rw [List.append_nil] at hsteps
  -- The branch leaves one related value on top of the entering stack.
  match hvals : L₁.values, hS' with
  | v' :: vs', hS' =>
    simp only [Rs] at hS'
    refine ⟨trace ++ [.administrative .exitControl],
      { L₁ with values := [v'] ++ L.values }, wasm', ?_, hpre, hL', ?_⟩
    · refine Steps.trans hsteps (Steps.single ?_)
      have hstep := Step.exitControl (α := α) (frame := ifFrame body' cont L.values)
        (locals := L₁) (arity := arity) (remainder := remainder) (controls := controls)
        (calls := calls) (store := ⟨synthRuntime m hostEnv, wasm'⟩)
        (ifFrame_not_throwing body' cont L.values)
      simpa [ifFrame, hvals] using hstep
    · exact Rs_cons hS'.1 (Rs_prefix hpre hS)

/-- Spike (b): the wall's `ifElse` on an `i32` condition is matched by
    `Step.iff`, the selected branch (by `BranchSim`), and `Step.exitControl`. -/
theorem bridge_ifElse {α : Type}
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostEnv : HostEnv α) (m : Module)
    (tB eB : List WInstr) (tB' eB' : Program)
    (L : Locals) (wasm : Store α) (cont : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame)
    (locA st₀ locA' stA' : List WVal) (c : Int)
    (hsim_t : BranchSimAt host ar callee hostEnv m tB tB' locA st₀)
    (hsim_e : BranchSimAt host ar callee hostEnv m eB eB' locA st₀)
    (hL : RLocals wasm.gcHeap L locA) (hS : Rs wasm.gcHeap L.values (.i32v c :: st₀))
    (hA : wRunF host ar callee [.ifElse tB eB] locA (.i32v c :: st₀) = some (.ok locA' stA')) :
    ∃ trace L' wasm',
      Steps ⟨.running ⟨L, .iff 0 1 tB' eB' [] [.anyref] :: cont, arity, remainder, controls, calls⟩,
            ⟨synthRuntime m hostEnv, wasm⟩⟩ trace
        ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩, ⟨synthRuntime m hostEnv, wasm'⟩⟩
      ∧ wasm.gcHeap <+: wasm'.gcHeap
      ∧ RLocals wasm'.gcHeap L' locA'
      ∧ Rs wasm'.gcHeap L'.values stA' := by
  -- The Talos twin of the `i32` condition.
  obtain ⟨params, locs, vals0⟩ := L
  match vals0, hS with
  | vc :: vals, hS =>
    simp only [Rs] at hS
    obtain ⟨u, rfl, hcu⟩ := R_i32v hS.1
    simp only [wRunF] at hA
    split at hA
    · -- else branch
      rename_i hc
      have hu : u = 0 := (UInt32.toInt32_toInt_eq_zero u).mp (hcu ▸ hc)
      subst hu
      split at hA
      · rename_i l₁ st₁ hbr
        simp only [Option.some.injEq, Out.ok.injEq] at hA
        obtain ⟨rfl, rfl⟩ := hA
        obtain ⟨trace, L', wasm', hsteps, hpre, hL', hS'⟩ :=
          bridge_branch host ar callee hostEnv m eB eB'
            ⟨params, locs, vals⟩ wasm cont arity remainder controls calls
            locA st₀ l₁ st₁ hsim_e hL hS.2 hbr
        refine ⟨.instruction (.iff 0 1 tB' eB' [] [.anyref]) :: trace, L', wasm', ?_, hpre, hL', hS'⟩
        refine Steps.cons ?_ hsteps
        have hstep := Step.iff (α := α) (params := params) (localValues := locs)
          (values := vals) (condition := 0) (thenBody := tB') (elseBody := eB')
          (paramArity := 0) (resultArity := 1) (paramTypes := []) (resultTypes := [.anyref])
          (code := cont) (arity := arity) (remainder := remainder) (controls := controls)
          (calls := calls) (store := ⟨synthRuntime m hostEnv, wasm⟩) (selectedBody := eB')
          (by simp)
        simpa [ifFrame] using hstep
      · simp at hA
      · simp at hA
    · -- then branch
      rename_i hc
      have hu : u ≠ 0 := fun h => hc ((UInt32.toInt32_toInt_eq_zero u).mpr h ▸ hcu)
      split at hA
      · rename_i l₁ st₁ hbr
        simp only [Option.some.injEq, Out.ok.injEq] at hA
        obtain ⟨rfl, rfl⟩ := hA
        obtain ⟨trace, L', wasm', hsteps, hpre, hL', hS'⟩ :=
          bridge_branch host ar callee hostEnv m tB tB'
            ⟨params, locs, vals⟩ wasm cont arity remainder controls calls
            locA st₀ l₁ st₁ hsim_t hL hS.2 hbr
        refine ⟨.instruction (.iff 0 1 tB' eB' [] [.anyref]) :: trace, L', wasm', ?_, hpre, hL', hS'⟩
        refine Steps.cons ?_ hsteps
        have hstep := Step.iff (α := α) (params := params) (localValues := locs)
          (values := vals) (condition := u) (thenBody := tB') (elseBody := eB')
          (paramArity := 0) (resultArity := 1) (paramTypes := []) (resultTypes := [.anyref])
          (code := cont) (arity := arity) (remainder := remainder) (controls := controls)
          (calls := calls) (store := ⟨synthRuntime m hostEnv, wasm⟩) (selectedBody := tB')
          (by simp [hu])
        simpa [ifFrame] using hstep
      · simp at hA
      · simp at hA

end Bridge
