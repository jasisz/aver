import Bridge.Config

/-!
# Spike (a): `call` of a host slot

Talos rule used: `Step.callHostReturn` (SmallStep.lean:3558–3573 at the pinned
commit). It reads, off the current module and host, `imports[i]` (only its
`params.length`/`results.length`) and `currentHost.funcs[i]?`, runs the concrete
`invoke` on `(values.take arity).reverse`, and replaces the arguments by
`results.take resultCount`. The step kind is `.host i`. `callHostTrap`/
`callHostThrow` (3574/3586) are the other outcomes; `HostSimulation` rules them
out whenever the wall's contract is defined.

The heap-frame half of `HostSimulation` IS needed: after the call, the locals
and the untouched part of the stack are related to the wall's values only
under the OLD heap; `Rs_prefix` carries them to the new heap.
-/

namespace Bridge
open Wasm Wasm.SmallStep CertPrelude
open AverCert.Schema (CarrierSpec)

theorem bridge_hostCall {α : Type}
    (env : TranslateEnv) (S : CarrierSpec env.carrier) (host : HostTbl) (ar : Nat → Option Nat)
    (callee : Callee)
    (hostEnv : HostEnv α) (hsim : HostSimulation env S host hostEnv) (hsorts : HostSorts env S host)
    (paramSorts : List STy) (result : STy) (nlocals : Nat) (body : Program)
    (wasm : Store α) (L : Locals) (code : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame)
    (locA stA stA' : List WVal) (f i : Nat) (sig : ImportSig)
    (hslot : slotLookup? env.imports f = some (i, sig))
    (hsorted : Sorted env S (stA.take sig.params.length).reverse sig.params)
    (hL : RLocals wasm.gcHeap L locA) (hS : Rs wasm.gcHeap L.values stA)
    (hA : wRunF host ar callee [.call f] locA stA = some (.ok locA stA')) :
    ∃ values' wasm',
      Step ⟨.running ⟨L, .call i :: code, arity, remainder, controls, calls⟩,
            ⟨synthRuntime (synthModule env paramSorts result nlocals body) hostEnv, wasm⟩⟩
        (.host i)
        ⟨.running ⟨{ L with values := values' }, code, arity, remainder, controls, calls⟩,
            ⟨synthRuntime (synthModule env paramSorts result nlocals body) hostEnv, wasm'⟩⟩
      ∧ wasm.gcHeap <+: wasm'.gcHeap
      ∧ RLocals wasm'.gcHeap L locA
      ∧ Rs wasm'.gcHeap values' stA' := by
  -- The wall's step.
  obtain ⟨hf, hhf, -⟩ := hsorts f i sig hslot
  simp only [wRunF, hhf] at hA
  split at hA
  · rename_i args st' hpop
    split at hA
    · rename_i r hr
      simp only [Option.some.injEq, Out.ok.injEq] at hA
      obtain ⟨-, rfl⟩ := hA
      simp only [popArgs] at hpop
      split at hpop
      · simp at hpop
      · rename_i hlen
        simp only [Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        -- Talos's host call.
        obtain ⟨hsig, -⟩ := slotLookup?_getElem hslot
        obtain ⟨hfn, hhfn⟩ := hsim.resolved i sig hsig
        have hRargs : Rs wasm.gcHeap (L.values.take sig.params.length).reverse
            (stA.take sig.params.length).reverse :=
          Rs_reverse (Rs_take _ hS)
        obtain ⟨r', wasm', hinv, hpre, hRr⟩ :=
          hsim.invoke f i sig hf hfn hslot hhf hhfn wasm _ _ r hsorted hRargs hr
        have hlt : i < env.imports.length := (List.getElem?_eq_some_iff.mp hsig).1
        have himports : i < (synthRuntime (synthModule env paramSorts result nlocals body)
            hostEnv).currentModule.imports.length := by
          simpa [synthModule_imports_length] using hlt
        have himport : (synthRuntime (synthModule env paramSorts result nlocals body)
            hostEnv).currentModule.imports[i]'himports = importDecl sig := by
          rw [List.getElem_eq_iff]
          simpa using synthModule_imports_getElem? hsig paramSorts result nlocals body
        have hhost : (synthRuntime (synthModule env paramSorts result nlocals body)
            hostEnv).currentHost.funcs[i]? = some hfn := by
          simpa using hhfn
        refine ⟨r' :: L.values.drop sig.params.length, wasm', ?_, hpre, Rs_prefix hpre hL,
          Rs_cons hRr (Rs_prefix hpre (Rs_drop _ hS))⟩
        have hstep := Step.callHostReturn (α := α)
          (store := ⟨synthRuntime (synthModule env paramSorts result nlocals body) hostEnv, wasm⟩)
          (functionIndex := i) (imp := importDecl sig) (hostFunction := hfn)
          (params := L.params) (localValues := L.locals)
          (values := L.values) (code := code) (arity := arity) (remainder := remainder)
          (controls := controls) (calls := calls) (results := [r']) (wasm := wasm')
          himports himport hhost (by simpa [importDecl_params_length] using hinv)
        simpa [importDecl_params_length, importDecl_results_length] using hstep
    · simp at hA
  · simp at hA

end Bridge
