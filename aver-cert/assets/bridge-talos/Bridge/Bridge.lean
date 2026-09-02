import Bridge.HostCall
import Bridge.IfElse
import Bridge.Instr

/-!
# The profile theorem: a successful `wFuncN` run terminates in Talos

`bridge_run` is the whole-list lemma, by induction on the typing derivation
(`HasTy`), one case per instruction kind, each closed by the corresponding
per-instruction lemma (`Instr.lean`, `HostCall.lean`, `IfElse.lean`). It is
stated FRAMED like `typed_run`: the wall's stack is a typed prefix over an
arbitrary `below`, so an `if` branch (typed from the empty stack) is the same
lemma at the empty prefix — this is what makes the induction go through
nested `ifElse` without the wall's `wRunF_frame`.

`wFuncN_terminatesWith` is the statement of brief §4.5: with the wall's
fuelled call semantics returning `some w` for the export, Talos, started in
the configuration `Config.lean` builds, reaches `.done [v]` with `v` related
to `w`. Talos's own `TerminatesWith` (SmallStep.lean:7066) is exactly
`∃ trace values store, Steps … ⟨.done values, store⟩ ∧ post values store`;
the theorem is stated in that shape and repackaged as `TerminatesWith`.

Premises, all explicit: `HostSimulation` and `HostSorts` for the host slots,
a typing derivation for the body (the coverage lemma's output), the
translation, related and well-sorted arguments.
-/

namespace Bridge
open Wasm Wasm.SmallStep CertPrelude

theorem Rs_replicate_null (heap : List GcObject) (n : Nat) :
    Rs heap (List.replicate n (.anyref none)) (List.replicate n .null) := by
  induction n with
  | zero => exact Rs_nil
  | succ n ih => simpa [List.replicate_succ, Rs, R] using ih

/-- The whole-list lemma. -/
theorem bridge_run {α : Type} (env : TranslateEnv) (host : HostTbl) (ar : Nat → Option Nat)
    (callee : Callee) (hostEnv : HostEnv α) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (fbody : Program)
    (hsim : HostSimulation env host hostEnv) (hsorts : HostSorts env host) {Γ : List STy} :
    ∀ {σ : List STy} {is : List WInstr} {σ' : List STy}, HasTy env Γ σ is σ' →
    ∀ (code' : Program), translateList env is = some code' →
    ∀ (locA st below : List WVal) (out : Out),
      Sorted env locA Γ → Sorted env st σ →
      wRunF host ar callee is locA (st ++ below) = some out →
      ∀ (cont : Program) (L : Locals) (wasm : Store α) (arity : Nat) (remainder : List Value)
        (controls : List ControlFrame) (calls : List CallFrame),
        RLocals wasm.gcHeap L locA → Rs wasm.gcHeap L.values (st ++ below) →
        ∃ locA' st' trace L' wasm',
          out = .ok locA' (st' ++ below) ∧ Sorted env locA' Γ ∧ Sorted env st' σ' ∧
          Steps ⟨.running ⟨L, code' ++ cont, arity, remainder, controls, calls⟩,
                ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm⟩⟩ trace
            ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩,
                ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm'⟩⟩ ∧
          wasm.gcHeap <+: wasm'.gcHeap ∧ RLocals wasm'.gcHeap L' locA' ∧
          Rs wasm'.gcHeap L'.values (st' ++ below) := by
  intro σ is σ' hty
  induction hty with
  | nil =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      simp only [translateList, Option.some.injEq] at htr
      subst htr
      simp only [wRunF, Option.some.injEq] at hrun
      exact ⟨locA, st, [], L, wasm, hrun.symm, hΓ, hσ, by simpa using Steps.refl _,
        List.prefix_refl _, hL, hS⟩
  | localGet h _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, Option.some.injEq] at hx
      subst hx
      obtain ⟨w, hw, hsort⟩ := Sorted_getElem? _ _ hΓ h
      simp only [wRunF, hw] at hrun
      obtain ⟨params, locs, values⟩ := L
      obtain ⟨tv, hstep, hR⟩ := bridge_localGet (synthRuntime (synthModule env paramSorts result
        nlocals fbody) hostEnv) wasm params locs values (xs' ++ cont) arity remainder controls calls
        locA _ w hL hw
      obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
        ih xs' hxs locA (w :: st) below out hΓ (Sorted_cons hsort hσ) hrun cont
          ⟨params, locs, tv :: values⟩ wasm arity remainder controls calls hL (Rs_cons hR hS)
      exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre, hL', hS'⟩
  | @localSet _ _ i _ _ h _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, Option.some.injEq] at hx
      subst hx
      match st, hσ, hS with
      | w :: st, hσ, hS =>
        simp only [Sorted] at hσ
        obtain ⟨params, locs, values0⟩ := L
        match values0, hS with
        | tv :: values, hS =>
          simp only [List.cons_append, Rs] at hS
          simp only [wRunF, List.cons_append] at hrun
          have hi : i < locA.length := by
            rw [Sorted_length hΓ]
            exact (List.getElem?_eq_some_iff.mp h).1
          obtain ⟨L₁, hstep, hL₁⟩ := bridge_localSet (synthRuntime (synthModule env paramSorts
            result nlocals fbody) hostEnv) wasm params locs values tv (xs' ++ cont) arity remainder
            controls calls locA _ w hi hL hS.1
          obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
            ih xs' hxs (locA.set _ w) st below out (Sorted_set _ hΓ h hσ.1) hσ.2 hrun cont
              { L₁ with values := values } wasm arity remainder controls calls hL₁ hS.2
          exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre,
            hL', hS'⟩
  | @i64Const _ _ n _ hn _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, hn, if_true, Option.some.injEq] at hx
      subst hx
      simp only [wRunF] at hrun
      obtain ⟨params, locs, values⟩ := L
      obtain ⟨hstep, hR⟩ := bridge_i64Const (synthRuntime (synthModule env paramSorts result
        nlocals fbody) hostEnv) wasm params locs values (xs' ++ cont) arity remainder controls calls
        n hn
      obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
        ih xs' hxs locA (.i64v n :: st) below out hΓ (Sorted_cons (by simp [HasSort]) hσ) hrun cont
          ⟨params, locs, .i64 (constI64 n) :: values⟩ wasm arity remainder controls calls hL
          (Rs_cons hR hS)
      exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre, hL', hS'⟩
  | @i32Const _ _ n _ hn _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, hn, if_true, Option.some.injEq] at hx
      subst hx
      simp only [wRunF] at hrun
      obtain ⟨params, locs, values⟩ := L
      obtain ⟨hstep, hR⟩ := bridge_i32Const (synthRuntime (synthModule env paramSorts result
        nlocals fbody) hostEnv) wasm params locs values (xs' ++ cont) arity remainder controls calls
        n hn
      obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
        ih xs' hxs locA (.i32v n :: st) below out hΓ (Sorted_cons (by simp [HasSort]) hσ) hrun cont
          ⟨params, locs, .i32 (constI32 n) :: values⟩ wasm arity remainder controls calls hL
          (Rs_cons hR hS)
      exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre, hL', hS'⟩
  | structGet hs hf _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, Option.some.injEq] at hx
      subst hx
      match st, hσ, hS with
      | w :: st, hσ, hS =>
        simp only [Sorted] at hσ
        obtain ⟨params, locs, values0⟩ := L
        match values0, hS with
        | tv :: values, hS =>
          simp only [List.cons_append, Rs] at hS
          rcases HasSort_ref hσ.1 with rfl | ⟨t', fs', rfl⟩ | ⟨t', es', rfl⟩
          · simp [wRunF] at hrun
          · simp only [wRunF, List.cons_append] at hrun
            split at hrun
            · rename_i hty
              subst hty
              obtain ⟨ts, hts, hfs⟩ := HasSort_structv hσ.1
              rw [hs] at hts
              simp only [Option.some.injEq] at hts
              subst hts
              split at hrun
              · rename_i v hv
                obtain ⟨_, hv', hvsort⟩ := Sorted_getElem? _ _ hfs hf
                rw [hv] at hv'
                simp only [Option.some.injEq] at hv'
                subst hv'
                obtain ⟨tv', hstep, hR⟩ := bridge_structGet (synthRuntime (synthModule env
                  paramSorts result nlocals fbody) hostEnv) wasm params locs values tv (xs' ++ cont)
                  arity remainder controls calls _ _ _ fs' v hS.1 hv
                obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
                  ih xs' hxs locA (v :: st) below out hΓ (Sorted_cons hvsort hσ.2) hrun cont
                    ⟨params, locs, tv' :: values⟩ wasm arity remainder controls calls hL
                    (Rs_cons hR hS.2)
                exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps,
                  hpre, hL', hS'⟩
              · simp at hrun
            · simp at hrun
          · simp [wRunF] at hrun
  | @structNew _ _ ty fs _ hs _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, hs, if_true, Option.some.injEq] at hx
      subst hx
      obtain ⟨hpop, hargs, hrest⟩ := popArgs_sorted (below := below) hσ
      rw [List.length_reverse] at hpop hargs hrest
      rw [List.reverse_reverse] at hargs
      simp only [wRunF, hpop] at hrun
      have hlen : fs.length ≤ st.length := by
        have := Sorted_length hσ
        simp only [List.length_append, List.length_reverse] at this
        omega
      obtain ⟨params, locs, values⟩ := L
      obtain ⟨wasm₁, hstep, hpre₁, hS₁⟩ := bridge_structNew env paramSorts result nlocals fbody
        hostEnv wasm params locs values (xs' ++ cont) arity remainder controls calls ty fs hs
        (st ++ below) hS (by simp only [List.length_append]; omega)
      rw [List.take_append_of_le_length hlen, List.drop_append_of_le_length hlen] at hS₁
      obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
        ih xs' hxs locA (.structv ty (st.take fs.length).reverse :: st.drop fs.length) below out hΓ
          (Sorted_cons ⟨fs, hs, hargs⟩ hrest) hrun cont
          ⟨params, locs, .anyref (some (.struct wasm.gcHeap.length)) :: values.drop fs.length⟩ wasm₁
          arity remainder controls calls (Rs_prefix hpre₁ hL) hS₁
      exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps,
        hpre₁.trans hpre, hL', hS'⟩
  | refIsNull _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, Option.some.injEq] at hx
      subst hx
      match st, hσ, hS with
      | w :: st, hσ, hS =>
        simp only [Sorted] at hσ
        obtain ⟨params, locs, values0⟩ := L
        match values0, hS with
        | tv :: values, hS =>
          simp only [List.cons_append, Rs] at hS
          have hshape := HasSort_ref hσ.1
          -- The pushed Boolean, by the shape of `w`.
          obtain ⟨b, hb, hrun'⟩ : ∃ b : Bool, ((w = .null → b = true) ∧
              ((∃ t fs, w = .structv t fs) → b = false) ∧ ((∃ t es, w = .arr t es) → b = false)) ∧
              wRunF host ar callee _ locA ((b32 b :: st) ++ below) = some out := by
            rcases hshape with rfl | ⟨t, fs, rfl⟩ | ⟨t, es, rfl⟩
            · exact ⟨true, ⟨fun _ => rfl, (fun ⟨_, _, h⟩ => nomatch h), (fun ⟨_, _, h⟩ => nomatch h)⟩,
                by simpa [wRunF] using hrun⟩
            · exact ⟨false, ⟨(fun h => nomatch h), fun _ => rfl, (fun ⟨_, _, h⟩ => nomatch h)⟩,
                by simpa [wRunF] using hrun⟩
            · exact ⟨false, ⟨(fun h => nomatch h), (fun ⟨_, _, h⟩ => nomatch h), fun _ => rfl⟩,
                by simpa [wRunF] using hrun⟩
          obtain ⟨r, hstep, hR⟩ := bridge_refIsNull (synthRuntime (synthModule env paramSorts result
            nlocals fbody) hostEnv) wasm params locs values tv (xs' ++ cont) arity remainder controls
            calls w hS.1 b hb hshape
          obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
            ih xs' hxs locA (b32 b :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2) hrun' cont
              ⟨params, locs, .i32 r :: values⟩ wasm arity remainder controls calls hL
              (Rs_cons hR hS.2)
          exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre,
            hL', hS'⟩
  | @call _ _ f i sig _ hs _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate, hs, Option.map_some, Option.some.injEq] at hx
      subst hx
      obtain ⟨hf, hhf, hsort⟩ := hsorts _ _ _ hs
      obtain ⟨hpop, hargs, hrest⟩ := popArgs_sorted (below := below) hσ
      rw [List.length_reverse] at hpop hargs hrest
      rw [List.reverse_reverse] at hargs
      simp only [wRunF, hhf, hpop] at hrun
      split at hrun
      · rename_i r hr
        have hA : wRunF host ar callee [.call f] locA (st ++ below) =
            some (.ok locA (r :: (st.drop sig.params.length ++ below))) := by
          simp [wRunF, hhf, hpop, hr]
        obtain ⟨values₁, wasm₁, hstep, hpre₁, hL₁, hS₁⟩ := bridge_hostCall env host ar callee hostEnv
          hsim hsorts paramSorts result nlocals fbody wasm L (xs' ++ cont) arity remainder controls
          calls locA (st ++ below) _ f i sig hs hL hS hA
        obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
          ih xs' hxs locA (r :: st.drop sig.params.length) below out hΓ
            (Sorted_cons (hsort _ _ hargs hr) hrest) hrun cont { L with values := values₁ } wasm₁
            arity remainder controls calls hL₁ hS₁
        exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps,
          hpre₁.trans hpre, hL', hS'⟩
      · simp at hrun
  | @i64Cmp _ _ op _ hop _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      match st, hσ, hS with
      | w₁ :: w₂ :: st, hσ, hS =>
        simp only [Sorted] at hσ
        obtain ⟨b, rfl⟩ := HasSort_i64 hσ.1
        obtain ⟨a, rfl⟩ := HasSort_i64 hσ.2.1
        obtain ⟨params, locs, values0⟩ := L
        match values0, hS with
        | tv₁ :: tv₂ :: values, hS =>
          simp only [List.cons_append, Rs] at hS
          obtain ⟨rhs, rfl, hb⟩ := R_i64v hS.1
          obtain ⟨lhs, rfl, ha⟩ := R_i64v hS.2.1
          obtain ⟨r, w, heq, hwsort, hstep, hR⟩ := bridge_i64Cmp (synthRuntime (synthModule env
            paramSorts result nlocals fbody) hostEnv) wasm params locs values lhs rhs (xs' ++ cont)
            arity remainder controls calls a b ha hb env host ar callee op hop x' hx
          rw [List.cons_append, List.cons_append, heq] at hrun
          obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
            ih xs' hxs locA (w :: st) below out hΓ (Sorted_cons hwsort hσ.2.2) hrun cont
              ⟨params, locs, .i32 r :: values⟩ wasm arity remainder controls calls hL
              (Rs_cons hR hS.2.2)
          exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre,
            hL', hS'⟩
  | @i32Cmp _ _ op _ hop _ ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      match st, hσ, hS with
      | w₁ :: w₂ :: st, hσ, hS =>
        simp only [Sorted] at hσ
        obtain ⟨b, rfl⟩ := HasSort_i32 hσ.1
        obtain ⟨a, rfl⟩ := HasSort_i32 hσ.2.1
        obtain ⟨params, locs, values0⟩ := L
        match values0, hS with
        | tv₁ :: tv₂ :: values, hS =>
          simp only [List.cons_append, Rs] at hS
          obtain ⟨rhs, rfl, hb⟩ := R_i32v hS.1
          obtain ⟨lhs, rfl, ha⟩ := R_i32v hS.2.1
          obtain ⟨r, w, heq, hwsort, hstep, hR⟩ := bridge_i32Cmp (synthRuntime (synthModule env
            paramSorts result nlocals fbody) hostEnv) wasm params locs values lhs rhs (xs' ++ cont)
            arity remainder controls calls a b ha hb env host ar callee op hop x' hx
          rw [List.cons_append, List.cons_append, heq] at hrun
          obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
            ih xs' hxs locA (w :: st) below out hΓ (Sorted_cons hwsort hσ.2.2) hrun cont
              ⟨params, locs, .i32 r :: values⟩ wasm arity remainder controls calls hL
              (Rs_cons hR hS.2.2)
          exact ⟨locA', st', _ :: trace, L', wasm', hout, hΓ', hσ', Steps.cons hstep hsteps, hpre,
            hL', hS'⟩
  | @ifElse _ σ₀' t tB eB is₀ ht he _ iht ihe ih =>
      intro code' htr locA st below out hΓ hσ hrun cont L wasm arity remainder controls calls hL hS
      obtain ⟨x', xs', hx, hxs, rfl⟩ := translateList_cons htr
      simp only [translate] at hx
      split at hx
      · rename_i tB' eB' htB heB
        simp only [Option.some.injEq] at hx
        subst hx
        match st, hσ, hS with
        | w :: st, hσ, hS =>
          simp only [Sorted] at hσ
          obtain ⟨c, rfl⟩ := HasSort_i32 hσ.1
          rw [List.cons_append] at hS
          -- A branch at this entry state, from its induction hypothesis.
          have hbranch : ∀ (bodyA : List WInstr) (body' : Program),
              (∀ (code' : Program), translateList env bodyA = some code' →
                ∀ (locA st below : List WVal) (out : Out),
                  Sorted env locA Γ → Sorted env st [] →
                  wRunF host ar callee bodyA locA (st ++ below) = some out →
                  ∀ (cont : Program) (L : Locals) (wasm : Store α) (arity : Nat)
                    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame),
                    RLocals wasm.gcHeap L locA → Rs wasm.gcHeap L.values (st ++ below) →
                    ∃ locA' st' trace L' wasm',
                      out = .ok locA' (st' ++ below) ∧ Sorted env locA' Γ ∧ Sorted env st' [t] ∧
                      Steps ⟨.running ⟨L, code' ++ cont, arity, remainder, controls, calls⟩,
                            ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm⟩⟩
                        trace
                        ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩,
                            ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm'⟩⟩ ∧
                      wasm.gcHeap <+: wasm'.gcHeap ∧ RLocals wasm'.gcHeap L' locA' ∧
                      Rs wasm'.gcHeap L'.values (st' ++ below)) →
              translateList env bodyA = some body' →
              BranchSimAt host ar callee hostEnv (synthModule env paramSorts result nlocals fbody)
                bodyA body' locA (st ++ below) := by
            intro bodyA body' ihb htrb locA' stA' hbr cont₁ L₁ wasm₁ arity₁ remainder₁ controls₁
              calls₁ hL₁ hS₁
            obtain ⟨locA₂, st₂, trace, L₂, wasm₂, hout, -, hσ₂, hsteps, hpre, hL₂, hS₂⟩ :=
              ihb body' htrb locA [] (st ++ below) _ hΓ Sorted_nil (by simpa using hbr) cont₁ L₁
                wasm₁ arity₁ remainder₁ controls₁ calls₁ hL₁ (by simpa using hS₁)
            obtain ⟨v, rfl, -⟩ := Sorted_singleton_inv hσ₂
            simp only [Out.ok.injEq] at hout
            obtain ⟨rfl, rfl⟩ := hout
            exact ⟨v, trace, L₂, wasm₂, rfl, hsteps, hpre, hL₂, by simpa using hS₂⟩
          have hsim_t := hbranch tB tB' iht htB
          have hsim_e := hbranch eB eB' ihe heB
          -- The wall's step, one branch at a time.
          simp only [wRunF, List.cons_append] at hrun
          have hfinish : ∀ (bodyA : List WInstr) (hty : HasTy env Γ [] bodyA [t])
              (l₁ : List WVal) (s₁ : List WVal),
              wRunF host ar callee bodyA locA (st ++ below) = some (.ok l₁ s₁) →
              wRunF host ar callee [.ifElse tB eB] locA (.i32v c :: (st ++ below)) = some (.ok l₁ s₁) →
              wRunF host ar callee is₀ l₁ s₁ = some out →
              ∃ locA' st' trace L' wasm',
                out = .ok locA' (st' ++ below) ∧ Sorted env locA' Γ ∧ Sorted env st' σ₀' ∧
                Steps ⟨.running ⟨L, (Instruction.iff 0 1 tB' eB' [] [.anyref] :: xs') ++ cont, arity,
                        remainder, controls, calls⟩,
                      ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm⟩⟩
                  trace
                  ⟨.running ⟨L', cont, arity, remainder, controls, calls⟩,
                      ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm'⟩⟩ ∧
                wasm.gcHeap <+: wasm'.gcHeap ∧ RLocals wasm'.gcHeap L' locA' ∧
                Rs wasm'.gcHeap L'.values (st' ++ below) := by
            intro bodyA hty l₁ s₁ hbr hA hrun
            obtain ⟨l₂, s₂, hout, hΓ₂, hσ₂⟩ :=
              typed_run host ar callee hsorts hty locA [] (st ++ below) _ hΓ Sorted_nil
                (by simpa using hbr)
            obtain ⟨v, rfl, hv⟩ := Sorted_singleton_inv hσ₂
            simp only [Out.ok.injEq] at hout
            obtain ⟨rfl, rfl⟩ := hout
            obtain ⟨trace₁, L₁, wasm₁, hsteps₁, hpre₁, hL₁, hS₁⟩ :=
              bridge_ifElse host ar callee hostEnv _ tB eB tB' eB' L wasm (xs' ++ cont) arity
                remainder controls calls locA (st ++ below) l₁ _ c hsim_t hsim_e hL hS hA
            obtain ⟨locA', st', trace, L', wasm', hout, hΓ', hσ', hsteps, hpre, hL', hS'⟩ :=
              ih xs' hxs l₁ (v :: st) below out hΓ₂ (Sorted_cons hv hσ.2) (by simpa using hrun) cont
                L₁ wasm₁ arity remainder controls calls hL₁ (by simpa using hS₁)
            exact ⟨locA', st', trace₁ ++ trace, L', wasm', hout, hΓ', hσ',
              Steps.trans hsteps₁ hsteps, hpre₁.trans hpre, hL', hS'⟩
          split at hrun
          · rename_i hc
            split at hrun
            · rename_i l₁ s₁ hbr
              exact hfinish eB he l₁ s₁ hbr (by simp [wRunF, hc, hbr]) hrun
            · rename_i v hbr
              obtain ⟨_, _, heq, -, -⟩ := typed_run host ar callee hsorts he locA [] (st ++ below) _
                hΓ Sorted_nil (by simpa using hbr)
              simp at heq
            · simp at hrun
          · rename_i hc
            split at hrun
            · rename_i l₁ s₁ hbr
              exact hfinish tB ht l₁ s₁ hbr (by simp [wRunF, hc, hbr]) hrun
            · rename_i v hbr
              obtain ⟨_, _, heq, -, -⟩ := typed_run host ar callee hsorts ht locA [] (st ++ below) _
                hΓ Sorted_nil (by simpa using hbr)
              simp at heq
            · simp at hrun
      · simp at hx

/-! ## The export -/

/-- Brief §4.5: a successful `wFuncN` run of the export is matched by a Talos
    run from `initialConfig` to `.done [v]` with `v` related to the wall's
    result. Fuel is the wall's only source of non-termination and it is
    already spent by the hypothesis. -/
theorem wFuncN_terminatesWith {α : Type} (env : TranslateEnv) (host : HostTbl) (hostEnv : HostEnv α)
    (hsim : HostSimulation env host hostEnv) (hsorts : HostSorts env host)
    (code : CodeTbl) (self fuel : Nat) (c : WCode) (hc : code self = some c)
    (paramSorts : List STy) (result : STy) (body' : Program)
    (hty : HasTy env (paramSorts ++ List.replicate c.nlocals .ref) [] c.body [result])
    (htr : translateList env c.body = some body')
    (vs : List WVal) (hvs : Sorted env vs paramSorts)
    (store0 : Store α) (args : List Value) (hargs : Rs store0.gcHeap args vs)
    (w : WVal) (hrun : wFuncN code host fuel self vs = some w) :
    ∃ trace v store',
      Steps (initialConfig (synthModule env paramSorts result c.nlocals body') hostEnv
          (synthFunction env paramSorts result c.nlocals body') store0 args)
        trace ⟨.done [v], store'⟩
      ∧ R store'.wasm.gcHeap v w := by
  match fuel, hrun with
  | 0, hrun => simp [wFuncN] at hrun
  | fuel + 1, hrun =>
    simp only [wFuncN, hc] at hrun
    -- The wall's initial locals are the arguments followed by null padding;
    -- Talos's are the arguments followed by the zero of the declared local type.
    have hΓ : Sorted env (initLocals c vs) (paramSorts ++ List.replicate c.nlocals .ref) :=
      Sorted_append hvs (Sorted_replicate_null _)
    have hL0 : RLocals store0.gcHeap
        ((synthFunction env paramSorts result c.nlocals body').toLocals args) (initLocals c vs) := by
      unfold RLocals
      simp only [Function.toLocals, synthFunction, List.map_replicate, localType_zero, initLocals]
      exact Rs_append_list hargs (Rs_replicate_null _ _)
    split at hrun
    · rename_i l v hbody
      simp only [Option.some.injEq] at hrun
      subst hrun
      obtain ⟨locA', st', trace, L', wasm', hout, -, -, hsteps, -, -, hS'⟩ :=
        bridge_run env host _ _ hostEnv paramSorts result c.nlocals body' hsim hsorts hty body' htr
          (initLocals c vs) [] [] _ hΓ Sorted_nil (by simpa using hbody) []
          ((synthFunction env paramSorts result c.nlocals body').toLocals args) store0 1 [] [] []
          hL0 (by simp [Function.toLocals, Rs])
      simp only [List.append_nil, Out.ok.injEq] at hout hS' hsteps
      obtain ⟨-, rfl⟩ := hout
      match hvals : L'.values, hS' with
      | [tv], hS' =>
        simp only [Rs] at hS'
        refine ⟨trace ++ [.administrative .finish], tv,
          ⟨synthRuntime (synthModule env paramSorts result c.nlocals body') hostEnv, wasm'⟩, ?_, hS'.1⟩
        have hfin := Step.finish (α := α) (locals := L') (arity := 1) (remainder := [])
          (store := ⟨synthRuntime (synthModule env paramSorts result c.nlocals body') hostEnv, wasm'⟩)
        rw [hvals] at hfin
        exact Steps.trans hsteps (Steps.single hfin)
    · rename_i v hbody
      -- A typed body never returns through `.ret`.
      obtain ⟨_, _, heq, -, -⟩ :=
        typed_run host _ _ hsorts hty (initLocals c vs) [] [] _ hΓ Sorted_nil (by simpa using hbody)
      simp at heq
    · simp at hrun

/-- The same statement in Talos's own vocabulary (`TerminatesWith`,
    SmallStep.lean:7066). -/
theorem wFuncN_TerminatesWith {α : Type} (env : TranslateEnv) (host : HostTbl) (hostEnv : HostEnv α)
    (hsim : HostSimulation env host hostEnv) (hsorts : HostSorts env host)
    (code : CodeTbl) (self fuel : Nat) (c : WCode) (hc : code self = some c)
    (paramSorts : List STy) (result : STy) (body' : Program)
    (hty : HasTy env (paramSorts ++ List.replicate c.nlocals .ref) [] c.body [result])
    (htr : translateList env c.body = some body')
    (vs : List WVal) (hvs : Sorted env vs paramSorts)
    (store0 : Store α) (args : List Value) (hargs : Rs store0.gcHeap args vs)
    (w : WVal) (hrun : wFuncN code host fuel self vs = some w) :
    TerminatesWith
      (initialConfig (synthModule env paramSorts result c.nlocals body') hostEnv
        (synthFunction env paramSorts result c.nlocals body') store0 args)
      (fun values store' => ∃ v, values = [v] ∧ R store'.wasm.gcHeap v w) := by
  obtain ⟨trace, v, store', hsteps, hR⟩ := wFuncN_terminatesWith env host hostEnv hsim hsorts code
    self fuel c hc paramSorts result body' hty htr vs hvs store0 args hargs w hrun
  exact ⟨trace, [v], store', hsteps, v, rfl, hR⟩

end Bridge
