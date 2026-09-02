import Bridge.Env

/-!
# Translation of the profile `RecordComputeNonRecursiveV1` into Talos syntax

`translate env : WInstr → Option Instruction` is defined for exactly the
instructions `PlanLower.lowerExprFragmentBody` emits for compute-face plans
without `selfCall` (brief §3): `localGet`, `localSet`, `i64Const`, `i32Const`,
`structGet` (carrier limb and user struct alike — the wall's `WInstr` does not
distinguish them), `structNew`, `refIsNull`, `call` (host slots only, via the
environment's import table), `ifElse` (block type `[] → [t]`), and the nine
comparison primitives `i64Eq/LtS/LeS/GeS/GtS`, `i32Eq/LtS/GtS/GeS`. Everything
else — `returnCall`, `ret`, `refNull/refTest/refCast`, `arrayNewFixed/
arrayNewData/arrayLen/arrayGet`, `i32And/i32LtU/i32LeS`, `i64Eqz`, the f64
family, `f64Const` — is `none`.

Immediates: the wall's `i32Const n`/`i64Const n` carry an unbounded `Int`; a
value outside the machine band has no Talos counterpart, so the translation
refuses it (the plan checker never admits one). The block type of `ifElse` is
erased by the wall's `WInstr` (`PlanBytes` re-derives it from the plan node's
`FragTy` when it prints bytes); Talos's `Step.iff` reads only the two arities,
so the translation records `paramArity := 0`, `resultArity := 1` and leaves
the type lists as inert data.

## Typing

The wall's `wRunF` is untyped where Talos is not. The judgment `HasTy` is the
minimal stack discipline that closes the gap: locals and stack slots carry a
sort (`STy`), an `if` branch is typed from the EMPTY stack (wasm's own rule,
which is what makes `exitControl`'s `take resultArity ++ belowStack` agree
with the wall's flat branch run), and `localSet` needs a declared slot. On the
profile every lowered body is typed — that is the coverage lemma of brief §3,
which this spike does not prove (it takes `HasTy` as a hypothesis).
`typed_run` is the wall-side consequence the bridge consumes: a typed run
returns `.ok`, touches only the typed prefix of the stack, and preserves sorts.
-/

namespace Bridge
open Wasm CertPrelude

def i32Band (n : Int) : Prop := -2147483648 ≤ n ∧ n < 2147483648
def i64Band (n : Int) : Prop := -9223372036854775808 ≤ n ∧ n < 9223372036854775808

instance : DecidablePred i32Band := fun n => by unfold i32Band; infer_instance
instance : DecidablePred i64Band := fun n => by unfold i64Band; infer_instance

/-- The i32 bit pattern of an in-band integer. -/
def constI32 (n : Int) : UInt32 := (Int32.ofInt n).toUInt32
/-- The i64 bit pattern of an in-band integer. -/
def constI64 (n : Int) : UInt64 := (Int64.ofInt n).toUInt64

mutual
def translate (env : TranslateEnv) : WInstr → Option Instruction
  | .localGet i => some (.localGet i)
  | .localSet i => some (.localSet i)
  | .i64Const n => if i64Band n then some (.constI64 (constI64 n)) else none
  | .i32Const n => if i32Band n then some (.const (constI32 n)) else none
  | .structGet ty f => some (.gc (.structGet ty f))
  | .structNew ty nf =>
      match structSorts? env.structs ty with
      | some fs => if fs.length = nf then some (.gc (.structNew ty)) else none
      | none => none
  | .refIsNull => some .refIsNull
  | .call f => (slotLookup? env.imports f).map fun p => .call p.1
  | .ifElse tB eB =>
      match translateList env tB, translateList env eB with
      | some tB', some eB' => some (.iff 0 1 tB' eB' [] [.anyref])
      | _, _ => none
  | .i64Eq => some .eqI64
  | .i64LtS => some .ltSI64
  | .i64LeS => some .leSI64
  | .i64GeS => some .geSI64
  | .i64GtS => some .gtSI64
  | .i32Eq => some .eq
  | .i32LtS => some .ltS
  | .i32GtS => some .gtS
  | .i32GeS => some .geS
  | _ => none

def translateList (env : TranslateEnv) : List WInstr → Option Program
  | [] => some []
  | x :: xs =>
      match translate env x, translateList env xs with
      | some x', some xs' => some (x' :: xs')
      | _, _ => none
end

theorem translateList_cons {env : TranslateEnv} {x : WInstr} {xs : List WInstr} {code : Program}
    (h : translateList env (x :: xs) = some code) :
    ∃ x' xs', translate env x = some x' ∧ translateList env xs = some xs' ∧ code = x' :: xs' := by
  simp only [translateList] at h
  split at h
  · rename_i x' xs' hx hxs
    simp only [Option.some.injEq] at h
    exact ⟨x', xs', hx, hxs, h.symm⟩
  · simp at h

/-- The five i64 comparisons of the profile. -/
def i64Cmps : List WInstr := [.i64Eq, .i64LtS, .i64LeS, .i64GeS, .i64GtS]
/-- The four i32 comparisons of the profile. -/
def i32Cmps : List WInstr := [.i32Eq, .i32LtS, .i32GtS, .i32GeS]

/-- Stack typing of a profile instruction list: `HasTy env Γ σ is σ'` reads
    "with locals of sorts `Γ`, `is` takes a stack of sorts `σ` (head = top) to
    one of sorts `σ'`". -/
inductive HasTy (env : TranslateEnv) (Γ : List STy) : List STy → List WInstr → List STy → Prop where
  | nil {σ} : HasTy env Γ σ [] σ
  | localGet {σ σ' i t is} (h : Γ[i]? = some t)
      (rest : HasTy env Γ (t :: σ) is σ') : HasTy env Γ σ (.localGet i :: is) σ'
  | localSet {σ σ' i t is} (h : Γ[i]? = some t)
      (rest : HasTy env Γ σ is σ') : HasTy env Γ (t :: σ) (.localSet i :: is) σ'
  | i64Const {σ σ' n is} (hn : i64Band n)
      (rest : HasTy env Γ (.i64 :: σ) is σ') : HasTy env Γ σ (.i64Const n :: is) σ'
  | i32Const {σ σ' n is} (hn : i32Band n)
      (rest : HasTy env Γ (.i32 :: σ) is σ') : HasTy env Γ σ (.i32Const n :: is) σ'
  | structGet {σ σ' ty f fs t is} (hs : structSorts? env.structs ty = some fs) (hf : fs[f]? = some t)
      (rest : HasTy env Γ (t :: σ) is σ') : HasTy env Γ (.ref :: σ) (.structGet ty f :: is) σ'
  | structNew {σ σ' ty fs is} (hs : structSorts? env.structs ty = some fs)
      (rest : HasTy env Γ (.ref :: σ) is σ') :
      HasTy env Γ (fs.reverse ++ σ) (.structNew ty fs.length :: is) σ'
  | refIsNull {σ σ' is}
      (rest : HasTy env Γ (.i32 :: σ) is σ') : HasTy env Γ (.ref :: σ) (.refIsNull :: is) σ'
  | call {σ σ' f i sig is} (hs : slotLookup? env.imports f = some (i, sig))
      (rest : HasTy env Γ (sig.result :: σ) is σ') :
      HasTy env Γ (sig.params.reverse ++ σ) (.call f :: is) σ'
  | i64Cmp {σ σ' op is} (hop : op ∈ i64Cmps)
      (rest : HasTy env Γ (.i32 :: σ) is σ') : HasTy env Γ (.i64 :: .i64 :: σ) (op :: is) σ'
  | i32Cmp {σ σ' op is} (hop : op ∈ i32Cmps)
      (rest : HasTy env Γ (.i32 :: σ) is σ') : HasTy env Γ (.i32 :: .i32 :: σ) (op :: is) σ'
  | ifElse {σ σ' t tB eB is} (ht : HasTy env Γ [] tB [t]) (he : HasTy env Γ [] eB [t])
      (rest : HasTy env Γ (t :: σ) is σ') : HasTy env Γ (.i32 :: σ) (.ifElse tB eB :: is) σ'

/-- The wall's abstract host table agrees with the environment's import
    table: every import IS a wall host slot of the declared arity, and its
    contract returns a value of the declared sort on arguments of the declared
    sorts. (The wall's contracts give the second half for REPRESENTED
    operands; on the profile every argument of a host call is one.) -/
def HostSorts (env : TranslateEnv) (host : HostTbl) : Prop :=
  ∀ f i sig, slotLookup? env.imports f = some (i, sig) →
    ∃ hf, host f = some (sig.params.length, hf) ∧
      ∀ ws w, Sorted env ws sig.params → hf ws = some w → HasSort env w sig.result

/-! ## The wall side of a typed run -/

theorem popArgs_sorted {env : TranslateEnv} {st below : List WVal} {ts σ : List STy}
    (h : Sorted env st (ts ++ σ)) :
    popArgs ts.length (st ++ below) =
      some ((st.take ts.length).reverse, st.drop ts.length ++ below) ∧
      Sorted env (st.take ts.length).reverse ts.reverse ∧ Sorted env (st.drop ts.length) σ := by
  obtain ⟨st₁, st₂, rfl, h₁, h₂⟩ := Sorted_append_inv h
  have hlen : st₁.length = ts.length := Sorted_length h₁
  have hle : ts.length ≤ (st₁ ++ st₂ ++ below).length := by
    simp only [List.length_append]; omega
  refine ⟨?_, ?_, ?_⟩
  · rw [← hlen]
    have hnl : ¬ (st₁ ++ (st₂ ++ below)).length < st₁.length := by
      simp only [List.length_append]; omega
    simp only [popArgs, List.append_assoc, hnl, if_false, List.take_left, List.drop_left]
  · rw [← hlen, List.take_left]
    exact Sorted_reverse h₁
  · rw [← hlen, List.drop_left]
    exact h₂

/-- A typed run of the wall's interpreter: it yields `.ok`, leaves the stack
    beneath the typed prefix untouched, and preserves the sorts of locals and
    stack. Induction on the typing derivation, one interpreter arm per case. -/
theorem typed_run (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    {env : TranslateEnv} (hhost : HostSorts env host) {Γ : List STy} :
    ∀ {σ : List STy} {is : List WInstr} {σ' : List STy}, HasTy env Γ σ is σ' →
    ∀ (locA st below : List WVal) (out : Out),
      Sorted env locA Γ → Sorted env st σ →
      wRunF host ar callee is locA (st ++ below) = some out →
      ∃ locA' st', out = .ok locA' (st' ++ below) ∧ Sorted env locA' Γ ∧ Sorted env st' σ' := by
  intro σ is σ' hty
  induction hty with
  | nil =>
      intro locA st below out hΓ hσ hrun
      simp only [wRunF, Option.some.injEq] at hrun
      exact ⟨locA, st, hrun.symm, hΓ, hσ⟩
  | localGet h _ ih =>
      intro locA st below out hΓ hσ hrun
      obtain ⟨w, hw, hsort⟩ := Sorted_getElem? _ _ hΓ h
      simp only [wRunF, hw] at hrun
      exact ih locA (w :: st) below out hΓ (Sorted_cons hsort hσ) hrun
  | localSet h _ ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w :: st, hσ =>
        simp only [Sorted] at hσ
        simp only [wRunF, List.cons_append] at hrun
        exact ih (locA.set _ w) st below out (Sorted_set _ hΓ h hσ.1) hσ.2 hrun
  | @i64Const _ _ n _ _ _ ih =>
      intro locA st below out hΓ hσ hrun
      simp only [wRunF] at hrun
      exact ih locA (.i64v n :: st) below out hΓ (Sorted_cons (by simp [HasSort]) hσ) hrun
  | @i32Const _ _ n _ _ _ ih =>
      intro locA st below out hΓ hσ hrun
      simp only [wRunF] at hrun
      exact ih locA (.i32v n :: st) below out hΓ (Sorted_cons (by simp [HasSort]) hσ) hrun
  | structGet hs hf _ ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w :: st, hσ =>
        simp only [Sorted] at hσ
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
              exact ih locA (v :: st) below out hΓ (Sorted_cons hvsort hσ.2) hrun
            · simp at hrun
          · simp at hrun
        · simp [wRunF] at hrun
  | structNew hs _ ih =>
      intro locA st below out hΓ hσ hrun
      obtain ⟨hpop, hargs, hrest⟩ := popArgs_sorted (below := below) hσ
      rw [List.length_reverse] at hpop hargs hrest
      rw [List.reverse_reverse] at hargs
      simp only [wRunF, hpop] at hrun
      refine ih locA (.structv _ _ :: st.drop _) below out hΓ (Sorted_cons ?_ hrest) hrun
      exact ⟨_, hs, hargs⟩
  | refIsNull _ ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w :: st, hσ =>
        simp only [Sorted] at hσ
        rcases HasSort_ref hσ.1 with rfl | ⟨t', fs', rfl⟩ | ⟨t', es', rfl⟩
        · simp only [wRunF, List.cons_append] at hrun
          exact ih locA (b32 true :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2) hrun
        · simp only [wRunF, List.cons_append] at hrun
          exact ih locA (b32 false :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2) hrun
        · simp only [wRunF, List.cons_append] at hrun
          exact ih locA (b32 false :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2) hrun
  | call hs _ ih =>
      intro locA st below out hΓ hσ hrun
      obtain ⟨hf, hhf, hsort⟩ := hhost _ _ _ hs
      obtain ⟨hpop, hargs, hrest⟩ := popArgs_sorted (below := below) hσ
      rw [List.length_reverse] at hpop hargs hrest
      rw [List.reverse_reverse] at hargs
      simp only [wRunF, hhf, hpop] at hrun
      split at hrun
      · rename_i r hr
        exact ih locA (r :: st.drop _) below out hΓ (Sorted_cons (hsort _ _ hargs hr) hrest) hrun
      · simp at hrun
  | i64Cmp hop _ ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w₁ :: w₂ :: st, hσ =>
        simp only [Sorted] at hσ
        obtain ⟨b, rfl⟩ := HasSort_i64 hσ.1
        obtain ⟨a, rfl⟩ := HasSort_i64 hσ.2.1
        simp only [i64Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
        rcases hop with rfl | rfl | rfl | rfl | rfl <;>
          simp only [wRunF, List.cons_append] at hrun <;>
          exact ih locA (b32 _ :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2.2) hrun
  | i32Cmp hop _ ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w₁ :: w₂ :: st, hσ =>
        simp only [Sorted] at hσ
        obtain ⟨b, rfl⟩ := HasSort_i32 hσ.1
        obtain ⟨a, rfl⟩ := HasSort_i32 hσ.2.1
        simp only [i32Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
        rcases hop with rfl | rfl | rfl | rfl <;>
          simp only [wRunF, List.cons_append] at hrun <;>
          exact ih locA (b32 _ :: st) below out hΓ (Sorted_cons (HasSort_b32 _) hσ.2.2) hrun
  | ifElse _ _ _ iht ihe ih =>
      intro locA st below out hΓ hσ hrun
      match st, hσ with
      | w :: st, hσ =>
        simp only [Sorted] at hσ
        obtain ⟨c, rfl⟩ := HasSort_i32 hσ.1
        simp only [wRunF, List.cons_append] at hrun
        split at hrun
        · -- else branch
          split at hrun
          · rename_i l₁ st₁ hbr
            obtain ⟨l₂, st₂, heq, hΓ₂, hσ₂⟩ :=
              ihe locA [] (st ++ below) _ hΓ Sorted_nil (by simpa using hbr)
            simp only [Out.ok.injEq] at heq
            obtain ⟨hl, hst⟩ := heq
            subst hl; subst hst
            obtain ⟨v, rfl, hv⟩ := Sorted_singleton_inv hσ₂
            exact ih l₁ (v :: st) below out hΓ₂ (Sorted_cons hv hσ.2) (by simpa using hrun)
          · rename_i v hbr
            obtain ⟨_, _, heq, _, _⟩ :=
              ihe locA [] (st ++ below) _ hΓ Sorted_nil (by simpa using hbr)
            simp at heq
          · simp at hrun
        · -- then branch
          split at hrun
          · rename_i l₁ st₁ hbr
            obtain ⟨l₂, st₂, heq, hΓ₂, hσ₂⟩ :=
              iht locA [] (st ++ below) _ hΓ Sorted_nil (by simpa using hbr)
            simp only [Out.ok.injEq] at heq
            obtain ⟨hl, hst⟩ := heq
            subst hl; subst hst
            obtain ⟨v, rfl, hv⟩ := Sorted_singleton_inv hσ₂
            exact ih l₁ (v :: st) below out hΓ₂ (Sorted_cons hv hσ.2) (by simpa using hrun)
          · rename_i v hbr
            obtain ⟨_, _, heq, _, _⟩ :=
              iht locA [] (st ++ below) _ hΓ Sorted_nil (by simpa using hbr)
            simp at heq
          · simp at hrun

end Bridge
