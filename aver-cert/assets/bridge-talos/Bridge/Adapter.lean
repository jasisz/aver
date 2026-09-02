import Bridge.Contracts
import Bridge.Instr

set_option autoImplicit false

/-!
# A concrete Talos host: the adapter that reifies the wall's abstract results (brief §9 (3), §4.3)

`HostSimulation` (Config.lean) is the theorem's interface; this file gives a
Talos `HostEnv` that satisfies it for ANY wall host table whose results on
sorted arguments are machine-shaped — in particular the compute face's
`recordComputeSlots` under the wall's contracts.

The adapter's `invoke`, per slot: read the arguments back into wall values
along their SORTS (`readArg`: an `i32`/`i64` word is its integer, a `.car`
argument is the carrier struct read off the heap, its limb array read as
`i32` words), apply the abstract contract function `hf`, and if it returns
`some w`, REIFY `w` into the heap (`reify`: numbers become words, structs and
arrays are allocated bottom-up, the old heap kept as a prefix) and return the
resulting Talos value. Trap where the contract is undefined or an argument is
unreadable — `HostSimulation` says nothing there.

Two premises beyond the wall's contracts, both about the REPRESENTATION, not
the plan or the bytes:

* `CarrierMachine S` — the carrier specification's words are wasm words:
  the `i64` small field and the `i32` sign are in band, the limb field is
  `null` or an array of in-band `i32` words. `CarrierSpec.car` fixes the
  three-field shape but says nothing about the band of the fields or the
  element type of the limb array; the runtime's representation satisfies it
  (`wat/*.wat`: limbs are `(array (mut i32))`). Without it no Talos value
  relates to a represented word with, say, an out-of-band small field, and
  no host at all could simulate the abstract table.
* the results of the wall's slots are machine-shaped (`Machine`): derived
  for the compute face from the contracts (`add/sub/mul` results are
  represented, hence machine words by `CarrierMachine`; `cmp/eq` return
  `cmpW`/`eqW ∈ {-1, 0, 1}`; `box` returns `carrierSmall C k` for a band `k`).
-/

namespace Bridge
open Wasm CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.StandardFace RecordComputeBridge
open AverCert.Schema (CarrierSpec)

/-! ## Machine-shaped wall values -/

/-- A wall value whose numeric leaves are all machine words: exactly the
    values some Talos value can relate to. -/
inductive Machine : WVal → Prop where
  | i32 {n : Int} (h : i32Band n) : Machine (.i32v n)
  | i64 {n : Int} (h : i64Band n) : Machine (.i64v n)
  | f64 (b : UInt64) : Machine (.f64v b)
  | null : Machine .null
  | structv (t : Nat) {fs : List WVal} (h : ∀ w ∈ fs, Machine w) : Machine (.structv t fs)
  | arr (t : Nat) {es : List WVal} (h : ∀ w ∈ es, Machine w) : Machine (.arr t es)

/-- The carrier specification's words are wasm words. -/
def CarrierMachine {C : Nat} (S : CarrierSpec C) : Prop :=
  ∀ n w, S.Repr n w →
    ∃ s l sg, w = .structv C [.i64v s, l, .i32v sg] ∧ i64Band s ∧ i32Band sg ∧
      (l = .null ∨ ∃ lt les, l = .arr lt les ∧ ∀ e ∈ les, ∃ k, e = .i32v k ∧ i32Band k)

theorem Machine_of_repr {C : Nat} {S : CarrierSpec C} (hmw : CarrierMachine S) {n : Int} {w : WVal}
    (hR : S.Repr n w) : Machine w := by
  obtain ⟨s, l, sg, rfl, hs, hsg, hl⟩ := hmw n w hR
  refine .structv C fun x hx => ?_
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hx
  rcases hx with rfl | rfl | rfl
  · exact .i64 hs
  · rcases hl with rfl | ⟨lt, les, rfl, hles⟩
    · exact .null
    · refine .arr lt fun e he => ?_
      obtain ⟨k, rfl, hk⟩ := hles e he
      exact .i32 hk
  · exact .i32 hsg

theorem Machine_carrierSmall (C : Nat) {k : Int} (hk : i64Band k) : Machine (carrierSmall C k) := by
  refine .structv C fun x hx => ?_
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hx
  rcases hx with rfl | rfl | rfl
  · exact .i64 hk
  · exact .null
  · exact .i32 (by simp [i32Band])

theorem Machine_cmpW (a b : Int) : Machine (.i32v (cmpW a b)) := by
  refine .i32 ?_
  unfold cmpW i32Band
  split <;> (try split) <;> simp

theorem Machine_eqW (a b : Int) : Machine (.i32v (eqW a b)) := by
  refine .i32 ?_
  unfold eqW i32Band
  split <;> simp

/-! ## Reification -/

mutual
def reify (heap : List GcObject) : WVal → Value × List GcObject
  | .i32v n => (.i32 (constI32 n), heap)
  | .i64v n => (.i64 (constI64 n), heap)
  | .f64v b => (.f64 b, heap)
  | .null => (.anyref none, heap)
  | .structv t fs =>
      (.anyref (some (.struct (reifyList heap fs).2.length)),
        (reifyList heap fs).2 ++ [.struct t (reifyList heap fs).1])
  | .arr t es =>
      (.anyref (some (.array (reifyList heap es).2.length)),
        (reifyList heap es).2 ++ [.array t (reifyList heap es).1])

def reifyList (heap : List GcObject) : List WVal → List Value × List GcObject
  | [] => ([], heap)
  | w :: ws =>
      ((reify heap w).1 :: (reifyList (reify heap w).2 ws).1, (reifyList (reify heap w).2 ws).2)
end

mutual
theorem reify_spec (heap : List GcObject) :
    ∀ (w : WVal), Machine w → heap <+: (reify heap w).2 ∧ R (reify heap w).2 (reify heap w).1 w
  | .i32v n, h => by
      cases h with
      | i32 hn => exact ⟨List.prefix_refl _, constI32_repr hn⟩
  | .i64v n, h => by
      cases h with
      | i64 hn => exact ⟨List.prefix_refl _, constI64_repr hn⟩
  | .f64v b, _ => ⟨List.prefix_refl _, rfl⟩
  | .null, _ => ⟨List.prefix_refl _, trivial⟩
  | .structv t fs, h => by
      cases h with
      | structv _ hfs =>
        obtain ⟨hpre, hRs⟩ := reifyList_spec heap fs hfs
        simp only [reify]
        refine ⟨hpre.trans (List.prefix_append _ _), ?_⟩
        simp only [R]
        refine ⟨_, ?_, Rs_append _ _ _ _ hRs⟩
        rw [List.getElem?_append_right (Nat.le_refl _), Nat.sub_self]
        rfl
  | .arr t es, h => by
      cases h with
      | arr _ hes =>
        obtain ⟨hpre, hRs⟩ := reifyList_spec heap es hes
        simp only [reify]
        refine ⟨hpre.trans (List.prefix_append _ _), ?_⟩
        simp only [R]
        refine ⟨_, ?_, Rs_append _ _ _ _ hRs⟩
        rw [List.getElem?_append_right (Nat.le_refl _), Nat.sub_self]
        rfl

theorem reifyList_spec (heap : List GcObject) :
    ∀ (ws : List WVal), (∀ w ∈ ws, Machine w) →
      heap <+: (reifyList heap ws).2 ∧ Rs (reifyList heap ws).2 (reifyList heap ws).1 ws
  | [], _ => ⟨List.prefix_refl _, Rs_nil⟩
  | w :: ws, h => by
      obtain ⟨hpre₁, hR⟩ := reify_spec heap w (h w List.mem_cons_self)
      obtain ⟨hpre₂, hRs⟩ :=
        reifyList_spec (reify heap w).2 ws (fun x hx => h x (List.mem_cons_of_mem _ hx))
      simp only [reifyList]
      exact ⟨hpre₁.trans hpre₂, Rs_cons (R_prefix hpre₂ hR) hRs⟩
end

/-! ## Reading arguments back along their sorts -/

def readI32 : Value → Option WVal
  | .i32 u => some (.i32v u.toInt32.toInt)
  | _ => none

/-- Read one argument of the given sort. `.ref` arguments (never a host
    argument of the profile: every role's parameters are `.car`/`.i64b`) and
    anything off-shape read as `none`. -/
def readArg (heap : List GcObject) : STy → Value → Option WVal
  | .i32, .i32 u => some (.i32v u.toInt32.toInt)
  | .i64, .i64 u => some (.i64v u.toInt64.toInt)
  | .i64b, .i64 u => some (.i64v u.toInt64.toInt)
  | .f64, .f64 b => some (.f64v b)
  | .car, .anyref (some (.struct a)) =>
      match heap[a]? with
      | some (.struct t [.i64 s, .anyref none, .i32 sg]) =>
          some (.structv t [.i64v s.toInt64.toInt, .null, .i32v sg.toInt32.toInt])
      | some (.struct t [.i64 s, .anyref (some (.array la)), .i32 sg]) =>
          match heap[la]? with
          | some (.array lt elems) =>
              (elems.mapM readI32).map fun es =>
                .structv t [.i64v s.toInt64.toInt, .arr lt es, .i32v sg.toInt32.toInt]
          | _ => none
      | _ => none
  | _, _ => none

def readArgs (heap : List GcObject) : List STy → List Value → Option (List WVal)
  | [], [] => some []
  | t :: ts, v :: vs =>
      match readArg heap t v, readArgs heap ts vs with
      | some w, some ws => some (w :: ws)
      | _, _ => none
  | _, _ => none

theorem Rs_cons_inv {heap : List GcObject} {vs : List Value} {w : WVal} {ws : List WVal}
    (h : Rs heap vs (w :: ws)) : ∃ v vs', vs = v :: vs' ∧ R heap v w ∧ Rs heap vs' ws := by
  match vs, h with
  | v :: vs', h =>
    simp only [Rs] at h
    exact ⟨v, vs', rfl, h.1, h.2⟩
  | [], h => simp [Rs] at h

theorem Rs_nil_inv {heap : List GcObject} {vs : List Value} (h : Rs heap vs []) : vs = [] := by
  match vs, h with
  | [], _ => rfl
  | _ :: _, h => simp [Rs] at h

theorem R_f64v {heap : List GcObject} {v : Value} {b : UInt64} (h : R heap v (.f64v b)) :
    v = .f64 b := by
  cases v with
  | f64 b' => simp only [R] at h; rw [h]
  | anyref r =>
    cases r with
    | none => simp [R] at h
    | some r => cases r <;> simp [R] at h
  | i32 _ => simp [R] at h
  | i64 _ => simp [R] at h
  | f32 _ => simp [R] at h
  | funcref _ => simp [R] at h
  | externref _ => simp [R] at h
  | v128 _ => simp [R] at h
  | exnref _ => simp [R] at h

theorem HasSort_f64 {env : TranslateEnv} {S : CarrierSpec env.carrier} {w : WVal}
    (h : HasSort env S w .f64) : ∃ b, w = .f64v b := by
  cases w <;> simp [HasSort] at h ⊢

/-- The limb words of a machine carrier read back as themselves. -/
theorem mapM_readI32 {heap : List GcObject} :
    ∀ {vs : List Value} {les : List WVal}, Rs heap vs les →
      (∀ e ∈ les, ∃ k, e = .i32v k ∧ i32Band k) → vs.mapM readI32 = some les
  | [], [], _, _ => rfl
  | v :: vs, e :: les, h, hles => by
      simp only [Rs] at h
      obtain ⟨k, rfl, -⟩ := hles e List.mem_cons_self
      obtain ⟨u, rfl, hk⟩ := R_i32v h.1
      have ih := mapM_readI32 h.2 (fun x hx => hles x (List.mem_cons_of_mem _ hx))
      simp [List.mapM_cons, readI32, ih, hk]
  | [], _ :: _, h, _ => by simp [Rs] at h
  | _ :: _, [], h, _ => by simp [Rs] at h

/-- A sorted, related argument of a non-`.ref` sort reads back exactly. -/
theorem readArg_of_R {env : TranslateEnv} {S : CarrierSpec env.carrier} (hmw : CarrierMachine S)
    {heap : List GcObject} {t : STy} (ht : t ≠ .ref) {w : WVal} {v : Value}
    (hs : HasSort env S w t) (hR : R heap v w) : readArg heap t v = some w := by
  cases t with
  | i32 =>
    obtain ⟨n, rfl⟩ := HasSort_i32 hs
    obtain ⟨u, rfl, rfl⟩ := R_i32v hR
    rfl
  | i64 =>
    obtain ⟨n, rfl⟩ := HasSort_i64 hs
    obtain ⟨u, rfl, rfl⟩ := R_i64v hR
    rfl
  | i64b =>
    obtain ⟨n, rfl, -⟩ := HasSort_i64b hs
    obtain ⟨u, rfl, rfl⟩ := R_i64v hR
    rfl
  | f64 =>
    obtain ⟨b, rfl⟩ := HasSort_f64 hs
    obtain rfl := R_f64v hR
    rfl
  | ref => exact absurd rfl ht
  | car =>
    obtain ⟨n, hRepr, -⟩ := HasSort_car hs
    obtain ⟨s, l, sg, rfl, -, -, hl⟩ := hmw n w hRepr
    obtain ⟨a, vs, rfl, hget, hvs⟩ := R_structv hR
    obtain ⟨v₀, vs₁, rfl, h₀, hvs⟩ := Rs_cons_inv hvs
    obtain ⟨v₁, vs₂, rfl, h₁, hvs⟩ := Rs_cons_inv hvs
    obtain ⟨v₂, vs₃, rfl, h₂, hvs⟩ := Rs_cons_inv hvs
    obtain rfl := Rs_nil_inv hvs
    obtain ⟨u₀, rfl, rfl⟩ := R_i64v h₀
    obtain ⟨u₂, rfl, rfl⟩ := R_i32v h₂
    rcases hl with rfl | ⟨lt, les, rfl, hles⟩
    · obtain rfl := R_null h₁
      simp [readArg, hget]
    · obtain ⟨la, evs, rfl, hla, hevs⟩ := R_arr h₁
      simp [readArg, hget, hla, mapM_readI32 hevs hles]

theorem readArgs_of_Rs {env : TranslateEnv} {S : CarrierSpec env.carrier} (hmw : CarrierMachine S)
    {heap : List GcObject} :
    ∀ {ts : List STy} {ws : List WVal} {vs : List Value}, (∀ t ∈ ts, t ≠ .ref) →
      Sorted env S ws ts → Rs heap vs ws → readArgs heap ts vs = some ws
  | [], [], [], _, _, _ => rfl
  | t :: ts, w :: ws, v :: vs, hne, hs, hR => by
      simp only [Sorted] at hs
      simp only [Rs] at hR
      have h₁ := readArg_of_R hmw (hne t List.mem_cons_self) hs.1 hR.1
      have h₂ := readArgs_of_Rs hmw (fun x hx => hne x (List.mem_cons_of_mem _ hx)) hs.2 hR.2
      simp [readArgs, h₁, h₂]
  | [], _ :: _, _, _, hs, _ => by simp [Sorted] at hs
  | _ :: _, [], _, _, hs, _ => by simp [Sorted] at hs
  | _ :: _, _ :: _, [], _, _, hR => by simp [Rs] at hR

/-! ## The adapter -/

/-- The trap-only resolver for a slot the wall's table does not wire. -/
def trapFn {α : Type} : HostFn α :=
  { params := [], results := [], invoke := fun st _ => .Trap st "aver adapter: slot not wired" }

/-- One slot: read the arguments along the signature, apply the contract
    function, reify the result. -/
def adapterFn {α : Type} (sig : ImportSig) (hf : List WVal → Option WVal) : HostFn α :=
  { params := sig.params.map valueTypeOf
    results := [valueTypeOf sig.result]
    invoke := fun st args =>
      match readArgs st.gcHeap sig.params args with
      | some ws =>
          match hf ws with
          | some w => .Return [(reify st.gcHeap w).1] { st with gcHeap := (reify st.gcHeap w).2 }
          | none => .Trap st "aver adapter: contract undefined on these arguments"
      | none => .Trap st "aver adapter: unreadable argument" }

/-- The host environment: positional over the environment's imports, each
    slot wired to the wall's table entry at that slot's function index. -/
def adapterEnv (α : Type) (env : TranslateEnv) (host : HostTbl) : HostEnv α :=
  { funcs := env.imports.map fun sig =>
      match host sig.slot with
      | some (_, hf) => adapterFn sig hf
      | none => trapFn }

/-- Brief §9 (3): the adapter simulates any wall host table whose results on
    sorted arguments are machine-shaped, over an environment whose imports
    take no `.ref` argument. -/
theorem HostSimulation_adapter {α : Type} (env : TranslateEnv) (S : CarrierSpec env.carrier)
    (host : HostTbl) (hmw : CarrierMachine S)
    (hparams : ∀ (i : Nat) (sig : ImportSig), env.imports[i]? = some sig → ∀ t ∈ sig.params, t ≠ .ref)
    (hres : ∀ (f i : Nat) (sig : ImportSig) (hf : List WVal → Option WVal),
      slotLookup? env.imports f = some (i, sig) → host f = some (sig.params.length, hf) →
      ∀ ws w, Sorted env S ws sig.params → hf ws = some w → Machine w) :
    HostSimulation env S host (adapterEnv α env host) where
  resolved := by
    intro i sig hsig
    simp only [adapterEnv, List.getElem?_map, hsig, Option.map_some]
    exact ⟨_, rfl⟩
  invoke := by
    intro f i sig hf hfn hslot hhost hfn_eq st args ws w hws hRs hfw
    obtain ⟨hsig, hslotf⟩ := slotLookup?_getElem hslot
    have hfn' : hfn = adapterFn sig hf := by
      simp only [adapterEnv, List.getElem?_map, hsig, Option.map_some, Option.some.injEq] at hfn_eq
      rw [hslotf, hhost] at hfn_eq
      exact hfn_eq.symm
    subst hfn'
    have hread := readArgs_of_Rs hmw (hparams i sig hsig) hws hRs
    have hmach := hres f i sig hf hslot hhost ws w hws hfw
    obtain ⟨hpre, hR⟩ := reify_spec st.gcHeap w hmach
    refine ⟨(reify st.gcHeap w).1, { st with gcHeap := (reify st.gcHeap w).2 }, ?_, hpre, hR⟩
    simp [adapterFn, hread, hfw]

/-! ## The compute face's host, concretely -/

theorem envOfClaim_params_ne_ref (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl)
    (i : Nat) (sig : ImportSig) (h : (envOfClaim ht C decls).imports[i]? = some sig) :
    ∀ t ∈ sig.params, t ≠ .ref := by
  simp only [envOfClaim, List.getElem?_map] at h
  match hht : ht[i]? with
  | none => simp [hht] at h
  | some (role, f) =>
    simp only [hht, Option.map_some, Option.some.injEq] at h
    subst h
    cases role <;> simp [roleSig]

/-- The results of the compute face's slots on sorted arguments are machine
    words: `add/sub/mul` by the contracts and `CarrierMachine`, `cmp/eq` by
    their exact conclusions, `box` by the band of its literal. -/
theorem recordComputeSlots_machine (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl)
    (S : CarrierSpec C) (add sub mul cmp eq : List WVal → Option WVal)
    (hc : ComputeContracts S add sub mul cmp eq) (hmw : CarrierMachine S)
    (hnd : hostTableIndicesDistinct ht = true) :
    ∀ (f i : Nat) (sig : ImportSig) (hf : List WVal → Option WVal),
      slotLookup? (envOfClaim ht C decls).imports f = some (i, sig) →
      recordComputeSlots C add sub mul cmp eq ht f = some (sig.params.length, hf) →
      ∀ ws w, Sorted (envOfClaim ht C decls) S ws sig.params → hf ws = some w → Machine w := by
  intro f i sig hf hslot hhost ws w hws hfw
  obtain ⟨role, hi, rfl⟩ := envOfClaim_import_role ht C decls f i sig hslot
  rw [recordComputeSlots_getElem C add sub mul cmp eq ht i role f (hostTableIndicesDistinct_nodup hnd) hi]
    at hhost
  simp only [Option.some.injEq, Prod.mk.injEq] at hhost
  obtain ⟨-, rfl⟩ := hhost
  cases role
  · simp only [roleSig] at hws
    obtain ⟨v, rfl, hv⟩ := Sorted_singleton_inv hws
    obtain ⟨k, rfl, hk⟩ := HasSort_i64b hv
    simp only [roleFn, boxRef, Option.some.injEq] at hfw
    subst hfw
    exact Machine_carrierSmall C hk
  · simp only [roleSig] at hws
    obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
    obtain ⟨a, hRa, -⟩ := HasSort_car hva
    obtain ⟨b, hRb, -⟩ := HasSort_car hvb
    exact Machine_of_repr hmw (hc._hadd a b va vb w hRa hRb hfw).1
  · simp only [roleSig] at hws
    obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
    obtain ⟨a, hRa, -⟩ := HasSort_car hva
    obtain ⟨b, hRb, -⟩ := HasSort_car hvb
    exact Machine_of_repr hmw (hc._hmul a b va vb w hRa hRb hfw).1
  · simp only [roleSig] at hws
    obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
    obtain ⟨a, hRa, -⟩ := HasSort_car hva
    obtain ⟨b, hRb, -⟩ := HasSort_car hvb
    exact Machine_of_repr hmw (hc._hsub a b va vb w hRa hRb hfw).1
  · simp [roleFn] at hfw
  · simp only [roleSig] at hws
    obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
    obtain ⟨a, hRa, hCa⟩ := HasSort_car hva
    obtain ⟨b, hRb, hCb⟩ := HasSort_car hvb
    rw [hc._hCmp a b va vb w hRa hRb hCa hCb hfw]
    exact Machine_cmpW a b
  · simp only [roleSig] at hws
    obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
    obtain ⟨a, hRa, hCa⟩ := HasSort_car hva
    obtain ⟨b, hRb, hCb⟩ := HasSort_car hvb
    rw [hc._hEq a b va vb w hRa hRb hCa hCb hfw]
    exact Machine_eqW a b

/-- Brief §9 (3), the instance: the adapter over the compute face's real host
    table simulates it under the wall's contract hypotheses, `CarrierMachine`
    and distinct indices. -/
theorem HostSimulation_recordCompute (α : Type) (ht : List (HostRole × Nat)) (C : Nat)
    (decls : List TypeDecl) (S : CarrierSpec C) (add sub mul cmp eq : List WVal → Option WVal)
    (hc : ComputeContracts S add sub mul cmp eq) (hmw : CarrierMachine S)
    (hnd : hostTableIndicesDistinct ht = true) :
    HostSimulation (envOfClaim ht C decls) S (recordComputeSlots C add sub mul cmp eq ht)
      (adapterEnv α (envOfClaim ht C decls) (recordComputeSlots C add sub mul cmp eq ht)) :=
  HostSimulation_adapter (envOfClaim ht C decls) S _ hmw (envOfClaim_params_ne_ref ht C decls)
    (recordComputeSlots_machine ht C decls S add sub mul cmp eq hc hmw hnd)

end Bridge
