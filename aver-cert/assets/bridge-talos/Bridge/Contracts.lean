import Bridge.EnvOfClaim

set_option autoImplicit false

/-!
# `HostSorts` from the wall's contracts (brief §9 (2)) and `Nodup` from the claim check (§9 (5))

The wall's `Obligation.holds` speaks about host results only for REPRESENTED
operands (`S.Repr a va`), and for `cmp`/`eq` only for CANONICAL ones
(`S.Canon va`). The bridge's sorts were refined to say exactly that
(`Env.lean`): a `.car` value is a represented canonical carrier, `box`'s
operand is a band literal. With that, `HostSorts` — every import of the
synthetic module is a wall host slot of the declared arity whose result is
well-sorted on well-sorted arguments — is a THEOREM about the compute face's
real host table (`StandardFace.recordComputeSlots`, verbatim in
`AverMin.lean`) from the five contract hypotheses copied verbatim
(`ComputeContracts`), given the claim check's distinct indices
(`hostTableIndicesDistinct`, the first conjunct of `StandardFace.hostTableBound`).

The `Nodup` that `hostRoleIdx?_slotLookup` needs (§9 (5)) is DERIVED from
that same conjunct: `hostTableIndicesDistinct` IS `natListNoDup` of the index
column, and `natListNoDup` implies `List.Nodup`. So it is a claim-check
hypothesis the wall already imposes on every accepted compute claim — nothing
new is asked of the certificate.
-/

namespace Bridge
open CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.StandardFace RecordComputeBridge
open AverCert.Schema (CarrierSpec)

/-! ## Item 5: distinct host indices are a claim-check fact -/

theorem natListNoDup_nodup : ∀ {l : List Nat}, natListNoDup l = true → l.Nodup
  | [], _ => List.nodup_nil
  | n :: rest, h => by
      simp only [natListNoDup, Bool.and_eq_true, Bool.not_eq_true'] at h
      rw [List.nodup_cons]
      refine ⟨fun hm => ?_, natListNoDup_nodup h.2⟩
      have hc : rest.contains n = true := List.contains_iff_mem.mpr hm
      rw [hc] at h
      exact Bool.noConfusion h.1

/-- The first conjunct of `StandardFace.hostTableBound` is the `Nodup` the
    host half of `envOfClaim` needs. -/
theorem hostTableIndicesDistinct_nodup {ht : List (HostRole × Nat)}
    (h : hostTableIndicesDistinct ht = true) : (ht.map Prod.snd).Nodup :=
  natListNoDup_nodup h

theorem hostTableBound_nodup {roles : CertDecode.AddSub.Roles} {ht : List (HostRole × Nat)}
    (h : hostTableBound roles ht = true) : (ht.map Prod.snd).Nodup := by
  simp only [hostTableBound, Bool.and_eq_true] at h
  exact hostTableIndicesDistinct_nodup h.1

/-! ## Item 2: the compute face's host table, slot by slot -/

/-- With distinct indices, the slot the wall wires at the index of the `i`-th
    table entry is that entry's role: the role's arity and contract function
    (`RecordComputeBridge.roleArity`/`roleFn`; `box` is the audited
    `boxRef`, `toIndex` the trap-only slot). -/
theorem recordComputeSlots_getElem (C : Nat) (add sub mul cmp eq : List WVal → Option WVal) :
    ∀ (ht : List (HostRole × Nat)) (i : Nat) (role : HostRole) (f : Nat),
      (ht.map Prod.snd).Nodup → ht[i]? = some (role, f) →
      recordComputeSlots C add sub mul cmp eq ht f =
        some (roleArity role, roleFn (boxRef C) add sub mul cmp eq role)
  | [], i, _, _, _, h => by simp at h
  | (r, idx) :: rest, 0, role, f, _, h => by
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp only [recordComputeSlots, if_true]
      cases r <;> rfl
  | (r, idx) :: rest, i + 1, role, f, hnd, h => by
      simp only [List.getElem?_cons_succ] at h
      simp only [List.map_cons, List.nodup_cons] at hnd
      have hmem : f ∈ rest.map Prod.snd := List.mem_map.mpr ⟨(role, f), List.mem_of_getElem? h, rfl⟩
      have hne : f ≠ idx := fun heq => hnd.1 (heq ▸ hmem)
      simp only [recordComputeSlots, hne, if_false]
      exact recordComputeSlots_getElem C add sub mul cmp eq rest i role f hnd.2 h

/-- Brief §9 (2). The refined sorts make the wall's contract domains and the
    bridge's sorted arguments coincide: `box` on a band literal returns
    `carrierSmall C k`, represented (`smallIntro`) and canonical
    (`canonSmall`); `add`/`sub`/`mul` on two canonical carriers return a
    represented canonical word (`_hadd`/`_hsub`/`_hmul`); `cmp`/`eq` on two
    canonical carriers return an `i32` (`_hCmp`/`_hEq`); `toIndex` has no
    admitted call and its slot never returns. -/
theorem HostSorts_of_contracts (ht : List (HostRole × Nat)) (C : Nat) (decls : List TypeDecl)
    (S : CarrierSpec C) (add sub mul cmp eq : List WVal → Option WVal)
    (hc : ComputeContracts S add sub mul cmp eq)
    (hnd : hostTableIndicesDistinct ht = true) :
    HostSorts (envOfClaim ht C decls) S (recordComputeSlots C add sub mul cmp eq ht) := by
  intro f i sig hslot
  obtain ⟨role, hi, rfl⟩ := envOfClaim_import_role ht C decls f i sig hslot
  have hnd' := hostTableIndicesDistinct_nodup hnd
  refine ⟨roleFn (boxRef C) add sub mul cmp eq role, ?_, ?_⟩
  · rw [recordComputeSlots_getElem C add sub mul cmp eq ht i role f hnd' hi]
    cases role <;> rfl
  · intro ws w hws hfw
    cases role
    · -- box: a band literal boxes to a canonical small carrier.
      simp only [roleSig] at hws ⊢
      obtain ⟨v, rfl, hv⟩ := Sorted_singleton_inv hws
      obtain ⟨k, rfl, hk⟩ := HasSort_i64b hv
      simp only [roleFn, boxRef, Option.some.injEq] at hfw
      subst hfw
      exact HasSort_of_canonRepr (S.smallIntro k) ((S.canonSmall k).mpr ((i64Band_iff k).mp hk))
    · -- add
      simp only [roleSig] at hws ⊢
      obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
      obtain ⟨a, hRa, -⟩ := HasSort_car hva
      obtain ⟨b, hRb, -⟩ := HasSort_car hvb
      obtain ⟨hR, hC⟩ := hc._hadd a b va vb w hRa hRb hfw
      exact HasSort_of_canonRepr hR hC
    · -- mul
      simp only [roleSig] at hws ⊢
      obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
      obtain ⟨a, hRa, -⟩ := HasSort_car hva
      obtain ⟨b, hRb, -⟩ := HasSort_car hvb
      obtain ⟨hR, hC⟩ := hc._hmul a b va vb w hRa hRb hfw
      exact HasSort_of_canonRepr hR hC
    · -- sub
      simp only [roleSig] at hws ⊢
      obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
      obtain ⟨a, hRa, -⟩ := HasSort_car hva
      obtain ⟨b, hRb, -⟩ := HasSort_car hvb
      obtain ⟨hR, hC⟩ := hc._hsub a b va vb w hRa hRb hfw
      exact HasSort_of_canonRepr hR hC
    · -- toIndex: the trap-only slot never returns.
      simp [roleFn] at hfw
    · -- cmp
      simp only [roleSig] at hws ⊢
      obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
      obtain ⟨a, hRa, hCa⟩ := HasSort_car hva
      obtain ⟨b, hRb, hCb⟩ := HasSort_car hvb
      rw [hc._hCmp a b va vb w hRa hRb hCa hCb hfw]
      simp [HasSort]
    · -- eq
      simp only [roleSig] at hws ⊢
      obtain ⟨va, vb, rfl, hva, hvb⟩ := Sorted_pair_inv hws
      obtain ⟨a, hRa, hCa⟩ := HasSort_car hva
      obtain ⟨b, hRb, hCb⟩ := HasSort_car hvb
      rw [hc._hEq a b va vb w hRa hRb hCa hCb hfw]
      simp [HasSort]

end Bridge
