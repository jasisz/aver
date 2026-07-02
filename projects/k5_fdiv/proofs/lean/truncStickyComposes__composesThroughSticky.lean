-- HAND PROOF (labeled, kernel-checked) for truncStickyComposes.composesThroughSticky.
--
-- GAP CLASS: floor / power-of-two composition. The RTL trunc-sticky rung of Lemma
-- 7.3.2 (Sticky-Plus, round-to-odd form): trunc(sticky(x,n),m) = trunc(x,m) for m < n.
--
-- SHAPE FINGERPRINT: after unfolding fpTrunc/fpSticky and cross-multiplying the
-- rational value equality, both significands reduce to the nested Euclidean floor
--   floor((2*floor(s*2^(n-2) / 2^(width-1)) + b) * 2^(m-1) / 2^(n-1))
--     = floor(s*2^(m-1) / 2^(width-1)),   b in {0,1}
-- i.e. a DOUBLED round-to-odd low bit coarsened away by a power-of-two floor. The
-- generic exact-cancel reaches every other rounding composition but not this one
-- (the doubled bit needs the absorb-remainder step). No honest claim-shape strategy
-- covers it yet, so it lives here as a labeled hand proof rather than a per-figure
-- figure pretending to be generic.
--
-- RETRY TRIGGER: graduate to the equality-saturation normalizer platform when it
-- lands (measured feasible -- the same identity connects in an e-graph in ~253us
-- with explanation extraction). At that point this sidecar should be deleted and
-- the law re-earn `auto` credit.
--
-- COMPOSITION BY CITATION: the proof does NOT re-prove the powers-of-two or the
-- nested-floor collapse. It CITES the proven pool laws emitted earlier in the same
-- module:
--   * pow2_law_homomorphism          (fprep)  -- 2^(p+q) = 2^p * 2^q
--   * pow2_law_positive              (fprep)  -- 2^k >= 1
--   * floorDiv_law_nestedFloorCollapse (round) -- floor(floor a d) e = floor a (d*e)
-- Deleting the fprep homomorphism pool law breaks this sidecar (the `hom` cite
-- fails to resolve). The two shape-only floor facts the #625 cancel-common-factor
-- and absorb-remainder strategies close (floor(a*c, d*c) = floor(a, d) via
-- Int.mul_ediv_mul_of_pos_left; floor(2*q+r, 2) = q via Int.add_mul_ediv_left) are
-- discharged over their core lemmas. Pure core: axioms within {propext,
-- Classical.choice, Quot.sound}. No Mathlib, no sorry.
  -- 2^(p+q) = 2^p * 2^q -- CITES the fprep pool homomorphism law (revert-test target).
  have hom : ∀ p q : Int, 0 ≤ p → 0 ≤ q →
      Domain.Fprep.pow2 (p + q) = Domain.Fprep.pow2 p * Domain.Fprep.pow2 q := by
    intro p q hp hq
    apply pow2_law_homomorphism p q
    simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le]
    exact ⟨hp, hq⟩
  -- 2^k >= 1 -- CITES the fprep pool positivity law.
  have ppos : ∀ k : Int, 1 ≤ Domain.Fprep.pow2 k := by
    intro k
    have h := pow2_law_positive k
    simpa only [ge_iff_le, eq_iff_iff, iff_true] using h
  have pow_pos : ∀ k : Int, 0 < Domain.Fprep.pow2 k := by
    intro k; have := ppos k; omega
  -- One-step definitional unfold of the power-of-two recursion (NOT the homomorphism).
  have pow_of_pos : ∀ k : Int, ¬ k ≤ 0 →
      Domain.Fprep.pow2 k = 2 * Domain.Fprep.pow2 (k - 1) := by
    intro k h; rw [Domain.Fprep.pow2.eq_def, if_neg h]
  -- floorDiv reduces to Int ediv on a positive divisor (definition unfold).
  have peel : ∀ a d : Int, 0 < d → floorDiv a d = a / d := by
    intro a d hd
    have hne : ¬((d == 0) = true) := by simp only [beq_iff_eq]; omega
    simp only [floorDiv]
    rw [if_neg hne]
    simp only [Except.withDefault]
  -- cancel a common positive factor -- the #625 cancel-common-factor core lemma.
  have cancel : ∀ a c d : Int, 0 < d → 0 < c →
      floorDiv (a * c) (d * c) = floorDiv a d := by
    intro a c d hd hc
    rw [peel (a * c) (d * c) (Int.mul_pos hd hc), peel a d hd]
    exact Int.mul_ediv_mul_of_pos_left a d hc
  -- absorb a low bit under a doubled divisor -- the #625 absorb-remainder core lemmas.
  have absorb2 : ∀ q r : Int, 0 ≤ r → r < 2 → floorDiv (2 * q + r) 2 = q := by
    intro q r h0 hr
    rw [peel (2 * q + r) 2 (by omega), show 2 * q + r = r + 2 * q by omega,
        Int.add_mul_ediv_left r q (by omega : (2 : Int) ≠ 0), Int.ediv_eq_zero_of_lt h0 hr]
    omega
  -- nested-floor collapse -- CITES the round pool law floorDiv_law_nestedFloorCollapse.
  have nested : ∀ a d e : Int, 0 < d → 0 < e →
      floorDiv (floorDiv a d) e = floorDiv a (d * e) := by
    intro a d e hd he
    apply floorDiv_law_nestedFloorCollapse a d e
    simp only [Bool.and_eq_true, decide_eq_true_eq]
    exact ⟨hd, he⟩
  -- The coarsening identity, composed from the cited pool laws + the two floor
  -- facts, driving the exponent regrouping by hand (2^(n-1) = 2^(n-m)*2^(m-1),
  -- 2^(n-m) = 2*2^(n-m-1)).
  have coarsen : ∀ s w n m b : Int,
      1 <= m → m < n → 1 <= w → 0 <= b → b < 2 →
      floorDiv ((2 * floorDiv (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1)) + b) * Domain.Fprep.pow2 (m - 1)) (Domain.Fprep.pow2 (n - 1))
        = floorDiv (s * Domain.Fprep.pow2 (m - 1)) (Domain.Fprep.pow2 (w - 1)) := by
    intro s w n m b hm hmn hw hb0 hb2
    have hpm1 : 0 < Domain.Fprep.pow2 (m - 1) := pow_pos _
    have hpnm1 : 0 < Domain.Fprep.pow2 (n - m - 1) := pow_pos _
    have hpw1 : 0 < Domain.Fprep.pow2 (w - 1) := pow_pos _
    have e1 : Domain.Fprep.pow2 (n - 1) = Domain.Fprep.pow2 (n - m) * Domain.Fprep.pow2 (m - 1) := by
      rw [show n - 1 = (n - m) + (m - 1) by omega, hom (n - m) (m - 1) (by omega) (by omega)]
    have e2 : Domain.Fprep.pow2 (n - m) = 2 * Domain.Fprep.pow2 (n - m - 1) := by
      rw [pow_of_pos (n - m) (by omega)]
    rw [e1, cancel (2 * floorDiv (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1)) + b) (Domain.Fprep.pow2 (m - 1))
          (Domain.Fprep.pow2 (n - m)) (pow_pos _) hpm1, e2,
        ← nested (2 * floorDiv (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1)) + b) 2 (Domain.Fprep.pow2 (n - m - 1))
          (by omega) hpnm1,
        absorb2 (floorDiv (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1))) b hb0 hb2,
        nested (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1)) (Domain.Fprep.pow2 (n - m - 1)) hpw1 hpnm1]
    have e3 : Domain.Fprep.pow2 (w - 1) * Domain.Fprep.pow2 (n - m - 1) = Domain.Fprep.pow2 (w + n - m - 2) := by
      rw [← hom (w - 1) (n - m - 1) (by omega) (by omega), show (w - 1) + (n - m - 1) = w + n - m - 2 by omega]
    have e4 : Domain.Fprep.pow2 (n - 2) = Domain.Fprep.pow2 (m - 1) * Domain.Fprep.pow2 (n - m - 1) := by
      rw [← hom (m - 1) (n - m - 1) (by omega) (by omega), show (m - 1) + (n - m - 1) = n - 2 by omega]
    rw [e3, e4, ← e3, ← Int.mul_assoc]
    exact cancel (s * Domain.Fprep.pow2 (m - 1)) (Domain.Fprep.pow2 (n - m - 1)) (Domain.Fprep.pow2 (w - 1)) hpw1 hpnm1
  have coarsen0 : ∀ s w n m : Int,
      1 <= m → m < n → 1 <= w →
      floorDiv (2 * floorDiv (s * Domain.Fprep.pow2 (n - 2)) (Domain.Fprep.pow2 (w - 1)) * Domain.Fprep.pow2 (m - 1)) (Domain.Fprep.pow2 (n - 1))
        = floorDiv (s * Domain.Fprep.pow2 (m - 1)) (Domain.Fprep.pow2 (w - 1)) := by
    intro s w n m hm hmn hw
    have h := coarsen s w n m 0 hm hmn hw (by omega) (by omega)
    simpa using h
  intro f n m h_when
  set_option maxHeartbeats 1000000 in
  simp only [_root_.fpSticky, _root_.fpTrunc, _root_.stickyHalf, _root_.truncStickyComposes, fpValue, sameValue, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at h_when ⊢ <;>
    (first
      | ((repeat' split) <;> (try simp only []) <;> grind only [])
      | ((try simp only []) <;> grind only []))
