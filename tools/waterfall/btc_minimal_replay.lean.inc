-- Ordinary Lean script returned by waterfall? in committed mode.
-- The original 0 <= b <= 255 guard remains; sample-membership premises are gone.
set_option maxHeartbeats 1000000 in
theorem isMinimalPush_law_directPushIsMinimalUnlessSmallNumber : ∀ (b : Int), ((b >= 0) && (b <= 255)) = true -> Domain.StackItem.isMinimalPush 1 [b] = (!(((b >= 1) && (b <= 16)) || (b == 129))) := by
  (
    expose_names
    intro _
    expose_names
    intro _
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Domain.StackItem.isMinimalPush]
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Domain.StackItem.singleByteMinimal]
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Domain.StackItem.asNumber,
      Domain.StackItem.smallOrDirect]
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Domain.StackItem.valued]
    expose_names
    grind +lax [Domain.StackItem.signedBy, Domain.StackItem.bigEndian, Domain.StackItem.unsigned])

#print axioms isMinimalPush_law_directPushIsMinimalUnlessSmallNumber
