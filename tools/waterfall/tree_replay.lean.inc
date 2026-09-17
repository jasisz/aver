-- The two checked replacements printed by waterfall?, with its UI-only
-- `[apply]` marker removed. No waterfall import or tactic is needed.
set_option maxHeartbeats 1000000 in
theorem fastElements_law_accumulator : ∀ (tree : Tree) (acc : List Int), fastElements tree acc = ((elements tree) ++ acc) := by
  (
    expose_names
    intro _
    expose_names
    fun_induction elements tree
    expose_names
    focus ((simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [WaterfallTree.fastElements]; done))
    expose_names
    focus
      ((simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [WaterfallTree.fastElements,
            WaterfallTree.elements];
          done)))

#print axioms fastElements_law_accumulator

theorem fastElements_law_traversal : ∀ (tree : Tree), fastElements tree ([] : List Int) = elements tree := by
  (
    expose_names
    focus
      ((simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [fastElements_law_accumulator,
            WaterfallTree.fastElements, WaterfallTree.elements];
          done)))

#print axioms fastElements_law_traversal
