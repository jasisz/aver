-- Concatenate after the unchanged definitions emitted from tree.av.
-- Both statements are copied verbatim from the Aver export.
set_option maxHeartbeats 1000000 in
theorem fastElements_law_accumulator : ∀ (tree : Tree) (acc : List Int), fastElements tree acc = ((elements tree) ++ acc) := by
  waterfall? (mode := .search) (effort := 1000) (report := true)

#print axioms fastElements_law_accumulator

theorem fastElements_law_traversal : ∀ (tree : Tree), fastElements tree ([] : List Int) = elements tree := by
  waterfall? [fastElements_law_accumulator]

#print axioms fastElements_law_traversal
