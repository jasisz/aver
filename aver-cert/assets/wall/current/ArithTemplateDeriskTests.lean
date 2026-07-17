import ArithTemplateDeriskBytes

/-!
Pin theorems for the arith-template de-risk, on REAL bytes from two
independently compiled modules (`ArithTemplateDeriskBytes`).

Honest pins: the body bytes at the manifest-declared `hostRoleTable` index
equal the template synthesized from the declaration alone. Forgery pins: a
misdeclaration — the add role declared at the sub helper's index (real bytes),
or a wrong embedded type/function index — fails the equality. Kernel-decided
throughout; `#print axioms` at the bottom must stay inside
`{propext, Classical.choice, Quot.sound}`.
-/

namespace ArithTemplateDeriskTests

open ArithTemplateDerisk ArithTemplateDeriskBytes

/-! ## Profile: both real declarations sit inside the single-byte regime -/

theorem addOne_params_in_profile : checkArithHostParams addOneParams = true := by
  decide +kernel

theorem json_params_in_profile : checkArithHostParams jsonParams = true := by
  decide +kernel

/-! ## Honest pins — declaration synthesizes EXACTLY the real bytes -/

theorem addOne_add_honest_pin :
    arithHelperBody .add addOneParams = addOneAddBody := by decide +kernel

theorem addOne_sub_honest_pin :
    arithHelperBody .sub addOneParams = addOneSubBody := by decide +kernel

theorem addOne_mul_honest_pin :
    arithHelperBody .mul addOneParams = addOneMulBody := by decide +kernel

theorem addOne_box_honest_pin :
    arithHelperBody .box addOneParams = addOneBoxBody := by decide +kernel

theorem json_add_honest_pin :
    arithHelperBody .add jsonParams = jsonAddBody := by decide +kernel

theorem json_sub_honest_pin :
    arithHelperBody .sub jsonParams = jsonSubBody := by decide +kernel

theorem json_mul_honest_pin :
    arithHelperBody .mul jsonParams = jsonMulBody := by decide +kernel

theorem json_box_honest_pin :
    arithHelperBody .box jsonParams = jsonBoxBody := by decide +kernel

/-! ## Forgeries — misdeclarations die on the same equality

Each theorem feeds the pin REAL bytes (the actual body sitting at the
misdeclared index) so the refusal is about the declaration, not about
synthetic garbage. -/

/-- Declaring the ADD role at the SUB helper's index: the pin compares the
    add template against the real sub body and refuses. (This is the exact
    swap `firstArithScan`'s fingerprint currently distinguishes by opcode
    order; here it dies on template equality with no scan.) -/
theorem addRole_declared_at_sub_index_dies :
    arithHelperBody .add jsonParams ≠ jsonSubBody := by decide +kernel

/-- The mirror swap: sub role declared at the add helper's index. -/
theorem subRole_declared_at_add_index_dies :
    arithHelperBody .sub jsonParams ≠ jsonAddBody := by decide +kernel

/-- Mul role declared at the add helper's index. -/
theorem mulRole_declared_at_add_index_dies :
    arithHelperBody .mul jsonParams ≠ jsonAddBody := by decide +kernel

/-- Add role declared at the box helper's index (the one name-bound role). -/
theorem addRole_declared_at_box_index_dies :
    arithHelperBody .add jsonParams ≠ jsonBoxBody := by decide +kernel

/-- A wrong declared carrier type index (off by one: another module's layout)
    fails the pin against the real add body. -/
theorem wrong_carrier_type_index_dies :
    arithHelperBody .add { jsonParams with carrier := 22 } ≠ jsonAddBody := by
  decide +kernel

/-- A wrong declared limb array type index fails the pin. -/
theorem wrong_limb_type_index_dies :
    arithHelperBody .add { jsonParams with limb := 21 } ≠ jsonAddBody := by
  decide +kernel

/-- A wrong declared sub-routine function index (normalize pointed at strip)
    fails the pin. -/
theorem wrong_normalize_callee_index_dies :
    arithHelperBody .add { jsonParams with normalize := 107 } ≠ jsonAddBody := by
  decide +kernel

/-- Cross-module confusion: add_one's declaration against json's real add
    body (every parameter honest for the WRONG module) fails the pin. -/
theorem foreign_module_declaration_dies :
    arithHelperBody .add addOneParams ≠ jsonAddBody := by decide +kernel

/-! ## Axiom hygiene -/

#print axioms addOne_add_honest_pin
#print axioms addOne_sub_honest_pin
#print axioms addOne_mul_honest_pin
#print axioms addOne_box_honest_pin
#print axioms json_add_honest_pin
#print axioms json_sub_honest_pin
#print axioms json_mul_honest_pin
#print axioms json_box_honest_pin
#print axioms addRole_declared_at_sub_index_dies
#print axioms subRole_declared_at_add_index_dies
#print axioms mulRole_declared_at_add_index_dies
#print axioms addRole_declared_at_box_index_dies
#print axioms wrong_carrier_type_index_dies
#print axioms wrong_limb_type_index_dies
#print axioms wrong_normalize_callee_index_dies
#print axioms foreign_module_declaration_dies

end ArithTemplateDeriskTests
