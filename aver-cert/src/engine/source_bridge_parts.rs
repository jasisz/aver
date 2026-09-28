// Included from source_bridges.rs, under the engine feature.
//
// Heavy body proofs depend on their model/image slices, not the complete
// artifact or plan tables. The small step bindings check the proof-local
// bodies against Plans.lean, which remains the authoritative plan data.

const BRIDGE_SUPPORT_MODULE: &str = "BridgeSupport";
const BRIDGE_IMAGES_MODULE: &str = "BridgeImages";
const BRIDGE_BODIES_MODULE: &str = "BridgeBodies";

fn bridge_part_header(description: &str, imports: &BTreeSet<String>) -> String {
    let imports: String = imports.iter().map(|name| format!("import {name}\n")).collect();
    format!(
        "-- {description}\n{imports}\n\
         set_option autoImplicit false\n\
         set_option maxRecDepth 200000\n\
         set_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\
         set_option maxHeartbeats {FILE_HEARTBEATS}\n\n\
         namespace AverCert.Bridge\n\n"
    )
}

fn render_bridge_support() -> String {
    let mut s = bridge_part_header(
        "Shared decoders and symbolic-evaluation lemmas; no program data.",
        &BTreeSet::from(["GrammarBridge".into()]),
    );
    s.push_str(LIST_DECODERS);
    s.push_str(STEP_LEMMAS);
    // Parametric in the source helper, so this module need not import the
    // model prelude that defines Except.withDefault.
    s.push_str(
        "\ntheorem withDefault_ite {α ε : Type} (F : Except ε α → α → α)\n    \
         (c : Prop) [Decidable c] (e : ε) (v d : α) :\n    \
         F (if c then Except.error e else Except.ok v) d =\n      \
         if c then F (Except.error e) d else F (Except.ok v) d := by\n  \
         split <;> rfl\n\nend AverCert.Bridge\n"
    );
    s
}

fn render_bridge_literals(plan: &BridgePlan) -> (String, BTreeMap<Vec<u8>, usize>) {
    let mut s = bridge_part_header(
        "The bytes of the String literals the bridge steps rewrite with.",
        &BTreeSet::from(["GrammarBridge".into()]),
    );
    // A String literal is definitionally String.ofList of its characters.
    // Reduce their UTF-8 encoding directly: evaluating List.toByteArray in
    // the kernel repeatedly appends to its growing accumulator. This lemma
    // avoids that work without changing strBytes or trusting producer bytes.
    s.push_str(
        "-- Read UTF-8 bytes directly from the literal's characters.\n\
         theorem strBytes_ofList (cs : List Char) :\n    \
         AverCert.GrammarBridge.strBytes (String.ofList cs) =\n      \
         (cs.flatMap String.utf8EncodeChar).map UInt8.toNat := by\n  \
         unfold AverCert.GrammarBridge.strBytes\n  \
         rw [String.toByteArray_ofList]\n  \
         unfold List.utf8Encode\n  \
         rw [List.toList_data_toByteArray]\n\n"
    );
    let mut index = BTreeMap::new();
    for (i, bytes) in plan.literals.iter().enumerate() {
        let Some(text) = lean_string_literal(bytes) else { continue };
        // lean_string_literal has already checked that this is UTF-8.
        let chars = lean_char_list(std::str::from_utf8(bytes).unwrap());
        let list = bytes.iter().map(u8::to_string).collect::<Vec<_>>().join(", ");
        s.push_str(&format!(
            "theorem strLit_{i} : AverCert.GrammarBridge.strBytes {text} = [{list}] := by\n  \
             first | exact (strBytes_ofList {chars}).trans (by decide +kernel) | sorry\n\n"
        ));
        index.insert(bytes.clone(), i);
    }
    s.push_str("end AverCert.Bridge\n");
    (s, index)
}

/// Partition by the actual function sequence, never by arithmetic on sparse
/// function indices. Heavy body proofs use only their direct image needs;
/// export proofs still import the step bindings of their whole call closure.
fn render_bridge_lean(
    plan: &BridgePlan,
    model_roots: &[String],
    exports_module: &str,
) -> (String, Vec<(String, String)>) {
    let steps: Vec<&BridgedFn> = plan.fns.values().collect();
    let owners: BTreeMap<u32, usize> = steps.iter().enumerate()
        .map(|(i, b)| (b.func_idx, i / BRIDGE_STEPS_PER_MODULE)).collect();
    let (literals, lit_index) = render_bridge_literals(plan);
    let mut parts = vec![
        (format!("{BRIDGE_SUPPORT_MODULE}.lean"), render_bridge_support()),
        (format!("{BRIDGE_LITS_MODULE}.lean"), literals),
    ];

    // Decoders/images retain their original names. Only their ownership
    // changes: a body slice no longer imports every source module.
    let mut defs_imports: BTreeSet<String> = model_roots.iter().cloned().collect();
    defs_imports.insert(BRIDGE_SUPPORT_MODULE.into());
    for (i, slice) in steps.chunks(BRIDGE_STEPS_PER_MODULE).enumerate() {
        let mut imports: BTreeSet<String> = slice.iter().map(|b| b.model_root.clone()).collect();
        imports.insert(BRIDGE_SUPPORT_MODULE.into());
        let name = format!("{BRIDGE_IMAGES_MODULE}{i}");
        let mut s = bridge_part_header("One slice of source decoders and images.", &imports);
        for b in slice { render_fn_defs(b, &mut s); }
        s.push_str("end AverCert.Bridge\n");
        parts.push((format!("{name}.lean"), s));
        defs_imports.insert(name);
    }
    let mut defs = bridge_part_header("The complete source image and call-depth tables.", &defs_imports);
    render_image_table(&plan.fns, &mut defs);
    defs.push_str("/-- Call depth over the acyclic part of the call graph. -/\ndef depth : _root_.Nat → _root_.Nat := fun g =>\n  match g with\n");
    for (f, d) in &plan.depth { defs.push_str(&format!("  | {f} => {d}\n")); }
    defs.push_str("  | _ => 0\n\nend AverCert.Bridge\n");
    parts.push((format!("{BRIDGE_DEFS_MODULE}.lean"), defs));

    for (i, slice) in steps.chunks(BRIDGE_STEPS_PER_MODULE).enumerate() {
        let mut imports = BTreeSet::from([BRIDGE_SUPPORT_MODULE.into(), BRIDGE_LITS_MODULE.into()]);
        for b in slice {
            for f in image_dependencies(b) {
                imports.insert(format!("{BRIDGE_IMAGES_MODULE}{}", owners[&f]));
            }
        }
        let mut body = bridge_part_header("Cached body proofs, parametric in the source image table.", &imports);
        for b in slice {
            render_step_body(b, &plan.fns, &lit_index, plan.with_default, &mut body);
        }
        body.push_str("end AverCert.Bridge\n");
        parts.push((format!("{BRIDGE_BODIES_MODULE}{i}.lean"), body));

        let imports = BTreeSet::from([
            BRIDGE_DEFS_MODULE.into(), "Plans".into(), format!("{BRIDGE_BODIES_MODULE}{i}"),
        ]);
        let mut bindings = bridge_part_header("Bind cached body proofs to this certificate's authoritative plans.", &imports);
        for b in slice { render_step_binding(b, &mut bindings); }
        bindings.push_str("end AverCert.Bridge\n");
        parts.push((format!("{BRIDGE_STEPS_MODULE}{i}.lean"), bindings));
    }
    parts.push((format!("{BRIDGE_NAMES_MODULE}.lean"), render_bridge_names(exports_module)));

    let slices = plan.bridges.len().div_ceil(BRIDGE_PROOFS_PER_MODULE).max(1);
    let per_slice = plan.bridges.len().div_ceil(slices).max(1);
    let mut corollaries = String::new();
    let mut proof_imports = String::new();
    for (i, slice) in plan.bridges.chunks(per_slice).enumerate() {
        let name = format!("{BRIDGE_PROOF_MODULE}{i}");
        let needed_steps: BTreeSet<usize> = slice.iter()
            .flat_map(|(_, f)| closure_of(*f, &plan.fns)).map(|f| owners[&f]).collect();
        // Keep numeric order, not the lexical order of module names.
        let step_imports: String = needed_steps.iter()
            .map(|i| format!("import {BRIDGE_STEPS_MODULE}{i}\n")).collect();
        let mut part = format!(
            "-- One slice of the plan-equals-source bridges of this certificate: the\n\
             -- export theorems, over the step lemmas of the slices imported below.\n\
             import {BRIDGE_NAMES_MODULE}\n\
             {step_imports}\n\
             set_option autoImplicit false\n\
             set_option maxRecDepth 200000\n\
             set_option linter.unusedSimpArgs false\n\
             set_option linter.unusedVariables false\n\
             set_option maxHeartbeats {FILE_HEARTBEATS}\n\n\
             namespace AverCert.Bridge\n\n"
        );
        for (bridge, func_idx) in slice {
            render_export(bridge, *func_idx, plan, &mut part, &mut corollaries);
        }
        part.push_str("end AverCert.Bridge\n");
        parts.push((format!("{name}.lean"), part));
        proof_imports.push_str(&format!("import {name}\n"));
    }
    let mut bridge = format!(
        "-- The plan-equals-source claims of this certificate: each bridge theorem\n\
         -- of the `{BRIDGE_PROOF_MODULE}` slices conjoined with the artifact-level\n\
         -- `Holds` fact.\n\
         {proof_imports}\
         import Final\n"
    );
    // The checker admits nested model files through these direct imports.
    for root in model_roots { bridge.push_str(&format!("import {root}\n")); }
    bridge.push_str("\nset_option autoImplicit false\n\n");
    bridge.push_str(&corollaries);
    (bridge, parts)
}

#[cfg(test)]
mod literal_proof_tests {
    use super::*;

    #[test]
    fn literal_proofs_encode_characters_without_building_byte_arrays() {
        let literals = [b"".to_vec(), b"a'\"\\\n\r\t".to_vec(), "é中🦀".as_bytes().to_vec(), vec![0xff]];
        let plan = BridgePlan {
            fns: BTreeMap::new(), bridges: Vec::new(), declined: Vec::new(),
            depth: BTreeMap::new(), literals: literals.into_iter().collect(),
            with_default: false, entries: BTreeMap::new(), type_pieces: Vec::new(),
        };
        let (text, index) = render_bridge_literals(&plan);
        assert_eq!(index.len(), 3, "invalid UTF-8 is still not a String literal");
        assert_eq!(text.matches("theorem strBytes_ofList").count(), 1);
        assert!(text.contains("String.toByteArray_ofList"));
        assert!(text.contains("List.toList_data_toByteArray"));
        assert!(text.contains("(strBytes_ofList []).trans (by decide +kernel)"));
        assert!(text.contains("['a', (Char.ofNat 39), (Char.ofNat 34), (Char.ofNat 92), (Char.ofNat 10), (Char.ofNat 13), (Char.ofNat 9)]"));
        assert!(text.contains("[(Char.ofNat 233), (Char.ofNat 20013), (Char.ofNat 129408)]"));
        assert!(text.contains("[195, 169, 228, 184, 173, 240, 159, 166, 128]"));
        for (bytes, i) in index {
            let literal = lean_string_literal(&bytes).unwrap();
            assert!(text.contains(&format!("theorem strLit_{i} : AverCert.GrammarBridge.strBytes {literal} =")));
        }
    }
}
