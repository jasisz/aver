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

/// The module of the element decoders, `decElem_k`, and their lemmas.
const BRIDGE_ELEMS_MODULE: &str = "BridgeElems";

/// The `rcases` pattern that takes a value apart down to its scalars and
/// Lists (records and tuples too, unlike [`rcases_pattern`]), or `None` for
/// a value with no parts.
fn full_rcases_pattern(enc: &SourceEncoder) -> Option<String> {
    let sub = |e: &SourceEncoder| full_rcases_pattern(e).unwrap_or_else(|| "_".to_string());
    match enc {
        SourceEncoder::Record { fields, .. } => Some(format!(
            "⟨{}⟩",
            fields.iter().map(|(_, f)| sub(f)).collect::<Vec<_>>().join(", ")
        )),
        SourceEncoder::Tuple { elems, .. } => Some(format!(
            "⟨{}⟩",
            elems.iter().map(sub).collect::<Vec<_>>().join(", ")
        )),
        _ => rcases_pattern(enc).map(|_| match enc {
            SourceEncoder::Sum { ctors, .. } => format!(
                "({})",
                ctors
                    .iter()
                    .map(|(_, fields)| format!(
                        "⟨{}⟩",
                        fields.iter().map(sub).collect::<Vec<_>>().join(", ")
                    ))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            SourceEncoder::Option(e) => format!("(⟨⟩ | {})", sub(e)),
            SourceEncoder::Result { ok, err } => format!("({} | {})", sub(err), sub(ok)),
            _ => "_".to_string(),
        }),
    }
}

/// `BridgeElems.lean`: per element encoder `k`, its decoder `decElem_k`
/// (the element's argument shapes, as a function's decoder expands them),
/// the decoder's two facts — it reads the element encoding back
/// (`decElem_k_enc`) and decodes nothing else (`decElem_k_sound`) — and the
/// List facts the generic `decList` lemmas give from them. A nested List
/// element's decoder comes first, so each block only cites earlier ones.
/// Producer data like every bridge proof: a fact that does not close costs
/// the bridges that need it their credit.
fn render_bridge_elems(plan: &BridgePlan) -> String {
    let mut imports: BTreeSet<String> = plan.elem_roots.clone();
    imports.insert(BRIDGE_SUPPORT_MODULE.into());
    let mut s = bridge_part_header(
        "Decoders of the elements of the bridged functions' List arguments.",
        &imports,
    );
    let mut sound = String::from(
        "\n         | (have e := decListInt_sound _ hl; subst e)\
         \n         | (have e := decListBool_sound _ hl; subst e)\
         \n         | (have e := decListString_sound _ hl; subst e)",
    );
    let mut enc = String::from(
        "AverCert.GrammarBridge.decodeStr_strBytes, decListInt_enc, decListBool_enc, \
         decListString_enc, decList",
    );
    for (k, elem) in plan.elems.elems.iter().enumerate() {
        let ty = elem.enc.binder_type();
        let gty = elem.enc.grammar_ty();
        let at = |value: &str| elem.enc.encode(value, &mut 0);
        let list_at =
            |value: &str| SourceEncoder::List(Box::new(elem.enc.clone())).encode(value, &mut 0);
        s.push_str(&format!(
            "/-- Decode one element of a List of `{ty}`. -/\n\
             noncomputable def decElem_{k} : _root_.AverCert.Grammar.SVal → _root_.Option {ty} := \
             fun a =>\n"
        ));
        let rhs = |alt: &Alt| {
            let mut rhs = format!("_root_.Option.some {}", alt.source);
            for (v, t, decode) in alt.binds.iter().rev() {
                rhs = format!("({decode} {v}).bind (fun {t} => {rhs})");
            }
            rhs
        };
        match elem.alts.as_slice() {
            // A whole-value shape (a List element) matches every value.
            [only] if only.binds.len() == 1 && only.pattern == only.binds[0].0 => {
                s.push_str(&format!("  (fun {} => {}) a\n\n", only.pattern, rhs(only)));
            }
            alts => {
                s.push_str("  match a with\n");
                for alt in alts {
                    s.push_str(&format!("  | {} => {}\n", alt.pattern, rhs(alt)));
                }
                s.push_str("  | _ => _root_.Option.none\n\n");
            }
        }
        let cases = full_rcases_pattern(&elem.enc)
            .map(|pattern| format!("rcases x with {pattern}\n  all_goals "))
            .unwrap_or_default();
        s.push_str(&format!(
            "theorem decElem_{k}_sound : ∀ (v : _root_.AverCert.Grammar.SVal) (x : {ty}),\n    \
             decElem_{k} v = _root_.Option.some x → v = {x} := by\n  \
             intro v x h\n  \
             unfold decElem_{k} at h\n  \
             (try split at h) <;> (try simp only [_root_.Option.bind_eq_some_iff, \
             _root_.Option.some.injEq, reduceCtorEq, AverCert.GrammarBridge.decodeStr_eq_some] at h)\n  \
             all_goals (repeat' (first\n    \
               | (obtain ⟨_, rfl, h⟩ := h)\n    \
               | (obtain ⟨_, hl, h⟩ := h\n       \
                  first{sound})))\n  \
             all_goals (try subst h)\n  \
             all_goals rfl\n\n\
             theorem decElem_{k}_enc : ∀ (x : {ty}), decElem_{k} ({x}) = _root_.Option.some x := by\n  \
             intro x\n  \
             {cases}first\n    \
               | (simp [decElem_{k}, {enc}]; done)\n    \
               | (simp [decElem_{k}, {enc}] <;> rfl)\n\n\
             theorem decList_{k}_enc : ∀ (l : _root_.List {ty}),\n    \
             decList {gty} decElem_{k} {list_l} = _root_.Option.some l :=\n  \
             decList_enc _ _ _ decElem_{k}_enc\n\n\
             theorem decList_{k}_sound : ∀ (a : _root_.List {ty}) {{v : _root_.AverCert.Grammar.SVal}},\n    \
             decList {gty} decElem_{k} v = _root_.Option.some a → v = {list_a} :=\n  \
             decList_sound _ _ _ decElem_{k}_sound\n\n\
             theorem decList_{k}_split {{v : _root_.AverCert.Grammar.SVal}} {{a : _root_.List {ty}}}\n    \
             (h : decList {gty} decElem_{k} v = _root_.Option.some a) :\n    \
             (v = _root_.AverCert.Grammar.SVal.nil {gty} ∧ a = []) ∨\n      \
             ∃ x t, v = _root_.AverCert.Grammar.SVal.cons {gty} ({x}) {list_t} ∧ a = x :: t :=\n  \
             decList_split _ _ _ decElem_{k}_sound h\n\n\
             theorem cons_map_{k} (x : {ty}) (l : _root_.List {ty}) :\n    \
             ({x}) :: l.map (fun x => {x}) = (x :: l).map (fun x => {x}) := rfl\n\n\
             theorem listOf_nil_{k} : _root_.AverCert.Grammar.listOf? (_root_.AverCert.Grammar.SVal.nil {gty}) =\n    \
             _root_.Option.some ({gty}, ([] : _root_.List {ty}).map (fun x => {x})) := rfl\n\n",
            x = at("x"),
            list_l = list_at("l"),
            list_a = list_at("a"),
            list_t = list_at("t"),
        ));
        sound.push_str(&format!(
            "\n         | (have e := decList_{k}_sound _ hl; subst e)"
        ));
        enc.push_str(&format!(", decElem_{k}_enc, decList_{k}_enc"));
    }
    s.push_str("end AverCert.Bridge\n");
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
    if !plan.elems.elems.is_empty() {
        parts.push((format!("{BRIDGE_ELEMS_MODULE}.lean"), render_bridge_elems(plan)));
    }

    // Decoders/images retain their original names. Only their ownership
    // changes: a body slice no longer imports every source module.
    let mut defs_imports: BTreeSet<String> = model_roots.iter().cloned().collect();
    defs_imports.insert(BRIDGE_SUPPORT_MODULE.into());
    for (i, slice) in steps.chunks(BRIDGE_STEPS_PER_MODULE).enumerate() {
        let mut imports: BTreeSet<String> = slice.iter().map(|b| b.model_root.clone()).collect();
        imports.insert(BRIDGE_SUPPORT_MODULE.into());
        if slice.iter().any(|b| !b.elem_decoders.is_empty()) {
            imports.insert(BRIDGE_ELEMS_MODULE.into());
        }
        let name = format!("{BRIDGE_IMAGES_MODULE}{i}");
        let mut s = bridge_part_header("One slice of source decoders and images.", &imports);
        for b in slice { render_fn_defs(b, &mut s); }
        s.push_str("end AverCert.Bridge\n");
        parts.push((format!("{name}.lean"), s));
        defs_imports.insert(name);
    }
    let mut defs = bridge_part_header("The complete source image table.", &defs_imports);
    render_image_table(&plan.fns, &mut defs);
    defs.push_str("end AverCert.Bridge\n");
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
            render_step_body(b, &plan.fns, &lit_index, plan.with_default, &plan.elems, &mut body);
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
        let images: BTreeSet<_> = slice.iter()
            .map(|(_, f)| format!("{BRIDGE_IMAGES_MODULE}{}", owners[f])).collect();
        let mut assembly = bridge_part_header(
            "Cached export assembly, parametric in the plan and image tables.", &images,
        );
        for (bridge, f) in slice { render_export_assembly(bridge, *f, plan, &mut assembly); }
        assembly.push_str("end AverCert.Bridge\n");
        parts.push((format!("{BRIDGE_ASSEMBLY_MODULE}{i}.lean"), assembly));
        let needed_steps: BTreeSet<usize> = slice.iter()
            .flat_map(|(_, f)| closure_of(*f, &plan.fns)).map(|f| owners[&f]).collect();
        // Keep numeric order, not the lexical order of module names.
        let step_imports: String = needed_steps.iter()
            .map(|i| format!("import {BRIDGE_STEPS_MODULE}{i}\n")).collect();
        let mut part = format!(
            "-- One slice of the plan-equals-source bridges of this certificate: the\n\
             -- export theorems, over the step lemmas of the slices imported below.\n\
             import {BRIDGE_NAMES_MODULE}\n\
             import {BRIDGE_ASSEMBLY_MODULE}{i}\n\
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
            elems: ElemDecoders::default(), elem_roots: BTreeSet::new(),
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
