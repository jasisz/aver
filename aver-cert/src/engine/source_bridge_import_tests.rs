//! Import routing for bridge proof slices. These tests exercise the whole
//! renderer so the owner map cannot drift from the actual step partition.
use super::*;

// Sparse indices deliberately differ from ordinal positions in the slices.
fn index(ordinal: u32) -> u32 {
    100 + 7 * ordinal
}

fn plan_with_functions(count: u32) -> BridgePlan {
    let fns: BTreeMap<_, _> = (0..count)
        .map(|ordinal| {
            let f = index(ordinal);
            (
                f,
                BridgedFn {
                    func_idx: f,
                    model: format!("M.f{f}"),
                    model_root: "AverModel.M".into(),
                    body: PlanExpr::Literal(PlanLit::Int(0)).lean(),
                    params: Vec::new(),
                    result: SourceEncoder::Int,
                    callees: Vec::new(),
                    fuel: false,
                    shapes: Vec::new(),
                    literals: BTreeSet::new(),
                    recursive: false,
                    constants: Vec::new(),
                    elem_decoders: BTreeSet::new(),
                    list_helpers: false,
                    matched_lists: BTreeSet::new(),
                },
            )
        })
        .collect();
    let bridges = fns
        .values()
        .map(|b| {
            let export = format!("f{}", b.func_idx);
            (
                SourceBridge {
                    theorem: SourceBridge::theorem_name(&export),
                    corollary: SourceBridge::corollary_name(&export),
                    export,
                    model: b.model.clone(),
                    kind: BridgeKind::Adequate,
                    params: b.params.clone(),
                    result: b.result.clone(),
                },
                b.func_idx,
            )
        })
        .collect();
    let entries = fns
        .keys()
        .enumerate()
        .map(|(position, f)| {
            (*f, (position, format!("⟨\"f{f}\", true, {f}, 0, AverCert.Plans.fn{f}⟩")))
        })
        .collect();
    BridgePlan {
        fns,
        bridges,
        declined: Vec::new(),
        depth: BTreeMap::new(),
        literals: BTreeSet::new(),
        with_default: false,
        entries,
        type_pieces: Vec::new(),
        elems: ElemDecoders::default(),
        elem_roots: BTreeSet::new(),
    }
}

fn proof_imports(plan: &BridgePlan) -> Vec<Vec<usize>> {
    let roots = vec!["AverModel.M".to_string()];
    let (bridge, parts) = render_bridge_lean(plan, &roots, "ArtifactInterface");
    assert!(bridge.contains("import AverModel.M\n"));
    parts
        .iter()
        .filter(|(name, _)| name.starts_with(BRIDGE_PROOF_MODULE))
        .map(|(_, text)| {
            assert!(text.contains("import BridgeNames\n"));
            text.lines()
                .filter_map(|line| line.strip_prefix("import BridgeSteps"))
                .map(|n| {
                    assert!(parts.iter().any(|(name, _)| name == &format!("BridgeSteps{n}.lean")));
                    n.parse().unwrap()
                })
                .collect()
        })
        .collect()
}

#[test]
fn independent_exports_import_only_their_step_slices() {
    // Two proof slices of 37 and 36 exports cross the 24-step boundaries;
    // the last step slice contains only one function.
    let plan = plan_with_functions(73);
    assert_eq!(proof_imports(&plan), [vec![0, 1], vec![1, 2, 3]]);
}

#[test]
fn imports_follow_transitive_callees_and_mutual_recursion() {
    let mut plan = plan_with_functions(96);
    // The first proof slice needs step slice 3 through 0 -> 72 -> 24 -> 0,
    // but never slice 2. Duplicate edges must not duplicate imports.
    for (caller, callees) in [(0, vec![72, 72]), (72, vec![24]), (24, vec![0])] {
        plan.fns.get_mut(&index(caller)).unwrap().callees =
            callees.into_iter().map(index).collect();
    }
    assert_eq!(proof_imports(&plan), [vec![0, 1, 3], vec![0, 1, 2, 3]]);
}

#[test]
fn internal_callees_import_once_in_numeric_slice_order() {
    let mut plan = plan_with_functions(24 * 12);
    // Only the caller is exported. The proof still needs both internal
    // callees, including a self-recursive one in a two-digit slice.
    plan.bridges.truncate(1);
    plan.fns.get_mut(&index(0)).unwrap().callees = vec![index(264), index(48)];
    plan.fns.get_mut(&index(264)).unwrap().callees = vec![index(264)];
    assert_eq!(proof_imports(&plan), [vec![0, 2, 11]]);
}

#[test]
fn expensive_step_bodies_do_not_import_program_wide_tables() {
    let plan = plan_with_functions(48);
    let (_, parts) = render_bridge_lean(&plan, &["AverModel.M".into()], "ArtifactInterface");
    let bodies: Vec<_> = parts.iter().filter(|(name, _)| name.starts_with("BridgeBodies")).collect();
    assert_eq!(bodies.len(), 2, "heavy body proofs have independently cached modules");
    for (_, text) in bodies {
        assert!(!text.contains("import Manifest\n"));
        assert!(!text.contains("import Plans\n"));
        assert!(!text.contains("import BridgeDefs\n"));
        assert!(!text.contains("AverCert.Plans.fnPlans"));
        assert!(text.contains("(I : AverCert.GrammarBridge.Table)"));
    }
}

#[test]
fn image_slices_import_the_recorded_model_files_not_namespaces() {
    let mut plan = plan_with_functions(48);
    for (ordinal, b) in plan.fns.values_mut().enumerate() {
        b.model_root = if ordinal < 24 { "AverModel.Left" } else { "AverModel.Right" }.into();
    }
    let (_, parts) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    let part = |name: &str| &parts.iter().find(|(path, _)| path == name).unwrap().1;
    assert!(part("BridgeImages0.lean").contains("import AverModel.Left\n"));
    assert!(!part("BridgeImages0.lean").contains("import AverModel.Right\n"));
    assert!(part("BridgeBodies0.lean").contains("import BridgeImages0\n"));
    assert!(!part("BridgeBodies0.lean").contains("import BridgeImages1\n"));
    assert!(part("BridgeBodies1.lean").contains("import BridgeImages1\n"));
    assert!(!part("BridgeBodies1.lean").contains("import BridgeImages0\n"));

    // A cross-slice call adds the callee's image, but not its proof body.
    plan.fns.get_mut(&index(0)).unwrap().callees.push(index(24));
    let (_, changed) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    let body = &changed.iter().find(|(path, _)| path == "BridgeBodies0.lean").unwrap().1;
    assert!(body.contains("import BridgeImages1\n"));
    assert!(!body.contains("import BridgeBodies1\n"));
}

#[test]
fn changing_one_body_leaves_disjoint_heavy_slices_identical() {
    let mut plan = plan_with_functions(48);
    let (_, before) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    plan.fns.get_mut(&index(0)).unwrap().body = PlanExpr::Literal(PlanLit::Int(1)).lean();
    let (_, after) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    let changed: Vec<_> = before.iter().zip(&after)
        .filter_map(|((name, old), (new_name, new))| {
            assert_eq!(name, new_name);
            (old != new).then_some(name.as_str())
        }).collect();
    assert_eq!(changed, ["BridgeBodies0.lean"]);
    let binding = &after.iter().find(|(name, _)| name == "BridgeSteps0.lean").unwrap().1;
    assert!(binding.contains("⟨AverCert.Plans.fn100, rfl, stepBody_100 I I_100⟩"));
}

#[test]
fn export_assembly_is_parametric_and_imports_only_its_own_images() {
    let mut plan = plan_with_functions(73);
    // The first export also calls an internal function in the last image
    // slice. Assembly assumes its step, without importing its body or image.
    plan.fns.get_mut(&index(0)).unwrap().callees.push(index(72));
    let (_, parts) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    let assembly: Vec<_> = parts.iter()
        .filter(|(name, _)| name.starts_with("BridgeAssembly")).collect();
    assert_eq!(assembly.len(), 2);
    let first = &assembly[0].1;
    assert!(first.contains("import BridgeImages0\n"));
    assert!(first.contains("import BridgeImages1\n"));
    assert!(!first.contains("import BridgeImages3\n"));
    assert!(first.contains("(step_604 : AverCert.GrammarBridge.Step fns I [] 604)"));
    for (_, text) in assembly {
        for global in ["Manifest", "Plans", "BridgeDefs", "BridgeNames", "BridgeSteps"] {
            assert!(!text.contains(&format!("import {global}")), "{global}");
        }
        assert!(!text.contains("AverCert.Plans"));
        assert!(text.contains("(fns : _root_.List AverCert.Schema.FnEntry)"));
        assert!(text.contains("(I : AverCert.GrammarBridge.Table)"));
    }
    let binding = &parts.iter().find(|(name, _)| name == "BridgeProof0.lean").unwrap().1;
    assert!(binding.contains("import BridgeAssembly0\n"));
    assert!(binding.contains("assembly_100 AverCert.Plans.fnPlans I I_100 step_100 step_604"));
}

#[test]
fn exact_assembly_carries_only_its_closures_depths() {
    let mut plan = plan_with_functions(2);
    plan.bridges[0].0.kind = BridgeKind::Exact;
    plan.depth.insert(index(0), 0);
    let (_, before) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    // An unrelated function's depth must not invalidate this proof.
    plan.depth.insert(index(1), 99);
    let (_, after) = render_bridge_lean(&plan, &[], "ArtifactInterface");
    let assembly = |parts: Vec<(String, String)>| parts.into_iter()
        .find(|(name, _)| name == "BridgeAssembly0.lean").unwrap().1;
    let first = assembly(before);
    assert!(first.contains("AverCert.GrammarBridge.exact_of_step fns I"));
    assert!(first.contains("AverCert.GrammarBridge.bridge_of_step fns I"));
    assert_eq!(first, assembly(after));
}
