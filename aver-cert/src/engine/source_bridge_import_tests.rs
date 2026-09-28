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
                    params: Vec::new(),
                    result: SourceEncoder::Int,
                    callees: Vec::new(),
                    fuel: false,
                    shapes: Vec::new(),
                    literals: BTreeSet::new(),
                    recursive: false,
                    constants: Vec::new(),
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
