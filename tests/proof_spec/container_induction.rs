use super::*;

// Container (motive2-lift) induction arm: an unconditional law about a rose-tree
// ADT walker whose recursion runs through a `List<Self>` field is closed on the
// walker's functional-induction principle `f.induct` with the sibling motive
// (`motive2`) computed from the claim AST. Plain constructor induction gives the
// node case no child-list hypothesis, so the legacy structural arm declined this
// shape and the law stayed a `simp <;> done` sorry. These fixtures pin the flip
// (sorry → GENUINE universal, `#print axioms`-clean) for the inequality and
// equational-with-container-lemma families, the name-blindness (a foreign-named
// org-chart witness), and the accumulator DECLINE (the naive lift is a false
// obligation, so the 2-param shape must fall back to bounded, never go red).

const TREE_SRC: &str = r#"module TreeMirror
    intent = "Rose tree size under mirror; container recursion through List<Tree>."
    effects []

type Tree
    Leaf
    Node(List<Tree>)

fn size(t: Tree) -> Int
    match t
        Tree.Leaf -> 1
        Tree.Node(kids) -> 1 + sizeList(kids)

fn sizeList(ts: List<Tree>) -> Int
    match ts
        [] -> 0
        [x, ..rest] -> size(x) + sizeList(rest)

fn mirror(t: Tree) -> Tree
    match t
        Tree.Leaf -> Tree.Leaf
        Tree.Node(kids) -> Tree.Node(List.reverse(mirrorList(kids)))

fn mirrorList(ts: List<Tree>) -> List<Tree>
    match ts
        [] -> []
        [x, ..rest] -> List.prepend(mirror(x), mirrorList(rest))

verify size law atLeastOne
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    size(t) >= 1 => true

verify size law mirrorPreservesSize
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    size(mirror(t)) => size(t)
"#;

const ORG_SRC: &str = r#"module OrgChart
    intent = "Cross-domain container-induction witness: org-chart headcount."
    effects []

type Employee
    Ic
    Manager(List<Employee>)

fn headcount(e: Employee) -> Int
    match e
        Employee.Ic -> 1
        Employee.Manager(reports) -> 1 + headcountList(reports)

fn headcountList(es: List<Employee>) -> Int
    match es
        [] -> 0
        [x, ..rest] -> headcount(x) + headcountList(rest)

verify headcount law atLeastOne
    given e: Employee = [Employee.Ic, Employee.Manager([]), Employee.Manager([Employee.Ic, Employee.Ic])]
    headcount(e) >= 1 => true
"#;

// The EQUATIONAL family in a foreign domain, structurally varied from the `mirror`
// witness (not an alpha-rename): the walker has NON-UNIT constants (a `Blob`
// base of 4 and a `Dir` increment of 2, vs `size`'s `1`/`1 +`), so the closer
// must carry those literals through the node case's `simp`/`omega` — it exercises
// the lift's numeric parameters, not just names. `sortEntries` reverses each
// directory listing and preserves total usage; the second (cross-domain) witness
// that the `walker_reverse` container lemma is DERIVED (a `usageList_reverse` over
// `List Entry`, name-blind), not a size/`sizeList`-specific template.
const DISK_EQ_SRC: &str = r#"module DiskUsage
    intent = "Cross-domain equational container witness with non-unit walker constants: reordering a directory preserves total usage."
    effects []

type Entry
    Blob
    Dir(List<Entry>)

fn usage(e: Entry) -> Int
    match e
        Entry.Blob -> 4
        Entry.Dir(items) -> 2 + usageList(items)

fn usageList(es: List<Entry>) -> Int
    match es
        [] -> 0
        [x, ..rest] -> usage(x) + usageList(rest)

fn sortEntries(e: Entry) -> Entry
    match e
        Entry.Blob -> Entry.Blob
        Entry.Dir(items) -> Entry.Dir(List.reverse(sortList(items)))

fn sortList(es: List<Entry>) -> List<Entry>
    match es
        [] -> []
        [x, ..rest] -> List.prepend(sortEntries(x), sortList(rest))

verify usage law reorderPreservesUsage
    given e: Entry = [Entry.Blob, Entry.Dir([]), Entry.Dir([Entry.Blob, Entry.Blob])]
    usage(sortEntries(e)) => usage(e)
"#;

// TailCall blindness — a GENUINE forced pair whose walker node-arm call sits in
// TAIL position (`Tree.Node(kids) -> sizeList(kids)`, no `1 +`). The TCO pass
// rewrites that call to `Expr::TailCall` before law_auto runs; before the walker
// fix the recognizer's `call_on_binder` saw only `Expr::FnCall` and so silently
// declined this shape to bounded. It must now be recognized and FLIP universal.
const TAIL_SRC: &str = r#"module TreeTail
    intent = "Genuine forced pair whose node-arm call is in tail position."
    effects []

type Tree
    Leaf
    Node(List<Tree>)

fn size(t: Tree) -> Int
    match t
        Tree.Leaf -> 1
        Tree.Node(kids) -> sizeList(kids)

fn sizeList(ts: List<Tree>) -> Int
    match ts
        [] -> 0
        [x, ..rest] -> size(x) + sizeList(rest)

verify size law nonNeg
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    size(t) >= 0 => true
"#;

// TailCall blindness, the other direction — a >2-fn SCC one of whose back-edges
// is a tail call (`size`'s leaf arm `sizeTail(t)`). Before the fix the SCC walker
// missed that edge, under-computed the mutual SCC to exactly `{size, sizeList}`,
// and WRONGLY ADMITTED the shape (emitting `size.induct`, then sorry-flooring — a
// contract violation, not a red build). The recognizer must now see the tail
// edge, compute the true 3-fn SCC, and DECLINE (no `motive2` emitted).
const SCC3_SRC: &str = r#"module TreeScc3
    intent = "Three-fn SCC where the extra back-edge is a tail call."
    effects []

type Tree
    Leaf
    Node(List<Tree>)

fn size(t: Tree) -> Int
    match t
        Tree.Leaf -> sizeTail(t)
        Tree.Node(kids) -> 1 + sizeList(kids)

fn sizeList(ts: List<Tree>) -> Int
    match ts
        [] -> 0
        [x, ..rest] -> size(x) + sizeList(rest)

fn sizeTail(t: Tree) -> Int
    match t
        Tree.Leaf -> 1
        Tree.Node(kids) -> 1 + sizeList(kids)

verify size law atLeastOne
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    size(t) >= 1 => true
"#;

// Non-canonical constructor order — the walker matches the `Node` arm FIRST. The
// joint `f.induct` numbers its cases in source-arm order, so a node-first source
// shifts `case1..case4` and the positional closers sorry-floor. The recognizer
// declines this fail-closed (leaf-first / nil-first is required), so no `motive2`
// is emitted and the law falls back to its bounded sorry — never a red build.
const SWAP_SRC: &str = r#"module TreeSwap
    intent = "Node-arm-first source: non-canonical constructor order."
    effects []

type Tree
    Leaf
    Node(List<Tree>)

fn size(t: Tree) -> Int
    match t
        Tree.Node(kids) -> 1 + sizeList(kids)
        Tree.Leaf -> 1

fn sizeList(ts: List<Tree>) -> Int
    match ts
        [] -> 0
        [x, ..rest] -> size(x) + sizeList(rest)

verify size law atLeastOne
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    size(t) >= 1 => true
"#;

const ACC_SRC: &str = r#"module TreeAcc
    intent = "Accumulator-through-container negative control."
    effects []

type Tree
    Leaf
    Node(List<Tree>)

fn size(t: Tree) -> Int
    match t
        Tree.Leaf -> 1
        Tree.Node(kids) -> 1 + sizeList(kids)

fn sizeList(ts: List<Tree>) -> Int
    match ts
        [] -> 0
        [x, ..rest] -> size(x) + sizeList(rest)

fn sizeAcc(t: Tree, acc: Int) -> Int
    match t
        Tree.Leaf -> acc + 1
        Tree.Node(kids) -> sizeListAcc(kids, acc + 1)

fn sizeListAcc(ts: List<Tree>, acc: Int) -> Int
    match ts
        [] -> acc
        [x, ..rest] -> sizeListAcc(rest, sizeAcc(x, acc))

verify sizeAcc law accEqualsDirect
    given t: Tree = [Tree.Leaf, Tree.Node([]), Tree.Node([Tree.Leaf, Tree.Leaf])]
    sizeAcc(t, 0) => size(t)
"#;

/// Emit `src` to a Lean project, run `--check --check-json`, and return the
/// parsed summary plus the emitted `<Module>.lean` source. Skips (returns `None`)
/// when `lake` is unavailable.
fn check_container(
    src: &str,
    module_file: &str,
    prefix: &str,
) -> Option<(serde_json::Value, String)> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping container-induction test: `lake` not available");
        return None;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let dir = temp_output_dir(&format!("{prefix}-src"));
    std::fs::create_dir_all(&dir).expect("create src dir");
    std::fs::write(dir.join("m.av"), src).expect("write m.av");
    let out = temp_output_dir(&format!("{prefix}-out"));
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(dir.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join(module_file)).expect("read emitted Lean");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    let _ = std::fs::remove_dir_all(&dir);
    let _ = std::fs::remove_dir_all(&out);
    Some((summary, lean))
}

#[test]
fn proof_lean_container_induction_inequality_and_equational_flip_universal() {
    // Both laws over the same `size`/`sizeList` forced pair flip from a
    // `simp <;> done` sorry (bounded) to a GENUINE universal: the inequality
    // `size t >= 1` (motive2 bound READ OFF the walker's `sizeList [] = 0` base,
    // NOT the claim's `1`), and the equational `size (mirror t) = size t` (R_naive
    // lift + the reusable `sizeList_reverse` container lemma resurfacing the
    // dropped reverse in the node case).
    let Some((summary, lean)) = check_container(TREE_SRC, "TreeMirror.lean", "aver-container-tree")
    else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(true), Some(0), Some(true), Some(2)),
        "container-induction laws must kernel-prove as GENUINE universals \
         (passed, 0 sorries, universal:true, both laws credited):\n{lean}"
    );
    // Pin that the close is genuinely the container arm, not an accidental
    // `simp` fluke: the walker's functional-induction principle drives it, the
    // sibling motive is supplied, and the container lemma is emitted.
    assert!(
        lean.contains("size.induct")
            && lean.contains("motive2 :=")
            && lean.contains("sizeList_reverse"),
        "expected `size.induct (motive2 := ..)` + the `sizeList_reverse` container \
         lemma in the emitted proof:\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_cross_domain_name_blind() {
    // The recognizer keys on the mutual-pair SHAPE, never on names: an org-chart
    // `headcount`/`headcountList` over `Employee { Ic; Manager(List<Employee>) }`
    // — same idiom, foreign names — flips to universal identically.
    let Some((summary, lean)) = check_container(ORG_SRC, "OrgChart.lean", "aver-container-org")
    else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(true), Some(0), Some(true), Some(1)),
        "the name-blind cross-domain witness must kernel-prove universally:\n{lean}"
    );
    assert!(
        lean.contains("headcount.induct") && lean.contains("motive2 :="),
        "expected `headcount.induct (motive2 := ..)` in the emitted proof:\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_equational_container_lemma_name_blind() {
    // The container lemma (`walker_reverse`) is DERIVED, not a size-specific
    // sidecar: the same equational shape in a foreign domain — with NON-UNIT
    // walker constants (`Blob` base 4, `Dir` increment 2), so the close is not a
    // constants-of-`1` coincidence — emits and cites a `usageList_reverse` over
    // `List Entry` and closes universally.
    let Some((summary, lean)) =
        check_container(DISK_EQ_SRC, "DiskUsage.lean", "aver-container-disk")
    else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(true), Some(0), Some(true), Some(1)),
        "the cross-domain equational witness must kernel-prove universally:\n{lean}"
    );
    assert!(
        lean.contains("usage.induct")
            && lean.contains("usageList_reverse")
            && !lean.contains("sizeList_reverse"),
        "expected a DERIVED `usageList_reverse` container lemma (name-blind, \
         not a `sizeList` template):\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_tail_position_node_arm_flips_universal() {
    // TailCall blindness (SHOULD-FIX): a genuine forced pair whose walker node-arm
    // call is in TAIL position (`Tree.Node(kids) -> sizeList(kids)`) is rewritten
    // to `Expr::TailCall` by the TCO pass before law_auto. Before the walker fix
    // the recognizer saw only `Expr::FnCall` and declined this to bounded; it must
    // now be recognized and FLIP to a GENUINE universal.
    let Some((summary, lean)) = check_container(TAIL_SRC, "TreeTail.lean", "aver-container-tail")
    else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(true), Some(0), Some(true), Some(1)),
        "the tail-position forced pair must kernel-prove as a GENUINE universal:\n{lean}"
    );
    assert!(
        lean.contains("size.induct") && lean.contains("motive2 :="),
        "expected `size.induct (motive2 := ..)` in the emitted proof:\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_three_fn_scc_via_tail_call_declines() {
    // TailCall blindness (the ADMISSION direction): a >2-fn SCC one of whose
    // back-edges is a tail call (`size`'s leaf arm `sizeTail(t)`). Before the fix
    // the SCC walker missed that edge, under-computed the SCC to `{size, sizeList}`
    // and WRONGLY ADMITTED the shape (emitting `size.induct`, then sorry-flooring).
    // The recognizer must now see the tail edge and DECLINE at recognition — no
    // `motive2` emitted, no red build.
    let Some((summary, lean)) = check_container(SCC3_SRC, "TreeScc3.lean", "aver-container-scc3")
    else {
        return;
    };
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["build_errors"].as_u64(),
        ),
        (Some(false), Some(0)),
        "the 3-fn SCC law must decline (universal:false) without going red \
         (build_errors:0):\n{lean}"
    );
    assert!(
        !lean.contains("size.induct") && !lean.contains("motive2 :="),
        "the 3-fn SCC law must DECLINE at recognition (no container-induction \
         proof emitted):\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_non_canonical_arm_order_declines() {
    // Constructor order (NOTE, fail-closed): a walker whose `match` puts the `Node`
    // arm FIRST shifts the joint `f.induct` case numbering, so the positional
    // closers would sorry-floor. The recognizer requires canonical leaf-first /
    // nil-first order and declines anything else at recognition — no `motive2`
    // emitted, bounded fallback, never a red build.
    let Some((summary, lean)) = check_container(SWAP_SRC, "TreeSwap.lean", "aver-container-swap")
    else {
        return;
    };
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["build_errors"].as_u64(),
        ),
        (Some(false), Some(0)),
        "the node-arm-first law must decline (universal:false) without going red \
         (build_errors:0):\n{lean}"
    );
    assert!(
        !lean.contains("size.induct") && !lean.contains("motive2 :="),
        "the non-canonical-order law must DECLINE at recognition:\n{lean}"
    );
}

#[test]
fn proof_lean_container_induction_accumulator_declines_to_bounded() {
    // NEGATIVE: `sizeAcc t 0 = size t` threads an accumulator. The pointwise lift
    // yields the FALSE leaf obligation `acc + 1 = 1` (measured: `E4_acc_naive`),
    // so the arm must DECLINE (the 2-param, non-list-walker subject fails the pair
    // recognizer) and the law falls back to its bounded sorry — never a false
    // universal, and never a red build.
    let Some((summary, lean)) = check_container(ACC_SRC, "TreeAcc.lean", "aver-container-acc")
    else {
        return;
    };
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["build_errors"].as_u64(),
        ),
        (Some(false), Some(0)),
        "the accumulator law must decline (universal:false) without going red \
         (build_errors:0):\n{lean}"
    );
    assert!(
        !lean.contains("sizeAcc.induct") && !lean.contains("motive2 :="),
        "the accumulator law must NOT emit the container-induction proof:\n{lean}"
    );
}
