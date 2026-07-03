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
