//! Literal cons-tail descent should use Lean's structural recursor, while
//! computed slices, and a tail the body matches on again, retain their checked
//! list-length measure.

use super::*;

const SOURCE: &str = r#"module StructuralList
    intent = "Only literal cons tails qualify for structural termination."

fn sumFrom(acc: Int, values: List<Int>) -> Int
    match values
        [] -> acc
        [head, ..tail] -> sumFrom(acc + head, tail)

verify sumFrom
    sumFrom(7, [1, 2, 3]) => 13

fn pairs(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> List.reverse(acc)
        [first, ..afterFirst] -> match afterFirst
            [] -> List.reverse(List.prepend(first, acc))
            [second, ..rest] -> pairs(rest, List.prepend(first + second, acc))

verify pairs
    pairs([1, 2, 3, 4], []) => [3, 7]
    pairs([1, 2, 3], []) => [3, 3]

fn ascending(values: List<Int>) -> Bool
    match values
        [] -> true
        [first, ..afterFirst] -> match afterFirst
            [] -> true
            [second, ..rest] -> match first <= second
                true -> ascending(afterFirst)
                false -> false

verify ascending
    ascending([1, 2, 3]) => true
    ascending([2, 1]) => false

fn sliced(values: List<Int>, count: Int) -> Int
    match values
        [] -> 0
        [head, ..tail] -> 1 + sliced(List.drop(tail, count), count)

verify sliced
    sliced([1, 2, 3], 0) => 3
    sliced([1, 2, 3], 1) => 2
"#;

#[test]
fn literal_tail_walks_are_structural_but_computed_slices_stay_well_founded() {
    let source_dir = tempfile::tempdir().unwrap();
    let file = source_dir.path().join("structural_list.av");
    std::fs::write(&file, SOURCE).unwrap();
    let output = tempfile::tempdir().unwrap();
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .arg("--examples")
        .arg(&file)
        .arg("-o")
        .arg(output.path())
        .output()
        .unwrap();
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output.path().join("StructuralList.lean")).unwrap();
    assert_eq!(
        lean.matches("termination_by structural values").count(),
        2,
        "{lean}"
    );
    assert_eq!(
        lean.matches("termination_by values.length").count(),
        2,
        "{lean}"
    );
    assert!(
        !lean.contains("partial def") && !lean.contains("__fuel"),
        "{lean}"
    );
    if !lean_required::lake_available() {
        eprintln!("skipping structural list proof check: `lake` not available");
        return;
    }
    let (summary, run) = run_lean_check_json(file.to_str().unwrap(), output.path(), 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["passed"], true, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    // Recursive equations remain usable with symbolic tails and arbitrary
    // accumulators, without native_decide or additional axioms.
    let audit = output.path().join("StructuralListAudit.lean");
    std::fs::write(
        &audit,
        r#"import Lean
import StructuralList
open StructuralList
theorem sum_step (acc head : Int) (tail : List Int) :
    sumFrom acc (head :: tail) = sumFrom (acc + head) tail := by rfl
theorem pairs_step (a b : Int) (tail acc : List Int) :
    pairs (a :: b :: tail) acc = pairs tail ((a + b) :: acc) := by rfl
open Lean in
run_cmd do
  for theoremName in [``sum_step, ``pairs_step] do
    let axioms ← collectAxioms theoremName
    unless axioms.isEmpty do throwError "structural equation acquired axioms: {axioms}"
"#,
    )
    .unwrap();
    let audit = Command::new("lake")
        .args(["env", "lean"])
        .arg(audit)
        .current_dir(output.path())
        .output()
        .unwrap();
    assert!(audit.status.success(), "{}", format_output(&audit));
}
