//! Lean 4.33 stopped silently unfolding semireducible definitions in some
//! implicit arguments. Unfolding an `ite` predicate with `simp` can leave its
//! Decidable instance indexed by the old predicate. Keep this tiny kernel test
//! beside the full rounding-family integration tests that exercise emission.

const SOURCE: &str = r#"
import Std
import Lean

def shifted (x : Int) : Int := x + 1
def absoluteShift (x : Int) : Int :=
  if shifted x < 0 then 0 - shifted x else shifted x
def bound (x : Int) : Bool := decide (absoluteShift x = x + 1)

theorem rewriteBoundary (x : Int) (h : 0 <= x + 1) : absoluteShift x = x + 1 := by
  dsimp +instances only [absoluteShift, shifted]
  rw [if_neg (by omega : Not (x + 1 < 0))]

theorem leaf (a : Int) (h : 0 <= a) : (if a < 0 then 0 - a else a) = a := by
  rw [if_neg (by omega : Not (a < 0))]

theorem applyBoundary (x : Int) (h : 0 <= x + 1) : bound x = true := by
  dsimp +instances only [bound, absoluteShift, shifted]
  simp only [decide_eq_true_eq]
  apply leaf
  exact h

#print axioms rewriteBoundary
#print axioms applyBoundary
"#;

#[test]
fn definitional_unfolding_keeps_decidable_instances_valid_for_rewrite_and_apply() {
    let dir = tempfile::Builder::new()
        .prefix("aver-decidable-normalization-")
        .tempdir()
        .unwrap();
    std::fs::write(
        dir.path().join("lean-toolchain"),
        super::super::prelude::generate_toolchain(),
    )
    .unwrap();
    std::fs::write(dir.path().join("Normalization.lean"), SOURCE).unwrap();
    let output = match std::process::Command::new("lean")
        .arg("Normalization.lean")
        .current_dir(dir.path())
        .output()
    {
        Ok(output) => output,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return,
        Err(error) => panic!("run pinned Lean normalization regression: {error}"),
    };
    let transcript = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.status.success(), "{transcript}");
    assert!(transcript.contains("rewriteBoundary"), "{transcript}");
    assert!(transcript.contains("applyBoundary"), "{transcript}");
    assert!(!transcript.contains("sorryAx"), "{transcript}");
}
