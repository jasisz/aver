//! What `tests/fixtures/run_last_turn` prints, read back with tolerant bounds.
//!
//! The ticker reports `Run.lastTurn()` once while it is seated, after each of
//! three ticks that park on a deadline, and after one tick that parks until
//! the next turn. The numbers are clock readings, so the backends agree on
//! the lines and on the bounds, never on the digits.

/// The deadline every tick of the fixture parks on (`Ticks.tick`).
pub const DEADLINE_MS: i64 = 250;

/// The labels the fixture prints, in order.
pub const LABELS: [&str; 5] = ["seated", "tick 1", "tick 2", "tick 3", "soon 4"];

/// `label waited W worked K` → `(label, W, K)`.
fn parse(line: &str) -> Option<(&str, i64, i64)> {
    let (label, rest) = line.split_once(" waited ")?;
    let (waited, worked) = rest.split_once(" worked ")?;
    Some((label, waited.parse().ok()?, worked.parse().ok()?))
}

/// Check one run's stdout: while seated both are 0; a tick waited roughly
/// its deadline, and the turn before it worked far less; the tick that parks
/// until the next turn waited for nothing.
pub fn check(stdout: &str) -> Result<(), String> {
    let lines: Vec<&str> = stdout.lines().collect();
    let readings: Vec<(&str, i64, i64)> = lines.iter().filter_map(|line| parse(line)).collect();
    let labels: Vec<&str> = readings.iter().map(|(label, _, _)| *label).collect();
    if labels != LABELS || readings.len() != lines.len() {
        return Err(format!("unexpected lines:\n{stdout}"));
    }
    let small = DEADLINE_MS / 2;
    for (label, waited, worked) in readings {
        let fits = match label {
            "seated" => waited == 0 && worked == 0,
            "soon 4" => (0..small).contains(&waited) && (0..small).contains(&worked),
            _ => (small..DEADLINE_MS * 20).contains(&waited) && (0..small).contains(&worked),
        };
        if !fits {
            return Err(format!(
                "`{label}` waited {waited} ms and worked {worked} ms, outside its bounds:\n{stdout}"
            ));
        }
    }
    Ok(())
}
