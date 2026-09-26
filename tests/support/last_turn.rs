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

/// One line the fixture prints: its label, turn, waited and worked.
struct Reading<'a> {
    label: &'a str,
    turn: i64,
    waited: i64,
    worked: i64,
}

/// `label turn T waited W worked K`.
fn parse(line: &str) -> Option<Reading<'_>> {
    let (label, rest) = line.split_once(" turn ")?;
    let (turn, rest) = rest.split_once(" waited ")?;
    let (waited, worked) = rest.split_once(" worked ")?;
    Some(Reading {
        label,
        turn: turn.parse().ok()?,
        waited: waited.parse().ok()?,
        worked: worked.parse().ok()?,
    })
}

/// Check one run's stdout: while seated all three are 0; every later line
/// names a later turn than the one before it; a tick waited roughly its
/// deadline, and the turn before it worked far less; the tick that parks
/// until the next turn waited for nothing.
pub fn check(stdout: &str) -> Result<(), String> {
    let lines: Vec<&str> = stdout.lines().collect();
    let readings: Vec<Reading<'_>> = lines.iter().filter_map(|line| parse(line)).collect();
    let labels: Vec<&str> = readings.iter().map(|reading| reading.label).collect();
    if labels != LABELS || readings.len() != lines.len() {
        return Err(format!("unexpected lines:\n{stdout}"));
    }
    let small = DEADLINE_MS / 2;
    let mut previous_turn = -1;
    for Reading {
        label,
        turn,
        waited,
        worked,
    } in readings
    {
        let fits = turn > previous_turn
            && match label {
                "seated" => turn == 0 && waited == 0 && worked == 0,
                "soon 4" => (0..small).contains(&waited) && (0..small).contains(&worked),
                _ => (small..DEADLINE_MS * 20).contains(&waited) && (0..small).contains(&worked),
            };
        if !fits {
            return Err(format!(
                "`{label}` in turn {turn} waited {waited} ms and worked {worked} ms, outside its bounds:\n{stdout}"
            ));
        }
        previous_turn = turn;
    }
    Ok(())
}
