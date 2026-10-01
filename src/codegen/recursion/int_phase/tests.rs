use super::*;
use crate::ast::TopLevel;

fn functions(source: &str) -> Vec<FnDef> {
    crate::source::parse_source(source)
        .unwrap()
        .into_iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd)
            } else {
                None
            }
        })
        .collect()
}

const ASCENDING: &str = r#"fn scan(label: String, from: Int, target: Int) -> Int
    match from >= target
        true -> from
        false -> advance(label, from, target)
fn advance(label: String, position: Int, end: Int) -> Int
    scan(label, position + 1, end)
"#;

#[test]
fn a_phase_measure_is_independent_of_names_and_member_order() {
    let fns = functions(ASCENDING);
    assert_eq!(detect(&[&fns[0], &fns[1]]), Some((1, Some(2), 0)));
    assert_eq!(detect(&[&fns[1], &fns[0]]), Some((1, Some(2), 1)));
    let fns = functions(
        r#"fn down(label: String, n: Int) -> Int
    match n <= 0
        true -> 0
        false -> step(label, n)
fn step(label: String, remaining: Int) -> Int
    down(label, remaining - 1)
"#,
    );
    assert_eq!(detect(&fns.iter().collect::<Vec<_>>()), Some((1, None, 0)));
}

#[test]
fn phase_measures_require_guard_progress_and_preservation_on_every_edge() {
    for source in [
        ASCENDING.replace("from >= target", "from < target"),
        ASCENDING.replace("from >= target", "from > target"),
        ASCENDING.replace("position + 1", "position"),
        ASCENDING.replace("position + 1", "position - 1"),
        ASCENDING.replace("position + 1, end", "position + 1, end + 1"),
        ASCENDING.replace(
            "advance(label, from, target)",
            "advance(label, from, target + 1)",
        ),
        ASCENDING.replace("true -> from", "true -> advance(label, from, target)"),
        ASCENDING.replace(
            "scan(label, position + 1, end)",
            "scan(label, scan(label, position, end), end)",
        ),
        ASCENDING.replace(
            "scan(label, position + 1, end)",
            "advance(label, position + 1, end)",
        ),
    ] {
        let fns = functions(&source);
        assert_eq!(detect(&fns.iter().collect::<Vec<_>>()), None, "{source}");
    }
}

#[test]
fn local_or_pattern_shadowing_cannot_supply_a_phase_measure() {
    for body in [
        "position = 1\n    scan(label, position + 1, end)",
        "end = 1\n    scan(label, position + 1, end)",
        "match 1\n        position -> scan(label, position + 1, end)",
        "match 1\n        end -> scan(label, position + 1, end)",
    ] {
        let source = ASCENDING.replace("scan(label, position + 1, end)", body);
        let fns = functions(&source);
        assert_eq!(detect(&fns.iter().collect::<Vec<_>>()), None, "{source}");
    }
}
