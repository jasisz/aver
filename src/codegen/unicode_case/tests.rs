use super::*;

#[derive(Default)]
struct Derived {
    lower_simple: Vec<SimpleRun>,
    upper_simple: Vec<SimpleRun>,
    lower_expand: Vec<Expansion>,
    upper_expand: Vec<Expansion>,
    cased: Vec<Range>,
    case_ignorable: Vec<Range>,
    max_lower_growth: u32,
    max_upper_growth: u32,
}

/// `Σ` and `ς` — the two scalars the context probes are built from.
const SIGMA: char = '\u{03A3}';
const FINAL_SIGMA: char = '\u{03C2}';

fn cased(c: char) -> bool {
    format!("{c}{SIGMA}").to_lowercase().ends_with(FINAL_SIGMA)
}

fn case_ignorable(c: char) -> bool {
    !cased(c) && format!("a{c}{SIGMA}").to_lowercase().ends_with(FINAL_SIGMA)
}

fn push_run(runs: &mut Vec<SimpleRun>, cp: u32, delta: i32) {
    match runs.last_mut() {
        Some(last) if last.1 + 1 == cp && last.2 == delta => last.1 = cp,
        _ => runs.push((cp, cp, delta)),
    }
}

fn push_range(ranges: &mut Vec<Range>, cp: u32) {
    match ranges.last_mut() {
        Some(last) if last.1 + 1 == cp => last.1 = cp,
        _ => ranges.push((cp, cp)),
    }
}

fn expansion(mapped: &[char]) -> [u32; 3] {
    let mut out = [0u32; 3];
    for (slot, c) in out.iter_mut().zip(mapped) {
        *slot = *c as u32;
    }
    out
}

fn growth(mapped: &[char], input: char) -> u32 {
    let out: usize = mapped.iter().map(|c| c.len_utf8()).sum();
    out.div_ceil(input.len_utf8()) as u32
}

/// Re-derive every table from `std` by walking the whole scalar
/// space. Surrogates have no `char` and are skipped, which also
/// stops any run or range from spanning the surrogate block.
fn derive_from_std() -> Derived {
    let mut d = Derived {
        max_lower_growth: 1,
        max_upper_growth: 1,
        ..Derived::default()
    };
    for cp in 0u32..=0x10_FFFF {
        let Some(c) = char::from_u32(cp) else {
            continue;
        };
        let lower: Vec<char> = c.to_lowercase().collect();
        let upper: Vec<char> = c.to_uppercase().collect();
        d.max_lower_growth = d.max_lower_growth.max(growth(&lower, c));
        d.max_upper_growth = d.max_upper_growth.max(growth(&upper, c));
        match lower.as_slice() {
            [one] if *one != c => push_run(&mut d.lower_simple, cp, *one as i32 - cp as i32),
            [_] => {}
            many => d.lower_expand.push((cp, expansion(many))),
        }
        match upper.as_slice() {
            [one] if *one != c => push_run(&mut d.upper_simple, cp, *one as i32 - cp as i32),
            [_] => {}
            many => d.upper_expand.push((cp, expansion(many))),
        }
        if cased(c) {
            push_range(&mut d.cased, cp);
        } else if case_ignorable(c) {
            push_range(&mut d.case_ignorable, cp);
        }
    }
    d
}

fn sorted_and_disjoint(ranges: &[Range]) -> bool {
    ranges
        .windows(2)
        .all(|w| w[0].0 <= w[0].1 && w[0].1 + 1 < w[1].0)
}

/// The drift guard. Red the day the toolchain's Unicode version
/// moves — regenerate with `dump_case_tables` and re-review.
#[test]
fn case_tables_match_std() {
    let d = derive_from_std();
    assert_eq!(d.lower_simple, LOWER_SIMPLE, "LOWER_SIMPLE");
    assert_eq!(d.upper_simple, UPPER_SIMPLE, "UPPER_SIMPLE");
    assert_eq!(d.lower_expand, LOWER_EXPAND, "LOWER_EXPAND");
    assert_eq!(d.upper_expand, UPPER_EXPAND, "UPPER_EXPAND");
    assert_eq!(d.cased, CASED, "CASED");
    assert_eq!(d.case_ignorable, CASE_IGNORABLE, "CASE_IGNORABLE");
    assert_eq!(d.max_lower_growth, MAX_LOWER_GROWTH, "MAX_LOWER_GROWTH");
    assert_eq!(d.max_upper_growth, MAX_UPPER_GROWTH, "MAX_UPPER_GROWTH");
    assert!(sorted_and_disjoint(CASED), "CASED must be sorted, disjoint");
    assert!(
        sorted_and_disjoint(CASE_IGNORABLE),
        "CASE_IGNORABLE must be sorted, disjoint"
    );
    // The probes make the two sets disjoint; the helper relies on
    // it (it tests ignorable first and only then cased).
    for (first, last) in CASED {
        assert!(
            !CASE_IGNORABLE.iter().any(|(f, l)| first <= l && f <= last),
            "CASED and CASE_IGNORABLE overlap around U+{first:04X}"
        );
    }
    // Every table is binary-searched by its first field.
    assert!(
        LOWER_SIMPLE.windows(2).all(|w| w[0].1 < w[1].0),
        "LOWER_SIMPLE must be sorted, disjoint"
    );
    assert!(
        UPPER_SIMPLE.windows(2).all(|w| w[0].1 < w[1].0),
        "UPPER_SIMPLE must be sorted, disjoint"
    );
    assert!(
        LOWER_EXPAND.windows(2).all(|w| w[0].0 < w[1].0),
        "LOWER_EXPAND must be sorted"
    );
    assert!(
        UPPER_EXPAND.windows(2).all(|w| w[0].0 < w[1].0),
        "UPPER_EXPAND must be sorted"
    );
    // `to2 == 0` is the "only two scalars" marker, so a genuine
    // second scalar must never be U+0000.
    assert!(
        LOWER_EXPAND
            .iter()
            .chain(UPPER_EXPAND)
            .all(|(_, to)| to[0] != 0 && to[1] != 0),
        "expansion slots 0 and 1 are always present"
    );
    // 24-bit fields, sign-extended in WAT for the deltas.
    assert!(
        LOWER_SIMPLE
            .iter()
            .chain(UPPER_SIMPLE)
            .all(|(_, _, delta)| (-0x80_0000..0x80_0000).contains(delta)),
        "deltas must fit a signed 24-bit field"
    );
}

/// Print the constants as Rust source. Ignored: it is the
/// regeneration path, not a check.
#[test]
#[ignore]
fn dump_case_tables() {
    let d = derive_from_std();
    println!("// @generated by `dump_case_tables` — see the module docs.");
    println!(
        "pub(in crate::codegen) const LOWER_SIMPLE: &[SimpleRun] = &{:?};",
        d.lower_simple
    );
    println!(
        "pub(in crate::codegen) const UPPER_SIMPLE: &[SimpleRun] = &{:?};",
        d.upper_simple
    );
    println!(
        "pub(in crate::codegen) const LOWER_EXPAND: &[Expansion] = &{:?};",
        d.lower_expand
    );
    println!(
        "pub(in crate::codegen) const UPPER_EXPAND: &[Expansion] = &{:?};",
        d.upper_expand
    );
    println!(
        "pub(in crate::codegen) const CASED: &[Range] = &{:?};",
        d.cased
    );
    println!(
        "pub(in crate::codegen) const CASE_IGNORABLE: &[Range] = &{:?};",
        d.case_ignorable
    );
    println!("// MAX_LOWER_GROWTH = {}", d.max_lower_growth);
    println!("// MAX_UPPER_GROWTH = {}", d.max_upper_growth);
}
