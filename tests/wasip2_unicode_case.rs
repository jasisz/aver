#![cfg(feature = "wasip2")]

//! Regression — `String.toLower` / `String.toUpper` under
//! `aver run --wasip2` must print exactly what Rust's
//! `str::to_lowercase` / `str::to_uppercase` produce.
//!
//! wasip2 shares the wasm-gc lowering, so the semantics are proven by
//! `tests/wasm_gc_unicode_case_regression.rs`. What this file adds is
//! the wrapping guard: the case helpers now read a passive data
//! segment and cache it in a wasm global, and the component encoder
//! (`src/codegen/wasip2/wrap.rs`) validates the core module before
//! wrapping it. A module that carries a global and an extra data
//! segment must still encode, instantiate, and run.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

/// Text that exercises every shape the ASCII helper could not express:
/// non-ASCII letters, a one-to-many expansion (`ß`), a mapping that
/// changes UTF-8 length (`ı`, `İ`), a final sigma and a medial one, a
/// sigma after an ASCII letter, a sigma whose neighbours are
/// case-ignorable, and an astral pair (Deseret).
const SAMPLE: &str = "ĄĆĘ ŁÓŚ ß ÀÉÎ ΩΔ ΑΣ ΑΣΒ ΣΣ aΣ aΣb ΑΣ'Β ΑΣ' aΣ\u{02B0}b İ ı ﬀ 𐐀𐐨 abcXYZ";

/// All Unicode `White_Space` scalar values used by Rust `str::trim`.
const TRIM_WHITE_SPACE: &str = "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{00A0}\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}";

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-case-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

fn run_wasip2(dir: &Path, fixture: &Path) -> std::process::Output {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    Command::new(aver_bin)
        .current_dir(dir)
        .arg("run")
        .arg("--wasip2")
        .arg(fixture)
        .output()
        .expect("aver run --wasip2 to launch")
}

/// Aver has no `\u` escape, so scalars go into the literal as raw
/// UTF-8; only what the lexer reads specially needs escaping.
fn escape_aver(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{000C}' => out.push_str("\\f"),
            _ => out.push(c),
        }
    }
    out
}

#[test]
fn wasip2_case_mapping_matches_rust() {
    let dir = tempdir("case");
    let literal = escape_aver(SAMPLE);
    let src = format!(
        r#"
fn main() -> Unit
    ! [Console.print]
    lower: String = String.toLower("{literal}")
    upper: String = String.toUpper("{literal}")
    Console.print("{{lower}}|{{upper}}")
"#
    );
    let fixture = write_fixture(&dir, "case.av", &src);
    let out = run_wasip2(&dir, &fixture);
    assert!(
        out.status.success(),
        "case.av failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let got = String::from_utf8_lossy(&out.stdout).into_owned();
    let want = format!("{}|{}\n", SAMPLE.to_lowercase(), SAMPLE.to_uppercase());
    assert_eq!(got, want, "wasip2 case mapping diverged from Rust std");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn wasip2_trim_matches_rust_unicode_whitespace() {
    let dir = tempdir("trim");
    let sample = format!("{TRIM_WHITE_SPACE}x{TRIM_WHITE_SPACE}");
    let literal = escape_aver(&sample);
    let src = format!(
        r#"
fn main() -> Unit
    ! [Console.print]
    text = "{literal}"
    Console.print("[{{String.trim(text)}}]")
"#
    );
    let fixture = write_fixture(&dir, "trim.av", &src);
    let out = run_wasip2(&dir, &fixture);
    assert!(
        out.status.success(),
        "trim.av failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let got = String::from_utf8_lossy(&out.stdout).into_owned();
    let want = format!("[{}]\n", sample.trim());
    assert_eq!(got, want, "wasip2 trim diverged from Rust std");
    let _ = std::fs::remove_dir_all(&dir);
}
