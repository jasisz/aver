//! Definition-backed Unicode case conversion. The mapping and context tables
//! are shared with wasm-gc and exhaustively checked against the VM's Rust std.
//! Balanced decision trees keep lookup depth logarithmic without adding axioms.

use crate::codegen::unicode_case::{
    CASE_IGNORABLE, CASED, Expansion, LOWER_EXPAND, LOWER_SIMPLE, SimpleRun, UPPER_EXPAND,
    UPPER_SIMPLE, ranges, simple,
};
use std::sync::OnceLock;

pub(super) fn source() -> &'static str {
    static SOURCE: OnceLock<String> = OnceLock::new();
    SOURCE.get_or_init(|| {
        let lower = scalar_map("Lower", LOWER_SIMPLE, LOWER_EXPAND);
        let upper = scalar_map("Upper", UPPER_SIMPLE, UPPER_EXPAND);
        format!(
            "{CORE}\n{lower}\n{upper}\nfunction StringCaseCased(n: int): bool {{\n{}\n}}\n\
             function StringCaseIgnorable(n: int): bool {{\n{}\n}}\n{CONVERSION}",
            ranges(CASED),
            ranges(CASE_IGNORABLE),
        )
    })
}

fn scalar_map(name: &str, runs: &[SimpleRun], expansions: &[Expansion]) -> String {
    let mut body = format!("StringCaseScalar(StringCase{name}Code(n))");
    for (from, to) in expansions.iter().rev() {
        let pieces = to
            .iter()
            .filter(|&&cp| cp != 0)
            .map(|cp| format!("StringCaseScalar({cp})"))
            .collect::<Vec<_>>()
            .join(" + ");
        body = format!("if n == {from} then {pieces} else\n{body}");
    }
    format!(
        "function StringCase{name}Code(n: int): int {{\n{}\n}}\n\
         function StringCase{name}Char(c: char): string {{\nvar n := c as int;\n{body}\n}}",
        simple(runs),
    )
}

const CORE: &str = r#"
// Invalid scalars are unreachable from the checked Unicode tables. Keeping
// this helper total makes that boundary explicit without a trusted contract.
function StringCaseScalar(n: int): string {
  if 0 <= n < 0x110000 && !(0xD800 <= n < 0xE000) then [n as char] else ""
}
"#;

const CONVERSION: &str = r#"
// The original input supplies both sides of the final-sigma context. A
// character may expand, so positions in the converted output cannot be used.
function StringCaseBefore(s: string, i: int): bool
  requires 0 <= i <= |s|
  decreases i
{
  if i == 0 then false
  else if StringCaseIgnorable(s[i-1] as int) then StringCaseBefore(s, i-1)
  else StringCaseCased(s[i-1] as int)
}

function StringCaseAfter(s: string, i: int): bool
  requires 0 <= i <= |s|
  decreases |s| - i
{
  if i == |s| then false
  else if StringCaseIgnorable(s[i] as int) then StringCaseAfter(s, i+1)
  else StringCaseCased(s[i] as int)
}

function StringLowerFrom(s: string, i: int): string
  requires 0 <= i <= |s|
  decreases |s| - i
{
  if i == |s| then ""
  else
    (if s[i] as int == 0x03A3 && StringCaseBefore(s, i) && !StringCaseAfter(s, i+1)
     then StringCaseScalar(0x03C2) else StringCaseLowerChar(s[i])) + StringLowerFrom(s, i+1)
}

function StringToLower(s: string): string { StringLowerFrom(s, 0) }

function StringToUpper(s: string): string
  decreases |s|
{
  if |s| == 0 then "" else StringCaseUpperChar(s[0]) + StringToUpper(s[1..])
}
"#;
