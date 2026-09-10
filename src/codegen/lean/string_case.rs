//! Unicode case conversion from the same VM-checked tables as Dafny and wasm.
//! Lean's native String.toLower/toUpper cover ASCII only. In particular, a
//! Char -> Char map cannot represent expansions or contextual final sigma.

use crate::codegen::unicode_case::{
    CASE_IGNORABLE, CASED, Expansion, LOWER_EXPAND, LOWER_SIMPLE, SimpleRun, UPPER_EXPAND,
    UPPER_SIMPLE, ranges, simple,
};
use std::sync::OnceLock;

pub(super) fn source() -> &'static str {
    static SOURCE: OnceLock<String> = OnceLock::new();
    SOURCE.get_or_init(|| {
        format!(
            "namespace AverUnicodeCase\n{}\n{}\n\
             def cased (c : Char) : Bool :=\n  let n : Int := c.toNat\n  {}\n\
             def ignorable (c : Char) : Bool :=\n  let n : Int := c.toNat\n  {}\n\
             {CONVERSION}\nend AverUnicodeCase\n",
            scalar_map("lower", LOWER_SIMPLE, LOWER_EXPAND),
            scalar_map("upper", UPPER_SIMPLE, UPPER_EXPAND),
            ranges(CASED),
            ranges(CASE_IGNORABLE),
        )
    })
}

fn scalar_map(name: &str, runs: &[SimpleRun], expansions: &[Expansion]) -> String {
    let mut body = format!("[Char.ofNat ({name}Code n).toNat]");
    for (from, to) in expansions.iter().rev() {
        let pieces = to
            .iter()
            .filter(|&&cp| cp != 0)
            .map(|cp| format!("Char.ofNat {cp}"))
            .collect::<Vec<_>>()
            .join(", ");
        body = format!("if n == {from} then [{pieces}] else\n  {body}");
    }
    format!(
        "def {name}Code (n : Int) : Int :=\n  {}\n\
         def {name}Char (c : Char) : List Char :=\n  let n : Int := c.toNat\n  {body}\n",
        simple(runs),
    )
}

const CONVERSION: &str = r#"
def afterCased : List Char → Bool
  | [] => false
  | c :: cs => if ignorable c then afterCased cs else cased c

def lowerChars (before : Bool) : List Char → List Char
  | [] => []
  | c :: cs =>
    let head := if c.toNat == 0x03A3 && before && !afterCased cs
      then [Char.ofNat 0x03C2] else lowerChar c
    let next := if ignorable c then before else cased c
    head ++ lowerChars next cs

def toLower (s : String) : String := String.ofList (lowerChars false s.toList)
def toUpper (s : String) : String := String.ofList (s.toList.flatMap upperChar)
"#;
