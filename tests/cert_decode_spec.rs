//! Integration tests for the in-kernel certificate profile decoder
//! (`aver-cert/assets/wall/current/CertDecode.lean`).
//!
//! The main test is a decoder differential: for every certkit fixture the Lean
//! kernel decoder (`CertDecode.decode…`, checked by `rfl`) agrees, TERM FOR
//! TERM, with the Python byte-level oracle (`tools/certkit/decode_ref.py`) on
//! every user-function body, the section walk, exports, imports and carrier —
//! and, for the CERTIFIED obligations, additionally with the Rust
//! `cert::rederive_obligations` (three oracles on those; two on the rest).
//! It compiles each fixture, emits a Lean witness pinning a test-only whole-file
//! section walk built from `CertDecode.readU`,
//! `decodeExports`, `decodeImports`, `decodeCarrier`, and per-function
//! `decodeCode` to the oracle values, and lets `lake env lean` be the verdict —
//! a divergence is a kernel `rfl` failure with a full repro, never a string
//! compare (the lesson from the v1 substring checker). It also drives the
//! 39-opcode coverage matrix (33 opcodes exercised by fixture bodies plus 6
//! covered by a synthetic single-body probe) fail-closed.
//!
//! The certkit fixtures are all pure (no import section), so the import-section
//! decode path (`decImportVec`, a non-zero function-index base) is exercised by
//! an extra effectful module the test itself writes and compiles: its
//! `Console.print` lowers to a real import, and the test asserts the decoded
//! import list is non-empty so this coverage cannot silently regress.
//!
//! A second test performs the mutation suite M1–M6 on real bytes: a flip in a
//! certified body changes the decode, a flip in a decoder-skipped section does
//! not, an export relabel changes the export map, and overlong LEB / truncation
//! / out-of-function-space indices all decode to `none` (never a garbage
//! success).
//!
//! Gated behind `wasm` (the wasm-gc backend) and skipped when `lake` or
//! `python3` is unavailable, mirroring `cert_verify_spec.rs`.
#![cfg(feature = "wasm")]

#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use scratch_dir::{ScratchDir, temp_dir};
use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURES: &[&str] = &[
    "certprobe",
    "certprobe2",
    "certkit_ops",
    "certkit_zoo",
    "certempty",
];

/// Full S1 transition corpus. Keep this explicit so adding a certkit fixture
/// requires consciously extending the Rust-splice ↔ kernel-decode regression.
const S1_FIXTURES: &[&str] = &[
    "arity3",
    "bool_window",
    "cell_at",
    "cert_goals",
    "certempty",
    "certkit_ops",
    "certkit_zoo",
    "certprobe",
    "certprobe2",
    "chainsum",
    "clockrange",
    "compose",
    "f64verbatim",
    "intdispatchgen",
    "letnamed",
    "manytypes",
    "meter",
    "mutual",
    "mutual3",
    "offsetrec",
    "opteval",
    "person",
    "rangepred",
    "recdecline",
    "recgen",
    "refinedsum",
    "signalgauge",
    "strdispatch",
    "stringconcat",
    "stringeq",
    "tupleproj",
    "verbatimgen",
    "verbatimwiden",
    "widenedmatch",
];

/// The 39 measured user-code opcode mnemonics (mirrors `diff_harness.py`).
const ALL_OPCODES: &[&str] = &[
    "array.new_data",
    "array.new_fixed",
    "call",
    "else",
    "end",
    "f64.add",
    "f64.const",
    "f64.div",
    "f64.eq",
    "f64.ge",
    "f64.gt",
    "f64.le",
    "f64.lt",
    "f64.mul",
    "f64.sub",
    "i32.and",
    "i32.const",
    "i32.eq",
    "i32.gt_s",
    "i32.le_s",
    "i32.lt_s",
    "i64.const",
    "i64.eq",
    "i64.eqz",
    "i64.ge_s",
    "i64.gt_s",
    "i64.le_s",
    "i64.lt_s",
    "if",
    "local.get",
    "local.set",
    "ref.cast",
    "ref.is_null",
    "ref.null",
    "ref.test",
    "return",
    "return_call",
    "struct.get",
    "struct.new",
];

/// The opcodes never surfaced by a certkit user-function body; the decoder's
/// arm for each is exercised by a synthetic single-body probe instead.
const SYNTH_OPCODES: &[&str] = &[
    "i32.and",
    "i32.eq",
    "i64.eqz",
    "return",
    "ref.null",
    "array.new_fixed",
];

/// Raw bytes of the synthetic probe body and the term it must decode to. Covers
/// exactly the six opcodes above (i32.and, i32.eq, i64.eqz, return, ref.null +
/// heaptype, array.new_fixed) followed by the function `end`.
const SYNTH_BYTES: &[u8] = &[
    0x71, 0x46, 0x50, 0x0f, 0xd0, 0x70, 0xfb, 0x08, 0x02, 0x03, 0x0b,
];
const SYNTH_TERM: &str = "[.i32And, .i32Eq, .i64Eqz, .ret, .refNull, .arrayNewFixed 2 3]";

/// Differential-only views stay in the test witness instead of enlarging the
/// checker-owned decoder API. They still reduce in Lean's kernel against the
/// production primitives, so an oracle mismatch remains an `rfl` failure.
const TEST_HELPERS: &str = r#"
namespace CertDecodeTest

def walkIds : Nat → Nat → Nat → Option (List Nat)
  | 0, _, _ => none
  | fuel + 1, n, len =>
      if len == 0 then some [] else
        let id := n &&& 0xff
        let n1 := n >>> 8
        match CertDecode.readU n1 (len - 1) with
        | none => none
        | some (size, n2, len2) =>
            match walkIds fuel (n2 >>> (8 * size)) (len2 - size) with
            | none => none
            | some tail => some (id :: tail)

def decodeBodyBytes (nfields : List Nat) (segs : List (List Nat))
    (fuel n len : Nat) : Option (List WInstr) :=
  match CertDecode.decBlock nfields segs fuel [] n len with
  | none => none
  | some (instrs, _, _, _, term) => if term == 0 then some instrs else none

def scanDefined (n len defIdx : Nat) : Option (List CertDecode.StringHost.Op) :=
  match CertDecode.StringHost.bodyLocs n len with
  | none => none
  | some locs =>
      match locs[defIdx]? with
      | none => none
      | some (_, bodyN, bodyLen) =>
          CertDecode.StringHost.scan (bodyLen + 1) bodyN bodyLen

end CertDecodeTest
"#;

// ---- environment ---------------------------------------------------------

fn lake_available() -> bool {
    Command::new("lake").arg("--version").output().is_ok()
}

fn python_available() -> bool {
    Command::new("python3").arg("--version").output().is_ok()
}

fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command.env(
        "AVER_CERT_DATA_CACHE",
        std::env::temp_dir().join("aver-cert-data-store"),
    );
    command
}

/// Copy the decoder prelude sources into a fresh temp dir and `lake build` them
/// (the witnesses import the resulting `.olean`s).
fn build_prelude() -> ScratchDir {
    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let src = repo.join("aver-cert/assets/wall/current");
    let dst = temp_dir("cdec-prelude");
    std::fs::create_dir_all(&dst).unwrap();
    for f in [
        "CertPrelude.lean",
        "CertDecode.lean",
        "CertPreludeSanity.lean",
        "lean-toolchain",
    ] {
        std::fs::copy(src.join(f), dst.join(f)).unwrap();
    }
    // The wall lakefile also lists the acceptance-side roots; this package only
    // carries the decoder prelude, so it gets its own minimal lakefile.
    std::fs::write(
        dst.join("lakefile.lean"),
        "import Lake\nopen Lake DSL\n\npackage «certprelude» where\n  version := v!\"0.1.0\"\n\n@[default_target]\nlean_lib «CertPrelude» where\n  srcDir := \".\"\n  roots := #[`CertPrelude, `CertPreludeSanity, `CertDecode]\n",
    )
    .unwrap();
    let o = Command::new("lake")
        .arg("build")
        .current_dir(&dst)
        .output()
        .expect("lake build runs");
    assert!(
        o.status.success(),
        "lake build of the decoder prelude failed:\n{}{}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    dst
}

fn compile_wasm_at(repo: &Path, av_path: &Path, name: &str, out: &Path) -> Vec<u8> {
    let c = aver_command()
        .current_dir(repo)
        .arg("compile")
        .arg(av_path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(out)
        .output()
        .expect("aver compile runs");
    assert!(
        c.status.success(),
        "compile {name} failed:\n{}",
        String::from_utf8_lossy(&c.stderr)
    );
    std::fs::read(out.join(format!("{name}.wasm"))).unwrap()
}

/// Emit the certificate for a fixture and return its model `.lean` files, which
/// `rederive_obligations` reads to recover the recursion combinator operator.
fn model_lean_files(repo: &Path, av_path: &Path, out: &Path) -> Vec<(String, String)> {
    let c = aver_command()
        .current_dir(repo)
        .arg("compile")
        .arg(av_path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(out)
        .output()
        .expect("aver compile --certify runs");
    if !c.status.success() {
        return Vec::new();
    }
    let mut files = Vec::new();
    if let Ok(entries) = std::fs::read_dir(out.join("cert")) {
        for e in entries.flatten() {
            let p = e.path();
            if p.extension().and_then(|x| x.to_str()) == Some("lean") {
                let name = p.file_name().unwrap().to_string_lossy().to_string();
                if let Ok(content) = std::fs::read_to_string(&p) {
                    files.push((name, content));
                }
            }
        }
    }
    // The rendered certificate omits the model project's build files. Restore
    // the entry-root metadata that the certificate engine originally received.
    if let Ok(source) = std::fs::read_to_string(av_path)
        && let Some(module_name) = source.lines().find_map(|line| {
            line.trim()
                .strip_prefix("module ")
                .and_then(|rest| rest.split_whitespace().next())
        })
    {
        files.push((
            "lakefile.lean".to_string(),
            format!("roots := #[`{module_name}]\n"),
        ));
    }
    files
}

/// An effectful module the test writes itself: `Console.print` lowers to a real
/// wasm import, so this is the one module in the differential whose import
/// section is non-empty and whose function-index base is non-zero.
const IMPORTS_FIXTURE_NAME: &str = "certimports";
const IMPORTS_FIXTURE_SRC: &str = r#"module CertImports
    intent =
        "Import-section witness for the certificate decoder differential."
    exposes [double]
    effects [Console]

fn double(x: Int) -> Int
    ? "Doubles an integer."
    x + x

verify double
    double(2) => 4
    double(-3) => -6

fn main()
    ! [Console.print]
    Console.print("{double(21)}")
"#;

/// The Python byte-level decoder oracle for a module.
fn oracle_json(repo: &Path, wasm: &Path) -> serde_json::Value {
    let o = Command::new("python3")
        .current_dir(repo)
        .arg("tools/certkit/decode_ref.py")
        .arg("json")
        .arg(wasm)
        .output()
        .expect("decode_ref.py runs");
    assert!(
        o.status.success(),
        "decode_ref.py failed:\n{}",
        String::from_utf8_lossy(&o.stderr)
    );
    serde_json::from_slice(&o.stdout).expect("decode_ref.py emits json")
}

// ---- Lean witness helpers ------------------------------------------------

fn hex_le(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes.iter().rev() {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn lean_str(s: &str) -> String {
    let mut out = String::from("\"");
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn exports_lit(exports: &serde_json::Value) -> String {
    let items: Vec<String> = exports
        .as_array()
        .unwrap()
        .iter()
        .map(|e| {
            let name = e[0].as_str().unwrap();
            let idx = e[1].as_u64().unwrap();
            format!("({}, {})", lean_str(name), idx)
        })
        .collect();
    format!("[{}]", items.join(", "))
}

fn imports_lit(imports: &serde_json::Value) -> String {
    let items: Vec<String> = imports
        .as_array()
        .unwrap()
        .iter()
        .map(|e| {
            format!(
                "({}, {})",
                lean_str(e[0].as_str().unwrap()),
                lean_str(e[1].as_str().unwrap())
            )
        })
        .collect();
    format!("[{}]", items.join(", "))
}

/// Extract the `⟨arity, nlocals, [body]⟩` `WCode` literal from a
/// `rederive_obligations` `code` string (`fun fn => if fn = N then some ⟨…⟩
/// else none`).
fn wcode_from_rederive(code: &str) -> String {
    let start = code.find("then some ").expect("rederive code shape") + "then some ".len();
    let end = code.rfind(" else none").expect("rederive code shape");
    code[start..end].trim().to_string()
}

/// Function indices on which a Rust-rendered sparse `CodeTbl` is populated.
/// The production renderer always emits one `fn = N then some ...` arm per
/// semantic table entry (one for ordinary families, several for mutual and
/// composition). The transition differential checks every arm, not merely the
/// obligation's `self` entry.
fn rust_code_indices(code: &str) -> Vec<u32> {
    let mut indices = code
        .split("fn = ")
        .skip(1)
        .map(|tail| {
            tail.chars()
                .take_while(char::is_ascii_digit)
                .collect::<String>()
                .parse::<u32>()
                .expect("Rust CodeTbl arm has a decimal function index")
        })
        .collect::<Vec<_>>();
    indices.sort_unstable();
    indices.dedup();
    indices
}

fn s1_family(o: &aver::codegen::cert::RederivedObligation) -> Option<&'static str> {
    if o.fragment_plan_lean.is_some() {
        return None;
    }
    if o.string_eq_plan_lean.is_some()
        || o.string_concat_plan_lean.is_some()
        || o.verbatim_plan_lean.is_some()
    {
        Some("verbatim-style")
    } else if o.int_dispatch_plan_lean.is_some() {
        Some("dispatch")
    } else if o.recursion_plan_lean.is_some() {
        Some("recursion")
    } else if o.mutual_plan_lean.is_some() {
        Some("mutual")
    } else if !o.composition_members.is_empty() {
        Some("composition")
    } else if o.field_projection_plan_lean.is_some() {
        Some("field-projection")
    } else if o.construct_plan_lean.is_some() {
        Some("construct")
    } else {
        None
    }
}

/// Run `lake env lean` on a witness source in the prebuilt prelude dir. Returns
/// (clean, combined-output). A divergence surfaces as a kernel error here.
fn run_lean(prelude: &Path, src: &str) -> (bool, String) {
    let file = prelude.join("DecodeWitness.lean");
    std::fs::write(&file, src).unwrap();
    let o = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("DecodeWitness.lean")
        .current_dir(prelude)
        .output()
        .expect("lake env lean runs");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    let _ = std::fs::remove_file(&file);
    let clean = o.status.success() && !combined.contains("error");
    (clean, combined)
}

fn option_nat(value: Option<u32>) -> String {
    value
        .map(|value| format!("some {value}"))
        .unwrap_or_else(|| "none".to_string())
}

/// Independent oracle for the Int-carrier struct index: `wasmparser` walks the
/// type section and reports the FIRST struct with three fields whose first
/// storage type is `i64` and whose third is `i32` — the same shape
/// `CertDecode.TypeEntry.isCarrier` recognises, written against a different
/// parser. `None` means the module carries no such struct at all.
fn wasmparser_carrier_index(bytes: &[u8]) -> Option<u32> {
    use wasmparser::{CompositeInnerType, Parser, Payload, StorageType, ValType};
    let mut type_idx = 0u32;
    for payload in Parser::new(0).parse_all(bytes) {
        let Ok(Payload::TypeSection(reader)) = payload else {
            continue;
        };
        for group in reader {
            for sub in group.expect("type rec group parses").into_types() {
                if let CompositeInnerType::Struct(s) = &sub.composite_type.inner {
                    let fields = &s.fields;
                    if fields.len() == 3
                        && matches!(fields[0].element_type, StorageType::Val(ValType::I64))
                        && matches!(fields[2].element_type, StorageType::Val(ValType::I32))
                    {
                        return Some(type_idx);
                    }
                }
                type_idx += 1;
            }
        }
    }
    None
}

fn string_host_roles_lit(roles: &aver::codegen::cert::StringHostRoles) -> String {
    format!(
        "[{}]",
        roles
            .iter()
            .map(|(index, role)| {
                let role = match role {
                    aver::codegen::cert::StringHostRole::Eq => ".eq",
                    aver::codegen::cert::StringHostRole::Concat => ".concat",
                };
                format!("({index}, {role})")
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

// ---- S3 host-role differential transition ------------------------------

/// Before the production witness relies on the in-kernel table, pin its result
/// to the independent Rust classifier on every certkit fixture plus json.av.
/// Kept permanently so either implementation changing requires an explicit,
/// corpus-wide parity decision.
#[test]
fn s3_kernel_role_table_matches_rust_classifier_on_full_corpus() {
    if !lake_available() {
        eprintln!("skipping S3 role-table differential: `lake` not available");
        return;
    }

    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-s3-role-differential");
    std::fs::create_dir_all(&out).unwrap();
    let mut corpus = S1_FIXTURES
        .iter()
        .map(|name| {
            (
                (*name).to_string(),
                repo.join(format!("tools/certkit/fixtures/{name}.av")),
            )
        })
        .collect::<Vec<_>>();
    corpus.push(("json".to_string(), repo.join("examples/data/json.av")));
    // `hello.av` touches no `Int`, so the compiler emits neither the carrier
    // struct nor the box helper. It is this corpus's only byte-provably
    // CARRIERLESS module and the only one that exercises the `some none` arm of
    // the three-state carrier decode against a real artifact.
    corpus.push(("hello".to_string(), repo.join("examples/core/hello.av")));

    let actual_fixture_names = std::fs::read_dir(repo.join("tools/certkit/fixtures"))
        .unwrap()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let path = entry.path();
            (path.extension().and_then(|ext| ext.to_str()) == Some("av"))
                .then(|| path.file_stem().unwrap().to_string_lossy().to_string())
        })
        .collect::<std::collections::BTreeSet<_>>();
    let declared_fixture_names = S1_FIXTURES
        .iter()
        .map(|name| (*name).to_string())
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(
        declared_fixture_names, actual_fixture_names,
        "S3 differential corpus must contain every certkit .av fixture"
    );

    let mut checked = 0usize;
    let mut carrierless_seen = 0usize;
    for (name, av_path) in corpus {
        let bytes = compile_wasm_at(&repo, &av_path, &name, &out);
        let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx, cmp_idx, eq_idx) =
            aver::codegen::cert::byte_derived_frag_host_role_indices(&bytes)
                .unwrap_or_else(|error| panic!("{name}: Rust role classifier failed: {error}"));
        // The production acceptance pin binds exactly the four name-derived
        // roles plus the carrierless proof; `add`/`mul`/`sub` are no longer
        // discovered from bytes at all (they are declared and confirmed
        // against a synthesized helper body), so only these decoders are on
        // the trusted path and worth a differential. `cmp` and `eq` earn their
        // place twice over: they are the only pair that share a declared
        // function type, so the export-name decode is the ONLY thing telling
        // the two roles apart.
        let helper_absent = if box_idx.is_some() { "false" } else { "true" };
        // The carrier STATE, from an independent oracle: `wasmparser` reads the
        // type section and reports the first struct shaped `{i64, _, i32}`. The
        // kernel's `carrierState` must agree on both arms, and its collapsed
        // reading `decodeCarrier` must agree on the present arm alone — the
        // difference between the two is exactly what makes a carrierless module
        // certifiable rather than merely unpinnable.
        let carrier = wasmparser_carrier_index(&bytes);
        let carrier_state = match carrier {
            Some(idx) => format!("some (some {idx})"),
            None => "some none".to_string(),
        };
        let src = format!(
            "import CertDecode\nopen CertPrelude\nset_option maxRecDepth 200000\n\n\
             def bytesN : Nat := 0x{}\n\
             def bytesLen : Nat := {}\n\n\
             example : CertDecode.AddSub.boxIdx bytesN bytesLen = {} := rfl\n\
             example : CertDecode.AddSub.toIndexIdx bytesN bytesLen = {} := rfl\n\
             example : CertDecode.AddSub.cmpIdx bytesN bytesLen = {} := rfl\n\
             example : CertDecode.AddSub.eqIdx bytesN bytesLen = {} := rfl\n\
             example : CertDecode.AddSub.carrierHelperAbsent bytesN bytesLen = {} := rfl\n\
             example : CertDecode.decodeCarrier bytesN bytesLen = {} := rfl\n\
             example : CertDecode.carrierState bytesN bytesLen = {} := rfl\n",
            hex_le(&bytes),
            bytes.len(),
            option_nat(box_idx),
            option_nat(to_index_idx),
            option_nat(cmp_idx),
            option_nat(eq_idx),
            helper_absent,
            option_nat(carrier),
            carrier_state,
        );
        let (ok, report) = run_lean(&prelude, &src);
        assert!(
            ok,
            "S3 Rust/kernel role-table differential DIVERGED on `{name}`:\n{report}"
        );
        if carrier.is_none() {
            carrierless_seen += 1;
        }
        checked += 1;
    }
    assert_eq!(checked, S1_FIXTURES.len() + 2);
    assert!(
        carrierless_seen > 0,
        "the S3 corpus must keep at least one byte-provably carrierless module, \
         or the `some none` arm of the carrier state goes unexercised"
    );
    eprintln!(
        "S3 role-table differential PASS: {checked} modules \
         (all certkit fixtures + json.av + hello.av), {carrierless_seen} carrierless"
    );
}

/// F5 transition differential: one decode-once equality per module pins the
/// entire ordered string-role list to the independent Rust classifier on every
/// certkit fixture plus json.av (the explicit stringeq fixture is in that set).
#[test]
fn f5_kernel_string_roles_match_rust_classifier_on_full_corpus() {
    if !lake_available() {
        eprintln!("skipping F5 string-role differential: `lake` not available");
        return;
    }

    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-f5-role-differential");
    std::fs::create_dir_all(&out).unwrap();
    let mut corpus = S1_FIXTURES
        .iter()
        .map(|name| {
            (
                (*name).to_string(),
                repo.join(format!("tools/certkit/fixtures/{name}.av")),
            )
        })
        .collect::<Vec<_>>();
    corpus.push(("json".to_string(), repo.join("examples/data/json.av")));

    let actual_fixture_names = std::fs::read_dir(repo.join("tools/certkit/fixtures"))
        .unwrap()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let path = entry.path();
            (path.extension().and_then(|ext| ext.to_str()) == Some("av"))
                .then(|| path.file_stem().unwrap().to_string_lossy().to_string())
        })
        .collect::<std::collections::BTreeSet<_>>();
    let declared_fixture_names = S1_FIXTURES
        .iter()
        .map(|name| (*name).to_string())
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(declared_fixture_names, actual_fixture_names);
    assert!(declared_fixture_names.contains("stringeq"));

    let mut checked = 0usize;
    for (name, av_path) in corpus {
        let bytes = compile_wasm_at(&repo, &av_path, &name, &out);
        let roles = aver::codegen::cert::byte_derived_string_host_roles(&bytes)
            .unwrap_or_else(|error| panic!("{name}: Rust F5 classifier failed: {error}"));
        let src = format!(
            "import CertDecode\nopen CertPrelude\nset_option maxRecDepth 200000\n\n\
             def bytesN : Nat := 0x{}\n\
             def bytesLen : Nat := {}\n\n\
             example : CertDecode.StringHost.roleTable bytesN bytesLen = some {} := rfl\n",
            hex_le(&bytes),
            bytes.len(),
            string_host_roles_lit(&roles),
        );
        let (ok, report) = run_lean(&prelude, &src);
        assert!(
            ok,
            "F5 Rust/kernel string-role differential DIVERGED on `{name}`:\n{report}"
        );
        checked += 1;
    }
    assert_eq!(checked, S1_FIXTURES.len() + 1);
    eprintln!(
        "F5 string-role differential PASS: {checked} modules (all certkit fixtures, including stringeq.av, + json.av)"
    );
}

fn first_i64_arith_offsets(bytes: &[u8], targets: &[u32]) -> Vec<(u32, usize, u8)> {
    let mut imported_funcs = 0u32;
    let mut defined = 0u32;
    let mut found = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("compiler-produced wasm must parse") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group must parse") {
                        let (_, import) = import.expect("import must parse");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported_funcs += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let func_idx = imported_funcs + defined;
                if targets.contains(&func_idx) {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let offset = operators.original_position();
                        let opcode = match operators.read().expect("operator must parse") {
                            wasmparser::Operator::I64Add => Some(0x7c),
                            wasmparser::Operator::I64Sub => Some(0x7d),
                            wasmparser::Operator::I64Mul => Some(0x7e),
                            _ => None,
                        };
                        if let Some(opcode) = opcode {
                            found.push((func_idx, offset, opcode));
                            break;
                        }
                    }
                }
                defined += 1;
            }
            _ => {}
        }
    }
    found
}

/// Spike controls ported to the audited kernel path: changing add's first
/// arithmetic byte to sub removes add and makes sub ambiguous; changing sub's
/// byte to add creates two add candidates. Both classifiers must decline.
fn function_opcode_offsets(bytes: &[u8], target: u32) -> (u32, Vec<(usize, u8)>) {
    let mut imported = 0u32;
    let mut defined = 0u32;
    let mut found = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("compiler-produced wasm must parse") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group must parse") {
                        let (_, import) = import.expect("import must parse");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                if imported + defined == target {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let offset = operators.original_position();
                        let operator = operators.read().expect("operator must parse");
                        let opcode = match operator {
                            wasmparser::Operator::I32Ne => Some(0x47),
                            _ => None,
                        };
                        if let Some(opcode) = opcode {
                            found.push((offset, opcode));
                        }
                    }
                }
                defined += 1;
            }
            _ => {}
        }
    }
    (imported, found)
}

/// F5 negative control: a valid i32.ne→i32.eq mutation in the loop body makes
/// the exact template comparison decline, in both Rust and the kernel scan.
#[test]
fn f5_mutated_string_eq_loop_opcode_changes_kernel_classification() {
    if !lake_available() {
        eprintln!("skipping F5 mutation control: `lake` not available");
        return;
    }
    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-f5-mutation");
    std::fs::create_dir_all(&out).unwrap();
    // The certificate fixture deliberately stays on the one-comparison shape
    // covered by the String.eq wall. Ordinary runtime compilation can scalarise
    // a match whose literal arms are all one-byte ASCII, so this mutation test
    // owns a multi-byte control arm that keeps the host helper reachable.
    let runtime_fixture = out.join("runtime_stringeq.av");
    std::fs::write(
        &runtime_fixture,
        r#"module RuntimeStringEq
    intent =
        "keep the ordinary wasm String.eq loop reachable for byte mutation"
    exposes [quoteOrSelf, bump]

fn quoteOrSelf(c: String) -> String
    match c
        "\"" -> "\\\""
        "verbatim" -> "verbatim"
        _ -> c

fn bump(n: Int) -> Int
    n + 1
"#,
    )
    .unwrap();
    let bytes = compile_wasm_at(&repo, &runtime_fixture, "runtime_stringeq", &out);
    let roles = aver::codegen::cert::byte_derived_string_host_roles(&bytes).unwrap();
    assert_eq!(roles, vec![(3, aver::codegen::cert::StringHostRole::Eq)]);
    let eq_idx = roles[0].0;
    let (imported, offsets) = function_opcode_offsets(&bytes, eq_idx);
    let mutation_offset = offsets
        .get(1)
        .expect("String.eq template has a second i32.ne in its loop")
        .0;
    let mut mutated = bytes.clone();
    assert_eq!(mutated[mutation_offset], 0x47);
    mutated[mutation_offset] = 0x46;
    wasmparser::Validator::new()
        .validate_all(&mutated)
        .expect("i32.eq mutation remains valid wasm");
    assert!(
        aver::codegen::cert::byte_derived_string_host_roles(&mutated)
            .unwrap()
            .is_empty(),
        "Rust F5 classifier must decline the mutated exact template"
    );

    let def_idx = eq_idx - imported;
    let src = format!(
        "import CertDecode\nopen CertPrelude\nset_option maxRecDepth 200000\n\n{TEST_HELPERS}\n\
         def original : Nat := 0x{}\n\
         def mutated : Nat := 0x{}\n\
         def bytesLen : Nat := {}\n\n\
         example : CertDecode.StringHost.roleTable original bytesLen = some [(3, .eq)] := rfl\n\
         example : CertDecode.StringHost.roleTable mutated bytesLen = some [] := rfl\n\
         example : (CertDecodeTest.scanDefined original bytesLen {def_idx}).bind (fun ops => ops[26]?) = some .i32Ne := rfl\n\
         example : (CertDecodeTest.scanDefined mutated bytesLen {def_idx}).bind (fun ops => ops[26]?) = some .other := rfl\n",
        hex_le(&bytes),
        hex_le(&mutated),
        bytes.len(),
    );
    let (ok, report) = run_lean(&prelude, &src);
    assert!(ok, "F5 kernel mutation control failed:\n{report}");
}

const TWO_EQ_WAT: &str = r#"
(module
  (type $string (array (mut i8)))
  (type $eqsig (func (param (ref null $string)) (param (ref null $string)) (result i32)))
  (func $eq0 (type $eqsig) (local i32 i32)
    local.get 0 array.len local.get 1 array.len i32.ne
    if i32.const 0 return end
    local.get 0 array.len local.set 2 i32.const 0 local.set 3
    block loop
      local.get 3 local.get 2 i32.ge_u br_if 1
      local.get 0 local.get 3 array.get_u $string
      local.get 1 local.get 3 array.get_u $string i32.ne
      if i32.const 0 return end
      local.get 3 i32.const 1 i32.add local.set 3 br 0
    end end
    i32.const 1)
  (func $eq1 (type $eqsig) (local i32 i32)
    local.get 0 array.len local.get 1 array.len i32.ne
    if i32.const 0 return end
    local.get 0 array.len local.set 2 i32.const 0 local.set 3
    block loop
      local.get 3 local.get 2 i32.ge_u br_if 1
      local.get 0 local.get 3 array.get_u $string
      local.get 1 local.get 3 array.get_u $string i32.ne
      if i32.const 0 return end
      local.get 3 i32.const 1 i32.add local.set 3 br 0
    end end
    i32.const 1)
  (export "eq0" (func $eq0)))
"#;

/// Positive no-uniqueness control: two exact String.eq helpers are both kept.
#[test]
fn f5_two_eq_helpers_are_both_classified_without_uniqueness_decline() {
    if !lake_available() {
        eprintln!("skipping F5 two-eq control: `lake` not available");
        return;
    }
    let bytes = wat::parse_str(TWO_EQ_WAT).expect("two-eq GC WAT must compile");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("two-eq module must validate");
    let expected = vec![
        (0, aver::codegen::cert::StringHostRole::Eq),
        (1, aver::codegen::cert::StringHostRole::Eq),
    ];
    assert_eq!(
        aver::codegen::cert::byte_derived_string_host_roles(&bytes).unwrap(),
        expected,
        "Rust F5 classifier must retain both independent matches"
    );

    let prelude = build_prelude();
    let src = format!(
        "import CertDecode\nopen CertPrelude\nset_option maxRecDepth 200000\n\n\
         def bytesN : Nat := 0x{}\n\
         def bytesLen : Nat := {}\n\n\
         example : CertDecode.StringHost.roleTable bytesN bytesLen = some [(0, .eq), (1, .eq)] := rfl\n",
        hex_le(&bytes),
        bytes.len(),
    );
    let (ok, report) = run_lean(&prelude, &src);
    assert!(ok, "F5 kernel two-eq control failed:\n{report}");
}

// ---- S1 differential transition -----------------------------------------

#[test]
fn s1_rust_splices_equal_kernel_decodes_on_full_corpus() {
    if !lake_available() {
        eprintln!("skipping S1 differential: `lake` not available");
        return;
    }

    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-s1-differential");
    std::fs::create_dir_all(&out).unwrap();
    let mut corpus = S1_FIXTURES
        .iter()
        .map(|name| {
            (
                (*name).to_string(),
                repo.join(format!("tools/certkit/fixtures/{name}.av")),
            )
        })
        .collect::<Vec<_>>();
    corpus.push(("json".to_string(), repo.join("examples/data/json.av")));

    let actual_fixture_names = std::fs::read_dir(repo.join("tools/certkit/fixtures"))
        .unwrap()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let path = entry.path();
            (path.extension().and_then(|ext| ext.to_str()) == Some("av"))
                .then(|| path.file_stem().unwrap().to_string_lossy().to_string())
        })
        .collect::<std::collections::BTreeSet<_>>();
    let declared_fixture_names = S1_FIXTURES
        .iter()
        .map(|name| (*name).to_string())
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(
        declared_fixture_names, actual_fixture_names,
        "S1 differential corpus must contain every certkit .av fixture"
    );

    let mut covered_families = std::collections::BTreeSet::new();
    let mut checked_obligations = 0usize;
    let mut checked_code_arms = 0usize;
    let mut checked_struct_facts = 0usize;

    for (name, av_path) in corpus {
        let bytes = compile_wasm_at(&repo, &av_path, &name, &out);
        let models = model_lean_files(&repo, &av_path, &out);
        let obligations = aver::codegen::cert::rederive_obligations(&bytes, &models)
            .unwrap_or_else(|error| panic!("{name}: Rust rederive failed: {error}"));

        let mut src = String::new();
        src.push_str("import CertDecode\nopen CertPrelude\nset_option maxRecDepth 200000\n\n");
        src.push_str(&format!("def bytesN : Nat := 0x{}\n", hex_le(&bytes)));
        src.push_str(&format!("def bytesLen : Nat := {}\n\n", bytes.len()));

        let mut module_obligations = 0usize;
        for obligation in obligations.iter().filter(|o| s1_family(o).is_some()) {
            let family = s1_family(obligation).unwrap();
            covered_families.insert(family);
            checked_obligations += 1;
            module_obligations += 1;

            src.push_str(&format!(
                "-- {family}: {export}\nexample : CertDecode.decodeCarrier bytesN bytesLen = some {carrier} := rfl\n",
                export = obligation.name,
                carrier = obligation.carrier,
            ));

            // F6: the export section maps this obligation's name to the Rust
            // `self_idx`. The production witness pins `self` through the
            // whole-module `exportsAccounted` conjunct (`WasmSlice.enumExports`);
            // this corpus-wide differential confirms the byte-derived export
            // table agrees with the Rust self index, so removing the Rust
            // `self` splice loses no constraint.
            src.push_str(&format!(
                "example : (match CertDecode.decodeExports bytesN bytesLen with | some es => (es.find? (fun e => e.1 == {name})).map Prod.snd | none => none) = some {self_idx} := rfl\n",
                name = lean_str(&obligation.name),
                self_idx = obligation.self_idx,
            ));

            let code_indices = rust_code_indices(&obligation.code);
            assert!(
                code_indices.contains(&obligation.self_idx),
                "{name}/{}: Rust CodeTbl does not contain its self index {}",
                obligation.name,
                obligation.self_idx
            );
            for index in code_indices {
                checked_code_arms += 1;
                src.push_str(&format!(
                    "example : CertDecode.decodeCode bytesN bytesLen {index} = ({code}) {index} := rfl\n",
                    code = obligation.code,
                ));
            }

            if let (Some(struct_idx), Some(field_count)) = (
                obligation.construct_struct_idx,
                obligation.construct_field_count,
            ) {
                checked_struct_facts += 1;
                src.push_str(&format!(
                    "example : CertDecode.decodeStructFieldCount bytesN bytesLen {struct_idx} = some {field_count} := rfl\n"
                ));
            }
            if let (Some(struct_idx), Some(field_count)) = (
                obligation.field_projection_struct_idx,
                obligation.field_projection_field_count,
            ) {
                checked_struct_facts += 1;
                src.push_str(&format!(
                    "example : CertDecode.decodeStructFieldCount bytesN bytesLen {struct_idx} = some {field_count} := rfl\n"
                ));
            }
        }

        let (ok, report) = run_lean(&prelude, &src);
        assert!(
            ok,
            "S1 Rust-splice/kernel-decode differential DIVERGED on `{name}` ({module_obligations} obligations):\n{report}"
        );
    }

    let expected_families = [
        "composition",
        "construct",
        "dispatch",
        "field-projection",
        "mutual",
        "recursion",
        "verbatim-style",
    ]
    .into_iter()
    .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(
        covered_families, expected_families,
        "S1 differential did not exercise every in-scope family"
    );
    assert!(checked_obligations > 0);
    assert!(checked_code_arms >= checked_obligations);
    assert!(checked_struct_facts > 0);
    eprintln!(
        "S1 differential PASS: {checked_obligations} obligations, {checked_code_arms} CodeTbl arms, {checked_struct_facts} struct facts"
    );
}

// ---- main differential + coverage ---------------------------------------

#[test]
fn cert_decode_three_way_differential_and_coverage() {
    if !lake_available() {
        eprintln!("skipping cert decode test: `lake` not available");
        return;
    }
    if !python_available() {
        eprintln!("skipping cert decode test: `python3` not available");
        return;
    }

    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-wasm");
    std::fs::create_dir_all(&out).unwrap();

    let mut covered: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();

    // Repo fixtures plus the test-authored effectful module (real emitter
    // round-trip either way; the extra module is the import-section witness).
    let imports_av = out.join(format!("{IMPORTS_FIXTURE_NAME}.av"));
    std::fs::write(&imports_av, IMPORTS_FIXTURE_SRC).unwrap();
    let mut modules: Vec<(String, PathBuf)> = FIXTURES
        .iter()
        .map(|f| {
            (
                f.to_string(),
                repo.join(format!("tools/certkit/fixtures/{f}.av")),
            )
        })
        .collect();
    modules.push((IMPORTS_FIXTURE_NAME.to_string(), imports_av));

    for (fixture, av_path) in &modules {
        let bytes = compile_wasm_at(&repo, av_path, fixture, &out);
        let wasm_path = out.join(format!("{fixture}.wasm"));
        let oracle = oracle_json(&repo, &wasm_path);

        // The import-section witness must actually witness something: a lowering
        // change that drops the imports would make this coverage vacuous again.
        if fixture.as_str() == IMPORTS_FIXTURE_NAME {
            assert!(
                !oracle["imports"].as_array().unwrap().is_empty(),
                "{fixture}: expected a non-empty import section (effect lowering changed?)"
            );
        }

        // Rust oracle: certified obligations, cross-checked against the Python
        // oracle (carrier + export-name → self), then pinned to the Lean decoder
        // with rederive's exact rendered code strings.
        let models = model_lean_files(&repo, av_path, &out);
        let obligations = aver::codegen::cert::rederive_obligations(&bytes, &models)
            .expect("rederive succeeds on a compiled module");

        let mut src = String::new();
        src.push_str("import CertDecode\nopen CertPrelude\n\n");
        src.push_str(TEST_HELPERS);
        src.push('\n');
        src.push_str(&format!("def bytesN : Nat := 0x{}\n", hex_le(&bytes)));
        src.push_str(&format!("def bytesLen : Nat := {}\n\n", bytes.len()));

        // Section-level bindings (oracle: Python byte decoder). The test-only
        // walker uses production `readU` and traverses the ENTIRE file, so a
        // malformed frame anywhere still fails the differential.
        let sections: Vec<String> = oracle["sections"]
            .as_array()
            .unwrap()
            .iter()
            .map(|s| s.as_u64().unwrap().to_string())
            .collect();
        src.push_str(&format!(
            "example : CertDecodeTest.walkIds 64 (bytesN >>> 64) (bytesLen - 8) = some [{}] := rfl\n",
            sections.join(", ")
        ));
        src.push_str(&format!(
            "example : CertDecode.decodeExports bytesN bytesLen = some {} := rfl\n",
            exports_lit(&oracle["exports"])
        ));
        src.push_str(&format!(
            "example : CertDecode.decodeImports bytesN bytesLen = some {} := rfl\n",
            imports_lit(&oracle["imports"])
        ));
        // The Python oracle reports the carrier three-state directly: a struct
        // index, or `null` when the type section decodes and holds none. Pin
        // BOTH kernel readings of it — `decodeCarrier`, which collapses "absent"
        // onto "unreadable", and `carrierState`, which keeps them apart so a
        // carrierless module has a state to declare.
        match oracle["carrier"].as_u64() {
            Some(c) => {
                src.push_str(&format!(
                    "example : CertDecode.decodeCarrier bytesN bytesLen = some {c} := rfl\n\
                     example : CertDecode.carrierState bytesN bytesLen = some (some {c}) := rfl\n"
                ));
            }
            None => {
                src.push_str(
                    "example : CertDecode.decodeCarrier bytesN bytesLen = none := rfl\n\
                     example : CertDecode.carrierState bytesN bytesLen = some none := rfl\n",
                );
            }
        }

        // Per-function code bindings for EVERY user function (oracle: Python).
        for f in oracle["funcs"].as_array().unwrap() {
            let idx = f["idx"].as_u64().unwrap();
            let arity = f["arity"].as_u64().unwrap();
            let nlocals = f["nlocals"].as_u64().unwrap();
            let body = f["body"].as_str().unwrap();
            src.push_str(&format!(
                "example : CertDecode.decodeCode bytesN bytesLen {idx} = some ⟨{arity}, {nlocals}, {body}⟩ := rfl\n"
            ));
            // opcode census for the coverage matrix.
        }

        // Certified obligations (oracle: Rust rederive) — the SAME rendered code
        // strings the production witness pins, bound to the Lean decoder.
        for o in &obligations {
            // cross-check Rust vs Python: the carrier and the export → self map.
            if let Some(c) = oracle["carrier"].as_u64() {
                assert_eq!(
                    o.carrier as u64, c,
                    "{fixture}: rederive carrier {} != oracle carrier {c}",
                    o.carrier
                );
            }
            let mapped = oracle["exports"].as_array().unwrap().iter().any(|e| {
                e[0].as_str() == Some(o.name.as_str()) && e[1].as_u64() == Some(o.self_idx as u64)
            });
            assert!(
                mapped,
                "{fixture}: rederive export {}→{} not in the decoded export map",
                o.name, o.self_idx
            );
            src.push_str(&format!(
                "example : CertDecode.decodeCode bytesN bytesLen {} = some {} := rfl\n",
                o.self_idx,
                wcode_from_rederive(&o.code)
            ));
        }

        let (ok, report) = run_lean(&prelude, &src);
        assert!(
            ok,
            "decoder differential DIVERGED on `{fixture}` (kernel rejected a binding):\n{report}"
        );

        for op in oracle["opcodes"].as_array().unwrap() {
            covered.insert(op.as_str().unwrap().to_string());
        }
    }

    // Synthetic single-body probe for the six opcodes no fixture body surfaces.
    {
        let mut src = String::new();
        src.push_str("import CertDecode\nopen CertPrelude\n\n");
        src.push_str(TEST_HELPERS);
        src.push('\n');
        src.push_str(&format!("def synthN : Nat := 0x{}\n", hex_le(SYNTH_BYTES)));
        src.push_str(&format!("def synthLen : Nat := {}\n\n", SYNTH_BYTES.len()));
        src.push_str(&format!(
            "example : CertDecodeTest.decodeBodyBytes [] [] {} synthN synthLen = some {} := rfl\n",
            SYNTH_BYTES.len(),
            SYNTH_TERM
        ));
        let (ok, report) = run_lean(&prelude, &src);
        assert!(ok, "synthetic opcode probe failed to decode:\n{report}");
        for op in SYNTH_OPCODES {
            covered.insert((*op).to_string());
        }
    }

    // Axiom footprint: the verdict path must stay on `[propext]` — no `sorryAx`,
    // no `native_decide` / `ofReduceBool` escape hatch (brief constraint). Pin
    // it as a regression on the richest fixture (certprobe2's nested body).
    {
        let bytes = std::fs::read(out.join("certprobe2.wasm")).unwrap();
        let oracle = oracle_json(&repo, &out.join("certprobe2.wasm"));
        let sumto = &oracle["funcs"].as_array().unwrap()[1];
        let mut src = String::new();
        src.push_str("import CertDecode\nopen CertPrelude\n\n");
        src.push_str(&format!("def bytesN : Nat := 0x{}\n", hex_le(&bytes)));
        src.push_str(&format!("def bytesLen : Nat := {}\n\n", bytes.len()));
        src.push_str(&format!(
            "theorem tCode : CertDecode.decodeCode bytesN bytesLen {} = some ⟨{}, {}, {}⟩ := rfl\n",
            sumto["idx"].as_u64().unwrap(),
            sumto["arity"].as_u64().unwrap(),
            sumto["nlocals"].as_u64().unwrap(),
            sumto["body"].as_str().unwrap()
        ));
        src.push_str("#print axioms tCode\n");
        let (ok, report) = run_lean(&prelude, &src);
        assert!(ok, "axiom-footprint witness failed to build:\n{report}");
        assert!(
            report.contains("propext"),
            "axiom line missing propext:\n{report}"
        );
        for forbidden in [
            "sorryAx",
            "ofReduceBool",
            "native",
            "Classical.choice",
            "Quot.sound",
        ] {
            assert!(
                !report.contains(forbidden),
                "verdict path carries a non-[propext] axiom `{forbidden}`:\n{report}"
            );
        }
    }

    // Coverage matrix, fail-closed: every one of the 39 must be exercised.
    let missing: Vec<&str> = ALL_OPCODES
        .iter()
        .copied()
        .filter(|op| !covered.contains(*op))
        .collect();
    assert!(
        missing.is_empty(),
        "opcode coverage FAIL — {} of 39 exercised, missing: {:?}",
        covered
            .iter()
            .filter(|c| ALL_OPCODES.contains(&c.as_str()))
            .count(),
        missing
    );
}

// ---- mutation suite M1–M7 ------------------------------------------------

/// Minimal section walk: returns (id, body_start, size) for each section.
fn walk_sections(b: &[u8]) -> Vec<(u8, usize, usize)> {
    let mut i = 8usize;
    let mut secs = Vec::new();
    while i < b.len() {
        let id = b[i];
        i += 1;
        let (size, ni) = read_uleb(b, i);
        secs.push((id, ni, size));
        i = ni + size;
    }
    secs
}

fn read_uleb(b: &[u8], mut i: usize) -> (usize, usize) {
    let mut r = 0usize;
    let mut s = 0u32;
    loop {
        let x = b[i];
        i += 1;
        r |= ((x & 0x7f) as usize) << s;
        if x & 0x80 == 0 {
            break;
        }
        s += 7;
    }
    (r, i)
}

fn encode_uleb(mut value: usize) -> Vec<u8> {
    let mut out = Vec::new();
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        out.push(byte);
        if value == 0 {
            return out;
        }
    }
}

/// Offset of the funcidx byte of the export named `name` (single-byte index).
fn export_funcidx_offset(b: &[u8], name: &str) -> usize {
    let secs = walk_sections(b);
    let (_, start, _) = *secs.iter().find(|s| s.0 == 7).unwrap();
    let (cnt, mut i) = read_uleb(b, start);
    for _ in 0..cnt {
        let (nlen, ni) = read_uleb(b, i);
        let this = std::str::from_utf8(&b[ni..ni + nlen]).unwrap_or("");
        i = ni + nlen;
        let _kind = b[i];
        i += 1;
        let idx_off = i;
        let (_idx, ni2) = read_uleb(b, i);
        i = ni2;
        if this == name {
            return idx_off;
        }
    }
    panic!("export {name} not found");
}

#[test]
fn cert_decode_mutations_fail_closed() {
    if !lake_available() {
        eprintln!("skipping cert decode mutation test: `lake` not available");
        return;
    }

    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let prelude = build_prelude();
    let out = temp_dir("cdec-mut-wasm");
    std::fs::create_dir_all(&out).unwrap();

    let bytes = compile_wasm_at(
        &repo,
        &repo.join("tools/certkit/fixtures/certprobe2.av"),
        "certprobe2",
        &out,
    );
    let models = model_lean_files(
        &repo,
        &repo.join("tools/certkit/fixtures/certprobe2.av"),
        &out,
    );
    let obligations = aver::codegen::cert::rederive_obligations(&bytes, &models).unwrap();
    let sumto = obligations.iter().find(|o| o.name == "sumTo").unwrap();
    let self_idx = sumto.self_idx;
    let orig_wcode = wcode_from_rederive(&sumto.code);

    // helper: build a witness over an explicit byte array + a single example.
    let witness = |b: &[u8], example: &str| -> String {
        format!(
            "import CertDecode\nopen CertPrelude\n\n{}\ndef bytesN : Nat := 0x{}\ndef bytesLen : Nat := {}\n\n{example}\n",
            TEST_HELPERS,
            hex_le(b),
            b.len()
        )
    };

    // M1: flip a byte inside the certified body → the original decode no longer
    // holds. Locate sumTo's body via the code section and flip its first opcode.
    {
        let secs = walk_sections(&bytes);
        let (_, cstart, _) = *secs.iter().find(|s| s.0 == 10).unwrap();
        let (_nf, mut i) = read_uleb(&bytes, cstart);
        // skip entry 0 (defined idx 0), reach entry 1 (sumTo, defined idx 1).
        let (sz0, ni) = read_uleb(&bytes, i);
        i = ni + sz0;
        let (_sz1, ni2) = read_uleb(&bytes, i);
        i = ni2; // start of sumTo entry body: nloc groups ...
        let (ng, ni3) = read_uleb(&bytes, i);
        assert_eq!(ng, 1, "sumTo locals shape changed");
        // one local group: count + ref valtype (0x63 + heaptype sleb)
        let (_c, ni4) = read_uleb(&bytes, ni3);
        let mut j = ni4;
        assert!(bytes[j] == 0x63 || bytes[j] == 0x64);
        j += 1;
        let (_ht, jb) = read_uleb(&bytes, j); // heaptype (small, uleb == sleb)
        let body0 = jb; // first opcode byte of the body (local.get)
        assert_eq!(
            bytes[body0], 0x20,
            "sumTo body does not start with local.get"
        );
        let example = format!(
            "example : CertDecode.decodeCode bytesN bytesLen {self_idx} = some {orig_wcode} := rfl"
        );
        // positive control: the binding holds on the un-mutated bytes …
        let (ok0, r0) = run_lean(&prelude, &witness(&bytes, &example));
        assert!(
            ok0,
            "M1 positive control: original binding must hold:\n{r0}"
        );
        let mut m = bytes.clone();
        m[body0 + 1] ^= 0x04; // flip the local index operand
        // … and fails only because of the body-byte flip.
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            !ok,
            "M1: a body-byte flip must break the original decode:\n{report}"
        );

        // M1b: an early top-level `end` cannot hide residual bytes inside the
        // same declared code entry.
        let mut m = bytes.clone();
        m[body0] = 0x0b;
        let example =
            format!("example : CertDecode.decodeCode bytesN bytesLen {self_idx} = none := rfl");
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            ok,
            "M1b: trailing bytes after a top-level end must fail closed:\n{report}"
        );
    }

    // M2: flip a byte in a decoder-skipped section (memory, id 5) → the decoded
    // obligations are unchanged.
    {
        let secs = walk_sections(&bytes);
        let mem = secs.iter().find(|s| s.0 == 5).expect("memory section");
        let off = mem.1 + mem.2 - 1; // last content byte of the memory section
        let mut m = bytes.clone();
        m[off] ^= 0x01;
        assert_ne!(m, bytes);
        let example = format!(
            "example : CertDecode.decodeCode bytesN bytesLen {self_idx} = some {orig_wcode} := rfl\n\
             example : CertDecode.decodeCarrier bytesN bytesLen = some {} := rfl",
            sumto.carrier
        );
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            ok,
            "M2: a flip in a skipped section must leave the decode unchanged:\n{report}"
        );
    }

    // M3: relabel sumTo's export funcidx → the decoded export map changes.
    {
        let off = export_funcidx_offset(&bytes, "sumTo");
        assert_eq!(bytes[off], 1, "sumTo export funcidx shape changed");
        let example =
            "example : (CertDecode.decodeExports bytesN bytesLen).bind (fun m => m.lookup \"sumTo\") = some 1 := rfl"
                .to_string();
        // positive control: sumTo → 1 in the un-mutated export map …
        let (ok0, r0) = run_lean(&prelude, &witness(&bytes, &example));
        assert!(
            ok0,
            "M3 positive control: original export map must hold:\n{r0}"
        );
        let mut m = bytes.clone();
        m[off] = 5; // relabel to another defined function index
        // … and fails only because the funcidx was relabelled.
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            !ok,
            "M3: an export relabel must change the decoded funcidx:\n{report}"
        );
    }

    // M4: overlong LEB in a section size → the walk decodes to none. Rewrite the
    // memory section's 1-byte size as an overlong two-byte encoding.
    {
        let secs = walk_sections(&bytes);
        let mem = secs.iter().find(|s| s.0 == 5).expect("memory section");
        // the size LEB sits just before the body start; find its extent.
        let (_size, after) = read_uleb(&bytes, mem.1 - 1); // 1-byte size assumed
        assert_eq!(after, mem.1, "memory section size is not a single byte");
        let size_byte = bytes[mem.1 - 1];
        assert!(size_byte < 0x80);
        let mut m: Vec<u8> = bytes[..mem.1 - 1].to_vec();
        m.push(size_byte | 0x80); // continuation set …
        m.push(0x00); // … then a trailing zero → overlong (rejected)
        m.extend_from_slice(&bytes[mem.1..]);
        let example = "example : CertDecode.decodeExports bytesN bytesLen = none := rfl\n\
             example : CertDecode.moduleFramingValid bytesN bytesLen = false := rfl"
            .to_string();
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            ok,
            "M4: an overlong size LEB must make the section walk fail closed:\n{report}"
        );
    }

    // M5: truncation inside the code section → the body decodes to none, while
    // the (earlier) export section still decodes.
    {
        let secs = walk_sections(&bytes);
        let (_, cstart, _) = *secs.iter().find(|s| s.0 == 10).unwrap();
        let (_nf, i) = read_uleb(&bytes, cstart);
        let (sz0, ni) = read_uleb(&bytes, i);
        let (_sz1, ni2) = read_uleb(&bytes, ni + sz0);
        let cut = ni2 + 3; // 3 bytes into sumTo's entry, mid-body
        let m = bytes[..cut].to_vec();
        let example = format!(
            "example : CertDecode.decodeCode bytesN bytesLen {self_idx} = none := rfl\n\
             example : (CertDecode.decodeExports bytesN bytesLen).isSome = true := rfl\n\
             example : CertDecode.moduleFramingValid bytesN bytesLen = false := rfl"
        );
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            ok,
            "M5: a truncated code section must decode to none, exports intact:\n{report}"
        );
    }

    // M6: a function index beyond the module's function space decodes to none
    // (an export relabelled past the end never yields a garbage body).
    {
        let example =
            "example : CertDecode.decodeCode bytesN bytesLen 100000 = none := rfl".to_string();
        let (ok, report) = run_lean(&prelude, &witness(&bytes, &example));
        assert!(
            ok,
            "M6: an out-of-function-space index must decode to none:\n{report}"
        );
    }

    // M7 (bounded-section GuardIso): keep every byte but declare the code
    // section one byte shorter. The old suffix cursor could borrow that byte
    // from the following module suffix; the bounded payload/body view must
    // decline the code and the whole-file framing walk.
    {
        let secs = walk_sections(&bytes);
        let (_, cstart, csize) = *secs.iter().find(|s| s.0 == 10).unwrap();
        let size_start = secs
            .iter()
            .find(|s| s.0 == 10)
            .map(|_| {
                let mut i = cstart - 1;
                while bytes[i] & 0x80 != 0 {
                    i -= 1;
                }
                // Walk backwards across continuation bytes to the section-id
                // successor. For this fixture the code size uses two bytes.
                while i > 8 && bytes[i - 1] & 0x80 != 0 {
                    i -= 1;
                }
                i
            })
            .unwrap();
        let (_old, size_end) = read_uleb(&bytes, size_start);
        assert_eq!(size_end, cstart);
        let encoded = encode_uleb(csize - 1);
        assert_eq!(encoded.len(), cstart - size_start);
        let mut m = bytes.clone();
        m[size_start..cstart].copy_from_slice(&encoded);
        let example = "example : CertDecode.codeLocs bytesN bytesLen = none := rfl\n\
             example : CertDecodeTest.walkIds 64 (bytesN >>> 64) (bytesLen - 8) = none := rfl\n\
             example : CertDecode.moduleFramingValid bytesN bytesLen = false := rfl"
            .to_string();
        let (ok, report) = run_lean(&prelude, &witness(&m, &example));
        assert!(
            ok,
            "M7: a declared-size/content mismatch must decline at the bounded section wall:\n{report}"
        );
    }
}
