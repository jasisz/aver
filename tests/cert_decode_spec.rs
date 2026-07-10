//! Integration tests for the in-kernel certificate profile decoder
//! (`tools/certkit/prelude/CertDecode.lean`).
//!
//! The main test is a decoder differential: for every certkit fixture the Lean
//! kernel decoder (`CertDecode.decode…`, checked by `rfl`) agrees, TERM FOR
//! TERM, with the Python byte-level oracle (`tools/certkit/decode_ref.py`) on
//! every user-function body, the section walk, exports, imports and carrier —
//! and, for the CERTIFIED obligations, additionally with the Rust
//! `cert::rederive_obligations` (three oracles on those; two on the rest).
//! It compiles each fixture, emits a Lean witness pinning `walkIds`,
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

use std::path::{Path, PathBuf};
use std::process::Command;

const FIXTURES: &[&str] = &[
    "certprobe",
    "certprobe2",
    "certkit_ops",
    "certkit_zoo",
    "certempty",
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

// ---- environment ---------------------------------------------------------

fn lake_available() -> bool {
    Command::new("lake").arg("--version").output().is_ok()
}

fn python_available() -> bool {
    Command::new("python3").arg("--version").output().is_ok()
}

fn temp_dir(prefix: &str) -> PathBuf {
    let mut d = std::env::temp_dir();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    d.push(format!("aver-{prefix}-{nanos}"));
    d
}

fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command
}

/// Copy the decoder prelude sources into a fresh temp dir and `lake build` them
/// (the witnesses import the resulting `.olean`s).
fn build_prelude() -> PathBuf {
    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let src = repo.join("tools/certkit/prelude");
    let dst = temp_dir("cdec-prelude");
    std::fs::create_dir_all(&dst).unwrap();
    for f in [
        "CertPrelude.lean",
        "CertDecode.lean",
        "CertPreludeSanity.lean",
        "lakefile.lean",
        "lean-toolchain",
    ] {
        std::fs::copy(src.join(f), dst.join(f)).unwrap();
    }
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
        src.push_str(&format!("def bytesN : Nat := 0x{}\n", hex_le(&bytes)));
        src.push_str(&format!("def bytesLen : Nat := {}\n\n", bytes.len()));

        // Section-level bindings (oracle: Python byte decoder). The `walkIds`
        // binding walks the ENTIRE file (one entry per section), so a malformed
        // section frame anywhere in the module fails the differential even in
        // regions the targeted decoders skip.
        let sections: Vec<String> = oracle["sections"]
            .as_array()
            .unwrap()
            .iter()
            .map(|s| s.as_u64().unwrap().to_string())
            .collect();
        src.push_str(&format!(
            "example : CertDecode.walkIds 64 (bytesN >>> 64) (bytesLen - 8) = some [{}] := rfl\n",
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
        if let Some(c) = oracle["carrier"].as_u64() {
            src.push_str(&format!(
                "example : CertDecode.decodeCarrier bytesN bytesLen = some {c} := rfl\n"
            ));
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
        src.push_str(&format!("def synthN : Nat := 0x{}\n", hex_le(SYNTH_BYTES)));
        src.push_str(&format!("def synthLen : Nat := {}\n\n", SYNTH_BYTES.len()));
        src.push_str(&format!(
            "example : CertDecode.decodeBodyBytes [] [] {} synthN synthLen = some {} := rfl\n",
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

    let _ = std::fs::remove_dir_all(&out);
    let _ = std::fs::remove_dir_all(&prelude);
}

// ---- mutation suite M1–M6 ------------------------------------------------

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
            "import CertDecode\nopen CertPrelude\n\ndef bytesN : Nat := 0x{}\ndef bytesLen : Nat := {}\n\n{example}\n",
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
        let example =
            "example : CertDecode.decodeExports bytesN bytesLen = none := rfl".to_string();
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
             example : (CertDecode.decodeExports bytesN bytesLen).isSome = true := rfl"
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

    let _ = std::fs::remove_dir_all(&out);
    let _ = std::fs::remove_dir_all(&prelude);
}
