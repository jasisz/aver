//! Guard-isolation checks for the certificate wall's whole-module and
//! byte-pin conjuncts.
//!
//! Each test hands the checker-owned wall a hostile artifact (hostile bytes,
//! a hostile declaration, or both) inside a real compiled package, and states
//! the verdict as Lean `example`s the kernel decides: the real conjunct
//! rejects the hostile artifact, and — where one conjunct is claimed to be the
//! sole rejector — a literal copy of the live wall definition weakened by
//! exactly that conjunct (cut from the materialized wall source, with the
//! removed text asserted to occur exactly once) accepts it. A moved or renamed
//! conjunct therefore fails the test loudly instead of leaving a stale hand
//! copy passing.
#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::aver_command;

#[path = "support/cert_wall.rs"]
mod cert_wall;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use cert_wall::materialize as materialize_wall;
use scratch_dir::{ScratchDir, temp_dir};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;

fn lake_available() -> bool {
    lean_required::lake_available()
}

/// Compile `fixture` (a path under the repository root) with `--certify`,
/// materialize the wall into its package and build the package's acceptance
/// root (`ArtifactCertificate`, which imports `Artifact`), so a probe can
/// import it. Returns the scratch directory, the package directory and the
/// artifact bytes.
fn built_package(fixture: &str, extra: &[&str], prefix: &str) -> (ScratchDir, PathBuf, Vec<u8>) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg(fixture)
        .args(extra)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "{fixture} compile failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let stem = Path::new(fixture)
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let wasm = std::fs::read(out_dir.join(format!("{stem}.wasm"))).unwrap();
    let cert = out_dir.join("cert");
    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .args(["build", "ArtifactCertificate"])
        .output()
        .expect("lake builds the acceptance root");
    assert!(
        build.status.success(),
        "{fixture} certificate failed to build before its guard probe:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    (out_dir, cert, wasm)
}

/// Stage the checker-owned wall alone (no package) and build it, for probes
/// over synthetic modules. `roots` names the modules a probe imports; the lake
/// library lists their whole import closure within the wall, because a
/// `roots` list is not extended by imports and a module outside it is never
/// built (and the modules that import package data cannot be built here).
fn built_wall(prefix: &str, roots: &[&str]) -> ScratchDir {
    let wall_dir = temp_dir(prefix);
    std::fs::create_dir_all(&wall_dir).unwrap();
    let wall = aver::codegen::cert::wall::resolve(aver::codegen::cert::wall::CURRENT_ID).unwrap();
    for source in wall.sources {
        std::fs::write(wall_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(wall_dir.join("lean-toolchain"), wall.toolchain).unwrap();
    let imports_of = |module: &str| -> Vec<String> {
        let source = wall
            .sources
            .iter()
            .find(|source| source.name.strip_suffix(".lean") == Some(module))
            .unwrap_or_else(|| panic!("the probe needs `{module}`, which the wall does not stage"));
        // Toolchain imports (`Std.*`) are not wall modules and need no root.
        source
            .contents
            .lines()
            .filter_map(|line| line.strip_prefix("import "))
            .map(|name| name.trim().to_string())
            .filter(|name| {
                wall.sources
                    .iter()
                    .any(|source| source.name.strip_suffix(".lean") == Some(name.as_str()))
            })
            .collect()
    };
    let mut closure: BTreeSet<String> = BTreeSet::new();
    let mut pending: Vec<String> = roots.iter().map(|root| root.to_string()).collect();
    while let Some(module) = pending.pop() {
        if closure.insert(module.clone()) {
            pending.extend(imports_of(&module));
        }
    }
    let roots = closure
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(
        wall_dir.join("lakefile.lean"),
        format!(
            "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
             @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n"
        ),
    )
    .unwrap();
    let build = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("build")
        .output()
        .expect("lake builds the staged wall");
    assert!(
        build.status.success(),
        "the staged wall failed to build:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    wall_dir
}

/// Elaborate `lean` as `{file}` inside `dir` with `lake env lean`, and assert
/// every `example` in it holds.
fn assert_probe_holds(dir: &Path, file: &str, lean: &str) {
    std::fs::write(dir.join(file), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(dir)
        .args(["env", "lean", file])
        .output()
        .expect("lake env lean runs the probe");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    assert!(
        check.status.success() && !combined.contains("error"),
        "{file} failed:\n{combined}"
    );
}

/// The declared host-role table of a package, from its public manifest.
fn manifest_roles(cert: &Path) -> serde_json::Value {
    let manifest: serde_json::Value =
        serde_json::from_slice(&std::fs::read(cert.join("cert-manifest.json")).unwrap()).unwrap();
    manifest["hostRoleTable"].clone()
}

fn role(roles: &serde_json::Value, name: &str) -> Option<u32> {
    roles[name].as_u64().map(|index| index as u32)
}

fn lean_option(index: Option<u32>) -> String {
    index.map_or_else(|| "none".to_string(), |index| format!("some {index}"))
}

/// A `CertDecode.AddSub.Roles` literal.
fn roles_lit(roles: &serde_json::Value, overrides: &[(&str, Option<u32>)]) -> String {
    let pick = |name: &str| -> String {
        overrides
            .iter()
            .find(|(field, _)| *field == name)
            .map_or_else(
                || lean_option(role(roles, name)),
                |(_, value)| lean_option(*value),
            )
    };
    format!(
        "({{ box := {}, add := {}, mul := {}, sub := {}, toIndex := {}, cmp := {}, eq := {}, divmod := {} }} : CertDecode.AddSub.Roles)",
        pick("box"),
        pick("add"),
        pick("mul"),
        pick("sub"),
        pick("toIndex"),
        pick("cmp"),
        pick("eq"),
        pick("divmod"),
    )
}

fn assert_manifest_decode_declines(wasm: &Path, cert: &Path, expected: &str) {
    let output = aver_command()
        .arg("cert")
        .arg("verify")
        .arg(wasm)
        .arg(cert)
        .output()
        .expect("run verifier for strict manifest decode");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !output.status.success(),
        "malformed manifest verified:\n{combined}"
    );
    assert!(
        combined.contains(expected),
        "wrong strict-decode error, expected `{expected}`:\n{combined}"
    );
}

fn section_offset_after_export(bytes: &[u8]) -> usize {
    let mut cursor = 8usize;
    while cursor < bytes.len() {
        let id = bytes[cursor];
        cursor += 1;
        let size = read_uleb_at(bytes, &mut cursor);
        cursor += size;
        if id == 7 {
            return cursor;
        }
    }
    panic!("compiler-produced module has no export section")
}

/// The declared closure of the emitted certificate, read back out of the
/// `Artifact.lean` the producer just wrote
/// (`closureClaim := ⟨roots, helpers, admitted⟩`). The whole-module guard-iso
/// needs it to keep its escaped-call control non-vacuous: an admitted index is
/// not a mutation that leaves the closure.
fn admitted_closure_indices(artifact_lean: &Path) -> BTreeSet<u32> {
    let text = std::fs::read_to_string(artifact_lean).expect("Artifact.lean exists");
    let start = text
        .find("closureClaim := ⟨")
        .expect("Artifact.lean declares the closure claim")
        + "closureClaim := ⟨".len();
    let end = start + text[start..].find('⟩').expect("the closure claim closes");
    let lists: Vec<&str> = text[start..end]
        .split(']')
        .map(|part| part.trim_start_matches([',', ' ']).trim_start_matches('['))
        .filter(|part| !part.trim().is_empty())
        .collect();
    let admitted = lists.last().expect("the admitted set is the third list");
    admitted
        .split(',')
        .map(|entry| {
            entry
                .trim()
                .parse::<u32>()
                .expect("the admitted set holds function indices")
        })
        .collect()
}

fn certified_opcode_offsets(bytes: &[u8]) -> (usize, u32, usize) {
    let mut imported_funcs = 0u32;
    let mut json_int = None;
    let mut json_entry_key = None;
    let mut code_ordinal = 0u32;
    let mut call = None;
    let mut local_get = None;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("compiler-produced json wasm must parse") {
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
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("export must parse");
                    if export.kind != wasmparser::ExternalKind::Func {
                        continue;
                    }
                    match export.name {
                        "jsonInt" => json_int = Some(export.index),
                        "jsonEntryKey" => json_entry_key = Some(export.index),
                        _ => {}
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let func_idx = imported_funcs + code_ordinal;
                if Some(func_idx) == json_int || Some(func_idx) == json_entry_key {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let opcode_offset = operators.original_position();
                        match operators.read().expect("operator must parse") {
                            wasmparser::Operator::Call { function_index }
                                if Some(func_idx) == json_int && call.is_none() =>
                            {
                                call = Some((opcode_offset + 1, function_index));
                            }
                            wasmparser::Operator::LocalGet { .. }
                                if Some(func_idx) == json_entry_key && local_get.is_none() =>
                            {
                                local_get = Some(opcode_offset);
                            }
                            _ => {}
                        }
                    }
                }
                code_ordinal += 1;
            }
            _ => {}
        }
    }
    let (call_offset, call_target) = call.expect("jsonInt must directly call a helper");
    // The hostile artifact below adds a delta to the byte at `call_offset`, the
    // lowest LEB128 byte of the call target, so the call lands on another
    // function index whatever the encoding width is. That only holds while the
    // low seven bits do not carry into the continuation bit.
    assert!(
        call_target & 0x7f != 0x7f,
        "GuardIso bumps the low LEB byte of the call target; it must not carry"
    );
    let local_get_offset = local_get.expect("jsonEntryKey must contain local.get");
    assert_eq!(bytes[local_get_offset], 0x20);
    (call_offset, call_target, local_get_offset)
}

/// The lowest LEB128 byte of a function index: the seven low bits plus the
/// continuation bit whenever the index needs a second byte.
fn leb_low_byte(index: u32) -> u8 {
    let low = (index & 0x7f) as u8;
    if index >= 0x80 { low | 0x80 } else { low }
}

/// Byte offsets of every instruction START equal to `opcode` inside the body
/// of the defined function at absolute wasm index `func_idx`. Positions are
/// derived by PARSING the module and walking the operator stream, never from
/// a fixed file position, so an immediate byte that happens to equal `opcode`
/// can never be mistaken for the instruction.
fn body_opcode_offsets(bytes: &[u8], func_idx: u32, opcode: u8) -> Vec<usize> {
    let mut imported_funcs = 0u32;
    let mut code_ordinal = 0u32;
    let mut hits = Vec::new();
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
                if imported_funcs + code_ordinal == func_idx {
                    let mut operators = body
                        .get_operators_reader()
                        .expect("helper body must expose its operators");
                    while !operators.eof() {
                        let at = operators.original_position();
                        operators.read().expect("operator must parse");
                        if bytes[at] == opcode {
                            hits.push(at);
                        }
                    }
                }
                code_ordinal += 1;
            }
            _ => {}
        }
    }
    hits
}

fn sole_body_opcode_offset(bytes: &[u8], func_idx: u32, opcode: u8, what: &str) -> usize {
    let hits = body_opcode_offsets(bytes, func_idx, opcode);
    assert_eq!(
        hits.len(),
        1,
        "the {what} body must carry exactly one {opcode:#04x} instruction; refit the mutation"
    );
    hits[0]
}

fn hex_le(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes.iter().rev() {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

/// Extract one top-level `def NAME ...` block (through the line before the
/// next top-level item) from a wall source file. Used to build LITERAL
/// weakened copies of live checker definitions: the copy is derived from the
/// exact source the certificate elaborated against, so a moved or renamed
/// conjunct fails this test loudly instead of letting a stale hand copy
/// keep passing.
fn extract_wall_def(source: &str, name: &str) -> String {
    let header = format!("def {name} ");
    let alt_header = format!("def {name} :");
    let bare_header = format!("def {name}");
    let start = source
        .lines()
        .scan(0usize, |offset, line| {
            let at = *offset;
            *offset += line.len() + 1;
            Some((at, line))
        })
        .find(|(_, line)| {
            line.starts_with(&header) || line.starts_with(&alt_header) || *line == bare_header
        })
        .map(|(at, _)| at)
        .unwrap_or_else(|| panic!("wall source has no top-level `def {name}`"));
    let rest = &source[start..];
    let mut end = rest.len();
    let mut offset = 0usize;
    for (index, line) in rest.lines().enumerate() {
        if index > 0 && !line.is_empty() && !line.starts_with(' ') && !line.starts_with('|') {
            end = offset;
            break;
        }
        offset += line.len() + 1;
    }
    rest[..end].trim_end().to_string()
}

/// Literal copies of the live `arithTableCheck`, each weakened by exactly the
/// listed conjuncts (asserted to occur exactly once) and renamed, wrapped in
/// the wall's own namespace.
fn weakened_arith_table_checks(cert_or_wall: &Path, copies: &[(&str, &[&str])]) -> String {
    let accepted_core = std::fs::read_to_string(cert_or_wall.join("AcceptedArtifactCore.lean"))
        .expect("the materialized wall has AcceptedArtifactCore.lean");
    let live = extract_wall_def(&accepted_core, "arithTableCheck");
    assert_eq!(
        live.matches("arithTableCheck").count(),
        1,
        "`arithTableCheck` is not a single top-level definition; refit the surgery"
    );
    let mut out = String::from("namespace AverCert.AcceptedArtifact\n\n");
    for (name, drop) in copies {
        let mut text = live.clone();
        for conjunct in *drop {
            assert_eq!(
                text.matches(conjunct).count(),
                1,
                "the conjunct `{conjunct}` moved; refit the GuardIso surgery"
            );
            text = text.replace(conjunct, "");
        }
        out.push_str(&format!(
            "/-! Live `arithTableCheck` weakened by exactly: {}. -/\n{}\n\n",
            drop.iter()
                .map(|conjunct| conjunct.trim().trim_end_matches("&&").trim())
                .collect::<Vec<_>>()
                .join("; "),
            text.replace("arithTableCheck", name)
        ));
    }
    out.push_str("end AverCert.AcceptedArtifact\n");
    out
}

const CMP_NAME: &str = "      (roles.cmp == _root_.CertDecode.AddSub.cmpIdx n len) &&\n";
const TO_INDEX_NAME: &str =
    "      (roles.toIndex == _root_.CertDecode.AddSub.toIndexIdx n len) &&\n";
const CARRIER_STATE: &str =
    "      (_root_.CertDecode.carrierState n len == some (some p.carrier)) &&\n";
const CMP_TEMPLATE: &str = "      arithRoleCheck n len .cmp roles.cmp p &&\n";
const TO_INDEX_TEMPLATE: &str = "      arithRoleCheck n len .toIndex roles.toIndex p &&\n";
const EQ_TEMPLATE: &str = "      arithRoleCheck n len .eq roles.eq p &&\n";
const DIVMOD_TEMPLATE: &str = " &&\n      arithRoleCheck n len .divmod roles.divmod p";

fn read_uleb_at(bytes: &[u8], cursor: &mut usize) -> usize {
    let mut value = 0usize;
    let mut shift = 0usize;
    loop {
        let byte = bytes[*cursor];
        *cursor += 1;
        value |= usize::from(byte & 0x7f) << shift;
        if byte & 0x80 == 0 {
            return value;
        }
        shift += 7;
    }
}

fn encode_uleb(mut value: usize) -> Vec<u8> {
    let mut out = Vec::new();
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            out.push(byte | 0x80);
        } else {
            out.push(byte);
            return out;
        }
    }
}

/// Split a module into `(section id, payload)` pairs and re-emit them with
/// re-encoded section sizes, so a tampered payload of any length reframes
/// correctly.
fn module_sections(bytes: &[u8]) -> Vec<(u8, Vec<u8>)> {
    let mut cursor = 8usize;
    let mut sections = Vec::new();
    while cursor < bytes.len() {
        let id = bytes[cursor];
        cursor += 1;
        let size = read_uleb_at(bytes, &mut cursor);
        sections.push((id, bytes[cursor..cursor + size].to_vec()));
        cursor += size;
    }
    sections
}

fn rebuild_module(sections: &[(u8, Vec<u8>)]) -> Vec<u8> {
    let mut out = vec![0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];
    for (id, payload) in sections {
        out.push(*id);
        out.extend(encode_uleb(payload.len()));
        out.extend(payload);
    }
    out
}

/// The `def types : TypeTable := … }` block of an emitted `Plans.lean`,
/// renamed, so a probe can state facts about the package's own declared type
/// table under hostile bytes without restating it.
fn plans_types_block(cert: &Path, rename: &str) -> String {
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let start = plans
        .find("def types : TypeTable :=")
        .expect("Plans.lean declares types");
    let end = plans[start..].find("\n\n").unwrap() + start;
    plans[start..end].replacen("def types :", &format!("def {rename} :"), 1)
}

/// A module WITH the Int box helper and an unrelated carrier-binop-signature
/// function whose body starts with `ref.as_non_null` (outside the retired role
/// scan's vocabulary). It must satisfy no carrierless declaration.
const POISONED_ROLE_SCAN_WAT: &str = r#"
(module
  (type $carrier (struct (field i64) (field anyref) (field i32)))
  (func $box (param i64) (result (ref null $carrier))
    local.get 0
    ref.null any
    i32.const 0
    struct.new $carrier)
  (func $add (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    struct.get $carrier 0
    local.get 1
    struct.get $carrier 0
    i64.add
    ref.null any
    i32.const 0
    struct.new $carrier)
  (func $unrelated (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    ref.as_non_null)
  (export "__rt_aint_from_i64" (func $box)))
"#;

/// The same module without the unscannable function: the healthy control
/// whose strict decode resolves the full table.
const HEALTHY_ROLE_SCAN_WAT: &str = r#"
(module
  (type $carrier (struct (field i64) (field anyref) (field i32)))
  (func $box (param i64) (result (ref null $carrier))
    local.get 0
    ref.null any
    i32.const 0
    struct.new $carrier)
  (func $add (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    struct.get $carrier 0
    local.get 1
    struct.get $carrier 0
    i64.add
    ref.null any
    i32.const 0
    struct.new $carrier)
  (export "__rt_aint_from_i64" (func $box)))
"#;

/// Five hostile artifacts leave all sibling whole-module conjuncts true, fail
/// exactly their named guard, and pass the literal one-conjunct-weakened copy.
/// The manifest's whole-module fields are also required and exact-shaped:
/// malformed candidates decline before any Lean build.
#[test]
fn whole_module_guards_are_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping whole-module GuardIso test: `lake` not available");
        return;
    }
    let (out_dir, cert, wasm) =
        built_package("examples/data/json.av", &[], "cert-whole-module-guard-iso");

    let manifest_path = cert.join("cert-manifest.json");
    let honest_manifest: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&manifest_path).unwrap()).unwrap();
    let wasm_path = out_dir.join("json.wasm");
    for (label, edit, expected) in [
        (
            "declaredUncertified removed",
            Box::new(|m: &mut serde_json::Value| {
                m.as_object_mut().unwrap().remove("declaredUncertified");
            }) as Box<dyn Fn(&mut serde_json::Value)>,
            "missing array field `declaredUncertified`",
        ),
        (
            "capability extra field",
            Box::new(|m: &mut serde_json::Value| {
                m["capabilities"][0]["extra"] = serde_json::json!(true);
            }),
            "must contain exactly fields module, name",
        ),
        (
            "absent start with an index",
            Box::new(|m: &mut serde_json::Value| {
                m["start"]["function_index"] = serde_json::json!(0);
            }),
            "absent start must use null",
        ),
        (
            "hostRoleTable removed",
            Box::new(|m: &mut serde_json::Value| {
                m.as_object_mut().unwrap().remove("hostRoleTable");
            }),
            "missing object field `hostRoleTable`",
        ),
        (
            "hostRoleTable extra field",
            Box::new(|m: &mut serde_json::Value| {
                m["hostRoleTable"]["extra"] = serde_json::json!(0);
            }),
            "must contain exactly fields box, add, mul, sub",
        ),
        (
            "stringHostRoles removed",
            Box::new(|m: &mut serde_json::Value| {
                m.as_object_mut().unwrap().remove("stringHostRoles");
            }),
            "missing array field `stringHostRoles`",
        ),
        (
            "stringHostRoles extra field",
            Box::new(|m: &mut serde_json::Value| {
                m["stringHostRoles"][0]["extra"] = serde_json::json!(true);
            }),
            "must contain exactly fields function_index, role",
        ),
    ] {
        let mut malformed = honest_manifest.clone();
        edit(&mut malformed);
        std::fs::write(
            &manifest_path,
            serde_json::to_vec_pretty(&malformed).unwrap(),
        )
        .unwrap();
        assert_manifest_decode_declines(&wasm_path, &cert, expected);
        let _ = label;
    }
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&honest_manifest).unwrap(),
    )
    .unwrap();

    let start_insert_offset = section_offset_after_export(&wasm);
    let (call_offset, call_target, local_get_offset) = certified_opcode_offsets(&wasm);
    let capability_offset = wasm
        .windows(b"console_print".len())
        .position(|window| window == b"console_print")
        .expect("json wasm must import aver.console_print");
    assert_eq!(wasm[capability_offset], b'c');
    assert_eq!(wasm[call_offset], leb_low_byte(call_target));
    // The escaped-call control re-points `jsonInt`'s first call by adding a
    // delta to the LOW LEB byte of its target. The delta is derived from the
    // closure the producer just declared, so the mutated call provably leaves
    // it.
    let admitted = admitted_closure_indices(&cert.join("Artifact.lean"));
    assert!(
        admitted.contains(&call_target),
        "jsonInt's first callee must be inside the declared closure: \
         {call_target} not in {admitted:?}"
    );
    let call_delta = (1u32..=0x7f)
        .find(|delta| {
            (call_target & 0x7f) + delta <= 0x7f && !admitted.contains(&(call_target + delta))
        })
        .expect("some in-byte call target must sit outside the admitted closure");
    let lean = format!(
        r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

def withoutExports (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.importsWithinCapabilities artifact = true ∧
  AcceptedArtifact.startAccounted artifact = true ∧
  AcceptedArtifact.closureIsolation artifact = true
def withoutCapabilities (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.exportsAccounted artifact = true ∧
  AcceptedArtifact.startAccounted artifact = true ∧
  AcceptedArtifact.closureIsolation artifact = true
def withoutStart (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.exportsAccounted artifact = true ∧
  AcceptedArtifact.importsWithinCapabilities artifact = true ∧
  AcceptedArtifact.closureIsolation artifact = true
def withoutClosure (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.exportsAccounted artifact = true ∧
  AcceptedArtifact.importsWithinCapabilities artifact = true ∧
  AcceptedArtifact.startAccounted artifact = true

-- (a) Existing byte-derived export removed only from the declaration.
def missingExportManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with
         declaredUncertified := manifest.subject.declaredUncertified.tail }} }}
def missingExportArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := missingExportManifest }}
example : AcceptedArtifact.exportsAccounted missingExportArtifact = false := by decide +kernel
example : withoutExports missingExportArtifact :=
  ⟨by decide +kernel, by decide +kernel, by decide +kernel⟩

-- (b) Actual console import is outside the declared capability set.
def unknownCapabilityManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with capabilities := [("aver", "xonsole_print")] }} }}
def unknownCapabilityBytes : Nat := ArtifactBytes.modBytes +
  (21 <<< (8 * {capability_offset}))
def unknownCapabilityArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := unknownCapabilityManifest, modBytes := unknownCapabilityBytes }}
example : CAPABILITY_REGISTRY.contains ("aver", "xonsole_print") = false := by decide
example : AcceptedArtifact.importsWithinCapabilities unknownCapabilityArtifact = false := by
  decide +kernel
example : withoutCapabilities unknownCapabilityArtifact :=
  ⟨by decide +kernel, by decide +kernel, by decide +kernel⟩

-- (c) Insert `start 0` after exports while the manifest declares absent.
def startSectionBytes : Nat :=
  (ArtifactBytes.modBytes &&& ((1 <<< (8 * {start_insert_offset})) - 1)) +
  (0x0108 <<< (8 * {start_insert_offset})) +
  ((ArtifactBytes.modBytes >>> (8 * {start_insert_offset})) <<<
    (8 * ({start_insert_offset} + 3)))
def undeclaredStartArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := startSectionBytes, modLen := ArtifactBytes.modLen + 3 }}
example : AcceptedArtifact.startAccounted undeclaredStartArtifact = false := by decide +kernel
example : withoutStart undeclaredStartArtifact :=
  ⟨by decide +kernel, by decide +kernel, by decide +kernel⟩

-- (d) jsonInt's first call leaves the admitted closure.
def escapedCallBytes : Nat := ArtifactBytes.modBytes +
  ({call_delta} <<< (8 * {call_offset}))
def escapedCallArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := escapedCallBytes }}
example : AcceptedArtifact.closureIsolation escapedCallArtifact = false := by decide +kernel
example : withoutClosure escapedCallArtifact :=
  ⟨by decide +kernel, by decide +kernel, by decide +kernel⟩

-- (e) `local.get` (0x20) -> `global.get` (0x23) in a certified root.
def globalReadBytes : Nat := ArtifactBytes.modBytes +
  (3 <<< (8 * {local_get_offset}))
def globalReadArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := globalReadBytes }}
example : AcceptedArtifact.closureIsolation globalReadArtifact = false := by decide +kernel
example : withoutClosure globalReadArtifact :=
  ⟨by decide +kernel, by decide +kernel, by decide +kernel⟩
"#
    );
    assert_probe_holds(&cert, "GuardIso.lean", &lean);
}

/// S3 GuardIso: module bytes stay identical while only the manifest's
/// host-role table moves. The real module-wide pin `decodedHostRoleTable`
/// (`arithTableCheck`) rejects a hostile `add` index (the function there does
/// not carry the add template) and a hostile or absent `toIndex` (the export
/// name binds it), and the literal copies of the live check weakened by
/// exactly the `toIndex` conjuncts attribute each rejection. A carriered
/// module cannot declare the table absent either.
#[test]
fn inkernel_host_role_table_guard_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping S3 host-role GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, _wasm) = built_package(
        "examples/data/json.av",
        &[],
        "cert-inkernel-host-role-guard-iso",
    );
    let roles = manifest_roles(&cert);
    let add_idx = role(&roles, "add").expect("json add role");
    let to_index_idx = role(&roles, "toIndex");
    let hostile_add = roles_lit(&roles, &[("add", Some(add_idx + 1))]);
    let hostile_to_index = roles_lit(
        &roles,
        &[("toIndex", Some(to_index_idx.map_or(0, |index| index + 1)))],
    );
    let absent_to_index = roles_lit(&roles, &[("toIndex", None)]);
    let weak = weakened_arith_table_checks(
        &cert,
        &[
            (
                "arithTableCheckWithoutToIndex",
                &[TO_INDEX_NAME, TO_INDEX_TEMPLATE],
            ),
            ("arithTableCheckWithoutToIndexName", &[TO_INDEX_NAME]),
        ],
    );
    let absent_block = if to_index_idx.is_some() {
        format!(
            r#"
-- The module exports `__aint_to_index`, but the table declares the role
-- ABSENT. `arithRoleCheck` is vacuous on `none`, so only the export-name
-- equality rejects it: the copy that keeps the template equality and drops
-- only the name equality ACCEPTS it.
def absentToIndexTable : CertDecode.AddSub.Roles := {absent_to_index}
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    (some absentToIndexTable) manifest.subject.arithParams = false := by decide +kernel
example : AcceptedArtifact.arithTableCheckWithoutToIndexName ArtifactBytes.modBytes
    ArtifactBytes.modLen (some absentToIndexTable) manifest.subject.arithParams = true := by
  decide +kernel
"#
        )
    } else {
        String::new()
    };
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

-- The honest control: the package's own proof of the pin.
example : AcceptedArtifact.decodedHostRoleTable Artifact.data := Artifact.roles_ok

-- Same bytes; only the manifest's add index is hostile. The full pin fails:
-- the function at the hostile index does not carry the canonical add body.
def hostileRoleTable : CertDecode.AddSub.Roles := {hostile_add}
def hostileArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest :=
      {{ manifest with subject :=
          {{ manifest.subject with hostRoleTable := some hostileRoleTable }} }} }}
example : ¬ AcceptedArtifact.decodedHostRoleTable hostileArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
      (some hostileRoleTable) manifest.subject.arithParams = true := h
  exact absurd bad (by decide +kernel)

{weak}
-- Same bytes; only the toIndex index is hostile. The fused vector read calls
-- an ABSTRACT contract at the declared index, so this index must be bound to
-- the `__aint_to_index` export and its template, or the contract could be
-- wired to any function. Rejected by the real check, ACCEPTED by the copy
-- weakened by exactly the two toIndex conjuncts.
def hostileToIndexTable : CertDecode.AddSub.Roles := {hostile_to_index}
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    (some hostileToIndexTable) manifest.subject.arithParams = false := by decide +kernel
example : AcceptedArtifact.arithTableCheckWithoutToIndex ArtifactBytes.modBytes
    ArtifactBytes.modLen (some hostileToIndexTable) manifest.subject.arithParams = true := by
  decide +kernel
{absent_block}
-- A carriered artifact cannot declare the table absent either: the box export
-- is present, so the carrierless `none`/`none` arm never closes.
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    none none = false := by decide +kernel
"#
    );
    assert_probe_holds(&cert, "HostRoleGuardIso.lean", &lean);
}

/// Per-role GuardIso for the two Int value-comparison host roles on a module
/// exporting both helpers. `cmp` is pinned twice — to its export name and to
/// its template — while `eq` is pinned by its template alone (the emitter
/// exports `__aint_eq` only when user code marks it live, while an Int literal
/// `match` calls it all the same). For each attack the REAL wall rejects it
/// and a literal copy of the live check weakened by exactly the attacked
/// role's conjuncts accepts it, while the copy weakened by the OTHER role's
/// conjuncts still rejects it. `eq` declared ABSENT passes the table pin by
/// design and is refused where it matters: the plan citing `==` lowers to an
/// index no code entry encodes, so the plans do not bind.
#[test]
fn inkernel_int_comparison_roles_guard_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping Int-comparison role GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, wasm) = built_package(
        "tools/certkit/fixtures/certprobe.av",
        &[],
        "cert-intcmp-role-guard-iso",
    );
    let roles = manifest_roles(&cert);
    let cmp_idx = role(&roles, "cmp").expect("certprobe must declare __aint_cmp");
    let eq_idx = role(&roles, "eq").expect("certprobe must declare __aint_eq");
    assert_ne!(
        cmp_idx, eq_idx,
        "the two helpers must be distinct functions"
    );
    assert!(
        wasm.windows(b"__aint_cmp".len())
            .any(|w| w == b"__aint_cmp"),
        "certprobe must export __aint_cmp"
    );

    let weak = weakened_arith_table_checks(
        &cert,
        &[
            ("weakCmpArithTableCheck", &[CMP_NAME, CMP_TEMPLATE]),
            ("weakEqArithTableCheck", &[EQ_TEMPLATE]),
            ("weakCmpNameArithTableCheck", &[CMP_NAME]),
        ],
    );
    let table = |name: &str, cmp: Option<u32>, eq: Option<u32>| {
        format!(
            "def {name}Table : CertDecode.AddSub.Roles := {}\n\
             def {name}Artifact : AcceptedArtifact.ArtifactData :=\n  \
             {{ Artifact.data with manifest :=\n      \
             {{ manifest with subject :=\n          \
             {{ manifest.subject with hostRoleTable := some {name}Table }} }} }}\n",
            roles_lit(&roles, &[("cmp", cmp), ("eq", eq)])
        )
    };
    let tables = [
        table("hostileCmp", Some(cmp_idx + 1), Some(eq_idx)),
        table("hostileEq", Some(cmp_idx), Some(eq_idx + 1)),
        table("absentCmp", None, Some(eq_idx)),
        table("absentEq", Some(cmp_idx), None),
        table("swapped", Some(eq_idx), Some(cmp_idx)),
    ]
    .join("\n");
    let check = |table: &str, which: &str, verdict: &str| {
        format!(
            "example : AcceptedArtifact.{which} ArtifactBytes.modBytes ArtifactBytes.modLen\n    \
             (some {table}Table) manifest.subject.arithParams = {verdict} := by decide +kernel\n"
        )
    };
    let mut examples = String::new();
    for (table_name, real, weak_cmp, weak_eq, weak_cmp_name) in [
        // A hostile `cmp` index: both cmp pins reject it; the eq copy keeps them.
        ("hostileCmp", "false", "true", "false", "false"),
        // A hostile `eq` index: only the eq template rejects it.
        ("hostileEq", "false", "false", "true", "false"),
        // `cmp` declared absent while exported: the name equality alone
        // rejects it (the template is vacuous on `none`).
        ("absentCmp", "false", "true", "false", "true"),
        // `eq` declared absent: accepted by the table pin by design.
        ("absentEq", "true", "true", "true", "true"),
        // Each role at the other one's index: refused by both roles' pins.
        ("swapped", "false", "false", "false", "false"),
    ] {
        examples.push_str(&check(table_name, "arithTableCheck", real));
        examples.push_str(&check(table_name, "weakCmpArithTableCheck", weak_cmp));
        examples.push_str(&check(table_name, "weakEqArithTableCheck", weak_eq));
        examples.push_str(&check(
            table_name,
            "weakCmpNameArithTableCheck",
            weak_cmp_name,
        ));
    }
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

-- BYTE-DERIVED GROUND TRUTH: `cmp` is bound by its export name.
example : CertDecode.AddSub.cmpIdx ArtifactBytes.modBytes ArtifactBytes.modLen
    = some {cmp_idx} := by decide +kernel

-- The honest control, and every weakened copy accepts it too, so each flip
-- below is caused by the hostile declaration and not by the surgery.
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    manifest.subject.hostRoleTable manifest.subject.arithParams = true := by decide +kernel
{weak}
example : AcceptedArtifact.weakCmpArithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    manifest.subject.hostRoleTable manifest.subject.arithParams = true := by decide +kernel
example : AcceptedArtifact.weakEqArithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    manifest.subject.hostRoleTable manifest.subject.arithParams = true := by decide +kernel
example : AcceptedArtifact.weakCmpNameArithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    manifest.subject.hostRoleTable manifest.subject.arithParams = true := by decide +kernel

{tables}
{examples}
-- `eq` declared ABSENT is refused by the plans: `sameKey` compares with `==`,
-- which lowers to a call at the (never encoded) absent index.
example : AcceptedArtifact.plansAccepted absentEqArtifact = false := by decide +kernel

-- The declared-type gate CANNOT separate the two roles: the two helpers
-- declare the same function type, so the swapped table passes it against the
-- module's own type section. The export-name and template pins are what tell
-- the roles apart.
example : AcceptedArtifact.roleTypesPinned ArtifactBytes.modBytes ArtifactBytes.modLen
    (AverCert.TypeTable.mctxOf swappedArtifact.manifest.subject manifest.types manifest.fnPlans)
    = true := by decide +kernel
"#
    );
    assert_probe_holds(&cert, "IntCmpRoleGuardIso.lean", &lean);
}

/// The mirror of the comparison-role GuardIso: the DECLARATION is honest and
/// one byte of a helper body moves. A live single-byte mutation inside each of
/// three template-pinned helpers — `__aint_cmp` (`i32.gt_s` -> `i32.lt_s`),
/// `__aint_eq` (`i64.eq` -> `i64.ne`) and the Euclidean `__aint_divmod`
/// helper (`i64.div_s` -> `i64.div_u`) — keeps every export, every index,
/// every declared function type and the module length, so nothing but the
/// template equality can tell the mutant from the certified module. For each
/// mutant the real pin refuses it under the certificate's own table, the copy
/// weakened by exactly that role's template conjunct accepts it, and the
/// copies weakened elsewhere still refuse it.
#[test]
fn inkernel_int_comparison_role_bodies_reject_a_flipped_body_byte() {
    if !lake_available() {
        eprintln!("skipping helper body-mutation GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, wasm) = built_package(
        "tools/certkit/fixtures/certprobe.av",
        &[],
        "cert-intcmp-body-mutation-guard-iso",
    );
    let roles = manifest_roles(&cert);
    let cmp_idx = role(&roles, "cmp").expect("certprobe must declare __aint_cmp");
    let eq_idx = role(&roles, "eq").expect("certprobe must declare __aint_eq");
    let divmod_idx = role(&roles, "divmod").expect("certprobe must declare __aint_divmod");

    let cmp_at = sole_body_opcode_offset(&wasm, cmp_idx, 0x4a, "__aint_cmp");
    let eq_at = sole_body_opcode_offset(&wasm, eq_idx, 0x51, "__aint_eq");
    let divmod_at = sole_body_opcode_offset(&wasm, divmod_idx, 0x7f, "__aint_divmod");
    let mut cmp_mut = wasm.clone();
    cmp_mut[cmp_at] = 0x48;
    let mut eq_mut = wasm.clone();
    eq_mut[eq_at] = 0x52;
    let mut divmod_mut = wasm.clone();
    divmod_mut[divmod_at] = 0x80;
    for (label, mutant) in [
        ("__aint_cmp", &cmp_mut),
        ("__aint_eq", &eq_mut),
        ("__aint_divmod", &divmod_mut),
    ] {
        wasmparser::Validator::new()
            .validate_all(mutant)
            .unwrap_or_else(|error| panic!("the {label} mutant must stay valid wasm: {error}"));
        assert_eq!(
            mutant.iter().zip(&wasm).filter(|(a, b)| a != b).count(),
            1,
            "the {label} mutant must differ in exactly one byte"
        );
    }

    let weak = weakened_arith_table_checks(
        &cert,
        &[
            ("weakCmpTemplateArithTableCheck", &[CMP_TEMPLATE]),
            ("weakEqTemplateArithTableCheck", &[EQ_TEMPLATE]),
            ("weakCmpNameArithTableCheck", &[CMP_NAME]),
            ("weakDivmodTemplateArithTableCheck", &[DIVMOD_TEMPLATE]),
        ],
    );
    let verdict = |bytes: &str, which: &str, value: &str| {
        format!(
            "example : AcceptedArtifact.{which} {bytes} ArtifactBytes.modLen\n    \
             manifest.subject.hostRoleTable manifest.subject.arithParams = {value} := by\n  \
             decide +kernel\n"
        )
    };
    let mut examples = String::new();
    for (bytes, sole) in [
        ("cmpMutBytes", "weakCmpTemplateArithTableCheck"),
        ("eqMutBytes", "weakEqTemplateArithTableCheck"),
        ("divmodMutBytes", "weakDivmodTemplateArithTableCheck"),
    ] {
        examples.push_str(&verdict(bytes, "arithTableCheck", "false"));
        for copy in [
            "weakCmpTemplateArithTableCheck",
            "weakEqTemplateArithTableCheck",
            "weakCmpNameArithTableCheck",
            "weakDivmodTemplateArithTableCheck",
        ] {
            examples.push_str(&verdict(
                bytes,
                copy,
                if copy == sole { "true" } else { "false" },
            ));
        }
        // A body edit is invisible to the declared-type gate.
        examples.push_str(&format!(
            "example : AcceptedArtifact.roleTypesPinned {bytes} ArtifactBytes.modLen\n    \
             (AverCert.TypeTable.mctxOf manifest.subject manifest.types manifest.fnPlans) = true := by\n  \
             decide +kernel\n"
        ));
    }
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

-- The three mutants, as the LITERAL numerals of the byte vectors the harness
-- validated as wasm.
def cmpMutBytes : Nat := 0x{cmp_hex}
def eqMutBytes : Nat := 0x{eq_hex}
def divmodMutBytes : Nat := 0x{divmod_hex}

-- Each mutant is the certified module with ONE byte moved by one opcode step.
example : ArtifactBytes.modBytes - cmpMutBytes = 2 <<< {cmp_shift} := by decide +kernel
example : eqMutBytes - ArtifactBytes.modBytes = 1 <<< {eq_shift} := by decide +kernel
example : divmodMutBytes - ArtifactBytes.modBytes = 1 <<< {divmod_shift} := by decide +kernel

-- The name-bound role reads the same export section in the mutants.
example : CertDecode.AddSub.cmpIdx cmpMutBytes ArtifactBytes.modLen = some {cmp_idx} := by
  decide +kernel

-- THE DECLINE: the certificate's own honest declaration refuses every mutant.
def cmpMutArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := cmpMutBytes }}
example : ¬ AcceptedArtifact.decodedHostRoleTable cmpMutArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck cmpMutBytes ArtifactBytes.modLen
      manifest.subject.hostRoleTable manifest.subject.arithParams = true := h
  exact absurd bad (by decide +kernel)
{weak}
-- Every weakened copy accepts the HONEST module.
example : AcceptedArtifact.weakCmpTemplateArithTableCheck ArtifactBytes.modBytes
    ArtifactBytes.modLen manifest.subject.hostRoleTable manifest.subject.arithParams = true := by
  decide +kernel
example : AcceptedArtifact.weakEqTemplateArithTableCheck ArtifactBytes.modBytes
    ArtifactBytes.modLen manifest.subject.hostRoleTable manifest.subject.arithParams = true := by
  decide +kernel
example : AcceptedArtifact.weakCmpNameArithTableCheck ArtifactBytes.modBytes
    ArtifactBytes.modLen manifest.subject.hostRoleTable manifest.subject.arithParams = true := by
  decide +kernel
example : AcceptedArtifact.weakDivmodTemplateArithTableCheck ArtifactBytes.modBytes
    ArtifactBytes.modLen manifest.subject.hostRoleTable manifest.subject.arithParams = true := by
  decide +kernel

-- ATTRIBUTION per mutant: rejected by the real check, accepted only by the
-- copy without that role's template conjunct.
{examples}"#,
        cmp_hex = hex_le(&cmp_mut),
        eq_hex = hex_le(&eq_mut),
        divmod_hex = hex_le(&divmod_mut),
        cmp_shift = 8 * cmp_at,
        eq_shift = 8 * eq_at,
        divmod_shift = 8 * divmod_at,
    );
    assert_probe_holds(&cert, "HelperBodyMutationGuardIso.lean", &lean);
}

/// A module WITH the Int box helper whose declared table is the carrierless
/// `null` must be REJECTED at the host-role pin: the carrierless arm demands
/// the box export be byte-provably absent. Two hand-built carriered modules
/// (one with an extra function the retired role scan could not read) and the
/// real `add_one` module all refuse the claim.
#[test]
fn a_carriered_module_cannot_claim_the_carrierless_null_table() {
    if !lake_available() {
        eprintln!("skipping carrierless-null-claim test: `lake` not available");
        return;
    }
    let poisoned = wat::parse_str(POISONED_ROLE_SCAN_WAT).expect("poisoned WAT compiles");
    let healthy = wat::parse_str(HEALTHY_ROLE_SCAN_WAT).expect("healthy WAT compiles");
    for (name, bytes) in [("poisoned", &poisoned), ("healthy", &healthy)] {
        wasmparser::Validator::new()
            .validate_all(bytes)
            .unwrap_or_else(|error| panic!("{name} module must be valid wasm: {error}"));
    }
    let (_out_dir, cert, _wasm) = built_package(
        "examples/certification/add_one.av",
        &[],
        "cert-poisoned-role-scan-pin",
    );
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

def poisonedBytes : Nat := 0x{poisoned_hex}
def poisonedLen : Nat := {poisoned_len}
def healthyBytes : Nat := 0x{healthy_hex}
def healthyLen : Nat := {healthy_len}

-- The attack itself: the module claims the carrierless `null`.
def nullClaimManifest : Manifest :=
  {{ manifest with subject := {{ manifest.subject with
      hostRoleTable := none, arithParams := none, stringHostRoles := [] }} }}
def poisonedArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with
      modBytes := poisonedBytes, modLen := poisonedLen,
      manifest := nullClaimManifest }}

-- The sibling string-role pin accepts it (no String helper)...
example : AcceptedArtifact.decodedStringHostRoles poisonedArtifact := by
  show CertDecode.StringHost.roleTable poisonedBytes poisonedLen = some []
  decide +kernel

-- ...and the host-role pin rejects it: the box export is present.
example : ¬ AcceptedArtifact.decodedHostRoleTable poisonedArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck poisonedBytes poisonedLen none none = true := h
  exact absurd bad (by decide +kernel)
example : AcceptedArtifact.arithTableCheck healthyBytes healthyLen none none = false := by
  decide +kernel
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    none none = false := by decide +kernel
"#,
        poisoned_hex = hex_le(&poisoned),
        poisoned_len = poisoned.len(),
        healthy_hex = hex_le(&healthy),
        healthy_len = healthy.len(),
    );
    assert_probe_holds(&cert, "PoisonedRoleScanPin.lean", &lean);
}

/// GuardIso for the LEB encoding of the arith template splices. The wall
/// synthesizes the helper bodies with every declared index spliced through
/// canonical LEB128; an earlier revision spliced call targets as single raw
/// bytes and bounded every index below 128. Three facts are pinned here, all
/// against kernel-evaluated modules built inside the fixture itself:
///
/// (a) a hostile artifact that declares `decompose = 166` (a two-byte LEB)
///     while its code-entry bytes leave `10 a6 21 ...` — a call to function
///     4262, not 166 — is REJECTED by the real template equality;
/// (b) a literal template copy that splices the call targets raw ACCEPTS that
///     same hostile artifact, so the LEB encoding is load-bearing, not
///     decoration: under raw splicing a certificate could declare one callee
///     while the module calls another;
/// (c) a literal copy of the retired `< 128` bound REJECTS the honest module
///     whose body carries the canonical two-byte call encodings — the false
///     negative that made high-index programs uncertifiable — while the real
///     check accepts it.
///
/// No certified package is needed: only the wall itself is staged, and the
/// fixture derives both modules from the template definitions under test.
#[test]
fn arith_call_target_leb_encoding_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping arith LEB GuardIso test: `lake` not available");
        return;
    }
    let wall_dir = built_wall(
        "cert-arith-leb-guard-iso",
        &[
            "CertPrelude",
            "CertDecode",
            "SchemaCore",
            "ArithTemplateDerisk",
            "WasmSlice",
            "Wasip2Envelope",
            "AcceptedArtifactCore",
        ],
    );

    let lean = r#"import AcceptedArtifactCore

open AverCert ArithTemplateDerisk CertPrelude
set_option maxRecDepth 300000
noncomputable section

-- Params shaped like a large honest program: the four sub-routine call
-- targets sit in the two-byte unsigned-LEB band (166..169, the indices the
-- notepad fixture's helpers land at), while carrier and limb stay small so
-- every non-call hole encodes identically under both templates below.
def highParams : ArithHostParams :=
  { carrier := 1, limb := 0, decompose := 166, normalize := 167,
    strip := 168, umagCmp := 169 }

-- The u32 bound admits them.
example : checkArithHostParams highParams = true := by decide

-- Literal copy of the retired single-byte bound. `checkArithHostParams` is a
-- hard conjunct of `arithTableCheck`, so under this bound every module whose
-- helper indices reach 128 was unprovable regardless of its bytes.
def checkArithHostParamsSingleByte (p : ArithHostParams) : Bool :=
  decide (p.carrier < 128) && decide (p.limb < 64) &&
  decide (p.decompose < 128) && decide (p.normalize < 128) &&
  decide (p.strip < 128) && decide (p.umagCmp < 128)

example : checkArithHostParamsSingleByte highParams = false := by decide

-- Literal copy of `addTemplateBody` with exactly one weakening: the four
-- sub-routine CALL targets are spliced as single raw bytes (the retired
-- synthesis) instead of through `uleb32Bytes`. Carrier and limb keep the real
-- encoders, so the acceptance flips below are attributable to the call-target
-- encoding alone.
def addTemplateBodyRawSplice (p : ArithHostParams) : List Nat :=
    [0x09, 0x01, 0x7e, 0x01, 0x63] ++
    s33Bytes p.limb ++
    [0x01, 0x7f, 0x01, 0x63] ++
    s33Bytes p.limb ++
    [0x05, 0x7f, 0x01, 0x63] ++
    s33Bytes p.limb ++
    [0x04, 0x7f, 0x06, 0x7e, 0x01, 0x63] ++
    s33Bytes p.limb ++
    [0x20, 0x00, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x01, 0xd1, 0x20, 0x01, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x01, 0xd1, 0x71, 0x04, 0x63] ++
    s33Bytes p.carrier ++
    [0x20, 0x00, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x00, 0x20, 0x01, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x00, 0x7c, 0x21, 0x02, 0x20, 0x00, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x00, 0x20, 0x02, 0x85, 0x20, 0x01, 0xfb, 0x02] ++
    uleb32Bytes p.carrier ++
    [0x00, 0x20, 0x02, 0x85, 0x83, 0x42, 0x00, 0x53, 0x04, 0x63] ++
    s33Bytes p.carrier ++
    [0x20, 0x00, 0x10] ++
    [p.decompose] ++
    [0x21, 0x04, 0x21, 0x03, 0x20, 0x01, 0x10] ++
    [p.decompose] ++
    [0x21, 0x06, 0x21, 0x05, 0x20, 0x06, 0x21, 0x07, 0x20, 0x03, 0x10] ++
    [p.strip] ++
    [0x21, 0x08, 0x20, 0x05, 0x10] ++
    [p.strip] ++
    [0x21, 0x09, 0x20, 0x04, 0x45, 0x20, 0x07, 0x45, 0x20, 0x04, 0x20, 0x07, 0x46, 0x72, 0x72, 0x04, 0x40, 0x41, 0x01, 0x20, 0x08, 0x20, 0x09, 0x4b, 0x04, 0x7f, 0x20, 0x08, 0x05, 0x20, 0x09, 0x0b, 0x6a, 0x21, 0x0a, 0x20, 0x0a, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0d, 0x42, 0x00, 0x21, 0x10, 0x02, 0x40, 0x03, 0x40, 0x20, 0x0d, 0x20, 0x0a, 0x4f, 0x0d, 0x01, 0x20, 0x10, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x7c, 0x7c, 0x21, 0x10, 0x20, 0x0b, 0x20, 0x0d, 0x20, 0x10, 0x42, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x83, 0xfb, 0x0e] ++
    uleb32Bytes p.limb ++
    [0x20, 0x10, 0x42, 0x20, 0x88, 0x21, 0x10, 0x20, 0x0d, 0x41, 0x01, 0x6a, 0x21, 0x0d, 0x0c, 0x00, 0x0b, 0x0b, 0x20, 0x04, 0x45, 0x04, 0x7f, 0x20, 0x07, 0x05, 0x20, 0x04, 0x0b, 0x21, 0x0c, 0x05, 0x20, 0x03, 0x20, 0x08, 0x20, 0x05, 0x20, 0x09, 0x10] ++
    [p.umagCmp] ++
    [0x21, 0x0f, 0x20, 0x0f, 0x45, 0x04, 0x40, 0x41, 0x00, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0c, 0x05, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7f, 0x20, 0x04, 0x05, 0x20, 0x07, 0x0b, 0x21, 0x0c, 0x20, 0x08, 0x20, 0x09, 0x4b, 0x04, 0x7f, 0x20, 0x08, 0x05, 0x20, 0x09, 0x0b, 0x21, 0x0a, 0x20, 0x0a, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0d, 0x42, 0x00, 0x21, 0x11, 0x02, 0x40, 0x03, 0x40, 0x20, 0x0d, 0x20, 0x0a, 0x4f, 0x0d, 0x01, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7e, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x05, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x0b, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7e, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x05, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x0b, 0x7d, 0x20, 0x11, 0x7d, 0x21, 0x12, 0x20, 0x12, 0x42, 0x00, 0x53, 0x04, 0x40, 0x20, 0x12, 0x42, 0x80, 0x80, 0x80, 0x80, 0x10, 0x7c, 0x21, 0x12, 0x42, 0x01, 0x21, 0x11, 0x05, 0x42, 0x00, 0x21, 0x11, 0x0b, 0x20, 0x0b, 0x20, 0x0d, 0x20, 0x12, 0x42, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x83, 0xfb, 0x0e] ++
    uleb32Bytes p.limb ++
    [0x20, 0x0d, 0x41, 0x01, 0x6a, 0x21, 0x0d, 0x0c, 0x00, 0x0b, 0x0b, 0x0b, 0x0b, 0x20, 0x0b, 0x20, 0x0c, 0x10] ++
    [p.normalize] ++
    [0x05, 0x20, 0x02, 0xd0] ++
    s33Bytes p.limb ++
    [0x41, 0x00, 0xfb, 0x00] ++
    uleb32Bytes p.carrier ++
    [0x0b, 0x05, 0x20, 0x00, 0x10] ++
    [p.decompose] ++
    [0x21, 0x04, 0x21, 0x03, 0x20, 0x01, 0x10] ++
    [p.decompose] ++
    [0x21, 0x06, 0x21, 0x05, 0x20, 0x06, 0x21, 0x07, 0x20, 0x03, 0x10] ++
    [p.strip] ++
    [0x21, 0x08, 0x20, 0x05, 0x10] ++
    [p.strip] ++
    [0x21, 0x09, 0x20, 0x04, 0x45, 0x20, 0x07, 0x45, 0x20, 0x04, 0x20, 0x07, 0x46, 0x72, 0x72, 0x04, 0x40, 0x41, 0x01, 0x20, 0x08, 0x20, 0x09, 0x4b, 0x04, 0x7f, 0x20, 0x08, 0x05, 0x20, 0x09, 0x0b, 0x6a, 0x21, 0x0a, 0x20, 0x0a, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0d, 0x42, 0x00, 0x21, 0x10, 0x02, 0x40, 0x03, 0x40, 0x20, 0x0d, 0x20, 0x0a, 0x4f, 0x0d, 0x01, 0x20, 0x10, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x7c, 0x7c, 0x21, 0x10, 0x20, 0x0b, 0x20, 0x0d, 0x20, 0x10, 0x42, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x83, 0xfb, 0x0e] ++
    uleb32Bytes p.limb ++
    [0x20, 0x10, 0x42, 0x20, 0x88, 0x21, 0x10, 0x20, 0x0d, 0x41, 0x01, 0x6a, 0x21, 0x0d, 0x0c, 0x00, 0x0b, 0x0b, 0x20, 0x04, 0x45, 0x04, 0x7f, 0x20, 0x07, 0x05, 0x20, 0x04, 0x0b, 0x21, 0x0c, 0x05, 0x20, 0x03, 0x20, 0x08, 0x20, 0x05, 0x20, 0x09, 0x10] ++
    [p.umagCmp] ++
    [0x21, 0x0f, 0x20, 0x0f, 0x45, 0x04, 0x40, 0x41, 0x00, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0c, 0x05, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7f, 0x20, 0x04, 0x05, 0x20, 0x07, 0x0b, 0x21, 0x0c, 0x20, 0x08, 0x20, 0x09, 0x4b, 0x04, 0x7f, 0x20, 0x08, 0x05, 0x20, 0x09, 0x0b, 0x21, 0x0a, 0x20, 0x0a, 0xfb, 0x07] ++
    uleb32Bytes p.limb ++
    [0x21, 0x0b, 0x41, 0x00, 0x21, 0x0d, 0x42, 0x00, 0x21, 0x11, 0x02, 0x40, 0x03, 0x40, 0x20, 0x0d, 0x20, 0x0a, 0x4f, 0x0d, 0x01, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7e, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x05, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x0b, 0x20, 0x0f, 0x41, 0x00, 0x4a, 0x04, 0x7e, 0x20, 0x0d, 0x20, 0x09, 0x49, 0x04, 0x7e, 0x20, 0x05, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x05, 0x20, 0x0d, 0x20, 0x08, 0x49, 0x04, 0x7e, 0x20, 0x03, 0x20, 0x0d, 0xfb, 0x0b] ++
    uleb32Bytes p.limb ++
    [0x05, 0x42, 0x00, 0x0b, 0x0b, 0x7d, 0x20, 0x11, 0x7d, 0x21, 0x12, 0x20, 0x12, 0x42, 0x00, 0x53, 0x04, 0x40, 0x20, 0x12, 0x42, 0x80, 0x80, 0x80, 0x80, 0x10, 0x7c, 0x21, 0x12, 0x42, 0x01, 0x21, 0x11, 0x05, 0x42, 0x00, 0x21, 0x11, 0x0b, 0x20, 0x0b, 0x20, 0x0d, 0x20, 0x12, 0x42, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x83, 0xfb, 0x0e] ++
    uleb32Bytes p.limb ++
    [0x20, 0x0d, 0x41, 0x01, 0x6a, 0x21, 0x0d, 0x0c, 0x00, 0x0b, 0x0b, 0x0b, 0x0b, 0x20, 0x0b, 0x20, 0x0c, 0x10] ++
    [p.normalize] ++
    [0x0b, 0x0b]

-- Weakened role check: identical to `AcceptedArtifact.arithRoleCheck` for the
-- add role except that it compares against the raw-splice template.
def arithRoleCheckRawSplice (n len : Nat) (idx? : Option Nat)
    (p : ArithHostParams) : Bool :=
  match idx? with
  | none => true
  | some idx =>
      AcceptedArtifact.bodyBytesAtFuncIndex n len idx ==
        some (addTemplateBodyRawSplice p)

-- The two syntheses genuinely diverge at these params (each two-byte call
-- target loses its second byte under raw splicing).
example : addTemplateBodyRawSplice highParams ≠ arithHelperBody .add highParams := by
  decide

-- Little-endian byte list -> the big-Nat representation the decoders read.
def natOfBytes (bytes : List Nat) : Nat :=
  bytes.foldr (fun b acc => b + 256 * acc) 0

-- The hostile artifact: a module whose single code entry is byte-for-byte the
-- RAW-SPLICE synthesis for `highParams`. Read as wasm, its first call site is
-- `10 a6 21 ...`: the target LEB is `a6 21` = function 4262, not the declared
-- 166. Only the module header and the code section are framed — exactly what
-- `bodyBytesAtFuncIndex` (funcImportBase + codeLocs) consumes.
def rawBody : List Nat := addTemplateBodyRawSplice highParams
def rawEntry : List Nat := uleb32Bytes rawBody.length ++ rawBody
def rawCodePayload : List Nat := 0x01 :: rawEntry
def rawModuleBytes : List Nat :=
  [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x0a] ++
  uleb32Bytes rawCodePayload.length ++ rawCodePayload

-- What the hostile bytes actually call: `a6 21` is the canonical encoding of
-- 4262, while the declared 166 encodes as `a6 01`.
example : uleb32Bytes 166 = [0xa6, 0x01] := by decide
example : uleb32Bytes 4262 = [0xa6, 0x21] := by decide

-- (a) The REAL check rejects the hostile artifact: the canonical template
-- carries `call 166` as `a6 01`, and these bytes do not.
example : AcceptedArtifact.arithRoleCheck (natOfBytes rawModuleBytes)
    rawModuleBytes.length .add (some 0) highParams = false := by decide +kernel

-- ...and not because of the bound: the declaration is inside the u32 band.
-- The rejection is the template equality's alone.
example : checkArithHostParams highParams = true := by decide

-- (b) The raw-splice copy ACCEPTS the same hostile artifact. Under the
-- retired synthesis a certificate could therefore declare `decompose = 166`
-- for a module that calls function 4262 at that hole.
example : arithRoleCheckRawSplice (natOfBytes rawModuleBytes)
    rawModuleBytes.length (some 0) highParams = true := by decide +kernel

-- The honest control: the same framing around the CANONICAL synthesis. The
-- real check accepts it, so the LEB path itself is exercised and (a) is
-- attributable to the raw splicing, not to the framing.
def honestBody : List Nat := arithHelperBody .add highParams
def honestEntry : List Nat := uleb32Bytes honestBody.length ++ honestBody
def honestCodePayload : List Nat := 0x01 :: honestEntry
def honestModuleBytes : List Nat :=
  [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x0a] ++
  uleb32Bytes honestCodePayload.length ++ honestCodePayload

example : AcceptedArtifact.arithRoleCheck (natOfBytes honestModuleBytes)
    honestModuleBytes.length .add (some 0) highParams = true := by decide +kernel

-- (c) The retired bound alone rejected this honest module: with the template
-- equality holding (just proved), the old-check conjunction still fails, and
-- it fails at the bound.
example : (checkArithHostParamsSingleByte highParams &&
    AcceptedArtifact.arithRoleCheck (natOfBytes honestModuleBytes)
      honestModuleBytes.length .add (some 0) highParams) = false := by
  decide +kernel

-- Symmetry: the raw-splice template does not match the honest LEB bytes
-- either — the two syntheses classify these two modules oppositely, so the
-- fixture cannot be satisfied by a template that ignores the splice bytes.
example : arithRoleCheckRawSplice (natOfBytes honestModuleBytes)
    honestModuleBytes.length (some 0) highParams = false := by decide +kernel
"#;
    std::fs::write(wall_dir.join("ArithLebGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("env")
        .arg("lean")
        .arg("ArithLebGuardIso.lean")
        .output()
        .expect("run the arith LEB GuardIso check");
    assert!(
        check.status.success(),
        "arith LEB GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
}

/// The two comparison helper templates at WIDE declared indices, where every
/// splice needs two bytes.
///
/// Every hole in every module the corpus measured is below `0x80`, so the
/// multi-byte branch of both encoders is empirically unexercised: the corpus
/// proves the templates only in the narrow regime, and the `boxTemplateBody`
/// doc block warns about exactly this class ("splicing either hole as a raw
/// byte would synthesize a body no emitter produces the moment the index
/// outgrows one byte"). This check closes the gap by construction rather than
/// by hoping a module grows large enough.
///
/// The splice check is LENGTH ARITHMETIC plus position: `cmp` has seven holes
/// (limb twice, decompose twice, strip twice, umag_cmp once) and `eq` has nine
/// (limb three times, carrier six), so the body grows by exactly the hole count
/// if and only if EVERY hole went through an encoder — a raw-spliced hole would
/// leave the length short. The whole pin path is then exercised at those
/// widths: a module framed around each wide body is accepted at its own role
/// and refused at the other one's.
///
/// Width alone does NOT separate the two encoders, though, and the check would
/// be hollow if it stopped there: at every hole value the wide parameters use,
/// `s33Bytes` and `uleb32Bytes` agree byte for byte, so swapping one encoder
/// for the other would go unnoticed. The `[64, 127]` band is where they part —
/// the s33 sign bit forces a second byte while the unsigned form still fits in
/// one — so a third instantiation puts `limb` at 100 and pins the divergence
/// directly.
#[test]
fn comparison_templates_splice_wide_indices_through_their_encoders() {
    if !lake_available() {
        eprintln!("skipping wide-index template check: `lake` not available");
        return;
    }
    let wall_dir = built_wall(
        "cert-intcmp-wide-template",
        &[
            "CertPrelude",
            "CertDecode",
            "SchemaCore",
            "ArithTemplateDerisk",
            "WasmSlice",
            "Wasip2Envelope",
            "AcceptedArtifactCore",
        ],
    );

    let lean = r#"import AcceptedArtifactCore

open AverCert ArithTemplateDerisk CertPrelude
set_option maxRecDepth 300000
noncomputable section

-- The regime every measured module sits in: each hole below 0x80, one byte
-- per splice. These are the smallest module's real declared indices.
def narrowParams : ArithHostParams :=
  { carrier := 2, limb := 1, decompose := 5, normalize := 6, strip := 7, umagCmp := 8 }

-- The regime no module has reached yet: every hole either comparison template
-- mentions needs two bytes. `normalize` occurs in neither body and is only
-- here because the record has the field.
def wideParams : ArithHostParams :=
  { carrier := 200, limb := 130, decompose := 300, normalize := 600,
    strip := 400, umagCmp := 500 }

-- The regime that tells the two ENCODERS apart. `limb` is the only index that
-- occurs at a signed s33 position, and 100 is inside `[64, 127]`, where the
-- s33 sign bit forces a second byte while the unsigned form still fits in one.
-- Everything else stays narrow so the length arithmetic below isolates it.
def bandParams : ArithHostParams :=
  { carrier := 2, limb := 100, decompose := 5, normalize := 6, strip := 7,
    umagCmp := 8 }

-- All three declarations are inside the u32 band the pin admits, so nothing
-- below is decided by the bound.
example : checkArithHostParams narrowParams = true := by decide
example : checkArithHostParams wideParams = true := by decide
example : checkArithHostParams bandParams = true := by decide

-- Each hole is spliced through the encoder its POSITION demands: a signed s33
-- in heap-type positions, an unsigned uleb32 everywhere else.
example : s33Bytes 130 = [0x82, 0x01] := by decide
example : uleb32Bytes 130 = [0x82, 0x01] := by decide
example : uleb32Bytes 200 = [0xc8, 0x01] := by decide
example : uleb32Bytes 300 = [0xac, 0x02] := by decide
example : uleb32Bytes 400 = [0x90, 0x03] := by decide
example : uleb32Bytes 500 = [0xf4, 0x03] := by decide

-- ...and at 130 the two encoders AGREE, which is exactly why the wide set
-- cannot witness an encoder swap on its own. At 100 they diverge in both
-- width and bytes, so every assertion keyed on `bandParams` below fails the
-- moment a signed splice is replaced by an unsigned one or vice versa.
example : s33Bytes 100 = [0xe4, 0x00] := by decide
example : uleb32Bytes 100 = [0x64] := by decide

-- LENGTH ARITHMETIC: seven holes in `cmp`, nine in `eq`, and each one grows by
-- exactly one byte. A hole spliced as a raw byte would not move at all.
example : (cmpTemplateBody narrowParams).length = 101 := by decide
example : (cmpTemplateBody wideParams).length = 101 + 7 := by decide
example : (eqTemplateBody narrowParams).length = 157 := by decide
example : (eqTemplateBody wideParams).length = 157 + 9 := by decide

-- POSITION, first hole: the locals vector of each body, where the limb index
-- sits at a SIGNED heap-type position inside `(ref null $mag)`.
example : (cmpTemplateBody wideParams).take 5 = [0x05, 0x01, 0x63, 0x82, 0x01] := by decide
example : (eqTemplateBody wideParams).take 5 = [0x02, 0x02, 0x63, 0x82, 0x01] := by decide

-- POSITION, last call hole of `cmp`: `call $umag_cmp` followed by the store to
-- the verdict local, so the two-byte target did not displace the instruction
-- after it.
example : ((cmpTemplateBody wideParams).drop 84).take 5 = [0x10, 0xf4, 0x03, 0x21, 0x08] := by
  decide

-- POSITION, first carrier hole of `eq`: `struct.get $aint $magf` reads the
-- carrier at an UNSIGNED type-index position, field index right behind it.
example : ((eqTemplateBody wideParams).drop 7).take 7 = [0x20, 0x00, 0xfb, 0x02, 0xc8, 0x01, 0x01]
    := by decide

-- The fixed tail of `eq` still closes five nested blocks and the function.
example : (eqTemplateBody wideParams).drop 159 = [0x20, 0x06, 0x0b, 0x0b, 0x0b, 0x0b, 0x0b] := by
  decide

-- ENCODER SEPARATION at `limb = 100`. `cmp` mentions `limb` at two s33 holes
-- and nowhere else, so its body grows by exactly two; an unsigned splice would
-- have left it at the narrow length, since `uleb32Bytes 100` is one byte.
example : (cmpTemplateBody bandParams).length = 101 + 2 := by decide
example : (cmpTemplateBody bandParams).take 12 =
    [0x05, 0x01, 0x63, 0xe4, 0x00, 0x01, 0x7f, 0x01, 0x63, 0xe4, 0x00, 0x05] := by decide

-- `eq` mentions `limb` at ONE s33 hole (locals) and TWO unsigned `array.get`
-- type-index holes, so it grows by exactly one: the signed hole widened and
-- the two unsigned ones did not. Both facts are pinned positionally below, so
-- swapping either encoder moves a byte the check reads.
example : (eqTemplateBody bandParams).length = 157 + 1 := by decide
example : (eqTemplateBody bandParams).take 5 = [0x02, 0x02, 0x63, 0xe4, 0x00] := by decide
example : ((eqTemplateBody bandParams).drop 116).take 14 =
    [0x20, 0x02, 0x20, 0x04, 0xfb, 0x0b, 0x64,
     0x20, 0x03, 0x20, 0x04, 0xfb, 0x0b, 0x64] := by decide

-- Little-endian byte list -> the big-Nat representation the decoders read, and
-- the minimal wasm framing `bodyBytesAtFuncIndex` consumes (header + code
-- section with one entry).
def natOfBytes (bytes : List Nat) : Nat :=
  bytes.foldr (fun b acc => b + 256 * acc) 0

def frame (body : List Nat) : List Nat :=
  let entry := uleb32Bytes body.length ++ body
  let payload := 0x01 :: entry
  [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x0a] ++
    uleb32Bytes payload.length ++ payload

def cmpModule : List Nat := frame (arithHelperBody .cmp wideParams)
def eqModule : List Nat := frame (arithHelperBody .eq wideParams)

def bandCmpModule : List Nat := frame (arithHelperBody .cmp bandParams)
def bandEqModule : List Nat := frame (arithHelperBody .eq bandParams)

-- The pin path runs at the encoder-separating width too, so the divergence
-- above is not merely a fact about the template function: it is the body the
-- acceptance predicate compares the real code section against.
example : AcceptedArtifact.arithRoleCheck (natOfBytes bandCmpModule) bandCmpModule.length
    .cmp (some 0) bandParams = true := by decide +kernel
example : AcceptedArtifact.arithRoleCheck (natOfBytes bandEqModule) bandEqModule.length
    .eq (some 0) bandParams = true := by decide +kernel

-- The whole pin path runs at these widths: the wide body is ACCEPTED at the
-- index its role is declared at. This is the statement the corpus cannot make.
example : AcceptedArtifact.arithRoleCheck (natOfBytes cmpModule) cmpModule.length
    .cmp (some 0) wideParams = true := by decide +kernel
example : AcceptedArtifact.arithRoleCheck (natOfBytes eqModule) eqModule.length
    .eq (some 0) wideParams = true := by decide +kernel

-- ...and the two roles stay distinguishable at wide indices, where the naive
-- worry is that a longer body blurs them: each module is refused at the other
-- role.
example : AcceptedArtifact.arithRoleCheck (natOfBytes cmpModule) cmpModule.length
    .eq (some 0) wideParams = false := by decide +kernel
example : AcceptedArtifact.arithRoleCheck (natOfBytes eqModule) eqModule.length
    .cmp (some 0) wideParams = false := by decide +kernel

-- ...and neither is accepted under the narrow declaration, which is what a
-- raw-spliced (truncated) synthesis would have produced.
example : AcceptedArtifact.arithRoleCheck (natOfBytes cmpModule) cmpModule.length
    .cmp (some 0) narrowParams = false := by decide +kernel
example : AcceptedArtifact.arithRoleCheck (natOfBytes eqModule) eqModule.length
    .eq (some 0) narrowParams = false := by decide +kernel
"#;
    std::fs::write(wall_dir.join("WideTemplateSplice.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("env")
        .arg("lean")
        .arg("WideTemplateSplice.lean")
        .output()
        .expect("run the wide-index template check");
    assert!(
        check.status.success(),
        "wide-index template check failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
}

/// String-role GuardIso: bytes and every sibling pin stay identical while only
/// the declared String.eq index moves. The decode-once string-role equality
/// rejects it; the host-role pin (a sibling reading the same bytes) still
/// holds of the hostile artifact; and the hostile index also makes the plan
/// that compares strings lower to a call the module does not make.
#[test]
fn inkernel_string_host_roles_guard_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping F5 string-role GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, _wasm) = built_package(
        "tools/certkit/fixtures/stringeq.av",
        &[],
        "cert-inkernel-string-role-guard-iso",
    );
    let manifest: serde_json::Value =
        serde_json::from_slice(&std::fs::read(cert.join("cert-manifest.json")).unwrap()).unwrap();
    let roles = manifest["stringHostRoles"].as_array().unwrap();
    assert_eq!(roles.len(), 1, "stringeq carries one String.eq helper");
    assert_eq!(roles[0]["role"], "stringEq");
    let eq_idx = roles[0]["function_index"].as_u64().unwrap();
    let wrong_eq_idx = eq_idx + 1;
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

def hostileStringRoles : List (Nat × CertDecode.StringHost.Role) :=
  [({wrong_eq_idx}, .eq)]
def hostileArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest :=
      {{ manifest with subject :=
          {{ manifest.subject with stringHostRoles := hostileStringRoles }} }} }}

-- Mutation is manifest-only: the exact module byte fact is unchanged.
example : hostileArtifact.modBytes = Artifact.data.modBytes := rfl
example : hostileArtifact.modLen = Artifact.data.modLen := rfl

-- The sibling pin reading the same bytes still holds of the hostile artifact.
example : AcceptedArtifact.decodedHostRoleTable hostileArtifact := Artifact.roles_ok

-- The string-role equality rejects it: the kernel's decode-once classifier
-- finds the String.eq helper at {eq_idx}, not at {wrong_eq_idx}.
example : ¬ AcceptedArtifact.decodedStringHostRoles hostileArtifact := by
  intro h
  have bad : CertDecode.StringHost.roleTable ArtifactBytes.modBytes ArtifactBytes.modLen =
      some hostileStringRoles := h
  exact absurd bad (by decide +kernel)

-- And the plan comparing strings lowers under the hostile index to a call the
-- module does not make.
example : AcceptedArtifact.plansAccepted hostileArtifact = false := by decide +kernel
"#
    );
    assert_probe_holds(&cert, "StringHostRoleGuardIso.lean", &lean);
}

/// The export accounting binds every obligation's `(export name, function
/// kind, self index)` triple to the byte-decoded export section, so an
/// obligation cannot claim a fabricated self index: the honest artifact passes
/// `exportsAccounted`, and decoupling one obligation's `self` from the index
/// its export name resolves to in the bytes fails it.
#[test]
fn self_index_is_kernel_bound_by_exports_accounted() {
    if !lake_available() {
        eprintln!("skipping self-binding GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, _wasm) = built_package(
        "tools/certkit/fixtures/certprobe2.av",
        &[],
        "cert-self-index-binding",
    );
    let lean = r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

example : AcceptedArtifact.exportsAccounted Artifact.data = true := Artifact.exports_ok

def hostileSelfManifest : Manifest :=
  match manifest.obligations with
  | o :: rest => { manifest with obligations := { o with self := o.self + 1 } :: rest }
  | [] => manifest
def hostileSelfArtifact : AcceptedArtifact.ArtifactData :=
  { Artifact.data with manifest := hostileSelfManifest }
example : AcceptedArtifact.exportsAccounted hostileSelfArtifact = false := by decide +kernel
"#;
    assert_probe_holds(&cert, "SelfBindingGuardIso.lean", lean);
}

/// A minimal but REAL Int runtime: the canonical `__rt_aint_from_i64` helper
/// over a three-field carrier struct, exported under its runtime name, in one
/// explicit rec group. The flag field's storage type is the only parameter —
/// `i32` is the shape `CertDecode.TypeEntry.isCarrier` recognises, and a
/// packed `i8` is a carrier that works exactly as well in a real engine while
/// decoding as no carrier at all. The two assemblies differ in that single
/// byte; the box helper's code entry is byte-identical in both.
fn arith_carrier_fixture(flag_field: &str) -> Vec<u8> {
    let wat = format!(
        r#"
(module
  (rec
    (type $mag (array (mut i64)))
    (type $aint (struct (field i64) (field (ref null $mag)) (field {flag_field}))))
  (func $box (param i64) (result (ref null $aint))
    local.get 0
    ref.null $mag
    i32.const 0
    struct.new $aint)
  (export "__rt_aint_from_i64" (func $box)))
"#
    );
    let bytes = wat::parse_str(&wat).expect("arith carrier fixture assembles");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("arith carrier fixture must be valid wasm");
    bytes
}

/// The two pins that tie a DECLARED Int carrier to the module's type section.
///
/// A module can hold a perfectly good Int carrier that `isCarrier` cannot see
/// (the packed-`i8` flag field below), which makes `carrierState` report the
/// carrierless state while the box helper is exported and carries the
/// canonical template body. Without a type-section pin, `arithParams.carrier`
/// would be confirmed only against helper bodies the wall itself synthesized
/// FROM it.
///
/// (a) the honest module (`i32` flag) is ACCEPTED by `arithTableCheck` and by
///     the type table's `carrierConfirmed`;
/// (b) the packed-`i8` module is REJECTED by both under the same declaration;
/// (c) a literal copy of the live `arithTableCheck` weakened by exactly the
///     `carrierState` conjunct ACCEPTS that same module — every other
///     conjunct, template equality included, holds of it;
/// (d) nor can the hidden module be declared carrierless instead: the type
///     table's carrierless arm matches it, but the role table's carrierless
///     arm needs the box export absent.
#[test]
fn declared_arith_carrier_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping declared-arith-carrier GuardIso test: `lake` not available");
        return;
    }
    let wall_dir = built_wall(
        "cert-arith-carrier-guard-iso",
        &[
            "CertPrelude",
            "CertDecode",
            "SchemaCore",
            "ArithTemplateDerisk",
            "WasmSlice",
            "Wasip2Envelope",
            "TypeTable",
            "AcceptedArtifactCore",
        ],
    );
    let hidden = arith_carrier_fixture("i8");
    let visible = arith_carrier_fixture("i32");
    assert_eq!(
        hidden.len(),
        visible.len(),
        "the two arith carrier fixtures must be the same length"
    );
    let differing: Vec<usize> = hidden
        .iter()
        .zip(visible.iter())
        .enumerate()
        .filter_map(|(i, (a, b))| (a != b).then_some(i))
        .collect();
    assert_eq!(
        differing.len(),
        1,
        "the two arith carrier fixtures must differ in exactly one byte, got {differing:?}"
    );
    assert_eq!((hidden[differing[0]], visible[differing[0]]), (0x78, 0x7f));
    let weak = weakened_arith_table_checks(
        &wall_dir,
        &[("arithTableCheckWithoutCarrierStruct", &[CARRIER_STATE])],
    );

    let lean = format!(
        r#"import AcceptedArtifactCore

open CertPrelude AverCert AverCert.Schema AverCert.AcceptedArtifact
set_option maxRecDepth 300000
noncomputable section

def hiddenBytes : Nat := 0x{hidden_hex}
def hiddenLen : Nat := {hidden_len}
def visibleBytes : Nat := 0x{visible_hex}
def visibleLen : Nat := {visible_len}

-- `$mag` is type 0, the carrier `$aint` type 1, the box function 0.
def params : ArithTemplateDerisk.ArithHostParams :=
  {{ carrier := 1, limb := 0, decompose := 0, normalize := 0, strip := 0, umagCmp := 0 }}
def roles : CertDecode.AddSub.Roles :=
  {{ box := some 0, add := none, mul := none, sub := none, toIndex := none,
     cmp := none, eq := none, divmod := none }}
def carrierTable : AverCert.Schema.TypeTable :=
  {{ carrier := some 1, mag := some 0, str := none, strVec := none, records := [], sums := [],
     options := [], results := [], vecs := [], lists := [], opaques := [], strSegs := [] }}
def carrierlessTable : AverCert.Schema.TypeTable :=
  {{ carrierTable with carrier := none, mag := none }}
def carrierConfirmedIn (n len : Nat) (tt : AverCert.Schema.TypeTable) : Bool :=
  match AverCert.TypeTable.firstRecGroup n len with
  | some grp => AverCert.TypeTable.carrierConfirmed n len grp tt
  | none => false

-- The exploit's links, pinned: the runtime is present in BOTH modules, and
-- the box body is the canonical template in both.
example : CertDecode.AddSub.carrierHelperAbsent hiddenBytes hiddenLen = false := by decide +kernel
example : CertDecode.AddSub.boxIdx hiddenBytes hiddenLen = some 0 := by decide +kernel
example : AcceptedArtifact.arithRoleCheck hiddenBytes hiddenLen .box (some 0) params = true := by
  decide +kernel
-- The single byte decides whether the carrier is visible to the wall at all.
example : CertDecode.carrierState hiddenBytes hiddenLen = some none := by decide +kernel
example : CertDecode.carrierState visibleBytes visibleLen = some (some 1) := by decide +kernel
{weak}
-- (a) The honest module is accepted by both pins.
example : AcceptedArtifact.arithTableCheck visibleBytes visibleLen (some roles) (some params)
    = true := by decide +kernel
example : carrierConfirmedIn visibleBytes visibleLen carrierTable = true := by decide +kernel

-- (b) The packed-`i8` module is rejected by both under the same declaration.
example : AcceptedArtifact.arithTableCheck hiddenBytes hiddenLen (some roles) (some params)
    = false := by decide +kernel
example : carrierConfirmedIn hiddenBytes hiddenLen carrierTable = false := by decide +kernel

-- (c) ATTRIBUTION: delete the `carrierState` conjunct and the same module is
-- accepted; the weakened copy still accepts the honest one.
example : AcceptedArtifact.arithTableCheckWithoutCarrierStruct hiddenBytes hiddenLen
    (some roles) (some params) = true := by decide +kernel
example : AcceptedArtifact.arithTableCheckWithoutCarrierStruct visibleBytes visibleLen
    (some roles) (some params) = true := by decide +kernel

-- (d) Declaring the hidden module carrierless matches its type section, but
-- the role table cannot follow: its box export is present.
example : carrierConfirmedIn hiddenBytes hiddenLen carrierlessTable = true := by decide +kernel
example : AcceptedArtifact.arithTableCheck hiddenBytes hiddenLen none none = false := by
  decide +kernel
"#,
        hidden_hex = hex_le(&hidden),
        hidden_len = hidden.len(),
        visible_hex = hex_le(&visible),
        visible_len = visible.len(),
    );
    assert_probe_holds(&wall_dir, "ArithCarrierGuardIso.lean", &lean);
}

/// The compiled person module's layout, read back out of the artifact: the
/// record struct index (the readMember export's declared parameter) and the
/// flattened type count (where the duplicate-entry tamper lands).
fn person_record_layout(bytes: &[u8]) -> (u32, u32) {
    let mut imported_funcs = 0u32;
    let mut func_types = Vec::new();
    let mut read_member = None;
    let mut funcs: Vec<Option<Vec<wasmparser::ValType>>> = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("person.wasm must parse") {
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
            wasmparser::Payload::TypeSection(reader) => {
                for group in reader {
                    for sub in group.expect("rec group must parse").into_types() {
                        match &sub.composite_type.inner {
                            wasmparser::CompositeInnerType::Func(ft) => {
                                funcs.push(Some(ft.params().to_vec()));
                            }
                            _ => funcs.push(None),
                        }
                    }
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                for type_idx in reader {
                    func_types.push(type_idx.expect("function type index must parse"));
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("export must parse");
                    if export.kind == wasmparser::ExternalKind::Func && export.name == "readMember"
                    {
                        read_member = Some(export.index);
                    }
                }
            }
            _ => {}
        }
    }
    let read_member = read_member.expect("person exports readMember");
    let type_idx = func_types[(read_member - imported_funcs) as usize];
    let params = funcs[type_idx as usize]
        .as_ref()
        .expect("readMember's type is a function type");
    assert_eq!(params.len(), 1, "readMember takes exactly the record");
    let struct_idx = match params[0] {
        wasmparser::ValType::Ref(rt) => match rt.heap_type() {
            wasmparser::HeapType::Concrete(idx) => idx
                .as_module_index()
                .expect("record parameter names a module type index"),
            other => panic!("readMember parameter heap type is concrete, got {other:?}"),
        },
        other => panic!("readMember parameter is a reference, got {other:?}"),
    };
    (struct_idx, funcs.len() as u32)
}

/// Append a singleton rectype duplicating the record's exact field shape at a
/// fresh flattened index, outside the opening rec group.
fn person_dup_tamper(bytes: &[u8], carrier: u32) -> Vec<u8> {
    assert!(carrier < 64, "single-byte s33 heap index expected");
    let mut sections = module_sections(bytes);
    let type_section = sections
        .iter_mut()
        .find(|(id, _)| *id == 1)
        .expect("person.wasm has a type section");
    let payload = &mut type_section.1;
    let mut cursor = 0usize;
    let rectype_count = read_uleb_at(payload, &mut cursor);
    let mut tampered = encode_uleb(rectype_count + 1);
    tampered.extend(&payload[cursor..]);
    tampered.extend([0x5f, 0x02, 0x63, carrier as u8, 0x00, 0x7f, 0x00]);
    *payload = tampered;
    rebuild_module(&sections)
}

/// The declared record layout is bound to the module's own type section:
/// over the real compiled `person.wasm`, the honest type table is confirmed,
/// and each hostile declaration — fields permuted, the record moved to a
/// byte-identical duplicate struct outside the opening rec group, the Int
/// carrier declared at another index — is refused by `typeTableConfirmed`.
/// The duplicate struct appended to the bytes changes nothing for the honest
/// declaration, which pins the record at its real index. (Field mutability
/// and a record struct's subtype form are deliberately NOT pinned: the plan
/// grammar never writes a field and never type-tests a record.)
#[test]
fn record_type_declaration_pin_is_isolated_and_weaken_confirmed() {
    if !lake_available() {
        eprintln!("skipping record-declaration GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, person) = built_package(
        "tools/certkit/fixtures/person.av",
        &[],
        "cert-record-decl-guard-iso",
    );
    let (struct_idx, dup_idx) = person_record_layout(&person);
    let manifest: serde_json::Value =
        serde_json::from_slice(&std::fs::read(cert.join("cert-manifest.json")).unwrap()).unwrap();
    let carrier = manifest["carrier_type_index"]
        .as_u64()
        .expect("person carries the Int carrier") as u32;
    let hostile_dup = person_dup_tamper(&person, carrier);
    wasmparser::Validator::new()
        .validate_all(&hostile_dup)
        .expect("the duplicate-entry module must be valid wasm");
    let honest_types = plans_types_block(&cert, "honestTypes");
    assert!(
        honest_types.contains(&format!("records := [⟨0, {struct_idx}, [.int, .bool]⟩]")),
        "person's declared record changed; refit the probe:\n{honest_types}"
    );
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema AverCert.Grammar
set_option maxRecDepth 300000
noncomputable section

{honest_types}

def dupBytes : Nat := 0x{dup_hex}
def dupLen : Nat := {dup_len}

def confirmed (n len : Nat) (tt : AverCert.Schema.TypeTable) : Bool :=
  AverCert.TypeTable.typeTableConfirmed n len manifest.subject tt manifest.fnPlans

-- The honest declaration, on the real bytes and on the bytes with a
-- duplicate struct appended.
example : confirmed ArtifactBytes.modBytes ArtifactBytes.modLen honestTypes = true := by
  decide +kernel
example : confirmed dupBytes dupLen honestTypes = true := by decide +kernel

-- Fields permuted.
def permutedTypes : AverCert.Schema.TypeTable :=
  {{ honestTypes with records := [⟨0, {struct_idx}, [.bool, .int]⟩] }}
example : confirmed ArtifactBytes.modBytes ArtifactBytes.modLen permutedTypes = false := by
  decide +kernel

-- The record declared at the duplicate struct: same field shape, but outside
-- the opening rec group the pins read.
def dupTypes : AverCert.Schema.TypeTable :=
  {{ honestTypes with records := [⟨0, {dup_idx}, [.int, .bool]⟩] }}
example : confirmed dupBytes dupLen dupTypes = false := by decide +kernel

-- The Int carrier declared at the record's own index.
def movedCarrierTypes : AverCert.Schema.TypeTable :=
  {{ honestTypes with carrier := some {struct_idx} }}
example : confirmed ArtifactBytes.modBytes ArtifactBytes.modLen movedCarrierTypes = false := by
  decide +kernel
"#,
        dup_hex = hex_le(&hostile_dup),
        dup_len = hostile_dup.len(),
    );
    assert_probe_holds(&cert, "RecordDeclGuardIso.lean", &lean);
}

/// The declared function type of every present helper is pinned
/// (`roleTypesPinned`), separately from its body (`arithTableCheck`'s template
/// equality). Pointing the `add` helper at another function type in the
/// FUNCTION section — its body bytes untouched — is invisible to the template
/// pin and refused by the type pin alone.
#[test]
fn role_types_pin_is_isolated_from_the_template_pin() {
    if !lake_available() {
        eprintln!("skipping role-type pin GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, wasm) = built_package(
        "tools/certkit/fixtures/certprobe2.av",
        &[],
        "cert-role-type-pin-guard-iso",
    );
    let roles = manifest_roles(&cert);
    let add_idx = role(&roles, "add").expect("certprobe2 add role");
    let box_idx = role(&roles, "box").expect("certprobe2 box role");
    // Locate the function-section entry of `add` and the type index `box`
    // declares; both are single-byte LEBs in this module.
    let mut imported = 0u32;
    let mut function_section = None;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        match payload.expect("certprobe2 parses") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group parses") {
                        let (_, import) = import.expect("import parses");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                function_section = Some(reader.range());
            }
            _ => {}
        }
    }
    let range = function_section.expect("certprobe2 has a function section");
    let mut cursor = range.start;
    let count = read_uleb_at(&wasm, &mut cursor);
    let entries_start = cursor;
    let mut types = Vec::with_capacity(count);
    for _ in 0..count {
        let at = cursor;
        let type_idx = read_uleb_at(&wasm, &mut cursor);
        assert_eq!(
            cursor - at,
            1,
            "single-byte function-section entries expected"
        );
        types.push(type_idx as u8);
    }
    let add_at = entries_start + (add_idx - imported) as usize;
    let box_type = types[(box_idx - imported) as usize];
    assert_ne!(
        wasm[add_at], box_type,
        "the add and box helpers must declare different types"
    );
    let mut mutant = wasm.clone();
    mutant[add_at] = box_type;
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

def mutBytes : Nat := 0x{mut_hex}
def M := AverCert.TypeTable.mctxOf manifest.subject manifest.types manifest.fnPlans

-- The honest module passes both pins.
example : AcceptedArtifact.roleTypesPinned ArtifactBytes.modBytes ArtifactBytes.modLen M = true := by
  decide +kernel

-- The mutant keeps every helper body, so the template pin still accepts it...
example : AcceptedArtifact.arithTableCheck mutBytes ArtifactBytes.modLen
    manifest.subject.hostRoleTable manifest.subject.arithParams = true := by decide +kernel
-- ...and the declared-type pin alone refuses it.
example : AcceptedArtifact.roleTypesPinned mutBytes ArtifactBytes.modLen M = false := by
  decide +kernel
"#,
        mut_hex = hex_le(&mutant),
    );
    assert_probe_holds(&cert, "RoleTypePinGuardIso.lean", &lean);
}

/// No obligation may be vacuous: a declared type no finite value inhabits
/// would make an obligation's hypothesis unsatisfiable and the obligation true
/// of any code. `declsWellFormed` — a conjunct of `plansAccepted` that reads
/// no byte at all — refuses each vacuity source over the real package's
/// declarations, and accepts a recursive sum that has a base case.
#[test]
fn inhabitation_check_rejects_vacuous_declarations() {
    if !lake_available() {
        eprintln!("skipping inhabitation-check GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, _wasm) = built_package(
        "tools/certkit/fixtures/person.av",
        &[],
        "cert-inhabitation-guard-iso",
    );
    let lean = r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema AverCert.Grammar
set_option maxRecDepth 300000
noncomputable section

def wf (tt : AverCert.Schema.TypeTable) (fns : List FnEntry) : Bool :=
  AverCert.TypeTable.declsWellFormed manifest.subject tt fns

-- The honest declarations are well formed.
example : wf manifest.types manifest.fnPlans = true := by decide +kernel

-- A self-referential newtype `R = [record R]`.
example : wf { manifest.types with records := manifest.types.records ++ [⟨9, 99, [.record 9]⟩] }
    manifest.fnPlans = false := by decide +kernel

-- A record with no finite value `R = [int, record R]`.
example : wf { manifest.types with records := manifest.types.records ++ [⟨9, 99, [.int, .record 9]⟩] }
    manifest.fnPlans = false := by decide +kernel

-- A sum whose only constructor recurses.
example : wf { manifest.types with sums := [⟨8, 90, [(91, [.sum 8])]⟩] }
    manifest.fnPlans = false := by decide +kernel

-- ...and the same sum with a base case is inhabited.
example : wf { manifest.types with sums := [⟨8, 90, [(91, [.sum 8]), (92, [.int])]⟩] }
    manifest.fnPlans = true := by decide +kernel

-- `eqref` in a record field.
example : wf { manifest.types with records := manifest.types.records ++ [⟨9, 99, [.eqref, .int]⟩] }
    manifest.fnPlans = false := by decide +kernel

-- `eqref` in a plan signature.
def eqrefPlans : List FnEntry :=
  manifest.fnPlans.map fun e =>
    { e with plan := { e.plan with sig := ⟨[.eqref], e.plan.sig.ret⟩ } }
example : wf manifest.types eqrefPlans = false := by decide +kernel
"#;
    assert_probe_holds(&cert, "InhabitationGuardIso.lean", lean);
}

/// A constructor struct must be declared FINAL with exactly its sum root as
/// supertype: `ref.test` on a non-final struct would also accept a subtype
/// the program never built, so the lowering's exact tag test would not mean
/// what the plan's `match` means. Flipping the nullary constructor's
/// `sub final` to `sub` (a valid module) is refused by the type table pin.
#[test]
fn ctor_struct_finality_is_pinned() {
    if !lake_available() {
        eprintln!("skipping constructor-finality GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, wasm) = built_package(
        "tools/certkit/fixtures/signalgauge.av",
        &[],
        "cert-ctor-finality-guard-iso",
    );
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    assert!(
        plans.contains("sums := [⟨0, 0, [(1, [.int]), (2, [.int]), (3, [.int]), (4, [])]⟩]"),
        "signalgauge's declared sum changed; refit the probe:\n{plans}"
    );
    // The nullary constructor struct: `sub final (root 0) (struct)`.
    let header = [0x4f, 0x01, 0x00, 0x5f, 0x00];
    let type_range = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .find_map(|payload| match payload.expect("signalgauge parses") {
            wasmparser::Payload::TypeSection(reader) => Some(reader.range()),
            _ => None,
        })
        .expect("signalgauge has a type section");
    let hits: Vec<usize> = wasm[type_range.clone()]
        .windows(header.len())
        .enumerate()
        .filter_map(|(offset, window)| (window == header).then_some(type_range.start + offset))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "exactly one final empty constructor struct expected"
    );
    let mut mutant = wasm.clone();
    mutant[hits[0]] = 0x50;
    wasmparser::Validator::new()
        .validate_all(&mutant)
        .expect("a non-final constructor struct is still a valid module");
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000
noncomputable section

def mutBytes : Nat := 0x{mut_hex}
def confirmed (n len : Nat) : Bool :=
  AverCert.TypeTable.typeTableConfirmed n len manifest.subject manifest.types manifest.fnPlans

example : confirmed ArtifactBytes.modBytes ArtifactBytes.modLen = true := by decide +kernel
example : confirmed mutBytes ArtifactBytes.modLen = false := by decide +kernel
"#,
        mut_hex = hex_le(&mutant),
    );
    assert_probe_holds(&cert, "CtorFinalityGuardIso.lean", &lean);
}

/// The policy axes are derived from the plans (`GrammarTotal.checkTermGroup`),
/// never declared: a call group gets L3 only when every member's recursion
/// descends by the canonical `n - 1` step on its first Int parameter, with
/// the `.mul` totality role exactly when a member multiplies. The honest
/// recursions of `recgen` and a mutual pair pass; an ascending step, a
/// non-literal step and a mutual group with one ascending member fall back
/// to L1.
#[test]
fn termination_check_grants_l3_only_to_descending_groups() {
    if !lake_available() {
        eprintln!("skipping termination-check GuardIso test: `lake` not available");
        return;
    }
    let (_out_dir, cert, _wasm) = built_package(
        "tools/certkit/fixtures/recgen.av",
        &[],
        "cert-termination-guard-iso",
    );
    // The mutual pair's plans, read from its own package, stated as data here.
    let (_mutual_dir, mutual_cert, _mutual_wasm) = built_package(
        "tools/certkit/fixtures/mutual.av",
        &[],
        "cert-termination-guard-iso-mutual",
    );
    let mutual_plans = std::fs::read_to_string(mutual_cert.join("Plans.lean")).unwrap();
    let plan_block = |text: &str, def: &str, rename: &str| -> String {
        let head = format!("def {def} : FnPlan :=");
        let at = text
            .find(&head)
            .unwrap_or_else(|| panic!("Plans.lean has no {def}"));
        let end = text[at..].find("\n\n").unwrap() + at;
        text[at..end].replacen(&format!("def {def} :"), &format!("def {rename} :"), 1)
    };
    let is_even = plan_block(&mutual_plans, "fn1", "isEvenP");
    let is_odd = plan_block(&mutual_plans, "fn2", "isOddP");
    let descent = "(.binOp .sub (.local 0) (.literal (.int 1)))";
    assert!(
        is_odd.contains(&format!("(.tailCall 1 [{descent}])")),
        "the mutual plan shape changed; refit the probe:\n{is_odd}"
    );
    let is_odd_ascending = is_odd
        .replacen("def isOddP :", "def isOddAscending :", 1)
        .replacen(
            &format!("(.tailCall 1 [{descent}])"),
            "(.tailCall 1 [(.binOp .add (.local 0) (.literal (.int 1)))])",
            1,
        );
    let recgen_plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let sum_from = plan_block(&recgen_plans, "fn1", "sumFromP");
    let self_call = format!("(.call (.fn 1) [{descent}])");
    assert!(
        sum_from.contains(&self_call),
        "sumFrom's plan shape changed; refit the probe:\n{sum_from}"
    );
    let ascending = sum_from
        .replacen("def sumFromP :", "def ascendingP :", 1)
        .replacen(
            &self_call,
            "(.call (.fn 1) [(.binOp .add (.local 0) (.literal (.int 1)))])",
            1,
        );
    let step_two = sum_from
        .replacen("def sumFromP :", "def stepTwoP :", 1)
        .replacen(
            &self_call,
            "(.call (.fn 1) [(.binOp .sub (.local 0) (.literal (.int 2)))])",
            1,
        );
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema AverCert.Grammar
set_option maxRecDepth 300000
noncomputable section

{sum_from}

{ascending}

{step_two}

{is_even}

{is_odd}

{is_odd_ascending}

-- The honest recursions: L3 at the add/sub role, and at the mul role for
-- the multiplying `factorial`.
example : checkTermGroup [(1, AverCert.Plans.fn1)] = some .addSub := by decide +kernel
example : checkTermGroup [(4, AverCert.Plans.fn4)] = some .mul := by decide +kernel
example : checkTermGroup [(5, AverCert.Plans.fn5)] = some .addSub := by decide +kernel
example : checkTermGroup [(1, isEvenP), (2, isOddP)] = some .addSub := by decide +kernel

-- An ascending step, a step of two, and a mutual group with one ascending
-- member get no termination claim: the derived policy is the partial one.
example : checkTermGroup [(1, ascendingP)] = none := by decide +kernel
example : checkTermGroup [(1, stepTwoP)] = none := by decide +kernel
example : checkTermGroup [(1, isEvenP), (2, isOddAscending)] = none := by decide +kernel
example : (groupPolicy [(1, ascendingP)]).1 = .simulatesModel := by decide +kernel
example : (groupPolicy [(1, sumFromP)]).1 = .simulatesModelTotally := by decide +kernel
"#
    );
    assert_probe_holds(&cert, "TerminationGuardIso.lean", &lean);
}
