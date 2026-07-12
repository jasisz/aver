#![cfg(feature = "wasm")]

use std::path::PathBuf;
use std::process::Command;

fn temp_dir(prefix: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    dir.push(format!("aver-{prefix}-{nanos}"));
    dir
}

fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command
}

fn assert_manifest_decode_declines(wasm: &std::path::Path, cert: &std::path::Path, expected: &str) {
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
    fn read_uleb(bytes: &[u8], cursor: &mut usize) -> usize {
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

    let mut cursor = 8usize;
    while cursor < bytes.len() {
        let id = bytes[cursor];
        cursor += 1;
        let size = read_uleb(bytes, &mut cursor);
        cursor += size;
        if id == 7 {
            return cursor;
        }
    }
    panic!("compiler-produced module has no export section")
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
    let (call_offset, call_target) = call.expect("jsonInt must directly call its box helper");
    assert!(call_target < 127, "GuardIso uses a one-byte call target");
    let local_get_offset = local_get.expect("jsonEntryKey must contain local.get");
    assert_eq!(bytes[local_get_offset], 0x20);
    (call_offset, call_target, local_get_offset)
}

/// Five hostile artifacts leave all sibling conjuncts true, fail exactly their
/// named whole-module guard, and pass the literal one-conjunct-weakened copy.
#[test]
fn whole_module_guards_are_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping whole-module GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-whole-module-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/data/json.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile json fixture for whole-module GuardIso");
    assert!(
        compile.status.success(),
        "json compile failed for whole-module GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build json certificate before whole-module GuardIso");
    assert!(
        build.status.success(),
        "json certificate failed before whole-module GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    // The three new JSON fields are required and their nested objects have an
    // exact shape; malformed candidates decline before any Lean build.
    let manifest_path = cert.join("cert-manifest.json");
    let honest_manifest: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&manifest_path).unwrap()).unwrap();
    let wasm_path = out_dir.join("json.wasm");
    let mut malformed = honest_manifest.clone();
    malformed
        .as_object_mut()
        .unwrap()
        .remove("declaredUncertified");
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(
        &wasm_path,
        &cert,
        "missing array field `declaredUncertified`",
    );

    let mut malformed = honest_manifest.clone();
    malformed["capabilities"][0]["extra"] = serde_json::json!(true);
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(
        &wasm_path,
        &cert,
        "must contain exactly fields module, name",
    );

    let mut malformed = honest_manifest.clone();
    malformed["start"]["function_index"] = serde_json::json!(0);
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(&wasm_path, &cert, "absent start must use null");
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&honest_manifest).unwrap(),
    )
    .unwrap();

    let wasm = std::fs::read(&wasm_path).unwrap();
    let start_insert_offset = section_offset_after_export(&wasm);
    let (call_offset, call_target, local_get_offset) = certified_opcode_offsets(&wasm);
    let capability_offset = wasm
        .windows(b"console_print".len())
        .position(|window| window == b"console_print")
        .expect("json wasm must import aver.console_print");
    assert_eq!(wasm[capability_offset], b'c');
    assert_eq!(wasm[call_offset], call_target as u8);
    let lean = format!(
        r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

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
example : AcceptedArtifact.exportsAccounted missingExportArtifact = false := rfl
example : withoutExports missingExportArtifact := ⟨rfl, rfl, rfl⟩

-- (b) Actual console import is outside the declared capability set.
def unknownCapabilityManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with capabilities := [("aver", "xonsole_print")] }} }}
def unknownCapabilityBytes : Nat := ArtifactBytes.modBytes +
  (21 <<< (8 * {capability_offset}))
def unknownCapabilityArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := unknownCapabilityManifest, modBytes := unknownCapabilityBytes }}
example : CAPABILITY_REGISTRY.contains ("aver", "xonsole_print") = false := rfl
example : AcceptedArtifact.importsWithinCapabilities unknownCapabilityArtifact = false := rfl
example : withoutCapabilities unknownCapabilityArtifact := ⟨rfl, rfl, rfl⟩

-- (c) Insert `start 0` after exports while the manifest declares absent.
def startSectionBytes : Nat :=
  (ArtifactBytes.modBytes &&& ((1 <<< (8 * {start_insert_offset})) - 1)) +
  (0x0108 <<< (8 * {start_insert_offset})) +
  ((ArtifactBytes.modBytes >>> (8 * {start_insert_offset})) <<<
    (8 * ({start_insert_offset} + 3)))
def undeclaredStartArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := startSectionBytes, modLen := ArtifactBytes.modLen + 3 }}
example : AcceptedArtifact.startAccounted undeclaredStartArtifact = false := rfl
example : withoutStart undeclaredStartArtifact := ⟨rfl, rfl, rfl⟩

-- (d) Spike negative control: jsonInt's call leaves the admitted closure.
def escapedCallBytes : Nat := ArtifactBytes.modBytes +
  (1 <<< (8 * {call_offset}))
def escapedCallArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := escapedCallBytes }}
example : AcceptedArtifact.closureIsolation escapedCallArtifact = false := rfl
example : withoutClosure escapedCallArtifact := ⟨rfl, rfl, rfl⟩

-- (e) `local.get` (0x20) -> `global.get` (0x23) in a certified root.
def globalReadBytes : Nat := ArtifactBytes.modBytes +
  (3 <<< (8 * {local_get_offset}))
def globalReadArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := globalReadBytes }}
example : AcceptedArtifact.closureIsolation globalReadArtifact = false := rfl
example : withoutClosure globalReadArtifact := ⟨rfl, rfl, rfl⟩
"#
    );
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run whole-module GuardIso");
    assert!(
        check.status.success(),
        "whole-module GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}
