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
    command.env(
        "AVER_CERT_DATA_CACHE",
        std::env::temp_dir().join("aver-cert-data-store"),
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

    let mut malformed = honest_manifest.clone();
    malformed.as_object_mut().unwrap().remove("hostRoleTable");
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(&wasm_path, &cert, "missing object field `hostRoleTable`");

    let mut malformed = honest_manifest.clone();
    malformed["hostRoleTable"]["extra"] = serde_json::json!(0);
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(
        &wasm_path,
        &cert,
        "must contain exactly fields box, add, mul, sub",
    );
    let mut malformed = honest_manifest.clone();
    malformed.as_object_mut().unwrap().remove("stringHostRoles");
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(&wasm_path, &cert, "missing array field `stringHostRoles`");
    let mut malformed = honest_manifest.clone();
    malformed["stringHostRoles"][0]["extra"] = serde_json::json!(true);
    std::fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&malformed).unwrap(),
    )
    .unwrap();
    assert_manifest_decode_declines(
        &wasm_path,
        &cert,
        "must contain exactly fields function_index, role",
    );
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

/// S3 GuardIso: module bytes and every per-claim decode stay identical, while
/// only the manifest's add role is changed. Full acceptance fails at the
/// module-wide role-table equality; deleting exactly that conjunct accepts the
/// same hostile manifest.
#[test]
fn inkernel_host_role_table_guard_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping S3 host-role GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-inkernel-host-role-guard-iso");
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
        .expect("compile json fixture for S3 GuardIso");
    assert!(
        compile.status.success(),
        "json compile failed for S3 GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = std::fs::read(out_dir.join("json.wasm")).unwrap();
    let (box_idx, add_idx, mul_idx, sub_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    let (box_idx, add_idx, mul_idx, sub_idx) = (
        box_idx.expect("json box role"),
        add_idx.expect("json add role"),
        mul_idx.expect("json mul role"),
        sub_idx.expect("json sub role"),
    );
    let wrong_add_idx = add_idx + 1;
    assert_ne!(wrong_add_idx, add_idx);

    let cert = out_dir.join("cert");
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build json certificate before S3 GuardIso");
    assert!(
        build.status.success(),
        "json certificate failed before S3 GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

def honestDecoded : AcceptedArtifact.decodedNonExprFacts Artifact.data := by
  have accepted : AcceptedArtifact.accepted Artifact.data := Artifact.certificate
  exact accepted.2.2.2.2.1

-- Same bytes and claims; only the manifest's add index is hostile.
def hostileRoleTable : CertDecode.AddSub.Roles :=
  {{ box := some {box_idx}, add := some {wrong_add_idx}, mul := some {mul_idx}, sub := some {sub_idx} }}
def hostileManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with hostRoleTable := hostileRoleTable }} }}
def hostileArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := hostileManifest }}

-- Literal one-conjunct-weakened copy: only the roleTable equality is absent.
def withoutHostRoleTable (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.decodedStringHostRoles artifact ∧
  AcceptedArtifact.decodedNonExprClaimFacts artifact

-- Every sibling decoded fact accepts the hostile manifest with identical bytes.
example : withoutHostRoleTable hostileArtifact := by
  change AcceptedArtifact.decodedStringHostRoles Artifact.data ∧
    AcceptedArtifact.decodedNonExprClaimFacts Artifact.data
  exact honestDecoded.2

-- The full predicate fails exactly at the omitted module-wide equality.
example : ¬ AcceptedArtifact.decodedNonExprFacts hostileArtifact := by
  intro h
  have bad := h.1
  change CertDecode.AddSub.roleTable ArtifactBytes.modBytes ArtifactBytes.modLen =
      some hostileRoleTable at bad
  rw [Artifact.decodedHostRoles] at bad
  have badTable : manifest.subject.hostRoleTable = hostileRoleTable :=
    Option.some.inj bad
  have badAdd := congrArg CertDecode.AddSub.Roles.add badTable
  change some {add_idx} = some {wrong_add_idx} at badAdd
  have distinct : (some {add_idx} : Option Nat) ≠ some {wrong_add_idx} := by decide
  exact distinct badAdd
"#
    );
    std::fs::write(cert.join("HostRoleGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("HostRoleGuardIso.lean")
        .output()
        .expect("run S3 host-role GuardIso");
    assert!(
        check.status.success(),
        "S3 host-role GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// F5 GuardIso: bytes and every sibling binding are identical, while only the
/// claimed String.eq index changes. Full acceptance fails at the decode-once
/// string-role equality and its literal one-conjunct-weakened copy accepts.
#[test]
fn inkernel_string_host_roles_guard_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping F5 string-role GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-inkernel-string-role-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/stringeq.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile stringeq fixture for F5 GuardIso");
    assert!(
        compile.status.success(),
        "stringeq compile failed for F5 GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = std::fs::read(out_dir.join("stringeq.wasm")).unwrap();
    let roles = aver::codegen::cert::byte_derived_string_host_roles(&wasm).unwrap();
    assert_eq!(roles.len(), 1);
    assert_eq!(roles[0].1, aver::codegen::cert::StringHostRole::Eq);
    let eq_idx = roles[0].0;
    let wrong_eq_idx = eq_idx + 1;

    let cert = out_dir.join("cert");
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build stringeq certificate before F5 GuardIso");
    assert!(
        build.status.success(),
        "stringeq certificate failed before F5 GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

def honestDecoded : AcceptedArtifact.decodedNonExprFacts Artifact.data := by
  have accepted : AcceptedArtifact.accepted Artifact.data := Artifact.certificate
  exact accepted.2.2.2.2.1

def hostileStringRoles : List (Nat × CertDecode.StringHost.Role) :=
  [({wrong_eq_idx}, .eq)]
def hostileManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with stringHostRoles := hostileStringRoles }} }}
def hostileArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := hostileManifest }}

def withoutStringHostRoles (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.decodedHostRoleTable artifact ∧
  AcceptedArtifact.decodedNonExprClaimFacts artifact

-- Mutation is manifest-only: the exact module byte fact is unchanged.
example : hostileArtifact.modBytes = Artifact.data.modBytes := rfl
example : hostileArtifact.modLen = Artifact.data.modLen := rfl

-- The retained claim binding covers the complete host builder, not merely the
-- classified index: the honest obligation is canonical and a vacuous host is
-- extensionally distinct at the real String.eq slot.
def deadHost : List WVal → Option WVal := fun _ => none
def deadConcat : Nat → List WVal → Option WVal := fun _ _ => none
def nerfedStringHost :
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) → HostTbl :=
  fun _ _ _ _ _ _ => none
example : quoteOrSelfOb.host =
    AcceptedArtifact.stringEqCanonicalHost {eq_idx} := rfl
example : nerfedStringHost ≠
    AcceptedArtifact.stringEqCanonicalHost {eq_idx} := by
  intro h
  have bad := congrFun (congrFun (congrFun (congrFun (congrFun (congrFun h
    deadHost) deadHost) deadHost) deadHost) deadConcat) {eq_idx}
  simp [nerfedStringHost, AcceptedArtifact.stringEqCanonicalHost] at bad

-- Every sibling decode accepts the hostile manifest.
example : withoutStringHostRoles hostileArtifact := by
  exact ⟨honestDecoded.1, honestDecoded.2.2⟩

-- Full acceptance fails exactly at the omitted string-role equality.
example : ¬ AcceptedArtifact.decodedNonExprFacts hostileArtifact := by
  intro h
  have bad := h.2.1
  change CertDecode.StringHost.roleTable ArtifactBytes.modBytes ArtifactBytes.modLen =
      some hostileStringRoles at bad
  rw [Artifact.decodedStringHostRoles] at bad
  have badRoles : manifest.subject.stringHostRoles = hostileStringRoles :=
    Option.some.inj bad
  change [({eq_idx}, CertDecode.StringHost.Role.eq)] =
      [({wrong_eq_idx}, CertDecode.StringHost.Role.eq)] at badRoles
  have distinct :
      ([({eq_idx}, CertDecode.StringHost.Role.eq)] :
        List (Nat × CertDecode.StringHost.Role)) ≠
      [({wrong_eq_idx}, CertDecode.StringHost.Role.eq)] := by decide
  exact distinct badRoles
"#
    );
    std::fs::write(cert.join("StringHostRoleGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("StringHostRoleGuardIso.lean")
        .output()
        .expect("run F5 string-role GuardIso");
    assert!(
        check.status.success(),
        "F5 string-role GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// S1 GuardIso: keep the mutual module bytes fixed and corrupt only the second
/// arm of `isEven`'s manifest-claimed shared code table. The strong witness
/// fails at `decodeCode bytes 2 = obligation.code 2`; a literal copy with that
/// one equality omitted accepts the same hostile manifest.
#[test]
fn inkernel_code_table_guard_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping S1 decode GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-inkernel-code-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/mutual.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile mutual fixture for S1 GuardIso");
    assert!(
        compile.status.success(),
        "mutual compile failed for S1 GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build mutual certificate before S1 GuardIso");
    assert!(
        build.status.success(),
        "mutual certificate failed before S1 GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 200000

-- Same module bytes; only the manifest obligation's code table is hostile.
def hostileEvenCode : CodeTbl := fun fn =>
  if fn = 2 then none else AverCert.isEvenOb.code fn
def hostileEvenOb : Obligation :=
  { AverCert.isEvenOb with code := hostileEvenCode }
def hostileManifest : Manifest :=
  { AverCert.manifest with
    obligations := hostileEvenOb :: AverCert.manifest.obligations.tail }

def evenObligation (m : Manifest) : Option Obligation :=
  m.obligations.find? (fun o => o.export_ = "isEven")

-- Exact S1 witness for isEven's shared SCC table: carrier plus both members.
def mutualDecodeWitness (m : Manifest) : Prop :=
  match evenObligation m with
  | some obligation =>
      AverCert.AcceptedArtifact.decodedObligationFacts
        AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
        obligation [1, 2]
  | none => False

-- Deliberately weakened copy: the cross-member equality at index 2 is absent.
def mutualDecodeWitnessWithoutCrossCode (m : Manifest) : Prop :=
  match evenObligation m with
  | some obligation =>
      AverCert.AcceptedArtifact.decodedObligationFacts
        AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
        obligation [1]
  | none => False

example : mutualDecodeWitness AverCert.manifest := by
  repeat' constructor

-- All retained byte equalities hold for the hostile manifest.
example : mutualDecodeWitnessWithoutCrossCode hostileManifest := by
  repeat' constructor

-- The omitted equality is the only difference, and it is load-bearing.
example : ¬ mutualDecodeWitness hostileManifest := by
  intro h
  change AverCert.AcceptedArtifact.decodedObligationFacts
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    hostileEvenOb [1, 2] at h
  have bad := h.2.2.1
  change CertDecode.decodeCode AverCert.ArtifactBytes.modBytes
    AverCert.ArtifactBytes.modLen 2 = hostileEvenOb.code 2 at bad
  have honestAtTwo :
      CertDecode.decodeCode AverCert.ArtifactBytes.modBytes
        AverCert.ArtifactBytes.modLen 2 = AverCert.isEvenOb.code 2 := rfl
  have hostileAtTwo : hostileEvenOb.code 2 = none := rfl
  rw [honestAtTwo, hostileAtTwo] at bad
  cases bad
"#;
    std::fs::write(cert.join("DecodeGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("DecodeGuardIso.lean")
        .output()
        .expect("run S1 decode GuardIso");
    assert!(
        check.status.success(),
        "S1 decode GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// F6 self retirement: the checker no longer splices a Rust `self` list into the
/// witness. This is sound because `exportsAccounted` already pins every
/// obligation's `(export name, function kind, self index)` triple into the
/// byte-decoded export section (`WasmSlice.enumExports`). This test confirms the
/// binding is load-bearing: the honest artifact passes `exportsAccounted`, and
/// decoupling one obligation's `self` from the index its export name resolves to
/// in the bytes fails it (so a manifest cannot claim a fabricated self index).
#[test]
fn self_index_is_kernel_bound_by_exports_accounted() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping F6 self-binding GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-self-index-binding");
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
        .expect("compile json fixture for F6 self-binding GuardIso");
    assert!(
        compile.status.success(),
        "json compile failed for F6 self-binding GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build json certificate before F6 self-binding GuardIso");
    assert!(
        build.status.success(),
        "json certificate failed before F6 self-binding GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

-- Honest artifact: every obligation's self index is the byte-derived export
-- index for its name, so the export accounting holds.
example : AcceptedArtifact.exportsAccounted Artifact.data = true := rfl

-- Decouple the first obligation's self index from its export name. The
-- (name, func-kind, self) triple is no longer a member of the byte-decoded
-- export section, so the accounting fails: `self` cannot be fabricated even
-- though the Rust checker no longer pins it with a separate `rfl` splice.
def hostileSelfManifest : Manifest :=
  match manifest.obligations with
  | o :: rest => { manifest with obligations := { o with self := o.self + 1 } :: rest }
  | [] => manifest
def hostileSelfArtifact : AcceptedArtifact.ArtifactData :=
  { Artifact.data with manifest := hostileSelfManifest }
example : AcceptedArtifact.exportsAccounted hostileSelfArtifact = false := rfl
"#;
    std::fs::write(cert.join("SelfBindingGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("SelfBindingGuardIso.lean")
        .output()
        .expect("run F6 self-binding GuardIso");
    assert!(
        check.status.success(),
        "F6 self-binding GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}
