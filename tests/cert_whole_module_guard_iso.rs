#![cfg(feature = "wasm")]

#[path = "support/cert_wall.rs"]
mod cert_wall;

use cert_wall::materialize as materialize_wall;
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
    materialize_wall(&cert);
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
    let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    let to_index = match to_index_idx {
        Some(index) => format!("some {index}"),
        None => "none".to_string(),
    };
    let (box_idx, add_idx, mul_idx, sub_idx) = (
        box_idx.expect("json box role"),
        add_idx.expect("json add role"),
        mul_idx.expect("json mul role"),
        sub_idx.expect("json sub role"),
    );
    let wrong_add_idx = add_idx + 1;
    assert_ne!(wrong_add_idx, add_idx);
    // A hostile `toIndex` declaration: any index other than the one the export
    // section binds. When the module exports no helper at all, declaring one is
    // itself the attack (the fused read's contract slot would become wirable).
    let hostile_to_index = match to_index_idx {
        Some(index) => format!("some {}", index + 1),
        None => "some 0".to_string(),
    };
    // The second toIndex attack, available only when the module really does
    // export the helper: declare the role ABSENT. `arithRoleCheck` is vacuous on
    // `none`, so the template equality accepts this and the export-name equality
    // is the only thing that rejects it. This is the case that says the two pins
    // are not redundant and neither may be dropped for the other.
    let to_index_none_block = match to_index_idx {
        None => String::new(),
        Some(_) => format!(
            r#"
-- Same bytes and claims; the manifest declares `toIndex` ABSENT while the
-- module exports `__aint_to_index`. Left unchecked this is the strongest form
-- of the attack: an unbound role is what `Subject.hostRoles` reports to claim
-- matching, so the producer would be free to decide whether the index-extraction
-- contract exists at all.
def absentToIndexTable : CertDecode.AddSub.Roles :=
  {{ box := some {box_idx}, add := some {add_idx}, mul := some {mul_idx}, sub := some {sub_idx},
     toIndex := none }}
def absentToIndexManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with hostRoleTable := some absentToIndexTable }} }}
def absentToIndexArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := absentToIndexManifest }}

-- Isolation: every sibling decoded fact still accepts it.
example : withoutHostRoleTable absentToIndexArtifact := by
  change AcceptedArtifact.decodedStringHostRoles Artifact.data ∧
    AcceptedArtifact.decodedNonExprClaimFacts Artifact.data
  exact honestDecoded.2

example : ¬ AcceptedArtifact.decodedNonExprFacts absentToIndexArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
      (some absentToIndexTable) Artifact.data.manifest.subject.arithParams = true := h.1
  exact absurd bad (by decide +kernel)

-- Attribution: the copy that KEEPS the toIndex template equality and drops only
-- the export-name equality ACCEPTS the absent declaration. The template pin is
-- blind to this attack by construction — it is vacuous on `none` — so the name
-- pin is load-bearing on its own and must not be replaced by the template one.
def arithTableCheckWithoutToIndexName (n len : Nat)
    (roles? : Option CertDecode.AddSub.Roles)
    (params? : Option ArithTemplateDerisk.ArithHostParams) : Bool :=
  match roles?, params? with
  | none, none => CertDecode.AddSub.carrierHelperAbsent n len
  | some roles, some p =>
      !CertDecode.AddSub.carrierHelperAbsent n len &&
      (roles.box == CertDecode.AddSub.boxIdx n len) &&
      ArithTemplateDerisk.checkArithHostParams p &&
      AcceptedArtifact.arithRoleCheck n len .box roles.box p &&
      AcceptedArtifact.arithRoleCheck n len .toIndex roles.toIndex p &&
      AcceptedArtifact.arithRoleCheck n len .add roles.add p &&
      AcceptedArtifact.arithRoleCheck n len .sub roles.sub p &&
      AcceptedArtifact.arithRoleCheck n len .mul roles.mul p
  | _, _ => false

example : arithTableCheckWithoutToIndexName ArtifactBytes.modBytes ArtifactBytes.modLen
    (some absentToIndexTable) Artifact.data.manifest.subject.arithParams = true := by
  decide +kernel
"#
        ),
    };

    let cert = out_dir.join("cert");
    materialize_wall(&cert);
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
  exact accepted.2.2.2.2.2.2.1

-- Same bytes and claims; only the manifest's add index is hostile.
def hostileRoleTable : CertDecode.AddSub.Roles :=
  {{ box := some {box_idx}, add := some {wrong_add_idx}, mul := some {mul_idx}, sub := some {sub_idx},
     toIndex := {to_index} }}
def hostileManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with hostRoleTable := some hostileRoleTable }} }}
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

-- The full predicate fails exactly at the omitted module-wide template pin:
-- the hostile add index does not carry the canonical add helper body.
example : ¬ AcceptedArtifact.decodedNonExprFacts hostileArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
      (some hostileRoleTable) Artifact.data.manifest.subject.arithParams = true := h.1
  exact absurd bad (by decide +kernel)

-- Same bytes and claims; only the manifest's toIndex index is hostile. The
-- fused vector-read face wires an ABSTRACT contract function at the declared
-- index and never interprets its body, so this index must be byte-bound to the
-- `__aint_to_index` export or the contract could be wired to any function.
def hostileToIndexTable : CertDecode.AddSub.Roles :=
  {{ box := some {box_idx}, add := some {add_idx}, mul := some {mul_idx}, sub := some {sub_idx},
     toIndex := {hostile_to_index} }}
def hostileToIndexManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with hostRoleTable := some hostileToIndexTable }} }}
def hostileToIndexArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := hostileToIndexManifest }}

-- Isolation: every sibling decoded fact still accepts the hostile toIndex.
example : withoutHostRoleTable hostileToIndexArtifact := by
  change AcceptedArtifact.decodedStringHostRoles Artifact.data ∧
    AcceptedArtifact.decodedNonExprClaimFacts Artifact.data
  exact honestDecoded.2

-- The full predicate fails exactly at the omitted export-name binding.
example : ¬ AcceptedArtifact.decodedNonExprFacts hostileToIndexArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
      (some hostileToIndexTable) Artifact.data.manifest.subject.arithParams = true := h.1
  exact absurd bad (by decide +kernel)

-- Attribution, one conjunct deep: a literal copy of `arithTableCheck` with ONLY
-- the two toIndex conjuncts removed ACCEPTS the same hostile table. So the
-- rejection above is caused by those conjuncts alone, not by a sibling check
-- that happens to dislike the hostile manifest for an unrelated reason. Both
-- have to come out: a hostile INDEX is refused by the export-name equality and
-- again by the template equality, since the function at the hostile index does
-- not carry the canonical index-helper body either.
def arithTableCheckWithoutToIndex (n len : Nat)
    (roles? : Option CertDecode.AddSub.Roles)
    (params? : Option ArithTemplateDerisk.ArithHostParams) : Bool :=
  match roles?, params? with
  | none, none => CertDecode.AddSub.carrierHelperAbsent n len
  | some roles, some p =>
      !CertDecode.AddSub.carrierHelperAbsent n len &&
      (roles.box == CertDecode.AddSub.boxIdx n len) &&
      ArithTemplateDerisk.checkArithHostParams p &&
      AcceptedArtifact.arithRoleCheck n len .box roles.box p &&
      AcceptedArtifact.arithRoleCheck n len .add roles.add p &&
      AcceptedArtifact.arithRoleCheck n len .sub roles.sub p &&
      AcceptedArtifact.arithRoleCheck n len .mul roles.mul p
  | _, _ => false

example : arithTableCheckWithoutToIndex ArtifactBytes.modBytes ArtifactBytes.modLen
    (some hostileToIndexTable) Artifact.data.manifest.subject.arithParams = true := by
  decide +kernel
{to_index_none_block}
-- A carriered artifact cannot declare the table absent either: the byte-derived
-- box export is present, so the carrierless `none`/`none` pin never closes.
def absentTableManifest : Manifest :=
  {{ manifest with subject :=
      {{ manifest.subject with hostRoleTable := none, arithParams := none }} }}
def absentTableArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with manifest := absentTableManifest }}
example : ¬ AcceptedArtifact.decodedNonExprFacts absentTableArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
      none none = true := h.1
  exact absurd bad (by decide +kernel)
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

/// A module WITH the Int box helper whose module-wide role scan fails: it
/// contains an unrelated carrier-binop-signature function whose body starts
/// with an instruction encoding outside the certificate decoder's scan
/// vocabulary (`ref.as_non_null`). The strict decode must land in the closed
/// `none` state that satisfies NO manifest declaration — in particular the
/// carrierless `null` this attack claims.
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

fn hex_le(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes.iter().rev() {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

/// Negative control for the round-two attack on the module-wide host-role
/// pin: a CARRIERED module (the Int box helper is exported) engineered so the
/// role scan fails, with a manifest claiming the carrierless `null`, must be
/// REJECTED at the host-role-table pin. The producer refuses to certify such
/// a module at all, and the kernel-side strict decode equals `some v` for no
/// manifest value `v`, so the claimed `null` (and every other declaration)
/// leaves the acceptance pin unprovable.
#[test]
fn a_carriered_module_cannot_claim_the_carrierless_null_table() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping poisoned role-scan pin test: `lake` not available");
        return;
    }
    let poisoned = wat::parse_str(POISONED_ROLE_SCAN_WAT).expect("poisoned WAT compiles");
    let healthy = wat::parse_str(HEALTHY_ROLE_SCAN_WAT).expect("healthy WAT compiles");
    for (name, bytes) in [("poisoned", &poisoned), ("healthy", &healthy)] {
        wasmparser::Validator::new()
            .validate_all(bytes)
            .unwrap_or_else(|error| panic!("{name} module must be valid wasm: {error}"));
    }

    // Producer honesty: the poisoned module is refused at disassembly with a
    // readable reason — no certificate package in the unverifiable state is
    // ever emitted.
    let refusal = aver::codegen::cert::byte_derived_frag_host_role_indices(&poisoned)
        .expect_err("the poisoned module must be refused by the producer");
    assert!(
        refusal.contains("__rt_aint_from_i64") && refusal.contains("role scan"),
        "the refusal must name the helper and the failed role scan, got: {refusal}"
    );

    // Healthy control: the identical module without the unscannable function
    // resolves its box and add roles.
    let (box_idx, add_idx, mul_idx, sub_idx, _to_index_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&healthy)
            .expect("the healthy control must classify");
    let (box_idx, add_idx) = (
        box_idx.expect("healthy box role"),
        add_idx.expect("healthy add role"),
    );
    assert_eq!((mul_idx, sub_idx), (None, None));

    // Kernel side, inside a production package environment: the full
    // acceptance predicate rejects the carrierless null claim exactly at the
    // host-role-table pin, and the literal one-conjunct-weakened copy accepts
    // the same artifact. The Rust-side classifier assertions above remain the
    // differential oracle over these two fixtures.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-poisoned-role-scan-pin");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/certification/add_one.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile add_one fixture for the poisoned role-scan pin test");
    assert!(
        compile.status.success(),
        "add_one compile failed for the poisoned role-scan pin test:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build add_one certificate before the poisoned role-scan pin test");
    assert!(
        build.status.success(),
        "add_one certificate failed before the poisoned role-scan pin test:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

-- A module WITH the Int box helper export whose role scan hits an unrelated
-- carrier-binop-signature function with an instruction encoding outside the
-- decoder's vocabulary. Bytes crafted by the test harness.
def poisonedBytes : Nat := 0x{poisoned_hex}
def poisonedLen : Nat := {poisoned_len}

-- The attack itself: this module claims the carrierless `null`.
def nullClaimManifest : Manifest :=
  {{ manifest with
      obligations := [],
      subject := {{ manifest.subject with
        hostRoleTable := none, arithParams := none, stringHostRoles := [] }} }}
def poisonedArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with
      modBytes := poisonedBytes, modLen := poisonedLen,
      manifest := nullClaimManifest,
      symFragmentClaims := [], stringEqClaims := [], stringConcatClaims := [],
      constructClaims := [], recursionClaims := [], mutualRecursionClaims := [],
      verbatimClaims := [], intDispatchClaims := [], fieldProjectionClaims := [],
      compositionMembers := [], compositionClaims := [] }}

-- Literal one-conjunct-weakened copy: only the role-table equality is absent.
def withoutHostRoleTable (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.decodedStringHostRoles artifact ∧
  AcceptedArtifact.decodedNonExprClaimFacts artifact

-- Every sibling decoded fact accepts the poisoned artifact...
example : withoutHostRoleTable poisonedArtifact := by
  constructor
  · show CertDecode.StringHost.roleTable poisonedBytes poisonedLen = some []
    rfl
  · exact ⟨trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial⟩

-- ...and the full predicate rejects it exactly at the host-role-table pin: the
-- carrierless `none`/`none` declaration demands the box export be byte-provably
-- absent, but this module exports it, so the pin never closes.
example : ¬ AcceptedArtifact.decodedNonExprFacts poisonedArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck poisonedBytes poisonedLen none none = true := h.1
  exact absurd bad (by decide +kernel)
"#,
        poisoned_hex = hex_le(&poisoned),
        poisoned_len = poisoned.len(),
    );
    std::fs::write(cert.join("PoisonedRoleScanPin.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("PoisonedRoleScanPin.lean")
        .output()
        .expect("run the poisoned role-scan pin check");
    assert!(
        check.status.success(),
        "poisoned role-scan pin check failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
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
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping arith LEB GuardIso test: `lake` not available");
        return;
    }
    let wall_dir = temp_dir("cert-arith-leb-guard-iso");
    std::fs::create_dir_all(&wall_dir).unwrap();
    let wall = aver::codegen::cert::wall::resolve(aver::codegen::cert::wall::CURRENT_ID).unwrap();
    for source in wall.sources {
        std::fs::write(wall_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(wall_dir.join("lean-toolchain"), wall.toolchain).unwrap();
    std::fs::write(
        wall_dir.join("lakefile.lean"),
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  \
         roots := #[`CertPrelude, `CertDecode, `SchemaCore, `ArithTemplateDerisk, \
         `PlanCheck, `PlanLower, `PlanBytes, `WasmSlice, `ExprFragmentAccepted, \
         `AcceptedArtifactCore]\n",
    )
    .unwrap();
    let build = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("build")
        .output()
        .expect("build the wall before the arith LEB GuardIso");
    assert!(
        build.status.success(),
        "wall build failed before the arith LEB GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = r#"import AcceptedArtifactCore

open AverCert ArithTemplateDerisk CertPrelude
set_option maxRecDepth 300000

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
    let _ = std::fs::remove_dir_all(wall_dir);
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
    materialize_wall(&cert);
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
  exact accepted.2.2.2.2.2.2.1

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
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) → HostTbl :=
  fun _ _ _ _ _ _ _ => none
example : quoteOrSelfOb.host =
    AcceptedArtifact.stringEqCanonicalHost {eq_idx} := rfl
example : nerfedStringHost ≠
    AcceptedArtifact.stringEqCanonicalHost {eq_idx} := by
  intro h
  have bad := congrFun (congrFun (congrFun (congrFun (congrFun (congrFun (congrFun h
    deadHost) deadHost) deadHost) deadHost) deadConcat) deadHost) {eq_idx}
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
    materialize_wall(&cert);
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
    materialize_wall(&cert);
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

/// The type index the named export's function declares (for anchoring nominal
/// type-gate probes to the byte-derived binding).
fn export_func_type_idx(bytes: &[u8], name: &str) -> u32 {
    let mut imported_funcs = 0u32;
    let mut func_types = Vec::new();
    let mut export_idx = None;
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
            wasmparser::Payload::FunctionSection(reader) => {
                for type_idx in reader {
                    func_types.push(type_idx.expect("function type index must parse"));
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("export must parse");
                    if export.kind == wasmparser::ExternalKind::Func && export.name == name {
                        export_idx = Some(export.index);
                    }
                }
            }
            _ => {}
        }
    }
    let export_idx = export_idx.unwrap_or_else(|| panic!("module exports no function `{name}`"));
    func_types[(export_idx - imported_funcs) as usize]
}

/// Nominal vector-read GuardIso: the fused `Vector.get`-or-default face pins
/// the claimed array type's element storage to the nullable carrier reference
/// (`checkVectorGetTypes` via `exprVectorGetTypesMatch`). Two hand-built
/// modules that differ ONLY in that element storage are indistinguishable to
/// the byte-binding gates (same export map, same code entry), so this gate is
/// the sole discriminator; a literal gate-weakened copy accepts the raw-i64
/// module. The honest surfaces are then confirmed on the real fused-read
/// artifact at its byte-derived binding, and the audited sym-plan encoder is
/// pinned to fail closed when the to-index role or the array binding is
/// absent from the byte-derived tables.
#[test]
fn inkernel_vector_get_nominal_type_guard_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping vector-read nominal GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-vector-get-nominal-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/cell_at.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile cell_at fixture for vector-read nominal GuardIso");
    assert!(
        compile.status.success(),
        "cell_at compile failed for vector-read nominal GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = std::fs::read(out_dir.join("cell_at.wasm")).unwrap();
    let type_idx = export_func_type_idx(&wasm, "cellAt");
    let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx) = (
        box_idx.expect("cell_at box role"),
        add_idx.expect("cell_at add role"),
        mul_idx.expect("cell_at mul role"),
        sub_idx.expect("cell_at sub role"),
        to_index_idx.expect("cell_at to-index role"),
    );

    let cert = out_dir.join("cert");
    // The manifest's fused-read host call carries the claim's carrier and
    // array-type surfaces; cross-check its helper indices against the
    // independent Rust role classifier before probing the gate with them.
    let manifest_text = std::fs::read_to_string(cert.join("Manifest.lean")).unwrap();
    let host_call = manifest_text
        .split("vectorGetOrDefaultHost ")
        .nth(1)
        .expect("manifest must carry the fused vector-read host");
    let leading_number = |s: &str| -> u32 {
        let digits: String = s.chars().take_while(char::is_ascii_digit).collect();
        digits
            .parse()
            .unwrap_or_else(|_| panic!("expected a number at `{}`", &s[..s.len().min(40)]))
    };
    let field = |key: &str| -> u32 {
        leading_number(
            host_call
                .split(key)
                .nth(1)
                .unwrap_or_else(|| panic!("manifest host call lacks `{key}`")),
        )
    };
    let carrier = leading_number(host_call);
    let arr_ty = field("arrTy := ");
    assert_eq!(field("toIndexIdx := "), to_index_idx);
    assert_eq!(field("boxIdx := "), box_idx);
    assert_ne!(arr_ty, carrier, "fixture carrier/array types must differ");

    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build cell_at certificate before vector-read nominal GuardIso");
    assert!(
        build.status.success(),
        "cell_at certificate failed before vector-read nominal GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let name_bytes = "cellAt"
        .bytes()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

def packLE : List Nat → Nat | [] => 0 | b :: bs => b + (packLE bs <<< 8)

-- Minimal modules: header, type section, then a shared func/export/code tail.
-- `f` is func 0 of type 0 with the canonical fused-read signature over
-- carrier 2 and array type 1; only the ARRAY ELEMENT STORAGE differs between
-- the two: `(mut (ref null 2))` (carrier elements) vs `(mut i64)` (raw
-- limbs). NOTE: like the int-dispatch signature probes, these hand-built
-- modules demonstrate guard DISCRIMINATION only (they would fail the
-- wasmparser chokepoint); the fused-read E2E tampers close the end-to-end gap
-- on a real validated artifact.
def hdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]
def carrierElemType : List Nat := [1, 14, 2, 96, 2, 99, 1, 99, 2, 1, 99, 2, 94, 99, 2, 1]
def rawElemType : List Nat := [1, 13, 2, 96, 2, 99, 1, 99, 2, 1, 99, 2, 94, 126, 1]
def tailSecs : List Nat := [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0, 10, 4, 1, 2, 0, 11]
def honestMod : List Nat := hdr ++ carrierElemType ++ tailSecs
def hostileMod : List Nat := hdr ++ rawElemType ++ tailSecs
def nameF : List Nat := [102]

-- The byte-binding gates cannot tell the two modules apart...
example : WasmSlice.funcBindingForExport (packLE honestMod) honestMod.length nameF =
    WasmSlice.funcBindingForExport (packLE hostileMod) hostileMod.length nameF := rfl
example : WasmSlice.exactFuncBindingForExport (packLE honestMod) honestMod.length nameF [2, 0, 11] =
    WasmSlice.exactFuncBindingForExport (packLE hostileMod) hostileMod.length nameF [2, 0, 11] := rfl
-- ...only the nominal vector-read type gate discriminates them...
example : WasmSlice.exprVectorGetTypesMatch (packLE honestMod) honestMod.length 0 2 1 = true := rfl
example : WasmSlice.exprVectorGetTypesMatch (packLE hostileMod) hostileMod.length 0 2 1 = false := rfl
-- ...and the fused-read face routes through exactly that gate: the plan
-- recognizer is blind to the module's type section either way.
def probePlan : ExprFragmentRawPlan := {{ profile := "expr-fragment-v1", params := [.adtRef, .intCarrier], result := .intCarrier, body := ({{ nodes := [{{ id := 0, ty := .intCarrier, kind := .vectorGetOrDefault 1 5 6 (0 : Int) }}], result := 0 }} : FragBlock) }}
example : WasmSlice.exprVectorGetOrDefaultArrTy? probePlan = some 1 := rfl
example : WasmSlice.exprFragmentNominalTypesMatch (packLE honestMod) honestMod.length 0 2 probePlan = true := rfl
example : WasmSlice.exprFragmentNominalTypesMatch (packLE hostileMod) hostileMod.length 0 2 probePlan = false := rfl
-- An array type confused with the carrier itself fail-closes on either module.
example : WasmSlice.exprVectorGetTypesMatch (packLE honestMod) honestMod.length 0 1 1 = false := rfl
-- The literal gate-weakened copy accepts every negative.
def weakVectorTypes (_ _ _ _ _ : Nat) : Bool := true
example : weakVectorTypes (packLE hostileMod) hostileMod.length 0 2 1 = true := rfl
example : weakVectorTypes (packLE honestMod) honestMod.length 0 1 1 = true := rfl

-- On the real artifact the honest claim surfaces pass the gate at the
-- byte-derived binding's type index, and the carrier-confused variant fails.
example : (WasmSlice.funcBindingForExport ArtifactBytes.modBytes ArtifactBytes.modLen
    [{name_bytes}]).map (fun b => b.typeIdx) = some {type_idx} := rfl
example : WasmSlice.exprVectorGetTypesMatch ArtifactBytes.modBytes ArtifactBytes.modLen
    {type_idx} {carrier} {arr_ty} = true := rfl
example : WasmSlice.exprVectorGetTypesMatch ArtifactBytes.modBytes ArtifactBytes.modLen
    {type_idx} {carrier} {carrier} = false := rfl

-- The claim really carries exactly those surfaces...
def honestHostTable : List (HostRole × Nat) :=
  [(.box, {box_idx}), (.add, {add_idx}), (.mul, {mul_idx}), (.sub, {sub_idx}), (.toIndex, {to_index_idx})]
def honestStructTable : List (String × Nat) := [("Vector<Int>", {arr_ty})]
example : Artifact.symFragmentClaims.map (fun c => (c.carrier, c.hostTable, c.structTable)) =
    [({carrier}, honestHostTable, honestStructTable)] := rfl

-- ...the audited encoder binds the fused node through the byte-derived
-- tables...
example : PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
    honestHostTable honestStructTable Plans.cellAtSymPlan = some Plans.cellAtPlan := rfl
-- ...and fail-closes when the to-index role or the array binding is absent.
def noToIndexTable : List (HostRole × Nat) :=
  [(.box, {box_idx}), (.add, {add_idx}), (.mul, {mul_idx}), (.sub, {sub_idx})]
example : PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
    noToIndexTable honestStructTable Plans.cellAtSymPlan = none := rfl
example : PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
    honestHostTable [] Plans.cellAtSymPlan = none := rfl
"#
    );
    std::fs::write(cert.join("VectorGetNominalGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("VectorGetNominalGuardIso.lean")
        .output()
        .expect("run vector-read nominal GuardIso");
    assert!(
        check.status.success(),
        "vector-read nominal GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// One `String.concat` module in four states: the Int-carrier struct present or
/// absent, crossed with the emitted locals prelude reserving a carrier slot or
/// none. The two DIAGONAL states are what the compiler actually emits — a
/// module that touches `Int` carries the struct and the scratch local, a module
/// that never touches `Int` carries neither — and the two OFF-DIAGONAL states
/// are the artifacts a producer would need in order to choose the more
/// convenient byte template for a module it does not fit.
///
/// Everything but the struct's first storage byte and the locals prelude is
/// shared: the same export, the same concatenation, the same data segment, the
/// same function and code sections.
fn string_concat_carrier_fixture(carrier_field: &str, locals: &str) -> Vec<u8> {
    let wat = format!(
        r#"
(module
  (type $str (array (mut i8)))
  (type $parts (array (mut (ref null $str))))
  (type $shape (struct (field {carrier_field}) (field anyref) (field i32)))
  (func $concat (param (ref null $parts)) (result (ref null $str))
    ref.null $str)
  (func $greet (param (ref null $str)) (result (ref null $str))
    {locals}
    i32.const 0
    i32.const 7
    array.new_data $str $hello
    local.get 0
    array.new_fixed $parts 2
    call $concat)
  (data $hello "Hello, ")
  (export "greet" (func $greet)))
"#
    );
    let bytes = wat::parse_str(&wat).expect("string-concat carrier fixture assembles");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("string-concat carrier fixture must be valid wasm");
    bytes
}

/// GuardIso for the `string-concat-v1` locals prelude. The family is the only
/// one that lowers in both carrier states of a module, so it is the only one
/// whose certificate carries a template SELECTOR at all — and a selector the
/// producer could pick freely would let a carriered module present the shorter
/// body, or a carrierless one the longer, whichever its bytes happened to fit.
///
/// Four facts, all against kernel-evaluated modules built inside the fixture:
///
/// (a) the two honest states are ACCEPTED by the real family predicate, so the
///     fixture exercises both templates and the rejections below are not an
///     artefact of the framing;
/// (b) a CARRIERED module presenting the zero-local body with a `carrier :=
///     none` claim is REJECTED, and the mirror — a CARRIERLESS module
///     presenting the one-local body with a `carrier := some 2` claim — is
///     rejected too, so neither direction is open;
/// (c) the literal one-conjunct-weakened copy — `stringConcatPlanAccepted`
///     with the `CertDecode.carrierState` equality deleted and nothing else
///     touched — ACCEPTS both hostile artifacts. This is the attribution: the
///     hostile bodies satisfy every other conjunct, byte binding included,
///     because the byte template each claim selected really is the byte
///     template sitting in that module.
///
/// Point (c) is why the pin cannot be dropped in favour of the byte equality
/// alone: the byte equality checks that the claim describes the module's bytes,
/// never that the module was entitled to those bytes.
#[test]
fn string_concat_carrier_state_guard_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping String.concat carrier-state GuardIso test: `lake` not available");
        return;
    }
    let wall_dir = temp_dir("cert-string-concat-carrier-guard-iso");
    std::fs::create_dir_all(&wall_dir).unwrap();
    let wall = aver::codegen::cert::wall::resolve(aver::codegen::cert::wall::CURRENT_ID).unwrap();
    for source in wall.sources {
        std::fs::write(wall_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(wall_dir.join("lean-toolchain"), wall.toolchain).unwrap();
    std::fs::write(
        wall_dir.join("lakefile.lean"),
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  \
         roots := #[`CertPrelude, `CertDecode, `SchemaCore, `ArithTemplateDerisk, \
         `PlanCheck, `PlanLower, `PlanBytes, `WasmSlice, `ExprFragmentAccepted, \
         `StringSoundness, `AcceptedArtifactCore]\n",
    )
    .unwrap();
    let build = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("build")
        .output()
        .expect("build the wall before the String.concat carrier-state GuardIso");
    assert!(
        build.status.success(),
        "wall build failed before the String.concat carrier-state GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    // The carrier struct is `{i64, anyref, i32}`; the decoy differs in exactly
    // one storage-type byte and is therefore NOT a carrier. The locals prelude
    // is either empty or one nullable reference to that same struct type.
    let carrier_local = "(local (ref null $shape))";
    let honest_carriered = string_concat_carrier_fixture("i64", carrier_local);
    let honest_carrierless = string_concat_carrier_fixture("i32", "");
    let hostile_carriered = string_concat_carrier_fixture("i64", "");
    let hostile_carrierless = string_concat_carrier_fixture("i32", carrier_local);

    let lean = format!(
        r#"import AcceptedArtifactCore
import StringSoundness

open CertPrelude AverCert AverCert.Schema AverCert.AcceptedArtifact
set_option maxRecDepth 300000

-- Four assemblies of ONE module. The struct at type index 2 is the Int carrier
-- shape `{{i64, anyref, i32}}` in the "carriered" pair and a decoy differing in
-- one storage-type byte in the "carrierless" pair. `greet` reserves one
-- nullable reference to that struct in the "oneLocal" pair and no locals at all
-- in the "zeroLocal" pair. Bytes crafted by the test harness.
def honestCarrieredBytes : Nat := 0x{honest_carriered_hex}
def honestCarrieredLen : Nat := {honest_carriered_len}
def honestCarrierlessBytes : Nat := 0x{honest_carrierless_hex}
def honestCarrierlessLen : Nat := {honest_carrierless_len}
def hostileCarrieredBytes : Nat := 0x{hostile_carriered_hex}
def hostileCarrieredLen : Nat := {hostile_carriered_len}
def hostileCarrierlessBytes : Nat := 0x{hostile_carrierless_hex}
def hostileCarrierlessLen : Nat := {hostile_carrierless_len}

-- The byte-derived carrier state of each: the struct SHAPE decides it, and the
-- locals prelude has no say in it whatsoever.
example : CertDecode.carrierState honestCarrieredBytes honestCarrieredLen
    = some (some 2) := by decide +kernel
example : CertDecode.carrierState hostileCarrieredBytes hostileCarrieredLen
    = some (some 2) := by decide +kernel
example : CertDecode.carrierState honestCarrierlessBytes honestCarrierlessLen
    = some none := by decide +kernel
example : CertDecode.carrierState hostileCarrierlessBytes hostileCarrierlessLen
    = some none := by decide +kernel

def greetName : AverCert.WasmSlice.ByteSeq := [103, 114, 101, 101, 116]
def helloBytes : List Nat := [72, 101, 108, 108, 111, 44, 32]

def concatPlan : StringConcatRawPlan :=
  {{ profile := "string-concat-v1",
     prefixes := [({{ dataIdx := 0, bytes := helloBytes }} : StringConcatChunk)],
     suffixes := [] }}

def concatSymPlan : SymRawPlan :=
  {{ profile := "sym-fragment-v1", params := [.string], result := .string,
     body := ({{ nodes := [{{ id := 0, ty := .string, kind := .constStringBytes helloBytes }},
                          {{ id := 1, ty := .string, kind := .param 0 }},
                          {{ id := 2, ty := .string, kind := .prim .stringConcat [0, 1] }}],
                result := 2 }} : SymBlock) }}

def concatBody : List WInstr :=
  [.i32Const (0), .i32Const (7), .arrayNewData 0 helloBytes, .localGet 0,
   .arrayNewFixed 1 2, .call 0]

-- One obligation shape, parameterised by exactly the two fields the carrier
-- state fixes: the declared carrier index and the frame's locals count.
def greetOb (carrierIdx nlocals : Nat) : Obligation :=
  {{ export_ := "greet", policy := .simulatesModel, carrier := carrierIdx,
     code := fun fn => if fn = 1 then some ⟨1, nlocals, concatBody⟩ else none,
     host := stringConcatCanonicalHost 0 0,
     self := 1, Dom := WVal, Cod := WVal,
     domRepr := fun _ v vs => vs = [v],
     codRepr := fun S v w => verbatimRepr S v w,
     model := fun v => StringSoundness.evalStringConcat 0 1 concatPlan v }}

def concatRoles : List (Nat × CertDecode.StringHost.Role) := [(0, .concat)]

-- Literal one-conjunct-weakened copy of `stringConcatPlanAccepted`: the
-- `CertDecode.carrierState` equality is deleted and every other conjunct, in
-- its original order, is kept verbatim.
def stringConcatPlanAcceptedWithoutCarrierState
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Option Nat)
    (resultTy containerTy concatFuncIdx : Nat)
    (stringHostRoles : List (Nat × CertDecode.StringHost.Role))
    (symPlan : SymRawPlan)
    (plan : StringConcatRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier.getD 0 ∧
  stringHostRoles.contains (concatFuncIdx, .concat) = true ∧
  obligation.host = stringConcatCanonicalHost concatFuncIdx resultTy ∧
  ∃ body codeEntry binding,
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringConcatPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringConcatRawPlan plan = true ∧
    AverCert.PlanLower.lowerStringConcatBody
      resultTy containerTy concatFuncIdx plan = some body ∧
    AverCert.PlanBytes.lowerStringConcatCodeEntry
      carrier resultTy containerTy concatFuncIdx plan = some codeEntry ∧
    AverCert.WasmSlice.exactFuncBindingForExport
      modBytes modLen exportNameBytes codeEntry = some binding ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some {{ arity := 1, nlocals := stringConcatNLocals carrier, body := body }}

def oneLocalEntry : AverCert.WasmSlice.ByteSeq :=
  (AverCert.PlanBytes.lowerStringConcatCodeEntry (some 2) 0 1 0 concatPlan).getD []
def zeroLocalEntry : AverCert.WasmSlice.ByteSeq :=
  (AverCert.PlanBytes.lowerStringConcatCodeEntry none 0 1 0 concatPlan).getD []

-- The two templates really are different bytes, so the fixture is not testing a
-- distinction without a difference.
example : oneLocalEntry ≠ zeroLocalEntry := by decide +kernel

def bindingIn (modBytes modLen : Nat) (entry : AverCert.WasmSlice.ByteSeq) :
    AverCert.WasmSlice.FuncBinding :=
  (AverCert.WasmSlice.exactFuncBindingForExport modBytes modLen greetName entry).getD
    ⟨0, 0, []⟩

-- (a) Isolation, honest carriered: carrier struct present, one-local body,
-- `carrier := some 2` claimed. The real predicate accepts.
example : stringConcatPlanAccepted honestCarrieredBytes honestCarrieredLen greetName
    "greet" (some 2) 0 1 0 concatRoles concatSymPlan concatPlan (greetOb 2 1) :=
  ⟨rfl, rfl, rfl, rfl, rfl, concatBody, oneLocalEntry,
   bindingIn honestCarrieredBytes honestCarrieredLen oneLocalEntry,
   rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- (a) Isolation, honest carrierless: no carrier struct, zero-local body,
-- `carrier := none` claimed. The real predicate accepts this too, which is the
-- point of the whole change.
example : stringConcatPlanAccepted honestCarrierlessBytes honestCarrierlessLen greetName
    "greet" none 0 1 0 concatRoles concatSymPlan concatPlan (greetOb 0 0) :=
  ⟨rfl, rfl, rfl, rfl, rfl, concatBody, zeroLocalEntry,
   bindingIn honestCarrierlessBytes honestCarrierlessLen zeroLocalEntry,
   rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- (b) A CARRIERED module claiming the carrierless template is rejected, exactly
-- at the carrier-state decode.
example : ¬ stringConcatPlanAccepted hostileCarrieredBytes hostileCarrieredLen greetName
    "greet" none 0 1 0 concatRoles concatSymPlan concatPlan (greetOb 0 0) := by
  intro h
  have bad : CertDecode.carrierState hostileCarrieredBytes hostileCarrieredLen = some none :=
    h.2.1
  exact absurd bad (by decide +kernel)

-- (b) The mirror: a CARRIERLESS module claiming the carriered template.
example : ¬ stringConcatPlanAccepted hostileCarrierlessBytes hostileCarrierlessLen greetName
    "greet" (some 2) 0 1 0 concatRoles concatSymPlan concatPlan (greetOb 2 1) := by
  intro h
  have bad : CertDecode.carrierState hostileCarrierlessBytes hostileCarrierlessLen
      = some (some 2) := h.2.1
  exact absurd bad (by decide +kernel)

-- (c) ATTRIBUTION. Delete that one conjunct and the SAME hostile artifacts are
-- accepted: every remaining conjunct holds of them, the byte binding included.
example : stringConcatPlanAcceptedWithoutCarrierState hostileCarrieredBytes
    hostileCarrieredLen greetName "greet" none 0 1 0 concatRoles concatSymPlan
    concatPlan (greetOb 0 0) :=
  ⟨rfl, rfl, rfl, rfl, concatBody, zeroLocalEntry,
   bindingIn hostileCarrieredBytes hostileCarrieredLen zeroLocalEntry,
   rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

example : stringConcatPlanAcceptedWithoutCarrierState hostileCarrierlessBytes
    hostileCarrierlessLen greetName "greet" (some 2) 0 1 0 concatRoles concatSymPlan
    concatPlan (greetOb 2 1) :=
  ⟨rfl, rfl, rfl, rfl, concatBody, oneLocalEntry,
   bindingIn hostileCarrierlessBytes hostileCarrierlessLen oneLocalEntry,
   rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

-- The byte equality on its own is blind to the attack, which is why it cannot
-- stand in for the pin: each hostile module really does carry the code entry
-- its claim synthesizes.
example : AverCert.WasmSlice.exactFuncBindingForExport hostileCarrieredBytes
    hostileCarrieredLen greetName zeroLocalEntry ≠ none := by decide +kernel
example : AverCert.WasmSlice.exactFuncBindingForExport hostileCarrierlessBytes
    hostileCarrierlessLen greetName oneLocalEntry ≠ none := by decide +kernel

-- And the honest-template equalities fail on the swapped bodies, so the module
-- pairs really are in the states the fixture claims.
example : AverCert.WasmSlice.exactFuncBindingForExport hostileCarrieredBytes
    hostileCarrieredLen greetName oneLocalEntry = none := by decide +kernel
example : AverCert.WasmSlice.exactFuncBindingForExport hostileCarrierlessBytes
    hostileCarrierlessLen greetName zeroLocalEntry = none := by decide +kernel
"#,
        honest_carriered_hex = hex_le(&honest_carriered),
        honest_carriered_len = honest_carriered.len(),
        honest_carrierless_hex = hex_le(&honest_carrierless),
        honest_carrierless_len = honest_carrierless.len(),
        hostile_carriered_hex = hex_le(&hostile_carriered),
        hostile_carriered_len = hostile_carriered.len(),
        hostile_carrierless_hex = hex_le(&hostile_carrierless),
        hostile_carrierless_len = hostile_carrierless.len(),
    );
    std::fs::write(wall_dir.join("StringConcatCarrierGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("env")
        .arg("lean")
        .arg("StringConcatCarrierGuardIso.lean")
        .output()
        .expect("run the String.concat carrier-state GuardIso check");
    assert!(
        check.status.success(),
        "String.concat carrier-state GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(wall_dir);
}

/// A minimal but REAL Int runtime: the canonical `__rt_aint_from_i64` helper
/// over a three-field carrier struct, exported under its runtime name. The flag
/// field's storage type is the only parameter — `i32` is the shape
/// `CertDecode.TypeEntry.isCarrier` recognises, and a packed `i8` is a carrier
/// that works exactly as well in a real engine while decoding as no carrier at
/// all (`isCarrier` compares the storage tag against `0x7f`, and packed `i8` is
/// `0x78`). The two assemblies differ in that single byte; the box helper's code
/// entry is byte-identical in both.
fn arith_carrier_fixture(flag_field: &str) -> Vec<u8> {
    let wat = format!(
        r#"
(module
  (type $mag (array (mut i64)))
  (type $aint (struct (field i64) (field (ref null $mag)) (field {flag_field})))
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

/// GuardIso for the two conjuncts that keep a DECLARED arith table, and the
/// carrier index every obligation declares, tied to the module's type section.
///
/// The attack this closes: `arithTableCheck` reads the export section
/// (`carrierHelperAbsent`, `boxIdx`, `toIndexIdx`), the code section
/// (`arithRoleCheck`) and a pure bound (`checkArithHostParams`) — and, before
/// this change, nothing else. `arithParams.carrier` was therefore confirmed only
/// against helper bodies the wall itself synthesized FROM it, never against the
/// type section. A module can hold a perfectly good Int carrier that
/// `isCarrier` cannot see (the packed-`i8` flag field below), which makes
/// `carrierState` report the carrierless state while a full arith table is
/// admissible. Pair that with the reserved carrier index the carrierless arm of
/// `decodedCarrierIndex` forces, and an Int-family claim could wire the box role
/// — int-dispatch needs no other — and state its obligation over `CarrierSpec 0`
/// while the values its code builds live at a different struct index.
///
/// Four facts, all against kernel-evaluated modules built inside the fixture:
///
/// (a) the honest module — same bytes, `i32` flag field — is ACCEPTED by both
///     pins, so the fixture exercises the admitting path;
/// (b) the packed-`i8` module is REJECTED by the real `arithTableCheck`, and by
///     the real `decodedObligationFacts` when its obligation declares the index
///     its own values actually live at;
/// (c) the literal one-conjunct-weakened copies — `arithTableCheck` without the
///     `carrierState` equality, and `decodedObligationFacts` without the carrier
///     equality — ACCEPT that same module. Both attributions are acceptances:
///     every other conjunct genuinely holds, template equality included;
/// (d) the exploit's remaining links are pinned as facts, so the chain is
///     legible rather than asserted: the helper export really is present, the
///     box body really does equal the canonical template, and the role a claim
///     would cite really is bound by `hostTableBound`.
#[test]
fn declared_arith_carrier_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping declared-arith-carrier GuardIso test: `lake` not available");
        return;
    }
    let wall_dir = temp_dir("cert-arith-carrier-guard-iso");
    std::fs::create_dir_all(&wall_dir).unwrap();
    let wall = aver::codegen::cert::wall::resolve(aver::codegen::cert::wall::CURRENT_ID).unwrap();
    for source in wall.sources {
        std::fs::write(wall_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(wall_dir.join("lean-toolchain"), wall.toolchain).unwrap();
    // The import closure of what this fixture needs — the sibling GuardIso
    // root list plus `StandardFace` and everything `StandardFace` transitively
    // imports. A `roots` list is NOT auto-extended: a module outside it is not
    // part of the library, so its olean is never built and the import fails,
    // which is why naming `StandardFace` alone is not enough. Nineteen roots
    // instead of the wall's full thirty-six keeps the fresh-temp-dir build
    // proportionate. Only files the wall actually EMBEDS may be named: the
    // staged directory is `wall.sources`, a strict subset of the repository.
    std::fs::write(
        wall_dir.join("lakefile.lean"),
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  \
         roots := #[`CertPrelude, `CertDecode, `SchemaCore, `ArithTemplateDerisk, \
         `PlanCheck, `PlanLower, `PlanBytes, `WasmSlice, `ExprFragmentAccepted, \
         `StringSoundness, `AcceptedArtifactCore, `ConstructVerbatimSoundness, \
         `DeclaredEnvelopeAcceptTransport, `DeclaredIndexEnvelope, `EnvelopeLowering, \
         `FieldProjectionSoundness, `IntDispatchSoundness, `WidenedEnvelope, \
         `StandardFace]\n",
    )
    .unwrap();
    let build = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("build")
        .output()
        .expect("build the wall before the declared-arith-carrier GuardIso");
    assert!(
        build.status.success(),
        "wall build failed before the declared-arith-carrier GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let hidden = arith_carrier_fixture("i8");
    let visible = arith_carrier_fixture("i32");
    // The whole difference between a carrier the wall sees and one it does not.
    // `zip` truncates, so the length equality has to be asserted first or the
    // one-byte claim below would hold vacuously for a shorter prefix — and the
    // "the box helper code entry is byte-identical in both" conclusion rests on
    // exactly this.
    assert_eq!(
        hidden.len(),
        visible.len(),
        "the two arith carrier fixtures must be the same length for the \
         byte-difference count below to mean anything"
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

    let lean = format!(
        r#"import AcceptedArtifactCore
import StandardFace

open CertPrelude AverCert AverCert.Schema AverCert.AcceptedArtifact
set_option maxRecDepth 300000

-- One Int runtime, assembled twice. The carrier struct at type index 1 has a
-- packed `i8` flag field in `hidden` and an `i32` flag field in `visible`; the
-- exported `__rt_aint_from_i64` body is byte-identical in both. Bytes crafted by
-- the test harness.
def hiddenBytes : Nat := 0x{hidden_hex}
def hiddenLen : Nat := {hidden_len}
def visibleBytes : Nat := 0x{visible_hex}
def visibleLen : Nat := {visible_len}

def params : ArithTemplateDerisk.ArithHostParams :=
  {{ carrier := 1, limb := 0, decompose := 0, normalize := 0, strip := 0, umagCmp := 0 }}
def roles : CertDecode.AddSub.Roles :=
  {{ box := some 0, add := none, mul := none, sub := none, toIndex := none }}

-- (d) The exploit's links, pinned rather than asserted.
-- The Int runtime is genuinely present in BOTH modules...
example : CertDecode.AddSub.carrierHelperAbsent hiddenBytes hiddenLen = false := by decide +kernel
example : CertDecode.AddSub.boxIdx hiddenBytes hiddenLen = some 0 := by decide +kernel
example : CertDecode.AddSub.toIndexIdx hiddenBytes hiddenLen = none := by decide +kernel
-- ...and the declared box index really does carry the canonical helper body, so
-- the template equality that is supposed to confirm `params.carrier` is fully
-- satisfied by a module whose type section holds no carrier the wall can see.
example : AcceptedArtifact.arithRoleCheck hiddenBytes hiddenLen .box (some 0) params = true := by
  decide +kernel
example : ArithTemplateDerisk.checkArithHostParams params = true := by decide
-- The role an Int-dispatch claim would cite is bound by the declared table.
example : StandardFace.hostTableBound roles [(HostRole.box, 0)] = true := by decide

-- The single byte decides whether the carrier is visible to the wall at all.
example : CertDecode.carrierState hiddenBytes hiddenLen = some none := by decide +kernel
example : CertDecode.carrierState visibleBytes visibleLen = some (some 1) := by decide +kernel

-- Literal one-conjunct-weakened copy of `arithTableCheck`: the `carrierState`
-- equality is deleted and every other conjunct, in its original order, is kept.
def arithTableCheckWithoutCarrierStruct (n len : Nat)
    (roles? : Option CertDecode.AddSub.Roles)
    (params? : Option ArithTemplateDerisk.ArithHostParams) : Bool :=
  match roles?, params? with
  | none, none => CertDecode.AddSub.carrierHelperAbsent n len
  | some roles, some p =>
      !CertDecode.AddSub.carrierHelperAbsent n len &&
      (roles.box == CertDecode.AddSub.boxIdx n len) &&
      (roles.toIndex == CertDecode.AddSub.toIndexIdx n len) &&
      ArithTemplateDerisk.checkArithHostParams p &&
      AcceptedArtifact.arithRoleCheck n len .box roles.box p &&
      AcceptedArtifact.arithRoleCheck n len .toIndex roles.toIndex p &&
      AcceptedArtifact.arithRoleCheck n len .add roles.add p &&
      AcceptedArtifact.arithRoleCheck n len .sub roles.sub p &&
      AcceptedArtifact.arithRoleCheck n len .mul roles.mul p
  | _, _ => false

-- (a) Isolation: the honest module is accepted by the real check.
example : AcceptedArtifact.arithTableCheck visibleBytes visibleLen (some roles) (some params)
    = true := by decide +kernel

-- (b) The packed-`i8` module is rejected — its type section names no carrier.
example : AcceptedArtifact.arithTableCheck hiddenBytes hiddenLen (some roles) (some params)
    = false := by decide +kernel

-- (c) ATTRIBUTION: delete that one conjunct and the SAME module is accepted.
example : arithTableCheckWithoutCarrierStruct hiddenBytes hiddenLen (some roles) (some params)
    = true := by decide +kernel

-- ...and the weakened copy still accepts the honest module, so the flip is
-- attributable to the deleted conjunct and not to the fixture's framing.
example : arithTableCheckWithoutCarrierStruct visibleBytes visibleLen (some roles) (some params)
    = true := by decide +kernel

/-! ### The second conjunct: the obligation's own carrier index -/

-- The exact code entry both assemblies decode to at function 0. Written as a
-- LITERAL, not as `decodeCode` applied to the module: if the stub's code table
-- were the decoder itself, the surviving conjunct of the weakened copy below
-- would hold as a beta-identity rather than by decoding the artifact, and the
-- two attributions would not be of equal strength.
def boxWCode : WCode := ⟨1, 0, [.localGet 0, .refNull, .i32Const (0), .structNew 1 3]⟩

-- ...and that literal really is what the bytes decode to, in both assemblies.
example : CertDecode.decodeCode hiddenBytes hiddenLen 0 = some boxWCode := by rfl
example : CertDecode.decodeCode visibleBytes visibleLen 0 = some boxWCode := by rfl

def stubOb (carrierIdx : Nat) : Obligation :=
  {{ export_ := "__rt_aint_from_i64", policy := .simulatesModel, carrier := carrierIdx,
     code := fun fn => if fn = 0 then some boxWCode else none,
     host := stringConcatCanonicalHost 0 0,
     self := 0, Dom := WVal, Cod := WVal,
     domRepr := fun _ v vs => vs = [v],
     codRepr := fun S v w => verbatimRepr S v w,
     model := fun v => v }}

/-! ### The scoping: which binding a family is wired to is the whole guarantee

`decodedObligationFacts` (strict) is what NINE families get;
`decodedCarrierFreeObligationFacts` (three-state) is wired to the String.concat
claim list alone. The pair of facts below is the same artifact and the same
obligation under both, so the accept/reject flip is attributable to the scoping
decision and to nothing else. -/

-- (a) Isolation, honest carriered module: the obligation declares the decoded
-- carrier index and the strict predicate accepts.
example : AcceptedArtifact.decodedObligationFacts visibleBytes visibleLen
    (stubOb 1) [0] := ⟨rfl, rfl, trivial⟩

-- ...and any other index is rejected, so the equality is not vacuous.
example : ¬ AcceptedArtifact.decodedObligationFacts visibleBytes visibleLen
    (stubOb 2) [0] := by
  intro h
  have hcar : CertDecode.decodeCarrier visibleBytes visibleLen = some 2 := h.1
  exact absurd hcar (by decide +kernel)

-- (b) THE RESTORED GUARANTEE. In a module with no decodable carrier struct the
-- strict binding admits NO index at all — not the struct index the module's own
-- values live at, and not the reserved `0` either. This is what keeps a
-- carrier-SENSITIVE but role-FREE family out of the carrierless state:
-- `construct-v1`'s named face pins `HEq o.Dom Int` and
-- `HEq o.domRepr (intArgDomRepr env.carrier)` while fixing `host = emptyHost`,
-- so neither the role table nor the arith-table carrier pin constrains it, and
-- this conjunct is the only thing standing between it and an Int-representation
-- face stated over an index the bytes do not license.
example : ¬ AcceptedArtifact.decodedObligationFacts hiddenBytes hiddenLen
    (stubOb 1) [0] := by
  intro h
  have hcar : CertDecode.decodeCarrier hiddenBytes hiddenLen = some 1 := h.1
  exact absurd hcar (by decide +kernel)

example : ¬ AcceptedArtifact.decodedObligationFacts hiddenBytes hiddenLen
    (stubOb 0) [0] := by
  intro h
  have hcar : CertDecode.decodeCarrier hiddenBytes hiddenLen = some 0 := h.1
  exact absurd hcar (by decide +kernel)

-- (c) ATTRIBUTION. The String.concat-scoped binding ACCEPTS that same artifact
-- and that same obligation at the reserved index. Every conjunct it keeps holds
-- of the artifact — the code binding equates the literal table above with the
-- real decode — so the rejection in (b) is attributable to the carrier binding,
-- and admitting it for a family whose face reads the CarrierSpec is exactly the
-- unsoundness the scoping prevents.
example : AcceptedArtifact.decodedCarrierFreeObligationFacts hiddenBytes hiddenLen
    (stubOb 0) [0] := ⟨rfl, rfl, trivial⟩

-- The relaxed binding is still not a free choice: it forces the reserved index
-- and rejects the struct index the module's values actually live at.
example : ¬ AcceptedArtifact.decodedCarrierFreeObligationFacts hiddenBytes hiddenLen
    (stubOb 1) [0] := by
  intro h
  have hcar : (1 : Nat) = 0 := h.1
  exact absurd hcar (by decide)

-- The `none` arm of `decodedCarrierIndex`, which no other example reaches: a
-- module whose type section does not decode admits nothing under EITHER
-- binding, reserved index included. Truncating the module mid-type-section is
-- the cheapest way to reach that state.
def truncatedLen : Nat := 12
example : CertDecode.carrierState hiddenBytes truncatedLen = none := by decide +kernel
example : ¬ AcceptedArtifact.decodedCarrierFreeObligationFacts hiddenBytes truncatedLen
    (stubOb 0) [0] := by
  intro h
  exact h.1
"#,
        hidden_hex = hex_le(&hidden),
        hidden_len = hidden.len(),
        visible_hex = hex_le(&visible),
        visible_len = visible.len(),
    );
    std::fs::write(wall_dir.join("ArithCarrierGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("env")
        .arg("lean")
        .arg("ArithCarrierGuardIso.lean")
        .output()
        .expect("run the declared-arith-carrier GuardIso check");
    assert!(
        check.status.success(),
        "declared-arith-carrier GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(wall_dir);
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

/// GuardIso for the `i32.and` fragment primitive's Boolean operand typing —
/// the one conjunct that keeps the new conjunction primitive sound. `i32.and`
/// over arbitrary raw i32 operands can produce a value outside {0, 1}
/// (`2 and 2 = 2`), and the wall interpreter's `.i32And` clause models the
/// operation on the {0, 1} domain, so declaring `boolI32` for a non-Boolean
/// conjunction would poison every downstream reader of the result AND break
/// the model/wasm agreement at once.
///
/// Attribution is BY ACCEPTANCE at both plan levels, with the weakened
/// checkers built as literal copies of the LIVE wall source text (surgery
/// asserts the strict conjunct exists exactly once before relaxing it):
///   - representation level: `checkExprFragmentRawPlan` rejects a plan whose
///     `i32.and` operands are raw i32 constants, while the copy whose only
///     change is the comparisons' loose `hasI32Ty` admission accepts it;
///   - source level: `checkSymRawPlan` (and therefore the audited encoder,
///     which the acceptance predicate matches on) rejects `bool.and` over two
///     Int operands, while the copy weakened by exactly the `[.bool, .bool]`
///     operand typing accepts it.
/// The honest fixture plan passes the real checkers AND both weakened copies,
/// so each rejection above is attributable to exactly the weakened conjunct.
#[test]
fn i32_and_boolean_operand_typing_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Bool.and GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-bool-and-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/bool_window.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile bool_window fixture for Bool.and GuardIso");
    assert!(
        compile.status.success(),
        "bool_window compile failed for Bool.and GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert = out_dir.join("cert");
    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build bool_window certificate before Bool.and GuardIso");
    assert!(
        build.status.success(),
        "bool_window certificate failed before Bool.and GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    // Literal copies from the LIVE materialized checker source.
    let plan_check = std::fs::read_to_string(cert.join("PlanCheck.lean"))
        .expect("materialized wall has PlanCheck.lean");
    let mut weak_frag = [
        extract_wall_def(&plan_check, "primResultTy?"),
        extract_wall_def(&plan_check, "checkBlockFuel"),
        extract_wall_def(&plan_check, "checkBlock"),
        extract_wall_def(&plan_check, "checkExprFragmentRawPlan"),
    ]
    .join("\n\n");
    let strict_frag = "hasTy nodes a .boolI32 && hasTy nodes b .boolI32";
    assert_eq!(
        weak_frag.matches(strict_frag).count(),
        1,
        "the strict i32.and operand conjunct moved; refit the GuardIso surgery"
    );
    weak_frag = weak_frag.replace(strict_frag, "hasI32Ty nodes a && hasI32Ty nodes b");
    for (from, to) in [
        ("checkExprFragmentRawPlan", "weakCheckExprFragmentRawPlan"),
        ("checkBlockFuel", "weakCheckBlockFuel"),
        ("checkBlock", "weakCheckBlock"),
        ("primResultTy?", "weakPrimResultTy?"),
    ] {
        weak_frag = weak_frag.replace(from, to);
    }

    let mut weak_sym = [
        extract_wall_def(&plan_check, "symPrimResultTy?"),
        extract_wall_def(&plan_check, "checkSymBlockFuel"),
        extract_wall_def(&plan_check, "checkSymBlock"),
        extract_wall_def(&plan_check, "checkSymRawPlan"),
    ]
    .join("\n\n");
    let strict_sym = "if symArgsHaveTys nodes args [.bool, .bool] then some .bool else none";
    assert_eq!(
        weak_sym.matches(strict_sym).count(),
        1,
        "the strict bool.and operand conjunct moved; refit the GuardIso surgery"
    );
    weak_sym = weak_sym.replace(
        strict_sym,
        "if symArgsExist nodes args then some .bool else none",
    );
    for (from, to) in [
        ("symPrimResultTy?", "weakSymPrimResultTy?"),
        ("checkSymRawPlan", "weakCheckSymRawPlan"),
        ("checkSymBlockFuel", "weakCheckSymBlockFuel"),
        ("checkSymBlock", "weakCheckSymBlock"),
    ] {
        weak_sym = weak_sym.replace(from, to);
    }

    // Lean structure-instance fields are indentation-sensitive: a field that
    // starts left of the first field's column ends the field block, so the
    // multi-line hostile literals keep `result` aligned under `nodes`.
    let lean = format!(
        r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema AverCert.PlanCheck
set_option maxRecDepth 300000

namespace BoolAndGuardIso

/-! Hostile representation plan: `i32.and` over two raw i32 constants outside
    {{0, 1}}, declaring a Boolean result (2 and 3 = 2 — not a Boolean, and the
    interpreter's {{0,1}}-domain model would not even agree with wasm here). -/
def hostilePlan : ExprFragmentRawPlan :=
  {{ profile := "expr-fragment-v1", params := [], result := .boolI32,
    body := ({{ nodes :=
                 [{{ id := 0, ty := .rawI32, kind := .constI32 2 }},
                  {{ id := 1, ty := .rawI32, kind := .constI32 3 }},
                  {{ id := 2, ty := .boolI32, kind := .prim .i32And [0, 1] }}],
               result := 2 }} : FragBlock) }}

-- The real checker rejects it...
example : AverCert.PlanCheck.checkExprFragmentRawPlan hostilePlan = false := rfl
-- ...while accepting the honest fixture plan at the same entry point.
example : AverCert.PlanCheck.checkExprFragmentRawPlan AverCert.Plans.inWindowPlan = true := rfl

/-! Literal copy of the live checker, weakened by EXACTLY the i32.and operand
    typing (Boolean operands relaxed to the comparisons' loose i32
    admission). -/
{weak_frag}

-- The weakened copy accepts the hostile plan: the rejection above is
-- attributable to exactly that conjunct...
example : weakCheckExprFragmentRawPlan hostilePlan = true := rfl
-- ...and the weakening is strict: the honest plan still passes it.
example : weakCheckExprFragmentRawPlan AverCert.Plans.inWindowPlan = true := rfl

/-! Hostile source plan: `bool.and` over two Int parameters. -/
def hostileSymPlan : SymRawPlan :=
  {{ profile := "sym-fragment-v1", params := [.int, .int], result := .bool,
    body := ({{ nodes :=
                 [{{ id := 0, ty := .int, kind := .param 0 }},
                  {{ id := 1, ty := .int, kind := .param 1 }},
                  {{ id := 2, ty := .bool, kind := .prim .boolAnd [0, 1] }}],
               result := 2 }} : SymBlock) }}

-- The real source checker rejects it, so the audited encoder — the arm
-- `symFragmentPlanAccepted` matches on before anything else — fail-closes
-- regardless of the tables the claim carries.
example : AverCert.PlanCheck.checkSymRawPlan hostileSymPlan = false := rfl
example : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan [] [] hostileSymPlan = none := rfl
example : AverCert.PlanCheck.checkSymRawPlan AverCert.Plans.inWindowSymPlan = true := rfl

/-! Literal copy of the live source checker, weakened by EXACTLY the
    `[.bool, .bool]` operand typing of `bool.and`. -/
{weak_sym}

example : weakCheckSymRawPlan hostileSymPlan = true := rfl
example : weakCheckSymRawPlan AverCert.Plans.inWindowSymPlan = true := rfl

end BoolAndGuardIso
"#
    );
    std::fs::write(cert.join("BoolAndGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("BoolAndGuardIso.lean")
        .output()
        .expect("run Bool.and GuardIso");
    assert!(
        check.status.success(),
        "Bool.and GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// The canonical tag-dispatch representation plan for a `slotCount`-shaped
/// export (`match Option { Some(_) -> 1; None -> 0 }`): scrutinee struct at
/// type `opt_idx`, box helper at function `box_idx`.
fn slot_count_probe_plan(opt_idx: u32, box_idx: u32) -> aver::codegen::cert::ExprFragmentPlan {
    use aver::codegen::cert::{
        ExprFragmentPlan, FragBlock, FragHostRole, FragNode, FragNodeKind, FragPrim, FragTy,
        FragValueId,
    };
    let arm = |k: i64| FragBlock {
        nodes: vec![
            FragNode {
                id: FragValueId(0),
                ty: FragTy::I64,
                kind: FragNodeKind::ConstI64(k),
            },
            FragNode {
                id: FragValueId(1),
                ty: FragTy::IntCarrier,
                kind: FragNodeKind::HostCall {
                    role: FragHostRole::Box,
                    func_idx: box_idx,
                    args: vec![FragValueId(0)],
                },
            },
        ],
        result: FragValueId(1),
    };
    ExprFragmentPlan {
        params: vec![FragTy::AdtRef],
        result: FragTy::IntCarrier,
        body: FragBlock {
            nodes: vec![
                FragNode {
                    id: FragValueId(0),
                    ty: FragTy::AdtRef,
                    kind: FragNodeKind::Local { index: 0 },
                },
                FragNode {
                    id: FragValueId(1),
                    ty: FragTy::RawI32,
                    kind: FragNodeKind::StructGetUser {
                        ty_idx: opt_idx,
                        field: 0,
                        value: FragValueId(0),
                    },
                },
                FragNode {
                    id: FragValueId(2),
                    ty: FragTy::RawI32,
                    kind: FragNodeKind::ConstI32(1),
                },
                FragNode {
                    id: FragValueId(3),
                    ty: FragTy::BoolI32,
                    kind: FragNodeKind::Prim {
                        op: FragPrim::I32Eq,
                        args: vec![FragValueId(1), FragValueId(2)],
                    },
                },
                FragNode {
                    id: FragValueId(4),
                    ty: FragTy::IntCarrier,
                    kind: FragNodeKind::If {
                        cond: FragValueId(3),
                        then_block: Box::new(arm(1)),
                        else_block: Box::new(arm(0)),
                    },
                },
            ],
            result: FragValueId(4),
        },
    }
}

/// One `slotCount` tag-dispatch module, parameterized by the CLAIMED carrier
/// index its dispatch body cites. Type layout:
///   0 `$fake` — an OPEN two-field struct `{i64, anyref}` (NOT a carrier),
///   1 `$real` — the real three-field carrier `{i64, anyref, i32}`, declared
///     `sub final $fake` so `(ref null 1) <: (ref null 0)` holds nominally,
///   2 `$opt`  — the scrutinee struct `{i32 tag, anyref payload}`,
///   3 the box helper's function type `[i64] -> (ref null 1)` (ALWAYS the
///     real carrier — this is the declared type the acceptance pin probes),
///   4 the dispatch function type `[(ref null 2)] -> (ref null claim)`.
/// Function 0 is the box helper, function 1 the exported `slotCount` whose
/// code entry is EXACTLY the canonical byte lowering for the claimed carrier.
fn tag_dispatch_type_confusion_module(claim_carrier: u32) -> Vec<u8> {
    assert!(claim_carrier < 64, "single-byte s33 heap index expected");
    let plan = slot_count_probe_plan(2, 0);
    let dispatch_entry =
        aver::codegen::cert::lower_expr_fragment_plan_code_entry_bytes(&plan, claim_carrier)
            .expect("probe plan lowers to code-entry bytes");
    let leb = |value: usize| -> Vec<u8> {
        assert!(value < 128, "single-byte section framing expected");
        vec![value as u8]
    };
    let section = |id: u8, payload: Vec<u8>| -> Vec<u8> {
        let mut out = vec![id];
        out.extend(leb(payload.len()));
        out.extend(payload);
        out
    };
    let mut types = vec![0x05];
    // 0 $fake: (sub (struct (field i64) (field anyref)))
    types.extend([0x50, 0x00, 0x5f, 0x02, 0x7e, 0x00, 0x6e, 0x00]);
    // 1 $real: (sub final $fake (struct (field i64) (field anyref) (field i32)))
    types.extend([0x4f, 0x01, 0x00, 0x5f, 0x03, 0x7e, 0x00, 0x6e, 0x00, 0x7f, 0x00]);
    // 2 $opt: (struct (field i32) (field anyref))
    types.extend([0x5f, 0x02, 0x7f, 0x00, 0x6e, 0x00]);
    // 3 box: (func (param i64) (result (ref null $real)))
    types.extend([0x60, 0x01, 0x7e, 0x01, 0x63, 0x01]);
    // 4 dispatch: (func (param (ref null $opt)) (result (ref null claim)))
    types.extend([0x60, 0x01, 0x63, 0x02, 0x01, 0x63, claim_carrier as u8]);
    let funcs = vec![0x02, 0x03, 0x04];
    let mut exports = vec![0x01, 0x09];
    exports.extend(b"slotCount");
    exports.extend([0x00, 0x01]);
    // Box body: local.get 0; ref.null any; i32.const 0; struct.new $real.
    let box_payload = vec![
        0x00, 0x20, 0x00, 0xd0, 0x6e, 0x41, 0x00, 0xfb, 0x00, 0x01, 0x0b,
    ];
    let mut code = vec![0x02];
    code.extend(leb(box_payload.len()));
    code.extend(box_payload);
    code.extend(dispatch_entry);
    let mut module = vec![0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];
    module.extend(section(1, types));
    module.extend(section(3, funcs));
    module.extend(section(7, exports));
    module.extend(section(10, code));
    module
}

/// GuardIso for the host-table declared-function-type pin
/// (`hostTableFuncTypesMatch`), on the LIVE attack arm: tag dispatch. Helper
/// BODIES are pinned by template byte equality elsewhere, but a helper's
/// DECLARED type was free — a box helper declared `[i64] -> (ref null $real)`
/// wasm-validates inside a dispatch body whose claimed carrier is a strict
/// SUPERTYPE `$fake` (nominal width subtyping), while every proof face models
/// the box role as `boxRef claim.carrier` over the claimed index — a fiction.
///
/// Both probe modules are REAL validated wasm (`wasmparser::validate_all`
/// passes, including the `(ref null $real) <: (ref null $fake)` use), so the
/// arm is reachable, not hypothetical. The hostile pair — claim.carrier = the
/// two-field non-carrier supertype, box declared to return the real carrier —
/// satisfies EVERY other acceptance conjunct (the canonical code entry for the
/// fake carrier really sits in the module, the export's own signature really
/// returns `(ref null $fake)`, the scrutinee gate holds), so:
///
///   - the REAL `symFragmentPlanAccepted` REJECTS it, and the rejection is
///     extracted at exactly the `hostTableFuncTypesMatch` conjunct;
///   - the copy weakened by EXACTLY that conjunct — a literal extraction from
///     the LIVE materialized wall source, surgery asserted exactly-once —
///     ACCEPTS the same hostile pair (attribution THROUGH acceptance);
///   - the honest twin (claim.carrier = the real carrier) is ACCEPTED by the
///     real predicate, so the rejection is not a framing artefact.
#[test]
fn host_table_declared_type_pin_is_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping host-table type-pin GuardIso test: `lake` not available");
        return;
    }
    let hostile = tag_dispatch_type_confusion_module(0);
    let honest = tag_dispatch_type_confusion_module(1);
    for (label, bytes) in [("hostile", &hostile), ("honest", &honest)] {
        wasmparser::Validator::new()
            .validate_all(bytes)
            .unwrap_or_else(|error| panic!("{label} probe module must be valid wasm: {error}"));
        assert_eq!(
            export_func_type_idx(bytes, "slotCount"),
            4,
            "{label} probe module binds slotCount to the dispatch type"
        );
    }

    let wall_dir = temp_dir("cert-host-table-type-pin-guard-iso");
    std::fs::create_dir_all(&wall_dir).unwrap();
    let wall = aver::codegen::cert::wall::resolve(aver::codegen::cert::wall::CURRENT_ID).unwrap();
    for source in wall.sources {
        std::fs::write(wall_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(wall_dir.join("lean-toolchain"), wall.toolchain).unwrap();
    std::fs::write(
        wall_dir.join("lakefile.lean"),
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  \
         roots := #[`CertPrelude, `CertDecode, `SchemaCore, `ArithTemplateDerisk, \
         `PlanCheck, `PlanLower, `PlanBytes, `WasmSlice, `ExprFragmentAccepted, \
         `AcceptedArtifactCore]\n",
    )
    .unwrap();
    let build = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("build")
        .output()
        .expect("build the wall before the host-table type-pin GuardIso");
    assert!(
        build.status.success(),
        "wall build failed before the host-table type-pin GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    // Literal weakened copy from the LIVE materialized acceptance source: the
    // host-table declared-type conjunct is deleted and nothing else moves.
    let accepted_core = std::fs::read_to_string(wall_dir.join("AcceptedArtifactCore.lean"))
        .expect("materialized wall has AcceptedArtifactCore.lean");
    let mut weak_def = extract_wall_def(&accepted_core, "symFragmentPlanAccepted");
    let strict_conjunct = "      AverCert.WasmSlice.hostTableFuncTypesMatch\n        \
                           modBytes modLen carrier hostTable = true ∧\n";
    assert_eq!(
        weak_def.matches(strict_conjunct).count(),
        1,
        "the host-table declared-type conjunct moved; refit the GuardIso surgery"
    );
    weak_def = weak_def.replace(strict_conjunct, "");
    weak_def = weak_def.replace("symFragmentPlanAccepted", "weakSymFragmentPlanAccepted");

    let lean = format!(
        r#"import AcceptedArtifactCore

open CertPrelude AverCert AverCert.Schema AverCert.AcceptedArtifact AverCert.PlanCheck
set_option maxRecDepth 300000

-- Two assemblies of ONE module (crafted and wasmparser-validated by the test
-- harness): type 0 is an open two-field NON-carrier struct, type 1 the real
-- three-field carrier declared as its final subtype, and the box helper at
-- function 0 is declared `[i64] -> (ref null 1)` in BOTH. Only the claimed
-- carrier the dispatch body cites differs: 0 (hostile) vs 1 (honest).
def hostileBytes : Nat := 0x{hostile_hex}
def hostileLen : Nat := {hostile_len}
def honestBytes : Nat := 0x{honest_hex}
def honestLen : Nat := {honest_len}

def slotCountName : AverCert.WasmSlice.ByteSeq := [115, 108, 111, 116, 67, 111, 117, 110, 116]
def probeHostTable : List (HostRole × Nat) := [(.box, 0)]
def probeStructTable : List (String × Nat) := [("Option", 2)]

def slotCountSym : SymRawPlan :=
  {{ profile := "sym-fragment-v1",
    params := [.app1 "Option" .int],
    result := .int,
    body :=
      {{ nodes :=
        [ {{ id := 0, ty := .app1 "Option" .int, kind := .param 0 }},
          {{ id := 1, ty := .int,
            kind := .tagMatch "Option" 0 1
              {{ nodes := [{{ id := 0, ty := .int, kind := .constInt 1 }}], result := 0 }}
              {{ nodes := [{{ id := 0, ty := .int, kind := .constInt 0 }}], result := 0 }} }} ],
        result := 1 }} }}

def probePlan : ExprFragmentRawPlan := {{ profile := "expr-fragment-v1", params := [.adtRef], result := .intCarrier, body := ({{ nodes := [{{ id := 0, ty := .adtRef, kind := .local 0 }}, {{ id := 1, ty := .rawI32, kind := .structGetUser 2 0 0 }}, {{ id := 2, ty := .rawI32, kind := .constI32 (1 : Int) }}, {{ id := 3, ty := .boolI32, kind := .prim .i32Eq [1, 2] }}, {{ id := 4, ty := .intCarrier, kind := .ifElse 3 ({{ nodes := [{{ id := 0, ty := .i64, kind := .constI64 (1 : Int) }}, {{ id := 1, ty := .intCarrier, kind := .hostCall .box 0 [0] }}], result := 1 }} : FragBlock) ({{ nodes := [{{ id := 0, ty := .i64, kind := .constI64 (0 : Int) }}, {{ id := 1, ty := .intCarrier, kind := .hostCall .box 0 [0] }}], result := 1 }} : FragBlock) }}], result := 4 }} : FragBlock) }}

example : encodeSymRawPlanToExprFragmentRawPlan probeHostTable probeStructTable slotCountSym
    = some probePlan := rfl

def bodyFor (carrier : Nat) : List WInstr :=
  (AverCert.PlanLower.lowerExprFragmentBody carrier probePlan).getD []
def entryFor (carrier : Nat) : AverCert.WasmSlice.ByteSeq :=
  (AverCert.PlanBytes.lowerExprFragmentCodeEntry carrier probePlan).getD []

-- The two dispatch entries genuinely differ (the scratch local and the `if`
-- block type cite the claimed carrier), so the byte gate really selects the
-- claimed-carrier lowering in each module.
example : entryFor 0 ≠ entryFor 1 := by decide +kernel

def probeOb (carrier : Nat) : Obligation :=
  {{ export_ := "slotCount", policy := .simulatesModel, carrier := carrier,
    code := fun i => if i = 1 then some ⟨1, 1, bodyFor carrier⟩ else none,
    host := fun _ _ _ _ _ _ => fun _ => none,
    self := 1, Dom := Unit, Cod := Int,
    domRepr := fun _ _ _ => True, codRepr := fun _ _ _ => True,
    model := fun _ => 0 }}

-- HONEST control: the claim names the real carrier and the REAL predicate
-- accepts, so the rejection below is not an artefact of the framing.
example : symFragmentPlanAccepted honestBytes honestLen slotCountName "slotCount"
    1 probeHostTable probeStructTable slotCountSym (probeOb 1) :=
  ⟨rfl, rfl, rfl, bodyFor 1, entryFor 1, ⟨1, 4, entryFor 1⟩,
   ⟨⟨rfl, rfl, rfl, rfl⟩, rfl, rfl, rfl, rfl⟩⟩

-- The declared-type pin is the discriminator, in both directions.
example : AverCert.WasmSlice.hostTableFuncTypesMatch honestBytes honestLen
    1 probeHostTable = true := by decide +kernel
example : AverCert.WasmSlice.hostTableFuncTypesMatch hostileBytes hostileLen
    0 probeHostTable = false := by decide +kernel

-- THE HOSTILE PAIR is rejected by the real predicate, at exactly that pin.
example : ¬ symFragmentPlanAccepted hostileBytes hostileLen slotCountName "slotCount"
    0 probeHostTable probeStructTable slotCountSym (probeOb 0) := by
  intro h
  have h' : AverCert.WasmSlice.hostTableFuncTypesMatch hostileBytes hostileLen
        0 probeHostTable = true ∧
      exprFragmentPlanAccepted hostileBytes hostileLen slotCountName "slotCount"
        0 probePlan (probeOb 0) := h
  exact absurd h'.1 (by decide +kernel)

/-! Literal copy of the live acceptance predicate, weakened by EXACTLY the
    host-table declared-function-type conjunct. -/
{weak_def}

-- ATTRIBUTION THROUGH ACCEPTANCE: the weakened copy accepts the SAME hostile
-- pair — every remaining conjunct holds of it, the byte binding included.
example : weakSymFragmentPlanAccepted hostileBytes hostileLen slotCountName "slotCount"
    0 probeHostTable probeStructTable slotCountSym (probeOb 0) :=
  ⟨rfl, rfl, bodyFor 0, entryFor 0, ⟨1, 4, entryFor 0⟩,
   ⟨⟨rfl, rfl, rfl, rfl⟩, rfl, rfl, rfl, rfl⟩⟩

-- The weakening is strict: the honest pair still passes the weakened copy.
example : weakSymFragmentPlanAccepted honestBytes honestLen slotCountName "slotCount"
    1 probeHostTable probeStructTable slotCountSym (probeOb 1) :=
  ⟨rfl, rfl, bodyFor 1, entryFor 1, ⟨1, 4, entryFor 1⟩,
   ⟨⟨rfl, rfl, rfl, rfl⟩, rfl, rfl, rfl, rfl⟩⟩

-- Attribution of the OLDER role-permutation probes is preserved: the add and
-- sub roles fix the SAME canonical declared signature, so this pin is blind to
-- a consistent add/sub table permutation and the host-builder equality remains
-- that vector's sole rejector.
example : ∀ carrier entry,
    AverCert.WasmSlice.checkHostRoleFuncType carrier .add entry
      = AverCert.WasmSlice.checkHostRoleFuncType carrier .sub entry :=
  fun _ _ => rfl
"#,
        hostile_hex = hex_le(&hostile),
        hostile_len = hostile.len(),
        honest_hex = hex_le(&honest),
        honest_len = honest.len(),
    );
    std::fs::write(wall_dir.join("HostTableTypePinGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&wall_dir)
        .arg("env")
        .arg("lean")
        .arg("HostTableTypePinGuardIso.lean")
        .output()
        .expect("run the host-table type-pin GuardIso check");
    assert!(
        check.status.success(),
        "host-table type-pin GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(wall_dir);
}
