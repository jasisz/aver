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

/// Byte offset of the immediate of the first `call <callee>` inside the body of
/// function `func`, plus the byte value sitting there. The `call` opcode is one
/// byte and every index this test uses is below 128, so the immediate is the
/// single byte right after it.
fn call_immediate_offset(bytes: &[u8], func: u32, callee: u32) -> usize {
    let mut imported_funcs = 0u32;
    let mut code_ordinal = 0u32;
    let mut found = None;
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
                if imported_funcs + code_ordinal == func {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let opcode_offset = operators.original_position();
                        if let wasmparser::Operator::Call { function_index } =
                            operators.read().expect("operator must parse")
                            && function_index == callee
                            && found.is_none()
                        {
                            found = Some(opcode_offset + 1);
                        }
                    }
                }
                code_ordinal += 1;
            }
            _ => {}
        }
    }
    let offset = found.expect("the named function must call the named callee");
    assert_eq!(
        u32::from(bytes[offset]),
        callee,
        "the call immediate must be a single byte"
    );
    offset
}

/// The arithmetic helper template pins the sub-routine CALL TARGETS, not just
/// the surrounding skeleton.
///
/// The wall rebuilds each arith helper body from the manifest's declared
/// `decompose`/`normalize`/`strip`/`umagCmp` indices and compares it to the real
/// code bytes. Repointing one `call` inside the add helper — one byte, still a
/// defined function, module still parses — must break that equality, otherwise
/// the declaration would fix only the shape of the helper and leave the callee
/// free. The isolation half is the honest artifact, which the certificate
/// already proves passes the same check.
///
/// This also anchors the producer-side property the low-index block exists for:
/// the declared sub-routine indices stay single-byte, so the rebuilt template
/// and the emitted body agree byte for byte in the first place.
#[test]
fn arith_template_pins_the_subroutine_call_targets() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping arith call-target GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-arith-call-target-guard-iso");
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
        .expect("compile json fixture for arith call-target GuardIso");
    assert!(
        compile.status.success(),
        "json compile failed for arith call-target GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = std::fs::read(out_dir.join("json.wasm")).unwrap();
    let (_, add_idx, _, _, _) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    let add_idx = add_idx.expect("json add role");
    // The declared sub-routine indices live in `Manifest.lean` — the literal the
    // wall reads when it rebuilds the helper bodies.
    let manifest_lean = std::fs::read_to_string(out_dir.join("cert/Manifest.lean")).unwrap();
    let decompose: u32 = manifest_lean
        .split("decompose := ")
        .nth(1)
        .expect("json manifest declares the decompose sub-routine index")
        .split(|c: char| !c.is_ascii_digit())
        .next()
        .expect("the declared index is a number")
        .parse()
        .expect("the declared index parses");
    assert!(
        decompose < 128,
        "the declared sub-routine index must fit one LEB byte, got {decompose}"
    );
    let call_offset = call_immediate_offset(&wasm, add_idx, decompose);

    let cert = out_dir.join("cert");
    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build json certificate before arith call-target GuardIso");
    assert!(
        build.status.success(),
        "json certificate failed before arith call-target GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let lean = format!(
        r#"import Artifact

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

-- Honest control: the declared table and params match the real bytes.
example : AcceptedArtifact.arithTableCheck ArtifactBytes.modBytes ArtifactBytes.modLen
    Artifact.data.manifest.subject.hostRoleTable
    Artifact.data.manifest.subject.arithParams = true := by decide +kernel

-- Repoint one `call` inside the add helper to the next function index. Nothing
-- else moves: same length, same declaration, same claims.
def repointedCallBytes : Nat := ArtifactBytes.modBytes +
  (1 <<< (8 * {call_offset}))

example : AcceptedArtifact.arithTableCheck repointedCallBytes ArtifactBytes.modLen
    Artifact.data.manifest.subject.hostRoleTable
    Artifact.data.manifest.subject.arithParams = false := by decide +kernel

def repointedCallArtifact : AcceptedArtifact.ArtifactData :=
  {{ Artifact.data with modBytes := repointedCallBytes }}
example : ¬ AcceptedArtifact.decodedHostRoleTable repointedCallArtifact := by
  intro h
  have bad : AcceptedArtifact.arithTableCheck repointedCallBytes ArtifactBytes.modLen
      Artifact.data.manifest.subject.hostRoleTable
      Artifact.data.manifest.subject.arithParams = true := h
  exact absurd bad (by decide +kernel)
"#
    );
    std::fs::write(cert.join("ArithCallTargetGuardIso.lean"), lean).unwrap();
    let check = Command::new("lake")
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("ArithCallTargetGuardIso.lean")
        .output()
        .expect("run arith call-target GuardIso");
    assert!(
        check.status.success(),
        "arith call-target GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}
