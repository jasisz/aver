use aver::source::require_module_declaration;
use colored::Colorize;

use super::commands::{DepLowering, load_compile_deps};
use super::shared::{format_type_errors, parse_file, read_file, resolve_module_root};

fn load_capability_target_manifest(
    file: &str,
    module_root_override: Option<&str>,
) -> Result<aver::provider::CapabilityTargetManifest, String> {
    let module_root = resolve_module_root(module_root_override);
    let source = read_file(file)?;
    let mut items = parse_file(&source)?;
    require_module_declaration(&items, file)?;
    let modules = load_compile_deps(&items, &module_root, DepLowering::PRISTINE);
    let result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(aver::ir::TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &modules,
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_list_build: false,
            ..Default::default()
        },
    );
    let typecheck = result
        .typecheck
        .expect("capabilities requested typechecking");
    if !typecheck.errors.is_empty() {
        return Err(format_type_errors(&typecheck.errors));
    }
    let required =
        aver::provider::required_capability_operations(&items, &modules, &typecheck.capabilities);
    aver::provider::CapabilityTargetManifest::build(&typecheck.capabilities, &required)
}

fn capability_target_row_json(row: &aver::provider::CapabilityTargetRow) -> serde_json::Value {
    let status = match &row.status {
        aver::provider::TargetBindingStatus::Provided(provider) => serde_json::json!({
            "kind": "provided",
            "provider": provider.identity,
            "fingerprint": provider.fingerprint,
        }),
        aver::provider::TargetBindingStatus::HostBound { reason } => serde_json::json!({
            "kind": "host-bound",
            "reason": {
                "code": reason.code(),
                "message": reason.description(),
            },
        }),
        aver::provider::TargetBindingStatus::Unsupported { reason } => {
            let mut detail = serde_json::json!({
                "code": reason.code(),
                "message": reason.description(),
            });
            if let aver::provider::UnsupportedReason::WitBoundaryTypeUnsupported(boundary) = reason
            {
                let (position, parameter_index) = match boundary.position {
                    aver::codegen::wasip2::CapabilityWitTypePosition::Parameter(index) => {
                        ("parameter", Some(index))
                    }
                    aver::codegen::wasip2::CapabilityWitTypePosition::Result => ("result", None),
                };
                let object = detail
                    .as_object_mut()
                    .expect("capability reason detail is an object");
                object.insert(
                    "capability".to_string(),
                    serde_json::json!(boundary.capability),
                );
                object.insert(
                    "operation".to_string(),
                    serde_json::json!(boundary.operation),
                );
                object.insert("position".to_string(), serde_json::json!(position));
                if let Some(index) = parameter_index {
                    object.insert("parameterIndex".to_string(), serde_json::json!(index));
                }
                object.insert(
                    "averType".to_string(),
                    serde_json::json!(boundary.aver_type),
                );
            }
            serde_json::json!({
                "kind": "unsupported",
                "reason": detail,
            })
        }
    };
    serde_json::json!({
        "capability": row.capability,
        "target": row.target.as_str(),
        "contractHash": row.contract_hash,
        "modelHash": row.model_hash,
        "declaredOperations": row.declared_operations,
        "requiredOperations": row.required_operations,
        "required": row.is_required(),
        "status": status,
    })
}

pub(super) fn cmd_capabilities(file: &str, module_root_override: Option<&str>, json: bool) {
    let manifest = match load_capability_target_manifest(file, module_root_override) {
        Ok(manifest) => manifest,
        Err(error) => {
            if json {
                println!(
                    "{}",
                    serde_json::json!({
                        "schemaVersion": 1,
                        "kind": "capabilityTargetManifestError",
                        "error": error,
                    })
                );
            } else {
                eprintln!("{}", error.red());
            }
            std::process::exit(1);
        }
    };

    if json {
        let value = serde_json::json!({
            "schemaVersion": 1,
            "kind": "capabilityTargetManifest",
            "program": file,
            "targets": aver::provider::CapabilityTarget::ALL
                .into_iter()
                .map(aver::provider::CapabilityTarget::as_str)
                .collect::<Vec<_>>(),
            "rows": manifest
                .rows()
                .iter()
                .map(capability_target_row_json)
                .collect::<Vec<_>>(),
        });
        println!(
            "{}",
            serde_json::to_string_pretty(&value).expect("capability manifest JSON is serializable")
        );
        return;
    }

    println!("Capability target manifest: {file}");
    if manifest.rows().is_empty() {
        println!("  no capability contracts");
        return;
    }
    for rows in manifest
        .rows()
        .chunks(aver::provider::CapabilityTarget::ALL.len())
    {
        let first = &rows[0];
        println!();
        println!("  {}", first.capability.cyan());
        println!("    contract_hash: {}", first.contract_hash);
        println!("    model_hash: {}", first.model_hash);
        println!(
            "    declared operations: {}",
            first
                .declared_operations
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(", ")
        );
        println!(
            "    required operations: {}",
            if first.required_operations.is_empty() {
                "<none>".to_string()
            } else {
                first
                    .required_operations
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        );
        for row in rows {
            let status = match &row.status {
                aver::provider::TargetBindingStatus::Provided(provider) => {
                    format!("provided by {}@{}", provider.identity, provider.fingerprint)
                }
                aver::provider::TargetBindingStatus::HostBound { reason } => {
                    format!("host-bound[{}] — {}", reason.code(), reason.description())
                }
                aver::provider::TargetBindingStatus::Unsupported { reason } => {
                    format!("unsupported[{}] — {}", reason.code(), reason.description())
                }
            };
            println!("    {:<7} {}", row.target.as_str(), status);
        }
    }
}
