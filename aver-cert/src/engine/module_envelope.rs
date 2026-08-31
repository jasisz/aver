use std::collections::{BTreeMap, BTreeSet, VecDeque};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleExportFact {
    pub name: String,
    pub kind: u8,
    pub index: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosureClaimFact {
    pub roots: Vec<u32>,
    pub helpers: Vec<u32>,
    pub admitted: Vec<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleEnvelopeFacts {
    pub exports: Vec<ModuleExportFact>,
    pub capabilities: Vec<(String, String)>,
    pub start: Option<u32>,
    pub closure_fuel: u32,
    pub closure: ClosureClaimFact,
}

impl ModuleEnvelopeFacts {
    pub(crate) fn declared_uncertified(
        &self,
        certified_names: impl IntoIterator<Item = String>,
        declined: &[(String, String)],
    ) -> Vec<(String, String)> {
        let certified = certified_names.into_iter().collect::<BTreeSet<_>>();
        let reasons = declined
            .iter()
            .cloned()
            .collect::<BTreeMap<String, String>>();
        self.exports
            .iter()
            .filter(|export| !certified.contains(&export.name))
            .map(|export| {
                let reason = reasons.get(&export.name).cloned().unwrap_or_else(|| {
                    "module export is outside the claimed certification obligations".to_string()
                });
                (export.name.clone(), reason)
            })
            .collect()
    }
}

fn external_kind_byte(kind: wasmparser::ExternalKind) -> u8 {
    match kind {
        wasmparser::ExternalKind::Func => 0,
        wasmparser::ExternalKind::Table => 1,
        wasmparser::ExternalKind::Memory => 2,
        wasmparser::ExternalKind::Global => 3,
        wasmparser::ExternalKind::Tag => 4,
        wasmparser::ExternalKind::FuncExact => 5,
    }
}

/// Re-derive the whole-module interface and direct-call graph from the exact
/// validated bytes.  Rust renders this summary twice (producer and verifier),
/// but it is not authority: the audited Lean folds independently enumerate the
/// sections and recompute the closure from `ArtifactBytes.modBytes`.
pub fn collect_module_envelope_facts(
    wasm_bytes: &[u8],
    certified: &[(String, u32)],
    artifact_target: &str,
) -> Result<ModuleEnvelopeFacts, String> {
    use wasmparser::{Operator, Parser, Payload, TypeRef};

    wasmparser::Validator::new()
        .validate_all(wasm_bytes)
        .map_err(|e| format!("wasm module failed validation: {e}"))?;

    let mut exports = Vec::new();
    let mut capabilities = Vec::new();
    let mut start = None;
    let mut imported_funcs = 0u32;
    let mut defined_funcs = 0u32;
    let mut calls_by_func = BTreeMap::<u32, Vec<u32>>::new();

    for payload in Parser::new(0).parse_all(wasm_bytes) {
        match payload.map_err(|e| format!("wasm parse: {e}"))? {
            Payload::ImportSection(reader) => {
                for group in reader {
                    let group = group.map_err(|e| format!("import read: {e}"))?;
                    for import in group {
                        let (_, import) = import.map_err(|e| format!("import read: {e}"))?;
                        let pair = (import.module.to_string(), import.name.to_string());
                        if !crate::format::is_capability_import_for_target(
                            artifact_target,
                            import.module,
                            import.name,
                        ) {
                            return Err(format!(
                                "module import `{}.{}` is outside the certificate capability registry for target `{artifact_target}`",
                                import.module, import.name,
                            ));
                        }
                        capabilities.push(pair);
                        if matches!(import.ty, TypeRef::Func(_)) {
                            imported_funcs += 1;
                        }
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                defined_funcs = reader.count();
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.map_err(|e| format!("export read: {e}"))?;
                    exports.push(ModuleExportFact {
                        name: export.name.to_string(),
                        kind: external_kind_byte(export.kind),
                        index: export.index,
                    });
                }
            }
            Payload::StartSection { func, .. } => {
                start = Some(func);
            }
            Payload::CodeSectionEntry(body) => {
                let func_idx = imported_funcs + calls_by_func.len() as u32;
                let mut calls = Vec::new();
                let mut operators = body
                    .get_operators_reader()
                    .map_err(|e| format!("operators read: {e}"))?;
                while !operators.eof() {
                    match operators
                        .read()
                        .map_err(|e| format!("operator read: {e}"))?
                    {
                        Operator::Call { function_index }
                        | Operator::ReturnCall { function_index } => calls.push(function_index),
                        _ => {}
                    }
                }
                calls_by_func.insert(func_idx, calls);
            }
            _ => {}
        }
    }

    if calls_by_func.len() != defined_funcs as usize {
        return Err(format!(
            "function/code section count mismatch: {defined_funcs} declarations, {} bodies",
            calls_by_func.len()
        ));
    }

    let mut roots = certified.iter().map(|(_, idx)| *idx).collect::<Vec<_>>();
    roots.sort_unstable();
    roots.dedup();
    let root_set = roots.iter().copied().collect::<BTreeSet<_>>();
    let mut reached = BTreeSet::new();
    let mut work = VecDeque::from(roots.clone());
    while let Some(func) = work.pop_front() {
        if !reached.insert(func) {
            continue;
        }
        if let Some(callees) = calls_by_func.get(&func) {
            work.extend(callees.iter().copied());
        }
    }
    let admitted = reached.iter().copied().collect::<Vec<_>>();
    let helpers = admitted
        .iter()
        .copied()
        .filter(|idx| !root_set.contains(idx))
        .collect::<Vec<_>>();
    // `closureFold` spends one unit for every worklist pop, including duplicate
    // edges to an already-seen helper. Exact reachable vertices alone are not
    // enough fuel for arithmetic-heavy modules where many roots share helpers.
    let reachable_edges = reached
        .iter()
        .filter_map(|func| calls_by_func.get(func))
        .try_fold(0u32, |count, calls| count.checked_add(calls.len() as u32))
        .ok_or_else(|| "closure edge count overflows u32".to_string())?;
    let closure_fuel = (roots.len() as u32)
        .checked_add(reachable_edges)
        .and_then(|n| n.checked_add(1))
        .ok_or_else(|| "closure fuel overflows u32".to_string())?;

    Ok(ModuleEnvelopeFacts {
        exports,
        capabilities,
        start,
        closure_fuel,
        closure: ClosureClaimFact {
            roots,
            helpers,
            admitted,
        },
    })
}

#[cfg(test)]
mod module_envelope_tests {
    #[test]
    fn audited_kernel_capability_registries_track_rust_import_pairs() {
        for (name, registry) in [
            ("WASM_GC_CAPABILITY_REGISTRY", crate::format::WASM_GC_CAPABILITIES),
            ("WASIP2_CAPABILITY_REGISTRY", crate::format::WASIP2_CAPABILITIES),
        ] {
            let marker = format!("def {name} : List (String × String) := [");
            let body = super::CERT_SCHEMA_CORE
                .split_once(&marker)
                .unwrap_or_else(|| panic!("SchemaCore is missing {name}"))
                .1
                .split_once("\n]")
                .unwrap_or_else(|| panic!("SchemaCore.{name} is not a closed list"))
                .0;
            let lean_pairs = body
                .lines()
                .map(str::trim)
                .filter(|line| line.starts_with("(\""))
                .map(|line| {
                    let row = line.strip_suffix(',').unwrap_or(line);
                    let pair = row
                        .strip_prefix("(\"")
                        .and_then(|line| line.strip_suffix("\")"))
                        .unwrap_or_else(|| panic!("invalid SchemaCore.{name} row `{line}`"));
                    let (module, field) = pair
                        .split_once("\", \"")
                        .unwrap_or_else(|| panic!("invalid SchemaCore.{name} pair `{pair}`"));
                    (
                        module.to_string(),
                        field.to_string(),
                    )
                })
                .collect::<Vec<_>>();
            let rust_pairs = registry
                .iter()
                .map(|(module, field)| ((*module).to_string(), (*field).to_string()))
                .collect::<Vec<_>>();
            assert_eq!(
                lean_pairs, rust_pairs,
                "SchemaCore.{name} must exactly match the Rust registry"
            );
        }
    }

    fn module_with_import(module: &str, field: &str) -> Vec<u8> {
        wat::parse_str(format!(
            "(module (import \"{module}\" \"{field}\" (func)))"
        ))
        .expect("valid imported module")
    }

    #[test]
    fn wasip2_registry_accepts_only_the_exact_target_and_version() {
        let bytes = module_with_import("wasi:cli/stdout@0.2.4", "get-stdout");
        let facts = super::collect_module_envelope_facts(
            &bytes,
            &[],
            crate::format::TARGET_WASIP2,
        )
        .expect("exact wasip2 import is admitted");
        assert_eq!(
            facts.capabilities,
            [("wasi:cli/stdout@0.2.4".into(), "get-stdout".into())]
        );

        let error = super::collect_module_envelope_facts(
            &bytes,
            &[],
            crate::format::TARGET_WASM_GC,
        )
        .expect_err("WASI imports are not part of the wasm-gc registry");
        assert!(error.contains("target `wasm-gc`"));

        for (module, field) in [
            ("wasi:cli/unknown@0.2.4", "get-stdout"),
            ("wasi:cli/stdout@0.2.5", "get-stdout"),
            ("wasi:cli/stdout@0.2.4", "unknown-operation"),
        ] {
            let bytes = module_with_import(module, field);
            let error = super::collect_module_envelope_facts(
                &bytes,
                &[],
                crate::format::TARGET_WASIP2,
            )
            .expect_err("unknown WASI import must fail closed");
            assert!(error.contains("target `wasip2`"));
        }
    }
}
