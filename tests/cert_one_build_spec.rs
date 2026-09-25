//! `--certify` adds the certificate package and changes no byte.
//!
//! A certificate is for the module that ships. This suite compiles every
//! certificate corpus program twice, plain and with `--certify`, and requires
//! the two `<name>.wasm` files to be identical. It needs no Lean: `--certify`
//! writes the package without building it.
#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use aver_cmd::aver_command;
use scratch_dir::temp_dir;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// One corpus program: its entry and, for a project, its module root.
struct Entry {
    file: String,
    module_root: Option<&'static str>,
}

/// The certificate corpus the Certification workflow and the coupling ratchet
/// (`tools/cert_ratchet.py`) compile: every certkit fixture, the JSON example
/// and the in-repo projects.
fn corpus(repo_root: &Path) -> Vec<Entry> {
    let mut fixtures: Vec<String> = std::fs::read_dir(repo_root.join("tools/certkit/fixtures"))
        .expect("certkit fixtures directory")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().into_string().ok()?;
            name.ends_with(".av")
                .then(|| format!("tools/certkit/fixtures/{name}"))
        })
        .collect();
    fixtures.sort();
    let mut out: Vec<Entry> = fixtures
        .into_iter()
        .map(|file| Entry {
            file,
            module_root: None,
        })
        .collect();
    out.push(Entry {
        file: "examples/data/json.av".into(),
        module_root: None,
    });
    for project in ["projects/k5_fdiv", "projects/payment_ops"] {
        out.push(Entry {
            file: format!("{project}/main.av"),
            module_root: Some(project),
        });
    }
    out
}

/// Compile `entry` for wasm-gc, with or without `--certify`, and return the
/// emitted module bytes.
fn compile(repo_root: &Path, entry: &Entry, certify: bool) -> Result<Vec<u8>, String> {
    let out_dir = temp_dir(if certify {
        "one-build-cert"
    } else {
        "one-build-plain"
    });
    let mut cmd = aver_command();
    cmd.current_dir(repo_root).arg("compile").arg(&entry.file);
    if let Some(root) = entry.module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.arg("--target").arg("wasm-gc").arg("-o").arg(&out_dir);
    if certify {
        cmd.arg("--certify");
    }
    let output = cmd.output().map_err(|e| format!("spawn aver: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "compile{} {} failed:\n{}{}",
            if certify { " --certify" } else { "" },
            entry.file,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let stem = Path::new(&entry.file)
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("entry has a UTF-8 stem");
    std::fs::read(out_dir.join(format!("{stem}.wasm")))
        .map_err(|e| format!("{}: read {stem}.wasm: {e}", entry.file))
}

#[test]
fn certify_compiles_byte_identical_modules_on_the_corpus() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let work = Mutex::new(corpus(&repo_root));
    let failures = Mutex::new(Vec::<String>::new());
    let workers = std::thread::available_parallelism().map_or(2, |n| n.get().min(4));
    std::thread::scope(|scope| {
        for _ in 0..workers {
            scope.spawn(|| {
                loop {
                    // Take the entry and release the lock before compiling.
                    let next = work.lock().unwrap().pop();
                    let Some(entry) = next else { break };
                    let verdict = match (
                        compile(&repo_root, &entry, false),
                        compile(&repo_root, &entry, true),
                    ) {
                        (Ok(plain), Ok(certified)) if plain == certified => continue,
                        (Ok(plain), Ok(certified)) => format!(
                            "{}: --certify changed the module ({} bytes plain, {} certified)",
                            entry.file,
                            plain.len(),
                            certified.len()
                        ),
                        (Err(e), _) | (_, Err(e)) => e,
                    };
                    failures.lock().unwrap().push(verdict);
                }
            });
        }
    });
    let failures = failures.into_inner().unwrap();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// Every function the plan printer prints in its unfused form, by the name
/// the printer reports, paired with the verdict `cert_shape::printer_may_admit`
/// gives on its source form. Mirrors the CLI's wasm-gc compile with every
/// fabricating pass off, so the printer sees exactly the form the kept set
/// protects.
fn printer_verdicts(repo_root: &Path, entry: &Entry) -> Result<Vec<(String, bool)>, String> {
    use aver::codegen::wasm_gc;
    use aver::ir::{PipelineConfig, TypecheckMode};
    use std::collections::HashMap;

    let path = repo_root.join(&entry.file);
    let source = std::fs::read_to_string(&path).map_err(|e| format!("read: {e}"))?;
    let mut items = aver::source::parse_source(&source)?;
    let module_root = repo_root
        .join(entry.module_root.unwrap_or("."))
        .to_string_lossy()
        .into_owned();

    // The predicate on the source form, keyed by the name the printer uses:
    // entry functions keep theirs, dependency functions are flattened to
    // `<Module_Path>_<fn>`.
    let mut may_admit: HashMap<String, bool> = items
        .iter()
        .filter_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) => {
                Some((fd.name.clone(), aver::ir::cert_shape::printer_may_admit(fd)))
            }
            _ => None,
        })
        .collect();

    let deps = aver::source::load_compile_deps(&items, &module_root)?;
    for module in &deps.modules {
        for fd in &module.fn_defs {
            may_admit.insert(
                format!("{}_{}", module.prefix.replace('.', "_"), fd.name),
                aver::ir::cert_shape::printer_may_admit(fd),
            );
        }
    }
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithCheckedLoaded(&deps.loaded)),
            marked: deps.marked.clone(),
            alloc_policy: Some(&neutral_policy),
            dep_modules: &deps.modules,
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_string_index: true,
            run_list_build: false,
            run_byte_sink: false,
            ..Default::default()
        },
    );
    let typecheck = result.typecheck.as_ref().ok_or("no typecheck")?;
    if !typecheck.errors.is_empty() {
        return Err(format!("type errors: {:?}", typecheck.errors));
    }
    let capabilities = &typecheck.capabilities;
    let required =
        aver::provider::required_capability_operations(&items, &deps.modules, capabilities);
    let plan = wasm_gc::CapabilityWasmGcPlan::build(capabilities, &required)?;
    let aliases = wasm_gc::flatten_multimodule(
        &mut items,
        &deps.modules,
        capabilities,
        wasm_gc::CapabilityFunctionSurface::Runtime,
    );
    aver::ir::pipeline::resolve_and_reannotate(&mut items);
    let output = wasm_gc::compile_to_wasm_gc_flattened_with_custom_capabilities(
        &items, None, None, &aliases, &plan, true,
    )
    .map_err(|e| format!("compile: {e}"))?;
    Ok(output
        .cert_plans
        .fns
        .into_iter()
        .filter(|f| f.plan.is_ok())
        .filter_map(|f| may_admit.get(&f.name).map(|ok| (f.name, *ok)))
        .collect())
}

/// `cert_shape::printer_may_admit` is a superset of what the printer admits:
/// a function the printer prints unfused is never one the fabricating passes
/// may rewrite. A violation would let a pass fuse a function whose
/// certificate describes the source form, so its plan would stop matching.
#[test]
fn printer_admission_implies_the_kept_predicate_on_the_corpus() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut failures = Vec::new();
    let mut admitted = 0usize;
    for entry in corpus(&repo_root) {
        match printer_verdicts(&repo_root, &entry) {
            Ok(verdicts) => {
                admitted += verdicts.len();
                for (name, may_admit) in verdicts {
                    if !may_admit {
                        failures.push(format!(
                            "{}: the printer admits `{name}` but printer_may_admit says no",
                            entry.file
                        ));
                    }
                }
            }
            Err(e) => failures.push(format!("{}: {e}", entry.file)),
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    assert!(admitted > 100, "only {admitted} printer-admitted functions");
}
