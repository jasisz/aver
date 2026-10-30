//! What `--certify` changes in the compile.
//!
//! A certificate is for the exact bytes it was made with: `aver-cert verify`
//! compares them byte for byte, so it rejects a certificate laid beside any
//! other build. `--certify` therefore compiles the bytes it certifies: the
//! functions the certificate plan printer could print keep their ordinary
//! form (`ir::cert_shape`), while a plain build gives them the faster shapes.
//! This suite checks that a `--certify` build is reproducible on the
//! certificate corpus, that the predicate choosing what it keeps never costs a
//! certificate, and that a plain build of certifiable functions takes the
//! faster shapes and still answers their examples as the VM does. It needs no
//! Lean: `--certify` writes the package without building it.
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

/// Compile `entry` for wasm-gc with `flags`, with or without `Int` trees, and
/// return the emitted module bytes and the names its certificate certifies
/// (none without `--certify`).
fn compile(
    repo_root: &Path,
    entry: &Entry,
    flags: &[&str],
    int_trees: bool,
) -> Result<(Vec<u8>, Vec<String>), String> {
    compile_with_env(repo_root, entry, flags, int_trees, &[])
}

/// [`compile`] with extra environment variables set.
fn compile_with_env(
    repo_root: &Path,
    entry: &Entry,
    flags: &[&str],
    int_trees: bool,
    env: &[(&str, &str)],
) -> Result<(Vec<u8>, Vec<String>), String> {
    let out_dir = temp_dir("one-build");
    let mut cmd = aver_command();
    cmd.envs(env.iter().copied());
    cmd.current_dir(repo_root).arg("compile").arg(&entry.file);
    if let Some(root) = entry.module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.arg("--target").arg("wasm-gc").arg("-o").arg(&out_dir);
    cmd.args(flags);
    if !int_trees {
        cmd.env("AVER_NO_INT_TREE", "1").env("AVER_NO_TWINS", "1");
    }
    let output = cmd.output().map_err(|e| format!("spawn aver: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "compile {} {}:\n{}{}",
            entry.file,
            flags.join(" "),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let stem = Path::new(&entry.file)
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("entry has a UTF-8 stem");
    let bytes = std::fs::read(out_dir.join(format!("{stem}.wasm")))
        .map_err(|e| format!("{}: read {stem}.wasm: {e}", entry.file))?;
    let certified = match std::fs::read(out_dir.join("cert").join("cert-manifest.json")) {
        Ok(json) => serde_json::from_slice::<serde_json::Value>(&json)
            .map_err(|e| format!("{}: manifest: {e}", entry.file))?["certified"]
            .as_array()
            .into_iter()
            .flatten()
            .filter_map(|f| Some(f["name"].as_str()?.to_string()))
            .collect(),
        Err(_) => Vec::new(),
    };
    Ok((bytes, certified))
}

/// Run `work` over the corpus on a few threads; every error it returns is a
/// failure.
fn over_corpus(work: impl Fn(&Path, &Entry) -> Result<(), String> + Sync) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let queue = Mutex::new(corpus(&repo_root));
    let failures = Mutex::new(Vec::<String>::new());
    let workers = std::thread::available_parallelism().map_or(2, |n| n.get().min(4));
    std::thread::scope(|scope| {
        for _ in 0..workers {
            scope.spawn(|| {
                loop {
                    // Take the entry and release the lock before working.
                    let next = queue.lock().unwrap().pop();
                    let Some(entry) = next else { break };
                    if let Err(e) = work(&repo_root, &entry) {
                        failures.lock().unwrap().push(e);
                    }
                }
            });
        }
    });
    let failures = failures.into_inner().unwrap();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// A `--certify` build compiles the same module every time, however much of
/// the package it bridges: anyone who builds the source with `--certify` gets
/// the bytes its certificate binds.
#[test]
fn certify_compiles_the_same_module_every_time_on_the_corpus() {
    over_corpus(|repo_root, entry| {
        let (once, _) = compile(repo_root, entry, &["--certify"], true)?;
        let (again, _) = compile(repo_root, entry, &["--certify", "--examples"], true)?;
        if once == again {
            Ok(())
        } else {
            Err(format!(
                "{}: two --certify builds differ ({} and {} bytes)",
                entry.file,
                once.len(),
                again.len()
            ))
        }
    });
}

/// The functions a `--certify` build keeps in their ordinary form get the
/// faster shapes in a plain build, which still answers their examples as the
/// VM does.
#[test]
fn a_plain_build_runs_the_corpus_examples_on_wasm_gc() {
    over_corpus(|repo_root, entry| {
        // The certkit fixtures carry examples of the functions the printer
        // prints; the larger programs take a minute here and run elsewhere.
        if !entry.file.starts_with("tools/certkit/") {
            return Ok(());
        }
        let out = aver_command()
            .current_dir(repo_root)
            .args(["verify", &entry.file, "--wasm-gc"])
            .output()
            .map_err(|e| format!("spawn aver: {e}"))?;
        if out.status.success() {
            Ok(())
        } else {
            Err(format!(
                "verify --wasm-gc {}:\n{}{}",
                entry.file,
                String::from_utf8_lossy(&out.stdout),
                String::from_utf8_lossy(&out.stderr)
            ))
        }
    });
}

/// A plain build computes an `Int` tree on machine words and matches a single
/// character on its codepoint in a function the printer prints; a `--certify`
/// build does neither there, and certifies the function.
#[test]
fn a_plain_build_takes_the_shapes_a_certified_build_leaves_out() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let tree = Entry {
        file: "tests/fixtures/wasm_gc_int_tree/kept.av".into(),
        module_root: None,
    };
    let (plain, _) = compile(&repo_root, &tree, &[], true).unwrap();
    let (no_trees, _) = compile(&repo_root, &tree, &[], false).unwrap();
    let (certified, names) = compile(&repo_root, &tree, &["--certify"], true).unwrap();
    assert_ne!(plain, certified, "the plain build computes `poly` on i64");
    assert_eq!(no_trees, certified, "the --certify build has no Int tree");
    assert!(names.iter().any(|n| n == "poly"), "{names:?}");

    // Without trees, so that only the string passes can tell the builds apart.
    let classifier = Entry {
        file: "tools/certkit/fixtures/stringeq.av".into(),
        module_root: None,
    };
    let (plain, _) = compile(&repo_root, &classifier, &[], false).unwrap();
    let (certified, names) = compile(&repo_root, &classifier, &["--certify"], false).unwrap();
    assert_ne!(
        plain, certified,
        "the plain build matches `quoteOrSelf` on codepoints"
    );
    assert!(names.iter().any(|n| n == "quoteOrSelf"), "{names:?}");
}

/// A slice compared with a string is compared in place in a `--certify` build
/// as in a plain one: the helper doing it comes after every other function
/// and type, so it moves no index the certified functions use, and a function
/// the certificate does not read keeps the bytes of a plain build.
#[test]
fn a_certified_build_compares_slices_in_place_outside_kept_functions() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tests/fixtures/cert_one_build/slice_eq.av".into(),
        module_root: None,
    };
    // Without trees, twins and self-tail loops, so that a difference can
    // only come from the kept functions or the slice comparison.
    let env = [("AVER_NO_SELF_TAIL_LOOPS", "1")];
    let (plain, _) = compile_with_env(&repo_root, &entry, &[], false, &env).unwrap();
    let (certified, names) =
        compile_with_env(&repo_root, &entry, &["--certify"], false, &env).unwrap();
    assert!(names.iter().any(|n| n == "poly"), "{names:?}");
    assert!(!names.iter().any(|n| n == "find"), "{names:?}");
    assert_eq!(plain, certified, "`find` has the same bytes in both builds");

    // Not vacuous: the last function of the module is the helper,
    // `(string, index, i64, i64, string) -> i32`.
    let mut types = Vec::new();
    let mut func_types = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(&certified) {
        match payload.expect("module parses") {
            wasmparser::Payload::TypeSection(reader) => {
                for group in reader {
                    for sub in group.expect("rec group").into_types() {
                        types.push(match sub.composite_type.inner {
                            wasmparser::CompositeInnerType::Func(f) => Some(f),
                            _ => None,
                        });
                    }
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                func_types.extend(reader.into_iter().map(|t| t.expect("function type")));
            }
            _ => {}
        }
    }
    let last = types[*func_types.last().expect("functions") as usize]
        .as_ref()
        .expect("a function type");
    assert_eq!(last.params().len(), 5, "{last:?}");
    assert_eq!(last.results(), [wasmparser::ValType::I32], "{last:?}");
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
        &items, None, None, &aliases, &plan, true, true,
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

/// A self tail call becomes a loop in a `--certify` build too, and the
/// function is still certified: the checker lowers its plan to the same loop.
#[test]
fn a_certified_build_keeps_the_self_tail_loop() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tools/certkit/fixtures/recgen.av".into(),
        module_root: None,
    };
    let (certified, names) = compile(&repo_root, &entry, &["--certify"], true).unwrap();
    assert!(names.iter().any(|n| n == "countDown"), "{names:?}");

    let mut imported = 0;
    let mut count_down = None;
    let mut bodies = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(&certified) {
        match payload.unwrap() {
            wasmparser::Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    if matches!(import.unwrap().ty, wasmparser::TypeRef::Func(_)) {
                        imported += 1;
                    }
                }
            }
            wasmparser::Payload::ExportSection(exports) => {
                for export in exports {
                    let export = export.unwrap();
                    if export.name == "countDown" {
                        count_down = Some(export.index);
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => bodies.push(body),
            _ => {}
        }
    }
    let index = count_down.expect("countDown is exported");
    let body = &bodies[(index - imported) as usize];
    let mut ops = body.get_operators_reader().unwrap();
    let (mut loops, mut self_calls) = (0, 0);
    while !ops.eof() {
        match ops.read().unwrap() {
            wasmparser::Operator::Loop { .. } => loops += 1,
            wasmparser::Operator::ReturnCall { function_index } if function_index == index => {
                self_calls += 1
            }
            _ => {}
        }
    }
    assert_eq!((loops, self_calls), (1, 0), "countDown is one loop");
}
