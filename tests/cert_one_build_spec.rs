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

/// A plain build computes every `Int` tree on machine words and matches a
/// single character on its codepoint in a function the printer prints; a
/// `--certify` build computes there only the trees the wall models (`a * b`
/// in `poly`, not its product by a literal) and matches the character by
/// value, and certifies the function.
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
    assert_ne!(
        plain, certified,
        "the plain build computes `c * 3` on i64 too"
    );
    assert_ne!(
        no_trees, certified,
        "the --certify build computes `a * b` on i64"
    );
    assert!(i64_muls(&certified, &["poly"])[0] > 0, "`a * b` on i64");
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

/// A word record (a one-field `Int` carrier the emitter holds as an `i64`)
/// is certified where it is passed, picked, matched out of a constructor,
/// stored in one, built and read. The `--certify` build
/// keeps the carrier on the machine word: `valueOf(w: Word)` takes an `i64`.
#[test]
fn a_certified_build_certifies_word_records_on_the_machine_word() {
    use wasmparser::{CompositeInnerType, Parser, Payload, TypeRef, ValType};
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tools/certkit/fixtures/wordcarrier.av".into(),
        module_root: None,
    };
    let (certified, names) = compile(&repo_root, &entry, &["--certify"], true).unwrap();
    for name in [
        "fromInt",
        "wordOf",
        "valueOf",
        "pick",
        "labelOf",
        "labelValue",
        "relabel",
    ] {
        assert!(names.iter().any(|n| n == name), "{name} not in {names:?}");
    }
    let mut types = Vec::new();
    let mut fn_types = Vec::new();
    let mut imported = 0;
    let mut value_of = None;
    for payload in Parser::new(0).parse_all(&certified) {
        match payload.unwrap() {
            Payload::TypeSection(reader) => {
                for group in reader {
                    types.extend(group.unwrap().into_types());
                }
            }
            Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    if matches!(import.unwrap().ty, TypeRef::Func(_)) {
                        imported += 1;
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                fn_types.extend(reader.into_iter().map(|t| t.unwrap()));
            }
            Payload::ExportSection(exports) => {
                for export in exports {
                    let export = export.unwrap();
                    if export.name == "valueOf" {
                        value_of = Some(export.index);
                    }
                }
            }
            _ => {}
        }
    }
    let index = value_of.expect("valueOf is exported");
    let ty = fn_types[(index - imported) as usize] as usize;
    let CompositeInnerType::Func(sig) = &types[ty].composite_type.inner else {
        panic!("valueOf has a function type");
    };
    assert_eq!(sig.params(), &[ValType::I64], "the word is an i64");
}

/// A Vector a function only passes along (chooses, stores in a constructor or
/// a record, matches or projects out, returns) is certified as an opaque
/// value: the plan holds the emitter's own reference, the version struct over
/// the Vector's array, and no plan operation reads it. A function that reads
/// a Vector (`Vector.get`) or builds one (`Vector.new`) is declined.
#[test]
fn a_certified_build_passes_vectors_along_as_opaque_values() {
    use wasmparser::{CompositeInnerType, HeapType, Parser, Payload, TypeRef, ValType};
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tools/certkit/fixtures/vectorpass.av".into(),
        module_root: None,
    };
    let (certified, names) = compile(&repo_root, &entry, &["--certify"], true).unwrap();
    for name in [
        "pick",
        "branchOf",
        "childrenOf",
        "sizeOf",
        "cellsOf",
        "withCells",
    ] {
        assert!(names.iter().any(|n| n == name), "{name} not in {names:?}");
    }
    for name in ["cellAt", "emptyCells"] {
        assert!(!names.iter().any(|n| n == name), "{name} in {names:?}");
    }
    let mut types = Vec::new();
    let mut fn_types = Vec::new();
    let mut imported = 0;
    let mut pick = None;
    for payload in Parser::new(0).parse_all(&certified) {
        match payload.unwrap() {
            Payload::TypeSection(reader) => {
                for group in reader {
                    types.extend(group.unwrap().into_types());
                }
            }
            Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    if matches!(import.unwrap().ty, TypeRef::Func(_)) {
                        imported += 1;
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                fn_types.extend(reader.into_iter().map(|t| t.unwrap()));
            }
            Payload::ExportSection(exports) => {
                for export in exports {
                    let export = export.unwrap();
                    if export.name == "pick" {
                        pick = Some(export.index);
                    }
                }
            }
            _ => {}
        }
    }
    let index = pick.expect("pick is exported");
    let ty = fn_types[(index - imported) as usize] as usize;
    let CompositeInnerType::Func(sig) = &types[ty].composite_type.inner else {
        panic!("pick has a function type");
    };
    let ValType::Ref(r) = sig.params()[0] else {
        panic!("a Vector parameter is a reference: {:?}", sig.params());
    };
    let HeapType::Concrete(idx) = r.heap_type() else {
        panic!("a Vector parameter has a concrete heap type: {r:?}");
    };
    let version = idx.as_module_index().expect("module type index") as usize;
    let CompositeInnerType::Struct(st) = &types[version].composite_type.inner else {
        panic!("a Vector value is a version struct, not a bare array");
    };
    assert_eq!(st.fields.len(), 3, "arr, diff and held");
}

/// The `i64.mul` count of each named export's body.
fn i64_muls(bytes: &[u8], names: &[&str]) -> Vec<usize> {
    use wasmparser::{Operator, Parser, Payload, TypeRef};
    let mut imported = 0;
    let mut index = vec![None; names.len()];
    let mut bodies = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        match payload.unwrap() {
            Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    if matches!(import.unwrap().ty, TypeRef::Func(_)) {
                        imported += 1;
                    }
                }
            }
            Payload::ExportSection(exports) => {
                for export in exports {
                    let export = export.unwrap();
                    if let Some(k) = names.iter().position(|n| *n == export.name) {
                        index[k] = Some(export.index);
                    }
                }
            }
            Payload::CodeSectionEntry(body) => bodies.push(body),
            _ => {}
        }
    }
    index
        .iter()
        .zip(names)
        .map(|(i, name)| {
            let i = i.unwrap_or_else(|| panic!("{name} is exported"));
            let mut ops = bodies[(i - imported) as usize]
                .get_operators_reader()
                .unwrap();
            let mut muls = 0;
            while !ops.eof() {
                if matches!(ops.read().unwrap(), Operator::I64Mul) {
                    muls += 1;
                }
            }
            muls
        })
        .collect()
}

/// A function the plan printer prints but that calls one it does not can
/// never be certified, so a `--certify` build does not keep it in the
/// ordinary form: `area` (which calls the unprintable `width`) computes its
/// `Int` tree on machine words as in a plain build, while the certified
/// `spread` keeps the boxed products by a literal its plan reads.
#[test]
fn a_certified_build_releases_a_function_that_calls_an_unprintable_one() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tests/fixtures/cert_one_build/released.av".into(),
        module_root: None,
    };
    let (certified, names) = compile(&repo_root, &entry, &["--certify"], true).unwrap();
    assert_eq!(names, vec!["spread".to_string()]);
    let muls = i64_muls(&certified, &["area", "spread"]);
    assert!(muls[0] > 0, "area computes on machine words: {muls:?}");
    assert_eq!(muls[1], 0, "spread is kept in the ordinary form");
}

/// The number of functions a module defines.
fn defined_functions(bytes: &[u8]) -> usize {
    use wasmparser::{Parser, Payload};
    Parser::new(0)
        .parse_all(bytes)
        .filter(|p| matches!(p, Ok(Payload::CodeSectionEntry(_))))
        .count()
}

/// Only the functions a `--certify` build keeps go without a twin: `loop`,
/// which the plan printer may print but declines for its tuple, gets the
/// twin a plain build gives it, so the two modules define the same functions.
#[test]
fn a_certified_build_gives_a_declined_function_its_twin() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = Entry {
        file: "tests/fixtures/cert_one_build/declined.av".into(),
        module_root: None,
    };
    let (plain, _) = compile(&repo_root, &entry, &[], true).unwrap();
    let (certified, names) = compile(&repo_root, &entry, &["--certify"], true).unwrap();
    assert_eq!(names, vec!["firstOf".to_string()]);
    let (untwinned, _) =
        compile_with_env(&repo_root, &entry, &[], true, &[("AVER_NO_TWINS", "1")]).unwrap();
    assert!(
        defined_functions(&plain) > defined_functions(&untwinned),
        "not vacuous: the plain build has a twin"
    );
    assert_eq!(defined_functions(&certified), defined_functions(&plain));
}
