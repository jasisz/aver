use std::path::PathBuf;
use std::process::Command;
#[cfg(any(feature = "wasm", feature = "wasip2"))]
use std::time::{SystemTime, UNIX_EPOCH};

fn run_aver(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(args)
        .output()
        .expect("run aver")
}

fn assert_success(label: &str, output: &std::process::Output) {
    assert!(
        output.status.success(),
        "{label} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(any(feature = "wasm", feature = "wasip2"))]
fn temp_output_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}"))
}

#[test]
fn check_warns_when_project_module_is_shadowed_by_the_stdlib() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("bytes.av"),
        "module Bytes\n    intent = \"project-local Bytes\"\n    exposes [fromList]\n    effects []\n\nrecord Bytes\n    values: List<Int>\n\nfn fromList(xs: List<Int>) -> Result<Bytes, String>\n    ? \"Accept anything.\"\n    Result.Ok(Bytes(values = xs))\n",
    )
    .expect("write bytes.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.octets(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root, "--json"]);
    // Shadowing is a warning, not an error — check must still pass.
    assert_success("aver check (shadowed)", &check);
    let stdout = String::from_utf8_lossy(&check.stdout);
    assert!(stdout.contains("\"slug\":\"stdlib-shadow\""), "{stdout}");
    assert!(
        stdout.contains("reserved by the Aver standard library"),
        "{stdout}"
    );
    // The module loader also warns on stderr at load time.
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert!(stderr.contains("is NOT loaded"), "{stderr}");
    assert!(stderr.contains("bytes.av"), "{stderr}");
}

/// The loader's stderr warning is deduplicated once per process per module
/// name (`source::warn_stdlib_shadow_once`), because module resolution runs
/// several times inside one command — the typecheck tree walk, the dep
/// compile walk, the per-unit check pass. Without the dedup a single
/// `aver check` prints the identical paragraph four times and
/// drowns the signal it exists to carry. Counts the LOADER line only: the
/// structured `warning[stdlib-shadow]:` finding is a separate channel with
/// its own (suppressible) reporting.
#[test]
fn stdlib_shadow_loader_warning_is_printed_once_per_command() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("bytes.av"),
        "module Bytes\n    intent = \"project-local Bytes\"\n    exposes [fromList]\n    effects []\n\nrecord Bytes\n    values: List<Int>\n\nfn fromList(xs: List<Int>) -> Result<Bytes, String>\n    ? \"Accept anything.\"\n    Result.Ok(Bytes(values = xs))\n",
    )
    .expect("write bytes.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.octets(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root]);
    assert_success("aver check (shadowed)", &check);
    let stderr = String::from_utf8_lossy(&check.stderr);
    let loader_lines = stderr
        .lines()
        .filter(|line| line.starts_with("warning: module 'Bytes' is reserved"))
        .count();
    assert_eq!(
        loader_lines, 1,
        "the loader's shadow warning must be emitted exactly once per process \
         per module name, across every resolution phase of one command\nstderr:\n{stderr}"
    );
}

#[test]
fn check_stays_silent_when_no_project_file_shadows_the_stdlib() {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.octets(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root, "--json"]);
    assert_success("aver check (no shadow)", &check);
    let stdout = String::from_utf8_lossy(&check.stdout);
    assert!(!stdout.contains("stdlib-shadow"), "{stdout}");
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert!(
        !stderr.contains("reserved by the Aver standard library"),
        "{stderr}"
    );
}

#[test]
fn embedded_bytes_module_works_outside_the_project_module_root() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = std::env::temp_dir()
        .join("aver-stdlib-no-project-modules")
        .to_string_lossy()
        .into_owned();

    let check = run_aver(&["check", &fixture, "--module-root", &missing_root]);
    assert_success("aver check", &check);

    let verify = run_aver(&["verify", &fixture, "--module-root", &missing_root]);
    assert_success("aver verify", &verify);

    let context = run_aver(&["context", &fixture, "--module-root", &missing_root]);
    assert_success("aver context", &context);
    let rendered = String::from_utf8_lossy(&context.stdout);
    assert!(rendered.contains("## Module: Bytes"), "{rendered}");
    assert!(rendered.contains("## Module: Digest32"), "{rendered}");
    assert!(rendered.contains("### record Digest32"), "{rendered}");
}

/// `Bytes.toHex` is the standard library's own `String.join`-over-a-
/// list-loop, and for a long time it missed the deforestation pass Aver
/// ships: the recogniser knew the Bool-driven loop and the list-driven
/// loop that reverses at the CALL site, but not the list-driven loop
/// that reverses in its own base case — which is what `hexParts` writes.
/// Pin both halves of the fix: the shape is recognised, and it is
/// recognised on the `aver run` path too (dependency modules used to be
/// loaded with the pass switched off there, so the very same source was
/// deforested for `aver compile` and left alone for the VM).
#[test]
fn stdlib_to_hex_is_deforested_and_the_vm_runs_the_fused_shape() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_hex_app.av")
        .to_string_lossy()
        .into_owned();

    let explained = run_aver(&["compile", &fixture, "--explain-passes", "--json"]);
    assert_success("aver compile --explain-passes", &explained);
    let report = String::from_utf8_lossy(&explained.stdout);
    assert!(
        report.contains("Bytes.hexParts__buffered"),
        "the pass must report the standard library's own fusion site: {report}"
    );

    let run = run_aver(&["run", &fixture, "--profile"]);
    assert_success("aver run --profile", &run);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.starts_with("000a107fff"),
        "hex output changed: {stdout}"
    );
    let profile = format!("{stdout}{}", String::from_utf8_lossy(&run.stderr));
    assert!(
        profile.contains("Bytes.hexParts__buffered"),
        "the VM must execute the buffered variant, not the list builder: {profile}"
    );
    assert!(
        !profile.contains("String.join"),
        "the intermediate list and its join must be gone: {profile}"
    );
}

/// The decoding direction. `Bytes.fromHex` hands `String.chars(text)`
/// straight into `parseHexChars`, which peels two cells a step and does
/// nothing else with the list, and `hexDigitValue` decides a character
/// with sixteen single-character arms behind a `String.toLower` — the
/// two shapes chars fusion rewrites. Pin both halves the same way
/// `toHex` is pinned: the pass reports them, and the VM executes what
/// the pass produced rather than the list spelling.
#[test]
fn stdlib_from_hex_walks_a_cursor_and_the_vm_runs_the_fused_shape() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_dehex_app.av")
        .to_string_lossy()
        .into_owned();

    let explained = run_aver(&["compile", &fixture, "--explain-passes", "--json"]);
    assert_success("aver compile --explain-passes", &explained);
    let report = String::from_utf8_lossy(&explained.stdout);
    assert!(
        report.contains("Bytes.parseHexChars__cursor"),
        "the pass must report the standard library's own character loop: {report}"
    );
    assert!(
        report.contains("Bytes.hexDigitValue"),
        "and the sixteen-arm character match it calls: {report}"
    );
    assert!(
        report.contains("Bytes.hexDigitValue__code"),
        "and the classifier variant that takes the codepoint across the \
         call, so the loop binds no one-character string for it: {report}"
    );

    let verify = run_aver(&["verify", &fixture]);
    assert_success("aver verify", &verify);

    let run = run_aver(&["run", &fixture, "--profile"]);
    assert_success("aver run --profile", &run);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.starts_with("00ff10abcdef"),
        "hex round-trip changed: {stdout}"
    );
    assert!(
        stdout.contains("expected an even number of hex characters")
            && stdout.contains("invalid hexadecimal character 'z'"),
        "the error arms read the character the cursor is on: {stdout}"
    );
    let profile = format!("{stdout}{}", String::from_utf8_lossy(&run.stderr));
    assert!(
        profile.contains("Bytes.parseHexChars__cursor"),
        "the VM must execute the cursor variant, not the list loop: {profile}"
    );
    assert!(
        !profile.contains("String.chars"),
        "the list of one-character strings must be gone: {profile}"
    );
}

/// The VM's own copy of the rewrite, pinned at the bytecode level. The
/// VM re-parses every dependency off disk and re-runs the fusing passes
/// on its copy, adopting the result only when the entry's symbol table
/// knows every synthesized name — `Bytes.hexDigitValue__code` included.
/// A silent fall-back to the pristine dependency would still answer
/// correctly, so output parity cannot catch it; what proves adoption is
/// the compiled artifact itself: the decoding loop reads the codepoint
/// at the cursor, and the classifier variant folds it.
#[test]
fn the_vm_bytecode_reads_the_codepoint_at_the_cursor() {
    use aver::ir::pipeline::{PipelineConfig, TypecheckMode};

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = manifest_dir.join("tests/fixtures/stdlib_bytes_dehex_app.av");
    let module_root = entry
        .parent()
        .expect("fixture dir")
        .to_string_lossy()
        .into_owned();
    let source = std::fs::read_to_string(&entry).expect("read fixture");

    // Load the Bytes dependency the way the compile drivers do for the
    // VM and Rust targets: the dep's own pipeline runs the fabricating
    // passes, so the entry's symbol table learns every synthesized name
    // and `adopt_deforestation_if_symbols_agree` can adopt the fused
    // copy the VM re-parses off disk.
    let bytes_source = std::fs::read_to_string(manifest_dir.join("stdlib/bytes.av"))
        .expect("read the standard library's Bytes module");
    let mut dep_items = aver::source::parse_source(&bytes_source).expect("Bytes parses");
    let stdlib_root = manifest_dir.join("stdlib").to_string_lossy().into_owned();
    let dep_result = aver::ir::pipeline::run(
        &mut dep_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&stdlib_root),
            }),
            ..Default::default()
        },
    );
    let dep_modules = vec![aver::codegen::ModuleInfo {
        prefix: "Bytes".to_string(),
        depends: vec![],
        exposes: vec![],
        exposes_opaque: vec![],
        type_defs: dep_items
            .iter()
            .filter_map(|i| match i {
                aver::ast::TopLevel::TypeDef(td) => Some(td.clone()),
                _ => None,
            })
            .collect(),
        fn_defs: dep_items
            .iter()
            .filter_map(|i| match i {
                aver::ast::TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
                _ => None,
            })
            .collect(),
        capability_items: vec![],
        capability_semantics: None,
        verify_blocks: aver::codegen::collect_verify_blocks(&dep_items),
        verify_laws: aver::codegen::collect_verify_laws(&dep_items),
        analysis: dep_result.analysis,
    }];

    let mut items = aver::source::parse_source(&source).expect("fixture parses");
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    let tc = pipeline_result.typecheck.expect("typecheck was requested");
    assert!(tc.errors.is_empty(), "fixture typechecks: {:?}", tc.errors);

    let mut arena = aver::nan_value::Arena::new();
    aver::vm::register_service_types(&mut arena);
    let (code, _globals) = aver::vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(&module_root),
        &entry.to_string_lossy(),
        pipeline_result.analysis.as_ref(),
    )
    .expect("VM compile");

    let loop_id = code
        .find("Bytes.parseHexChars__cursor__collected")
        .expect("the VM adopted the fused decoding loop");
    assert!(
        code.get(loop_id)
            .code
            .contains(&aver::vm::opcode::STR_CURSOR_CODE),
        "the adopted loop binds the codepoint at the cursor"
    );
    let classifier_id = code
        .find("Bytes.hexDigitValue__code")
        .expect("the VM adopted the classifier's codepoint variant");
    assert!(
        code.get(classifier_id)
            .code
            .contains(&aver::vm::opcode::STR_FOLD_LOWER),
        "the classifier variant folds case on the codepoint"
    );
}

/// `Json` is loaded as a dependency, then parsed again by the VM compiler.
/// The second copy must adopt the same String-index workers its symbol table
/// learned from the first; output parity alone would not catch a silent fall
/// back to repeated source-level `String.charAt` scans or allocation of each
/// character's surface `Option<String>`.
#[test]
fn the_vm_adopts_string_index_workers_from_a_dependency() {
    let run = run_aver(&[
        "run",
        "bench/scenarios/json_parse.av",
        "--module-root",
        "examples/data",
        "--profile",
    ]);
    assert_success("aver run Json benchmark --profile", &run);
    let profile = format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert!(
        profile.contains("STR_INDEX_CODE_AT") && profile.contains("Json.parseStringChunk__indexed"),
        "the VM must execute dependency-side indexed workers:\n{profile}"
    );
}

/// The byte sink, pinned at the bytecode level the same way the cursor
/// is. The VM re-parses the dependency and re-runs the fusing passes on
/// its copy; a silent fall-back to the list spelling would still answer
/// correctly, so output parity cannot catch it. What proves adoption is
/// the compiled artifact: the decoding loop pushes bytes and finalizes
/// them, and the caller allocates the byte builder where the list
/// builder used to start.
#[test]
fn the_vm_bytecode_collects_bytes_at_the_cursor() {
    use aver::ir::pipeline::{PipelineConfig, TypecheckMode};

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let entry = manifest_dir.join("tests/fixtures/stdlib_bytes_dehex_app.av");
    let module_root = entry
        .parent()
        .expect("fixture dir")
        .to_string_lossy()
        .into_owned();
    let source = std::fs::read_to_string(&entry).expect("read fixture");

    let bytes_source = std::fs::read_to_string(manifest_dir.join("stdlib/bytes.av"))
        .expect("read the standard library's Bytes module");
    let mut dep_items = aver::source::parse_source(&bytes_source).expect("Bytes parses");
    let stdlib_root = manifest_dir.join("stdlib").to_string_lossy().into_owned();
    let dep_result = aver::ir::pipeline::run(
        &mut dep_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&stdlib_root),
            }),
            ..Default::default()
        },
    );
    let dep_modules = vec![aver::codegen::ModuleInfo {
        prefix: "Bytes".to_string(),
        depends: vec![],
        exposes: vec![],
        exposes_opaque: vec![],
        type_defs: dep_items
            .iter()
            .filter_map(|i| match i {
                aver::ast::TopLevel::TypeDef(td) => Some(td.clone()),
                _ => None,
            })
            .collect(),
        fn_defs: dep_items
            .iter()
            .filter_map(|i| match i {
                aver::ast::TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
                _ => None,
            })
            .collect(),
        capability_items: vec![],
        capability_semantics: None,
        verify_blocks: aver::codegen::collect_verify_blocks(&dep_items),
        verify_laws: aver::codegen::collect_verify_laws(&dep_items),
        analysis: dep_result.analysis,
    }];

    let mut items = aver::source::parse_source(&source).expect("fixture parses");
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    let tc = pipeline_result.typecheck.expect("typecheck was requested");
    assert!(tc.errors.is_empty(), "fixture typechecks: {:?}", tc.errors);

    let mut arena = aver::nan_value::Arena::new();
    aver::vm::register_service_types(&mut arena);
    let (code, _globals) = aver::vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(&module_root),
        &entry.to_string_lossy(),
        pipeline_result.analysis.as_ref(),
    )
    .expect("VM compile");

    let loop_id = code
        .find("Bytes.parseHexChars__cursor__collected")
        .expect("the VM adopted the retargeted decoding loop");
    let loop_code = &code.get(loop_id).code;
    assert!(
        loop_code.contains(&aver::vm::opcode::BYTE_BUILDER_PUSH)
            && loop_code.contains(&aver::vm::opcode::BYTE_BUILDER_FINALIZE),
        "the adopted loop pushes bytes and finalizes them"
    );
    assert!(
        !loop_code.contains(&aver::vm::opcode::LIST_BUILDER_PUSH)
            && !loop_code.contains(&aver::vm::opcode::LIST_BUILDER_FINALIZE),
        "and the list builder is gone from it"
    );
    let caller_id = code
        .find("Bytes.fromHex")
        .expect("the VM compiled the caller");
    assert!(
        code.get(caller_id)
            .code
            .contains(&aver::vm::opcode::BYTE_BUILDER_NEW),
        "the caller starts the byte builder where the list builder began"
    );
}

/// The two rewrites meet on one function. `Bytes.parseHexChars` reads a
/// list `String.chars` builds and writes a list it prepends into and
/// reverses; chars fusion replaces the first, list build replaces the
/// second, and what comes out is a loop with a cursor on one side and a
/// builder on the other — the shape the fusion-ceiling probe measured by
/// hand.
///
/// The composition is what is pinned here: the collected variant is
/// built FROM the cursor variant (its name carries both suffixes), and
/// the answers are the ones `Bytes.fromHex` has always given, error arms
/// included.
#[test]
fn stdlib_from_hex_walks_a_cursor_and_collects_into_a_builder() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_dehex_app.av")
        .to_string_lossy()
        .into_owned();

    let explained = run_aver(&["compile", &fixture, "--explain-passes", "--json"]);
    assert_success("aver compile --explain-passes", &explained);
    let report = String::from_utf8_lossy(&explained.stdout);
    assert!(
        report.contains("Bytes.parseHexChars__cursor__collected"),
        "the two passes must compose on the standard library's own decoding \
         loop — a cursor in, a builder out: {report}"
    );

    let verify = run_aver(&["verify", &fixture]);
    assert_success("aver verify", &verify);

    let run = run_aver(&["run", &fixture, "--profile"]);
    assert_success("aver run --profile", &run);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.starts_with("00ff10abcdef"),
        "hex round-trip changed: {stdout}"
    );
    assert!(
        stdout.contains("expected an even number of hex characters")
            && stdout.contains("invalid hexadecimal character 'z'"),
        "the error arms still report what they always did: {stdout}"
    );
    let profile = format!("{stdout}{}", String::from_utf8_lossy(&run.stderr));
    assert!(
        profile.contains("Bytes.parseHexChars__cursor__collected"),
        "the VM must execute the doubly-fused variant: {profile}"
    );
    assert!(
        !profile.contains("List.reverse"),
        "the accumulator is built in order, so nothing is left to reverse: {profile}"
    );
}

/// The third rewrite on the same function. After chars fusion and list
/// build, `Bytes.fromHex` still built a `List<Int>` only so `fromList`
/// could walk it a second time and wrap it in `Bytes`. The byte sink
/// retargets the collected variant's builder to bytes and deletes the
/// `fromList` call: the validation rides every push, and the loop's
/// answer IS the `Result<Bytes, String>` the pair used to compute.
///
/// Pinned the same way the first two rewrites are: the pass reports the
/// retarget, the VM executes the retargeted variant, and the answers —
/// error arms included — are the ones `Bytes.fromHex` has always given.
#[test]
fn stdlib_from_hex_collects_bytes_without_an_intermediate_list() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_dehex_app.av")
        .to_string_lossy()
        .into_owned();

    let explained = run_aver(&["compile", &fixture, "--explain-passes", "--json"]);
    assert_success("aver compile --explain-passes", &explained);
    let report = String::from_utf8_lossy(&explained.stdout);
    assert!(
        report.contains("\"byte_fns\":[\"Bytes.parseHexChars__cursor__collected\"]"),
        "the pass must report the standard library's decoding loop as \
         retargeted to the byte builder: {report}"
    );
    assert!(
        report.contains("\"byte_retargets\":1"),
        "one fromList call site deleted: {report}"
    );

    let run = run_aver(&["run", &fixture, "--profile"]);
    assert_success("aver run --profile", &run);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.starts_with("00ff10abcdef"),
        "hex round-trip changed: {stdout}"
    );
    assert!(
        stdout.contains("expected an even number of hex characters")
            && stdout.contains("invalid hexadecimal character 'z'"),
        "the error arms still report what they always did: {stdout}"
    );
    let profile = format!("{stdout}{}", String::from_utf8_lossy(&run.stderr));
    assert!(
        profile.contains("Bytes.parseHexChars__cursor__collected"),
        "the VM must execute the retargeted variant: {profile}"
    );
    assert!(
        !profile.contains("Bytes.fromList"),
        "the second pass over the collected list must be gone: {profile}"
    );
    assert!(
        !profile.contains("Bytes.allInRange"),
        "and the per-element validation walk with it: {profile}"
    );
}

#[cfg(feature = "wasm")]
#[test]
fn embedded_crypto_sha256_matches_fips_vectors_on_wasm_gc() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = temp_output_dir("aver-stdlib-wasm-no-project-modules")
        .to_string_lossy()
        .into_owned();

    let verify = run_aver(&[
        "verify",
        &fixture,
        "--module-root",
        &missing_root,
        "--wasm-gc",
    ]);
    assert_success("aver verify --wasm-gc", &verify);
    let rendered = String::from_utf8_lossy(&verify.stdout);
    // The fixture grows verify cases over time; pin the shape (every case
    // passed, and at least the original FIPS vectors ran) rather than an
    // exact count that rots silently.
    let (passed, total) = rendered
        .lines()
        .find_map(|line| {
            let (lhs, _) = line.split_once(" cases passed")?;
            let (p, t) = lhs.rsplit(' ').next()?.split_once('/')?;
            Some((p.parse::<u32>().ok()?, t.parse::<u32>().ok()?))
        })
        .unwrap_or_else(|| panic!("no `N/M cases passed` summary in:\n{rendered}"));
    assert_eq!(passed, total, "{rendered}");
    assert!(
        total >= 13,
        "fewer cases than the original FIPS vectors: {rendered}"
    );
}

#[cfg(feature = "wasip2")]
#[test]
fn embedded_crypto_sha256_compiles_to_valid_wasip2_component() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = temp_output_dir("aver-stdlib-wasip2-no-project-modules")
        .to_string_lossy()
        .into_owned();
    let output_dir = temp_output_dir("aver-stdlib-wasip2-output");
    let output = output_dir.to_string_lossy().into_owned();

    let compile = run_aver(&[
        "compile",
        &fixture,
        "--module-root",
        &missing_root,
        "--target",
        "wasip2",
        "-o",
        &output,
    ]);
    assert_success("aver compile --target wasip2", &compile);
    let component = output_dir.join("stdlib_bytes_app.component.wasm");
    assert!(
        std::fs::metadata(&component).is_ok_and(|m| m.len() > 0),
        "missing generated component: {}",
        component.display()
    );
    let _ = std::fs::remove_dir_all(output_dir);
}
