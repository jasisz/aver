// Coverage-guided fuzz target: arbitrary bytes -> emitted Rust project.
//
// This is the high-throughput half of the Rust validity oracle. It drives
// accepted programs through the production dependency preparation, runtime
// lowering, resolved-identity context, and Rust emitter without spawning
// rustc on every AFL iteration. Any type-correct Aver program must reach a
// complete ProjectOutput without panicking or asking the emitter to insert a
// compile_error! placeholder.
//
// Rust's type checker remains the authority for whether those files form a
// valid crate. The nightly workflow therefore takes a bounded, deterministic
// sample of this target's evolved queue and replays it through
// `aver compile --target rust --check`. Splitting the two loops preserves AFL
// throughput while retaining a real rustc oracle.

#[path = "common.rs"]
mod common;

use aver::ir::{PipelineConfig, TypecheckMode};

const MAX_INPUT_SIZE: usize = 8 * 1024;

fn main() {
    afl::fuzz!(|data: &[u8]| {
        if data.len() > MAX_INPUT_SIZE {
            return;
        }
        let counters = common::counters();
        counters.record_exec();

        let setup = common::try_multimodule_input(data);
        let single_root;
        let (source, module_root): (&str, &str) = match &setup {
            Some(setup) => {
                let Some(root) = setup.module_root.to_str() else {
                    return;
                };
                (setup.entry_source.as_str(), root)
            }
            None => {
                let Ok(source) = std::str::from_utf8(data) else {
                    return;
                };
                // `load_compile_deps` also supplies embedded standard-library
                // modules. Give single-file inputs an empty, stable project
                // root so arbitrary dependency names cannot resolve against
                // files from the fuzz crate's working directory.
                single_root = std::env::temp_dir().join("aver-fuzz-rust-single-root");
                if std::fs::create_dir_all(&single_root).is_err() {
                    return;
                }
                let Some(root) = single_root.to_str() else {
                    return;
                };
                (source, root)
            }
        };

        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else {
            return;
        };
        counters.record_lex_ok();
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(mut items) = parser.parse() else {
            return;
        };
        let (nodes, depth) = common::ast_metrics(&items);
        counters.record_parse_ok(nodes, depth);

        let Ok(prepared) = aver::source::load_compile_deps(&items, module_root) else {
            return;
        };
        let aver::source::PreparedCompileDeps { modules, loaded } = prepared;
        let mut result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::WithCheckedLoaded(&loaded)),
                run_interp_lower: true,
                run_buffer_build: true,
                run_chars_fusion: true,
                run_string_index: true,
                run_list_build: true,
                run_build_symbols: true,
                dep_modules: &modules,
                ..Default::default()
            },
        );
        let Some(typecheck) = result.typecheck.as_ref() else {
            return;
        };
        if !typecheck.errors.is_empty() {
            return;
        }
        counters.record_typecheck_clean();

        let typecheck = result
            .typecheck
            .take()
            .expect("Rust fuzz pipeline requested typechecking");
        let view = result.codegen_view(items);
        let mut ctx = aver::codegen::build_context(
            view.items,
            &typecheck,
            view.analysis.as_ref(),
            "aver_fuzz_rust".to_string(),
            modules,
            view.symbol_table,
            view.resolved_items,
        );
        let output = aver::codegen::rust::transpile(&mut ctx);

        if !output.generated_compile_errors().is_empty() {
            std::process::abort();
        }
        let has_manifest = output.files.iter().any(|(path, _)| path == "Cargo.toml");
        let has_entry = output
            .files
            .iter()
            .any(|(path, _)| path == "src/aver_generated/entry/mod.rs");
        if !has_manifest || !has_entry {
            std::process::abort();
        }
    });
    common::counters().flush();
}
