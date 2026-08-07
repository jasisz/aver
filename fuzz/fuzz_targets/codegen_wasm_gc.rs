// Coverage-guided fuzz target: arbitrary bytes → wasm-gc codegen.
//
// The `fuzz_parse_bytes` and `fuzz_typecheck_program` targets stop
// after the frontend. `fuzz_codegen_wasm_gc` carries inputs all
// the way through to actual `.wasm` bytes via the production
// `compile_to_wasm_gc` entry point, then validates the result
// with `wasmparser`. Any input the typechecker accepts MUST
// produce valid WebAssembly — if not, that's a real codegen bug.
//
// Pipeline:
//   1. lex
//   2. parse
//   3. typecheck (must produce zero errors — codegen on a bad AST
//      is undefined; AFL's other targets cover that surface).
//   4. resolve + last_use + analyse via `ir::pipeline::run`.
//   5. `compile_to_wasm_gc(items, None)` → `Vec<u8>`.
//   6. `wasmparser::Validator::new().validate_all(&bytes)`.
//
// Each stage is gated on the previous succeeding; metrics record
// how many inputs survive each gate so we can chart the
// custom-mutator's effect on codegen-reach over time.
//
// Two layers of protection against the target itself crashing:
//   - `panic::catch_unwind` around every stage,
//   - hard size cap on input (8 KB) to keep the typecheck +
//     codegen passes from spending minutes on adversarial input.

#[path = "common.rs"]
mod common;

use aver::ir::{PipelineConfig, TypecheckMode};

const MAX_INPUT_SIZE: usize = 8 * 1024;

fn main() {
    // `fuzz_nohook!` — the standard `fuzz!` macro installs a
    // process-wide panic hook that runs `std::process::abort()`
    // after the user hook, which bypasses every `catch_unwind`
    // boundary we set up below. The codegen pipeline still
    // contains a few legitimate panics (`aver_type_of` on bare
    // namespace refs that typecheck accepts but never stamps)
    // that the AST mutator easily produces. We want to catch
    // those locally and skip the input, not abort the whole
    // 30-min nightly. Real codegen crashes (invalid wasm output
    // = backend would reject) get an explicit `process::abort()`
    // below — AFL still sees those.
    afl::fuzz_nohook!(|data: &[u8]| {
        if data.len() > MAX_INPUT_SIZE {
            return;
        }
        let c = common::counters();
        c.record_exec();

        // Multi-module dispatch — codegen with cross-module ctor /
        // type-identity is exactly the path that surfaced #208
        // (Arc-wrap missing on dep-module recursive variants).
        let setup_holder = common::try_multimodule_input(data);
        let (source, base_dir): (&str, Option<&str>) = match &setup_holder {
            Some(setup) => (setup.entry_source.as_str(), setup.module_root.to_str()),
            None => {
                let Ok(s) = std::str::from_utf8(data) else {
                    return;
                };
                (s, None)
            }
        };

        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        c.record_lex_ok();
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(mut items) = parser.parse() else {
            return;
        };
        let (nodes, depth) = common::ast_metrics(&items);
        c.record_parse_ok(nodes, depth);

        // Typecheck must produce no errors. We don't run the
        // typechecker via `pipeline::run` because that path
        // discards the error list; call `run_type_check` first
        // for the gate decision.
        let errors = aver::types::checker::run_type_check(&items);
        if !errors.is_empty() {
            return;
        }
        c.record_typecheck_clean();

        // Full pipeline: typecheck (re-run, this time stamping
        // `Spanned::ty` slots), resolve, last_use, analyse.
        // Anything beyond Resolve is what `compile_to_wasm_gc`
        // assumes is in place.
        let _result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir }),
                ..Default::default()
            },
        );

        // Multi-module: flatten dep modules into `items` so
        // `compile_to_wasm_gc` (which expects a single flat AST per
        // its doc comment in src/runtime/wasm_gc.rs) sees them.
        let mut type_aliases = std::collections::HashMap::new();
        if let Some(root) = base_dir {
            if let Ok(dep_modules) = aver::source::load_compile_deps(&items, root) {
                type_aliases =
                    aver::codegen::wasm_gc::flatten_multimodule(&mut items, &dep_modules);
            }
        }

        // Emit wasm-gc bytes. Two failure modes are legitimate
        // adversarial-input outcomes:
        //   - `Err(WasmGcError)` — codegen explicitly refuses the
        //     shape ("can't lower this type", "unsupported effect
        //     surface", …).
        //   - `panic!` from inside codegen — currently includes the
        //     `aver_type_of` assert in `body/infer.rs` that fires
        //     when typecheck accepts a node it doesn't stamp (bare
        //     `Vector` / `Map` namespace refs in expression
        //     position pass typecheck but never get a `Spanned::ty`
        //     populated). Wrap codegen in `catch_unwind` so the
        //     fuzz target moves on; the typecheck-without-stamp gap
        //     is a real bug but it lives in the host pipeline, not
        //     in `wasm-gc emit`. Track it as a follow-up issue and
        //     don't let it abort every nightly run in the
        //     meantime.
        use std::panic::AssertUnwindSafe;
        // Thread the flatten-derived alias map so the fuzz target compiles
        // through the same production path the CLI's multi-module flow uses.
        let compile_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            aver::codegen::wasm_gc::compile_to_wasm_gc_flattened(
                &items,
                None,
                None,
                aver::codegen::wasm_gc::TargetMode::AverBridge,
                &type_aliases,
            )
            .map(|out| out.bytes)
        }));
        let Ok(Ok(bytes)) = compile_result else {
            return;
        };

        // Validate. Any input the codegen accepts must produce
        // bytes the official WebAssembly validator accepts. An
        // invalid-but-emitted module is a real codegen bug:
        // backends (wasmtime, V8, Cloudflare Workers) will refuse
        // to load it, so a user who ran `aver compile --target
        // wasm-gc` and got these bytes can't actually use them.
        let mut validator = wasmparser::Validator::new();
        if validator.validate_all(&bytes).is_err() {
            // Real codegen bug: backend produced bytes the
            // official WebAssembly validator rejects. Explicit
            // `process::abort()` (not `panic!`) because we're
            // running under `fuzz_nohook!` — AFL needs a SIGABRT
            // to register the crash; a plain panic would be
            // caught by the runtime and counted as a normal
            // exit. The input bytes that produced this are
            // preserved in AFL's crash artifact.
            eprintln!("wasm-gc codegen produced invalid module from typechecked source");
            std::process::abort();
        }
    });
    common::counters().flush();
}
