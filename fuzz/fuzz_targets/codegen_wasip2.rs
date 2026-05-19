// Coverage-guided fuzz target: arbitrary bytes → wasip2
// component codegen.
//
// Sister target to `fuzz_codegen_wasm_gc`. Where that one chases
// the wasm-gc bridge mode, this one drives the wasip2 path that
// `aver compile --target wasip2` ships through:
//
//   1. lex / parse / typecheck (zero errors required)
//   2. `ir::pipeline::run` (resolve + last_use + analyse)
//   3. `compile_to_wasm_gc_for_wasip2(items, None)` — core module
//      with the canonical-ABI shape, different `TargetMode` arm in
//      `wasm_gc::module::emit_module_with` than the bridge target.
//   4. `wasip2::compile_to_component(core_wasm, CliCommand)` —
//      wit-component wrap pass that adapts the core module into a
//      `wasi:cli/command` component (different surface from the
//      core wasm validator the wasm-gc target uses).
//   5. `wasmparser::Validator::new_with_features(WasmFeatures::default())`
//      with the component-model bit explicitly on. Any invalid
//      component is a real codegen bug — wasmtime / Spin /
//      wasmCloud will refuse to load it.
//
// Targets the *third* backend (after VM and wasm-gc bridge). Iron
// 0.21 review flagged wasip2 as the largest fuzz gap — newer
// pipeline, less battle-tested codegen, different validator
// surface. This target closes that gap.
//
// `fuzz_nohook!` + `catch_unwind` boundaries mirror
// `fuzz_codegen_wasm_gc`: legitimate adversarial-input outcomes
// (codegen `Err`, internal panic from the typecheck-no-stamp gap)
// → skip; a successfully-emitted but invalid component → explicit
// `process::abort()` for AFL to register.

#[path = "common.rs"]
mod common;

use aver::ir::{PipelineConfig, TypecheckMode};

const MAX_INPUT_SIZE: usize = 8 * 1024;

fn main() {
    afl::fuzz_nohook!(|data: &[u8]| {
        if data.len() > MAX_INPUT_SIZE {
            return;
        }
        let c = common::counters();
        c.record_exec();
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };
        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        c.record_lex_ok();
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(mut items) = parser.parse() else { return };
        let (nodes, depth) = common::ast_metrics(&items);
        c.record_parse_ok(nodes, depth);

        let errors = aver::types::checker::run_type_check(&items);
        if !errors.is_empty() {
            return;
        }
        c.record_typecheck_clean();

        let _result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );

        // Stage 1: core wasm (canonical-ABI shape). Same panic
        // hazards as the bridge target — `aver_type_of` on bare
        // namespace refs trips here too. `catch_unwind` so the
        // target keeps going.
        use std::panic::AssertUnwindSafe;
        let core_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            aver::codegen::wasm_gc::compile_to_wasm_gc_for_wasip2(&items, None)
        }));
        let Ok(Ok(core_bytes)) = core_result else {
            return;
        };

        // Stage 2: component wrap. wit-component does its own
        // validation pass internally; an `Err` here = the wrap
        // pass rejected the core module, which is a legitimate
        // adversarial outcome (skip). A panic inside the wrap
        // pass is more interesting — track via catch_unwind.
        let component_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            aver::codegen::wasip2::compile_to_component(
                &core_bytes,
                aver::codegen::wasip2::Wasip2World::CliCommand,
            )
        }));
        let Ok(Ok((component_bytes, _wit))) = component_result else {
            return;
        };

        // Component-model validation. `WasmFeatures::default()`
        // enables the component model today; spelling it out via
        // `new_with_features` keeps the check explicit so a
        // future wasmparser default flip doesn't silently turn
        // this into a core-module-only check.
        let mut validator = wasmparser::Validator::new_with_features(
            wasmparser::WasmFeatures::default(),
        );
        if validator.validate_all(&component_bytes).is_err() {
            // Real codegen bug: emitted bytes that the component-
            // model validator rejects. Hosts (wasmtime, Spin,
            // wasmCloud) refuse to load this. `process::abort()`
            // so AFL registers the crash under `fuzz_nohook!`.
            eprintln!(
                "wasip2 codegen produced invalid component from typechecked source"
            );
            std::process::abort();
        }
    });
    common::counters().flush();
}
