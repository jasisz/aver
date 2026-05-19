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
    afl::fuzz!(|data: &[u8]| {
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
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );

        // Emit wasm-gc bytes. The codegen API returns
        // `Result<Vec<u8>, WasmGcError>`; an `Err` here is the
        // codegen telling us "I can't handle this shape", which
        // is a legitimate outcome on adversarial input — not a
        // crash. Stop here.
        let Ok(bytes) = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, None) else {
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
            // Panic so AFL records this as a crash. The validator
            // error message is in `is_err()`'s payload; we let
            // AFL's crash dump preserve the input bytes so we can
            // re-run the pipeline manually to get the message.
            panic!(
                "wasm-gc codegen produced invalid module from typechecked source"
            );
        }
    });
    common::counters().flush();
}
