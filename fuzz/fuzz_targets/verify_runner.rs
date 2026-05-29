// Coverage-guided fuzz target: arbitrary bytes → aver verify pipeline.
//
// Runs `verify <fn>` blocks through the VM-backed verify runner
// (the same path `aver verify` invokes for cases-form blocks). No
// Lean / Dafny / Z3 — those are external solvers that don't make
// sense as a fuzz target (subprocess spawn per exec). Just the
// in-process Aver pipeline:
//
//   bytes → lex → parse → typecheck (zero errors required)
//        → ir::pipeline::run (resolve + last_use + analyse)
//        → run_verify_for_items_vm
//
// The verify runner constructs a VM, compiles every `fn` in
// scope, executes each `verify` case (LHS evaluation → equality
// against RHS), and returns a `Vec<VerifyResult>`. Failure mode:
// any panic / abort inside the runner, regardless of input shape.
// Returning `Err(_)` from the runner itself is *not* a crash —
// it's the runner saying "I can't verify this program", which
// is a legitimate outcome on adversarial input.

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

        // Multi-module dispatch: inputs prefixed with the magic byte
        // sequence get parsed as a small multi-file project and run
        // through the verify pipeline with the synthetic tmpdir as
        // module root. Catches cross-module fn / type-identity bugs
        // (#208 + #213 class) that single-file fuzzing can't surface.
        if let Some(setup) = common::try_multimodule_input(data) {
            let _ = aver::diagnostics::vm_verify::run_verify_for_items_vm(
                match aver::source::parse_source(&setup.entry_source) {
                    Ok(items) => items,
                    Err(_) => return,
                },
                None,
                setup.module_root.to_str(),
                setup.entry_path.to_str().unwrap_or("multi.av"),
            );
            return;
        }

        let Ok(source) = std::str::from_utf8(data) else {
            return;
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

        // Gate verify on a clean typecheck. The runner internally
        // re-typechecks but a quick pre-check here cuts the
        // typecheck-failure proportion before we pay the verify
        // pipeline cost.
        let errors = aver::types::checker::run_type_check(&items);
        if !errors.is_empty() {
            return;
        }
        c.record_typecheck_clean();

        // Run the IR pipeline so `Spanned::ty` slots are stamped
        // and resolver has slot-allocated every binding — the
        // verify runner assumes both.
        let _result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );

        // Invoke the production verify runner. The `Err(_)` arm is
        // a legitimate "verify can't process this program"
        // signal — not a crash. A panic from anywhere inside the
        // verify pipeline IS a crash and AFL will surface it.
        // `source_file` label is purely diagnostic — we pass a
        // synthetic name so the runner's error messages name the
        // fuzz input rather than the empty string.
        let _ = aver::diagnostics::vm_verify::run_verify_for_items_vm(
            items,
            None,
            None,
            "fuzz_input.av",
        );
    });
    common::counters().flush();
}
