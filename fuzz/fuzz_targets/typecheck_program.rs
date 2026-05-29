// Coverage-guided fuzz target: arbitrary bytes → full frontend pipeline.
//
// Pre-Iron we only fuzzed the lexer + parser. The typechecker, resolver
// and pre-codegen passes are equally exposed to user input through any
// program that survives parsing, and a panic there is the same class
// of bug — an ICE that takes down the whole `aver` invocation when a
// real user feeds in a malformed-but-parseable source file.
//
// Invariant under test: from `bytes` to "all frontend passes complete
// without panic". Real "this source is broken" answers come back as
// `Vec<TypeError>` or `Err(_)` from the relevant stage — those are
// fine. Panic / abort / stack overflow / hang is a bug.
//
// Pipeline order matches `aver check` / `aver run`:
//   1. lex
//   2. parse
//   3. typecheck (no base_dir — multi-file resolution lives outside
//      this target's scope; `aver` itself handles disk IO above us)
//   4. resolve (slot allocation, dependency rewiring)
//
// Steps 3 and 4 are gated on the prior step producing an `Ok` — we
// don't want to count "parser said no" as a typecheck panic, and we
// don't want to count "typecheck wrote errors" as a resolver panic.
//
// Iron 0.21 Hardcore Fuzz Phase 0: metrics surface to
// `/tmp/aver_fuzz_metrics_fuzz_typecheck_program.txt`. The `typecheck_clean`
// counter is the headline metric this target adds over `parse_bytes`
// — once Phase 1 lands the custom mutator, the ratio
// `typecheck_clean / parse_ok` is the direct measure of whether
// structured mutations push inputs deeper into the pipeline.

#[path = "common.rs"]
mod common;

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let c = common::counters();
        c.record_exec();

        // Multi-module dispatch — multi-file inputs exercise the
        // cross-module typecheck path (TypecheckMode::Full with a
        // base_dir loads dep modules on demand). Catches Type::Named
        // identity collisions across modules.
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
        // typecheck via pipeline (picks up dep modules via base_dir on
        // multi-module dispatch). Real type errors are not panics and
        // not findings — we only care that the function returns
        // cleanly without unwinding.
        let tc_result =
            aver::ir::pipeline::typecheck(&items, &aver::ir::TypecheckMode::Full { base_dir });
        if tc_result.errors.is_empty() {
            c.record_typecheck_clean();
        }
        // The resolver mutates its input — feed it the same `items`
        // afterwards. If typecheck wrote `Spanned::ty` annotations,
        // resolve sees them; if not, resolve falls through the
        // `Type::Invalid` recovery path that the post-A4 matcher now
        // suppresses cascading from.
        aver::resolver::resolve_program(&mut items);
    });
    common::counters().flush();
}
