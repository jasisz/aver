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

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };
        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(mut items) = parser.parse() else { return };
        // run_type_check returns a Vec<TypeError> for valid frontend
        // inputs that have real type problems — that's not a panic and
        // not a fuzz finding. We only care that the function returns
        // cleanly without unwinding.
        let _ = aver::types::checker::run_type_check(&items);
        // The resolver mutates its input — feed it the same `items`
        // afterwards. If typecheck wrote `Spanned::ty` annotations,
        // resolve sees them; if not, resolve falls through the
        // `Type::Invalid` recovery path that the post-A4 matcher now
        // suppresses cascading from.
        aver::resolver::resolve_program(&mut items);
    });
}
