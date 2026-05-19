// Coverage-guided fuzz target: arbitrary bytes → Aver lexer + parser.
//
// Invariant under test: the lexer and parser must never panic on any
// byte sequence. `Err(LexerError)` and `Err(ParseError)` are fine — those
// are normal "this isn't valid Aver" answers. Panic / abort / stack
// overflow / hang is a bug to fix.
//
// AFL++ drives this via `cargo afl fuzz` with a seed corpus in
// `corpus/parser/` and a token dictionary in `dicts/aver.dict`. Crashes
// AFL finds get minimized by `afl-tmin` and committed as
// `tests/regressions/parser/<hash>.av`, which `cargo test --test
// parser_regressions` re-runs on every PR to prevent regressions.
//
// Iron 0.21 Hardcore Fuzz Phase 0: emits per-campaign metrics to
// `/tmp/aver_fuzz_metrics_fuzz_parse_bytes.txt`. CI uploads the file
// alongside the AFL queue artifact so we can chart parse-success
// rate, ast-shape distribution, and depth high-water marks
// independently from AFL's bitmap coverage.

#[path = "common.rs"]
mod common;

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let c = common::counters();
        c.record_exec();
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };
        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        c.record_lex_ok();
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(items) = parser.parse() else { return };
        let (nodes, depth) = common::ast_metrics(&items);
        c.record_parse_ok(nodes, depth);
    });
    // Final snapshot — interval flush may not fire if AFL terminates
    // the persistent loop mid-budget. CI reads this file after the
    // fuzz step.
    common::counters().flush();
}
