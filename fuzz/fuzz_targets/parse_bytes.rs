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

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };
        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        let mut parser = aver::parser::Parser::new(tokens);
        let _ = parser.parse();
    });
}
