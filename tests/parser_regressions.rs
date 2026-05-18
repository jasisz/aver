//! Gate every minimized AFL++ crash committed under
//! `tests/regressions/parser/`. Each `.av` file there is a byte
//! sequence that once made the lexer or parser panic; the test re-feeds
//! it and asserts the pipeline now answers with `Ok`/`Err`, never with
//! a panic.
//!
//! When AFL++ finds a new crash, run `cargo afl tmin` to minimize the
//! reproducer, then drop the result here as `<short-hash>.av`. The
//! file's name is purely informational — discovery is by `*.av` glob.

use std::fs;
use std::panic;
use std::path::PathBuf;

use aver::lexer::Lexer;
use aver::parser::Parser;

#[test]
fn parser_regressions_do_not_panic() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/regressions/parser");
    if !dir.exists() {
        return;
    }
    let mut tested = 0usize;
    for entry in fs::read_dir(&dir).expect("read regressions dir") {
        let path = entry.expect("dir entry").path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        let bytes = fs::read(&path).expect("read regression");
        let display = path.display().to_string();
        let result = panic::catch_unwind(|| {
            let Ok(source) = std::str::from_utf8(&bytes) else {
                return;
            };
            let mut lexer = Lexer::new(source);
            let Ok(tokens) = lexer.tokenize() else { return };
            let mut parser = Parser::new(tokens);
            let _ = parser.parse();
        });
        assert!(result.is_ok(), "parser regression panicked on {}", display);
        tested += 1;
    }
    eprintln!("parser_regressions_do_not_panic: {} files", tested);
}
