//! The capability grammar: `kind = capability` in a module header,
//! `operation` and `opaque` as top-level declarations.
//!
//! Three properties are pinned here, and they point in different
//! directions on purpose.
//!
//! 1. **The silent-misparse trap, both halves.** Before this grammar
//!    existed, an unknown word at top level was not a parse error:
//!    `parse_top_level` fell through to `parse_expr`, and because Aver
//!    has no juxtaposition, `opaque Token` became two adjacent
//!    expression statements with no diagnostic beyond an
//!    `unknown-ident` anchored on the wrong line. `operation` and
//!    `opaque` must now PARSE. `capability Foo` must now be a HARD
//!    ERROR, because a capability is a kind of module and there is
//!    exactly one way to declare one.
//!
//! 2. **Round-trip.** `unparse` is the only writer of `TopLevel`, so an
//!    ignore arm there would silently delete a capability declaration
//!    from the output — valid Aver, just missing its boundary. The
//!    round-trip assertion below fails against any such stub.
//!
//! 3. **Fail-closed.** A file declaring a capability is refused by
//!    `check`, `verify` and `proof` alike, whether the declaration is
//!    in the entry file or in a dependency. Parsing without refusing
//!    would be worse than a parse error: it would let someone believe
//!    a boundary is enforced when nothing registers it.

use std::fs;
use std::process::Command;

use aver::ast::*;
use aver::lexer::Lexer;
use aver::parser::Parser;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

fn parse_error(src: &str) -> String {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser
        .parse()
        .expect_err("expected a parse error")
        .to_string()
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-capability-{tag}-{}", std::process::id()));
    if dir.exists() {
        fs::remove_dir_all(&dir).ok();
    }
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

/// The refusal text every entry point must produce. Matches
/// `aver::types::checker::CAPABILITY_UNSUPPORTED`.
const REFUSAL: &str = "capability declarations are parsed but not yet supported";

/// One module carrying every new grammar form at once.
const CAPABILITY_SOURCE: &str = "\
module Net
    kind = capability
    exposes [greet]
    effects []

opaque ConnectionToken

operation open(host: String, port: Int) -> Result<ConnectionToken, Int>
    ? \"Establish a stream connection.\"
    oracle = generative
    replay = recorded
    hostile = [openOk, openRefused]

operation send(tok: ConnectionToken, line: String) -> Result<Int, Int>
    oracle = generativeOutput
    replay = recorded
    hostile = [sendOk, sendBroken]
    unmodelled = [shutdown]

fn greet(x: Int) -> Int
    ? \"ordinary code above the boundary\"
    x
";

// ---------------------------------------------------------------------------
// 1a. `operation` and `opaque` PARSE — the half that must now succeed
// ---------------------------------------------------------------------------

#[test]
fn opaque_declaration_parses_as_a_capability_item() {
    // Before: two `TopLevel::Stmt(Expr(Ident))` items and no parse
    // error at all — the declaration vanished into expression position.
    let items = parse("opaque ConnectionToken\n");
    assert_eq!(items.len(), 1, "expected exactly one item, got {items:?}");
    assert_eq!(
        items[0],
        TopLevel::Capability(CapabilityItem::Opaque {
            name: "ConnectionToken".to_string(),
            line: 1,
        })
    );
}

#[test]
fn operation_signature_and_attributes_parse() {
    // Before: this errored at the `:` inside the parameter list —
    // by accident, not because `operation` was rejected.
    let items = parse(
        "operation open(host: String, port: Int) -> Result<ConnectionToken, Int>\n\
         \x20   ? \"Establish a stream connection.\"\n\
         \x20   oracle = generative\n\
         \x20   replay = recorded\n\
         \x20   hostile = [openOk, openRefused]\n\
         \x20   unmodelled = [shutdown]\n",
    );
    assert_eq!(items.len(), 1, "expected exactly one item, got {items:?}");
    let TopLevel::Capability(CapabilityItem::Operation(op)) = &items[0] else {
        panic!("expected a capability operation, got {:?}", items[0]);
    };
    assert_eq!(op.name, "open");
    assert_eq!(
        op.params,
        vec![
            ("host".to_string(), "String".to_string()),
            ("port".to_string(), "Int".to_string()),
        ]
    );
    assert_eq!(op.return_type, "Result<ConnectionToken, Int>");
    assert_eq!(op.desc.as_deref(), Some("Establish a stream connection."));
    assert_eq!(op.oracle.as_deref(), Some("generative"));
    assert_eq!(op.replay.as_deref(), Some("recorded"));
    assert_eq!(op.hostile, vec!["openOk", "openRefused"]);
    assert_eq!(op.unmodelled, vec!["shutdown"]);
}

#[test]
fn operation_without_an_attribute_block_parses() {
    // `operation size() -> Int` used to error at the `->`, also by
    // accident. Attribute admissibility is a later phase; the grammar
    // must not invent a rule it cannot yet enforce.
    let items = parse("operation size() -> Int\n");
    let TopLevel::Capability(CapabilityItem::Operation(op)) = &items[0] else {
        panic!("expected a capability operation, got {:?}", items[0]);
    };
    assert_eq!(op.name, "size");
    assert!(op.params.is_empty());
    assert_eq!(op.return_type, "Int");
    assert_eq!(op.oracle, None);
}

#[test]
fn kind_capability_is_a_module_header_field() {
    let items = parse("module Net\n    kind = capability\n    exposes [greet]\n");
    let TopLevel::Module(m) = &items[0] else {
        panic!("expected a module, got {:?}", items[0]);
    };
    assert_eq!(m.kind.as_deref(), Some("capability"));
    assert_eq!(m.kind_line, Some(2));
    assert_eq!(m.exposes, vec!["greet"]);
}

#[test]
fn ordinary_module_has_no_kind() {
    let items = parse("module Net\n    exposes [greet]\n");
    let TopLevel::Module(m) = &items[0] else {
        panic!("expected a module, got {:?}", items[0]);
    };
    assert_eq!(m.kind, None);
}

// ---------------------------------------------------------------------------
// 1b. `capability Foo` is a HARD ERROR — the half that must now fail
// ---------------------------------------------------------------------------

#[test]
fn capability_block_at_top_level_is_a_parse_error() {
    // Before: two bogus-span `unknown-ident`s from the typechecker and
    // no parse error. A capability is a KIND OF MODULE; a `capability`
    // block would duplicate exposes / depends / visibility / name
    // resolution the module already supplies.
    let msg = parse_error("capability Tcp\n");
    assert!(
        msg.contains("kind = capability"),
        "the error must name the one way to declare a capability, got: {msg}"
    );
    assert!(
        msg.contains("kind of module"),
        "the error must say a capability is a kind of module, got: {msg}"
    );
    assert!(
        msg.contains("Tcp"),
        "the error must name the offending declaration, got: {msg}"
    );
}

#[test]
fn opaque_with_fields_is_a_parse_error() {
    // An opaque capability type has no representation anywhere. A field
    // list would enter the contract hash and bind every provider to one
    // implementation's internals.
    let msg = parse_error("opaque ConnectionToken\n    fd: Int\n");
    assert!(
        msg.contains("no representation"),
        "expected the representation-less rule, got: {msg}"
    );
    assert!(
        msg.contains("ConnectionToken"),
        "the error must name the type, got: {msg}"
    );
}

#[test]
fn unknown_operation_field_is_a_parse_error() {
    let msg = parse_error("operation open() -> Int\n    orcale = generative\n");
    assert!(
        msg.contains("Unknown operation field 'orcale'"),
        "a typo'd attribute must be refused, not dropped, got: {msg}"
    );
    assert!(
        msg.contains("oracle, replay, hostile, unmodelled"),
        "the error must list the allowed fields, got: {msg}"
    );
}

#[test]
fn unknown_module_header_field_is_a_parse_error() {
    // The header loop used to `break` on an unrecognised field, leaving
    // the DEDENT unconsumed so the line was re-read at top level as an
    // ordinary binding. A mistyped `kind` was therefore silent.
    let msg = parse_error("module Net\n    kimd = capability\n    exposes [greet]\n");
    assert!(
        msg.contains("Unknown module header field"),
        "expected a loud unknown-field error, got: {msg}"
    );
    assert!(
        msg.contains("intent, kind, depends, exposes, effects"),
        "the error must list the allowed header fields, got: {msg}"
    );

    // A typo'd bracket-list field takes the same path: `exposse [greet]`
    // would otherwise fall through to the expression parser and become
    // an index expression.
    let msg = parse_error("module Net\n    exposse [greet]\n");
    assert!(
        msg.contains("Unknown module header field"),
        "a mistyped bracket-list field must be refused too, got: {msg}"
    );
}

#[test]
fn indented_items_still_end_the_module_header() {
    // The unknown-field error is scoped to HEADER-FIELD SHAPE — an
    // identifier followed by `=` or `[`. An indented `fn` cannot be
    // confused with a field, so it must still end the header and be
    // re-read as a top-level item. `format_cmd`'s
    // `normalizes_line_endings_and_trailing_ws` pins this shape.
    let items = parse("module A\n    fn x() -> Int\n        1\n");
    assert_eq!(items.len(), 2, "expected a module and a fn, got {items:?}");
    assert!(matches!(&items[0], TopLevel::Module(m) if m.name == "A"));
    assert!(matches!(&items[1], TopLevel::FnDef(fd) if fd.name == "x"));
}

#[test]
fn duplicate_operation_field_is_a_parse_error() {
    let msg =
        parse_error("operation open() -> Int\n    oracle = generative\n    oracle = output\n");
    assert!(
        msg.contains("Duplicate operation field 'oracle'"),
        "a repeated attribute must not silently win last-write, got: {msg}"
    );
}

// ---------------------------------------------------------------------------
// Every new word stays CONTEXTUAL
// ---------------------------------------------------------------------------

#[test]
fn contextual_words_remain_ordinary_identifiers() {
    // `replay` is a live function name in the corpus, `binding` is a
    // live pattern binder, `output` occurs dozens of times as an
    // identifier. None of the new words may be reserved.
    let src = "\
fn operation(x: Int) -> Int
    x

fn opaque(x: Int) -> Int
    x

fn capability(x: Int) -> Int
    x

fn kind(x: Int) -> Int
    x

fn replay(output: Int) -> Int
    operation = 1
    opaque = 2
    capability = 3
    kind = 4
    output + operation + opaque + capability + kind
";
    let items = parse(src);
    let names: Vec<&str> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) => Some(fd.name.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(
        names,
        vec!["operation", "opaque", "capability", "kind", "replay"],
        "every new word must still be usable as a function name"
    );
    assert!(
        !items
            .iter()
            .any(|i| matches!(i, TopLevel::Capability(_) | TopLevel::Module(_))),
        "no contextual word may be promoted to a declaration outside item position"
    );
}

#[test]
fn contextual_words_still_bind_at_top_level() {
    // `opaque = 1` has `Assign` at peek(1), so the guarded arms above
    // must not claim it.
    for word in ["operation", "opaque", "capability", "kind"] {
        let items = parse(&format!("{word} = 1\n"));
        assert_eq!(
            items.len(),
            1,
            "`{word} = 1` must stay a single binding, got {items:?}"
        );
        assert!(
            matches!(&items[0], TopLevel::Stmt(Stmt::Binding(n, _, _)) if n == word),
            "`{word} = 1` must stay a binding, got {:?}",
            items[0]
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Round-trip — kills any `=> Ok(())` stub in `write_top_level`
// ---------------------------------------------------------------------------

#[test]
fn capability_declarations_survive_unparse_and_reparse() {
    let items = parse(CAPABILITY_SOURCE);

    // The parse itself pins the item set: a dropped dispatch arm would
    // change these counts before unparse ever runs.
    let capability_items: Vec<&CapabilityItem> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::Capability(c) => Some(c),
            _ => None,
        })
        .collect();
    assert_eq!(
        capability_items.len(),
        3,
        "expected `opaque` + two `operation`s, got {capability_items:?}"
    );
    assert_eq!(
        capability_items
            .iter()
            .map(|c| c.name())
            .collect::<Vec<_>>(),
        vec!["ConnectionToken", "open", "send"]
    );

    let text = aver::ast::unparse::unparse(&items).expect("unparse failed");
    assert!(
        text.contains("kind = capability"),
        "the module kind must survive unparse:\n{text}"
    );
    assert!(
        text.contains("opaque ConnectionToken"),
        "the opaque declaration must survive unparse:\n{text}"
    );
    assert!(
        text.contains("operation open(host: String, port: Int) -> Result<ConnectionToken, Int>"),
        "the operation signature must survive unparse:\n{text}"
    );
    assert!(
        text.contains("hostile = [openOk, openRefused]"),
        "the model attributes must survive unparse:\n{text}"
    );
    assert!(
        text.contains("unmodelled = [shutdown]"),
        "`unmodelled` must survive unparse:\n{text}"
    );

    // The property that catches a partial writer: reparsing must give
    // back an equal item list, not merely a parseable one.
    let reparsed = parse(&text);
    assert_eq!(
        reparsed, items,
        "unparse → reparse must be lossless for capability declarations"
    );
}

// ---------------------------------------------------------------------------
// 3. Fail-closed: refused by check, verify AND proof
// ---------------------------------------------------------------------------

fn run(cmd: &str, dir: &std::path::Path, file: &str) -> (i32, String) {
    let out = Command::new(aver_bin())
        .current_dir(dir)
        .args([cmd, "--module-root", dir.to_str().expect("utf-8 dir"), file])
        .output()
        .unwrap_or_else(|e| panic!("run aver {cmd}: {e}"));
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (out.status.code().unwrap_or(-1), text)
}

#[test]
fn capability_file_is_refused_by_check_verify_and_proof() {
    let dir = temp_dir("entry");
    fs::write(dir.join("net.av"), CAPABILITY_SOURCE).expect("write net.av");

    for cmd in ["check", "verify", "proof"] {
        let (code, text) = run(cmd, &dir, "net.av");
        assert_ne!(
            code, 0,
            "`aver {cmd}` must refuse a capability file, but exited 0:\n{text}"
        );
        assert!(
            text.contains(REFUSAL),
            "`aver {cmd}` must refuse with the capability message, got:\n{text}"
        );
        // Each of the four declarations is reported, not just the first
        // one the scan happens to reach.
        for named in [
            "module 'Net' declares `kind = capability`",
            "`opaque ConnectionToken`",
            "`operation open`",
            "`operation send`",
        ] {
            assert!(
                text.contains(named),
                "`aver {cmd}` must name {named} in its refusal, got:\n{text}"
            );
        }
    }
}

#[test]
fn capability_in_a_dependency_is_refused_by_check_verify_and_proof() {
    // `proof` has no `--deps` flag. If the refusal only ran for the
    // entry file, a capability declared one module away would slip
    // through every entry point that loads it — the same shape as the
    // entry-scoped checking gap.
    let dir = temp_dir("dep");
    fs::write(dir.join("Net.av"), CAPABILITY_SOURCE).expect("write Net.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Net]
    exposes [go]
    effects []

fn go(x: Int) -> Int
    ? \"calls into the dependency\"
    Net.greet(x)

verify go
    go(1) => 1
",
    )
    .expect("write main.av");

    for cmd in ["check", "verify", "proof"] {
        let (code, text) = run(cmd, &dir, "main.av");
        assert_ne!(
            code, 0,
            "`aver {cmd}` must refuse a capability in a dependency, but exited 0:\n{text}"
        );
        assert!(
            text.contains(REFUSAL),
            "`aver {cmd}` must refuse the dependency's capability, got:\n{text}"
        );
        assert!(
            text.contains("`operation open`"),
            "`aver {cmd}` must reach the dependency's operations, got:\n{text}"
        );
    }
}

#[test]
fn a_module_without_a_capability_still_passes() {
    // The refusal must not fire on ordinary code. Without this, a green
    // suite above proves only that everything is rejected.
    let dir = temp_dir("clean");
    fs::write(
        dir.join("clean.av"),
        "\
module Clean
    exposes [go]
    effects []

fn go(x: Int) -> Int
    ? \"ordinary\"
    x

verify go
    go(1) => 1
",
    )
    .expect("write clean.av");

    let (code, text) = run("check", &dir, "clean.av");
    assert_eq!(
        code, 0,
        "an ordinary module must still check clean:\n{text}"
    );
    assert!(
        !text.contains(REFUSAL),
        "the refusal must not fire on ordinary code:\n{text}"
    );
}
