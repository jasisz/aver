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
//! 3. **Mandatory semantics + fail-closed runtime.** Every capability
//!    module chooses one homogeneous semantic class. Valid contracts
//!    type-check, while execution without a provider fails at the exact
//!    operation boundary.

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

fn tool_available(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-capability-{tag}-{}", std::process::id()));
    if dir.exists() {
        fs::remove_dir_all(&dir).ok();
    }
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn collect_files_with_extension(dir: &std::path::Path, extension: &str) -> String {
    let mut paths = Vec::new();
    let mut pending = vec![dir.to_path_buf()];
    while let Some(current) = pending.pop() {
        for entry in fs::read_dir(current).expect("read generated proof directory") {
            let path = entry.expect("read generated proof entry").path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().and_then(|value| value.to_str()) == Some(extension) {
                paths.push(path);
            }
        }
    }
    paths.sort();
    paths
        .into_iter()
        .map(|path| fs::read_to_string(path).expect("read generated proof file"))
        .collect::<Vec<_>>()
        .join("\n")
}

const MISSING_SEMANTICS: &str = "must declare `semantics = pure` or `semantics = effectful`";

/// One module carrying every new grammar form at once.
const CAPABILITY_SOURCE: &str = "\
module Net
    kind = capability
    semantics = effectful
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
fn capability_kind_and_semantics_are_module_header_fields() {
    let items = parse(
        "module Net\n    kind = capability\n    semantics = effectful\n    exposes [greet]\n",
    );
    let TopLevel::Module(m) = &items[0] else {
        panic!("expected a module, got {:?}", items[0]);
    };
    assert_eq!(m.kind.as_deref(), Some("capability"));
    assert_eq!(m.kind_line, Some(2));
    assert_eq!(m.semantics.as_deref(), Some("effectful"));
    assert_eq!(m.semantics_line, Some(3));
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
        msg.contains("intent, kind, semantics, depends, exposes, effects"),
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

    // An emptied list is still a value someone wrote, so the repeat is
    // tracked by field name rather than by "did the value change".
    let msg = parse_error("operation open() -> Int\n    hostile = []\n    hostile = [a]\n");
    assert!(
        msg.contains("Duplicate operation field 'hostile'"),
        "a repeat after an empty list must be caught too, got: {msg}"
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

fn semantics(x: Int) -> Int
    x

fn replay(output: Int) -> Int
    operation = 1
    opaque = 2
    capability = 3
    kind = 4
    semantics = 5
    output + operation + opaque + capability + kind + semantics
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
        vec![
            "operation",
            "opaque",
            "capability",
            "kind",
            "semantics",
            "replay"
        ],
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
    for word in ["operation", "opaque", "capability", "kind", "semantics"] {
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
        text.contains("semantics = effectful"),
        "the mandatory semantics must survive unparse:\n{text}"
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
// 3. Mandatory semantics and provider-bound runtime
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

const VALID_EFFECT_CAPABILITY: &str = "\
module Clock
    kind = capability
    semantics = effectful
    exposes [now]
    effects []

operation now() -> Int
    oracle = generative
    replay = recorded
    hostile = [zero]

fn zero(path: BranchPath, call: Int) -> Int
    0
";

const VALID_PURE_CAPABILITY: &str = "\
module Digest
    kind = capability
    semantics = pure
    exposes [digest]
    effects []

opaque Context

operation digest(text: String) -> String
";

#[test]
fn missing_semantics_is_refused_by_check_verify_and_proof() {
    let dir = temp_dir("missing-semantics");
    fs::write(
        dir.join("Clock.av"),
        VALID_EFFECT_CAPABILITY.replace("    semantics = effectful\n", ""),
    )
    .expect("write Clock.av");

    for cmd in ["check", "verify", "proof"] {
        let (code, text) = run(cmd, &dir, "Clock.av");
        assert_ne!(code, 0, "`aver {cmd}` accepted missing semantics:\n{text}");
        assert!(
            text.contains(MISSING_SEMANTICS),
            "`aver {cmd}` must explain the mandatory field, got:\n{text}"
        );
    }
}

#[test]
fn valid_effectful_contract_typechecks_in_a_dependency() {
    let dir = temp_dir("effectful-dep");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [tick]
    effects [Clock.now]

fn tick() -> Int
    ? \"reads the provider-bound clock\"
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write main.av");

    let (code, text) = run("check", &dir, "main.av");
    assert_eq!(code, 0, "a valid capability dependency must check:\n{text}");
}

#[test]
fn capability_effects_require_operation_granularity() {
    let dir = temp_dir("effect-shorthand");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [tick]
    effects [Clock]

fn tick() -> Int
    ! [Clock]
    Clock.now()
",
    )
    .expect("write main.av");

    let (code, text) = run("check", &dir, "main.av");
    assert_ne!(
        code, 0,
        "capability namespace shorthand widened silently:\n{text}"
    );
    assert!(
        text.contains("Capability effect shorthand 'Clock' is not allowed"),
        "the error must require exact provider atoms:\n{text}"
    );
}

#[test]
fn execution_without_a_provider_fails_at_the_operation_boundary() {
    let dir = temp_dir("provider-missing");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> Int
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write main.av");

    let (code, text) = run("run", &dir, "main.av");
    assert_ne!(code, 0, "run unexpectedly found a provider:\n{text}");
    assert!(
        text.contains("capability provider missing for 'Clock.now'"),
        "the error must identify the exact boundary, got:\n{text}"
    );

    let output_dir = dir.join("generated-rust");
    let output = Command::new(aver_bin())
        .current_dir(&dir)
        .args([
            "compile",
            "--module-root",
            dir.to_str().expect("utf-8 dir"),
            "main.av",
            "--target",
            "rust",
            "-o",
        ])
        .arg(&output_dir)
        .output()
        .expect("compile host-bound Rust provider artifact");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.status.success(), "Rust compilation failed:\n{text}");
    assert!(
        text.contains("capability Clock: host-bound[runtime-provider-required]")
            && text.contains("contract_hash=sha256:")
            && text.contains("model_hash=sha256:"),
        "compiled target must name its host boundary and contract identity:\n{text}"
    );
    assert!(output_dir.join("src/provider_support.rs").is_file());

    #[cfg(feature = "wasm")]
    {
        let output = Command::new(aver_bin())
            .current_dir(&dir)
            .args([
                "run",
                "main.av",
                "--module-root",
                dir.to_str().expect("utf-8 dir"),
                "--wasm-gc",
            ])
            .output()
            .expect("run wasm-gc provider gate");
        let report = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert_ne!(
            output.status.code().unwrap_or(-1),
            0,
            "run --wasm-gc emitted an artifact with an unbound provider:\n{report}"
        );
        assert!(
            report.contains("error[capability-target-unsupported]")
                && report.contains("target `wasm-gc`")
                && report.contains("reason[host-import-adapter-not-generated]")
                && report.contains("required operations: Clock.now")
                && report.contains("contract_hash: sha256:")
                && report.contains("model_hash: sha256:"),
            "the wasm-gc run shortcut must share the compile-time provider gate:\n{report}"
        );
    }

    #[cfg(feature = "wasip2")]
    {
        let output = Command::new(aver_bin())
            .current_dir(&dir)
            .args([
                "run",
                "main.av",
                "--module-root",
                dir.to_str().expect("utf-8 dir"),
                "--wasip2",
            ])
            .output()
            .expect("run wasip2 provider gate");
        let report = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert_ne!(
            output.status.code().unwrap_or(-1),
            0,
            "run --wasip2 emitted a component with an unbound provider:\n{report}"
        );
        assert!(
            report.contains("error[capability-target-unsupported]")
                && report.contains("target `wasip2`")
                && report.contains("reason[wit-boundary-type-unsupported]")
                && report.contains("operation `Clock.now` result has Aver type `Int`")
                && report.contains("required operations: Clock.now")
                && report.contains("contract_hash: sha256:")
                && report.contains("model_hash: sha256:"),
            "the wasip2 run shortcut must share the compile-time provider gate:\n{report}"
        );
    }

    fs::write(
        dir.join("unused.av"),
        "\
module UnusedClient
    depends [Clock]
    exposes [main]

fn main() -> Int
    1
",
    )
    .expect("write unused.av");
    let (code, text) = run("compile", &dir, "unused.av");
    assert_eq!(
        code, 0,
        "an unused contract does not require a provider binding:\n{text}"
    );
}

#[test]
fn capability_given_and_declared_hostile_profile_share_the_oracle_path() {
    let dir = temp_dir("given-hostile");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [tick]
    effects [Clock.now]

fn seven(path: BranchPath, call: Int) -> Int
    7

fn tick() -> Int
    ! [Clock.now]
    Clock.now()

verify tick law followsProviderModel
    given clock: Clock.now = [seven]
    tick() => clock(BranchPath.Root, 0)
",
    )
    .expect("write main.av");

    let (code, text) = run("verify", &dir, "main.av");
    assert_eq!(code, 0, "declared capability given must verify:\n{text}");
    assert!(
        text.contains("followsProviderModel"),
        "the passing law must be named in the report:\n{text}"
    );

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args([
            "verify",
            "--module-root",
            dir.to_str().expect("utf-8 dir"),
            "--hostile",
            "--verbose",
            "main.av",
        ])
        .output()
        .expect("run hostile verify");
    let hostile = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_ne!(
        out.status.code().unwrap_or(-1),
        0,
        "hostile profile unexpectedly preserved the law:\n{hostile}"
    );
    assert!(
        hostile.contains("Clock.zero"),
        "the report must identify the declared hostile profile:\n{hostile}"
    );
}

#[test]
fn proof_registers_dependency_capabilities_before_duplicate_given_checks() {
    let dir = temp_dir("proof-dependency-registry");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    effects [Clock.now]

fn seven(path: BranchPath, call: Int) -> Int
    7

fn tick() -> Int
    ! [Clock.now]
    Clock.now()

verify tick law duplicateProvider
    given first: Clock.now = [seven]
    given second: Clock.now = [seven]
    tick() => 7
",
    )
    .expect("write main.av");

    let (code, text) = run("proof", &dir, "main.av");
    assert_ne!(
        code, 0,
        "proof export missed the depends-only capability classification:\n{text}"
    );
    assert!(
        text.contains("2 `given` bindings for the same effect 'Clock.now'"),
        "dependency capability must hit the same duplicate-given rejection as builtins:\n{text}"
    );
}

#[test]
fn direct_and_independent_calls_share_dispatch_while_callbacks_are_rejected() {
    let dir = temp_dir("structural-dispatch");
    fs::write(
        dir.join("Source.av"),
        "\
module Source
    kind = capability
    semantics = effectful
    exposes [read]
    effects []

operation read() -> Result<Int, String>
    oracle = generative
    replay = recorded
",
    )
    .expect("write Source.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Source]
    exposes [direct, again, raw, unwrapped]
    effects [Source.read]

fn okSeven(path: BranchPath, call: Int) -> Result<Int, String>
    Result.Ok(7)

fn direct() -> Result<Int, String>
    ! [Source.read]
    Source.read()

fn again() -> Result<Int, String>
    ! [Source.read]
    Source.read()

fn raw() -> Tuple<Result<Int, String>, Result<Int, String>>
    ! [Source.read]
    (Source.read(), Source.read())!

fn unwrapped() -> Result<Tuple<Int, Int>, String>
    ! [Source.read]
    Result.Ok((Source.read(), Source.read())?!)

verify direct law structuralDispatch
    given source: Source.read = [okSeven]
    direct() => Result.Ok(7)

verify again law repeatedDirectDispatch
    given source: Source.read = [okSeven]
    again() => Result.Ok(7)

verify raw law independentRawDispatch
    given source: Source.read = [okSeven]
    raw() => (Result.Ok(7), Result.Ok(7))

verify unwrapped law independentUnwrapDispatch
    given source: Source.read = [okSeven]
    unwrapped() => Result.Ok((7, 7))
",
    )
    .expect("write main.av");

    let (code, text) = run("verify", &dir, "main.av");
    assert_eq!(
        code, 0,
        "capability dispatch drifted between direct, `!`, and `?!` calls:\n{text}"
    );

    fs::write(
        dir.join("bad_callback.av"),
        "\
module BadCallback
    depends [Source]
    effects [Source.read]

fn callOne(reader: Fn() -> Result<Int, String> ! [Source.read]) -> Result<Int, String>
    ! [Source.read]
    reader()

fn bad() -> Result<Int, String>
    ! [Source.read]
    callOne(Source.read)
",
    )
    .expect("write bad_callback.av");
    let (code, text) = run("check", &dir, "bad_callback.av");
    assert_ne!(
        code, 0,
        "a capability operation escaped as a value:\n{text}"
    );
    assert!(
        text.contains("Capability operation 'Source.read' is not a value"),
        "the rejection must identify the provider boundary rather than a generic Fn mismatch:\n{text}"
    );
}

#[test]
fn proof_registers_dependency_capabilities_and_pins_both_hashes() {
    let dir = temp_dir("proof-dependency");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [tick]
    effects [Clock.now]

fn tick() -> Int
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write main.av");

    let output = dir.join("proof-out");
    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args([
            "proof",
            "--module-root",
            dir.to_str().expect("utf-8 dir"),
            "--output",
            output.to_str().expect("utf-8 output"),
            "main.av",
        ])
        .output()
        .expect("run proof");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        out.status.code().unwrap_or(-1),
        0,
        "proof must classify capability dependencies without a separate --deps flag:\n{report}"
    );

    let clock_items = parse(VALID_EFFECT_CAPABILITY);
    let (registry, errors) =
        aver::capability::CapabilityRegistry::from_module("Clock", &clock_items);
    assert!(errors.is_empty(), "fixture contract errors: {errors:?}");
    let contract = registry.contracts().next().expect("Clock contract");
    let lean = collect_files_with_extension(&output, "lean");
    assert!(
        lean.contains(&format!("contract_hash = {}", contract.contract_hash)),
        "generated trust header must pin the provider ABI identity:\n{lean}"
    );
    assert!(
        lean.contains(&format!("model_hash    = {}", contract.model_hash)),
        "generated trust header must pin the proof-model identity:\n{lean}"
    );
    assert!(
        lean.contains("Provider implementations are outside the theorem"),
        "the external provider trust boundary must be explicit:\n{lean}"
    );
}

#[test]
fn pure_capability_is_effect_free_but_still_provider_bound_and_proof_opaque() {
    let dir = temp_dir("pure-proof");
    fs::write(dir.join("Digest.av"), VALID_PURE_CAPABILITY).expect("write Digest.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Digest]
    exposes [same]

fn same(text: String) -> Bool
    Digest.digest(text) == Digest.digest(text)

verify same law deterministicProvider
    given text: String = [\"abc\"]
    same(text) holds
",
    )
    .expect("write main.av");

    let (code, text) = run("check", &dir, "main.av");
    assert_eq!(
        code, 0,
        "pure provider calls must not require an effect declaration:\n{text}"
    );

    let output = dir.join("lean-out");
    let mut proof_command = Command::new(aver_bin());
    proof_command.current_dir(&dir).args([
        "proof",
        "--module-root",
        dir.to_str().expect("utf-8 dir"),
        "--output",
        output.to_str().expect("utf-8 output"),
    ]);
    if tool_available("lake") {
        proof_command.arg("--check");
    }
    let proof = proof_command
        .arg("main.av")
        .output()
        .expect("run pure capability proof");
    let proof_report = format!(
        "{}{}",
        String::from_utf8_lossy(&proof.stdout),
        String::from_utf8_lossy(&proof.stderr)
    );
    assert_eq!(
        proof.status.code().unwrap_or(-1),
        0,
        "Lean must emit and, when lake is available, check the opaque deterministic provider model:\n{proof_report}"
    );
    let lean = collect_files_with_extension(&output, "lean");
    assert!(lean.contains("opaque Context : Type"), "{lean}");
    assert!(lean.contains("opaque digest : String → String"), "{lean}");
    assert!(
        lean.contains("Pure operations are represented as opaque"),
        "the trust header must state the exact pure-provider assumption:\n{lean}"
    );

    fs::write(
        dir.join("run.av"),
        "\
module Runner
    depends [Digest]
    exposes [main]

fn main() -> String
    Digest.digest(\"abc\")
",
    )
    .expect("write run.av");
    let (code, text) = run("run", &dir, "run.av");
    assert_ne!(
        code, 0,
        "pure capability unexpectedly found a provider:\n{text}"
    );
    assert!(
        text.contains("capability provider missing for 'Digest.digest'"),
        "pure and effectful operations must fail at the same provider boundary:\n{text}"
    );

    let dafny_output = dir.join("dafny-out");
    let dafny = Command::new(aver_bin())
        .current_dir(&dir)
        .args([
            "proof",
            "--backend",
            "dafny",
            "--module-root",
            dir.to_str().expect("utf-8 dir"),
            "--output",
            dafny_output.to_str().expect("utf-8 output"),
            "main.av",
        ])
        .output()
        .expect("emit pure capability Dafny proof");
    let dafny_report = format!(
        "{}{}",
        String::from_utf8_lossy(&dafny.stdout),
        String::from_utf8_lossy(&dafny.stderr)
    );
    assert_eq!(
        dafny.status.code().unwrap_or(-1),
        0,
        "Dafny emission must accept the same pure contract:\n{dafny_report}"
    );
    let dafny = collect_files_with_extension(&dafny_output, "dfy");
    assert!(dafny.contains("type Context"), "{dafny}");
    assert!(
        dafny.contains("function digest(text: string): string"),
        "{dafny}"
    );
    assert!(dafny.contains("Aver_Digest.digest(text)"), "{dafny}");
}

#[test]
fn private_operation_stays_an_effect_identity_without_becoming_callable() {
    let dir = temp_dir("private-operation");
    fs::write(
        dir.join("Private.av"),
        "\
module Private
    kind = capability
    semantics = effectful
    exposes [public]
    effects [Private.secret]

operation secret() -> Int
    oracle = generative
    replay = recorded

fn public() -> Int
    ! [Private.secret]
    Private.secret()
",
    )
    .expect("write Private.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Private]
    exposes [usePublic]
    effects [Private.secret]

fn usePublic() -> Int
    ! [Private.secret]
    Private.public()
",
    )
    .expect("write main.av");

    let (code, text) = run("check", &dir, "main.av");
    assert_eq!(
        code, 0,
        "the private operation name must remain declarable as a transitive effect:\n{text}"
    );

    fs::write(
        dir.join("bad.av"),
        "\
module Bad
    depends [Private]
    effects [Private.secret]

fn steal() -> Int
    ! [Private.secret]
    Private.secret()
",
    )
    .expect("write bad.av");
    let (code, text) = run("check", &dir, "bad.av");
    assert_ne!(
        code, 0,
        "an unexposed operation became externally callable:\n{text}"
    );
    assert!(
        text.contains("Private.secret") && text.contains("not exposed"),
        "the diagnostic must distinguish visibility from a missing effect identity:\n{text}"
    );
}

#[test]
fn capability_resource_identity_is_unobservable_through_wrappers() {
    let dir = temp_dir("resource-identity");
    fs::write(
        dir.join("Tokens.av"),
        "\
module Tokens
    kind = capability
    semantics = effectful
    exposes [identity]
    exposes opaque [Connection]
    effects [Tokens.mint]

opaque Token

type Connection
    Open(Token)
    Closed

operation mint() -> Token
    oracle = generative
    replay = recorded

fn identity(connection: Connection) -> Connection
    connection
",
    )
    .expect("write Tokens.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Tokens]

fn equal(left: Tokens.Connection, right: Tokens.Connection) -> Bool
    left == right

fn count(index: Map<Tokens.Connection, Int>) -> Int
    Map.len(index)

fn contains(items: List<Tokens.Connection>, needle: Tokens.Connection) -> Bool
    List.contains(items, needle)
",
    )
    .expect("write main.av");

    let (code, text) = run("check", &dir, "main.av");
    assert_ne!(
        code, 0,
        "provider token identity became observable:\n{text}"
    );
    assert!(
        text.contains("Equality is not defined for capability resource values"),
        "a represented wrapper must inherit the token's no-equality rule:\n{text}"
    );
    assert!(
        text.contains("cannot be used as a Map key")
            || text.contains("map key type must be hashable"),
        "a represented wrapper must inherit the token's no-hash rule:\n{text}"
    );
    assert!(
        text.contains("'List.contains' is not defined for capability resource values"),
        "equality-bearing helpers must not reintroduce token identity:\n{text}"
    );
}

#[test]
fn resource_minting_oracle_receives_an_unconstrained_token() {
    let dir = temp_dir("resource-mint");
    fs::write(
        dir.join("Mint.av"),
        "\
module Mint
    kind = capability
    semantics = effectful
    exposes [succeeds]
    effects [Mint.mint]

opaque Token

operation mint() -> Result<Token, String>
    oracle = generative
    replay = recorded
    hostile = [accept]

fn accept(path: BranchPath, call: Int, fresh: Token) -> Result<Token, String>
    Result.Ok(fresh)

fn succeeds() -> Bool
    ! [Mint.mint]
    match Mint.mint()
        Result.Ok(_) -> true
        Result.Err(_) -> false

verify succeeds law providerMayMint
    given mint: Mint.mint = [accept]
    succeeds() holds
",
    )
    .expect("write Mint.av");

    let (code, text) = run("verify", &dir, "Mint.av");
    assert_eq!(
        code, 0,
        "the VM must supply a representationless fresh token to the oracle stub:\n{text}"
    );

    let output = dir.join("proof-out");
    let mut proof_command = Command::new(aver_bin());
    proof_command.current_dir(&dir).args([
        "proof",
        "--module-root",
        dir.to_str().expect("utf-8 dir"),
        "--output",
        output.to_str().expect("utf-8 output"),
    ]);
    if tool_available("lake") {
        proof_command.arg("--check");
    }
    let proof = proof_command
        .arg("Mint.av")
        .output()
        .expect("run resource proof");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&proof.stdout),
        String::from_utf8_lossy(&proof.stderr)
    );
    assert_eq!(
        proof.status.code().unwrap_or(-1),
        0,
        "proof lifting must emit and, when lake is available, check the same unconstrained token passed to the oracle:\n{report}"
    );
    let lean = collect_files_with_extension(&output, "lean");
    assert!(
        lean.contains("capFresh_Mint_mint"),
        "the emitted Lean law lost its hidden resource witness:\n{lean}"
    );

    let dafny_output = dir.join("proof-dafny-out");
    let has_dafny = tool_available("dafny");
    let mut dafny_command = Command::new(aver_bin());
    dafny_command.current_dir(&dir).args([
        "proof",
        "--backend",
        "dafny",
        "--module-root",
        dir.to_str().expect("utf-8 dir"),
        "--output",
        dafny_output.to_str().expect("utf-8 output"),
    ]);
    if has_dafny {
        dafny_command.arg("--check");
    }
    let dafny = dafny_command
        .arg("Mint.av")
        .output()
        .expect("run resource Dafny proof");
    let dafny_report = format!(
        "{}{}",
        String::from_utf8_lossy(&dafny.stdout),
        String::from_utf8_lossy(&dafny.stderr)
    );
    if !has_dafny {
        assert_eq!(
            dafny.status.code().unwrap_or(-1),
            0,
            "Dafny emission failed without invoking an external verifier:\n{dafny_report}"
        );
    }
    let dafny_source = collect_files_with_extension(&dafny_output, "dfy");
    assert!(
        dafny_source.contains("capFresh_Mint_mint"),
        "the emitted Dafny law lost its hidden resource witness:\n{dafny_source}"
    );
    if has_dafny {
        assert!(
            dafny_report.contains("verified, 0 errors"),
            "Dafny samples and universal lemmas must bind the same unconstrained token:\n{dafny_report}"
        );
        assert!(
            !dafny_report.contains("resolution/type errors detected"),
            "the resource parameter escaped its declaration scope:\n{dafny_report}"
        );
    }
}

#[test]
fn replay_obeys_recorded_suppressed_and_reissued_capability_semantics() {
    use aver::replay::{
        CapabilityProvenance, EffectRecord, JsonValue, RecordedOutcome, SessionRecording,
        session_recording_to_string_pretty,
    };

    fn provenance(module: &str, source: &str) -> Vec<CapabilityProvenance> {
        let items = parse(source);
        let (registry, errors) = aver::capability::CapabilityRegistry::from_module(module, &items);
        assert!(errors.is_empty(), "capability fixture errors: {errors:?}");
        let contract = registry.contract(module).expect("fixture contract");
        vec![CapabilityProvenance {
            capability: module.to_string(),
            contract_hash: contract.contract_hash.clone(),
            model_hash: contract.model_hash.clone(),
            provider: "test.recording/provider".to_string(),
            fingerprint: "fixture-v1".to_string(),
        }]
    }

    fn recording(
        dir: &std::path::Path,
        effect_type: &str,
        effect_output: JsonValue,
        program_output: JsonValue,
    ) -> SessionRecording {
        SessionRecording {
            schema_version: 1,
            request_id: "capability-replay".to_string(),
            timestamp: "2026-08-17T00:00:00Z".to_string(),
            program_file: "main.av".to_string(),
            module_root: dir.to_string_lossy().into_owned(),
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
            capabilities: Vec::new(),
            effects: vec![EffectRecord {
                seq: 1,
                effect_type: effect_type.to_string(),
                args: vec![],
                outcome: RecordedOutcome::Value(effect_output),
                caller_fn: "main".to_string(),
                source_line: 7,
                group_id: None,
                branch_path: None,
                effect_occurrence: None,
            }],
            output: RecordedOutcome::Value(program_output),
        }
    }

    let dir = temp_dir("replay-recorded");
    fs::write(dir.join("Clock.av"), VALID_EFFECT_CAPABILITY).expect("write Clock.av");
    fs::write(
        dir.join("main.av"),
        "\
module Client
    depends [Clock]
    exposes [main]
    effects [Clock.now]

fn main() -> Int
    ! [Clock.now]
    Clock.now()
",
    )
    .expect("write main.av");
    let mut session = recording(&dir, "Clock.now", JsonValue::Int(123), JsonValue::Int(123));
    session.capabilities = provenance("Clock", VALID_EFFECT_CAPABILITY);
    fs::write(
        dir.join("recorded.json"),
        session_recording_to_string_pretty(&session),
    )
    .expect("write recording");
    let replay = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["replay", "recorded.json", "--test", "--check-args"])
        .output()
        .expect("replay recorded capability");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert_eq!(
        replay.status.code().unwrap_or(-1),
        0,
        "recorded generative capability must replay without a live provider:\n{report}"
    );

    let suppressed = temp_dir("replay-suppressed");
    fs::write(
        suppressed.join("Log.av"),
        "\
module Log
    kind = capability
    semantics = effectful
    exposes [flush]
    effects []

operation flush() -> Unit
    oracle = output
    replay = suppressed
",
    )
    .expect("write Log.av");
    fs::write(
        suppressed.join("main.av"),
        "\
module Client
    depends [Log]
    exposes [main]
    effects [Log.flush]

fn main() -> Int
    ! [Log.flush]
    Log.flush()
    42
",
    )
    .expect("write main.av");
    let mut session = recording(
        &suppressed,
        "Log.flush",
        JsonValue::Null,
        JsonValue::Int(42),
    );
    let suppressed_source = fs::read_to_string(suppressed.join("Log.av")).expect("read Log.av");
    session.capabilities = provenance("Log", &suppressed_source);
    fs::write(
        suppressed.join("suppressed.json"),
        session_recording_to_string_pretty(&session),
    )
    .expect("write suppressed recording");
    let replay = Command::new(aver_bin())
        .current_dir(&suppressed)
        .args(["replay", "suppressed.json", "--test"])
        .output()
        .expect("replay suppressed capability");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert_eq!(
        replay.status.code().unwrap_or(-1),
        0,
        "suppressed output must consume its trace without re-emission:\n{report}"
    );

    fs::write(
        suppressed.join("Log.av"),
        suppressed_source.replace("replay = suppressed", "replay = reissued"),
    )
    .expect("write reissued Log.av");
    let reissued_source = fs::read_to_string(suppressed.join("Log.av")).expect("read Log.av");
    session.capabilities = provenance("Log", &reissued_source);
    fs::write(
        suppressed.join("suppressed.json"),
        session_recording_to_string_pretty(&session),
    )
    .expect("rewrite reissued recording provenance");
    let replay = Command::new(aver_bin())
        .current_dir(&suppressed)
        .args(["replay", "suppressed.json", "--test"])
        .output()
        .expect("replay reissued capability");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert_ne!(
        replay.status.code().unwrap_or(-1),
        0,
        "reissued output was silently treated as suppression:\n{report}"
    );
    assert!(
        report.contains("reissued replay event 'Log.flush' requires a live provider"),
        "reissued output must require a live provider:\n{report}"
    );
}
