//! A `Map` key type must have an ordering, and `aver check` says so.
//!
//! These are the claims `tests/map_iteration_order.rs` used to make about
//! programs that can no longer be written. A map iterates its entries sorted by
//! key — on the VM, in compiled Rust, and in the exported proof model — so the
//! key needs a total order every one of those can state. `Int`, `String` and
//! `Bool` have one. `Float` does not (IEEE 754 total order puts a NaN outside
//! the finite range, and the sign of a NaN produced by an invalid operation is
//! not fixed), and tuples, lists, records and variants were being ordered by
//! their PRINTED form, which is not a property of the value.
//!
//! What used to be here instead. A float-keyed map wide enough to reach the
//! real sort, holding a NaN, was a live crash: the shared key comparator was
//! not a total order over the NaN-boxed representation, and the standard
//! library detects the cycle and aborts the process. A tuple-keyed
//! iteration-order law was exported with a refusal comment naming the printed
//! representation. Neither program typechecks now, so the refusal moved to the
//! front door and the exporter's own gate stays behind it as a backstop — see
//! `MODELLED_MAP_KEY_TYPES` in `src/codegen/common.rs`, and the comparator's
//! own defence-in-depth tests in `src/types/map.rs`.
#![cfg(feature = "runtime")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

/// The one sentence every door emits. Asserting on the slug alone would pass
/// on a refusal that had stopped saying what to do instead.
const ORDERING_MESSAGE: &str = "a Map key type must have an ordering";
const REPAIR: &str = "use Int, String or Bool as the key";

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("expected to create a temp module directory");
    dir
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// Run `aver check` over `source` written into its own directory, and return
/// everything the command said.
fn check(prefix: &str, source: &str) -> (std::process::Output, String) {
    check_modules(prefix, &[("main.av", source)], "main.av")
}

/// [`check`] for a program that spans several modules.
fn check_modules(
    prefix: &str,
    files: &[(&str, &str)],
    entry: &str,
) -> (std::process::Output, String) {
    let dir = temp_dir(prefix);
    for (name, source) in files {
        std::fs::write(dir.join(name), source).expect("expected to write the module");
    }
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "check",
            dir.join(entry).to_str().expect("utf-8 temp path"),
            "--module-root",
            dir.to_str().expect("utf-8 temp path"),
        ])
        .output()
        .expect("expected the `aver` binary to run");
    let _ = std::fs::remove_dir_all(&dir);
    let said = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (out, said)
}

fn assert_refused(prefix: &str, source: &str, key: &str) {
    let (out, said) = check(prefix, source);
    assert!(
        !out.status.success(),
        "`{key}` in key position must be a type error:\n{}",
        format_output(&out)
    );
    assert!(
        said.contains("error[map-key-unordered]"),
        "the refusal must carry its own slug so it can be silenced and \
         classified:\n{said}"
    );
    assert!(
        said.contains(&format!("{ORDERING_MESSAGE}, and `{key}` has none")),
        "the refusal must name the key type it turned down:\n{said}"
    );
    assert!(
        said.contains(REPAIR),
        "the refusal must say what to write instead:\n{said}"
    );
}

fn assert_accepted(prefix: &str, source: &str) {
    let (out, said) = check(prefix, source);
    assert!(
        out.status.success(),
        "this program is legal and must check clean:\n{}",
        format_output(&out)
    );
    assert!(
        !said.contains("map-key-unordered"),
        "nothing here is in key position:\n{said}"
    );
}

const HEADER: &str = concat!(
    "module Ban\n",
    "    intent =\n",
    "        \"A map keyed on a type with no ordering.\"\n",
    "        \"The key decides the order every backend iterates in, so it needs one.\"\n",
    "    effects []\n\n",
);

/// A `Float` key is refused where the old suite kept a float-keyed map wide
/// enough to reach the sort.
///
/// That fixture existed because ordering float keys by raw bit pattern once a
/// NaN is involved is not a total order — a NaN compared above `1.0` and below
/// `-1.0` while `-1.0 < 1.0` — and the standard library aborts the process on
/// an input that wide. The map that provoked it cannot be declared any more.
#[test]
fn a_float_key_is_refused_in_a_signature() {
    assert_refused(
        "aver-ban-float-signature",
        &format!(
            "{HEADER}fn keyCount(m: Map<Float, Int>) -> Int\n\
             \x20   ? \"How many keys the map holds.\"\n\
             \x20   Map.len(m)\n\n\
             verify keyCount\n\
             \x20   keyCount({{}}) => 0\n"
        ),
        "Float",
    );
}

/// A `Tuple` key is refused where the old suite exported an iteration-order
/// law with a comment about the printed representation.
///
/// The comparator's catch-all ordered such a key by rendering it, and a
/// position in the sequence that comes from rendered syntax is not
/// reconstructible from the value. That is the whole family — tuples, lists,
/// records, variants — and each member has its own test below, because the
/// checker reaches them through different arms and one of them standing for
/// the rest is an assumption, not a fact.
#[test]
fn a_tuple_key_is_refused_in_a_signature() {
    assert_refused(
        "aver-ban-tuple-signature",
        &format!(
            "{HEADER}fn keyCount(m: Map<Tuple<Int, Int>, Int>) -> Int\n\
             \x20   ? \"How many keys the map holds.\"\n\
             \x20   Map.len(m)\n\n\
             verify keyCount\n\
             \x20   keyCount({{}}) => 0\n"
        ),
        "Tuple<Int, Int>",
    );
}

/// A map LITERAL decides its own key type, and the same rule reaches it.
///
/// The old fixture wrote `Map.keys({2.0 => 1, 1.0 => 2})` with no annotation
/// anywhere, which is how the key type reached the exporter without ever
/// appearing in a signature.
#[test]
fn a_float_key_is_refused_in_a_map_literal() {
    assert_refused(
        "aver-ban-float-literal",
        &format!(
            "{HEADER}fn built() -> Int\n\
             \x20   ? \"How many keys the literal holds.\"\n\
             \x20   Map.len({{2.0 => 1, 1.0 => 2}})\n\n\
             verify built\n\
             \x20   built() => 2\n"
        ),
        "Float",
    );
}

/// `Map.set` decides a key type too, on an empty literal that had none.
#[test]
fn a_float_key_is_refused_through_map_set() {
    assert_refused(
        "aver-ban-float-set",
        &format!(
            "{HEADER}fn built() -> Int\n\
             \x20   ? \"How many keys the map holds after one write.\"\n\
             \x20   Map.len(Map.set({{}}, 1.0, 9))\n\n\
             verify built\n\
             \x20   built() => 1\n"
        ),
        "Float",
    );
}

/// `Map.fromList` is the third door onto the same decision.
#[test]
fn a_float_key_is_refused_through_map_from_list() {
    assert_refused(
        "aver-ban-float-from-list",
        &format!(
            "{HEADER}fn built() -> Int\n\
             \x20   ? \"How many keys the map holds.\"\n\
             \x20   Map.len(Map.fromList([(1.0, 9)]))\n\n\
             verify built\n\
             \x20   built() => 1\n"
        ),
        "Float",
    );
}

/// A `verify` given is an annotation the user writes, and it is checked.
///
/// Every refused law in the old fixtures carried one — `given m:
/// Map<Float, Int> = [{1.0 => 3, 2.0 => 5}]` — so a rule that stopped at
/// function signatures would leave the shape those laws were written in
/// legal.
#[test]
fn a_float_key_is_refused_in_a_verify_given() {
    assert_refused(
        "aver-ban-float-given",
        &format!(
            "{HEADER}fn howMany(n: Int) -> Int\n\
             \x20   ? \"Counts up.\"\n\
             \x20   n + 1\n\n\
             verify howMany law countsThem\n\
             \x20   given m: Map<Float, Int> = [{{1.0 => 3}}]\n\
             \x20   given n: Int = [1, 2]\n\
             \x20   howMany(n) => n + 1\n"
        ),
        "Float",
    );
}

/// A key inside a record FIELD is refused, wherever the record is compared.
///
/// The old colliding-type fixture put a `Map<Float, Int>` in `Entry.scores`
/// precisely because no signature named the map: it was reached by following
/// the record into its declaration. The declaration itself is the door now.
#[test]
fn a_float_key_is_refused_in_a_record_field() {
    assert_refused(
        "aver-ban-float-field",
        &format!(
            "{HEADER}record Entry\n\
             \x20   label: String\n\
             \x20   scores: Map<Float, Int>\n\n\
             fn labelOf(e: Entry) -> String\n\
             \x20   ? \"The entry's label.\"\n\
             \x20   e.label\n\n\
             verify labelOf\n\
             \x20   labelOf(Entry(label = \"x\", scores = {{}})) => \"x\"\n"
        ),
        "Float",
    );
}

/// A dependency's signature is checked when it is registered, so the key type
/// cannot arrive from one module over.
///
/// The old cross-module fixture reached its observer through
/// `MapKeys.floatValues(m)`, whose `Map<Float, Int>` was declared in the other
/// file. Checking only the entry module would leave that spelling writable and
/// make the refusal depend on which file the user pointed at.
#[test]
fn a_float_key_is_refused_in_a_dependency_signature() {
    let dep = concat!(
        "module MapKeys\n",
        "    intent =\n",
        "        \"Reads a float-keyed map for a caller in another module.\"\n",
        "    effects []\n\n",
        "fn floatValues(m: Map<Float, Int>) -> Int\n",
        "    ? \"How many values the map holds.\"\n",
        "    Map.len(m)\n\n",
        "verify floatValues\n",
        "    floatValues({}) => 0\n",
    );
    let main = concat!(
        "module Main\n",
        "    intent =\n",
        "        \"Calls one module over.\"\n",
        "        \"The key type is declared there, not here.\"\n",
        "    effects []\n",
        "    depends [MapKeys]\n\n",
        "fn count(n: Int) -> Int\n",
        "    ? \"Counts up.\"\n",
        "    n + 1\n\n",
        "verify count\n",
        "    count(1) => 2\n",
    );
    let (out, said) = check_modules(
        "aver-ban-float-dependency",
        &[("mapkeys.av", dep), ("main.av", main)],
        "main.av",
    );
    assert!(
        !out.status.success(),
        "a dependency's own signature must be refused too:\n{}",
        format_output(&out)
    );
    assert!(
        said.contains(&format!("{ORDERING_MESSAGE}, and `Float` has none")),
        "the refusal must reach across the module boundary:\n{said}"
    );
}

/// The rule is about the KEY, and only about the key.
///
/// `Float` is still a perfectly good map value, list element and parameter —
/// nothing there is ordered by the map. A rule that reached further would take
/// working programs down with it, and the exporter's map model would lose the
/// float values it already carries.
#[test]
fn float_stays_legal_everywhere_except_the_key() {
    assert_accepted(
        "aver-ban-float-value",
        &format!(
            "{HEADER}fn total(m: Map<String, Float>, xs: List<Float>, f: Float) -> Int\n\
             \x20   ? \"How many entries the map and the list hold between them, once the sample is positive.\"\n\
             \x20   match f > 0.0\n\
             \x20       true -> Map.len(m) + List.len(xs)\n\
             \x20       false -> 0\n\n\
             verify total\n\
             \x20   total({{}}, [], 1.0) => 0\n\
             \x20   total({{\"a\" => 1.0}}, [2.0], 1.0) => 2\n\
             \x20   total({{\"a\" => 1.0}}, [2.0], -1.0) => 0\n"
        ),
    );
}

/// The three key types with an ordering stay legal, in every door above.
#[test]
fn ordered_key_types_stay_legal() {
    for key in ["Int", "String", "Bool"] {
        assert_accepted(
            &format!("aver-ban-ok-{}", key.to_lowercase()),
            &format!(
                "{HEADER}fn keyCount(m: Map<{key}, Int>) -> Int\n\
                 \x20   ? \"How many keys the map holds.\"\n\
                 \x20   Map.len(m)\n\n\
                 verify keyCount\n\
                 \x20   keyCount({{}}) => 0\n"
            ),
        );
    }
}

/// Reading a map decides its key type too, when the map itself is an empty
/// literal.
///
/// `Map.has({}, 1.0)` carries no annotation and no `Map.set`: the literal
/// arrives with its key still unresolved and the call is what fixes it. The
/// first cut of this rule checked only the builtins that obviously decide a
/// key and let these three through — `aver check` exited 0 on a program
/// holding a float-keyed map.
#[test]
fn a_float_key_is_refused_through_a_read_of_an_empty_map() {
    for (name, call) in [
        ("has", "Map.has({}, 1.0)"),
        ("get", "Map.has({}, 1.0)"),
        ("remove", "Map.len(Map.remove({}, 1.0))"),
    ] {
        let body = if name == "remove" { call } else { call };
        let ret = if name == "remove" { "Int" } else { "Bool" };
        let expected = if name == "remove" { "0" } else { "false" };
        assert_refused(
            &format!("aver-ban-read-{name}"),
            &format!(
                "{HEADER}fn probe() -> {ret}\n\
                 \x20   ? \"Whether an empty map holds a float key.\"\n\
                 \x20   {body}\n\n\
                 verify probe\n\
                 \x20   probe() => {expected}\n"
            ),
            "Float",
        );
    }
}

/// A record in key position is refused.
///
/// It reaches the checker through a different arm than a tuple does — a named
/// type resolved against the program's own declarations — and the wasm-gc
/// tests that used to exercise a record key were deleted with the programs
/// they tested, so this is the only thing holding that arm.
#[test]
fn a_record_key_is_refused() {
    assert_refused(
        "aver-ban-record-key",
        &format!(
            "{HEADER}record Point\n\
             \x20   x: Int\n\
             \x20   y: Int\n\n\
             fn keyCount(m: Map<Point, Int>) -> Int\n\
             \x20   ? \"How many keys the map holds.\"\n\
             \x20   Map.len(m)\n\n\
             verify keyCount\n\
             \x20   keyCount({{}}) => 0\n"
        ),
        "Point",
    );
}

/// A variant in key position is refused, through the same arm as a record and
/// a different one from a tuple.
#[test]
fn a_variant_key_is_refused() {
    assert_refused(
        "aver-ban-variant-key",
        &format!(
            "{HEADER}type Colour\n\
             \x20   Red\n\
             \x20   Green\n\n\
             fn keyCount(m: Map<Colour, Int>) -> Int\n\
             \x20   ? \"How many keys the map holds.\"\n\
             \x20   Map.len(m)\n\n\
             verify keyCount\n\
             \x20   keyCount({{}}) => 0\n"
        ),
        "Colour",
    );
}

/// A capability operation's signature is a door of its own.
///
/// It is validated before the typechecker's annotation walk ever sees it, so
/// it carries its own copy of the rule — and its own diagnostic, which names
/// the operation.
#[test]
fn a_float_key_is_refused_in_a_capability_operation() {
    let dir = temp_dir("aver-ban-capability");
    std::fs::write(
        dir.join("Kv.av"),
        "module Kv\n\
         \x20   kind = capability\n\
         \x20   semantics = pure\n\
         \x20   exposes [index]\n\n\
         operation index(values: Map<Float, Int>) -> Int\n",
    )
    .expect("expected to write the capability module");
    std::fs::write(
        dir.join("main.av"),
        "module Main\n\
         \x20   intent = \"Names a capability whose operation is keyed on a Float.\"\n\
         \x20   depends [Kv]\n\
         \x20   exposes [main]\n\
         \x20   effects [Kv.index]\n\n\
         fn main() -> Int\n\
         \x20   ? \"How many entries the host holds.\"\n\
         \x20   ! [Kv.index]\n\
         \x20   Kv.index({})\n",
    )
    .expect("expected to write the client module");

    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "check",
            dir.join("main.av").to_str().expect("utf-8 path"),
            "--module-root",
            dir.to_str().expect("utf-8 path"),
        ])
        .output()
        .expect("expected the `aver` binary to run");
    let said = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        !out.status.success(),
        "a capability operation keyed on a Float must be refused:\n{said}"
    );
    assert!(
        said.contains(ORDERING_MESSAGE) && said.contains("Kv.index"),
        "the refusal must name the operation it turned down:\n{said}"
    );
}

/// The language's list of key types, the proof exporter's backstop, and the
/// Lean model's orderings are three separate statements of the same fact, and
/// this holds them to each other.
///
/// They are deliberately not one constant read three times: the exporter is
/// the backstop for the language rule, and a backstop that reads the thing it
/// guards widens whenever that widens. What must not happen is a fourth key
/// type entering the language while the model has no ordering for it — the
/// prelude's priority-50 catch-all would answer, and an iteration-order claim
/// over that key would export as a theorem about the order the entries were
/// written in.
#[test]
fn the_language_the_exporter_and_the_model_agree_on_which_keys_order() {
    fn read(path: &str) -> String {
        std::fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(path))
            .unwrap_or_else(|e| panic!("expected to read {path}: {e}"))
    }

    let language = read("src/types/map_key.rs");
    let exporter = read("src/codegen/common.rs");
    let prelude = read("src/codegen/lean/prelude.rs");

    let listed = |text: &str, marker: &str| -> Vec<String> {
        let line = text
            .lines()
            .find(|l| l.contains(marker))
            .unwrap_or_else(|| panic!("expected {marker} to still be declared on one line"));
        line.split('"')
            .skip(1)
            .step_by(2)
            .map(str::to_string)
            .collect()
    };

    let admitted = listed(&language, "pub const ORDERED_MAP_KEY_TYPES");
    let modelled = listed(&exporter, "const MODELLED_MAP_KEY_TYPES");
    assert_eq!(
        admitted, modelled,
        "the exporter's backstop must admit exactly the key types the language does —          widen one and you must decide, deliberately, to widen the other"
    );
    assert!(
        !exporter.contains("MODELLED_MAP_KEY_TYPES: &[&str] = crate::types::"),
        "the backstop must keep its own list rather than read the language's, or it          stops being a second opinion"
    );

    for key in &admitted {
        // `String` is spelled `String` on both sides; `Int` and `Bool` too.
        let instance = format!("AverKeyOrder {key}");
        assert!(
            prelude.contains(&instance),
            "the proof model has no ordering for `{key}`, so a claim about the order a              `{key}`-keyed map iterates in would be exported against the prelude's              catch-all instance, which reports written order"
        );
    }
}
