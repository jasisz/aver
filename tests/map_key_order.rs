//! A map key must have an order, and every backend must state the same one.
//!
//! A map iterates its entries sorted by key. That order is only meaningful if
//! the VM, the compiled binary and the exported proof model agree on it, so
//! the key type has to be one all three can order. Composite keys — a record,
//! a variant, a list, a tuple — order out of their parts and are perfectly
//! good keys. `Float` is not: a NaN has no place in the finite range, the
//! generated Rust cannot even build such a map (`f64` is neither `Eq` nor
//! `Hash`), and the proof model has no faithful counterpart for the runtime's
//! total order. A `Map` and a `Vector` have no order of their own either.
//!
//! A record orders by FIELD NAME and a variant by CONSTRUCTOR NAME, not by
//! the order either was declared in: declaration order is not observable
//! anywhere else in Aver — a record is built and read by name, there is no
//! positional pattern, and nothing renders one implicitly — so ordering by it
//! would make reordering two fields silently change how every map on that key
//! iterates.
#![cfg(feature = "runtime")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const NO_ORDER: &str = "the key type has to order";

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("expected to create a temp module directory");
    dir
}

fn aver(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(repo_root())
        .args(args)
        .output()
        .expect("expected the `aver` binary to run")
}

fn said(out: &std::process::Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

const HEADER: &str = concat!(
    "module Key\n",
    "    intent =\n",
    "        \"A map keyed on a type the backends have to agree how to order.\"\n",
    "    effects []\n\n",
);

fn check_source(prefix: &str, source: &str) -> std::process::Output {
    let dir = temp_dir(prefix);
    let file = dir.join("main.av");
    std::fs::write(&file, source).expect("expected to write the module");
    let out = aver(&[
        "check",
        file.to_str().expect("utf-8 path"),
        "--module-root",
        dir.to_str().expect("utf-8 path"),
    ]);
    let _ = std::fs::remove_dir_all(&dir);
    out
}

fn assert_refused(prefix: &str, source: &str, what: &str) {
    let out = check_source(prefix, source);
    let text = said(&out);
    assert!(
        !out.status.success(),
        "`{what}` in key position must be a type error:\n{text}"
    );
    assert!(
        text.contains(NO_ORDER) && text.contains(&format!("`{what}`")),
        "the refusal must name what it could not order:\n{text}"
    );
}

fn assert_accepted(prefix: &str, source: &str) {
    let out = check_source(prefix, source);
    let text = said(&out);
    assert!(
        out.status.success(),
        "this key type orders and must be accepted:\n{text}"
    );
    assert!(
        !text.contains(NO_ORDER),
        "nothing here is unorderable:\n{text}"
    );
}

/// Every door that decides a key type carries the rule.
///
/// The list is the point: a key type is fixed in more places than an
/// annotation, and one door left out is a program that typechecks and then
/// cannot be compiled.
#[test]
fn every_door_that_decides_a_key_type_refuses_a_float() {
    let doors: &[(&str, String)] = &[
        (
            "signature",
            format!(
                "{HEADER}fn count(m: Map<Float, Int>) -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(m)\n"
            ),
        ),
        (
            "literal",
            format!(
                "{HEADER}fn count() -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len({{1.5 => 1}})\n"
            ),
        ),
        (
            "map-set",
            format!(
                "{HEADER}fn count() -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(Map.set({{}}, 1.5, 1))\n"
            ),
        ),
        (
            "from-list",
            format!(
                "{HEADER}fn count() -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(Map.fromList([(1.5, 1)]))\n"
            ),
        ),
        (
            "read-of-empty-map",
            format!(
                "{HEADER}fn holds() -> Bool\n\
                 \x20   ? \"Whether an empty map holds a float key.\"\n\
                 \x20   Map.has({{}}, 1.5)\n"
            ),
        ),
        (
            "record-field",
            format!(
                "{HEADER}record Sample\n\
                 \x20   label: String\n\
                 \x20   weight: Float\n\n\
                 fn count(m: Map<Sample, Int>) -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(m)\n"
            ),
        ),
        (
            "variant-payload",
            format!(
                "{HEADER}type Reading\n\
                 \x20   Exact(Float)\n\
                 \x20   Missing\n\n\
                 fn count(m: Map<Reading, Int>) -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(m)\n"
            ),
        ),
        (
            "inside-a-list-key",
            format!(
                "{HEADER}fn count(m: Map<List<Float>, Int>) -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(m)\n"
            ),
        ),
    ];
    for (door, source) in doors {
        assert_refused(&format!("aver-key-float-{door}"), source, "Float");
    }
}

/// A map and a vector have no order of their own, so neither keys a map.
#[test]
fn a_container_with_no_order_of_its_own_cannot_be_a_key() {
    assert_refused(
        "aver-key-map",
        &format!(
            "{HEADER}fn count(m: Map<Map<String, Int>, Int>) -> Int\n\
             \x20   ? \"How many entries.\"\n\
             \x20   Map.len(m)\n"
        ),
        "Map",
    );
    assert_refused(
        "aver-key-vector",
        &format!(
            "{HEADER}fn count(m: Map<Vector<Int>, Int>) -> Int\n\
             \x20   ? \"How many entries.\"\n\
             \x20   Map.len(m)\n"
        ),
        "Vector",
    );
}

/// The types a program actually keys maps on stay legal — including the
/// user's own.
///
/// Banning a record or a variant here would take the newtype pattern the
/// language teaches (`examples/core/order_total.av`) and make it unusable as
/// a key, which is most of what a map is for.
#[test]
fn a_key_built_out_of_ordered_parts_is_accepted() {
    assert_accepted(
        "aver-key-record",
        &format!(
            "{HEADER}record Point\n\
             \x20   x: Int\n\
             \x20   y: Int\n\n\
             fn count(m: Map<Point, String>) -> Int\n\
             \x20   ? \"How many entries.\"\n\
             \x20   Map.len(m)\n"
        ),
    );
    assert_accepted(
        "aver-key-variant",
        &format!(
            "{HEADER}type Colour\n\
             \x20   Red\n\
             \x20   Blue\n\n\
             fn count(m: Map<Colour, Int>) -> Int\n\
             \x20   ? \"How many entries.\"\n\
             \x20   Map.len(m)\n"
        ),
    );
    for key in ["Int", "String", "Bool", "List<Int>", "Tuple<Int, String>"] {
        assert_accepted(
            "aver-key-builtin",
            &format!(
                "{HEADER}fn count(m: Map<{key}, Int>) -> Int\n\
                 \x20   ? \"How many entries.\"\n\
                 \x20   Map.len(m)\n"
            ),
        );
    }
}

/// `Float` is refused as a KEY and stays legal everywhere else.
#[test]
fn float_stays_legal_as_a_value() {
    assert_accepted(
        "aver-key-float-value",
        &format!(
            "{HEADER}fn total(m: Map<String, Float>, xs: List<Float>) -> Int\n\
             \x20   ? \"How many entries the two hold between them.\"\n\
             \x20   Map.len(m) + List.len(xs)\n\n\
             verify total\n\
             \x20   total({{}}, []) => 0\n\
             \x20   total({{\"a\" => 1.5}}, [2.5]) => 2\n"
        ),
    );
}

/// The VM and the compiled binary iterate a composite-keyed map the same way.
///
/// This is the property the whole rule exists for. Before it, the VM ordered
/// any non-scalar key by its PRINTED form while compiled Rust compared the
/// value: a map keyed on `(2, 1)`, `(10, 1)`, `(3, 1)` read `ten, two, three`
/// under `aver run` and `two, three, ten` from the same program compiled —
/// two answers, same source, same commit.
///
/// The record here declares `y` before `x`, and the variant declares `Red`
/// before `Blue`, so the expected sequences are only right if both backends
/// order by NAME rather than by declaration.
#[test]
fn every_executable_backend_iterates_a_composite_key_the_same_way() {
    let dir = temp_dir("aver-key-cross-backend");
    let source = concat!(
        "module Keys\n",
        "    intent =\n",
        "        \"Reads four composite-keyed maps in iteration order.\"\n",
        "    exposes [main]\n",
        "    depends []\n",
        "    effects [Console.print]\n\n",
        "record Point\n",
        "    y: Int\n",
        "    x: Int\n\n",
        "type Colour\n",
        "    Red\n",
        "    Blue\n\n",
        "fn byTuple() -> String\n",
        "    ? \"Values of a tuple-keyed map, in iteration order.\"\n",
        "    String.join(Map.values({(2, 1) => \"two\", (10, 1) => \"ten\", (3, 1) => \"three\"}), \",\")\n\n",
        "fn byRecord() -> String\n",
        "    ? \"Values of a record-keyed map, in iteration order.\"\n",
        "    String.join(Map.values({Point(y = 9, x = 2) => \"a\", Point(y = 1, x = 1) => \"b\"}), \",\")\n\n",
        "fn byVariant() -> String\n",
        "    ? \"Values of a variant-keyed map, in iteration order.\"\n",
        "    String.join(Map.values({Colour.Red => \"r\", Colour.Blue => \"b\"}), \",\")\n\n",
        "fn byList() -> String\n",
        "    ? \"Values of a list-keyed map, in iteration order.\"\n",
        "    String.join(Map.values({[2, 1] => \"b\", [1, 9] => \"a\", [2] => \"c\"}), \",\")\n\n",
        "fn main() -> Unit\n",
        "    ? \"Prints each sequence.\"\n",
        "    ! [Console.print]\n",
        "    Console.print(\"{byTuple()} | {byRecord()} | {byVariant()} | {byList()}\")\n",
    );
    let file = dir.join("main.av");
    std::fs::write(&file, source).expect("expected to write the module");

    let run = aver(&[
        "run",
        file.to_str().expect("utf-8 path"),
        "--module-root",
        dir.to_str().expect("utf-8 path"),
    ]);
    assert!(
        run.status.success(),
        "the program must run:\n{}",
        said(&run)
    );
    let on_the_vm = String::from_utf8_lossy(&run.stdout).trim().to_string();

    // Componentwise, by field name, by constructor name, lexicographic —
    // every one of them a property of the value rather than of the source.
    assert_eq!(
        on_the_vm, "two,three,ten | b,a | b,r | a,c,b",
        "the VM must order each key out of its parts"
    );

    let out_dir = dir.join("out");
    let compile = aver(&[
        "compile",
        file.to_str().expect("utf-8 path"),
        "--module-root",
        dir.to_str().expect("utf-8 path"),
        "-o",
        out_dir.to_str().expect("utf-8 path"),
    ]);
    assert!(
        compile.status.success(),
        "a composite-keyed map must compile:\n{}",
        said(&compile)
    );
    let built = Command::new("cargo")
        .current_dir(&out_dir)
        .args(["run", "--quiet"])
        .output()
        .expect("expected cargo to run the emitted project");
    let compiled = String::from_utf8_lossy(&built.stdout).trim().to_string();
    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        built.status.success(),
        "the emitted project must build and run:\n{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert_eq!(
        compiled, on_the_vm,
        "the compiled binary must read a map the same way the VM does"
    );
}

/// The export refusal reads the key type of the map the claim actually reads.
///
/// It used to collect key types from every signature the claim's cone
/// mentions, so an unused `Map<String, Int>` parameter was enough to make the
/// gate think every key in reach was one the model can order — and a claim
/// over a map built in a local binding exported as a kernel-certified
/// theorem that `aver verify` refutes on the same source.
#[test]
fn an_unused_parameter_no_longer_opens_the_export_gate() {
    let dir = temp_dir("aver-key-gate");
    let source = concat!(
        "module Gate\n",
        "    intent =\n",
        "        \"Reads a map it builds itself, while its signature names another one.\"\n",
        "    exposes [keysOf]\n",
        "    depends []\n",
        "    effects []\n\n",
        "record Point\n",
        "    x: Int\n",
        "    y: Int\n\n",
        "fn keysOf(s: Map<String, Int>) -> Int\n",
        "    ? \"Counts the keys of a map it builds itself, ignoring the one it was given.\"\n",
        "    m = {Point(x = 2, y = 0) => 1, Point(x = 1, y = 0) => 2}\n",
        "    List.len(Map.keys(m))\n\n",
        "verify keysOf\n",
        "    keysOf({}) => 2\n",
    );
    std::fs::write(dir.join("main.av"), source).expect("expected to write the module");
    let out_dir = dir.join("out");
    let proof = aver(&[
        "proof",
        dir.join("main.av").to_str().expect("utf-8 path"),
        "--module-root",
        dir.to_str().expect("utf-8 path"),
        "-o",
        out_dir.to_str().expect("utf-8 path"),
    ]);
    assert!(
        proof.status.success(),
        "emitting the proof project must succeed:\n{}",
        said(&proof)
    );
    let lean = std::fs::read_to_string(out_dir.join("Gate.lean"))
        .expect("expected the generated Lean module to exist");
    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        !lean.contains("example : keysOf"),
        "the claim must not be exported at all:\n{lean}"
    );
    assert!(
        lean.contains("map iteration order is not exported"),
        "the refusal must say what it declined and why:\n{lean}"
    );
}
