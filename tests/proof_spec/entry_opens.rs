// The `open` lines of an emitted Lean file name the module's DIRECT
// dependencies — one rule for a dependency file and for the entry file.
// Measured on the first external project (89 modules): the entry used to
// open the whole transitive closure, so a match binder spelled like a
// function two transitive modules export was Lean's "ambiguous pattern,
// use fully qualified name", and a user function named `none` made the
// bare `none` an ambiguous term against `Option.none`. Imports stay
// transitive (every file has to compile); only `open` narrows. A name two
// direct dependencies both declare is hidden from both opens, and the
// emitter spells `Option.some` / `Option.none` in full.
//
// Narrowing the `open` is only sound because nothing a module emits
// depends on a transitive namespace being open: a user type declared in
// another module is spelled with that module's path (`A.Fraction`,
// `X.Shape.circle`) in signatures, fields and constructor expressions, so a
// type a direct dependency merely re-exposes, two opened modules that
// declare the same type name, and a user type spelled like a Lean root type
// (`Sum`) all resolve on their own. A capability resource is spelled the
// same way (`Kv.Handle`): the entry reaches one without opening its module
// whenever it threads a transitive capability's operation. The hidden names
// are read off the emitted text, indented `mutual` members included, and
// `some` / `none` join the renamed binders (`some'`), like `id` / `max` /
// `min` before them.
//
// Every emitted file sets `autoImplicit false`, so a type name the emitter
// leaves unresolved is a build error rather than an implicit type variable
// Lean binds silently — the shape the bare resource had.
//
// The lake-backed tests are guarded by the standard `lake --version` skip.

use super::*;
use std::collections::HashMap;

/// Write `files` into a fresh module-root dir, run
/// `aver proof <entry> --backend lean --module-root <root> -o <out>`
/// WITHOUT `--check`, and return the raw output plus the requested
/// emitted files. Fast (no lake) — for assertions on emitted text.
fn emit_multi(
    files: &[(&str, &str)],
    entry: &str,
    read_back: &[&str],
) -> (std::process::Output, HashMap<String, String>) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-entry-opens-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    for (name, source) in files {
        let path = src.join(name);
        if let Some(dir) = path.parent() {
            std::fs::create_dir_all(dir).expect("create module dir");
        }
        std::fs::write(&path, source).unwrap_or_else(|e| panic!("write {name}: {e}"));
    }
    let out = temp_output_dir("aver-entry-opens-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join(entry))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let mut leans = HashMap::new();
    for name in read_back {
        let lean = std::fs::read_to_string(out.join(name))
            .unwrap_or_else(|e| panic!("read generated {name}: {e}"));
        leans.insert((*name).to_string(), lean);
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
    (run, leans)
}

fn lake_available() -> bool {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping entry-opens proof test: `lake` not available");
        return false;
    }
    true
}

/// Bottom of the chain: exports `message`.
const A_MESSAGE: &str = r#"module A
    intent =
        "Exports a function named message."
    exposes [message]
    effects []

fn message(n: Int) -> Int
    n + 1
"#;

/// Middle of the chain: exports `message` too, built on A's.
const B_MESSAGE: &str = r#"module B
    intent =
        "Exports a function named message as well, built on the one in A."
    exposes [message]
    depends [A]
    effects []

fn message(n: Int) -> Int
    A.message(n) + 1
"#;

/// Entry over `B` only: matches with a binder named `message`, which two
/// modules of the transitive closure export as a function.
const ENTRY_BINDER_MESSAGE: &str = r#"module Demo
    intent =
        "Matches with a binder spelled like a function two modules below export."
    depends [B]
    effects []

fn pick(o: Option<Int>) -> Int
    match o
        Option.Some(message) -> B.message(message)
        Option.None -> 0

verify pick
    pick(Option.Some(1)) => 3
    pick(Option.None) => 0
"#;

#[test]
fn entry_binder_named_like_two_transitive_dependency_fns_builds_and_passes() {
    // Red on the previous emitter: `Demo.lean` opened `A` and `B`, so the
    // pattern `.some message` failed with
    //   ambiguous pattern, use fully qualified name, possible interpretations [B.message, A.message]
    if !lake_available() {
        return;
    }
    let (summary, run, _) = super::cross_file::run_multi(
        &[
            ("A.av", A_MESSAGE),
            ("B.av", B_MESSAGE),
            ("Demo.av", ENTRY_BINDER_MESSAGE),
        ],
        "Demo.av",
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool(),
        ),
        (Some(0), Some(true)),
        "an entry whose binder is spelled like a function of two transitive \
         modules must build and pass: the entry opens only its direct `depends`\n{}",
        format_output(&run)
    );
}

/// Direct dependency exporting a function named `none`.
const P_NONE: &str = r#"module P
    intent =
        "Exports a function named none."
    exposes [none]
    effects []

fn none() -> Int
    0
"#;

/// Entry returning `Option<Int>` values while `P` (opened) declares `none`.
const ENTRY_OPTION_VALUES: &str = r#"module Demo
    intent =
        "Returns Option values while a direct dependency exports a function named none."
    depends [P]
    effects []

fn lookup(n: Int) -> Option<Int>
    match n > P.none()
        true -> Option.Some(n)
        false -> Option.None

verify lookup
    lookup(1) => Option.Some(1)
    lookup(0) => Option.None
"#;

#[test]
fn entry_next_to_dependency_fn_named_none_builds_option_values() {
    // Red on the previous emitter: the sampled case `lookup 0 = none`
    // failed with
    //   Ambiguous term none  Possible interpretations: P.none : Int  Option.none : Option ?m
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[("P.av", P_NONE), ("Demo.av", ENTRY_OPTION_VALUES)],
        "Demo.av",
        &["Demo.lean"],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool(),
        ),
        (Some(0), Some(true)),
        "Option values next to an opened module declaring `none` must build: \
         the emitter spells the constructors in full\n{}",
        format_output(&run)
    );
    let demo = &leans["Demo.lean"];
    assert!(
        demo.contains("Option.none") && demo.contains("Option.some"),
        "expected fully spelled Option constructors in the entry:\n{demo}"
    );
}

/// Two direct dependencies of `D` export `named`.
const X_NAMED: &str = r#"module X
    intent =
        "Exports a function named named."
    exposes [named]
    effects []

fn named(n: Int) -> Int
    n + 1
"#;

const Y_NAMED: &str = r#"module Y
    intent =
        "Exports a function named named too."
    exposes [named]
    effects []

fn named(n: Int) -> Int
    n + 2
"#;

/// Dependency file with the shape of the entry case: two DIRECT
/// dependencies export `named`, and a match binder is spelled `named`.
const D_BINDER_NAMED: &str = r#"module D
    intent =
        "Matches with a binder spelled like a function both direct dependencies export."
    exposes [pick]
    depends [X, Y]
    effects []

fn pick(o: Option<Int>) -> Int
    match o
        Option.Some(named) -> X.named(named) + Y.named(named)
        Option.None -> 0
"#;

const ENTRY_OVER_D: &str = r#"module Demo
    intent =
        "Calls through a dependency whose own dependencies share a function name."
    depends [D]
    effects []

fn run(n: Int) -> Int
    D.pick(Option.Some(n))

verify run
    run(1) => 5
    run(0) => 3
"#;

#[test]
fn dependency_binder_named_like_two_direct_dependency_fns_builds() {
    // Red on the previous emitter: `D.lean` opened `X` and `Y` outright, so
    // the pattern `.some named` failed with
    //   ambiguous pattern, use fully qualified name, possible interpretations [Y.named, X.named]
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("X.av", X_NAMED),
            ("Y.av", Y_NAMED),
            ("D.av", D_BINDER_NAMED),
            ("Demo.av", ENTRY_OVER_D),
        ],
        "Demo.av",
        &["D.lean"],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool(),
        ),
        (Some(0), Some(true)),
        "a dependency whose binder is spelled like a function both of its direct \
         dependencies declare must build: the shared name is hidden from both opens\n{}",
        format_output(&run)
    );
    let d = &leans["D.lean"];
    assert!(
        d.contains("open X hiding named\n") && d.contains("open Y hiding named\n"),
        "expected the shared function name hidden from both opens:\n{d}"
    );
}

#[test]
fn entry_opens_only_its_direct_depends_and_imports_the_closure() {
    // Text-level statement of the rule on the `Demo -> B -> A` chain: the
    // entry imports both modules (they have to compile) but opens `B` only;
    // `B.lean` opens `A` as before.
    let (_, leans) = emit_multi(
        &[
            ("A.av", A_MESSAGE),
            ("B.av", B_MESSAGE),
            ("Demo.av", ENTRY_BINDER_MESSAGE),
        ],
        "Demo.av",
        &["Demo.lean", "B.lean"],
    );
    let demo = &leans["Demo.lean"];
    let opens: Vec<&str> = demo
        .lines()
        .filter(|line| line.starts_with("open "))
        .collect();
    assert_eq!(
        opens,
        vec!["open B"],
        "the entry must open exactly its direct `depends`:\n{demo}"
    );
    assert!(
        demo.contains("import A\n") && demo.contains("import B\n"),
        "imports stay transitive:\n{demo}"
    );
    let b = &leans["B.lean"];
    let b_opens: Vec<&str> = b.lines().filter(|line| line.starts_with("open ")).collect();
    assert_eq!(
        b_opens,
        vec!["open A"],
        "a dependency file opens its own depends:\n{b}"
    );
}

#[test]
fn entry_without_transitive_dependencies_emits_byte_identically() {
    // No-movement guarantee: an entry whose `depends` already IS the whole
    // closure (one dependency, no dependencies of its own) emits the same
    // bytes as before the rule change. The golden
    // `tests/fixtures/map_order_cross_module.baseline.lean` was produced by
    // the previous emitter. Fast (no lake).
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = temp_output_dir("aver-entry-opens-byte-identity");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/map_order_cross_module/main.av")
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg("tests/fixtures/map_order_cross_module")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let emitted = std::fs::read_to_string(out.join("MapOrderCrossModule.lean"))
        .expect("read emitted entry Lean");
    let baseline = std::fs::read_to_string(
        repo_root.join("tests/fixtures/map_order_cross_module.baseline.lean"),
    )
    .expect("read baseline golden Lean");
    assert_eq!(
        emitted, baseline,
        "an entry with no transitive dependencies must emit byte-identically \
         to the golden produced before the direct-depends rule"
    );
    let _ = std::fs::remove_dir_all(&out);
}

/// Asserts a lake-backed run built cleanly and every sample passed.
fn assert_builds_and_passes(summary: &serde_json::Value, run: &std::process::Output, why: &str) {
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool(),
        ),
        (Some(0), Some(true)),
        "{why}\n{}",
        format_output(run)
    );
}

// ---- A record a direct dependency only re-exposes ----

/// Bottom: declares the record.
const A_FRACTION: &str = r#"module A
    intent =
        "Bottom: declares the record."
    exposes [Fraction, one]
    effects []

record Fraction
    top: Int
    bottom: Int

fn one() -> Fraction
    Fraction(top = 1, bottom = 1)
"#;

/// Middle: re-exposes A's record and uses it in signatures.
const B_FRACTION: &str = r#"module B
    intent =
        "Middle: re-exposes A's record type and uses it in signatures."
    exposes [Fraction, make, top]
    depends [A]
    effects []

fn make(n: Int) -> Fraction
    Fraction(top = n, bottom = 1)

fn top(f: Fraction) -> Int
    f.top
"#;

/// Entry over `B` only: names `Fraction` bare in signatures — `A`, the
/// owner, is never opened by the entry.
const ENTRY_BARE_REEXPOSED_RECORD: &str = r#"module Main
    intent =
        "Entry depends only on B and names Fraction bare in a signature."
    depends [B]
    effects []

fn mk(n: Int) -> Fraction
    B.make(n + 1)

fn topOf(f: Fraction) -> Int
    B.top(f)

verify mk
    mk(1) => B.make(2)

verify topOf
    topOf(B.make(5)) => 5

verify topOf law roundtrip
    given n: Int = [0, 1, 4]
    topOf(mk(n)) == n + 1 holds
"#;

#[test]
fn signatures_spell_a_re_exposed_record_by_its_owner() {
    // Text-level statement of the rule: the entry and `B` both use `Fraction`
    // bare, and both spell it `A.Fraction`; `A` itself keeps the bare name.
    let (_, leans) = emit_multi(
        &[
            ("A.av", A_FRACTION),
            ("B.av", B_FRACTION),
            ("Main.av", ENTRY_BARE_REEXPOSED_RECORD),
        ],
        "Main.av",
        &["Main.lean", "B.lean", "A.lean"],
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("def mk (n : Int) : A.Fraction :=")
            && main.contains("def topOf (f : A.Fraction) : Int :="),
        "expected the entry's signatures to carry the owner module of `Fraction`:\n{main}"
    );
    let b = &leans["B.lean"];
    assert!(
        b.contains("def make (n : Int) : A.Fraction :=")
            && b.contains("def top (f : A.Fraction) : Int :="),
        "expected the re-exposing module's signatures to carry the owner too:\n{b}"
    );
    let a = &leans["A.lean"];
    assert!(
        a.contains("def one  : Fraction :=") && a.contains("structure Fraction where"),
        "expected the owner to keep the bare name of its own type:\n{a}"
    );
}

#[test]
fn entry_naming_a_re_exposed_record_bare_builds_and_passes() {
    // Red on the first direct-depends emitter (the closure-opening emitter
    // before it was green, `A` being open in the entry): `Main.lean` spelled
    // the bare `Fraction`, which `open B` does not resolve, so Lean
    // auto-bound it as an implicit and failed with
    //   Main.lean:14:2: Type mismatch  make (n + 1) has type A.Fraction
    //   Main.lean:26:44: don't know how to synthesize implicit argument `Fraction`
    // (22 errors). Green now: 0 errors, passed.
    if !lake_available() {
        return;
    }
    let (summary, run, _) = super::cross_file::run_multi(
        &[
            ("A.av", A_FRACTION),
            ("B.av", B_FRACTION),
            ("Main.av", ENTRY_BARE_REEXPOSED_RECORD),
        ],
        "Main.av",
        &[],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a record a direct dependency only re-exposes must resolve without its owner \
         being opened: the signature spells it by the owner module",
    );
}

// ---- A sum a direct dependency only re-exposes ----

const A_SHAPE: &str = r#"module A
    intent =
        "Bottom: declares the sum."
    exposes [Shape]
    effects []

type Shape
    Circle(Int)
    Dot
"#;

const B_SHAPE: &str = r#"module B
    intent =
        "Middle: re-exposes A's sum and builds it."
    exposes [Shape, mk]
    depends [A]
    effects []

fn mk(n: Int) -> Shape
    match n > 0
        true -> Shape.Circle(n)
        false -> Shape.Dot
"#;

/// Entry over `B` only: matches, constructs and names A's sum bare.
const ENTRY_BARE_REEXPOSED_SUM: &str = r#"module Main
    intent =
        "Entry depends only on B; matches and constructs A's sum through B's re-export."
    depends [B]
    effects []

fn radius(n: Int) -> Int
    match B.mk(n)
        Shape.Circle(r) -> r
        Shape.Dot -> 0

fn dot() -> Shape
    Shape.Dot

fn isDot(s: Shape) -> Bool
    match s
        Shape.Circle(_) -> false
        Shape.Dot -> true

verify radius
    radius(3) => 3
    radius(0) => 0

verify isDot
    isDot(dot()) => true
    isDot(B.mk(1)) => false
"#;

#[test]
fn entry_constructing_a_re_exposed_sum_builds_and_passes() {
    // Red on the first direct-depends emitter: `def dot : Shape := Shape.dot`
    // with `Shape` unresolved in the entry —
    //   Main.lean:14:2: Invalid field notation: ... The expression Shape has type `Sort ?u.2`
    //   Main.lean:29:10: don't know how to synthesize implicit argument `Shape`
    // (7 errors). Green now: 0 errors, passed; the signature reads
    // `: A.Shape` and the constructor `A.Shape.dot`.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("A.av", A_SHAPE),
            ("B.av", B_SHAPE),
            ("Main.av", ENTRY_BARE_REEXPOSED_SUM),
        ],
        "Main.av",
        &["Main.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a sum a direct dependency only re-exposes must resolve in signatures and \
         constructor expressions without its owner being opened",
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("def dot  : A.Shape :=\n  A.Shape.dot")
            && main.contains("def isDot (s : A.Shape) : Bool :="),
        "expected the owner module on the sum's signature and constructor:\n{main}"
    );
}

// ---- Two direct dependencies declaring the same type name ----

const X_SHAPE: &str = r#"module X
    intent =
        "Shape in X."
    exposes [Shape, area]
    effects []

type Shape
    Circle(Int)
    Dot

fn area(s: Shape) -> Int
    match s
        Shape.Circle(r) -> r * r
        Shape.Dot -> 0
"#;

const Y_SHAPE: &str = r#"module Y
    intent =
        "Shape in Y."
    exposes [Shape, area]
    effects []

type Shape
    Circle(Int)
    Dot

fn area(s: Shape) -> Int
    match s
        Shape.Circle(r) -> 2 * r
        Shape.Dot -> 1
"#;

/// Entry opening both: qualified patterns and constructor expressions over
/// each `Shape`, plus a binder spelled like the shared function `area`.
const ENTRY_TWO_SHAPES: &str = r#"module Main
    intent =
        "Both Shapes in scope; patterns and constructor expressions over each."
    depends [X, Y]
    effects []

fn areaX(s: X.Shape) -> Int
    match s
        X.Shape.Circle(r) -> r * r
        X.Shape.Dot -> 0

fn areaY(s: Y.Shape) -> Int
    match s
        Y.Shape.Circle(area) -> Y.area(Y.Shape.Circle(area))
        Y.Shape.Dot -> 1

fn mkX(r: Int) -> X.Shape
    X.Shape.Circle(r)

verify areaX
    areaX(X.Shape.Circle(2)) => 4
    areaX(X.Shape.Dot) => 0

verify areaY
    areaY(Y.Shape.Circle(2)) => 4
    areaY(Y.Shape.Dot) => 1

verify mkX
    mkX(3) => X.Shape.Circle(3)
"#;

#[test]
fn constructor_expressions_over_same_named_types_of_two_opened_modules_build() {
    // Red before: the constructor expression dropped the module the source
    // wrote, `X.Shape.Circle(3)` emitting as `Shape.circle 3` —
    //   Main.lean:33:18: Ambiguous term Shape.circle
    //     Possible interpretations: Y.Shape.circle 3 : Y.Shape, X.Shape.circle 3 : X.Shape
    // (the closure-opening emitter also had `Main.lean:21:12: ambiguous
    // pattern ... [Y.area, X.area]` on the binder). Green now: 0 errors.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("X.av", X_SHAPE),
            ("Y.av", Y_SHAPE),
            ("Main.av", ENTRY_TWO_SHAPES),
        ],
        "Main.av",
        &["Main.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "two opened modules declaring the same type name must stay apart in \
         constructor expressions: each is spelled by its owner",
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("def mkX (r : Int) : X.Shape :=\n  X.Shape.circle r")
            && main.contains("example : mkX 3 = X.Shape.circle 3 := by")
            && main.contains("(s : Y.Shape)"),
        "expected every `Shape` spelled by its owner module:\n{main}"
    );
}

// ---- The same type name in a module and in its dependency ----

const DOMAIN_SCRIPT_OP: &str = r#"module Script
    intent =
        "Domain script ops."
    exposes [Op, run]
    effects []

type Op
    Push(Int)
    Drop

fn run(op: Op, acc: Int) -> Int
    match op
        Op.Push(v) -> acc + v
        Op.Drop -> 0
"#;

/// `Infra.Script` declares its own `Op` and builds `Domain.Script.Op`
/// values: inside `namespace Infra.Script` a bare `Op.push` is its own.
const INFRA_SCRIPT_OP: &str = r#"module Script
    intent =
        "Infra script ops, built over the domain ones."
    exposes [Op, run, lift]
    depends [Domain.Script]
    effects []

type Op
    Push(Int)
    Drop

fn run(op: Op, acc: Int) -> Int
    match op
        Op.Push(v) -> acc + v * 2
        Op.Drop -> 0

fn lift(op: Op) -> Domain.Script.Op
    match op
        Op.Push(v) -> Domain.Script.Op.Push(v)
        Op.Drop -> Domain.Script.Op.Drop
"#;

const ENTRY_TWO_OPS: &str = r#"module Main
    intent =
        "Opens Domain.Script and Infra.Script; binder run; both Op types."
    depends [Domain.Script, Infra.Script]
    effects []

fn both(op: Infra.Script.Op, acc: Int) -> Int
    Infra.Script.run(op, acc) + Domain.Script.run(Infra.Script.lift(op), acc)

fn pick(o: Option<Int>) -> Int
    match o
        Option.Some(run) -> Domain.Script.run(Domain.Script.Op.Push(run), 0)
        Option.None -> 0

fn isPush(op: Domain.Script.Op) -> Bool
    match op
        Domain.Script.Op.Push(_) -> true
        Domain.Script.Op.Drop -> false

verify both
    both(Infra.Script.Op.Push(3), 1) => 11
    both(Infra.Script.Op.Drop, 1) => 0

verify pick
    pick(Option.Some(2)) => 2
    pick(Option.None) => 0

verify isPush
    isPush(Domain.Script.Op.Push(1)) => true
    isPush(Domain.Script.Op.Drop) => false
"#;

#[test]
fn dependency_building_its_dependency_s_same_named_type_builds() {
    // Red before: `Domain.Script.Op.Push(v)` inside `Infra.Script` emitted as
    // `Op.push v`, which the surrounding namespace resolves to the module's
    // OWN `Op` —
    //   Infra/Script.lean:18:15: Type mismatch  Op.push v has type Op
    // (2 errors, the same on the closure-opening emitter). Green now: 0.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("domain/script.av", DOMAIN_SCRIPT_OP),
            ("infra/script.av", INFRA_SCRIPT_OP),
            ("Main.av", ENTRY_TWO_OPS),
        ],
        "Main.av",
        &["Infra/Script.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a module building a value of its dependency's type that shares its own \
         type's name must spell the constructor by the owner module",
    );
    let infra = &leans["Infra/Script.lean"];
    assert!(
        infra.contains("def lift (op : Op) : Domain.Script.Op :=")
            && infra.contains("| .push v => Domain.Script.Op.push v")
            && infra.contains("| .drop => Domain.Script.Op.drop"),
        "expected the foreign `Op` qualified and the module's own `Op` bare:\n{infra}"
    );
}

// ---- A user type spelled like a Lean root type ----

const T_PRELUDE_NAMES: &str = r#"module T
    intent =
        "Declares types spelled like Lean prelude types and constructors."
    exposes [Maybe, Nat, Except, Prod, Sum, toInt, fromMaybe]
    effects []

type Maybe
    None
    Some(Int)

record Nat
    value: Int

record Except
    code: Int

record Prod
    left: Int
    right: Int

type Sum
    Inl(Int)
    Inr(Int)

fn toInt(n: Nat) -> Int
    n.value

fn fromMaybe(m: Maybe) -> Int
    match m
        Maybe.Some(v) -> v
        Maybe.None -> 0
"#;

const ENTRY_OVER_PRELUDE_NAMES: &str = r#"module Main
    intent =
        "Uses recursion (fuel, Nat) and Result (Except) next to a module declaring Nat and Except."
    depends [T]
    effects []

fn count(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [_, ..rest] -> 1 + count(rest)

fn safe(n: Int) -> Result<Int, String>
    match n >= 0
        true -> Result.Ok(n)
        false -> Result.Err("neg")

fn pick(m: T.Maybe) -> Int
    match m
        T.Maybe.Some(v) -> v + 1
        T.Maybe.None -> T.toInt(T.Nat(value = 5))

fn sumSide(s: T.Sum) -> Int
    match s
        T.Sum.Inl(l) -> l
        T.Sum.Inr(r) -> 0 - r

verify count
    count([1, 2, 3]) => 3
    count([]) => 0

verify safe
    safe(1) => Result.Ok(1)
    safe(-1) => Result.Err("neg")

verify pick
    pick(T.Maybe.Some(1)) => 2
    pick(T.Maybe.None) => 5

verify sumSide
    sumSide(T.Sum.Inl(2)) => 2
    sumSide(T.Sum.Inr(2)) => -2

verify count law nonneg
    given xs: List<Int> = [[], [1], [1, 2]]
    count(xs) >= 0 holds
"#;

#[test]
fn user_type_spelled_like_a_lean_root_type_builds() {
    // Red before: `T.Sum.Inl(2)` emitted as `Sum.inl 2`, Lean's root `Sum` —
    //   Main.lean:43:18: Application type mismatch: The argument Sum.inl 2 has type ?m.3 ⊕ ?m.2
    // (2 errors, the same on the closure-opening emitter). Green now: 0.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("T.av", T_PRELUDE_NAMES),
            ("Main.av", ENTRY_OVER_PRELUDE_NAMES),
        ],
        "Main.av",
        &["Main.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a user sum named like a Lean root type must be spelled by its owner module \
         so its constructors never hit the root one",
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("sumSide (T.Sum.inl 2) = 2") && main.contains("{ value := 5 : T.Nat }"),
        "expected the owner module on the root-named types:\n{main}"
    );
}

// ---- A shared function declared inside a `mutual` block ----

/// A measure-lowered mutual pair exporting `itemAt`; the pair is emitted
/// indented inside `mutual … end`.
const X_MUTUAL_ITEM_AT: &str = r#"module X
    intent =
        "A mutual pair (measure-lowered) exporting itemAt."
    exposes [itemAt]
    effects []

fn hereOrDeeper(head: List<Int>, tail: List<List<Int>>, n: Int) -> List<Int>
    ? "This position's item, or one further down."
    match n <= 0
        true -> head
        false -> itemAt(tail, n - 1)

fn itemAt(items: List<List<Int>>, n: Int) -> List<Int>
    ? "The nth item down from the top, counting from nought."
    match items
        [] -> []
        [head, ..tail] -> hereOrDeeper(head, tail, n)
"#;

const Y_PLAIN_ITEM_AT: &str = r#"module Y
    intent =
        "Plain itemAt."
    exposes [itemAt]
    effects []

fn itemAt(items: List<Int>, n: Int) -> Int
    match items
        [] -> 0
        [head, ..tail] -> head + n
"#;

const ENTRY_BINDER_ITEM_AT: &str = r#"module Main
    intent =
        "Binder itemAt while X (mutual block) and Y (plain) both export itemAt."
    depends [X, Y]
    effects []

fn pick(o: Option<Int>) -> Int
    match o
        Option.Some(itemAt) -> Y.itemAt([itemAt], 1)
        Option.None -> 0

fn first(xs: List<List<Int>>) -> List<Int>
    X.itemAt(xs, 0)

verify pick
    pick(Option.Some(2)) => 3
    pick(Option.None) => 0

verify first
    first([[1], [2]]) => [1]
"#;

#[test]
fn shared_fn_declared_inside_a_mutual_block_is_hidden_from_both_opens() {
    // Red before: X's `itemAt` is declared as `  def itemAt` inside
    // `mutual … end`, and the indented line was not read as a declaration,
    // so neither open hid the name —
    //   Main.lean:19:10: ambiguous pattern, use fully qualified name, possible interpretations [Y.itemAt, X.itemAt]
    // (3 errors, the same on the closure-opening emitter). Green now: 0.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("X.av", X_MUTUAL_ITEM_AT),
            ("Y.av", Y_PLAIN_ITEM_AT),
            ("Main.av", ENTRY_BINDER_ITEM_AT),
        ],
        "Main.av",
        &["Main.lean", "X.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a function name declared inside a `mutual` block must be hidden from the \
         opens like a top-level one",
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("open X hiding itemAt\n") && main.contains("open Y hiding itemAt\n"),
        "expected the shared name hidden from both opens:\n{main}"
    );
    let x = &leans["X.lean"];
    assert!(
        x.contains("mutual\n") && x.contains("\n  def itemAt "),
        "the fixture must keep X's `itemAt` indented inside `mutual`:\n{x}"
    );
}

// ---- Binders named `some` / `none` next to a module exporting them ----

const P_OPTION_NAMES: &str = r#"module P
    intent =
        "Exports functions named like prelude constructors."
    exposes [none, some, ok, error]
    effects []

fn none() -> Int
    0

fn some(n: Int) -> Int
    n + 1

fn ok() -> Int
    7

fn error() -> String
    "bad"
"#;

const ENTRY_BINDERS_SOME_NONE: &str = r#"module Main
    intent =
        "Builds Option, Result and Unit values next to an opened module declaring none/some/ok/error/unit."
    depends [P]
    effects []

fn lookup(n: Int) -> Option<Int>
    match n > P.none()
        true -> Option.Some(P.some(n))
        false -> Option.None

fn check(n: Int) -> Result<Int, String>
    match n == P.ok()
        true -> Result.Ok(n)
        false -> Result.Err(P.error())

fn unwrap(o: Option<Int>) -> Int
    match o
        Option.Some(some) -> some
        Option.None -> P.none()

fn unwrapR(r: Result<Int, String>) -> Int
    match r
        Result.Ok(ok) -> ok
        Result.Err(error) -> 0 - 1

verify lookup
    lookup(1) => Option.Some(2)
    lookup(0) => Option.None

verify check
    check(7) => Result.Ok(7)
    check(1) => Result.Err("bad")

verify unwrap
    unwrap(Option.Some(4)) => 4
    unwrap(Option.None) => 0

verify unwrapR
    unwrapR(Result.Ok(4)) => 4
    unwrapR(Result.Err("x")) => -1
"#;

#[test]
fn binder_named_some_next_to_one_opened_module_exporting_some_builds() {
    // Red before: one opened module declaring `some` is not the two-module
    // shape the `hiding` rule covers, so the binder `some` met the root
    // alias of `Option.some` —
    //   Main.lean:22:10: ambiguous pattern, use fully qualified name, possible interpretations [P.some, @Option.some]
    // (3 errors; the closure-opening emitter also had `Main.lean:17:24:
    // Ambiguous term some`). Green now: 0 — `some` / `none` are renamed
    // `some'` / `none'` wherever the source spells them, as `id` / `max` /
    // `min` already were.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("P.av", P_OPTION_NAMES),
            ("Main.av", ENTRY_BINDERS_SOME_NONE),
        ],
        "Main.av",
        &["Main.lean", "P.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a binder named `some` next to a single opened module exporting `some` \
         must stay a pattern variable",
    );
    let main = &leans["Main.lean"];
    let p = &leans["P.lean"];
    assert!(
        main.contains("| .some some' => some'")
            && main.contains("| .none => P.none'")
            && p.contains("def some' (n : Int) : Int :=")
            && p.contains("def none'  : Int :="),
        "expected `some` / `none` renamed at the binder and the declaration:\n{main}\n{p}"
    );
}

// ---- A capability resource reached through a transitive dependency ----

const KV_CAPABILITY: &str = r#"module Kv
    intent =
        "A key-value database declared as a contract."
    kind = capability
    semantics = effectful
    exposes [Handle, open, count]

resource Handle

operation open(dir: String) -> Result<Handle, String>
    oracle = generativeOutput
    replay = recorded

operation count(handle: Handle) -> Result<Int, String>
    oracle = generative
    replay = recorded
"#;

const BOX_OVER_KV: &str = r#"module Box
    intent =
        "A store that is either memory or an open database; size asks the database through Kv."
    exposes [Store, size, memSize]
    depends [Kv]
    effects [Kv.count]

type Store
    Memory(Int)
    Database(Kv.Handle)

fn size(s: Store) -> Result<Int, String>
    ! [Kv.count]
    match s
        Store.Memory(n) -> Result.Ok(n)
        Store.Database(h) -> Kv.count(h)

fn memSize(s: Store) -> Int
    match s
        Store.Memory(n) -> n
        Store.Database(_) -> 0

verify memSize
    memSize(Store.Memory(3)) => 3
"#;

/// Entry over `Box` only; it threads `Kv.count` itself, so its oracle
/// parameter carries the resource type of a module it never opens.
const ENTRY_THREADING_KV_COUNT: &str = r#"module Main
    intent =
        "Entry depends only on Box but threads the Kv.count oracle through its own function."
    depends [Box]
    effects [Kv.count]

fn sizeOf(s: Box.Store) -> Result<Int, String>
    ! [Kv.count]
    Box.size(s)

fn doubled(s: Box.Store) -> Result<Int, String>
    ! [Kv.count]
    n = sizeOf(s)?
    Result.Ok(n * 2)

verify sizeOf
    sizeOf(Box.Store.Memory(4)) => Result.Ok(4)

verify doubled
    doubled(Box.Store.Memory(4)) => Result.Ok(8)
"#;

#[test]
fn oracle_parameter_spells_a_transitive_capability_resource_by_its_module() {
    // Red before: the entry opened `Box` only and the oracle parameter read
    //   def sizeOf (path : BranchPath) (rnd_Kv_count : BranchPath → Int → Handle → Except String Int) …
    // which Lean accepted under its default `autoImplicit` by binding `Handle`
    // as an implicit type variable (with the option off: `Main.lean:174:66:
    // Unknown identifier Handle`). Green now: `Kv.Handle`, like every other
    // user type declared in another module; `Kv` itself keeps the bare name.
    let (_, leans) = emit_multi(
        &[
            ("kv.av", KV_CAPABILITY),
            ("box.av", BOX_OVER_KV),
            ("Main.av", ENTRY_THREADING_KV_COUNT),
        ],
        "Main.av",
        &["Main.lean", "Box.lean", "Kv.lean"],
    );
    let main = &leans["Main.lean"];
    assert!(
        main.contains("→ Kv.Handle →") && !main.contains("→ Handle →"),
        "expected the resource spelled by its capability module in the entry's \
         oracle parameters:\n{main}"
    );
    assert!(
        !main.contains("\nopen Kv\n"),
        "the entry must reach the resource without opening the capability module:\n{main}"
    );
    let kv = &leans["Kv.lean"];
    let box_lean = &leans["Box.lean"];
    assert!(
        kv.contains("structure Handle where") && box_lean.contains("| database (_ : Kv.Handle)"),
        "expected the bare name inside the declaring module and the qualified one \
         in a dependency:\n{kv}\n{box_lean}"
    );
}

// ---- `autoImplicit false` in every emitted file ----

fn lean_files_under(dir: &std::path::Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("read emitted project dir") {
        let path = entry.expect("read dir entry").path();
        if path.is_dir() {
            lean_files_under(&path, out);
        } else if path.extension().is_some_and(|e| e == "lean") {
            out.push(path);
        }
    }
}

#[test]
fn every_emitted_lean_file_turns_auto_implicit_off() {
    // The fixture pulls in every kind of file the export produces: the
    // entry, standard module files (`Bytes.lean`, `Crypto/Digest32.lean`),
    // the prelude (`AverCommon.lean`) and the SHA-256 model (`Crypto.lean`).
    // Each carries the option next to its other `set_option` lines, so an
    // unresolved type name fails the build instead of becoming a type
    // variable. The prelude binds its own `α β ε` explicitly for the same
    // reason. Fast (no lake); the lake-backed proof of the same fixture is
    // `lean_kernel`'s crypto probe.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = temp_output_dir("aver-entry-opens-auto-implicit");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/stdlib_bytes_app.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let mut files = Vec::new();
    lean_files_under(&out, &mut files);
    let mut names: Vec<String> = files
        .iter()
        .map(|p| p.strip_prefix(&out).unwrap().to_string_lossy().into_owned())
        .collect();
    names.sort();
    assert_eq!(
        names,
        [
            "AverCommon.lean",
            "Bytes.lean",
            "Crypto.lean",
            "Crypto/Digest32.lean",
            "StdlibBytesApp.lean",
            "lakefile.lean",
        ],
        "the fixture must exercise every kind of emitted file"
    );
    for path in &files {
        if path.file_name().is_some_and(|n| n == "lakefile.lean") {
            continue;
        }
        let text = std::fs::read_to_string(path).expect("read emitted Lean");
        assert!(
            text.contains("\nset_option autoImplicit false\n"),
            "{} must turn `autoImplicit` off:\n{text}",
            path.display()
        );
    }
    let _ = std::fs::remove_dir_all(&out);
}
