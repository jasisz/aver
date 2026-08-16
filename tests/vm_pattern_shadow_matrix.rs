//! Regression net for issues #948/#949 — positional binding slots —
//! updated for the shadowing ban (issue #954).
//!
//! The original witness (issue #948): the VM printed a constant `3/3`
//! for `once(5)/once(1)` while compiled Rust answered `11/3`, because
//! the MIR statement-chain lowering looked a binding's slot up by NAME
//! in the last-wins `local_slots` map, so a later same-spelled pattern
//! binder stole the statement's slot. The fix gave bindings their
//! slots by position (`stmt_binding_slots`, per-arm `binding_slots`).
//!
//! Issue #954 then banned the trigger at the front door: a binder may
//! no longer spell a name already in scope, so every same-spelling
//! witness in this file stopped being a runnable program. The file now
//! carries two kinds of tests, and is honest about which is which:
//!
//! - **Error witnesses** (`*_ILLEGAL_SRC`): the original shadowing
//!   programs, verbatim, now pinned to be REJECTED with the standard
//!   shadow error. The full (binder position × shadowed kind) matrix
//!   lives in `src/resolver.rs` unit tests; these pin the front door
//!   on the exact programs that once made executors disagree.
//! - **Value tests**: the positional-slot machinery still matters —
//!   sibling arms may legally reuse a name (never in each other's
//!   scope), and compiler-synthesized code reuses spellings freely —
//!   so the order-controlled matrix keeps its cells with DISTINCT
//!   binder names (same hand-computed values, same three backends),
//!   and each old witness's renamed control keeps its runtime path
//!   covered. `ESCAPE_SIBLING_ARMS_SRC` is untouched: two sibling
//!   arms binding the same `n` are still legal and still must splice
//!   per-arm slots correctly.
//!
//! Recorded pre-#948-fix reds are kept on the constants they belong
//! to, because they document what the positional-slot machinery
//! protects — reachable today through sibling arms and synthesized
//! code even though user-written nested shadowing is now refused.

#![cfg(feature = "runtime")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-shadow-matrix-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

fn run_vm(prefix: &str, source: &str) -> String {
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` (VM) to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{prefix} VM run failed:\n{}",
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Run `aver run` on a program that must be REJECTED by the shadowing
/// ban; returns stderr for the message assertion.
fn run_rejected(prefix: &str, source: &str) -> String {
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` (VM) to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "{prefix}: the shadowing program must be rejected, not executed:\n{}",
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stderr).to_string()
}

fn run_self_host(prefix: &str, source: &str) -> String {
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .arg("--module-root")
        .arg(&module_root)
        .arg("--self-host")
        .output()
        .expect("expected `aver run --self-host` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{prefix} self-host run failed:\n{}",
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Compile to Rust, `cargo build --offline` against a shared target
/// dir (same amortisation pattern as `rust_codegen_differential`),
/// run the produced binary, return trimmed stdout.
fn run_compiled_rust(prefix: &str, source: &str) -> String {
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let project = module_root.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("shadow_{prefix}");

    let compile = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(&name)
        .arg("-o")
        .arg(&project)
        .arg("--module-root")
        .arg(&module_root)
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    assert!(
        compile.status.success(),
        "{prefix}: aver compile --target rust failed:\n{}",
        format_output(&compile)
    );

    let target = repo_root().join("target").join("shadow-matrix-shared");
    fs::create_dir_all(&target).expect("create cargo target dir");
    let build = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target)
        .output()
        .expect("expected `cargo build` to spawn");
    assert!(
        build.status.success(),
        "{prefix}: cargo build failed on emitted project:\n{}",
        format_output(&build)
    );

    let bin = target
        .join("debug")
        .join(format!("{name}{}", std::env::consts::EXE_SUFFIX));
    let out = Command::new(&bin)
        .output()
        .expect("expected compiled binary to run");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{prefix}: compiled binary exited non-zero:\n{}",
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// The issue-#948 witness, verbatim shape. Pre-#948-fix the VM printed
/// a constant `3/3` where compiled Rust answered `11/3`; post-#954 the
/// program is refused outright.
const WITNESS_ILLEGAL_SRC: &str = r#"module Tmp

fn once(sh: Int) -> Int
    v = sh * 2
    match Option.Some(v + 1)
        Option.Some(v) -> v
        Option.None -> 0

fn main()
    ! [Console.print]
    Console.print(String.fromInt(once(5)))
    Console.print(String.fromInt(once(1)))
"#;

const WITNESS_ERROR: &str = "the pattern binding 'v' shadows the binding 'v' defined at \
     line 4; every name means one thing in its scope — rename one of them";

/// The order-controlled slot matrix, distinct binder names. Cell
/// shapes (the shadowing spellings these were born with are compile
/// errors now — see `WITNESS_ILLEGAL_SRC` and the resolver matrix):
///   cell1  ctor binder after a statement binding, read in arm body
///   cell2  ctor binder beside an fn parameter, read in arm body
///   cell3  ctor binder + statement binding, wrapper + original
///          read after the match
///   cell4  cons-head binder after a statement binding, read in arm body
///   cell5  cons-tail binder (different type) after a statement binding,
///          read in arm body
///   cell6  tuple binders after a statement binding, read in arm body
///   cell7  nested match two deep — inner ctor binder under the outer
///          arm's binder, read in the inner arm body
///   cell8  inner ctor binder + statement binding, inner arm reads both
///          the inner and the outer binder
///   cell9  ctor binder beside an fn parameter, wrapper + original
///          parameter read after the match
///   cell10 cons-head binder + statement binding, wrapper + original
///          read after the match
///   cell11 first arm binds, second arm reads the original statement
///          binding
const MATRIX_SRC: &str = r#"module Tmp

fn cell1(x: Int) -> Int
    v = x * 2
    match Option.Some(v + 1)
        Option.Some(w) -> w
        Option.None -> 0

fn cell2(v: Int) -> Int
    match Option.Some(v + 1)
        Option.Some(w) -> w * 10
        Option.None -> 0

fn cell3(x: Int) -> Int
    v = x * 2
    r = match Option.Some(v + 1)
        Option.Some(w) -> w * 10
        Option.None -> 0
    r + v

fn cell4(x: Int) -> Int
    v = x + 1
    match [v + 1, v + 2]
        [] -> 0
        [w, ..rest] -> w * 100 + List.len(rest)

fn cell5(x: Int) -> Int
    v = x + 3
    match [x, x * 2]
        [] -> v
        [h, ..u] -> h * 10 + List.len(u)

fn cell6(x: Int) -> Int
    v = x * 2
    match (v + 1, v + 2)
        (u, w) -> u * 100 + w

fn cell7(x: Int) -> Int
    match Option.Some(x + 1)
        Option.Some(v) -> match Option.Some(v * 2)
            Option.Some(u) -> u + 1
            Option.None -> 0
        Option.None -> 0

fn cell8(x: Int) -> Int
    v = x * 5
    match Option.Some(x + 1)
        Option.Some(w) -> match Option.Some(w + 1)
            Option.Some(u) -> u * 10 + w
            Option.None -> 0
        Option.None -> 0

fn cell9(v: Int) -> Int
    r = match Option.Some(v * 3)
        Option.Some(u) -> u + 1
        Option.None -> 0
    r * 100 + v

fn cell10(x: Int) -> Int
    v = x + 2
    r = match [v]
        [] -> 0
        [u, ..rest] -> u * 3
    r + v * 1000

fn intToOpt(x: Int) -> Option<Int>
    match x > 3
        true -> Option.Some(x + 100)
        false -> Option.None

fn cell11(x: Int) -> Int
    v = x * 7
    match intToOpt(x)
        Option.Some(u) -> u
        Option.None -> v

fn printCells(x: Int)
    ! [Console.print]
    Console.print(String.fromInt(cell1(x)))
    Console.print(String.fromInt(cell2(x)))
    Console.print(String.fromInt(cell3(x)))
    Console.print(String.fromInt(cell4(x)))
    Console.print(String.fromInt(cell5(x)))
    Console.print(String.fromInt(cell6(x)))
    Console.print(String.fromInt(cell7(x)))
    Console.print(String.fromInt(cell8(x)))
    Console.print(String.fromInt(cell9(x)))
    Console.print(String.fromInt(cell10(x)))
    Console.print(String.fromInt(cell11(x)))

fn main()
    ! [Console.print]
    printCells(5)
    printCells(1)
"#;

/// Hand-computed truth, confirmed cell by cell against the compiled
/// Rust backend before the #948 fix. Renaming the inner binders does
/// not move a single value: correct lexical scoping gives the arm body
/// the inner binder and everything after the match the outer one, and
/// that is exactly what the distinct spellings say outright.
///
/// x = 5: cell1 = 11, cell2 = 60, cell3 = 120, cell4 = 701,
///        cell5 = 51, cell6 = 1112, cell7 = 13, cell8 = 76,
///        cell9 = 1605, cell10 = 7021, cell11 = 105.
/// x = 1: cell1 = 3, cell2 = 20, cell3 = 32, cell4 = 301,
///        cell5 = 11, cell6 = 304, cell7 = 5, cell8 = 32,
///        cell9 = 401, cell10 = 3009, cell11 = 7.
const EXPECTED: &str = "11\n60\n120\n701\n51\n1112\n13\n76\n1605\n7021\n105\n3\n20\n32\n301\n11\n304\n5\n32\n401\n3009\n7";

/// The ALIAS-pass witness (`src/ir/alias.rs`), verbatim: the statement
/// binding `v = pick(outer)` extracts a Vector out of a Map, and a
/// later Int pattern binder reuses the spelling `v`. Pre-#948-fix the
/// pass fetched the statement binding's slot BY NAME from the
/// last-wins `local_slots` map, judged the binder's Int slot, and left
/// the extracted vector owned-eligible — the VM answered `-989/-997`
/// where self-host and compiled Rust answered `7011/7003`. Post-#954
/// the spelling itself is refused.
const ALIAS_VEC_ILLEGAL_SRC: &str = r#"module Tmp

fn build() -> Map<Int, Vector<Int>>
    Map.set({}, 1, Vector.fromList([7, 7, 7]))

fn pick(m: Map<Int, Vector<Int>>) -> Vector<Int>
    match Map.get(m, 1)
        Option.Some(inner) -> inner
        Option.None -> Vector.new(1, 0)

fn unwrapVec(o: Option<Vector<Int>>) -> Vector<Int>
    match o
        Option.Some(inner) -> inner
        Option.None -> Vector.new(1, 0)

fn at0(vec: Vector<Int>) -> Int
    match Vector.get(vec, 0)
        Option.Some(e) -> e
        Option.None -> 0 - 1

fn corrupt(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = unwrapVec(Vector.set(v, 0, x))
    r = match Option.Some(x)
        Option.Some(v) -> v + 1
        Option.None -> 0
    at0(pick(outer)) * 1000 + at0(s) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(corrupt(5)))
    Console.print(String.fromInt(corrupt(1)))
"#;

const ALIAS_VEC_ERROR: &str = "the pattern binding 'v' shadows the binding 'v' defined at \
     line 23; every name means one thing in its scope — rename one of them";

/// The renamed control twin — pre-#948-fix it answered `7011/7003`
/// everywhere, pinning the shadowing spelling as the trigger. It keeps
/// the alias-pass runtime path covered: the map-held vector must stay
/// off the owned fast path whatever the neighbouring binders are
/// called.
const ALIAS_VEC_SRC: &str = r#"module Tmp

fn build() -> Map<Int, Vector<Int>>
    Map.set({}, 1, Vector.fromList([7, 7, 7]))

fn pick(m: Map<Int, Vector<Int>>) -> Vector<Int>
    match Map.get(m, 1)
        Option.Some(inner) -> inner
        Option.None -> Vector.new(1, 0)

fn unwrapVec(o: Option<Vector<Int>>) -> Vector<Int>
    match o
        Option.Some(inner) -> inner
        Option.None -> Vector.new(1, 0)

fn at0(vec: Vector<Int>) -> Int
    match Vector.get(vec, 0)
        Option.Some(e) -> e
        Option.None -> 0 - 1

fn control(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = unwrapVec(Vector.set(v, 0, x))
    r = match Option.Some(x)
        Option.Some(y) -> y + 1
        Option.None -> 0
    at0(pick(outer)) * 1000 + at0(s) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

/// The Map-in-Map variant. Pre-#948-fix the VM PANICKED at
/// `src/vm/execute/slots.rs:482` — the issue-#926 runtime fence
/// falsifying the static ownership grant the alias pass mis-stamped.
const ALIAS_MAP_ILLEGAL_SRC: &str = r#"module Tmp

fn build() -> Map<Int, Map<Int, Int>>
    Map.set({}, 1, Map.set({}, 10, 7))

fn pick(m: Map<Int, Map<Int, Int>>) -> Map<Int, Int>
    match Map.get(m, 1)
        Option.Some(inner) -> inner
        Option.None -> {}

fn at10(mm: Map<Int, Int>) -> Int
    match Map.get(mm, 10)
        Option.Some(e) -> e
        Option.None -> 0 - 1

fn corrupt(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = Map.set(v, 10, x)
    r = match Option.Some(x)
        Option.Some(v) -> v + 1
        Option.None -> 0
    at10(pick(outer)) * 1000 + at10(s) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(corrupt(5)))
    Console.print(String.fromInt(corrupt(1)))
"#;

const ALIAS_MAP_ERROR: &str = "the pattern binding 'v' shadows the binding 'v' defined at \
     line 18; every name means one thing in its scope — rename one of them";

const ALIAS_MAP_SRC: &str = r#"module Tmp

fn build() -> Map<Int, Map<Int, Int>>
    Map.set({}, 1, Map.set({}, 10, 7))

fn pick(m: Map<Int, Map<Int, Int>>) -> Map<Int, Int>
    match Map.get(m, 1)
        Option.Some(inner) -> inner
        Option.None -> {}

fn at10(mm: Map<Int, Int>) -> Int
    match Map.get(mm, 10)
        Option.Some(e) -> e
        Option.None -> 0 - 1

fn control(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = Map.set(v, 10, x)
    r = match Option.Some(x)
        Option.Some(y) -> y + 1
        Option.None -> 0
    at10(pick(outer)) * 1000 + at10(s) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

/// control(5), control(1) — the shadowing twin that used to sit next
/// to these is the error witness above.
const ALIAS_EXPECTED: &str = "7011\n7003";

/// Same-disease witness in the ESCAPE pass (`src/ir/escape.rs`): two
/// SIBLING arms of an inline-eligible fn bind the same name. The pass
/// used to resolve arm-binder slots by name through the fn-level
/// last-wins map, handing BOTH arms the second arm's slot — the first
/// arm's body then spliced into the caller with its binder
/// unsubstituted, a dangling slot reference.
///
/// Recorded pre-#948-fix reds:
///   VM: panicked at `src/vm/execute/dispatch.rs:216` — "index out of
///       bounds: the len is 1 but the index is 1".
///   emitted Rust: rustc E0425 "cannot find value `n` in this scope".
///   wasm-gc: validation error (pinned in
///       `tests/wasm_gc_codegen_regression.rs`).
///   self-host: `6 / 30` — correct (it does not run this pass).
///
/// UNTOUCHED by the #954 ban on purpose: sibling arms are not in each
/// other's scope, so `eval` is still a legal program — this is the
/// shape that keeps the per-arm `binding_slots` machinery honest in
/// user-written code.
const ESCAPE_SIBLING_ARMS_SRC: &str = r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn eval(p: Shape) -> Int
    match p
        Shape.Circle(n) -> n + 1
        Shape.Square(n) -> n * 10

fn evalRenamed(p: Shape) -> Int
    match p
        Shape.Circle(a) -> a + 1
        Shape.Square(b) -> b * 10

fn main()
    ! [Console.print]
    Console.print(String.fromInt(eval(Shape.Circle(5))))
    Console.print(String.fromInt(eval(Shape.Square(3))))
    Console.print(String.fromInt(evalRenamed(Shape.Circle(5))))
    Console.print(String.fromInt(evalRenamed(Shape.Square(3))))
"#;

const ESCAPE_EXPECTED: &str = "6\n30\n6\n30";

/// The VM `MirExpr::FnValue` witness (`src/vm/compiler/expr.rs`),
/// verbatim: a top-level fn referenced as a VALUE was hijacked by a
/// same-spelled pattern binder in the enclosing fn. Pre-#948-fix the
/// VM said "cannot call non-function (got Unit)" while compiled Rust
/// answered `26/6`. Post-#954 the hijacking spelling is refused — this
/// is the same disease issue #951 reported for a CALLEE position.
const FN_VALUE_ILLEGAL_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn callWith(f: Fn(Int) -> Int, x: Int) -> Int
    f(x)

fn hijack(x: Int) -> Int
    r = match Option.Some(x * 3)
        Option.Some(dbl) -> dbl + 1
        Option.None -> 0
    callWith(dbl, x) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(hijack(5)))
    Console.print(String.fromInt(hijack(1)))
"#;

const FN_VALUE_ERROR: &str = "the pattern binding 'dbl' shadows the function 'dbl' defined \
     at line 3; every name means one thing in its scope — rename one of them";

/// The renamed control — green everywhere pre-#948-fix. Keeps the
/// fn-as-value compile path covered.
/// No self-host executor here: the self-host parser does not accept
/// `Fn(..)`-typed parameters yet.
const FN_VALUE_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn callWith(f: Fn(Int) -> Int, x: Int) -> Int
    f(x)

fn control(x: Int) -> Int
    r = match Option.Some(x * 3)
        Option.Some(y) -> y + 1
        Option.None -> 0
    callWith(dbl, x) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

const FN_VALUE_EXPECTED: &str = "26\n6";

/// The CANDIDATE-MAP witness at the escape pass's door
/// (`src/ir/escape.rs`, `rewrite_in_expr`), verbatim: the param
/// `area: Fn(Shape) -> Int` shadows the one-argument inlinable module
/// fn `area`, and `area(Shape.Circle(5))` inside `apply` was spliced
/// with the MODULE fn's arm body no matter which fn the caller passed
/// — the VM answered `10/10` where the truth is `100/10`. Post-#954
/// the parameter spelling is refused.
const ESCAPE_SHADOWED_CALLEE_ILLEGAL_SRC: &str = r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn area(s: Shape) -> Int
    match s
        Shape.Circle(n) -> n * 2
        Shape.Square(n) -> n * 3

fn big(s: Shape) -> Int
    100

fn apply(area: Fn(Shape) -> Int) -> Int
    area(Shape.Circle(5))

fn main()
    ! [Console.print]
    Console.print(String.fromInt(apply(big)))
    Console.print(String.fromInt(apply(area)))
"#;

const ESCAPE_SHADOWED_CALLEE_ERROR: &str = "the parameter 'area' shadows the function 'area' \
     defined at line 7; every name means one thing in its scope — rename one of them";

/// The renamed control: a `Fn`-typed param spelled unlike any module
/// fn must call the PASSED fn. VM-only, like the original witness: the
/// emitted Rust for this higher-order shape does not compile
/// (pre-existing E0308, issue #952) and the self-host parser does not
/// accept `Fn(..)`-typed parameters yet.
const ESCAPE_SHADOWED_CALLEE_SRC: &str = r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn area(s: Shape) -> Int
    match s
        Shape.Circle(n) -> n * 2
        Shape.Square(n) -> n * 3

fn big(s: Shape) -> Int
    100

fn applyRenamed(f: Fn(Shape) -> Int) -> Int
    f(Shape.Circle(5))

fn main()
    ! [Console.print]
    Console.print(String.fromInt(applyRenamed(big)))
    Console.print(String.fromInt(applyRenamed(area)))
"#;

const ESCAPE_SHADOWED_CALLEE_EXPECTED: &str = "100\n10";

// ── Error witnesses: the original shadowing spellings are refused ──

#[test]
fn witness_shadowed_pattern_binder_is_rejected() {
    let stderr = run_rejected("witness-reject", WITNESS_ILLEGAL_SRC);
    assert!(
        stderr.contains(WITNESS_ERROR),
        "the #948 witness must be refused with the standard shadow error:\n{stderr}"
    );
}

#[test]
fn alias_shadowed_vector_witness_is_rejected() {
    let stderr = run_rejected("alias-vec-reject", ALIAS_VEC_ILLEGAL_SRC);
    assert!(
        stderr.contains(ALIAS_VEC_ERROR),
        "the alias-vector witness must be refused with the standard shadow error:\n{stderr}"
    );
}

#[test]
fn alias_shadowed_map_witness_is_rejected() {
    let stderr = run_rejected("alias-map-reject", ALIAS_MAP_ILLEGAL_SRC);
    assert!(
        stderr.contains(ALIAS_MAP_ERROR),
        "the alias-map witness must be refused with the standard shadow error:\n{stderr}"
    );
}

#[test]
fn fn_value_hijack_witness_is_rejected() {
    let stderr = run_rejected("fnv-reject", FN_VALUE_ILLEGAL_SRC);
    assert!(
        stderr.contains(FN_VALUE_ERROR),
        "the fn-value hijack witness must be refused with the standard shadow error:\n{stderr}"
    );
}

#[test]
fn escape_shadowed_callee_witness_is_rejected() {
    let stderr = run_rejected("esc-callee-reject", ESCAPE_SHADOWED_CALLEE_ILLEGAL_SRC);
    assert!(
        stderr.contains(ESCAPE_SHADOWED_CALLEE_ERROR),
        "the shadowed-callee witness must be refused with the standard shadow error:\n{stderr}"
    );
}

// ── Value tests: positional slots with distinct names ──────────────

#[test]
fn alias_vector_in_map_control_vm() {
    assert_eq!(
        run_vm("alias-vec-vm", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "the VM mutated a map-held vector through the alias pass's owned fast path"
    );
}

#[test]
fn alias_vector_in_map_control_compiled_rust() {
    assert_eq!(
        run_compiled_rust("aliasvec", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "compiled Rust diverged on the alias-vector control"
    );
}

#[test]
fn alias_vector_in_map_control_self_host() {
    assert_eq!(
        run_self_host("alias-vec-sh", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "self-host diverged on the alias-vector control"
    );
}

#[test]
fn alias_map_in_map_control_vm() {
    assert_eq!(
        run_vm("alias-map-vm", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "the VM must not trip the issue-#926 ownership fence on a map extracted \
         from a map"
    );
}

#[test]
fn alias_map_in_map_control_compiled_rust() {
    assert_eq!(
        run_compiled_rust("aliasmap", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "compiled Rust diverged on the alias-map control"
    );
}

#[test]
fn alias_map_in_map_control_self_host() {
    assert_eq!(
        run_self_host("alias-map-sh", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "self-host diverged on the alias-map control"
    );
}

#[test]
fn escape_sibling_arm_binders_vm() {
    assert_eq!(
        run_vm("esc-vm", ESCAPE_SIBLING_ARMS_SRC),
        ESCAPE_EXPECTED,
        "the escape pass spliced a sibling arm with the wrong binder slot \
         (pre-#948-fix: VM slot-index panic)"
    );
}

#[test]
fn escape_sibling_arm_binders_compiled_rust() {
    assert_eq!(
        run_compiled_rust("escarms", ESCAPE_SIBLING_ARMS_SRC),
        ESCAPE_EXPECTED,
        "emitted Rust must compile and answer (pre-#948-fix: rustc E0425 on the \
         unsubstituted binder)"
    );
}

#[test]
fn escape_sibling_arm_binders_self_host() {
    assert_eq!(
        run_self_host("esc-sh", ESCAPE_SIBLING_ARMS_SRC),
        ESCAPE_EXPECTED,
        "self-host diverged on the escape sibling-arms witness"
    );
}

#[test]
fn fn_value_control_vm() {
    assert_eq!(
        run_vm("fnv-vm", FN_VALUE_SRC),
        FN_VALUE_EXPECTED,
        "a fn referenced as a value must reach the callee whatever the \
         neighbouring binders are called"
    );
}

#[test]
fn fn_value_control_compiled_rust() {
    assert_eq!(
        run_compiled_rust("fnv", FN_VALUE_SRC),
        FN_VALUE_EXPECTED,
        "compiled Rust diverged on the fn-value control"
    );
}

#[test]
fn escape_callee_param_control_vm() {
    assert_eq!(
        run_vm("esc-callee-vm", ESCAPE_SHADOWED_CALLEE_SRC),
        ESCAPE_SHADOWED_CALLEE_EXPECTED,
        "a Fn-typed param must call the PASSED fn, not a spliced module-fn body"
    );
}

#[test]
fn shadow_matrix_vm() {
    assert_eq!(
        run_vm("matrix-vm", MATRIX_SRC),
        EXPECTED,
        "VM diverged from the slot-matrix truth"
    );
}

#[test]
fn shadow_matrix_compiled_rust() {
    assert_eq!(
        run_compiled_rust("matrix", MATRIX_SRC),
        EXPECTED,
        "compiled Rust diverged from the slot-matrix truth"
    );
}

#[test]
fn shadow_matrix_self_host() {
    assert_eq!(
        run_self_host("matrix-sh", MATRIX_SRC),
        EXPECTED,
        "self-host diverged from the slot-matrix truth"
    );
}
