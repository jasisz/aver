//! Regression net for issue #948 — a match-pattern binder that
//! shadows an earlier binding must read its own slot, and the
//! shadowed binding must keep its own slot, on every backend.
//!
//! The witness (issue #948): the VM printed a constant `3/3` for
//! `once(5)/once(1)` while compiled Rust answered `11/3`. Root
//! cause: the MIR statement-chain lowering looked the statement
//! binding's slot up by NAME in `FnResolution.local_slots` — a
//! last-allocation-wins map — so a later pattern binder spelled
//! the same steals the statement's slot. The `let` then writes
//! the pattern binder's slot and every read of the statement
//! binding hits an uninitialized one.
//!
//! The matrix below is order-controlled: every cell's value
//! depends on the argument, each cell is evaluated for two
//! arguments, and each cell pins one combination of
//!   - shadowing binder position: constructor-pattern binder,
//!     cons-pattern head, cons-pattern tail, tuple-pattern
//!     binder, nested match two deep, second arm after a
//!     binder arm;
//!   - shadowed thing: statement binding, fn parameter,
//!     earlier pattern binder;
//!   - read position: arm body, nested arm, after the match
//!     via a wrapping binding (both the wrapper and the
//!     original shadowed binding are read).
//!
//! Recorded pre-fix reds (truth = the compiled-Rust column,
//! which matched every hand-computed value):
//!   VM:        cell1 3/3 (want 11/3), cell3 32/32 (want
//!              120/32), cell4 301/301 (want 701/301), cell6
//!              304/304 (want 1112/304), cell10 2006/2006
//!              (want 7021/3009), cell11 runtime error
//!              "String.fromInt: argument must be an Int"
//!              (want 105/7).
//!   self-host: cell3 220/60 (want 120/32), cell9 1616/404
//!              (want 1605/401), cell10 21021/9009 (want
//!              7021/3009), cell11 runtime error "expected int
//!              argument" (want 105/7) — its resolver leaked
//!              arm-binder slots past the match, aliasing the
//!              wrapper binding's slot.
//!
//! Every backend must print `EXPECTED` exactly; asserting each
//! against the same literal is the three-way differential plus
//! protection against all backends agreeing on a wrong value.
//!
//! The suite also carries the SAME-DISEASE witnesses the #948 review
//! flushed out — three more consumers that resolved a specific
//! binding's slot by name through the last-wins `local_slots` map:
//! the alias pass (`ALIAS_VEC_SRC` / `ALIAS_MAP_SRC`), the escape
//! pass's arm binders (`ESCAPE_SIBLING_ARMS_SRC`), and the VM's
//! fn-as-value path (`FN_VALUE_SRC`). Each witness sits next to a
//! renamed control that must answer identically. Recorded pre-fix
//! reds live on each source constant.

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

/// The issue-#948 witness, verbatim shape.
const WITNESS_SRC: &str = r#"module Tmp

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

const WITNESS_OUT: &str = "11\n3";

/// The order-controlled shadow matrix. Cell shapes:
///   cell1  ctor binder shadows statement binding, read in arm body
///   cell2  ctor binder shadows fn parameter, read in arm body
///   cell3  ctor binder shadows statement binding, wrapper + original
///          read after the match
///   cell4  cons-head binder shadows statement binding, read in arm body
///   cell5  cons-tail binder shadows statement binding (different type),
///          read in arm body
///   cell6  tuple binder shadows statement binding, read in arm body
///   cell7  nested match two deep — inner ctor binder shadows the outer
///          arm's binder, read in the inner arm body
///   cell8  inner ctor binder shadows a statement binding, inner arm
///          reads both the inner and the outer binder
///   cell9  ctor binder shadows fn parameter, wrapper + original
///          parameter read after the match
///   cell10 cons-head binder shadows statement binding, wrapper +
///          original read after the match
///   cell11 first arm binds the shadowing name, second arm reads the
///          original statement binding
const MATRIX_SRC: &str = r#"module Tmp

fn cell1(x: Int) -> Int
    v = x * 2
    match Option.Some(v + 1)
        Option.Some(v) -> v
        Option.None -> 0

fn cell2(v: Int) -> Int
    match Option.Some(v + 1)
        Option.Some(v) -> v * 10
        Option.None -> 0

fn cell3(x: Int) -> Int
    v = x * 2
    r = match Option.Some(v + 1)
        Option.Some(v) -> v * 10
        Option.None -> 0
    r + v

fn cell4(x: Int) -> Int
    v = x + 1
    match [v + 1, v + 2]
        [] -> 0
        [v, ..rest] -> v * 100 + List.len(rest)

fn cell5(x: Int) -> Int
    v = x + 3
    match [x, x * 2]
        [] -> 0
        [h, ..v] -> h * 10 + List.len(v)

fn cell6(x: Int) -> Int
    v = x * 2
    match (v + 1, v + 2)
        (v, w) -> v * 100 + w

fn cell7(x: Int) -> Int
    match Option.Some(x + 1)
        Option.Some(v) -> match Option.Some(v * 2)
            Option.Some(v) -> v + 1
            Option.None -> 0
        Option.None -> 0

fn cell8(x: Int) -> Int
    v = x * 5
    match Option.Some(x + 1)
        Option.Some(w) -> match Option.Some(w + 1)
            Option.Some(v) -> v * 10 + w
            Option.None -> 0
        Option.None -> 0

fn cell9(v: Int) -> Int
    r = match Option.Some(v * 3)
        Option.Some(v) -> v + 1
        Option.None -> 0
    r * 100 + v

fn cell10(x: Int) -> Int
    v = x + 2
    r = match [v]
        [] -> 0
        [v, ..rest] -> v * 3
    r + v * 1000

fn intToOpt(x: Int) -> Option<Int>
    match x > 3
        true -> Option.Some(x + 100)
        false -> Option.None

fn cell11(x: Int) -> Int
    v = x * 7
    match intToOpt(x)
        Option.Some(v) -> v
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
/// Rust backend before the fix (Rust emits pattern syntax by source
/// name, so its scoping never depended on the resolver slots).
///
/// x = 5: cell1 = 11, cell2 = 60, cell3 = 120, cell4 = 701,
///        cell5 = 51, cell6 = 1112, cell7 = 13, cell8 = 76,
///        cell9 = 1605, cell10 = 7021, cell11 = 105.
/// x = 1: cell1 = 3, cell2 = 20, cell3 = 32, cell4 = 301,
///        cell5 = 11, cell6 = 304, cell7 = 5, cell8 = 32,
///        cell9 = 401, cell10 = 3009, cell11 = 7.
const EXPECTED: &str = "11\n60\n120\n701\n51\n1112\n13\n76\n1605\n7021\n105\n3\n20\n32\n301\n11\n304\n5\n32\n401\n3009\n7";

/// Same-disease witness in the ALIAS pass (`src/ir/alias.rs`): the
/// statement binding `v = pick(outer)` extracts a Vector out of a Map,
/// and a later Int pattern binder reuses the spelling `v`. The pass
/// used to fetch the statement binding's slot BY NAME from the
/// last-wins `local_slots` map, judged the binder's Int slot (never a
/// collection → no flag), and left the extracted vector owned-eligible
/// — the VM's in-place fast path then mutated the arena entry the map
/// still holds.
///
/// Recorded pre-fix reds:
///   VM: `-989 / -997` for corrupt(5)/corrupt(1) — `Vector.set`
///       consumed (`mem::take`) the map's stored vector, so the
///       re-read `at0(pick(outer))` saw a gutted entry (-1), while
///       self-host and compiled Rust answered `7011 / 7003`.
///   The `control` twin (binder renamed `y`) answered 7011/7003
///   everywhere pre-fix — pinning the shadowing spelling as the
///   trigger.
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

fn corrupt(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = unwrapVec(Vector.set(v, 0, x))
    r = match Option.Some(x)
        Option.Some(v) -> v + 1
        Option.None -> 0
    at0(pick(outer)) * 1000 + at0(s) + r

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
    Console.print(String.fromInt(corrupt(5)))
    Console.print(String.fromInt(corrupt(1)))
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

/// The Map-in-Map variant of the same alias-pass witness. Pre-fix the
/// VM did not corrupt silently — it PANICKED at
/// `src/vm/execute/slots.rs:482`: "Map.set was granted ownership of
/// slot 4 statically, and something off the operand stack … still
/// holds it" — the issue-#926 runtime fence falsifying the static
/// grant the alias pass mis-stamped. Self-host and compiled Rust:
/// `7011 / 7003 / 7011 / 7003`.
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

fn corrupt(x: Int) -> Int
    outer = build()
    v = pick(outer)
    s = Map.set(v, 10, x)
    r = match Option.Some(x)
        Option.Some(v) -> v + 1
        Option.None -> 0
    at10(pick(outer)) * 1000 + at10(s) + r

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
    Console.print(String.fromInt(corrupt(5)))
    Console.print(String.fromInt(corrupt(1)))
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

/// corrupt(5), corrupt(1), control(5), control(1) — identical by
/// design: the shadowing spelling must not change the answer.
const ALIAS_EXPECTED: &str = "7011\n7003\n7011\n7003";

/// Same-disease witness in the ESCAPE pass (`src/ir/escape.rs`): two
/// SIBLING arms of an inline-eligible fn bind the same name. The pass
/// used to resolve arm-binder slots by name through the fn-level
/// last-wins map, handing BOTH arms the second arm's slot — the first
/// arm's body then spliced into the caller with its binder
/// unsubstituted, a dangling slot reference.
///
/// Recorded pre-fix reds:
///   VM: panicked at `src/vm/execute/dispatch.rs:216` — "index out of
///       bounds: the len is 1 but the index is 1".
///   emitted Rust: rustc E0425 "cannot find value `n` in this scope".
///   wasm-gc: validation error (pinned in
///       `tests/wasm_gc_codegen_regression.rs`).
///   self-host: `6 / 30` — correct (it does not run this pass).
/// `evalRenamed` is the renamed control: distinct binder spellings,
/// green everywhere pre-fix.
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

/// Same-class witness on the VM's `MirExpr::FnValue` path
/// (`src/vm/compiler/expr.rs`): a top-level fn referenced as a VALUE
/// was hijacked by a same-spelled pattern binder in the enclosing fn —
/// even though the binder is out of scope at the use site (the
/// resolver left `dbl` bare precisely because no local was in scope
/// there). The compiler's name-keyed local-slot check fired first and
/// loaded the binder's (uninitialized) slot.
///
/// Recorded pre-fix reds:
///   VM: "Type error [line 7]: cannot call non-function (got Unit =
///       \"Unit\") in callWith at ip=6" (exit 1).
///   compiled Rust: `26 / 6` — correct.
/// `control` renames the binder to `y`, green everywhere pre-fix.
/// No self-host executor here: the self-host parser does not accept
/// `Fn(..)`-typed parameters yet.
const FN_VALUE_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn callWith(f: Fn(Int) -> Int, x: Int) -> Int
    f(x)

fn hijack(x: Int) -> Int
    r = match Option.Some(x * 3)
        Option.Some(dbl) -> dbl + 1
        Option.None -> 0
    callWith(dbl, x) + r

fn control(x: Int) -> Int
    r = match Option.Some(x * 3)
        Option.Some(y) -> y + 1
        Option.None -> 0
    callWith(dbl, x) + r

fn main()
    ! [Console.print]
    Console.print(String.fromInt(hijack(5)))
    Console.print(String.fromInt(hijack(1)))
    Console.print(String.fromInt(control(5)))
    Console.print(String.fromInt(control(1)))
"#;

const FN_VALUE_EXPECTED: &str = "26\n6\n26\n6";

#[test]
fn alias_shadowed_vector_in_map_vm() {
    assert_eq!(
        run_vm("alias-vec-vm", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "the VM mutated a map-held vector through a shadowed statement binding"
    );
}

#[test]
fn alias_shadowed_vector_in_map_compiled_rust() {
    assert_eq!(
        run_compiled_rust("aliasvec", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "compiled Rust diverged on the alias-vector witness"
    );
}

#[test]
fn alias_shadowed_vector_in_map_self_host() {
    assert_eq!(
        run_self_host("alias-vec-sh", ALIAS_VEC_SRC),
        ALIAS_EXPECTED,
        "self-host diverged on the alias-vector witness"
    );
}

#[test]
fn alias_shadowed_map_in_map_vm() {
    assert_eq!(
        run_vm("alias-map-vm", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "the VM must not trip the issue-#926 ownership fence on a shadowed \
         statement binding (pre-fix: panic at slots.rs:482)"
    );
}

#[test]
fn alias_shadowed_map_in_map_compiled_rust() {
    assert_eq!(
        run_compiled_rust("aliasmap", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "compiled Rust diverged on the alias-map witness"
    );
}

#[test]
fn alias_shadowed_map_in_map_self_host() {
    assert_eq!(
        run_self_host("alias-map-sh", ALIAS_MAP_SRC),
        ALIAS_EXPECTED,
        "self-host diverged on the alias-map witness"
    );
}

#[test]
fn escape_sibling_arm_binders_vm() {
    assert_eq!(
        run_vm("esc-vm", ESCAPE_SIBLING_ARMS_SRC),
        ESCAPE_EXPECTED,
        "the escape pass spliced a sibling arm with the wrong binder slot \
         (pre-fix: VM slot-index panic)"
    );
}

#[test]
fn escape_sibling_arm_binders_compiled_rust() {
    assert_eq!(
        run_compiled_rust("escarms", ESCAPE_SIBLING_ARMS_SRC),
        ESCAPE_EXPECTED,
        "emitted Rust must compile and answer (pre-fix: rustc E0425 on the \
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
fn fn_value_shadowed_by_out_of_scope_binder_vm() {
    assert_eq!(
        run_vm("fnv-vm", FN_VALUE_SRC),
        FN_VALUE_EXPECTED,
        "a fn referenced as a value was hijacked by an out-of-scope pattern \
         binder (pre-fix: 'cannot call non-function (got Unit)')"
    );
}

#[test]
fn fn_value_shadowed_by_out_of_scope_binder_compiled_rust() {
    assert_eq!(
        run_compiled_rust("fnv", FN_VALUE_SRC),
        FN_VALUE_EXPECTED,
        "compiled Rust diverged on the fn-value witness"
    );
}

#[test]
fn witness_shadowed_pattern_binder_on_the_vm() {
    assert_eq!(
        run_vm("witness", WITNESS_SRC),
        WITNESS_OUT,
        "issue #948 witness: the VM must answer 11/3, not a constant"
    );
}

#[test]
fn shadow_matrix_vm() {
    assert_eq!(
        run_vm("matrix-vm", MATRIX_SRC),
        EXPECTED,
        "VM diverged from the shadow-matrix truth"
    );
}

#[test]
fn shadow_matrix_compiled_rust() {
    assert_eq!(
        run_compiled_rust("matrix", MATRIX_SRC),
        EXPECTED,
        "compiled Rust diverged from the shadow-matrix truth"
    );
}

#[test]
fn shadow_matrix_self_host() {
    assert_eq!(
        run_self_host("matrix-sh", MATRIX_SRC),
        EXPECTED,
        "self-host diverged from the shadow-matrix truth"
    );
}
