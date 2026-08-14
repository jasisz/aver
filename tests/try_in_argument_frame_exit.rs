//! `?` in argument position must return from the function that wrote it.
//!
//! The VM has three frame-exit paths, all in `src/vm/execute/dispatch.rs`:
//! `RETURN`, the `?!` error branch of `CALL_PAR`, and `PROPAGATE_ERR`. Only
//! `RETURN` knows that the frame it is leaving may not exist.
//!
//! A chunk that makes no user-function call is classified `leaf`
//! (`classify_leaf_chunk`, `src/vm/compiler/classify.rs`) and its callers'
//! `CALL_KNOWN` is upgraded to the frameless `CALL_LEAF`, which pushes NO
//! `CallFrame` — the caller's context is parked in the interpreter-local
//! `leaf_return` instead. `RETURN` consults that local first. The two error
//! exits do not: they run `self.frames.pop()`, which in a frameless leaf pops
//! the CALLER's frame, so the error returns out of the caller instead of out of
//! the function whose body contains the `?`.
//!
//! Two conditions have to meet for the shape to bite, which is why it survived:
//! the enclosing body must contain no user-function call (any `f(x)` in it
//! restores the frame, and so does any `let` binding, since the `CALL_LEAF`
//! upgrade also demands `local_count == arity`) — and the `?` must sit
//! mid-expression rather than in a let binding. `g(f(x)?)` where both `f` and
//! `g` are builtins is exactly that intersection, and it is the ordinary way to
//! write "read the name, then read the file".
//!
//! The self-hosted interpreter and the Rust backend both return from the
//! enclosing function. The VM ends the whole program with `main returned
//! error` and exit 1 — an error the caller was written to handle is reported as
//! an uncaught failure of `main`, and the lines after the call never run.
//!
//! Every program below prints a final `reached the end` marker: that line is
//! the one the buggy VM never reaches.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-try-arg-{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

fn run(prefix: &str, source: &str, extra: &[&str]) -> String {
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&module_root)
        .arg("run")
        .arg(&path)
        .arg("--module-root")
        .arg(&module_root)
        .args(extra)
        .output()
        .expect("expected `aver run` to execute");
    let rendered = format_output(&out);
    let succeeded = out.status.success();
    let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
    let _ = fs::remove_dir_all(&module_root);
    assert!(
        succeeded,
        "{prefix}: `aver run {extra:?}` exited non-zero — an error propagated \
         with `?` escaped the function that wrote it and ended the \
         program:\n{rendered}"
    );
    stdout
}

/// The VM (`aver run`) and the self-hosted interpreter (`--self-host`, the
/// semantics oracle) must agree, and both must equal `expected`.
fn assert_backends_agree(prefix: &str, source: &str, expected: &str) {
    let vm = run(prefix, source, &[]);
    assert_eq!(
        vm, expected,
        "{prefix}: the VM disagrees with the expected semantics of `?`"
    );
    let self_host = run(&format!("{prefix}-sh"), source, &["--self-host"]);
    assert_eq!(
        self_host, vm,
        "{prefix}: the VM disagrees with the self-hosted interpreter"
    );
}

/// The minimal witness. `leafTry`'s body calls only builtins and binds no
/// local, so the chunk is a frameless leaf; the `?` sits in `Int.div`'s second
/// argument, with `100` already pushed. The error belongs to `leafTry`, and
/// `main` is written to handle it.
#[test]
fn try_in_an_argument_returns_from_the_enclosing_function() {
    let src = r#"module TryInArgument
    intent = "`?` inside an argument returns from the function that wrote it"
    depends []
    effects [Console]

fn leafTry(s: String) -> Result<Int, String>
    ? "Divides by a parsed number; every call in the body is a builtin."
    Int.div(100, Int.fromString(s)?)

fn describe(r: Result<Int, String>) -> String
    ? "Renders a result without pinning the error text."
    match r
        Result.Ok(v) -> "ok {v}"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(leafTry("abc")))
    Console.print(describe(leafTry("4")))
    Console.print("reached the end")
"#;
    assert_backends_agree("minimal", src, "err\nok 25\nreached the end");
}

/// Which argument the `?` sits in decides how much of the enclosing call is
/// already on the stack when the frame exits. `secondArg` fires with `100`
/// pushed; `firstArg` fires with nothing pushed but the leaf's own parameters
/// still live below `bp`. Both have to unwind to the same place.
#[test]
fn try_unwinds_from_either_argument_position() {
    let src = r#"module TryArgumentPosition
    intent = "`?` in the first and in the second argument unwind alike"
    depends []
    effects [Console]

fn secondArg(s: String) -> Result<Int, String>
    ? "`?` fires with the first argument already on the stack."
    Int.div(100, Int.fromString(s)?)

fn firstArg(s: String, d: Int) -> Result<Int, String>
    ? "`?` fires before any sibling argument is pushed."
    Int.div(Int.fromString(s)?, d)

fn describe(r: Result<Int, String>) -> String
    ? "Renders a result without pinning the error text."
    match r
        Result.Ok(v) -> "ok {v}"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(secondArg("bad")))
    Console.print(describe(firstArg("bad", 5)))
    Console.print(describe(secondArg("5")))
    Console.print(describe(firstArg("100", 4)))
    Console.print("reached the end")
"#;
    assert_backends_agree(
        "argument_position",
        src,
        "err\nerr\nok 20\nok 25\nreached the end",
    );
}

/// Two `?` in one argument list, and a `?` two call levels down. The second
/// case leaves an unwrapped value from the FIRST `?` on the stack when the
/// second one fires, so a boundary that truncates to the wrong base pointer
/// takes that value with it.
#[test]
fn try_unwinds_from_nested_and_repeated_argument_positions() {
    let src = r#"module TryNested
    intent = "repeated and nested `?` in argument position unwind to the same frame"
    depends []
    effects [Console]

fn twoTries(a: String, b: String) -> Result<Int, String>
    ? "Both arguments propagate; the first is already unwrapped when the second fires."
    Int.div(Int.fromString(a)?, Int.fromString(b)?)

fn twoLevels(s: String) -> Result<Int, String>
    ? "`?` sits in the argument of a call that is itself an argument."
    Int.div(1000, Int.max(Int.fromString(s)?, 1))

fn describe(r: Result<Int, String>) -> String
    ? "Renders a result without pinning the error text."
    match r
        Result.Ok(v) -> "ok {v}"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(twoTries("bad", "2")))
    Console.print(describe(twoTries("100", "bad")))
    Console.print(describe(twoTries("100", "4")))
    Console.print(describe(twoLevels("bad")))
    Console.print(describe(twoLevels("9")))
    Console.print("reached the end")
"#;
    assert_backends_agree(
        "nested",
        src,
        "err\nerr\nok 25\nerr\nok 111\nreached the end",
    );
}

/// The `?!` error branch of `CALL_PAR` is the third frame-exit path and pops a
/// frame the same unconditional way. A product of two builtin calls, bound to
/// nothing, keeps the enclosing chunk on the frameless-leaf path, so the failing
/// branch returns out of `main` instead of out of `leafPar`.
#[test]
fn an_independent_product_failure_returns_from_the_enclosing_function() {
    let src = r#"module TryProduct
    intent = "a failing `?!` returns from the function that wrote it"
    depends []
    effects [Console]

fn leafPar(a: Int, b: Int) -> Result<Tuple<Int, Int>, String>
    ? "Two independent divisions, no local binding and no user call."
    Result.Ok((Int.div(100, a), Int.div(100, b))?!)

fn describe(r: Result<Tuple<Int, Int>, String>) -> String
    ? "Renders a result without pinning the error text."
    match r
        Result.Ok(_) -> "ok"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(leafPar(0, 5)))
    Console.print(describe(leafPar(5, 0)))
    Console.print(describe(leafPar(4, 5)))
    Console.print("reached the end")
"#;
    assert_backends_agree("product", src, "err\nerr\nok\nreached the end");
}

/// The original witness: `Disk.readText(Disk.readText(p)?)` — read a file name
/// out of one file, then read that file. Both calls are effects and neither is
/// a user function, so the enclosing body is a frameless leaf and the missing
/// file ends the program instead of returning `Err` to the caller.
#[test]
fn try_in_an_effectful_argument_returns_from_the_enclosing_function() {
    let src = r#"module TryEffectfulArgument
    intent = "`?` inside an effectful call's argument returns from the enclosing function"
    depends []
    effects [Console, Disk]

fn indirect(p: String) -> Result<String, String>
    ? "Reads a file whose name is stored in another file."
    ! [Disk.readText]
    Disk.readText(Disk.readText(p)?)

fn main() -> Unit
    ! [Console.print, Disk.readText]
    match indirect("no-such-index-file-9d2f.txt")
        Result.Ok(v) -> Console.print("ok {v}")
        Result.Err(_) -> Console.print("err")
    Console.print("reached the end")
"#;
    assert_backends_agree("effectful", src, "err\nreached the end");
}

/// Controls. Neither shape is on the frameless-leaf path, and both were already
/// correct — they pin that the fix is about where the error exits, not about
/// what `?` computes.
///
/// `letBound` binds the propagated value, which pushes `local_count` past
/// `arity` and blocks the `CALL_LEAF` upgrade. `viaUserFn` calls a user
/// function, which makes the chunk non-leaf outright. Both must keep answering
/// exactly as the argument-position spelling does.
#[test]
fn let_bound_and_non_leaf_spellings_stay_correct() {
    let src = r#"module TryControl
    intent = "the let-bound and non-leaf spellings of the same computation"
    depends []
    effects [Console]

fn divideBy(n: Int, d: Int) -> Result<Int, String>
    ? "A user function, so its caller is not a leaf."
    Int.div(n, d)

fn letBound(s: String) -> Result<Int, String>
    ? "Control: the propagated value is bound before it is used."
    n = Int.fromString(s)?
    Int.div(100, n)

fn viaUserFn(s: String) -> Result<Int, String>
    ? "Control: the enclosing call is a user function, so a frame exists."
    divideBy(Int.fromString(s)?, 2)

fn describe(r: Result<Int, String>) -> String
    ? "Renders a result without pinning the error text."
    match r
        Result.Ok(v) -> "ok {v}"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(letBound("bad")))
    Console.print(describe(letBound("4")))
    Console.print(describe(viaUserFn("bad")))
    Console.print(describe(viaUserFn("50")))
    Console.print("reached the end")
"#;
    assert_backends_agree("control", src, "err\nok 25\nerr\nok 25\nreached the end");
}
