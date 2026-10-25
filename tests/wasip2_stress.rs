//! Wasip2 stress regression tests.
//!
//! Each test compiles a small Aver program and runs it through
//! `aver run --wasip2`. Catches the two bugs squashed during the
//! 2026-05-09 stress sweep — and is meant to keep them squashed.
//!
//! 1. `roundtrip_100x` — write+read+delete loop verifying the
//!    `Disk.writeText` open-flags constant. Pre-fix this returned
//!    `err=200` (every second write failed because flags=5 =
//!    `CREATE | EXCLUSIVE` rejected pre-existing paths). Now
//!    flags=9 (`CREATE | TRUNCATE`) and the second-write succeeds,
//!    leaving only the second-delete-fail per iteration.
//! 2. `large_file_write_read_roundtrip` — 50 KB write+read +
//!    length verification. Catches the
//!    `blocking-write-and-flush > 4096 bytes` trap (wasmtime-wasi
//!    hard-rejects single calls over 4096; the chunked write loop
//!    in `emit_disk_write_text` walks the buffer in 4096-byte
//!    slices).
//! 3. `large_console_print` — same `> 4096` regression but on the
//!    Console.* call site (separate emitter,
//!    `emit_console_print_wasip2`).
//! 4. `list_dir_many_entries` — 200-entry listDir, exercises the
//!    `read-directory-entry` loop until `Ok(None)` for many
//!    iterations (catches resource leaks / off-by-one).
//! 5. `random_distribution_sanity` — 10 000 `Random.int(0, 99)`
//!    samples; avg falls in [40, 60] and full 0..99 range hit.
//! 6. `time_sleep_at_least_requested` — 10×50 ms sleep loop
//!    completes in >= 500 ms (pollable model is real, not
//!    busy-waiting / no-op).
//! 7. `binary_disk_short_read_and_size` — byte-exact whole/positional
//!    reads, write/append, EOF shortening, metadata size, and a large
//!    requested upper bound that must not be preallocated eagerly.
//! 8. `disk_sync_accepts_a_file_and_a_directory_and_leaves_content_alone`
//!    — `emit_disk_sync` opens the path itself, so it carries its own
//!    open-flags constant. Plain `open-at` (flags = 0, read-only) is
//!    what admits a file AND a directory, and a directory sync is the
//!    half that makes a newly created file's name durable. The absent
//!    path must stay an error, and the file's bytes must survive.
//! 9. `disk_sync_repeated_does_not_exhaust_descriptors` — 500 syncs;
//!    each opens a descriptor that the emitter has to drop again.
//! 10. `disk_reads_larger_than_linear_memory` — a file larger than the
//!     module's 128 MiB of linear memory read whole and in chunks. Each
//!     chunk the host hands over lands where the one before it did and
//!     every read gives its linear memory back, so reading never grows
//!     with the file; before, the reads ran out of linear memory.
//!
//! All fixtures run in a per-test tempdir which we preopen as the
//! component's working directory (the embedded runner in
//! `aver run --wasip2` already does `preopened_dir(".", ".",
//! DirPerms::all(), FilePerms::all())`); paths in the Aver source
//! resolve against that dir.

#![cfg(feature = "wasip2")]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-stress-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

fn run_wasip2(dir: &Path, fixture: &Path, args: &[&str]) -> std::process::Output {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(dir).arg("run").arg("--wasip2").arg(fixture);
    if !args.is_empty() {
        cmd.arg("--");
        for a in args {
            cmd.arg(a);
        }
    }
    cmd.output().expect("aver run --wasip2 to launch")
}

fn assert_ok(output: &std::process::Output, fixture: &str) {
    assert!(
        output.status.success(),
        "{fixture} failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn stdout(output: &std::process::Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// Two different keys the VM hashes to the same 64 bits stay two keys here
/// too. `cross_backend_stress` holds the VM, wasm-gc and self-host to the same
/// expected output; this is the wasip2 run of that program.
#[test]
fn colliding_map_keys_stay_two_keys() {
    let dir = tempdir("collide");
    let fixture = write_fixture(
        &dir,
        "collide.av",
        include_str!("fixtures/map_colliding_keys_app.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "collide.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/map_colliding_keys_app.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// A fallback with an effect runs before the value is looked at, present or
/// missing. `cross_backend_stress` holds the VM and wasm-gc to the same output.
#[test]
fn effectful_fallback_runs_first() {
    let dir = tempdir("fallback");
    let fixture = write_fixture(
        &dir,
        "fallback.av",
        include_str!("fixtures/effectful_fallback_app.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "fallback.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/effectful_fallback_app.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// The error of `Result.fromOption` may unwrap another value itself; the
/// present value still comes out. `cross_backend_stress` holds the VM and
/// wasm-gc to the same output.
#[test]
fn nested_unwrap_in_error_keeps_value() {
    let dir = tempdir("nested-unwrap");
    let fixture = write_fixture(
        &dir,
        "nested.av",
        include_str!("fixtures/nested_unwrap_error_app.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "nested.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/nested_unwrap_error_app.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// 200 000 nested calls fit, and a recursion that never ends is Wasmtime's
/// stack-overflow trap, said as an Aver error with exit status 1 rather than
/// a host abort. `cross_backend_stress` holds the VM and wasm-gc to the same.
#[test]
fn deep_recursion_fits_and_runaway_recursion_is_an_aver_error() {
    let dir = tempdir("deep");
    let fixture = write_fixture(&dir, "deep.av", include_str!("fixtures/deep_recursion.av"));
    let out = run_wasip2(&dir, &fixture, &["200000"]);
    assert_ok(&out, "deep.av 200000");
    assert_eq!(stdout(&out).trim(), "20000100000");

    let out = run_wasip2(&dir, &fixture, &[]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "deep.av runaway:\n{stderr}");
    assert!(
        stderr.contains("stack overflow: recursion too deep")
            && !stderr.contains("has overflowed its stack"),
        "deep.av runaway:\n{stderr}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// A literal call to the smart constructor of an `Int` refinement gives the
/// refined value on wasip2 as on the VM (`cross_backend_stress` holds the VM
/// and wasm-gc to the same expected output).
#[test]
fn literal_int_refinement_discharge_matches_expected() {
    let dir = tempdir("litint");
    let fixture = write_fixture(
        &dir,
        "ranges.av",
        include_str!("fixtures/literal_int_refinement.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "ranges.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/literal_int_refinement.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `Bits.popCount` gives the VM's answers on wasip2 (`cross_backend_stress`
/// holds the VM and wasm-gc to the same expected output).
#[test]
fn bits_pop_count_matches_expected() {
    let dir = tempdir("popcount");
    let fixture = write_fixture(
        &dir,
        "popcount.av",
        include_str!("fixtures/bits_pop_count.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "popcount.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/bits_pop_count.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// An empty literal bound to a name and typed by a later read builds on
/// wasip2 and prints what the VM prints (`cross_backend_stress` runs the
/// VM and wasm-gc).
#[test]
fn bound_empty_literal_settled_later_matches_expected() {
    let dir = tempdir("bound-empty");
    let fixture = write_fixture(
        &dir,
        "bound_empty.av",
        include_str!("fixtures/bound_empty_settled_later_app.av"),
    );
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "bound_empty.av");
    assert_eq!(
        stdout(&out).trim(),
        include_str!("fixtures/bound_empty_settled_later_app.expected").trim()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `==` on recursive values sharing subterms, built apart, compares each
/// shared pair once, and a map keyed by them hashes each shared node once:
/// at 64 levels it answers as at 12, within a minute. `cross_backend_stress`
/// runs the same file on wasm-gc.
#[test]
fn shared_recursive_sum_equality_ends() {
    let dir = tempdir("shared-sum");
    let fixture = write_fixture(
        &dir,
        "shared.av",
        include_str!("fixtures/shared_recursive_sum_app.av"),
    );
    let run = |depth: &str| {
        let mut child = Command::new(env!("CARGO_BIN_EXE_aver"))
            .current_dir(&dir)
            .args(["run", "--wasip2"])
            .arg(&fixture)
            .args(["--", depth])
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("aver run --wasip2 to launch");
        let started = std::time::Instant::now();
        while child.try_wait().expect("run is waitable").is_none() {
            if started.elapsed() > std::time::Duration::from_secs(60) {
                let _ = child.kill();
                panic!("depth {depth} did not end within a minute");
            }
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
        let out = child.wait_with_output().expect("run output");
        assert_ok(&out, "shared.av");
        stdout(&out).trim().to_string()
    };
    let shallow = run("12");
    let tail = "map 2 3 false\nkeys 2 true\nkey record true false mutual true false\n\
                dag true false\nrecord true\nmutual true false";
    assert!(shallow.ends_with(tail), "{shallow}");
    assert_eq!(run("64"), shallow);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn roundtrip_100x() {
    let dir = tempdir("rt100");
    let src = r#"
fn loop_io(n: Int, ok_count: Int, err_count: Int) -> Tuple<Int, Int>
    ! [Disk.writeText, Disk.readText, Disk.delete]
    match n == 0
        true -> (ok_count, err_count)
        false -> step(n, ok_count, err_count)

fn step(n: Int, ok_count: Int, err_count: Int) -> Tuple<Int, Int>
    ! [Disk.writeText, Disk.readText, Disk.delete]
    path: String = "rt_{n}.txt"
    body: String = "iter {n}"
    a: Int = match Disk.writeText(path, body)
        Result.Ok(_) -> ok_count + 1
        Result.Err(_) -> ok_count
    b: Int = match Disk.writeText(path, body)
        Result.Ok(_) -> err_count
        Result.Err(_) -> err_count + 1
    c: Int = match Disk.readText(path)
        Result.Ok(_) -> a + 1
        Result.Err(_) -> a
    d: Int = match Disk.delete(path)
        Result.Ok(_) -> c + 1
        Result.Err(_) -> c
    e: Int = match Disk.delete(path)
        Result.Ok(_) -> b
        Result.Err(_) -> b + 1
    loop_io(n - 1, d, e)

fn main() -> Unit
    ! [Disk.writeText, Disk.readText, Disk.delete, Console.print]
    res: Tuple<Int, Int> = loop_io(100, 0, 0)
    match res
        (ok, err) -> Console.print("ok={ok} err={err}")
"#;
    let fixture = write_fixture(&dir, "rt.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "rt.av");
    let s = stdout(&out);
    assert!(
        s.contains("ok=300 err=100"),
        "expected 'ok=300 err=100' (3 successful ops × 100 iter + 1 second-delete-fail × 100), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn large_file_write_read_roundtrip() {
    let dir = tempdir("large");
    let src = r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn main() -> Unit
    ! [Args.get, Disk.writeText, Disk.readText, Disk.delete, Console.print]
    args: List<String> = Args.get()
    payload: String = first_or(args, "x")
    write_len: Int = String.len(payload)
    match Disk.writeText("big.txt", payload)
        Result.Ok(_) -> Console.print("write ok")
        Result.Err(e) -> Console.print("write err: {e}")
    read_len: Int = match Disk.readText("big.txt")
        Result.Ok(c) -> String.len(c)
        Result.Err(_) -> -1
    Console.print("write_len={write_len} read_len={read_len}")
    match Disk.delete("big.txt")
        Result.Ok(_) -> Console.print("ok")
        Result.Err(_) -> Console.print("cleanup err")
"#;
    let fixture = write_fixture(&dir, "large.av", src);
    let payload = "x".repeat(50_000);
    let out = run_wasip2(&dir, &fixture, &[&payload]);
    assert_ok(&out, "large.av");
    let s = stdout(&out);
    assert!(s.contains("write ok"), "expected 'write ok', got:\n{s}");
    assert!(
        s.contains("write_len=50000 read_len=50000"),
        "expected matched write_len / read_len, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn binary_disk_short_read_and_size() {
    let dir = tempdir("disk-bytes");
    std::fs::write(dir.join("source.bin"), [0, 127, 128, 255, 1, 2])
        .expect("write binary source fixture");
    std::fs::write(
        dir.join("large.bin"),
        (0..50_000)
            .map(|index| (index % 256) as u8)
            .collect::<Vec<_>>(),
    )
    .expect("write large binary source fixture");
    let src = r#"module Probe
    intent = "Exercise byte-exact Disk operations through direct WASI bindings."
    depends [Bytes]
    exposes [main]
    effects [Console.print, Disk.readBytes, Disk.readBytesAt, Disk.writeBytes, Disk.appendBytes, Disk.size]

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readBytes, Disk.readBytesAt, Disk.writeBytes, Disk.appendBytes, Disk.size]
    all = Disk.readBytes("source.bin")?
    part = Disk.readBytesAt("source.bin", 2, 99)?
    past = Disk.readBytesAt("source.bin", 99, 4)?
    Disk.writeBytes("copy.bin", all)?
    Disk.appendBytes("copy.bin", part)?
    length = Disk.size("source.bin")?
    large = Disk.readBytesAt("large.bin", 100, 5000000000)?
    largeLength = List.len(Bytes.octets(large))
    Console.print("{Bytes.toHex(all)}:{Bytes.toHex(part)}:{Bytes.toHex(past)}:{length}:{largeLength}")
    Result.Ok(Unit)
"#;
    let fixture = write_fixture(&dir, "disk_bytes.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "disk_bytes.av");
    let s = stdout(&out);
    assert!(
        s.contains("007f80ff0102:80ff0102::6:49900"),
        "expected byte-exact whole/short reads and metadata size, got:\n{s}"
    );
    assert_eq!(
        std::fs::read(dir.join("copy.bin")).expect("read binary copy"),
        [0, 127, 128, 255, 1, 2, 128, 255, 1, 2]
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn disk_sync_accepts_a_file_and_a_directory_and_leaves_content_alone() {
    let dir = tempdir("disk-sync");
    std::fs::create_dir_all(dir.join("nest")).expect("create nested fixture dir");
    std::fs::write(dir.join("nest/payload.bin"), [0, 127, 128, 255])
        .expect("write binary sync fixture");
    let src = r#"module Probe
    intent = "Exercise Disk.sync through direct WASI bindings."
    exposes [main]
    effects [Console.print, Disk.sync]

fn shape(outcome: Result<Unit, String>) -> String
    ? "Reduce an outcome to ok/err so host error text stays out of the output."
    match outcome
        Result.Ok(_) -> "ok"
        Result.Err(_) -> "err"

fn main() -> Unit
    ! [Console.print, Disk.sync]
    file: String = shape(Disk.sync("nest/payload.bin"))
    parent: String = shape(Disk.sync("nest"))
    root: String = shape(Disk.sync("."))
    missing: String = shape(Disk.sync("nest/absent.bin"))
    Console.print("file={file} parent={parent} root={root} missing={missing}")
"#;
    let fixture = write_fixture(&dir, "disk_sync.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "disk_sync.av");
    let s = stdout(&out);
    // A file AND a directory must both open. `open-flags = directory`
    // would lose the file; `open-flags = create | exclusive` would lose
    // the existing file and wrongly succeed on the absent one.
    assert!(
        s.contains("file=ok parent=ok root=ok missing=err"),
        "expected sync to accept a file, a directory, and the preopen root, and to \
         refuse an absent path, got:\n{s}"
    );
    // A read-only open. `open-flags = truncate` would empty the file.
    assert_eq!(
        std::fs::read(dir.join("nest/payload.bin")).expect("read synced payload"),
        [0, 127, 128, 255],
        "Disk.sync must not rewrite the file it flushes"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn disk_sync_repeated_does_not_exhaust_descriptors() {
    let dir = tempdir("disk-sync-loop");
    std::fs::write(dir.join("log.bin"), b"seed").expect("write sync loop fixture");
    // Every call opens a fresh descriptor and must drop it again. A
    // missing `resource-drop` leaks one handle per iteration.
    let src = r#"module Probe
    intent = "Sync the same path many times; each call must release its descriptor."
    exposes [main]
    effects [Console.print, Disk.sync]

fn flushMany(n: Int, ok: Int) -> Int
    ! [Disk.sync]
    match n == 0
        true -> ok
        false -> flushMany(n - 1, step(ok))

fn step(ok: Int) -> Int
    ! [Disk.sync]
    match Disk.sync("log.bin")
        Result.Ok(_) -> ok + 1
        Result.Err(_) -> ok

fn main() -> Unit
    ! [Console.print, Disk.sync]
    ok: Int = flushMany(500, 0)
    Console.print("ok={ok}")
"#;
    let fixture = write_fixture(&dir, "disk_sync_loop.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "disk_sync_loop.av");
    let s = stdout(&out);
    assert!(
        s.contains("ok=500"),
        "expected 500 successful syncs, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn large_console_print() {
    let dir = tempdir("print");
    let src = r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn main() -> Unit
    ! [Args.get, Console.print]
    args: List<String> = Args.get()
    payload: String = first_or(args, "default")
    Console.print(payload)
"#;
    let fixture = write_fixture(&dir, "print.av", src);
    // 5000 bytes — over the 4096-byte single-call cap. Pre-fix
    // this trapped with "Buffer too large for blocking-write-
    // and-flush". Post-fix the chunked-write loop walks it in
    // 4096-byte slices. The `+ 1` covers the trailing newline
    // the `__rt_println_to_lm` bridge helper appends so the
    // wasip2 `Console.print` matches VM / AverBridge's `println!`
    // semantic (see PR #204).
    let payload = "y".repeat(5_000);
    let out = run_wasip2(&dir, &fixture, &[&payload]);
    assert_ok(&out, "print.av");
    let s = stdout(&out);
    assert_eq!(
        s.len(),
        5_001,
        "expected exactly 5001 bytes printed (5000 'y' + '\\n'), got {} bytes",
        s.len()
    );
    assert!(s.ends_with('\n'), "expected trailing newline");
    assert!(
        s[..5_000].chars().all(|c| c == 'y'),
        "expected first 5000 bytes to all be 'y'"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn list_dir_many_entries() {
    let dir = tempdir("listdir");
    let stress_dir = dir.join("entries");
    std::fs::create_dir_all(&stress_dir).expect("mkdir entries");
    for i in 0..200 {
        std::fs::write(stress_dir.join(format!("file_{i:03}.txt")), b"x").expect("write entry");
    }

    let src = r#"
fn count(items: List<String>, n: Int) -> Int
    match items
        [] -> n
        [_, ..rest] -> count(rest, n + 1)

fn main() -> Unit
    ! [Disk.listDir, Console.print]
    match Disk.listDir("entries")
        Result.Ok(items) -> Console.print("count={count(items, 0)}")
        Result.Err(e) -> Console.print("err: {e}")
"#;
    let fixture = write_fixture(&dir, "listdir.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "listdir.av");
    let s = stdout(&out);
    assert!(
        s.contains("count=200"),
        "expected 'count=200' from 200-entry listDir, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn random_distribution_sanity() {
    let dir = tempdir("random");
    let src = r#"
fn loop_rand(n: Int, sum: Int, mn: Int, mx: Int) -> Tuple<Int, Int, Int>
    ! [Random.int]
    match n == 0
        true -> (sum, mn, mx)
        false -> step(n, sum, mn, mx)

fn step(n: Int, sum: Int, mn: Int, mx: Int) -> Tuple<Int, Int, Int>
    ! [Random.int]
    r: Int = Random.int(0, 99)
    new_mn: Int = match r < mn
        true -> r
        false -> mn
    new_mx: Int = match r > mx
        true -> r
        false -> mx
    loop_rand(n - 1, sum + r, new_mn, new_mx)

fn main() -> Unit
    ! [Random.int, Console.print]
    res: Tuple<Int, Int, Int> = loop_rand(10000, 0, 100, -1)
    match res
        (sum, mn, mx) -> Console.print("avg={Int.div(sum, 10000)} min={mn} max={mx}")
"#;
    let fixture = write_fixture(&dir, "random.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "random.av");
    let s = stdout(&out);
    // Ideal uniform avg = 49.5; allow [40, 60]. 10k samples gives
    // narrow enough confidence interval that anything outside is a
    // real distribution bug.
    let avg_line = s.lines().find(|l| l.starts_with("avg=")).expect(&s);
    let avg: i64 = avg_line
        .strip_prefix("avg=")
        .and_then(|rest| rest.split_whitespace().next())
        .and_then(|n| n.parse().ok())
        .expect(&s);
    assert!(
        (40..=60).contains(&avg),
        "Random.int(0, 99) avg over 10000 samples should be in [40, 60], got {avg} (full line: {avg_line})"
    );
    assert!(
        s.contains("min=0") || s.contains("min=1") || s.contains("min=2"),
        "min should be at or near 0 over 10k samples; line: {avg_line}"
    );
    assert!(
        s.contains("max=99") || s.contains("max=98") || s.contains("max=97"),
        "max should be at or near 99 over 10k samples; line: {avg_line}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn time_sleep_at_least_requested() {
    let dir = tempdir("sleep");
    let src = r#"
fn loop_sleep(n: Int) -> Unit
    ! [Time.sleep]
    match n == 0
        true -> Unit
        false -> step(n)

fn step(n: Int) -> Unit
    ! [Time.sleep]
    Time.sleep(50)
    loop_sleep(n - 1)

fn main() -> Unit
    ! [Time.sleep, Time.unixMs, Console.print]
    t0: Int = Time.unixMs()
    loop_sleep(10)
    t1: Int = Time.unixMs()
    Console.print("elapsed_ms={t1 - t0}")
"#;
    let fixture = write_fixture(&dir, "sleep.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "sleep.av");
    let s = stdout(&out);
    let line = s.lines().find(|l| l.starts_with("elapsed_ms=")).expect(&s);
    let elapsed: i64 = line
        .strip_prefix("elapsed_ms=")
        .unwrap()
        .trim()
        .parse()
        .expect(&s);
    // 10 × 50 ms = 500 ms minimum. Tolerate up to 2× for CI noise
    // (real wall-clock test). If we ever start busy-waiting or
    // no-op'ing Time.sleep, this drops to ~0 and fails.
    assert!(
        elapsed >= 500,
        "10×50ms loop should take >= 500ms, got {elapsed}ms (Time.sleep regression?)"
    );
    assert!(
        elapsed < 2000,
        "10×50ms loop should be well under 2s, got {elapsed}ms"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn disk_reads_larger_than_linear_memory() {
    let dir = tempdir("disk-large");
    let size: usize = 136 << 20;
    let mut content = vec![b'a'; size];
    content[size - 1] = b'z';
    std::fs::write(dir.join("large.txt"), &content).expect("write large fixture");
    drop(content);
    let src = r#"module Probe
    intent = "Read a file larger than linear memory, whole and a chunk at a time."
    depends [Bytes]
    exposes [main]
    effects [Console.print, Disk.readText, Disk.readBytesAt]

fn chunks(offset: Int, left: Int, total: Int) -> Result<Int, String>
    ? "Reads left chunks of 1 MiB, 3 MiB apart, and adds up their lengths."
    ! [Disk.readBytesAt]
    match left == 0
        true -> Result.Ok(total)
        false -> chunk(offset, left, total, Disk.readBytesAt("large.txt", offset, 1048576)?)

fn chunk(offset: Int, left: Int, total: Int, bytes: Bytes) -> Result<Int, String>
    ? "Goes on after one chunk."
    ! [Disk.readBytesAt]
    chunks(offset + 3145728, left - 1, total + Bytes.len(bytes))

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText, Disk.readBytesAt]
    text = Disk.readText("large.txt")?
    read = chunks(0, 46, 0)?
    ends = String.endsWith(text, "az")
    Console.print("{String.byteLength(text)}:{ends}:{read}")
    Result.Ok(Unit)
"#;
    let fixture = write_fixture(&dir, "disk_large.av", src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert_ok(&out, "disk_large.av");
    assert_eq!(stdout(&out).trim(), format!("{size}:true:{}", 46 << 20));
    let _ = std::fs::remove_dir_all(&dir);
}
