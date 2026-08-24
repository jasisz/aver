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
fn second_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [_, ..rest] -> first_or(rest, dflt)

fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn main() -> Unit
    ! [Args.get, Disk.writeText, Disk.readText, Disk.delete, Console.print]
    args: List<String> = Args.get()
    payload: String = second_or(args, "x")
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
fn large_console_print() {
    let dir = tempdir("print");
    let src = r#"
fn second_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [_, ..rest] -> first_or(rest, dflt)

fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn main() -> Unit
    ! [Args.get, Console.print]
    args: List<String> = Args.get()
    payload: String = second_or(args, "default")
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
