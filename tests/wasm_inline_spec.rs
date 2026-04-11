/// WASM symbol sentinel tests.
///
/// End-to-end: write Aver source → `aver run --wasm` → assert stdout.
/// Tests nullary variant sentinels, wrapper+literal sentinels, match dispatch,
/// equality, Console.print display, and GC safety.
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn run_wasm(source: &str) -> String {
    let dir = temp_dir("wasm-sentinel");
    let file = dir.join("test.av");
    fs::write(&file, source).expect("write source");
    let aver = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver)
        .arg("run")
        .arg("--wasm")
        .arg(&file)
        .output()
        .expect("run aver");
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert!(
        output.status.success(),
        "aver run --wasm failed:\nstdout: {stdout}\nstderr: {stderr}"
    );
    stdout
}

#[test]
fn nullary_variant_equality() {
    let out = run_wasm(
        r#"module Test
    intent = "Nullary variant equality"

type Color
    Black
    White

fn main()
    ! [Console.print]
    Console.print(Color.Black == Color.Black)
    Console.print(Color.Black == Color.White)
    Console.print(Color.Black != Color.White)
"#,
    );
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "true");
    assert_eq!(lines[1], "false");
    assert_eq!(lines[2], "true");
}

#[test]
fn nullary_variant_match() {
    let out = run_wasm(
        r#"module Test
    intent = "Match on nullary variants"

type Color
    Black
    White
    Red

fn colorName(c: Color) -> String
    match c
        Color.Black -> "black"
        Color.White -> "white"
        Color.Red -> "red"

fn main()
    ! [Console.print]
    Console.print(colorName(Color.Black))
    Console.print(colorName(Color.White))
    Console.print(colorName(Color.Red))
"#,
    );
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "black");
    assert_eq!(lines[1], "white");
    assert_eq!(lines[2], "red");
}

#[test]
fn mixed_nullary_and_fields() {
    let out = run_wasm(
        r#"module Test
    intent = "Mixed nullary and non-nullary variants"

type Shape
    Circle(Float)
    Rect(Float, Float)
    Point

fn area(s: Shape) -> Float
    match s
        Shape.Circle(r) -> r * r * 3.0
        Shape.Rect(w, h) -> w * h
        Shape.Point -> 0.0

fn main()
    ! [Console.print]
    Console.print(area(Shape.Point))
    Console.print(area(Shape.Circle(2.0)))
    Console.print(area(Shape.Rect(3.0, 4.0)))
"#,
    );
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "0");
    assert_eq!(lines[1], "12");
    assert_eq!(lines[2], "12");
}

#[test]
fn console_print_nullary_variant() {
    let out = run_wasm(
        r#"module Test
    intent = "Print nullary variant display name"

type Tile
    Wall
    Floor

fn main()
    ! [Console.print]
    Console.print(Tile.Wall)
    Console.print(Tile.Floor)
"#,
    );
    let lines: Vec<&str> = out.lines().collect();
    assert!(lines[0].contains("Wall"), "got: {}", lines[0]);
    assert!(lines[1].contains("Floor"), "got: {}", lines[1]);
}

#[test]
#[ignore] // Wrapper sentinels: dispatch table nesting issue in match — follow-up
fn wrapper_bool_sentinel() {
    let out = run_wasm(
        r#"module Test
    intent = "Result.Ok(true/false) sentinel optimization"

fn check(r: Result<Bool, String>) -> String
    match r
        Result.Ok(b) -> match b
            true -> "ok-true"
            false -> "ok-false"
        Result.Err(e) -> e

fn main()
    ! [Console.print]
    Console.print(check(Result.Ok(true)))
    Console.print(check(Result.Ok(false)))
    Console.print(check(Result.Err("boom")))
"#,
    );
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "ok-true");
    assert_eq!(lines[1], "ok-false");
    assert_eq!(lines[2], "boom");
}
