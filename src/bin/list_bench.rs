use std::env;
use std::hint::black_box;
use std::time::{Duration, Instant};

use aver::ast::TopLevel;
use aver::interpreter::{Interpreter, Value};
use aver::resolver;
use aver::source::parse_source;
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::value::list_from_vec;

const PROGRAM: &str = r#"
module ListBench

fn buildPrepend(n: Int, acc: List<Int>) -> List<Int>
    match n
        0 -> acc
        _ -> buildPrepend(n - 1, List.prepend(n, acc))

fn buildAppend(n: Int, acc: List<Int>) -> List<Int>
    match n
        0 -> acc
        _ -> buildAppend(n - 1, List.append(acc, n))

fn finishPrepend(n: Int) -> List<Int>
    List.reverse(buildPrepend(n, []))

fn sumList(xs: List<Int>, acc: Int) -> Int
    match xs
        [] -> acc
        [h, ..t] -> sumList(t, acc + h)

fn concatLists(a: List<Int>, b: List<Int>) -> List<Int>
    List.concat(a, b)

fn reverseList(xs: List<Int>) -> List<Int>
    List.reverse(xs)
"#;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Workload {
    PrependBuild,
    AppendBuild,
    PrependThenReverse,
    SumMatch,
    ConcatBuiltin,
    ReverseBuiltin,
}

impl Workload {
    fn all() -> &'static [Workload] {
        &[
            Workload::PrependBuild,
            Workload::AppendBuild,
            Workload::PrependThenReverse,
            Workload::SumMatch,
            Workload::ConcatBuiltin,
            Workload::ReverseBuiltin,
        ]
    }

    fn name(self) -> &'static str {
        match self {
            Workload::PrependBuild => "prepend_build",
            Workload::AppendBuild => "append_build",
            Workload::PrependThenReverse => "prepend_reverse",
            Workload::SumMatch => "sum_match",
            Workload::ConcatBuiltin => "concat_builtin",
            Workload::ReverseBuiltin => "reverse_builtin",
        }
    }

    fn fn_name(self) -> &'static str {
        match self {
            Workload::PrependBuild => "buildPrepend",
            Workload::AppendBuild => "buildAppend",
            Workload::PrependThenReverse => "finishPrepend",
            Workload::SumMatch => "sumList",
            Workload::ConcatBuiltin => "concatLists",
            Workload::ReverseBuiltin => "reverseList",
        }
    }

    fn parse(name: &str) -> Option<Workload> {
        Self::all().iter().copied().find(|w| w.name() == name)
    }

    fn args(self, size: usize) -> Vec<Value> {
        let size_int = Value::Int(size as i64);
        let empty = list_from_vec(vec![]);
        let ints = make_int_list(size);
        let half = make_int_list(size / 2);
        match self {
            Workload::PrependBuild => vec![size_int, empty],
            Workload::AppendBuild => vec![Value::Int(size as i64), list_from_vec(vec![])],
            Workload::PrependThenReverse => vec![size_int],
            Workload::SumMatch => vec![ints, Value::Int(0)],
            Workload::ConcatBuiltin => vec![half.clone(), half],
            Workload::ReverseBuiltin => vec![ints],
        }
    }
}

fn make_int_list(size: usize) -> Value {
    let items = (0..size).map(|i| Value::Int(i as i64)).collect::<Vec<_>>();
    list_from_vec(items)
}

fn usage() -> &'static str {
    "Usage: cargo run --release --bin list_bench -- [--size N] [--iters N] [--workload NAME|all]"
}

fn build_interpreter() -> Result<Interpreter, String> {
    let mut items = parse_source(PROGRAM)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, None);
    if !tc_result.errors.is_empty() {
        return Err(format!("type errors: {:?}", tc_result.errors));
    }

    resolver::resolve_program(&mut items);

    let mut interp = Interpreter::new();
    for item in &items {
        match item {
            TopLevel::FnDef(fd) => interp.exec_fn_def(fd).map_err(|e| e.to_string())?,
            TopLevel::Stmt(stmt) => {
                interp.exec_stmt(stmt).map_err(|e| e.to_string())?;
            }
            _ => {}
        }
    }
    Ok(interp)
}

fn run_workload(
    interp: &mut Interpreter,
    workload: Workload,
    size: usize,
    iters: usize,
) -> Result<Duration, String> {
    let fn_val = interp
        .lookup(workload.fn_name())
        .map_err(|e| e.to_string())?;
    let args = workload.args(size);

    black_box(
        interp
            .call_value_pub(fn_val.clone(), args.clone())
            .map_err(|e| e.to_string())?,
    );

    let started = Instant::now();
    for _ in 0..iters {
        let result = interp
            .call_value_pub(fn_val.clone(), args.clone())
            .map_err(|e| e.to_string())?;
        black_box(result);
    }
    Ok(started.elapsed())
}

fn parse_args() -> Result<(usize, usize, Option<Workload>), String> {
    let mut size = 20_000usize;
    let mut iters = 20usize;
    let mut workload = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--size" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --size".to_string())?;
                size = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --size '{}'", value))?;
            }
            "--iters" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --iters".to_string())?;
                iters = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --iters '{}'", value))?;
            }
            "--workload" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --workload".to_string())?;
                if value != "all" {
                    workload = Some(
                        Workload::parse(&value)
                            .ok_or_else(|| format!("unknown workload '{}'", value))?,
                    );
                }
            }
            "--help" | "-h" => {
                return Err(usage().to_string());
            }
            other => return Err(format!("unknown argument '{}'\n{}", other, usage())),
        }
    }

    Ok((size, iters, workload))
}

fn main() {
    let (size, iters, workload) = match parse_args() {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };

    let mut interp = match build_interpreter() {
        Ok(interp) => interp,
        Err(msg) => {
            eprintln!("cannot build benchmark interpreter: {msg}");
            std::process::exit(1);
        }
    };

    let workloads = match workload {
        Some(one) => vec![one],
        None => Workload::all().to_vec(),
    };

    println!("list_bench size={size} iters={iters}");
    for workload in workloads {
        match run_workload(&mut interp, workload, size, iters) {
            Ok(total) => {
                let avg = total / iters as u32;
                println!("{:<18} total={:?} avg={:?}", workload.name(), total, avg);
            }
            Err(msg) => {
                eprintln!("{} failed: {}", workload.name(), msg);
                std::process::exit(1);
            }
        }
    }
}
