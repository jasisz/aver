#![allow(
    clippy::all,
    unused_variables,
    unused_mut,
    dead_code,
    unused_imports,
    unused_parens,
    non_snake_case,
    non_camel_case_types,
    unreachable_patterns,
    hidden_glob_reexports
)]
// Aver Rust emission
#[macro_use]
extern crate aver_rt;
pub use ::aver_rt::AverMap as HashMap;
pub use ::aver_rt::AverStr;
pub use ::aver_rt::Buffer;

mod runtime_support;
pub use runtime_support::*;

mod replay_support;
pub use replay_support::*;

mod self_host_support;

pub mod aver_generated;

fn main() {
    if let Ok(n_str) = std::env::var("AVER_BENCH_ITER") {
        let n: usize = n_str.parse().unwrap_or(0);
        let warmup: usize = std::env::var("AVER_BENCH_WARMUP")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);
        for _ in 0..warmup {
            let _ = std::hint::black_box(aver_generated::entry::main());
        }
        for _ in 0..n {
            let t = std::time::Instant::now();
            let _ = std::hint::black_box(aver_generated::entry::main());
            eprintln!("__bench_iter_ms__: {}", t.elapsed().as_secs_f64() * 1000.0);
        }
        std::process::exit(0);
    }
    let child = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(|| {
            let __result = aver_generated::entry::main();
            __result.map_err(|e| e.to_string())
        })
        .expect("thread spawn");
    match child.join().expect("thread join") {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
