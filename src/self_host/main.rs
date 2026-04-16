#![allow(
    unused_variables,
    unused_mut,
    dead_code,
    unused_imports,
    unused_parens,
    non_snake_case,
    non_camel_case_types,
    unreachable_patterns,
    hidden_glob_reexports,
    clippy::all
)]
// Aver Rust emission
#[macro_use]
extern crate aver_rt;
pub use ::aver_rt::AverMap as HashMap;
pub use ::aver_rt::AverStr;

mod runtime_support;
pub use runtime_support::*;

mod replay_support;
pub use replay_support::*;

mod self_host_support;

pub mod aver_generated;

fn main() {
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
