//! The proof kernel in `tools/proof-kernel` is the largest Aver program in
//! the repository and the one the certificate is meant to cover. It used to
//! fail wasm-gc validation, with and without `--certify`, because a
//! `Result.Ok(..)` operand of `==` was built as the enclosing function's
//! `Result` instantiation (see `empty_literal_operands_spec`). Both builds
//! must produce a module that passes the compiler's own validation.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_command, format_output, repo_root};

fn compile_kernel(extra: &[&str]) {
    let out_dir = std::env::temp_dir().join(format!(
        "aver-proof-kernel-wasm-gc-{}-{}",
        std::process::id(),
        extra.join("")
    ));
    let _ = std::fs::remove_dir_all(&out_dir);
    let out = aver_command()
        .args([
            "compile",
            "tools/proof-kernel/main.av",
            "--module-root",
            "tools/proof-kernel",
            "--target",
            "wasm-gc",
        ])
        .args(extra)
        .arg("-o")
        .arg(&out_dir)
        .current_dir(repo_root())
        .output()
        .expect("aver runs");
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn the_proof_kernel_compiles_to_a_valid_wasm_gc_module() {
    compile_kernel(&[]);
}

#[test]
fn the_proof_kernel_compiles_to_a_valid_wasm_gc_module_under_certify() {
    compile_kernel(&["--certify"]);
}
