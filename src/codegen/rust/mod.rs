/// Rust backend for the Aver transpiler.
///
/// Transforms Aver AST → valid Rust source code.
mod builtins;
mod expr;
mod pattern;
mod project;
mod runtime;
mod toplevel;
mod types;

use std::collections::HashSet;

use crate::ast::TopLevel;
use crate::codegen::{CodegenContext, ProjectOutput};

/// Transpile an Aver program to a Rust project.
pub fn transpile(ctx: &CodegenContext) -> ProjectOutput {
    let mut sections = Vec::new();

    // Preamble
    sections.push("#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns)]".to_string());
    sections.push("use std::collections::HashMap;".to_string());
    sections.push(String::new());

    // Runtime helpers
    sections.push(runtime::generate_runtime());
    sections.push(String::new());

    // Collect info about which services are used
    let used_services = detect_used_services(ctx);

    // Service type definitions (conditionally emitted)
    let has_tcp = used_services.contains("Tcp");
    let has_http = used_services.contains("Http");
    let has_http_server = used_services.contains("HttpServer");

    if has_tcp {
        sections.push(runtime::generate_tcp_types());
        sections.push(String::new());
    }

    if has_http || has_http_server {
        sections.push(runtime::generate_http_types());
        sections.push(String::new());
    }

    if has_http_server {
        sections.push(runtime::generate_http_server_types());
        sections.push(String::new());
    }

    // Service runtime modules
    if has_tcp {
        sections.push(runtime::generate_tcp_runtime());
        sections.push(String::new());
    }

    if has_http {
        sections.push(runtime::generate_http_runtime());
        sections.push(String::new());
    }

    if has_http_server {
        sections.push(runtime::generate_http_server_runtime());
        sections.push(String::new());
    }

    // Module type definitions (inlined from depends)
    for module in &ctx.modules {
        for td in &module.type_defs {
            sections.push(toplevel::emit_type_def(td));
            sections.push(String::new());
        }
    }

    // Module function definitions (inlined from depends)
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            let is_memo = ctx.memo_fns.contains(&fd.name);
            sections.push(toplevel::emit_fn_def(fd, is_memo, ctx));
            sections.push(String::new());
        }
    }

    // Type definitions (structs and enums)
    for td in &ctx.type_defs {
        sections.push(toplevel::emit_type_def(td));
        sections.push(String::new());
    }

    // Function definitions (excluding main)
    for fd in &ctx.fn_defs {
        if fd.name == "main" {
            continue;
        }
        let is_memo = ctx.memo_fns.contains(&fd.name);
        sections.push(toplevel::emit_fn_def(fd, is_memo, ctx));
        sections.push(String::new());
    }

    // Main function
    let main_fn = ctx.fn_defs.iter().find(|fd| fd.name == "main");
    let top_level_stmts: Vec<_> = ctx
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Stmt(stmt) = item {
                Some(stmt)
            } else {
                None
            }
        })
        .collect();

    sections.push(toplevel::emit_main(main_fn, &top_level_stmts, ctx));
    sections.push(String::new());

    // Verify blocks → #[cfg(test)]
    let verify_blocks: Vec<_> = ctx
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Verify(vb) = item {
                Some(vb)
            } else {
                None
            }
        })
        .collect();

    if !verify_blocks.is_empty() {
        sections.push(toplevel::emit_verify_blocks(&verify_blocks, ctx));
    }

    let main_rs = sections.join("\n");
    let cargo_toml = project::generate_cargo_toml(&ctx.project_name, &used_services);

    ProjectOutput {
        files: vec![
            ("Cargo.toml".to_string(), cargo_toml),
            ("src/main.rs".to_string(), main_rs),
        ],
    }
}

/// Detect which effectful services are used in the program (including modules).
fn detect_used_services(ctx: &CodegenContext) -> HashSet<String> {
    let mut services = HashSet::new();
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item {
            for eff in &fd.effects {
                services.insert(eff.clone());
            }
        }
    }
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            for eff in &fd.effects {
                services.insert(eff.clone());
            }
        }
    }
    services
}
