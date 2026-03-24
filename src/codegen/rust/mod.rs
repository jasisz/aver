/// Rust backend for the Aver transpiler.
///
/// Transforms Aver AST -> valid Rust source code.
mod builtins;
mod expr;
mod liveness;
mod pattern;
mod policy;
mod project;
mod runtime;
mod syntax;
mod toplevel;
mod types;

use std::collections::{BTreeMap, HashSet};

use crate::ast::{FnDef, TopLevel, TypeDef};
use crate::codegen::common::module_prefix_to_rust_segments;
use crate::codegen::{CodegenContext, ProjectOutput};
use crate::types::Type;

#[derive(Default)]
struct ModuleTreeNode {
    content: Option<String>,
    children: BTreeMap<String, ModuleTreeNode>,
}

/// Transpile an Aver program to a Rust project.
pub fn transpile(ctx: &CodegenContext) -> ProjectOutput {
    let used_services = detect_used_services(ctx);
    let needs_http_types = needs_named_type(ctx, "Header")
        || needs_named_type(ctx, "HttpResponse")
        || needs_named_type(ctx, "HttpRequest");
    let needs_tcp_types = needs_named_type(ctx, "Tcp.Connection");

    let has_tcp_runtime = used_services.contains("Tcp");
    let has_http_runtime = used_services.contains("Http");
    let has_http_server_runtime = used_services.contains("HttpServer");

    let has_tcp_types = has_tcp_runtime || needs_tcp_types;
    let has_http_types = has_http_runtime || has_http_server_runtime || needs_http_types;
    let has_http_server_types = has_http_server_runtime || needs_named_type(ctx, "HttpRequest");

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

    let mut files = vec![
        (
            "Cargo.toml".to_string(),
            project::generate_cargo_toml(
                &ctx.project_name,
                &used_services,
                ctx.policy.is_some(),
                &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("aver-rt"),
            ),
        ),
        (
            "src/main.rs".to_string(),
            render_root_main(main_fn, ctx.policy.is_some(), !verify_blocks.is_empty()),
        ),
        (
            "src/runtime_support.rs".to_string(),
            render_runtime_support(has_tcp_types, has_http_types, has_http_server_types),
        ),
    ];

    if let Some(config) = &ctx.policy {
        files.push((
            "src/policy_support.rs".to_string(),
            format!("{}\n", policy::generate_policy_runtime(config)),
        ));
    }

    if !verify_blocks.is_empty() {
        files.push((
            "src/verify.rs".to_string(),
            render_verify_module(&verify_blocks, ctx),
        ));
    }

    let mut module_tree = ModuleTreeNode::default();
    insert_module_content(
        &mut module_tree,
        &[String::from("entry")],
        render_generated_module(
            root_module_depends(&ctx.items),
            entry_module_sections(ctx, main_fn, &top_level_stmts),
        ),
    );

    for module in &ctx.modules {
        let path = module_prefix_to_rust_segments(&module.prefix);
        insert_module_content(
            &mut module_tree,
            &path,
            render_generated_module(module.depends.clone(), module_sections(module, ctx)),
        );
    }

    emit_module_tree_files(&module_tree, "src/aver_generated", &mut files);
    files.sort_by(|left, right| left.0.cmp(&right.0));

    ProjectOutput { files }
}

fn render_root_main(main_fn: Option<&FnDef>, has_policy: bool, has_verify: bool) -> String {
    let mut sections = vec![
        "#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns)]".to_string(),
        "pub use std::collections::HashMap;".to_string(),
        String::new(),
        "mod runtime_support;".to_string(),
        "pub use runtime_support::*;".to_string(),
    ];

    if has_policy {
        sections.push(String::new());
        sections.push("mod policy_support;".to_string());
        sections.push("pub use policy_support::*;".to_string());
    }

    sections.push(String::new());
    sections.push("pub mod aver_generated;".to_string());

    if has_verify {
        sections.push(String::new());
        sections.push("#[cfg(test)]".to_string());
        sections.push("mod verify;".to_string());
    }

    // Spawn main on a thread with 64 MB stack to avoid overflow in deep recursion.
    sections.push(String::new());
    let returns_result = main_fn.is_some_and(|fd| fd.return_type.starts_with("Result<"));
    if returns_result {
        let ret_type = types::type_annotation_to_rust(&main_fn.unwrap().return_type);
        sections.push(format!("fn main() -> {} {{", ret_type));
        sections.push("    let child = std::thread::Builder::new()".to_string());
        sections.push("        .stack_size(64 * 1024 * 1024)".to_string());
        sections.push("        .spawn(aver_generated::entry::main)".to_string());
        sections.push("        .expect(\"thread spawn\");".to_string());
        sections.push("    child.join().expect(\"thread join\")".to_string());
    } else {
        sections.push("fn main() {".to_string());
        if main_fn.is_some() {
            sections.push("    let child = std::thread::Builder::new()".to_string());
            sections.push("        .stack_size(64 * 1024 * 1024)".to_string());
            sections.push("        .spawn(|| aver_generated::entry::main())".to_string());
            sections.push("        .expect(\"thread spawn\");".to_string());
            sections.push("    child.join().expect(\"thread join\");".to_string());
        }
    }
    sections.push("}".to_string());
    sections.push(String::new());

    sections.join("\n")
}

fn render_runtime_support(
    has_tcp_types: bool,
    has_http_types: bool,
    has_http_server_types: bool,
) -> String {
    let mut sections = vec![runtime::generate_runtime()];
    if has_tcp_types {
        sections.push(runtime::generate_tcp_types());
    }
    if has_http_types {
        sections.push(runtime::generate_http_types());
    }
    if has_http_server_types {
        sections.push(runtime::generate_http_server_types());
    }
    format!("{}\n", sections.join("\n\n"))
}

fn render_verify_module(
    verify_blocks: &[&crate::ast::VerifyBlock],
    ctx: &CodegenContext,
) -> String {
    [
        "#[allow(unused_imports)]".to_string(),
        "use crate::*;".to_string(),
        "#[allow(unused_imports)]".to_string(),
        "use crate::aver_generated::entry::*;".to_string(),
        String::new(),
        toplevel::emit_verify_blocks(verify_blocks, ctx),
        String::new(),
    ]
    .join("\n")
}

fn render_generated_module(depends: Vec<String>, sections: Vec<String>) -> String {
    if sections.is_empty() {
        String::new()
    } else {
        let mut lines = vec![
            "#[allow(unused_imports)]".to_string(),
            "use crate::*;".to_string(),
        ];
        for dep in depends {
            let path = module_prefix_to_rust_segments(&dep).join("::");
            lines.push("#[allow(unused_imports)]".to_string());
            lines.push(format!("use crate::aver_generated::{}::*;", path));
        }
        lines.push(String::new());
        lines.push(sections.join("\n\n"));
        lines.push(String::new());
        lines.join("\n")
    }
}

fn entry_module_sections(
    ctx: &CodegenContext,
    main_fn: Option<&FnDef>,
    top_level_stmts: &[&crate::ast::Stmt],
) -> Vec<String> {
    let mut sections = Vec::new();

    for td in &ctx.type_defs {
        if is_shared_runtime_type(td) {
            continue;
        }
        sections.push(toplevel::emit_public_type_def(td, ctx));
    }

    for fd in &ctx.fn_defs {
        if fd.name == "main" {
            continue;
        }
        let is_memo = ctx.memo_fns.contains(&fd.name);
        sections.push(toplevel::emit_public_fn_def(fd, is_memo, ctx));
    }

    if main_fn.is_some() || !top_level_stmts.is_empty() {
        sections.push(toplevel::emit_public_main(main_fn, top_level_stmts, ctx));
    }

    sections
}

fn module_sections(module: &crate::codegen::ModuleInfo, ctx: &CodegenContext) -> Vec<String> {
    let mut sections = Vec::new();

    for td in &module.type_defs {
        if is_shared_runtime_type(td) {
            continue;
        }
        sections.push(toplevel::emit_public_type_def(td, ctx));
    }

    for fd in &module.fn_defs {
        let is_memo = ctx.memo_fns.contains(&fd.name);
        sections.push(toplevel::emit_public_fn_def(fd, is_memo, ctx));
    }

    sections
}

fn insert_module_content(node: &mut ModuleTreeNode, segments: &[String], content: String) {
    let child = node.children.entry(segments[0].clone()).or_default();
    if segments.len() == 1 {
        child.content = Some(content);
    } else {
        insert_module_content(child, &segments[1..], content);
    }
}

fn emit_module_tree_files(node: &ModuleTreeNode, rel_dir: &str, files: &mut Vec<(String, String)>) {
    let mut parts = Vec::new();

    if let Some(content) = &node.content
        && !content.trim().is_empty()
    {
        parts.push(content.trim_end().to_string());
    }

    for child_name in node.children.keys() {
        parts.push(format!("pub mod {};", child_name));
    }

    let mut mod_rs = parts.join("\n\n");
    if !mod_rs.is_empty() {
        mod_rs.push('\n');
    }
    files.push((format!("{}/mod.rs", rel_dir), mod_rs));

    for (child_name, child) in &node.children {
        emit_module_tree_files(child, &format!("{}/{}", rel_dir, child_name), files);
    }
}

fn root_module_depends(items: &[TopLevel]) -> Vec<String> {
    items
        .iter()
        .find_map(|item| {
            if let TopLevel::Module(module) = item {
                Some(module.depends.clone())
            } else {
                None
            }
        })
        .unwrap_or_default()
}

/// Detect which effectful services are used in the program (including modules).
fn detect_used_services(ctx: &CodegenContext) -> HashSet<String> {
    let mut services = HashSet::new();
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item {
            for eff in &fd.effects {
                services.insert(eff.clone());
                if let Some((service, _)) = eff.split_once('.') {
                    services.insert(service.to_string());
                }
            }
        }
    }
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            for eff in &fd.effects {
                services.insert(eff.clone());
                if let Some((service, _)) = eff.split_once('.') {
                    services.insert(service.to_string());
                }
            }
        }
    }
    services
}

fn is_shared_runtime_type(td: &TypeDef) -> bool {
    matches!(
        td,
        TypeDef::Product { name, .. }
            if matches!(name.as_str(), "Header" | "HttpResponse" | "HttpRequest")
    )
}

fn needs_named_type(ctx: &CodegenContext, wanted: &str) -> bool {
    ctx.fn_sigs.values().any(|(params, ret, _effects)| {
        params.iter().any(|p| type_contains_named(p, wanted)) || type_contains_named(ret, wanted)
    })
}

fn type_contains_named(ty: &Type, wanted: &str) -> bool {
    match ty {
        Type::Named(name) => name == wanted,
        Type::Result(ok, err) => {
            type_contains_named(ok, wanted) || type_contains_named(err, wanted)
        }
        Type::Option(inner) | Type::List(inner) => type_contains_named(inner, wanted),
        Type::Tuple(items) => items.iter().any(|t| type_contains_named(t, wanted)),
        Type::Map(k, v) => type_contains_named(k, wanted) || type_contains_named(v, wanted),
        Type::Fn(params, ret, _effects) => {
            params.iter().any(|t| type_contains_named(t, wanted))
                || type_contains_named(ret, wanted)
        }
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Unknown => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ModuleTreeNode, emit_module_tree_files, insert_module_content, render_generated_module,
        transpile,
    };
    use crate::codegen::build_context;
    use crate::source::parse_source;
    use crate::tco;
    use crate::types::checker::run_type_check_full;
    use std::collections::HashSet;

    fn ctx_from_source(source: &str, project_name: &str) -> crate::codegen::CodegenContext {
        let mut items = parse_source(source).expect("source should parse");
        tco::transform_program(&mut items);
        let tc = run_type_check_full(&items, None);
        assert!(
            tc.errors.is_empty(),
            "source should typecheck without errors: {:?}",
            tc.errors
        );
        build_context(items, &tc, HashSet::new(), project_name.to_string(), vec![])
    }

    fn generated_rust_entry_file(out: &crate::codegen::ProjectOutput) -> &str {
        out.files
            .iter()
            .find_map(|(name, content)| {
                (name == "src/aver_generated/entry/mod.rs").then_some(content.as_str())
            })
            .expect("expected generated Rust entry module")
    }

    #[test]
    fn generated_module_imports_direct_depends() {
        let rendered = render_generated_module(
            vec!["Domain.Types".to_string(), "App.Commands".to_string()],
            vec!["pub fn demo() {}".to_string()],
        );

        assert!(rendered.contains("use crate::aver_generated::domain::types::*;"));
        assert!(rendered.contains("use crate::aver_generated::app::commands::*;"));
        assert!(rendered.contains("pub fn demo() {}"));
    }

    #[test]
    fn module_tree_files_do_not_reexport_children() {
        let mut tree = ModuleTreeNode::default();
        insert_module_content(
            &mut tree,
            &["app".to_string(), "cli".to_string()],
            "pub fn run() {}".to_string(),
        );

        let mut files = Vec::new();
        emit_module_tree_files(&tree, "src/aver_generated", &mut files);

        let root_mod = files
            .iter()
            .find(|(path, _)| path == "src/aver_generated/mod.rs")
            .map(|(_, content)| content)
            .expect("root mod.rs should exist");

        assert!(root_mod.contains("pub mod app;"));
        assert!(!root_mod.contains("pub use app::*;"));
    }

    #[test]
    fn list_cons_match_uses_cloned_uncons_fast_path() {
        let ctx = ctx_from_source(
            r#"
module Demo

fn headPlusTailLen(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h + List.len(t)
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("aver_rt::list_uncons_cloned(&"));
    }

    #[test]
    fn list_literal_clones_ident_when_used_afterward() {
        let ctx = ctx_from_source(
            r#"
module Demo

record Audit
    message: String

fn useTwice(audit: Audit) -> List<Audit>
    first = [audit]
    [audit]
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("let first = aver_rt::AverList::from_vec(vec![audit.clone()]);"));
        assert!(entry.contains("aver_rt::AverList::from_vec(vec![audit])"));
    }

    #[test]
    fn record_update_clones_base_when_value_is_used_afterward() {
        let ctx = ctx_from_source(
            r#"
module Demo

record PaymentState
    paymentId: String
    currency: String

fn touch(state: PaymentState) -> String
    updated = PaymentState.update(state, currency = "EUR")
    state.paymentId
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("..state.clone()"));
    }
}
