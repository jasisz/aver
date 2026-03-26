/// Rust backend for the Aver transpiler.
///
/// Transforms Aver AST -> valid Rust source code.
mod builtins;
mod expr;
mod liveness;
mod pattern;
mod policy;
mod project;
mod replay;
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
    let needs_terminal_types = needs_named_type(ctx, "Terminal.Size");

    let has_tcp_runtime = used_services.contains("Tcp");
    let has_http_runtime = used_services.contains("Http");
    let has_http_server_runtime = used_services.contains("HttpServer");
    let has_terminal_runtime = used_services.contains("Terminal");

    let has_tcp_types = has_tcp_runtime || needs_tcp_types;
    let has_http_types = has_http_runtime || has_http_server_runtime || needs_http_types;
    let has_http_server_types = has_http_server_runtime || needs_named_type(ctx, "HttpRequest");
    let has_terminal_types = has_terminal_runtime || needs_terminal_types;

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
                ctx.emit_replay_runtime,
                &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("aver-rt"),
            ),
        ),
        (
            "src/main.rs".to_string(),
            render_root_main(
                main_fn,
                ctx.policy.is_some(),
                ctx.emit_replay_runtime,
                ctx.guest_entry.as_deref(),
                !verify_blocks.is_empty(),
            ),
        ),
        (
            "src/runtime_support.rs".to_string(),
            render_runtime_support(
                has_tcp_types,
                has_http_types,
                has_http_server_types,
                ctx.emit_replay_runtime,
                ctx.emit_self_host_runtime,
            ),
        ),
    ];

    if let Some(config) = &ctx.policy {
        files.push((
            "src/policy_support.rs".to_string(),
            format!("{}\n", policy::generate_policy_runtime(config)),
        ));
    }

    if ctx.emit_replay_runtime {
        files.push((
            "src/replay_support.rs".to_string(),
            replay::generate_replay_runtime(
                ctx.policy.is_some(),
                has_terminal_types,
                has_tcp_types,
                has_http_types,
                has_http_server_types,
            ),
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

fn render_root_main(
    main_fn: Option<&FnDef>,
    has_policy: bool,
    has_replay: bool,
    guest_entry: Option<&str>,
    has_verify: bool,
) -> String {
    let mut sections = vec![
        "#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns)]".to_string(),
        "#[macro_use] extern crate aver_rt;".to_string(),
        "pub use ::aver_rt::AverMap as HashMap;".to_string(),
        "pub use ::aver_rt::AverStr;".to_string(),
        String::new(),
        "mod runtime_support;".to_string(),
        "pub use runtime_support::*;".to_string(),
    ];

    if has_policy {
        sections.push(String::new());
        sections.push("mod policy_support;".to_string());
        sections.push("pub use policy_support::*;".to_string());
    }

    if has_replay {
        sections.push(String::new());
        sections.push("mod replay_support;".to_string());
        sections.push("pub use replay_support::*;".to_string());
    }

    sections.push(String::new());
    sections.push("pub mod aver_generated;".to_string());

    if has_verify {
        sections.push(String::new());
        sections.push("#[cfg(test)]".to_string());
        sections.push("mod verify;".to_string());
    }

    // Spawn main on a thread with 256 MB stack to avoid overflow in deep recursion.
    sections.push(String::new());
    let returns_result = main_fn.is_some_and(|fd| fd.return_type.starts_with("Result<"));
    let result_unit_string =
        main_fn.is_some_and(|fd| fd.return_type.replace(' ', "") == "Result<Unit,String>");
    if returns_result {
        if result_unit_string {
            sections.push("fn main() {".to_string());
            sections.push("    let child = std::thread::Builder::new()".to_string());
            sections.push("        .stack_size(256 * 1024 * 1024)".to_string());
            if has_replay && guest_entry.is_none() {
                sections.push("        .spawn(|| {".to_string());
                sections.push("            let __result = aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, aver_generated::entry::main);".to_string());
                sections.push("            __result.map_err(|e| e.to_string())".to_string());
                sections.push("        })".to_string());
            } else {
                sections.push("        .spawn(|| {".to_string());
                sections
                    .push("            let __result = aver_generated::entry::main();".to_string());
                sections.push("            __result.map_err(|e| e.to_string())".to_string());
                sections.push("        })".to_string());
            }
            sections.push("        .expect(\"thread spawn\");".to_string());
            sections.push("    match child.join().expect(\"thread join\") {".to_string());
            sections.push("        Ok(()) => {}".to_string());
            sections.push("        Err(e) => {".to_string());
            sections.push("            eprintln!(\"{}\", e);".to_string());
            sections.push("            std::process::exit(1);".to_string());
            sections.push("        }".to_string());
            sections.push("    }".to_string());
        } else {
            let ret_type = types::type_annotation_to_rust(&main_fn.unwrap().return_type);
            sections.push(format!("fn main() -> {} {{", ret_type));
            if has_replay && guest_entry.is_none() {
                sections.push(
                    "    aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, aver_generated::entry::main)"
                        .to_string(),
                );
            } else {
                sections.push("    aver_generated::entry::main()".to_string());
            }
        }
    } else {
        sections.push("fn main() {".to_string());
        if main_fn.is_some() {
            sections.push("    let child = std::thread::Builder::new()".to_string());
            sections.push("        .stack_size(256 * 1024 * 1024)".to_string());
            if has_replay && guest_entry.is_none() {
                sections.push("        .spawn(|| aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, || aver_generated::entry::main()))".to_string());
            } else {
                sections.push("        .spawn(|| aver_generated::entry::main())".to_string());
            }
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
    has_replay: bool,
    emit_self_host_runtime: bool,
) -> String {
    let mut sections = vec![runtime::generate_runtime(
        has_replay,
        has_http_server_types,
        emit_self_host_runtime,
    )];
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
        if ctx.emit_replay_runtime {
            sections.push(replay::emit_replay_value_impl(td));
        }
    }

    // Detect mutual TCO groups among non-main functions.
    let non_main_fns: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| fd.name != "main").collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&non_main_fns);
    let mut mutual_tco_members: HashSet<String> = HashSet::new();

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&FnDef> = group_indices.iter().map(|&idx| non_main_fns[idx]).collect();
        for fd in &group_fns {
            mutual_tco_members.insert(fd.name.clone());
        }
        sections.push(toplevel::emit_mutual_tco_block(
            group_id + 1,
            &group_fns,
            ctx,
            "pub ",
        ));
    }

    for fd in &ctx.fn_defs {
        if fd.name == "main" || mutual_tco_members.contains(&fd.name) {
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
        if ctx.emit_replay_runtime {
            sections.push(replay::emit_replay_value_impl(td));
        }
    }

    // Detect mutual TCO groups among module functions.
    let fn_refs: Vec<&FnDef> = module.fn_defs.iter().collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&fn_refs);
    let mut mutual_tco_members: HashSet<String> = HashSet::new();

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&FnDef> = group_indices.iter().map(|&idx| fn_refs[idx]).collect();
        for fd in &group_fns {
            mutual_tco_members.insert(fd.name.clone());
        }
        sections.push(toplevel::emit_mutual_tco_block(
            group_id + 1,
            &group_fns,
            ctx,
            "pub ",
        ));
    }

    for fd in &module.fn_defs {
        if mutual_tco_members.contains(&fd.name) {
            continue;
        }
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
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_contains_named(inner, wanted)
        }
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

    fn generated_file<'a>(out: &'a crate::codegen::ProjectOutput, path: &str) -> &'a str {
        out.files
            .iter()
            .find_map(|(name, content)| (name == path).then_some(content.as_str()))
            .unwrap_or_else(|| panic!("expected generated file '{}'", path))
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

        // The common []/[h,..t] pattern uses aver_list_match! macro
        assert!(entry.contains("aver_list_match!"));
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

    #[test]
    fn mutual_tco_generates_trampoline_instead_of_regular_calls() {
        let ctx = ctx_from_source(
            r#"
module Demo

fn isEven(n: Int) -> Bool
    match n == 0
        true -> true
        false -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    match n == 0
        true -> false
        false -> isEven(n - 1)
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        // Should generate trampoline enum and dispatch
        assert!(entry.contains("enum __MutualTco1"));
        assert!(entry.contains("fn __mutual_tco_trampoline_1"));
        assert!(entry.contains("loop {"));

        // Wrapper functions delegate to trampoline
        assert!(entry.contains("pub fn isEven"));
        assert!(entry.contains("pub fn isOdd"));
        assert!(entry.contains("__mutual_tco_trampoline_1("));

        // Should NOT contain direct recursive calls between the two
        assert!(!entry.contains("isOdd((n - 1i64))"));
    }

    #[test]
    fn field_access_does_not_double_clone() {
        let ctx = ctx_from_source(
            r#"
module Demo

record User
    name: String
    age: Int

fn greet(u: User) -> String
    u.name
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        // Field access should produce exactly one .clone(), never .clone().clone()
        assert!(
            !entry.contains(".clone().clone()"),
            "double clone detected in generated code:\n{}",
            entry
        );
    }

    #[test]
    fn single_field_variant_display_avoids_vec_join() {
        let ctx = ctx_from_source(
            r#"
module Demo

type Wrapper
    Wrap(Int)
    Pair(Int, Int)
    Empty
"#,
            "demo",
        );

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);

        // Single-field variant Wrap(Int): should NOT use vec![].join()
        assert!(
            !entry.contains("vec![f0.aver_display_inner()].join"),
            "single-field variant should use direct format, not vec join:\n{}",
            entry
        );
        // Multi-field variant Pair(Int, Int): SHOULD still use vec![].join()
        assert!(
            entry.contains("vec![f0.aver_display_inner(), f1.aver_display_inner()].join(\", \")"),
            "multi-field variant should use vec join:\n{}",
            entry
        );
    }

    #[test]
    fn replay_codegen_wraps_guest_entry_in_scoped_runtime() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn runGuestProgram(path: String) -> Result<String, String>
    ! [Disk.readText]
    Disk.readText(path)
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;
        ctx.guest_entry = Some("runGuestProgram".to_string());

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);
        let replay_support = generated_file(&out, "src/replay_support.rs");
        let cargo_toml = generated_file(&out, "Cargo.toml");

        assert!(entry.contains("aver_replay::with_guest_scope_result(\"runGuestProgram\""));
        assert!(replay_support.contains("pub mod aver_replay"));
        assert!(cargo_toml.contains("serde_json = \"1\""));
    }

    #[test]
    fn replay_codegen_uses_guest_args_param_override_when_present() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn runGuestProgram(path: String, guestArgs: List<String>) -> Result<String, String>
    ! [Args.get]
    Result.Ok(String.join(Args.get(), ","))
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;
        ctx.guest_entry = Some("runGuestProgram".to_string());

        let out = transpile(&ctx);
        let entry = generated_rust_entry_file(&out);
        let cargo_toml = generated_file(&out, "Cargo.toml");

        assert!(entry.contains("aver_replay::with_guest_scope_args_result(\"runGuestProgram\""));
        assert!(entry.contains("guestArgs.clone()"));
        assert!(cargo_toml.contains("edition = \"2024\""));
    }

    #[test]
    fn replay_codegen_wraps_root_main_when_no_guest_entry_is_set() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn main() -> Result<String, String>
    ! [Disk.readText]
    Disk.readText("demo.av")
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;

        let out = transpile(&ctx);
        let root_main = generated_file(&out, "src/main.rs");

        assert!(
            root_main.contains("aver_replay::with_guest_scope(\"main\", serde_json::Value::Null")
        );
    }
}
