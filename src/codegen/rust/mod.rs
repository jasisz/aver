/// Rust backend for the Aver transpiler.
///
/// Transforms Aver AST -> valid Rust source code.
mod builtins;
pub mod emit_ctx;
mod expr;
mod from_mir;
pub use from_mir::{CoverageReport, MirEmitCtx, coverage_report};
mod pattern;
mod policy;
mod project;
mod replay;
mod runtime;
mod self_host;
mod syntax;
mod toplevel;
mod types;

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{FnDef, TopLevel, TypeDef};
use crate::codegen::common::module_prefix_to_rust_segments;
use crate::codegen::{CodegenContext, ProjectOutput};
use crate::types::Type;

/// Synthesize Rust's `mod.rs` cascade from a flat list of (segments, body)
/// modules. Every parent directory along each module's path gets a
/// `mod.rs` that declares `pub mod {child};` for each immediate child;
/// the leaf node's `mod.rs` carries the body. Backend-local because the
/// cascade is a Rust/Cargo-specific filesystem convention — Lean and
/// Dafny just write the leaf path directly.
fn synthesize_rust_module_cascade(
    rel_dir: &str,
    modules: &[(Vec<String>, String)],
) -> Vec<(String, String)> {
    let mut by_dir: BTreeMap<Vec<String>, (Option<String>, BTreeSet<String>)> = BTreeMap::new();
    for (segments, content) in modules {
        by_dir.entry(segments.clone()).or_default().0 = Some(content.clone());
        for i in 0..segments.len() {
            let parent: Vec<String> = segments[..i].to_vec();
            by_dir
                .entry(parent)
                .or_default()
                .1
                .insert(segments[i].clone());
        }
    }
    let mut files = Vec::new();
    for (dir_segs, (content, children)) in by_dir {
        let dir_path = if dir_segs.is_empty() {
            rel_dir.to_string()
        } else {
            format!("{}/{}", rel_dir, dir_segs.join("/"))
        };
        let mut parts: Vec<String> = Vec::new();
        if let Some(c) = content
            && !c.trim().is_empty()
        {
            parts.push(c.trim_end().to_string());
        }
        for child in children {
            parts.push(format!("pub mod {};", child));
        }
        let mut mod_rs = parts.join("\n\n");
        if !mod_rs.is_empty() {
            mod_rs.push('\n');
        }
        files.push((format!("{}/mod.rs", dir_path), mod_rs));
    }
    files
}

/// Transpile an Aver program to a Rust project.
pub fn transpile(ctx: &mut CodegenContext) -> ProjectOutput {
    let has_embedded_policy = ctx.policy.is_some();
    let has_runtime_policy = ctx.runtime_policy_from_env;
    let embedded_independence_cancel = ctx
        .policy
        .as_ref()
        .is_some_and(|config| config.independence_mode == crate::config::IndependenceMode::Cancel);
    let used_services = detect_used_services(ctx);
    let needs_http_types =
        needs_named_type(ctx, "HttpResponse") || needs_named_type(ctx, "HttpRequest");
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

    // `main` fn lookup is identity-safe by parser invariant — only
    // entry scope can declare it, and at most once. The view's
    // `entry_fns()` enumerates the same set in the same source order,
    // so iterating either substrate yields the same answer here.
    // temporary-migration-bridge: downstream `render_root_main` still
    // takes `Option<&FnDef>`; signature swap is a PR D follow-up.
    let main_fn = ctx.fn_defs.iter().find(|fd| fd.name == "main");
    debug_assert_eq!(
        main_fn.is_some(),
        ctx.resolved_program
            .entry_fns()
            .any(|rfd| rfd.name == "main"),
        "ctx.fn_defs and ctx.resolved_program.entry_fns() must agree on \
         main fn presence (epic #170 Phase 1 invariant)"
    );
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
                has_embedded_policy,
                has_runtime_policy,
                ctx.emit_replay_runtime,
            ),
        ),
        (
            "src/main.rs".to_string(),
            render_root_main(
                main_fn,
                has_embedded_policy,
                ctx.emit_replay_runtime,
                ctx.guest_entry.as_deref(),
                !verify_blocks.is_empty(),
                ctx.emit_self_host_support,
            ),
        ),
        (
            "src/runtime_support.rs".to_string(),
            render_runtime_support(
                has_tcp_types,
                has_http_types,
                has_http_server_types,
                ctx.emit_replay_runtime,
                embedded_independence_cancel,
            ),
        ),
    ];

    if ctx.emit_self_host_support {
        files.push((
            "src/self_host_support.rs".to_string(),
            self_host::generate_self_host_support(),
        ));
    }

    if has_embedded_policy && let Some(config) = &ctx.policy {
        files.push((
            "src/policy_support.rs".to_string(),
            format!("{}\n", policy::generate_policy_runtime(config)),
        ));
    }

    if ctx.emit_replay_runtime {
        files.push((
            "src/replay_support.rs".to_string(),
            replay::generate_replay_runtime(
                has_embedded_policy,
                has_runtime_policy,
                has_terminal_types,
                has_tcp_types,
                has_http_types,
                has_http_server_types,
                embedded_independence_cancel,
            ),
        ));
    }

    if !verify_blocks.is_empty() {
        files.push((
            "src/verify.rs".to_string(),
            render_verify_module(&verify_blocks, ctx),
        ));
    }

    let mut rust_modules: Vec<(Vec<String>, String)> = Vec::new();
    rust_modules.push((
        vec!["entry".to_string()],
        render_generated_module(
            root_module_depends(&ctx.items),
            entry_module_sections(ctx, main_fn, &top_level_stmts),
        ),
    ));

    for i in 0..ctx.modules.len() {
        // Set extra_fn_defs so find_fn_def_by_name resolves intra-module
        // bare-name calls (e.g. buildFibStats calling finalizeFibStats).
        ctx.extra_fn_defs = ctx.modules[i].fn_defs.clone();
        let module = &ctx.modules[i];
        let segments = module_prefix_to_rust_segments(&module.prefix);
        rust_modules.push((
            segments,
            render_generated_module(module.depends.clone(), module_sections(module, ctx)),
        ));
    }
    ctx.extra_fn_defs.clear();

    files.extend(synthesize_rust_module_cascade(
        "src/aver_generated",
        &rust_modules,
    ));
    files.sort_by(|left, right| left.0.cmp(&right.0));

    ProjectOutput { files }
}

fn render_root_main(
    main_fn: Option<&FnDef>,
    has_policy: bool,
    has_replay: bool,
    guest_entry: Option<&str>,
    has_verify: bool,
    has_self_host_support: bool,
) -> String {
    let mut sections = vec![
        "#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns, hidden_glob_reexports)]".to_string(),
        "// Aver Rust emission".to_string(),
        "#[macro_use] extern crate aver_rt;".to_string(),
        "pub use ::aver_rt::AverMap as HashMap;".to_string(),
        "pub use ::aver_rt::AverStr;".to_string(),
        "pub use ::aver_rt::Buffer;".to_string(),
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

    if has_self_host_support {
        sections.push(String::new());
        sections.push("mod self_host_support;".to_string());
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
    // `aver bench --target rust` sets `AVER_BENCH_ITER` (and optionally
    // `AVER_BENCH_WARMUP`) to drive an in-process benchmark loop that
    // calls `aver_generated::entry::main` N times under one process.
    // Spawning the binary per-iter from the host harness floors at
    // ~2–3 ms macOS process-spawn cost and reports pure noise on any
    // workload below ~1 ms (`fib`, `factorial`, `record`). One env-var
    // read on process start when unset; zero generated complexity in
    // the user's main body.
    let bench_dispatch_lines = |has_replay: bool| -> Vec<String> {
        let user_call = if has_replay {
            "aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, aver_generated::entry::main)"
        } else {
            "aver_generated::entry::main()"
        };
        vec![
            "    if let Ok(n_str) = std::env::var(\"AVER_BENCH_ITER\") {".to_string(),
            "        let n: usize = n_str.parse().unwrap_or(0);".to_string(),
            "        let warmup: usize = std::env::var(\"AVER_BENCH_WARMUP\").ok().and_then(|s| s.parse().ok()).unwrap_or(0);".to_string(),
            "        for _ in 0..warmup {".to_string(),
            format!("            let _ = std::hint::black_box({});", user_call),
            "        }".to_string(),
            "        for _ in 0..n {".to_string(),
            "            let t = std::time::Instant::now();".to_string(),
            format!("            let _ = std::hint::black_box({});", user_call),
            "            eprintln!(\"__bench_iter_ms__: {}\", t.elapsed().as_secs_f64() * 1000.0);".to_string(),
            "        }".to_string(),
            "        std::process::exit(0);".to_string(),
            "    }".to_string(),
        ]
    };
    if returns_result {
        if result_unit_string {
            sections.push("fn main() {".to_string());
            sections.extend(bench_dispatch_lines(has_replay && guest_entry.is_none()));
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
            sections.extend(bench_dispatch_lines(has_replay && guest_entry.is_none()));
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
            sections.extend(bench_dispatch_lines(has_replay && guest_entry.is_none()));
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
    embedded_independence_cancel: bool,
) -> String {
    let mut sections = vec![runtime::generate_runtime(
        has_replay,
        has_http_server_types,
        embedded_independence_cancel,
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

    // Detect mutual TCO groups among non-main functions. Set-form membership
    // is read from `ctx.mutual_tco_members` (populated by the analyze stage's
    // unioned per-module sets); the index-keyed `groups` form still comes
    // from `find_mutual_tco_groups` because trampoline emission needs the
    // structural shape, not just the names.
    // temporary-migration-bridge: `find_mutual_tco_groups` walks
    // `&[&FnDef]` to detect tail-call SCCs against the raw AST body
    // shape. Position-aligned with `ctx.resolved_program.entry_fns()`
    // (both iterate the same source-ordered set). PR D migrates the
    // SCC analyser to consume `ResolvedFnDef` bodies directly.
    let non_main_fns: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| fd.name != "main").collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&non_main_fns);

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&FnDef> = group_indices.iter().map(|&idx| non_main_fns[idx]).collect();
        sections.push(toplevel::emit_mutual_tco_block(
            group_id + 1,
            &group_fns,
            ctx,
            None,
            "pub ",
        ));
    }

    // Epic #170 Phase 4: emit each entry fn from a paired
    // (`&FnDef`, `&ResolvedFnDef`) input. The AST view carries
    // source-shape metadata the emitter still reads (param
    // annotations, effects), the resolved view carries the body the
    // expr emitter walks. `ctx.resolved_program.fn_by_id(fn_id)`
    // is the identity-keyed lookup — no bare-name walk over the
    // resolved list.
    for fd in &ctx.fn_defs {
        let Some(fn_id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
            continue;
        };
        let is_mutual = ctx.mutual_tco_members.contains(&fn_id);
        if fd.name == "main" || is_mutual {
            continue;
        }
        let Some(resolved_fd) = ctx.resolved_program.fn_by_id(fn_id) else {
            // Synthetic FnDefs (memo wrappers, TCO hoists) inserted
            // post-pipeline don't have a resolved twin yet — fall back
            // to on-demand resolve for those. `temporary-migration-bridge`:
            // PR E moves synthetic-fn resolve into a typed builder.
            let resolved_owned = ctx.resolve_fn_def(fd, None);
            let is_memo = ctx.memo_fns.contains(&fd.name);
            sections.push(toplevel::emit_public_fn_def(
                fd,
                resolved_owned.as_ref(),
                is_memo,
                ctx,
                None,
            ));
            continue;
        };
        let is_memo = ctx.memo_fns.contains(&fd.name);
        sections.push(toplevel::emit_public_fn_def(
            fd,
            resolved_fd,
            is_memo,
            ctx,
            None,
        ));
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

    // Same shape as the entry path: groups (with indices) come from
    // `find_mutual_tco_groups`, set-form membership reads from
    // `module.analysis.mutual_tco_members` (bare names, scope-local
    // per module, DAG invariant keeps them unambiguous) when the
    // analyze stage ran. Fall back to projecting `ctx.mutual_tco_members`
    // (FnId set) back to bare names for this module's scope.
    let fn_refs: Vec<&FnDef> = module.fn_defs.iter().collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&fn_refs);
    let module_mutual_owned: HashSet<String> = match module.analysis.as_ref() {
        Some(a) => a.mutual_tco_members.clone(),
        None => ctx
            .mutual_tco_members
            .iter()
            .filter_map(|id| {
                let entry = ctx.symbol_table.fn_entry(*id);
                entry
                    .key
                    .scope
                    .as_deref()
                    .filter(|s| *s == module.prefix)
                    .map(|_| entry.key.name.clone())
            })
            .collect(),
    };
    let module_mutual = &module_mutual_owned;

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&FnDef> = group_indices.iter().map(|&idx| fn_refs[idx]).collect();
        sections.push(toplevel::emit_mutual_tco_block(
            group_id + 1,
            &group_fns,
            ctx,
            Some(&module.prefix),
            "pub ",
        ));
    }

    for fd in &module.fn_defs {
        if module_mutual.contains(&fd.name) {
            continue;
        }
        let is_memo = ctx.memo_fns.contains(&fd.name);
        // Same pair-API as the entry loop above. Module fns route
        // through `fn_id_for_decl` (pointer-eq scope on `&FnDef`) →
        // `resolved_program.fn_by_id(fn_id)` so a same-bare-name
        // entry-scope twin never accidentally provides this body.
        let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
            .and_then(|id| ctx.resolved_program.fn_by_id(id));
        let resolved_owned = if resolved_fd.is_some() {
            None
        } else {
            // temporary-migration-bridge: synthetic / mid-rewrite fns
            // fall back to on-demand resolve in the dep scope.
            Some(ctx.resolve_fn_def(fd, Some(&module.prefix)))
        };
        let resolved_ref: &crate::ir::hir::ResolvedFnDef =
            resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
        sections.push(toplevel::emit_public_fn_def(
            fd,
            resolved_ref,
            is_memo,
            ctx,
            Some(&module.prefix),
        ));
    }

    sections
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
                services.insert(eff.node.clone());
                if let Some((service, _)) = eff.node.split_once('.') {
                    services.insert(service.to_string());
                }
            }
        }
    }
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            for eff in &fd.effects {
                services.insert(eff.node.clone());
                if let Some((service, _)) = eff.node.split_once('.') {
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
            if matches!(name.as_str(), "HttpResponse" | "HttpRequest")
    )
}

fn needs_named_type(ctx: &CodegenContext, wanted: &str) -> bool {
    // Walks resolved fn defs (entry + every dep module). The wasm-gc
    // counterpart `items_reference_name` uses the same shape — a
    // substring scan over signature surface — so the discovery
    // semantics stay identical post #180 Phase 7 fn_sigs drop.
    let scan = |params: &[(String, Type)], ret: &Type| -> bool {
        params.iter().any(|(_, p)| type_contains_named(p, wanted))
            || type_contains_named(ret, wanted)
    };
    if ctx
        .resolved_program
        .entry_fns()
        .any(|rfd| scan(&rfd.params, &rfd.return_type))
    {
        return true;
    }
    ctx.resolved_program.modules.iter().any(|m| {
        m.fn_defs
            .iter()
            .any(|rfd| scan(&rfd.params, &rfd.return_type))
    })
}

// syntax-discovery-only: walks ALL fn signatures asking "does
// any param / return type contain a Named ref with this exact
// bare-string name?" Callers pass builtin record names
// (`"HttpResponse"`, `"Tcp.Connection"`, `"Terminal.Size"`)
// whose typed registration carries no `id` — those refs are
// matched by the name surface they were declared with. The
// query is a discovery walk, not an identity decision.
fn type_contains_named(ty: &Type, wanted: &str) -> bool {
    match ty {
        Type::Named { name, .. } => name == wanted,
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
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Invalid
        | Type::Var(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{render_generated_module, synthesize_rust_module_cascade, transpile};
    use crate::codegen::build_context;
    use crate::source::parse_source;

    use std::collections::HashSet;

    fn ctx_from_source(source: &str, project_name: &str) -> crate::codegen::CodegenContext {
        let mut items = parse_source(source).expect("source should parse");
        crate::ir::pipeline::tco(&mut items);
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "source should typecheck without errors: {:?}",
            tc.errors
        );
        // No pipeline analyze stage run here — supply a locally-built
        // SymbolTable so build_context can populate its FnId-keyed
        // sets. Cheap traversal, no analysis.
        let symbol_table = crate::ir::SymbolTable::build(&items, &[]);
        let resolved_items = crate::ir::hir::resolve_program(&symbol_table, &items);
        build_context(
            items,
            &tc,
            None,
            HashSet::new(),
            project_name.to_string(),
            vec![],
            symbol_table,
            resolved_items,
        )
    }

    /// Multi-module ctx builder for cross-scope regression tests.
    /// `entry_src` is the entry-module source; `dep_sources` is a
    /// list of `(prefix, source)` for dep modules. The entry's
    /// `depends [...]` list must mention every dep prefix.
    fn ctx_from_multi(
        entry_src: &str,
        dep_sources: &[(&str, &str)],
        project_name: &str,
    ) -> crate::codegen::CodegenContext {
        let mut entry_items = parse_source(entry_src).expect("entry source should parse");
        crate::ir::pipeline::tco(&mut entry_items);

        // Build `LoadedModule` views for the typechecker (`WithLoaded`
        // mode walks the same shape an on-disk multi-file compile
        // produces, so the typechecker sees dep symbols without
        // touching the filesystem).
        let loaded: Vec<crate::source::LoadedModule> = dep_sources
            .iter()
            .map(|(prefix, src)| {
                let items = parse_source(src).expect("dep source should parse");
                crate::source::LoadedModule {
                    dep_name: prefix.to_string(),
                    items,
                    path: std::path::PathBuf::from(format!("{}.av", prefix)),
                }
            })
            .collect();

        let modules: Vec<crate::codegen::ModuleInfo> = loaded
            .iter()
            .map(|lm| {
                let depends = lm
                    .items
                    .iter()
                    .find_map(|i| match i {
                        crate::ast::TopLevel::Module(m) => Some(m.depends.clone()),
                        _ => None,
                    })
                    .unwrap_or_default();
                let type_defs = lm
                    .items
                    .iter()
                    .filter_map(|i| match i {
                        crate::ast::TopLevel::TypeDef(td) => Some(td.clone()),
                        _ => None,
                    })
                    .collect();
                let fn_defs = lm
                    .items
                    .iter()
                    .filter_map(|i| match i {
                        crate::ast::TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
                        _ => None,
                    })
                    .collect();
                crate::codegen::ModuleInfo {
                    prefix: lm.dep_name.clone(),
                    depends,
                    type_defs,
                    fn_defs,
                    analysis: None,
                }
            })
            .collect();

        let tc = crate::ir::pipeline::typecheck(
            &entry_items,
            &crate::ir::TypecheckMode::WithLoaded(&loaded),
        );
        assert!(
            tc.errors.is_empty(),
            "entry+dep source should typecheck without errors: {:?}",
            tc.errors
        );
        let symbol_table = crate::ir::SymbolTable::build(&entry_items, &modules);
        let resolved_items = crate::ir::hir::resolve_program(&symbol_table, &entry_items);
        build_context(
            entry_items,
            &tc,
            None,
            HashSet::new(),
            project_name.to_string(),
            modules,
            symbol_table,
            resolved_items,
        )
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
    fn cross_module_same_bare_name_fns_resolve_via_qualified_path() {
        // Epic #170 Phase 3: pins the architectural invariant that
        // Rust codegen distinguishes same-bare-name fns across the
        // entry module and a dep module by using the fully-qualified
        // path for cross-module calls. The entry's `helper(n)` body
        // is `n + 1`; the dep `Worker.helper(n)` body is `n + 100`.
        // The emitted Rust must:
        //   1. emit BOTH fn defs (one per module file)
        //   2. emit `Worker.walk` body as a fully-qualified call to
        //      `crate::aver_generated::worker::helper` — NOT a bare
        //      `helper(n)` that the entry's `use worker::*` wildcard
        //      shadow would silently mis-resolve.
        //   3. emit `main`'s `Worker.walk(20)` as a fully-qualified
        //      call too — same anti-shadow rule.
        let mut ctx = ctx_from_multi(
            r#"
module Entry
    depends [Worker]
    intent = "Entry with own same-bare 'helper'."
    effects []

fn helper(n: Int) -> Int
    n + 1

fn main() -> Int
    helper(10) + Worker.walk(20)
"#,
            &[(
                "Worker",
                r#"
module Worker
    exposes [walk]
    intent = "Worker module with same-bare 'helper'."
    effects []

fn helper(n: Int) -> Int
    n + 100

fn walk(n: Int) -> Int
    helper(n)
"#,
            )],
            "cross_module_helper",
        );

        let out = transpile(&mut ctx);

        let worker = generated_file(&out, "src/aver_generated/worker/mod.rs");
        assert!(
            worker.contains("pub fn helper"),
            "Worker module must emit its OWN helper:\n{worker}"
        );
        assert!(
            worker.contains("n + 100"),
            "Worker.helper body must keep its OWN literal (100):\n{worker}"
        );
        // Critical anti-shadow check: Worker.walk calls Worker.helper
        // through the canonical crate path, never bare `helper(n)`
        // (which would resolve to whoever's in scope after
        // `use worker::*`).
        assert!(
            worker.contains("crate::aver_generated::worker::helper"),
            "Worker.walk must call its own helper via the canonical \
             crate path (not bare-name `helper(n)`):\n{worker}"
        );

        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("pub fn helper"),
            "Entry module must emit its OWN helper:\n{entry}"
        );
        assert!(
            entry.contains("n + 1i64") || entry.contains("(n + 1i64)"),
            "Entry.helper body must keep its OWN literal (1):\n{entry}"
        );
        // Entry's `Worker.walk(20)` must qualify through the crate
        // path too — bare `walk(20)` would also be ambiguous against
        // a hypothetical entry `walk`.
        assert!(
            entry.contains("crate::aver_generated::worker::walk"),
            "main()'s Worker.walk(20) must qualify through the \
             canonical crate path:\n{entry}"
        );
    }

    #[test]
    fn emission_banner_appears_in_root_main() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn main() -> Int
    1
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let root_main = generated_file(&out, "src/main.rs");

        assert!(root_main.contains("// Aver Rust emission"));
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
        let modules = vec![(
            vec!["app".to_string(), "cli".to_string()],
            "pub fn run() {}".to_string(),
        )];
        let files = synthesize_rust_module_cascade("src/aver_generated", &modules);

        let root_mod = files
            .iter()
            .find(|(path, _)| path == "src/aver_generated/mod.rs")
            .map(|(_, content)| content)
            .expect("root mod.rs should exist");

        assert!(root_mod.contains("pub mod app;"));
        assert!(!root_mod.contains("pub use app::*;"));
    }

    #[test]
    fn list_cons_match_uses_cloned_uncons_fast_path_when_optimized() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn headPlusTailLen(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h + List.len(t)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // The common []/[h,..t] pattern uses aver_list_match! macro
        assert!(entry.contains("aver_list_match!"));
    }

    #[test]
    fn list_cons_match_stays_structured_in_semantic_mode() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn headPlusTailLen(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h + List.len(t)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Both modes now use the aver_list_match! macro for []/[h,..t] patterns
        assert!(entry.contains("aver_list_match!"));
    }

    #[test]
    fn list_literal_clones_ident_when_used_afterward() {
        let mut ctx = ctx_from_source(
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

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("let first = aver_rt::AverList::from_vec(vec![audit.clone()]);"));
        // Borrowed param always needs .clone() when consumed
        assert!(entry.contains("aver_rt::AverList::from_vec(vec![audit.clone()])"));
    }

    #[test]
    fn record_update_clones_base_when_value_is_used_afterward() {
        let mut ctx = ctx_from_source(
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

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("..state.clone()"));
    }

    #[test]
    fn mutual_tco_generates_trampoline_instead_of_regular_calls() {
        let mut ctx = ctx_from_source(
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

        let out = transpile(&mut ctx);
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
        let mut ctx = ctx_from_source(
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

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Field access should produce exactly one .clone(), never .clone().clone()
        assert!(
            !entry.contains(".clone().clone()"),
            "double clone detected in generated code:\n{}",
            entry
        );
    }

    #[test]
    fn borrowed_record_field_return_clones_for_owned_result() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

record User
    name: String

fn getName(user: User) -> String
    user.name
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("pub fn getName(user: &User) -> AverStr"));
        assert!(
            entry.contains("user.name.clone()"),
            "missing owned clone:\n{}",
            entry
        );
    }

    #[test]
    fn vector_get_with_literal_default_lowers_to_direct_unwrap_or_code() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    Option.withDefault(Vector.get(grid, idx), 0)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("grid.get(idx as usize).cloned().unwrap_or(0i64)"));
    }

    #[test]
    fn vector_set_default_stays_structured_in_semantic_mode() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn updateOrKeep(vec: Vector<Int>, idx: Int, value: Int) -> Vector<Int>
    Option.withDefault(Vector.set(vec, idx, value), vec)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Both modes now use the inlined set_unchecked fast path
        assert!(entry.contains("set_unchecked"));
        assert!(!entry.contains(".unwrap_or("));
    }

    #[test]
    fn vector_set_default_uses_ir_leaf_fast_path_when_optimized() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn updateOrKeep(vec: Vector<Int>, idx: Int, value: Int) -> Vector<Int>
    Option.withDefault(Vector.set(vec, idx, value), vec)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("set_unchecked"));
        assert!(!entry.contains(".unwrap_or("));
    }

    #[test]
    fn vector_set_uses_owned_update_lowering() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn update(vec: Vector<Int>, idx: Int, value: Int) -> Option<Vector<Int>>
    Vector.set(vec, idx, value)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains(".set_owned("));
        assert!(!entry.contains(".set(idx as usize,"));
    }

    #[test]
    fn map_remove_uses_owned_update_lowering() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn dropKey(m: Map<String, Int>, key: String) -> Map<String, Int>
    Map.remove(m, key)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains(".remove_owned(&"));
    }

    #[test]
    fn semantic_keeps_known_leaf_wrapper_call_structured() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    Option.withDefault(Vector.get(grid, idx), 0)

fn read(grid: Vector<Int>, idx: Int) -> Int
    cellAt(grid, idx)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("cellAt(grid, idx)"));
        assert!(!entry.contains("__aver_thin_arg0"));
    }

    #[test]
    fn optimized_keeps_known_leaf_wrapper_callsite_and_leaves_absorption_to_rust() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    Option.withDefault(Vector.get(grid, idx), 0)

fn read(grid: Vector<Int>, idx: Int) -> Int
    cellAt(grid, idx)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("cellAt(grid, idx)"));
        assert!(!entry.contains("__aver_thin_arg0"));
    }

    #[test]
    fn optimized_keeps_known_dispatch_wrapper_callsite_and_leaves_absorption_to_rust() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn bucket(n: Int) -> Int
    match n == 0
        true -> 0
        false -> 1

fn readBucket(n: Int) -> Int
    bucket(n)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("bucket(n)"));
        assert!(!entry.contains("__aver_thin_arg0"));
    }

    #[test]
    fn bool_match_on_gte_normalizes_to_base_comparison_when_optimized() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn bucket(n: Int) -> Int
    match n >= 10
        true -> 7
        false -> 3
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("if (n < 10i64) { 3i64 } else { 7i64 }"));
    }

    #[test]
    fn bool_match_stays_as_match_in_semantic_mode() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn bucket(n: Int) -> Int
    match n >= 10
        true -> 7
        false -> 3
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Both modes now use the normalized if-else form
        assert!(entry.contains("if (n < 10i64) { 3i64 } else { 7i64 }"));
    }

    #[test]
    fn optimized_self_tco_uses_dispatch_table_for_wrapper_match() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn loop(r: Result<Int, String>) -> Int
    match r
        Result.Ok(n) -> n
        Result.Err(_) -> loop(Result.Ok(1))
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Uses native Rust match directly (r cloned since not-Copy Ident without last_use info)
        assert!(entry.contains("match r.clone() {"));
        assert!(entry.contains("Ok(n)"));
        assert!(!entry.contains("__dispatch_subject"));
    }

    #[test]
    fn optimized_mutual_tco_uses_dispatch_table_for_wrapper_match() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn left(r: Result<Int, String>) -> Int
    match r
        Result.Ok(n) -> n
        Result.Err(_) -> right(Result.Ok(1))

fn right(r: Result<Int, String>) -> Int
    match r
        Result.Ok(n) -> n
        Result.Err(_) -> left(Result.Ok(1))
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Uses native Rust match directly (r cloned since not-Copy Ident without last_use info)
        assert!(entry.contains("match r.clone() {"));
        assert!(entry.contains("Ok(n)"));
        assert!(!entry.contains("__dispatch_subject"));
    }

    #[test]
    fn single_field_variant_display_avoids_vec_join() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

type Wrapper
    Wrap(Int)
    Pair(Int, Int)
    Empty
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
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

        let out = transpile(&mut ctx);
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

        let out = transpile(&mut ctx);
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

        let out = transpile(&mut ctx);
        let root_main = generated_file(&out, "src/main.rs");

        assert!(
            root_main.contains("aver_replay::with_guest_scope(\"main\", serde_json::Value::Null")
        );
    }

    #[test]
    fn runtime_policy_codegen_uses_runtime_loader() {
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
        ctx.runtime_policy_from_env = true;

        let out = transpile(&mut ctx);
        let root_main = generated_file(&out, "src/main.rs");
        let replay_support = generated_file(&out, "src/replay_support.rs");
        let cargo_toml = generated_file(&out, "Cargo.toml");

        assert!(!root_main.contains("mod policy_support;"));
        assert!(replay_support.contains("load_runtime_policy_from_env"));
        assert!(cargo_toml.contains("url = \"2\""));
        assert!(cargo_toml.contains("toml = \"0.8\""));
    }

    #[test]
    fn replay_codegen_can_keep_embedded_policy_when_requested() {
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
        ctx.policy = Some(crate::config::ProjectConfig {
            effect_policies: std::collections::HashMap::new(),
            check_suppressions: Vec::new(),
            independence_mode: crate::config::IndependenceMode::default(),
            shape_layers: Vec::new(),
            shape_expected: Vec::new(),
        });

        let out = transpile(&mut ctx);
        let root_main = generated_file(&out, "src/main.rs");
        let replay_support = generated_file(&out, "src/replay_support.rs");

        assert!(root_main.contains("mod policy_support;"));
        assert!(replay_support.contains("aver_policy::check_disk"));
        assert!(!replay_support.contains("RuntimeEffectPolicy"));
    }

    #[test]
    fn self_host_support_is_emitted_as_separate_module() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn runGuestProgram(prog: Int, moduleFns: Int) -> Result<String, String>
    Result.Ok("ok")
"#,
            "demo",
        );
        ctx.emit_self_host_support = true;
        ctx.guest_entry = Some("runGuestProgram".to_string());

        let out = transpile(&mut ctx);
        let root_main = generated_file(&out, "src/main.rs");
        let runtime_support = generated_file(&out, "src/runtime_support.rs");
        let self_host_support = generated_file(&out, "src/self_host_support.rs");
        let entry = generated_rust_entry_file(&out);

        assert!(root_main.contains("mod self_host_support;"));
        assert!(!runtime_support.contains("with_fn_store"));
        assert!(self_host_support.contains("pub fn with_program_fn_store"));
        assert!(entry.contains("crate::self_host_support::with_program_fn_store("));
    }

    #[test]
    fn independent_product_codegen_avoids_string_specific_error_type() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn left() -> Result<Int, Int>
    Result.Ok(1)

fn right() -> Result<Int, Int>
    Result.Ok(2)

fn main() -> Result<Tuple<Int, Int>, Int>
    data = (left(), right())?!
    Result.Ok(data)
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(!entry.contains("Ok::<_, aver_rt::AverStr>"));
        assert!(entry.contains("crate::aver_replay::exit_effect_group();"));
        assert!(entry.contains("match (_r0, _r1)"));
        assert!(!entry.contains("let _r0 = left()?;"));
    }

    #[test]
    fn independent_product_codegen_emits_cancel_runtime_and_scope_propagation() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn left() -> Result<Int, String>
    Result.Ok(1)

fn right() -> Result<Int, String>
    Result.Ok(2)

fn main() -> Result<Tuple<Int, Int>, String>
    data = (left(), right())?!
    Result.Ok(data)
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;
        ctx.policy = Some(crate::config::ProjectConfig {
            effect_policies: std::collections::HashMap::new(),
            check_suppressions: Vec::new(),
            independence_mode: crate::config::IndependenceMode::Cancel,
            shape_layers: Vec::new(),
            shape_expected: Vec::new(),
        });

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);
        let runtime_support = generated_file(&out, "src/runtime_support.rs");
        let replay_support = generated_file(&out, "src/replay_support.rs");

        assert!(entry.contains("crate::run_cancelable_branch"));
        assert!(entry.contains("capture_parallel_scope_context"));
        assert!(entry.contains("_s.spawn(move ||"));
        assert!(runtime_support.contains("pub fn run_cancelable_branch"));
        assert!(runtime_support.contains("panic_any(AverCancelled)"));
        assert!(replay_support.contains("pub fn capture_parallel_scope_context"));
        assert!(replay_support.contains("pub fn independence_mode_is_cancel()"));
    }

    #[test]
    fn runtime_policy_codegen_parses_independence_mode() {
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
        ctx.runtime_policy_from_env = true;

        let out = transpile(&mut ctx);
        let replay_support = generated_file(&out, "src/replay_support.rs");

        assert!(replay_support.contains("independence_mode_cancel"));
        assert!(replay_support.contains("[independence].mode must be 'complete' or 'cancel'"));
    }

    #[test]
    fn effectful_codegen_inserts_cancel_checkpoint_before_builtin_calls() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn main() -> Result<String, String>
    ! [Disk.readText]
    Disk.readText("demo.av")
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("crate::cancel_checkpoint(); (aver_rt::read_text"));
    }

    #[test]
    fn replay_support_matches_group_effects_by_occurrence_and_args() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn left() -> Result<Int, String>
    Result.Ok(1)

fn right() -> Result<Int, String>
    Result.Ok(2)

fn main() -> Result<Tuple<Int, Int>, String>
    data = (left(), right())?!
    Result.Ok(data)
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;

        let out = transpile(&mut ctx);
        let replay_support = generated_file(&out, "src/replay_support.rs");

        assert!(replay_support.contains("candidate.effect_occurrence"));
        assert!(replay_support.contains("candidate.args != args"));
    }
}
