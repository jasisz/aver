/// Rust backend for the Aver transpiler.
///
/// Transforms Aver AST -> valid Rust source code.
mod builtins;
pub(crate) mod composition;
pub mod emit_ctx;
mod expr;
mod from_mir;
pub use from_mir::{CoverageReport, MirEmitCtx, coverage_report, coverage_report_with_blockers};
mod pattern;
mod policy;
mod project;
mod provider;
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

/// Whether the shared proof analysis selected a byte-wide physical carrier
/// for this nominal refinement. Rust intentionally consumes only `U8` in the
/// first slice; every other proven width keeps the established representation.
pub(super) fn uses_packed_u8(ctx: &CodegenContext, type_name: &str) -> bool {
    matches!(
        ctx.packed_sequence_layouts
            .get(type_name)
            .map(|layout| layout.element),
        Some(crate::codegen::proof_lower::PackedIntElement::U8)
    )
}

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
    let required = provider::required_operations(ctx);
    transpile_project(ctx, required, composition::ProviderComposition::default())
}

#[cfg(feature = "runtime")]
pub fn transpile_with_provider_manifest(
    ctx: &mut CodegenContext,
    manifest: Option<&crate::config::ProviderPackageManifest>,
) -> Result<ProjectOutput, String> {
    let known_capabilities = ctx
        .capabilities
        .contracts()
        .map(|contract| contract.module.clone())
        .collect();
    transpile_with_provider_manifest_for_project(ctx, manifest, &known_capabilities)
}

/// Transpile one entry program using a provider manifest owned by its whole
/// project. Bindings known to the project but unreachable from this program
/// remain inactive and are not emitted as Cargo dependencies.
#[cfg(feature = "runtime")]
pub fn transpile_with_provider_manifest_for_project(
    ctx: &mut CodegenContext,
    manifest: Option<&crate::config::ProviderPackageManifest>,
    known_capabilities: &BTreeSet<String>,
) -> Result<ProjectOutput, String> {
    let required = provider::required_operations(ctx);
    let composition =
        composition::plan_for_project(&ctx.capabilities, &required, manifest, known_capabilities)?;
    Ok(transpile_project(ctx, required, composition))
}

fn transpile_project(
    ctx: &mut CodegenContext,
    required_provider_operations: BTreeSet<String>,
    provider_composition: composition::ProviderComposition,
) -> ProjectOutput {
    // Every refusal below is recorded on the context as it happens; this
    // transpile reports its own, not a previous one's.
    ctx.substituted_compile_errors.borrow_mut().clear();
    ctx.omitted_verify_cases.borrow_mut().clear();
    // ETAP-2 SLICE 1: make Int representation EXPLICIT in the MIR the Rust
    // backend codegens from. This runs ONLY here (the Rust entry) — the VM,
    // wasm-gc, proof, Dafny and Lean backends never call `transpile`, so
    // their `ctx.mir_program` keeps the all-`Int` representation and never
    // sees a `Box`/`Unbox` node. The rewrite reuses the (already-computed)
    // `bare_i64` range+escape analysis to tag each fn's `repr` and insert
    // the explicit boundary nodes; the body emitter below then lowers those
    // nodes trivially instead of deciding representation itself.
    if let Some(prog) = ctx.mir_program.take() {
        // Rust consumes and drops returned aggregate wrappers (`Result`,
        // records, tuples) at projection / `?` boundaries. Refine collection
        // ownership once more with that backend-specific fact before the Int
        // representation rewrite. Arena backends deliberately never run this
        // pass: a logically-unwrapped aggregate remains a live holder there.
        let prog = crate::ir::mir::optimize::own_param_refine_for_rust(prog);
        // Mutual-TCO members are emitted with an unconditionally boxed
        // (`AverInt`) trampoline signature regardless of the unboxing
        // analysis, so the rewrite must not tag them bare (it would desync
        // the boxed signature from a bare-rewritten body / caller).
        let boxed = ctx.mutual_tco_members.clone();
        // ETAP-2 SLICE 0+1: derive the per-carrier-type proven bound from the
        // same refinement-via-opaque inputs the proof side reads, so a
        // carrier function-slot can lower to native `i64`. An empty table
        // (no opaque-bounded carrier in scope) keeps the all-`Int` behavior.
        // Scoped so the immutable `&ctx` borrow ends before the assignment.
        let carrier = {
            let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
            crate::codegen::proof_lower::carrier_interval_table(&inputs)
        };
        ctx.mir_program = Some(
            crate::ir::mir::optimize::bare_i64_rewrite::rewrite_for_rust(prog, &boxed, &carrier),
        );
    }
    let has_embedded_policy = ctx.policy.is_some();
    let has_runtime_policy = ctx.runtime_policy_from_env;
    let embedded_independence_cancel = ctx
        .policy
        .as_ref()
        .is_some_and(|config| config.independence_mode == crate::config::IndependenceMode::Cancel);
    let used_services = detect_used_services(ctx);
    let has_provider_runtime = !required_provider_operations.is_empty();
    let needs_http_types =
        needs_named_type(ctx, "HttpResponse") || needs_named_type(ctx, "HttpRequest");
    let needs_tcp_types = needs_named_type(ctx, "Tcp.Connection");
    let needs_terminal_types = needs_named_type(ctx, "Terminal.Size");
    // Oracle-proof stub fns take a leading `BranchPath` param. They are
    // emitted as dead-at-runtime fns (module-level fns emit regardless of
    // reachability), so the type must be in scope to compile.
    let needs_branch_path = needs_named_type(ctx, "BranchPath");

    let has_tcp_runtime = used_services.contains("Tcp");
    let has_http_runtime = used_services.contains("Http");
    let has_http_server_runtime = used_services.contains("HttpServer");
    let has_terminal_runtime = used_services.contains("Terminal");

    let has_tcp_types =
        (has_tcp_runtime || needs_tcp_types) && ctx.capabilities.contract("Tcp").is_none();
    let has_http_types = has_http_runtime || has_http_server_runtime || needs_http_types;
    let has_http_server_types = has_http_server_runtime || needs_named_type(ctx, "HttpRequest");
    let has_terminal_types = has_terminal_runtime || needs_terminal_types;

    // Root dispatch consumes the canonical resolved declaration. Resolve
    // `main` once by entry-scope identity and keep the AST lookup only for the
    // construction invariant below; no emitter receives the raw declaration.
    let main_fn = ctx.fn_defs.iter().find(|fd| fd.name == "main");
    let resolved_main_fn = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry("main"))
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    debug_assert_eq!(
        main_fn.is_some(),
        resolved_main_fn.is_some(),
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
                &provider_composition,
            ),
        ),
        (
            "src/main.rs".to_string(),
            render_root_main(
                resolved_main_fn,
                has_embedded_policy,
                ctx.emit_replay_runtime,
                ctx.guest_entry.as_deref(),
                !verify_blocks.is_empty(),
                ctx.emit_self_host_support,
                has_provider_runtime.then_some(&provider_composition),
            ),
        ),
        (
            "src/runtime_support.rs".to_string(),
            render_runtime_support(
                has_tcp_types,
                has_http_types,
                has_http_server_types,
                has_terminal_types,
                needs_branch_path,
                ctx.emit_replay_runtime,
                embedded_independence_cancel,
            ),
        ),
    ];

    if has_provider_runtime {
        files.push((
            "src/provider_support.rs".to_string(),
            provider::generate_provider_runtime(
                &ctx.capabilities,
                &required_provider_operations,
                ctx.policy.as_ref().map(|config| config.tcp_settings),
                ctx.runtime_policy_from_env,
            ),
        ));
        files.push((
            "src/lib.rs".to_string(),
            render_root_library(
                has_embedded_policy,
                ctx.emit_replay_runtime,
                ctx.emit_self_host_support,
            ),
        ));
    }

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
            replay::generate_replay_runtime(replay::ReplayRuntimeOptions {
                has_embedded_policy,
                has_runtime_policy,
                has_terminal_types,
                has_tcp_types,
                has_http_types,
                has_http_server_types,
                embedded_independence_cancel,
                standard_capabilities: ctx
                    .capabilities
                    .contracts()
                    .filter(|contract| crate::stdlib::is_standard_capability(&contract.module))
                    .map(|contract| contract.module.clone())
                    .collect(),
                has_provider_runtime,
                capability_operations: ctx
                    .capabilities
                    .operations()
                    .map(|operation| (operation.canonical_name.clone(), operation.module.clone()))
                    .collect(),
                live_replay_capabilities: {
                    let mut live = std::collections::BTreeSet::new();
                    for name in &required_provider_operations {
                        let Some(operation) = ctx.capabilities.operation(name) else {
                            continue;
                        };
                        let is_pure =
                            ctx.capabilities
                                .contract(&operation.module)
                                .is_some_and(|contract| {
                                    contract.semantics
                                        == crate::capability::CapabilitySemantics::Pure
                                });
                        if is_pure
                            || operation.replay
                                == Some(crate::capability::ReplaySemantics::Reissued)
                        {
                            live.insert(operation.module.clone());
                        }
                    }
                    live.into_iter().collect()
                },
            }),
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
            codegen_depends(root_module_depends(&ctx.items), &ctx.items, None),
            entry_module_sections(ctx, resolved_main_fn, &top_level_stmts),
        ),
    ));

    for i in 0..ctx.modules.len() {
        let module = &ctx.modules[i];
        let segments = module_prefix_to_rust_segments(&module.prefix);
        let discovery_items = module
            .fn_defs
            .iter()
            .cloned()
            .map(TopLevel::FnDef)
            .chain(module.type_defs.iter().cloned().map(TopLevel::TypeDef))
            .collect::<Vec<_>>();
        rust_modules.push((
            segments,
            render_generated_module(
                codegen_depends(
                    module.depends.clone(),
                    &discovery_items,
                    Some(&module.prefix),
                ),
                module_sections(module, ctx),
            ),
        ));
    }
    files.extend(synthesize_rust_module_cascade(
        "src/aver_generated",
        &rust_modules,
    ));
    files.sort_by(|left, right| left.0.cmp(&right.0));

    ProjectOutput {
        files,
        substituted_compile_errors: ctx.substituted_compile_errors.borrow().clone(),
        omitted_verify_cases: ctx.omitted_verify_cases.borrow().clone(),
    }
}

fn render_root_library(has_policy: bool, has_replay: bool, has_self_host_support: bool) -> String {
    let mut sections = vec![
        "#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns, hidden_glob_reexports)]".to_string(),
        "// Aver Rust library emission — native provider host boundary".to_string(),
        "#[macro_use] extern crate aver_rt;".to_string(),
        "pub use ::aver_rt::AverMap as HashMap;".to_string(),
        "pub use ::aver_rt::AverStr;".to_string(),
        "pub use ::aver_rt::Buffer;".to_string(),
        "pub use ::aver_rt::ByteBuilder;".to_string(),
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
    sections.push("pub mod provider_support;".to_string());
    sections.push(
        "pub use provider_support::{install_provider_bindings, install_provider_bindings_exact, preflight_required_providers};"
            .to_string(),
    );
    sections.push(String::new());
    sections.push("pub mod aver_generated;".to_string());
    sections.push(String::new());
    sections.join("\n")
}

fn render_root_main(
    main_fn: Option<&crate::ir::hir::ResolvedFnDef>,
    has_policy: bool,
    has_replay: bool,
    guest_entry: Option<&str>,
    has_verify: bool,
    has_self_host_support: bool,
    provider_composition: Option<&composition::ProviderComposition>,
) -> String {
    let has_provider_runtime = provider_composition.is_some();
    let mut sections = vec![
        "#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns, hidden_glob_reexports)]".to_string(),
        "// Aver Rust emission".to_string(),
        "#[macro_use] extern crate aver_rt;".to_string(),
        "pub use ::aver_rt::AverMap as HashMap;".to_string(),
        "pub use ::aver_rt::AverStr;".to_string(),
        "pub use ::aver_rt::Buffer;".to_string(),
        "pub use ::aver_rt::ByteBuilder;".to_string(),
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

    if let Some(provider_composition) = provider_composition {
        sections.push(String::new());
        sections.push("pub mod provider_support;".to_string());
        sections.push(
            "pub use provider_support::{install_provider_bindings, install_provider_bindings_exact};"
                .to_string(),
        );
        sections.push(String::new());
        sections.extend(provider_composition.render_bootstrap());
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
    let returns_result = main_fn.is_some_and(|fd| matches!(&fd.return_type, Type::Result(_, _)));
    let result_unit_string = main_fn.is_some_and(|fd| {
        matches!(
            &fd.return_type,
            Type::Result(ok, err)
                if matches!(ok.as_ref(), Type::Unit) && matches!(err.as_ref(), Type::Str)
        )
    });
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
            if has_provider_runtime {
                sections.push("    if let Err(error) = bootstrap_provider_bindings() { eprintln!(\"{}\", error); std::process::exit(1); }".to_string());
            }
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
            let ret_type = types::type_to_rust(&main_fn.unwrap().return_type);
            sections.push(format!("fn main() -> {} {{", ret_type));
            if has_provider_runtime {
                sections.push("    bootstrap_provider_bindings().unwrap_or_else(|error| panic!(\"{}\", error));".to_string());
            }
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
        if has_provider_runtime {
            sections.push("    if let Err(error) = bootstrap_provider_bindings() { eprintln!(\"{}\", error); std::process::exit(1); }".to_string());
        }
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
    has_terminal_types: bool,
    needs_branch_path: bool,
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
    if has_terminal_types {
        sections.push(runtime::generate_terminal_types());
    }
    if needs_branch_path {
        sections.push(runtime::generate_branch_path_types());
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

fn codegen_depends(
    mut depends: Vec<String>,
    items: &[TopLevel],
    current_module: Option<&str>,
) -> Vec<String> {
    depends.retain(|dependency| Some(dependency.as_str()) != current_module);
    for dependency in crate::stdlib::implicit_stdlib_deps(items) {
        if crate::stdlib::is_standard_capability(&dependency)
            && Some(dependency.as_str()) != current_module
            && !depends.iter().any(|existing| existing == &dependency)
        {
            depends.push(dependency);
        }
    }
    depends
}

/// Emit one mutual-TCO block (enum + trampoline + wrappers) via the MIR
/// walker. The HIR emitter is gone (rust-on-MIR W6/Stage-3): every
/// member resolves to a `(MirFn, ResolvedFnDef)` pair and the trampoline
/// is synthesized all-or-nothing from MIR. A member missing a `MirFn` /
/// `ResolvedFnDef`, or a `None` from the walker, is a hard codegen error
/// (`compile_error!` in the wrapper) — never a panic and never a silent
/// drop. TCO is verified behaviorally (build + run vs VM + self-host
/// regen), not by byte-parity.
fn emit_mutual_tco_block_routed(
    group_id: usize,
    group_fns: &[&crate::ir::hir::ResolvedFnDef],
    ctx: &CodegenContext,
    scope: Option<&str>,
    visibility: &str,
) -> String {
    // Every member already carries its canonical FnId. MIR lookup is the
    // only remaining join and cannot accidentally cross a same-name scope.
    let mir_fns: Option<Vec<&crate::ir::mir::MirFn>> =
        ctx.mir_program.as_ref().and_then(|program| {
            group_fns
                .iter()
                .map(|fd| program.fn_by_id(fd.fn_id))
                .collect()
        });
    let code = mir_fns.and_then(|mir_fns| {
        from_mir::emit_mir_mutual_tco_block(group_id, group_fns, &mir_fns, ctx, scope, visibility)
    });
    code.unwrap_or_else(|| {
        let names = group_fns
            .iter()
            .map(|fd| fd.name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        // A trampoline is all-or-nothing, so one member's unclassified name
        // takes the whole group with it. Report that name rather than the
        // roster: the roster is what the reporter of #1076 was handed, and
        // it points at every function except the one at fault.
        let message = group_fns
            .iter()
            .filter_map(|fd| toplevel::unresolved_name_reason(fd, scope))
            .next()
            .unwrap_or_else(|| format!("MIR walker could not render mutual-TCO block [{names}]"));
        ctx.substituted_compile_errors
            .borrow_mut()
            .push(message.clone());
        format!(
            "{}fn __mutual_tco_block_{}_render_error() {{ compile_error!({:?}); }}",
            visibility, group_id, message
        )
    })
}

fn missing_resolved_fn_error(fd: &FnDef, scope: Option<&str>, ctx: &CodegenContext) -> String {
    let qualified = scope
        .map(|prefix| format!("{prefix}.{}", fd.name))
        .unwrap_or_else(|| fd.name.clone());
    let message = format!(
        "Rust codegen requires resolved HIR for function `{qualified}`; \
         all synthesis and rewriting must finish before CodegenContext is built"
    );
    ctx.substituted_compile_errors
        .borrow_mut()
        .push(message.clone());
    format!("compile_error!({message:?});")
}

fn entry_module_sections(
    ctx: &CodegenContext,
    main_fn: Option<&crate::ir::hir::ResolvedFnDef>,
    top_level_stmts: &[&crate::ast::Stmt],
) -> Vec<String> {
    let mut sections = Vec::new();

    for td in &ctx.type_defs {
        if is_shared_runtime_type(td) {
            continue;
        }
        sections.push(toplevel::emit_public_type_def(td, ctx));
        if ctx.emit_replay_runtime {
            sections.push(replay::emit_replay_value_impl(
                td,
                uses_packed_u8(ctx, crate::codegen::common::type_def_name(td)),
            ));
        }
    }

    // Detect mutual TCO groups directly from resolved tail-call identities.
    let non_main_fns: Vec<&crate::ir::hir::ResolvedFnDef> = ctx
        .resolved_program
        .entry_fns()
        .filter(|fd| fd.name != "main")
        .collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&non_main_fns);

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&crate::ir::hir::ResolvedFnDef> =
            group_indices.iter().map(|&idx| non_main_fns[idx]).collect();
        sections.push(emit_mutual_tco_block_routed(
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
        if fd.name == "main" {
            continue;
        }
        let Some(fn_id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
            sections.push(missing_resolved_fn_error(fd, None, ctx));
            continue;
        };
        let is_mutual = ctx.mutual_tco_members.contains(&fn_id);
        if is_mutual {
            continue;
        }
        let Some(resolved_fd) = ctx.resolved_program.fn_by_id(fn_id) else {
            sections.push(missing_resolved_fn_error(fd, None, ctx));
            continue;
        };
        sections.push(toplevel::emit_public_fn_def(fd, resolved_fd, ctx, None));
    }

    if main_fn.is_some() || !top_level_stmts.is_empty() {
        sections.push(toplevel::emit_public_main(main_fn, top_level_stmts, ctx));
    }

    sections
}

fn module_sections(module: &crate::codegen::ModuleInfo, ctx: &CodegenContext) -> Vec<String> {
    let mut sections = Vec::new();

    if let Some(resource_types) =
        provider::resource_types_by_module(&ctx.capabilities).get(&module.prefix)
    {
        for name in resource_types {
            sections.push(provider::emit_resource_type(
                &module.prefix,
                name,
                ctx.emit_replay_runtime,
            ));
        }
    }

    for td in &module.type_defs {
        if is_shared_runtime_type(td) {
            continue;
        }
        sections.push(toplevel::emit_public_type_def(td, ctx));
        let canonical = format!(
            "{}.{}",
            module.prefix,
            crate::codegen::common::type_def_name(td)
        );
        if canonical == "Bytes.Bytes" && ctx.capabilities.uses_standard_bytes() {
            sections.push(provider::emit_standard_bytes_codec(uses_packed_u8(
                ctx, "Bytes",
            )));
        }
        if ctx.capabilities.boundary_type(&canonical).is_some() {
            sections.push(provider::emit_represented_type_codec(&module.prefix, td));
        }
        if ctx.emit_replay_runtime {
            sections.push(replay::emit_replay_value_impl(
                td,
                uses_packed_u8(ctx, crate::codegen::common::type_def_name(td)),
            ));
        }
    }

    // Capability hostile profiles are executable specifications, not runtime
    // implementation.  Drop their full model-only closure here, at the
    // runtime-backend boundary.  The capability registry preserves helpers
    // that are also reachable from an exported ordinary function.
    let verification_only_fns = ctx.capabilities.verification_only_function_names(
        &module.prefix,
        &module.fn_defs,
        &module.exposes,
    );
    let fn_refs: Vec<&crate::ir::hir::ResolvedFnDef> = ctx
        .resolved_program
        .module_fns(&module.prefix)
        .filter(|fd| !verification_only_fns.contains(&fd.name))
        .collect();
    let mutual_groups = toplevel::find_mutual_tco_groups(&fn_refs);

    for (group_id, group_indices) in mutual_groups.iter().enumerate() {
        let group_fns: Vec<&crate::ir::hir::ResolvedFnDef> =
            group_indices.iter().map(|&idx| fn_refs[idx]).collect();
        sections.push(emit_mutual_tco_block_routed(
            group_id + 1,
            &group_fns,
            ctx,
            Some(&module.prefix),
            "pub ",
        ));
    }

    for fd in &module.fn_defs {
        if verification_only_fns.contains(&fd.name) {
            continue;
        }
        let Some(fn_id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
            sections.push(missing_resolved_fn_error(fd, Some(&module.prefix), ctx));
            continue;
        };
        if ctx.mutual_tco_members.contains(&fn_id) {
            continue;
        }
        let Some(resolved_fd) = ctx.resolved_program.fn_by_id(fn_id) else {
            sections.push(missing_resolved_fn_error(fd, Some(&module.prefix), ctx));
            continue;
        };
        sections.push(toplevel::emit_public_fn_def(
            fd,
            resolved_fd,
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

    fn ctx_from_source(source: &str, project_name: &str) -> crate::codegen::CodegenContext {
        let mut items = parse_source(source).expect("source should parse");
        // Run the canonical compiler pipeline exactly as the CLI
        // (`main::commands::compile`) does, so the in-process
        // `resolved_items` / `symbol_table` / `analysis` — and the MIR
        // `build_context` lowers from them — match the real pipeline.
        // The previous hand-rolled tco + typecheck + `resolve_program`
        // skipped the `last_use` and `analyze` stages, so its resolved
        // AST carried no `last_use` stamps; the per-fn MIR then diverged
        // from the CLI's (e.g. the `Option.withDefault(Vector.set(v, …),
        // v)` fusion's same-vector reads lost their last-use ownership),
        // silently testing a different MIR shape than production emits.
        let pipeline_result = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                run_build_symbols: true,
                ..Default::default()
            },
        );
        let tc = pipeline_result.typecheck.expect("typecheck was requested");
        assert!(
            tc.errors.is_empty(),
            "source should typecheck without errors: {:?}",
            tc.errors
        );
        build_context(
            items,
            &tc,
            pipeline_result.analysis.as_ref(),
            project_name.to_string(),
            vec![],
            pipeline_result.symbol_table,
            pipeline_result.resolved_items,
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
                        crate::ast::TopLevel::FnDef(fd) => Some(fd.clone()),
                        _ => None,
                    })
                    .collect();
                let (capability_items, capability_semantics) =
                    crate::codegen::capability_metadata(&lm.items);
                let decl = crate::visibility::module_decl(&lm.items);
                crate::codegen::ModuleInfo {
                    prefix: lm.dep_name.clone(),
                    depends,
                    exposes: decl.map(|d| d.exposes.clone()).unwrap_or_default(),
                    exposes_opaque: decl.map(|d| d.exposes_opaque.clone()).unwrap_or_default(),
                    type_defs,
                    fn_defs,
                    capability_items,
                    capability_semantics,
                    verify_blocks: crate::codegen::collect_verify_blocks(&lm.items),
                    verify_laws: crate::codegen::collect_verify_laws(&lm.items),
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
    fn capability_model_closure_is_omitted_from_runtime_rust() {
        let mut ctx = ctx_from_multi(
            r#"
module Entry
    depends [Clock]

fn main() -> Int
    Clock.visible()

verify main
    main() => 2
"#,
            &[(
                "Clock",
                r#"
module Clock
    kind = capability
    semantics = effectful
    exposes [now, visible]

operation now() -> Int
    oracle = generative
    replay = recorded
    hostile = [normal]

fn modelOnly() -> Int
    40

fn shared() -> Int
    2

fn normal(branch: BranchPath, call: Int) -> Int
    modelOnly() + shared()

fn visible() -> Int
    shared()
"#,
            )],
            "capability-model-runtime-boundary",
        );

        let out = transpile(&mut ctx);
        let clock = generated_file(&out, "src/aver_generated/clock/mod.rs");
        let verify = generated_file(&out, "src/verify.rs");

        assert!(clock.contains("pub fn visible("), "{clock}");
        assert!(clock.contains("pub fn shared("), "{clock}");
        assert!(!clock.contains("fn normal("), "{clock}");
        assert!(!clock.contains("fn modelOnly("), "{clock}");
        assert!(verify.contains("fn test_main_case_1()"), "{verify}");
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
            worker.contains("n.add(&aver_rt::AverInt::from_i64(100))"),
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
            entry.contains("n.add(&aver_rt::AverInt::from_i64(1))"),
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
    List.concat(first, [audit])
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // `first` is consumed by `List.concat` so it stays live (an unused
        // binding is correctly dead-code-eliminated); both `[audit]` literals
        // then clone the borrowed `audit` because it is used more than once.
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
    "{updated.currency}-{state.paymentId}"
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // `updated` is consumed by the interpolation so it stays live; the
        // record update then clones the borrowed `state` because `state` is
        // used again afterward (`state.paymentId`).
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
    fn missing_resolved_function_is_a_hard_codegen_error() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn helper(n: Int) -> Int
    n + 1
"#,
            "missing-resolved-fn",
        );
        ctx.resolved_program = crate::codegen::program_view::ResolvedProgramView::default();

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("compile_error!"), "{entry}");
        assert!(
            out.generated_compile_errors()
                .iter()
                .any(|error| error.contains("resolved HIR for function `helper`")),
            "{:?}",
            out.generated_compile_errors()
        );
    }

    #[test]
    fn mutual_tco_variant_for_keyword_named_fn_is_an_identifier() {
        // A function whose Aver name is a Rust keyword takes part in a
        // mutual-TCO group. The trampoline variant is the function name
        // capitalised, so it must be capitalised BEFORE the keyword
        // escape — escaping first yields `R#await`, which is not an
        // identifier and stops the parser on the generated project.
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn await(budget: Int) -> Int
    match budget == 0
        true -> 0
        false -> resume(budget - 1)

fn resume(budget: Int) -> Int
    match budget == 0
        true -> 1
        false -> await(budget - 1)

fn move(n: Int) -> Int
    match n == 0
        true -> 2
        false -> impl(n - 1)

fn impl(n: Int) -> Int
    match n == 0
        true -> 3
        false -> move(n - 1)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        // Both pairs really went through the trampoline.
        assert!(
            entry.contains("enum __MutualTco1"),
            "no trampoline:\n{entry}"
        );
        assert!(
            entry.contains("enum __MutualTco2"),
            "no trampoline:\n{entry}"
        );

        // Variants are the capitalised names — plain identifiers, no escape.
        for variant in ["Await", "Resume", "Move", "Impl"] {
            assert!(
                entry.contains(&format!("::{}", variant)),
                "missing variant `{variant}`:\n{entry}"
            );
        }

        // Nothing anywhere may carry a capitalised raw-identifier prefix.
        assert!(
            !entry.contains("R#"),
            "capitalised raw-identifier prefix in generated code:\n{entry}"
        );

        // The wrapper fns keep the ordinary lowercase escape.
        assert!(entry.contains("pub fn r#await"), "no escaped fn:\n{entry}");
        assert!(entry.contains("pub fn r#move"), "no escaped fn:\n{entry}");
        assert!(entry.contains("pub fn r#impl"), "no escaped fn:\n{entry}");
    }

    #[test]
    fn self_tco_keyword_named_fn_stays_escaped() {
        // Control for the mutual-TCO variant fix: a keyword-named function
        // that only recurses into itself emits no trampoline enum, and its
        // name keeps the lowercase raw-identifier escape.
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn await(n: Int) -> Int
    match n == 0
        true -> 0
        false -> await(n - 1)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(entry.contains("pub fn r#await"), "no escaped fn:\n{entry}");
        assert!(!entry.contains("R#"), "bad escape:\n{entry}");
    }

    /// Every Rust word that cannot be an identifier gets escaped, not just
    /// the handful that recursion happens to exercise. The escape table used
    /// to hold only the words someone had run into, so a plain
    /// `fn become(n: Int) -> Int` emitted `pub fn become` and the generated
    /// project did not parse — no recursion, no trampoline, nothing exotic.
    ///
    /// The list is the strict and reserved-for-future keywords of Rust
    /// edition 2024, read off `rustc` rather than remembered: a word belongs
    /// here exactly when `pub fn WORD() {}` is rejected and `pub fn r#WORD()
    /// {}` is accepted. Words Aver's own lexer reserves (`fn`, `match`,
    /// `type`, `true`, `false`) can never reach the emitter, so they are
    /// checked through the escape helper instead of through a program.
    #[test]
    fn every_rust_reserved_word_is_escaped_as_a_fn_name() {
        use crate::codegen::rust::syntax::aver_name_to_rust;

        // Reachable as an Aver fn name: not an Aver keyword, and spellable
        // in Rust once escaped.
        let nameable = [
            "as", "async", "await", "box", "break", "const", "continue", "dyn", "else", "enum",
            "extern", "for", "gen", "if", "impl", "in", "let", "loop", "mod", "move", "mut", "pub",
            "ref", "return", "static", "struct", "trait", "unsafe", "use", "where", "while",
            "yield", "abstract", "become", "do", "final", "macro", "override", "priv", "typeof",
            "unsized", "virtual", "try",
        ];

        let mut src = String::from("module Demo\n\n");
        for name in nameable {
            src.push_str(&format!("fn {name}(n: Int) -> Int\n    n + 1\n\n"));
        }
        let mut ctx = ctx_from_source(&src, "demo");
        let entry = {
            let out = transpile(&mut ctx);
            generated_rust_entry_file(&out).to_string()
        };

        for name in nameable {
            assert!(
                entry.contains(&format!("pub fn r#{name}(")),
                "`{name}` was emitted unescaped, so the project will not parse:\n{entry}"
            );
        }

        // Aver reserves these itself, so no program can carry them; the
        // table still has to know them, because it is the escape helper and
        // not the Aver lexer that other emitters ask.
        for name in ["fn", "match", "type", "true", "false"] {
            assert_eq!(
                aver_name_to_rust(name),
                format!("r#{name}"),
                "`{name}` is a Rust keyword but the escape table does not list it"
            );
        }
    }

    /// `crate`, `self`, `super`, `Self` and `_` are the five names Rust
    /// cannot spell as identifiers at all — `r#crate` is a parse error, not
    /// an escape — and none of them is an Aver keyword, so a program can
    /// carry all five. Each gets the `_avr_` prefix instead, in every
    /// position the emitter writes a name: a `fn` name, a parameter, a
    /// binding, a record field.
    ///
    /// What is pinned here is that the rename is applied at the SAME name
    /// on both sides of each pair — the field declaration and the field
    /// read, the fn declaration and its call — because a rename applied to
    /// only one of them is a project that does not build.
    #[test]
    fn never_spellable_names_are_renamed_rather_than_refused() {
        let cases: &[(&str, &[&str])] = &[
            (
                "fn crate(n: Int) -> Int\n    n + 1\n\nfn calls(n: Int) -> Int\n    crate(n)\n",
                &["pub fn _avr_crate(", "_avr_crate("],
            ),
            ("fn takes(self: Int) -> Int\n    self + 1\n", &["_avr_self"]),
            (
                "fn binds(n: Int) -> Int\n    super = n + 1\n    super\n",
                &["let _avr_super"],
            ),
            ("fn discards(_: Int) -> Int\n    7\n", &["_avr__"]),
            (
                "record Holder\n  crate: Int\n\nfn read(h: Holder) -> Int\n    h.crate\n",
                &["_avr_crate: ", "._avr_crate"],
            ),
        ];

        for (body, wanted) in cases {
            let mut ctx = ctx_from_source(&format!("module Demo\n\n{body}"), "demo");
            let out = transpile(&mut ctx);
            let entry = generated_rust_entry_file(&out);
            for want in *wanted {
                assert!(
                    entry.contains(want),
                    "expected the renamed spelling `{want}` in:\n{entry}"
                );
            }
            assert!(
                !entry.contains("r#crate")
                    && !entry.contains("r#self")
                    && !entry.contains("r#super")
                    && !entry.contains("r#_"),
                "a name with no raw spelling was escaped instead of renamed:\n{entry}"
            );
        }
    }

    /// The rename has to be INJECTIVE, or two different things silently
    /// become one. The prefix is doubled for a name that already carries
    /// it, so a user's own `_avr_self` and Aver's rename of `self` stay
    /// apart — and a program holding both builds and keeps them apart.
    #[test]
    fn the_mangle_prefix_is_doubled_so_the_rename_stays_injective() {
        use crate::codegen::rust::syntax::aver_name_to_rust;

        assert_eq!(aver_name_to_rust("self"), "_avr_self");
        assert_eq!(aver_name_to_rust("_avr_self"), "_avr__avr_self");
        assert_ne!(
            aver_name_to_rust("self"),
            aver_name_to_rust("_avr_self"),
            "the rename collapsed two different Aver names onto one Rust name"
        );

        // Identity for everything else — which is every name in the corpus,
        // so no existing program's emitted bytes move.
        for name in ["value", "isOdd", "_private", "日本語", ""] {
            assert_eq!(aver_name_to_rust(name), name);
        }
        assert_eq!(aver_name_to_rust("await"), "r#await");
    }

    /// A binding written at module level, outside any function, is a
    /// `TopLevel::Stmt` — it lives in `ctx.items` and in nothing else, so
    /// the emitter renders it into `fn main` as `let {name} = …;`. That is
    /// exactly as unspellable as a binding inside a function and gets the
    /// same rename; `let r#self = 41i64;` is ``error: `self` cannot be a
    /// raw identifier``.
    #[test]
    fn module_level_bindings_are_renamed_too() {
        let mut ctx = ctx_from_source(
            "module Demo\n\nself = 41\n\nfn read() -> Int\n    self\n",
            "demo",
        );
        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("_avr_self"),
            "the module-level binding was not renamed:\n{entry}"
        );
        assert!(
            !entry.contains("r#self"),
            "`r#self` is a parse error, not an escape:\n{entry}"
        );
    }

    /// `_` is the odd one out among the five: it is not a keyword but the
    /// wildcard, and Aver's lexer takes it as an ordinary identifier
    /// (`src/lexer.rs`), so `fn _(n: Int)` parses and runs. Rust has no
    /// spelling for it anywhere it needs a name — `pub fn _()` is
    /// ``expected identifier, found reserved identifier``, `mut _` is
    /// ``mut` must be followed by a named binding``, `_` as a call argument
    /// is ``in expressions, `_` can only be used on the left-hand side of
    /// an assignment``, and `r#_` is `` `_` cannot be a raw identifier ``.
    ///
    /// So it is renamed like the other four, and the prefix leads with an
    /// underscore precisely so `_avr__` still reads as "unused" to rustc:
    /// a user who writes `_` is saying they do not care about the binding,
    /// and the rename must not turn that into an unused-variable warning.
    ///
    /// Every shape is checked, because each one used to need the wildcard
    /// to survive a DIFFERENT position: a plain parameter is a pattern, a
    /// self-TCO parameter is the loop's mutable state (`mut _`), a mutual
    /// group additionally passes it by name to build the trampoline
    /// variant, and a collection parameter the ownership analysis proves
    /// uniquely owned is taken by value as `mut _` with no recursion at
    /// all.
    #[test]
    fn the_wildcard_is_renamed_in_every_position_that_needs_a_name() {
        const SELF_TCO_WILDCARD_PARAM: &str = "\
fn count(n: Int, _: Int) -> Int
    match n == 0
        true -> 0
        false -> count(n - 1, 0)
";
        const MUTUAL_TCO_WILDCARD_PARAM: &str = "\
fn isEven(n: Int, _: Int) -> Bool
    match n == 0
        true -> true
        false -> isOdd(n - 1, 0)

fn isOdd(n: Int, _: Int) -> Bool
    match n == 0
        true -> false
        false -> isEven(n - 1, 0)
";
        // Recursive, but the call is not in tail position, so no loop and
        // no trampoline is built: the parameter stays a plain pattern.
        const NON_TAIL_RECURSIVE_WILDCARD_PARAM: &str = "\
fn deep(n: Int, _: Int) -> Int
    match n == 0
        true -> 0
        false -> deep(n - 1, 0) + 1
";
        // Not recursive at all — but the parameter is a COLLECTION, and
        // that is the other thing that can put a `mut` in front of it.
        // `own_param` proves a `Vector` parameter uniquely owned when every
        // call site passes a fresh value, and the owned spelling is `mut p:
        // T`. The proof is by parameter POSITION and never reads the name,
        // so it reaches the wildcard too.
        const OWNED_COLLECTION_WILDCARD_PARAM: &str = "\
fn firstOr(v: Vector<Int>, _: Vector<Int>) -> Int
    Option.withDefault(Vector.get(v, 0), 0)

fn caller() -> Int
    firstOr(Vector.new(5, 7), Vector.new(3, 1))
";

        for body in [
            "fn _(n: Int) -> Int\n    n + 1\n",
            "record Holder\n  _: Int\n\nfn read(h: Holder) -> Int\n    h._\n",
            SELF_TCO_WILDCARD_PARAM,
            MUTUAL_TCO_WILDCARD_PARAM,
            NON_TAIL_RECURSIVE_WILDCARD_PARAM,
            OWNED_COLLECTION_WILDCARD_PARAM,
        ] {
            let mut ctx = ctx_from_source(&format!("module Demo\n\n{body}"), "demo");
            let out = transpile(&mut ctx);
            let entry = generated_rust_entry_file(&out);
            assert!(
                entry.contains("_avr__"),
                "the wildcard was not renamed:\n{body}\n{entry}"
            );
            assert!(
                !entry.contains("mut _:") && !entry.contains("mut _,") && !entry.contains("mut _)"),
                "`mut _` is ``error: `mut` must be followed by a named \
                 binding``:\n{body}\n{entry}"
            );
        }

        // A `_` BINDING is the one position with no name to spell: nothing
        // can read it, so the emitter drops the statement outright. Pinned
        // so the loop above is not quietly asked the wrong question if that
        // ever changes.
        let mut ctx = ctx_from_source(
            "module Demo\n\nfn binds(n: Int) -> Int\n    _ = n + 1\n    9\n",
            "demo",
        );
        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);
        assert!(
            !entry.contains("let _"),
            "an unread `_` binding should not be emitted at all:\n{entry}"
        );
    }

    /// A verify block's generated test is named `test_<fn>_case_<n>`, and
    /// the function name goes in the MIDDLE of that identifier. Escaping it
    /// first produced `fn test_r#await_case_1()`, where the `#` ends the
    /// identifier — ``error: prefix `test_r` is unknown`` under `cargo
    /// test`. A name embedded inside a longer identifier never needs the
    /// escape, so the test name is composed from the raw Aver name.
    #[test]
    fn verify_test_names_are_composed_from_the_raw_fn_name() {
        let out = transpile(&mut ctx_from_source(
            r#"
module Demo

fn await(n: Int) -> Int
    n + 1

fn become(n: Int) -> Int
    n + 2

verify await
    await(1) => 2

verify become
    become(1) => 3
"#,
            "demo",
        ));
        let verify = generated_file(&out, "src/verify.rs");

        let test_fns: Vec<&str> = verify
            .lines()
            .map(str::trim)
            .filter(|line| line.starts_with("fn "))
            .collect();
        assert!(
            !test_fns.is_empty(),
            "expected the verify module to declare test fns:\n{verify}"
        );
        for line in &test_fns {
            assert!(
                !line.contains('#'),
                "a generated test fn name carries a `#`, so the identifier ends \
                 there and the module does not parse: {line}"
            );
        }
        for wanted in ["fn test_await_case_1()", "fn test_become_case_1()"] {
            assert!(
                verify.contains(wanted),
                "expected `{wanted}` in the generated verify module:\n{verify}"
            );
        }
    }

    /// The trampoline variant is the function name capitalised, so a
    /// function named `self` in a mutual-recursion group produces the
    /// variant `Self` — a Rust keyword, and one with no raw spelling, so an
    /// enum carrying it would not parse.
    ///
    /// Capitalising FIRST and spelling the result afterwards is what
    /// answers this with no special case: `Self` is one of the five names
    /// with no spelling, so the variant is renamed exactly like any other,
    /// and the fn name (`_avr_self`) and the variant (`_avr_Self`) stay
    /// distinct.
    #[test]
    fn mutual_tco_fn_named_self_builds_a_renamed_variant() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn self(n: Int) -> Int
    match n == 0
        true -> 0
        false -> other(n - 1)

fn other(n: Int) -> Int
    match n == 0
        true -> 1
        false -> self(n - 1)
"#,
            "demo",
        );
        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("_avr_Self"),
            "the trampoline variant was not renamed:\n{entry}"
        );
        assert!(
            entry.contains("pub fn _avr_self"),
            "the function name was not renamed:\n{entry}"
        );
        assert!(
            !entry.contains("r#Self") && !entry.contains("R#"),
            "a variant with no raw spelling was escaped instead of renamed:\n{entry}"
        );
    }

    /// The same collapse, reached by a name that is NOT itself a Rust word.
    /// `ſ` (U+017F LATIN SMALL LETTER LONG S) upper-cases to `S`, so `ſelf`
    /// capitalises to `Self` — comparing the FUNCTION name against the
    /// unspellable list would not catch this, because `ſelf` is not on it.
    /// Asking the question after capitalising is what catches it, and it
    /// lands on the same variant `fn self` does.
    ///
    /// Aver accepts the name (camelCase style warning only) and it is a
    /// perfectly good Rust identifier on its own, so without a trampoline
    /// there is no variant and the name is emitted verbatim — the rename
    /// reaches the variant, not the function.
    #[test]
    fn a_name_that_capitalises_onto_self_is_renamed_only_in_its_variant() {
        let mut mutual = ctx_from_source(
            "
module Demo

fn ſelf(n: Int) -> Int
    match n == 0
        true -> 0
        false -> other(n - 1)

fn other(n: Int) -> Int
    match n == 0
        true -> 1
        false -> ſelf(n - 1)
",
            "demo",
        );
        let out = transpile(&mut mutual);
        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("_avr_Self"),
            "a variant that collapses onto `Self` was not renamed:\n{entry}"
        );
        assert!(
            entry.contains("ſelf"),
            "the function name itself is spellable and must be kept:\n{entry}"
        );

        // Without the trampoline there is no variant, and `ſelf` is a valid
        // Rust function name, so nothing is renamed at all.
        let mut alone = ctx_from_source(
            "
module Demo

fn ſelf(n: Int) -> Int
    n + 1
",
            "demo",
        );
        let out = transpile(&mut alone);
        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("pub fn ſelf"),
            "the name should be emitted as-is:\n{entry}"
        );
        assert!(
            !entry.contains("_avr_"),
            "a spellable name must not be renamed:\n{entry}"
        );
    }

    /// The direct contract of the trampoline variant helper, independent of
    /// any program that happens to drive it: capitalise, then spell.
    #[test]
    fn fn_name_to_variant_capitalises_then_spells() {
        use super::toplevel::fn_name_to_variant;

        // Ordinary names are untouched apart from the case change, so
        // already-generated trampolines keep the variants they had.
        assert_eq!(fn_name_to_variant("isOdd"), "IsOdd");
        assert_eq!(fn_name_to_variant("ping"), "Ping");

        // Rust keywords capitalise out of keyword-hood; the escape must not
        // run afterwards, because `R#await` is not an identifier.
        for (name, variant) in [
            ("await", "Await"),
            ("move", "Move"),
            ("impl", "Impl"),
            ("become", "Become"),
            ("try", "Try"),
        ] {
            assert_eq!(fn_name_to_variant(name), variant);
            assert!(!fn_name_to_variant(name).contains('#'));
        }

        // The three names that capitalise onto `Self` — the one keyword
        // with no raw spelling — all land on the renamed variant, and the
        // rename is what the enum carries.
        for name in ["self", "Self", "ſelf"] {
            assert_eq!(fn_name_to_variant(name), "_avr_Self");
        }
        assert_eq!(fn_name_to_variant("_"), "_avr__");

        // Names whose first character has no uppercase form come through
        // unchanged — and are still not keywords.
        assert_eq!(fn_name_to_variant("_await"), "_await");
        assert_eq!(fn_name_to_variant("日本語"), "日本語");
        assert_eq!(fn_name_to_variant(""), "");
    }

    /// A keyword-named function in a mutual group that also carries a
    /// by-reference parameter: the trampoline splits its parameters, passing
    /// the ref-counted ones alongside the enum rather than inside it, and
    /// spells both the variant and the borrowed parameter name. Both
    /// spellings have to survive the escape.
    #[test]
    fn mutual_tco_keyword_fn_with_borrowed_param_spells_both_names() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn await(unsafe: String, n: Int) -> Int
    match n == 0
        true -> String.len(unsafe)
        false -> resume(unsafe, n - 1)

fn resume(unsafe: String, n: Int) -> Int
    match n == 0
        true -> 0
        false -> await(unsafe, n - 1)
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(
            entry.contains("enum __MutualTco1"),
            "no trampoline:\n{entry}"
        );
        assert!(entry.contains("Await"), "missing variant:\n{entry}");
        assert!(
            entry.contains("r#unsafe"),
            "keyword-named param lost its escape:\n{entry}"
        );
        assert!(!entry.contains("R#"), "capitalised escape prefix:\n{entry}");
    }

    /// A keyword-named function defined in a DEP module and called across
    /// the module boundary: the call site spells the escaped name behind a
    /// module path, which is a different emitter arm from the same-module
    /// call. This is the shape the original report actually had — the module
    /// had been uncompilable since it was written and only surfaced when a
    /// second module started depending on it.
    ///
    /// The dep module's own trampoline is NOT asserted here: `ctx_from_multi`
    /// runs the TCO stage over the entry module only, so a dep-module mutual
    /// group never forms in this harness even though it does in production.
    /// `rust_dep_module_keyword_mutual_recursion_builds_and_matches_vm`
    /// (`tests/rust_codegen_differential.rs`) covers that through a real
    /// two-file `aver compile` and `cargo build`.
    #[test]
    fn keyword_named_fn_in_a_dep_module_is_called_with_the_escaped_name() {
        let mut ctx = ctx_from_multi(
            r#"
module Entry
    depends [Worker]
    intent = "Calls a keyword-named mutual pair across a module boundary."
    effects []

fn main() -> Int
    Worker.await(4)
"#,
            &[(
                "Worker",
                r#"
module Worker
    exposes [await]
    intent = "A keyword-named mutual pair."
    effects []

fn await(n: Int) -> Int
    match n == 0
        true -> 0
        false -> resume(n - 1)

fn resume(n: Int) -> Int
    match n == 0
        true -> 1
        false -> await(n - 1)
"#,
            )],
            "dep_keyword_mutual",
        );

        let out = transpile(&mut ctx);
        let worker = generated_file(&out, "src/aver_generated/worker/mod.rs");

        assert!(
            worker.contains("pub fn r#await"),
            "the dep module's definition keeps the escape:\n{worker}"
        );
        assert!(
            !worker.contains("R#"),
            "capitalised escape prefix:\n{worker}"
        );

        // The cross-module call site spells the escaped name behind the path.
        let entry = generated_rust_entry_file(&out);
        assert!(
            entry.contains("worker::r#await"),
            "cross-module call must use the escaped name:\n{entry}"
        );
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

        assert!(entry.contains(
            "(idx).to_usize().and_then(|__i| grid.get(__i).cloned()).unwrap_or(aver_rt::AverInt::from_i64(0))"
        ));
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

        assert!(entry.contains("if (n < aver_rt::AverInt::from_i64(10)) { aver_rt::AverInt::from_i64(3) } else { aver_rt::AverInt::from_i64(7) }"));
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
        assert!(entry.contains("if (n < aver_rt::AverInt::from_i64(10)) { aver_rt::AverInt::from_i64(3) } else { aver_rt::AverInt::from_i64(7) }"));
    }

    #[test]
    fn wave2_generic_user_sum_match_emits_native_rust_match() {
        // rust-on-MIR Wave 2: a generic match over a user sum type (no
        // bool / list / dispatch-table shortcut) graduates through
        // `emit_mir_match`. The production output is byte-identical to
        // the HIR walker by the parity gate, so asserting the shape here
        // pins the graduated emit. `Shape.Circle(r)` / `Shape.Square(s)`
        // → a native Rust `match` with `Demo_Shape::Circle(r) => { … }`
        // arms.
        let mut ctx = ctx_from_source(
            r#"
module Demo

type Shape
    Circle(Float)
    Square(Float)

fn area(sh: Shape) -> Float
    match sh
        Shape.Circle(r) -> r * r * 3.14
        Shape.Square(s) -> s * s
"#,
            "demo",
        );

        let out = transpile(&mut ctx);
        let entry = generated_rust_entry_file(&out);

        assert!(
            entry.contains("match sh.clone() {"),
            "generic user-sum match should emit a native Rust match on the cloned subject: {entry}"
        );
        assert!(entry.contains("Shape::Circle(r) =>"));
        assert!(entry.contains("Shape::Square(s) =>"));
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

        // Uses native Rust match directly. `r` is the match subject's
        // last use, so the `last_use` pass (now run by `ctx_from_source`
        // via the full pipeline, matching the CLI) lets it move into the
        // match without a `.clone()`.
        assert!(entry.contains("match r {"));
        assert!(entry.contains("Ok(n @ _)"));
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

        // Uses native Rust match directly. `r` is the match subject's
        // last use, so the `last_use` pass (now run by `ctx_from_source`
        // via the full pipeline, matching the CLI) lets it move into the
        // match without a `.clone()`.
        assert!(entry.contains("match r {"));
        assert!(entry.contains("Ok(n @ _)"));
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
    fn runtime_policy_configures_the_generated_tcp_provider() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn main() -> Result<Unit, String>
    ! [Tcp.ping]
    Tcp.ping("127.0.0.1", 1)
"#,
            "demo",
        );
        ctx.emit_replay_runtime = true;
        ctx.runtime_policy_from_env = true;

        let out = transpile(&mut ctx);
        let provider_support = generated_file(&out, "src/provider_support.rs");
        let replay_support = generated_file(&out, "src/replay_support.rs");

        assert!(provider_support.contains("crate::aver_replay::tcp_provider_settings_from_env()?"));
        assert!(provider_support.contains("StandardTcpProvider::new(standard_tcp_settings)"));
        assert!(replay_support.contains("pub(crate) fn tcp_provider_settings_from_env()"));
        assert!(replay_support.contains("validate_effect_section_keys(name, section)"));
    }

    #[test]
    fn embedded_policy_configures_the_generated_tcp_provider() {
        let mut ctx = ctx_from_source(
            r#"
module Demo

fn main() -> Result<Unit, String>
    ! [Tcp.ping]
    Tcp.ping("127.0.0.1", 1)
"#,
            "demo",
        );
        ctx.policy = Some(
            crate::config::ProjectConfig::parse(
                "[effects.Tcp]\nconnect_timeout_secs = 7\nrequest_idle_timeout_secs = 45\n",
            )
            .expect("Tcp policy"),
        );

        let out = transpile(&mut ctx);
        let provider_support = generated_file(&out, "src/provider_support.rs");

        assert!(provider_support.contains("TcpSettings::from_secs(7, 45)?"));
        assert!(provider_support.contains("StandardTcpProvider::new(standard_tcp_settings)"));
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
            tcp_settings: crate::config::TcpEffectSettings::default(),
            tcp_settings_configured: false,
            check_suppressions: Vec::new(),
            verify: crate::config::VerifySettings::default(),
            independence_mode: crate::config::IndependenceMode::default(),
            shape_layers: Vec::new(),
            shape_expected: Vec::new(),
            provider_manifest: None,
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
            tcp_settings: crate::config::TcpEffectSettings::default(),
            tcp_settings_configured: false,
            check_suppressions: Vec::new(),
            verify: crate::config::VerifySettings::default(),
            independence_mode: crate::config::IndependenceMode::Cancel,
            shape_layers: Vec::new(),
            shape_expected: Vec::new(),
            provider_manifest: None,
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

        // Disk is a provider-backed standard capability now, so the call
        // goes through the typed provider door — but the cancel
        // checkpoint must still fire before the host boundary, exactly
        // like the builtin table always did.
        assert!(entry.contains("crate::cancel_checkpoint();"));
        assert!(entry.contains("crate::provider_support::invoke"));
        assert!(!entry.contains("aver_rt::read_text"));
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
