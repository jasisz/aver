//! `aver run --wasm-gc` — embedded wasmtime host for the wasm-gc backend.
//!
//! Mirrors the legacy `cmd_run_wasm` shape but configured for engine
//! GC + tail calls. Effect imports are wired against native Rust
//! implementations (Console.print/error/warn cross via the
//! `__rt_string_*` LM transport bridge that every emitted module
//! exports). Imports we don't have a real impl for get an auto-stub
//! that returns the zero value of the declared result type — so a
//! program that *declares* an effect but doesn't run it through code
//! still instantiates cleanly.

use colored::Colorize;
use std::process;

use super::shared;
use super::shared::{parse_file, read_file, resolve_module_root};

#[cfg(feature = "wasm")]
use super::commands::{flatten_multimodule, load_compile_deps};

pub(super) fn cmd_run_wasm_gc(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
) {
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (file, module_root_override, program_args);
        eprintln!("{}", "WASM requires --features wasm".red());
        process::exit(1);
    }

    #[cfg(feature = "wasm")]
    {
        use aver::codegen::wasm_gc;
        use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

        let module_root = resolve_module_root(module_root_override);
        let source = match read_file(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };
        let mut items = match parse_file(&source) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };
        let neutral_policy = NeutralAllocPolicy;
        let result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full {
                    base_dir: Some(&module_root),
                }),
                alloc_policy: Some(&neutral_policy),
                run_interp_lower: false,
                run_buffer_build: false,
                ..Default::default()
            },
        );
        if let Some(tc) = &result.typecheck
            && !tc.errors.is_empty()
        {
            eprintln!("{}", shared::format_type_errors(&tc.errors).red());
            process::exit(1);
        }
        let dep_modules = load_compile_deps(&items, &module_root, false, false);
        flatten_multimodule(&mut items, &dep_modules);

        let bytes = match wasm_gc::compile_to_wasm_gc(&items, result.analysis.as_ref()) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("{}", format!("{e}").red());
                process::exit(1);
            }
        };

        if let Err(e) = run_wasm_gc_with_host(&bytes, &program_args) {
            eprintln!("{}", format!("WASM execution error: {}", e).red());
            process::exit(1);
        }
    }
}

#[cfg(feature = "wasm")]
struct RunWasmGcHost {
    program_args: Vec<String>,
}

#[cfg(feature = "wasm")]
fn run_wasm_gc_with_host(wasm_bytes: &[u8], program_args: &[String]) -> Result<(), String> {
    use wasmtime::*;

    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(OptLevel::Speed);
    config.max_wasm_stack(8 * 1024 * 1024);
    let engine = Engine::new(&config).map_err(|e| format!("engine: {e:#}"))?;
    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("module: {e:#}"))?;

    let mut store = Store::new(
        &engine,
        RunWasmGcHost {
            program_args: program_args.to_vec(),
        },
    );
    let mut linker: Linker<RunWasmGcHost> = Linker::new(&engine);
    linker.allow_shadowing(true);

    // Auto-stub every import — return the zero value for each declared
    // result type. Real impls below (Console.*, Args.*, Random, Time,
    // Float math) shadow the stubs for effects we actually support.
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let result_tys: Vec<ValType> = ft.results().collect();
        let func_ty = FuncType::new(&engine, ft.params(), ft.results());
        let _ = linker.func_new(
            &module_name,
            &field_name,
            func_ty,
            move |_caller, _params, results| {
                for (slot, ty) in results.iter_mut().zip(result_tys.iter()) {
                    *slot = match ty {
                        ValType::I32 => Val::I32(0),
                        ValType::I64 => Val::I64(0),
                        ValType::F32 => Val::F32(0),
                        ValType::F64 => Val::F64(0),
                        ValType::V128 => Val::V128(0u128.into()),
                        ValType::Ref(_) => Val::AnyRef(None),
                    };
                }
                Ok(())
            },
        );
    }

    linker
        .func_wrap(
            "aver",
            "args_len",
            |caller: Caller<'_, RunWasmGcHost>| -> i64 { caller.data().program_args.len() as i64 },
        )
        .map_err(|e| format!("link args_len: {e:#}"))?;

    linker
        .func_wrap("aver", "random_int", |min: i64, max: i64| -> i64 {
            use std::collections::hash_map::RandomState;
            use std::hash::{BuildHasher, Hasher};
            let s = RandomState::new();
            let mut h = s.build_hasher();
            h.write_u64(min as u64 ^ max as u64);
            let range = (max - min + 1) as u64;
            if range == 0 {
                return min;
            }
            min + (h.finish() % range) as i64
        })
        .map_err(|e| format!("link random_int: {e:#}"))?;

    linker
        .func_wrap("aver", "random_float", || -> f64 {
            use std::collections::hash_map::RandomState;
            use std::hash::{BuildHasher, Hasher};
            let s = RandomState::new();
            let mut h = s.build_hasher();
            h.write_u64(0xdeadbeef_cafef00d);
            (h.finish() as f64) / (u64::MAX as f64)
        })
        .map_err(|e| format!("link random_float: {e:#}"))?;

    linker
        .func_wrap("aver", "time_unix_ms", || -> i64 {
            use std::time::{SystemTime, UNIX_EPOCH};
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as i64)
                .unwrap_or(0)
        })
        .map_err(|e| format!("link time_unix_ms: {e:#}"))?;

    linker
        .func_wrap("aver", "time_sleep", |millis: i64| {
            std::thread::sleep(std::time::Duration::from_millis(millis.max(0) as u64));
        })
        .map_err(|e| format!("link time_sleep: {e:#}"))?;

    linker
        .func_wrap("aver", "float_sin", |x: f64| -> f64 { x.sin() })
        .map_err(|e| format!("link float_sin: {e:#}"))?;
    linker
        .func_wrap("aver", "float_cos", |x: f64| -> f64 { x.cos() })
        .map_err(|e| format!("link float_cos: {e:#}"))?;
    linker
        .func_wrap("aver", "float_atan2", |y: f64, x: f64| -> f64 { y.atan2(x) })
        .map_err(|e| format!("link float_atan2: {e:#}"))?;
    linker
        .func_wrap("aver", "float_pow", |b: f64, e: f64| -> f64 { b.powf(e) })
        .map_err(|e| format!("link float_pow: {e:#}"))?;

    register_aver_string_consumer(&mut linker, "console_print", true)?;
    register_aver_string_consumer(&mut linker, "console_error", false)?;
    register_aver_string_consumer(&mut linker, "console_warn", false)?;

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("instantiate: {e:#}"))?;

    if let Some(start) = instance.get_func(&mut store, "_start") {
        start
            .call(&mut store, &[], &mut [])
            .map_err(|e| format!("_start trap: {e:#}"))?;
    } else if let Some(main) = instance.get_func(&mut store, "main") {
        let n = main.ty(&store).results().len();
        let mut out: Vec<Val> = (0..n).map(|_| Val::I32(0)).collect();
        main.call(&mut store, &[], &mut out)
            .map_err(|e| format!("main trap: {e:#}"))?;
    } else {
        return Err("module exports neither _start nor main".into());
    }
    Ok(())
}

#[cfg(feature = "wasm")]
fn register_aver_string_consumer(
    linker: &mut wasmtime::Linker<RunWasmGcHost>,
    name: &'static str,
    to_stdout: bool,
) -> Result<(), String> {
    use wasmtime::*;
    // Signature: param `(ref null any)`, no result. The host pulls the
    // LM-transport bridge and reads bytes back. Engine subtyping turns
    // any concrete `(ref null $string)` the wasm side passes us into a
    // valid AnyRef value.
    let string_ref_ty = ValType::Ref(RefType::new(true, HeapType::Any));
    let func_ty = FuncType::new(&linker.engine().clone(), vec![string_ref_ty], vec![]);
    linker
        .func_new(
            "aver",
            name,
            func_ty,
            move |mut caller: Caller<'_, RunWasmGcHost>,
                  params: &[Val],
                  _results: &mut [Val]|
                  -> Result<(), wasmtime::Error> {
                let any_ref = match &params[0] {
                    Val::AnyRef(r) => r.clone(),
                    _ => return Ok(()),
                };
                let Some(any_ref) = any_ref else { return Ok(()) };
                let to_lm = caller
                    .get_export("__rt_string_to_lm")
                    .and_then(|e| e.into_func());
                let memory = caller.get_export("memory").and_then(|e| e.into_memory());
                if let (Some(to_lm), Some(memory)) = (to_lm, memory) {
                    let mut out = [Val::I32(0)];
                    if to_lm
                        .call(&mut caller, &[Val::AnyRef(Some(any_ref))], &mut out)
                        .is_ok()
                    {
                        let len = match out[0] {
                            Val::I32(n) => n,
                            _ => 0,
                        };
                        if len > 0 {
                            let mut buf = vec![0u8; len as usize];
                            let _ = memory.read(&caller, 0, &mut buf);
                            // Match VM semantics — Console.print appends a
                            // newline, see `src/services/console.rs`.
                            let text = String::from_utf8_lossy(&buf);
                            if to_stdout {
                                println!("{}", text);
                            } else {
                                eprintln!("{}", text);
                            }
                        }
                    }
                }
                Ok(())
            },
        )
        .map_err(|e| format!("link {name}: {e:#}"))?;
    Ok(())
}
