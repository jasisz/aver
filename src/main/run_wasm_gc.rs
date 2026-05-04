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

    // One walk over imports — for every `(module, name)` declared by
    // the wasm module, register a host fn that uses the import's own
    // FuncType (so engine-side type identity matches without manual
    // sub-typing) and dispatches per name. Defaults to a typed-zero
    // stub when we don't have a real impl. Programs that declare an
    // effect but never call it instantiate cleanly; programs that do
    // call it get real semantics.
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let result_tys: Vec<ValType> = ft.results().collect();
        let func_ty = FuncType::new(&engine, ft.params(), ft.results());
        let module_name_for_closure = module_name.clone();
        let field_name_for_closure = field_name.clone();
        linker
            .func_new(
                &module_name,
                &field_name,
                func_ty,
                move |mut caller: Caller<'_, RunWasmGcHost>,
                      params: &[Val],
                      results: &mut [Val]|
                      -> Result<(), wasmtime::Error> {
                    if module_name_for_closure == "aver"
                        && dispatch_aver_import(
                            &field_name_for_closure,
                            &mut caller,
                            params,
                            results,
                        )?
                    {
                        return Ok(());
                    }
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
            )
            .map_err(|e| format!("link {module_name}.{field_name}: {e:#}"))?;
    }

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

/// Per-effect-name dispatch — returns true if we provided a real
/// implementation, false if the caller should fall back to the
/// typed-zero stub.
#[cfg(feature = "wasm")]
fn dispatch_aver_import(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "args_len" => {
            results[0] = Val::I64(caller.data().program_args.len() as i64);
            Ok(true)
        }
        "args_get" => {
            let idx = match params[0] {
                Val::I64(n) => n,
                _ => 0,
            };
            let text = caller
                .data()
                .program_args
                .get(idx.max(0) as usize)
                .cloned()
                .unwrap_or_default();
            let r = lm_string_from_host(caller, &text)?;
            results[0] = Val::AnyRef(r);
            Ok(true)
        }
        "console_print" => host_print(caller, params, true).map(|()| true),
        "console_error" | "console_warn" => host_print(caller, params, false).map(|()| true),
        "console_read_line" => {
            // Read one line from stdin. EOF / IO error → Result.Err("EOF").
            // Trailing '\n' / '\r\n' is stripped to match VM semantics.
            use std::io::BufRead;
            let mut line = String::new();
            let read = std::io::stdin().lock().read_line(&mut line);
            let result_ref = match read {
                Ok(0) | Err(_) => host_result_err_string(caller, "EOF")?,
                Ok(_) => {
                    while line.ends_with('\n') || line.ends_with('\r') {
                        line.pop();
                    }
                    host_result_ok_string(caller, &line)?
                }
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "time_now" => {
            let text = aver_rt::time_now();
            let r = lm_string_from_host(caller, &text)?;
            results[0] = Val::AnyRef(r);
            Ok(true)
        }
        "time_unix_ms" => {
            use std::time::{SystemTime, UNIX_EPOCH};
            let ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as i64)
                .unwrap_or(0);
            results[0] = Val::I64(ms);
            Ok(true)
        }
        "time_sleep" => {
            if let Some(Val::I64(ms)) = params.first() {
                std::thread::sleep(std::time::Duration::from_millis(ms.max(&0).unsigned_abs()));
            }
            Ok(true)
        }
        "random_int" => {
            let (min, max) = match (params.first(), params.get(1)) {
                (Some(Val::I64(a)), Some(Val::I64(b))) => (*a, *b),
                _ => (0, 0),
            };
            // aver_rt::random_int returns Result, but the wasm import
            // contract is a plain i64. The host falls back to `min` on
            // an inverted range — same surface the VM exposes.
            let v = aver_rt::random::random_int(min, max).unwrap_or(min);
            results[0] = Val::I64(v);
            Ok(true)
        }
        "random_float" => {
            results[0] = Val::F64(aver_rt::random::random_float().to_bits());
            Ok(true)
        }
        "float_sin" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).sin().to_bits());
            }
            Ok(true)
        }
        "float_cos" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).cos().to_bits());
            }
            Ok(true)
        }
        "float_atan2" => {
            if let (Some(Val::F64(y)), Some(Val::F64(x))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*y).atan2(f64::from_bits(*x)).to_bits());
            }
            Ok(true)
        }
        "float_pow" => {
            if let (Some(Val::F64(b)), Some(Val::F64(e))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*b).powf(f64::from_bits(*e)).to_bits());
            }
            Ok(true)
        }
        "terminal_enable_raw_mode" => {
            let _ = aver_rt::terminal_enable_raw_mode();
            Ok(true)
        }
        "terminal_disable_raw_mode" => {
            let _ = aver_rt::terminal_disable_raw_mode();
            Ok(true)
        }
        "terminal_clear" => {
            let _ = aver_rt::terminal_clear();
            Ok(true)
        }
        "terminal_move_to" => {
            let x = params.first().and_then(val_i64).unwrap_or(0);
            let y = params.get(1).and_then(val_i64).unwrap_or(0);
            let _ = aver_rt::terminal_move_to(x, y);
            Ok(true)
        }
        "terminal_print" => {
            // Same shape as console_print (any_ref payload through the
            // LM bridge), but writes via aver_rt::terminal_print so it
            // respects raw-mode without injecting a trailing newline.
            let text = lm_string_to_host(caller, params.first())?;
            if let Some(t) = text {
                let _ = aver_rt::terminal_print(&t);
            }
            Ok(true)
        }
        "terminal_set_color" => {
            let text = lm_string_to_host(caller, params.first())?;
            if let Some(t) = text {
                let _ = aver_rt::terminal_set_color(&t);
            }
            Ok(true)
        }
        "terminal_reset_color" => {
            let _ = aver_rt::terminal_reset_color();
            Ok(true)
        }
        "terminal_hide_cursor" => {
            let _ = aver_rt::terminal_hide_cursor();
            Ok(true)
        }
        "terminal_show_cursor" => {
            let _ = aver_rt::terminal_show_cursor();
            Ok(true)
        }
        "terminal_flush" => {
            let _ = aver_rt::terminal_flush();
            Ok(true)
        }
        "terminal_read_key" => {
            let key = aver_rt::terminal_read_key();
            let opt_ref = match key {
                Some(text) => host_option_string_some(caller, &text)?,
                None => host_option_string_none(caller)?,
            };
            results[0] = Val::AnyRef(opt_ref);
            Ok(true)
        }
        "terminal_size" => {
            let (w, h) = aver_rt::terminal_size().unwrap_or((80, 24));
            let rec_ref = host_terminal_size_make(caller, w, h)?;
            results[0] = Val::AnyRef(rec_ref);
            Ok(true)
        }
        "disk_read_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let result_ref = match aver_rt::read_text(&path) {
                Ok(text) => host_result_ok_string(caller, &text)?,
                Err(e) => host_result_err_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_write_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let content = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let result_ref = match aver_rt::write_text(&path, &content) {
                Ok(()) => host_result_ok_unit(caller)?,
                Err(e) => host_result_err_unit_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_append_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let content = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let result_ref = match aver_rt::append_text(&path, &content) {
                Ok(()) => host_result_ok_unit(caller)?,
                Err(e) => host_result_err_unit_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_exists" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            results[0] = Val::I32(if aver_rt::path_exists(&path) { 1 } else { 0 });
            Ok(true)
        }
        "disk_delete" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let result_ref = match aver_rt::delete_file(&path) {
                Ok(()) => host_result_ok_unit(caller)?,
                Err(e) => host_result_err_unit_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_delete_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let result_ref = match aver_rt::delete_dir(&path) {
                Ok(()) => host_result_ok_unit(caller)?,
                Err(e) => host_result_err_unit_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_list_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let result_ref = match aver_rt::list_dir(&path) {
                Ok(list) => {
                    let names: Vec<String> = list.iter().cloned().collect();
                    host_result_ok_list_string(caller, &names)?
                }
                Err(e) => host_result_err_list_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        "disk_make_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let result_ref = match aver_rt::make_dir(&path) {
                Ok(()) => host_result_ok_unit(caller)?,
                Err(e) => host_result_err_unit_string(caller, &e)?,
            };
            results[0] = Val::AnyRef(result_ref);
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Decode an `i64` param without panicking if it's a different val kind.
#[cfg(feature = "wasm")]
fn val_i64(v: &wasmtime::Val) -> Option<i64> {
    match v {
        wasmtime::Val::I64(n) => Some(*n),
        _ => None,
    }
}

/// Print one Aver string to stdout/stderr via the LM transport bridge.
#[cfg(feature = "wasm")]
fn host_print(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    to_stdout: bool,
) -> Result<(), wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match params.first() {
        Some(Val::AnyRef(r)) => r.clone(),
        _ => return Ok(()),
    };
    let Some(any_ref) = any_ref else { return Ok(()) };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(());
    };
    let mut out = [Val::I32(0)];
    if to_lm
        .call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)
        .is_ok()
    {
        let len = match out[0] {
            Val::I32(n) => n,
            _ => 0,
        };
        if len > 0 {
            let mut buf = vec![0u8; len as usize];
            let _ = memory.read(&caller, 0, &mut buf);
            let text = String::from_utf8_lossy(&buf);
            if to_stdout {
                println!("{}", text);
            } else {
                eprintln!("{}", text);
            }
        } else {
            // Empty string still emits a newline in VM semantics.
            if to_stdout {
                println!();
            } else {
                eprintln!();
            }
        }
    }
    Ok(())
}

/// Decode an Aver String ref (passed as `any_ref` in the import ABI)
/// back to a Rust `String` via the LM transport bridge.
#[cfg(feature = "wasm")]
fn lm_string_to_host(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<String>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => r.clone(),
        _ => return Ok(None),
    };
    let Some(any_ref) = any_ref else {
        return Ok(None);
    };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(None);
    };
    let mut out = [Val::I32(0)];
    to_lm.call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)?;
    let len = match out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };
    let mut buf = vec![0u8; len];
    if len > 0 {
        memory.read(&caller, 0, &mut buf)?;
    }
    Ok(Some(String::from_utf8_lossy(&buf).into_owned()))
}

/// Build an `Option<String>::Some(text)` ref via the
/// `__rt_option_string_some` factory.
#[cfg(feature = "wasm")]
fn host_option_string_some(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_option_string_some")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Build an `Option<String>::None` ref via `__rt_option_string_none`.
#[cfg(feature = "wasm")]
fn host_option_string_none(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_option_string_none")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Build a `Terminal.Size(width, height)` record via the
/// `__rt_record_terminal_size_make` factory.
#[cfg(feature = "wasm")]
fn host_terminal_size_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    width: i64,
    height: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_terminal_size_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(
        &mut *caller,
        &[Val::I64(width), Val::I64(height)],
        &mut out,
    )?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// `Result<Unit, String>::Ok(())` via the matching factory export.
#[cfg(feature = "wasm")]
fn host_result_ok_unit(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_unit_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

#[cfg(feature = "wasm")]
fn host_result_err_unit_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_unit_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Build a `Result<List<String>, String>::Ok(list)` ref. The list is
/// constructed bottom-up via repeated `__rt_list_string_cons` calls,
/// terminated by `__rt_list_string_nil`.
#[cfg(feature = "wasm")]
fn host_result_ok_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    items: &[String],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let nil = caller
        .get_export("__rt_list_string_nil")
        .and_then(|e| e.into_func());
    let cons = caller
        .get_export("__rt_list_string_cons")
        .and_then(|e| e.into_func());
    let factory = caller
        .get_export("__rt_result_list_string_string_ok")
        .and_then(|e| e.into_func());
    let (Some(nil), Some(cons), Some(factory)) = (nil, cons, factory) else {
        return Ok(None);
    };
    let mut tail_out = [Val::AnyRef(None)];
    nil.call(&mut *caller, &[], &mut tail_out)?;
    let mut current = match &tail_out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    };
    // Cons in reverse so the resulting list keeps the input order.
    for text in items.iter().rev() {
        let head = match lm_string_from_host(caller, text)? {
            Some(r) => r,
            None => return Ok(None),
        };
        let mut next = [Val::AnyRef(None)];
        cons.call(
            &mut *caller,
            &[Val::AnyRef(Some(head)), Val::AnyRef(current.clone())],
            &mut next,
        )?;
        current = match &next[0] {
            Val::AnyRef(r) => r.clone(),
            _ => None,
        };
    }
    let mut wrapped = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(current)], &mut wrapped)?;
    Ok(match &wrapped[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

#[cfg(feature = "wasm")]
fn host_result_err_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_list_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Build a `Result<String,String>::Ok(text)` ref by calling the
/// module's exported factory `__rt_result_string_string_ok`. Returns
/// `Ok(None)` if the factory or string bridge isn't exported (the
/// program declared `Console.readLine` but didn't reach the type
/// registration that materialises the factory).
#[cfg(feature = "wasm")]
fn host_result_ok_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Build a `Result<String,String>::Err(text)` ref via the symmetric
/// `__rt_result_string_string_err` factory.
#[cfg(feature = "wasm")]
fn host_result_err_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    })
}

/// Materialise a host-supplied UTF-8 string as an Aver string ref via
/// the LM transport bridge: write bytes into linear memory at offset 0,
/// call `__rt_string_from_lm(len)` to wrap them in an `(array i8)`.
/// Returns the resulting AnyRef so the caller can hand it back as a
/// host import result.
#[cfg(feature = "wasm")]
fn lm_string_from_host<T: 'static>(
    caller: &mut wasmtime::Caller<'_, T>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::*;
    let bytes = text.as_bytes();
    let from_lm = caller
        .get_export("__rt_string_from_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let grow = caller
        .get_export("__rt_memory_grow")
        .and_then(|e| e.into_func());
    let pages = caller
        .get_export("__rt_memory_pages")
        .and_then(|e| e.into_func());
    let (Some(from_lm), Some(memory), Some(grow), Some(pages)) = (from_lm, memory, grow, pages)
    else {
        return Ok(None);
    };
    // Grow LM if the string doesn't fit. One page = 64 KiB; small
    // strings fit by default since the bridge ships with `(memory 1)`.
    let needed_pages = ((bytes.len() + 65535) >> 16) as i32;
    let mut current_out = [Val::I32(0)];
    pages.call(&mut *caller, &[], &mut current_out)?;
    let current_pages = match current_out[0] {
        Val::I32(n) => n,
        _ => 0,
    };
    if needed_pages > current_pages {
        let mut grow_out = [Val::I32(0)];
        grow.call(
            &mut *caller,
            &[Val::I32(needed_pages - current_pages)],
            &mut grow_out,
        )?;
    }
    memory.write(&mut *caller, 0, bytes)?;
    let mut from_lm_out = [Val::AnyRef(None)];
    from_lm.call(
        &mut *caller,
        &[Val::I32(bytes.len() as i32)],
        &mut from_lm_out,
    )?;
    let r = match &from_lm_out[0] {
        Val::AnyRef(r) => r.clone(),
        _ => None,
    };
    Ok(r)
}

