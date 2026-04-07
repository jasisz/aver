//! Aver WASM Runner — host that provides capabilities (Console.print etc.)
//! to Aver programs compiled to WASM.
//!
//! Usage: aver-wasm-runner <program.wasm>

use anyhow::{Context, Result};
use std::env;
use wasmtime::*;

// ---------------------------------------------------------------------------
// Value layout constants (must match src/codegen/wasm/value.rs)
// ---------------------------------------------------------------------------

const TAG_SHIFT: u32 = 60;
const PAYLOAD_MASK: u64 = (1u64 << 60) - 1;
const TAG_INT: u64 = 0;
const TAG_CONST: u64 = 1;
const TAG_HEAP: u64 = 2;

// Const payloads
const CONST_FALSE: u64 = 0;
const CONST_TRUE: u64 = 1;
const CONST_UNIT: u64 = 2;
const CONST_NONE: u64 = 3;
const CONST_EMPTY_LIST: u64 = 4;

// Heap object kinds
const OBJ_STRING: u64 = 0;
const OBJ_WRAPPER: u64 = 3;
const OBJ_LIST_CONS: u64 = 4;

// Wrapper tags
const WRAP_OK: u64 = 0;
const WRAP_ERR: u64 = 1;
const WRAP_SOME: u64 = 2;

// Header layout
const HDR_KIND_SHIFT: u32 = 56;
const HDR_TAG_SHIFT: u32 = 48;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: aver-wasm-runner <program.wasm>");
        std::process::exit(1);
    }
    let wasm_path = &args[1];

    let engine = Engine::default();
    let module = Module::from_file(&engine, wasm_path)
        .with_context(|| format!("Failed to load {}", wasm_path))?;

    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

    // -----------------------------------------------------------------------
    // Provide host capabilities (effects → WASM imports)
    // -----------------------------------------------------------------------

    // Console.print — void import, decodes tagged Value and prints
    linker.func_wrap("aver_host", "console_print", |mut caller: Caller<'_, ()>, val: i64| {
        let s = format_value(val as u64, &mut caller);
        println!("{}", s);
    })?;

    // Console.error
    linker.func_wrap("aver_host", "console_error", |mut caller: Caller<'_, ()>, val: i64| {
        let s = format_value(val as u64, &mut caller);
        eprintln!("{}", s);
    })?;

    // Console.warn
    linker.func_wrap("aver_host", "console_warn", |mut caller: Caller<'_, ()>, val: i64| {
        let s = format_value(val as u64, &mut caller);
        eprintln!("[warn] {}", s);
    })?;

    // -----------------------------------------------------------------------
    // Instantiate and run
    // -----------------------------------------------------------------------

    let instance = linker
        .instantiate(&mut store, &module)
        .context("Failed to instantiate WASM module")?;

    // Try _start first, then main
    if let Some(start) = instance.get_func(&mut store, "_start") {
        let results = &mut [Val::I64(0)];
        start.call(&mut store, &[], results)?;
    } else if let Some(main_fn) = instance.get_func(&mut store, "main") {
        let results = &mut [Val::I64(0)];
        main_fn.call(&mut store, &[], results)?;
    } else {
        eprintln!("No _start or main function found");
        std::process::exit(1);
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Value decoding
// ---------------------------------------------------------------------------

fn format_value(val: u64, caller: &mut Caller<'_, ()>) -> String {
    let tag = (val >> TAG_SHIFT) & 0xF;
    let payload = val & PAYLOAD_MASK;

    match tag {
        t if t == TAG_INT => {
            // Sign-extend 60-bit to i64
            let n = if payload >= (1u64 << 59) {
                (payload as i64) - (1i64 << 60)
            } else {
                payload as i64
            };
            n.to_string()
        }
        t if t == TAG_CONST => match payload {
            p if p == CONST_FALSE => "false".to_string(),
            p if p == CONST_TRUE => "true".to_string(),
            p if p == CONST_UNIT => "()".to_string(),
            p if p == CONST_NONE => "None".to_string(),
            p if p == CONST_EMPTY_LIST => "[]".to_string(),
            _ => format!("Const({})", payload),
        },
        t if t == TAG_HEAP => {
            format_heap_value(payload as u32, caller)
        }
        _ => format!("Unknown(tag={}, payload={})", tag, payload),
    }
}

fn format_heap_value(handle: u32, caller: &mut Caller<'_, ()>) -> String {
    let memory = match caller.get_export("memory") {
        Some(Extern::Memory(m)) => m,
        _ => return "<no memory>".to_string(),
    };
    let data = memory.data(&caller);

    if (handle as usize + 8) > data.len() {
        return format!("<invalid handle {}>", handle);
    }

    // Read header
    let header = u64::from_le_bytes(
        data[handle as usize..handle as usize + 8]
            .try_into()
            .unwrap(),
    );
    let kind = (header >> HDR_KIND_SHIFT) & 0xFF;
    let variant_tag = (header >> HDR_TAG_SHIFT) & 0xFF;
    let field_count = header & 0xFFFFFFFF;

    match kind {
        k if k == OBJ_STRING => {
            let len = field_count as usize;
            let start = handle as usize + 8;
            if start + len <= data.len() {
                let bytes = &data[start..start + len];
                String::from_utf8_lossy(bytes).to_string()
            } else {
                format!("<string len={} overflow>", len)
            }
        }
        k if k == OBJ_WRAPPER => {
            let tag_name = match variant_tag {
                t if t == WRAP_OK => "Result.Ok",
                t if t == WRAP_ERR => "Result.Err",
                t if t == WRAP_SOME => "Option.Some",
                _ => "Wrapper",
            };
            if field_count >= 1 {
                let inner_offset = handle as usize + 8;
                let inner = u64::from_le_bytes(
                    data[inner_offset..inner_offset + 8].try_into().unwrap(),
                );
                // Recursive format — need caller mut ref, use simplified version
                let inner_str = format_value_from_memory(inner, data);
                format!("{}({})", tag_name, inner_str)
            } else {
                tag_name.to_string()
            }
        }
        k if k == OBJ_LIST_CONS => {
            // Format list as [a, b, c]
            let mut items = Vec::new();
            let mut current_handle = handle;
            loop {
                let hdr = u64::from_le_bytes(
                    data[current_handle as usize..current_handle as usize + 8]
                        .try_into()
                        .unwrap(),
                );
                let k = (hdr >> HDR_KIND_SHIFT) & 0xFF;
                if k != OBJ_LIST_CONS {
                    break;
                }
                let head = u64::from_le_bytes(
                    data[current_handle as usize + 8..current_handle as usize + 16]
                        .try_into()
                        .unwrap(),
                );
                let tail = u64::from_le_bytes(
                    data[current_handle as usize + 16..current_handle as usize + 24]
                        .try_into()
                        .unwrap(),
                );
                items.push(format_value_from_memory(head, data));
                // Check if tail is HeapRef
                let tail_tag = (tail >> TAG_SHIFT) & 0xF;
                if tail_tag == TAG_HEAP {
                    current_handle = (tail & PAYLOAD_MASK) as u32;
                } else {
                    // Tail is empty list or something else
                    break;
                }
                if items.len() > 100 {
                    items.push("...".to_string());
                    break;
                }
            }
            format!("[{}]", items.join(", "))
        }
        _ => format!("<object kind={} fields={}>", kind, field_count),
    }
}

/// Format a value using only memory data (no Caller — for recursive calls).
fn format_value_from_memory(val: u64, data: &[u8]) -> String {
    let tag = (val >> TAG_SHIFT) & 0xF;
    let payload = val & PAYLOAD_MASK;

    match tag {
        t if t == TAG_INT => {
            let n = if payload >= (1u64 << 59) {
                (payload as i64) - (1i64 << 60)
            } else {
                payload as i64
            };
            n.to_string()
        }
        t if t == TAG_CONST => match payload {
            p if p == CONST_FALSE => "false".to_string(),
            p if p == CONST_TRUE => "true".to_string(),
            p if p == CONST_UNIT => "()".to_string(),
            p if p == CONST_NONE => "None".to_string(),
            p if p == CONST_EMPTY_LIST => "[]".to_string(),
            _ => format!("Const({})", payload),
        },
        t if t == TAG_HEAP => {
            let handle = payload as u32;
            if (handle as usize + 8) > data.len() {
                return format!("<invalid ref {}>", handle);
            }
            let header = u64::from_le_bytes(
                data[handle as usize..handle as usize + 8]
                    .try_into()
                    .unwrap(),
            );
            let kind = (header >> HDR_KIND_SHIFT) & 0xFF;
            let variant_tag = (header >> HDR_TAG_SHIFT) & 0xFF;
            let field_count = header & 0xFFFFFFFF;

            match kind {
                k if k == OBJ_STRING => {
                    let len = field_count as usize;
                    let start = handle as usize + 8;
                    if start + len <= data.len() {
                        String::from_utf8_lossy(&data[start..start + len]).to_string()
                    } else {
                        format!("<string overflow>")
                    }
                }
                k if k == OBJ_WRAPPER => {
                    let tag_name = match variant_tag {
                        t if t == WRAP_OK => "Result.Ok",
                        t if t == WRAP_ERR => "Result.Err",
                        t if t == WRAP_SOME => "Option.Some",
                        _ => "Wrapper",
                    };
                    if field_count >= 1 {
                        let inner = u64::from_le_bytes(
                            data[handle as usize + 8..handle as usize + 16]
                                .try_into()
                                .unwrap(),
                        );
                        format!("{}({})", tag_name, format_value_from_memory(inner, data))
                    } else {
                        tag_name.to_string()
                    }
                }
                _ => format!("<object kind={}>", kind),
            }
        }
        _ => format!("?{}", val),
    }
}
