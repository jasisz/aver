/// Aver WASM import ABI.
///
/// Single source of truth for the mapping between Aver effects and WASM imports.
/// Only true host effects appear here — runtime helpers (alloc, print formatting)
/// stay inside the module.
///
/// Import module: `"aver"`. Function names: stable ABI.
/// Signatures are environment-neutral (ptr+len, not fd numbers or iovecs).
use wasm_encoder::ValType;

/// One entry in the ABI table.
pub struct AbiImport {
    /// Aver effect name, e.g. `"Console.print"`.
    pub effect: &'static str,
    /// WASM import function name under module `"aver"`, e.g. `"console_print"`.
    pub import_name: &'static str,
    /// WASM parameter types.
    pub params: &'static [ValType],
    /// WASM result types.
    pub results: &'static [ValType],
}

/// The ABI module name used in WASM imports.
pub const ABI_MODULE: &str = "aver";

/// Complete ABI table. Each Aver host effect maps to exactly one import.
pub const ABI_TABLE: &[AbiImport] = &[
    // --- Args ---
    // Number of program args visible via Args.get().
    AbiImport {
        effect: "Args._len",
        import_name: "args_len",
        params: &[],
        results: &[ValType::I32],
    },
    // Indexed program arg, returned as raw ptr+len.
    AbiImport {
        effect: "Args._get",
        import_name: "args_get",
        params: &[ValType::I32],
        results: &[ValType::I32, ValType::I32],
    },
    // --- Console ---
    // Writes bytes to stdout. Host decides how (terminal, browser console, test buffer).
    AbiImport {
        effect: "Console.print",
        import_name: "console_print",
        params: &[ValType::I32, ValType::I32], // ptr, len
        results: &[],
    },
    // Writes bytes to stderr.
    AbiImport {
        effect: "Console.error",
        import_name: "console_error",
        params: &[ValType::I32, ValType::I32],
        results: &[],
    },
    // Writes bytes to stderr at warning severity. Distinct from
    // `Console.error` so JS hosts can route the two streams
    // differently (`console.warn` vs `console.error`).
    AbiImport {
        effect: "Console.warn",
        import_name: "console_warn",
        params: &[ValType::I32, ValType::I32],
        results: &[],
    },
    // Reads a line from stdin. Host allocates string in WASM linear memory
    // using the exported `alloc` function, returns (ptr, len).
    AbiImport {
        effect: "Console.readLine",
        import_name: "console_readLine",
        params: &[],
        results: &[ValType::I32, ValType::I32], // ptr, len
    },
    // --- Terminal ---
    AbiImport {
        effect: "Terminal.enableRawMode",
        import_name: "terminal_enableRawMode",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.disableRawMode",
        import_name: "terminal_disableRawMode",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.clear",
        import_name: "terminal_clear",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.moveTo",
        import_name: "terminal_moveTo",
        params: &[ValType::I32, ValType::I32],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.print",
        import_name: "terminal_print",
        params: &[ValType::I32, ValType::I32],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.setColor",
        import_name: "terminal_setColor",
        params: &[ValType::I32, ValType::I32],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.resetColor",
        import_name: "terminal_resetColor",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.readKey",
        import_name: "terminal_readKey",
        params: &[],
        results: &[ValType::I32, ValType::I32], // ptr, len or (NONE_SENTINEL, 0)
    },
    AbiImport {
        effect: "Terminal.size",
        import_name: "terminal_size",
        params: &[],
        results: &[ValType::I32, ValType::I32], // width, height
    },
    AbiImport {
        effect: "Terminal.hideCursor",
        import_name: "terminal_hideCursor",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.showCursor",
        import_name: "terminal_showCursor",
        params: &[],
        results: &[],
    },
    AbiImport {
        effect: "Terminal.flush",
        import_name: "terminal_flush",
        params: &[],
        results: &[],
    },
    // --- Random ---
    // Returns a random integer in [min, max] inclusive.
    AbiImport {
        effect: "Random.int",
        import_name: "random_int",
        params: &[ValType::I64, ValType::I64], // min, max
        results: &[ValType::I64],
    },
    // Returns a random float in [0.0, 1.0).
    AbiImport {
        effect: "Random.float",
        import_name: "random_float",
        params: &[],
        results: &[ValType::F64],
    },
    // --- Fetch / Request / Response (JS host bridge) ---
    // Used by `--bridge fetch` deployments (Cloudflare Workers,
    // Fastly Compute, Deno Deploy, …). Bootstrap (worker.js)
    // implements these against Request/Response from the JS Fetch
    // API. Lazy host imports — guest only pays the host crossing
    // for fields it actually reads.
    AbiImport {
        effect: "Request.method",
        import_name: "request_method",
        params: &[],
        results: &[ValType::I32], // OBJ_STRING ptr
    },
    AbiImport {
        effect: "Request.url",
        import_name: "request_url",
        params: &[],
        results: &[ValType::I32],
    },
    AbiImport {
        effect: "Request.body",
        import_name: "request_body",
        params: &[],
        results: &[ValType::I32],
    },
    // Bulk-transfer the request's `Headers` into a guest
    // `Map<String, List<String>>`. Single host crossing — the JS
    // bootstrap walks `request.headers`, allocates OBJ_STRING
    // handles via the runtime's bump allocator, builds a list of
    // (name, [value, …]) tuples, and folds the list into a HAMT
    // root via `rt_map_from_list`. Multi-value entries
    // (Set-Cookie via `getSetCookie()`, Vary, …) keep separate
    // values in the value list. Returns the OBJ_HAMT handle
    // (or `0` for an empty map).
    AbiImport {
        effect: "Request.headersLoad",
        import_name: "request_headers_load",
        params: &[],
        results: &[ValType::I32],
    },
    // Response.text(status, body) → host stores status/body, returns
    // an opaque handle (i32). The handle is what the user fn returns
    // from the HTTP handler; the pack bootstrap reads stashed
    // status/body to build the native Response.
    AbiImport {
        effect: "Response.text",
        import_name: "response_text",
        // status: i32, body_ptr: i32, body_len: i32
        params: &[ValType::I32, ValType::I32, ValType::I32],
        results: &[ValType::I32],
    },
    // Response.set_header(name, value) → push (name, value) onto the
    // pending Response's headers. Multi-value headers (Set-Cookie,
    // Vary, …) are produced by calling this multiple times with the
    // same name. The Fetch-bridge emitter inserts one call per entry
    // when constructing `HttpResponse`.
    AbiImport {
        effect: "Response.setHeader",
        import_name: "response_set_header",
        // name_ptr: i32, name_len: i32, value_ptr: i32, value_len: i32
        params: &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        results: &[],
    },
    // --- Http (client) ---
    // All six methods (GET/HEAD/DELETE/POST/PUT/PATCH) share one
    // generic host import. Method name comes through as a string
    // arg; body and content-type are empty strings for the
    // bodyless verbs. Request headers are pushed onto a
    // host-maintained pending list via `http_add_request_header` /
    // `http_clear_request_headers` before the `http_send` fires —
    // same walk-and-set pattern as `response_set_header`.
    //
    // Multi-result return:
    //   (status: i64, body_ptr: i32, err_ptr: i32)
    //   - Transport ok: status > 0, body_ptr = OBJ_STRING handle,
    //     err_ptr = 0 → wrapped `Result.Ok(HttpResponse{...})`.
    //   - Transport err: status = 0, body_ptr = 0,
    //     err_ptr = OBJ_STRING with the error message →
    //     wrapped `Result.Err(msg)`.
    //
    // Response headers are an empty `Map` for 0.14 — bulk-transfer
    // back into the guest's Map<String, List<String>> shape lands
    // in 0.15. Same caveat applies to `req.headers` under the
    // Fetch bridge.
    //
    // The JS host (Cloudflare Workers, Deno, Bun, browser
    // playground) is expected to wrap `http_send` with
    // `WebAssembly.Suspending` so the synchronous WASM call point
    // can suspend on `await fetch(...)`. WASI preview 1 has no
    // HTTP client at all, so the WASI shim returns a transport
    // error — programs that need real HTTP under standalone
    // wasmtime should wait for preview 2 wasi-http or run via
    // a JS host instead.
    AbiImport {
        effect: "Http.send",
        import_name: "http_send",
        // method_ptr, method_len,
        // url_ptr, url_len,
        // body_ptr, body_len,
        // ct_ptr, ct_len
        params: &[
            ValType::I32, ValType::I32,
            ValType::I32, ValType::I32,
            ValType::I32, ValType::I32,
            ValType::I32, ValType::I32,
        ],
        // (status: i64, body_ptr: i32, headers_map: i32, err_ptr: i32)
        // The headers map is the OBJ_HAMT handle returned by the
        // host's same `rt_map_from_list` round trip used for
        // request.headers — empty (`0`) on transport error.
        results: &[ValType::I64, ValType::I32, ValType::I32, ValType::I32],
    },
    AbiImport {
        effect: "Http.addRequestHeader",
        import_name: "http_add_request_header",
        // name_ptr, name_len, value_ptr, value_len
        params: &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        results: &[],
    },
    AbiImport {
        effect: "Http.clearRequestHeaders",
        import_name: "http_clear_request_headers",
        params: &[],
        results: &[],
    },
    // --- Env ---
    // Process environment access. Reads return -1 for "name unset"
    // (matches Aver's NONE_SENTINEL); on Some, the host hands back
    // an OBJ_STRING pointer that the emitter wraps in `Option.Some`.
    // Writes are best-effort: WASI preview 1 has no `setenv`, JS hosts
    // get a frozen `env`, so most bridges treat `env_set` as a no-op.
    // Aver's type system says `set: (String, String) -> Unit`, so
    // we honour that contract regardless of host capability.
    AbiImport {
        effect: "Env.get",
        import_name: "env_get",
        params: &[ValType::I32, ValType::I32], // name_ptr, name_len
        results: &[ValType::I32],              // -1 = None, OBJ_STRING ptr = Some
    },
    AbiImport {
        effect: "Env.set",
        import_name: "env_set",
        params: &[ValType::I32, ValType::I32, ValType::I32, ValType::I32], // name+value
        results: &[],
    },
    // --- Time ---
    // Returns current time as milliseconds since Unix epoch.
    AbiImport {
        effect: "Time.unixMs",
        import_name: "time_unixMs",
        params: &[],
        results: &[ValType::I64],
    },
    // Sleeps for the given number of milliseconds.
    AbiImport {
        effect: "Time.sleep",
        import_name: "time_sleep",
        params: &[ValType::I64], // millis
        results: &[],
    },
    // Returns current UTC timestamp as ISO string.
    // Host allocates string in WASM memory, returns (ptr, len).
    AbiImport {
        effect: "Time.now",
        import_name: "time_now",
        params: &[],
        results: &[ValType::I32, ValType::I32], // ptr, len
    },
    // --- Formatting / Printing ---
    // Formats and prints any value. Host reads WASM memory for heap values.
    // tag: 0=Int(i64), 1=Float(f64 bits as i64), 2=Bool(i32 as i64),
    //      3=String(ptr as i64), 4=Heap(ptr as i64)
    AbiImport {
        effect: "Print.value",
        import_name: "print_value",
        params: &[ValType::I32, ValType::I64], // tag, val
        results: &[],
    },
    // Formats any value to string (for interpolation).
    AbiImport {
        effect: "Format.value",
        import_name: "format_value",
        params: &[ValType::I32, ValType::I64],  // tag, val
        results: &[ValType::I32, ValType::I32], // ptr, len
    },
    // --- Math (no native WASM instructions) ---
    AbiImport {
        effect: "Float.sin",
        import_name: "math_sin",
        params: &[ValType::F64],
        results: &[ValType::F64],
    },
    AbiImport {
        effect: "Float.cos",
        import_name: "math_cos",
        params: &[ValType::F64],
        results: &[ValType::F64],
    },
    AbiImport {
        effect: "Float.atan2",
        import_name: "math_atan2",
        params: &[ValType::F64, ValType::F64],
        results: &[ValType::F64],
    },
    AbiImport {
        effect: "Float.pow",
        import_name: "math_pow",
        params: &[ValType::F64, ValType::F64],
        results: &[ValType::F64],
    },
];

/// Lookup an ABI entry by Aver effect name.
pub fn lookup(effect: &str) -> Option<&'static AbiImport> {
    ABI_TABLE.iter().find(|e| e.effect == effect)
}

/// Collect unique ABI imports needed by the program.
/// Sources: effect declarations in user functions + explicit builtin calls.
pub fn collect_needed_imports(
    fn_sigs: &std::collections::HashMap<
        String,
        (Vec<crate::types::Type>, crate::types::Type, Vec<String>),
    >,
    user_fn_names: &[&str],
    host_call_names: &std::collections::HashSet<String>,
) -> Vec<&'static AbiImport> {
    let mut seen = std::collections::HashSet::new();
    let mut imports = Vec::new();

    let mut add_effect = |effect: &str| {
        if effect == "Args.get" {
            for hidden in ["Args._len", "Args._get"] {
                if seen.insert(hidden.to_string())
                    && let Some(abi) = lookup(hidden)
                {
                    imports.push(abi);
                }
            }
            return;
        }

        if seen.insert(effect.to_string())
            && let Some(abi) = lookup(effect)
        {
            imports.push(abi);
        }
    };

    // 1. Effects from user function signatures
    for name in user_fn_names {
        if let Some((_, _, effects)) = fn_sigs.get(*name) {
            for effect in effects {
                add_effect(effect);
            }
        }
    }

    // 2. Explicit builtin calls that need host imports (e.g. Float.sin → math_sin)
    for call_name in host_call_names {
        add_effect(call_name);
    }

    // Stable order
    imports.sort_by_key(|a| a.import_name);
    imports.dedup_by_key(|a| a.import_name);
    imports
}
