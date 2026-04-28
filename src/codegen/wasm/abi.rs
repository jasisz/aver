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
