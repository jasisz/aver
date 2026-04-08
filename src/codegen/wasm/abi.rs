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
    // --- Random ---
    // Returns a random integer in [min, max] inclusive.
    AbiImport {
        effect: "Random.int",
        import_name: "random_int",
        params: &[ValType::I64, ValType::I64], // min, max
        results: &[ValType::I64],
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

    // 1. Effects from user function signatures
    for name in user_fn_names {
        if let Some((_, _, effects)) = fn_sigs.get(*name) {
            for effect in effects {
                if seen.insert(effect.clone())
                    && let Some(abi) = lookup(effect)
                {
                    imports.push(abi);
                }
            }
        }
    }

    // 2. Explicit builtin calls that need host imports (e.g. Float.sin → math_sin)
    for call_name in host_call_names {
        if seen.insert(call_name.clone())
            && let Some(abi) = lookup(call_name)
        {
            imports.push(abi);
        }
    }

    // Stable order
    imports.sort_by_key(|a| a.import_name);
    imports.dedup_by_key(|a| a.import_name);
    imports
}
