//! Compiles the WAT-source-of-truth runtime into a standalone wasm module.
//!
//! Source layout: `runtime/wat/<namespace>.part.wat` files are textual
//! fragments — they contain `(func ...)` and `(export ...)` forms, no
//! `(module ...)` wrapper. They are concatenated in a fixed order (prelude
//! first, since it declares memory and globals the rest depends on) inside
//! a synthetic `(module ... )`, parsed once, and the resulting wasm bytes
//! are the runtime artifact that user modules import from.
//!
//! The fragments-not-modules choice means each .part.wat file isn't valid
//! standalone WAT — wat2wasm/LSP on a single file will error. That's the
//! cost of avoiding cross-fragment imports; functions in `maps.part.wat`
//! call functions in `alloc.part.wat` directly because they live in one
//! module after concatenation. The header comment in each file should
//! say so.

const PRELUDE_WAT: &str = include_str!("wat/prelude.part.wat");
const ALLOC_WAT: &str = include_str!("wat/alloc.part.wat");

/// Build the runtime module's WAT source by concatenating fragments
/// inside a `(module ...)` wrapper. Order matters — prelude declares
/// memory and globals; later fragments reference them.
fn runtime_wat_source() -> String {
    let mut s = String::with_capacity(4096);
    s.push_str("(module\n");
    s.push_str(PRELUDE_WAT);
    s.push('\n');
    s.push_str(ALLOC_WAT);
    s.push('\n');
    s.push(')');
    s
}

/// Parse the runtime WAT source into wasm bytes. Validation is the
/// caller's job (e.g. via `wasmparser::Validator`); this just textual
/// → binary conversion.
pub fn build_runtime_wasm() -> Result<Vec<u8>, String> {
    let src = runtime_wat_source();
    wat::parse_str(&src).map_err(|e| format!("runtime WAT parse failed: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_wat_parses_into_bytes() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        assert!(bytes.starts_with(b"\0asm"), "expected wasm magic header");
    }

    #[test]
    fn runtime_wasm_validates() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("runtime wasm must validate");
    }

    #[test]
    fn runtime_exports_memory_and_alloc() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        let mut found_memory = false;
        let mut found_alloc = false;
        let mut found_heap_ptr = false;
        for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
            if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.expect("export entry");
                    match export.name {
                        "memory" => found_memory = true,
                        "rt_alloc" => found_alloc = true,
                        "heap_ptr" => found_heap_ptr = true,
                        _ => {}
                    }
                }
            }
        }
        assert!(found_memory, "runtime must export memory");
        assert!(found_alloc, "runtime must export rt_alloc");
        assert!(found_heap_ptr, "runtime must export heap_ptr");
    }
}
