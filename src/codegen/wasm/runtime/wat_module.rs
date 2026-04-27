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
const TRUNCATE_WAT: &str = include_str!("wat/truncate.part.wat");
const OBJ_WAT: &str = include_str!("wat/obj.part.wat");
const UNWRAP_WAT: &str = include_str!("wat/unwrap.part.wat");
const WRAP_WAT: &str = include_str!("wat/wrap.part.wat");
const STR_EQ_WAT: &str = include_str!("wat/str_eq.part.wat");
const STR_CONCAT_WAT: &str = include_str!("wat/str_concat.part.wat");
const LIST_CONS_WAT: &str = include_str!("wat/list_cons.part.wat");
const STR_SEARCH_WAT: &str = include_str!("wat/str_search.part.wat");

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
    s.push_str(TRUNCATE_WAT);
    s.push('\n');
    s.push_str(OBJ_WAT);
    s.push('\n');
    s.push_str(UNWRAP_WAT);
    s.push('\n');
    s.push_str(WRAP_WAT);
    s.push('\n');
    s.push_str(STR_EQ_WAT);
    s.push('\n');
    s.push_str(STR_CONCAT_WAT);
    s.push('\n');
    s.push_str(LIST_CONS_WAT);
    s.push('\n');
    s.push_str(STR_SEARCH_WAT);
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
    fn runtime_exports_expected_symbols() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        let expected = [
            "memory",
            "heap_ptr",
            "rt_alloc",
            "rt_truncate",
            "rt_obj_kind",
            "rt_obj_tag",
            "rt_obj_meta",
            "rt_obj_field",
            "rt_obj_field_f64",
            "rt_obj_field_i32",
            "rt_unwrap",
            "rt_unwrap_f64",
            "rt_unwrap_i32",
            "rt_wrap",
            "rt_wrap_f64",
            "rt_wrap_i32",
            "rt_str_eq",
            "rt_str_concat",
            "rt_list_cons",
            "rt_list_cons_f64",
            "rt_str_byte_len",
            "rt_str_find",
            "rt_str_starts_with",
            "rt_str_ends_with",
            "rt_str_contains",
        ];
        let mut found: std::collections::HashSet<&str> =
            std::collections::HashSet::new();
        for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
            if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.expect("export entry");
                    if let Some(&name) = expected.iter().find(|&&n| n == export.name) {
                        found.insert(name);
                    }
                }
            }
        }
        for name in expected {
            assert!(found.contains(name), "runtime must export {}", name);
        }
    }
}
