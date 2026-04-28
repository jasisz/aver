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
const LISTS_WAT: &str = include_str!("wat/lists.part.wat");
const MAPS_WAT: &str = include_str!("wat/maps.part.wat");
const VECTORS_WAT: &str = include_str!("wat/vectors.part.wat");
const INT_TO_STR_WAT: &str = include_str!("wat/int_to_str.part.wat");
const TO_STR_OBJ_WAT: &str = include_str!("wat/to_str_obj.part.wat");
const STR_LEN_WAT: &str = include_str!("wat/str_len.part.wat");
const CHAR_TO_CODE_WAT: &str = include_str!("wat/char_to_code.part.wat");
const BYTE_HEX_WAT: &str = include_str!("wat/byte_hex.part.wat");
const CHAR_FROM_CODE_WAT: &str = include_str!("wat/char_from_code.part.wat");
const STR_CHAR_AT_WAT: &str = include_str!("wat/str_char_at.part.wat");
const STR_CASE_WAT: &str = include_str!("wat/str_case.part.wat");
const STR_TRIM_WAT: &str = include_str!("wat/str_trim.part.wat");
const STR_OPS_WAT: &str = include_str!("wat/str_ops.part.wat");
const FROM_STR_WAT: &str = include_str!("wat/from_str.part.wat");
const COLLECT_WAT: &str = include_str!("wat/collect.part.wat");

/// `aver_to_wasi.wat` is a standalone module (full `(module …)`),
/// not a fragment — it lives separately because its imports are
/// `wasi_snapshot_preview1.*`, which the main runtime doesn't touch.
const AVER_TO_WASI_WAT: &str = include_str!("wat/aver_to_wasi.wat");

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
    s.push_str(LISTS_WAT);
    s.push('\n');
    s.push_str(MAPS_WAT);
    s.push('\n');
    s.push_str(VECTORS_WAT);
    s.push('\n');
    s.push_str(INT_TO_STR_WAT);
    s.push('\n');
    s.push_str(TO_STR_OBJ_WAT);
    s.push('\n');
    s.push_str(STR_LEN_WAT);
    s.push('\n');
    s.push_str(CHAR_TO_CODE_WAT);
    s.push('\n');
    s.push_str(BYTE_HEX_WAT);
    s.push('\n');
    s.push_str(CHAR_FROM_CODE_WAT);
    s.push('\n');
    s.push_str(STR_CHAR_AT_WAT);
    s.push('\n');
    s.push_str(STR_CASE_WAT);
    s.push('\n');
    s.push_str(STR_TRIM_WAT);
    s.push('\n');
    s.push_str(STR_OPS_WAT);
    s.push('\n');
    s.push_str(FROM_STR_WAT);
    s.push('\n');
    s.push_str(COLLECT_WAT);
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

/// Build the aver→WASI translation shim. Use under `--bridge wasip1`
/// to satisfy user.wasm's `aver/*` imports against
/// `wasi_snapshot_preview1.fd_write`.
pub fn build_aver_to_wasi_wasm() -> Result<Vec<u8>, String> {
    wat::parse_str(AVER_TO_WASI_WAT).map_err(|e| format!("aver_to_wasi WAT parse failed: {}", e))
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
    fn aver_to_wasi_bridge_parses_and_validates() {
        let bytes = build_aver_to_wasi_wasm().expect("aver_to_wasi WAT must parse");
        assert!(bytes.starts_with(b"\0asm"));
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("aver_to_wasi wasm must validate");
        let mut found_console = false;
        let mut found_print_value = false;
        for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
            if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.expect("export entry");
                    match export.name {
                        "console_print" => found_console = true,
                        "print_value" => found_print_value = true,
                        _ => {}
                    }
                }
            }
        }
        assert!(found_console, "bridge must export aver/console_print");
        assert!(found_print_value, "bridge must export aver/print_value");
    }

    #[test]
    fn runtime_wasm_validates() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("runtime wasm must validate");
    }

    #[test]
    fn runtime_export_contract_is_exact() {
        let bytes = build_runtime_wasm().expect("runtime WAT must parse");
        fn kind_name(kind: wasmparser::ExternalKind) -> &'static str {
            match kind {
                wasmparser::ExternalKind::Func => "func",
                wasmparser::ExternalKind::Table => "table",
                wasmparser::ExternalKind::Memory => "memory",
                wasmparser::ExternalKind::Global => "global",
                wasmparser::ExternalKind::Tag => "tag",
            }
        }

        let expected = [
            ("memory", "memory"),
            ("heap_ptr", "global"),
            ("collect_mark", "global"),
            ("collect_from", "global"),
            ("collect_dst", "global"),
            ("rt_alloc", "func"),
            ("rt_truncate", "func"),
            ("rt_obj_kind", "func"),
            ("rt_obj_tag", "func"),
            ("rt_obj_meta", "func"),
            ("rt_obj_field", "func"),
            ("rt_obj_field_f64", "func"),
            ("rt_obj_field_i32", "func"),
            ("rt_unwrap", "func"),
            ("rt_unwrap_f64", "func"),
            ("rt_unwrap_i32", "func"),
            ("rt_wrap", "func"),
            ("rt_wrap_f64", "func"),
            ("rt_wrap_i32", "func"),
            ("rt_str_eq", "func"),
            ("rt_str_concat", "func"),
            ("rt_list_cons", "func"),
            ("rt_list_cons_f64", "func"),
            ("rt_str_byte_len", "func"),
            ("rt_str_find", "func"),
            ("rt_str_starts_with", "func"),
            ("rt_str_ends_with", "func"),
            ("rt_str_contains", "func"),
            ("rt_list_take", "func"),
            ("rt_list_drop", "func"),
            ("rt_list_concat", "func"),
            ("rt_list_reverse", "func"),
            ("rt_list_contains", "func"),
            ("rt_list_zip", "func"),
            ("rt_map_get", "func"),
            ("rt_map_set", "func"),
            ("rt_map_has", "func"),
            ("rt_map_from_list", "func"),
            ("rt_map_keys", "func"),
            ("rt_map_entries", "func"),
            ("rt_map_len", "func"),
            ("rt_vec_from_list", "func"),
            ("rt_vec_get", "func"),
            ("rt_vec_len", "func"),
            ("rt_vec_set", "func"),
            ("rt_vec_new", "func"),
            ("rt_vec_to_list", "func"),
            ("rt_int_to_str", "func"),
            ("rt_float_to_str", "func"),
            ("rt_i64_to_str_obj", "func"),
            ("rt_f64_to_str_obj", "func"),
            ("rt_str_len", "func"),
            ("rt_char_to_code", "func"),
            ("rt_byte_to_hex", "func"),
            ("rt_byte_from_hex", "func"),
            ("rt_char_from_code", "func"),
            ("rt_str_char_at", "func"),
            ("rt_str_to_lower", "func"),
            ("rt_str_to_upper", "func"),
            ("rt_str_trim", "func"),
            ("rt_str_slice", "func"),
            ("rt_str_chars", "func"),
            ("rt_str_copy_range", "func"),
            ("rt_str_split", "func"),
            ("rt_str_join", "func"),
            ("rt_str_replace", "func"),
            ("rt_int_from_str", "func"),
            ("rt_float_from_str", "func"),
            ("rt_collect_begin", "func"),
            ("rt_rebase_i32", "func"),
            ("rt_collect_end", "func"),
            ("rt_retain_i32", "func"),
        ];
        let mut expected = expected.to_vec();
        expected.sort_unstable();
        let mut actual = Vec::new();
        for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
            if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.expect("export entry");
                    actual.push((export.name, kind_name(export.kind)));
                }
            }
        }
        actual.sort_unstable();
        assert_eq!(actual, expected, "aver_runtime export ABI changed");
    }
}
