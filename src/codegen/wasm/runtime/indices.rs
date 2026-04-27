// Runtime function and type index definitions for the WASM backend.
//
// This module is the single source of truth for:
// - runtime function indices
// - canonical WASM function signatures used by runtime helpers and host imports
// - mapping runtime functions back to their type-section entries

use wasm_encoder::{TypeSection, ValType};

/// Import slots for runtime functions that live in the `aver_runtime`
/// module rather than as local user-module functions. The emitter fills
/// this in as it declares each `aver_runtime.*` import; the runtime
/// migration grows this struct one entry per migrated function.
#[derive(Debug, Clone, Copy, Default)]
pub struct AverRuntimeImports {
    pub rt_alloc: u32,
    pub rt_truncate: u32,
    pub rt_obj_kind: u32,
    pub rt_obj_tag: u32,
    pub rt_obj_meta: u32,
    pub rt_obj_field: u32,
    pub rt_obj_field_f64: u32,
    pub rt_obj_field_i32: u32,
    pub rt_unwrap: u32,
    pub rt_unwrap_f64: u32,
    pub rt_unwrap_i32: u32,
    pub rt_wrap: u32,
    pub rt_wrap_f64: u32,
    pub rt_wrap_i32: u32,
    pub rt_str_eq: u32,
    pub rt_str_concat: u32,
    pub rt_list_cons: u32,
    pub rt_list_cons_f64: u32,
    pub rt_str_byte_len: u32,
    pub rt_str_find: u32,
    pub rt_str_starts_with: u32,
    pub rt_str_ends_with: u32,
    pub rt_str_contains: u32,
    pub rt_list_take: u32,
    pub rt_list_drop: u32,
    pub rt_list_concat: u32,
    pub rt_list_reverse: u32,
    pub rt_list_contains: u32,
    pub rt_list_zip: u32,
    pub rt_map_get: u32,
    pub rt_map_set: u32,
    pub rt_map_has: u32,
    pub rt_map_keys: u32,
    pub rt_map_entries: u32,
    pub rt_vec_from_list: u32,
    pub rt_vec_get: u32,
    pub rt_vec_len: u32,
    pub rt_vec_set: u32,
    pub rt_vec_new: u32,
    pub rt_vec_to_list: u32,
    pub rt_int_to_str: u32,
    pub rt_float_to_str: u32,
    pub rt_i64_to_str_obj: u32,
    pub rt_f64_to_str_obj: u32,
    pub rt_str_len: u32,
    pub rt_char_to_code: u32,
    pub rt_byte_to_hex: u32,
    pub rt_byte_from_hex: u32,
}

/// Index assignments for runtime functions within the module.
///
/// Some entries are imported from the `aver_runtime` module (the WAT
/// runtime migration is gradually moving everything there). The rest
/// are local function indices into the user module, starting at `base`
/// (which itself starts after all imports).
#[derive(Debug, Clone, Copy)]
pub struct RuntimeFuncIndices {
    pub alloc: u32,           // import index (aver_runtime.rt_alloc)
    pub truncate: u32,        // import index (aver_runtime.rt_truncate)
    pub collect_begin: u32,   // (i32) -> ()
    pub collect_end: u32,     // () -> ()
    pub rebase_i32: u32,      // (i32) -> i32
    pub retain_i32: u32,      // (i32) -> i32
    pub wrap: u32,            // (i32, i64, i32) -> i32
    pub wrap_f64: u32,        // (i32, f64) -> i32
    pub wrap_i32: u32,        // (i32, i32, i32) -> i32
    pub unwrap: u32,          // (i32) -> i64
    pub unwrap_f64: u32,      // (i32) -> f64
    pub unwrap_i32: u32,      // (i32) -> i32
    pub obj_kind: u32,        // (i32) -> i32
    pub obj_tag: u32,         // (i32) -> i32
    pub obj_meta: u32,        // (i32) -> i32
    pub obj_field: u32,       // (i32, i32) -> i64
    pub obj_field_f64: u32,   // (i32, i32) -> f64
    pub obj_field_i32: u32,   // (i32, i32) -> i32
    pub list_cons: u32,       // (i64, i32, i32) -> i32
    pub list_cons_f64: u32,   // (f64, i32) -> i32
    pub int_to_str: u32,      // (i64, i32) -> i32
    pub float_to_str: u32,    // (f64, i32) -> i32
    pub fd_write_buf: u32,    // (i32, i32) -> ()
    pub str_eq: u32,          // (i32, i32) -> i32
    pub str_concat: u32,      // (i32, i32) -> i32
    pub i64_to_str_obj: u32,  // (i64) -> i32
    pub f64_to_str_obj: u32,  // (f64) -> i32
    pub list_take: u32,       // (i32, i32) -> i32
    pub list_drop: u32,       // (i32, i32) -> i32
    pub list_concat: u32,     // (i32, i32) -> i32
    pub list_reverse: u32,    // (i32) -> i32
    pub list_contains: u32,   // (i32, i64) -> i32
    pub list_zip: u32,        // (i32, i32) -> i32
    pub map_get: u32,         // (i32, i32) -> i32
    pub map_set: u32,         // (i32, i32, i64, i32) -> i32
    pub map_has: u32,         // (i32, i32) -> i32
    pub map_keys: u32,        // (i32) -> i32
    pub map_entries: u32,     // (i32) -> i32
    pub vec_from_list: u32,   // (i32, i32) -> i32
    pub vec_get: u32,         // (i32, i64) -> i32
    pub vec_len: u32,         // (i32) -> i64
    pub vec_set: u32,         // (i32, i64, i64) -> i32
    pub vec_new: u32,         // (i64, i64, i32) -> i32
    pub vec_to_list: u32,     // (i32) -> i32
    pub str_len: u32,         // (i32) -> i64
    pub str_byte_len: u32,    // (i32) -> i64
    pub str_find: u32,        // (i32, i32, i32) -> i32
    pub str_starts_with: u32, // (i32, i32) -> i32
    pub str_ends_with: u32,   // (i32, i32) -> i32
    pub str_contains: u32,    // (i32, i32) -> i32
    pub str_char_at: u32,     // (i32, i64) -> i32
    pub char_from_code: u32,  // (i64) -> i32
    pub char_to_code: u32,    // (i32) -> i64
    pub byte_to_hex: u32,     // (i64) -> i32
    pub byte_from_hex: u32,   // (i32) -> i32
    pub str_trim: u32,        // (i32) -> i32
    pub str_slice: u32,       // (i32, i32, i32) -> i32
    pub str_chars: u32,       // (i32) -> i32
    pub str_split: u32,       // (i32, i32) -> i32
    pub str_join: u32,        // (i32, i32) -> i32
    pub str_replace: u32,     // (i32, i32, i32) -> i32
    pub str_to_lower: u32,    // (i32) -> i32
    pub str_to_upper: u32,    // (i32) -> i32
    pub int_from_str: u32,    // (i32) -> i32
    pub float_from_str: u32,  // (i32) -> i32
    /// Total number of runtime functions.
    pub count: u32,
    /// Import function index for writing to stdout (either WASI fd_write or aver/console_print).
    pub fd_write_import: u32,
    /// Which adapter mode is active.
    pub adapter: super::super::WasmAdapter,
}

impl RuntimeFuncIndices {
    /// `base` is the function index where local runtime functions start
    /// (after all imports). `imports` is the slot table for runtime
    /// functions already migrated to the imported `aver_runtime` module
    /// — those indices are used directly instead of bumping `base`.
    pub fn new(base: u32, imports: AverRuntimeImports) -> Self {
        let mut i = base;
        let mut next = || {
            let idx = i;
            i += 1;
            idx
        };
        RuntimeFuncIndices {
            alloc: imports.rt_alloc,
            truncate: imports.rt_truncate,
            collect_begin: next(),
            collect_end: next(),
            rebase_i32: next(),
            retain_i32: next(),
            wrap: imports.rt_wrap,
            wrap_f64: imports.rt_wrap_f64,
            wrap_i32: imports.rt_wrap_i32,
            unwrap: imports.rt_unwrap,
            unwrap_f64: imports.rt_unwrap_f64,
            unwrap_i32: imports.rt_unwrap_i32,
            obj_kind: imports.rt_obj_kind,
            obj_tag: imports.rt_obj_tag,
            obj_meta: imports.rt_obj_meta,
            obj_field: imports.rt_obj_field,
            obj_field_f64: imports.rt_obj_field_f64,
            obj_field_i32: imports.rt_obj_field_i32,
            list_cons: imports.rt_list_cons,
            list_cons_f64: imports.rt_list_cons_f64,
            int_to_str: imports.rt_int_to_str,
            float_to_str: imports.rt_float_to_str,
            fd_write_buf: next(),
            str_eq: imports.rt_str_eq,
            str_concat: imports.rt_str_concat,
            i64_to_str_obj: imports.rt_i64_to_str_obj,
            f64_to_str_obj: imports.rt_f64_to_str_obj,
            list_take: imports.rt_list_take,
            list_drop: imports.rt_list_drop,
            list_concat: imports.rt_list_concat,
            list_reverse: imports.rt_list_reverse,
            list_contains: imports.rt_list_contains,
            list_zip: imports.rt_list_zip,
            map_get: imports.rt_map_get,
            map_set: imports.rt_map_set,
            map_has: imports.rt_map_has,
            map_keys: imports.rt_map_keys,
            map_entries: imports.rt_map_entries,
            vec_from_list: imports.rt_vec_from_list,
            vec_get: imports.rt_vec_get,
            vec_len: imports.rt_vec_len,
            vec_set: imports.rt_vec_set,
            vec_new: imports.rt_vec_new,
            vec_to_list: imports.rt_vec_to_list,
            str_len: imports.rt_str_len,
            str_byte_len: imports.rt_str_byte_len,
            str_find: imports.rt_str_find,
            str_starts_with: imports.rt_str_starts_with,
            str_ends_with: imports.rt_str_ends_with,
            str_contains: imports.rt_str_contains,
            str_char_at: next(),
            char_from_code: next(),
            char_to_code: imports.rt_char_to_code,
            byte_to_hex: imports.rt_byte_to_hex,
            byte_from_hex: imports.rt_byte_from_hex,
            str_trim: next(),
            str_slice: next(),
            str_chars: next(),
            str_split: next(),
            str_join: next(),
            str_replace: next(),
            str_to_lower: next(),
            str_to_upper: next(),
            int_from_str: next(),
            float_from_str: next(),
            // count = number of LOCAL runtime functions only (alloc is imported).
            count: i - base,
            fd_write_import: 0,
            adapter: super::super::WasmAdapter::Aver,
        }
    }

    /// Pairs of (function index, stable name) for every runtime function.
    /// Consumed by the wasm name section so disassembly and `--target wat`
    /// output read like source instead of `call 18`.
    pub fn name_pairs(&self) -> Vec<(u32, &'static str)> {
        vec![
            (self.alloc, "alloc"),
            (self.truncate, "truncate"),
            (self.collect_begin, "collect_begin"),
            (self.collect_end, "collect_end"),
            (self.rebase_i32, "rebase_i32"),
            (self.retain_i32, "retain_i32"),
            (self.wrap, "wrap"),
            (self.wrap_f64, "wrap_f64"),
            (self.wrap_i32, "wrap_i32"),
            (self.unwrap, "unwrap"),
            (self.unwrap_f64, "unwrap_f64"),
            (self.unwrap_i32, "unwrap_i32"),
            (self.obj_kind, "obj_kind"),
            (self.obj_tag, "obj_tag"),
            (self.obj_meta, "obj_meta"),
            (self.obj_field, "obj_field"),
            (self.obj_field_f64, "obj_field_f64"),
            (self.obj_field_i32, "obj_field_i32"),
            (self.list_cons, "list_cons"),
            (self.list_cons_f64, "list_cons_f64"),
            (self.int_to_str, "int_to_str"),
            (self.float_to_str, "float_to_str"),
            (self.fd_write_buf, "fd_write_buf"),
            (self.str_eq, "str_eq"),
            (self.str_concat, "str_concat"),
            (self.i64_to_str_obj, "i64_to_str_obj"),
            (self.f64_to_str_obj, "f64_to_str_obj"),
            (self.list_take, "list_take"),
            (self.list_drop, "list_drop"),
            (self.list_concat, "list_concat"),
            (self.list_reverse, "list_reverse"),
            (self.list_contains, "list_contains"),
            (self.list_zip, "list_zip"),
            (self.map_get, "map_get"),
            (self.map_set, "map_set"),
            (self.map_has, "map_has"),
            (self.map_keys, "map_keys"),
            (self.map_entries, "map_entries"),
            (self.vec_from_list, "vec_from_list"),
            (self.vec_get, "vec_get"),
            (self.vec_len, "vec_len"),
            (self.vec_set, "vec_set"),
            (self.vec_new, "vec_new"),
            (self.vec_to_list, "vec_to_list"),
            (self.str_len, "str_len"),
            (self.str_byte_len, "str_byte_len"),
            (self.str_find, "str_find"),
            (self.str_starts_with, "str_starts_with"),
            (self.str_ends_with, "str_ends_with"),
            (self.str_contains, "str_contains"),
            (self.str_char_at, "str_char_at"),
            (self.char_from_code, "char_from_code"),
            (self.char_to_code, "char_to_code"),
            (self.byte_to_hex, "byte_to_hex"),
            (self.byte_from_hex, "byte_from_hex"),
            (self.str_trim, "str_trim"),
            (self.str_slice, "str_slice"),
            (self.str_chars, "str_chars"),
            (self.str_split, "str_split"),
            (self.str_join, "str_join"),
            (self.str_replace, "str_replace"),
            (self.str_to_lower, "str_to_lower"),
            (self.str_to_upper, "str_to_upper"),
            (self.int_from_str, "int_from_str"),
            (self.float_from_str, "float_from_str"),
        ]
    }
}

#[derive(Default)]
struct TypeRegistry {
    entries: Vec<(Vec<ValType>, Vec<ValType>)>,
}

impl TypeRegistry {
    fn intern(
        &mut self,
        type_section: &mut TypeSection,
        params: &[ValType],
        results: &[ValType],
    ) -> u32 {
        if let Some((idx, _)) = self
            .entries
            .iter()
            .enumerate()
            .find(|(_, (ps, rs))| ps.as_slice() == params && rs.as_slice() == results)
        {
            return idx as u32;
        }

        let idx = self.entries.len() as u32;
        type_section
            .ty()
            .function(params.to_vec(), results.to_vec());
        self.entries.push((params.to_vec(), results.to_vec()));
        idx
    }

    fn count(&self) -> u32 {
        self.entries.len() as u32
    }
}

/// Runtime and import function type signatures. Indices into the type section.
#[derive(Debug, Clone, Copy)]
pub struct RtTypeIndices {
    pub alloc: u32,                  // (i32) -> i32
    pub i32_to_empty: u32,           // (i32) -> ()
    pub wrap_i64: u32,               // (i32, i64, i32) -> i32
    pub wrap_f64: u32,               // (i32, f64) -> i32
    pub wrap_i32: u32,               // (i32, i32, i32) -> i32
    pub unwrap_i64: u32,             // (i32) -> i64
    pub unwrap_f64: u32,             // (i32) -> f64
    pub unwrap_i32: u32,             // (i32) -> i32
    pub obj_kind: u32,               // (i32) -> i32
    pub obj_tag: u32,                // (i32) -> i32
    pub obj_meta: u32,               // (i32) -> i32
    pub obj_field_i64: u32,          // (i32, i32) -> i64
    pub obj_field_f64: u32,          // (i32, i32) -> f64
    pub obj_field_i32: u32,          // (i32, i32) -> i32
    pub list_cons_i64: u32,          // (i64, i32, i32) -> i32
    pub list_cons_f64: u32,          // (f64, i32) -> i32
    pub print_i64: u32,              // (i64) -> ()
    pub print_f64: u32,              // (f64) -> ()
    pub print_i32: u32,              // (i32) -> ()
    pub int_to_str: u32,             // (i64, i32) -> i32
    pub float_to_str: u32,           // (f64, i32) -> i32
    pub fd_write_buf: u32,           // (i32, i32) -> ()
    pub wasi_fd_write: u32,          // (i32, i32, i32, i32) -> i32
    pub i32_i32_to_i32: u32,         // (i32, i32) -> i32
    pub i64_i32_to_i32: u32,         // (i64, i32) -> i32
    pub i64_to_i32: u32,             // (i64) -> i32
    pub f64_to_i32: u32,             // (f64) -> i32
    pub i32_i64_to_i32: u32,         // (i32, i64) -> i32
    pub i32_i64_i32_to_i32: u32,     // (i32, i64, i32) -> i32
    pub i32_i32_i64_to_i32: u32,     // (i32, i32, i64) -> i32
    pub i32_i32_i64_i32_to_i32: u32, // (i32, i32, i64, i32) -> i32
    pub i32_i64_i64_to_i32: u32,     // (i32, i64, i64) -> i32
    pub i64_i64_to_i32: u32,         // (i64, i64) -> i32
    pub i64_i64_i32_to_i32: u32,     // (i64, i64, i32) -> i32
    pub f64_to_f64: u32,             // (f64) -> f64
    pub f64_f64_to_f64: u32,         // (f64, f64) -> f64
    pub i32_i32_i32_to_i32: u32,     // (i32, i32, i32) -> i32
    pub empty_to_i32: u32,           // () -> i32
    pub empty_to_i32_i32: u32,       // () -> (i32, i32)
    pub i32_to_i32_i32: u32,         // (i32) -> (i32, i32)
    pub i32_i64_to_empty: u32,       // (i32, i64) -> ()
    pub i32_i64_to_i32_i32: u32,     // (i32, i64) -> (i32, i32)
    pub i64_i64_to_i64: u32,         // (i64, i64) -> i64
    pub empty_to_i64: u32,           // () -> i64
    pub empty_to_empty: u32,         // () -> ()
    pub count: u32,                  // total number of distinct base signatures
}

/// Emit and intern all base signatures used by runtime helpers and ABI imports.
pub fn emit_base_type_section(type_section: &mut TypeSection) -> RtTypeIndices {
    let mut registry = TypeRegistry::default();

    let alloc = registry.intern(type_section, &[ValType::I32], &[ValType::I32]);
    let i32_to_empty = registry.intern(type_section, &[ValType::I32], &[]);
    let wrap_i64 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I64, ValType::I32],
        &[ValType::I32],
    );
    let wrap_f64 = registry.intern(type_section, &[ValType::I32, ValType::F64], &[ValType::I32]);
    let wrap_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    );
    let unwrap_i64 = registry.intern(type_section, &[ValType::I32], &[ValType::I64]);
    let unwrap_f64 = registry.intern(type_section, &[ValType::I32], &[ValType::F64]);
    let unwrap_i32 = registry.intern(type_section, &[ValType::I32], &[ValType::I32]);
    let obj_field_i64 =
        registry.intern(type_section, &[ValType::I32, ValType::I32], &[ValType::I64]);
    let obj_field_f64 =
        registry.intern(type_section, &[ValType::I32, ValType::I32], &[ValType::F64]);
    let list_cons_i64 = registry.intern(
        type_section,
        &[ValType::I64, ValType::I32, ValType::I32],
        &[ValType::I32],
    );
    let list_cons_f64 =
        registry.intern(type_section, &[ValType::F64, ValType::I32], &[ValType::I32]);
    let print_i64 = registry.intern(type_section, &[ValType::I64], &[]);
    let print_f64 = registry.intern(type_section, &[ValType::F64], &[]);
    let print_i32 = registry.intern(type_section, &[ValType::I32], &[]);
    let fd_write_buf = registry.intern(type_section, &[ValType::I32, ValType::I32], &[]);
    let wasi_fd_write = registry.intern(
        type_section,
        &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    );
    let i32_i32_to_i32 =
        registry.intern(type_section, &[ValType::I32, ValType::I32], &[ValType::I32]);
    let i64_i32_to_i32 =
        registry.intern(type_section, &[ValType::I64, ValType::I32], &[ValType::I32]);
    let i64_to_i32 = registry.intern(type_section, &[ValType::I64], &[ValType::I32]);
    let f64_to_i32 = registry.intern(type_section, &[ValType::F64], &[ValType::I32]);
    let i32_i64_to_i32 =
        registry.intern(type_section, &[ValType::I32, ValType::I64], &[ValType::I32]);
    let i32_i64_i32_to_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I64, ValType::I32],
        &[ValType::I32],
    );
    let i32_i32_i64_to_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I32, ValType::I64],
        &[ValType::I32],
    );
    let i32_i32_i64_i32_to_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I32, ValType::I64, ValType::I32],
        &[ValType::I32],
    );
    let i32_i64_i64_to_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I64, ValType::I64],
        &[ValType::I32],
    );
    let i64_i64_to_i32 =
        registry.intern(type_section, &[ValType::I64, ValType::I64], &[ValType::I32]);
    let i64_i64_i32_to_i32 = registry.intern(
        type_section,
        &[ValType::I64, ValType::I64, ValType::I32],
        &[ValType::I32],
    );
    let f64_to_f64 = registry.intern(type_section, &[ValType::F64], &[ValType::F64]);
    let f64_f64_to_f64 =
        registry.intern(type_section, &[ValType::F64, ValType::F64], &[ValType::F64]);
    let i32_i32_i32_to_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I32, ValType::I32],
        &[ValType::I32],
    );
    let empty_to_i32 = registry.intern(type_section, &[], &[ValType::I32]);
    let empty_to_i32_i32 = registry.intern(type_section, &[], &[ValType::I32, ValType::I32]);
    let i32_to_i32_i32 =
        registry.intern(type_section, &[ValType::I32], &[ValType::I32, ValType::I32]);
    let i32_i64_to_empty = registry.intern(type_section, &[ValType::I32, ValType::I64], &[]);
    let i32_i64_to_i32_i32 = registry.intern(
        type_section,
        &[ValType::I32, ValType::I64],
        &[ValType::I32, ValType::I32],
    );
    let i64_i64_to_i64 =
        registry.intern(type_section, &[ValType::I64, ValType::I64], &[ValType::I64]);
    let empty_to_i64 = registry.intern(type_section, &[], &[ValType::I64]);
    let empty_to_empty = registry.intern(type_section, &[], &[]);

    RtTypeIndices {
        alloc,
        i32_to_empty,
        wrap_i64,
        wrap_f64,
        wrap_i32,
        unwrap_i64,
        unwrap_f64,
        unwrap_i32,
        obj_kind: unwrap_i32,
        obj_tag: unwrap_i32,
        obj_meta: unwrap_i32,
        obj_field_i64,
        obj_field_f64,
        obj_field_i32: i32_i32_to_i32,
        list_cons_i64,
        list_cons_f64,
        print_i64,
        print_f64,
        print_i32,
        int_to_str: i64_i32_to_i32,
        float_to_str: list_cons_f64,
        fd_write_buf,
        wasi_fd_write,
        i32_i32_to_i32,
        i64_i32_to_i32,
        i64_to_i32,
        f64_to_i32,
        i32_i64_to_i32,
        i32_i64_i32_to_i32,
        i32_i32_i64_to_i32,
        i32_i32_i64_i32_to_i32,
        i32_i64_i64_to_i32,
        i64_i64_to_i32,
        i64_i64_i32_to_i32,
        f64_to_f64,
        f64_f64_to_f64,
        i32_i32_i32_to_i32,
        empty_to_i32,
        empty_to_i32_i32,
        i32_to_i32_i32,
        i32_i64_to_empty,
        i32_i64_to_i32_i32,
        i64_i64_to_i64,
        empty_to_i64,
        empty_to_empty,
        count: registry.count(),
    }
}

/// Lookup the canonical type index for a function signature already interned in the base table.
pub fn lookup_type_index(
    rti: &RtTypeIndices,
    params: &[ValType],
    results: &[ValType],
) -> Option<u32> {
    if params == [ValType::I32] && results.is_empty() {
        return Some(rti.i32_to_empty);
    }
    if params == [ValType::I32] && results == [ValType::I32] {
        return Some(rti.unwrap_i32);
    }
    if params == [ValType::I32, ValType::I64, ValType::I32] && results == [ValType::I32] {
        return Some(rti.wrap_i64);
    }
    if params == [ValType::I32, ValType::F64] && results == [ValType::I32] {
        return Some(rti.wrap_f64);
    }
    if params == [ValType::I32, ValType::I32, ValType::I32] && results == [ValType::I32] {
        return Some(rti.wrap_i32);
    }
    if params == [ValType::I32] && results == [ValType::I64] {
        return Some(rti.unwrap_i64);
    }
    if params == [ValType::I32] && results == [ValType::F64] {
        return Some(rti.unwrap_f64);
    }
    if params == [ValType::I32, ValType::I32] && results == [ValType::I64] {
        return Some(rti.obj_field_i64);
    }
    if params == [ValType::I32, ValType::I32] && results == [ValType::F64] {
        return Some(rti.obj_field_f64);
    }
    if params == [ValType::I64, ValType::I32, ValType::I32] && results == [ValType::I32] {
        return Some(rti.list_cons_i64);
    }
    if params == [ValType::F64, ValType::I32] && results == [ValType::I32] {
        return Some(rti.list_cons_f64);
    }
    if params == [ValType::I64] && results.is_empty() {
        return Some(rti.print_i64);
    }
    if params == [ValType::F64] && results.is_empty() {
        return Some(rti.print_f64);
    }
    if params == [ValType::I32] && results.is_empty() {
        return Some(rti.print_i32);
    }
    if params == [ValType::I32, ValType::I32] && results.is_empty() {
        return Some(rti.fd_write_buf);
    }
    if params == [ValType::I32, ValType::I32, ValType::I32, ValType::I32]
        && results == [ValType::I32]
    {
        return Some(rti.wasi_fd_write);
    }
    if params == [ValType::I64, ValType::I32] && results == [ValType::I32] {
        return Some(rti.i64_i32_to_i32);
    }
    if params == [ValType::I64] && results == [ValType::I32] {
        return Some(rti.i64_to_i32);
    }
    if params == [ValType::F64] && results == [ValType::I32] {
        return Some(rti.f64_to_i32);
    }
    if params == [ValType::I32, ValType::I64] && results == [ValType::I32] {
        return Some(rti.i32_i64_to_i32);
    }
    if params == [ValType::I32, ValType::I64, ValType::I32] && results == [ValType::I32] {
        return Some(rti.i32_i64_i32_to_i32);
    }
    if params == [ValType::I32, ValType::I32, ValType::I64] && results == [ValType::I32] {
        return Some(rti.i32_i32_i64_to_i32);
    }
    if params == [ValType::I32, ValType::I32, ValType::I64, ValType::I32]
        && results == [ValType::I32]
    {
        return Some(rti.i32_i32_i64_i32_to_i32);
    }
    if params == [ValType::I32, ValType::I64, ValType::I64] && results == [ValType::I32] {
        return Some(rti.i32_i64_i64_to_i32);
    }
    if params == [ValType::I64, ValType::I64] && results == [ValType::I32] {
        return Some(rti.i64_i64_to_i32);
    }
    if params == [ValType::I64, ValType::I64, ValType::I32] && results == [ValType::I32] {
        return Some(rti.i64_i64_i32_to_i32);
    }
    if params == [ValType::F64] && results == [ValType::F64] {
        return Some(rti.f64_to_f64);
    }
    if params == [ValType::F64, ValType::F64] && results == [ValType::F64] {
        return Some(rti.f64_f64_to_f64);
    }
    if params == [ValType::I32, ValType::I32, ValType::I32] && results == [ValType::I32] {
        return Some(rti.i32_i32_i32_to_i32);
    }
    if params.is_empty() && results == [ValType::I32] {
        return Some(rti.empty_to_i32);
    }
    if params.is_empty() && results == [ValType::I32, ValType::I32] {
        return Some(rti.empty_to_i32_i32);
    }
    if params == [ValType::I32] && results == [ValType::I32, ValType::I32] {
        return Some(rti.i32_to_i32_i32);
    }
    if params == [ValType::I32, ValType::I64] && results.is_empty() {
        return Some(rti.i32_i64_to_empty);
    }
    if params == [ValType::I32, ValType::I64] && results == [ValType::I32, ValType::I32] {
        return Some(rti.i32_i64_to_i32_i32);
    }
    if params == [ValType::I64, ValType::I64] && results == [ValType::I64] {
        return Some(rti.i64_i64_to_i64);
    }
    if params.is_empty() && results == [ValType::I64] {
        return Some(rti.empty_to_i64);
    }
    if params.is_empty() && results.is_empty() {
        return Some(rti.empty_to_empty);
    }

    None
}

/// Get the type index for a given runtime function.
///
/// Note: `alloc` is an imported function (from `aver_runtime`), not a
/// local runtime function — callers iterate only over local rt fns and
/// must not pass alloc's index here.
pub fn rt_type_index(
    rt: &RuntimeFuncIndices,
    rti: &RtTypeIndices,
    func_idx: u32,
    import_func_count: u32,
) -> u32 {
    let local_idx = func_idx - import_func_count;

    if local_idx == rt.collect_begin - import_func_count {
        return rti.i32_to_empty;
    }
    if local_idx == rt.collect_end - import_func_count {
        return rti.empty_to_empty;
    }
    if local_idx == rt.rebase_i32 - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.retain_i32 - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.fd_write_buf - import_func_count {
        return rti.fd_write_buf;
    }
    if local_idx == rt.str_char_at - import_func_count {
        return rti.i32_i64_to_i32;
    }
    if local_idx == rt.char_from_code - import_func_count {
        return rti.i64_to_i32;
    }
    if local_idx == rt.str_trim - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.str_slice - import_func_count {
        return rti.i32_i32_i32_to_i32;
    }
    if local_idx == rt.str_chars - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.str_split - import_func_count {
        return rti.i32_i32_to_i32;
    }
    if local_idx == rt.str_join - import_func_count {
        return rti.i32_i32_to_i32;
    }
    if local_idx == rt.str_replace - import_func_count {
        return rti.i32_i32_i32_to_i32;
    }
    if local_idx == rt.str_to_lower - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.str_to_upper - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.int_from_str - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.float_from_str - import_func_count {
        return rti.unwrap_i32;
    }

    panic!(
        "Unknown runtime function index: {} (base={})",
        func_idx, import_func_count
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_type_section_interns_shared_signatures() {
        let mut type_section = TypeSection::new();
        let rti = emit_base_type_section(&mut type_section);

        assert_eq!(rti.alloc, rti.unwrap_i32);
        assert_eq!(rti.obj_kind, rti.unwrap_i32);
        assert_eq!(rti.obj_tag, rti.unwrap_i32);
        assert_eq!(rti.obj_meta, rti.unwrap_i32);
        assert_eq!(rti.list_cons_f64, rti.float_to_str);
    }

    #[test]
    fn abi_lookup_covers_effect_signatures() {
        let mut type_section = TypeSection::new();
        let rti = emit_base_type_section(&mut type_section);

        assert_eq!(
            lookup_type_index(&rti, &[ValType::I64, ValType::I64], &[ValType::I64]),
            Some(rti.i64_i64_to_i64)
        );
        assert_eq!(
            lookup_type_index(&rti, &[], &[ValType::I64]),
            Some(rti.empty_to_i64)
        );
        assert_eq!(
            lookup_type_index(
                &rti,
                &[ValType::I32, ValType::I64],
                &[ValType::I32, ValType::I32]
            ),
            Some(rti.i32_i64_to_i32_i32)
        );
    }
}
