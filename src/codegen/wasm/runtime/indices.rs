// Runtime function and type index definitions for the WASM backend.
//
// Contains the `RuntimeFuncIndices` struct (index assignments for all runtime
// functions), `RtTypeIndices` (type section indices), and the `rt_type_index`
// dispatch function.

/// Index assignments for runtime functions within the module.
#[derive(Debug, Clone, Copy)]
pub struct RuntimeFuncIndices {
    pub alloc: u32,
    pub wrap: u32,           // (i32, i64) -> i32
    pub wrap_f64: u32,       // (i32, f64) -> i32
    pub wrap_i32: u32,       // (i32, i32) -> i32
    pub unwrap: u32,         // (i32) -> i64
    pub unwrap_f64: u32,     // (i32) -> f64
    pub unwrap_i32: u32,     // (i32) -> i32
    pub obj_kind: u32,       // (i32) -> i32
    pub obj_tag: u32,        // (i32) -> i32
    pub obj_field: u32,      // (i32, i32) -> i64
    pub obj_field_f64: u32,  // (i32, i32) -> f64
    pub obj_field_i32: u32,  // (i32, i32) -> i32
    pub list_cons: u32,      // (i64, i32) -> i32
    pub list_cons_f64: u32,  // (f64, i32) -> i32
    pub print_i64: u32,      // (i64) -> ()
    pub print_f64: u32,      // (f64) -> ()
    pub print_string: u32,   // (i32) -> ()
    pub print_bool: u32,     // (i32) -> ()
    pub print_heap: u32,     // (i32) -> ()
    pub int_to_str: u32,     // (i64, i32) -> i32
    pub float_to_str: u32,   // (f64, i32) -> i32
    pub fd_write_buf: u32,   // (i32, i32) -> ()
    pub str_eq: u32,         // (i32, i32) -> i32
    pub str_concat: u32,     // (i32, i32) -> i32
    pub i64_to_str_obj: u32, // (i64) -> i32
    pub f64_to_str_obj: u32, // (f64) -> i32
    pub list_take: u32,      // (i32, i64) -> i32
    pub list_drop: u32,      // (i32, i64) -> i32
    pub list_concat: u32,    // (i32, i32) -> i32
    pub list_reverse: u32,   // (i32) -> i32
    pub list_contains: u32,  // (i64, i32) -> i32 (value, list) -> bool
    pub list_zip: u32,       // (i32, i32) -> i32
    pub map_get: u32,        // (i32, i32) -> i32  (map, key) -> Option ptr
    pub map_set: u32,        // (i32, i32, i64) -> i32  (map, key, value) -> map
    pub map_has: u32,        // (i32, i32) -> i32  (map, key) -> bool
    pub map_keys: u32,       // (i32) -> i32  (map) -> list
    pub print_value: u32,    // (i64) -> ()  generic value printer
    pub vec_from_list: u32,  // (i32) -> i32  list → vector
    pub vec_get: u32,        // (i32, i64) -> i32  (vec, idx) → Option
    pub vec_len: u32,        // (i32) -> i64  vec → Int
    pub vec_set: u32,        // (i32, i64, i64) -> i32  (vec, idx, val) → Option<Vector>
    pub vec_new: u32,        // (i64, i64) -> i32  (size, fill) → vec
    pub str_trim: u32,       // (i32) -> i32  trim whitespace
    pub str_slice: u32,      // (i32, i32, i32) -> i32  substring
    pub str_chars: u32,      // (i32) -> i32  string → list of chars
    pub str_join: u32,       // (i32, i32) -> i32  join list with separator
    pub int_from_str: u32,   // (i32) -> i32  parse int → Result wrapper
    /// Total number of runtime functions.
    pub count: u32,
    /// Import function index for writing to stdout (either WASI fd_write or aver/console_print).
    pub fd_write_import: u32,
    /// Which adapter mode is active.
    pub adapter: super::super::WasmAdapter,
}

impl RuntimeFuncIndices {
    pub fn new(base: u32) -> Self {
        let mut i = base;
        let mut next = || {
            let idx = i;
            i += 1;
            idx
        };
        RuntimeFuncIndices {
            alloc: next(),
            wrap: next(),
            wrap_f64: next(),
            wrap_i32: next(),
            unwrap: next(),
            unwrap_f64: next(),
            unwrap_i32: next(),
            obj_kind: next(),
            obj_tag: next(),
            obj_field: next(),
            obj_field_f64: next(),
            obj_field_i32: next(),
            list_cons: next(),
            list_cons_f64: next(),
            print_i64: next(),
            print_f64: next(),
            print_string: next(),
            print_bool: next(),
            print_heap: next(),
            int_to_str: next(),
            float_to_str: next(),
            fd_write_buf: next(),
            str_eq: next(),
            str_concat: next(),
            i64_to_str_obj: next(),
            f64_to_str_obj: next(),
            list_take: next(),
            list_drop: next(),
            list_concat: next(),
            list_reverse: next(),
            list_contains: next(),
            list_zip: next(),
            map_get: next(),
            map_set: next(),
            map_has: next(),
            map_keys: next(),
            print_value: next(),
            vec_from_list: next(),
            vec_get: next(),
            vec_len: next(),
            vec_set: next(),
            vec_new: next(),
            str_trim: next(),
            str_slice: next(),
            str_chars: next(),
            str_join: next(),
            int_from_str: next(),
            count: i - base,
            fd_write_import: 0,
            adapter: super::super::WasmAdapter::Aver,
        }
    }
}

/// Runtime function type signatures. Indices into the type section.
/// These must match the order in emitter.rs type_section construction.
#[derive(Debug, Clone, Copy)]
pub struct RtTypeIndices {
    pub alloc: u32,          // 0: (i32) -> i32
    pub wrap_i64: u32,       // 1: (i32, i64) -> i32
    pub wrap_f64: u32,       // 2: (i32, f64) -> i32
    pub wrap_i32: u32,       // 3: (i32, i32) -> i32
    pub unwrap_i64: u32,     // 4: (i32) -> i64
    pub unwrap_f64: u32,     // 5: (i32) -> f64
    pub unwrap_i32: u32,     // 6: (i32) -> i32
    pub obj_kind: u32,       // 7: (i32) -> i32  (same as unwrap_i32)
    pub obj_tag: u32,        // 8: (i32) -> i32  (same as unwrap_i32)
    pub obj_field_i64: u32,  // 9: (i32, i32) -> i64
    pub obj_field_f64: u32,  // 10: (i32, i32) -> f64
    pub obj_field_i32: u32,  // 11: (i32, i32) -> i32
    pub list_cons_i64: u32,  // 12: (i64, i32) -> i32
    pub list_cons_f64: u32,  // 13: (f64, i32) -> i32
    pub print_i64: u32,      // 14: (i64) -> ()
    pub print_f64: u32,      // 15: (f64) -> ()
    pub print_i32_void: u32, // 16: (i32) -> ()
    pub int_to_str: u32,     // 17: (i64, i32) -> i32
    pub float_to_str: u32,   // 18: (f64, i32) -> i32
    pub fd_write_buf: u32,   // 19: (i32, i32) -> ()
    pub wasi_fd_write: u32,  // 20: (i32, i32, i32, i32) -> i32
}

/// Get the type index for a given runtime function.
pub fn rt_type_index(
    rt: &RuntimeFuncIndices,
    rti: &RtTypeIndices,
    func_idx: u32,
    import_func_count: u32,
) -> u32 {
    let local_idx = func_idx - import_func_count;
    let alloc_local = rt.alloc - import_func_count;

    if local_idx == alloc_local {
        return rti.alloc;
    }
    if local_idx == rt.wrap - import_func_count {
        return rti.wrap_i64;
    }
    if local_idx == rt.wrap_f64 - import_func_count {
        return rti.wrap_f64;
    }
    if local_idx == rt.wrap_i32 - import_func_count {
        return rti.wrap_i32;
    }
    if local_idx == rt.unwrap - import_func_count {
        return rti.unwrap_i64;
    }
    if local_idx == rt.unwrap_f64 - import_func_count {
        return rti.unwrap_f64;
    }
    if local_idx == rt.unwrap_i32 - import_func_count {
        return rti.unwrap_i32;
    }
    if local_idx == rt.obj_kind - import_func_count {
        return rti.obj_kind;
    }
    if local_idx == rt.obj_tag - import_func_count {
        return rti.obj_tag;
    }
    if local_idx == rt.obj_field - import_func_count {
        return rti.obj_field_i64;
    }
    if local_idx == rt.obj_field_f64 - import_func_count {
        return rti.obj_field_f64;
    }
    if local_idx == rt.obj_field_i32 - import_func_count {
        return rti.obj_field_i32;
    }
    if local_idx == rt.list_cons - import_func_count {
        return rti.list_cons_i64;
    }
    if local_idx == rt.list_cons_f64 - import_func_count {
        return rti.list_cons_f64;
    }
    if local_idx == rt.print_i64 - import_func_count {
        return rti.print_i64;
    }
    if local_idx == rt.print_f64 - import_func_count {
        return rti.print_f64;
    }
    if local_idx == rt.print_string - import_func_count {
        return rti.print_i32_void;
    }
    if local_idx == rt.print_bool - import_func_count {
        return rti.print_i32_void;
    }
    if local_idx == rt.print_heap - import_func_count {
        return rti.print_i32_void;
    }
    if local_idx == rt.int_to_str - import_func_count {
        return rti.int_to_str;
    }
    if local_idx == rt.float_to_str - import_func_count {
        return rti.float_to_str;
    }
    if local_idx == rt.fd_write_buf - import_func_count {
        return rti.fd_write_buf;
    }
    if local_idx == rt.str_eq - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.str_concat - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.i64_to_str_obj - import_func_count {
        return 18;
    } // (i64)->i32
    if local_idx == rt.f64_to_str_obj - import_func_count {
        return 19;
    } // (f64)->i32
    if local_idx == rt.list_take - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.list_drop - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.list_concat - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.list_reverse - import_func_count {
        return rti.alloc;
    } // (i32)->i32
    if local_idx == rt.list_contains - import_func_count {
        return 20;
    } // (i32,i64)->i32
    if local_idx == rt.list_zip - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.map_get - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.map_set - import_func_count {
        return 21;
    } // (i32,i32,i64)->i32
    if local_idx == rt.map_has - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.map_keys - import_func_count {
        return rti.alloc;
    } // (i32)->i32
    if local_idx == rt.print_value - import_func_count {
        return rti.print_i64;
    } // (i64)->()
    if local_idx == rt.vec_from_list - import_func_count {
        return rti.alloc;
    } // (i32)->i32
    if local_idx == rt.vec_get - import_func_count {
        return 20;
    } // (i32,i64)->i32
    if local_idx == rt.vec_len - import_func_count {
        return rti.unwrap_i64;
    } // (i32)->i64
    if local_idx == rt.vec_set - import_func_count {
        return 22;
    } // (i32,i64,i64)->i32
    if local_idx == rt.vec_new - import_func_count {
        return 23;
    } // (i64,i64)->i32
    if local_idx == rt.str_trim - import_func_count {
        return rti.alloc;
    } // (i32)->i32
    if local_idx == rt.str_slice - import_func_count {
        return 24;
    } // (i32,i32,i32)->i32 — new type
    if local_idx == rt.str_chars - import_func_count {
        return rti.alloc;
    } // (i32)->i32
    if local_idx == rt.str_join - import_func_count {
        return rti.wrap_i32;
    } // (i32,i32)->i32
    if local_idx == rt.int_from_str - import_func_count {
        return rti.alloc;
    } // (i32)->i32

    panic!(
        "Unknown runtime function index: {} (base={})",
        func_idx, import_func_count
    );
}
