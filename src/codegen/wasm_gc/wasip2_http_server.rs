//! Phase 3.0 — `aver_http_handle(request, outparam) -> ()` body
//! emitter for `--target wasip2 --world wasi:http/proxy`.
//!
//! The wasi:http/proxy world exports `wasi:http/incoming-handler.
//! handle(request: own<incoming-request>, response: own<response-
//! outparam>)`. The host calls into us once per inbound HTTP
//! request; we decode the incoming-request resource into an Aver
//! `HttpRequest`, run the user's source-level handler fn, encode
//! the returned `HttpResponse` into an outgoing-response resource,
//! and call `response-outparam.set` — the host then writes the
//! response bytes back to the client.
//!
//! Pipeline (mirrors the client in `wasip2_http.rs`, inverted):
//!
//! 1. Read `[method]incoming-request.method` → variant (12-byte
//!    retptr). Decode the 10-case discriminator into an Aver String
//!    (`"GET"`, `"POST"`, …, or the `other(string)` payload).
//! 2. Read `[method]incoming-request.path-with-query` →
//!    `option<string>` (12-byte retptr). When Some, split on the
//!    first `?` into path + query; when None, emit `path = "/"`,
//!    `query = ""`.
//! 3. Read `[method]incoming-request.headers` → `own<fields>`
//!    handle (flat i32). Iterate `[method]fields.entries` →
//!    `list<tuple<string, list<u8>>>` and materialise a
//!    `Map<String, List<String>>` mirroring the client's response-
//!    header walk (reverse-iterate + Map.set on prepended list).
//! 4. Read `[method]incoming-request.consume` →
//!    `result<incoming-body>` (8-byte retptr). On Ok, take the
//!    body handle.
//! 5. Read `[method]incoming-body.stream` → `result<input-stream>`
//!    (8-byte retptr). On Ok, take the stream handle.
//! 6. Drain the input-stream with blocking-read into a growing
//!    `cabi_realloc` buffer (same loop the client uses to drain
//!    response bodies); `closed` is EOF.
//! 7. Drop input-stream; call `[static]incoming-body.finish`
//!    (takes ownership of body) → future-trailers; drop trailers.
//!    Drop headers fields (child resource of incoming-request).
//! 8. Build `HttpRequest` via `struct.new` over (method, path,
//!    query, body_string, headers_map).
//! 9. Call user handler — wasm fn idx known at module-emit time.
//! 10. Extract `HttpResponse.{status, body, headers}` from the
//!     returned struct ref.
//! 11. Allocate response fields, append every (key, value) pair
//!     from the response Map, plus a synthesised `Content-Length`.
//!     Construct `outgoing-response` (takes ownership of fields).
//! 12. `set-status-code(out_response, status_u16)` — inline result
//!     tag, ignored.
//! 13. `outgoing-response.body` → outgoing-body (8-byte retptr Ok).
//!     `outgoing-body.write` → output-stream (8-byte retptr Ok).
//!     Chunked blocking-write-and-flush over the response bytes,
//!     drop output-stream, `outgoing-body.finish(None)` (transfers
//!     body ownership).
//! 14. `response-outparam.set(outparam, Ok(out_response))` —
//!     consumes both the outparam and the outgoing-response handle,
//!     so no further drops on the happy path. Drop incoming-request
//!     at the end (all child resources already gone).
//!
//! v1 PoC scope:
//! - Single-request lifecycle: body materialised as one Aver String
//!   (no streaming). Suits Aver's pure-fn handler shape; chunked
//!   responses arrive as a single buffer here.
//! - No trailers in either direction. `outgoing-body.finish` always
//!   passes `option<trailers> = None`; incoming trailers are
//!   silently dropped.
//! - Errors past step 4 (consume / stream / blocking-read non-
//!   closed Err) trap via `unreachable` — a real production server
//!   would surface them via `response-outparam.set(Err(error-code))`,
//!   but that requires composing the 7-position canonical-ABI
//!   error-code variant which is a follow-up. Phase 1 of 0.19 keeps
//!   it strict: malformed requests get a wasm trap, not a quiet 5xx.

use wasm_encoder::{Function, ValType};

/// Per-helper allocation metadata, populated in `module.rs` after
/// every dependent slot exists (types, imports, helpers, user
/// handler fn idx). Same shape as `HttpGetIndices` on the client
/// side.
pub(super) struct ServerHandlerIndices {
    /// Reserved for parity with `HttpGetIndices` — kept so future
    /// follow-ups that move the proxy body out of the `_start` slot
    /// into its own fn idx have a place to plug in. The `_start`
    /// path consumes these from `start_type_idx` / `start_wasm_idx`
    /// directly, hence `#[allow(dead_code)]`.
    #[allow(dead_code)]
    pub fn_type: u32,
    #[allow(dead_code)]
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `HttpRequest` struct type idx (method, path, query, body,
    /// headers).
    pub http_request_type_idx: u32,
    /// `HttpResponse` struct type idx (status, body, headers).
    pub http_response_type_idx: u32,
    /// `Map<String, List<String>>` slot triple — used for both the
    /// request-headers in (built bottom-up from fields.entries) and
    /// the response-headers iteration (read field-by-field, append
    /// to outgoing fields).
    pub headers_keys_array_type_idx: u32,
    pub headers_values_array_type_idx: u32,
    pub headers_map_type_idx: u32,
    /// `List<String>` cons-cell type idx — head = String ref, tail
    /// = list ref.
    pub list_string_type_idx: u32,
    /// `Option<List<String>>` struct type idx — returned by
    /// `Map.get` when probing for an existing entry during the
    /// request-headers build.
    pub option_list_string_type_idx: u32,
}

/// Bundle of wasm fn indices the body references via `Call(idx)`.
/// Same idea as `HttpGetHelperFns` on the client. Splitting from
/// `ServerHandlerIndices` keeps the `emit_aver_http_handle`
/// signature small.
pub(super) struct ServerHandlerHelperFns {
    pub cabi_realloc_fn: u32,
    pub str_to_lm_fn: u32,
    pub from_lm_fn: u32,

    // Incoming-request side.
    pub incoming_request_method_fn: u32,
    pub incoming_request_path_with_query_fn: u32,
    pub incoming_request_headers_fn: u32,
    pub incoming_request_consume_fn: u32,
    pub drop_incoming_request_fn: u32,

    // Headers reading reuses the client's slots.
    pub fields_entries_fn: u32,
    pub drop_fields_fn: u32,

    // Incoming body reading reuses the client's slots.
    pub incoming_body_stream_fn: u32,
    pub incoming_body_finish_fn: u32,
    pub drop_incoming_body_fn: u32,
    pub blocking_read_fn: u32,
    pub drop_input_stream_fn: u32,
    pub drop_future_trailers_fn: u32,

    // Outgoing-response side.
    pub fields_new_fn: u32,
    pub fields_append_fn: u32,
    pub outgoing_response_new_fn: u32,
    pub set_status_code_fn: u32,
    pub outgoing_response_body_fn: u32,
    pub outgoing_body_write_fn: u32,
    pub outgoing_body_finish_fn: u32,
    pub blocking_write_fn: u32,
    pub drop_output_stream_fn: u32,
    pub drop_outgoing_body_fn: u32,

    pub response_outparam_set_fn: u32,

    // Map<String, List<String>> per-instantiation helpers.
    pub map_set_fn: u32,
    pub map_get_fn: u32,

    /// The user's `(HttpRequest) -> HttpResponse` handler fn —
    /// resolved at module emit time from the `--handler` name.
    pub user_handler_fn: u32,
}

/// Same `INITIAL_CAP` as `emit_map_empty` in `maps.rs`. The
/// request-headers map allocates this once per inbound request;
/// follow-up work can shrink it when small-headers becomes the
/// hot path.
const INITIAL_CAP: i32 = 16384;

/// Canonical HTTP method discriminants per `wasi:http/types.method`
/// variant. Order MUST match the WIT — the host writes the raw
/// disc byte at retptr+0.
const METHOD_NAMES: &[&[u8]] = &[
    b"GET", b"HEAD", b"POST", b"PUT", b"DELETE", b"CONNECT", b"OPTIONS", b"TRACE", b"PATCH",
];

/// Emit the body of `aver_http_handle(request: i32, outparam: i32)
/// -> ()`. See the module-level docstring for the pipeline.
pub(super) fn emit_aver_http_handle(
    indices: &ServerHandlerIndices,
    h: &ServerHandlerHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, Instruction, MemArg, RefType};

    let string_idx = indices.string_type_idx;
    let req_idx = indices.http_request_type_idx;
    let resp_idx = indices.http_response_type_idx;
    let keys_arr_idx = indices.headers_keys_array_type_idx;
    let values_arr_idx = indices.headers_values_array_type_idx;
    let map_idx = indices.headers_map_type_idx;
    let list_str_idx = indices.list_string_type_idx;
    let opt_list_str_idx = indices.option_list_string_type_idx;

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_idx),
    });
    let req_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(req_idx),
    });
    let resp_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(resp_idx),
    });
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(map_idx),
    });
    let keys_arr_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(keys_arr_idx),
    });
    let values_arr_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(values_arr_idx),
    });
    let list_str_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_str_idx),
    });
    let opt_list_str_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(opt_list_str_idx),
    });

    // Locals layout — params first (req, outparam), then i32s
    // packed dense, then ref-typed locals at the end.
    //
    //  0  = req       (i32)  [param]  incoming-request handle
    //  1  = outparam  (i32)  [param]  response-outparam handle
    //  2  = retptr_method   (i32) 12 bytes
    //  3  = retptr_path     (i32) 12 bytes
    //  4  = method_disc     (i32) variant disc 0..=9
    //  5  = path_opt_tag    (i32) option<string> disc 0|1
    //  6  = url_ptr         (i32) host-allocated path-with-query ptr
    //  7  = url_len         (i32) host-allocated path-with-query len
    //  8  = q_pos           (i32) '?' position (= url_len when none)
    //  9  = path_len        (i32)
    // 10  = query_len       (i32)
    // 11  = body_buf        (i32) growing body buffer
    // 12  = body_cap        (i32)
    // 13  = body_len        (i32)
    // 14  = data_ptr        (i32) per-iter blocking-read data ptr
    // 15  = data_len        (i32)
    // 16  = retptr_read     (i32) 12-byte blocking-read retptr
    // 17  = new_cap         (i32)
    // 18  = k               (i32) byte-copy iterator
    // 19  = req_fields      (i32) incoming-request.headers handle
    // 20  = retptr_entries  (i32) 8 bytes
    // 21  = entries_ptr     (i32)
    // 22  = entries_len     (i32)
    // 23  = h_idx           (i32) entries iteration cursor
    // 24  = h_entry_addr    (i32)
    // 25  = h_name_ptr      (i32)
    // 26  = h_name_len      (i32)
    // 27  = h_val_ptr       (i32)
    // 28  = h_val_len       (i32)
    // 29  = retptr_consume  (i32) 8 bytes
    // 30  = body_handle     (i32)
    // 31  = retptr_stream   (i32) 8 bytes
    // 32  = stream          (i32) input-stream handle
    // 33  = trailers        (i32) future-trailers handle
    // 34  = out_fields      (i32) response fields handle
    // 35  = ob_retptr_4     (i32) reusable 4-byte fields.append retptr
    // 36  = uh_idx          (i32) response-headers iter cursor
    // 37  = uh_cap          (i32) response-headers map cap
    // 38  = uh_key_len      (i32)
    // 39  = uh_val_len      (i32)
    // 40  = uh_key_buf      (i32) cabi_realloc'd key bytes
    // 41  = cl_body_len     (i32)
    // 42  = cl_buf          (i32) 16-byte int→decimal scratch
    // 43  = cl_pos          (i32)
    // 44  = cl_n            (i32)
    // 45  = out_response    (i32) outgoing-response handle
    // 46  = ob_retptr       (i32) 8-byte retptr for body() / write()
    // 47  = ob_handle       (i32) outgoing-body handle
    // 48  = ob_stream       (i32) outgoing output-stream handle
    // 49  = ob_finish_retptr(i32) 40-byte retptr for body.finish
    // 50  = ob_body_len     (i32) response body byte count after to_lm
    // 51  = ob_off          (i32) chunked-write offset
    //
    // Ref locals start at 52:
    // 52  = method_str (ref string)
    // 53  = path_str   (ref string)
    // 54  = query_str  (ref string)
    // 55  = body_str   (ref string)
    // 56  = h_name_str (ref string)
    // 57  = h_val_str  (ref string)
    // 58  = uh_key     (ref string)
    // 59  = uh_val     (ref string)
    // 60  = uh_keys    (ref keys_arr)
    // 61  = uh_values  (ref values_arr)
    // 62  = uh_node    (ref list_string)
    // 63  = req_headers_map  (ref map)  built from incoming-request.headers
    // 64  = resp_headers_map (ref map)  pulled out of HttpResponse
    // 65  = h_opt      (ref Option<List<String>>)
    // 66  = h_tail     (ref list_string)
    // 67  = req_struct (ref HttpRequest)
    // 68  = resp_struct (ref HttpResponse)
    let i32_count = 52u32 - 2; // 50 i32 locals (after the 2 params)
    let mut f = Function::new([
        (i32_count, ValType::I32),
        (8, s_ref),
        (1, keys_arr_ref),
        (1, values_arr_ref),
        (1, list_str_ref),
        (2, map_ref),
        (1, opt_list_str_ref),
        (1, list_str_ref),
        (1, req_ref),
        (1, resp_ref),
    ]);

    let p_req = 0u32;
    let p_outparam = 1u32;
    let l_retptr_method = 2u32;
    let l_retptr_path = 3u32;
    let l_method_disc = 4u32;
    let l_path_opt_tag = 5u32;
    let l_url_ptr = 6u32;
    let l_url_len = 7u32;
    let l_q_pos = 8u32;
    let l_path_len = 9u32;
    let l_query_len = 10u32;
    let l_body_buf = 11u32;
    let l_body_cap = 12u32;
    let l_body_len = 13u32;
    let l_data_ptr = 14u32;
    let l_data_len = 15u32;
    let l_retptr_read = 16u32;
    let l_new_cap = 17u32;
    let l_k = 18u32;
    let l_req_fields = 19u32;
    let l_retptr_entries = 20u32;
    let l_entries_ptr = 21u32;
    let l_entries_len = 22u32;
    let l_h_idx = 23u32;
    let l_h_entry_addr = 24u32;
    let l_h_name_ptr = 25u32;
    let l_h_name_len = 26u32;
    let l_h_val_ptr = 27u32;
    let l_h_val_len = 28u32;
    let l_retptr_consume = 29u32;
    let l_body_handle = 30u32;
    let l_retptr_stream = 31u32;
    let l_stream = 32u32;
    let l_trailers = 33u32;
    let l_out_fields = 34u32;
    let l_ob_retptr_4 = 35u32;
    let l_uh_idx = 36u32;
    let l_uh_cap = 37u32;
    let l_uh_key_len = 38u32;
    let l_uh_val_len = 39u32;
    let l_uh_key_buf = 40u32;
    let l_cl_body_len = 41u32;
    let l_cl_buf = 42u32;
    let l_cl_pos = 43u32;
    let l_cl_n = 44u32;
    let l_out_response = 45u32;
    let l_ob_retptr = 46u32;
    let l_ob_handle = 47u32;
    let l_ob_stream = 48u32;
    let l_ob_finish_retptr = 49u32;
    let l_ob_body_len = 50u32;
    let l_ob_off = 51u32;

    let l_method_str = 52u32;
    let l_path_str = 53u32;
    let l_query_str = 54u32;
    let l_body_str = 55u32;
    let l_h_name_str = 56u32;
    let l_h_val_str = 57u32;
    let l_uh_key = 58u32;
    let l_uh_val = 59u32;
    let l_uh_keys = 60u32;
    let l_uh_values = 61u32;
    let l_uh_node = 62u32;
    let l_req_headers_map = 63u32;
    let l_resp_headers_map = 64u32;
    let l_h_opt = 65u32;
    let l_h_tail = 66u32;
    let l_req_struct = 67u32;
    let l_resp_struct = 68u32;

    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let mem1_o4 = MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    };
    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_o8 = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };

    // Inline helper: allocate `(array i8)` of `bytes.len()` and store
    // `bytes` into it, leaving the ref on the wasm stack. Used for
    // method-name literals and other static strings the handler
    // materialises (path = "/", query = "").
    let push_static_str = |f: &mut Function, bytes: &[u8]| {
        f.instruction(&Instruction::I32Const(bytes.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_idx));
        // Stack: arr
        for (i, b) in bytes.iter().enumerate() {
            f.instruction(&Instruction::LocalTee(l_method_str));
            // Stack: arr arr
            // overwriting l_method_str is fine — this closure is called
            // either to build the method ref (which we then LocalSet
            // l_method_str) or for path/query/empty-body literals where
            // we LocalSet the dedicated ref local right after.
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_idx));
            f.instruction(&Instruction::LocalGet(l_method_str));
        }
        // Stack: arr  (refreshed via LocalGet on the last iteration)
        // For empty bytes the LocalTee loop never ran, so the stack
        // still has the original arr from ArrayNewDefault.
        if bytes.is_empty() {
            // Already on stack — nothing to do.
        }
    };

    // ── 1. Read incoming-request.method into l_method_str. ─────
    //
    // method variant flat retptr layout (12 bytes, align 4):
    //   +0: disc i32 (0..=9)
    //   +4: payload str_ptr i32 (only when disc == 9)
    //   +8: payload str_len i32 (only when disc == 9)
    //
    // For disc 0..=8 we look up the canonical METHOD_NAMES bytes and
    // emit them inline. For disc == 9 we materialise the host string
    // via __rt_string_from_lm (after memory.copy from the host's
    // bump-heap ptr into LM[0..len]).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_method));

    f.instruction(&Instruction::LocalGet(p_req));
    f.instruction(&Instruction::LocalGet(l_retptr_method));
    f.instruction(&Instruction::Call(h.incoming_request_method_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_method));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::LocalSet(l_method_disc));

    // Branch per known disc. Each arm pushes the matching byte
    // sequence and falls through to a final LocalSet at the bottom.
    for (disc, name) in METHOD_NAMES.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(l_method_disc));
        f.instruction(&Instruction::I32Const(disc as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        push_static_str(&mut f, name);
        f.instruction(&Instruction::LocalSet(l_method_str));
        f.instruction(&Instruction::End);
    }
    // disc == 9: other(string). Decode (ptr, len) at retptr+4..+12,
    // memory.copy into LM[0..len], call from_lm.
    f.instruction(&Instruction::LocalGet(l_method_disc));
    f.instruction(&Instruction::I32Const(9));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        let str_ptr_l = l_data_ptr; // reuse i32 scratch
        let str_len_l = l_data_len;
        f.instruction(&Instruction::LocalGet(l_retptr_method));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(str_ptr_l));
        f.instruction(&Instruction::LocalGet(l_retptr_method));
        f.instruction(&Instruction::I32Load(mem4_o8));
        f.instruction(&Instruction::LocalSet(str_len_l));
        // memory.copy LM[0..len] = bytes[str_ptr..str_ptr+len]
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(str_ptr_l));
        f.instruction(&Instruction::LocalGet(str_len_l));
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        f.instruction(&Instruction::LocalGet(str_len_l));
        f.instruction(&Instruction::Call(h.from_lm_fn));
        f.instruction(&Instruction::LocalSet(l_method_str));
    }
    f.instruction(&Instruction::End);

    // ── 2. Read incoming-request.path-with-query into
    //    l_path_str + l_query_str. option<string> retptr (12 bytes
    //    align 4): +0 opt_tag, +4 str_ptr, +8 str_len.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_path));

    f.instruction(&Instruction::LocalGet(p_req));
    f.instruction(&Instruction::LocalGet(l_retptr_path));
    f.instruction(&Instruction::Call(h.incoming_request_path_with_query_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_path));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::LocalSet(l_path_opt_tag));

    f.instruction(&Instruction::LocalGet(l_path_opt_tag));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Some(string) — read (ptr, len), copy into LM, find '?'.
        f.instruction(&Instruction::LocalGet(l_retptr_path));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_url_ptr));
        f.instruction(&Instruction::LocalGet(l_retptr_path));
        f.instruction(&Instruction::I32Load(mem4_o8));
        f.instruction(&Instruction::LocalSet(l_url_len));

        // memory.copy LM[0..url_len] from host ptr.
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_url_ptr));
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

        // q_pos = url_len; scan LM[0..url_len] for '?'.
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::LocalSet(l_q_pos));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_k));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::LocalGet(l_url_len));
            f.instruction(&Instruction::I32GeU);
            f.instruction(&Instruction::BrIf(1));

            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Load8U(mem1));
            f.instruction(&Instruction::I32Const(b'?' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::If(BlockType::Empty));
            {
                f.instruction(&Instruction::LocalGet(l_k));
                f.instruction(&Instruction::LocalSet(l_q_pos));
                f.instruction(&Instruction::Br(2)); // exit outer block
            }
            f.instruction(&Instruction::End);

            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(l_k));
            f.instruction(&Instruction::Br(0));
        }
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);

        // path_len = q_pos (0..q_pos before '?'); query_len = max(0,
        // url_len - q_pos - 1). When no '?' present q_pos == url_len
        // so query_len = -1 logically — clamp to 0.
        f.instruction(&Instruction::LocalGet(l_q_pos));
        f.instruction(&Instruction::LocalSet(l_path_len));

        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::LocalGet(l_q_pos));
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_url_len));
            f.instruction(&Instruction::LocalGet(l_q_pos));
            f.instruction(&Instruction::I32Sub);
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Sub);
            f.instruction(&Instruction::LocalSet(l_query_len));
        }
        f.instruction(&Instruction::Else);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_query_len));
        f.instruction(&Instruction::End);

        // Build path: allocate $string of path_len, copy bytes from
        // LM[0..path_len].
        f.instruction(&Instruction::LocalGet(l_path_len));
        f.instruction(&Instruction::ArrayNewDefault(string_idx));
        f.instruction(&Instruction::LocalSet(l_path_str));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_k));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::LocalGet(l_path_len));
            f.instruction(&Instruction::I32GeU);
            f.instruction(&Instruction::BrIf(1));

            f.instruction(&Instruction::LocalGet(l_path_str));
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Load8U(mem1));
            f.instruction(&Instruction::ArraySet(string_idx));

            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(l_k));
            f.instruction(&Instruction::Br(0));
        }
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);

        // Build query: allocate, copy bytes from LM[q_pos+1..url_len].
        f.instruction(&Instruction::LocalGet(l_query_len));
        f.instruction(&Instruction::ArrayNewDefault(string_idx));
        f.instruction(&Instruction::LocalSet(l_query_str));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_k));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::LocalGet(l_query_len));
            f.instruction(&Instruction::I32GeU);
            f.instruction(&Instruction::BrIf(1));

            f.instruction(&Instruction::LocalGet(l_query_str));
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::LocalGet(l_q_pos));
            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Load8U(mem1));
            f.instruction(&Instruction::ArraySet(string_idx));

            f.instruction(&Instruction::LocalGet(l_k));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(l_k));
            f.instruction(&Instruction::Br(0));
        }
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::Else);
    {
        // None — default to path = "/", query = "".
        push_static_str(&mut f, b"/");
        f.instruction(&Instruction::LocalSet(l_path_str));
        push_static_str(&mut f, b"");
        f.instruction(&Instruction::LocalSet(l_query_str));
    }
    f.instruction(&Instruction::End);

    // ── 3. Read incoming-request.headers → fields handle, walk
    //    entries into Map<String, List<String>>. Multi-valued
    //    headers (Set-Cookie etc.) accumulate via Map.get +
    //    cons-prepend — same logic as the client's response-
    //    header loop in wasip2_http.rs.
    f.instruction(&Instruction::LocalGet(p_req));
    f.instruction(&Instruction::Call(h.incoming_request_headers_fn));
    f.instruction(&Instruction::LocalSet(l_req_fields));

    // Initialise empty map (matches emit_map_empty).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(keys_arr_idx));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(values_arr_idx));
    f.instruction(&Instruction::StructNew(map_idx));
    f.instruction(&Instruction::LocalSet(l_req_headers_map));

    // fields.entries(handle, retptr) — 8 bytes (entries_ptr, entries_len).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_entries));

    f.instruction(&Instruction::LocalGet(l_req_fields));
    f.instruction(&Instruction::LocalGet(l_retptr_entries));
    f.instruction(&Instruction::Call(h.fields_entries_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_entries));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_entries_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_entries));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_entries_len));

    // Reverse iterate so prepended cons cells preserve the host's
    // emission order — same trick the client uses for Set-Cookie.
    f.instruction(&Instruction::LocalGet(l_entries_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_h_idx));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32LtS);
        f.instruction(&Instruction::BrIf(1));

        // entry_addr = entries_ptr + h_idx * 16
        f.instruction(&Instruction::LocalGet(l_entries_ptr));
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Mul);
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_h_entry_addr));

        f.instruction(&Instruction::LocalGet(l_h_entry_addr));
        f.instruction(&Instruction::I32Load(mem4));
        f.instruction(&Instruction::LocalSet(l_h_name_ptr));
        f.instruction(&Instruction::LocalGet(l_h_entry_addr));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_h_name_len));
        f.instruction(&Instruction::LocalGet(l_h_entry_addr));
        f.instruction(&Instruction::I32Load(mem4_o8));
        f.instruction(&Instruction::LocalSet(l_h_val_ptr));
        f.instruction(&Instruction::LocalGet(l_h_entry_addr));
        f.instruction(&Instruction::I32Load(MemArg {
            offset: 12,
            align: 2,
            memory_index: 0,
        }));
        f.instruction(&Instruction::LocalSet(l_h_val_len));

        // name → LM[0..name_len] → from_lm
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_h_name_ptr));
        f.instruction(&Instruction::LocalGet(l_h_name_len));
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        f.instruction(&Instruction::LocalGet(l_h_name_len));
        f.instruction(&Instruction::Call(h.from_lm_fn));
        f.instruction(&Instruction::LocalSet(l_h_name_str));

        // value → LM[0..val_len] → from_lm
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_h_val_ptr));
        f.instruction(&Instruction::LocalGet(l_h_val_len));
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        f.instruction(&Instruction::LocalGet(l_h_val_len));
        f.instruction(&Instruction::Call(h.from_lm_fn));
        f.instruction(&Instruction::LocalSet(l_h_val_str));

        // existing = Map.get(map, name_str)
        f.instruction(&Instruction::LocalGet(l_req_headers_map));
        f.instruction(&Instruction::LocalGet(l_h_name_str));
        f.instruction(&Instruction::Call(h.map_get_fn));
        f.instruction(&Instruction::LocalSet(l_h_opt));

        // tail = Some(prev) ? prev : ref.null
        f.instruction(&Instruction::LocalGet(l_h_opt));
        f.instruction(&Instruction::StructGet {
            struct_type_index: opt_list_str_idx,
            field_index: 0,
        });
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Result(list_str_ref)));
        {
            f.instruction(&Instruction::LocalGet(l_h_opt));
            f.instruction(&Instruction::StructGet {
                struct_type_index: opt_list_str_idx,
                field_index: 1,
            });
        }
        f.instruction(&Instruction::Else);
        f.instruction(&Instruction::RefNull(HeapType::Concrete(list_str_idx)));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::LocalSet(l_h_tail));

        // new_list = struct.new $list_string (val_str, tail)
        f.instruction(&Instruction::LocalGet(l_h_val_str));
        f.instruction(&Instruction::LocalGet(l_h_tail));
        f.instruction(&Instruction::StructNew(list_str_idx));

        // Map.set(map, name_str, new_list) — note arg order: pre-push map + key.
        // Easier: build set call directly. emit_http_get uses helpers'
        // expectations of (map, key, value) on stack.
        // We already have new_list on stack; need to pre-stack (map, key).
        // Re-order: store new_list, then push map + key + new_list.
        f.instruction(&Instruction::LocalSet(l_h_tail)); // reuse as scratch
        f.instruction(&Instruction::LocalGet(l_req_headers_map));
        f.instruction(&Instruction::LocalGet(l_h_name_str));
        f.instruction(&Instruction::LocalGet(l_h_tail));
        f.instruction(&Instruction::Call(h.map_set_fn));
        f.instruction(&Instruction::LocalSet(l_req_headers_map));

        // h_idx--
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalSet(l_h_idx));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // ── 4. consume(req) → incoming-body handle. retptr 8 bytes:
    //    tag i8 at +0, body handle at +4 (Ok case).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_consume));

    f.instruction(&Instruction::LocalGet(p_req));
    f.instruction(&Instruction::LocalGet(l_retptr_consume));
    f.instruction(&Instruction::Call(h.incoming_request_consume_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_consume));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_consume));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_body_handle));

    // ── 5. body.stream(body) → input-stream handle. retptr 8 bytes.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_stream));

    f.instruction(&Instruction::LocalGet(l_body_handle));
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::Call(h.incoming_body_stream_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_stream));

    // ── 6. Drain stream → growing cabi_realloc buf.
    //    `blocking-read` returns result<list<u8>, stream-error>;
    //    Err.tag=1 (closed) is EOF. Same loop the client uses.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_body_buf));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalSet(l_body_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_body_len));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_read));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::I64Const(4096));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::Call(h.blocking_read_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Err — stream-error tag at +4: 1 = closed (EOF, exit), 0 =
        // last-operation-failed (trap, matches client v1 behaviour).
        f.instruction(&Instruction::LocalGet(l_retptr_read));
        f.instruction(&Instruction::I32Load8U(mem1_o4));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::Br(3));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::Unreachable);
    }
    f.instruction(&Instruction::End);

    // Ok — (data_ptr, data_len) at +4 / +8.
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_data_len));

    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));

    // Grow buf if needed: double until cap >= buf_len + data_len.
    f.instruction(&Instruction::LocalGet(l_body_cap));
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_body_len));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_body_cap));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_body_buf));
        f.instruction(&Instruction::LocalGet(l_body_cap));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_body_buf));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::LocalSet(l_body_cap));
    }
    f.instruction(&Instruction::End);

    // memory.copy buf+len ← data_ptr, data_len bytes.
    f.instruction(&Instruction::LocalGet(l_body_buf));
    f.instruction(&Instruction::LocalGet(l_body_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(l_body_len));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_body_len));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // ── 7. Drop input-stream; incoming-body.finish takes ownership
    //    of body → future-trailers; drop trailers; drop fields.
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(h.drop_input_stream_fn));

    f.instruction(&Instruction::LocalGet(l_body_handle));
    f.instruction(&Instruction::Call(h.incoming_body_finish_fn));
    f.instruction(&Instruction::LocalSet(l_trailers));
    f.instruction(&Instruction::LocalGet(l_trailers));
    f.instruction(&Instruction::Call(h.drop_future_trailers_fn));

    f.instruction(&Instruction::LocalGet(l_req_fields));
    f.instruction(&Instruction::Call(h.drop_fields_fn));

    // Suppress dead-code lint on drop_incoming_body_fn: the client
    // pipeline needs it (Step F error paths), but the server's
    // happy path never holds a live body handle without finish()
    // running. Declaration-only.
    let _ = h.drop_incoming_body_fn;

    // ── 7b. Materialise the body bytes as a fresh Aver String.
    f.instruction(&Instruction::LocalGet(l_body_len));
    f.instruction(&Instruction::ArrayNewDefault(string_idx));
    f.instruction(&Instruction::LocalSet(l_body_str));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_k));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::LocalGet(l_body_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_body_str));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::LocalGet(l_body_buf));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_idx));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_k));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // ── 8. Build HttpRequest struct ref. Field order matches
    //    `BUILTIN_RECORDS`: method, path, query, body, headers.
    f.instruction(&Instruction::LocalGet(l_method_str));
    f.instruction(&Instruction::LocalGet(l_path_str));
    f.instruction(&Instruction::LocalGet(l_query_str));
    f.instruction(&Instruction::LocalGet(l_body_str));
    f.instruction(&Instruction::LocalGet(l_req_headers_map));
    f.instruction(&Instruction::StructNew(req_idx));
    f.instruction(&Instruction::LocalSet(l_req_struct));

    // ── 9. Call user handler.
    f.instruction(&Instruction::LocalGet(l_req_struct));
    f.instruction(&Instruction::Call(h.user_handler_fn));
    f.instruction(&Instruction::LocalSet(l_resp_struct));

    // ── 10. Extract HttpResponse.{status, body, headers}.
    f.instruction(&Instruction::LocalGet(l_resp_struct));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 0, // status: i64
    });
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(l_ob_off)); // reuse i32 scratch for status

    f.instruction(&Instruction::LocalGet(l_resp_struct));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 2, // headers
    });
    f.instruction(&Instruction::LocalSet(l_resp_headers_map));

    // ── 11. Build outgoing fields, append user headers + Content-
    //    Length, then construct outgoing-response.
    f.instruction(&Instruction::Call(h.fields_new_fn));
    f.instruction(&Instruction::LocalSet(l_out_fields));

    // Pre-allocate a 4-byte retptr reused across every fields.append.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_ob_retptr_4));

    // Content-Length from response body byte count.
    {
        f.instruction(&Instruction::LocalGet(l_resp_struct));
        f.instruction(&Instruction::StructGet {
            struct_type_index: resp_idx,
            field_index: 1, // body
        });
        f.instruction(&Instruction::ArrayLen);
        f.instruction(&Instruction::LocalSet(l_cl_body_len));

        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_cl_buf));

        f.instruction(&Instruction::LocalGet(l_cl_buf));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_cl_pos));
        f.instruction(&Instruction::LocalGet(l_cl_body_len));
        f.instruction(&Instruction::LocalSet(l_cl_n));

        f.instruction(&Instruction::Loop(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_cl_pos));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Sub);
            f.instruction(&Instruction::LocalSet(l_cl_pos));

            f.instruction(&Instruction::LocalGet(l_cl_pos));
            f.instruction(&Instruction::LocalGet(l_cl_n));
            f.instruction(&Instruction::I32Const(10));
            f.instruction(&Instruction::I32RemU);
            f.instruction(&Instruction::I32Const(b'0' as i32));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Store8(mem1));

            f.instruction(&Instruction::LocalGet(l_cl_n));
            f.instruction(&Instruction::I32Const(10));
            f.instruction(&Instruction::I32DivU);
            f.instruction(&Instruction::LocalSet(l_cl_n));

            f.instruction(&Instruction::LocalGet(l_cl_n));
            f.instruction(&Instruction::BrIf(0));
        }
        f.instruction(&Instruction::End);

        // Write "Content-Length" (14 bytes) at LM[0..14].
        for (i, b) in b"Content-Length".iter().enumerate() {
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::I32Store8(mem1));
        }

        f.instruction(&Instruction::LocalGet(l_out_fields));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(14));
        f.instruction(&Instruction::LocalGet(l_cl_pos));
        f.instruction(&Instruction::LocalGet(l_cl_buf));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(l_cl_pos));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalGet(l_ob_retptr_4));
        f.instruction(&Instruction::Call(h.fields_append_fn));
    }

    // Walk response headers map → fields.append.
    f.instruction(&Instruction::LocalGet(l_resp_headers_map));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_idx,
        field_index: 1, // cap
    });
    f.instruction(&Instruction::LocalSet(l_uh_cap));
    f.instruction(&Instruction::LocalGet(l_resp_headers_map));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_idx,
        field_index: 2, // keys array
    });
    f.instruction(&Instruction::LocalSet(l_uh_keys));
    f.instruction(&Instruction::LocalGet(l_resp_headers_map));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_idx,
        field_index: 3, // values array
    });
    f.instruction(&Instruction::LocalSet(l_uh_values));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_uh_idx));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::LocalGet(l_uh_cap));
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::BrIf(1));

        f.instruction(&Instruction::LocalGet(l_uh_keys));
        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::ArrayGet(keys_arr_idx));
        f.instruction(&Instruction::LocalSet(l_uh_key));

        f.instruction(&Instruction::LocalGet(l_uh_key));
        f.instruction(&Instruction::RefIsNull);
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            f.instruction(&Instruction::LocalGet(l_uh_values));
            f.instruction(&Instruction::LocalGet(l_uh_idx));
            f.instruction(&Instruction::ArrayGet(values_arr_idx));
            f.instruction(&Instruction::LocalSet(l_uh_node));

            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            {
                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::RefIsNull);
                f.instruction(&Instruction::BrIf(1));

                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: list_str_idx,
                    field_index: 0,
                });
                f.instruction(&Instruction::LocalSet(l_uh_val));

                // Marshal key to cabi-realloc'd scratch (so val's
                // to_lm doesn't clobber the bytes before fields.append
                // reads them).
                f.instruction(&Instruction::LocalGet(l_uh_key));
                f.instruction(&Instruction::Call(h.str_to_lm_fn));
                f.instruction(&Instruction::LocalSet(l_uh_key_len));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::I32Const(1));
                f.instruction(&Instruction::LocalGet(l_uh_key_len));
                f.instruction(&Instruction::Call(h.cabi_realloc_fn));
                f.instruction(&Instruction::LocalSet(l_uh_key_buf));
                f.instruction(&Instruction::LocalGet(l_uh_key_buf));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::LocalGet(l_uh_key_len));
                f.instruction(&Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });

                // val into LM[0..val_len] — last marshal so LM is safe
                // to consume in-place.
                f.instruction(&Instruction::LocalGet(l_uh_val));
                f.instruction(&Instruction::Call(h.str_to_lm_fn));
                f.instruction(&Instruction::LocalSet(l_uh_val_len));

                f.instruction(&Instruction::LocalGet(l_out_fields));
                f.instruction(&Instruction::LocalGet(l_uh_key_buf));
                f.instruction(&Instruction::LocalGet(l_uh_key_len));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::LocalGet(l_uh_val_len));
                f.instruction(&Instruction::LocalGet(l_ob_retptr_4));
                f.instruction(&Instruction::Call(h.fields_append_fn));

                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: list_str_idx,
                    field_index: 1,
                });
                f.instruction(&Instruction::LocalSet(l_uh_node));
                f.instruction(&Instruction::Br(0));
            }
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);
        }
        f.instruction(&Instruction::End);

        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_uh_idx));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // ── 12. outgoing-response(fields) — takes ownership of fields.
    f.instruction(&Instruction::LocalGet(l_out_fields));
    f.instruction(&Instruction::Call(h.outgoing_response_new_fn));
    f.instruction(&Instruction::LocalSet(l_out_response));

    // set-status-code(resp, status u16). Drop the result tag (Aver
    // already validated the int upstream; an invalid code surfaces
    // as a host error later).
    f.instruction(&Instruction::LocalGet(l_out_response));
    f.instruction(&Instruction::LocalGet(l_ob_off)); // status (i32, from i64 wrap)
    f.instruction(&Instruction::Call(h.set_status_code_fn));
    f.instruction(&Instruction::Drop);

    // ── 13. outgoing-response.body → outgoing-body. retptr 8 bytes.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_ob_retptr));

    f.instruction(&Instruction::LocalGet(l_out_response));
    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::Call(h.outgoing_response_body_fn));

    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_ob_handle));

    // outgoing-body.write → output-stream. retptr 8 bytes.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_ob_retptr));

    f.instruction(&Instruction::LocalGet(l_ob_handle));
    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::Call(h.outgoing_body_write_fn));

    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_ob_handle));
        f.instruction(&Instruction::Call(h.drop_outgoing_body_fn));
        f.instruction(&Instruction::Unreachable);
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_ob_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_ob_stream));

    // Marshal response body into LM, chunked write.
    f.instruction(&Instruction::LocalGet(l_resp_struct));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(h.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_ob_body_len));

    super::wasip2_helpers::emit_chunked_blocking_write(
        &mut f,
        l_ob_body_len,
        l_ob_off, // reused as chunk offset; status int held in this slot
        // until set-status-code consumed it, free now.
        h.blocking_write_fn,
        &|f| {
            f.instruction(&Instruction::LocalGet(l_ob_stream));
        },
        &|f| {
            f.instruction(&Instruction::LocalGet(l_ob_body_len));
            f.instruction(&Instruction::I32Const(15));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Const(-16));
            f.instruction(&Instruction::I32And);
        },
        None,
    );

    // Drop output-stream.
    f.instruction(&Instruction::LocalGet(l_ob_stream));
    f.instruction(&Instruction::Call(h.drop_output_stream_fn));

    // outgoing-body.finish(body, None, retptr) — transfers body
    // ownership. retptr 40 bytes align 8 (error-code's option<u64>).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(40));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_ob_finish_retptr));

    f.instruction(&Instruction::LocalGet(l_ob_handle));
    f.instruction(&Instruction::I32Const(0)); // option<trailers> tag = None
    f.instruction(&Instruction::I32Const(0)); // trailers handle (unused)
    f.instruction(&Instruction::LocalGet(l_ob_finish_retptr));
    f.instruction(&Instruction::Call(h.outgoing_body_finish_fn));
    // Result tag ignored — body bytes are already on the wire, the
    // host writes any error-code into retptr and we have no way to
    // surface it past this point (response-outparam.set already
    // committed to Ok). v1 PoC trade-off.

    // ── 14. response-outparam.set(outparam, Ok(out_response)).
    //    9-position canonical-ABI lowering — see slot docs. All
    //    padding positions are zero; only `tag=0` and `pos1=handle`
    //    carry semantics for Ok.
    f.instruction(&Instruction::LocalGet(p_outparam));
    f.instruction(&Instruction::I32Const(0)); // result tag = Ok
    f.instruction(&Instruction::LocalGet(l_out_response)); // pos 1 = Ok handle
    f.instruction(&Instruction::I32Const(0)); // pos 2
    f.instruction(&Instruction::I64Const(0)); // pos 3 (i64)
    f.instruction(&Instruction::I32Const(0)); // pos 4
    f.instruction(&Instruction::I32Const(0)); // pos 5
    f.instruction(&Instruction::I32Const(0)); // pos 6
    f.instruction(&Instruction::I32Const(0)); // pos 7
    f.instruction(&Instruction::Call(h.response_outparam_set_fn));

    // ── 15. Drop incoming-request — all child resources released.
    f.instruction(&Instruction::LocalGet(p_req));
    f.instruction(&Instruction::Call(h.drop_incoming_request_fn));

    f.instruction(&Instruction::End);
    f
}
