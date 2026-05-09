//! Phase 2.0 — `__rt_http_get(url: ref string) ->
//! ref Result<HttpResponse, String>` body emitter.
//!
//! Lowers `Http.get(url)` to the wasi:http/outgoing-handler.handle
//! pipeline. The compiler-side helper owns the entire request /
//! response lifecycle so source-level Aver never sees a wasi handle:
//!
//! 1. Parse the URL into scheme + authority + path-with-query.
//! 2. `[constructor]fields()` → empty headers handle (we send no
//!    request headers in the v1 PoC).
//! 3. `[constructor]outgoing-request(fields)` → ownership of fields
//!    transfers in.
//! 4. `set-scheme(Some(http|https))` / `set-authority(Some(_))` /
//!    `set-path-with-query(Some(_))`. v1 ignores their result tags;
//!    invalid values surface as `outgoing-handler.handle` Err.
//! 5. `outgoing-handler.handle(req, None)` — ownership of req
//!    transfers in. Read the retptr tag: Err ⇒ bail to Result.Err
//!    ("http: connection failed"); Ok ⇒ extract the
//!    future-incoming-response handle.
//! 6. `future.subscribe()` → pollable. Block on
//!    `wasi:io/poll.poll([pollable], 1, retptr)`, then drop
//!    pollable.
//! 7. `future.get()` — assume Some(Ok(Ok(_))); read the
//!    incoming-response handle out of the four-level nested retptr.
//! 8. `response.status()` — u16 inline.
//! 9. `response.consume()` → incoming-body handle (Ok-only path).
//! 10. `body.stream()` → input-stream handle.
//! 11. Drain `input-stream.blocking-read` into a growing
//!     cabi_realloc buffer; `Err(closed)` is EOF.
//! 12. Drop input-stream; `body.finish()` (transfers body
//!     ownership) → future-trailers; drop future-trailers; drop
//!     incoming-response; drop future-incoming-response.
//! 13. Materialise body bytes as a fresh `(array i8)`, build
//!     HttpResponse { status, body, headers = empty Map<String,
//!     List<String>> }, wrap in Result.Ok.
//!
//! v1 PoC scope:
//! - Plain `http://` only. `https://` lowers identically (set-scheme
//!   tag = 1) but actual TLS depends on the wasmtime-wasi-http
//!   build the host links; we don't claim TLS works.
//! - Empty response headers — we never call
//!   `[method]incoming-response.headers`. TODO: surface real
//!   headers in a follow-up; needs an extra import slot.
//! - URL parser is minimal: scheme + authority + optional
//!   path-with-query. No userinfo, no IPv6 brackets, no port-only
//!   authority.
//! - Errors past `handle()` (consume / stream / blocking-read
//!   non-closed Err) collapse to a generic `Result.Err` with a
//!   short tag identifying the failed step.

use wasm_encoder::{Function, ValType};

/// Per-helper allocation metadata, populated in `module.rs` after
/// the relevant import / cabi_realloc / bridge / type slots exist.
/// Mirrors `DiskReadTextIndices` in shape — orchestrator threads
/// these through `funcs.function(fn_type)` and `codes.function(
/// emit_http_get(...))` in lockstep.
pub(super) struct HttpGetIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `Result<HttpResponse, String>` struct type idx.
    pub result_http_response_string_type_idx: u32,
    /// `HttpResponse` struct type idx (status: i64, body: ref
    /// string, headers: ref map).
    pub http_response_type_idx: u32,
    /// `Map<String, List<String>>` slot triple — initialised empty
    /// via the inline `array.new_default + struct.new` sequence
    /// (matches `emit_map_empty`), then populated entry-by-entry
    /// via `Map.set` (`map_set_fn` in `HttpGetHelperFns`).
    pub headers_keys_array_type_idx: u32,
    pub headers_values_array_type_idx: u32,
    pub headers_map_type_idx: u32,
    /// `List<String>` cons-cell type idx. Each header value lands
    /// either in a singleton `[value]` list or prepended onto the
    /// existing list when the same field-key reappears (Set-Cookie
    /// + multi-instance headers).
    pub list_string_type_idx: u32,
    /// `Option<List<String>>` struct type idx. Returned by
    /// `Map.get` over the headers map; consumed via `struct.get`
    /// to extract tag (offset 0) + payload (offset 1).
    pub option_list_string_type_idx: u32,
}

/// Bundle of wasm fn indices the body references via `Call(idx)`.
/// Built once in `module.rs` after the import + helper allocation
/// passes resolve every dependency. Splitting the bundle off from
/// `HttpGetIndices` keeps the `emit_http_get` signature readable
/// (would be ~25 positional args otherwise).
pub(super) struct HttpGetHelperFns {
    pub cabi_realloc_fn: u32,
    pub str_to_lm_fn: u32,
    pub fields_new_fn: u32,
    pub outgoing_request_new_fn: u32,
    pub set_scheme_fn: u32,
    pub set_authority_fn: u32,
    pub set_path_with_query_fn: u32,
    pub handle_fn: u32,
    pub future_subscribe_fn: u32,
    pub poll_fn: u32,
    pub drop_pollable_fn: u32,
    pub future_get_fn: u32,
    pub status_fn: u32,
    pub consume_fn: u32,
    pub body_stream_fn: u32,
    pub blocking_read_fn: u32,
    pub body_finish_fn: u32,
    pub drop_input_stream_fn: u32,
    /// Only fires on the never-reached early-failure branch (we
    /// always make it to `handle()`, which transfers ownership).
    /// Allocated for completeness — wasm-validator-required.
    pub drop_outgoing_request_fn: u32,
    pub drop_future_response_fn: u32,
    pub drop_incoming_response_fn: u32,
    pub drop_future_trailers_fn: u32,
    /// Step F — drop incoming-body on error paths between
    /// `consume()` and `body.finish()`. On the happy path
    /// `body.finish` transfers ownership; on stream/read failure
    /// we own a live body handle that needs explicit drop.
    pub drop_incoming_body_fn: u32,
    /// Step G — `[method]incoming-response.headers: (this) ->
    /// own<fields>`. Returns the fields handle carrying response
    /// headers. Must be dropped (via `drop_fields_fn`) BEFORE the
    /// parent incoming-response is dropped.
    pub headers_fn: u32,
    /// Step G — `[method]fields.entries: (this, retptr) -> ()`.
    /// Writes (entries_ptr i32, entries_len i32) at retptr; each
    /// entry is 16 bytes: (str_ptr, str_len, val_ptr, val_len).
    pub entries_fn: u32,
    /// Step G — `[resource-drop]fields`. Called after entries are
    /// drained, before drop_incoming_response.
    pub drop_fields_fn: u32,
    /// `__rt_string_from_lm(len)` — bridge helper for converting
    /// LM[0..len] bytes into a fresh Aver `(array i8)`. Used by
    /// the headers loop to materialise field-name and field-value
    /// strings (we memory.copy from cabi_realloc heap to LM[0..]
    /// then call this).
    pub from_lm_fn: u32,
    /// `Map.set` for the `Map<String, List<String>>` headers
    /// instantiation. Sourced from `fn_map.map_helpers["Map<
    /// String,List<String>>"].set` at wiring time.
    pub map_set_fn: u32,
    /// `Map.get` for the same instantiation. Used to look up the
    /// existing list of values for a field-key so multi-instance
    /// headers (Set-Cookie etc.) accumulate properly via prepend
    /// instead of overwriting.
    pub map_get_fn: u32,
    /// Step J — `[method]outgoing-request.set-method`. Called
    /// after the request constructor for any non-GET method.
    /// GET is the default, so we skip the call for method_tag==0.
    pub set_method_fn: u32,
    /// Step K — `[method]outgoing-request.body`. Returns
    /// `result<own<outgoing-body>>` via retptr; called once per
    /// body-bearing method (POST/PUT/PATCH).
    pub outgoing_request_body_fn: u32,
    /// Step K — `[method]outgoing-body.write`. Yields the
    /// `output-stream` for body bytes; called once.
    pub outgoing_body_write_fn: u32,
    /// Step K — `[static]outgoing-body.finish`. Closes the body,
    /// transferring ownership; result via retptr (~40 bytes).
    pub outgoing_body_finish_fn: u32,
    /// Step K — `[method]fields.append`. Used per user header
    /// (k/v pair) and once for Content-Type on body-bearing
    /// methods.
    pub fields_append_fn: u32,
    /// Step K — `[resource-drop]outgoing-body`. Error-path drop
    /// for the (rare) case where body() succeeded but write()
    /// failed before finish() could take ownership.
    pub drop_outgoing_body_fn: u32,
    /// Step K — `[method]output-stream.blocking-write-and-flush`.
    /// Reused from the Console.print path (already wired); pushed
    /// in 4096-byte chunks via the shared `emit_chunked_blocking_
    /// write` helper to satisfy wasmtime-wasi's per-call cap.
    pub blocking_write_fn: u32,
    /// Step K — `[resource-drop]output-stream`. Drop the body
    /// stream after the write loop finishes (or on the rare
    /// error path).
    pub drop_output_stream_fn: u32,
}

/// Same `INITIAL_CAP` as `emit_map_empty` in `maps.rs`. Wastes
/// ~128 KB of memory per `Http.get` call when headers are unused
/// (v1 always); the v2 follow-up that surfaces real headers will
/// fill the slots and amortise the allocation.
const INITIAL_CAP: i32 = 16384;

/// `wasi:http/types.error-code` variant case names, ordered by
/// canonical-ABI discriminant. The host writes the discriminant
/// byte at retptr+24 when `future-incoming-response.get` returns
/// the inner `Err` arm. Each name maps directly; payload-bearing
/// variants (DNS-error, TLS-alert-received, internal-error, …)
/// reduce to the variant name only — extracting the inner
/// option<string> / record details is a follow-up. Order MUST
/// match the WIT spec since the host writes raw discriminant
/// indices.
const ERROR_CODE_NAMES: &[&[u8]] = &[
    b"http: DNS-timeout",
    b"http: DNS-error",
    b"http: destination-not-found",
    b"http: destination-unavailable",
    b"http: destination-IP-prohibited",
    b"http: destination-IP-unroutable",
    b"http: connection-refused",
    b"http: connection-terminated",
    b"http: connection-timeout",
    b"http: connection-read-timeout",
    b"http: connection-write-timeout",
    b"http: connection-limit-reached",
    b"http: TLS-protocol-error",
    b"http: TLS-certificate-error",
    b"http: TLS-alert-received",
    b"http: HTTP-request-denied",
    b"http: HTTP-request-length-required",
    b"http: HTTP-request-body-size",
    b"http: HTTP-request-method-invalid",
    b"http: HTTP-request-URI-invalid",
    b"http: HTTP-request-URI-too-long",
    b"http: HTTP-request-header-section-size",
    b"http: HTTP-request-header-size",
    b"http: HTTP-request-trailer-section-size",
    b"http: HTTP-request-trailer-size",
    b"http: HTTP-response-incomplete",
    b"http: HTTP-response-header-section-size",
    b"http: HTTP-response-header-size",
    b"http: HTTP-response-body-size",
    b"http: HTTP-response-trailer-section-size",
    b"http: HTTP-response-trailer-size",
    b"http: HTTP-response-transfer-coding",
    b"http: HTTP-response-content-coding",
    b"http: HTTP-response-timeout",
    b"http: HTTP-upgrade-failed",
    b"http: HTTP-protocol-error",
    b"http: loop-detected",
    b"http: configuration-error",
    b"http: internal-error",
];

/// Body emitter. See module-level docstring for the per-step
/// pipeline. Keep this fn under ~1000 LoC; if it grows past that,
/// split URL parsing into its own emitted helper (allocate a fn
/// idx in `module.rs`, call it from here).
///
/// Signature: `(method_tag: i32, url: ref string) -> ref Result<
/// HttpResponse, String>`. `method_tag` selects the HTTP verb
/// per wasi:http's `method` variant ordinals (0=GET, 1=HEAD,
/// 2=POST, 3=PUT, 4=DELETE, 8=PATCH). For GET we skip the
/// set-method call (default); for others we call set-method
/// after the request constructor.
pub(super) fn emit_http_get(indices: &HttpGetIndices, h: &HttpGetHelperFns) -> Function {
    use wasm_encoder::{BlockType, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });

    // Locals layout — params first (Step K added 3 more), then
    // locals densely packed.
    //   0  = method (i32)         [param]  HTTP verb tag
    //   1  = url    (ref string)  [param]
    //   2  = content_type (ref string) [param]  unused for body-less
    //   3  = body   (ref string)  [param]  request body (utf-8 bytes)
    //   4  = headers (ref map)    [param]  user headers Map<String, List<String>>
    //   5  = url_len           (i32)
    //   6  = i                 (i32)  scheme search cursor / colon idx
    //   7  = j                 (i32)  authority/path split cursor
    //   8  = scheme_tag        (i32)  0 = http, 1 = https
    //   9  = auth_ptr          (i32)
    //   10 = auth_len          (i32)
    //   11 = path_ptr          (i32)
    //   12 = path_len          (i32)
    //   13 = fields            (i32)  outgoing fields handle
    //   14 = req               (i32)  outgoing-request handle
    //   15 = retptr1           (i32)  handle()
    //   16 = future            (i32)
    //   17 = pollable          (i32)
    //   18 = in_buf            (i32)  4-byte slot for poll()
    //   19 = retptr2           (i32)  poll()
    //   20 = retptr3           (i32)  future.get()
    //   21 = response          (i32)
    //   22 = retptr4           (i32)  consume()
    //   23 = body_handle       (i32)  incoming-body (response side)
    //   24 = retptr5           (i32)  body.stream()
    //   25 = stream            (i32)  input-stream handle
    //   26 = buf_ptr           (i32)  growing body buffer
    //   27 = buf_cap           (i32)
    //   28 = buf_len           (i32)
    //   29 = data_ptr          (i32)  per-iter blocking-read ptr
    //   30 = data_len          (i32)
    //   31 = retptr_read       (i32)  per-iter blocking-read retptr (12B)
    //   32 = new_cap           (i32)
    //   33 = k                 (i32)  byte-copy iterator
    //   34 = trailers          (i32)  future-trailers handle
    //   35 = h_fields          (i32)  fields handle from incoming-response.headers
    //   36 = h_retptr          (i32)  retptr for fields.entries (8 bytes)
    //   37 = h_entries_ptr     (i32)
    //   38 = h_entries_len     (i32)
    //   39 = h_idx             (i32)  loop counter
    //   40 = h_entry_addr      (i32)
    //   41 = h_name_ptr        (i32)
    //   42 = h_name_len        (i32)
    //   43 = h_val_ptr         (i32)
    //   44 = h_val_len         (i32)
    //   45 = uh_idx            (i32)  user-headers cap-iter counter (Step K)
    //   46 = uh_cap            (i32)  user-headers map cap
    //   47 = uh_key_len        (i32)  per-pair key bytelen (after to_lm)
    //   48 = uh_val_len        (i32)  per-pair val bytelen
    //   49 = uh_key_buf        (i32)  cabi_realloc'd backing for key bytes
    //   50 = ob_handle         (i32)  outgoing-body handle (request side)
    //   51 = ob_stream         (i32)  output-stream from outgoing-body.write
    //   52 = ob_body_len       (i32)  body content bytelen (after to_lm)
    //   53 = ob_off            (i32)  chunked-write offset
    //   54 = ob_retptr         (i32)  retptr for request.body / body.write / finish
    //   55 = arr (ref string)         body-response string ref + Err scratch
    //   56 = h_name_str (ref string)  per-response-header name
    //   57 = h_val_str  (ref string)  per-response-header value
    //   58 = uh_key (ref string)      per-user-header key (Step K)
    //   59 = uh_val (ref string)      per-user-header value
    //   60 = resp (ref HttpResponse)
    //   61 = h_map (ref map)          accumulated response headers map
    //   62 = h_opt (ref Option<List<String>>)
    //   63 = uh_node (ref list_string) inner list iter cursor (Step K)
    //   64 = uh_keys (ref keys_array)  user-headers keys array
    //   65 = uh_values (ref values_array) user-headers values array
    let resp_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.http_response_type_idx),
    });
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.headers_map_type_idx),
    });
    let list_str_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_string_type_idx),
    });
    let opt_list_str_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.option_list_string_type_idx),
    });
    let keys_arr_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.headers_keys_array_type_idx),
    });
    let values_arr_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.headers_values_array_type_idx),
    });
    let mut f = Function::new(vec![
        (50, ValType::I32),
        (5, s_ref),
        (1, resp_ref),
        (1, map_ref),
        (1, opt_list_str_ref),
        (1, list_str_ref),
        (1, keys_arr_ref),
        (1, values_arr_ref),
        // Step K2 — Content-Length scratch: cl_body_len, cl_buf,
        // cl_pos, cl_n. Appended at the end so existing ref-typed
        // local indices don't shift.
        (4, ValType::I32),
    ]);

    let p_method = 0u32;
    let p_url = 1u32;
    let p_content_type = 2u32;
    let p_body = 3u32;
    let p_headers = 4u32;
    let l_url_len = 5u32;
    let l_i = 6u32;
    let l_j = 7u32;
    let l_scheme_tag = 8u32;
    let l_auth_ptr = 9u32;
    let l_auth_len = 10u32;
    let l_path_ptr = 11u32;
    let l_path_len = 12u32;
    let l_fields = 13u32;
    let l_req = 14u32;
    let l_retptr1 = 15u32;
    let l_future = 16u32;
    let l_pollable = 17u32;
    let l_in_buf = 18u32;
    let l_retptr2 = 19u32;
    let l_retptr3 = 20u32;
    let l_response = 21u32;
    let l_retptr4 = 22u32;
    let l_body = 23u32;
    let l_retptr5 = 24u32;
    let l_stream = 25u32;
    let l_buf_ptr = 26u32;
    let l_buf_cap = 27u32;
    let l_buf_len = 28u32;
    let l_data_ptr = 29u32;
    let l_data_len = 30u32;
    let l_retptr_read = 31u32;
    let l_new_cap = 32u32;
    let l_k = 33u32;
    let l_trailers = 34u32;
    let l_h_fields = 35u32;
    let l_h_retptr = 36u32;
    let l_h_entries_ptr = 37u32;
    let l_h_entries_len = 38u32;
    let l_h_idx = 39u32;
    let l_h_entry_addr = 40u32;
    let l_h_name_ptr = 41u32;
    let l_h_name_len = 42u32;
    let l_h_val_ptr = 43u32;
    let l_h_val_len = 44u32;
    let l_uh_idx = 45u32;
    let l_uh_cap = 46u32;
    let l_uh_key_len = 47u32;
    let l_uh_val_len = 48u32;
    let l_uh_key_buf = 49u32;
    let l_ob_handle = 50u32;
    let l_ob_stream = 51u32;
    let l_ob_body_len = 52u32;
    let l_ob_off = 53u32;
    let l_ob_retptr = 54u32;
    let l_arr = 55u32;
    let l_h_name_str = 56u32;
    let l_h_val_str = 57u32;
    let l_uh_key = 58u32;
    let l_uh_val = 59u32;
    let l_resp = 60u32;
    let l_h_map = 61u32;
    let l_h_opt = 62u32;
    let l_uh_node = 63u32;
    let l_uh_keys = 64u32;
    let l_uh_values = 65u32;
    let l_cl_body_len = 66u32;
    let l_cl_buf = 67u32;
    let l_cl_pos = 68u32;
    let l_cl_n = 69u32;

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
    let mem1_off = |off: u64| MemArg {
        offset: off,
        align: 0,
        memory_index: 0,
    };

    let string_type_idx = indices.string_type_idx;
    let result_idx = indices.result_http_response_string_type_idx;
    let resp_idx = indices.http_response_type_idx;
    let keys_arr_idx = indices.headers_keys_array_type_idx;
    let values_arr_idx = indices.headers_values_array_type_idx;
    let map_idx = indices.headers_map_type_idx;

    // Allocate a fresh (array i8) holding `msg` bytes, store it
    // into l_arr, then build `Result.Err(arr)` (tag = 0, ok = null
    // HttpResponse, err = arr) and Return. Inlined per call site —
    // the bytes change but the shape is identical, so a closure
    // keeps the source compact without paying a runtime fn-call.
    let emit_err = |f: &mut Function, msg: &[u8]| {
        f.instruction(&Instruction::I32Const(msg.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        f.instruction(&Instruction::LocalSet(l_arr));
        for (i, b) in msg.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::ArraySet(string_type_idx));
        }
        // Result.Err: tag = 0, ok = ref-null HttpResponse, err = arr.
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(resp_idx)));
        f.instruction(&Instruction::LocalGet(l_arr));
        f.instruction(&Instruction::StructNew(result_idx));
        f.instruction(&Instruction::Return);
    };

    // ── 1. Marshal URL → LM[0..url_len] ────────────────────────
    f.instruction(&Instruction::LocalGet(p_url));
    f.instruction(&Instruction::Call(h.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_url_len));

    // ── 2a. Find "://" — sets l_i = colon index, or bails ──────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_i));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        // if i + 3 > url_len: not found → bail.
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Const(3));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::I32GtS);
        f.instruction(&Instruction::If(BlockType::Empty));
        emit_err(&mut f, b"malformed url");
        f.instruction(&Instruction::End);

        // Match LM[i..i+3] == "://" — three independent byte
        // compares ANDed together. Short-circuit isn't needed
        // here: every byte already lives in LM after str_to_lm.
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Load8U(mem1));
        f.instruction(&Instruction::I32Const(b':' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Load8U(mem1_off(1)));
        f.instruction(&Instruction::I32Const(b'/' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32And);
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Load8U(mem1_off(2)));
        f.instruction(&Instruction::I32Const(b'/' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32And);
        f.instruction(&Instruction::BrIf(1)); // exit Block — found

        // i++; continue.
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_i));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // ── 2b. Decode scheme: i==4 ⇒ "http" (0), i==5 && LM[4]=='s'
    //   ⇒ "https" (1). Anything else bails. ─────────────────────
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Check LM[0..4] == "http".
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Load8U(mem1_off(0)));
        f.instruction(&Instruction::I32Const(b'h' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Load8U(mem1_off(1)));
        f.instruction(&Instruction::I32Const(b't' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32And);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Load8U(mem1_off(2)));
        f.instruction(&Instruction::I32Const(b't' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32And);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Load8U(mem1_off(3)));
        f.instruction(&Instruction::I32Const(b'p' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::I32And);
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        emit_err(&mut f, b"malformed url");
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_scheme_tag));
    }
    f.instruction(&Instruction::Else);
    {
        f.instruction(&Instruction::LocalGet(l_i));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            // Check LM[0..5] == "https".
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Load8U(mem1_off(0)));
            f.instruction(&Instruction::I32Const(b'h' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Load8U(mem1_off(1)));
            f.instruction(&Instruction::I32Const(b't' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::I32And);
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Load8U(mem1_off(2)));
            f.instruction(&Instruction::I32Const(b't' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::I32And);
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Load8U(mem1_off(3)));
            f.instruction(&Instruction::I32Const(b'p' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::I32And);
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Load8U(mem1_off(4)));
            f.instruction(&Instruction::I32Const(b's' as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::I32And);
            f.instruction(&Instruction::I32Eqz);
            f.instruction(&Instruction::If(BlockType::Empty));
            emit_err(&mut f, b"malformed url");
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalSet(l_scheme_tag));
        }
        f.instruction(&Instruction::Else);
        emit_err(&mut f, b"malformed url");
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    // ── 2c. Find authority/path split: scan from i+3 for first
    //   '/'. l_j ends at either the slash idx or url_len. ──────
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_j));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        // if j >= url_len: exit (no '/' before EOS).
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::I32GeS);
        f.instruction(&Instruction::BrIf(1));

        // if LM[j] == '/': exit.
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::I32Load8U(mem1));
        f.instruction(&Instruction::I32Const(b'/' as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::BrIf(1));

        // j++; continue.
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_j));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // auth_ptr = i + 3, auth_len = j - auth_ptr.
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_auth_ptr));

    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_auth_ptr));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_auth_len));

    f.instruction(&Instruction::LocalGet(l_auth_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"malformed url");
    f.instruction(&Instruction::End);

    // path: if j < url_len: ptr = j, len = url_len - j;
    //       else: write '/' at LM[url_len] and use that.
    f.instruction(&Instruction::LocalGet(l_j));
    f.instruction(&Instruction::LocalGet(l_url_len));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::LocalSet(l_path_ptr));
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::LocalGet(l_j));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalSet(l_path_len));
    }
    f.instruction(&Instruction::Else);
    {
        // The transient buffer (LM page 1) has plenty of room past
        // url_len for one byte; URLs are bounded by the helper's
        // single-page-1 invariant (str_to_lm grows page 1 if it
        // needs more, so url_len < page_1_size always holds when
        // we get here).
        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::I32Const(b'/' as i32));
        f.instruction(&Instruction::I32Store8(mem1));

        f.instruction(&Instruction::LocalGet(l_url_len));
        f.instruction(&Instruction::LocalSet(l_path_ptr));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalSet(l_path_len));
    }
    f.instruction(&Instruction::End);

    // ── 2d. Step K — relocate authority + path bytes to the
    //   cabi_realloc bump heap. The URL parse left them at
    //   LM[auth_ptr..auth_ptr+auth_len] / LM[path_ptr..path_ptr+
    //   path_len]; subsequent steps (user-header to_lm calls,
    //   Content-Type write, body to_lm) all clobber LM[0..],
    //   which would invalidate set-authority / set-path-with-
    //   query that read from those addresses much later in the
    //   pipeline. Copy once now, point auth_ptr/path_ptr at the
    //   bump heap, never look at LM[0..url_len] again.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_auth_len));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_uh_key_buf));
    f.instruction(&Instruction::LocalGet(l_uh_key_buf));
    f.instruction(&Instruction::LocalGet(l_auth_ptr));
    f.instruction(&Instruction::LocalGet(l_auth_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(l_uh_key_buf));
    f.instruction(&Instruction::LocalSet(l_auth_ptr));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_uh_key_buf));
    f.instruction(&Instruction::LocalGet(l_uh_key_buf));
    f.instruction(&Instruction::LocalGet(l_path_ptr));
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(l_uh_key_buf));
    f.instruction(&Instruction::LocalSet(l_path_ptr));

    // ── 3. fields = [constructor]fields() ──────────────────────
    f.instruction(&Instruction::Call(h.fields_new_fn));
    f.instruction(&Instruction::LocalSet(l_fields));

    // ── 3b. Step K — populate fields with user headers and
    //   Content-Type before passing ownership to the constructor.
    //
    //   For each non-empty slot in the headers map, walk the
    //   inner List<String> cons-cells; per (key, value) pair
    //   marshal both into LM (key into a cabi_realloc'd scratch
    //   to survive the val's overwriting of LM[0..]) then call
    //   fields.append. fields.append's flat result tag is dropped
    //   — invalid header syntax surfaces later as
    //   `outgoing-handler.handle` Err if the host enforces it.
    //
    //   Body-less methods (GET/HEAD/DELETE) still walk this loop
    //   but the dispatcher passes an empty map, so it's a no-op
    //   beyond the cap-iter (~16k iterations of "is keys[i] null?
    //   yes, skip"). Acceptable for v1 PoC.
    f.instruction(&Instruction::LocalGet(p_headers));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.headers_map_type_idx,
        field_index: 1, // cap
    });
    f.instruction(&Instruction::LocalSet(l_uh_cap));
    f.instruction(&Instruction::LocalGet(p_headers));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.headers_map_type_idx,
        field_index: 2, // keys array
    });
    f.instruction(&Instruction::LocalSet(l_uh_keys));
    f.instruction(&Instruction::LocalGet(p_headers));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.headers_map_type_idx,
        field_index: 3, // values array
    });
    f.instruction(&Instruction::LocalSet(l_uh_values));

    // Pre-allocate a 4-byte retptr reused across every
    // fields.append call (host writes the result tag here; we
    // ignore it). Sized to canonical-ABI's `result<_,
    // header-error>` flat encoding (1-byte disc + 1-byte payload
    // padded to 4-byte align).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_ob_retptr));

    // ── 3a-cl. Step K2 — Content-Length header for body methods.
    //
    // wasmtime-wasi-http defaults to Transfer-Encoding: chunked
    // when no Content-Length is set on the outgoing fields.
    // Many HTTP servers (including Python's stdlib
    // BaseHTTPRequestHandler) don't decode chunked bodies, so we
    // surface an explicit Content-Length here using the body
    // arg's `(array i8)` length — the byte count is exact and
    // doesn't require a duplicate `to_lm` pass.
    //
    // Inline int→decimal: write digits backwards into a
    // cabi_realloc'd 16-byte buffer (do-while always emits at
    // least one digit so body_len=0 surfaces "0"). Then write
    // the literal "Content-Length" name to LM[0..14] and call
    // fields.append(fields, 0, 14, digit_ptr, digit_len, retptr).
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // body_len = array.len p_body
        f.instruction(&Instruction::LocalGet(p_body));
        f.instruction(&Instruction::ArrayLen);
        f.instruction(&Instruction::LocalSet(l_cl_body_len));

        // 16-byte scratch in bump heap.
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_cl_buf));

        // pos = buf + 16; n = body_len
        f.instruction(&Instruction::LocalGet(l_cl_buf));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_cl_pos));
        f.instruction(&Instruction::LocalGet(l_cl_body_len));
        f.instruction(&Instruction::LocalSet(l_cl_n));

        // do-while: pos--, *pos = '0' + n%10, n /= 10, repeat
        // while n != 0. Even body_len=0 emits exactly one '0'
        // because the loop body runs once before the test.
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

        // Write "Content-Length" name (14 bytes) to LM[0..14].
        for (i, b) in b"Content-Length".iter().enumerate() {
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::I32Store8(mem1));
        }

        // fields.append(fields, name=LM[0..14], value=LM[cl_pos..cl_buf+16], retptr)
        f.instruction(&Instruction::LocalGet(l_fields));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(14));
        f.instruction(&Instruction::LocalGet(l_cl_pos));
        f.instruction(&Instruction::LocalGet(l_cl_buf));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(l_cl_pos));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::Call(h.fields_append_fn));
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_uh_idx));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        // Exit loop when idx >= cap.
        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::LocalGet(l_uh_cap));
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::BrIf(1));

        // key = keys[idx]; if null, skip to next slot.
        f.instruction(&Instruction::LocalGet(l_uh_keys));
        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::ArrayGet(indices.headers_keys_array_type_idx));
        f.instruction(&Instruction::LocalSet(l_uh_key));

        f.instruction(&Instruction::LocalGet(l_uh_key));
        f.instruction(&Instruction::RefIsNull);
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        {
            // node = values[idx]
            f.instruction(&Instruction::LocalGet(l_uh_values));
            f.instruction(&Instruction::LocalGet(l_uh_idx));
            f.instruction(&Instruction::ArrayGet(
                indices.headers_values_array_type_idx,
            ));
            f.instruction(&Instruction::LocalSet(l_uh_node));

            // Inner loop: walk the List<String> cons-cells.
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            {
                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::RefIsNull);
                f.instruction(&Instruction::BrIf(1));

                // val = node.head
                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: indices.list_string_type_idx,
                    field_index: 0,
                });
                f.instruction(&Instruction::LocalSet(l_uh_val));

                // Marshal key to a cabi_realloc'd scratch (so the
                // subsequent to_lm val doesn't clobber the bytes
                // before fields.append reads them).
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

                // Marshal val to LM[0..val_len]. (Now key bytes
                // at LM[0..] are stale — fine, we point at
                // l_uh_key_buf for the key.)
                f.instruction(&Instruction::LocalGet(l_uh_val));
                f.instruction(&Instruction::Call(h.str_to_lm_fn));
                f.instruction(&Instruction::LocalSet(l_uh_val_len));

                // fields.append(fields, key_buf, key_len, 0, val_len, retptr)
                f.instruction(&Instruction::LocalGet(l_fields));
                f.instruction(&Instruction::LocalGet(l_uh_key_buf));
                f.instruction(&Instruction::LocalGet(l_uh_key_len));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::LocalGet(l_uh_val_len));
                f.instruction(&Instruction::LocalGet(l_ob_retptr));
                f.instruction(&Instruction::Call(h.fields_append_fn));

                // node = node.tail
                f.instruction(&Instruction::LocalGet(l_uh_node));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: indices.list_string_type_idx,
                    field_index: 1,
                });
                f.instruction(&Instruction::LocalSet(l_uh_node));
                f.instruction(&Instruction::Br(0));
            }
            f.instruction(&Instruction::End); // inner loop
            f.instruction(&Instruction::End); // inner block
        }
        f.instruction(&Instruction::End); // if key non-null

        // idx++
        f.instruction(&Instruction::LocalGet(l_uh_idx));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_uh_idx));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    // ── 3c. Step K — Content-Type for body-bearing methods.
    //   POST=2, PUT=3, PATCH=8. Identify via `method >= 2 &&
    //   method != 4` (GET=0, HEAD=1, DELETE=4 skip; everything
    //   else with content_type semantics gets the header).
    //
    //   Inline-write the literal "Content-Type" name bytes to
    //   LM[0..12] via i32.store8 so we can pass them as
    //   (name_ptr=0, name_len=12). content_type ARG bytes go
    //   into a cabi_realloc'd buffer to survive — same trick as
    //   the user-header loop above. The LM[0..12] write happens
    //   AFTER the val marshal so val's to_lm doesn't clobber.
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Marshal content_type val into LM[0..val_len], then
        // memcpy to a cabi-realloc buf, then write "Content-Type"
        // bytes into LM[0..12].
        f.instruction(&Instruction::LocalGet(p_content_type));
        f.instruction(&Instruction::Call(h.str_to_lm_fn));
        f.instruction(&Instruction::LocalSet(l_uh_val_len));

        // Allocate buffer for val bytes (key="Content-Type" goes
        // at LM[0..12] — written next).
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_uh_val_len));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_uh_key_buf));
        f.instruction(&Instruction::LocalGet(l_uh_key_buf));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_uh_val_len));
        f.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

        // Write "Content-Type" (12 bytes) at LM[0..12]. Pre-byte
        // i32.store8 sequence — opaque but compact.
        for (i, b) in b"Content-Type".iter().enumerate() {
            f.instruction(&Instruction::I32Const(i as i32));
            f.instruction(&Instruction::I32Const(*b as i32));
            f.instruction(&Instruction::I32Store8(mem1));
        }

        // fields.append(fields, name_ptr=0, name_len=12,
        //                       val_ptr=key_buf, val_len, retptr)
        f.instruction(&Instruction::LocalGet(l_fields));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(12));
        f.instruction(&Instruction::LocalGet(l_uh_key_buf));
        f.instruction(&Instruction::LocalGet(l_uh_val_len));
        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::Call(h.fields_append_fn));
    }
    f.instruction(&Instruction::End);

    // ── 4. req = [constructor]outgoing-request(fields) ─────────
    // fields ownership transfers in; do NOT drop fields after.
    f.instruction(&Instruction::LocalGet(l_fields));
    f.instruction(&Instruction::Call(h.outgoing_request_new_fn));
    f.instruction(&Instruction::LocalSet(l_req));

    // ── 4b. set-method(req, p_method) — Step J ─────────────────
    // Constructor defaults to GET. Skip the host call when
    // p_method == 0 to keep the GET fast path identical to the
    // original __rt_http_get. For other tags pass (req, tag, 0, 0)
    // — the `other_str_*` slots are unused for the named methods
    // (GET/HEAD/POST/PUT/DELETE/CONNECT/OPTIONS/TRACE/PATCH all
    // map to discriminants 0..=8). v1 ignores the result tag —
    // an invalid method surfaces later as `outgoing-handler.handle`
    // Err.
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_req));
        f.instruction(&Instruction::LocalGet(p_method));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Call(h.set_method_fn));
        f.instruction(&Instruction::Drop);
    }
    f.instruction(&Instruction::End);

    // ── 5. set-scheme(req, Some(scheme_tag)) ───────────────────
    // option<scheme>: opt_tag = 1 (Some), scheme variant tag =
    // l_scheme_tag (0=HTTP/1=HTTPS), scheme str_ptr/len = 0
    // (unused for the named variants). v1 ignores result tag.
    f.instruction(&Instruction::LocalGet(l_req));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scheme_tag));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(h.set_scheme_fn));
    f.instruction(&Instruction::Drop);

    // ── 6. set-authority(req, Some(auth)) ──────────────────────
    f.instruction(&Instruction::LocalGet(l_req));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_auth_ptr));
    f.instruction(&Instruction::LocalGet(l_auth_len));
    f.instruction(&Instruction::Call(h.set_authority_fn));
    f.instruction(&Instruction::Drop);

    // ── 7. set-path-with-query(req, Some(path)) ────────────────
    f.instruction(&Instruction::LocalGet(l_req));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_path_ptr));
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::Call(h.set_path_with_query_fn));
    f.instruction(&Instruction::Drop);

    // ── 7b. Step K — body marshalling for body-bearing methods.
    //   Sequence (POST/PUT/PATCH only):
    //   - request.body(req) → outgoing-body
    //   - body.write(outgoing-body) → output-stream
    //   - chunked blocking-write of body bytes
    //   - drop output-stream
    //   - body.finish(outgoing-body, None) — transfers ownership
    //
    //   Each retptr-bearing call uses l_ob_retptr (8 bytes for
    //   request.body / body.write; 40 bytes for body.finish's
    //   result<_, error-code>). The scratch is realloc'd between
    //   calls — wasteful but simple.
    //
    //   Failures collapse to a single Err message per step. The
    //   resource-drop choreography on early failure paths follows
    //   the child-before-parent rule.
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::LocalGet(p_method));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // request.body(req, retptr) → result<own<outgoing-body>>
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(4));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_ob_retptr));

        f.instruction(&Instruction::LocalGet(l_req));
        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::Call(h.outgoing_request_body_fn));

        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::I32Load8U(mem1));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Ne);
        f.instruction(&Instruction::If(BlockType::Empty));
        emit_err(&mut f, b"http: request body unavailable");
        f.instruction(&Instruction::End);

        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_ob_handle));

        // outgoing-body.write(body, retptr) → result<own<output-stream>>
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
            emit_err(&mut f, b"http: body write stream unavailable");
        }
        f.instruction(&Instruction::End);

        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::I32Load(mem4_o4));
        f.instruction(&Instruction::LocalSet(l_ob_stream));

        // Marshal body content into LM[0..body_len], then write
        // through the output-stream. wasmtime-wasi-http sends the
        // request with Transfer-Encoding: chunked when no
        // Content-Length is set on the fields — a custom server
        // implementation that doesn't decode chunked will see
        // Content-Length=0 and miss the body. Setting Content-
        // Length explicitly would require an int-to-decimal-LM
        // helper, deferred to a follow-up; v1 relies on the
        // host's chunked encoding which all modern HTTP clients +
        // servers handle correctly.
        f.instruction(&Instruction::LocalGet(p_body));
        f.instruction(&Instruction::Call(h.str_to_lm_fn));
        f.instruction(&Instruction::LocalSet(l_ob_body_len));

        // Chunked blocking-write-and-flush — wasmtime-wasi caps a
        // single call to 4096 bytes, so the shared helper walks
        // LM[0..body_len] in 4096-byte slices. Each iteration
        // pushes the output-stream handle and a 16-byte-aligned
        // retptr just past body_len.
        super::wasip2_helpers::emit_chunked_blocking_write(
            &mut f,
            l_ob_body_len,
            l_ob_off,
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

        // Drop output-stream after the write loop.
        f.instruction(&Instruction::LocalGet(l_ob_stream));
        f.instruction(&Instruction::Call(h.drop_output_stream_fn));

        // outgoing-body.finish(body, None, retptr) — takes ownership
        // of body; result<_, error-code> via retptr (~40 bytes,
        // align 8 due to error-code's option<u64>). v1 ignores
        // the specific error-code variant on failure — surfaces a
        // generic "http: body finish failed" so the caller knows
        // the body upload didn't fully commit.
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(8));
        f.instruction(&Instruction::I32Const(40));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_ob_retptr));

        f.instruction(&Instruction::LocalGet(l_ob_handle));
        f.instruction(&Instruction::I32Const(0)); // option<trailers> tag = None
        f.instruction(&Instruction::I32Const(0)); // trailers handle (unused)
        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::Call(h.outgoing_body_finish_fn));

        f.instruction(&Instruction::LocalGet(l_ob_retptr));
        f.instruction(&Instruction::I32Load8U(mem1));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Ne);
        f.instruction(&Instruction::If(BlockType::Empty));
        emit_err(&mut f, b"http: body finish failed");
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);

    // ── 8. handle(req, None) → retptr1 ─────────────────────────
    // result<own<future-incoming-response>, error-code>. The
    // error-code variant carries `option<u64>` etc., forcing the
    // whole result's alignment to 8. Layout: tag at 0 (padded
    // to 8), payload (future handle i32 OR ~24-byte error-code)
    // starts at offset 8. Conservatively allocate 32 bytes.
    // Ownership of req transfers in; do NOT drop req.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(40));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr1));

    f.instruction(&Instruction::LocalGet(l_req));
    f.instruction(&Instruction::I32Const(0)); // opt_tag = None
    f.instruction(&Instruction::I32Const(0)); // opt_handle (unused)
    f.instruction(&Instruction::LocalGet(l_retptr1));
    f.instruction(&Instruction::Call(h.handle_fn));

    f.instruction(&Instruction::LocalGet(l_retptr1));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"http: connection failed");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_retptr1));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_future));

    // ── 9. pollable = future.subscribe(future); poll([pollable]) ─
    // Mirrors emit_time_sleep's pollable wait pattern.
    f.instruction(&Instruction::LocalGet(l_future));
    f.instruction(&Instruction::Call(h.future_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_in_buf));
    f.instruction(&Instruction::LocalGet(l_in_buf));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr2));

    f.instruction(&Instruction::LocalGet(l_in_buf));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr2));
    f.instruction(&Instruction::Call(h.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(h.drop_pollable_fn));

    // ── 10. future.get(future, retptr3) ────────────────────────
    // Four-level nested option<result<result<incoming-response,
    // error-code>, _>>. error-code's `option<u64>` payload forces
    // align=8 through every wrapping layer:
    //   inner result  align=8 size=40
    //   middle result align=8 size=48
    //   outer option  align=8 size=56
    //
    // Layout (assumed Some-Ok-Ok happy path, post-poll):
    //   offset 0:  outer option tag (1 byte, padded to 8)
    //   offset 8:  middle result tag (1, padded)
    //   offset 16: inner result tag (1, padded)
    //   offset 24: incoming-response handle (i32, Ok payload)
    //
    // Allocate 64 bytes (safety margin past the 56 strictly
    // required); align 8 forces cabi_realloc to round up anyway.
    // After the read, drop the future-incoming-response.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(64));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr3));

    f.instruction(&Instruction::LocalGet(l_future));
    f.instruction(&Instruction::LocalGet(l_retptr3));
    f.instruction(&Instruction::Call(h.future_get_fn));

    // Check the three nested result/option discriminants. Any
    // negative outcome bails to Result.Err — the v1 PoC uses a
    // single generic message per layer; future iterations could
    // surface the inner error-code variant tag for richer
    // diagnostics.
    //
    // Outer option tag at offset 0: 0 = None (future not ready,
    // shouldn't happen post-poll), 1 = Some.
    f.instruction(&Instruction::LocalGet(l_retptr3));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_future));
        f.instruction(&Instruction::Call(h.drop_future_response_fn));
        emit_err(&mut f, b"http: future not ready");
    }
    f.instruction(&Instruction::End);

    // Middle result tag at offset 8: 0 = Ok (we got a result),
    // 1 = Err (get() called twice — never our case).
    f.instruction(&Instruction::LocalGet(l_retptr3));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_future));
        f.instruction(&Instruction::Call(h.drop_future_response_fn));
        emit_err(&mut f, b"http: get already consumed");
    }
    f.instruction(&Instruction::End);

    // Inner result tag at offset 16: 0 = Ok (incoming-response
    // handle at +24), 1 = Err (error-code discriminant at +24).
    // On Err, dispatch on the error-code byte to surface the
    // specific failure (connection-refused vs DNS-timeout vs
    // TLS-protocol-error etc.) instead of a generic message.
    // Each case bails via emit_err's Return; an unknown disc
    // (host bug or future variant) falls through to a generic
    // "http: unknown error" tail.
    f.instruction(&Instruction::LocalGet(l_retptr3));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 16,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_future));
        f.instruction(&Instruction::Call(h.drop_future_response_fn));

        // Read error-code variant discriminant at retptr+24.
        // Stash via l_h_idx (i32 scratch — header parsing's
        // counter is dead at this point because the inner-Err
        // branch returns before reaching the headers loop).
        f.instruction(&Instruction::LocalGet(l_retptr3));
        f.instruction(&Instruction::I32Load8U(MemArg {
            offset: 24,
            align: 0,
            memory_index: 0,
        }));
        f.instruction(&Instruction::LocalSet(l_h_idx));

        for (disc, name) in ERROR_CODE_NAMES.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_h_idx));
            f.instruction(&Instruction::I32Const(disc as i32));
            f.instruction(&Instruction::I32Eq);
            f.instruction(&Instruction::If(BlockType::Empty));
            emit_err(&mut f, name);
            f.instruction(&Instruction::End);
        }
        // Fallback for an unknown discriminant (host bug or
        // post-0.2.4 spec extension). No way to surface the
        // raw byte through emit_err's static-bytes API; pick
        // a clear message.
        emit_err(&mut f, b"http: unknown error");
    }
    f.instruction(&Instruction::End);

    // Inner Ok payload at offset 24: incoming-response handle.
    f.instruction(&Instruction::LocalGet(l_retptr3));
    f.instruction(&Instruction::I32Load(MemArg {
        offset: 24,
        align: 2,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(l_response));

    // ── 11. status = response.status() — u16 inline ────────────
    // Call BEFORE drop_future_response so the result handle's
    // resource table state is unambiguous (some hosts invalidate
    // borrows when the parent owning resource is dropped).
    // Stash via the in_buf slot (we no longer need it past poll).
    f.instruction(&Instruction::LocalGet(l_response));
    f.instruction(&Instruction::Call(h.status_fn));
    f.instruction(&Instruction::LocalSet(l_in_buf));

    f.instruction(&Instruction::LocalGet(l_future));
    f.instruction(&Instruction::Call(h.drop_future_response_fn));
    // wasi returns the u16 zero-extended in an i32 — no extra
    // load needed. Local l_in_buf now holds the status code.

    // ── 11b. headers — Step G ──────────────────────────────────
    //
    // headers = response.headers() → own<fields>
    // entries = fields.entries() → list<tuple<field-key, field-value>>
    // for each entry:
    //   key = string from LM (via memory.copy + __rt_string_from_lm)
    //   val = string from LM (same)
    //   singleton = struct.new $list_string (val, ref.null)
    //   map = Map.set(map, key, singleton)
    // drop fields  (child of incoming-response, must drop before
    //              drop_incoming_response)
    //
    // Multi-valued headers (same field-key in multiple entries —
    // e.g. Set-Cookie) are mapped via simple insert: each Map.set
    // overwrites the previous value. Surfacing the full multi-set
    // would require Map.get + List.prepend per entry; deferred to
    // a follow-up since real HTTP responses rarely repeat header
    // names besides Set-Cookie.

    // Initialise empty map (matches `emit_map_empty`).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(keys_arr_idx));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(values_arr_idx));
    f.instruction(&Instruction::StructNew(map_idx));
    f.instruction(&Instruction::LocalSet(l_h_map));

    // headers = response.headers() → fields handle
    f.instruction(&Instruction::LocalGet(l_response));
    f.instruction(&Instruction::Call(h.headers_fn));
    f.instruction(&Instruction::LocalSet(l_h_fields));

    // retptr for fields.entries — list<tuple<...>> lowers to
    // (ptr i32, len i32) = 8 bytes via retptr.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_h_retptr));

    f.instruction(&Instruction::LocalGet(l_h_fields));
    f.instruction(&Instruction::LocalGet(l_h_retptr));
    f.instruction(&Instruction::Call(h.entries_fn));

    f.instruction(&Instruction::LocalGet(l_h_retptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_h_entries_ptr));
    f.instruction(&Instruction::LocalGet(l_h_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_h_entries_len));

    // Reverse iteration: idx = entries_len - 1, decrementing,
    // exit when idx becomes -1 (signed). Prepending in reverse
    // order yields forward order in the final list:
    //   entries: [Set-Cookie: a, Set-Cookie: b]
    //   reverse: process b first  → map[Set-Cookie] = [b]
    //            process a second → map[Set-Cookie] = [a, b]
    // RFC 6265 (Set-Cookie) requires preserving server-emit
    // order; this scheme does that without needing List.append
    // (which would be O(n²) over the loop).
    f.instruction(&Instruction::LocalGet(l_h_entries_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_h_idx));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    {
        // exit when idx < 0 (also covers entries_len == 0 since
        // 0 - 1 = -1 wraps to a negative i32 under signed compare).
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32LtS);
        f.instruction(&Instruction::BrIf(1));

        // entry_addr = entries_ptr + idx * 16.
        f.instruction(&Instruction::LocalGet(l_h_entries_ptr));
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(16));
        f.instruction(&Instruction::I32Mul);
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_h_entry_addr));

        // Read tuple<string, list<u8>> at entry_addr:
        //   +0 name_ptr i32, +4 name_len i32,
        //   +8 val_ptr  i32, +12 val_len  i32
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

        // name_str: memory.copy name bytes → LM[0..name_len],
        //           call __rt_string_from_lm(name_len). Stash so
        //           we can use it for both Map.get (probe existing)
        //           and Map.set (insert new list).
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

        // val_str: same shape — name bytes already lifted into a
        // fresh GC array, LM[0..] is free to overwrite.
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

        // existing = Map.get(map, name_str) → Option<List<String>>
        f.instruction(&Instruction::LocalGet(l_h_map));
        f.instruction(&Instruction::LocalGet(l_h_name_str));
        f.instruction(&Instruction::Call(h.map_get_fn));
        f.instruction(&Instruction::LocalSet(l_h_opt));

        // Build new_list = struct.new $list_string (val_str, tail)
        // where tail = Some(prev) ⇒ prev, None ⇒ ref.null.
        // Then call Map.set(map, name_str, new_list).
        //
        // Stack discipline: push (map, name_str, val_str, tail) then
        //   struct.new pops 2 → (map, name_str, new_list)
        //   call map_set pops 3 → (new_map)
        f.instruction(&Instruction::LocalGet(l_h_map));
        f.instruction(&Instruction::LocalGet(l_h_name_str));
        f.instruction(&Instruction::LocalGet(l_h_val_str));

        // tail = if opt.tag == 1 then opt.payload else ref.null
        f.instruction(&Instruction::LocalGet(l_h_opt));
        f.instruction(&Instruction::StructGet {
            struct_type_index: indices.option_list_string_type_idx,
            field_index: 0,
        });
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Result(list_str_ref)));
        {
            f.instruction(&Instruction::LocalGet(l_h_opt));
            f.instruction(&Instruction::StructGet {
                struct_type_index: indices.option_list_string_type_idx,
                field_index: 1,
            });
        }
        f.instruction(&Instruction::Else);
        {
            f.instruction(&Instruction::RefNull(HeapType::Concrete(
                indices.list_string_type_idx,
            )));
        }
        f.instruction(&Instruction::End);

        // struct.new $list_string pops (val_str, tail), pushes new_list.
        f.instruction(&Instruction::StructNew(indices.list_string_type_idx));

        // Map.set(map, name_str, new_list) → new map ref.
        f.instruction(&Instruction::Call(h.map_set_fn));
        f.instruction(&Instruction::LocalSet(l_h_map));

        // idx--; continue.
        f.instruction(&Instruction::LocalGet(l_h_idx));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Sub);
        f.instruction(&Instruction::LocalSet(l_h_idx));
        f.instruction(&Instruction::Br(0));
    }
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // Drop fields BEFORE drop_incoming_response (fields is a
    // child resource of incoming-response per WIT).
    f.instruction(&Instruction::LocalGet(l_h_fields));
    f.instruction(&Instruction::Call(h.drop_fields_fn));

    // ── 12. consume(response, retptr4) — assume Ok ─────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr4));

    f.instruction(&Instruction::LocalGet(l_response));
    f.instruction(&Instruction::LocalGet(l_retptr4));
    f.instruction(&Instruction::Call(h.consume_fn));

    f.instruction(&Instruction::LocalGet(l_retptr4));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_response));
        f.instruction(&Instruction::Call(h.drop_incoming_response_fn));
        emit_err(&mut f, b"http: consume failed");
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_retptr4));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_body));

    // ── 13. body.stream(body, retptr5) — assume Ok ─────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr5));

    f.instruction(&Instruction::LocalGet(l_body));
    f.instruction(&Instruction::LocalGet(l_retptr5));
    f.instruction(&Instruction::Call(h.body_stream_fn));

    f.instruction(&Instruction::LocalGet(l_retptr5));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        // Step F — body has been consumed but stream() failed.
        // Drop body explicitly (finish() never runs on this path).
        // Order: child resources first, then parent.
        f.instruction(&Instruction::LocalGet(l_body));
        f.instruction(&Instruction::Call(h.drop_incoming_body_fn));
        f.instruction(&Instruction::LocalGet(l_response));
        f.instruction(&Instruction::Call(h.drop_incoming_response_fn));
        emit_err(&mut f, b"http: body stream failed");
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_retptr5));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_stream));

    // ── 14. Drain stream into growing cabi_realloc buffer ──────
    // Mirrors emit_disk_read_text's read loop. blocking-read
    // returns result<list<u8>, stream-error>; closed = EOF.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::Call(h.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));

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
        // stream-error tag at +4: 0 = last-operation-failed
        // (fail), 1 = closed (EOF, exit loop normally).
        f.instruction(&Instruction::LocalGet(l_retptr_read));
        f.instruction(&Instruction::I32Load8U(mem1_o4));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::Br(3)); // exit Block (depth 0=If, 1=Loop, 2=Block)
        f.instruction(&Instruction::End);
        // Real failure — drop stream + body + response (child→parent
        // order). Step F: body was leaked here pre-fix.
        f.instruction(&Instruction::LocalGet(l_stream));
        f.instruction(&Instruction::Call(h.drop_input_stream_fn));
        f.instruction(&Instruction::LocalGet(l_body));
        f.instruction(&Instruction::Call(h.drop_incoming_body_fn));
        f.instruction(&Instruction::LocalGet(l_response));
        f.instruction(&Instruction::Call(h.drop_incoming_response_fn));
        emit_err(&mut f, b"http: read failed");
    }
    f.instruction(&Instruction::End);

    // Ok branch — (data_ptr, data_len) at +4 / +8.
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_data_len));

    // Empty Ok = no bytes available right now, but stream still
    // open — exit loop (caller polls again next iter, but we
    // don't poll between blocking-reads; treat as EOF for v1).
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));

    // Grow buffer if cap < buf_len + data_len: keep doubling.
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_buf_len));
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
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    {
        f.instruction(&Instruction::LocalGet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_buf_cap));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::Call(h.cabi_realloc_fn));
        f.instruction(&Instruction::LocalSet(l_buf_ptr));
        f.instruction(&Instruction::LocalGet(l_new_cap));
        f.instruction(&Instruction::LocalSet(l_buf_cap));
    }
    f.instruction(&Instruction::End);

    // memory.copy(buf_ptr+buf_len, data_ptr, data_len).
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });

    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // Loop
    f.instruction(&Instruction::End); // Block

    // ── 15. Drop input-stream; finish body → trailers; drop
    //   trailers; drop response. Order matters: finish takes
    //   ownership of body, so body is no longer ours after. ────
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(h.drop_input_stream_fn));

    f.instruction(&Instruction::LocalGet(l_body));
    f.instruction(&Instruction::Call(h.body_finish_fn));
    f.instruction(&Instruction::LocalSet(l_trailers));

    f.instruction(&Instruction::LocalGet(l_trailers));
    f.instruction(&Instruction::Call(h.drop_future_trailers_fn));

    f.instruction(&Instruction::LocalGet(l_response));
    f.instruction(&Instruction::Call(h.drop_incoming_response_fn));

    // drop_outgoing_request is registered as an import for the
    // never-reached early-failure branch but never actually called
    // from this helper — outgoing-handler.handle takes ownership
    // unconditionally, so by the time we'd need to drop req, the
    // host already owns it. Declared imports without callers are
    // legal in core wasm; wit-component still binds them.
    let _ = h.drop_outgoing_request_fn;

    // ── 16. Build HttpResponse + Result.Ok ─────────────────────
    // HttpResponse field order matches `BUILTIN_RECORDS`:
    //   status: Int (i64), body: String (ref array i8),
    //   headers: Map<String, List<String>> (ref map struct).
    //
    // Ok payload is built inline; the surrounding Result is
    // tag = 1 (Ok), ok = HttpResponse, err = ref-null string.

    // Body bytes → fresh (array i8) of size buf_len, copied byte
    // by byte. Reuses the disk-read pattern.
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::ArrayNewDefault(string_type_idx));
    f.instruction(&Instruction::LocalSet(l_arr));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_k));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::ArraySet(string_type_idx));
    f.instruction(&Instruction::LocalGet(l_k));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_k));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Build HttpResponse fields onto the stack in declaration
    // order, then struct.new HttpResponse, store into l_resp.
    //
    // status: i64 — wasi returned u16 zero-extended in i32; widen.
    f.instruction(&Instruction::LocalGet(l_in_buf));
    f.instruction(&Instruction::I64ExtendI32U);

    // body: ref string (l_arr, populated above).
    f.instruction(&Instruction::LocalGet(l_arr));

    // headers: Map<String, List<String>> built from
    // incoming-response.headers + fields.entries in step 11b.
    f.instruction(&Instruction::LocalGet(l_h_map));

    f.instruction(&Instruction::StructNew(resp_idx));
    f.instruction(&Instruction::LocalSet(l_resp));

    // Result.Ok: tag = 1, ok = HttpResponse, err = ref-null string.
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_resp));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
    f.instruction(&Instruction::StructNew(result_idx));

    f.instruction(&Instruction::End); // fn end
    f
}
