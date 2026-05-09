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
}

/// Same `INITIAL_CAP` as `emit_map_empty` in `maps.rs`. Wastes
/// ~128 KB of memory per `Http.get` call when headers are unused
/// (v1 always); the v2 follow-up that surfaces real headers will
/// fill the slots and amortise the allocation.
const INITIAL_CAP: i32 = 16384;

/// Body emitter. See module-level docstring for the per-step
/// pipeline. Keep this fn under ~800 LoC; if it grows past that,
/// split URL parsing into its own emitted helper (allocate a fn
/// idx in `module.rs`, call it from here).
pub(super) fn emit_http_get(indices: &HttpGetIndices, h: &HttpGetHelperFns) -> Function {
    use wasm_encoder::{BlockType, HeapType, Instruction, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });

    // Locals layout — kept densely packed because wasm doesn't
    // care about gaps but readers do:
    //   0  = url (ref string) [param]
    //   1  = url_len           (i32)  byte length of LM-resident url
    //   2  = i                 (i32)  scheme search cursor / colon idx
    //   3  = j                 (i32)  authority/path split cursor
    //   4  = scheme_tag        (i32)  0 = http, 1 = https
    //   5  = auth_ptr          (i32)  LM offset of authority
    //   6  = auth_len          (i32)
    //   7  = path_ptr          (i32)
    //   8  = path_len          (i32)
    //   9  = fields            (i32)  outgoing fields handle
    //   10 = req               (i32)  outgoing-request handle
    //   11 = retptr1           (i32)  handle()
    //   12 = future            (i32)  future-incoming-response handle
    //   13 = pollable          (i32)
    //   14 = in_buf            (i32)  4-byte slot holding pollable for poll()
    //   15 = retptr2           (i32)  poll()
    //   16 = retptr3           (i32)  future.get()
    //   17 = response          (i32)  incoming-response handle
    //   18 = retptr4           (i32)  consume()
    //   19 = body              (i32)  incoming-body handle
    //   20 = retptr5           (i32)  body.stream()
    //   21 = stream            (i32)  input-stream handle
    //   22 = buf_ptr           (i32)  growing body buffer (cabi_realloc)
    //   23 = buf_cap           (i32)
    //   24 = buf_len           (i32)
    //   25 = data_ptr          (i32)  per-iter blocking-read result ptr
    //   26 = data_len          (i32)
    //   27 = retptr_read       (i32)  per-iter blocking-read retptr (12B)
    //   28 = new_cap           (i32)  scratch for capacity doubling
    //   29 = k                 (i32)  byte-copy iterator
    //   30 = trailers          (i32)  future-trailers handle
    //   31 = h_fields          (i32)  fields handle from incoming-response.headers
    //   32 = h_retptr          (i32)  retptr for fields.entries (8 bytes)
    //   33 = h_entries_ptr     (i32)  list base address
    //   34 = h_entries_len     (i32)  list length (entry count)
    //   35 = h_idx             (i32)  loop counter
    //   36 = h_entry_addr      (i32)  entries_ptr + idx*16
    //   37 = h_name_ptr        (i32)  per-entry: field-key str ptr
    //   38 = h_name_len        (i32)
    //   39 = h_val_ptr         (i32)  per-entry: field-value list ptr
    //   40 = h_val_len         (i32)
    //   41 = arr (ref string)         body string ref + Err scratch
    //   42 = h_name_str (ref string)  per-header name lifted from LM
    //   43 = h_val_str  (ref string)  per-header value lifted from LM
    //   44 = resp (ref HttpResponse)  built before wrapping in Result.Ok
    //   45 = h_map (ref map)          accumulated Map<String, List<String>>
    //   46 = h_opt (ref Option<List<String>>)
    //                                 result of Map.get probe per entry
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
    let mut f = Function::new(vec![
        (40, ValType::I32),
        (3, s_ref),
        (1, resp_ref),
        (1, map_ref),
        (1, opt_list_str_ref),
    ]);

    let p_url = 0u32;
    let l_url_len = 1u32;
    let l_i = 2u32;
    let l_j = 3u32;
    let l_scheme_tag = 4u32;
    let l_auth_ptr = 5u32;
    let l_auth_len = 6u32;
    let l_path_ptr = 7u32;
    let l_path_len = 8u32;
    let l_fields = 9u32;
    let l_req = 10u32;
    let l_retptr1 = 11u32;
    let l_future = 12u32;
    let l_pollable = 13u32;
    let l_in_buf = 14u32;
    let l_retptr2 = 15u32;
    let l_retptr3 = 16u32;
    let l_response = 17u32;
    let l_retptr4 = 18u32;
    let l_body = 19u32;
    let l_retptr5 = 20u32;
    let l_stream = 21u32;
    let l_buf_ptr = 22u32;
    let l_buf_cap = 23u32;
    let l_buf_len = 24u32;
    let l_data_ptr = 25u32;
    let l_data_len = 26u32;
    let l_retptr_read = 27u32;
    let l_new_cap = 28u32;
    let l_k = 29u32;
    let l_trailers = 30u32;
    let l_h_fields = 31u32;
    let l_h_retptr = 32u32;
    let l_h_entries_ptr = 33u32;
    let l_h_entries_len = 34u32;
    let l_h_idx = 35u32;
    let l_h_entry_addr = 36u32;
    let l_h_name_ptr = 37u32;
    let l_h_name_len = 38u32;
    let l_h_val_ptr = 39u32;
    let l_h_val_len = 40u32;
    let l_arr = 41u32;
    let l_h_name_str = 42u32;
    let l_h_val_str = 43u32;
    let l_resp = 44u32;
    let l_h_map = 45u32;
    let l_h_opt = 46u32;

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

    // ── 3. fields = [constructor]fields() ──────────────────────
    f.instruction(&Instruction::Call(h.fields_new_fn));
    f.instruction(&Instruction::LocalSet(l_fields));

    // ── 4. req = [constructor]outgoing-request(fields) ─────────
    // fields ownership transfers in; do NOT drop fields after.
    f.instruction(&Instruction::LocalGet(l_fields));
    f.instruction(&Instruction::Call(h.outgoing_request_new_fn));
    f.instruction(&Instruction::LocalSet(l_req));

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
    // handle at +24), 1 = Err (error-code discriminant at +24,
    // payload at +32 — connection refused, DNS error, etc.).
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
        emit_err(&mut f, b"http: response error");
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
