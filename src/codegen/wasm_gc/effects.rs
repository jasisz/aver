//! Effectful builtin tracking — `Console.print`, `Http.get`, etc.
//!
//! These don't get bodies in the user module. Instead, the codegen
//! emits `(import "aver" "<name>" (func ...))` and the host (browser
//! / workerd / wasmtime+wasi) supplies the implementation. Same shape
//! the legacy backend uses for effects, just without the
//! `aver_runtime.wasm` middleman.
//!
//! Imports take the lowest fn indices in wasm — `0..K` for K
//! registered effects. User fn indices and builtin helper fn
//! indices shift up by K.

use std::collections::HashMap;

use wasm_encoder::ValType;

use super::WasmGcError;
use super::types::TypeRegistry;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EffectName {
    /// `Console.print(String) -> Unit`. Imported as `aver.console_print`
    /// — the host writes the string to its stdout (or equivalent).
    ConsolePrint,
    ConsoleError,
    ConsoleWarn,
    /// `Time.unixMs() -> Int`. Imported as `aver.time_unix_ms` — host
    /// supplies the current unix timestamp in milliseconds.
    TimeUnixMs,
    // ── Fetch bridge — used by `--handler X` on `--target wasm-gc`.
    //   The synthesised `aver_http_handle` wrapper reads request
    //   fields through these and dispatches the user's HttpResponse
    //   via `Response.text` / `Response.setHeader`. Native wasm-gc
    //   shapes — refs to `(array i8)` carriers, not `(ptr, len)`
    //   pairs. Map<String, List<String>> for headers carries the
    //   per-instance struct ref the registry's Map slot allocates.
    /// `() -> String` — HTTP method (`"GET"`, `"POST"`, …).
    RequestMethod,
    /// `() -> String` — pathname only (no query string).
    RequestUrl,
    /// `() -> String` — query string after `?`, no leading `?`.
    RequestQuery,
    /// `() -> String` — request body (empty string when absent).
    RequestBody,
    /// `() -> Map<String, List<String>>` — host-supplied request
    /// headers map. Empty map when the request has no headers.
    RequestHeadersLoad,
    /// `(status: Int, body: String) -> Unit` — finalize the
    /// pending response. Header writes (`Response.setHeader`) must
    /// happen BEFORE this call.
    ResponseText,
    /// `(name: String, value: String) -> Unit` — append a header
    /// to the pending response. Multi-value headers are produced
    /// by repeated calls with the same name.
    ResponseSetHeader,
    // ── Outgoing HTTP (Http.* surface) — the aver_http_handle
    //   wrapper doesn't use these, but user code reachable from
    //   the handler may; register them so the typechecker's
    //   `! [Http.send]` declaration has a wasm import to resolve.
    /// `(method: String, url: String, body: String, contentType: String)
    /// -> (status: Int, body: String, headers: Map<String, List<String>>, err: String)`.
    HttpSend,
    /// `(name: String, value: String) -> Unit`.
    HttpAddRequestHeader,
    /// `() -> Unit`.
    HttpClearRequestHeaders,
    /// `(name: String) -> String` — Workers env binding lookup;
    /// returns empty string when the binding is absent.
    EnvGet,
    /// `(name: String, value: String) -> Unit` — no-op on
    /// Workers (env is read-only).
    EnvSet,
    // ── CLI / runtime side effects covered by the legacy
    //    `abi.rs` table; ported one-for-one so wasm-gc has the
    //    same surface area.
    /// `() -> String` — read one line from stdin.
    ConsoleReadLine,
    /// `() -> Int` — number of CLI args.
    ArgsLen,
    /// `(i: Int) -> String` — i-th CLI arg.
    ArgsGet,
    /// `() -> Float` — uniform [0, 1).
    RandomFloat,
    /// `(lo: Int, hi: Int) -> Int` — uniform inclusive on both ends.
    RandomInt,
    /// `(ms: Int) -> Unit` — block (or no-op when the host can't).
    TimeSleep,
    /// `() -> String` — RFC 3339 timestamp from the host clock.
    TimeNow,
    /// libm bridges — wasm has no native sin/cos/atan2/pow, so the
    /// host (Math.sin / `f64::sin`) supplies them.
    FloatSin,
    FloatCos,
    FloatAtan2,
    FloatPow,
    // ── Terminal raw-mode TUI surface — same set the legacy `abi.rs`
    //   ports. Hosts that don't implement Terminal (workers, browser)
    //   should treat these as no-ops; Aver `! [Terminal.foo]` effect
    //   declarations drive when imports are emitted.
    /// `() -> Unit` — switch the host terminal to raw mode (no line
    /// buffering, no echo). Pairs with `Terminal.disableRawMode`.
    TerminalEnableRawMode,
    /// `() -> Unit` — restore the host terminal's pre-raw mode.
    TerminalDisableRawMode,
    /// `() -> Unit` — clear the entire terminal screen.
    TerminalClear,
    /// `(row: Int, col: Int) -> Unit` — move the cursor to the given
    /// 1-indexed row/col.
    TerminalMoveTo,
    /// `(s: String) -> Unit` — write `s` to the terminal at the
    /// current cursor position. Doesn't append a newline.
    TerminalPrint,
    /// `(color: Int) -> Unit` — set the foreground colour to the
    /// given ANSI 256 colour code.
    TerminalSetColor,
    /// `() -> Unit` — reset foreground colour to default.
    TerminalResetColor,
    /// `() -> Option<String>` — read one keypress non-blocking. None
    /// if no input is available; Some(byte_string) for printable +
    /// special-key sequences.
    TerminalReadKey,
    /// `() -> Terminal.Size` — current terminal dimensions in
    /// (width, height) cells.
    TerminalSize,
    /// `() -> Unit` — hide the cursor.
    TerminalHideCursor,
    /// `() -> Unit` — show the cursor.
    TerminalShowCursor,
    /// `() -> Unit` — flush any buffered terminal output to the host.
    TerminalFlush,
}

impl EffectName {
    pub(super) fn from_dotted(s: &str) -> Option<Self> {
        match s {
            "Console.print" => Some(Self::ConsolePrint),
            "Console.error" => Some(Self::ConsoleError),
            "Console.warn" => Some(Self::ConsoleWarn),
            "Time.unixMs" => Some(Self::TimeUnixMs),
            "Request.method" => Some(Self::RequestMethod),
            "Request.url" | "Request.path" => Some(Self::RequestUrl),
            "Request.query" => Some(Self::RequestQuery),
            "Request.body" => Some(Self::RequestBody),
            "Request.headersLoad" | "Request.headers" => Some(Self::RequestHeadersLoad),
            "Response.text" => Some(Self::ResponseText),
            "Response.setHeader" => Some(Self::ResponseSetHeader),
            "Http.send" => Some(Self::HttpSend),
            "Http.addRequestHeader" => Some(Self::HttpAddRequestHeader),
            "Http.clearRequestHeaders" => Some(Self::HttpClearRequestHeaders),
            "Env.get" => Some(Self::EnvGet),
            "Env.set" => Some(Self::EnvSet),
            "Console.readLine" => Some(Self::ConsoleReadLine),
            "Args._len" | "Args.len" => Some(Self::ArgsLen),
            "Args._get" | "Args.get" => Some(Self::ArgsGet),
            "Random.float" => Some(Self::RandomFloat),
            "Random.int" => Some(Self::RandomInt),
            "Time.sleep" => Some(Self::TimeSleep),
            "Time.now" => Some(Self::TimeNow),
            "Float.sin" => Some(Self::FloatSin),
            "Float.cos" => Some(Self::FloatCos),
            "Float.atan2" => Some(Self::FloatAtan2),
            "Float.pow" => Some(Self::FloatPow),
            "Terminal.enableRawMode" => Some(Self::TerminalEnableRawMode),
            "Terminal.disableRawMode" => Some(Self::TerminalDisableRawMode),
            "Terminal.clear" => Some(Self::TerminalClear),
            "Terminal.moveTo" => Some(Self::TerminalMoveTo),
            "Terminal.print" => Some(Self::TerminalPrint),
            "Terminal.setColor" => Some(Self::TerminalSetColor),
            "Terminal.resetColor" => Some(Self::TerminalResetColor),
            "Terminal.readKey" => Some(Self::TerminalReadKey),
            "Terminal.size" => Some(Self::TerminalSize),
            "Terminal.hideCursor" => Some(Self::TerminalHideCursor),
            "Terminal.showCursor" => Some(Self::TerminalShowCursor),
            "Terminal.flush" => Some(Self::TerminalFlush),
            _ => None,
        }
    }

    pub(super) fn canonical(self) -> &'static str {
        match self {
            Self::ConsolePrint => "Console.print",
            Self::ConsoleError => "Console.error",
            Self::ConsoleWarn => "Console.warn",
            Self::TimeUnixMs => "Time.unixMs",
            Self::RequestMethod => "Request.method",
            Self::RequestUrl => "Request.url",
            Self::RequestQuery => "Request.query",
            Self::RequestBody => "Request.body",
            Self::RequestHeadersLoad => "Request.headersLoad",
            Self::ResponseText => "Response.text",
            Self::ResponseSetHeader => "Response.setHeader",
            Self::HttpSend => "Http.send",
            Self::HttpAddRequestHeader => "Http.addRequestHeader",
            Self::HttpClearRequestHeaders => "Http.clearRequestHeaders",
            Self::EnvGet => "Env.get",
            Self::EnvSet => "Env.set",
            Self::ConsoleReadLine => "Console.readLine",
            Self::ArgsLen => "Args._len",
            Self::ArgsGet => "Args._get",
            Self::RandomFloat => "Random.float",
            Self::RandomInt => "Random.int",
            Self::TimeSleep => "Time.sleep",
            Self::TimeNow => "Time.now",
            Self::FloatSin => "Float.sin",
            Self::FloatCos => "Float.cos",
            Self::FloatAtan2 => "Float.atan2",
            Self::FloatPow => "Float.pow",
            Self::TerminalEnableRawMode => "Terminal.enableRawMode",
            Self::TerminalDisableRawMode => "Terminal.disableRawMode",
            Self::TerminalClear => "Terminal.clear",
            Self::TerminalMoveTo => "Terminal.moveTo",
            Self::TerminalPrint => "Terminal.print",
            Self::TerminalSetColor => "Terminal.setColor",
            Self::TerminalResetColor => "Terminal.resetColor",
            Self::TerminalReadKey => "Terminal.readKey",
            Self::TerminalSize => "Terminal.size",
            Self::TerminalHideCursor => "Terminal.hideCursor",
            Self::TerminalShowCursor => "Terminal.showCursor",
            Self::TerminalFlush => "Terminal.flush",
        }
    }

    /// Wasm import (module, field) pair. Module is always `aver` for
    /// our effects — host supplies a single namespace.
    pub(super) fn import_pair(self) -> (&'static str, &'static str) {
        match self {
            Self::ConsolePrint => ("aver", "console_print"),
            Self::ConsoleError => ("aver", "console_error"),
            Self::ConsoleWarn => ("aver", "console_warn"),
            Self::TimeUnixMs => ("aver", "time_unix_ms"),
            Self::RequestMethod => ("aver", "request_method"),
            Self::RequestUrl => ("aver", "request_url"),
            Self::RequestQuery => ("aver", "request_query"),
            Self::RequestBody => ("aver", "request_body"),
            Self::RequestHeadersLoad => ("aver", "request_headers_load"),
            Self::ResponseText => ("aver", "response_text"),
            Self::ResponseSetHeader => ("aver", "response_set_header"),
            Self::HttpSend => ("aver", "http_send"),
            Self::HttpAddRequestHeader => ("aver", "http_add_request_header"),
            Self::HttpClearRequestHeaders => ("aver", "http_clear_request_headers"),
            Self::EnvGet => ("aver", "env_get"),
            Self::EnvSet => ("aver", "env_set"),
            Self::ConsoleReadLine => ("aver", "console_read_line"),
            Self::ArgsLen => ("aver", "args_len"),
            Self::ArgsGet => ("aver", "args_get"),
            Self::RandomFloat => ("aver", "random_float"),
            Self::RandomInt => ("aver", "random_int"),
            Self::TimeSleep => ("aver", "time_sleep"),
            Self::TimeNow => ("aver", "time_now"),
            Self::FloatSin => ("aver", "float_sin"),
            Self::FloatCos => ("aver", "float_cos"),
            Self::FloatAtan2 => ("aver", "float_atan2"),
            Self::FloatPow => ("aver", "float_pow"),
            Self::TerminalEnableRawMode => ("aver", "terminal_enable_raw_mode"),
            Self::TerminalDisableRawMode => ("aver", "terminal_disable_raw_mode"),
            Self::TerminalClear => ("aver", "terminal_clear"),
            Self::TerminalMoveTo => ("aver", "terminal_move_to"),
            Self::TerminalPrint => ("aver", "terminal_print"),
            Self::TerminalSetColor => ("aver", "terminal_set_color"),
            Self::TerminalResetColor => ("aver", "terminal_reset_color"),
            Self::TerminalReadKey => ("aver", "terminal_read_key"),
            Self::TerminalSize => ("aver", "terminal_size"),
            Self::TerminalHideCursor => ("aver", "terminal_hide_cursor"),
            Self::TerminalShowCursor => ("aver", "terminal_show_cursor"),
            Self::TerminalFlush => ("aver", "terminal_flush"),
        }
    }

    /// Param types declared in the wasm import. Strings are passed as
    /// `(ref null any)` — engine subtyping accepts our `(ref null
    /// $string)` and the host doesn't have to know the concrete type
    /// idx. The headers Map crossing uses the registered concrete
    /// `Map<String, List<String>>` ref so the host bridge has a
    /// type-safe handle.
    pub(super) fn params(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn => {
                Ok(vec![any_ref_ty()])
            }
            Self::TimeUnixMs => Ok(vec![]),
            Self::RequestMethod
            | Self::RequestUrl
            | Self::RequestQuery
            | Self::RequestBody
            | Self::RequestHeadersLoad
            | Self::HttpClearRequestHeaders => Ok(vec![]),
            Self::ResponseText => Ok(vec![ValType::I64, any_ref_ty()]),
            Self::ResponseSetHeader => Ok(vec![any_ref_ty(), any_ref_ty()]),
            Self::HttpSend => Ok(vec![
                any_ref_ty(),
                any_ref_ty(),
                any_ref_ty(),
                any_ref_ty(),
            ]),
            Self::HttpAddRequestHeader => Ok(vec![any_ref_ty(), any_ref_ty()]),
            Self::EnvGet => Ok(vec![any_ref_ty()]),
            Self::EnvSet => Ok(vec![any_ref_ty(), any_ref_ty()]),
            Self::ConsoleReadLine
            | Self::ArgsLen
            | Self::RandomFloat
            | Self::TimeNow => Ok(vec![]),
            Self::ArgsGet | Self::TimeSleep => Ok(vec![ValType::I64]),
            Self::RandomInt => Ok(vec![ValType::I64, ValType::I64]),
            Self::FloatSin | Self::FloatCos => Ok(vec![ValType::F64]),
            Self::FloatAtan2 | Self::FloatPow => Ok(vec![ValType::F64, ValType::F64]),
            Self::TerminalEnableRawMode
            | Self::TerminalDisableRawMode
            | Self::TerminalClear
            | Self::TerminalResetColor
            | Self::TerminalReadKey
            | Self::TerminalSize
            | Self::TerminalHideCursor
            | Self::TerminalShowCursor
            | Self::TerminalFlush => Ok(vec![]),
            Self::TerminalMoveTo => Ok(vec![ValType::I64, ValType::I64]),
            Self::TerminalPrint => Ok(vec![any_ref_ty()]),
            Self::TerminalSetColor => Ok(vec![ValType::I64]),
        }
    }

    pub(super) fn results(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn => Ok(vec![]),
            Self::TimeUnixMs => Ok(vec![ValType::I64]),
            Self::RequestMethod
            | Self::RequestUrl
            | Self::RequestQuery
            | Self::RequestBody
            | Self::EnvGet => Ok(vec![string_ref_ty(registry)?]),
            Self::RequestHeadersLoad => Ok(vec![map_string_list_string_ref_ty(registry)?]),
            Self::ResponseText
            | Self::ResponseSetHeader
            | Self::HttpAddRequestHeader
            | Self::HttpClearRequestHeaders
            | Self::EnvSet => Ok(vec![]),
            Self::HttpSend => Ok(vec![
                ValType::I64,
                string_ref_ty(registry)?,
                map_string_list_string_ref_ty(registry)?,
                string_ref_ty(registry)?,
            ]),
            Self::ConsoleReadLine | Self::ArgsGet | Self::TimeNow => {
                Ok(vec![string_ref_ty(registry)?])
            }
            Self::ArgsLen | Self::RandomInt => Ok(vec![ValType::I64]),
            Self::TimeSleep => Ok(vec![]),
            Self::RandomFloat
            | Self::FloatSin
            | Self::FloatCos
            | Self::FloatAtan2
            | Self::FloatPow => Ok(vec![ValType::F64]),
            Self::TerminalEnableRawMode
            | Self::TerminalDisableRawMode
            | Self::TerminalClear
            | Self::TerminalMoveTo
            | Self::TerminalPrint
            | Self::TerminalSetColor
            | Self::TerminalResetColor
            | Self::TerminalHideCursor
            | Self::TerminalShowCursor
            | Self::TerminalFlush => Ok(vec![]),
            Self::TerminalReadKey => {
                // `Option<String>` ref — eager-registered when any
                // call site reaches for `String.charAt` / `Char.fromCode`,
                // or now also when `Terminal.readKey` shows up.
                let opt_idx = registry
                    .option_type_idx("Option<String>")
                    .ok_or(WasmGcError::Validation(
                        "Terminal.readKey: Option<String> slot not registered".into(),
                    ))?;
                Ok(vec![ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
                })])
            }
            Self::TerminalSize => {
                let idx = registry
                    .record_type_idx("Terminal.Size")
                    .ok_or(WasmGcError::Validation(
                        "Terminal.size: Terminal.Size record slot not registered \
                         (did you call Terminal.size in user code?)"
                            .into(),
                    ))?;
                Ok(vec![ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(idx),
                })])
            }
        }
    }
}

fn map_string_list_string_ref_ty(
    registry: &TypeRegistry,
) -> Result<ValType, WasmGcError> {
    let slots = registry
        .map_slots("Map<String,List<String>>")
        .ok_or(WasmGcError::Validation(
            "fetch effect requires `Map<String, List<String>>` slot but none was registered"
                .into(),
        ))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(slots.map),
    }))
}

fn any_ref_ty() -> ValType {
    ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Abstract {
            shared: false,
            ty: wasm_encoder::AbstractHeapType::Any,
        },
    })
}

/// Per-module registry of used effects. Allocates wasm fn indices
/// starting at 0 (imports come first); the offset returned via
/// `import_count` is the value that gets added to every other fn
/// index in the module.
#[derive(Default)]
pub(super) struct EffectRegistry {
    order: Vec<EffectName>,
    wasm_fn_idx: HashMap<EffectName, u32>,
    wasm_type_idx: HashMap<EffectName, u32>,
}

impl EffectRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn register(&mut self, name: EffectName) {
        if !self.order.contains(&name) {
            self.order.push(name);
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = EffectName> + '_ {
        self.order.iter().copied()
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    /// Reserve type and fn-idx slots for each registered effect.
    /// Imports occupy fn-idx 0..K; type indices come from the same
    /// counter the user-fn types use, deferred by the caller.
    pub(super) fn assign_slots(&mut self, next_type_idx: &mut u32) {
        for (i, name) in self.order.iter().copied().enumerate() {
            self.wasm_fn_idx.insert(name, i as u32);
            self.wasm_type_idx.insert(name, *next_type_idx);
            *next_type_idx += 1;
        }
    }

    pub(super) fn lookup_wasm_fn_idx(&self, name: EffectName) -> Option<u32> {
        self.wasm_fn_idx.get(&name).copied()
    }

    pub(super) fn lookup_wasm_type_idx(&self, name: EffectName) -> Option<u32> {
        self.wasm_type_idx.get(&name).copied()
    }
}

fn string_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "effect requires String repr but no string type slot was allocated".into(),
        ))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}
