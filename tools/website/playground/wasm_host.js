import { TerminalBuffer } from "./browser_terminal.js";
import { EffectReplayState, REPLAY_MODE } from "./replay_state.js";

const COLOR_NAMES = new Set([
    "default",
    "red",
    "green",
    "yellow",
    "blue",
    "white",
    "cyan",
    "magenta",
    "black",
]);

const KEY_QUEUE_HEAD = 0;
const KEY_QUEUE_TAIL = 1;
const KEY_QUEUE_DATA = 2;
const KEY_QUEUE_CAPACITY = 128;
const KEY_CODE_UP = 1;
const KEY_CODE_DOWN = 2;
const KEY_CODE_LEFT = 3;
const KEY_CODE_RIGHT = 4;
const KEY_CODE_ESCAPE = 5;
const KEY_CODE_ENTER = 6;
const KEY_CODE_CHAR_BASE = 1024;

function chooseRandomInt(min, max) {
    const lo = BigInt.asIntN(64, min);
    const hi = BigInt.asIntN(64, max);
    if (hi <= lo) return lo;
    const span = hi - lo + 1n;
    const buf = new Uint32Array(2);
    if (globalThis.crypto?.getRandomValues) {
        globalThis.crypto.getRandomValues(buf);
    } else {
        buf[0] = Math.floor(Math.random() * 0xffffffff);
        buf[1] = Math.floor(Math.random() * 0xffffffff);
    }
    const rnd = (BigInt(buf[0]) << 32n) | BigInt(buf[1]);
    return lo + (rnd % span);
}

function sleepMillis(ms) {
    const millis = Math.max(0, Number(ms));
    if (typeof SharedArrayBuffer === "function" && typeof Atomics.wait === "function") {
        const buffer = new SharedArrayBuffer(4);
        const view = new Int32Array(buffer);
        Atomics.wait(view, 0, 0, millis);
        return;
    }
    const start = performance.now();
    while (performance.now() - start < millis) {
        // Busy fallback for engines without Atomics.wait in workers.
    }
}

function decodeKeyCode(code) {
    switch (code) {
        case KEY_CODE_UP:
            return "up";
        case KEY_CODE_DOWN:
            return "down";
        case KEY_CODE_LEFT:
            return "left";
        case KEY_CODE_RIGHT:
            return "right";
        case KEY_CODE_ESCAPE:
            return "esc";
        case KEY_CODE_ENTER:
            return "enter";
        default:
            if (code >= KEY_CODE_CHAR_BASE) {
                return String.fromCodePoint(code - KEY_CODE_CHAR_BASE);
            }
            return null;
    }
}

/// Wasm-gc playground host.
///
/// Bridges effect imports (`aver/*`) for browser-played games. Strings
/// cross via the LM transport (`__rt_string_from_lm` / `_to_lm` —
/// host writes UTF-8 bytes into linear memory, calls a getter that
/// materialises the bytes as a wasm-gc `(array i8)` ref, and vice
/// versa). Structured returns (`Option<String>`, `Result<String,String>`,
/// `Terminal.Size`) are constructed via wasm-owned factory exports
/// (`__rt_option_string_some/none`, `__rt_result_string_string_ok/err`,
/// `__rt_record_terminal_size_make`) — JS can't build wasm-gc structs
/// directly so the binary exports per-type constructors.
export class AverBrowserHost {
    constructor(postMessageFn) {
        this.post = postMessageFn;
        this.instance = null;
        this.encoder = new TextEncoder();
        this.decoder = new TextDecoder();
        this.terminal = new TerminalBuffer(80, 35);
        // Recording / replay state — drives the Step 4 native wasm-gc
        // record/replay path so the playground can record under V8
        // wasm-gc directly (instead of bouncing through VM-in-wasm32).
        this.recorder = new EffectReplayState();
        this.keyQueue = [];
        this.keyQueueView = null;
        this.lineQueue = [];
        this.lineBufferView = null;
        this.lineBufferBytes = null;
        this.programArgs = [];
        this.rawMode = false;
        this.lastFlushMs = 0;
    }

    setInstance(instance) {
        this.instance = instance;
        this.callerFnTable = this.materialiseCallerFnTable(instance);
    }

    /// Read the caller-fn name table once at instance creation.
    /// Compiler exports `__caller_fn_count() -> i32` and
    /// `__caller_fn_name(i32) -> ref null $string`; we walk
    /// `0..count`, decode each ref via `averToJs` (which uses the
    /// existing LM bridge), cache the JS strings in an array.
    /// Per effect call the trailing `i32` arg indexes into this
    /// array — no LM round-trip on the hot path.
    materialiseCallerFnTable(instance) {
        const exports = instance.exports;
        if (typeof exports.__caller_fn_count !== "function" ||
            typeof exports.__caller_fn_name !== "function") {
            return [];
        }
        const count = exports.__caller_fn_count();
        const out = [];
        for (let i = 0; i < count; i++) {
            const ref = exports.__caller_fn_name(i);
            out.push(ref == null ? "main" : this.averToJs(ref));
        }
        return out;
    }

    callerFnFromIdx(idx) {
        if (typeof idx !== "number") return "main";
        const name = this.callerFnTable && this.callerFnTable[idx];
        return name || "main";
    }

    setTerminalSize(cols, rows) {
        this.terminal.resize(cols, rows);
        this.postTerminalSnapshot();
    }

    setSharedKeyBuffer(buffer) {
        this.keyQueueView = buffer ? new Int32Array(buffer) : null;
        this.keyQueue = [];
    }

    setSharedLineBuffer(buffer) {
        this.lineBufferView = buffer ? new Int32Array(buffer) : null;
        this.lineBufferBytes = buffer ? new Uint8Array(buffer) : null;
    }

    setProgramArgs(args) {
        this.programArgs = Array.isArray(args) ? args.map((arg) => String(arg)) : [];
    }

    enqueueKey(key) {
        this.keyQueue.push(key);
    }

    enqueueLine(line) {
        this.lineQueue.push(line);
        this.post({ type: "line-queue", queued: this.lineQueue.length });
    }

    /// Blocking readLine: wait on SharedArrayBuffer for main thread.
    /// Layout: [0]=ready flag, [1]=length, [2..]=UTF-8 bytes.
    blockingReadLine() {
        const view = this.lineBufferView;
        if (!view) {
            if (this.lineQueue.length > 0) return this.lineQueue.shift();
            throw new Error(
                "Console.readLine() requires cross-origin isolation. Serve the playground with COOP/COEP headers.",
            );
        }
        Atomics.store(view, 0, 0);
        this.post({ type: "readline-wait" });
        Atomics.wait(view, 0, 0);
        const len = Atomics.load(view, 1);
        if (len <= 0) return "";
        const bytes = this.lineBufferBytes.slice(8, 8 + len);
        return new TextDecoder().decode(bytes);
    }

    dequeueKey() {
        if (this.keyQueue.length > 0) return this.keyQueue.shift();
        if (this.keyQueueView) {
            const head = Atomics.load(this.keyQueueView, KEY_QUEUE_HEAD);
            const tail = Atomics.load(this.keyQueueView, KEY_QUEUE_TAIL);
            if (head === tail) return null;
            const slot = KEY_QUEUE_DATA + (head % KEY_QUEUE_CAPACITY);
            const code = Atomics.load(this.keyQueueView, slot);
            Atomics.store(this.keyQueueView, KEY_QUEUE_HEAD, head + 1);
            return decodeKeyCode(code);
        }
        return null;
    }

    memU8() {
        return new Uint8Array(this.instance.exports.memory.buffer);
    }

    ensurePages(needed) {
        const exports = this.instance.exports;
        const cur = Number(exports.__rt_memory_pages());
        if (needed > cur) exports.__rt_memory_grow(needed - cur);
    }

    /// JS string → wasm-gc `(ref null $string)` via LM transport.
    jsToAver(text) {
        const s = text ?? "";
        const upperBytes = s.length * 3;
        this.ensurePages(((upperBytes + 65535) >> 16) || 1);
        const { written } = this.encoder.encodeInto(s, this.memU8());
        return this.instance.exports.__rt_string_from_lm(written);
    }

    /// wasm-gc string ref → JS string via LM transport.
    averToJs(s) {
        const len = Number(this.instance.exports.__rt_string_to_lm(s));
        return this.decoder.decode(this.memU8().subarray(0, len));
    }

    postConsole(level, text) {
        this.post({ type: "console", level, text });
    }

    postTerminalSnapshot() {
        // Throttle to ~60fps so the message queue can't grow unbounded
        // when the wasm module flushes faster than the UI renders.
        const now = performance.now();
        if (now - this.lastFlushMs < 16) return;
        this.lastFlushMs = now;
        const snapshot = this.terminal.toSnapshot();
        // No memory reporting: under wasm-gc the engine owns the program
        // heap and host code can't observe it. The exported `memory` is
        // a 1-page LM transport buffer for string round-trips, not a
        // user heap — reporting its size would mislead. Leaving the
        // field out keeps the UI label empty (handled in `app.js`).
        this.post({
            type: "terminal",
            cols: this.terminal.cols,
            rows: this.terminal.rows,
            snapshot,
            blank: this.terminal.isBlank(),
            rawMode: this.rawMode,
        }, [snapshot.chars.buffer, snapshot.colors.buffer]);
    }

    /// Run `realCall()` if the recorder is in Normal or Recording
    /// mode, returning its native result. In Replay mode the cached
    /// outcome is decoded via `decodeOutcome(json)` instead and the
    /// real call is skipped — same shape the wasm-gc executor's
    /// `try_replay` enforces. Recording mode appends the live result
    /// (translated through `encodeOutcome(value)`) to the trace
    /// before returning. Effects that don't carry a return value
    /// pass null encoders / decoders.
    recordOrDispatch(effectType, args, realCall, decodeOutcome, encodeOutcome, callerFn) {
        const r = this.recorder;
        if (r.mode === REPLAY_MODE.REPLAYING) {
            const replayResult = r.replayEffect(effectType, args);
            if (!replayResult.skip) {
                const outcome = replayResult.outcome ?? { kind: "value", value: null };
                if (outcome.kind === "runtime_error") {
                    throw new Error(outcome.message ?? `replay runtime error in ${effectType}`);
                }
                return decodeOutcome
                    ? decodeOutcome(outcome.value ?? null)
                    : undefined;
            }
        }
        const live = realCall();
        if (r.mode === REPLAY_MODE.RECORDING) {
            const outcomeJson = encodeOutcome
                ? encodeOutcome(live)
                : null;
            const record = r.recordEffect(
                effectType,
                args,
                { kind: "value", value: outcomeJson },
                callerFn || "main",
            );
            // Stream the freshly-recorded effect to the main thread
            // so it can mirror the trace incrementally. Lets the
            // user click Stop mid-game and still walk away with
            // every effect captured before the worker terminate'd.
            // No-op when the recorder rejected the entry (mode race).
            if (record !== null) {
                this.post({ type: "trace-effect", effect: record });
            }
        }
        return live;
    }

    createImports() {
        // Every effect import declares a trailing `caller_fn:
        // any_ref` param now (see `effects.rs::params`). Each
        // callback below picks it up as `callerIdx`, decodes via
        // LM transport, and pipes the resulting JS string through
        // `recordOrDispatch` as the recorder's caller_fn stamp.
        // Pure imports (`float_*`) and the group markers ignore
        // the trailing arg — JS just lets the extra value drop on
        // the floor.
        const dec = (callerIdx) => this.averToJs(callerIdx);
        return {
            aver: {
                args_len: (callerIdx) =>
                    this.recordOrDispatch(
                        "Args.len",
                        [],
                        () => BigInt(this.programArgs.length),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        this.callerFnFromIdx(callerIdx),
                    ),
                args_get: (index, callerIdx) => {
                    const idx = Number(index);
                    return this.recordOrDispatch(
                        "Args.get",
                        [idx],
                        () => {
                            const value =
                                idx >= 0 && idx < this.programArgs.length
                                    ? this.programArgs[idx]
                                    : "";
                            return this.jsToAver(value);
                        },
                        (json) => this.jsToAver(json ?? ""),
                        () =>
                            idx >= 0 && idx < this.programArgs.length
                                ? this.programArgs[idx]
                                : "",
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                console_print: (sref, callerIdx) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.print",
                        [text],
                        () => this.postConsole("stdout", text),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                console_error: (sref, callerIdx) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.error",
                        [text],
                        () => this.postConsole("stderr", text),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                console_warn: (sref, callerIdx) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.warn",
                        [text],
                        () => this.postConsole("stderr", text),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                console_read_line: (callerIdx) => {
                    const exports = this.instance.exports;
                    return this.recordOrDispatch(
                        "Console.readLine",
                        [],
                        () => {
                            try {
                                const line = this.blockingReadLine();
                                return exports.__rt_result_string_string_ok(
                                    this.jsToAver(line),
                                );
                            } catch (err) {
                                const msg =
                                    err instanceof Error ? err.message : String(err);
                                return exports.__rt_result_string_string_err(
                                    this.jsToAver(msg),
                                );
                            }
                        },
                        (json) => this.decodeResultStringMarker(json),
                        (_ref) => {
                            const peek = this.lineQueue.length
                                ? this.lineQueue[0]
                                : "";
                            return { $ok: peek };
                        },
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                random_int: (min, max, callerIdx) =>
                    this.recordOrDispatch(
                        "Random.int",
                        [Number(min), Number(max)],
                        () => chooseRandomInt(min, max),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        this.callerFnFromIdx(callerIdx),
                    ),
                random_float: (callerIdx) =>
                    this.recordOrDispatch(
                        "Random.float",
                        [],
                        () => Math.random(),
                        (json) => Number(json ?? 0),
                        (v) => Number(v),
                        this.callerFnFromIdx(callerIdx),
                    ),
                time_unix_ms: (callerIdx) =>
                    this.recordOrDispatch(
                        "Time.unixMs",
                        [],
                        () => BigInt(Date.now()),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        this.callerFnFromIdx(callerIdx),
                    ),
                time_now: (callerIdx) =>
                    this.recordOrDispatch(
                        "Time.now",
                        [],
                        () => this.jsToAver(new Date().toISOString()),
                        (json) => this.jsToAver(json ?? ""),
                        () => new Date().toISOString(),
                        this.callerFnFromIdx(callerIdx),
                    ),
                time_sleep: (millis, callerIdx) =>
                    this.recordOrDispatch(
                        "Time.sleep",
                        [Number(millis)],
                        () => sleepMillis(millis),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                // Float math is pure — no recording, no replay. The
                // wasm-gc imports list these because the engine
                // doesn't expose `f64.sin` directly. The trailing
                // `callerIdx` arg is ignored.
                float_sin: (x, _callerIdx) => Math.sin(x),
                float_cos: (x, _callerIdx) => Math.cos(x),
                float_atan2: (y, x, _callerIdx) => Math.atan2(y, x),
                float_pow: (b, e, _callerIdx) => Math.pow(b, e),
                terminal_enable_raw_mode: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.enableRawMode",
                        [],
                        () => {
                            this.rawMode = true;
                            this.post({ type: "raw-mode", enabled: true });
                        },
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_disable_raw_mode: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.disableRawMode",
                        [],
                        () => {
                            this.rawMode = false;
                            this.post({ type: "raw-mode", enabled: false });
                        },
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_clear: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.clear",
                        [],
                        () => this.terminal.clear(),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_move_to: (x, y, callerIdx) => {
                    const xi = Number(x);
                    const yi = Number(y);
                    this.recordOrDispatch(
                        "Terminal.moveTo",
                        [xi, yi],
                        () => this.terminal.moveTo(xi, yi),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                terminal_print: (sref, callerIdx) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Terminal.print",
                        [text],
                        () => this.terminal.print(text),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                terminal_set_color: (sref, callerIdx) => {
                    const color = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Terminal.setColor",
                        [color],
                        () =>
                            this.terminal.setColor(
                                COLOR_NAMES.has(color) ? color : "default",
                            ),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                terminal_reset_color: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.resetColor",
                        [],
                        () => this.terminal.resetColor(),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_read_key: (callerIdx) => {
                    const exports = this.instance.exports;
                    return this.recordOrDispatch(
                        "Terminal.readKey",
                        [],
                        () => {
                            const key = this.dequeueKey();
                            if (!key) return exports.__rt_option_string_none();
                            return exports.__rt_option_string_some(this.jsToAver(key));
                        },
                        (json) => {
                            if (json && typeof json === "object" && "$some" in json) {
                                return exports.__rt_option_string_some(
                                    this.jsToAver(json.$some ?? ""),
                                );
                            }
                            return exports.__rt_option_string_none();
                        },
                        (_ref) => {
                            const head = this.keyQueue[0];
                            return head
                                ? { $some: head }
                                : { $none: true };
                        },
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                terminal_size: (callerIdx) => {
                    const exports = this.instance.exports;
                    const cols = this.terminal.cols;
                    const rows = this.terminal.rows;
                    return this.recordOrDispatch(
                        "Terminal.size",
                        [],
                        () =>
                            exports.__rt_record_terminal_size_make(
                                BigInt(cols),
                                BigInt(rows),
                            ),
                        (json) => {
                            const fields = json?.$record?.fields ?? {};
                            return exports.__rt_record_terminal_size_make(
                                BigInt(fields.width ?? cols),
                                BigInt(fields.height ?? rows),
                            );
                        },
                        () => ({
                            $record: {
                                type: "Terminal.Size",
                                fields: { width: cols, height: rows },
                            },
                        }),
                        this.callerFnFromIdx(callerIdx),
                    );
                },
                terminal_hide_cursor: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.hideCursor",
                        [],
                        () => this.terminal.hideCursor(),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_show_cursor: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.showCursor",
                        [],
                        () => this.terminal.showCursor(),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                terminal_flush: (callerIdx) =>
                    this.recordOrDispatch(
                        "Terminal.flush",
                        [],
                        () => this.postTerminalSnapshot(),
                        () => undefined,
                        () => null,
                        this.callerFnFromIdx(callerIdx),
                    ),
                // Independent-product structural-scope markers — same
                // contract the wasm-gc CLI host enforces. Trailing
                // `callerIdx` ignored (group state lives in the
                // recorder, not in trace records).
                record_enter_group: (_callerIdx) => {
                    this.recorder.enterGroup();
                },
                record_set_branch: (i, _callerIdx) => {
                    this.recorder.setBranch(Number(i));
                },
                record_exit_group: (_callerIdx) => {
                    this.recorder.exitGroup();
                },
            },
        };
    }

    /// Decode a `Result<String, String>` marker JSON into the wasm-gc
    /// engine value via the module's factory exports. Mirrors the
    /// Rust-side `decode_result_string` helper.
    decodeResultStringMarker(json) {
        const exports = this.instance.exports;
        if (json && typeof json === "object" && "$ok" in json) {
            return exports.__rt_result_string_string_ok(this.jsToAver(json.$ok ?? ""));
        }
        if (json && typeof json === "object" && "$err" in json) {
            return exports.__rt_result_string_string_err(
                this.jsToAver(json.$err ?? ""),
            );
        }
        // Fallback: empty Ok (defensively, when the trace is
        // malformed at this position).
        return exports.__rt_result_string_string_ok(this.jsToAver(""));
    }
}
