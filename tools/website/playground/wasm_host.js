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
        // callback below picks it up as `callerRef`, decodes via
        // LM transport, and pipes the resulting JS string through
        // `recordOrDispatch` as the recorder's caller_fn stamp.
        // Pure imports (`float_*`) and the group markers ignore
        // the trailing arg — JS just lets the extra value drop on
        // the floor.
        const dec = (callerRef) => this.averToJs(callerRef);
        return {
            aver: {
                args_len: (callerRef) =>
                    this.recordOrDispatch(
                        "Args.len",
                        [],
                        () => BigInt(this.programArgs.length),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        dec(callerRef),
                    ),
                args_get: (index, callerRef) => {
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
                        dec(callerRef),
                    );
                },
                console_print: (sref, callerRef) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.print",
                        [text],
                        () => this.postConsole("stdout", text),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    );
                },
                console_error: (sref, callerRef) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.error",
                        [text],
                        () => this.postConsole("stderr", text),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    );
                },
                console_warn: (sref, callerRef) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Console.warn",
                        [text],
                        () => this.postConsole("stderr", text),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    );
                },
                console_read_line: (callerRef) => {
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
                        dec(callerRef),
                    );
                },
                random_int: (min, max, callerRef) =>
                    this.recordOrDispatch(
                        "Random.int",
                        [Number(min), Number(max)],
                        () => chooseRandomInt(min, max),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        dec(callerRef),
                    ),
                random_float: (callerRef) =>
                    this.recordOrDispatch(
                        "Random.float",
                        [],
                        () => Math.random(),
                        (json) => Number(json ?? 0),
                        (v) => Number(v),
                        dec(callerRef),
                    ),
                time_unix_ms: (callerRef) =>
                    this.recordOrDispatch(
                        "Time.unixMs",
                        [],
                        () => BigInt(Date.now()),
                        (json) => BigInt(json ?? 0),
                        (v) => Number(v),
                        dec(callerRef),
                    ),
                time_now: (callerRef) =>
                    this.recordOrDispatch(
                        "Time.now",
                        [],
                        () => this.jsToAver(new Date().toISOString()),
                        (json) => this.jsToAver(json ?? ""),
                        () => new Date().toISOString(),
                        dec(callerRef),
                    ),
                time_sleep: (millis, callerRef) =>
                    this.recordOrDispatch(
                        "Time.sleep",
                        [Number(millis)],
                        () => sleepMillis(millis),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                // Float math is pure — no recording, no replay. The
                // wasm-gc imports list these because the engine
                // doesn't expose `f64.sin` directly. The trailing
                // `callerRef` arg is ignored.
                float_sin: (x, _callerRef) => Math.sin(x),
                float_cos: (x, _callerRef) => Math.cos(x),
                float_atan2: (y, x, _callerRef) => Math.atan2(y, x),
                float_pow: (b, e, _callerRef) => Math.pow(b, e),
                terminal_enable_raw_mode: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.enableRawMode",
                        [],
                        () => {
                            this.rawMode = true;
                            this.post({ type: "raw-mode", enabled: true });
                        },
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_disable_raw_mode: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.disableRawMode",
                        [],
                        () => {
                            this.rawMode = false;
                            this.post({ type: "raw-mode", enabled: false });
                        },
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_clear: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.clear",
                        [],
                        () => this.terminal.clear(),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_move_to: (x, y, callerRef) => {
                    const xi = Number(x);
                    const yi = Number(y);
                    this.recordOrDispatch(
                        "Terminal.moveTo",
                        [xi, yi],
                        () => this.terminal.moveTo(xi, yi),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    );
                },
                terminal_print: (sref, callerRef) => {
                    const text = this.averToJs(sref);
                    this.recordOrDispatch(
                        "Terminal.print",
                        [text],
                        () => this.terminal.print(text),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    );
                },
                terminal_set_color: (sref, callerRef) => {
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
                        dec(callerRef),
                    );
                },
                terminal_reset_color: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.resetColor",
                        [],
                        () => this.terminal.resetColor(),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_read_key: (callerRef) => {
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
                        dec(callerRef),
                    );
                },
                terminal_size: (callerRef) => {
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
                        dec(callerRef),
                    );
                },
                terminal_hide_cursor: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.hideCursor",
                        [],
                        () => this.terminal.hideCursor(),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_show_cursor: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.showCursor",
                        [],
                        () => this.terminal.showCursor(),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                terminal_flush: (callerRef) =>
                    this.recordOrDispatch(
                        "Terminal.flush",
                        [],
                        () => this.postTerminalSnapshot(),
                        () => undefined,
                        () => null,
                        dec(callerRef),
                    ),
                // Independent-product structural-scope markers — same
                // contract the wasm-gc CLI host enforces. Trailing
                // `callerRef` ignored (group state lives in the
                // recorder, not in trace records).
                record_enter_group: (_callerRef) => {
                    this.recorder.enterGroup();
                },
                record_set_branch: (i, _callerRef) => {
                    this.recorder.setBranch(Number(i));
                },
                record_exit_group: (_callerRef) => {
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
