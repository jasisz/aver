import { TerminalBuffer } from "./browser_terminal.js";

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

    createImports() {
        return {
            aver: {
                args_len: () => BigInt(this.programArgs.length),
                args_get: (index) => {
                    const idx = Number(index);
                    const value =
                        idx >= 0 && idx < this.programArgs.length ? this.programArgs[idx] : "";
                    return this.jsToAver(value);
                },
                console_print: (sref) => {
                    this.postConsole("stdout", this.averToJs(sref));
                },
                console_error: (sref) => {
                    this.postConsole("stderr", this.averToJs(sref));
                },
                console_warn: (sref) => {
                    this.postConsole("stderr", this.averToJs(sref));
                },
                console_read_line: () => {
                    const exports = this.instance.exports;
                    try {
                        const line = this.blockingReadLine();
                        return exports.__rt_result_string_string_ok(this.jsToAver(line));
                    } catch (err) {
                        const msg = err instanceof Error ? err.message : String(err);
                        return exports.__rt_result_string_string_err(this.jsToAver(msg));
                    }
                },
                random_int: (min, max) => chooseRandomInt(min, max),
                random_float: () => Math.random(),
                time_unix_ms: () => BigInt(Date.now()),
                time_now: () => this.jsToAver(new Date().toISOString()),
                time_sleep: (millis) => sleepMillis(millis),
                float_sin: (x) => Math.sin(x),
                float_cos: (x) => Math.cos(x),
                float_atan2: (y, x) => Math.atan2(y, x),
                float_pow: (b, e) => Math.pow(b, e),
                terminal_enable_raw_mode: () => {
                    this.rawMode = true;
                    this.post({ type: "raw-mode", enabled: true });
                },
                terminal_disable_raw_mode: () => {
                    this.rawMode = false;
                    this.post({ type: "raw-mode", enabled: false });
                },
                terminal_clear: () => this.terminal.clear(),
                terminal_move_to: (x, y) => this.terminal.moveTo(Number(x), Number(y)),
                terminal_print: (sref) => this.terminal.print(this.averToJs(sref)),
                terminal_set_color: (sref) => {
                    const color = this.averToJs(sref);
                    this.terminal.setColor(COLOR_NAMES.has(color) ? color : "default");
                },
                terminal_reset_color: () => this.terminal.resetColor(),
                terminal_read_key: () => {
                    const key = this.dequeueKey();
                    const exports = this.instance.exports;
                    if (!key) return exports.__rt_option_string_none();
                    return exports.__rt_option_string_some(this.jsToAver(key));
                },
                terminal_size: () => {
                    return this.instance.exports.__rt_record_terminal_size_make(
                        BigInt(this.terminal.cols),
                        BigInt(this.terminal.rows),
                    );
                },
                terminal_hide_cursor: () => this.terminal.hideCursor(),
                terminal_show_cursor: () => this.terminal.showCursor(),
                terminal_flush: () => this.postTerminalSnapshot(),
            },
        };
    }
}
