import { TerminalBuffer } from "./browser_terminal.js";

const IO_SCRATCH_SIZE = 128;
const NONE_SENTINEL = -1;

const OBJ_STRING = 0;
const OBJ_RECORD = 1;
const OBJ_VARIANT = 2;
const OBJ_WRAPPER = 3;
const OBJ_LIST_CONS = 4;
const OBJ_TUPLE = 5;
const OBJ_WRAPPER_F64 = 7;
const OBJ_WRAPPER_I32 = 8;
const OBJ_LIST_CONS_F64 = 9;
const OBJ_VECTOR = 10;
const OBJ_MAP_ENTRY = 11;

const WRAP_OK = 0;
const WRAP_ERR = 1;
const WRAP_SOME = 2;

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

function clampI32(value) {
    return value | 0;
}

function asSigned64String(value) {
    return BigInt.asIntN(64, value).toString();
}

function chooseRandomInt(min, max) {
    const lo = BigInt.asIntN(64, min);
    const hi = BigInt.asIntN(64, max);
    if (hi <= lo) {
        return lo;
    }

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

export class AverBrowserHost {
    constructor(postMessageFn) {
        this.post = postMessageFn;
        this.instance = null;
        this.decoder = new TextDecoder();
        this.encoder = new TextEncoder();
        this.terminal = new TerminalBuffer(80, 24);
        this.keyQueue = [];
        this.keyQueueView = null;
        this.lineQueue = [];
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

    setProgramArgs(args) {
        this.programArgs = Array.isArray(args) ? args.map((arg) => String(arg)) : [];
    }

    enqueueKey(key) {
        this.keyQueue.push(key);
    }

    enqueueLine(line) {
        this.lineQueue.push(line);
        this.post({
            type: "line-queue",
            queued: this.lineQueue.length,
        });
    }

    createImports() {
        return {
            aver: {
                args_len: () => this.programArgs.length,
                args_get: (index) => {
                    const idx = clampI32(index);
                    const value =
                        idx >= 0 && idx < this.programArgs.length ? this.programArgs[idx] : "";
                    return this.writeGuestString(value);
                },
                console_print: (ptr, len) => {
                    this.postConsole("stdout", this.readRawString(ptr, len));
                },
                console_error: (ptr, len) => {
                    this.postConsole("stderr", this.readRawString(ptr, len));
                },
                console_readLine: () => {
                    const line = this.lineQueue.length > 0 ? this.lineQueue.shift() : "";
                    this.post({
                        type: "line-queue",
                        queued: this.lineQueue.length,
                    });
                    return this.writeGuestString(line);
                },
                print_value: (tag, value) => {
                    this.postConsole("stdout", this.formatTaggedValue(tag, value));
                },
                format_value: (tag, value) => {
                    return this.writeGuestString(this.formatTaggedValue(tag, value));
                },
                random_int: (min, max) => {
                    return chooseRandomInt(min, max);
                },
                time_now: () => {
                    return this.writeGuestString(new Date().toISOString());
                },
                time_unixMs: () => {
                    return BigInt(Date.now());
                },
                time_sleep: (millis) => {
                    sleepMillis(millis);
                },
                terminal_enableRawMode: () => {
                    this.rawMode = true;
                    this.post({ type: "raw-mode", enabled: true });
                },
                terminal_disableRawMode: () => {
                    this.rawMode = false;
                    this.post({ type: "raw-mode", enabled: false });
                },
                terminal_clear: () => {
                    this.terminal.clear();
                },
                terminal_moveTo: (x, y) => {
                    this.terminal.moveTo(x, y);
                },
                terminal_print: (ptr, len) => {
                    this.terminal.print(this.readRawString(ptr, len));
                },
                terminal_setColor: (ptr, len) => {
                    const color = this.readRawString(ptr, len);
                    this.terminal.setColor(COLOR_NAMES.has(color) ? color : "default");
                },
                terminal_resetColor: () => {
                    this.terminal.resetColor();
                },
                terminal_readKey: () => {
                    const key = this.dequeueKey();
                    if (!key) {
                        return [NONE_SENTINEL, 0];
                    }
                    return this.writeGuestString(key);
                },
                terminal_size: () => {
                    return [this.terminal.cols, this.terminal.rows];
                },
                terminal_hideCursor: () => {
                    this.terminal.hideCursor();
                },
                terminal_showCursor: () => {
                    this.terminal.showCursor();
                },
                terminal_flush: () => {
                    this.postTerminalSnapshot();
                },
                math_sin: (x) => Math.sin(x),
                math_cos: (x) => Math.cos(x),
                math_atan2: (y, x) => Math.atan2(y, x),
                math_pow: (base, exp) => Math.pow(base, exp),
            },
        };
    }

    postTerminalSnapshot() {
        // Throttle to ~60fps to prevent message queue from growing
        // unboundedly when the WASM module flushes faster than the
        // main thread can render.
        const now = performance.now();
        if (now - this.lastFlushMs < 16) {
            return;
        }
        this.lastFlushMs = now;

        const snapshot = this.terminal.toSnapshot();
        this.post({
            type: "terminal",
            cols: this.terminal.cols,
            rows: this.terminal.rows,
            snapshot,
            blank: this.terminal.isBlank(),
            rawMode: this.rawMode,
        }, [snapshot.chars.buffer, snapshot.colors.buffer]);
    }

    dequeueKey() {
        if (this.keyQueueView) {
            const head = Atomics.load(this.keyQueueView, KEY_QUEUE_HEAD);
            const tail = Atomics.load(this.keyQueueView, KEY_QUEUE_TAIL);
            if (head === tail) {
                return null;
            }
            const slot = KEY_QUEUE_DATA + (head % KEY_QUEUE_CAPACITY);
            const code = Atomics.load(this.keyQueueView, slot);
            Atomics.store(this.keyQueueView, KEY_QUEUE_HEAD, head + 1);
            return decodeKeyCode(code);
        }

        if (this.keyQueue.length === 0) {
            return null;
        }

        return this.keyQueue.shift();
    }

    postConsole(level, text) {
        this.post({
            type: "console",
            level,
            text,
        });
    }

    memoryView() {
        return new Uint8Array(this.instance.exports.memory.buffer);
    }

    dataView() {
        return new DataView(this.instance.exports.memory.buffer);
    }

    readBytes(ptr, len) {
        const start = clampI32(ptr);
        const count = clampI32(len);
        if (start < 0 || count < 0) {
            return new Uint8Array();
        }
        const mem = this.memoryView();
        const end = Math.min(mem.length, start + count);
        return mem.slice(start, end);
    }

    readRawString(ptr, len) {
        return this.decoder.decode(this.readBytes(ptr, len));
    }

    writeGuestString(text) {
        const bytes = this.encoder.encode(text);
        // Use the tail of IO_SCRATCH (bytes 96-127) for short strings.
        // Bytes 0-95 are reserved for IO_IOVEC, int_buf, float_buf etc.
        const SCRATCH_STRING_BASE = 96;
        const SCRATCH_STRING_CAP = IO_SCRATCH_SIZE - SCRATCH_STRING_BASE; // 32 bytes
        if (bytes.length <= SCRATCH_STRING_CAP) {
            const mem = this.memoryView();
            mem.set(bytes, SCRATCH_STRING_BASE);
            return [SCRATCH_STRING_BASE, bytes.length];
        }
        // Longer string: write at the end of memory.
        const mem = this.memoryView();
        const ptr = Math.max(IO_SCRATCH_SIZE, mem.length - bytes.length - 64);
        mem.set(bytes, ptr);
        return [ptr, bytes.length];
    }

    readU64(offset) {
        if (offset < 0 || offset + 8 > this.dataView().byteLength) {
            return 0n;
        }
        return this.dataView().getBigUint64(offset, true);
    }

    readHeapString(ptr, quoted) {
        const addr = Number(ptr);
        const header = this.readU64(addr);
        const len = Number(header & 0xffffffffn);
        const text = this.decoder.decode(this.readBytes(addr + 8, len));
        return quoted ? JSON.stringify(text) : text;
    }

    formatTaggedValue(tag, value) {
        switch (tag) {
            case 0:
                return asSigned64String(value);
            case 1: {
                const bits = BigInt.asUintN(64, value);
                const bytes = new ArrayBuffer(8);
                const view = new DataView(bytes);
                view.setBigUint64(0, bits, true);
                return String(view.getFloat64(0, true));
            }
            case 2:
                return value !== 0n ? "true" : "false";
            case 3:
                return this.readHeapString(Number(value), false);
            case 4:
                if (value === 0n) {
                    return "[]";
                }
                if (BigInt.asIntN(32, value) === BigInt(NONE_SENTINEL)) {
                    return "Option.None";
                }
                return this.formatWasmValue(value);
            case 5:
                return "";
            default:
                return String(value);
        }
    }

    formatWasmValue(value) {
        const signed = BigInt.asIntN(64, value);
        const ptr = Number(BigInt.asUintN(32, value));
        const mem = this.memoryView();
        if (ptr < IO_SCRATCH_SIZE || ptr + 8 > mem.length) {
            return signed.toString();
        }

        const header = this.readU64(ptr);
        const kind = Number((header >> 56n) & 0xffn);
        const tag = Number((header >> 48n) & 0xffn);
        const fieldCount = Number(header & 0xffffffffn);

        if (kind > OBJ_MAP_ENTRY) {
            return signed.toString();
        }

        switch (kind) {
            case OBJ_STRING:
                return this.readHeapString(ptr, true);
            case OBJ_MAP_ENTRY:
                return this.formatMap(ptr);
            case OBJ_LIST_CONS:
            case OBJ_LIST_CONS_F64:
                return this.formatList(ptr, kind === OBJ_LIST_CONS_F64);
            case OBJ_TUPLE:
                return this.formatTuple(ptr, fieldCount);
            case OBJ_WRAPPER:
            case OBJ_WRAPPER_F64:
            case OBJ_WRAPPER_I32:
                return this.formatWrapper(ptr, kind, tag);
            case OBJ_RECORD:
                return this.formatRecord(ptr, fieldCount);
            case OBJ_VARIANT:
                return this.formatVariant(ptr, fieldCount, tag);
            case OBJ_VECTOR:
                return this.formatVector(ptr, fieldCount);
            default:
                return signed.toString();
        }
    }

    formatMap(ptr) {
        const seen = new Set();
        const entries = [];
        let current = ptr;

        while (current !== 0 && current + 24 <= this.memoryView().length) {
            const header = this.readU64(current);
            const kind = Number((header >> 56n) & 0xffn);
            if (kind !== OBJ_MAP_ENTRY) {
                break;
            }

            const tuplePtr = Number(BigInt.asUintN(32, this.readU64(current + 8)));
            if (tuplePtr !== 0 && tuplePtr + 24 <= this.memoryView().length) {
                const key = this.formatWasmValue(this.readU64(tuplePtr + 8));
                if (!seen.has(key)) {
                    seen.add(key);
                    const val = this.formatWasmValue(this.readU64(tuplePtr + 16));
                    entries.push(`${key}: ${val}`);
                }
            }

            current = Number(BigInt.asUintN(32, this.readU64(current + 16)));
        }

        return `{${entries.join(", ")}}`;
    }

    formatList(ptr, isF64) {
        const items = [];
        let current = ptr;

        while (current !== 0 && current + 24 <= this.memoryView().length) {
            const header = this.readU64(current);
            const kind = Number((header >> 56n) & 0xffn);
            if (kind !== (isF64 ? OBJ_LIST_CONS_F64 : OBJ_LIST_CONS)) {
                break;
            }

            const head = this.readU64(current + 8);
            if (isF64) {
                const bytes = new ArrayBuffer(8);
                const view = new DataView(bytes);
                view.setBigUint64(0, head, true);
                items.push(String(view.getFloat64(0, true)));
            } else {
                items.push(this.formatWasmValue(head));
            }
            current = Number(BigInt.asUintN(32, this.readU64(current + 16)));
        }

        return `[${items.join(", ")}]`;
    }

    formatTuple(ptr, count) {
        const items = [];
        for (let i = 0; i < count; i += 1) {
            items.push(this.formatWasmValue(this.readU64(ptr + 8 + i * 8)));
        }
        return `(${items.join(", ")})`;
    }

    formatWrapper(ptr, kind, tag) {
        const prefix =
            tag === WRAP_OK ? "Result.Ok" : tag === WRAP_ERR ? "Result.Err" : tag === WRAP_SOME ? "Option.Some" : "Wrapper";
        const inner = this.readU64(ptr + 8);

        if (kind === OBJ_WRAPPER_F64) {
            const bytes = new ArrayBuffer(8);
            const view = new DataView(bytes);
            view.setBigUint64(0, inner, true);
            return `${prefix}(${String(view.getFloat64(0, true))})`;
        }

        if (kind === OBJ_WRAPPER_I32) {
            const innerI32 = Number(BigInt.asIntN(32, inner));
            if (innerI32 === NONE_SENTINEL) {
                return `${prefix}(Option.None)`;
            }
            if (innerI32 >= IO_SCRATCH_SIZE) {
                return `${prefix}(${this.formatWasmValue(BigInt(innerI32))})`;
            }
            return `${prefix}(${innerI32})`;
        }

        return `${prefix}(${this.formatWasmValue(inner)})`;
    }

    formatRecord(ptr, count) {
        const fields = [];
        for (let i = 0; i < count; i += 1) {
            fields.push(this.formatWasmValue(this.readU64(ptr + 8 + i * 8)));
        }
        return `Record(${fields.join(", ")})`;
    }

    formatVariant(ptr, count, tag) {
        const fields = [];
        for (let i = 0; i < count; i += 1) {
            fields.push(this.formatWasmValue(this.readU64(ptr + 8 + i * 8)));
        }
        return `Variant#${tag}(${fields.join(", ")})`;
    }

    formatVector(ptr, count) {
        const items = [];
        for (let i = 0; i < count; i += 1) {
            items.push(this.formatWasmValue(this.readU64(ptr + 8 + i * 8)));
        }
        return `Vector[${items.join(", ")}]`;
    }
}
