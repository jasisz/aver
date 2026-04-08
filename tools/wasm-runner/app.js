import { TERMINAL_COLOR_NAMES, TERMINAL_SPACE_CODE } from "./browser_terminal.js";

const dom = {
    dropzone: document.querySelector("[data-dropzone]"),
    fileInput: document.querySelector("[data-file-input]"),
    fileMeta: document.querySelector("[data-file-meta]"),
    runButton: document.querySelector("[data-run]"),
    stopButton: document.querySelector("[data-stop]"),
    clearButton: document.querySelector("[data-clear]"),
    status: document.querySelector("[data-status]"),
    rawMode: document.querySelector("[data-raw-mode]"),
    lineQueued: document.querySelector("[data-line-queued]"),
    terminal: document.querySelector("[data-terminal]"),
    terminalCanvas: document.querySelector("[data-terminal-canvas]"),
    terminalEmpty: document.querySelector("[data-terminal-empty]"),
    console: document.querySelector("[data-console]"),
    lineInput: document.querySelector("[data-line-input]"),
    lineButton: document.querySelector("[data-line-button]"),
    isolationNote: document.querySelector("[data-isolation-note]"),
    isolationCopy: document.querySelector("[data-isolation-copy]"),
};

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

const state = {
    wasmBytes: null,
    wasmName: null,
    worker: null,
    queuedLines: [],
    rawMode: false,
    sharedKeyView: null,
    programArgs: [],
    pendingTerminal: null,
    terminalFrame: 0,
    lastTerminal: null,
    canvasLayout: null,
};

const TERM_COLORS = {
    default: "#e2e8f0",
    red: "#fda4af",
    green: "#86efac",
    yellow: "#fde68a",
    blue: "#93c5fd",
    white: "#ffffff",
    cyan: "#67e8f9",
    magenta: "#f0abfc",
    black: "#111827",
};
const TERM_BG = "#111827";
const TERM_CURSOR_BG = "#fff6d1";
const TERM_CURSOR_FG = "#172117";
const TERM_FONT = '"SFMono-Regular", Menlo, Monaco, Consolas, monospace';

function colorName(colorIndex) {
    return TERMINAL_COLOR_NAMES[colorIndex] ?? "default";
}

function codePointsToString(chars, start, end) {
    let text = "";
    const chunkSize = 64;
    for (let offset = start; offset < end; offset += chunkSize) {
        const chunk = chars.subarray(offset, Math.min(end, offset + chunkSize));
        text += String.fromCodePoint(...chunk);
    }
    return text;
}

function setStatus(text, tone = "idle") {
    dom.status.textContent = text;
    dom.status.dataset.tone = tone;
}

function setRawMode(enabled) {
    state.rawMode = enabled;
    dom.rawMode.textContent = enabled ? "raw mode on" : "raw mode off";
    dom.rawMode.dataset.active = enabled ? "true" : "false";
}

function setLineQueueCount(count) {
    dom.lineQueued.textContent = `${count} queued line${count === 1 ? "" : "s"}`;
}

function appendConsole(level, text) {
    const line = document.createElement("div");
    line.className = `console-line console-${level}`;
    line.textContent = text;
    dom.console.appendChild(line);
    dom.console.scrollTop = dom.console.scrollHeight;
}

function clearOutput() {
    dom.console.textContent = "";
    dom.terminal.dataset.empty = "true";
    dom.terminalEmpty.textContent = "Load a module to render a terminal frame.";
    dom.terminal.style.height = "";
    state.lastTerminal = null;
    state.pendingTerminal = null;
    if (state.terminalFrame) {
        cancelAnimationFrame(state.terminalFrame);
        state.terminalFrame = 0;
    }
    clearTerminalCanvas();
}

function terminalMetrics() {
    const styles = getComputedStyle(dom.terminal);
    const cellWidth = Number.parseFloat(styles.getPropertyValue("--cell-width")) || 10;
    const cellHeight = Number.parseFloat(styles.getPropertyValue("--cell-height")) || 18;
    const paddingX =
        Number.parseFloat(styles.paddingLeft || "0") +
        Number.parseFloat(styles.paddingRight || "0");
    const paddingY =
        Number.parseFloat(styles.paddingTop || "0") +
        Number.parseFloat(styles.paddingBottom || "0");
    const innerWidth = Math.max(0, dom.terminal.clientWidth - paddingX);
    const innerHeight = Math.max(0, dom.terminal.clientHeight - paddingY);
    const cols = Math.max(20, Math.floor(innerWidth / cellWidth));
    const rows = Math.max(8, Math.floor(innerHeight / cellHeight));
    return { cols, rows };
}

function terminalAvailableHeight() {
    const rect = dom.terminal.getBoundingClientRect();
    return Math.max(220, window.innerHeight - rect.top - 28);
}

function autoSizeTerminalSurface(preferredRows = null) {
    if (dom.terminal.dataset.empty === "true" && preferredRows == null) {
        dom.terminal.style.height = "";
        return;
    }

    const styles = getComputedStyle(dom.terminal);
    const cellHeight = Number.parseFloat(styles.getPropertyValue("--cell-height")) || 18;
    const paddingY =
        Number.parseFloat(styles.paddingTop || "0") +
        Number.parseFloat(styles.paddingBottom || "0");
    const maxHeight = terminalAvailableHeight();

    let nextHeight = maxHeight;
    if (preferredRows != null) {
        const fitted = preferredRows * cellHeight + paddingY;
        nextHeight = Math.min(Math.max(220, fitted), maxHeight);
    }

    dom.terminal.style.height = `${Math.round(nextHeight)}px`;
}

function spawnWorker() {
    if (state.worker) {
        state.worker.terminate();
    }

    state.sharedKeyView = createSharedKeyView();
    const worker = new Worker(new URL("./worker.js", import.meta.url), { type: "module" });
    worker.onmessage = handleWorkerMessage;
    state.worker = worker;
    worker.postMessage({
        type: "init-input",
        keyBuffer: state.sharedKeyView ? state.sharedKeyView.buffer : null,
    });

    autoSizeTerminalSurface();
    const { cols, rows } = terminalMetrics();
    worker.postMessage({ type: "resize", cols, rows });
    for (const line of state.queuedLines) {
        worker.postMessage({ type: "line", line });
    }
    state.queuedLines = [];
    setLineQueueCount(0);
    return worker;
}

function handleWorkerMessage(event) {
    const message = event.data;
    switch (message.type) {
        case "console":
            appendConsole(message.level, message.text);
            break;
        case "terminal":
            queueTerminalFrame(message);
            break;
        case "status":
            setStatus(message.text, message.level);
            break;
        case "raw-mode":
            setRawMode(message.enabled);
            if (message.enabled) {
                dom.terminal.focus();
            }
            break;
        case "line-queue":
            setLineQueueCount(message.queued);
            break;
        case "finished":
            if (state.worker) {
                state.worker.terminate();
                state.worker = null;
            }
            dom.runButton.disabled = false;
            dom.stopButton.disabled = true;
            setRawMode(false);
            if (!message.ok && message.error) {
                appendConsole("stderr", message.error);
            }
            break;
        default:
            break;
    }
}

async function loadSelectedFile(file) {
    state.wasmBytes = await file.arrayBuffer();
    state.wasmName = file.name;
    dom.fileMeta.textContent = `${file.name} · ${(file.size / 1024).toFixed(1)} KB`;
    dom.runButton.disabled = false;
    setStatus("Module loaded. Ready to run.", "success");
}

async function onFileChange(fileList) {
    const [file] = fileList;
    if (!file) {
        return;
    }
    if (!file.name.endsWith(".wasm")) {
        setStatus("Select a `.wasm` file.", "error");
        return;
    }
    await loadSelectedFile(file);
}

async function runSelectedModule() {
    if (!state.wasmBytes) {
        setStatus("Load a `.wasm` file first.", "error");
        return;
    }

    clearOutput();
    setRawMode(false);
    dom.runButton.disabled = true;
    dom.stopButton.disabled = false;
    dom.terminal.dataset.empty = "false";
    dom.terminal.focus({ preventScroll: true });
    const worker = spawnWorker();
    const wasmBytes = state.wasmBytes.slice(0);
    worker.postMessage({ type: "run", wasmBytes, programArgs: state.programArgs }, [wasmBytes]);
}

function stopRun() {
    if (state.worker) {
        state.worker.terminate();
        state.worker = null;
    }
    state.sharedKeyView = null;
    dom.runButton.disabled = !state.wasmBytes;
    dom.stopButton.disabled = true;
    setRawMode(false);
    setStatus("Run stopped.", "idle");
}

function queueConsoleLine() {
    const line = dom.lineInput.value;
    dom.lineInput.value = "";
    state.queuedLines.push(line);
    setLineQueueCount(state.queuedLines.length);
    if (state.worker) {
        state.worker.postMessage({ type: "line", line });
    }
}

function normalizeKey(event) {
    switch (event.key) {
        case "ArrowUp":
            return "up";
        case "ArrowDown":
            return "down";
        case "ArrowLeft":
            return "left";
        case "ArrowRight":
            return "right";
        case "Escape":
            return "esc";
        case "Enter":
            return "enter";
        default:
            return event.key.length === 1 ? event.key : null;
    }
}

function createSharedKeyView() {
    if (typeof SharedArrayBuffer !== "function" || !window.crossOriginIsolated) {
        return null;
    }

    const bytes = Int32Array.BYTES_PER_ELEMENT * (KEY_QUEUE_DATA + KEY_QUEUE_CAPACITY);
    return new Int32Array(new SharedArrayBuffer(bytes));
}

function keyToCode(key) {
    switch (key) {
        case "up":
            return KEY_CODE_UP;
        case "down":
            return KEY_CODE_DOWN;
        case "left":
            return KEY_CODE_LEFT;
        case "right":
            return KEY_CODE_RIGHT;
        case "esc":
            return KEY_CODE_ESCAPE;
        case "enter":
            return KEY_CODE_ENTER;
        default:
            return key.length === 1 ? KEY_CODE_CHAR_BASE + key.codePointAt(0) : 0;
    }
}

function enqueueSharedKey(key) {
    if (!state.sharedKeyView) {
        return false;
    }

    const code = keyToCode(key);
    if (code === 0) {
        return false;
    }

    const head = Atomics.load(state.sharedKeyView, KEY_QUEUE_HEAD);
    const tail = Atomics.load(state.sharedKeyView, KEY_QUEUE_TAIL);
    if (tail - head >= KEY_QUEUE_CAPACITY) {
        Atomics.store(state.sharedKeyView, KEY_QUEUE_HEAD, head + 1);
    }
    const slot = KEY_QUEUE_DATA + (tail % KEY_QUEUE_CAPACITY);
    Atomics.store(state.sharedKeyView, slot, code);
    Atomics.store(state.sharedKeyView, KEY_QUEUE_TAIL, tail + 1);
    return true;
}

function updateIsolationNote() {
    const interactiveReady =
        typeof SharedArrayBuffer === "function" && window.crossOriginIsolated;

    if (interactiveReady) {
        dom.isolationNote.dataset.tone = "ok";
        dom.isolationCopy.innerHTML =
            'Interactive <code>Terminal.readKey()</code> is live. This page is running in an isolated context.';
        return;
    }

    dom.isolationNote.dataset.tone = "warn";
    dom.isolationCopy.innerHTML =
        'Live keyboard input needs cross-origin isolation. Start this runner with <code>python3 serve.py 4173</code>, not plain <code>python3 -m http.server</code>.';
}

function applyTerminalFrame(message) {
    dom.terminal.style.setProperty("--terminal-rows", String(message.rows ?? 24));
    const isEmpty = message.blank && !message.rawMode;
    dom.terminal.dataset.empty = isEmpty ? "true" : "false";
    dom.terminalEmpty.textContent = "Terminal cleared.";
    if (isEmpty) {
        dom.terminal.style.height = "";
    } else {
        autoSizeTerminalSurface(message.rows ?? null);
    }
    state.lastTerminal = message;
    if (!isEmpty && message.snapshot) {
        drawTerminalSnapshot(message.snapshot);
    } else {
        clearTerminalCanvas();
    }
}

function queueTerminalFrame(message) {
    state.pendingTerminal = message;
    if (state.terminalFrame) {
        return;
    }

    state.terminalFrame = requestAnimationFrame(() => {
        state.terminalFrame = 0;
        if (!state.pendingTerminal) {
            return;
        }
        applyTerminalFrame(state.pendingTerminal);
        state.pendingTerminal = null;
    });
}

function terminalStyleMetrics() {
    const styles = getComputedStyle(dom.terminal);
    return {
        cellWidth: Number.parseFloat(styles.getPropertyValue("--cell-width")) || 10,
        cellHeight: Number.parseFloat(styles.getPropertyValue("--cell-height")) || 18,
    };
}

function canvasContext(snapshot) {
    const canvas = dom.terminalCanvas;
    const { cellWidth, cellHeight } = terminalStyleMetrics();
    const width = Math.max(1, snapshot.cols * cellWidth);
    const height = Math.max(1, snapshot.rows * cellHeight);
    const dpr = window.devicePixelRatio || 1;
    const pixelWidth = Math.max(1, Math.round(width * dpr));
    const pixelHeight = Math.max(1, Math.round(height * dpr));
    const layoutChanged =
        !state.canvasLayout ||
        state.canvasLayout.pixelWidth !== pixelWidth ||
        state.canvasLayout.pixelHeight !== pixelHeight ||
        state.canvasLayout.width !== width ||
        state.canvasLayout.height !== height ||
        state.canvasLayout.dpr !== dpr ||
        state.canvasLayout.cellWidth !== cellWidth ||
        state.canvasLayout.cellHeight !== cellHeight;

    if (layoutChanged) {
        canvas.style.width = `${width}px`;
        canvas.style.height = `${height}px`;
        canvas.width = pixelWidth;
        canvas.height = pixelHeight;
        state.canvasLayout = {
            width,
            height,
            pixelWidth,
            pixelHeight,
            dpr,
            cellWidth,
            cellHeight,
        };
    }

    const ctx = canvas.getContext("2d");
    if (layoutChanged) {
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
        ctx.textBaseline = "top";
        ctx.font = `${Math.max(12, cellHeight - 2)}px ${TERM_FONT}`;
    }
    return { ctx, cellWidth, cellHeight, width, height };
}

function clearTerminalCanvas() {
    const canvas = dom.terminalCanvas;
    const ctx = canvas.getContext("2d");
    if (!ctx) {
        return;
    }
    ctx.clearRect(0, 0, canvas.width, canvas.height);
}

function drawTerminalSnapshot(snapshot) {
    const { ctx, cellWidth, cellHeight, width, height } = canvasContext(snapshot);
    const chars = snapshot.chars;
    const colors = snapshot.colors;
    ctx.fillStyle = TERM_BG;
    ctx.fillRect(0, 0, width, height);

    for (let row = 0; row < snapshot.rows; row += 1) {
        const rowBase = row * snapshot.cols;
        let col = 0;
        while (col < snapshot.cols) {
            const colorIndex = colors[rowBase + col] ?? 0;
            let endCol = col + 1;
            let hasVisibleChar = (chars[rowBase + col] ?? TERMINAL_SPACE_CODE) !== TERMINAL_SPACE_CODE;

            while (endCol < snapshot.cols && colors[rowBase + endCol] === colorIndex) {
                if ((chars[rowBase + endCol] ?? TERMINAL_SPACE_CODE) !== TERMINAL_SPACE_CODE) {
                    hasVisibleChar = true;
                }
                endCol += 1;
            }

            if (hasVisibleChar) {
                const start = rowBase + col;
                const end = rowBase + endCol;
                ctx.fillStyle = TERM_COLORS[colorName(colorIndex)] || TERM_COLORS.default;
                ctx.fillText(
                    codePointsToString(chars, start, end),
                    col * cellWidth,
                    row * cellHeight,
                );
            }

            col = endCol;
        }
    }

    if (snapshot.cursor) {
        const { x, y, code } = snapshot.cursor;
        ctx.fillStyle = TERM_CURSOR_BG;
        ctx.fillRect(x * cellWidth, y * cellHeight, cellWidth, cellHeight);
        if (code && code !== TERMINAL_SPACE_CODE) {
            ctx.fillStyle = TERM_CURSOR_FG;
            ctx.fillText(String.fromCodePoint(code), x * cellWidth, y * cellHeight);
        }
    }
}

dom.fileInput.addEventListener("change", async (event) => {
    await onFileChange(event.target.files);
});

dom.dropzone.addEventListener("dragover", (event) => {
    event.preventDefault();
    dom.dropzone.dataset.drag = "true";
});

dom.dropzone.addEventListener("dragleave", () => {
    dom.dropzone.dataset.drag = "false";
});

dom.dropzone.addEventListener("drop", async (event) => {
    event.preventDefault();
    dom.dropzone.dataset.drag = "false";
    await onFileChange(event.dataTransfer.files);
});

dom.runButton.addEventListener("click", async () => {
    await runSelectedModule();
});

dom.stopButton.addEventListener("click", () => {
    stopRun();
});

dom.clearButton.addEventListener("click", () => {
    clearOutput();
    setStatus("Output cleared.", "idle");
});

dom.lineButton.addEventListener("click", () => {
    queueConsoleLine();
});

dom.lineInput.addEventListener("keydown", (event) => {
    if (event.key === "Enter") {
        event.preventDefault();
        queueConsoleLine();
    }
});

window.addEventListener("keydown", (event) => {
    if (!state.rawMode || !state.worker) {
        return;
    }
    const key = normalizeKey(event);
    if (!key) {
        return;
    }
    event.preventDefault();
    if (!enqueueSharedKey(key)) {
        state.worker.postMessage({ type: "key", key });
    }
});

new ResizeObserver(() => {
    if (!state.worker) {
        return;
    }
    const { cols, rows } = terminalMetrics();
    state.worker.postMessage({ type: "resize", cols, rows });
}).observe(dom.terminal);

window.addEventListener("resize", () => {
    autoSizeTerminalSurface(state.lastTerminal?.rows ?? null);
});

setLineQueueCount(0);
setRawMode(false);
updateIsolationNote();
setStatus("Drop a compiled Aver `.wasm` module to start.", "idle");
