import { TERMINAL_COLOR_NAMES, TERMINAL_SPACE_CODE } from "./browser_terminal.js";

const dom = {
    dropzone: document.querySelector("[data-dropzone]"),
    fileInput: document.querySelector("[data-file-input]"),
    fileMeta: document.querySelector("[data-file-meta]"),
    compileMeta: document.querySelector("[data-compile-meta]"),
    editorTabs: document.querySelector("[data-editor-tabs]"),
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
    memory: document.querySelector("[data-memory]"),
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
    sharedLineBuffer: null,
    programArgs: [],
    activeGame: null,
    pendingTerminal: null,
    terminalFrame: 0,
    lastTerminal: null,
    canvasLayout: null,
    /// Virtual filesystem for the editor.
    /// Map<path, source>. Path is a simple string — for single-file edit
    /// it's "playground.av"; for folder drop we keep the relative path.
    files: new Map(),
    activeFile: null,
    wasmOnly: false,
    // Snapshot of game source at load time. Non-null means "prebuilt
    // .wasm owns Run until the user edits" — see forkPrebuiltIfEdited.
    pristineFiles: null,
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

function formatBytes(bytes) {
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + " KB";
    return (bytes / (1024 * 1024)).toFixed(1) + " MB";
}

let lastMemUpdate = 0;
function updateMemoryDisplay(heapBytes, pageBytes) {
    if (!dom.memory) return;
    const now = performance.now();
    if (now - lastMemUpdate < 500) return;
    lastMemUpdate = now;
    if (pageBytes) {
        dom.memory.textContent = "heap " + formatBytes(heapBytes) + " / " + formatBytes(pageBytes);
    } else {
        dom.memory.textContent = "mem " + formatBytes(heapBytes);
    }
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
    line.className = `console-line ${level}`;
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
    const cellWidth = Number.parseFloat(styles.getPropertyValue("--cell-width")) || 9;
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
    // Let flex handle sizing — don't set explicit height.
    dom.terminal.style.height = "";
}

function spawnWorker(fixedSize) {
    if (state.worker) {
        state.worker.terminate();
    }

    state.sharedKeyView = createSharedKeyView();
    state.sharedLineBuffer = createSharedLineBuffer();
    const worker = new Worker(new URL("./worker.js", import.meta.url), { type: "module" });
    worker.onmessage = handleWorkerMessage;
    state.worker = worker;
    worker.postMessage({
        type: "init-input",
        keyBuffer: state.sharedKeyView ? state.sharedKeyView.buffer : null,
        lineBuffer: state.sharedLineBuffer,
    });

    autoSizeTerminalSurface();
    const { cols, rows } = fixedSize || terminalMetrics();
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
            if (message.memoryBytes != null) {
                updateMemoryDisplay(message.memoryBytes, message.memoryPages);
            }
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
        case "readline-wait": {
            // Worker is blocked waiting for a line — show input
            const readlineBar = document.querySelector("[data-readline-bar]");
            if (readlineBar) {
                readlineBar.style.display = "flex";
                const input = readlineBar.querySelector("input");
                if (input) input.focus();
            }
            break;
        }
        case "finished":
            if (state.worker) {
                state.worker.terminate();
                state.worker = null;
            }
            dom.runButton.disabled = false;
            dom.stopButton.disabled = true;
    dom.stopButton.hidden = true;
            setRawMode(false);
            if (!message.ok && message.error) {
                appendConsole("stderr", message.error);
            }
            if (state.activeGame) {
                setStatus("Game ended.", "idle");
            }
            break;
        default:
            break;
    }
}

async function loadSelectedFile(file) {
    state.wasmBytes = await file.arrayBuffer();
    state.wasmName = file.name;
    state.activeGame = null;
    dom.fileMeta.textContent = `${file.name} · ${(file.size / 1024).toFixed(1)} KB`;
    dom.runButton.disabled = false;
    document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
    renderPrebuiltMeta(file.name, state.wasmBytes.byteLength);
    clearOutput();

    // Check imports to decide terminal vs console
    try {
        const mod = await WebAssembly.compile(state.wasmBytes);
        const imports = WebAssembly.Module.imports(mod);
        const usesTerminal = imports.some(i => i.name && i.name.startsWith("terminal_"));
        setOutputMode(usesTerminal ? "terminal" : "console");
        const readlineBar = document.querySelector("[data-readline-bar]");
        if (readlineBar) readlineBar.style.display = usesTerminal ? "none" : "flex";
    } catch (_) {
        setOutputMode("terminal");
    }

    setStatus(`Running ${file.name}…`, "info");
    runSelectedModule({ cols: 80, rows: 35 });
}

async function onFileChange(fileList) {
    const files = Array.from(fileList || []);
    if (files.length === 0) return;

    // If there's a single .wasm among them, route to binary-only mode.
    if (files.length === 1 && files[0].name.endsWith(".wasm")) {
        await loadSelectedFile(files[0]);
        enterWasmOnlyMode(files[0].name);
        return;
    }

    // A dropped .replay.json seeds state.lastRecording so ▶ Replay can
    // pick it up without asking for the file again.
    if (files.length === 1 && /\.replay\.json$|\.json$/i.test(files[0].name)) {
        state.lastRecording = await files[0].text();
        setStatus(`Loaded recording ${files[0].name} — click ▶ Replay to verify.`, "success");
        return;
    }

    // Otherwise: collect .av files (flat list or from a dropped folder).
    const avFiles = files.filter((f) => f.name.endsWith(".av"));
    if (avFiles.length === 0) {
        setStatus("Drop a .wasm, .av, folder, or .replay.json.", "error");
        return;
    }

    // Reset the virtual fs and rebuild from the dropped set.
    leaveWasmOnlyMode();
    state.files.clear();
    state.activeFile = null;
    state.wasmBytes = null;
    state.wasmName = null;
    let firstPath = null;
    for (const f of avFiles) {
        // webkitRelativePath is set for folder drops; fall back to name.
        const path = f.webkitRelativePath || f.name;
        const source = await f.text();
        state.files.set(path, source);
        if (!firstPath || /main\.av$/i.test(path)) {
            firstPath = path;
        }
    }
    if (firstPath) setActiveFile(firstPath);
    setStatus(
        avFiles.length === 1
            ? `Loaded ${avFiles[0].name}`
            : `Loaded ${avFiles.length} .av files`,
        "success"
    );
}

async function runSelectedModule(fixedSize) {
    if (!state.wasmBytes) {
        setStatus("Load a `.wasm` file first.", "error");
        return;
    }

    clearOutput();
    setRawMode(false);
    if (dom.memory) dom.memory.textContent = "";
    dom.runButton.disabled = true;
    dom.stopButton.disabled = false;
    dom.stopButton.hidden = false;
    dom.terminal.dataset.empty = "false";
    dom.terminal.focus({ preventScroll: true });
    const worker = spawnWorker(fixedSize);
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
    dom.stopButton.hidden = true;
    setRawMode(false);
    setStatus("Run stopped.", "idle");
}

function queueConsoleLine() {
    const line = dom.lineInput.value;
    dom.lineInput.value = "";
    // Echo input to console
    appendConsole("stdout", `> ${line}`);
    if (state.sharedLineBuffer) {
        sendLineToWorker(line);
    } else {
        state.queuedLines.push(line);
        setLineQueueCount(state.queuedLines.length);
        if (state.worker) {
            state.worker.postMessage({ type: "line", line });
        }
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

// Shared buffer for blocking Console.readLine:
// Layout: [0]=ready (Int32), [1]=length (Int32), [8..]=UTF-8 bytes
const LINE_BUFFER_SIZE = 1024;

function createSharedLineBuffer() {
    if (typeof SharedArrayBuffer !== "function" || !window.crossOriginIsolated) {
        return null;
    }
    return new SharedArrayBuffer(LINE_BUFFER_SIZE);
}

function sendLineToWorker(line) {
    const buf = state.sharedLineBuffer;
    if (!buf) {
        // Fallback: queue-based (non-blocking)
        state.worker?.postMessage({ type: "line", line });
        return;
    }
    const view = new Int32Array(buf);
    const bytes = new TextEncoder().encode(line);
    const byteView = new Uint8Array(buf);
    const maxLen = LINE_BUFFER_SIZE - 8;
    const len = Math.min(bytes.length, maxLen);
    byteView.set(bytes.subarray(0, len), 8);
    Atomics.store(view, 1, len); // length
    Atomics.store(view, 0, 1);   // ready = 1
    Atomics.notify(view, 0);     // wake worker
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

function runtimeKeyForCurrentModule(key) {
    const normalized = key === "escape" ? "esc" : key;
    if (normalized === "esc" && state.activeGame === "doom") {
        return "escape";
    }
    return normalized;
}

function dispatchRuntimeKey(key) {
    if (!state.worker) {
        return;
    }
    const runtimeKey = runtimeKeyForCurrentModule(key);
    if (!enqueueSharedKey(runtimeKey)) {
        state.worker.postMessage({ type: "key", key: runtimeKey });
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

    if (!dom.isolationNote || !dom.isolationCopy) {
        return;
    }

    if (interactiveReady) {
        dom.isolationNote.hidden = true;
        return;
    }

    dom.isolationNote.hidden = false;
    dom.isolationNote.dataset.tone = "warn";
    dom.isolationCopy.innerHTML =
        'Interactive <code>Terminal.readKey()</code> and <code>Console.readLine()</code> need cross-origin isolation. Start this runner with <code>python3 serve.py 4173</code>, not plain <code>python3 -m http.server</code>.';
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
        cellWidth: Number.parseFloat(styles.getPropertyValue("--cell-width")) || 9,
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

    // Render each character individually at grid-aligned positions.
    // This prevents wide glyphs (braille, emoji) from shifting subsequent columns.
    let prevColorIndex = -1;
    for (let row = 0; row < snapshot.rows; row += 1) {
        const rowBase = row * snapshot.cols;
        const y = row * cellHeight;
        for (let col = 0; col < snapshot.cols; col += 1) {
            const code = chars[rowBase + col] ?? TERMINAL_SPACE_CODE;
            if (code === TERMINAL_SPACE_CODE) continue;
            const colorIndex = colors[rowBase + col] ?? 0;
            if (colorIndex !== prevColorIndex) {
                ctx.fillStyle = TERM_COLORS[colorName(colorIndex)] || TERM_COLORS.default;
                prevColorIndex = colorIndex;
            }
            ctx.fillText(String.fromCodePoint(code), col * cellWidth, y);
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
    const items = event.dataTransfer?.items;
    // If the drop includes any directory entries, walk them — a flat
    // `.files` list drops folder contents silently in every browser.
    if (items && Array.from(items).some(it => it.webkitGetAsEntry?.()?.isDirectory)) {
        const collected = [];
        for (const it of items) {
            const entry = it.webkitGetAsEntry?.();
            if (entry) await walkEntry(entry, "", collected);
        }
        await onFileChange(collected);
        return;
    }
    await onFileChange(event.dataTransfer.files);
});

// Recursively walk a DataTransferItem entry tree, pushing File objects
// with webkitRelativePath set so the multi-file loader can mirror the
// folder layout in the virtual fs.
async function walkEntry(entry, prefix, out) {
    if (entry.isFile) {
        const file = await new Promise((res, rej) => entry.file(res, rej));
        Object.defineProperty(file, "webkitRelativePath", {
            value: prefix + entry.name,
            configurable: true,
        });
        out.push(file);
        return;
    }
    if (entry.isDirectory) {
        const reader = entry.createReader();
        // readEntries paginates — loop until empty.
        const entries = [];
        while (true) {
            const batch = await new Promise((res, rej) => reader.readEntries(res, rej));
            if (!batch.length) break;
            entries.push(...batch);
        }
        for (const child of entries) {
            await walkEntry(child, prefix + entry.name + "/", out);
        }
    }
}

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
    dispatchRuntimeKey(key);
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
setStatus("Pick a game or write Aver code to start.", "idle");

// ---------------------------------------------------------------------------
// Game gallery
// ---------------------------------------------------------------------------

const outputPane = document.querySelector("[data-output-pane]");

function setOutputMode(mode) {
    // mode: "console" | "terminal"
    if (outputPane) outputPane.dataset.mode = mode;
}

document.querySelectorAll("[data-game]").forEach(btn => {
    btn.addEventListener("click", async () => {
        const name = btn.dataset.game;
        const args = btn.dataset.args ? btn.dataset.args.split(" ") : [];
        document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
        btn.classList.add("active");
        state.programArgs = args;
        state.activeGame = name;
        const isConsoleGame = btn.hasAttribute("data-console-game");
        setOutputMode(isConsoleGame ? "console" : "terminal");
        buildTouchControls(name);
        clearOutput();
        const readlineBar = document.querySelector("[data-readline-bar]");
        if (readlineBar) readlineBar.style.display = isConsoleGame ? "flex" : "none";
        setStatus(`Loading ${name}…`, "info");

        try {
            // Fetch .wasm + all .av source files in parallel so the
            // editor shows real tabs (editable) at the same moment the
            // binary is ready to run — no separate source-viewer mode.
            const [resp] = await Promise.all([
                fetch(`./${name}.wasm`),
                loadGameSourcesAsTabs(name),
            ]);
            if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
            const bytes = await resp.arrayBuffer();
            state.wasmBytes = bytes;
            state.wasmName = `${name}.wasm`;
            const sizeKiB = (bytes.byteLength / 1024).toFixed(1);
            dom.fileMeta.textContent = `${name}.wasm — ${sizeKiB} KiB`;
            dom.runButton.disabled = false;
            // Show real prebuilt size (not a fake compile timing) so
            // users don't see "compiled 0ms · 9.5 KiB" on a 29 KiB
            // roguelike that was shipped by the CLI.
            renderPrebuiltMeta(`${name}.wasm`, bytes.byteLength);
            // Don't auto-run — loading a game should surface the source
            // for reading first. ▶ Run is right there when the user's
            // ready. (Pre-edit: runs the prebuilt bytes; post-edit:
            // forkPrebuiltIfEdited kicks in and Run recompiles.)
            setStatus(`Loaded ${name}.wasm · ${sizeKiB} KiB · ▶ Run to play`, "success");
        } catch (e) {
            setStatus(`Failed to load ${name}: ${e.message}`, "error");
        }
    });
});

// ---------------------------------------------------------------------------
// Code editor + in-browser compile
// ---------------------------------------------------------------------------

const EXAMPLES = {
    hello: `module Hello\n    intent = "minimal effect demo — print a greeting to console"\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print("Hello, World!")\n    Console.print("Hello from the Aver Playground!")\n`,
    calculator: `module Calculator\n    intent = "tiny arithmetic API with verified contracts"\n\nfn add(a: Int, b: Int) -> Int\n    ? "integer sum"\n    a + b\n\nverify add\n    add(1, 2) => 3\n    add(0, 0) => 0\n    add(0 - 1, 1) => 0\n\nfn divide(a: Int, b: Int) -> Result<Int, String>\n    ? "integer division; returns Err on divide-by-zero"\n    match b\n        0 -> Result.Err("Division by zero")\n        _ -> Result.Ok(a / b)\n\nverify divide\n    divide(10, 2) => Result.Ok(5)\n    divide(10, 0) => Result.Err("Division by zero")\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(add(2, 3))\n    Console.print(divide(10, 2))\n    Console.print(divide(10, 0))\n`,
    fibonacci: `module Fibonacci\n    intent = "naive Fibonacci — showcases verify on a recursive function"\n\nfn fib(n: Int) -> Int\n    ? "naive Fibonacci — O(2^n), good for demonstrating verify, not for production"\n    match n\n        0 -> 0\n        1 -> 1\n        _ -> fib(n - 1) + fib(n - 2)\n\nverify fib\n    fib(0) => 0\n    fib(1) => 1\n    fib(2) => 1\n    fib(10) => 55\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print("fib(10) = {fib(10)}")\n    Console.print("fib(20) = {fib(20)}")\n`,
    shapes: `module Shapes\n    intent = "sum-type driven area calculation"\n\ntype Shape\n    Circle(Float)\n    Rectangle(Float, Float)\n\nfn area(shape: Shape) -> Float\n    ? "area for a Shape variant"\n    match shape\n        Shape.Circle(r) -> 3.14159 * r * r\n        Shape.Rectangle(w, h) -> w * h\n\nverify area\n    area(Shape.Circle(1.0)) => 3.14159\n    area(Shape.Rectangle(3.0, 4.0)) => 12.0\n    area(Shape.Rectangle(0.0, 5.0)) => 0.0\n\nfn main() -> Unit\n    ! [Console.print]\n    c = Shape.Circle(5.0)\n    r = Shape.Rectangle(3.0, 4.0)\n    Console.print("circle area = {Float.toString(area(c))}")\n    Console.print("rect area = {Float.toString(area(r))}")\n`,
    quicksort: `module Quicksort\n    intent = "classic quicksort with verified partition helpers"\n\nfn filterLess(xs: List<Int>, pivot: Int) -> List<Int>\n    ? "keep elements strictly less than pivot"\n    match xs\n        [] -> []\n        [h, ..t] -> match h < pivot\n            true -> List.prepend(h, filterLess(t, pivot))\n            false -> filterLess(t, pivot)\n\nverify filterLess\n    filterLess([], 3) => []\n    filterLess([1, 5, 2, 8], 3) => [1, 2]\n    filterLess([5, 5, 5], 5) => []\n\nfn filterGte(xs: List<Int>, pivot: Int) -> List<Int>\n    ? "keep elements greater than or equal to pivot"\n    match xs\n        [] -> []\n        [h, ..t] -> match h >= pivot\n            true -> List.prepend(h, filterGte(t, pivot))\n            false -> filterGte(t, pivot)\n\nverify filterGte\n    filterGte([], 3) => []\n    filterGte([1, 5, 2, 8], 3) => [5, 8]\n    filterGte([5, 5, 5], 5) => [5, 5, 5]\n\nfn quicksort(xs: List<Int>) -> List<Int>\n    ? "classic quicksort: pivot + recurse on less/gte partitions"\n    match xs\n        [] -> []\n        [pivot, ..rest] -> List.concat(List.concat(quicksort(filterLess(rest, pivot)), [pivot]), quicksort(filterGte(rest, pivot)))\n\nverify quicksort\n    quicksort([]) => []\n    quicksort([3, 1, 2]) => [1, 2, 3]\n    quicksort([5, 5, 5]) => [5, 5, 5]\n\nfn main() -> Unit\n    ! [Console.print]\n    input = [38, 27, 43, 3, 9, 82, 10]\n    Console.print("input:  {input}")\n    Console.print("sorted: {quicksort(input)}")\n`,
    rle: `module Rle\n    intent = "run-length encoding: compress consecutive repeats"\n\ntype RlePair\n    Pair(String, Int)\n\nfn encode(chars: List<String>, current: String, count: Int, acc: List<RlePair>) -> List<RlePair>\n    ? "run-length encoding accumulator loop"\n    match chars\n        [] -> List.reverse(List.prepend(RlePair.Pair(current, count), acc))\n        [h, ..t] -> match h == current\n            true -> encode(t, current, count + 1, acc)\n            false -> encode(t, h, 1, List.prepend(RlePair.Pair(current, count), acc))\n\nverify encode\n    encode([], "a", 3, []) => [RlePair.Pair("a", 3)]\n    encode(["a"], "a", 1, []) => [RlePair.Pair("a", 2)]\n\nfn rleEncode(input: String) -> List<RlePair>\n    ? "run-length encode a string: 'aaab' -> [(a,3),(b,1)]"\n    chars = String.chars(input)\n    match chars\n        [] -> []\n        [first, ..rest] -> encode(rest, first, 1, [])\n\nverify rleEncode\n    rleEncode("") => []\n    rleEncode("aaa") => [RlePair.Pair("a", 3)]\n    rleEncode("aab") => [RlePair.Pair("a", 2), RlePair.Pair("b", 1)]\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(rleEncode("aaabbbccddddee"))\n`,

    // ─── Trust-loop starters ──────────────────────────────────────────
    // Each demonstrates one leg of Aver's thesis: effects → verify →
    // decisions → errors-that-teach. Deep-linkable via ?example=<slug>.

    "effect-violation": `module Starter\n    intent = "greet the user — but forgets to declare the effect"\n\nfn greet() -> Unit\n    Console.print("hello, world")\n\nfn main() -> Unit\n    ! [Console.print]\n    greet()\n`,

    "verify-first": `module Starter\n    intent = "pure arithmetic, locked by verify"\n\nfn add(a: Int, b: Int) -> Int\n    ? "sum of two ints"\n    a + b\n\nverify add\n    add(1, 2) => 3\n    add(2, 3) => 5\n    add(0, 0) => 0\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(add(2, 3))\n`,

    "decision-block": `module Payments\n    intent = "transactions as append-only log"\n\ndecision ImmutableTransactions\n    date = "2026-04-20"\n    reason =\n        "Audit trail requires transactions be append-only and non-modifiable."\n        "Mutable ledger would break compliance with SOX."\n    chosen = "ImmutableWithVersioning"\n    rejected = ["MutableLedger"]\n    impacts = [createTransaction]\n\nfn createTransaction(amount: Int) -> Int\n    ? "build an immutable transaction record"\n    amount\n\nverify createTransaction\n    createTransaction(100) => 100\n    createTransaction(0) => 0\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(createTransaction(100))\n`,

    "result-monadic": `module Starter\n    intent = "idiomatic Result with ? short-circuit"\n\nfn parseNumber(s: String) -> Result<Int, String>\n    ? "parse a string into an int, with a friendly error"\n    match Int.fromString(s)\n        Result.Ok(n) -> Result.Ok(n)\n        Result.Err(_) -> Result.Err("not a number: {s}")\n\nverify parseNumber\n    parseNumber("42") => Result.Ok(42)\n    parseNumber("") => Result.Err("not a number: ")\n    parseNumber("abc") => Result.Err("not a number: abc")\n\nfn doubleParsed(s: String) -> Result<Int, String>\n    ? "parse then double; propagates parse errors via ?"\n    n = parseNumber(s)?\n    Result.Ok(n + n)\n\nverify doubleParsed\n    doubleParsed("21") => Result.Ok(42)\n    doubleParsed("oops") => Result.Err("not a number: oops")\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(doubleParsed("21"))\n    Console.print(doubleParsed("oops"))\n`,
};

const codeEditor = document.querySelector("[data-code-editor]");

// ── Virtual multi-file state ──────────────────────────────────────
// The editor now holds a Map<path, source>; `codeEditor.value`
// mirrors the currently-active entry. All listeners call
// `getActiveSource()` / `getProjectFiles()` instead of touching
// `codeEditor.value` directly, so the same flow works for a single
// freshly-typed file or a dropped folder.
function getActiveSource() {
    if (state.activeFile && state.files.has(state.activeFile)) {
        return state.files.get(state.activeFile);
    }
    return codeEditor ? codeEditor.value : "";
}
function getActiveName() {
    return state.activeFile || "playground.av";
}
function getProjectFiles() {
    // Return a plain object {path: source} for multi-file consumers.
    // Single-file fallback: use whatever's in the editor.
    if (state.files.size === 0) {
        return { "playground.av": codeEditor ? codeEditor.value : "" };
    }
    const out = {};
    for (const [k, v] of state.files) out[k] = v;
    return out;
}

function renderTabs() {
    if (!dom.editorTabs) return;
    dom.editorTabs.textContent = "";
    for (const [path] of state.files) {
        const tab = document.createElement("span");
        tab.className = "tab" + (path === state.activeFile ? " active" : "");
        tab.dataset.path = path;
        const label = document.createElement("span");
        label.textContent = path;
        tab.appendChild(label);
        const close = document.createElement("span");
        close.className = "tab-close";
        close.textContent = "×";
        close.title = `Close ${path}`;
        close.addEventListener("click", (e) => {
            e.stopPropagation();
            closeFile(path);
        });
        tab.appendChild(close);
        tab.addEventListener("click", () => setActiveFile(path));
        dom.editorTabs.appendChild(tab);
    }
    if (state.files.size > 0) {
        const addBtn = document.createElement("button");
        addBtn.type = "button";
        addBtn.className = "tab-new";
        addBtn.textContent = "+ new";
        addBtn.title = "Create a new .av tab in the editor";
        addBtn.addEventListener("click", newFilePrompt);
        dom.editorTabs.appendChild(addBtn);
    }
}

function setActiveFile(path) {
    if (!state.files.has(path)) return;
    // Save current editor value back into the map before switching.
    if (state.activeFile && state.files.has(state.activeFile)) {
        state.files.set(state.activeFile, codeEditor.value);
    }
    state.activeFile = path;
    codeEditor.value = state.files.get(path);
    updateHighlight();
    renderTabs();
}

function openFile(path, source, { activate = true } = {}) {
    state.files.set(path, source);
    if (activate) setActiveFile(path);
    else renderTabs();
}

function closeFile(path) {
    if (!state.files.has(path)) return;
    state.files.delete(path);
    if (state.activeFile === path) {
        const next = state.files.keys().next().value || null;
        state.activeFile = null;
        if (next) setActiveFile(next);
        else {
            codeEditor.value = "";
            renderTabs();
        }
    } else {
        renderTabs();
    }
}

function newFilePrompt() {
    const name = prompt("New file name (e.g. Helpers.av):", "Helpers.av");
    if (!name) return;
    const normalized = name.endsWith(".av") ? name : `${name}.av`;
    if (state.files.has(normalized)) {
        setActiveFile(normalized);
        return;
    }
    leaveWasmOnlyMode();
    openFile(normalized, `module ${normalized.replace(/\.av$/, "")}\n    intent = ""\n`);
}

// ── "Binary .wasm only" overlay ───────────────────────────────────
// When the user drops a compiled .wasm we still support Run (the
// bytes live in state.wasmBytes), but there's no source to edit.
// Show a clear overlay instead of an empty editor so analysis
// buttons aren't a silent mystery.
function enterWasmOnlyMode(wasmName) {
    state.wasmOnly = true;
    // Park any current source tabs silently — they're irrelevant
    // while we're displaying a binary. The user can drop an .av /
    // folder to leave this mode.
    if (codeEditor) codeEditor.value = "";
    if (dom.editorTabs) {
        dom.editorTabs.textContent = "";
        const tab = document.createElement("span");
        tab.className = "tab active";
        tab.textContent = `🛆 ${wasmName} (binary)`;
        tab.title = "Compiled WASM — no Aver source. Drop an .av file or folder to edit source.";
        dom.editorTabs.appendChild(tab);
    }
    showEditorOverlay(
        "Compiled WASM loaded",
        `No Aver source available. You can ▶ Run this binary. Drop an .av file or a folder of .av files to resume editing.`
    );
}
function leaveWasmOnlyMode() {
    state.wasmOnly = false;
    hideEditorOverlay();
    renderTabs();
}
function showEditorOverlay(title, message) {
    let overlay = document.querySelector(".editor-overlay");
    if (!overlay) {
        overlay = document.createElement("div");
        overlay.className = "editor-overlay";
        const wrap = document.querySelector(".editor-wrap");
        if (wrap) wrap.appendChild(overlay);
    }
    overlay.innerHTML = "";
    const h = document.createElement("strong");
    h.textContent = title;
    const p = document.createElement("p");
    p.textContent = message;
    overlay.appendChild(h);
    overlay.appendChild(p);
    overlay.style.display = "flex";
}
function hideEditorOverlay() {
    const overlay = document.querySelector(".editor-overlay");
    if (overlay) overlay.style.display = "none";
}
const compileRunBtn = document.querySelector("[data-compile-run]");
const examplesSelect = document.querySelector("[data-examples]");

let compiler = null;

async function loadCompiler() {
    if (compiler) return compiler;
    setStatus("Loading compiler (first time)…", "info");
    const mod = await import("./wasm/aver.js");
    await mod.default("./wasm/aver_bg.wasm");
    compiler = mod;
    return compiler;
}

// Dispatch check/verify/why/context/audit to the multi-file binding
// when the virtual fs has more than one tab, so depends[...] resolve
// against the in-browser file map instead of surfacing as bogus
// "Unknown identifier 'Types'" noise on every game fork.
function runAnalysis(comp, kind, fallbackSource) {
    const entry = pickEntryFile();
    if (state.files.size > 1 && entry && typeof comp[`aver_${kind}_project`] === "function") {
        const filesObj = Object.fromEntries(state.files);
        return comp[`aver_${kind}_project`](JSON.stringify(filesObj), entry);
    }
    return comp[`aver_${kind}`](fallbackSource);
}

if (compileRunBtn) {
    compileRunBtn.addEventListener("click", async () => {
        // Run the prebuilt bytes whenever we have them AND the source
        // hasn't been edited — covers both dropped .wasm (wasmOnly) and
        // freshly-clicked games (source shown as tabs but still
        // pristine). First edit drops state.wasmBytes → recompile.
        if (state.wasmBytes && (state.wasmOnly || state.pristineFiles)) {
            // Re-run the already-loaded game/binary. Terminal size is
            // whatever was configured at first load; re-Run doesn't
            // change output mode, just replays the module.
            const activeGameBtn = document.querySelector("[data-game].active");
            const fixed = activeGameBtn && !activeGameBtn.hasAttribute("data-console-game")
                ? { cols: 80, rows: 35 }
                : undefined;
            runSelectedModule(fixed);
            return;
        }
        const entry = pickEntryFile();
        const source = entry ? state.files.get(entry) : getActiveSource();
        if (!source || !source.trim()) return;

        document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
        state.activeGame = null;

        // Terminal vs console detection needs to look at *all* sources
        // for multi-file projects — main.av might import a renderer
        // that's the only thing calling Terminal.
        const anySourceUsesTerminal = state.files.size > 0
            ? Array.from(state.files.values()).some(s => s.includes("Terminal."))
            : source.includes("Terminal.");
        setOutputMode(anySourceUsesTerminal ? "terminal" : "console");
        clearOutput();
        const readlineBar = document.querySelector("[data-readline-bar]");
        if (readlineBar) readlineBar.style.display = anySourceUsesTerminal ? "none" : "flex";

        try {
            const comp = await loadCompiler();
            const t0 = performance.now();
            let wasmBytes;
            // Route multi-file projects through the project binding so
            // `depends [Types, Map, ...]` resolve against the virtual
            // fs instead of erroring on "unknown module".
            if (state.files.size > 1 && entry) {
                setStatus(`Compiling project (entry: ${entry})…`, "info");
                const filesObj = Object.fromEntries(state.files);
                wasmBytes = comp.aver_compile_project(JSON.stringify(filesObj), entry);
            } else {
                setStatus("Compiling…", "info");
                wasmBytes = comp.aver_compile(source);
            }
            const ms = (performance.now() - t0).toFixed(0);

            state.wasmBytes = wasmBytes.buffer;
            state.wasmName = "playground.wasm";
            renderCompileMeta(wasmBytes.length, ms);
            dom.runButton.disabled = false;
            runSelectedModule();
        } catch (e) {
            const msg = e.message || String(e);
            setStatus("Compile error", "error");
            appendConsole("stderr", msg);
        }
    });
}

// Decide which file in the virtual fs is the `fn main` entry for
// project compilation. Prefer "main.av" (CLI convention); fall back
// to any file whose source declares `module Main`; otherwise the
// active tab.
function pickEntryFile() {
    if (state.files.size === 0) return null;
    for (const key of state.files.keys()) {
        if (/^main\.av$/i.test(key) || /\/main\.av$/i.test(key)) return key;
    }
    for (const [key, src] of state.files) {
        if (/^\s*module\s+Main\b/m.test(src)) return key;
    }
    return state.activeFile || state.files.keys().next().value || null;
}

const highlightEl = document.querySelector("[data-highlight]");

function updateHighlight() {
    if (!highlightEl || !codeEditor) return;
    import("./highlight.js").then(({ highlightAver }) => {
        highlightEl.innerHTML = highlightAver(codeEditor.value) + "\n";
        // Sync scroll
        highlightEl.scrollTop = codeEditor.scrollTop;
        highlightEl.scrollLeft = codeEditor.scrollLeft;
    });
}

function exampleTabName(key) {
    // Name tabs after the example so the multi-file editor shows
    // something recognizable instead of a generic placeholder.
    const pascal = key
        .split(/[-_]/)
        .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
        .join("");
    return `${pascal || "Playground"}.av`;
}

function loadExampleAsTab(key) {
    const source = EXAMPLES[key];
    if (!source) return;
    leaveWasmOnlyMode();
    state.files.clear();
    state.activeFile = null;
    state.pristineFiles = null;
    state.wasmBytes = null;
    state.wasmName = null;
    openFile(exampleTabName(key), source);
}

// Compare the virtual fs against the pristine snapshot (set when a
// game is loaded). Any divergence "forks" the prebuilt binary: we
// drop the cached bytes so the next Run recompiles from source.
function forkPrebuiltIfEdited() {
    if (!state.pristineFiles) return;
    for (const [path, pristine] of state.pristineFiles) {
        if ((state.files.get(path) ?? "") !== pristine) {
            state.pristineFiles = null;
            state.wasmBytes = null;
            state.wasmName = null;
            setStatus("Edited — Run rebuilds.", "info");
            return;
        }
    }
}

if (codeEditor) {
    codeEditor.addEventListener("input", () => {
        // Mirror the current buffer into the virtual fs whenever the
        // user types — keeps getActiveSource() cheap and correct.
        if (state.activeFile && state.files.has(state.activeFile)) {
            state.files.set(state.activeFile, codeEditor.value);
        }
        // When game source was loaded pristine and the user edits it,
        // drop the prebuilt bytes so the next Run recompiles the
        // active file — they're "forking" the game.
        forkPrebuiltIfEdited();
        updateHighlight();
    });
    codeEditor.addEventListener("scroll", () => {
        if (highlightEl) {
            highlightEl.scrollTop = codeEditor.scrollTop;
            highlightEl.scrollLeft = codeEditor.scrollLeft;
        }
    });
    codeEditor.addEventListener("keydown", (e) => {
        if ((e.ctrlKey || e.metaKey) && e.key === "Enter") {
            e.preventDefault();
            compileRunBtn?.click();
        }
        if (e.key === "Tab") {
            e.preventDefault();
            const s = codeEditor.selectionStart;
            const end = codeEditor.selectionEnd;
            codeEditor.value = codeEditor.value.substring(0, s) + "    " + codeEditor.value.substring(end);
            codeEditor.selectionStart = codeEditor.selectionEnd = s + 4;
            if (state.activeFile) state.files.set(state.activeFile, codeEditor.value);
        }
    });
    // Bootstrap with the hello example as the initial tab so new
    // visitors see a tab bar + editable source from first paint.
    loadExampleAsTab("hello");
}

if (examplesSelect) {
    examplesSelect.addEventListener("change", () => {
        const name = examplesSelect.value;
        if (EXAMPLES[name]) loadExampleAsTab(name);
    });
}

// Check button
const checkBtn = document.querySelector("[data-check]");
if (checkBtn) {
    checkBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        setOutputMode("console");
        state.activeGame = null;
        clearOutput();

        try {
            const comp = await loadCompiler();
            setStatus("Checking…", "info");
            const json = runAnalysis(comp, "check", source);
            const bundle = JSON.parse(json);
            const diagnostics = bundle.diagnostics || [];
            const lines = source.split("\n");
            let hasError = false;

            if (diagnostics.length === 0) {
                appendConsole("stdout", "✓ All checks passed.");
            }

            for (const d of diagnostics) {
                const isErr = d.severity === "error" || d.severity === "fail";
                if (isErr) hasError = true;
                const tag = d.severity;
                const channel = isErr ? "stderr" : "warning";
                const lineNum = d.span?.line || 0;
                const col = d.span?.col || 0;

                appendConsole(channel,
                    `\n${tag}[${d.slug}]: ${d.summary}`);
                appendConsole(channel,
                    `  at: ${d.span?.file || "playground"}:${lineNum}:${col}`);

                if (d.fn_name) {
                    appendConsole(channel, `  in-fn: ${d.fn_name}`);
                }
                if (d.repair?.primary) {
                    appendConsole(channel, `  repair: ${d.repair.primary}`);
                }

                if (lineNum > 0 && lineNum <= lines.length) {
                    const snippet = lines[lineNum - 1];
                    const pad = String(lineNum).length;
                    appendConsole("stdout", `${" ".repeat(pad + 1)} |`);
                    appendConsole("stdout", ` ${lineNum} | ${snippet}`);
                    const underline = d.regions?.[0]?.underline;
                    if (underline && underline.col > 0) {
                        const caretPad = " ".repeat(underline.col - 1);
                        const carets = "^".repeat(Math.max(1, underline.len || 1));
                        appendConsole("stdout",
                            `${" ".repeat(pad + 1)} | ${caretPad}${carets}${underline.label ? "  " + underline.label : ""}`);
                    } else {
                        appendConsole("stdout", `${" ".repeat(pad + 1)} |`);
                    }
                }
            }

            setStatus(hasError ? "Check found errors" : "Check passed",
                      hasError ? "error" : "success");
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Check failed", "error");
        }
    });
}

// Verify button — runs the canonical analysis pipeline with verify
// block execution enabled. Shares rendering with Check but also reports
// pass/fail counts per block.
const verifyBtn = document.querySelector("[data-verify]");
if (verifyBtn) {
    verifyBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        setOutputMode("console");
        state.activeGame = null;
        clearOutput();

        try {
            const comp = await loadCompiler();
            setStatus("Verifying…", "info");
            const json = runAnalysis(comp, "verify", source);
            const bundle = JSON.parse(json);
            const diagnostics = bundle.diagnostics || [];
            const lines = source.split("\n");

            const verifyFailSlugs = new Set([
                "verify-mismatch",
                "verify-runtime-error",
                "verify-unexpected-err",
            ]);
            const verifyFailures = diagnostics.filter((d) => verifyFailSlugs.has(d.slug));
            const staticIssues = diagnostics.filter((d) => !verifyFailSlugs.has(d.slug));
            const hasStaticErrors = staticIssues.some(
                (d) => d.severity === "error" || d.severity === "fail"
            );
            const verifySummary = bundle.verify_summary;

            if (hasStaticErrors) {
                appendConsole("stderr",
                    "Static errors found — verify blocks were not executed.\n");
            }

            for (const d of staticIssues) {
                const tag = d.severity;
                const isErr = d.severity === "error" || d.severity === "fail";
                const channel = isErr ? "stderr" : "warning";
                const lineNum = d.span?.line || 0;
                const col = d.span?.col || 0;
                appendConsole(channel, `\n${tag}[${d.slug}]: ${d.summary}`);
                appendConsole(channel,
                    `  at: ${d.span?.file || "playground"}:${lineNum}:${col}`);
                if (d.repair?.primary) {
                    appendConsole(channel, `  repair: ${d.repair.primary}`);
                }
            }

            if (verifyFailures.length === 0 && !hasStaticErrors) {
                appendConsole("success", "✓ All verify cases passed.");
            }

            if (verifySummary?.blocks?.length && !hasStaticErrors) {
                appendConsole("stdout", "");
                for (const b of verifySummary.blocks) {
                    const tag = b.failed > 0 ? "✗" : "✓";
                    const channel = b.failed > 0 ? "stderr" : "success";
                    const breakdown = b.skipped > 0
                        ? ` (${b.passed}/${b.total} passed, ${b.skipped} skipped)`
                        : ` (${b.passed}/${b.total} passed)`;
                    appendConsole(channel, `  ${tag} ${b.name}${breakdown}`);
                }
            }

            for (const d of verifyFailures) {
                const lineNum = d.span?.line || 0;
                const col = d.span?.col || 0;
                appendConsole("stderr", `\nfail[${d.slug}]: ${d.summary}`);
                appendConsole("stdout",
                    `  at: ${d.span?.file || "playground"}:${lineNum}:${col}`);
                for (const [key, value] of d.fields || []) {
                    appendConsole("stdout", `  ${key}: ${value}`);
                }
                if (lineNum > 0 && lineNum <= lines.length) {
                    const snippet = lines[lineNum - 1];
                    const pad = String(lineNum).length;
                    appendConsole("stdout", `${" ".repeat(pad + 1)} |`);
                    appendConsole("stdout", ` ${lineNum} | ${snippet}`);
                    appendConsole("stdout", `${" ".repeat(pad + 1)} |`);
                }
            }

            const anyFail = hasStaticErrors || verifyFailures.length > 0;
            setStatus(
                anyFail
                    ? `Verify: ${verifyFailures.length} failing case(s)`
                    : "Verify passed",
                anyFail ? "error" : "success"
            );
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Verify failed", "error");
        }
    });
}

// Why button — file-local justification summary: which functions have
// description, verify, coverage, decision impact; which don't.
const whyBtn = document.querySelector("[data-why]");
if (whyBtn) {
    whyBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        setOutputMode("console");
        state.activeGame = null;
        clearOutput();
        clearCompileMeta();

        try {
            const comp = await loadCompiler();
            setStatus("Analyzing justification…", "info");
            const json = runAnalysis(comp, "why", source);
            const bundle = JSON.parse(json);
            const summary = bundle.why_summary;
            if (!summary) {
                appendConsole("stderr", "No why summary in response.");
                setStatus("Why failed", "error");
                return;
            }

            // Header row with download
            const header = document.createElement("div");
            header.className = "why-header-row";
            const title = document.createElement("span");
            title.className = "title";
            title.textContent = "Why — justification map";
            header.appendChild(title);
            header.appendChild(makeDownloadButton(
                "⬇ Download as .jsonl",
                "Save the why bundle as JSON Lines — same shape as `aver why --json`",
                () => downloadJsonl(bundle, "why.jsonl")
            ));
            dom.console.appendChild(header);

            const pct = (part) =>
                summary.total_lines > 0
                    ? Math.round((part * 100) / summary.total_lines) + "%"
                    : "0%";

            appendConsole("stdout", `${summary.file_label}: ${summary.total_lines} lines`);
            appendConsole("success",
                `  justified   ${summary.justified_lines} lines (${pct(summary.justified_lines)})`);
            appendConsole("warning",
                `  partial     ${summary.partial_lines} lines (${pct(summary.partial_lines)})`);
            appendConsole("stderr",
                `  unjustified ${summary.unjustified_lines} lines (${pct(summary.unjustified_lines)})`);

            if (!summary.has_module_intent) {
                appendConsole("warning", "  ! module has no intent block");
            }

            for (const d of summary.decisions || []) {
                appendConsole("stdout",
                    `  decision ${d.name} (${d.date}): ${d.reason_prefix}`);
            }

            const problematic = (summary.functions || []).filter(
                (f) => f.level !== "justified"
            );
            problematic.sort((a, b) => {
                const pa = a.level === "unjustified" ? 0 : 1;
                const pb = b.level === "unjustified" ? 0 : 1;
                if (pa !== pb) return pa - pb;
                return b.lines - a.lines;
            });

            if (problematic.length === 0) {
                appendConsole("success", "\n✓ Every function is fully justified.");
            } else {
                appendConsole("stdout", "");
                for (const f of problematic) {
                    const hints = (f.missing || []).join(", ");
                    const suffix = hints ? `  (${hints})` : "";
                    const channel = f.level === "unjustified" ? "stderr" : "warning";
                    appendConsole(channel, `  ${f.level}: ${f.name}${suffix}`);
                }
            }

            const justifiedPct =
                summary.total_lines > 0
                    ? (summary.justified_lines * 100) / summary.total_lines
                    : 0;
            setStatus(
                problematic.length === 0
                    ? "Why: fully justified"
                    : `Why: ${Math.round(justifiedPct)}% justified`,
                justifiedPct >= 60 ? "success" : "info"
            );
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Why failed", "error");
        }
    });
}

// Context button — file-local module/API shape: module name, intent,
// depends, exposed fns with signatures/effects/qualifiers, types,
// decisions. Entry file only; dependency bodies are not expanded.
const contextBtn = document.querySelector("[data-context]");
if (contextBtn) {
    contextBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        setOutputMode("console");
        state.activeGame = null;
        clearOutput();
        clearCompileMeta();

        try {
            const comp = await loadCompiler();
            setStatus("Building context…", "info");
            const json = runAnalysis(comp, "context", source);
            const bundle = JSON.parse(json);
            const ctx = bundle.context_summary;
            if (!ctx) {
                appendConsole("stderr", "No context summary in response.");
                setStatus("Context failed", "error");
                return;
            }

            const panel = renderContextPanel(ctx);
            dom.console.appendChild(panel);

            setStatus("Context ready", "success");
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Context failed", "error");
        }
    });
}

// Compile meta: compile time + raw WASM size + hover-only footnote
// explaining the inline runtime. We tried lazy-loading binaryen from
// three CDNs (unpkg 404s, esm.sh chokes on Node-style module.require,
// skypack's build pipeline fails on binaryen's native wasm blob) —
// three strikes, not worth keeping the code path. CLI users who want
// the optimized size run `aver compile --wasm-opt oz` locally.
function clearCompileMeta() {
    if (dom.compileMeta) dom.compileMeta.textContent = "";
}

// For prebuilt games: show the .wasm size we just fetched, not a fake
// "compiled Nms" number — no compile happened here.
function renderPrebuiltMeta(name, rawSize) {
    if (!dom.compileMeta) return;
    dom.compileMeta.textContent = "";
    const label = document.createElement("span");
    label.className = "size-label";
    label.textContent = "prebuilt ";
    dom.compileMeta.appendChild(label);
    const main = document.createElement("span");
    main.className = "size-main";
    main.textContent = `${name} · ${(rawSize / 1024).toFixed(1)} KiB`;
    dom.compileMeta.appendChild(main);
    const footnote = document.createElement("span");
    footnote.className = "compile-footnote";
    footnote.textContent = "*";
    footnote.title =
        "Game binaries are compiled by the CLI with `aver compile " +
        "--wasm-opt oz` and served as static files. Edit any tab to " +
        "fork — next ▶ Run recompiles the active file in the browser.";
    dom.compileMeta.appendChild(footnote);
}

function renderCompileMeta(rawSize, compileMs) {
    if (!dom.compileMeta) return;
    dom.compileMeta.textContent = "";

    const label = document.createElement("span");
    label.className = "size-label";
    label.textContent = "compiled ";
    dom.compileMeta.appendChild(label);

    const main = document.createElement("span");
    main.className = "size-main";
    main.textContent = `${compileMs}ms · ${(rawSize / 1024).toFixed(1)} KiB`;
    dom.compileMeta.appendChild(main);

    const footnote = document.createElement("span");
    footnote.className = "compile-footnote";
    footnote.textContent = "*";
    footnote.title =
        "Raw WASM size, no wasm-opt. Includes Aver's inline runtime " +
        "(bump allocator, strings, lists, maps). The CLI's " +
        "`aver compile --wasm-opt oz` strips unused runtime and " +
        "simplifies the generated code — a one-line program drops to " +
        "~250 B, real programs roughly halve.";
    dom.compileMeta.appendChild(footnote);
}

// Context panel — markdown-style rendered DOM. Matches what an LLM
// would see via `aver context`, presented as a module card instead
// of plain-text console lines.
function contextToMarkdown(ctx) {
    const lines = [];
    lines.push(`# module ${ctx.module_name || "(unnamed)"}`);
    lines.push("");
    if (ctx.intent) {
        lines.push(`> ${ctx.intent.replace(/\n/g, " ")}`);
        lines.push("");
    }
    const metaRow = (label, items, codeWrap) => {
        if (!items || items.length === 0) return;
        const rendered = codeWrap
            ? items.map((x) => `\`${x}\``).join(", ")
            : items.join(", ");
        lines.push(`**${label}:** ${rendered}`);
    };
    metaRow("depends", ctx.depends);
    metaRow("exposes", ctx.exposes, true);
    metaRow("exposes opaque", ctx.exposes_opaque, true);
    metaRow("api effects", ctx.api_effects, true);
    if (ctx.main_effects) metaRow("main effects", ctx.main_effects, true);

    if ((ctx.types || []).length > 0) {
        lines.push("");
        lines.push("## Types");
        lines.push("");
        for (const t of ctx.types) {
            const members = (t.fields_or_variants || []).join(", ");
            const suffix = members ? ` (${members})` : "";
            lines.push(`- **${t.kind}** \`${t.name}\`${suffix}`);
        }
    }

    if ((ctx.functions || []).length > 0) {
        lines.push("");
        lines.push("## Functions");
        for (const f of ctx.functions) {
            lines.push("");
            lines.push(`### ${f.name}${f.is_exposed ? "" : " (private)"}`);
            lines.push("");
            lines.push("```aver");
            lines.push(f.signature);
            lines.push("```");
            if (f.description) {
                lines.push("");
                lines.push(`> ${f.description}`);
            }
            const flags = [...(f.qualifiers || [])];
            if (f.auto_memo) flags.push("AUTO_MEMO");
            if (f.auto_tco) flags.push("AUTO_TCO");
            if (flags.length > 0) {
                lines.push("");
                lines.push(`**Flags:** ${flags.map((x) => `\`${x}\``).join(", ")}`);
            }
            if ((f.effects || []).length > 0) {
                lines.push("");
                lines.push(`**Effects:** ${f.effects.map((x) => `\`${x}\``).join(", ")}`);
            }
            if (f.verify_count > 0) {
                lines.push("");
                lines.push(`**Verify (${f.verify_count} case${f.verify_count > 1 ? "s" : ""}):**`);
                for (const sample of f.verify_samples || []) {
                    lines.push(`- \`${sample}\``);
                }
            }
        }
    }

    if ((ctx.decisions || []).length > 0) {
        lines.push("");
        lines.push("## Decisions");
        for (const d of ctx.decisions) {
            lines.push("");
            lines.push(`### ${d.name}`);
            lines.push("");
            lines.push(`**Date:** ${d.date}  `);
            if (d.reason_prefix) lines.push(`**Reason:** ${d.reason_prefix}`);
            if ((d.impacts || []).length > 0) {
                lines.push(`**Impacts:** ${d.impacts.map((x) => `\`${x}\``).join(", ")}`);
            }
        }
    }

    lines.push("");
    return lines.join("\n");
}

function triggerDownload(filename, content, mime) {
    const blob = new Blob([content], { type: `${mime};charset=utf-8` });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(() => URL.revokeObjectURL(url), 100);
}

function downloadContextMarkdown(ctx) {
    triggerDownload(
        `${ctx.module_name || "context"}.md`,
        contextToMarkdown(ctx),
        "text/markdown"
    );
}

function downloadJsonl(bundles, filename) {
    // JSON Lines format — one canonical bundle per line. Matches
    // `aver <cmd> --json` output shape; consumers can pipe to `jq`.
    const arr = Array.isArray(bundles) ? bundles : [bundles];
    const content = arr.map((b) => JSON.stringify(b)).join("\n") + "\n";
    triggerDownload(filename, content, "application/x-ndjson");
}

// ── Minimal uncompressed ZIP writer ───────────────────────────────
// Avoids pulling JSZip (~100 KiB) from a CDN — the playground project
// is a handful of tiny .av files, "store" compression is plenty.
// Spec: APPNOTE.TXT — Local File Header + Central Directory + EOCD.
const ZIP_CRC_TABLE = (() => {
    const t = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
        let c = n;
        for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
        t[n] = c;
    }
    return t;
})();
function zipCrc32(bytes) {
    let c = 0xffffffff;
    for (let i = 0; i < bytes.length; i++) {
        c = ZIP_CRC_TABLE[(c ^ bytes[i]) & 0xff] ^ (c >>> 8);
    }
    return (c ^ 0xffffffff) >>> 0;
}
function buildZip(entries) {
    const u16 = (arr, o, v) => { arr[o] = v & 0xff; arr[o + 1] = (v >> 8) & 0xff; };
    const u32 = (arr, o, v) => {
        arr[o] = v & 0xff;
        arr[o + 1] = (v >>> 8) & 0xff;
        arr[o + 2] = (v >>> 16) & 0xff;
        arr[o + 3] = (v >>> 24) & 0xff;
    };
    const enc = new TextEncoder();
    const parts = [];
    const central = [];
    let offset = 0;
    for (const e of entries) {
        const name = enc.encode(e.name);
        const data = e.data instanceof Uint8Array ? e.data : enc.encode(e.data);
        const crc = zipCrc32(data);
        const size = data.length;

        const lfh = new Uint8Array(30 + name.length);
        u32(lfh, 0, 0x04034b50);    // local file header sig
        u16(lfh, 4, 20);            // version needed
        u16(lfh, 6, 0);             // flags
        u16(lfh, 8, 0);             // compression: store
        u16(lfh, 10, 0);            // mod time
        u16(lfh, 12, 0x21);         // mod date (1980-01-01)
        u32(lfh, 14, crc);
        u32(lfh, 18, size);         // compressed
        u32(lfh, 22, size);         // uncompressed
        u16(lfh, 26, name.length);
        u16(lfh, 28, 0);            // extra len
        lfh.set(name, 30);
        parts.push(lfh, data);

        const cdh = new Uint8Array(46 + name.length);
        u32(cdh, 0, 0x02014b50);    // central dir sig
        u16(cdh, 4, 20);            // version made by
        u16(cdh, 6, 20);            // version needed
        u16(cdh, 8, 0);             // flags
        u16(cdh, 10, 0);            // compression
        u16(cdh, 12, 0);
        u16(cdh, 14, 0x21);
        u32(cdh, 16, crc);
        u32(cdh, 20, size);
        u32(cdh, 24, size);
        u16(cdh, 28, name.length);
        u16(cdh, 30, 0);            // extra
        u16(cdh, 32, 0);            // comment
        u16(cdh, 34, 0);            // disk start
        u16(cdh, 36, 0);            // int attrs
        u32(cdh, 38, 0);            // ext attrs
        u32(cdh, 42, offset);
        cdh.set(name, 46);
        central.push(cdh);

        offset += lfh.length + size;
    }
    const cdStart = offset;
    const cdSize = central.reduce((s, c) => s + c.length, 0);
    const eocd = new Uint8Array(22);
    u32(eocd, 0, 0x06054b50);
    u16(eocd, 4, 0);            // disk number
    u16(eocd, 6, 0);            // central dir disk
    u16(eocd, 8, entries.length);
    u16(eocd, 10, entries.length);
    u32(eocd, 12, cdSize);
    u32(eocd, 16, cdStart);
    u16(eocd, 20, 0);           // comment len
    return new Blob([...parts, ...central, eocd], { type: "application/zip" });
}

// Download the virtual fs — single .av when there's one file, a
// "store"-zip otherwise. Game forks come back as a proper multi-
// file project the user can drop straight into `aver compile`.
function downloadProject() {
    if (state.wasmOnly && state.wasmBytes) {
        const blob = new Blob([state.wasmBytes], { type: "application/wasm" });
        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = state.wasmName || "program.wasm";
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(() => URL.revokeObjectURL(url), 100);
        return;
    }
    if (state.files.size === 0) {
        setStatus("Nothing to download.", "info");
        return;
    }
    const projectName = state.activeGame
        || (state.activeFile ? state.activeFile.replace(/\.av$/, "") : "playground");
    if (state.files.size === 1) {
        const [name, source] = state.files.entries().next().value;
        triggerDownload(name, source, "text/plain");
        setStatus(`Downloaded ${name}.`, "success");
        return;
    }
    const entries = Array.from(state.files, ([name, data]) => ({ name, data }));
    const blob = buildZip(entries);
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `${projectName}.zip`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(() => URL.revokeObjectURL(url), 100);
    setStatus(`Downloaded ${projectName}.zip (${state.files.size} files).`, "success");
}

document.querySelector("[data-download]")?.addEventListener("click", downloadProject);

// ── Record / Replay ─────────────────────────────────────────────────
// Routes through the VM-in-wasm: Record captures every effect call
// made by fn main(), Replay verifies that a second run reproduces the
// same trace effect-for-effect. Same semantics as the CLI's
// `aver run --record` / `aver replay --test`, minus disk IO.

document.querySelector("[data-record]")?.addEventListener("click", async () => {
    if (state.wasmOnly) {
        setStatus("No source — drop .av or a folder first.", "error");
        return;
    }
    const entry = pickEntryFile();
    const filesObj = state.files.size > 0
        ? Object.fromEntries(state.files)
        : { "playground.av": getActiveSource() };
    if (!entry || !filesObj[entry]?.trim()) {
        setStatus("Nothing to record.", "error");
        return;
    }
    try {
        const comp = await loadCompiler();
        setStatus("Recording…", "info");
        const json = comp.aver_run_record(JSON.stringify(filesObj), entry);
        const res = JSON.parse(json);
        if (!res.ok) {
            setStatus("Record failed", "error");
            appendConsole("stderr", res.error || "unknown error");
            return;
        }
        state.lastRecording = res.recording;
        const base = (entry.replace(/\.av$/, "") || "playground");
        triggerDownload(`${base}.replay.json`, res.recording, "application/json");
        const note = res.runtime_error
            ? ` (main threw: ${res.runtime_error})`
            : "";
        setStatus(`Recorded ${res.effect_count} effect(s) → ${base}.replay.json${note}`, "success");
    } catch (e) {
        appendConsole("stderr", e.message || String(e));
        setStatus("Record failed", "error");
    }
});

document.querySelector("[data-replay]")?.addEventListener("click", async () => {
    if (state.wasmOnly) {
        setStatus("No source — drop .av or a folder first.", "error");
        return;
    }
    if (!state.lastRecording) {
        setStatus("Drop a .replay.json first (or hit 🔴 Record).", "error");
        return;
    }
    const entry = pickEntryFile();
    const filesObj = state.files.size > 0
        ? Object.fromEntries(state.files)
        : { "playground.av": getActiveSource() };
    if (!entry || !filesObj[entry]?.trim()) {
        setStatus("Nothing to replay against.", "error");
        return;
    }
    try {
        const comp = await loadCompiler();
        setStatus("Replaying…", "info");
        const json = comp.aver_replay_run(
            JSON.stringify(filesObj),
            entry,
            state.lastRecording
        );
        const res = JSON.parse(json);
        if (!res.ok) {
            setStatus("Replay failed", "error");
            appendConsole("stderr", res.error || "unknown error");
            return;
        }
        if (res.matched) {
            setStatus(`Replay matched · ${res.replayed}/${res.total} effects`, "success");
        } else {
            setStatus(
                `Replay diverged · ${res.replayed}/${res.total}${res.error ? " · " + res.error : ""}`,
                "error"
            );
            if (res.error) appendConsole("stderr", res.error);
        }
    } catch (e) {
        appendConsole("stderr", e.message || String(e));
        setStatus("Replay failed", "error");
    }
});

function makeDownloadButton(label, title, onClick) {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "download-btn";
    btn.textContent = label;
    btn.title = title;
    btn.addEventListener("click", onClick);
    return btn;
}

function renderContextPanel(ctx) {
    const panel = document.createElement("div");
    panel.className = "context-panel";

    // Header row: title + download button
    const headerRow = document.createElement("div");
    headerRow.className = "ctx-header-row";
    const title = document.createElement("h2");
    title.textContent = ctx.module_name
        ? `module ${ctx.module_name}`
        : "(no module declaration)";
    headerRow.appendChild(title);
    const dlBtn = document.createElement("button");
    dlBtn.className = "ctx-download-btn";
    dlBtn.type = "button";
    dlBtn.textContent = "⬇ Download as .md";
    dlBtn.title = "Export this context as a Markdown file — same shape as `aver context --output ctx.md`";
    dlBtn.addEventListener("click", () => downloadContextMarkdown(ctx));
    headerRow.appendChild(dlBtn);
    panel.appendChild(headerRow);

    if (ctx.intent) {
        const bq = document.createElement("blockquote");
        bq.textContent = ctx.intent.replace(/\n/g, " ");
        panel.appendChild(bq);
    }

    const metaLine = (label, items, joiner = ", ", codeWrap = false) => {
        if (!items || items.length === 0) return;
        const row = document.createElement("div");
        row.className = "ctx-meta";
        const lbl = document.createElement("strong");
        lbl.textContent = `${label}: `;
        row.appendChild(lbl);
        items.forEach((item, idx) => {
            if (idx > 0) row.appendChild(document.createTextNode(joiner));
            if (codeWrap) {
                const c = document.createElement("code");
                c.textContent = item;
                row.appendChild(c);
            } else {
                row.appendChild(document.createTextNode(item));
            }
        });
        panel.appendChild(row);
    };
    metaLine("depends", ctx.depends);
    metaLine("exposes", ctx.exposes, ", ", true);
    metaLine("exposes opaque", ctx.exposes_opaque, ", ", true);
    metaLine("api effects", ctx.api_effects, ", ", true);
    if (ctx.main_effects) {
        metaLine("main effects", ctx.main_effects, ", ", true);
    }

    if ((ctx.types || []).length > 0) {
        const h = document.createElement("h3");
        h.textContent = "Types";
        panel.appendChild(h);
        for (const t of ctx.types) {
            const row = document.createElement("div");
            row.className = "ctx-type";
            const kind = document.createElement("span");
            kind.className = "ctx-type-kind";
            kind.textContent = t.kind;
            row.appendChild(kind);
            row.appendChild(document.createTextNode(t.name));
            if (t.fields_or_variants && t.fields_or_variants.length > 0) {
                row.appendChild(
                    document.createTextNode(` (${t.fields_or_variants.join(", ")})`)
                );
            }
            panel.appendChild(row);
        }
    }

    if ((ctx.functions || []).length > 0) {
        const h = document.createElement("h3");
        h.textContent = "Functions";
        panel.appendChild(h);
        for (const f of ctx.functions) {
            const card = document.createElement("div");
            card.className = "ctx-fn" + (f.is_exposed ? " exposed" : "");
            const sig = document.createElement("div");
            sig.className = "ctx-fn-sig";
            sig.textContent = f.signature;
            card.appendChild(sig);
            if (f.description) {
                const desc = document.createElement("div");
                desc.className = "ctx-fn-desc";
                desc.textContent = `? "${f.description}"`;
                card.appendChild(desc);
            }
            // Signature now carries only params + return type (effects
            // stripped in Rust render_signature), so effects get their
            // own tag row here. Keeps the signature readable and makes
            // the effect surface scannable at a glance.
            const tags = document.createElement("div");
            tags.className = "ctx-tags";
            const effects = f.effects || [];
            if (!f.is_exposed) {
                const t = document.createElement("span");
                t.className = "ctx-tag";
                t.textContent = "private";
                tags.appendChild(t);
            }
            for (const q of f.qualifiers || []) {
                const t = document.createElement("span");
                t.className = "ctx-tag" + (q === "PURE" ? " pure" : "");
                t.textContent = q;
                tags.appendChild(t);
            }
            if (f.auto_memo) {
                const t = document.createElement("span");
                t.className = "ctx-tag pure";
                t.textContent = "AUTO_MEMO";
                tags.appendChild(t);
            }
            if (f.auto_tco) {
                const t = document.createElement("span");
                t.className = "ctx-tag pure";
                t.textContent = "AUTO_TCO";
                tags.appendChild(t);
            }
            // Empty effects == PURE (which qualifiers already carries),
            // so don't double-tag it. Only render explicit effects.
            for (const e of effects) {
                const t = document.createElement("span");
                t.className = "ctx-tag effect";
                t.textContent = `! ${e}`;
                tags.appendChild(t);
            }
            card.appendChild(tags);
            if (f.verify_count > 0) {
                const v = document.createElement("div");
                v.className = "ctx-verify";
                v.textContent = `verify: ${f.verify_count} case(s)`;
                card.appendChild(v);
                for (const sample of f.verify_samples || []) {
                    const s = document.createElement("div");
                    s.className = "ctx-verify-sample";
                    s.textContent = sample;
                    card.appendChild(s);
                }
            }
            panel.appendChild(card);
        }
    }

    if ((ctx.decisions || []).length > 0) {
        const h = document.createElement("h3");
        h.textContent = "Decisions";
        panel.appendChild(h);
        for (const d of ctx.decisions) {
            const card = document.createElement("div");
            card.className = "ctx-decision";
            const head = document.createElement("div");
            const name = document.createElement("span");
            name.className = "ctx-decision-name";
            name.textContent = d.name;
            const date = document.createElement("span");
            date.className = "ctx-decision-date";
            date.textContent = ` (${d.date})`;
            head.appendChild(name);
            head.appendChild(date);
            card.appendChild(head);
            if (d.reason_prefix) {
                const reason = document.createElement("div");
                reason.className = "ctx-decision-reason";
                reason.textContent = d.reason_prefix;
                card.appendChild(reason);
            }
            if ((d.impacts || []).length > 0) {
                const impacts = document.createElement("div");
                impacts.className = "ctx-meta";
                impacts.textContent = `impacts: ${d.impacts.join(", ")}`;
                card.appendChild(impacts);
            }
            panel.appendChild(card);
        }
    }

    const hasAnyBody =
        (ctx.types || []).length > 0 ||
        (ctx.functions || []).length > 0 ||
        (ctx.decisions || []).length > 0;
    if (!hasAnyBody) {
        const empty = document.createElement("div");
        empty.className = "ctx-empty";
        empty.textContent =
            "(no types, functions, or decisions — add some and run Context again)";
        panel.appendChild(empty);
    }
    return panel;
}

// Audit panel — structured DOM with collapsible sections and
// per-section re-run / fix actions that update only their own slice.
const auditBtn = document.querySelector("[data-audit]");

function parseAuditBundle(json) {
    const bundle = JSON.parse(json);
    const diagnostics = bundle.diagnostics || [];
    const verifyFailSlugs = new Set([
        "verify-mismatch", "verify-runtime-error", "verify-unexpected-err",
    ]);
    const formatSlugs = new Set(["needs-format"]);
    const verifyFailures = diagnostics.filter((d) => verifyFailSlugs.has(d.slug));
    const formatIssues = diagnostics.filter((d) => formatSlugs.has(d.slug));
    const staticIssues = diagnostics.filter(
        (d) => !verifyFailSlugs.has(d.slug) && !formatSlugs.has(d.slug)
    );
    return {
        verifySummary: bundle.verify_summary,
        staticIssues,
        verifyFailures,
        formatIssues,
    };
}

function renderDiag(container, d) {
    const isErr = d.severity === "error" || d.severity === "fail";
    const cls = isErr ? "diag-err" : "diag-warn";
    const head = document.createElement("div");
    head.className = `diag-line ${cls}`;
    head.textContent = `${d.severity}[${d.slug}]: ${d.summary}`;
    container.appendChild(head);
    const meta = document.createElement("div");
    meta.className = "diag-line diag-meta";
    meta.textContent = `at: ${d.span?.file || "playground"}:${d.span?.line || 0}:${d.span?.col || 0}`;
    container.appendChild(meta);
    if (d.repair?.primary) {
        const rep = document.createElement("div");
        rep.className = "diag-line diag-repair";
        rep.textContent = `repair: ${d.repair.primary}`;
        container.appendChild(rep);
    }
}

function buildSection({ key, label, status, statusText, countText, teaching, issues, actions, extras }) {
    const section = document.createElement("details");
    section.className = "audit-section";
    section.dataset.section = key;
    if ((issues && issues.length > 0) || status === "fail") section.open = true;

    const summary = document.createElement("summary");
    summary.innerHTML = `
        <span class="status-${status}">${statusText}</span>
        <span class="section-label">${label}</span>
        <span class="status-count">${countText ?? ""}</span>
    `;
    for (const a of actions || []) {
        const btn = document.createElement("button");
        btn.className = "rerun-btn";
        btn.textContent = a.label;
        btn.addEventListener("click", (e) => {
            e.stopPropagation();
            e.preventDefault();
            a.fn();
        });
        summary.appendChild(btn);
    }
    section.appendChild(summary);

    const body = document.createElement("div");
    body.className = "section-body";
    if (teaching) {
        const t = document.createElement("div");
        t.className = "teach";
        t.textContent = teaching;
        body.appendChild(t);
    }
    if (extras) extras(body);
    if (issues && issues.length > 0) {
        for (const d of issues) renderDiag(body, d);
    } else if (!extras) {
        const ok = document.createElement("div");
        ok.className = "diag-line diag-ok";
        ok.textContent = "✓ clean";
        body.appendChild(ok);
    }
    section.appendChild(body);
    return section;
}

function renderStaticSection(data, actions) {
    const { staticIssues } = data;
    const staticErrors = staticIssues.filter(
        (d) => d.severity === "error" || d.severity === "fail"
    ).length;
    const staticWarnings = staticIssues.length - staticErrors;
    return buildSection({
        key: "static",
        label: "Static Checks — types, contracts, perf, naming",
        status: staticErrors > 0 ? "fail" : (staticWarnings > 0 ? "warn" : "pass"),
        statusText: staticErrors > 0 ? "✗" : (staticWarnings > 0 ? "!" : "✓"),
        countText: staticIssues.length === 0
            ? "no issues"
            : `${staticErrors} error · ${staticWarnings} warn`,
        teaching: "What `aver check` runs — the static lint layer. Catches code smells, type errors, effect mismatches, naming convention breaks before the program ever executes.",
        issues: staticIssues,
        actions,
    });
}

function renderVerifySection(data, actions) {
    const { verifyFailures, verifySummary } = data;
    const verifyFailed = verifyFailures.length;
    const verifyPassed = (verifySummary?.blocks || [])
        .reduce((acc, b) => acc + b.passed, 0);
    const verifyTotal = (verifySummary?.blocks || [])
        .reduce((acc, b) => acc + b.total, 0);
    return buildSection({
        key: "verify",
        label: "Verify — executed contract checks",
        status: verifyFailed > 0 ? "fail" : (verifyTotal > 0 ? "pass" : "warn"),
        statusText: verifyFailed > 0 ? "✗" : (verifyTotal > 0 ? "✓" : "—"),
        countText: verifyTotal > 0 ? `${verifyPassed}/${verifyTotal} passed` : "no verify blocks",
        teaching: "Each `verify` block's cases run in the VM. Pass rate tells you how well your function matches the spec you wrote alongside it. Verify is Aver's core contract.",
        issues: verifyFailures,
        actions,
        extras: (body) => {
            if (!verifySummary?.blocks?.length) {
                const none = document.createElement("div");
                none.className = "diag-line diag-meta";
                none.textContent = "(no verify blocks in this file)";
                body.appendChild(none);
                return;
            }
            for (const b of verifySummary.blocks) {
                const line = document.createElement("div");
                const cls = b.failed > 0 ? "diag-err" : "diag-ok";
                line.className = `diag-line ${cls}`;
                const tag = b.failed > 0 ? "✗" : "✓";
                const breakdown = b.skipped > 0
                    ? `${b.passed}/${b.total} passed, ${b.skipped} skipped`
                    : `${b.passed}/${b.total} passed`;
                line.textContent = `${tag} ${b.name}  ${breakdown}`;
                body.appendChild(line);
            }
        },
    });
}

function renderFormatSection(data, actions) {
    const { formatIssues } = data;
    const needsFormat = formatIssues.length > 0;
    return buildSection({
        key: "format",
        label: "Format — canonical Aver shape",
        status: needsFormat ? "warn" : "pass",
        statusText: needsFormat ? "!" : "✓",
        countText: needsFormat ? "needs format" : "clean",
        teaching: "Mechanical rewrites: indent, effect order, verify placement, blank runs. Re-run re-checks; Fix applies the formatter to your editor.",
        issues: formatIssues,
        actions,
    });
}

// In-place section replacement — re-runs audit under the hood, swaps
// just the targeted section's DOM node while preserving the rest of
// the panel (open/closed states on sibling sections stay intact).
async function rerunAuditSection(key) {
    if (state.wasmOnly) return;
    const source = getActiveSource();
    if (!source?.trim()) return;
    try {
        const comp = await loadCompiler();
        setStatus(`Re-running ${key}…`, "info");
        const json = runAnalysis(comp, "audit", source);
        const data = parseAuditBundle(json);
        const old = document.querySelector(`.audit-section[data-section="${key}"]`);
        if (!old) return;
        let replacement;
        if (key === "static") {
            replacement = renderStaticSection(data, sectionActions("static"));
        } else if (key === "verify") {
            replacement = renderVerifySection(data, sectionActions("verify"));
        } else if (key === "format") {
            replacement = renderFormatSection(data, sectionActions("format"));
        }
        // Preserve prior open/closed state on the section being replaced.
        if (replacement) {
            replacement.open = old.open;
            old.replaceWith(replacement);
        }
        updateAuditStatus(data);
    } catch (e) {
        appendConsole("stderr", e.message || String(e));
        setStatus(`${key} failed`, "error");
    }
}

async function applyFormatFix() {
    if (state.wasmOnly) return;
    // Format every tab in the virtual fs, not just the active one —
    // the audit panel summarises violations across the project, so
    // ⚡ Fix formatting should flatten all of them in one go. Ends
    // on whichever tab actually changed (entry first) so the user
    // sees the diff immediately.
    const targets = state.files.size > 0
        ? Array.from(state.files.entries())
        : [[getActiveName() || "playground.av", getActiveSource()]];
    if (targets.every(([, src]) => !src?.trim())) return;
    try {
        const comp = await loadCompiler();
        setStatus("Formatting…", "info");
        const changed = [];
        for (const [path, src] of targets) {
            if (!src?.trim()) continue;
            const rewritten = comp.aver_format(src);
            if (rewritten !== src) {
                if (state.files.has(path)) state.files.set(path, rewritten);
                changed.push(path);
            }
        }
        if (changed.length === 0) {
            setStatus("Already formatted", "success");
        } else {
            // Prefer showing the entry file if it changed, else the
            // first changed file. setActiveFile syncs codeEditor +
            // highlight in one call.
            const entry = pickEntryFile();
            const focus = changed.includes(entry) ? entry : changed[0];
            if (state.files.has(focus)) setActiveFile(focus);
            else {
                codeEditor.value = state.files.get(focus) || "";
                updateHighlight();
            }
            setStatus(
                changed.length === 1 ? `Formatted ${changed[0]}` : `Formatted ${changed.length} files`,
                "success"
            );
        }
        await rerunAuditSection("format");
    } catch (e) {
        appendConsole("stderr", e.message || String(e));
        setStatus("Format failed", "error");
    }
}

function sectionActions(key) {
    if (key === "format") {
        return [
            { label: "↻ re-run", fn: () => rerunAuditSection("format") },
            { label: "⚡ Fix formatting", fn: applyFormatFix },
        ];
    }
    return [{ label: "↻ re-run", fn: () => rerunAuditSection(key) }];
}

function updateAuditStatus(data) {
    const { staticIssues, verifyFailures, verifySummary, formatIssues } = data;
    const staticErrors = staticIssues.filter(
        (d) => d.severity === "error" || d.severity === "fail"
    ).length;
    const staticWarnings = staticIssues.length - staticErrors;
    const verifyFailed = verifyFailures.length;
    const verifyPassed = (verifySummary?.blocks || [])
        .reduce((acc, b) => acc + b.passed, 0);
    const verifyTotal = (verifySummary?.blocks || [])
        .reduce((acc, b) => acc + b.total, 0);
    const needsFormat = formatIssues.length > 0;
    const parts = [];
    if (staticErrors > 0) parts.push(`${staticErrors} error(s)`);
    if (staticWarnings > 0) parts.push(`${staticWarnings} warning(s)`);
    if (verifyTotal > 0) parts.push(`verify ${verifyPassed}/${verifyTotal}`);
    if (needsFormat) parts.push("needs format");
    const allClean = staticErrors === 0 && staticWarnings === 0
        && verifyFailed === 0 && !needsFormat;
    setStatus(
        allClean ? "Audit: clean" : `Audit: ${parts.join(", ")}`,
        allClean ? "success" : (staticErrors > 0 || verifyFailed > 0 ? "error" : "info")
    );
}

if (auditBtn) {
    auditBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        setOutputMode("console");
        state.activeGame = null;
        clearOutput();
        clearCompileMeta();

        try {
            const comp = await loadCompiler();
            setStatus("Auditing…", "info");
            const json = runAnalysis(comp, "audit", source);
            const data = parseAuditBundle(json);

            const panel = document.createElement("div");
            panel.className = "audit-panel";

            const header = document.createElement("div");
            header.className = "audit-header-row";
            const title = document.createElement("span");
            title.className = "title";
            title.textContent = "Audit — 3-axis health report";
            header.appendChild(title);
            const bundle = JSON.parse(json);
            header.appendChild(makeDownloadButton(
                "⬇ Download as .jsonl",
                "Save the audit bundle as JSON Lines — same shape as `aver audit --json`, pipe-friendly with jq",
                () => downloadJsonl(bundle, "audit.jsonl")
            ));
            panel.appendChild(header);

            panel.appendChild(renderStaticSection(data, sectionActions("static")));
            panel.appendChild(renderVerifySection(data, sectionActions("verify")));
            panel.appendChild(renderFormatSection(data, sectionActions("format")));
            dom.console.appendChild(panel);

            updateAuditStatus(data);
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Audit failed", "error");
        }
    });
}

// Format button — rewrite editor content to canonical form.
// Non-destructive on parse error: falls back to original text.
const formatBtn = document.querySelector("[data-format]");
if (formatBtn) {
    formatBtn.addEventListener("click", async () => {
        if (state.wasmOnly) {
            setStatus("No source — drop .av or folder first.", "error");
            return;
        }
        const source = getActiveSource();
        if (!source?.trim()) return;

        try {
            const comp = await loadCompiler();
            setStatus("Formatting…", "info");
            const rewritten = comp.aver_format(source);
            if (rewritten === source) {
                setStatus("Already formatted", "success");
                return;
            }
            codeEditor.value = rewritten;
            if (state.activeFile) state.files.set(state.activeFile, rewritten);
            updateHighlight();
            setStatus("Formatted", "success");
        } catch (e) {
            appendConsole("stderr", e.message || String(e));
            setStatus("Format failed", "error");
        }
    });
}

// ---------------------------------------------------------------------------
// Game source viewer
// ---------------------------------------------------------------------------

const GAME_SOURCES = {
    life:     ["examples/games/life.av"],
    snake:    ["examples/games/snake.av"],
    wumpus:   ["examples/games/wumpus.av"],
    tetris:   ["examples/games/tetris/pieces.av", "examples/games/tetris/board.av", "examples/games/tetris/logic.av", "examples/games/tetris/main.av"],
    checkers: ["examples/games/checkers/board.av", "examples/games/checkers/rules.av", "examples/games/checkers/ai.av", "examples/games/checkers/render.av", "examples/games/checkers/main.av"],
    rogue:    ["examples/games/rogue/types.av", "examples/games/rogue/map.av", "examples/games/rogue/fov.av", "examples/games/rogue/combat.av", "examples/games/rogue/pathfinding.av", "examples/games/rogue/render.av", "examples/games/rogue/main.av"],
    doom:     ["examples/games/doom/types.av", "examples/games/doom/math.av", "examples/games/doom/rng.av", "examples/games/doom/level.av", "examples/games/doom/enemy.av", "examples/games/doom/render.av", "examples/games/doom/main.av"],
};

const PLAYGROUND_SOURCES_ROOT = "./sources/";
const sourceCache = {};

async function fetchSource(path) {
    if (sourceCache[path]) return sourceCache[path];
    try {
        const resp = await fetch(PLAYGROUND_SOURCES_ROOT + path);
        if (!resp.ok) return `// Failed to load bundled source: ${path}`;
        const text = await resp.text();
        sourceCache[path] = text;
        return text;
    } catch (_) {
        return `// Failed to load bundled source: ${path}`;
    }
}

// Load a game's bundled .av files straight into the multi-file
// editor. Unifies the game-preview flow with the normal edit flow —
// every file is a real tab (editable), the prebuilt .wasm still
// powers ▶ Run so play is instant.
async function loadGameSourcesAsTabs(gameName) {
    const paths = GAME_SOURCES[gameName];
    if (!paths) return;
    leaveWasmOnlyMode();
    state.files.clear();
    state.activeFile = null;
    const sources = await Promise.all(paths.map(fetchSource));
    let firstPath = null;
    paths.forEach((path, i) => {
        const name = path.split("/").pop();
        state.files.set(name, sources[i]);
        if (!firstPath || /^main\.av$/i.test(name)) firstPath = name;
    });
    if (firstPath) setActiveFile(firstPath);
    // Snapshot the pristine source so edit-vs-prebuilt reconciliation
    // stays simple: any divergence drops state.wasmBytes and the next
    // Run recompiles the active tab.
    state.pristineFiles = new Map(state.files);
}

// ---------------------------------------------------------------------------
// Per-game touch controls
// ---------------------------------------------------------------------------
// Each game declares its own button groups. "grid:arrows" and "grid:wasd"
// are special layout types; everything else is a flow layout.

const GAME_TOUCH = {
    life: [
        { label: "Cursor", layout: "grid:arrows", keys: [
            { key: "up", text: "↑" }, { key: "left", text: "←" },
            { key: "down", text: "↓" }, { key: "right", text: "→" },
        ]},
        { label: "Edit", keys: [
            { key: " ", text: "Spc" }, { key: "enter", text: "Go" },
            { key: "1", text: "1" }, { key: "2", text: "2" },
            { key: "3", text: "3" }, { key: "c", text: "C" },
        ]},
        { label: "Sim", keys: [
            { key: "+", text: "+" }, { key: "-", text: "-" },
            { key: "0", text: "0" }, { key: "e", text: "E" },
            { key: "r", text: "R" }, { key: "q", text: "Q" },
        ]},
    ],
    snake: [
        { label: "Direction", layout: "grid:arrows", keys: [
            { key: "up", text: "↑" }, { key: "left", text: "←" },
            { key: "down", text: "↓" }, { key: "right", text: "→" },
        ]},
        { label: "", keys: [
            { key: "q", text: "Q" },
        ]},
    ],
    tetris: [
        { label: "Move", layout: "grid:arrows", keys: [
            { key: "up", text: "↑" }, { key: "left", text: "←" },
            { key: "down", text: "↓" }, { key: "right", text: "→" },
        ]},
        { label: "", keys: [
            { key: " ", text: "Spc" }, { key: "q", text: "Q" },
        ]},
    ],
    checkers: [
        { label: "Cursor", layout: "grid:arrows", keys: [
            { key: "up", text: "↑" }, { key: "left", text: "←" },
            { key: "down", text: "↓" }, { key: "right", text: "→" },
        ]},
        { label: "Play", keys: [
            { key: "enter", text: "↵" }, { key: "esc", text: "Esc" },
            { key: "q", text: "Q" },
        ]},
        { label: "AI", keys: [
            { key: "+", text: "+" }, { key: "-", text: "-" },
        ]},
    ],
    rogue: [
        { label: "Move", layout: "grid:arrows", keys: [
            { key: "up", text: "↑" }, { key: "left", text: "←" },
            { key: "down", text: "↓" }, { key: "right", text: "→" },
        ]},
        { label: "", keys: [
            { key: ">", text: ">" }, { key: "q", text: "Quit" },
        ]},
    ],
    doom: [
        { label: "Move", layout: "grid:wasd", keys: [
            { key: "q", text: "Q" }, { key: "w", text: "W" }, { key: "e", text: "E" },
            { key: "a", text: "A" }, { key: "s", text: "S" }, { key: "d", text: "D" },
        ]},
        { label: "Turn", keys: [
            { key: "left", text: "←" }, { key: "right", text: "→" },
        ]},
        { label: "", keys: [
            { key: " ", text: "Spc" }, { key: "esc", text: "Esc" },
        ]},
    ],
    // wumpus: console game, no touch controls
};

function buildTouchControls(gameName) {
    const container = document.querySelector("[data-touch-controls]");
    if (!container) return;
    container.innerHTML = "";
    const config = GAME_TOUCH[gameName];
    if (!config) {
        container.style.display = "none";
        return;
    }
    container.style.display = "flex";
    for (const group of config) {
        const div = document.createElement("div");
        div.className = "touch-group";
        if (group.label) {
            const lbl = document.createElement("div");
            lbl.className = "touch-label";
            lbl.textContent = group.label;
            div.appendChild(lbl);
        }
        const wrap = document.createElement("div");
        if (group.layout === "grid:arrows") {
            wrap.className = "touch-grid arrows";
        } else if (group.layout === "grid:wasd") {
            wrap.className = "touch-grid wasd";
        } else {
            wrap.className = "touch-actions";
        }
        for (const k of group.keys) {
            const btn = document.createElement("button");
            btn.dataset.key = k.key;
            btn.textContent = k.text;
            btn.addEventListener("click", (e) => {
                e.preventDefault();
                dispatchRuntimeKey(k.key);
            });
            wrap.appendChild(btn);
        }
        div.appendChild(wrap);
        container.appendChild(div);
    }
}

// About overlay
document.querySelector("[data-show-about]")?.addEventListener("click", (e) => {
    e.preventDefault();
    const about = document.querySelector("[data-about]");
    if (about) about.style.display = "flex";
});
document.querySelector("[data-close-about]")?.addEventListener("click", () => {
    const about = document.querySelector("[data-about]");
    if (about) about.style.display = "none";
});
document.querySelector("[data-about]")?.addEventListener("click", (e) => {
    if (e.target === e.currentTarget) e.currentTarget.style.display = "none";
});

// WASM overlay
document.querySelector("[data-show-wasm]")?.addEventListener("click", (e) => {
    e.preventDefault();
    const el = document.querySelector("[data-wasm-overlay]");
    if (el) el.style.display = "flex";
});
document.querySelector("[data-close-wasm]")?.addEventListener("click", () => {
    const el = document.querySelector("[data-wasm-overlay]");
    if (el) el.style.display = "none";
});
document.querySelector("[data-wasm-overlay]")?.addEventListener("click", (e) => {
    if (e.target === e.currentTarget) e.currentTarget.style.display = "none";
});

// Auto-launch game from ?game= URL parameter
const urlGame = new URLSearchParams(window.location.search).get("game");
if (urlGame) {
    const btn = document.querySelector(`[data-game="${urlGame}"]`);
    if (btn) btn.click();
}

// Auto-load starter from ?example= URL parameter. Shareable link that
// drops the user straight into a trust-loop demo.
const urlExample = new URLSearchParams(window.location.search).get("example");
if (urlExample && EXAMPLES[urlExample] && codeEditor) {
    loadExampleAsTab(urlExample);
    if (examplesSelect) examplesSelect.value = urlExample;
}

// Draggable divider between editor pane and output pane.
// Same UX as the old game-in-divider; now lives at workspace level
// so it works in every mode (edit, game, dropped folder).
{
    const divider = document.querySelector("[data-workspace-divider]");
    const layout = document.querySelector(".workspace");
    const editor = document.querySelector(".editor-pane");
    if (divider && layout && editor) {
        let dragging = false;
        divider.addEventListener("mousedown", (e) => {
            e.preventDefault();
            dragging = true;
            divider.classList.add("dragging");
        });
        divider.addEventListener("touchstart", (e) => {
            dragging = true;
            divider.classList.add("dragging");
        });
        const onMove = (clientX) => {
            if (!dragging) return;
            const rect = layout.getBoundingClientRect();
            const pct = ((clientX - rect.left) / rect.width) * 100;
            const clamped = Math.min(Math.max(pct, 10), 80);
            editor.style.flex = "none";
            editor.style.width = clamped + "%";
        };
        document.addEventListener("mousemove", (e) => onMove(e.clientX));
        document.addEventListener("touchmove", (e) => {
            if (dragging && e.touches[0]) onMove(e.touches[0].clientX);
        });
        const onEnd = () => {
            dragging = false;
            divider.classList.remove("dragging");
        };
        document.addEventListener("mouseup", onEnd);
        document.addEventListener("touchend", onEnd);
    }
}
