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
            setRawMode(false);
            if (!message.ok && message.error) {
                appendConsole("stderr", message.error);
            }
            // If a game just ended, show back-to-editor option
            if (workspace && workspace.dataset.mode === "game") {
                setStatus("Game ended. Click ← Back or pick another.", "idle");
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
    // Switch to game mode (hide editor)
    document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
    setWorkspaceMode("game");
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
const workspace = document.querySelector(".workspace");

function setOutputMode(mode) {
    // mode: "console" | "terminal"
    if (outputPane) outputPane.dataset.mode = mode;
}

function setWorkspaceMode(mode) {
    // mode: "edit" | "game"
    if (workspace) workspace.dataset.mode = mode;
}

function backToEditor() {
    stopRun();
    state.programArgs = [];
    state.activeGame = null;
    const ws = document.querySelector(".workspace");
    if (ws) ws.dataset.showSource = "false";
    setWorkspaceMode("edit");
    setOutputMode("console");
    buildTouchControls(null);
    document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
    setStatus("Ready.", "success");
}

document.querySelector("[data-back]")?.addEventListener("click", backToEditor);

document.querySelectorAll("[data-game]").forEach(btn => {
    btn.addEventListener("click", async () => {
        const name = btn.dataset.game;
        const args = btn.dataset.args ? btn.dataset.args.split(" ") : [];
        document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
        btn.classList.add("active");
        state.programArgs = args;
        state.activeGame = name;
        const isConsoleGame = btn.hasAttribute("data-console-game");
        setWorkspaceMode("game");
        setOutputMode(isConsoleGame ? "console" : "terminal");
        buildTouchControls(name);
        clearOutput();
        const readlineBar = document.querySelector("[data-readline-bar]");
        if (readlineBar) readlineBar.style.display = isConsoleGame ? "flex" : "none";
        setStatus(`Loading ${name}…`, "info");

        try {
            const resp = await fetch(`./${name}.wasm`);
            if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
            const bytes = await resp.arrayBuffer();
            state.wasmBytes = bytes;
            state.wasmName = `${name}.wasm`;
            const sizeKiB = (bytes.byteLength / 1024).toFixed(1);
            dom.fileMeta.textContent = `${name}.wasm — ${sizeKiB} KiB`;
            dom.runButton.disabled = false;
            setStatus(`Native WASM · ${name}.wasm · ${sizeKiB} KiB`, "success");
            runSelectedModule(isConsoleGame ? undefined : { cols: 80, rows: 35 });
        } catch (e) {
            setStatus(`Failed to load ${name}: ${e.message}`, "error");
        }
    });
});

// ---------------------------------------------------------------------------
// Code editor + in-browser compile
// ---------------------------------------------------------------------------

const EXAMPLES = {
    hello: `fn main() -> Unit\n    ! [Console.print]\n    Console.print("Hello, World!")\n    Console.print("Hello from the Aver Playground!")`,
    calculator: `fn add(a: Int, b: Int) -> Int\n    a + b\n\nfn divide(a: Int, b: Int) -> Result<Int, String>\n    match b\n        0 -> Result.Err("Division by zero")\n        _ -> Result.Ok(a / b)\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(add(2, 3))\n    Console.print(divide(10, 2))\n    Console.print(divide(10, 0))\n\nverify add\n    add(1, 2) => 3\n    add(0, 0) => 0`,
    fibonacci: `fn fib(n: Int) -> Int\n    match n\n        0 -> 0\n        1 -> 1\n        _ -> fib(n - 1) + fib(n - 2)\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print("fib(10) = {fib(10)}")\n    Console.print("fib(20) = {fib(20)}")\n\nverify fib\n    fib(0) => 0\n    fib(1) => 1\n    fib(10) => 55`,
    shapes: `type Shape\n    Circle(Float)\n    Rectangle(Float, Float)\n\nfn area(shape: Shape) -> Float\n    match shape\n        Shape.Circle(r) -> 3.14159 * r * r\n        Shape.Rectangle(w, h) -> w * h\n\nfn main() -> Unit\n    ! [Console.print]\n    c = Shape.Circle(5.0)\n    r = Shape.Rectangle(3.0, 4.0)\n    Console.print("circle area = {Float.toString(area(c))}")\n    Console.print("rect area = {Float.toString(area(r))}")\n\nverify area\n    area(Shape.Circle(1.0)) => 3.14159\n    area(Shape.Rectangle(3.0, 4.0)) => 12.0`,
    quicksort: `fn filterLess(xs: List<Int>, pivot: Int) -> List<Int>\n    match xs\n        [] -> []\n        [h, ..t] -> match h < pivot\n            true -> List.prepend(h, filterLess(t, pivot))\n            false -> filterLess(t, pivot)\n\nfn filterGte(xs: List<Int>, pivot: Int) -> List<Int>\n    match xs\n        [] -> []\n        [h, ..t] -> match h >= pivot\n            true -> List.prepend(h, filterGte(t, pivot))\n            false -> filterGte(t, pivot)\n\nfn quicksort(xs: List<Int>) -> List<Int>\n    match xs\n        [] -> []\n        [pivot, ..rest] -> List.concat(List.concat(quicksort(filterLess(rest, pivot)), [pivot]), quicksort(filterGte(rest, pivot)))\n\nfn main() -> Unit\n    ! [Console.print]\n    input = [38, 27, 43, 3, 9, 82, 10]\n    Console.print("input:  {input}")\n    Console.print("sorted: {quicksort(input)}")\n\nverify quicksort\n    quicksort([]) => []\n    quicksort([3, 1, 2]) => [1, 2, 3]`,
    rle: `type RlePair\n    Pair(String, Int)\n\nfn encode(chars: List<String>, current: String, count: Int, acc: List<RlePair>) -> List<RlePair>\n    match chars\n        [] -> List.reverse(List.prepend(RlePair.Pair(current, count), acc))\n        [h, ..t] -> match h == current\n            true -> encode(t, current, count + 1, acc)\n            false -> encode(t, h, 1, List.prepend(RlePair.Pair(current, count), acc))\n\nfn rleEncode(input: String) -> List<RlePair>\n    chars = String.chars(input)\n    match chars\n        [] -> []\n        [first, ..rest] -> encode(rest, first, 1, [])\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(rleEncode("aaabbbccddddee"))`,
};

const codeEditor = document.querySelector("[data-code-editor]");
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

if (compileRunBtn) {
    compileRunBtn.addEventListener("click", async () => {
        const source = codeEditor.value;
        if (!source.trim()) return;

        document.querySelectorAll("[data-game]").forEach(b => b.classList.remove("active"));
        setWorkspaceMode("edit");
        state.activeGame = null;

        const usesTerminal = source.includes("Terminal.");
        setOutputMode(usesTerminal ? "terminal" : "console");
        clearOutput();
        // Show readline bar for console programs
        const readlineBar = document.querySelector("[data-readline-bar]");
        if (readlineBar) readlineBar.style.display = usesTerminal ? "none" : "flex";

        try {
            const comp = await loadCompiler();
            setStatus("Compiling…", "info");
            const t0 = performance.now();
            const wasmBytes = comp.aver_compile(source);
            const ms = (performance.now() - t0).toFixed(0);

            state.wasmBytes = wasmBytes.buffer;
            state.wasmName = "playground.wasm";
            dom.fileMeta.textContent = `Compiled in ${ms}ms — ${(wasmBytes.length / 1024).toFixed(1)} KB`;
            dom.runButton.disabled = false;
            runSelectedModule();
        } catch (e) {
            const msg = e.message || String(e);
            setStatus("Compile error", "error");
            appendConsole("stderr", msg);
        }
    });
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

if (codeEditor) {
    codeEditor.addEventListener("input", updateHighlight);
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
        }
    });
    codeEditor.value = EXAMPLES.hello;
    updateHighlight();
}

if (examplesSelect) {
    examplesSelect.addEventListener("change", () => {
        const name = examplesSelect.value;
        if (EXAMPLES[name] && codeEditor) {
            codeEditor.value = EXAMPLES[name];
            updateHighlight();
        }
    });
}

// Check button
const checkBtn = document.querySelector("[data-check]");
if (checkBtn) {
    checkBtn.addEventListener("click", async () => {
        const source = codeEditor?.value;
        if (!source?.trim()) return;

        setWorkspaceMode("edit");
        setOutputMode("console");
        state.activeGame = null;
        clearOutput();

        try {
            const comp = await loadCompiler();
            setStatus("Checking…", "info");
            const json = comp.aver_check(source);
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
                const lineNum = d.span?.line || 0;
                const col = d.span?.col || 0;

                appendConsole("stderr",
                    `\n${tag}[${d.slug}]: ${d.summary}`);
                appendConsole("stderr",
                    `  at: ${d.span?.file || "playground"}:${lineNum}:${col}`);

                if (d.fn_name) {
                    appendConsole("stderr", `  in-fn: ${d.fn_name}`);
                }
                if (d.repair?.primary) {
                    appendConsole("stderr", `  repair: ${d.repair.primary}`);
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
        const source = codeEditor?.value;
        if (!source?.trim()) return;

        setWorkspaceMode("edit");
        setOutputMode("console");
        state.activeGame = null;
        clearOutput();

        try {
            const comp = await loadCompiler();
            setStatus("Verifying…", "info");
            const json = comp.aver_verify(source);
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

            if (hasStaticErrors) {
                appendConsole("stderr",
                    "Static errors found — verify blocks were not executed.\n");
            }

            for (const d of staticIssues) {
                const tag = d.severity;
                const lineNum = d.span?.line || 0;
                const col = d.span?.col || 0;
                appendConsole(
                    d.severity === "error" || d.severity === "fail" ? "stderr" : "stdout",
                    `\n${tag}[${d.slug}]: ${d.summary}`
                );
                appendConsole("stdout",
                    `  at: ${d.span?.file || "playground"}:${lineNum}:${col}`);
                if (d.repair?.primary) {
                    appendConsole("stdout", `  repair: ${d.repair.primary}`);
                }
            }

            if (verifyFailures.length === 0 && !hasStaticErrors) {
                appendConsole("stdout", "✓ All verify cases passed.");
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

async function showGameSource(gameName) {
    const paths = GAME_SOURCES[gameName];
    if (!paths) return;

    const tabsEl = document.querySelector("[data-source-tabs]");
    const codeEl = document.querySelector("[data-source-code]");
    if (!tabsEl || !codeEl) return;

    tabsEl.innerHTML = "";
    codeEl.textContent = "Loading…";

    const sources = await Promise.all(paths.map(fetchSource));

    tabsEl.innerHTML = "";
    paths.forEach((path, i) => {
        const name = path.split("/").pop();
        const btn = document.createElement("button");
        btn.textContent = name;
        if (i === 0) btn.classList.add("active");
        btn.addEventListener("click", () => {
            tabsEl.querySelectorAll("button").forEach(b => b.classList.remove("active"));
            btn.classList.add("active");
            import("./highlight.js").then(({ highlightAver }) => {
                codeEl.innerHTML = highlightAver(sources[i]);
            });
        });
        tabsEl.appendChild(btn);
    });

    // Show first file highlighted
    import("./highlight.js").then(({ highlightAver }) => {
        codeEl.innerHTML = highlightAver(sources[0]);
    });
}

// Toggle source button
document.querySelector("[data-toggle-source]")?.addEventListener("click", () => {
    const ws = document.querySelector(".workspace");
    if (!ws) return;
    const showing = ws.dataset.showSource === "true";
    ws.dataset.showSource = showing ? "false" : "true";
});

// Load source when game is selected
document.querySelectorAll("[data-game]").forEach(btn => {
    btn.addEventListener("click", () => {
        const ws = document.querySelector(".workspace");
        if (ws) ws.dataset.showSource = "false";
        showGameSource(btn.dataset.game);
    });
});

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

// Draggable divider between source viewer and game output
{
    const divider = document.querySelector("[data-game-divider]");
    const layout = document.querySelector(".game-layout");
    const source = document.querySelector("[data-source-viewer]");
    if (divider && layout && source) {
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
            source.style.flex = "none";
            source.style.width = clamped + "%";
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
