const DEFAULT_COLOR = "default";
const DEFAULT_COLOR_INDEX = 0;
const SPACE_CODE = 32;

export const TERMINAL_COLOR_NAMES = [
    "default",
    "red",
    "green",
    "yellow",
    "blue",
    "white",
    "cyan",
    "magenta",
    "black",
];
export const TERMINAL_SPACE_CODE = SPACE_CODE;

const COLOR_TO_INDEX = new Map(
    TERMINAL_COLOR_NAMES.map((name, index) => [name, index]),
);

function clamp(value, min, max) {
    return Math.min(Math.max(value, min), max);
}

function cellCount(cols, rows) {
    return cols * rows;
}

function makeBuffers(cols, rows) {
    const chars = new Uint32Array(cellCount(cols, rows));
    chars.fill(SPACE_CODE);
    const colors = new Uint8Array(cellCount(cols, rows));
    colors.fill(DEFAULT_COLOR_INDEX);
    return { chars, colors };
}

function codePointOrSpace(ch) {
    return ch.codePointAt(0) ?? SPACE_CODE;
}

export class TerminalBuffer {
    constructor(cols = 80, rows = 24) {
        this.cols = Math.max(1, cols);
        this.rows = Math.max(1, rows);
        this.cursorX = 0;
        this.cursorY = 0;
        this.cursorVisible = true;
        this.currentColorIndex = DEFAULT_COLOR_INDEX;
        const { chars, colors } = makeBuffers(this.cols, this.rows);
        this.charCodes = chars;
        this.colorCodes = colors;
    }

    cellIndex(x, y) {
        return y * this.cols + x;
    }

    clearRow(rowIdx) {
        const start = rowIdx * this.cols;
        const end = start + this.cols;
        this.charCodes.fill(SPACE_CODE, start, end);
        this.colorCodes.fill(DEFAULT_COLOR_INDEX, start, end);
    }

    resize(cols, rows) {
        const nextCols = Math.max(1, cols);
        const nextRows = Math.max(1, rows);
        const { chars: nextCharCodes, colors: nextColorCodes } = makeBuffers(nextCols, nextRows);
        const copyCols = Math.min(this.cols, nextCols);
        const copyRows = Math.min(this.rows, nextRows);

        for (let row = 0; row < copyRows; row += 1) {
            const srcStart = row * this.cols;
            const srcEnd = srcStart + copyCols;
            const dstStart = row * nextCols;
            nextCharCodes.set(this.charCodes.subarray(srcStart, srcEnd), dstStart);
            nextColorCodes.set(this.colorCodes.subarray(srcStart, srcEnd), dstStart);
        }

        this.cols = nextCols;
        this.rows = nextRows;
        this.charCodes = nextCharCodes;
        this.colorCodes = nextColorCodes;
        this.cursorX = Math.min(this.cursorX, this.cols - 1);
        this.cursorY = Math.min(this.cursorY, this.rows - 1);
    }

    clear() {
        this.charCodes.fill(SPACE_CODE);
        this.colorCodes.fill(DEFAULT_COLOR_INDEX);
        this.cursorX = 0;
        this.cursorY = 0;
    }

    moveTo(x, y) {
        this.cursorX = clamp(x, 0, this.cols - 1);
        this.cursorY = clamp(y, 0, this.rows - 1);
    }

    setColor(color) {
        this.currentColorIndex = COLOR_TO_INDEX.get(color || DEFAULT_COLOR) ?? DEFAULT_COLOR_INDEX;
    }

    resetColor() {
        this.currentColorIndex = DEFAULT_COLOR_INDEX;
    }

    hideCursor() {
        this.cursorVisible = false;
    }

    showCursor() {
        this.cursorVisible = true;
    }

    isBlank() {
        for (let i = 0; i < this.charCodes.length; i += 1) {
            if (
                this.charCodes[i] !== SPACE_CODE ||
                this.colorCodes[i] !== DEFAULT_COLOR_INDEX
            ) {
                return false;
            }
        }
        return true;
    }

    print(text) {
        for (const ch of text) {
            if (ch === "\r") {
                this.cursorX = 0;
                continue;
            }
            if (ch === "\n") {
                this.newLine();
                continue;
            }
            if (ch === "\t") {
                for (let i = 0; i < 4; i += 1) {
                    this.writeChar(" ");
                }
                continue;
            }
            this.writeChar(ch);
        }
    }

    writeChar(ch) {
        if (this.cursorY >= this.rows) {
            this.scrollUp();
            this.cursorY = this.rows - 1;
        }

        const index = this.cellIndex(this.cursorX, this.cursorY);
        this.charCodes[index] = codePointOrSpace(ch);
        this.colorCodes[index] = this.currentColorIndex;
        this.cursorX += 1;
        if (this.cursorX >= this.cols) {
            this.newLine();
        }
    }

    newLine() {
        this.cursorX = 0;
        this.cursorY += 1;
        if (this.cursorY >= this.rows) {
            this.scrollUp();
            this.cursorY = this.rows - 1;
        }
    }

    scrollUp() {
        if (this.rows === 1) {
            this.clearRow(0);
            return;
        }

        const stride = this.cols;
        this.charCodes.copyWithin(0, stride);
        this.colorCodes.copyWithin(0, stride);
        this.clearRow(this.rows - 1);
    }

    toSnapshot() {
        const cursorIndex = this.cellIndex(this.cursorX, this.cursorY);
        const cursor =
            this.cursorVisible && this.cursorY >= 0 && this.cursorY < this.rows
                ? {
                    x: this.cursorX,
                    y: this.cursorY,
                    code: this.charCodes[cursorIndex] ?? SPACE_CODE,
                    colorIndex: this.colorCodes[cursorIndex] ?? DEFAULT_COLOR_INDEX,
                }
                : null;

        return {
            cols: this.cols,
            rows: this.rows,
            chars: this.charCodes.slice(),
            colors: this.colorCodes.slice(),
            cursor,
        };
    }
}
