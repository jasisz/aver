// Minimal Aver syntax highlighter for the playground.
// Strategy: tokenize first, then wrap tokens in spans. Never mix escaping with regex replace.

function esc(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

const KEYWORDS = new Set([
    "type", "record", "module", "depends", "exposes", "opaque", "fn",
    "match", "verify", "decision", "law", "given", "when", "effects",
    "trace", "intent", "reason", "date", "chosen", "rejected", "impacts",
    "author",
]);

const CONSTANTS = new Set(["true", "false", "Unit"]);

const BUILTINS = new Set([
    "Console", "Http", "HttpServer", "Disk", "Tcp", "Int", "Float",
    "String", "Bool", "List", "Vector", "Map", "Char", "Byte",
    "Result", "Option", "Args", "Time", "Env", "Random", "Terminal",
]);

// Token types
const T_COMMENT = "comment";
const T_STRING = "string";
const T_NUMBER = "number";
const T_KEYWORD = "keyword";
const T_CONSTANT = "constant";
const T_BUILTIN = "builtin";
const T_TYPE = "type";
const T_FN_NAME = "fn";
const T_EFFECT = "effect";
const T_OPERATOR = "operator";
const T_DESC = "desc";
const T_PLAIN = "plain";

function tokenize(line) {
    const tokens = [];
    let i = 0;

    while (i < line.length) {
        // Comment
        if (line[i] === "/" && line[i + 1] === "/") {
            tokens.push({ type: T_COMMENT, text: line.slice(i) });
            return tokens;
        }

        // String
        if (line[i] === '"') {
            let j = i + 1;
            while (j < line.length && line[j] !== '"') {
                if (line[j] === "\\") j++;
                j++;
            }
            j = Math.min(j + 1, line.length);
            tokens.push({ type: T_STRING, text: line.slice(i, j) });
            i = j;
            continue;
        }

        // Effect: ! [...]
        if (line[i] === "!" && /^\!\s*\[/.test(line.slice(i))) {
            const m = line.slice(i).match(/^(\!\s*\[)([^\]]*)(\])/);
            if (m) {
                tokens.push({ type: T_EFFECT, text: m[1] });
                tokens.push({ type: T_TYPE, text: m[2] });
                tokens.push({ type: T_EFFECT, text: m[3] });
                i += m[0].length;
                continue;
            }
        }

        // Description: ? "..."
        if (line[i] === "?" && /^\?\s*"/.test(line.slice(i))) {
            const m = line.slice(i).match(/^(\?\s*)("(?:[^"\\]|\\.)*")/);
            if (m) {
                tokens.push({ type: T_EFFECT, text: m[1] });
                tokens.push({ type: T_DESC, text: m[2] });
                i += m[0].length;
                continue;
            }
        }

        // Number
        if (/\d/.test(line[i])) {
            const m = line.slice(i).match(/^\d+\.?\d*/);
            if (m) {
                tokens.push({ type: T_NUMBER, text: m[0] });
                i += m[0].length;
                continue;
            }
        }

        // Arrow operators
        if (line[i] === "-" && line[i + 1] === ">") {
            tokens.push({ type: T_OPERATOR, text: "->" });
            i += 2;
            continue;
        }
        if (line[i] === "=" && line[i + 1] === ">") {
            tokens.push({ type: T_OPERATOR, text: "=>" });
            i += 2;
            continue;
        }

        // Word
        if (/[a-zA-Z_]/.test(line[i])) {
            const m = line.slice(i).match(/^[a-zA-Z_][a-zA-Z0-9_]*/);
            if (m) {
                const word = m[0];
                // Check if this is "fn <name>"
                if (word === "fn" && tokens.length > 0 || word === "fn") {
                    tokens.push({ type: T_KEYWORD, text: word });
                    i += word.length;
                    // Grab the function name after whitespace
                    const after = line.slice(i).match(/^(\s+)([a-zA-Z_][a-zA-Z0-9_]*)/);
                    if (after) {
                        tokens.push({ type: T_PLAIN, text: after[1] });
                        tokens.push({ type: T_FN_NAME, text: after[2] });
                        i += after[0].length;
                    }
                    continue;
                }
                if (KEYWORDS.has(word)) {
                    tokens.push({ type: T_KEYWORD, text: word });
                } else if (CONSTANTS.has(word)) {
                    tokens.push({ type: T_CONSTANT, text: word });
                } else if (BUILTINS.has(word)) {
                    tokens.push({ type: T_BUILTIN, text: word });
                } else if (word[0] >= "A" && word[0] <= "Z") {
                    tokens.push({ type: T_TYPE, text: word });
                } else {
                    tokens.push({ type: T_PLAIN, text: word });
                }
                i += word.length;
                continue;
            }
        }

        // Other character
        tokens.push({ type: T_PLAIN, text: line[i] });
        i++;
    }

    return tokens;
}

const CLASS_MAP = {
    [T_COMMENT]: "hl-comment",
    [T_STRING]: "hl-string",
    [T_NUMBER]: "hl-number",
    [T_KEYWORD]: "hl-keyword",
    [T_CONSTANT]: "hl-constant",
    [T_BUILTIN]: "hl-builtin",
    [T_TYPE]: "hl-type",
    [T_FN_NAME]: "hl-fn",
    [T_EFFECT]: "hl-effect",
    [T_OPERATOR]: "hl-operator",
    [T_DESC]: "hl-desc",
};

export function highlightAver(source) {
    return source.split("\n").map(line => {
        const tokens = tokenize(line);
        return tokens.map(t => {
            const cls = CLASS_MAP[t.type];
            const safe = esc(t.text);
            return cls ? `<span class="${cls}">${safe}</span>` : safe;
        }).join("");
    }).join("\n");
}
