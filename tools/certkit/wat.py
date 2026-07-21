"""Minimal `wasm-tools print` (WAT) reader for certkit.

Parses only what Stage A needs: the type section (struct/array/func types),
data segments, exports, and the folded-free instruction bodies of USER
functions (exports that are not runtime helpers). It is NOT a general WAT
parser -- it assumes the exact flat, one-instruction-per-line shape that
`wasm-tools print` emits for our wasm-gc backend (no folded s-expr
instructions inside function bodies).

Runtime helper bodies (loops, linear memory, br/br_if) are never parsed as
code -- they are identified by export name / call index and become host
contract slots (see extract.py).
"""
import re
import subprocess
import sys

RUNTIME_PREFIXES = ("__rt_", "__caller")
# `__aint_to_index` is the named host-role export the fused vector-read
# contract binds (mirror of `__rt_aint_from_i64` for box) — a runtime helper,
# never certified as user code.
RUNTIME_EXACT = {"_start", "memory", "__aint_to_index"}


def wat_of(wasm_path):
    return subprocess.run(
        ["wasm-tools", "print", wasm_path],
        check=True, capture_output=True, text=True,
    ).stdout


def _split_top(s):
    """Split a parenthesised s-expr body into top-level tokens/groups."""
    out, depth, cur = [], 0, ""
    for ch in s:
        if ch == "(":
            depth += 1
            cur += ch
        elif ch == ")":
            depth -= 1
            cur += ch
        elif ch.isspace() and depth == 0:
            if cur:
                out.append(cur)
                cur = ""
        else:
            cur += ch
    if cur:
        out.append(cur)
    return out


def _strip_outer(sexpr, keyword):
    """`(keyword <inner>)` -> `<inner>` (removes only the matching outer parens)."""
    s = sexpr.strip()
    assert s.startswith("(" + keyword) and s.endswith(")"), s
    return s[len("(" + keyword):-1].strip()


def parse_types(wat):
    """Return list of type descriptors indexed by type index.

    Each is one of:
      ("array", elem_str)
      ("struct", [field_str, ...])
      ("func", [param_str,...], [result_str,...])
    """
    types = {}
    for m in re.finditer(r"\(type \(;(\d+);\)\s+(.*)\)\s*$", wat, re.M):
        idx = int(m.group(1))
        body = m.group(2).strip()
        if body.startswith("(array"):
            elem = _strip_outer(body, "array")
            types[idx] = ("array", elem)
        elif body.startswith("(struct"):
            inner = _strip_outer(body, "struct")
            fields = []
            for grp in _split_top(inner):
                if grp.startswith("(field"):
                    fields.append(_strip_outer(grp, "field"))
            types[idx] = ("struct", fields)
        elif body.startswith("(func"):
            inner = _strip_outer(body, "func")
            params, results = [], []
            for grp in _split_top(inner):
                if grp.startswith("(param"):
                    params += _split_top(_strip_outer(grp, "param"))
                elif grp.startswith("(result"):
                    results += _split_top(_strip_outer(grp, "result"))
            types[idx] = ("func", params, results)
    n = (max(types) + 1) if types else 0
    return [types.get(i) for i in range(n)]


def parse_data(wat):
    """Return list of data segment byte-strings indexed by segment index."""
    segs = {}
    for m in re.finditer(r'\(data \(;(\d+);\)\s+"((?:[^"\\]|\\.)*)"\s*\)', wat):
        segs[int(m.group(1))] = _unescape(m.group(2))
    n = (max(segs) + 1) if segs else 0
    return [segs.get(i, b"") for i in range(n)]


def _unescape(s):
    out = bytearray()
    i = 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            nxt = s[i + 1]
            if nxt in "tnr\"'\\":
                out += {"t": b"\t", "n": b"\n", "r": b"\r",
                        '"': b'"', "'": b"'", "\\": b"\\"}[nxt]
                i += 2
                continue
            hexpair = s[i + 1:i + 3]
            if len(hexpair) == 2 and all(h in "0123456789abcdefABCDEF" for h in hexpair):
                out.append(int(hexpair, 16))
                i += 3
                continue
        out += c.encode("utf-8")
        i += 1
    return bytes(out)


def parse_exports(wat):
    """Return dict export_name -> ('func'|'memory'|..., index)."""
    out = {}
    for m in re.finditer(r'\(export "((?:[^"\\]|\\.)*)" \((\w+) (\d+)\)\)', wat):
        out[m.group(1)] = (m.group(2), int(m.group(3)))
    return out


def user_functions(wat):
    """Map user-function export name -> func index (runtime helpers excluded)."""
    out = {}
    for name, (kind, idx) in parse_exports(wat).items():
        if kind != "func":
            continue
        if name in RUNTIME_EXACT or name.startswith(RUNTIME_PREFIXES):
            continue
        out[name] = idx
    return out


# Function header line: (func (;IDX;) (type T) (param ...) (result ...)
_FUNC_HDR = re.compile(r"^\s*\(func \(;(\d+);\)\s+\(type (\d+)\)(.*)$")
_LOCAL = re.compile(r"^\s*\(local (.*)\)\s*$")


def function_bodies(wat):
    """Return dict func_index -> {'type': int, 'locals': [str], 'lines': [str]}.

    `lines` is the raw flat instruction stream (one op per line), with the
    trailing `)` stripped. Structured ops (if/else/end) are preserved as-is.
    """
    lines = wat.splitlines()
    funcs = {}
    i = 0
    while i < len(lines):
        m = _FUNC_HDR.match(lines[i])
        if not m:
            i += 1
            continue
        idx, ty = int(m.group(1)), int(m.group(2))
        # One-line func like `(func (;0;) (type 10))` closes on the header
        # line: paren depth returns to 0. Multi-line headers with a
        # `(result ...)` end in `))` but stay open (depth > 0).
        depth0 = lines[i].count("(") - lines[i].count(")")
        if depth0 <= 0:
            funcs[idx] = {"type": ty, "locals": [], "lines": []}
            i += 1
            continue
        body, locals_ = [], []
        i += 1
        depth = 1  # we're inside the (func ...) paren
        while i < len(lines):
            ln = lines[i]
            lm = _LOCAL.match(ln)
            if lm:
                locals_ += _split_top(lm.group(1))
                i += 1
                continue
            stripped = ln.strip()
            if stripped == ")":
                i += 1
                break
            body.append(stripped)
            i += 1
        funcs[idx] = {"type": ty, "locals": locals_, "lines": body}
    return funcs


def opcode_of(line):
    """First whitespace-delimited token = the mnemonic."""
    return line.split()[0] if line.split() else ""


if __name__ == "__main__":
    # Opcode census over user functions of the given wasm files.
    census = {}
    for path in sys.argv[1:]:
        wat = wat_of(path)
        ufns = set(user_functions(wat).values())
        bodies = function_bodies(wat)
        for idx in ufns:
            for ln in bodies.get(idx, {}).get("lines", []):
                op = opcode_of(ln)
                census.setdefault(op, set()).add(path.split("/")[-1])
    for op in sorted(census):
        print(f"{len(census[op]):3d}  {op}")
    print(f"\n{len(census)} distinct user-code opcodes")
