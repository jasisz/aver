#!/usr/bin/env python3
"""Extract Lean certificate definitions from a compiled Aver wasm-gc module.

Given `app.wasm`, disassemble with `wasm-tools print`, parse the type section,
data segments, exports, and the bodies of USER functions (exports that are not
runtime helpers), and emit:

  * `<Module>.lean` — a `CodeTbl` mapping each user function index to its body
    as a `CertPrelude.WInstr` term (if/else/end folded; struct/array immediates
    and data-segment bytes resolved at extraction time);
  * `<module>.meta.json` — metadata the differential harness needs: the carrier
    type index, per-function signatures, the list-cons and string type indices,
    ADT variant struct types, and each user function's call targets (resolved to
    the runtime-helper export name where one exists, else by call index).

Runtime helper bodies are never emitted as code; they surface only as named
host-contract slots in the metadata.

Python stdlib only.
"""
import json
import re
import struct
import sys

sys.path.insert(0, __file__.rsplit("/", 1)[0])
import wat  # noqa: E402


# ---- type classification -------------------------------------------------

def carrier_type_index(types):
    """The Int carrier: struct {mut i64, mut (ref null <arr i64>), mut i32}."""
    for i, t in enumerate(types):
        if t and t[0] == "struct" and len(t[1]) == 3:
            f0, f1, f2 = t[1]
            norm = lambda s: s.replace(" ", "")
            if norm(f0) == "(muti64)" and norm(f2) == "(muti32)" \
                    and "arrayi64" in _arr_shape(types, f1):
                return i
    return None


def _arr_shape(types, field):
    m = re.search(r"ref null (\d+)", field)
    if not m:
        return ""
    idx = int(m.group(1))
    t = types[idx] if idx < len(types) else None
    if t and t[0] == "array":
        return "array" + t[1].replace(" ", "").replace("(mut", "").replace(")", "")
    return ""


def string_type_index(types):
    """Byte string: (array (mut i8))."""
    for i, t in enumerate(types):
        if t and t[0] == "array" and t[1].replace(" ", "") == "(muti8)":
            return i
    return None


def cons_type_index(types, carrier):
    """List cons cell: struct whose second field refs the cons type itself."""
    for i, t in enumerate(types):
        if t and t[0] == "struct" and len(t[1]) == 2:
            m = re.search(r"ref null (\d+)", t[1][1])
            if m and int(m.group(1)) == i:
                return i
    return None


def func_types(types):
    return {i: t for i, t in enumerate(types) if t and t[0] == "func"}


def struct_field_count(types, idx):
    t = types[idx] if idx < len(types) else None
    return len(t[1]) if t and t[0] == "struct" else 0


# ---- instruction conversion ---------------------------------------------

def hexfloat_to_bits(tok):
    """Parse a WAT f64 literal ('0x1p+1', '-0x1.8p+0', 'nan', 'inf') to u64 bits."""
    tl = tok.lower()
    if tl in ("nan", "+nan", "-nan") or tl.startswith("nan:"):
        val = float("nan")
    elif tl in ("inf", "+inf"):
        val = float("inf")
    elif tl == "-inf":
        val = float("-inf")
    else:
        val = float.fromhex(tok)
    return struct.unpack("<Q", struct.pack("<d", val))[0]


_REF_IDX = re.compile(r"\(ref(?:\s+null)?\s+(\d+)\)")


def _ref_target(rest):
    m = _REF_IDX.search(rest)
    return int(m.group(1)) if m else None


def convert_body(lines, types, data_segs):
    """Fold the flat instruction stream into a list of Lean WInstr term strings."""
    # Track the last two i32.const values so array.new_data can resolve
    # its (offset, len) operands to concrete bytes.
    pending_i32 = []

    def one(line):
        parts = line.split()
        op = parts[0]
        if op == "local.get":
            return f".localGet {int(parts[1])}"
        if op == "local.set":
            return f".localSet {int(parts[1])}"
        if op == "i64.const":
            return f".i64Const ({int(parts[1])})"
        if op == "i32.const":
            v = int(parts[1])
            pending_i32.append(v)
            return f".i32Const ({v})"
        if op == "f64.const":
            return f".f64Const 0x{hexfloat_to_bits(parts[1]):016x}"
        if op == "ref.null":
            return ".refNull"
        if op == "ref.is_null":
            return ".refIsNull"
        if op == "ref.test":
            return f".refTest {_ref_target(line)}"
        if op == "ref.cast":
            return f".refCast {_ref_target(line)}"
        if op == "struct.new":
            ty = int(parts[1])
            return f".structNew {ty} {struct_field_count(types, ty)}"
        if op == "struct.get":
            return f".structGet {int(parts[1])} {int(parts[2])}"
        if op == "array.new_fixed":
            return f".arrayNewFixed {int(parts[1])} {int(parts[2])}"
        if op == "array.new_data":
            ty, seg = int(parts[1]), int(parts[2])
            length = pending_i32[-1] if pending_i32 else 0
            offset = pending_i32[-2] if len(pending_i32) >= 2 else 0
            payload = data_segs[seg] if seg < len(data_segs) else b""
            chunk = list(payload[offset:offset + length])
            return f".arrayNewData {ty} {_lean_nat_list(chunk)}"
        simple = {
            "i64.eqz": ".i64Eqz", "i64.eq": ".i64Eq", "i64.le_s": ".i64LeS",
            "i64.lt_s": ".i64LtS", "i64.ge_s": ".i64GeS", "i64.gt_s": ".i64GtS",
            "i32.eq": ".i32Eq", "i32.and": ".i32And", "i32.lt_s": ".i32LtS",
            "i32.le_s": ".i32LeS", "i32.gt_s": ".i32GtS",
            "f64.add": ".f64Add", "f64.sub": ".f64Sub", "f64.mul": ".f64Mul",
            "f64.div": ".f64Div", "f64.eq": ".f64Eq", "f64.lt": ".f64Lt",
            "f64.le": ".f64Le", "f64.ge": ".f64Ge", "f64.gt": ".f64Gt",
            "call": None, "return_call": None, "return": ".ret",
        }
        if op == "call":
            return f".call {int(parts[1])}"
        if op == "return_call":
            return f".returnCall {int(parts[1])}"
        if op in simple:
            return simple[op]
        raise ValueError(f"unmodelled user-code opcode: {op!r} in {line!r}")

    # Recursive-descent fold of if/else/end.
    pos = 0

    def block():
        nonlocal pos
        out = []
        while pos < len(lines):
            line = lines[pos]
            op = line.split()[0]
            if op == "if":
                pos += 1
                then_b = block()          # consumes up to else/end
                else_b = []
                if pos < len(lines) and lines[pos].split()[0] == "else":
                    pos += 1
                    else_b = block()
                # now at 'end'
                assert lines[pos].split()[0] == "end", lines[pos]
                pos += 1
                out.append(f".ifElse [{', '.join(then_b)}] [{', '.join(else_b)}]")
            elif op in ("else", "end"):
                return out
            else:
                out.append(one(line))
                pos += 1
        return out

    return block()


def _lean_nat_list(nats):
    return "[" + ", ".join(str(n) for n in nats) + "]"


# ---- signature / kind inference -----------------------------------------

def kind_of(valtype, ctx):
    """Map a wasm valtype string to an input/output kind for the harness."""
    v = valtype.replace(" ", "")
    if v == "f64":
        return {"kind": "float"}
    if v == "i32":
        return {"kind": "bool"}
    if v == "i64":
        return {"kind": "i64"}
    if v == "eqref":
        return {"kind": "adt"}   # any user ADT; variant chosen from source
    m = re.search(r"refnull?(\d+)", v)
    if m:
        idx = int(m.group(1))
        if idx == ctx["carrier"]:
            return {"kind": "int"}
        if idx == ctx["string"]:
            return {"kind": "string"}
        if idx == ctx["cons"]:
            return {"kind": "list_int"}
        return {"kind": "adt", "type": idx}
    return {"kind": "unknown", "wasm": valtype}


def extract(wasm_path):
    w = wat.wat_of(wasm_path)
    types = wat.parse_types(w)
    data_segs = wat.parse_data(w)
    exports = wat.parse_exports(w)
    user = wat.user_functions(w)
    bodies = wat.function_bodies(w)
    ftypes = func_types(types)
    idx2export = {i: n for n, (k, i) in exports.items() if k == "func"}

    ctx = {
        "carrier": carrier_type_index(types),
        "string": string_type_index(types),
        "cons": None,
    }
    ctx["cons"] = cons_type_index(types, ctx["carrier"])

    funcs_meta = []
    lean_arms = []
    for name, idx in sorted(user.items(), key=lambda kv: kv[1]):
        ty = bodies[idx]["type"]
        _, params, results = types[ty]
        nlocals = len(bodies[idx]["locals"])
        body_terms = convert_body(bodies[idx]["lines"], types, data_segs)
        arity = len(params)
        lean_arms.append(
            f"  | {idx} => some ⟨{arity}, {nlocals}, [{', '.join(body_terms)}]⟩")
        calls = []
        for ln in bodies[idx]["lines"]:
            if ln.startswith(("call ", "return_call ")):
                t = int(ln.split()[1])
                calls.append({"idx": t, "export": idx2export.get(t)})
        funcs_meta.append({
            "name": name, "idx": idx, "arity": arity,
            "params": [kind_of(p, ctx) for p in params],
            "results": [kind_of(r, ctx) for r in results],
            "calls": calls,
        })

    return {
        "wasm": wasm_path,
        "carrier": ctx["carrier"], "string": ctx["string"], "cons": ctx["cons"],
        "functions": funcs_meta,
    }, lean_arms


def emit_lean(module, meta, lean_arms):
    arms = "\n".join(lean_arms)
    return f"""-- Generated by tools/certkit/extract.py from {meta['wasm']}
-- User-function bodies of module `{module}` as CertPrelude.WInstr terms.
-- Runtime helpers are host-contract slots, not code (see {module.lower()}.meta.json).
import CertPrelude
namespace CertGen
open CertPrelude

def {module}Code : CodeTbl := fun fn =>
  match fn with
{arms}
  | _ => none

end CertGen
"""


def main():
    if len(sys.argv) < 3:
        print("usage: extract.py app.wasm ModuleName [outdir]", file=sys.stderr)
        sys.exit(2)
    wasm_path, module = sys.argv[1], sys.argv[2]
    outdir = sys.argv[3] if len(sys.argv) > 3 else "."
    meta, lean_arms = extract(wasm_path)
    with open(f"{outdir}/{module}.lean", "w") as fh:
        fh.write(emit_lean(module, meta, lean_arms))
    with open(f"{outdir}/{module.lower()}.meta.json", "w") as fh:
        json.dump(meta, fh, indent=2)
    print(f"wrote {outdir}/{module}.lean and {outdir}/{module.lower()}.meta.json")
    print(f"  carrier=type{meta['carrier']} string=type{meta['string']} "
          f"cons=type{meta['cons']} funcs={len(meta['functions'])}")


if __name__ == "__main__":
    main()
