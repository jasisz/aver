#!/usr/bin/env python3
"""Byte-level reference decoder for the AverUserProfile/v0 certificate profile.

This is the Python leg of the three-way decoder differential: it decodes a
compiled wasm-gc module DIRECTLY from its bytes (section walk, type section,
import/function/export/data sections, and the 39-opcode user-code fragment)
into the same pure data the in-kernel `CertDecode.lean` decoder and the Rust
`cert::rederive_obligations` produce. `extract.py` (WAT-level) and this file
(byte-level) are independent implementations of the same grammar, so `validate`
cross-checks them; `json` emits the oracle the decoder witness/tests pin.

Python stdlib only.
"""
import json
import os
import re
import sys

sys.path.insert(0, __file__.rsplit("/", 1)[0])
import wat        # noqa: E402
import extract    # noqa: E402


# ---- LEB128 --------------------------------------------------------------

def uleb(b, i):
    r = 0
    s = 0
    st = i
    while True:
        x = b[i]
        i += 1
        r |= (x & 0x7f) << s
        if not (x & 0x80):
            if i - st > 1 and x == 0:
                raise ValueError("overlong LEB")
            break
        s += 7
    return r, i


def sleb(b, i):
    r = 0
    s = 0
    while True:
        x = b[i]
        i += 1
        r |= (x & 0x7f) << s
        s += 7
        if not (x & 0x80):
            if s < 64 and (x & 0x40):
                r |= -(1 << s)
            break
    return r, i


def walk(b):
    assert b[0:4] == b"\x00asm", "bad magic"
    i = 8
    secs = []
    while i < len(b):
        sid = b[i]
        i += 1
        sz, i = uleb(b, i)
        secs.append((sid, i, sz))
        i += sz
    return secs


def find(secs, sid):
    xs = [s for s in secs if s[0] == sid]
    return xs[0] if xs else None


# ---- type section (arity, struct field counts, carrier) ------------------

NUM_VT = {0x7f, 0x7e, 0x7d, 0x7c, 0x7b}


def read_valtype(b, i):
    v = b[i]
    if v in NUM_VT:
        return i + 1
    if v in (0x63, 0x64):
        _, i = sleb(b, i + 1)
        return i
    if 0x6a <= v <= 0x73:          # abstract-heap shorthand (eqref, anyref, ...)
        return i + 1
    raise ValueError(f"valtype {v:#x}")


def read_storagetype(b, i):
    v = b[i]
    if v in (0x78, 0x77):          # packed i8 / i16
        return i + 1
    return read_valtype(b, i)


def read_field(b, i):
    v = b[i]
    i = read_storagetype(b, i)
    return v, i + 1                # storage first byte + skip mut byte


def decode_types(b, secs):
    ty = find(secs, 1)
    arities = {}                   # typeidx -> param count (func types)
    nfields = {}                   # typeidx -> field count (struct types)
    carrier = None
    if ty is None:
        return arities, nfields, carrier
    i = ty[1]
    count, i = uleb(b, i)
    tidx = 0

    def comptype(i):
        nonlocal tidx, carrier
        v = b[i]
        if v == 0x60:              # func
            i += 1
            np, i = uleb(b, i)
            for _ in range(np):
                i = read_valtype(b, i)
            nr, i = uleb(b, i)
            for _ in range(nr):
                i = read_valtype(b, i)
            arities[tidx] = np
        elif v == 0x5f:            # struct
            i += 1
            nf, i = uleb(b, i)
            fbs = []
            for _ in range(nf):
                sfb, i = read_field(b, i)
                fbs.append(sfb)
            nfields[tidx] = nf
            if carrier is None and nf == 3 and fbs[0] == 0x7e and fbs[2] == 0x7f:
                carrier = tidx
        elif v == 0x5e:            # array
            i += 1
            _, i = read_field(b, i)
        else:
            raise ValueError(f"comptype {v:#x}")
        tidx += 1
        return i

    def subtype(i):
        v = b[i]
        if v in (0x50, 0x4f):      # sub / sub final
            i += 1
            n, i = uleb(b, i)
            for _ in range(n):
                _, i = uleb(b, i)
        return comptype(i)

    for _ in range(count):
        if b[i] == 0x4e:           # explicit rec group
            i += 1
            n, i = uleb(b, i)
            for _ in range(n):
                i = subtype(i)
        else:
            i = subtype(i)
    return arities, nfields, carrier


# ---- import / function / export / data sections --------------------------

def decode_imports(b, secs):
    imp = find(secs, 2)
    out = []
    nfunc = 0
    if imp is None:
        return out, 0
    i = imp[1]
    n, i = uleb(b, i)
    for _ in range(n):
        ml, i = uleb(b, i)
        mod = b[i:i + ml].decode("latin1")
        i += ml
        nl, i = uleb(b, i)
        nm = b[i:i + nl].decode("latin1")
        i += nl
        kind = b[i]
        i += 1
        if kind == 0:
            _, i = uleb(b, i)
            out.append((mod, nm))
            nfunc += 1
        else:
            raise ValueError(f"non-function import kind {kind}")
    return out, nfunc


def decode_funcsec(b, secs):
    fs = find(secs, 3)
    out = []
    if fs is None:
        return out
    i = fs[1]
    n, i = uleb(b, i)
    for _ in range(n):
        t, i = uleb(b, i)
        out.append(t)
    return out


def decode_exports(b, secs):
    ex = find(secs, 7)
    out = []
    i = ex[1]
    n, i = uleb(b, i)
    for _ in range(n):
        nl, i = uleb(b, i)
        nm = b[i:i + nl].decode("latin1")
        i += nl
        kind = b[i]
        i += 1
        idx, i = uleb(b, i)
        if kind == 0:
            out.append((nm, idx))
    return out


def decode_data(b, secs):
    d = find(secs, 11)
    out = []
    if d is None:
        return out
    i = d[1]
    n, i = uleb(b, i)
    for _ in range(n):
        flag = b[i]
        i += 1
        if flag != 0x01:
            raise ValueError(f"non-passive data flag {flag}")
        cnt, i = uleb(b, i)
        out.append(b[i:i + cnt])
        i += cnt
    return out


# ---- body decode ---------------------------------------------------------

def skip_blocktype(b, i):
    bt = b[i]
    if bt == 0x40 or bt in NUM_VT:
        return i + 1
    if bt in (0x63, 0x64):
        _, i = sleb(b, i + 1)
        return i
    _, i = sleb(b, i)              # typeidx s33
    return i


SIMPLE = {
    0x50: ".i64Eqz", 0x51: ".i64Eq", 0x57: ".i64LeS", 0x53: ".i64LtS",
    0x59: ".i64GeS", 0x55: ".i64GtS", 0x46: ".i32Eq", 0x71: ".i32And",
    0x48: ".i32LtS", 0x4c: ".i32LeS", 0x4a: ".i32GtS", 0xa0: ".f64Add",
    0xa1: ".f64Sub", 0xa2: ".f64Mul", 0xa3: ".f64Div", 0x61: ".f64Eq",
    0x63: ".f64Lt", 0x65: ".f64Le", 0x66: ".f64Ge", 0x64: ".f64Gt",
    0xd1: ".refIsNull", 0x0f: ".ret",
}


def decode_block(b, i, end, nfields, data_segs, pend):
    out = []
    while i < end:
        op = b[i]
        i += 1
        if op in (0x0b, 0x05):     # end / else — leave the terminator
            return out, i - 1
        if op == 0x04:             # if
            i = skip_blocktype(b, i)
            tb, i = decode_block(b, i, end, nfields, data_segs, pend)
            eb = []
            if b[i] == 0x05:
                i += 1
                eb, i = decode_block(b, i, end, nfields, data_segs, pend)
            i += 1                 # closing end
            out.append(f".ifElse [{', '.join(tb)}] [{', '.join(eb)}]")
        elif op == 0x20:
            v, i = uleb(b, i)
            out.append(f".localGet {v}")
        elif op == 0x21:
            v, i = uleb(b, i)
            out.append(f".localSet {v}")
        elif op == 0x42:
            v, i = sleb(b, i)
            out.append(f".i64Const ({v})")
        elif op == 0x41:
            v, i = sleb(b, i)
            out.append(f".i32Const ({v})")
            pend.append(v)
        elif op == 0x44:
            bits = int.from_bytes(b[i:i + 8], "little")
            i += 8
            out.append(f".f64Const 0x{bits:016x}")
        elif op == 0xd0:
            _, i = sleb(b, i)
            out.append(".refNull")
        elif op == 0x10:
            v, i = uleb(b, i)
            out.append(f".call {v}")
        elif op == 0x12:
            v, i = uleb(b, i)
            out.append(f".returnCall {v}")
        elif op == 0xfb:
            sub, i = uleb(b, i)
            if sub == 0x00:
                t, i = uleb(b, i)
                out.append(f".structNew {t} {nfields.get(t, 0)}")
            elif sub == 0x02:
                t, i = uleb(b, i)
                f, i = uleb(b, i)
                out.append(f".structGet {t} {f}")
            elif sub == 0x08:
                t, i = uleb(b, i)
                m, i = uleb(b, i)
                out.append(f".arrayNewFixed {t} {m}")
            elif sub == 0x09:
                t, i = uleb(b, i)
                seg, i = uleb(b, i)
                length = pend[-1] if pend else 0
                offset = pend[-2] if len(pend) >= 2 else 0
                payload = data_segs[seg] if seg < len(data_segs) else b""
                chunk = list(payload[offset:offset + length])
                out.append(f".arrayNewData {t} [{', '.join(str(x) for x in chunk)}]")
            elif sub in (0x14, 0x15):
                h, i = sleb(b, i)
                out.append(f".refTest {h}")
            elif sub in (0x16, 0x17):
                h, i = sleb(b, i)
                out.append(f".refCast {h}")
            else:
                raise ValueError(f"gc subop {sub:#x}")
        elif op in SIMPLE:
            out.append(SIMPLE[op])
        else:
            raise ValueError(f"opcode {op:#x}")
    return out, i


def analyze(wasm_path):
    b = open(wasm_path, "rb").read()
    secs = walk(b)
    w = wat.wat_of(wasm_path)
    user = wat.user_functions(w)
    bodies = wat.function_bodies(w)
    arities, nfields, carrier = decode_types(b, secs)
    imports, nimp = decode_imports(b, secs)
    funcsec = decode_funcsec(b, secs)
    exports = decode_exports(b, secs)
    data_segs = decode_data(b, secs)

    code = find(secs, 10)
    j = code[1]
    nf, j = uleb(b, j)
    entries = []
    for _ in range(nf):
        esz, j = uleb(b, j)
        bend = j + esz
        k = j
        ng, k = uleb(b, k)
        nloc = 0
        for _ in range(ng):
            c, k = uleb(b, k)
            t = b[k]
            k += 1
            if t in (0x63, 0x64):
                _, k = sleb(b, k)
            nloc += c
        entries.append((k, bend, nloc))
        j = bend

    funcs = []
    opcodes = set()
    for name, fidx in sorted(user.items(), key=lambda kv: kv[1]):
        di = fidx - nimp
        bstart, bend, nloc = entries[di]
        body, _ = decode_block(b, bstart, bend, nfields, data_segs, [])
        tyidx = funcsec[di]
        arity = arities.get(tyidx, 0)
        funcs.append({
            "name": name, "idx": fidx, "arity": arity, "nlocals": nloc,
            "body": "[" + ", ".join(body) + "]",
        })
        for ln in bodies.get(fidx, {}).get("lines", []):
            if ln.split():
                opcodes.add(ln.split()[0])

    return {
        "carrier": carrier,
        "imports": imports,
        "exports": exports,
        "opcodes": sorted(opcodes),
        "funcs": funcs,
        "sections": [s[0] for s in secs],
    }


# ---- CLI -----------------------------------------------------------------

def _validate(paths):
    ok_all = True
    for path in paths:
        r = analyze(path)
        meta, arms = extract.extract(path)
        exp = {}
        for a in arms:
            m = re.match(r"\s*\|\s*(\d+)\s*=>\s*some\s*⟨(\d+),\s*(\d+),\s*\[(.*)\]⟩", a)
            exp[int(m.group(1))] = (int(m.group(2)), int(m.group(3)), "[" + m.group(4) + "]")
        ok = True
        for f in r["funcs"]:
            if (f["arity"], f["nlocals"], f["body"]) != exp[f["idx"]]:
                ok = False
                print(f"  MISMATCH {os.path.basename(path)} fn{f['idx']} {f['name']}")
        if r["carrier"] != meta["carrier"]:
            ok = False
            print(f"  CARRIER {r['carrier']} != {meta['carrier']} in {path}")
        ok_all = ok_all and ok
        print(f"{os.path.basename(path):20} carrier={r['carrier']} "
              f"exports={len(r['exports'])} funcs={len(r['funcs'])} "
              f"{'OK' if ok else 'FAIL'}")
    return ok_all


def main():
    if len(sys.argv) < 3:
        print("usage: decode_ref.py {validate|json} <wasm>...", file=sys.stderr)
        sys.exit(2)
    mode = sys.argv[1]
    if mode == "validate":
        sys.exit(0 if _validate(sys.argv[2:]) else 1)
    elif mode == "json":
        print(json.dumps(analyze(sys.argv[2])))
    else:
        print(f"unknown mode {mode}", file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
