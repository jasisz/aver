#!/usr/bin/env python3
"""Byte-level reference decoder for the AverUserProfile/v1 certificate profile.

This is the Python leg of the decoder differential: it reads a compiled
wasm-gc module DIRECTLY from its bytes (the section walk, the Int carrier from
the type section, the import and export sections) into the same pure data the
in-kernel `CertDecode.lean` decoder produces; `json` emits the oracle the
decoder witness and tests pin. No instruction is decoded: the checker binds a
planned function's code entry by re-encoding its plan and comparing the bytes.

Python stdlib only.
"""
import json
import sys


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


def analyze(wasm_path):
    b = open(wasm_path, "rb").read()
    secs = walk(b)
    _arities, _nfields, carrier = decode_types(b, secs)
    imports, _nimp = decode_imports(b, secs)
    exports = decode_exports(b, secs)
    return {
        "carrier": carrier,
        "imports": imports,
        "exports": exports,
        "sections": [s[0] for s in secs],
    }


# ---- CLI -----------------------------------------------------------------

def main():
    if len(sys.argv) != 3 or sys.argv[1] != "json":
        print("usage: decode_ref.py json <wasm>", file=sys.stderr)
        sys.exit(2)
    print(json.dumps(analyze(sys.argv[2])))


if __name__ == "__main__":
    main()
