"""Generate the structure/coverage prototype: per-block decode theorems for the
type, export, function and code sections, the tiling and framing checks, and
the per-function closure scans, all read through 1 KiB chunk windows."""
import re
import sys

build, wasm = sys.argv[1], sys.argv[2]
BLOCK = int(sys.argv[3]) if len(sys.argv) > 3 else 64
PER_FILE = 24
b = open(wasm, 'rb').read()


def uleb(i):
    r = sh = 0
    while True:
        x = b[i]; i += 1; r |= (x & 0x7f) << sh; sh += 7
        if x < 128:
            return r, i


def sleb(i):
    r = sh = 0
    while True:
        x = b[i]; i += 1; r |= (x & 0x7f) << sh; sh += 7
        if x < 128:
            return i


def valtype(i):
    t = b[i]
    if t in (0x63, 0x64):
        return sleb(i + 1)
    return i + 1


def storage(i):
    if b[i] in (0x77, 0x78):
        return i + 1
    return valtype(i)


def comptype(i):
    t = b[i]; i += 1
    if t == 0x60:
        for _ in range(2):
            n, i = uleb(i)
            for _ in range(n):
                i = valtype(i)
        return i
    if t == 0x5f:
        n, i = uleb(i)
        for _ in range(n):
            i = storage(i) + 1
        return i
    if t == 0x5e:
        return storage(i) + 1
    raise ValueError(hex(t))


def subtype(i):
    if b[i] in (0x50, 0x4f):
        n, i = uleb(i + 1)
        for _ in range(n):
            _, i = uleb(i)
    return comptype(i)


def rectype(i):
    if b[i] == 0x4e:
        n, j = uleb(i + 1)
        subs = []
        for _ in range(n):
            k = subtype(j); subs.append((j, k - j)); j = k
        return j, subs
    return subtype(i), None


secs = []
i = 8
while i < len(b):
    hdr = i
    sz, j = uleb(i + 1)
    secs.append((b[i], hdr, j, sz))
    i = j + sz
sec = {sid: (hdr, j, sz) for sid, hdr, j, sz in secs}


def entries_of(sid, reader):
    _, j, sz = sec[sid]
    n, k = uleb(j)
    out = []
    for _ in range(n):
        e = reader(k); out.append((k, e - k)); k = e
    assert k == j + sz
    return out


groups = []
types = entries_of(1, lambda k: rectype(k)[0])
_, subs = rectype(types[0][0])
group_hdr = types[0]


def export_entry(k):
    n, k = uleb(k); k += n + 1; _, k = uleb(k); return k


exports = entries_of(7, export_entry)
funcs = entries_of(3, lambda k: uleb(k)[1])
code = entries_of(10, lambda k: (lambda s, e: e + s)(*uleb(k)))


def blocks(entries):
    out = []
    for s in range(0, len(entries), BLOCK):
        run = entries[s:s + BLOCK]
        out.append((run[0][0], [l for _, l in run]))
    return out


def lean_list(xs):
    return '[' + ', '.join(str(x) for x in xs) + ']'


HEAD = """import ScaleSections
import ProtoBytes

set_option maxRecDepth 200000
set_option maxHeartbeats 0

namespace AverCert.ProtoStruct
open AverCert CertDecode AverCert.ScaleSections

"""
theorems = []
plan = [('subtype', 'readTypeEntry', subs, None),
        ('type', 'readRecEntry', types[1:], None),
        ('export', 'readExportEntry', exports, None),
        ('func', 'readU', funcs, None),
        ('code', 'AverCert.ByteWindow.readCodeEntry', code, None)]
for name, reader, ents, _ in plan:
    bl = blocks(ents)
    for bi, (start, ls) in enumerate(bl):
        theorems.append(f'theorem {name}_b{bi} : blockOk {reader} ProtoBytes.chunks {start} {lean_list(ls)} = true := by\n  decide +kernel\n')
    first = ents[0][0]
    last = ents[-1][0] + ents[-1][1]
    tl = '[' + ', '.join(f'({s}, {lean_list(ls)})' for s, ls in bl) + ']'
    theorems.append(f'theorem {name}_tiled : tiled {first} {last} {tl} = true := by\n  decide +kernel\n')

hdrs = lean_list([h for _, h, _, _ in secs])
theorems.append(f'theorem framing : headerOk ProtoBytes.chunks && framingOk ProtoBytes.chunks {len(b)} 8 {hdrs} = true := by\n  decide +kernel\n')

# closure: every admitted function scanned on its own window
data = open(f'{build}/ArtifactData.lean').read()
m = re.search(r'closureClaim := ⟨\[(.*?)\], \[(.*?)\], \[(.*?)\]⟩', data, re.S)
admitted = [int(x) for x in m.group(3).split(',') if x.strip()]
for s in range(0, len(admitted), BLOCK):
    run = admitted[s:s + BLOCK]
    conj = ' && '.join(f'(calleesAt ProtoBytes.chunks Artifact.layout {f}).isSome' for f in run)
    theorems.append(f'theorem closure_b{s // BLOCK} : ({conj}) = true := by\n  decide +kernel\n')

names = []
for k in range(0, len(theorems), PER_FILE):
    nm = f'ProtoStruct{k // PER_FILE}'
    body = HEAD + ('import ArtifactLayout\n' if False else '')
    txt = HEAD.replace('import ProtoBytes\n', 'import ProtoBytes\nimport ArtifactLayout\n') + '\n'.join(theorems[k:k + PER_FILE]) + '\nend AverCert.ProtoStruct\n'
    open(f'{build}/{nm}.lean', 'w').write(txt)
    names.append(nm)
print('subtypes', len(subs), 'types', len(types), 'exports', len(exports), 'funcs', len(funcs), 'code', len(code), 'admitted', len(admitted))
print('theorems', len(theorems))
print(' '.join(names))
