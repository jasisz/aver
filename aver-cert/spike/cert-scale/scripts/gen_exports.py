"""Export accounting prototype: per-block export entries against declared
name keys, per-64 manifest name runs against the same keys, and one sorted
distinctness check over all keys."""
import re
import sys

build, wasm = sys.argv[1], sys.argv[2]
BLOCK = 64
b = open(wasm, 'rb').read()


def uleb(i):
    r = sh = 0
    while True:
        x = b[i]; i += 1; r |= (x & 0x7f) << sh; sh += 7
        if x < 128:
            return r, i


i = 8; sec = {}
while i < len(b):
    sz, j = uleb(i + 1); sec.setdefault(b[i], (j, sz)); i = j + sz
j, sz = sec[7]
n, k = uleb(j)
ents = []
for _ in range(n):
    s = k
    ln, k = uleb(k); name = b[k:k + ln]; k += ln
    kind = b[k]; k += 1
    idx, k = uleb(k)
    ents.append((s, k - s, name, kind, idx))


def key(bs):
    return int.from_bytes(bs, 'little') * 65536 + len(bs)


man = open(f'{build}/Manifest.lean').read()
thms = []
for s in range(0, len(ents), BLOCK):
    run = ents[s:s + BLOCK]
    ks = ', '.join(f'({key(e[2])}, {e[3]}, {e[4]})' for e in run)
    ls = ', '.join(str(e[1]) for e in run)
    thms.append(f'theorem exp_b{s // BLOCK} : exportBlock ProtoBytes.chunks {run[0][0]} [{ls}] [{ks}] = true := by\n  decide +kernel\n')
for kind, pat, proj in [('certified', r'def Plans\.subject_exports_(\d+) : List \(String\) :=\n(.*?)\n\n', ''),
                        ('declared', r'def Plans\.subject_declaredUncertified_(\d+) : List \(String × String\) :=\n(.*?)\n\n', '.map (·.1)')]:
    for m in re.finditer(pat, man, re.S):
        idx, body = m.group(1), m.group(2)
        if kind == 'certified':
            names = re.findall(r'"([^"]*)"', body)
        else:
            names = re.findall(r'\("([^"]*)", "', body)
        keys = ', '.join(str(key(nm.encode())) for nm in names)
        lst = 'subject_exports' if kind == 'certified' else 'subject_declaredUncertified'
        thms.append(f'theorem names_{kind}_{idx} : namesBlock (AverCert.Plans.{lst}_{idx}{proj}) [{keys}] = true := by\n  decide +kernel\n')
allkeys = ', '.join(str(key(e[2])) for e in ents)
thms.append(f'theorem exp_distinct : AverCert.SortedKeys.strictly (AverCert.SortedKeys.msort [{allkeys}]) = true := by\n  decide +kernel\n')

HEAD = """import ScaleSections
import ProtoBytes
import Manifest
import SortedKeys

set_option maxRecDepth 200000
set_option maxHeartbeats 0

namespace AverCert.ProtoExp
open AverCert CertDecode AverCert.ScaleSections

"""
names = []
PER = 40
for k in range(0, len(thms), PER):
    nm = f'ProtoExp{k // PER}'
    open(f'{build}/{nm}.lean', 'w').write(HEAD + '\n'.join(thms[k:k + PER]) + '\nend AverCert.ProtoExp\n')
    names.append(nm)
print(len(ents), 'exports', len(thms), 'theorems')
print(' '.join(names))
