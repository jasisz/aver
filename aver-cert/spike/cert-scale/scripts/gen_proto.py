"""Generate the cert-scale prototype Lean files for one kept build dir.

ProtoBytes.lean   the module as 1 KiB chunks (what the checker would render)
ProtoCode<k>.lean one `decide +kernel` theorem per planned function:
                  its lowering packed == the chunk window of its code entry
ProtoOld<k>.lean  the same per-function code equality the current wall
                  decides (List Nat entry read off the whole-module numeral)
"""
import re
import sys

build, wasm = sys.argv[1], sys.argv[2]
per_file = int(sys.argv[3]) if len(sys.argv) > 3 else 100
W = 1024

b = open(wasm, 'rb').read()
chunks = [b[i:i + W] for i in range(0, len(b), W)]
with open(f'{build}/ProtoBytes.lean', 'w') as f:
    f.write('import ScaleBytes\n\nnamespace AverCert.ProtoBytes\n\n')
    f.write('/-- The module bytes as little-endian 1 KiB chunks. -/\ndef chunks : List Nat :=\n  [')
    f.write(',\n   '.join('0x' + (c[::-1].hex() or '0') for c in chunks))
    f.write(']\n\n')
    f.write(f'def modLen : Nat := {len(b)}\n\n')
    f.write('theorem chunks_fit : AverCert.ScaleBytes.chunksFit 1024 chunks = true := by decide +kernel\n\n')
    f.write('end AverCert.ProtoBytes\n')

plans = open(f'{build}/Plans.lean').read()
entries = re.findall(r'⟨"([^"]*)", (true|false), (\d+), (\d+), (fn\d+)⟩', plans)
print('plans', len(entries))

with open(f'{build}/ProtoCtx.lean', 'w') as f:
    f.write("""import ScaleBytes
import ArtifactLayout
import Manifest

namespace AverCert.Proto
/-- The lowering context every plan check uses. -/
abbrev protoM : AverCert.Grammar.MCtx :=
  AverCert.TypeTable.mctxOf AverCert.manifest.subject AverCert.Plans.types AverCert.Plans.fnPlans
end AverCert.Proto
""")

HEAD = """import ScaleBytes
import ProtoBytes
import ProtoCtx

set_option maxRecDepth 200000
set_option maxHeartbeats 1600000

namespace AverCert.Proto
open AverCert AverCert.Schema AverCert.ScaleBytes

"""
nfiles = 0
for k in range(0, len(entries), per_file):
    part = entries[k:k + per_file]
    with open(f'{build}/ProtoCode{k}.lean', 'w') as f:
        f.write(HEAD)
        for (_, _, idx, _, fn) in part:
            f.write(f'theorem code_{idx} : codePacked ProtoBytes.chunks Artifact.layout protoM {idx} Plans.{fn} = true := by\n  decide +kernel\n\n')
        f.write('end AverCert.Proto\n')
    with open(f'{build}/ProtoOld{k}.lean', 'w') as f:
        f.write(HEAD)
        for (_, _, idx, _, fn) in part:
            f.write(f'theorem old_{idx} : codeOld ArtifactBytes.modBytes Artifact.layout protoM {idx} Plans.{fn} = true := by\n  decide +kernel\n\n')
        f.write('end AverCert.Proto\n')
    nfiles += 1
print('files', nfiles)
