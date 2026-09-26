"""Probe: name Strings tied to packed byte numerals by definitional unfolding
(`String.ofList (unpackChars key len)`) instead of by String.toList."""
import re
import sys
build = sys.argv[1]
man = open(f'{build}/Manifest.lean').read()
out = ['import ScaleSections', 'import Manifest', 'set_option maxRecDepth 200000',
       'set_option maxHeartbeats 0', '',
       'def unpackChars : Nat → Nat → List Char',
       '  | 0, _ => []',
       '  | k + 1, x => Char.ofNat (x % 256) :: unpackChars k (x / 256)', '']
blocks = re.findall(r'def Plans\.subject_exports_(\d+) : List \(String\) :=\n(.*?)\n\n', man, re.S)
for idx, body in blocks[:4]:
    names = re.findall(r'"([^"]*)"', body)
    rhs = ', '.join(f'String.ofList (unpackChars {len(n)} {int.from_bytes(n.encode(), "little")})' for n in names)
    out.append(f'theorem pin_rfl_{idx} : AverCert.Plans.subject_exports_{idx} = [{rhs}] := rfl')
    out.append(f'theorem pin_kernel_{idx} : AverCert.Plans.subject_exports_{idx} = [{rhs}] := by decide +kernel')
open(f'{build}/ProtoNames2.lean', 'w').write('\n'.join(out) + '\n')
