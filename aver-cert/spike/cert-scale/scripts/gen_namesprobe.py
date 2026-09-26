"""Probe why declared-name blocks cost more than certified ones."""
import re
import sys
build = sys.argv[1]
man = open(f'{build}/Manifest.lean').read()
m = re.search(r'def Plans\.subject_declaredUncertified_0 : List \(String × String\) :=\n(.*?)\n\n', man, re.S)
names = re.findall(r'\("([^"]*)", "', m.group(1))


def key(bs):
    return int.from_bytes(bs, 'little') * 65536 + len(bs)


keys = ', '.join(str(key(n.encode())) for n in names)
lit = ', '.join(f'"{n}"' for n in names)
pairs = ', '.join(f'("{n}", "r")' for n in names)
with open(f'{build}/ProtoNames.lean', 'w') as f:
    f.write('import ScaleSections\nimport Manifest\nset_option maxRecDepth 200000\nset_option maxHeartbeats 0\nopen AverCert.ScaleSections\n')
    f.write(f'theorem viaPlans : namesBlock (AverCert.Plans.subject_declaredUncertified_0.map (·.1)) [{keys}] = true := by decide +kernel\n')
    f.write(f'theorem viaLit : namesBlock [{lit}] [{keys}] = true := by decide +kernel\n')
    f.write(f'theorem viaPairs : namesBlock (([{pairs}] : List (String × String)).map (·.1)) [{keys}] = true := by decide +kernel\n')
    f.write('theorem lenOnly : (AverCert.Plans.subject_declaredUncertified_0.map (·.1)).length = 64 := by decide +kernel\n')
print(len(names))
