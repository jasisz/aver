"""Split CheckerWitness.lean: each decided report pin alone, and the rest
(bridge, law and rfl pins) together."""
import re
import sys

build = sys.argv[1]
src = open(f'{build}/CheckerWitness.lean').read()
lines = src.split('\n')
imports = [l for l in lines if l.startswith('import ')]
body = '\n'.join(l for l in lines if not l.startswith('import ') and not l.startswith('--'))
# declarations start at 'def ', 'theorem ' or 'set_option ... in'
decls = re.split(r'\n(?=(?:def |theorem |set_option maxHeartbeats \d+ in\n))', '\n' + body)
decided, rest = [], []
for d in decls:
    if 'decide +kernel' in d:
        decided.append(d)
    elif d.strip():
        rest.append(d)
for d in decided:
    name = re.search(r'theorem _root_\.AverCertChecker\.(\S+)', d).group(1)
    with open(f'{build}/Wit_{name}.lean', 'w') as f:
        f.write('\n'.join(imports) + '\n' + d.strip() + '\n')
    print(f'Wit_{name}', end=' ')
with open(f'{build}/Wit_rest.lean', 'w') as f:
    f.write('\n'.join(imports) + '\n' + '\n'.join(r.strip('\n') for r in rest) + '\n')
print('Wit_rest')
