"""Sum lake build times of an aver-cert check log by module group, and join
peak RSS per lean process from samples.log in build order."""
import re
import sys
from collections import defaultdict

log = open(sys.argv[1]).read()
rows = re.findall(r'Built (\S+) \(([\d.]+)(ms|s)\)', log)
groups = defaultdict(float)
per = []
for name, t, unit in rows:
    secs = float(t) / (1000 if unit == 'ms' else 1)
    per.append((name, secs))
    if name.startswith('AverModel'):
        g = 'source model (AverModel.*)'
    elif name.startswith('Bridge'):
        g = 'bridges (Bridge*)'
    elif name.startswith('ArtifactPlans'):
        g = 'plan chunks (ArtifactPlans*)'
    elif name.startswith('Artifact'):
        g = 'byte facts (Artifact*, other)'
    elif name in ('Laws',):
        g = 'laws'
    elif name in ('Plans', 'Manifest', 'ArtifactBytes', 'ArtifactComponentBytes', 'Module'):
        g = 'package data'
    else:
        g = 'wall + other'
    groups[g] += secs
for g, s in sorted(groups.items(), key=lambda x: -x[1]):
    print(f'{g:34s} {s:8.1f}s')
print('total', round(sum(groups.values()), 1))
print()
for name, s in sorted(per, key=lambda x: -x[1])[:25]:
    print(f'{name:28s} {s:7.1f}s')
