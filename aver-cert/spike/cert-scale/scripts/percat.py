"""Per-category kernel time of the generated prototype files: pair each
theorem (in file order) with its `type checking took` profile line."""
import re
import sys
from collections import defaultdict

build, prof = sys.argv[1], sys.argv[2]
mods = sys.argv[3:]
cat = defaultdict(float)
cnt = defaultdict(int)
mx = defaultdict(float)
for m in mods:
    names = re.findall(r'^theorem (\S+)', open(f'{build}/{m}.lean').read(), re.M)
    times = []
    for t, u in re.findall(r'type checking took ([\d.]+)(ms|s)', open(f'{prof}/{m}.log').read()):
        times.append(float(t) / (1000 if u == 'ms' else 1))
    if len(times) != len(names):
        print('mismatch', m, len(times), len(names))
    for n, t in zip(names, times):
        c = re.sub(r'_b?\d+$', '', n)
        cat[c] += t; cnt[c] += 1; mx[c] = max(mx[c], t)
for c in sorted(cat, key=lambda c: -cat[c]):
    print(f'{c:12s} decls={cnt[c]:4d} kernel={cat[c]:7.2f}s max={mx[c]:.2f}s')
print('total', round(sum(cat.values()), 1))
