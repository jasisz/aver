"""Split the theorems of a kept certificate module into one file each.

Each split file imports the original module (so every definition and earlier
theorem is available as an .olean) and re-proves ONE theorem under a fresh
name, so `lean --profile` and `/usr/bin/time -l` measure that declaration
alone. Prints the split module names."""
import re
import sys

build, module = sys.argv[1], sys.argv[2]
src = open(f'{build}/{module}.lean').read()
first = src.index('\ntheorem ')
pre = src[:first]
keep = [l for l in pre.splitlines() if l.startswith(('set_option', 'namespace', 'open '))]
body = src[first + 1:]
end = body.rfind('\nend ')
ns_end = body[end + 1:].strip()
body = body[:end]
blocks = re.split(r'\n(?=theorem )', body)
for block in blocks:
    name = re.match(r'theorem (\S+)', block).group(1)
    renamed = block.replace(f'theorem {name}', f'theorem split_{name}', 1)
    out = f'Split_{module}_{name}'
    with open(f'{build}/{out}.lean', 'w') as f:
        f.write(f'import {module}\n\n' + '\n'.join(keep) + '\n\n' + renamed.rstrip() + '\n\n' + ns_end + '\n')
    print(out)
