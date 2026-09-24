#!/usr/bin/env python3
"""Remove the laws whose Lean theorems broke the build, so the rest get audited.

usage: strip_laws.py <module-root copy> <lake.log> <lean export dir>

One hard error (an `unsolved goals`, a failed `rewrite`, a heartbeat timeout)
fails `lake build`, and then the audit records nothing and the
speculative-universal probe keeps every candidate bounded: one law takes the
verdict of all the others with it. This finds the law owning each error line
through the `-- aver:law-class` marker above it, deletes that `verify ... law`
block from the source copy and prints the removed labels, one per line.
"""
import os
import re
import sys

root, lake_log, lean_dir = sys.argv[1:4]
log = open(lake_log, encoding="utf-8", errors="replace").read()
labels = set()
for mm in re.finditer(r"(?m)^error: (\S+\.lean):(\d+):\d+:", log):
    path, line = os.path.join(lean_dir, mm.group(1)), int(mm.group(2))
    try:
        lines = open(path, encoding="utf-8").read().splitlines()
    except OSError:
        continue
    for i in range(min(line, len(lines)) - 1, -1, -1):
        m = re.match(r"-- aver:law-class \S+ \S+ (\S+)", lines[i])
        if m:
            labels.add(m.group(1))
            break

for label in sorted(labels):
    parts = label.split(".")
    fn, law = parts[-2], parts[-1]
    module = parts[:-2]
    candidates = []
    for dirpath, _, files in os.walk(root):
        for f in files:
            if f.endswith(".av"):
                candidates.append(os.path.join(dirpath, f))
    if module:
        want = "/".join(module).lower() + ".av"
        candidates = [c for c in candidates if os.path.relpath(c, root).lower() == want] or candidates
    for c in candidates:
        src = open(c, encoding="utf-8").read().split("\n")
        out, skip, hit = [], False, False
        for ln in src:
            if re.match(rf"^verify\s+{re.escape(fn)}\s+law\s+{re.escape(law)}\s*$", ln):
                skip, hit = True, True
                continue
            if skip and (ln.startswith((" ", "\t")) or ln.strip() == ""):
                continue
            skip = False
            out.append(ln)
        if hit:
            open(c, "w", encoding="utf-8").write("\n".join(out))
            print(label)
            break
