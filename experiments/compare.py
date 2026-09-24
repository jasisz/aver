#!/usr/bin/env python3
"""Compare the per-law Lean tiers of two aver builds.

    compare.py <verdicts-dir>

<verdicts-dir>/<target>-<side>/ holds `check.json` (the `--check-json` line)
and `proof_manifest.json` for side `main` and side `branch`. Prints a
markdown report: the summary counters per target and side, then per target
every law whose tier changed, with the laws universal on main that are not
universal on the branch listed first.
"""
import json
import pathlib
import sys

root = pathlib.Path(sys.argv[1])
targets = sorted({p.name.rsplit("-", 1)[0] for p in root.iterdir() if p.is_dir()})


def load(target, side):
    d = root / f"{target}-{side}"
    check, laws = {}, {}
    try:
        lines = [l for l in (d / "check.json").read_text().splitlines() if l.startswith("{")]
        check = json.loads(lines[-1]) if lines else {}
    except (OSError, ValueError):
        pass
    try:
        manifest = json.loads((d / "proof_manifest.json").read_text())
        laws = {l["law"]: l["tier"] for l in manifest.get("laws", [])}
    except (OSError, ValueError):
        pass
    status = ""
    try:
        status = (d / "status.txt").read_text().replace("\n", " ").strip()
    except OSError:
        pass
    sha = ""
    try:
        sha = (d / "aver-sha.txt").read_text().strip()[:8]
    except OSError:
        pass
    return check, laws, status, sha


print("| target | side | aver | universal_laws | bounded_laws | sorries | build_errors | isolated | manifest universal | status |")
print("|---|---|---|---:|---:|---:|---:|---:|---:|---|")
data = {}
for t in targets:
    for side in ("main", "branch"):
        check, laws, status, sha = load(t, side)
        data[(t, side)] = laws
        uni = sum(1 for v in laws.values() if v == "universal")
        print(f"| {t} | {side} | {sha} | {check.get('universal_laws', '')} | {check.get('bounded_laws', '')} | "
              f"{check.get('sorries', '')} | {check.get('build_errors', '')} | "
              f"{len(check.get('isolated_errors', []))} | {uni} | {status} |")

for t in targets:
    before, after = data[(t, "main")], data[(t, "branch")]
    regressed = sorted(l for l, v in before.items() if v == "universal" and after.get(l) != "universal")
    gained = sorted(l for l, v in after.items() if v == "universal" and before.get(l) != "universal")
    changed = sorted(l for l in set(before) | set(after)
                     if before.get(l) != after.get(l) and l not in regressed and l not in gained)
    print(f"\n### {t}\n")
    print(f"universal on main and not on the branch: **{len(regressed)}**")
    for l in regressed:
        print(f"- {l}: {before.get(l)} -> {after.get(l)}")
    print(f"\nuniversal on the branch and not on main: **{len(gained)}**")
    for l in gained:
        print(f"- {l}: {before.get(l)} -> {after.get(l)}")
    if changed:
        print("\nother tier changes:")
        for l in changed:
            print(f"- {l}: {before.get(l)} -> {after.get(l)}")
