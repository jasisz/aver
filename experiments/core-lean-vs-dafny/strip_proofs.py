#!/usr/bin/env python3
"""Turn an `aver proof` Lean export into a model that builds fast and cannot
fail: every top-level theorem keeps its statement and gets `sorry` as its
proof, and every `example` is dropped. Definitions, termination proofs,
instances and the prelude files are left exactly as exported.

    strip_proofs.py <export-dir>

The law files in this experiment import the stripped modules and prove the
exported statements themselves, so nothing the export tried to prove can
break (or help) them.
"""
import pathlib
import re
import sys

KEEP = {"AverCommon.lean", "lakefile.lean", "Bytes.lean", "Crypto.lean"}
THEOREM = re.compile(r"^(@\[[^\]]*\]\s*)?(private |protected )?theorem\s")
PROOF_BY = re.compile(r":=\s*by\b")
PROOF_TERM = re.compile(r":=[ \t]*\n")


def blocks(lines):
    """Split into top-level blocks: a block starts at a column-0 line that is
    not inside a block comment."""
    out, cur, depth = [], [], 0
    for line in lines:
        starts = depth == 0 and line[:1] not in ("", " ", "\t")
        if starts and cur:
            out.append(cur)
            cur = []
        cur.append(line)
        depth += line.count("/-") - line.count("-/")
        depth = max(depth, 0)
    if cur:
        out.append(cur)
    return out


def strip(text):
    result = []
    stats = {"theorems": 0, "examples": 0, "untouched": 0}
    for blk in blocks(text.split("\n")):
        head = blk[0]
        if head.startswith("example"):
            while result and result[-1][0].startswith("set_option") and result[-1][0].rstrip().endswith(" in"):
                result.pop()
            stats["examples"] += 1
            continue
        if THEOREM.match(head):
            body = "\n".join(blk)
            m = PROOF_BY.search(body) or PROOF_TERM.search(body)
            if m:
                result.append((body[: m.start()].rstrip() + " := by sorry").split("\n"))
                stats["theorems"] += 1
                continue
            stats["untouched"] += 1
        result.append(blk)
    return "\n".join(line for blk in result for line in blk) + "\n", stats


def main():
    root = pathlib.Path(sys.argv[1])
    total = {"theorems": 0, "examples": 0, "untouched": 0}
    for path in sorted(root.rglob("*.lean")):
        if ".lake" in path.parts or path.name in KEEP or "Crypto" in path.parts:
            continue
        new, stats = strip(path.read_text())
        path.write_text(new)
        for k in total:
            total[k] += stats[k]
        print(f"{path.relative_to(root)}: {stats}")
    print(f"total: {total}")


if __name__ == "__main__":
    main()
