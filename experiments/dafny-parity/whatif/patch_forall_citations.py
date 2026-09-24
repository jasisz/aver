#!/usr/bin/env python3
"""What-if: cite a law at concrete terms instead of through a `forall`.

usage: patch_forall_citations.py <file.dfy>   (rewrites the file in place)

The exporter cites an earlier law inside a lemma as

    forall k: int ensures (pow2(k) >= 1) == true { pow2_positive(k); }

Z3 then has to instantiate that quantifier itself, and on the K5 Fprep
lemmas it does not (the goal only mentions pow2 after unfolding
pow2Signed, at a different fuel layer). This rewrite replaces each such
line with direct calls of the cited lemma at the lemma's own integer
parameters, their negations, and pairwise sums and their negations -- the
terms that appear once pow2Signed and the homomorphism are unfolded. It is
a measurement of what the codegen change would buy, not the change.
"""
import itertools
import re
import sys

path = sys.argv[1]
lines = open(path, encoding="utf-8").read().split("\n")
out = []
params = []
patched = 0
for line in lines:
    m = re.match(r"\s*lemma\b.*?([A-Za-z_]\w*)\(([^)]*)\)\s*$", line)
    if m:
        params = [p.split(":")[0].strip() for p in m.group(2).split(",") if p.strip().endswith("int")]
    f = re.match(r"(\s*)forall (\w+): int ensures .* \{ (\w+)\(\2\); \}\s*$", line)
    if f and params:
        indent, _, lemma = f.groups()
        terms = []
        for p in params:
            terms += [p, f"0 - {p}"]
        for a, b in itertools.combinations(params, 2):
            terms += [f"{a} + {b}", f"0 - ({a} + {b})"]
        out.append(indent + " ".join(f"{lemma}({t});" for t in terms))
        patched += 1
        continue
    out.append(line)
open(path, "w", encoding="utf-8").write("\n".join(out))
print(f"patched {patched} forall citations in {path}")
