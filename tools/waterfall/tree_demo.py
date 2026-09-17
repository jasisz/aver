#!/usr/bin/env python3
"""Check the tree proof snippets against an existing, built Aver export.

By default only replay is checked, with no waterfall import or search path.
Pass --waterfall to also rediscover the proofs and print Lean's suggestions.
"""

import argparse
import json
import os
from pathlib import Path

from run import ALLOWED_AXIOMS, audit, declarations, run


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--export", required=True, type=Path)
    parser.add_argument("--waterfall", type=Path)
    args = parser.parse_args()
    exported = args.export.resolve(strict=True)
    original = (exported / "WaterfallTree.lean").read_text()
    prefix = original.split("-- verify", 1)[0]
    expected = [(name, statement) for name, _, _, statement in declarations(original)]
    results = {}
    for mode in (["search", "replay"] if args.waterfall else ["replay"]):
        snippet = Path(__file__).with_name(f"tree_{mode}.lean.inc").read_text()
        for name, statement in expected:
            if f"theorem {name} : {statement} := by" not in snippet:
                raise ValueError("Proof snippet does not match the exported statement")
        file = exported / f"Tree{mode.title()}.lean"
        file.write_text(("import waterfall\n" if mode == "search" else "")
                        + prefix + snippet + "\nend WaterfallTree\n")
        env = dict(os.environ)
        env.pop("LEAN_PATH", None)
        if mode == "search":
            env["LEAN_PATH"] = str(args.waterfall.resolve() / ".lake/build/lib/lean")
        command = ["lake", "env", "lean", file.name]
        if mode == "replay":
            command += ["-o", ".lake/build/lib/lean/TreeReplay.olean"]
        status, text = run(command, exported,
                           exported.parent / f"tree-{mode}.log", 60, env)
        axioms = {name: audit(text, "WaterfallTree." + name) for name, _ in expected}
        accepted = status["exit_code"] == 0 and all(
            value is not None and set(value) <= ALLOWED_AXIOMS for value in axioms.values())
        results[mode] = {**status, "axioms": axioms, "accepted": accepted}
        print(text, end="")
        if mode == "replay" and accepted:
            fresh, text = run(["lake", "env", "leanchecker", "--fresh", "TreeReplay"],
                              exported, exported.parent / "tree-fresh.log", 120, env)
            results["fresh"] = {**fresh, "accepted": fresh["exit_code"] == 0}
            print(text, end="")
    (exported.parent / "tree-demo.json").write_text(json.dumps(results, indent=2) + "\n")
    if not all(result["accepted"] for result in results.values()):
        raise SystemExit("Tree proof search/replay failed or did not pass the axiom audit")


if __name__ == "__main__":
    main()
