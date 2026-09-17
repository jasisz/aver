#!/usr/bin/env python3
"""Check a retained btc-listener proof script without loading waterfall."""

import argparse
import json
import os
from pathlib import Path
import re

from btc_trials import generalize_samples, header
from run import ALLOWED_AXIOMS, audit, run


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--export", required=True, type=Path)
    parser.add_argument("--module", required=True)
    parser.add_argument("--snippet", required=True, type=Path)
    args = parser.parse_args()
    exported = args.export.resolve(strict=True)
    text = (exported / (args.module.replace(".", "/") + ".lean")).read_text()
    snippet = args.snippet.read_text()
    names = re.findall(r"(?m)^theorem (\S+) :", snippet)
    if not names or "waterfall" in re.sub(r"(?m)--.*$", "", snippet):
        raise ValueError("Expected a nonempty ordinary Lean proof snippet")
    for name in names:
        _, original = header(text, name)
        marker = re.search(r"(?m)^-- aver:law-class " + re.escape(name) + r" (\S+)", text)
        if not marker:
            raise ValueError("Unknown exported law")
        expected = generalize_samples(original)[0] if marker.group(1) == "bounded-domain" else original
        _, actual = header(snippet, name)
        if actual != expected:
            raise ValueError("Retained statement differs from the original guarded claim")
    stem = "BtcReplay" + args.module.replace(".", "")
    file = exported / (stem + ".lean")
    file.write_text(text.split("-- verify", 1)[0] + snippet + "\nend " + args.module + "\n")
    env = dict(os.environ)
    env.pop("LEAN_PATH", None)
    status, log = run(["lake", "env", "lean", file.name, "-o",
                       f".lake/build/lib/lean/{stem}.olean"], exported,
                      exported / (stem + ".log"), 120, env)
    axioms = {name: audit(log, args.module + "." + name) for name in names}
    accepted = status["exit_code"] == 0 and all(
        value is not None and set(value) <= ALLOWED_AXIOMS for value in axioms.values())
    result = {"module": args.module, "replay": {**status, "axioms": axioms, "accepted": accepted}}
    if accepted:
        fresh, _ = run(["lake", "env", "leanchecker", "--fresh", stem], exported,
                       exported / (stem + "-fresh.log"), 180, env)
        result["fresh"] = {**fresh, "accepted": fresh["exit_code"] == 0}
    (exported / (stem + ".json")).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result), flush=True)
    if not accepted or not result["fresh"]["accepted"]:
        raise SystemExit("Retained proof failed replay or the fresh kernel check")


if __name__ == "__main__":
    main()
