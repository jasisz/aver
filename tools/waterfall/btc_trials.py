#!/usr/bin/env python3
"""Try isolated btc-listener propositions, including stronger bounded-law goals.

For a bounded statement, only the generated sample-membership premises are
removed. The source's `when` premises remain. These are stronger candidate
theorems, not a claim that the original export was universal. Explanation
obligations are reported individually, never silently bypassed.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

from run import ALLOWED_AXIOMS, TOOLCHAIN, WATERFALL_REV, audit, digest, run


def header(text, name):
    match = re.search(r"(?ms)^theorem " + re.escape(name) + r" : (.*?) := by\n", text)
    if not match:
        parts = list(re.finditer(r"(?ms)^theorem " + re.escape(name)
                                 + r"_part\d+ : (.*?) := by\n", text))
        if not parts:
            raise ValueError(f"No exact theorem header for {name}")
        stronger = {generalize_samples(part.group(1))[0] for part in parts}
        if len(stronger) != 1:
            raise ValueError("Partitioned statements disagree after removing samples")
        match = parts[0]
    return match, match.group(1)


def generalize_samples(statement):
    binders, body = statement.split(", ", 1)
    names = re.findall(r"\(([^\s()]+) : ", binders)
    premises = body.split(" -> ")
    if not names or len(premises) <= len(names):
        raise ValueError("Cannot identify sample premises")
    for name, premise in zip(names, premises):
        if not premise.startswith(name + " = "):
            raise ValueError("Refusing to remove a premise that is not sample membership")
    return binders + ", " + " -> ".join(premises[len(names):]), premises[:len(names)]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--export", required=True, type=Path)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--module", required=True)
    parser.add_argument("--waterfall", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--law", action="append", help="Exact exported law/obligation name")
    parser.add_argument("--mode", choices=["search", "committed"], default="search")
    parser.add_argument("--effort", type=int, default=1000)
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument("--hint", action="append", default=[], help="Explicit imported lemma")
    parser.add_argument("--local-lemma", action="append", default=[],
                        help="Copy one already audited local helper proof from the baseline")
    parser.add_argument("--suggest", action="store_true")
    args = parser.parse_args()
    exported = args.export.resolve(strict=True)
    source = args.source.resolve(strict=True)
    waterfall = args.waterfall.resolve(strict=True)
    if subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=waterfall,
                               text=True).strip() != WATERFALL_REV:
        raise ValueError("Waterfall revision does not match the experiment")
    if (exported / "lean-toolchain").read_text().strip() != TOOLCHAIN:
        raise ValueError("Unexpected exported toolchain")
    output = args.out.resolve()
    output.mkdir(parents=True, exist_ok=False)
    module_file = exported / (args.module.replace(".", "/") + ".lean")
    text = module_file.read_text()
    prefix = text.split("-- verify", 1)[0]
    namespace = re.findall(r"(?m)^namespace (\S+)$", prefix)
    if namespace != [args.module] or "-- aver:law-" in prefix:
        raise ValueError("Unexpected namespace or theorem in definition prefix")
    markers = re.findall(r"(?m)^-- aver:law-(class|obligation) (\S+) (\S+) (\S+)$", text)
    selected = [entry for entry in markers if not args.law or entry[3] in args.law]
    if args.law and set(args.law) != {entry[3] for entry in selected}:
        raise ValueError("Requested law/obligation not present")
    manifest = json.loads((exported / "proof_manifest.json").read_text())
    baseline = {entry["law"]: entry for key in ["laws", "obligations"]
                for entry in manifest.get(key, [])}
    support = ""
    for name in args.local_lemma:
        admitted = [entry for entry in baseline.values()
                    if entry["theorem"] == args.module + "." + name]
        if (len(admitted) != 1 or admitted[0]["tier"] != "universal"
                or not set(admitted[0]["axioms"]) <= ALLOWED_AXIOMS
                or any(entry[1] == name for entry in selected)):
            raise ValueError("Local support must be an audited, different theorem")
        match, _ = header(text, name)
        following = re.search(r"(?m)^\S", text[match.end():])
        end = match.end() + following.start() if following else len(text)
        support += text[match.start():end] + "\n"
    report = {"source_sha": subprocess.check_output(["git", "rev-parse", "HEAD"],
                                                    cwd=source, text=True).strip(),
              "waterfall_sha": WATERFALL_REV, "toolchain": TOOLCHAIN,
              "module": args.module, "export_sha256": digest(module_file),
              "mode": args.mode, "effort": args.effort, "timeout_seconds": args.timeout,
              "max_heartbeats": 1000000, "hints": args.hint,
              "local_lemmas": args.local_lemma, "trials": []}
    env = dict(os.environ)
    env["LEAN_PATH"] = str(waterfall / ".lake/build/lib/lean")
    for index, (category, name, kind, law) in enumerate(selected):
        match, original = header(text, name)
        statement, removed = (generalize_samples(original) if kind == "bounded-domain"
                              else (original, []))
        row = {"law": law, "category": category, "baseline": baseline.get(law),
               "export_kind": kind, "original_statement": original,
               "statement": statement, "removed_sample_premises": removed}
        stem = "BtcTrial_" + output.name.replace("-", "_") + "_" + str(index)
        file = exported / (stem + ".lean")
        command = "waterfall?" if args.suggest else "waterfall"
        rules = args.hint + args.local_lemma
        hints = " [" + ", ".join(rules) + "]" if rules else ""
        file.write_text(
            "import waterfall\n" + prefix + support + "\nset_option maxHeartbeats 1000000 in\n"
            + f"theorem {name} : {statement} := by\n"
            + f"  {command} (mode := .{args.mode}) (effort := {args.effort}) (report := true){hints}\n"
            + f"\n#print axioms {name}\nend {args.module}\n")
        # Retain the exact test input with the report, including its unchanged
        # definitions. It imports dependencies, never the original target module.
        (output / (stem + ".lean")).write_text(file.read_text())
        status, log = run(["lake", "env", "lean", file.name], exported,
                          output / (stem + ".log"), args.timeout, env)
        axioms = audit(log, args.module + "." + name)
        accepted = (status["exit_code"] == 0 and axioms is not None
                    and set(axioms) <= ALLOWED_AXIOMS)
        row.update({**status, "axioms": axioms, "accepted": accepted})
        report["trials"].append(row)
        (output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps({key: value for key, value in row.items()
                          if key not in ["statement", "original_statement", "removed_sample_premises"]}),
              flush=True)
    if digest(module_file) != report["export_sha256"]:
        raise SystemExit("Export changed during experiment; discard results")


if __name__ == "__main__":
    main()
