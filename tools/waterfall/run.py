#!/usr/bin/env python3
"""Compare Aver's emitted proofs with isolated waterfall trials on the same statements.

This is an experiment, not a compiler feature. Each waterfall trial keeps only
the export's definitions and ONE original law statement. No baseline law or
sample theorem is in scope. A successful trial must pass an axiom audit.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
WATERFALL_REV = "e04ea93b678c831c404067dd86573b821bc528e4"
TOOLCHAIN = "leanprover/lean4:v4.34.0"
ALLOWED_AXIOMS = {"propext", "Classical.choice", "Quot.sound"}
CASES = {
    "list_fold": "tests/fixtures/source_recursion/list_fold.av",
    "reverse_algebra": "tests/fixtures/source_recursion/reverse_algebra.av",
    "floor_digits": "tests/fixtures/source_recursion/floor_digits.av",
    "roundtrip": "tests/fixtures/source_recursion/roundtrip.av",
    "false_accumulator": "tests/fixtures/source_recursion/false_accumulator.av",
    "tree": "tools/waterfall/tree.av",
}


def run(command, cwd, log, timeout, env=None):
    start = time.monotonic()
    process = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT, start_new_session=True)
    timed_out = False
    try:
        output, _ = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        os.killpg(process.pid, signal.SIGKILL)
        output, _ = process.communicate()
    text = output.decode(errors="replace")
    log.write_text(text)
    return {"exit_code": process.returncode, "timeout": timed_out,
            "seconds": round(time.monotonic() - start, 3)}, text


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def declarations(text):
    """Read the emitter's single-line law headers; fail closed if format changes."""
    result = []
    for name, kind, law in re.findall(r"(?m)^-- aver:law-class (\S+) (\S+) (\S+)$", text):
        match = re.search(r"(?m)^theorem " + re.escape(name) + r" : (.+) := by$", text)
        if not match:
            raise ValueError(f"Cannot extract exact statement: {law}")
        result.append((name, kind, law, match.group(1)))
    return result


def audit(text, name):
    match = re.search(re.escape("'" + name + "'")
                      + r" depends on axioms: \[([^]]*)\]", text, re.DOTALL)
    if match:
        return [x.strip() for x in match.group(1).split(",") if x.strip()]
    if f"'{name}' does not depend on any axioms" in text:
        return []
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", required=True, type=Path)
    parser.add_argument("--waterfall", required=True, type=Path,
                        help="Clean checkout at the pinned WATERFALL_REV")
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--case", action="append", choices=CASES)
    parser.add_argument("--mode", action="append", choices=["search", "committed"])
    parser.add_argument("--effort", type=int, default=1000)
    parser.add_argument("--heartbeats", type=int, default=1000000)
    parser.add_argument("--timeout", type=int, default=30)
    args = parser.parse_args()
    binary = args.aver.resolve(strict=True)
    upstream = args.waterfall.resolve(strict=True)
    output = args.out.resolve()
    output.mkdir(parents=True, exist_ok=False)
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=upstream,
                                       text=True).strip()
    dirty = subprocess.check_output(["git", "diff", "HEAD", "--"], cwd=upstream)
    if revision != WATERFALL_REV or dirty:
        raise SystemExit("Use a clean checkout at " + WATERFALL_REV)
    setup, _ = run(["lake", "+" + TOOLCHAIN, "build"], upstream,
                   output / "waterfall-build.log", 180)
    if setup["exit_code"]:
        raise SystemExit("Waterfall build failed; see waterfall-build.log")
    report = {
        "aver_sha": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT,
                                             text=True).strip(),
        "aver_binary_sha256": digest(binary), "waterfall_sha": revision,
        "toolchain": TOOLCHAIN, "effort": args.effort,
        "max_heartbeats": args.heartbeats, "trial_timeout_seconds": args.timeout,
        "workers": 1, "setup": setup, "cases": [],
    }
    report_path = output / "report.json"

    def save():
        report_path.write_text(json.dumps(report, indent=2) + "\n")

    for case in args.case or CASES:
        source = ROOT / CASES[case]
        # Guided explanations need all reason obligations checked, not just the
        # final equality. This first experiment deliberately excludes them.
        if re.search(r"(?m)^\s+because\s", source.read_text()):
            raise ValueError("Guided laws need a separate obligation experiment")
        directory = output / case
        directory.mkdir()
        exported = directory / "baseline"
        status, log = run([str(binary), "proof", str(source), "--check", "--check-json",
                           "-o", str(exported)], ROOT, directory / "baseline.log", 180)
        summary = next((json.loads(line) for line in reversed(log.splitlines())
                        if line.startswith('{"backend":')), None)
        entry = {"case": case, "source": CASES[case], "source_sha256": digest(source),
                 "baseline": status, "baseline_summary": summary, "trials": []}
        report["cases"].append(entry)
        manifest_path = exported / "proof_manifest.json"
        if status["timeout"] or not manifest_path.exists() or summary is None or summary.get("build_errors"):
            entry["error"] = "Baseline failed to elaborate; no valid comparison"
            save()
            print(json.dumps(entry), flush=True)
            continue
        if (exported / "lean-toolchain").read_text().strip() != TOOLCHAIN:
            raise ValueError("Exported Lean toolchain differs from the experiment")
        manifest = json.loads(manifest_path.read_text())
        baseline_laws = {law["law"]: law for law in manifest["laws"]}
        files = [p for p in exported.glob("*.lean") if "-- aver:law-class" in p.read_text()]
        if len(files) != 1:
            raise ValueError("This experiment expects a single module with laws")
        original = files[0].read_text()
        prefix = original.split("-- verify", 1)[0]
        namespaces = re.findall(r"(?m)^namespace (\S+)$", prefix)
        if len(namespaces) != 1 or re.search(r"(?m)^theorem .*_law_", prefix):
            raise ValueError("Unexpected namespace or law in definition prefix")
        namespace = namespaces[0]
        ending = "\nend " + namespace + "\n"
        env = dict(os.environ)
        env["LEAN_PATH"] = str(upstream / ".lake/build/lib/lean") + os.pathsep + env.get("LEAN_PATH", "")
        for index, (name, kind, law, statement) in enumerate(declarations(original)):
            if kind != "universal":
                entry["trials"].append({"law": law, "skipped": "bounded statement", "kind": kind})
                continue
            for mode in args.mode or ["search", "committed"]:
                stem = f"Trial{index}_{mode}"
                trial_file = exported / (stem + ".lean")
                # Keeping definitions in the same module enables waterfall's
                # automatic discovery without supplying bespoke proof hints.
                trial_file.write_text(
                    "import waterfall\n" + prefix
                    + f"\nset_option maxHeartbeats {args.heartbeats} in\n"
                    + f"theorem {name} : {statement} := by\n"
                    + f"  waterfall (mode := .{mode}) (effort := {args.effort}) (report := true)\n"
                    + f"\n#print axioms {name}\n" + ending)
                trial, text = run(["lake", "env", "lean", trial_file.name], exported,
                                  directory / (stem + ".log"), args.timeout, env)
                axioms = audit(text, namespace + "." + name)
                accepted = (trial["exit_code"] == 0 and not trial["timeout"]
                            and axioms is not None and set(axioms) <= ALLOWED_AXIOMS)
                row = {"law": law, "theorem": namespace + "." + name,
                       "statement_sha256": hashlib.sha256(statement.encode()).hexdigest(),
                       "baseline_tier": baseline_laws[law]["tier"],
                       "baseline_axioms": baseline_laws[law]["axioms"],
                       "mode": mode, **trial, "axioms": axioms, "accepted": accepted}
                entry["trials"].append(row)
                save()
                print(json.dumps({"case": case, **row}), flush=True)
    report["binary_unchanged"] = digest(binary) == report["aver_binary_sha256"]
    save()
    if not report["binary_unchanged"]:
        raise SystemExit("Aver binary changed during the experiment")
    if any(case.get("error") for case in report["cases"]):
        raise SystemExit("An export could not be compared; inspect report.json")
    if any(trial.get("accepted") for case in report["cases"]
           if case["case"] == "false_accumulator" for trial in case["trials"]):
        raise SystemExit("A false universal law was accepted; discard these results")


if __name__ == "__main__":
    main()
