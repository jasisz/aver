#!/usr/bin/env python3
"""Reproduce #1293 without installing or changing a production proof backend."""
import argparse
import json
from pathlib import Path
import shutil
import statistics
import subprocess
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
EXPECTED = {
    "identity.positiveChain": "universal",
    "amount.selectionIsDecoded": "universal",
    "advance.preservesOrder": "universal",
    "identity.falseReason": "failed",
    "advance.brokenInduction": "failed",
}


def run(args, cwd, log=None, allowed=(0,)):
    start = time.monotonic_ns()
    result = subprocess.run(
        [str(x) for x in args], cwd=cwd, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
    )
    elapsed = time.monotonic_ns() - start
    if log:
        log.write_text(result.stdout)
    if result.returncode not in allowed:
        raise RuntimeError(f"{args}: exit {result.returncode}\n{result.stdout[-6000:]}")
    return result.stdout, elapsed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--target-dir", type=Path)
    parser.add_argument("--runs", type=int, default=3)
    args = parser.parse_args()
    if args.runs < 3:
        parser.error("at least three runs are required")
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    aver = args.aver.resolve()
    baseline = out / "baseline"
    target = (args.target_dir or out / "cargo-target").resolve()
    corpus = HERE / "corpus.av"

    print("Exporting the current text baseline (two intentional false reasons)...", flush=True)
    _, baseline_ns = run(
        [aver, "proof", corpus, "--check-json", "-o", baseline], ROOT,
        out / "baseline.log", allowed=(1,),
    )
    manifest = json.loads((baseline / "proof_manifest.json").read_text())
    actual = {x["law"]: x["tier"] for x in manifest["laws"]}
    assert actual == EXPECTED, actual
    shutil.copy2(corpus, out / "corpus.av")
    # False reasons deliberately escape these declared samples.
    run([aver, "verify", corpus], ROOT, out / "verify.log")
    hostile, _ = run([aver, "verify", corpus, "--hostile"], ROOT, out / "hostile.log", allowed=(1,))
    assert "verify-hostile-mismatch" in hostile
    assert "identity law falseReason.because1" in hostile
    assert "advance law brokenInduction.because1" in hostile
    run(
        ["cargo", "build", "--locked", "--manifest-path", HERE / "plan/Cargo.toml",
         "--target-dir", target], ROOT, out / "plan-build.log",
    )
    source_plan, _ = run([target / "debug/aver-proof-expr-spike-plan", corpus], ROOT)
    plan = json.loads(source_plan)
    (out / "plan.json").write_text(json.dumps(plan, indent=2) + "\n")
    # A human can read the original source and this plan without decoding Expr.
    lines = []
    for law in plan["laws"]:
        lines.append(law["theorem"])
        for stage in law["stages"]:
            lines.append(
                f"  {stage['label']} @ corpus.av:{stage['line']}: {stage['shape']}"
                f"; branch lines {stage['branches']}"
            )
    (out / "plan.txt").write_text("\n".join(lines) + "\n")
    all_results = []
    for repeat in range(args.runs):
        print(f"Comparing text / Expr, run {repeat + 1}/{args.runs}...", flush=True)
        # Fresh processes prevent kernel memoization from favoring the second lane.
        order = ["text", "expr"] if repeat % 2 == 0 else ["expr", "text"]
        results = {"laws": [], "corrupt_rejected": True, "shortcut_rejected": True}
        process_ns = 0
        for lane in order:
            plan["lanes"] = [lane]
            (baseline / "plan.json").write_text(json.dumps(plan))
            destination = out / f"run-{repeat + 1}" / lane
            bridge = (HERE / "Bridge.lean").read_text()
            bridge += f'\n#run_spike "plan.json" {json.dumps(str(destination))}\n'
            (baseline / "Bridge.lean").write_text(bridge)
            _, elapsed = run(
                ["lake", "env", "lean", "Bridge.lean"], baseline,
                out / f"run-{repeat + 1}-{lane}.log",
            )
            process_ns += elapsed
            measured = json.loads((destination / "results.json").read_text())
            results["laws"].extend(measured["laws"])
            results["corrupt_rejected"] &= measured["corrupt_rejected"]
            results["shortcut_rejected"] &= measured["shortcut_rejected"]
        assert results["corrupt_rejected"]
        assert results["shortcut_rejected"]
        for lane in ("text", "expr"):
            rows = [r for r in results["laws"] if r["lane"] == lane]
            assert len(rows) == len(EXPECTED)
            for row in rows:
                name = row["law"].removeprefix(plan["module"] + ".").replace("_law_", ".")
                assert row["status"] == (
                    "universal" if EXPECTED[name] == "universal" else "rejected"
                ), row
                if row["status"] == "rejected":
                    assert row["stage"].endswith(".because1"), row
                    if lane == "expr" and name == "advance.brokenInduction":
                        assert "/branch2@" in row["diagnostic"], row
                        assert "/fact2" in row["diagnostic"], row
        results["process_ns"] = process_ns
        all_results.append(results)

    measurements = {}
    for lane in ("text", "expr"):
        samples = []
        for results in all_results:
            good = [r for r in results["laws"] if r["lane"] == lane and r["status"] == "universal"]
            samples.append({
                "construct_ms": sum(
                    sum(s["construct_ns"] for s in r["stages"]) + r["compose_ns"] for r in good
                ) / 1e6,
                "kernel_ms": sum(
                    sum(s["check_ns"] for s in r["stages"]) + r["check_ns"] for r in good
                ) / 1e6,
                "pretty_bytes": sum(r["pretty_bytes"] for r in good),
                "raw_bytes": sum(r["raw_bytes"] for r in good),
            })
        measurements[lane] = {
            "samples": samples,
            "median": {key: statistics.median(s[key] for s in samples) for key in samples[0]},
        }
    versions = {
        "compiler": run([aver, "--version"], ROOT)[0].strip(),
        "source_commit": run(["git", "rev-parse", "HEAD"], ROOT)[0].strip(),
        "lean": run(["lake", "env", "lean", "--version"], baseline)[0].strip(),
        "lean_toolchain": (baseline / "lean-toolchain").read_text().strip(),
        "max_heartbeats_per_stage": 200000,
        "baseline_process_ms": baseline_ns / 1e6,
    }
    summary = {"versions": versions, "runs": args.runs, "measurements": measurements}
    (out / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))
    print(f"PASS: both lanes prove three original laws, reject two false reasons; artifacts: {out}")


if __name__ == "__main__":
    main()
