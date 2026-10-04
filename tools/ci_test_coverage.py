#!/usr/bin/env python3
"""Fail when an integration test file is compiled by no CI job that runs it.

Every `tests/*.rs` target runs in ci.yml's integration shards
(`tools/ci_test_shard.py` takes the target list from `cargo metadata`), but
only under the default features. A file, or a test inside it, gated on
`feature = "wasm"` (or `wasm-compile`, `certify`) or `feature = "wasip2"`
compiles to nothing there. Those files run only where a workflow names them
under the feature: the explicit loops of ci.yml's WASM lanes, the cert lanes
of `.github/cert-lanes.json`, or another step that passes `--features`. A
file missing from every such list never runs, and nothing says so.

This check reads the workflows and the lane file the same way CI does and
lists each feature-gated file that no run covers. A file that is
deliberately not run goes in ALLOWED_GAPS with the reason.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]

# Features that only a feature-enabled run compiles, mapped to the feature a
# workflow passes to enable them. `wasm` enables `wasm-compile` and `certify`.
GATED_FEATURES = {
    "wasm": "wasm",
    "wasm-compile": "wasm",
    "certify": "wasm",
    "wasip2": "wasip2",
}

# name -> reason. Keep each reason specific enough to re-check.
ALLOWED_GAPS: dict[str, str] = {}

NEGATED = re.compile(r"not\(\s*feature\s*=\s*\"[^\"]+\"\s*\)")
FEATURE = re.compile(r"feature\s*=\s*\"([^\"]+)\"")


def required_features(source: str) -> set[str]:
    """The workflow features a file needs before all of its tests compile."""
    needed: set[str] = set()
    for line in source.splitlines():
        stripped = line.strip()
        if stripped.startswith("//") or "cfg" not in stripped:
            continue
        for match in re.finditer(r"cfg!?\((.*)\)", stripped):
            body = NEGATED.sub("", match.group(1))
            for feature in FEATURE.findall(body):
                if feature in GATED_FEATURES:
                    needed.add(GATED_FEATURES[feature])
    return needed


def workflow_runs(text: str, stems: set[str]) -> list[tuple[str, frozenset[str]]]:
    """(test, features) for each step that names a test and passes features.

    A step is the text from one `- name:`/`- uses:` item to the next. Comment
    lines are dropped, so a test mentioned only in prose does not count.
    """
    runs: list[tuple[str, frozenset[str]]] = []
    steps = re.split(r"\n\s*- (?:name|uses):", text)
    for step in steps:
        code = "\n".join(
            line for line in step.splitlines() if not line.strip().startswith("#")
        )
        flags = re.findall(r"--features\s+([A-Za-z0-9_,\-]+)", code)
        if not flags:
            continue
        features = frozenset(
            feature for flag in flags for feature in flag.split(",") if feature
        )
        for word in sorted(set(re.findall(r"[A-Za-z0-9_]+", code)) & stems):
            runs.append((word, features))
    return runs


def lane_runs(lanes: list[dict]) -> list[tuple[str, frozenset[str]]]:
    runs = []
    for lane in lanes:
        features = frozenset((lane.get("features") or "wasm").split(","))
        runs.append((lane["suite"], features))
        if lane.get("wasip2_tests"):
            # cert.yml's `Run wasip2 certificate tests` step.
            for suite in ("cert_verify_spec", "cert_certify_spec"):
                runs.append((suite, frozenset({"wasm", "wasip2"})))
    return runs


def gaps(root: Path = REPO_ROOT) -> list[str]:
    files = sorted((root / "tests").glob("*.rs"))
    stems = {path.stem for path in files}
    runs: list[tuple[str, frozenset[str]]] = []
    for workflow in sorted((root / ".github/workflows").glob("*.yml")):
        runs.extend(workflow_runs(workflow.read_text(), stems))
    runs.extend(lane_runs(json.loads((root / ".github/cert-lanes.json").read_text())))

    problems = []
    for name in sorted(ALLOWED_GAPS):
        if name not in stems:
            problems.append(f"{name}: listed in ALLOWED_GAPS but tests/{name}.rs is gone")
    for path in files:
        needed = required_features(path.read_text())
        if not needed:
            continue  # the default-feature integration shards run it
        # Each gated feature needs some run of this file that enables it; a
        # file with separate wasm-gc and wasip2 tests may get them from two.
        covered = all(
            any(name == path.stem and feature in features for name, features in runs)
            for feature in needed
        )
        if covered:
            if path.stem in ALLOWED_GAPS:
                problems.append(f"{path.stem}: runs now, drop it from ALLOWED_GAPS")
            continue
        if path.stem in ALLOWED_GAPS:
            continue
        problems.append(
            f"tests/{path.name} needs --features {','.join(sorted(needed))} "
            "but no CI step runs it with them"
        )
    return problems


def main() -> int:
    problems = gaps()
    if problems:
        print("integration tests that no CI job runs:")
        for problem in problems:
            print(f"  {problem}")
        print(
            "Add the file to the matching loop in .github/workflows/ci.yml (or a lane in "
            ".github/cert-lanes.json), or to ALLOWED_GAPS in tools/ci_test_coverage.py with the reason."
        )
        return 1
    print("every feature-gated integration test file runs in some CI job")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
