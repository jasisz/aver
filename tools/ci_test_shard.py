#!/usr/bin/env python3
"""Run one measured shard of aver-lang integration tests.

Most integration targets are assigned whole to one runner with longest-first
bin packing.  A very small set of targets whose individual test cases dominate
the wall clock are split case by case across every runner with nextest
instead, again longest-first over the measured case weights, so two of the
slowest cases do not meet on one runner while another runner idles.  Every
shard lists the same cases in the same order and computes the same
assignment, so each case runs on exactly one shard.  The measured schedule
lives beside this script so a CI run can be reproduced and a new target or
case still lands safely at the conservative default weight.
"""

from __future__ import annotations

import argparse
import json
import subprocess
from pathlib import Path
from typing import Mapping


SCHEDULE_PATH = Path(__file__).with_name("ci_test_schedule.json")
DEFAULT_WEIGHT_SECONDS = 1.0


def integration_targets(metadata: dict[str, object]) -> list[str]:
    packages = metadata.get("packages", [])
    for package in packages:
        if package.get("name") != "aver-lang":
            continue
        return sorted(
            target["name"]
            for target in package.get("targets", [])
            if "test" in target.get("kind", [])
        )
    raise SystemExit("cargo metadata did not contain the aver-lang package")


def load_case_weights(path: Path = SCHEDULE_PATH) -> dict[str, float]:
    """Measured seconds of individual split-target cases, keyed `target::case`."""
    payload = json.loads(path.read_text())
    raw = payload.get("case_weights_seconds", {})
    if not isinstance(raw, dict):
        raise SystemExit(f"invalid case weights in {path}")
    weights: dict[str, float] = {}
    for key, weight in raw.items():
        if not isinstance(key, str) or "::" not in key:
            raise SystemExit(f"case weight key must be `target::case`, got {key!r}")
        if not isinstance(weight, (int, float)) or weight <= 0:
            raise SystemExit(f"weight for {key} must be positive")
        weights[key] = float(weight)
    return weights


def load_schedule(path: Path = SCHEDULE_PATH) -> tuple[dict[str, float], set[str]]:
    payload = json.loads(path.read_text())
    if payload.get("schema") != 1:
        raise SystemExit(f"unsupported CI test schedule schema in {path}")
    raw_weights = payload.get("weights_seconds")
    raw_split = payload.get("split_targets")
    if not isinstance(raw_weights, dict) or not isinstance(raw_split, list):
        raise SystemExit(f"invalid CI test schedule in {path}")
    weights: dict[str, float] = {}
    for target, weight in raw_weights.items():
        if not isinstance(target, str) or not isinstance(weight, (int, float)):
            raise SystemExit(f"invalid target weight in {path}")
        if weight <= 0:
            raise SystemExit(f"weight for {target} must be positive")
        weights[target] = float(weight)
    if any(not isinstance(target, str) for target in raw_split):
        raise SystemExit(f"invalid split target in {path}")
    return weights, set(raw_split)


def weighted_shards(
    targets: list[str],
    count: int,
    weights: Mapping[str, float],
    split_targets: set[str],
) -> tuple[list[list[str]], list[float]]:
    known = set(targets)
    stale = (set(weights) | split_targets) - known
    if stale:
        raise SystemExit(
            "CI test schedule names missing targets: " + ", ".join(sorted(stale))
        )

    shards: list[list[str]] = [[] for _ in range(count)]
    loads = [0.0] * count
    regular = [target for target in targets if target not in split_targets]
    ordered = sorted(
        regular,
        key=lambda target: (-weights.get(target, DEFAULT_WEIGHT_SECONDS), target),
    )
    for target in ordered:
        shard = min(range(count), key=lambda index: (loads[index], index))
        shards[shard].append(target)
        loads[shard] += weights.get(target, DEFAULT_WEIGHT_SECONDS)
    for shard in shards:
        shard.sort()
    return shards, loads


def cargo_test_command(targets: list[str]) -> list[str]:
    command = ["cargo", "test", "-p", "aver-lang"]
    for target in targets:
        command.extend(["--test", target])
    return command


def split_cases(split_targets: set[str]) -> list[tuple[str, str]]:
    """Every (target, case) of the split targets, as nextest lists them."""
    command = ["cargo", "nextest", "list", "-p", "aver-lang"]
    for target in sorted(split_targets):
        command.extend(["--test", target])
    command.extend(["--message-format", "json"])
    listing = json.loads(subprocess.check_output(command, text=True))
    cases = []
    for suite in listing.get("rust-suites", {}).values():
        target = suite.get("binary-name")
        if target not in split_targets:
            continue
        for case in suite.get("testcases", {}):
            cases.append((target, case))
    missing = split_targets - {target for target, _ in cases}
    if missing:
        raise SystemExit("nextest listed no cases for: " + ", ".join(sorted(missing)))
    return sorted(cases)


def weighted_cases(
    cases: list[tuple[str, str]],
    count: int,
    case_weights: Mapping[str, float],
    initial_loads: list[float],
) -> list[list[tuple[str, str]]]:
    """Longest-first assignment of split-target cases onto `count` shards."""
    known = {f"{target}::{case}" for target, case in cases}
    stale = set(case_weights) - known
    if stale:
        raise SystemExit(
            "CI test schedule names missing cases: " + ", ".join(sorted(stale))
        )
    loads = list(initial_loads)
    shards: list[list[tuple[str, str]]] = [[] for _ in range(count)]
    weight = lambda item: case_weights.get(f"{item[0]}::{item[1]}", DEFAULT_WEIGHT_SECONDS)
    for item in sorted(cases, key=lambda item: (-weight(item), item)):
        shard = min(range(count), key=lambda index: (loads[index], index))
        shards[shard].append(item)
        loads[shard] += weight(item)
    for shard in shards:
        shard.sort()
    return shards


def nextest_command(cases: list[tuple[str, str]]) -> list[str]:
    command = ["cargo", "nextest", "run", "-p", "aver-lang"]
    for target in sorted({target for target, _ in cases}):
        command.extend(["--test", target])
    expression = " | ".join(
        f"(binary(={target}) & test(={case}))" for target, case in cases
    )
    command.extend(["-E", expression])
    return command


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("index", type=int, help="zero-based shard index")
    parser.add_argument("count", type=int, help="total shard count")
    parser.add_argument(
        "--plan", action="store_true", help="print the shard without running tests"
    )
    args = parser.parse_args()
    if args.count < 1:
        parser.error("count must be positive")
    if not 0 <= args.index < args.count:
        parser.error("index must be in [0, count)")
    return args


def main() -> int:
    args = parse_args()
    metadata = json.loads(
        subprocess.check_output(
            ["cargo", "metadata", "--no-deps", "--format-version", "1"],
            text=True,
        )
    )
    targets = integration_targets(metadata)
    weights, split_targets = load_schedule()
    shards, loads = weighted_shards(targets, args.count, weights, split_targets)
    selected = shards[args.index]
    if not selected and not split_targets:
        raise SystemExit(f"integration-test shard {args.index}/{args.count} is empty")

    print(
        f"integration-test shard {args.index + 1}/{args.count} "
        f"(~{loads[args.index]:.1f}s measured whole-target load): "
        + ", ".join(selected),
        flush=True,
    )
    split_selected: list[tuple[str, str]] = []
    if split_targets:
        case_shards = weighted_cases(
            split_cases(split_targets), args.count, load_case_weights(), loads
        )
        split_selected = case_shards[args.index]
        print(
            f"test-case share {args.index + 1}/{args.count}: "
            + ", ".join(f"{target}::{case}" for target, case in split_selected),
            flush=True,
        )
    if args.plan:
        return 0
    if selected:
        result = subprocess.run(cargo_test_command(selected), check=False)
        if result.returncode != 0:
            return result.returncode
    if split_selected:
        return subprocess.run(nextest_command(split_selected), check=False).returncode
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
