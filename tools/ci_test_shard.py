#!/usr/bin/env python3
"""Run one deterministic shard of aver-lang integration-test targets."""

from __future__ import annotations

import argparse
import json
import subprocess


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


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("index", type=int, help="zero-based shard index")
    parser.add_argument("count", type=int, help="total shard count")
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
    selected = integration_targets(metadata)[args.index :: args.count]
    if not selected:
        raise SystemExit(f"integration-test shard {args.index}/{args.count} is empty")

    print(
        f"integration-test shard {args.index + 1}/{args.count}: "
        + ", ".join(selected),
        flush=True,
    )
    command = ["cargo", "test", "-p", "aver-lang"]
    for target in selected:
        command.extend(["--test", target])
    return subprocess.run(command, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
