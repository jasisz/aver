#!/usr/bin/env python3
"""Profiling helpers.

`heavy VERIFY_LOG N` prints the N slowest modules of the verifier's build, read
from the `aver-cert timing:` lines that `AVER_CERT_TIMINGS=1` prints (Lake's
`Built <Module> (<time>)` lines).

`cumulative LOG...` reads the `cumulative profiling times:` block Lean prints
under `-Dprofiler=true` and emits one JSON object per log with seconds per
category.
"""
import json
import re
import sys

TIME = re.compile(r"^([0-9.]+)(ms|s|min)?$")


def seconds(text):
    text = text.strip()
    match = TIME.match(text)
    if not match:
        return None
    value = float(match.group(1))
    unit = match.group(2) or "s"
    return value / 1000 if unit == "ms" else value * 60 if unit == "min" else value


def build_seconds(text):
    # Lake prints e.g. `(12s)`, `(850ms)`, `(1.2min)` or `(1m 3s)`.
    total = 0.0
    found = False
    for value, unit in re.findall(r"([0-9.]+)\s*(ms|min|m|s)\b", text):
        found = True
        value = float(value)
        total += value / 1000 if unit == "ms" else value * 60 if unit in ("m", "min") else value
    return total if found else None


def heavy(log, count):
    modules = {}
    for line in open(log, encoding="utf-8", errors="replace"):
        match = re.search(r"Built ([A-Za-z0-9_.«»]+)(?::[a-z]+)? \((.*?)\)", line)
        if match:
            took = build_seconds(match.group(2))
            if took is not None:
                modules[match.group(1)] = max(took, modules.get(match.group(1), 0.0))
    ranked = sorted(modules.items(), key=lambda item: -item[1])[:count]
    for name, took in ranked:
        print(f"{name}\t{took:.1f}")


def cumulative(logs):
    for log in logs:
        categories = {}
        inside = False
        for line in open(log, encoding="utf-8", errors="replace"):
            if line.startswith("cumulative profiling times:"):
                inside = True
                categories = {}
                continue
            if inside:
                if not line.startswith("\t") and not line.startswith("  "):
                    inside = False
                    continue
                parts = line.strip().rsplit(" ", 1)
                if len(parts) == 2 and seconds(parts[1]) is not None:
                    categories[parts[0]] = seconds(parts[1])
        print(json.dumps({"log": log, "categories": categories}))


if __name__ == "__main__":
    if sys.argv[1] == "heavy":
        heavy(sys.argv[2], int(sys.argv[3]))
    elif sys.argv[1] == "cumulative":
        cumulative(sys.argv[2:])
    else:
        raise SystemExit("usage: profile.py heavy LOG N | cumulative LOG...")
