#!/usr/bin/env python3
"""Regression tests for deterministic measured CI test sharding."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "ci_test_shard", REPO_ROOT / "tools" / "ci_test_shard.py"
)
assert SPEC is not None and SPEC.loader is not None
sharding = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = sharding
SPEC.loader.exec_module(sharding)


class WeightedShardTests(unittest.TestCase):
    def test_longest_targets_land_on_different_shards(self) -> None:
        targets = ["slow_a", "slow_b", "small_a", "small_b"]
        shards, loads = sharding.weighted_shards(
            targets,
            2,
            {"slow_a": 100.0, "slow_b": 90.0},
            set(),
        )

        self.assertNotEqual(
            next(i for i, shard in enumerate(shards) if "slow_a" in shard),
            next(i for i, shard in enumerate(shards) if "slow_b" in shard),
        )
        self.assertEqual(sorted(sum(shards, [])), sorted(targets))
        self.assertEqual(loads, [100.0, 92.0])

    def test_split_targets_are_removed_from_whole_target_bins(self) -> None:
        shards, _loads = sharding.weighted_shards(
            ["huge", "ordinary"],
            2,
            {"huge": 500.0},
            {"huge"},
        )

        self.assertEqual(sum(shards, []), ["ordinary"])

    def test_slowest_cases_spread_and_every_case_runs_once(self) -> None:
        cases = [("huge", f"case_{i}") for i in range(10)] + [("big", "x"), ("big", "y")]
        weights = {
            "huge::case_0": 600.0,
            "huge::case_1": 590.0,
            "huge::case_2": 580.0,
            "big::x": 480.0,
            "big::y": 300.0,
        }
        shards = sharding.weighted_cases(cases, 4, weights, [0.0] * 4)
        heavy = [
            sum(1 for target, case in shard if f"{target}::{case}" in weights)
            for shard in shards
        ]
        self.assertEqual(max(heavy), 2)
        self.assertEqual(sorted(sum(shards, [])), sorted(cases))
        # The same input gives the same assignment on every runner.
        self.assertEqual(shards, sharding.weighted_cases(list(reversed(cases)), 4, weights, [0.0] * 4))

    def test_nextest_command_names_its_cases_exactly(self) -> None:
        command = sharding.nextest_command([("big", "x"), ("huge", "m::y")])
        self.assertEqual(command[command.index("-E") + 1],
                         "(binary(=big) & test(=x)) | (binary(=huge) & test(=m::y))")
        self.assertEqual([command[i + 1] for i, arg in enumerate(command) if arg == "--test"],
                         ["big", "huge"])

    def test_stale_case_weight_fails_closed(self) -> None:
        with self.assertRaisesRegex(SystemExit, "missing cases: huge::gone"):
            sharding.weighted_cases([("huge", "here")], 2, {"huge::gone": 5.0}, [0.0, 0.0])

    def test_stale_schedule_entry_fails_closed(self) -> None:
        with self.assertRaisesRegex(SystemExit, "missing targets: old_name"):
            sharding.weighted_shards(
                ["current"],
                1,
                {"old_name": 10.0},
                set(),
            )

    def test_repository_schedule_covers_real_split_targets(self) -> None:
        weights, split_targets = sharding.load_schedule()
        self.assertEqual(
            split_targets,
            {"provider_vm_host_spec", "verify_step_budget_spec"},
        )
        self.assertGreater(weights["provider_vm_host_spec"], 900)
        for key in sharding.load_case_weights():
            self.assertIn(key.split("::", 1)[0], split_targets)


if __name__ == "__main__":
    unittest.main()
