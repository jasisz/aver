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
        self.assertEqual(
            sharding.nextest_command({"huge"}, 1, 4)[-2:],
            ["--partition", "slice:2/4"],
        )

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


if __name__ == "__main__":
    unittest.main()
