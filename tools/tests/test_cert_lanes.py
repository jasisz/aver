#!/usr/bin/env python3
"""Keep the certification hardening shards exhaustive in PR and full CI."""

from __future__ import annotations

import json
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]


class CertificationLaneTests(unittest.TestCase):
    def setUp(self) -> None:
        self.lanes = json.loads((REPO_ROOT / ".github/cert-lanes.json").read_text())

    def test_hardening_shards_cover_the_whole_suite_on_pull_requests(self) -> None:
        lanes = [lane for lane in self.lanes if lane["suite"] == "cert_hardening_spec"]
        self.assertGreater(len(lanes), 1, "hardening must not return to one timed-out job")
        partitions = []
        for lane in lanes:
            self.assertEqual(lane["mode"], "nextest")
            self.assertEqual(lane["filter"], "all()")
            self.assertEqual(set(lane["features"].split(",")), {"wasm", "wasip2"})
            self.assertTrue(lane.get("smoke"), "every hardening shard must run on PRs")
            index, count = map(int, lane["partition"].split("/"))
            self.assertEqual(count, len(lanes))
            partitions.append(index)
        self.assertEqual(sorted(partitions), list(range(1, len(lanes) + 1)))

    def test_extra_wasip2_tests_run_once_in_both_matrices(self) -> None:
        for smoke_only in (False, True):
            with self.subTest(smoke_only=smoke_only):
                selected = [
                    lane for lane in self.lanes
                    if not smoke_only or lane.get("smoke")
                ]
                owners = [lane for lane in selected if lane.get("wasip2_tests")]
                self.assertEqual(len(owners), 1)
                self.assertEqual(owners[0]["suite"], "cert_hardening_spec")
                self.assertIn("wasip2", owners[0]["features"].split(","))


if __name__ == "__main__":
    unittest.main()
