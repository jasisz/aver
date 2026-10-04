#!/usr/bin/env python3
"""The proof-step ratchet names every reopened law and asks for every gain."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("steps_ratchet", REPO_ROOT / "tools" / "steps_ratchet.py")
assert SPEC is not None and SPEC.loader is not None
ratchet = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = ratchet
SPEC.loader.exec_module(ratchet)


def measured(*closed: str, laws: int = 9) -> dict:
    return {"closed": sorted(closed), "laws": laws}


class CompareTests(unittest.TestCase):
    def test_equal_state_passes(self) -> None:
        self.assertEqual(ratchet.compare({"a.av": ["f.l"]}, {"a.av": measured("f.l")}), ([], []))

    def test_reopened_laws_are_named(self) -> None:
        drops, gains = ratchet.compare({"a.av": ["f.l", "g.l"], "b.av": ["h.l"]}, {"a.av": measured("f.l")})
        self.assertEqual(gains, [])
        self.assertIn("a.av: `g.l` no longer closes by steps", drops)
        self.assertIn("b.av: no longer in the corpus", drops)

    def test_gains_must_be_recorded(self) -> None:
        drops, gains = ratchet.compare({"a.av": ["f.l"]}, {"a.av": measured("f.l", "g.l"), "b.av": measured("h.l")})
        self.assertEqual(drops, [])
        self.assertEqual(len(gains), 2)

    def test_summary_counts_steps_only(self) -> None:
        report = {"closed_by": {"f.l": "steps", "g.l": "open"}}
        self.assertEqual(ratchet.summarize(report), {"closed": ["f.l"], "laws": 2})

    def test_module_roots(self) -> None:
        self.assertEqual(ratchet.module_root("projects/k5_fdiv/domain/round.av"), "projects/k5_fdiv")
        self.assertEqual(ratchet.module_root("examples/games/tetris/logic.av"), "examples/games/tetris")
        self.assertEqual(ratchet.module_root("examples/refinement/natural_app.av"), "examples")


if __name__ == "__main__":
    unittest.main()
