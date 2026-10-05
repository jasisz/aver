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


STEPS = ("steps",)
BOTH = ("steps", "tactic")


def measured(steps=(), tactic=(), laws: int = 9) -> dict:
    return {"steps": sorted(steps), "tactic": sorted(tactic), "laws": laws}


class CompareTests(unittest.TestCase):
    def test_equal_state_passes(self) -> None:
        old = {"a.av": {"steps": ["f.l"], "tactic": ["g.l"]}}
        self.assertEqual(ratchet.compare(old, {"a.av": measured(["f.l"], ["g.l"])}, BOTH), ([], []))

    def test_reopened_laws_are_named(self) -> None:
        old = {"a.av": {"steps": ["f.l", "g.l"]}, "b.av": {"steps": ["h.l"]}}
        drops, gains = ratchet.compare(old, {"a.av": measured(["f.l"])}, STEPS)
        self.assertEqual(gains, [])
        self.assertIn("a.av: `g.l` no longer closes (was steps)", drops)
        self.assertIn("b.av: no longer in the corpus", drops)

    def test_a_law_falling_from_steps_to_tactics_is_a_loss(self) -> None:
        old = {"a.av": {"steps": ["f.l"]}}
        drops, gains = ratchet.compare(old, {"a.av": measured([], ["f.l"])}, BOTH)
        self.assertIn("a.av: `f.l` is closed by tactic now (was steps)", drops)
        self.assertIn("a.av: `f.l` newly closes by tactic", gains)

    def test_withdrawing_a_portfolio_shows_in_the_baseline(self) -> None:
        old = {"a.av": {"tactic": ["f.l"]}}
        drops, gains = ratchet.compare(old, {"a.av": measured(["f.l"], [])}, BOTH)
        self.assertIn("a.av: `f.l` is closed by steps now (was tactic)", drops)
        self.assertIn("a.av: `f.l` newly closes by steps", gains)

    def test_the_cheap_run_reads_only_the_steps_level(self) -> None:
        old = {"a.av": {"steps": ["f.l"], "tactic": ["g.l"]}}
        self.assertEqual(ratchet.compare(old, {"a.av": measured(["f.l"])}, STEPS), ([], []))

    def test_gains_must_be_recorded(self) -> None:
        old = {"a.av": {"steps": ["f.l"]}}
        drops, gains = ratchet.compare(old, {"a.av": measured(["f.l", "g.l"]), "b.av": measured(["h.l"])}, STEPS)
        self.assertEqual(drops, [])
        self.assertEqual(len(gains), 2)

    def test_recording_one_level_keeps_the_other(self) -> None:
        old = {"a.av": {"steps": ["f.l"], "tactic": ["g.l"]}}
        merged = ratchet.recorded(old, {"a.av": measured(["f.l", "h.l"])}, STEPS, full=True)
        self.assertEqual(merged, {"a.av": {"steps": ["f.l", "h.l"], "tactic": ["g.l"]}})

    def test_summary_splits_the_levels(self) -> None:
        report = {"closed_by": {"f.l": "steps", "g.l": "open", "h.l": "tactic"}}
        self.assertEqual(ratchet.summarize(report), {"steps": ["f.l"], "tactic": ["h.l"], "laws": 3})

    def test_only_new_steps_level_laws_go_to_lean(self) -> None:
        base = {"a.av": {"steps": ["f.l"], "tactic": ["g.l"]}, "b.av": {"steps": ["h.l"]}}
        head = {
            "a.av": {"steps": ["f.l", "g.l"]},
            "b.av": {"steps": ["h.l"]},
            "c.av": {"steps": ["k.l"]},
        }
        self.assertEqual(ratchet.steps_gains(base, head), {"a.av": ["g.l"], "c.av": ["k.l"]})
        self.assertEqual(ratchet.steps_gains(head, base), {})

    def test_module_roots(self) -> None:
        self.assertEqual(ratchet.module_root("projects/k5_fdiv/domain/round.av"), "projects/k5_fdiv")
        self.assertEqual(ratchet.module_root("examples/games/tetris/logic.av"), "examples/games/tetris")
        self.assertEqual(ratchet.module_root("examples/refinement/natural_app.av"), "examples")


if __name__ == "__main__":
    unittest.main()
