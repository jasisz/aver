#!/usr/bin/env python3
"""Focused, network-free tests for playground website copy updates."""

from __future__ import annotations

import importlib.util
import re
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "rebuild_playground", REPO_ROOT / "tools" / "website" / "rebuild_playground.py"
)
assert SPEC is not None and SPEC.loader is not None
rebuild_playground = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = rebuild_playground
SPEC.loader.exec_module(rebuild_playground)


class MainIndexUpdateTests(unittest.TestCase):
    def test_updates_program_rows_without_legacy_games_section(self) -> None:
        size_values = {
            "life": ("12.0 KiB", 30),
            "snake": ("8.0 KiB", 20),
            "tetris": ("16.0 KiB", 40),
            "checkers": ("32.0 KiB", 80),
            "rogue": ("40.0 KiB", 100),
            "doom": ("24.0 KiB", 60),
            "wumpus": ("4.0 KiB", 10),
            "eggcatch": ("20.0 KiB", 50),
        }
        rows = []
        for index, game in enumerate(rebuild_playground.GAMES):
            href = (
                f"/playground/?game={game.slug}"
                if index % 2 == 0
                else f"../playground/?game={game.slug}"
            )
            if index % 2 == 0:
                opening = (
                    f'<a class="program-row featured" href="{href}" '
                    'style="--artifact: 1%">'
                )
            else:
                opening = (
                    f'<a href="{href}" style="color: inherit; --artifact: 1%" '
                    'class="program-row">'
                )
            rows.append(
                opening
                + f"<span>{game.slug}</span>"
                + '<span class="size"><i></i><b>0.0 KiB</b></span>'
                + "<span>Run</span></a>"
            )

        source = '<main><p id="untouched">Programs</p>' + "".join(rows) + "</main>"
        sizes = {slug: value for slug, (value, _percent) in size_values.items()}

        updated = rebuild_playground.update_main_index(source, sizes)

        self.assertIn('<p id="untouched">Programs</p>', updated)
        self.assertNotIn("<b>0.0 KiB</b>", updated)
        for game in rebuild_playground.GAMES:
            size, percent = size_values[game.slug]
            row = re.search(
                rf'<a\b[^>]*href="[^"]*playground/\?game={game.slug}"'
                r"[^>]*>.*?</a>",
                updated,
            )
            self.assertIsNotNone(row)
            assert row is not None
            self.assertIn(f"--artifact: {percent}%", row.group(0))
            self.assertIn(f"<b>{size}</b>", row.group(0))


if __name__ == "__main__":
    unittest.main()
