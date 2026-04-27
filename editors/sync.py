#!/usr/bin/env python3
"""
Sync editor grammars from `editors/keywords.json`.

The JSON is the single source of truth; this script regenerates the
keyword + namespace + builtin-type listings in:
  - editors/sublime/Aver.sublime-syntax
  - editors/vscode/syntaxes/aver.tmLanguage.json
  - tools/website/playground/highlight.js

Modes:
  --write   write the regenerated files in place
  --check   exit 1 if any target file differs from the regeneration

Hook `editors/sync.py --check` into the release pipeline so `effects` /
`trace` / future keywords can't ship out of sync again.

Each render only rewrites the small bits of grammar that hold word lists
(keyword alternations, namespace alternations, the JS Sets). Surrounding
file structure — token rules, scope names, JSON formatting — is preserved
verbatim by using regex replacements that target individual fields.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path


REPO = Path(__file__).resolve().parents[1]
KEYWORDS_JSON = REPO / "editors" / "keywords.json"

SUBLIME = REPO / "editors" / "sublime" / "Aver.sublime-syntax"
VSCODE = REPO / "editors" / "vscode" / "syntaxes" / "aver.tmLanguage.json"
HIGHLIGHT_JS = REPO / "tools" / "website" / "playground" / "highlight.js"


def load_keywords() -> dict:
    with KEYWORDS_JSON.open() as f:
        return json.load(f)


def alt(words: list[str]) -> str:
    """Regex-alternation form, longest-first to avoid prefix shadowing."""
    return "|".join(sorted(words, key=lambda w: (-len(w), w)))


# ---------------------------------------------------------------------------
# sublime
# ---------------------------------------------------------------------------

def render_sublime(text: str, kw: dict) -> str:
    decl = alt(kw["declaration"])
    ctrl = alt(kw["control"])
    ns = alt(kw["namespaces"])

    text = re.sub(
        r"(- match: '\\b\()[^)]+(\)\\b'\n\s*scope: keyword\.declaration\.aver)",
        rf"\g<1>{decl}\g<2>",
        text,
    )
    text = re.sub(
        r"(- match: '\\b\()[^)]+(\)\\b'\n\s*scope: keyword\.control\.aver)",
        rf"\g<1>{ctrl}\g<2>",
        text,
    )
    text = re.sub(
        r"(- match: '\\b\()[^)]+(\)\(\\\.\)'\n\s*captures:\n\s*1: support\.class\.namespace\.aver)",
        rf"\g<1>{ns}\g<2>",
        text,
    )
    return text


# ---------------------------------------------------------------------------
# vscode tmLanguage
# ---------------------------------------------------------------------------

def _replace_tm_match_field(text: str, field: str, alternation: str, suffix: str) -> str:
    """Replace alternation inside `"<field>": { "match": "\\b(<alt>)<suffix>"`."""
    pattern = rf'("{field}":\s*\{{\s*"match":\s*"\\\\b\()[^)]+(\){suffix})'
    new, n = re.subn(pattern, rf"\g<1>{alternation}\g<2>", text)
    if n != 1:
        raise SystemExit(f"sync: failed to locate `{field}` match field "
                         f"in vscode grammar (got {n} replacements)")
    return new


def render_vscode(text: str, kw: dict) -> str:
    all_keywords = kw["declaration"] + kw["control"] + kw["header_field"]
    text = _replace_tm_match_field(text, "keywords", alt(all_keywords), r"\\\\b\"")
    text = _replace_tm_match_field(text, "namespace-access", alt(kw["namespaces"]), r"\\\\.")
    text = _replace_tm_match_field(text, "type-names", alt(kw["builtin_types"]), r"\\\\b\"")
    return text


# ---------------------------------------------------------------------------
# highlight.js
# ---------------------------------------------------------------------------

def _fmt_js_set(words: list[str], per_line: int = 7) -> str:
    """`new Set([...])` body, ~`per_line` quoted strings per line."""
    chunks = []
    for i in range(0, len(words), per_line):
        chunks.append(", ".join(f'"{w}"' for w in words[i:i + per_line]))
    body = ",\n    ".join(chunks)
    return f"new Set([\n    {body},\n]);"


def render_highlight_js(text: str, kw: dict) -> str:
    all_keywords = kw["declaration"] + kw["control"] + kw["header_field"]

    def replace(name: str, body: str) -> None:
        nonlocal text
        new, n = re.subn(
            rf"const {name} = new Set\(\[.*?\]\);",
            f"const {name} = {body}",
            text,
            count=1,
            flags=re.DOTALL,
        )
        if n != 1:
            raise SystemExit(f"sync: failed to locate `const {name}` in highlight.js")
        text = new

    replace("KEYWORDS", _fmt_js_set(all_keywords))
    replace(
        "CONSTANTS",
        f'new Set([{", ".join(f"\"{w}\"" for w in kw["constants"])}]);',
    )
    replace("BUILTINS", _fmt_js_set(kw["namespaces"]))
    return text


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

TARGETS = [
    (SUBLIME, render_sublime),
    (VSCODE, render_vscode),
    (HIGHLIGHT_JS, render_highlight_js),
]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true",
                        help="rewrite target files in place")
    parser.add_argument("--check", action="store_true",
                        help="exit 1 if any target differs from rendered form")
    args = parser.parse_args()

    if not (args.write or args.check):
        parser.error("pass --write or --check")

    kw = load_keywords()
    drift: list[str] = []

    for path, render in TARGETS:
        original = path.read_text()
        rendered = render(original, kw)
        if rendered == original:
            continue
        if args.check:
            drift.append(str(path.relative_to(REPO)))
        if args.write:
            path.write_text(rendered)
            print(f"  updated {path.relative_to(REPO)}")

    if args.check and drift:
        print("editor grammars out of sync with editors/keywords.json:",
              file=sys.stderr)
        for p in drift:
            print(f"  {p}", file=sys.stderr)
        print("run: python3 editors/sync.py --write", file=sys.stderr)
        return 1

    if args.write and not drift:
        print("all targets already match keywords.json")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
