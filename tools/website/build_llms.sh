#!/bin/bash
# Build tools/website/llms.txt and llms-full.txt from canonical sources.
#
# Sources of truth:
#   .claude/commands/aver.md         — language guide (skill)
#   .claude/commands/aver-tooling.md — toolchain guide (skill)
#   tools/website/llms_intro.md      — onboarding header for llms.txt
#   tools/website/llms_outro.md      — further-reading footer for llms.txt
#   tools/website/llms_full_intro.md — index header for llms-full.txt
#
# Each skill file starts with one line of skill prompt
# ("You are an expert ..."). We strip that with `tail -n +3` so it
# doesn't leak into the public artifacts.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
WEB="$REPO_ROOT/tools/website"
SKILLS="$REPO_ROOT/.claude/commands"

strip_skill_prompt() {
    # Drop the 1-line "You are an expert ..." preamble + the blank line after.
    tail -n +3 "$1"
}

build_llms_txt() {
    local out="$WEB/llms.txt"
    {
        cat "$WEB/llms_intro.md"
        echo
        strip_skill_prompt "$SKILLS/aver.md"
        echo
        cat "$WEB/llms_outro.md"
    } > "$out"
    echo "wrote $out ($(wc -l < "$out") lines, $(wc -c < "$out") bytes)"
}

build_llms_full_txt() {
    local out="$WEB/llms-full.txt"
    {
        cat "$WEB/llms_full_intro.md"
        echo
        echo "## Toolchain"
        echo
        strip_skill_prompt "$SKILLS/aver-tooling.md"
    } > "$out"
    echo "wrote $out ($(wc -l < "$out") lines, $(wc -c < "$out") bytes)"
}

build_llms_txt
build_llms_full_txt
