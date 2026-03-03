#!/usr/bin/env bash
# Prepare a sandboxed environment for an agent challenge.
#
# Usage:
#   bash challenges/prepare.sh inventory           # single challenge
#   bash challenges/prepare.sh --all               # all challenges, separate staging dirs
#   bash challenges/prepare.sh bank /tmp/my-test   # custom staging path
#
# What it does:
#   1. Builds aver and installs it globally (cargo install)
#   2. Creates a staging directory with ONLY the files the agent may see
#   3. Prints the ready-to-paste agent prompt
#
# The agent gets: README.md, docs/, examples/, challenges/<name>/
# The agent does NOT get: src/, tests/, Cargo.toml, .git/

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m'

# ── Build aver once ──
build_aver() {
    echo -e "${BOLD}[build] Building and installing aver...${NC}"
    (cd "$PROJECT_ROOT" && cargo install --path . --force --quiet 2>&1)
    AVER_PATH=$(which aver 2>/dev/null || echo "NOT FOUND")
    echo -e "  ${GREEN}✓${NC} aver installed at $AVER_PATH"
    if ! aver --help > /dev/null 2>&1; then
        echo -e "  ${RED}✗ aver binary not working${NC}"
        exit 1
    fi
}

# ── Prepare one challenge ──
prepare_one() {
    local CHALLENGE="$1"
    local STAGING="$2"
    local CHALLENGE_DIR="$PROJECT_ROOT/challenges/$CHALLENGE"

    # Validate
    if [ ! -d "$CHALLENGE_DIR" ]; then
        echo -e "${RED}Error: challenge '$CHALLENGE' not found at $CHALLENGE_DIR${NC}"
        return 1
    fi
    if [ ! -f "$CHALLENGE_DIR/TASK.md" ] || [ ! -f "$CHALLENGE_DIR/evaluate.sh" ]; then
        echo -e "${RED}Error: $CHALLENGE missing TASK.md or evaluate.sh${NC}"
        return 1
    fi

    # Create staging
    if [ -d "$STAGING" ]; then
        rm -rf "$STAGING"
    fi
    mkdir -p "$STAGING"

    # Copy allowed files
    cp "$PROJECT_ROOT/README.md" "$STAGING/"
    cp -r "$PROJECT_ROOT/docs" "$STAGING/"

    # Examples: agent learns conventions from these
    mkdir -p "$STAGING/examples/models"
    cp "$PROJECT_ROOT/examples/calculator.av" "$STAGING/examples/"    # ? descriptions, decision, verify, Result
    cp "$PROJECT_ROOT/examples/lists.av" "$STAGING/examples/"         # List ops, Option, match
    cp "$PROJECT_ROOT/examples/temperature.av" "$STAGING/examples/"   # decision, pipe, nested match
    cp "$PROJECT_ROOT/examples/shapes.av" "$STAGING/examples/"        # sum types, pattern matching
    cp "$PROJECT_ROOT/examples/fibonacci.av" "$STAGING/examples/"      # larger project: decisions, records, recursion
    cp "$PROJECT_ROOT/examples/app.av" "$STAGING/examples/"           # module depends (imports fibonacci)
    cp "$PROJECT_ROOT/examples/models/user.av" "$STAGING/examples/models/"  # simple module
    cp "$PROJECT_ROOT/examples/app_dot.av" "$STAGING/examples/"       # dotted module import
    mkdir -p "$STAGING/challenges/$CHALLENGE"
    cp "$CHALLENGE_DIR/TASK.md" "$STAGING/challenges/$CHALLENGE/"
    # NOTE: evaluate.sh is NOT copied — agent must not see grading criteria

    if [ -f "$CHALLENGE_DIR/AGENT_INSTRUCTIONS.md" ]; then
        cp "$CHALLENGE_DIR/AGENT_INSTRUCTIONS.md" "$STAGING/challenges/$CHALLENGE/"
    fi

    # Clean
    rm -f "$STAGING/challenges/$CHALLENGE/solution.av"
    rm -f "$STAGING/challenges/$CHALLENGE/solution_compare."*
    rm -f "$STAGING/challenges/$CHALLENGE/notes.md"
    rm -f "$STAGING/challenges/$CHALLENGE/.solution"*

    echo -e "  ${GREEN}✓${NC} $CHALLENGE → $STAGING"
}

# ── Print prompt for one challenge ──
print_prompt() {
    local CHALLENGE="$1"
    local STAGING="$2"

    echo -e "${BOLD}─── $CHALLENGE ───${NC}"
    cat <<PROMPT
You are an AI agent being tested on your ability to learn a new programming language and compare it with one you already know.

Working directory: $STAGING

Your task (do these steps IN ORDER):

PHASE 1 — Solve it in your language first:
1. Read challenges/$CHALLENGE/TASK.md — task requirements
2. Pick a language you're comfortable with and implement the solution:
   challenges/$CHALLENGE/solution_compare.* (e.g. solution_compare.py, .ts, .rs)
   Write it naturally — use whatever patterns, idioms, and structures you normally would.
   Include tests using the language's native test framework.

PHASE 2 — Now learn Aver and solve it again:
3. Read README.md — the complete language reference
4. Read docs/services.md — full API for all built-in namespaces
5. Read ALL files in examples/ — these show idiomatic Aver conventions
   Pay attention to: function descriptions, decision blocks, module structure, verify blocks
6. Implement the same solution in Aver: challenges/$CHALLENGE/solution.av
   Do NOT just translate your first solution line-by-line — write idiomatic Aver.
   For larger solutions, consider splitting into modules (see examples/app.av for how)
7. Verify it works by running:
   aver check challenges/$CHALLENGE/solution.av
   aver verify challenges/$CHALLENGE/solution.av
   aver run challenges/$CHALLENGE/solution.av

Rules:
- Do not ask questions — everything you need is in the docs
- Do not read files outside this directory
- Do not modify any existing files — only create your solution files and notes.md
- IMPORTANT: finish Phase 1 completely before starting Phase 2

You are done when ALL of these pass:
- solution_compare.* exists and runs with tests passing
- aver check — no type errors
- aver verify — all verify cases pass
- aver run — executes without runtime errors

After both implementations are done, write challenges/$CHALLENGE/notes.md comparing them:

Comparison:
- Which language made the problem easier to express? Why?
- Where did Aver's constraints (no if/else, no loops, no mutation) help or hurt?
- How did error handling compare (Result/match vs exceptions/try-catch/etc.)?
- How did testability compare (verify blocks vs your language's test framework)?
- What would you steal from Aver for your chosen language, and vice versa?
- Which version do you prefer reading? Which do you prefer writing?
- Lines of code comparison — is one significantly shorter?
- If you had to maintain one of these for a year, which would you choose?

Be honest and specific — reference actual code from both versions. We want genuine comparison, not flattery.
PROMPT
    echo ""
}

# ── Main ──

if [ "${1:-}" = "--all" ]; then
    build_aver
    echo ""
    echo -e "${BOLD}Preparing all challenges...${NC}"

    CHALLENGES=()
    STAGINGS=()
    for dir in "$PROJECT_ROOT"/challenges/*/; do
        name=$(basename "$dir")
        if [ -f "$dir/TASK.md" ] && [ -f "$dir/evaluate.sh" ]; then
            staging="/tmp/aver-challenge-${name}-$$"
            prepare_one "$name" "$staging"
            CHALLENGES+=("$name")
            STAGINGS+=("$staging")
        fi
    done

    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}Agent prompts (one per challenge):${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    for i in "${!CHALLENGES[@]}"; do
        print_prompt "${CHALLENGES[$i]}" "${STAGINGS[$i]}"
    done
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${BOLD}Summary:${NC}"
    for i in "${!CHALLENGES[@]}"; do
        echo -e "  ${CHALLENGES[$i]}:"
        echo -e "    agent dir:  cd ${STAGINGS[$i]}"
        echo -e "    evaluate:   bash $PROJECT_ROOT/challenges/${CHALLENGES[$i]}/evaluate.sh ${STAGINGS[$i]}/challenges/${CHALLENGES[$i]}/solution.av"
    done
    echo ""
    echo -e "Assessment rubric: ${BOLD}$PROJECT_ROOT/challenges/ASSESSMENT.md${NC}"
else
    CHALLENGE="${1:?Usage: prepare.sh <challenge-name> [staging-dir]  OR  prepare.sh --all}"
    STAGING="${2:-/tmp/aver-challenge-${CHALLENGE}-$$}"

    build_aver
    echo ""
    echo -e "${BOLD}Preparing challenge: $CHALLENGE${NC}"
    prepare_one "$CHALLENGE" "$STAGING"
    echo "  Contents:"
    find "$STAGING" -type f | sed "s|$STAGING/|    |" | sort

    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    print_prompt "$CHALLENGE" "$STAGING"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "To start the agent:  ${BOLD}cd $STAGING${NC}"
    echo -e "To evaluate after:   ${BOLD}bash $PROJECT_ROOT/challenges/$CHALLENGE/evaluate.sh $STAGING/challenges/$CHALLENGE/solution.av${NC}"
    echo -e "Assessment rubric:   ${BOLD}$PROJECT_ROOT/challenges/ASSESSMENT.md${NC}"
fi
