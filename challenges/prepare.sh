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
    rm -f "$STAGING/challenges/$CHALLENGE/.solution"*

    echo -e "  ${GREEN}✓${NC} $CHALLENGE → $STAGING"
}

# ── Print prompt for one challenge ──
print_prompt() {
    local CHALLENGE="$1"
    local STAGING="$2"

    echo -e "${BOLD}─── $CHALLENGE ───${NC}"
    cat <<PROMPT
You are an AI agent being tested on your ability to write a program in Aver, a programming language you have not seen before.

Working directory: $STAGING

Your task:
1. Read README.md — the complete language reference
2. Read docs/services.md — full API for all built-in namespaces
3. Read ALL files in examples/ — these show idiomatic Aver conventions
   Pay attention to: function descriptions, decision blocks, module structure, verify blocks
4. Read challenges/$CHALLENGE/TASK.md — task requirements
5. Write your solution to challenges/$CHALLENGE/solution.av
   For larger solutions, consider splitting into modules (see examples/app.av for how)
6. Verify it works by running:
   aver check challenges/$CHALLENGE/solution.av
   aver verify challenges/$CHALLENGE/solution.av
   aver run challenges/$CHALLENGE/solution.av
Rules:
- Do not ask questions — everything you need is in the docs
- Do not read files outside this directory
- Do not modify any existing files — only create solution.av and notes.md

You are done when ALL of these pass:
- aver check — no type errors
- aver verify — all verify cases pass
- aver run — executes without runtime errors

When finished, write challenges/$CHALLENGE/notes.md covering:

Process:
- Which docs you read and in what order
- How many attempts before check/verify/run all passed
- Which error messages helped you, which ones were unhelpful or misleading

Language review:
- Rate Aver 1-10 for: readability, learnability, expressiveness, error messages
- What felt natural or well-designed
- What felt awkward, missing, or frustrating
- Did the lack of if/else bother you? Was match sufficient?
- How does Aver compare to languages you know (pick 2-3 specific comparisons)

Suggestions:
- What would you add to the language if you could
- What would you change about the docs to learn faster
- Would you use Aver for a real project? Why or why not
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
