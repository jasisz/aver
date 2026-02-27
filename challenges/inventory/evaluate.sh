#!/usr/bin/env bash
# Evaluate an agent's solution to the inventory challenge.
# Usage: ./evaluate.sh [path-to-solution]
# Default: challenges/inventory/solution.av

SOLUTION="${1:-challenges/inventory/solution.av}"
PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$PROJECT_ROOT"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

pass=0
fail=0
warn=0

check() {
    local label="$1"
    local result="$2"  # 0 = pass, 1 = fail
    if [ "$result" -eq 0 ]; then
        echo -e "  ${GREEN}✓${NC} $label"
        pass=$((pass + 1))
    else
        echo -e "  ${RED}✗${NC} $label"
        fail=$((fail + 1))
    fi
}

do_warn() {
    local label="$1"
    echo -e "  ${YELLOW}!${NC} $label"
    warn=$((warn + 1))
}

echo "=== Inventory Challenge Evaluation ==="
echo "Solution: $SOLUTION"
echo ""

# ── 1. File exists ──
echo "1. File existence"
if [ -f "$SOLUTION" ]; then
    check "solution.av exists" 0
else
    check "solution.av exists" 1
    echo ""
    echo -e "${RED}Solution file not found. Aborting.${NC}"
    exit 1
fi

# ── 2. Type check ──
echo "2. Static analysis"
CHECK_OUTPUT=$(aver check "$SOLUTION" 2>&1)
CHECK_EXIT=$?
if echo "$CHECK_OUTPUT" | grep -q "Type check passed"; then
    check "cargo run -- check passes" 0
else
    check "cargo run -- check passes" 1
    echo "$CHECK_OUTPUT" | head -5
fi

# ── 3. Verify ──
echo "3. Verify blocks"
VERIFY_OUTPUT=$(aver verify "$SOLUTION" 2>&1)
VERIFY_EXIT=$?
VERIFY_LAST=$(echo "$VERIFY_OUTPUT" | tail -1)
echo "  $VERIFY_LAST"

TOTAL_LINE=$(echo "$VERIFY_OUTPUT" | grep "^Total:" || echo "")
if echo "$TOTAL_LINE" | grep -qE "^Total:.*passed$"; then
    VERIFY_PASSED=$(echo "$TOTAL_LINE" | grep -oE '[0-9]+/[0-9]+' | head -1 | cut -d/ -f1)
    VERIFY_TOTAL=$(echo "$TOTAL_LINE" | grep -oE '[0-9]+/[0-9]+' | head -1 | cut -d/ -f2)
    if [ "$VERIFY_PASSED" = "$VERIFY_TOTAL" ] && [ "$VERIFY_TOTAL" -gt 0 ]; then
        check "all verify cases pass" 0
    else
        check "all verify cases pass" 1
    fi
    if [ "$VERIFY_TOTAL" -ge 10 ]; then
        check "at least 10 verify cases ($VERIFY_TOTAL found)" 0
    else
        check "at least 10 verify cases ($VERIFY_TOTAL found)" 1
    fi
else
    check "all verify cases pass" 1
    check "at least 8 verify cases (0 found)" 1
fi

# ── 4. Run ──
echo "4. Runtime"
if aver run "$SOLUTION" > /dev/null 2>&1; then
    check "cargo run -- run succeeds" 0
else
    check "cargo run -- run succeeds" 1
fi

# ── 5. Convention checks (grep-based) ──
echo "5. Aver conventions"
CONTENT=$(cat "$SOLUTION")

# Module block
if echo "$CONTENT" | grep -q '^module '; then
    check "has module block" 0
else
    check "has module block" 1
fi

# Intent
if echo "$CONTENT" | grep -q 'intent ='; then
    check "has module intent" 0
else
    check "has module intent" 1
fi

# Decision block
DECISION_COUNT=$(echo "$CONTENT" | grep -c '^decision ' || true)
if [ "$DECISION_COUNT" -ge 1 ]; then
    check "has decision block ($DECISION_COUNT found)" 0
else
    check "has decision block ($DECISION_COUNT found)" 1
fi

# Record type
if echo "$CONTENT" | grep -q '^record '; then
    check "uses record type" 0
else
    check "uses record type" 1
fi

# ? descriptions
DESC_COUNT=$(echo "$CONTENT" | grep -c '?\ "' || true)
if [ "$DESC_COUNT" -ge 4 ]; then
    check "function descriptions ($DESC_COUNT found)" 0
else
    check "function descriptions ($DESC_COUNT found)" 1
fi

# Namespaced constructors (Result.Ok, Result.Err)
if echo "$CONTENT" | grep -qE 'Result\.Ok|Result\.Err'; then
    check "uses namespaced Result constructors" 0
else
    check "uses namespaced Result constructors" 1
fi

# No if/else as keywords (ignore occurrences inside strings)
if echo "$CONTENT" | grep -vE '^\s*\?' | grep -vE '^\s*"' | grep -vE '^\s*//' | grep -qE '^\s*(if|else)\b'; then
    check "no if/else (uses match)" 1
else
    check "no if/else (uses match)" 0
fi

# List operations
if echo "$CONTENT" | grep -qE 'List\.map|List\.filter|List\.fold'; then
    check "uses List higher-order functions" 0
else
    do_warn "no List.map/filter/fold found (may use recursion instead)"
fi

# ── Summary ──
echo ""
echo "=== Results ==="
total=$((pass + fail))
echo -e "Passed: ${GREEN}${pass}${NC}/${total}"
if [ "$fail" -gt 0 ]; then
    echo -e "Failed: ${RED}${fail}${NC}/${total}"
fi
if [ "$warn" -gt 0 ]; then
    echo -e "Warnings: ${YELLOW}${warn}${NC}"
fi
echo ""

if [ "$fail" -eq 0 ]; then
    echo -e "${GREEN}CHALLENGE PASSED${NC}"
    exit 0
else
    echo -e "${RED}CHALLENGE FAILED${NC}"
    exit 1
fi
