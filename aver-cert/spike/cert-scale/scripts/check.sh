#!/bin/bash
# Run aver-cert check on the btc build, keeping the build dir, one Lean at a time.
S=/private/tmp/claude-502/-Users-szymon-tezewski-PycharmProjects-lumen-rs/209eaacb-8ad3-40eb-85c7-5d60cb39ef76/scratchpad/cert-scale
A=/Users/szymon.tezewski/PycharmProjects/lumen-rs/.claude/worktrees/agent-ad03386bd1b99fbdb/target/debug
start=$(date +%s)
AVER_CERT_TIMINGS=1 AVER_CERT_KEEP_BUILD_DEVONLY=1 AVER_CERT_PHASE_TIMEOUT_SECS=9000 \
  /usr/bin/time -l "$A/aver-cert" check "$S/btc-out/main.wasm" "$S/btc-out/cert" > "$S/check.log" 2>&1
echo "check rc=$? took $(( $(date +%s) - start ))s"
/usr/bin/grep -E "maximum resident|kept|CHECKED|DECLINED|rror" "$S/check.log" | head -20
