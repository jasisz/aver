#!/bin/bash
# Compile btc-listener with --certify into btc-out.
S=/private/tmp/claude-502/-Users-szymon-tezewski-PycharmProjects-lumen-rs/209eaacb-8ad3-40eb-85c7-5d60cb39ef76/scratchpad/cert-scale
A=/Users/szymon.tezewski/PycharmProjects/lumen-rs/.claude/worktrees/agent-ad03386bd1b99fbdb/target/debug
cd "$S/btc" || exit 1
rm -rf "$S/btc-out"
start=$(date +%s)
/usr/bin/time -l "$A/aver" compile main.av --target wasm-gc --certify -o "$S/btc-out" > "$S/compile.log" 2>&1
echo "compile rc=$? took $(( $(date +%s) - start ))s"
tail -25 "$S/compile.log"
