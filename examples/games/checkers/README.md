# Checkers

Checkers on 8×8 board with backward captures, maximum capture rule, and an alpha-beta AI opponent whose root candidates are scored in parallel batches.

## Run

Interpreter (slow, use depth 3-4):
```bash
aver run --module-root examples/games/checkers examples/games/checkers/main.av -- 4
```

Compiled native Rust (fast, use depth 6-10):
```bash
AVER_RUNTIME_PATH=aver-rt aver compile --module-root examples/games/checkers examples/games/checkers/main.av -o /tmp/checkers-rs
cd /tmp/checkers-rs && cargo build --release
./target/release/main 8
```

## Controls

| Key | Action |
|-----|--------|
| ← → ↑ ↓ | Move cursor |
| Enter | Select piece / confirm move |
| ESC | Deselect |
| Q | Quit |

## Rules

- Men move diagonally forward, capture forward and backward
- Captures are mandatory (forced capture rule)
- Maximum capture: must take the longest chain
- Multi-jump chains are a single turn
- Kings move and capture in all diagonal directions
- Promotion on reaching the last row

## AI

Alpha-beta pruning with root-parallel scoring on the first search layer. Candidate moves are evaluated in batches of 4 via independent products, then ranked on the right side of the board. Evaluation: piece count (kings 3×) + center control bonus.

## Modules

| File | Lines | Verify | Role |
|------|-------|--------|------|
| `board.av` | 275 | 56 | Grid, cell types, piece operations |
| `rules.av` | 295 | 32 | Legal moves, forced capture, multi-jump, maximum capture |
| `ai.av` | 270 | 25 | Alpha-beta search, evaluation, scored candidates |
| `render.av` | 310 | check OK | Terminal UI, cursor, board drawing, AI panel |
| `main.av` | 438 | 31 | Game loop, input, state threading, AI timing |

## Verify

```bash
aver verify --module-root examples/games/checkers examples/games/checkers/board.av
aver verify --module-root examples/games/checkers examples/games/checkers/rules.av
aver verify --module-root examples/games/checkers examples/games/checkers/ai.av
aver verify --module-root examples/games/checkers examples/games/checkers/main.av
```
