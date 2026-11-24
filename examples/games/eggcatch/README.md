# Egg Catch

A four-position catching game in the manner of a pocket game with a fixed display. Four hens sit on perches, two on each side; each lays eggs onto a chute that slopes towards the middle, where a robot holds a basket at the end of one chute at a time. An egg that rolls off a chute with no basket under it breaks. Three broken eggs end the run; fifteen catches in a row mend one. The eggs roll faster every six catches.

3 modules: a pure engine, a pure view, and a shell that owns every effect.

## Run

```bash
aver run --module-root examples/games/eggcatch examples/games/eggcatch/main.av
aver run --module-root examples/games/eggcatch examples/games/eggcatch/main.av -- 42   # session seed
```

## Controls

| Key | Action |
|-----|--------|
| Q / A | Basket to the upper / lower left chute |
| P / L | Basket to the upper / lower right chute |
| ← → | Change the basket's side, keep its height |
| ↑ ↓ | Change the basket's height, keep its side |
| 1 / 2 | Start a run: easy / hard (hard starts faster and pays 2 per egg) |
| Space / Enter | Start again in the same mode; resume from a pause |
| Z | Pause |
| Esc | Quit |

## Verify and replay

```bash
aver audit --module-root examples/games/eggcatch examples/games/eggcatch/main.av
aver run --module-root examples/games/eggcatch examples/games/eggcatch/main.av --record recordings/
aver replay recordings/<file>.json --test --check-args
```

Keys and the frame clock both reach the engine as `Input` values, so a whole session — title, runs, pauses, restarts — is one input list, and `Core.replay` is a left fold of `Core.step` over it (`stepAll.leftFold`). Other laws state that every egg is accounted for (`step.preserves`), that the score never drops, that a basket at an egg's chute never misses it, and that a frame is always 18 rows of 45 cells.

## Modules

| File | Role |
|------|------|
| `core.av` | Pure engine: chutes, eggs, levels and modes, `step`, `replay`, the laws |
| `view.av` | Pure view: a frame as rows of ink spans, eggs and chutes in Braille dots, `changedFrom` for repainting only the rows that changed |
| `main.av` | Shell: key polling, painting, the frame clock — the only effects |
