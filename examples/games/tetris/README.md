# Tetris

Classic Tetris in Aver — 4 modules, 66 verify cases.

## Run

```bash
aver run --module-root examples/games/tetris examples/games/tetris/main.av
```

## Controls

| Key | Action |
|-----|--------|
| ← → | Move |
| ↑ | Rotate |
| ↓ | Soft drop |
| Space | Hard drop |
| Q / ESC | Quit |

## Verify

```bash
aver verify --module-root examples/games/tetris examples/games/tetris/board.av
aver verify --module-root examples/games/tetris examples/games/tetris/pieces.av
aver verify --module-root examples/games/tetris examples/games/tetris/logic.av
```

## Modules

| File | Lines | Role |
|------|-------|------|
| `board.av` | 158 | Grid operations: get/set cell, clear lines |
| `pieces.av` | 155 | PieceKind sum type, 7 tetrominoes × 4 rotations |
| `logic.av` | 159 | Collision, movement, rotation, scoring |
| `main.av` | 251 | Rendering, game loop, entry point |
