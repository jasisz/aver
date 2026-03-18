# The Dungeon of Non-Aver

A roguelike where you fight the programming concepts that Aver deliberately rejects.

## Run

Compiled native Rust (recommended):
```bash
AVER_RUNTIME_PATH=aver-rt aver compile --module-root examples/games/rogue examples/games/rogue/main.av -o /tmp/rogue-rs
cd /tmp/rogue-rs && cargo build --release
./target/release/main
```

Interpreter (slow but works):
```bash
aver run --module-root examples/games/rogue examples/games/rogue/main.av
```

## Controls

| Key | Action |
|-----|--------|
| ← → ↑ ↓ | Move / Attack adjacent enemy |
| > | Descend stairs |
| Q | Quit |

## Enemies

| Symbol | Name | Behavior | Combat |
|--------|------|----------|--------|
| ? | Wild if/else | Randomly chases OR wanders (no pattern matching = guessing) | 50% double hit (branch prediction fail) |
| ∞ | Feral for-loop | Patrols in circles, chases when close | Respawns once at half HP (off-by-one) |
| ∅ | The Null | Teleports randomly every turn | Steals EXP instead of dealing damage |

## Items

| Symbol | Name | Effect |
|--------|------|--------|
| ♥ | Potion of Purity | Restores HP (referential transparency) |
| ⚡ | Scroll of Pattern Match | +ATK (exhaustive destruction) |
| ◆ | Shield of Immutability | +DEF (nothing can change you) |

## Features

- Procedurally generated dungeons (pure, deterministic from seed)
- Field of view with shadowcasting
- BFS pathfinding for enemy AI
- Turn-based combat with unique per-enemy mechanics
- Floor progression with scaling enemies
- Message log with thematic humor

## Modules

| File | Role |
|------|------|
| `types.av` | Points, tiles, entity/item types, damage formula |
| `map.av` | Map generation, room placement, corridors |
| `fov.av` | Shadowcasting field of view |
| `pathfinding.av` | BFS pathfinding for AI |
| `combat.av` | Damage, death, movement, entity management |
| `render.av` | Terminal rendering, HUD, message log |
| `main.av` | Game loop, AI behaviors, state threading |

## Verify

```bash
for f in types map fov pathfinding combat; do
  aver verify --module-root examples/games/rogue examples/games/rogue/$f.av
done
```
