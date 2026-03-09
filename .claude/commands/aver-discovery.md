Use this command when entering an Aver codebase and you need progressive discovery instead of reading raw source files immediately.

## Core idea

AI should explore codebases like humans explore docs:
- start high
- zoom in
- spend token budget deliberately

`aver context --budget` turns prompt budget into a navigation primitive.

## Default workflow

1. Start with a small context export from the real entrypoint.

```bash
aver context path/to/main.av --module-root path/to/root --budget 10kb
```

2. Read the selection summary:
- included depth
- used bytes
- whether output was truncated
- next-depth size

3. Decide what to do next:
- if the map is enough, open only the relevant modules
- if it is too shallow, raise the budget
- if one module dominates the question, run `context` for that module directly

4. Only then read raw source files.

## Heuristics

- Use Markdown `context` first for fast scanning.
- Use `--json` when you want a resumable artifact or want to save the snapshot.
- `10kb` is a good first pass.
- `24kb` is a good second pass for medium projects.
- Use `--depth N` or `--depth unlimited` only when you intentionally want a fuller dump.

## Example

For the workflow engine:

```bash
aver context projects/workflow_engine/main.av \
  --module-root projects/workflow_engine \
  --budget 10kb
```

Then, if needed:

```bash
aver context projects/workflow_engine/main.av \
  --module-root projects/workflow_engine \
  --budget 24kb \
  --json \
  --output projects/workflow_engine/CONTEXT.json
```

## What not to do

- Do not jump straight to `--depth unlimited` by default.
- Do not start by reading every source file in dependency order.
- Do not treat `context` as a full source dump. It is a navigation map first.
