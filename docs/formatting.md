# Aver — Formatting

`aver format` is intentionally conservative. It normalizes layout without trying to rewrite the program structure or invent style preferences.

## Commands

```bash
aver format .                       # format all .av files under current directory
aver format path/to/file.av         # format one file
aver format . --check               # non-zero exit if changes would be made
```

## What it does

- normalize line endings to `\n`
- remove trailing spaces and tabs at end of line
- convert leading tab indentation to 4 spaces
- collapse long blank runs to at most 2 consecutive empty lines inside blocks
- enforce one blank line between top-level blocks
- move `verify <fn>` blocks directly under the matching `fn <fn>` declaration
- enforce exactly one trailing newline at end of file

## What it does not do

- change program semantics
- reorder unrelated definitions
- rewrite expressions for style
- reflow prose strings or comments
- act like a full opinionated pretty-printer

The current formatter is best understood as a stability tool for repositories and generated edits, not as a heavy syntax-normalizing compiler pass.
