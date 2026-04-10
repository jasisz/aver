You are an expert at using the Aver toolchain in this repository. Use the installed `aver` binary directly.

## Main commands

### Run

```bash
aver run file.av
aver run file.av --module-root .
aver run file.av -- arg1 arg2 arg3
```

- Aver program args are available through `Args.get()`
- `--record <dir>` records effect traces for replay

### Check

```bash
aver check file-or-dir --module-root . --deps
```

`check` handles static contract diagnostics:
- missing `intent =`
- missing `?` descriptions on relevant functions
- missing `verify` on pure, non-trivial, non-`main` functions
- coverage-style warnings for thin `verify` examples
- file size warnings

Warnings do not make `check` fail.

### Verify

```bash
aver verify file-or-dir --module-root . --deps
```

`verify` runs only declared `left => right` examples.
It fails on:
- mismatched examples
- parse/type errors
- execution errors

It is not a coverage tool.

### Format

```bash
aver format .
aver format examples
aver format examples --check
```

`format` accepts files or directories and walks `.av` files recursively.

### Context

```bash
aver context file.av --module-root .
```

Default:
- `--depth auto`
- `--budget 10kb`

This is the preferred AI discovery workflow:

1. start with a small budget
2. inspect the architecture map
3. look at selection metadata
4. zoom in only where needed

Examples:

```bash
aver context examples/modules/app.av --budget 10kb
aver context projects/workflow_engine/main.av --module-root projects/workflow_engine --budget 24kb
aver context projects/workflow_engine/main.av --module-root projects/workflow_engine --json --budget 24kb --output projects/workflow_engine/CONTEXT.json
```

Notes:
- `--depth N` and `--depth unlimited` bypass the auto-budget behavior
- `--decisions-only` exports only `decision` blocks
- selection metadata is printed to stdout and embedded in JSON output

### Compile

```bash
aver compile file.av -o /tmp/out --module-root .
aver compile file.av --target wasm -o /tmp/out
aver compile file.av --target wasm --wasm-opt oz -o /tmp/out
aver compile file.av --target wasm --wasm-opt oz --strip -o /tmp/out
```

- Default: Rust codegen, emits a modular Cargo project
- `--target wasm`: standalone WASM module with aver/* imports
- `--wasm-opt oz`: post-process with binaryen for ~50% size reduction
- `--strip`: omit variant/sentinel name tables from data section

### Proof

```bash
aver proof file.av -o /tmp/proof --module-root . --verify-mode auto
```

Lean export modes:
- `auto`
- `sorry`
- `theorem-skeleton`

### Replay

```bash
aver replay recordings/ --test --diff
```

Use replay for effectful debugging and regression capture.

## Recommended workflows

### Logic bug

1. add or tighten a `verify`
2. run `aver verify ...`
3. fix code
4. keep the example

### Effect bug

1. run with `--record`
2. inspect replay artifact
3. run `aver replay ... --test --diff`

### Project discovery

1. `aver context <entry> --budget 10kb`
2. if needed, raise budget or target a specific module
3. only then open raw source files

## Important current semantics

- no `aver decisions`; use `aver context --decisions-only`
- `check` and `verify` accept directories
- exact method-level effects only
- no effect aliases
- broad namespace effects do not satisfy child effects
