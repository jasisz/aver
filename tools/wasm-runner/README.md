# WASM Runner

Static browser host for Aver modules compiled with:

```bash
aver compile app.av --target wasm
```

The runner implements the default `aver/*` import ABI in JavaScript and supports:

- `Console.*`
- `Terminal.*`
- `Random.int`
- `Time.now`, `Time.unixMs`, `Time.sleep`
- `Print.value`, `Format.value`
- `Float.sin`, `Float.cos`, `Float.atan2`, `Float.pow`

## Run

Serve the runner with the bundled script:

```bash
cd tools/wasm-runner
python3 serve.py 4173
```

Then open:

```text
http://localhost:4173
```

Drop a compiled `.wasm` file onto the page or use the file picker.

## Notes

- The terminal host uses a retained text grid rendered in the browser.
- `Terminal.readKey()` uses a shared-memory input queue, so interactive games need the isolation headers that `serve.py` adds.
- `Console.readLine()` consumes pre-queued lines from the UI. This keeps the host synchronous without changing the WASM ABI.
- `Time.sleep()` blocks only the worker running the WASM module, not the UI thread, so terminal demos can animate normally.
