# edge.averlang.dev

An Aver `fn handler(req: HttpRequest) -> HttpResponse` compiled
with `aver compile --preset cloudflare` and deployed to Cloudflare
Workers. The handler declares `! [Time.unixMs]` as its only runtime
effect — routing, response bodies, and headers are pure Aver code.

- `GET /` → minimal HTML landing page
- `GET /api` → JSON manifesto with the request's `cf-ipcountry` and a
  server-side `Time.unixMs()` timestamp

## Source

`app.av` — the whole program. Single module, ~100 lines, declares
`! [Time]` as its only effect. Pure routing + string interpolation
otherwise.

## Build

From the repo root:

```bash
cargo build --features wasm
./target/debug/aver compile tools/edge/app.av \
    --preset cloudflare \
    --handler handler \
    -o tools/edge/dist
```

`--preset cloudflare` expands to `--target wasm --bridge fetch --pack
cloudflare`: a single bundled `app.wasm` (runtime inlined via
`wasm-merge`), `worker.js` (ES-module bootstrap with `aver/*` host
imports wired against `console.*` / `Date.now()` / `Math.random()` /
Fetch + JSPI), and `wrangler.toml`. Cloudflare Workers reject
`WebAssembly.instantiate(bytes, …)` from runtime-fetched bytes, so
the single-bundle shape is the only viable path here — browsers /
Deno / Bun can keep the thinner `--target edge-wasm` shape with a
runtime fetched from `averlang.dev/runtime/`.

The `wrangler.toml` checked in is pre-edited with the
`edge.averlang.dev` route + observability + worker name. Subsequent
recompiles preserve it (the pack writes `wrangler.toml` only when
it doesn't already exist), so the only file that changes on regen
is `app.wasm` plus the `worker.js` bridge template.

## Deploy

```bash
cd tools/edge/dist
npx wrangler login                    # one-time, browser auth
npx wrangler deploy                   # ships app.wasm + worker.js
```

First deploy creates the `aver-edge-demo` worker plus the
`edge.averlang.dev` route in the `averlang.dev` zone (DNS record
gets auto-provisioned by wrangler when `custom_domain = true`).
Subsequent deploys update in place.

To verify locally before pushing:

```bash
cd tools/edge/dist
npx wrangler dev                      # spins up a local worker on :8787
curl http://127.0.0.1:8787/api        # JSON manifesto
curl http://127.0.0.1:8787/           # HTML landing
```

## Verify production

```bash
curl -s https://edge.averlang.dev/api
curl -sI https://edge.averlang.dev/   # check content-type, cf-ray
```

## Iterating

Edit `app.av`, re-run the compile command, `wrangler deploy`. The
`wrangler.toml` is preserved across regens; the fetch-bridge in
`worker.js` rarely needs touching by hand — it's the same template
every Cloudflare-targeted Aver program uses.
