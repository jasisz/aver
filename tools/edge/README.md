# edge.averlang.dev

A pure Aver `fn handler(req: HttpRequest) -> HttpResponse` compiled to
`--target edge-wasm --bridge fetch --pack cloudflare`, running on
Cloudflare Workers.

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

The compiler drops `app.wasm` + `worker.js` + `wrangler.toml` into
`tools/edge/dist/`. The `wrangler.toml` checked in is pre-edited with
the `edge.averlang.dev` route — re-applying it after a regen is the
only manual step.

## Deploy

The worker.js fetches the shared Aver runtime from
`https://averlang.dev/runtime/latest/aver_runtime.wasm`, so
**averlang.dev needs to be deployed first** (it serves the runtime
asset with the right `Access-Control-Allow-Origin` / `Cross-Origin-Resource-Policy`
headers from `tools/website/_headers`).

### 1. Publish the runtime on averlang.dev

```bash
cd tools/website
npx wrangler login                    # one-time
npx wrangler deploy                   # ships index.html, _headers, runtime/, …
```

Verify the runtime is reachable with the right headers:

```bash
curl -I https://averlang.dev/runtime/v0.14.0/aver_runtime.wasm
# Expect:
#   content-type: application/wasm
#   access-control-allow-origin: *
#   cross-origin-resource-policy: cross-origin
#   cache-control: public, max-age=31536000, immutable
```

### 2. Deploy the edge demo

```bash
cd tools/edge/dist
npx wrangler deploy                   # ships app.wasm + worker.js
```

First deploy creates the `aver-edge-demo` worker plus the
`edge.averlang.dev` route in the `averlang.dev` zone (DNS record gets
auto-provisioned by wrangler when `custom_domain = true`).
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

Edit `app.av`, re-run the compile command, re-apply the `routes` block
in `dist/wrangler.toml` if it got overwritten, `wrangler deploy`. The
fetch bridge in `worker.js` rarely needs touching — it's the same
template every Cloudflare-targeted Aver program uses.
