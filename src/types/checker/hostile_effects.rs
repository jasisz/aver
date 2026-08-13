//! Per-classified-effect hostile profiles for `aver verify --hostile`.
//!
//! Where `hostile_values` produces boundary values for typed `given`
//! domains, this module produces boundary *behaviours* for the
//! classified-effect oracles themselves. The user usually reaches for
//! one stub when they think about a law (`fn fairDie(...) -> Int 4`),
//! but the world has more shapes than that. A reasonable hostile sweep
//! tries each effect under a small set of plausible adversarial
//! profiles:
//!
//! - `Time.now` could be **frozen** (every call returns the same value),
//!   could go **backwards** (NTP correction, suspend/resume), or could
//!   **race forward** (clock skew, leap second jump).
//! - `Random.int(min, max)` could **always return min**, could **always
//!   return max**, could **alternate** (most-likely-fair-coin assumption
//!   broken).
//! - `Disk.readText` could **always succeed**, could **always fail**,
//!   could **return empty** (file existed, content was empty).
//!
//! Profiles are stable, deterministic, and hand-picked per method. Each
//! is rendered as an Aver source body that the verify pipeline parses
//! and appends as a `TopLevel::FnDef` before type-check; from then on
//! it lives in the VM as an ordinary user-space oracle stub. No new
//! runtime path; same machinery the user uses today.

/// One profile = one adversarial behaviour for one classified effect
/// method. The verify pipeline injects an `FnDef` per profile, then the
/// hostile case expansion multiplies user cases through every profile a
/// given fn uses.
#[derive(Debug, Clone)]
pub struct HostileProfile {
    /// Short label for diagnostics ("frozen", "min", "always_err"). The
    /// user sees this in the failure: "profile: Time.now/frozen — your
    /// law assumed monotonicity".
    pub name: &'static str,
    /// Synthetic fn name; collision-safe by construction
    /// (`__hostile_<effect_namespace>_<method>_<profile>`).
    pub stub_fn_name: String,
    /// Aver source code for the stub fn. Stable, parseable independently;
    /// the verify pipeline parses each body and inserts the resulting
    /// `TopLevel::FnDef` before type-check.
    pub stub_body: String,
}

/// Hostile profiles for a single classified effect method. Empty for
/// methods whose semantic shape doesn't admit useful adversarial
/// variation in 0.13 (most `Output`-only effects: their trace event
/// fires identically and there's no return value to vary).
pub fn hostile_profiles_for(method: &str) -> Vec<HostileProfile> {
    let stub_name = |profile: &str| format!("__hostile_{}_{}", method.replace('.', "_"), profile);
    match method {
        // ─── Snapshot ────────────────────────────────────────────────
        "Args.get" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}() -> List<String>\n    ? \"honest: a couple of plausible cli args\"\n    [\"--input\", \"data.txt\"]\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "empty",
                stub_fn_name: stub_name("empty"),
                stub_body: format!(
                    "fn {}() -> List<String>\n    ? \"hostile: process started with no args\"\n    []\n",
                    stub_name("empty")
                ),
            },
            HostileProfile {
                name: "many",
                stub_fn_name: stub_name("many"),
                stub_body: format!(
                    "fn {}() -> List<String>\n    ? \"hostile: many args, edge values\"\n    [\"\", \"\\0\", \"--flag\"]\n",
                    stub_name("many")
                ),
            },
        ],
        "Env.get" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(key: String) -> Option<String>\n    ? \"honest: env var has a plausible value\"\n    Option.Some(\"value\")\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "missing",
                stub_fn_name: stub_name("missing"),
                stub_body: format!(
                    "fn {}(key: String) -> Option<String>\n    ? \"hostile: env var never set\"\n    Option.None\n",
                    stub_name("missing")
                ),
            },
            HostileProfile {
                name: "empty",
                stub_fn_name: stub_name("empty"),
                stub_body: format!(
                    "fn {}(key: String) -> Option<String>\n    ? \"hostile: env var set to empty string\"\n    Option.Some(\"\")\n",
                    stub_name("empty")
                ),
            },
        ],
        "Terminal.size" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}() -> Terminal.Size\n    ? \"honest: typical 80x24 terminal\"\n    Terminal.Size(width = 80, height = 24)\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "minimal",
                stub_fn_name: stub_name("minimal"),
                stub_body: format!(
                    "fn {}() -> Terminal.Size\n    ? \"hostile: tiny terminal, layout under pressure\"\n    Terminal.Size(width = 1, height = 1)\n",
                    stub_name("minimal")
                ),
            },
        ],
        // ─── Generative ───────────────────────────────────────────────
        "Random.int" => vec![
            HostileProfile {
                name: "midrange",
                stub_fn_name: stub_name("midrange"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n    ? \"honest: roll lands in the middle of the range\"\n    Int.div(min + max, 2)\n",
                    stub_name("midrange")
                ),
            },
            HostileProfile {
                name: "always_min",
                stub_fn_name: stub_name("always_min"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n    ? \"hostile: stuck at lower bound\"\n    min\n",
                    stub_name("always_min")
                ),
            },
            HostileProfile {
                name: "always_max",
                stub_fn_name: stub_name("always_max"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n    ? \"hostile: stuck at upper bound\"\n    max\n",
                    stub_name("always_max")
                ),
            },
            HostileProfile {
                name: "alternating",
                stub_fn_name: stub_name("alternating"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n    ? \"hostile: alternates min/max — most-likely-fair-coin assumption broken\"\n    match Int.mod(n, 2)\n        0 -> min\n        _ -> max\n",
                    stub_name("alternating")
                ),
            },
        ],
        "Random.float" => vec![
            HostileProfile {
                name: "midrange",
                stub_fn_name: stub_name("midrange"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Float\n    ? \"honest: every roll lands at 0.5\"\n    0.5\n",
                    stub_name("midrange")
                ),
            },
            HostileProfile {
                name: "always_zero",
                stub_fn_name: stub_name("always_zero"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Float\n    ? \"hostile: every roll is exactly 0.0\"\n    0.0\n",
                    stub_name("always_zero")
                ),
            },
            HostileProfile {
                name: "always_one",
                stub_fn_name: stub_name("always_one"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Float\n    ? \"hostile: every roll is exactly 1.0\"\n    1.0\n",
                    stub_name("always_one")
                ),
            },
        ],
        "Time.now" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> String\n    ? \"honest: clock advances 1s per call\"\n    match n\n        0 -> \"2026-01-01T00:00:00Z\"\n        1 -> \"2026-01-01T00:00:01Z\"\n        2 -> \"2026-01-01T00:00:02Z\"\n        _ -> \"2026-01-01T00:00:03Z\"\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "frozen",
                stub_fn_name: stub_name("frozen"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> String\n    ? \"hostile: clock never advances between calls\"\n    \"2026-01-01T00:00:00Z\"\n",
                    stub_name("frozen")
                ),
            },
            HostileProfile {
                name: "epoch",
                stub_fn_name: stub_name("epoch"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> String\n    ? \"hostile: clock returns epoch — exposes deadline-vs-now assumptions\"\n    \"1970-01-01T00:00:00Z\"\n",
                    stub_name("epoch")
                ),
            },
            HostileProfile {
                name: "backward",
                stub_fn_name: stub_name("backward"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> String\n    ? \"hostile: clock goes backwards — NTP correction, suspend/resume\"\n    match n\n        0 -> \"2026-01-01T00:00:03Z\"\n        1 -> \"2026-01-01T00:00:02Z\"\n        2 -> \"2026-01-01T00:00:01Z\"\n        _ -> \"2026-01-01T00:00:00Z\"\n",
                    stub_name("backward")
                ),
            },
            HostileProfile {
                name: "fast_forward",
                stub_fn_name: stub_name("fast_forward"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> String\n    ? \"hostile: clock skips forward — leap-second jump, clock skew\"\n    match n\n        0 -> \"2026-01-01T00:00:00Z\"\n        1 -> \"2027-06-15T12:00:00Z\"\n        _ -> \"2099-12-31T23:59:59Z\"\n",
                    stub_name("fast_forward")
                ),
            },
        ],
        "Time.unixMs" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Int\n    ? \"honest: clock advances 1s per call\"\n    1735689600000 + n * 1000\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "frozen_zero",
                stub_fn_name: stub_name("frozen_zero"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Int\n    ? \"hostile: clock stuck at unix epoch\"\n    0\n",
                    stub_name("frozen_zero")
                ),
            },
            HostileProfile {
                name: "saturated",
                stub_fn_name: stub_name("saturated"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Int\n    ? \"hostile: clock at i64 saturation — overflow-on-arithmetic territory\"\n    9223372036854000000\n",
                    stub_name("saturated")
                ),
            },
            HostileProfile {
                name: "backward",
                stub_fn_name: stub_name("backward"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Int\n    ? \"hostile: clock goes backwards — NTP correction\"\n    1735689600000 - n * 1000\n",
                    stub_name("backward")
                ),
            },
            HostileProfile {
                name: "fast_forward",
                stub_fn_name: stub_name("fast_forward"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Int\n    ? \"hostile: clock skips forward by years per call — leap, skew\"\n    1735689600000 + n * 31536000000\n",
                    stub_name("fast_forward")
                ),
            },
        ],
        "Disk.readText" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Result<String, String>\n    ? \"honest: file reads back a plausible payload\"\n    Result.Ok(\"hello\\n\")\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Result<String, String>\n    ? \"hostile: every read fails — disk is on fire\"\n    Result.Err(\"hostile: disk read failed\")\n",
                    stub_name("always_err")
                ),
            },
            HostileProfile {
                name: "empty_ok",
                stub_fn_name: stub_name("empty_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Result<String, String>\n    ? \"hostile: file exists but content is empty\"\n    Result.Ok(\"\")\n",
                    stub_name("empty_ok")
                ),
            },
        ],
        "Disk.exists" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Bool\n    ? \"honest: file exists\"\n    true\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "never",
                stub_fn_name: stub_name("never"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Bool\n    ? \"hostile: nothing exists on the filesystem\"\n    false\n",
                    stub_name("never")
                ),
            },
            HostileProfile {
                name: "always",
                stub_fn_name: stub_name("always"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Bool\n    ? \"hostile: every path exists — every cleanup deletes something\"\n    true\n",
                    stub_name("always")
                ),
            },
        ],
        "Disk.listDir" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, dir: String) -> Result<List<String>, String>\n    ? \"honest: directory has a few entries\"\n    Result.Ok([\"a.txt\", \"b.txt\"])\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "empty",
                stub_fn_name: stub_name("empty"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, dir: String) -> Result<List<String>, String>\n    ? \"hostile: directory is empty\"\n    Result.Ok([])\n",
                    stub_name("empty")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, dir: String) -> Result<List<String>, String>\n    ? \"hostile: directory listing fails\"\n    Result.Err(\"hostile: listdir failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Console.readLine" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Result<String, String>\n    ? \"honest: user typed a plausible line\"\n    Result.Ok(\"hello\")\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "eof",
                stub_fn_name: stub_name("eof"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Result<String, String>\n    ? \"hostile: stdin is closed before any input\"\n    Result.Err(\"hostile: eof\")\n",
                    stub_name("eof")
                ),
            },
            HostileProfile {
                name: "empty",
                stub_fn_name: stub_name("empty"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Result<String, String>\n    ? \"hostile: user pressed enter without typing\"\n    Result.Ok(\"\")\n",
                    stub_name("empty")
                ),
            },
        ],
        // GenerativeOutput effects (Http / Disk-write / Tcp): symmetric
        // shape — request emitted to trace, response from oracle. The
        // hostile profile varies the response.
        "Http.get" | "Http.head" | "Http.delete" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, url: String) -> Result<HttpResponse, String>\n    ? \"honest: request succeeds with a 200 + small body\"\n    Result.Ok(HttpResponse(status = 200, body = \"ok\", headers = {{}}))\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, url: String) -> Result<HttpResponse, String>\n    ? \"hostile: request fails — network is down\"\n    Result.Err(\"hostile: connection refused\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Http.post" | "Http.put" | "Http.patch" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, url: String, body: String, contentType: String, headers: Map<String, List<String>>) -> Result<HttpResponse, String>\n    ? \"honest: write succeeds with a 200 + ack body\"\n    Result.Ok(HttpResponse(status = 200, body = \"ok\", headers = {{}}))\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, url: String, body: String, contentType: String, headers: Map<String, List<String>>) -> Result<HttpResponse, String>\n    ? \"hostile: write request fails — server unreachable\"\n    Result.Err(\"hostile: connection refused\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Disk.writeText" | "Disk.appendText" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String, content: String) -> Result<Unit, String>\n    ? \"honest: write succeeds\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String, content: String) -> Result<Unit, String>\n    ? \"hostile: write fails — disk full or permission denied\"\n    Result.Err(\"hostile: write failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Disk.delete" | "Disk.deleteDir" | "Disk.makeDir" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Result<Unit, String>\n    ? \"honest: filesystem op succeeds\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, file: String) -> Result<Unit, String>\n    ? \"hostile: filesystem op fails\"\n    Result.Err(\"hostile: op failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.send" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int, data: String) -> Result<String, String>\n    ? \"honest: tcp send succeeds with an ack\"\n    Result.Ok(\"ack\")\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int, data: String) -> Result<String, String>\n    ? \"hostile: tcp send fails\"\n    Result.Err(\"hostile: send failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.sendBytes" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int, payload: Bytes) -> Result<Bytes, String>\n    ? \"honest: tcp sendBytes echoes the payload\"\n    Result.Ok(payload)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int, payload: Bytes) -> Result<Bytes, String>\n    ? \"hostile: tcp sendBytes fails\"\n    Result.Err(\"hostile: send failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.ping" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int) -> Result<Unit, String>\n    ? \"honest: host is reachable\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, host: String, port: Int) -> Result<Unit, String>\n    ? \"hostile: ping fails\"\n    Result.Err(\"hostile: unreachable\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.connect" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, host: String, port: Int) -> Result<Tcp.Connection, String>\n    ? \"hostile: cannot establish connection\"\n    Result.Err(\"hostile: refused\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.readLine" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection) -> Result<String, String>\n    ? \"honest: peer sends a plausible line\"\n    Result.Ok(\"hello\")\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection) -> Result<String, String>\n    ? \"hostile: connection dropped before read\"\n    Result.Err(\"hostile: dropped\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.readBytes" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                // The real builtin is read-exact: `Ok` always carries exactly
                // `count` bytes, and a negative / over-limit / out-of-i64
                // count is an `Err` on every backend. The honest world must
                // model that, or frame-length-validating user code never sees
                // a success path. Branch order and message text mirror the
                // VM builtin (the fidelity target — hostile verification
                // runs on the VM): `count_arg` in `services/tcp.rs` rejects
                // counts outside i64 FIRST (both signs, generic "read
                // limit" text), then `aver_rt::tcp::read_bytes` rejects
                // negative counts, then counts over the 10485760-byte
                // `BODY_LIMIT` (named in the message). Pinned branch by
                // branch in `tests/hostile_readbytes_stub_fidelity.rs`.
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>\n    ? \"honest: peer sends exactly the count bytes that were asked for\"\n    match count > 9223372036854775807\n        true -> Result.Err(\"Tcp.readBytes: count {{count}} exceeds the read limit\")\n        false -> match count < 0 - 9223372036854775808\n            true -> Result.Err(\"Tcp.readBytes: count {{count}} exceeds the read limit\")\n            false -> match count < 0\n                true -> Result.Err(\"Tcp.readBytes: count {{count}} is negative\")\n                false -> match count > 10485760\n                    true -> Result.Err(\"Tcp.readBytes: count {{count}} exceeds the 10485760 byte limit\")\n                    false -> Bytes.fromList(List.fromVector(Vector.new(count, 0)))\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "short_read",
                stub_fn_name: stub_name("short_read"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>\n    ? \"hostile: peer goes away mid-frame\"\n    Result.Err(\"hostile: failed to fill whole buffer\")\n",
                    stub_name("short_read")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>\n    ? \"hostile: read fails\"\n    Result.Err(\"hostile: read failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.writeLine" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, line: String) -> Result<Unit, String>\n    ? \"honest: connection write succeeds\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, line: String) -> Result<Unit, String>\n    ? \"hostile: connection write fails\"\n    Result.Err(\"hostile: dropped\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.writeBytes" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>\n    ? \"honest: bytes reach the peer\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>\n    ? \"hostile: write fails\"\n    Result.Err(\"hostile: write failed\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Tcp.close" => vec![
            HostileProfile {
                name: "normal_ok",
                stub_fn_name: stub_name("normal_ok"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection) -> Result<Unit, String>\n    ? \"honest: close succeeds\"\n    Result.Ok(Unit)\n",
                    stub_name("normal_ok")
                ),
            },
            HostileProfile {
                name: "always_err",
                stub_fn_name: stub_name("always_err"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection) -> Result<Unit, String>\n    ? \"hostile: close on already-dropped connection\"\n    Result.Err(\"hostile: dropped\")\n",
                    stub_name("always_err")
                ),
            },
        ],
        "Terminal.readKey" => vec![
            HostileProfile {
                name: "normal",
                stub_fn_name: stub_name("normal"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Option<String>\n    ? \"honest: user pressed a normal key\"\n    Option.Some(\"a\")\n",
                    stub_name("normal")
                ),
            },
            HostileProfile {
                name: "no_input",
                stub_fn_name: stub_name("no_input"),
                stub_body: format!(
                    "fn {}(path: BranchPath, n: Int) -> Option<String>\n    ? \"hostile: terminal returns no key — user idle\"\n    Option.None\n",
                    stub_name("no_input")
                ),
            },
        ],
        // Output-only effects don't have an oracle response to vary —
        // their trace event fires identically. Hostile mode for these
        // is the absence of a stub plus the trace assertions the user
        // already writes.
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn time_now_has_multiple_profiles() {
        let profiles = hostile_profiles_for("Time.now");
        assert!(profiles.len() >= 2);
        assert!(profiles.iter().any(|p| p.name == "frozen"));
        assert!(profiles.iter().any(|p| p.name == "epoch"));
    }

    #[test]
    fn random_int_covers_min_and_max() {
        let profiles = hostile_profiles_for("Random.int");
        assert!(profiles.iter().any(|p| p.name == "always_min"));
        assert!(profiles.iter().any(|p| p.name == "always_max"));
    }

    #[test]
    fn unclassified_method_returns_empty() {
        assert!(hostile_profiles_for("HttpServer.listen").is_empty());
        assert!(hostile_profiles_for("Console.print").is_empty());
        assert!(hostile_profiles_for("Foo.bar").is_empty());
    }

    #[test]
    fn stub_fn_names_are_unique_per_method_and_profile() {
        use std::collections::HashSet;
        let methods = [
            "Time.now",
            "Random.int",
            "Disk.readText",
            "Http.get",
            "Tcp.connect",
        ];
        let mut seen = HashSet::new();
        for m in methods {
            for p in hostile_profiles_for(m) {
                assert!(
                    seen.insert(p.stub_fn_name.clone()),
                    "duplicate stub fn name: {}",
                    p.stub_fn_name
                );
            }
        }
    }

    #[test]
    fn stub_bodies_parse_as_aver_source() {
        // Quick sanity — every stub source we emit should at least pass
        // the lexer. Full type-check happens once the verify pipeline
        // appends the FnDef and runs the regular checker pass.
        for method in [
            "Time.now",
            "Random.int",
            "Random.float",
            "Disk.readText",
            "Disk.exists",
            "Console.readLine",
            "Http.get",
            "Http.post",
            "Tcp.connect",
            "Args.get",
            "Env.get",
        ] {
            for p in hostile_profiles_for(method) {
                let mut lexer = crate::lexer::Lexer::new(&p.stub_body);
                let tokens = lexer
                    .tokenize()
                    .unwrap_or_else(|e| panic!("{}/{}: lex failed: {}", method, p.name, e));
                let mut parser = crate::parser::Parser::new(tokens);
                parser
                    .parse()
                    .unwrap_or_else(|e| panic!("{}/{}: parse failed: {}", method, p.name, e));
            }
        }
    }

    fn render_signature(params: &[String], ret: &str) -> String {
        format!("({}) -> {}", params.join(", "), ret)
    }

    fn render_errors(errors: &[crate::types::checker::TypeError]) -> String {
        errors
            .iter()
            .map(|e| format!("  {}: {}", e.line, e.message))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Every stub body must (a) type-check cleanly as an Aver fn, and
    /// (b) have a signature that matches what the runtime expects from
    /// an oracle for that classified effect — same parameter list as
    /// `effect_classification::oracle_signature`, same return type,
    /// same record-field shapes (Terminal.Size: width/height, not
    /// rows/cols).
    ///
    /// Both halves are checked here. The signature half was for a long
    /// time a promise in this comment and nothing else: the body only
    /// asserted each stub compiled standalone, which a `Tcp.close` stub
    /// that inherited `Tcp.writeLine`'s `line` parameter does perfectly
    /// well. Such a stub is rejected at the moment a user binds it in a
    /// `given`, so the failure lands on the user running `aver verify
    /// --hostile`, not on us.
    ///
    /// The roster is `classifications_for_proof_subset()`, not a list
    /// next to the loop. The hand-written list was another copy of the
    /// registry, and a method missing from it was not checked at all —
    /// the guard reported success over an effect it never looked at.
    #[test]
    fn stub_bodies_typecheck_with_signature_matching_oracle_classification() {
        use crate::source::parse_source;
        use crate::types::Type;
        use crate::types::checker::effect_classification::{
            EffectDimension, classifications_for_proof_subset, oracle_signature,
        };
        use crate::types::checker::run_type_check_full;

        // The method list is derived, not written down. Hand-listing it meant
        // an effect could gain profiles without ever being checked here, which
        // is the failure this test exists to prevent one level down.
        for c in classifications_for_proof_subset() {
            let method = c.method;
            // Output effects have no oracle channel for a hostile world to
            // answer through; the injector skips them by design.
            if matches!(c.dimension, EffectDimension::Output) {
                continue;
            }
            let Some(Type::Fn(oracle_params, oracle_ret, _)) = oracle_signature(method) else {
                panic!(
                    "{method}: classified as {:?} but has no oracle signature — \
                     every non-Output effect binds an oracle",
                    c.dimension
                );
            };
            // Compared by rendered type, because the classification table
            // names nominals before the module graph assigns them ids while a
            // typechecked signature carries them. Rendering is also the
            // stricter comparison: `Type::compatible` lets `Type::Invalid`
            // match anything and relates two id-less nominals by name suffix,
            // so a stub declaring `Connection` would pass against an oracle
            // taking `Tcp.Connection`.
            let want_params: Vec<String> = oracle_params.iter().map(|t| t.display()).collect();
            let want_ret = oracle_ret.display();

            for p in hostile_profiles_for(method) {
                // `Bytes` is a stdlib module, so a stub that names it (as a
                // type or to call `Bytes.fromList`) has to depend on it.
                //
                // This prelude is NOT what the injector provides. The real
                // injector, `vm_verify::inject_hostile_effect_stubs_for_blocks`,
                // appends the stub straight into the user's own module and adds
                // no dependency at all — so a user module that calls
                // `Tcp.readBytes` without naming `Bytes` itself type-checks here
                // and still fails under `aver verify --hostile` with `Unknown
                // identifier Bytes`, skipping the whole file. Synthesizing the
                // dependency keeps this test about stub signatures rather than
                // about that separate injector defect; the defect itself is
                // pinned by `stub_injection_typechecks_the_module_the_injector_builds`
                // below, which is ignored until the injector is fixed.
                let depends = if p.stub_body.contains("Bytes") {
                    "    depends [Bytes]\n"
                } else {
                    ""
                };
                let src = format!(
                    "module M\n    intent = \"t\"\n{}    effects []\n\n{}",
                    depends, p.stub_body
                );
                let items = parse_source(&src)
                    .unwrap_or_else(|e| panic!("{}/{}: parse: {:?}", method, p.name, e));
                let result = run_type_check_full(&items, Some(env!("CARGO_MANIFEST_DIR")));
                if !result.errors.is_empty() {
                    panic!(
                        "{}/{}: typecheck errors:\n{}\n\nbody:\n{}",
                        method,
                        p.name,
                        render_errors(&result.errors),
                        p.stub_body
                    );
                }

                // The half the doc comment above has always promised: a stub
                // that parses and typechecks can still have the wrong shape for
                // the oracle slot it is injected into — a missing `(path, n)`
                // prefix on a generative effect, a record with the wrong field
                // names, a parameter inherited from a neighbouring method.
                let (stub_params, stub_ret, _) =
                    result.fn_sigs.get(&p.stub_fn_name).unwrap_or_else(|| {
                        panic!(
                            "{}/{}: the checker recorded no signature for stub fn '{}'",
                            method, p.name, p.stub_fn_name
                        )
                    });
                let got_params: Vec<String> = stub_params.iter().map(|t| t.display()).collect();
                let got_ret = stub_ret.display();

                assert!(
                    got_params == want_params,
                    "{}/{}: hostile stub takes a different parameter list than an oracle \
                     for {} — a user binding this stub in a `given` gets a type error\n  \
                     oracle: {}\n  stub:   {}\n\nbody:\n{}",
                    method,
                    p.name,
                    method,
                    render_signature(&want_params, &want_ret),
                    render_signature(&got_params, &got_ret),
                    p.stub_body
                );
                assert!(
                    got_ret == want_ret,
                    "{}/{}: hostile stub returns {} where an oracle for {} returns {}\n\nbody:\n{}",
                    method,
                    p.name,
                    got_ret,
                    method,
                    want_ret,
                    p.stub_body
                );
            }
        }
    }

    /// The same stubs, type-checked in the module the injector really
    /// builds instead of the one the guard above synthesizes.
    ///
    /// The guard wraps each stub in a module carrying `depends [Bytes]`
    /// whenever the body mentions `Bytes`. The injector supplies no such
    /// thing: `vm_verify::inject_hostile_effect_stubs_for_blocks` appends
    /// the stub bodies into the user's own module as plain
    /// `TopLevel::FnDef` and adds no dependency at all. So the guard is
    /// green over stubs that cannot in fact be injected, and a user module
    /// that reads exact bytes off a connection without ever naming `Bytes`
    /// itself passes `aver verify` and is refused by `aver verify
    /// --hostile` with `error[1:0]: Unknown identifier 'Bytes'` — the whole
    /// file skipped, no cases run. Writing `depends [Bytes]` into that same
    /// file by hand makes the hostile run work.
    ///
    /// Ignored until the injector supplies what the stubs it injects need:
    /// https://github.com/jasisz/aver/issues/877. Delete the `ignore` when
    /// that lands.
    #[test]
    #[ignore = "the injector adds no dependency for the stubs it injects — issue #877"]
    fn stub_injection_typechecks_the_module_the_injector_builds() {
        use crate::checker::merge_verify_blocks;
        use crate::diagnostics::vm_verify::inject_hostile_effect_stubs_for_blocks;
        use crate::source::parse_source;
        use crate::types::checker::run_type_check_full;

        // An ordinary user module. It calls `Tcp.readBytes`, which the
        // hostile profiles model, and it never spells `Bytes` anywhere.
        let src = "module Main\n    \
                   intent = \"Read one exact frame and classify the outcome.\"\n    \
                   effects [Tcp]\n\n\
                   fn frameVerdict(conn: Tcp.Connection) -> String\n    \
                   ? \"Classify one exact-frame read.\"\n    \
                   ! [Tcp.readBytes]\n    \
                   match Tcp.readBytes(conn, 4)\n        \
                   Result.Ok(_) -> \"ok\"\n        \
                   Result.Err(_) -> \"err\"\n\n\
                   verify frameVerdict law neverReads\n    \
                   given conn: Tcp.Connection = \
                   [Tcp.Connection(id = \"fake\", host = \"127.0.0.1\", port = 1)]\n    \
                   frameVerdict(conn) => \"err\"\n";

        let mut items = parse_source(src).unwrap_or_else(|e| panic!("parse: {e:?}"));

        // The module is clean before injection, so anything below is the
        // injector's doing and not a flaw in this fixture.
        let before = run_type_check_full(&items, Some(env!("CARGO_MANIFEST_DIR")));
        assert!(
            before.errors.is_empty(),
            "the fixture module must type-check on its own:\n{}",
            render_errors(&before.errors)
        );

        let blocks = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &blocks);

        let after = run_type_check_full(&items, Some(env!("CARGO_MANIFEST_DIR")));
        assert!(
            after.errors.is_empty(),
            "hostile stub injection left the user's module untypecheckable — this is \
             what a user running `aver verify --hostile` gets, and the file is skipped \
             whole:\n{}",
            render_errors(&after.errors)
        );
    }

    /// The injector (`vm_verify::collect_effect_profile_combinations`)
    /// skips `Output`-dimension effects by design (trace-only, no oracle
    /// channel for the world to be hostile through) — and then silently
    /// skips any method whose profile list is empty. This guard makes the
    /// second skip unreachable: a method added to the classification
    /// table without profiles fails here, instead of hostile verification
    /// quietly running with that effect unmodelled.
    ///
    /// Non-emptiness is not enough on its own. One profile is one world,
    /// and a sweep across one world varies nothing: the run passes, and
    /// the pass says only that the law survived the single behaviour we
    /// happened to write down. Two is the floor, and every effect but one
    /// meets it.
    #[test]
    fn every_classified_non_output_effect_ships_enough_hostile_profiles() {
        use crate::types::checker::effect_classification::{
            EffectDimension, classifications_for_proof_subset,
        };

        /// Fewest adversarial worlds an effect may ship.
        const MIN_PROFILES: usize = 2;
        /// `Tcp.connect` is the one effect that cannot reach the floor.
        /// Its honest world would have to return `Result.Ok(<connection>)`,
        /// and a connection handle is opaque: only a verify-trace body may
        /// fabricate one (`effect_classification::is_verify_fabricable_handle`),
        /// while a hostile stub is an ordinary top-level fn. So it ships the
        /// failure world alone, and hostile verification of a fn that
        /// connects never sees the connection succeed.
        const SINGLE_PROFILE_EFFECTS: [&str; 1] = ["Tcp.connect"];

        let mut short: Vec<String> = Vec::new();
        for c in classifications_for_proof_subset() {
            if matches!(c.dimension, EffectDimension::Output) {
                continue;
            }
            let count = hostile_profiles_for(c.method).len();
            let floor = if SINGLE_PROFILE_EFFECTS.contains(&c.method) {
                1
            } else {
                MIN_PROFILES
            };
            if count < floor {
                short.push(format!("{} ships {count}, needs {floor}", c.method));
            }
        }
        assert!(
            short.is_empty(),
            "classified non-Output effects with too few hostile profiles — hostile \
             verification would run these under fewer worlds than a sweep needs, or \
             (at zero) skip the effect entirely: {short:?}"
        );

        // A stale exemption is worse than none: it would hold an effect at
        // one profile long after the reason expired.
        for method in SINGLE_PROFILE_EFFECTS {
            assert!(
                hostile_profiles_for(method).len() < MIN_PROFILES,
                "{method} now ships {} hostile profiles — drop it from \
                 SINGLE_PROFILE_EFFECTS so the floor applies to it again",
                hostile_profiles_for(method).len()
            );
        }
    }
}
