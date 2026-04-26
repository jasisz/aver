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
        "Terminal.size" => vec![HostileProfile {
            name: "minimal",
            stub_fn_name: stub_name("minimal"),
            stub_body: format!(
                "fn {}() -> Terminal.Size\n    ? \"hostile: tiny terminal, layout under pressure\"\n    Terminal.Size(rows = 1, cols = 1)\n",
                stub_name("minimal")
            ),
        }],
        // ─── Generative ───────────────────────────────────────────────
        "Random.int" => vec![
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
        ],
        "Random.float" => vec![
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
        ],
        "Time.unixMs" => vec![
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
        ],
        "Disk.readText" => vec![
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
        "Http.get" | "Http.head" | "Http.delete" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, url: String) -> Result<HttpResponse, String>\n    ? \"hostile: request fails — network is down\"\n    Result.Err(\"hostile: connection refused\")\n",
                stub_name("always_err")
            ),
        }],
        "Http.post" | "Http.put" | "Http.patch" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, url: String, body: String, contentType: String, headers: List<Header>) -> Result<HttpResponse, String>\n    ? \"hostile: write request fails — server unreachable\"\n    Result.Err(\"hostile: connection refused\")\n",
                stub_name("always_err")
            ),
        }],
        "Disk.writeText" | "Disk.appendText" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, file: String, content: String) -> Result<Unit, String>\n    ? \"hostile: write fails — disk full or permission denied\"\n    Result.Err(\"hostile: write failed\")\n",
                stub_name("always_err")
            ),
        }],
        "Disk.delete" | "Disk.deleteDir" | "Disk.makeDir" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, file: String) -> Result<Unit, String>\n    ? \"hostile: filesystem op fails\"\n    Result.Err(\"hostile: op failed\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.send" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, host: String, port: Int, data: String) -> Result<String, String>\n    ? \"hostile: tcp send fails\"\n    Result.Err(\"hostile: send failed\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.ping" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, host: String, port: Int) -> Result<Unit, String>\n    ? \"hostile: ping fails\"\n    Result.Err(\"hostile: unreachable\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.connect" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, host: String, port: Int) -> Result<Tcp.Connection, String>\n    ? \"hostile: cannot establish connection\"\n    Result.Err(\"hostile: refused\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.readLine" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection) -> Result<String, String>\n    ? \"hostile: connection dropped before read\"\n    Result.Err(\"hostile: dropped\")\n",
                stub_name("always_err")
            ),
        }],
        "Tcp.writeLine" | "Tcp.close" => vec![HostileProfile {
            name: "always_err",
            stub_fn_name: stub_name("always_err"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int, conn: Tcp.Connection, line: String) -> Result<Unit, String>\n    ? \"hostile: connection write fails\"\n    Result.Err(\"hostile: dropped\")\n",
                stub_name("always_err")
            ),
        }],
        "Terminal.readKey" => vec![HostileProfile {
            name: "no_input",
            stub_fn_name: stub_name("no_input"),
            stub_body: format!(
                "fn {}(path: BranchPath, n: Int) -> Option<String>\n    ? \"hostile: terminal returns no key — user idle\"\n    Option.None\n",
                stub_name("no_input")
            ),
        }],
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
}
