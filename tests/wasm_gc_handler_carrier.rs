//! Regression: the synthesized `aver_http_handle` wrapper (`--handler X` /
//! `--preset cloudflare`) reads `Http.Response.status` and hands it to the
//! `Response.text` host import as an i64. Under `Int = ℤ` the status field is
//! the `$AverInt` carrier (the Http.Response factory lifts the host's i64 to a
//! Small), so the wrapper must lower it back to i64 before the i64 local —
//! exactly mirroring the factory. It didn't, so it stored a `(ref null $aint)`
//! into an i64 local and the whole module failed wasm validation.
//!
//! A status LITERAL (`status = 200`) is itself Int arithmetic, so it flips the
//! bignum gate on — meaning this hit EVERY Cloudflare-handler program, not just
//! arithmetic-heavy ones. The backend validates the module internally before
//! writing it, so a successful `aver compile` is the assertion: it would error
//! (no `.wasm` written) without the fix.

#![cfg(feature = "wasm")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn cloudflare_handler_with_bignum_status_validates() {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-handler-carrier-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let src = dir.join("app.av");
    std::fs::write(
        &src,
        r#"module HandlerApp
    intent = "Minimal Cloudflare handler exercising the aver_http_handle wrapper."
    exposes [handler]

fn handler(req: HttpRequest) -> Http.Response
    ? "Always 200 with a tiny body. The status literal flips the bignum gate, so Http.Response.status is the $AverInt carrier the wrapper must lower."
    Http.Response(
        status = 200,
        body = "hi",
        headers = { "content-type" => ["text/plain"] },
    )
"#,
    )
    .expect("write handler source");

    let out_dir = dir.join("out");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")))
        .arg("compile")
        .arg(&src)
        .arg("--preset")
        .arg("cloudflare")
        .arg("--handler")
        .arg("handler")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile executes");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let wasm_written = std::fs::read_dir(&out_dir)
        .map(|rd| {
            rd.filter_map(Result::ok)
                .any(|e| e.path().extension().is_some_and(|x| x == "wasm"))
        })
        .unwrap_or(false);

    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        output.status.success() && wasm_written,
        "Cloudflare handler wrapper failed to compile/validate — likely a \
         `type mismatch: expected i64, found (ref null $type)` from reading the \
         $AverInt-carrier Http.Response.status into an i64 local without lowering.\n\
         stdout:\n{stdout}\nstderr:\n{stderr}"
    );
}
