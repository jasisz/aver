use std::path::PathBuf;
use std::process::ExitCode;

use aver_cert::output::{self, Stream, Style};
use aver_cert::{CheckVerdict, Explanation, Verdict};
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "aver-cert",
    version,
    about = "Independent verifier for Aver artifact certificates",
    after_help = "Every Lean toolchain step runs under a wall-clock limit (900 seconds per \
                  step by default). Set AVER_CERT_PHASE_TIMEOUT_SECS to change it; when a \
                  step exceeds the limit it is stopped and the certificate is not accepted."
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Full fail-closed certificate check.
    Verify {
        /// The wasm-gc module the certificate is about.
        artifact: PathBuf,
        /// The emitted `cert/` directory.
        cert_dir: PathBuf,
    },
    /// Fast developer preflight. Trusts the freshly built or explicitly cached
    /// `.olean` closure and skips whole-closure `leanchecker --fresh`.
    Check {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
    /// Human-readable report backed by the same trusted check as `verify`.
    Explain {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
    /// Alias of `explain`.
    Inspect {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
}

#[derive(Debug, Eq, PartialEq)]
enum Route {
    StrictVerify {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
    TrustedOleanCheck {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
    StrictExplain {
        artifact: PathBuf,
        cert_dir: PathBuf,
    },
}

impl Command {
    fn into_route(self) -> Route {
        match self {
            Self::Verify { artifact, cert_dir } => Route::StrictVerify { artifact, cert_dir },
            Self::Check { artifact, cert_dir } => Route::TrustedOleanCheck { artifact, cert_dir },
            Self::Explain { artifact, cert_dir } | Self::Inspect { artifact, cert_dir } => {
                Route::StrictExplain { artifact, cert_dir }
            }
        }
    }
}

/// Prints a verdict and the lines under it. Every value goes through the
/// output sanitizer; only the verdict word is the checker's own.
fn print_verdict(
    head: &'static str,
    style: Style,
    summary: &str,
    note: Option<&str>,
    laws: &[String],
    faces: &[String],
) {
    output::line(Stream::Out, head, style, summary, Style::Plain);
    if let Some(note) = note {
        output::plain(Stream::Out, &format!("  {note}"));
    }
    for law in laws {
        output::line(Stream::Out, " ", Style::Plain, law, Style::YellowPlain);
    }
    for face in faces {
        output::line(Stream::Out, " ", Style::Plain, face, Style::Plain);
    }
}

fn main() -> ExitCode {
    match Cli::parse().command.into_route() {
        Route::StrictVerify { artifact, cert_dir } => match aver_cert::verify(&artifact, &cert_dir)
        {
            Ok(Verdict::Certified {
                summary,
                faces,
                laws,
                bridged_laws,
                source_bridges,
            }) => {
                let claims: Vec<String> = laws
                    .into_iter()
                    .chain(bridged_laws)
                    .chain(source_bridges)
                    .collect();
                print_verdict(
                    "CERTIFIED",
                    Style::Green,
                    &summary,
                    Some(aver_cert::ARTIFACT_DECODE_LINE),
                    &claims,
                    &faces,
                );
                ExitCode::SUCCESS
            }
            Ok(Verdict::NoExports(summary)) => {
                output::line(
                    Stream::Err,
                    "NO CERTIFIED EXPORTS (admission only, no behavioral claims)",
                    Style::Yellow,
                    &summary,
                    Style::Plain,
                );
                ExitCode::FAILURE
            }
            Err(reason) => {
                output::line(Stream::Err, "DECLINED", Style::Red, &reason, Style::Plain);
                ExitCode::FAILURE
            }
        },
        Route::TrustedOleanCheck { artifact, cert_dir } => {
            match aver_cert::check(&artifact, &cert_dir) {
                Ok(CheckVerdict::Checked {
                    summary,
                    faces,
                    laws,
                    bridged_laws,
                    source_bridges,
                }) => {
                    let claims: Vec<String> = laws
                        .into_iter()
                        .chain(bridged_laws)
                        .chain(source_bridges)
                        .collect();
                    print_verdict(
                        "CHECKED",
                        Style::Cyan,
                        &summary,
                        Some(
                            "trusted freshly built or explicitly cached .olean closure; \
                             whole-closure leanchecker --fresh replay was skipped",
                        ),
                        &claims,
                        &faces,
                    );
                    ExitCode::SUCCESS
                }
                Ok(CheckVerdict::NoExports(summary)) => {
                    output::line(
                        Stream::Err,
                        "NO CHECKED EXPORTS (developer preflight only, no behavioral claims)",
                        Style::Yellow,
                        &summary,
                        Style::Plain,
                    );
                    ExitCode::FAILURE
                }
                Err(reason) => {
                    output::line(
                        Stream::Err,
                        "CHECK FAILED",
                        Style::Red,
                        &reason,
                        Style::Plain,
                    );
                    ExitCode::FAILURE
                }
            }
        }
        Route::StrictExplain { artifact, cert_dir } => {
            match aver_cert::explain(&artifact, &cert_dir) {
                Ok(Explanation::Certified) => ExitCode::SUCCESS,
                Ok(Explanation::NoExports) => ExitCode::FAILURE,
                Err(reason) => {
                    output::line(
                        Stream::Err,
                        "error:",
                        Style::RedPlain,
                        &reason,
                        Style::Plain,
                    );
                    ExitCode::FAILURE
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_route(subcommand: &str) -> Route {
        Cli::try_parse_from(["aver-cert", subcommand, "app.wasm", "out/cert"])
            .expect("certificate command should parse")
            .command
            .into_route()
    }

    #[test]
    fn explain_and_inspect_share_the_same_strict_route() {
        let explain = parse_route("explain");
        let inspect = parse_route("inspect");
        assert_eq!(explain, inspect);
        assert!(matches!(explain, Route::StrictExplain { .. }));
    }
}
