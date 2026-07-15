use std::path::PathBuf;
use std::process::ExitCode;

use aver_cert::{CheckVerdict, Explanation, Verdict};
use clap::{Parser, Subcommand};
use colored::Colorize;

#[derive(Parser)]
#[command(
    name = "aver-cert",
    version,
    about = "Independent verifier for Aver artifact certificates"
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

fn main() -> ExitCode {
    match Cli::parse().command.into_route() {
        Route::StrictVerify { artifact, cert_dir } => match aver_cert::verify(&artifact, &cert_dir)
        {
            Ok(Verdict::Certified { summary, faces }) => {
                println!("{} {}", "CERTIFIED".green().bold(), summary);
                println!("  {}", aver_cert::ARTIFACT_DECODE_LINE);
                for face in faces {
                    println!("  {face}");
                }
                ExitCode::SUCCESS
            }
            Ok(Verdict::NoExports(summary)) => {
                eprintln!(
                    "{} {}",
                    "NO CERTIFIED EXPORTS (admission only, no behavioral claims)"
                        .yellow()
                        .bold(),
                    summary
                );
                ExitCode::FAILURE
            }
            Err(reason) => {
                eprintln!("{} {}", "DECLINED".red().bold(), reason);
                ExitCode::FAILURE
            }
        },
        Route::TrustedOleanCheck { artifact, cert_dir } => {
            match aver_cert::check(&artifact, &cert_dir) {
                Ok(CheckVerdict::Checked { summary, faces }) => {
                    println!("{} {}", "CHECKED".cyan().bold(), summary);
                    println!(
                        "  trusted freshly built or explicitly cached .olean closure; \
                         whole-closure leanchecker --fresh replay was skipped"
                    );
                    for face in faces {
                        println!("  {face}");
                    }
                    ExitCode::SUCCESS
                }
                Ok(CheckVerdict::NoExports(summary)) => {
                    eprintln!(
                        "{} {}",
                        "NO CHECKED EXPORTS (developer preflight only, no behavioral claims)"
                            .yellow()
                            .bold(),
                        summary
                    );
                    ExitCode::FAILURE
                }
                Err(reason) => {
                    eprintln!("{} {}", "CHECK FAILED".red().bold(), reason);
                    ExitCode::FAILURE
                }
            }
        }
        Route::StrictExplain { artifact, cert_dir } => {
            match aver_cert::explain(&artifact, &cert_dir) {
                Ok(Explanation::Certified) => ExitCode::SUCCESS,
                Ok(Explanation::NoExports) => ExitCode::FAILURE,
                Err(reason) => {
                    eprintln!("{} {}", "error:".red(), reason);
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
