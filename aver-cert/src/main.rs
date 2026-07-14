use std::path::PathBuf;
use std::process::ExitCode;

use aver_cert::{Explanation, Verdict};
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

fn main() -> ExitCode {
    match Cli::parse().command {
        Command::Verify { artifact, cert_dir } => match aver_cert::verify(&artifact, &cert_dir) {
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
        Command::Explain { artifact, cert_dir } | Command::Inspect { artifact, cert_dir } => {
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
