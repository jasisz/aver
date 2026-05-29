//! `aver shape` — dispatch into `diagnostics::shape`.

use std::path::{Path, PathBuf};

use aver::config::ProjectConfig;
use aver::diagnostics::shape::{self, CorpusEntry, RenderOptions};

use super::shared::resolve_module_root;

pub fn cmd_shape(arg: &str, module_root: Option<&str>, summary: bool, json: bool, lint: bool) {
    let path = Path::new(arg);

    if !path.exists() {
        eprintln!("aver shape: '{}' does not exist", arg);
        std::process::exit(2);
    }

    let config_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let config = match ProjectConfig::load_from_dir(&config_dir) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("aver shape: {}", e);
            std::process::exit(2);
        }
    };

    let (fingerprints, basis) = match config.as_ref() {
        Some(c) if !c.shape_layers.is_empty() => {
            match shape::fingerprints_from_config(&c.shape_layers) {
                Ok(fps) => (fps, "aver.toml"),
                Err(e) => {
                    eprintln!("aver shape: {}", e);
                    std::process::exit(2);
                }
            }
        }
        _ => (shape::builtin_v0_layer_fingerprints(), "built-in v0"),
    };

    if path.is_dir() {
        run_corpus(
            path,
            module_root,
            summary,
            json,
            lint,
            &fingerprints,
            basis,
            config.as_ref(),
            &config_dir,
        );
    } else {
        run_single_file(
            path,
            module_root,
            summary,
            json,
            lint,
            &fingerprints,
            basis,
            config.as_ref(),
            &config_dir,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn run_single_file(
    path: &Path,
    module_root: Option<&str>,
    summary: bool,
    json: bool,
    lint: bool,
    fingerprints: &[shape::LayerFingerprint],
    basis: &str,
    config: Option<&ProjectConfig>,
    config_dir: &Path,
) {
    // Same rule as `aver verify` / `aver check`: explicit `--module-root`
    // wins, otherwise cwd. No walker — keep one rule across commands.
    let resolved_root = resolve_module_root(module_root);
    let report = match shape::analyze_path_with(path, Some(&resolved_root), fingerprints, basis) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("aver shape: {}", e);
            std::process::exit(2);
        }
    };

    if json {
        let value = shape::render_json(&report);
        println!(
            "{}",
            serde_json::to_string_pretty(&value).unwrap_or_else(|_| value.to_string())
        );
    } else {
        let text = shape::render_text(&report, &RenderOptions { summary });
        print!("{}", text);
    }

    if lint {
        run_lint_single(path, config, config_dir, report.layer.as_ref());
    }
}

#[allow(clippy::too_many_arguments)]
fn run_corpus(
    root: &Path,
    module_root: Option<&str>,
    summary: bool,
    json: bool,
    lint: bool,
    fingerprints: &[shape::LayerFingerprint],
    basis: &str,
    config: Option<&ProjectConfig>,
    config_dir: &Path,
) {
    // Same rule as single-file: `--module-root` wins, else cwd.
    let resolved_root = resolve_module_root(module_root);
    let entries = match shape::analyze_dir(root, Some(&resolved_root), fingerprints, basis) {
        Ok(es) => es,
        Err(e) => {
            eprintln!("aver shape: {}", e);
            std::process::exit(2);
        }
    };

    if json {
        let values = shape::render_corpus_json(&entries);
        // NDJSON: one object per line. Easier to stream-process than a
        // wrapping array.
        for v in &values {
            println!(
                "{}",
                serde_json::to_string(v).unwrap_or_else(|_| v.to_string())
            );
        }
    } else {
        let text = shape::render_corpus_text(&entries, &RenderOptions { summary });
        print!("{}", text);
    }

    if lint {
        run_lint_corpus(&entries, config, config_dir, root);
    }
}

fn run_lint_single(
    path: &Path,
    config: Option<&ProjectConfig>,
    config_dir: &Path,
    verdict: Option<&shape::LayerVerdict>,
) {
    let Some(cfg) = config else {
        eprintln!(
            "\naver shape --lint: no aver.toml in {} — nothing to lint against.",
            config_dir.display()
        );
        return;
    };
    if cfg.shape_expected.is_empty() {
        eprintln!(
            "\naver shape --lint: aver.toml has no [[shape.expected]] entries — nothing to lint against."
        );
        return;
    }
    let rel = path
        .strip_prefix(config_dir)
        .unwrap_or(path)
        .to_string_lossy()
        .to_string();
    let Some(expected_name) = cfg.shape_expected_for(&rel) else {
        return;
    };
    let Some(verdict) = verdict else {
        eprintln!(
            "\naver shape --lint: {} has too few fns for a layer guess; can't compare against expected layer '{}'.",
            rel, expected_name,
        );
        return;
    };
    if verdict.layer.as_str() != expected_name {
        eprintln!(
            "\naver shape --lint: {} declared as '{}' in aver.toml [[shape.expected]] but histogram nearest-layer is '{}' (confidence {:.2}, basis: {}).",
            rel,
            expected_name,
            verdict.layer.as_str(),
            verdict.confidence,
            verdict.basis,
        );
        std::process::exit(1);
    }
}

fn run_lint_corpus(
    entries: &[CorpusEntry],
    config: Option<&ProjectConfig>,
    config_dir: &Path,
    corpus_root: &Path,
) {
    let Some(cfg) = config else {
        eprintln!(
            "\naver shape --lint: no aver.toml in {} — nothing to lint against.",
            config_dir.display()
        );
        return;
    };
    if cfg.shape_expected.is_empty() {
        eprintln!(
            "\naver shape --lint: aver.toml has no [[shape.expected]] entries — nothing to lint against."
        );
        return;
    }
    let mut mismatches = 0usize;
    for entry in entries {
        let CorpusEntry::Analyzed { rel_path, report } = entry else {
            continue;
        };
        // For glob matching we use the path relative to config_dir, not
        // corpus_root — the convention is "globs are written relative to
        // the project root", and `aver.toml` lives at the project root.
        let abs = corpus_root.join(rel_path);
        let lint_path = abs
            .strip_prefix(config_dir)
            .unwrap_or(&abs)
            .to_string_lossy()
            .to_string();
        let Some(expected_name) = cfg.shape_expected_for(&lint_path) else {
            continue;
        };
        let Some(verdict) = report.layer.as_ref() else {
            continue;
        };
        if verdict.layer.as_str() != expected_name {
            eprintln!(
                "aver shape --lint: {} declared as '{}' but nearest-layer is '{}' (confidence {:.2}, basis: {}).",
                lint_path,
                expected_name,
                verdict.layer.as_str(),
                verdict.confidence,
                verdict.basis,
            );
            mismatches += 1;
        }
    }
    if mismatches > 0 {
        eprintln!("\naver shape --lint: {} mismatch(es).", mismatches);
        std::process::exit(1);
    }
}
