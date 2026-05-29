//! `aver shape` — dispatch into `diagnostics::shape`.

use std::path::{Path, PathBuf};

use aver::config::ProjectConfig;
use aver::diagnostics::shape::{self, RenderOptions};

pub fn cmd_shape(file: &str, module_root: Option<&str>, summary: bool, json: bool, lint: bool) {
    let path = Path::new(file);

    // aver.toml lives in cwd or the file's project root (we use cwd here
    // to match `aver verify` / `aver check` semantics).
    let config_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let config = match ProjectConfig::load_from_dir(&config_dir) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("aver shape: {}", e);
            std::process::exit(2);
        }
    };

    // Layer fingerprint table: project override beats built-in v0.
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

    let report = match shape::analyze_path_with(path, module_root, &fingerprints, basis) {
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
        let Some(cfg) = config.as_ref() else {
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
        // Match against the relative path from config_dir if possible —
        // glob patterns are usually written relative to the project root.
        let rel = path
            .strip_prefix(&config_dir)
            .unwrap_or(path)
            .to_string_lossy()
            .to_string();
        let Some(expected_name) = cfg.shape_expected_for(&rel) else {
            return; // file isn't covered by any glob — silent pass
        };
        let Some(verdict) = report.layer.as_ref() else {
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
}
