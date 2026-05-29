//! `aver shape` — dispatch into `diagnostics::shape`.

use std::path::Path;

use aver::diagnostics::shape::{self, RenderOptions};

pub fn cmd_shape(file: &str, module_root: Option<&str>, summary: bool, json: bool) {
    let path = Path::new(file);
    match shape::analyze_path(path, module_root) {
        Ok(report) => {
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
        }
        Err(e) => {
            eprintln!("aver shape: {}", e);
            std::process::exit(2);
        }
    }
}
