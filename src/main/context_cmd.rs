use std::collections::HashSet;
use std::fs;
use std::process;

use colored::Colorize;

use crate::context_data::collect_contexts;
use crate::context_format::{
    collect_all_decisions, format_context_json, format_context_md, format_decisions_json,
    format_decisions_md,
};
use crate::shared::resolve_module_root;

pub(super) fn cmd_context(
    file: &str,
    module_root_override: Option<&str>,
    output: Option<&str>,
    json: bool,
    decisions_only: bool,
) {
    let module_root = resolve_module_root(module_root_override);
    let mut visited = HashSet::new();
    let contexts = collect_contexts(file, &module_root, &mut visited);

    if contexts.is_empty() {
        eprintln!("{}", "No content found.".yellow());
        process::exit(1);
    }

    let content = if decisions_only {
        let decisions = collect_all_decisions(&contexts);
        if json {
            format_decisions_json(&decisions, file)
        } else {
            format_decisions_md(&decisions, file)
        }
    } else if json {
        format_context_json(&contexts, file)
    } else {
        format_context_md(&contexts, file)
    };

    match output {
        None => print!("{}", content),
        Some(out_path) => {
            if let Err(e) = fs::write(out_path, &content) {
                eprintln!("{} Cannot write to '{}': {}", "Error:".red(), out_path, e);
                process::exit(1);
            }
            if decisions_only {
                println!("{}", format!("Decisions written to {}", out_path).green());
            } else {
                println!("{}", format!("Context written to {}", out_path).green());
            }
        }
    }
}
