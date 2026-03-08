use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::process;

use colored::Colorize;

use crate::cli::ContextDepth;
use crate::context_data::collect_contexts;
use crate::context_format::{
    ContextSelection, collect_all_decisions, format_context_json, format_context_md,
    format_decisions_json, format_decisions_md,
};
use crate::shared::resolve_module_root;

#[derive(Clone)]
struct SelectedContext {
    contexts: Vec<crate::context_data::FileContext>,
    selection: ContextSelection,
}

fn byte_label(bytes: usize) -> String {
    if bytes >= 1024 * 1024 && bytes.is_multiple_of(1024 * 1024) {
        format!("{}mb", bytes / (1024 * 1024))
    } else if bytes >= 1024 && bytes.is_multiple_of(1024) {
        format!("{}kb", bytes / 1024)
    } else {
        format!("{}b", bytes)
    }
}

fn render_context_content(
    contexts: &[crate::context_data::FileContext],
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    selection: Option<&ContextSelection>,
) -> String {
    if decisions_only {
        let decisions = collect_all_decisions(contexts);
        if json {
            format_decisions_json(&decisions, entry_label, selection)
        } else {
            format_decisions_md(&decisions, entry_label, selection)
        }
    } else if json {
        format_context_json(contexts, entry_label, selection)
    } else {
        format_context_md(contexts, entry_label, selection)
    }
}

fn collect_contexts_at_depth(
    file: &str,
    module_root: &str,
    depth: Option<usize>,
) -> Vec<crate::context_data::FileContext> {
    let mut visited = HashSet::new();
    collect_contexts(file, module_root, &mut visited, depth)
}

fn same_contexts(
    left: &[crate::context_data::FileContext],
    right: &[crate::context_data::FileContext],
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(l, r)| l.source_file == r.source_file && l.module_name == r.module_name)
}

fn render_with_selection(
    contexts: &[crate::context_data::FileContext],
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    mut selection: ContextSelection,
) -> (String, ContextSelection) {
    loop {
        let content = render_context_content(
            contexts,
            entry_label,
            json,
            decisions_only,
            Some(&selection),
        );
        let used_bytes = content.len();
        if used_bytes == selection.used_bytes {
            return (content, selection);
        }
        selection.used_bytes = used_bytes;
    }
}

fn collect_contexts_auto(
    file: &str,
    module_root: &str,
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    budget: usize,
) -> SelectedContext {
    let depth_zero = collect_contexts_at_depth(file, module_root, Some(0));
    if depth_zero.is_empty() {
        return SelectedContext {
            contexts: depth_zero,
            selection: ContextSelection {
                depth_mode: "auto".to_string(),
                budget_bytes: Some(budget),
                included_depth: Some(0),
                next_depth: None,
                next_used_bytes: None,
                truncated: false,
                used_bytes: 0,
            },
        };
    }

    let (_, depth_zero_selection) = render_with_selection(
        &depth_zero,
        entry_label,
        json,
        decisions_only,
        ContextSelection {
            depth_mode: "auto".to_string(),
            budget_bytes: Some(budget),
            included_depth: Some(0),
            next_depth: None,
            next_used_bytes: None,
            truncated: false,
            used_bytes: 0,
        },
    );

    if depth_zero_selection.used_bytes > budget {
        let (_, selection) = render_with_selection(
            &depth_zero,
            entry_label,
            json,
            decisions_only,
            ContextSelection {
                depth_mode: "auto".to_string(),
                budget_bytes: Some(budget),
                included_depth: Some(0),
                next_depth: None,
                next_used_bytes: None,
                truncated: true,
                used_bytes: 0,
            },
        );
        return SelectedContext {
            contexts: depth_zero,
            selection,
        };
    }

    let mut fitting = vec![(depth_zero.clone(), 0usize)];
    let mut current = depth_zero;

    for depth in 1usize.. {
        let next = collect_contexts_at_depth(file, module_root, Some(depth));
        if next.is_empty() || same_contexts(&next, &current) {
            let (contexts, included_depth) = fitting
                .last()
                .cloned()
                .expect("depth 0 should always be available");
            let (_, selection) = render_with_selection(
                &contexts,
                entry_label,
                json,
                decisions_only,
                ContextSelection {
                    depth_mode: "auto".to_string(),
                    budget_bytes: Some(budget),
                    included_depth: Some(included_depth),
                    next_depth: None,
                    next_used_bytes: None,
                    truncated: false,
                    used_bytes: 0,
                },
            );
            return SelectedContext {
                contexts,
                selection,
            };
        }

        let (_, next_selection) = render_with_selection(
            &next,
            entry_label,
            json,
            decisions_only,
            ContextSelection {
                depth_mode: "auto".to_string(),
                budget_bytes: Some(budget),
                included_depth: Some(depth),
                next_depth: None,
                next_used_bytes: None,
                truncated: false,
                used_bytes: 0,
            },
        );
        if next_selection.used_bytes > budget {
            for (contexts, included_depth) in fitting.iter().rev() {
                let (_, selection) = render_with_selection(
                    contexts,
                    entry_label,
                    json,
                    decisions_only,
                    ContextSelection {
                        depth_mode: "auto".to_string(),
                        budget_bytes: Some(budget),
                        included_depth: Some(*included_depth),
                        next_depth: Some(depth),
                        next_used_bytes: Some(next_selection.used_bytes),
                        truncated: true,
                        used_bytes: 0,
                    },
                );
                if selection.used_bytes <= budget {
                    return SelectedContext {
                        contexts: contexts.to_vec(),
                        selection,
                    };
                }
            }

            let (contexts, included_depth) = fitting
                .first()
                .cloned()
                .expect("depth 0 should always be available");
            let (_, selection) = render_with_selection(
                &contexts,
                entry_label,
                json,
                decisions_only,
                ContextSelection {
                    depth_mode: "auto".to_string(),
                    budget_bytes: Some(budget),
                    included_depth: Some(included_depth),
                    next_depth: Some(depth),
                    next_used_bytes: Some(next_selection.used_bytes),
                    truncated: true,
                    used_bytes: 0,
                },
            );
            return SelectedContext {
                contexts,
                selection,
            };
        }

        fitting.push((next.clone(), depth));
        current = next;
    }

    unreachable!("unbounded loop should return on stable or oversized depth")
}

fn summarize_selection(selection: &ContextSelection) -> String {
    let mut parts = vec![format!("mode {}", selection.depth_mode)];
    if let Some(depth) = selection.included_depth {
        parts.push(format!("included depth {}", depth));
    }
    parts.push(format!("used {}", byte_label(selection.used_bytes)));
    if let Some(budget) = selection.budget_bytes {
        parts.push(format!("budget {}", byte_label(budget)));
    }
    if selection.truncated {
        parts.push("truncated".to_string());
    }
    if let (Some(next_depth), Some(next_used)) = (selection.next_depth, selection.next_used_bytes) {
        parts.push(format!(
            "next depth {} would use {}",
            next_depth,
            byte_label(next_used)
        ));
    }
    parts.join(", ")
}

pub(super) fn cmd_context(
    file: &str,
    module_root_override: Option<&str>,
    output: Option<&str>,
    json: bool,
    decisions_only: bool,
    depth: ContextDepth,
    budget: usize,
) {
    let module_root = resolve_module_root(module_root_override);

    // Strip module_root prefix so the entry path is relative to the project root
    let entry_label = Path::new(file)
        .strip_prefix(&module_root)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| file.to_string());

    let selected = match depth {
        ContextDepth::Auto => collect_contexts_auto(
            file,
            &module_root,
            &entry_label,
            json,
            decisions_only,
            budget,
        ),
        ContextDepth::Unlimited => SelectedContext {
            contexts: collect_contexts_at_depth(file, &module_root, None),
            selection: ContextSelection {
                depth_mode: "unlimited".to_string(),
                budget_bytes: None,
                included_depth: None,
                next_depth: None,
                next_used_bytes: None,
                truncated: false,
                used_bytes: 0,
            },
        },
        ContextDepth::Limited(limit) => SelectedContext {
            contexts: collect_contexts_at_depth(file, &module_root, Some(limit)),
            selection: ContextSelection {
                depth_mode: "limited".to_string(),
                budget_bytes: None,
                included_depth: Some(limit),
                next_depth: None,
                next_used_bytes: None,
                truncated: false,
                used_bytes: 0,
            },
        },
    };
    if selected.contexts.is_empty() {
        eprintln!("{}", "No content found.".yellow());
        process::exit(1);
    }

    let (content, selection) = render_with_selection(
        &selected.contexts,
        &entry_label,
        json,
        decisions_only,
        selected.selection,
    );

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
            println!("{}", summarize_selection(&selection));
        }
    }
}
