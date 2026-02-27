use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use colored::Colorize;

use aver::ast::{DecisionBlock, TopLevel};
use aver::source::parse_source;

use crate::context_format::{format_decisions_json, format_decisions_md};

const DEFAULT_DECISIONS_PATH: &str = "decisions/architecture.av";
const DEFAULT_DOCS_PATH: &str = "docs/decisions.md";
const GENERATED_BEGIN: &str = "<!-- BEGIN AUTO-GENERATED: decisions -->";
const GENERATED_END: &str = "<!-- END AUTO-GENERATED: decisions -->";

pub(super) fn cmd_decisions(
    source_path: Option<&str>,
    output: Option<&str>,
    json: bool,
    docs: bool,
) {
    if docs && json {
        eprintln!(
            "{} --docs supports Markdown output only (remove --json).",
            "Error:".red()
        );
        process::exit(1);
    }

    let source = source_path.unwrap_or(DEFAULT_DECISIONS_PATH);
    let files = match collect_decision_files(source) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("{} {}", "Error:".red(), e);
            process::exit(1);
        }
    };

    let decisions = match parse_decisions_from_files(&files) {
        Ok(decisions) => decisions,
        Err(e) => {
            eprintln!("{} {}", "Error:".red(), e);
            process::exit(1);
        }
    };

    let decision_refs: Vec<&DecisionBlock> = decisions.iter().collect();
    let rendered = if json {
        format_decisions_json(&decision_refs, source)
    } else {
        format_decisions_md(&decision_refs, source)
    };

    if docs {
        let docs_path = output.unwrap_or(DEFAULT_DOCS_PATH);
        let generated_fragment = strip_decisions_markdown_preamble(&rendered);
        let docs_content = match fs::read_to_string(docs_path) {
            Ok(content) => upsert_generated_section(&content, &generated_fragment),
            Err(_) => make_docs_template(&generated_fragment),
        };
        if let Err(e) = fs::write(docs_path, docs_content) {
            eprintln!(
                "{} Cannot write generated docs '{}': {}",
                "Error:".red(),
                docs_path,
                e
            );
            process::exit(1);
        }
        println!(
            "{}",
            format!("Decisions docs updated at {}", docs_path).green()
        );
        return;
    }

    match output {
        Some(path) => {
            if let Err(e) = fs::write(path, rendered) {
                eprintln!("{} Cannot write '{}': {}", "Error:".red(), path, e);
                process::exit(1);
            }
            println!("{}", format!("Decisions written to {}", path).green());
        }
        None => print!("{}", rendered),
    }
}

fn collect_decision_files(path: &str) -> Result<Vec<PathBuf>, String> {
    let path_buf = PathBuf::from(path);
    if !path_buf.exists() {
        return Err(format!("Path '{}' does not exist", path));
    }

    let mut files = Vec::new();
    collect_decision_files_rec(&path_buf, &mut files)?;
    files.sort_by(|a, b| a.to_string_lossy().cmp(&b.to_string_lossy()));
    if files.is_empty() {
        return Err(format!("No .av files found under '{}'", path));
    }
    Ok(files)
}

fn collect_decision_files_rec(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if path.is_file() {
        if path.extension().is_some_and(|ext| ext == "av") {
            out.push(path.to_path_buf());
        }
        return Ok(());
    }

    let read_dir = fs::read_dir(path)
        .map_err(|e| format!("Cannot read directory '{}': {}", path.display(), e))?;

    for entry in read_dir {
        let entry = entry
            .map_err(|e| format!("Cannot read directory entry in '{}': {}", path.display(), e))?;
        let entry_path = entry.path();
        if entry_path.is_dir() {
            collect_decision_files_rec(&entry_path, out)?;
        } else if entry_path.extension().is_some_and(|ext| ext == "av") {
            out.push(entry_path);
        }
    }
    Ok(())
}

fn parse_decisions_from_files(files: &[PathBuf]) -> Result<Vec<DecisionBlock>, String> {
    let mut seen = std::collections::HashSet::new();
    let mut decisions = Vec::new();
    for file in files {
        let source = fs::read_to_string(file)
            .map_err(|e| format!("Cannot read '{}': {}", file.display(), e))?;
        let items = parse_source(&source)
            .map_err(|e| format!("Parse error in '{}': {}", file.display(), e))?;
        for item in items {
            if let TopLevel::Decision(d) = item {
                let key = format!(
                    "{}|{}|{}|{}|{:?}|{:?}|{:?}",
                    d.name, d.date, d.chosen, d.reason, d.rejected, d.impacts, d.author
                );
                if seen.insert(key) {
                    decisions.push(d);
                }
            }
        }
    }

    // Deterministic output: newest date first, then decision name.
    decisions.sort_by(|a, b| b.date.cmp(&a.date).then_with(|| a.name.cmp(&b.name)));
    Ok(decisions)
}

fn strip_decisions_markdown_preamble(md: &str) -> String {
    let mut lines = md.lines();
    if let Some(first) = lines.next() {
        if first.starts_with("# Aver Decisions - ") {
            let _ = lines.next();
            let _ = lines.next();
            let _ = lines.next();
            return lines.collect::<Vec<_>>().join("\n").trim().to_string();
        }
    }
    md.trim().to_string()
}

fn upsert_generated_section(existing: &str, generated: &str) -> String {
    let Some(begin_pos) = existing.find(GENERATED_BEGIN) else {
        return make_docs_template(generated);
    };
    let Some(end_pos) = existing.find(GENERATED_END) else {
        return make_docs_template(generated);
    };
    if end_pos <= begin_pos {
        return make_docs_template(generated);
    }

    let mut out = String::new();
    out.push_str(&existing[..begin_pos + GENERATED_BEGIN.len()]);
    out.push('\n');
    out.push('\n');
    out.push_str(generated.trim());
    out.push('\n');
    out.push('\n');
    out.push_str(&existing[end_pos..]);
    out
}

fn make_docs_template(generated: &str) -> String {
    format!(
        "# Aver — Decisions\n\n\
This page is partially generated from `decision` blocks.\n\n\
- Update source decisions in `.av` files.\n\
- Regenerate this section with:\n\n\
```bash\n\
aver decisions --docs\n\
```\n\n\
## Generated Decisions\n\n\
{}\n\n\
{}\n{}\n",
        GENERATED_BEGIN,
        generated.trim(),
        GENERATED_END
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upsert_replaces_only_generated_segment() {
        let old = "# X\n\npre\n<!-- BEGIN AUTO-GENERATED: decisions -->\nold\n<!-- END AUTO-GENERATED: decisions -->\npost\n";
        let new_doc = upsert_generated_section(old, "new-content");
        assert!(new_doc.contains("pre"));
        assert!(new_doc.contains("post"));
        assert!(new_doc.contains("new-content"));
        assert!(!new_doc.contains("\nold\n"));
    }

    #[test]
    fn strip_preamble_removes_context_title_block() {
        let full = "# Aver Decisions - x\n\n_Generated by `aver context --decisions-only`_\n\n## A (2026-01-01)\n";
        let stripped = strip_decisions_markdown_preamble(full);
        assert_eq!(stripped, "## A (2026-01-01)");
    }
}
