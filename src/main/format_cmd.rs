use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use aver::ast::TopLevel;
use aver::lexer::Lexer;
use aver::parser::Parser;
use colored::Colorize;

pub(super) fn cmd_format(path: &str, check: bool) {
    let root = Path::new(path);
    let mut files = Vec::new();
    if let Err(e) = collect_av_files(root, &mut files) {
        eprintln!("{}", e.red());
        process::exit(1);
    }
    files.sort();

    if files.is_empty() {
        eprintln!(
            "{}",
            format!("No .av files found under '{}'", root.display()).red()
        );
        process::exit(1);
    }

    let mut changed = Vec::new();
    for file in &files {
        let src = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "{}",
                    format!("Cannot read '{}': {}", file.display(), e).red()
                );
                process::exit(1);
            }
        };
        let formatted = match try_format_source(&src) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "{}",
                    format!("Cannot format '{}': {}", file.display(), e).red()
                );
                process::exit(1);
            }
        };
        if formatted != src {
            changed.push(file.clone());
            if !check && let Err(e) = fs::write(file, formatted) {
                eprintln!(
                    "{}",
                    format!("Cannot write '{}': {}", file.display(), e).red()
                );
                process::exit(1);
            }
        }
    }

    if check {
        if changed.is_empty() {
            println!("{}", "Format check passed".green());
            return;
        }
        println!("{}", "Format check failed".red());
        println!("Files that need formatting:");
        for f in &changed {
            println!("  {}", f.display());
        }
        process::exit(1);
    }

    if changed.is_empty() {
        println!("{}", "Already formatted".green());
    } else {
        for f in &changed {
            println!("{} {}", "formatted".green(), f.display());
        }
        println!("{}", format!("Formatted {} file(s)", changed.len()).green());
    }
}

fn collect_av_files(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if !path.exists() {
        return Err(format!("Path '{}' does not exist", path.display()));
    }

    if path.is_file() {
        if is_av_file(path) {
            out.push(path.to_path_buf());
            return Ok(());
        }
        return Err(format!("'{}' is not an .av file", path.display()));
    }

    let entries = fs::read_dir(path)
        .map_err(|e| format!("Cannot read directory '{}': {}", path.display(), e))?;
    for entry_res in entries {
        let entry = entry_res
            .map_err(|e| format!("Cannot read directory entry in '{}': {}", path.display(), e))?;
        let p = entry.path();
        if p.is_dir() {
            collect_av_files(&p, out)?;
        } else if is_av_file(&p) {
            out.push(p);
        }
    }
    Ok(())
}

fn is_av_file(path: &Path) -> bool {
    path.extension().and_then(|e| e.to_str()) == Some("av")
}

fn normalize_leading_indent(line: &str) -> String {
    let mut end = 0usize;
    for (idx, ch) in line.char_indices() {
        if ch == ' ' || ch == '\t' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }

    let (indent, rest) = line.split_at(end);
    if rest.is_empty() {
        return String::new();
    }

    let mut out = String::new();
    for ch in indent.chars() {
        if ch == '\t' {
            out.push_str("    ");
        } else {
            out.push(ch);
        }
    }
    out.push_str(rest);
    out
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum BlockKind {
    Fn(String),
    Verify(String),
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TopBlock {
    text: String,
    kind: BlockKind,
    start_line: usize,
}

#[derive(Default)]
struct FormatAstInfo {
    kind_by_line: HashMap<usize, BlockKind>,
}

fn classify_block(header_line: &str) -> BlockKind {
    let trimmed = header_line.trim();
    if let Some(rest) = trimmed.strip_prefix("fn ") {
        let name = rest
            .split(['(', ' ', '\t'])
            .next()
            .unwrap_or_default()
            .to_string();
        if !name.is_empty() {
            return BlockKind::Fn(name);
        }
    }
    if let Some(rest) = trimmed.strip_prefix("verify ") {
        let name = rest
            .split([' ', '\t'])
            .next()
            .unwrap_or_default()
            .to_string();
        if !name.is_empty() {
            return BlockKind::Verify(name);
        }
    }
    BlockKind::Other
}

fn is_top_level_start(line: &str) -> bool {
    if line.is_empty() {
        return false;
    }
    if line.starts_with(' ') || line.starts_with('\t') {
        return false;
    }
    !line.trim_start().starts_with("//")
}

fn split_top_level_blocks(lines: &[String], ast_info: Option<&FormatAstInfo>) -> Vec<TopBlock> {
    if lines.is_empty() {
        return Vec::new();
    }

    let starts: Vec<usize> = lines
        .iter()
        .enumerate()
        .filter_map(|(idx, line)| is_top_level_start(line).then_some(idx))
        .collect();

    if starts.is_empty() {
        let text = lines.join("\n").trim_end_matches('\n').to_string();
        if text.is_empty() {
            return Vec::new();
        }
        return vec![TopBlock {
            text,
            kind: BlockKind::Other,
            start_line: 1,
        }];
    }

    let mut blocks = Vec::new();

    // Preserve preamble comments/metadata before first top-level declaration.
    let first = starts[0];
    if first > 0 {
        let mut pre = lines[..first].to_vec();
        while pre.last().is_some_and(|l| l.is_empty()) {
            pre.pop();
        }
        if !pre.is_empty() {
            blocks.push(TopBlock {
                text: pre.join("\n"),
                kind: BlockKind::Other,
                start_line: 1,
            });
        }
    }

    for (i, start) in starts.iter().enumerate() {
        let end = starts.get(i + 1).copied().unwrap_or(lines.len());
        let mut segment = lines[*start..end].to_vec();
        while segment.last().is_some_and(|l| l.is_empty()) {
            segment.pop();
        }
        if segment.is_empty() {
            continue;
        }
        let header = segment[0].clone();
        let start_line = *start + 1;
        let kind = ast_info
            .and_then(|info| info.kind_by_line.get(&start_line).cloned())
            .unwrap_or_else(|| classify_block(&header));
        blocks.push(TopBlock {
            text: segment.join("\n"),
            kind,
            start_line,
        });
    }

    blocks
}

fn reorder_verify_blocks(blocks: Vec<TopBlock>) -> Vec<TopBlock> {
    let verify_blocks: Vec<TopBlock> = blocks
        .iter()
        .filter(|b| matches!(b.kind, BlockKind::Verify(_)))
        .cloned()
        .collect();

    if verify_blocks.is_empty() {
        return blocks;
    }

    let mut by_fn: HashMap<String, Vec<usize>> = HashMap::new();
    for (idx, block) in verify_blocks.iter().enumerate() {
        if let BlockKind::Verify(name) = &block.kind {
            by_fn.entry(name.clone()).or_default().push(idx);
        }
    }

    let mut used = vec![false; verify_blocks.len()];
    let mut out = Vec::new();

    for block in blocks {
        match block.kind.clone() {
            BlockKind::Verify(_) => {}
            BlockKind::Fn(name) => {
                out.push(block);
                if let Some(indices) = by_fn.remove(&name) {
                    for idx in indices {
                        used[idx] = true;
                        out.push(verify_blocks[idx].clone());
                    }
                }
            }
            BlockKind::Other => out.push(block),
        }
    }

    for (idx, block) in verify_blocks.iter().enumerate() {
        if !used[idx] {
            out.push(block.clone());
        }
    }

    out
}

fn parse_ast_info_checked(source: &str) -> Result<FormatAstInfo, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = Parser::new(tokens);
    let items = parser.parse().map_err(|e| e.to_string())?;

    let mut info = FormatAstInfo::default();
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                info.kind_by_line
                    .insert(fd.line, BlockKind::Fn(fd.name.clone()));
            }
            TopLevel::Verify(vb) => {
                info.kind_by_line
                    .insert(vb.line, BlockKind::Verify(vb.fn_name.clone()));
            }
            _ => {}
        }
    }
    Ok(info)
}

fn normalize_source_lines(source: &str) -> Vec<String> {
    let normalized = source.replace("\r\n", "\n").replace('\r', "\n");

    let mut lines = Vec::new();
    for raw in normalized.split('\n') {
        let trimmed = raw.trim_end_matches([' ', '\t']);
        let line = normalize_leading_indent(trimmed);
        lines.push(line);
    }

    let lines = normalize_inline_module_intent(lines);
    normalize_inline_decision_fields(lines)
}

fn normalize_internal_blank_runs(text: &str) -> String {
    let mut out = Vec::new();
    let mut blank_run = 0usize;
    for raw in text.split('\n') {
        if raw.is_empty() {
            blank_run += 1;
            if blank_run <= 2 {
                out.push(String::new());
            }
        } else {
            blank_run = 0;
            out.push(raw.to_string());
        }
    }
    while out.first().is_some_and(|l| l.is_empty()) {
        out.remove(0);
    }
    while out.last().is_some_and(|l| l.is_empty()) {
        out.pop();
    }
    out.join("\n")
}

fn normalize_inline_module_intent(lines: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lines.len());
    let mut in_module_header = false;

    for line in lines {
        let trimmed = line.trim();
        let indent = line.chars().take_while(|c| *c == ' ').count();

        if indent == 0 && trimmed.starts_with("module ") {
            in_module_header = true;
            out.push(line);
            continue;
        }

        if in_module_header && indent == 0 && !trimmed.is_empty() && !trimmed.starts_with("//") {
            in_module_header = false;
        }

        if in_module_header && indent > 0 {
            let head = &line[indent..];
            if let Some(rhs) = head.strip_prefix("intent =") {
                let rhs_trimmed = rhs.trim_start();
                if rhs_trimmed.starts_with('"') {
                    out.push(format!("{}intent =", " ".repeat(indent)));
                    out.push(format!("{}{}", " ".repeat(indent + 4), rhs_trimmed));
                    continue;
                }
            }
        }

        out.push(line);
    }

    out
}

const DECISION_FIELDS: [&str; 6] = ["date", "author", "reason", "chosen", "rejected", "impacts"];

fn starts_with_decision_field(content: &str) -> bool {
    DECISION_FIELDS
        .iter()
        .any(|field| content.starts_with(&format!("{field} =")))
}

fn find_next_decision_field_boundary(s: &str) -> Option<usize> {
    let mut best: Option<usize> = None;
    for field in DECISION_FIELDS {
        let needle = format!(" {field} =");
        let mut search_from = 0usize;
        while let Some(rel) = s[search_from..].find(&needle) {
            let idx = search_from + rel;
            // Require at least two spaces before the next field marker, so
            // normal single-space tokens don't split accidentally.
            let spaces_before = s[..idx].chars().rev().take_while(|c| *c == ' ').count();
            // `needle` starts at one of the separating spaces, so include it.
            let total_separator_spaces = spaces_before + 1;
            if total_separator_spaces >= 2 {
                let field_start = idx + 1;
                best = Some(best.map_or(field_start, |cur| cur.min(field_start)));
                break;
            }
            search_from = idx + 1;
        }
    }
    best
}

fn split_inline_decision_fields(content: &str) -> Vec<String> {
    if !starts_with_decision_field(content) {
        return vec![content.to_string()];
    }
    let mut out = Vec::new();
    let mut rest = content.trim_end().to_string();
    while let Some(idx) = find_next_decision_field_boundary(&rest) {
        let left = rest[..idx].trim_end().to_string();
        if left.is_empty() {
            break;
        }
        out.push(left);
        rest = rest[idx..].trim_start().to_string();
    }
    if !rest.is_empty() {
        out.push(rest.trim_end().to_string());
    }
    if out.is_empty() {
        vec![content.to_string()]
    } else {
        out
    }
}

fn normalize_inline_decision_fields(lines: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lines.len());
    let mut in_decision = false;

    for line in lines {
        let trimmed = line.trim();
        let indent = line.chars().take_while(|c| *c == ' ').count();

        if indent == 0 && trimmed.starts_with("decision ") {
            in_decision = true;
            out.push(line);
            continue;
        }

        if in_decision && indent == 0 && !trimmed.is_empty() && !trimmed.starts_with("//") {
            in_decision = false;
        }

        if in_decision && trimmed.is_empty() {
            continue;
        }

        if in_decision && indent > 0 {
            let content = &line[indent..];
            let parts = split_inline_decision_fields(content);
            if parts.len() > 1 {
                for part in parts {
                    out.push(format!("{}{}", " ".repeat(indent), part));
                }
                continue;
            }
        }

        out.push(line);
    }

    out
}

fn try_format_source(source: &str) -> Result<String, String> {
    let lines = normalize_source_lines(source);
    let normalized = lines.join("\n");
    let ast_info = parse_ast_info_checked(&normalized)?;

    // 3) Split into top-level blocks and co-locate verify blocks under their functions.
    let blocks = split_top_level_blocks(&lines, Some(&ast_info));
    let reordered = reorder_verify_blocks(blocks);

    // 4) Rejoin with one blank line between top-level blocks.
    let mut non_empty_blocks = Vec::new();
    for block in reordered {
        let text = normalize_internal_blank_runs(&block.text);
        let text = text.trim_matches('\n').to_string();
        if !text.is_empty() {
            non_empty_blocks.push(text);
        }
    }

    if non_empty_blocks.is_empty() {
        return Ok("\n".to_string());
    }
    let mut out = non_empty_blocks.join("\n\n");
    out.push('\n');
    Ok(out)
}

#[cfg(test)]
pub(super) fn format_source(source: &str) -> String {
    match try_format_source(source) {
        Ok(formatted) => formatted,
        Err(err) => panic!("format_source received invalid Aver source: {err}"),
    }
}

#[cfg(test)]
mod tests {
    use super::{format_source, try_format_source};

    #[test]
    fn normalizes_line_endings_and_trailing_ws() {
        let src = "module A\r\n    fn x() -> Int   \r\n        1\t \r\n";
        let got = format_source(src);
        assert_eq!(got, "module A\n    fn x() -> Int\n        1\n");
    }

    #[test]
    fn converts_leading_tabs_only() {
        let src = "\tfn x() -> String\n\t\t\"a\\tb\"\n";
        let got = format_source(src);
        assert_eq!(got, "    fn x() -> String\n        \"a\\tb\"\n");
    }

    #[test]
    fn collapses_long_blank_runs() {
        let src = "module A\n\n\n\nfn x() -> Int\n    1\n";
        let got = format_source(src);
        assert_eq!(got, "module A\n\nfn x() -> Int\n    1\n");
    }

    #[test]
    fn keeps_single_final_newline() {
        let src = "module A\nfn x() -> Int\n    1\n\n\n";
        let got = format_source(src);
        assert_eq!(got, "module A\n\nfn x() -> Int\n    1\n");
    }

    #[test]
    fn rejects_removed_eq_expr_syntax() {
        let src = "fn x() -> Int\n    = 1\n";
        let err = try_format_source(src).expect_err("old '= expr' syntax should fail");
        assert!(
            err.contains("no longer use '= expr'"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn moves_verify_directly_under_function() {
        let src = r#"module Demo

fn a(x: Int) -> Int
    x + 1

fn b(x: Int) -> Int
    x + 2

verify a
    a(1) => 2

verify b
    b(1) => 3
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"module Demo

fn a(x: Int) -> Int
    x + 1

verify a
    a(1) => 2

fn b(x: Int) -> Int
    x + 2

verify b
    b(1) => 3
"#
        );
    }

    #[test]
    fn leaves_orphan_verify_at_end() {
        let src = r#"module Demo

verify missing
    missing(1) => 2
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"module Demo

verify missing
    missing(1) => 2
"#
        );
    }

    #[test]
    fn expands_inline_module_intent_to_block() {
        let src = r#"module Demo
    intent = "Inline intent."
    exposes [x]
fn x() -> Int
    1
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"module Demo
    intent =
        "Inline intent."
    exposes [x]

fn x() -> Int
    1
"#
        );
    }

    #[test]
    fn splits_inline_decision_fields_to_separate_lines() {
        let src = r#"module Demo
    intent = "x"
    exposes [main]

decision D
    date = "2026-03-02"
    chosen = "A"    rejected = ["B"]
    impacts = [main]
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"module Demo
    intent =
        "x"
    exposes [main]

decision D
    date = "2026-03-02"
    chosen = "A"
    rejected = ["B"]
    impacts = [main]
"#
        );
    }
}
