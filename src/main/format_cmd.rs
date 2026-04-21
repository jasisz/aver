use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use aver::ast::TopLevel;
use aver::diagnostics::model::AnalysisReport;
use aver::diagnostics::needs_format_diagnostic;
use aver::lexer::Lexer;
use aver::parser::Parser;
use aver::types::{Type, parse_type_str_strict};
use colored::Colorize;

#[allow(dead_code)]
pub(super) fn cmd_format(path: &str, check: bool, json: bool) {
    // JSON mode implies --check: it's a report of diffs, never writes.
    let check = check || json;

    let root = Path::new(path);
    let mut files = Vec::new();
    if let Err(e) = collect_av_files(root, &mut files) {
        if json {
            emit_fatal_json("cannot-collect", &e);
        } else {
            eprintln!("{}", e.red());
        }
        process::exit(1);
    }
    files.sort();

    if files.is_empty() {
        let msg = format!("No .av files found under '{}'", root.display());
        if json {
            emit_fatal_json("no-files", &msg);
        } else {
            eprintln!("{}", msg.red());
        }
        process::exit(1);
    }

    // Keep original + formatted around so JSON mode can emit per-line diff
    // regions via the canonical factory.
    struct Changed {
        path: PathBuf,
        original: String,
        formatted: String,
    }
    let mut changed: Vec<Changed> = Vec::new();

    for file in &files {
        let src = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Cannot read '{}': {}", file.display(), e);
                if json {
                    emit_fatal_json("read-failed", &msg);
                } else {
                    eprintln!("{}", msg.red());
                }
                process::exit(1);
            }
        };
        let formatted = match try_format_source(&src) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("Cannot format '{}': {}", file.display(), e);
                if json {
                    emit_fatal_json("format-failed", &msg);
                } else {
                    eprintln!("{}", msg.red());
                }
                process::exit(1);
            }
        };
        if formatted != src {
            if !check && let Err(e) = fs::write(file, &formatted) {
                eprintln!(
                    "{}",
                    format!("Cannot write '{}': {}", file.display(), e).red()
                );
                process::exit(1);
            }
            changed.push(Changed {
                path: file.clone(),
                original: src,
                formatted,
            });
        }
    }

    if json {
        for c in &changed {
            let file_label = c.path.display().to_string();
            let diag = needs_format_diagnostic(&file_label, &c.original, &c.formatted);
            let report = AnalysisReport::with_diagnostics(file_label, vec![diag]);
            println!("{}", report.to_json());
        }
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"format\":{{\"clean\":{},\"needs_format\":{}}}}}",
            files.len(),
            files.len() - changed.len(),
            changed.len()
        );
        if !changed.is_empty() {
            process::exit(1);
        }
        return;
    }

    if check {
        if changed.is_empty() {
            println!("{}", "Format check passed".green());
            return;
        }
        println!("{}", "Format check failed".red());
        println!("Files that need formatting:");
        for c in &changed {
            println!("  {}", c.path.display());
        }
        process::exit(1);
    }

    if changed.is_empty() {
        println!("{}", "Already formatted".green());
    } else {
        for c in &changed {
            println!("{} {}", "formatted".green(), c.path.display());
        }
        println!("{}", format!("Formatted {} file(s)", changed.len()).green());
    }
}

fn emit_fatal_json(kind: &str, message: &str) {
    use aver::diagnostics::json_escape;
    println!(
        "{{\"schema_version\":1,\"kind\":\"file-error\",\"error_kind\":\"{}\",\"error\":{}}}",
        kind,
        json_escape(message)
    );
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

fn effect_namespace(effect: &str) -> &str {
    match effect.split_once('.') {
        Some((namespace, _)) => namespace,
        None => effect,
    }
}

fn sorted_effects(effects: &[String]) -> Vec<String> {
    let mut sorted = effects.to_vec();
    sorted.sort();
    sorted
}

fn format_block_effect_declaration(indent: &str, effects: &[String]) -> Vec<String> {
    let effects = sorted_effects(effects);
    let inline = format!("{}! [{}]", indent, effects.join(", "));
    if inline.len() <= 100 {
        return vec![inline];
    }

    let mut out = vec![format!("{}! [", indent)];
    let mut start = 0usize;
    while start < effects.len() {
        let namespace = effect_namespace(&effects[start]);
        let mut end = start + 1;
        while end < effects.len() && effect_namespace(&effects[end]) == namespace {
            end += 1;
        }
        out.push(format!("{}    {},", indent, effects[start..end].join(", ")));
        start = end;
    }
    out.push(format!("{}]", indent));
    out
}

fn split_top_level(src: &str, delimiter: char) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut angle_depth = 0usize;
    let mut prev = None;

    for (idx, ch) in src.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.checked_sub(1)?,
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.checked_sub(1)?,
            '<' => angle_depth += 1,
            '>' if prev != Some('-') && angle_depth > 0 => angle_depth -= 1,
            _ => {}
        }

        if ch == delimiter && paren_depth == 0 && bracket_depth == 0 && angle_depth == 0 {
            parts.push(src[start..idx].to_string());
            start = idx + ch.len_utf8();
        }
        prev = Some(ch);
    }

    if paren_depth != 0 || bracket_depth != 0 || angle_depth != 0 {
        return None;
    }

    parts.push(src[start..].to_string());
    Some(parts)
}

fn find_matching_paren(src: &str, open_idx: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (idx, ch) in src.char_indices().skip_while(|(idx, _)| *idx < open_idx) {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn format_type_for_source(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(ok, err) => format!(
            "Result<{}, {}>",
            format_type_for_source(ok),
            format_type_for_source(err)
        ),
        Type::Option(inner) => format!("Option<{}>", format_type_for_source(inner)),
        Type::List(inner) => format!("List<{}>", format_type_for_source(inner)),
        Type::Vector(inner) => format!("Vector<{}>", format_type_for_source(inner)),
        Type::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(format_type_for_source)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Map(key, value) => format!(
            "Map<{}, {}>",
            format_type_for_source(key),
            format_type_for_source(value)
        ),
        Type::Fn(params, ret, effects) => {
            let params = params
                .iter()
                .map(format_type_for_source)
                .collect::<Vec<_>>()
                .join(", ");
            let ret = format_type_for_source(ret);
            let effects = sorted_effects(effects);
            if effects.is_empty() {
                format!("Fn({params}) -> {ret}")
            } else {
                format!("Fn({params}) -> {ret} ! [{}]", effects.join(", "))
            }
        }
        Type::Unknown => "Unknown".to_string(),
        Type::Named(name) => name.clone(),
    }
}

fn normalize_type_annotation(type_src: &str) -> String {
    let trimmed = type_src.trim();
    match parse_type_str_strict(trimmed) {
        Ok(ty) => format_type_for_source(&ty),
        Err(_) => trimmed.to_string(),
    }
}

fn normalize_function_header_effects_line(line: &str) -> String {
    let indent_len = line.chars().take_while(|c| *c == ' ').count();
    let indent = " ".repeat(indent_len);
    let trimmed = line.trim();
    if !trimmed.starts_with("fn ") {
        return line.to_string();
    }

    let open_idx = match trimmed.find('(') {
        Some(idx) => idx,
        None => return line.to_string(),
    };
    let close_idx = match find_matching_paren(trimmed, open_idx) {
        Some(idx) => idx,
        None => return line.to_string(),
    };

    let params_src = &trimmed[open_idx + 1..close_idx];
    let params = match split_top_level(params_src, ',') {
        Some(parts) => parts,
        None => return line.to_string(),
    };
    let formatted_params = params
        .into_iter()
        .filter(|part| !part.trim().is_empty())
        .map(|param| {
            let (name, ty) = match param.split_once(':') {
                Some(parts) => parts,
                None => return param.trim().to_string(),
            };
            format!("{}: {}", name.trim(), normalize_type_annotation(ty))
        })
        .collect::<Vec<_>>()
        .join(", ");

    let mut formatted = format!(
        "{}{}{})",
        indent,
        &trimmed[..open_idx + 1],
        formatted_params
    );
    let remainder = trimmed[close_idx + 1..].trim();
    if let Some(return_type) = remainder.strip_prefix("->") {
        formatted.push_str(" -> ");
        formatted.push_str(&normalize_type_annotation(return_type));
    } else if !remainder.is_empty() {
        formatted.push(' ');
        formatted.push_str(remainder);
    }

    formatted
}

fn normalize_function_header_effects(lines: Vec<String>) -> Vec<String> {
    lines
        .into_iter()
        .map(|line| normalize_function_header_effects_line(&line))
        .collect()
}

fn normalize_effect_declaration_blocks(lines: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lines.len());
    let mut i = 0usize;

    while i < lines.len() {
        let line = &lines[i];
        let trimmed = line.trim();
        if !trimmed.starts_with("! [") {
            out.push(line.clone());
            i += 1;
            continue;
        }

        let indent_len = line.chars().take_while(|c| *c == ' ').count();
        let indent = " ".repeat(indent_len);
        let mut inner = String::new();
        let mut consumed = 0usize;
        let mut found_close = false;

        while i + consumed < lines.len() {
            let current = &lines[i + consumed];
            let current_trimmed = current.trim();
            let segment = if consumed == 0 {
                current_trimmed.trim_start_matches("! [")
            } else {
                current_trimmed
            };

            if let Some(before_close) = segment.strip_suffix(']') {
                if !inner.is_empty() && !before_close.trim().is_empty() {
                    inner.push(' ');
                }
                inner.push_str(before_close.trim());
                found_close = true;
                consumed += 1;
                break;
            }

            if !inner.is_empty() && !segment.trim().is_empty() {
                inner.push(' ');
            }
            inner.push_str(segment.trim());
            consumed += 1;
        }

        if !found_close {
            out.push(line.clone());
            i += 1;
            continue;
        }

        let effects: Vec<String> = if inner.trim().is_empty() {
            vec![]
        } else {
            inner
                .split(',')
                .map(str::trim)
                .filter(|part| !part.is_empty())
                .map(ToString::to_string)
                .collect()
        };

        out.extend(format_block_effect_declaration(&indent, &effects));
        i += consumed;
    }

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

    let lines = normalize_effect_declaration_blocks(lines);
    let lines = normalize_function_header_effects(lines);
    let lines = normalize_module_intent_blocks(lines);
    normalize_inline_decision_fields(lines)
}

fn normalize_module_intent_blocks(lines: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(lines.len());
    let mut in_module_header = false;
    let mut i = 0usize;

    while i < lines.len() {
        let line = &lines[i];
        let trimmed = line.trim();
        let indent = line.chars().take_while(|c| *c == ' ').count();

        if indent == 0 && trimmed.starts_with("module ") {
            in_module_header = true;
            out.push(line.clone());
            i += 1;
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
                    let mut parts = vec![rhs_trimmed.to_string()];
                    let mut consumed = 1usize;

                    while i + consumed < lines.len() {
                        let next = &lines[i + consumed];
                        let next_indent = next.chars().take_while(|c| *c == ' ').count();
                        let next_trimmed = next.trim();

                        if next_indent <= indent || next_trimmed.is_empty() {
                            break;
                        }
                        if !next_trimmed.starts_with('"') {
                            break;
                        }

                        parts.push(next_trimmed.to_string());
                        consumed += 1;
                    }

                    if parts.len() > 1 {
                        out.push(format!("{}intent =", " ".repeat(indent)));
                        for part in parts {
                            out.push(format!("{}{}", " ".repeat(indent + 4), part));
                        }
                        i += consumed;
                        continue;
                    }
                }
            }
        }

        out.push(line.clone());
        i += 1;
    }

    out
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

pub fn try_format_source(source: &str) -> Result<String, String> {
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
pub fn format_source(source: &str) -> String {
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
    fn keeps_inline_module_intent_inline() {
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
    intent = "Inline intent."
    exposes [x]

fn x() -> Int
    1
"#
        );
    }

    #[test]
    fn expands_multiline_module_intent_to_block() {
        let src = r#"module Demo
    intent = "First line."
        "Second line."
    exposes [x]
fn x() -> Int
    1
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"module Demo
    intent =
        "First line."
        "Second line."
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
    intent = "x"
    exposes [main]

decision D
    date = "2026-03-02"
    chosen = "A"
    rejected = ["B"]
    impacts = [main]
"#
        );
    }

    #[test]
    fn keeps_inline_function_description_inline() {
        let src = r#"fn add(a: Int, b: Int) -> Int
    ? "Adds two numbers."
    a + b
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn add(a: Int, b: Int) -> Int
    ? "Adds two numbers."
    a + b
"#
        );
    }

    #[test]
    fn keeps_short_effect_lists_inline() {
        let src = r#"fn apply(f: Fn(Int) -> Int ! [Console.warn, Console.print], x: Int) -> Int
    ! [Http.post, Console.print, Http.get, Console.warn]
    f(x)
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn apply(f: Fn(Int) -> Int ! [Console.print, Console.warn], x: Int) -> Int
    ! [Console.print, Console.warn, Http.get, Http.post]
    f(x)
"#
        );
    }

    #[test]
    fn keeps_medium_effect_lists_inline_when_they_fit() {
        let src = r#"fn run() -> Unit
    ! [Args, Console, Disk, Http, Random, Tcp, Terminal, Time]
    Unit
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn run() -> Unit
    ! [Args, Console, Disk, Http, Random, Tcp, Terminal, Time]
    Unit
"#
        );
    }

    #[test]
    fn expands_long_effect_lists_to_multiline_alphabetical_groups() {
        let src = r#"fn main() -> Unit
    ! [Args.get, Console.print, Console.warn, Time.now, Disk.makeDir, Disk.exists, Disk.readText, Disk.writeText, Disk.appendText]
    Unit
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn main() -> Unit
    ! [
        Args.get,
        Console.print, Console.warn,
        Disk.appendText, Disk.exists, Disk.makeDir, Disk.readText, Disk.writeText,
        Time.now,
    ]
    Unit
"#
        );
    }

    #[test]
    fn sorts_function_type_effects_inline() {
        let src = r#"fn useHandler(handler: Fn(Int) -> Result<String, String> ! [Time.now, Args.get, Console.warn, Console.print, Disk.readText], value: Int) -> Unit
    handler(value)
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn useHandler(handler: Fn(Int) -> Result<String, String> ! [Args.get, Console.print, Console.warn, Disk.readText, Time.now], value: Int) -> Unit
    handler(value)
"#
        );
    }

    #[test]
    fn keeps_long_function_type_effects_inline() {
        let src = r#"fn apply(handler: Fn(Int) -> Int ! [Time.now, Args.get, Console.warn, Console.print, Disk.readText], value: Int) -> Int
    handler(value)
"#;
        let got = format_source(src);
        assert_eq!(
            got,
            r#"fn apply(handler: Fn(Int) -> Int ! [Args.get, Console.print, Console.warn, Disk.readText, Time.now], value: Int) -> Int
    handler(value)
"#
        );
    }
}
