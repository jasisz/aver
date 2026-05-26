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

    // Keep original source + formatter violations so JSON/tty modes can
    // render precise per-rule diagnostics via the canonical factory.
    struct Changed {
        path: PathBuf,
        original: String,
        violations: Vec<aver::diagnostics::model::FormatViolation>,
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
        let (formatted, violations) = match try_format_source(&src) {
            Ok(pair) => pair,
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
                violations,
            });
        }
    }

    if json {
        for c in &changed {
            let file_label = c.path.display().to_string();
            let diag = needs_format_diagnostic(&file_label, &c.violations, &c.original);
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
        for (i, c) in changed.iter().enumerate() {
            if i > 0 {
                println!();
            }
            let file_label = c.path.display().to_string();
            let diag = needs_format_diagnostic(&file_label, &c.violations, &c.original);
            // verbose=true so violation regions render in tty too —
            // parity with `--check --json` consumers.
            print!("{}", aver::tty_render::render_tty(&diag, true));
        }
        println!();
        println!(
            "{}: {} file(s) need formatting",
            "Format check failed".red(),
            changed.len()
        );
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

/// Normalize a single line's leading indent: expand tabs to 4 spaces,
/// strip trailing whitespace implicitly by collapsing empty content.
///
/// Returns the rewritten line plus an optional violation if the input
/// carried mixed / tab-based indent. The caller supplies `source_line`
/// so the violation can point back to the original source.
fn normalize_leading_indent_tracked(
    line: &str,
    source_line: usize,
) -> (String, Option<aver::diagnostics::model::FormatViolation>) {
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
        // Line was only whitespace: not a format violation — formatter
        // collapses to empty regardless of what the user typed.
        return (String::new(), None);
    }

    let had_tab = indent.contains('\t');
    let mut out = String::new();
    for ch in indent.chars() {
        if ch == '\t' {
            out.push_str("    ");
        } else {
            out.push(ch);
        }
    }
    out.push_str(rest);

    let violation = if had_tab {
        Some(aver::diagnostics::model::FormatViolation {
            line: source_line,
            col: 1,
            rule: "tab-indent",
            message: "tab in leading indent; formatter expands to 4 spaces".to_string(),
            before: Some(indent.replace('\t', "\\t")),
            after: Some(indent.replace('\t', "    ")),
        })
    } else {
        None
    };

    (out, violation)
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
    format_bracketed_effect_list(indent, "! ", effects)
}

fn format_module_effects_declaration(indent: &str, effects: &[String]) -> Vec<String> {
    format_bracketed_effect_list(indent, "effects ", effects)
}

fn format_bracketed_effect_list(indent: &str, lead: &str, effects: &[String]) -> Vec<String> {
    let effects = sorted_effects(effects);
    let inline = format!("{}{}[{}]", indent, lead, effects.join(", "));
    if inline.len() <= 100 {
        return vec![inline];
    }

    let mut out = vec![format!("{}{}[", indent, lead)];
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
        Type::Var(name) => name.clone(),
        Type::Invalid => "Invalid".to_string(),
        Type::Named { name, .. } => name.clone(),
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

/// Per-line formatter for function headers.
///
/// When `line_offset` is provided, each rewritten line pushes a
/// `bad-function-header` violation keyed on the original source line.
/// `line_offset` is a `Vec<usize>` mapping input-line-index → source
/// line number (1-based) so the factory can point back at the user's
/// source accurately.
fn normalize_function_header_effects_tracked(
    lines: Vec<String>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
    line_offset: Option<&[usize]>,
) -> Vec<String> {
    lines
        .into_iter()
        .enumerate()
        .map(|(idx, line)| {
            let rewritten = normalize_function_header_effects_line(&line);
            if rewritten != line {
                let source_line = line_offset.and_then(|off| off.get(idx)).copied().unwrap_or(idx + 1);
                violations.push(aver::diagnostics::model::FormatViolation {
                    line: source_line,
                    col: 1,
                    rule: "bad-function-header",
                    message:
                        "function signature spacing / parameter separator differs from canonical form"
                            .to_string(),
                    before: Some(line.clone()),
                    after: Some(rewritten.clone()),
                });
            }
            rewritten
        })
        .collect()
}

fn normalize_effect_declaration_blocks_tracked(
    lines: Vec<String>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
    line_offset: Option<&[usize]>,
) -> Vec<String> {
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

        let original_block: Vec<String> = lines[i..i + consumed].to_vec();
        let rewritten_block = format_block_effect_declaration(&indent, &effects);
        if original_block != rewritten_block {
            let source_line = line_offset
                .and_then(|off| off.get(i))
                .copied()
                .unwrap_or(i + 1);
            let rule = {
                let mut sorted = effects.clone();
                sorted.sort();
                if effects != sorted {
                    "effects-unsorted"
                } else {
                    "effects-reshape"
                }
            };
            let message = match rule {
                "effects-unsorted" => {
                    "effect list out of order; formatter sorts alphabetically".to_string()
                }
                _ => "effect declaration reshaped to canonical form".to_string(),
            };
            violations.push(aver::diagnostics::model::FormatViolation {
                line: source_line,
                col: 1,
                rule,
                message,
                before: Some(original_block.join(" | ")),
                after: Some(rewritten_block.join(" | ")),
            });
        }
        out.extend(rewritten_block);
        i += consumed;
    }

    out
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum BlockKind {
    Fn(String),
    /// Oracle stub fn — first param is `path: BranchPath`, no
    /// declared effects. These typically sit between a verified fn
    /// and its `verify ... law` block (the law's `given` clause
    /// names them); the format reorderer treats them as glue inside
    /// a fn-and-its-verifies group, not standalone fn definitions
    /// that would push the verify block out of place.
    FnStub(String),
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

/// Heuristic: is this fn an oracle stub? Stubs start with
/// `(path: BranchPath, n: Int, ...)` and declare no effects of
/// their own — they exist so a `verify ... law` block can name
/// them in a `given` clause. Tagging them lets the format
/// reorderer keep them in place between the fn under test and its
/// verify block instead of demanding the verify follow the fn
/// directly.
fn is_oracle_stub_fn(fd: &aver::ast::FnDef) -> bool {
    if !fd.effects.is_empty() {
        return false;
    }
    let Some((_, first_ty)) = fd.params.first() else {
        return false;
    };
    first_ty.trim() == "BranchPath"
        || first_ty.trim().starts_with("BranchPath ")
        || first_ty.trim().starts_with("BranchPath\t")
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

fn reorder_verify_blocks_tracked(
    blocks: Vec<TopBlock>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
) -> Vec<TopBlock> {
    let verify_blocks: Vec<TopBlock> = blocks
        .iter()
        .filter(|b| matches!(b.kind, BlockKind::Verify(_)))
        .cloned()
        .collect();

    if verify_blocks.is_empty() {
        return blocks;
    }

    // Remember the original position (0-based index in `blocks`) of
    // each verify block so we can flag a violation if it ends up moving.
    let mut original_positions: HashMap<(String, usize), usize> = HashMap::new();
    for (pos, block) in blocks.iter().enumerate() {
        if let BlockKind::Verify(name) = &block.kind {
            original_positions.insert((name.clone(), block.start_line), pos);
        }
    }

    let mut by_fn: HashMap<String, Vec<usize>> = HashMap::new();
    for (idx, block) in verify_blocks.iter().enumerate() {
        if let BlockKind::Verify(name) = &block.kind {
            by_fn.entry(name.clone()).or_default().push(idx);
        }
    }

    let mut used = vec![false; verify_blocks.len()];
    let mut out: Vec<TopBlock> = Vec::new();

    let blocks_vec: Vec<TopBlock> = blocks;
    let mut i = 0;
    while i < blocks_vec.len() {
        let block = &blocks_vec[i];
        match block.kind.clone() {
            BlockKind::Verify(_) => {
                i += 1;
            }
            BlockKind::Fn(name) => {
                out.push(block.clone());
                i += 1;
                // Greedy: pull every FnStub block that immediately
                // follows. They're considered glue between the fn
                // under test and its verify block, not standalone
                // fn definitions.
                while i < blocks_vec.len() && matches!(blocks_vec[i].kind, BlockKind::FnStub(_)) {
                    out.push(blocks_vec[i].clone());
                    i += 1;
                }
                if let Some(indices) = by_fn.remove(&name) {
                    for idx in indices {
                        used[idx] = true;
                        out.push(verify_blocks[idx].clone());
                    }
                }
            }
            BlockKind::FnStub(_) => {
                // Stub not preceded by a verified fn (rare — usually
                // a top-level helper). Push as-is; treat like Other.
                out.push(block.clone());
                i += 1;
            }
            BlockKind::Other => {
                out.push(block.clone());
                i += 1;
            }
        }
    }

    for (idx, block) in verify_blocks.iter().enumerate() {
        if !used[idx] {
            out.push(block.clone());
        }
    }

    // Any verify block whose final position (in `out`) differs from its
    // original position (in `blocks`) is a violation — the formatter
    // moved it. Key by (name, start_line) to disambiguate duplicates.
    for (new_pos, block) in out.iter().enumerate() {
        if let BlockKind::Verify(name) = &block.kind {
            let key = (name.clone(), block.start_line);
            if let Some(&orig_pos) = original_positions.get(&key)
                && orig_pos != new_pos
            {
                violations.push(aver::diagnostics::model::FormatViolation {
                    line: block.start_line,
                    col: 1,
                    rule: "verify-misplaced",
                    message: format!(
                        "verify block '{}' should be placed immediately after its function",
                        name
                    ),
                    before: None,
                    after: None,
                });
            }
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
                let kind = if is_oracle_stub_fn(&fd) {
                    BlockKind::FnStub(fd.name.clone())
                } else {
                    BlockKind::Fn(fd.name.clone())
                };
                info.kind_by_line.insert(fd.line, kind);
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

/// Normalize source lines and accumulate per-rule format violations.
///
/// Each violation references the **original** 1-based source line,
/// tracked through `normalize_leading_indent_tracked`. Rules further
/// downstream still operate on `Vec<String>` today and remain silent
/// contributors to the `violations` accumulator — migration is
/// incremental, one rule at a time.
fn normalize_source_lines_tracked(
    source: &str,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
) -> Vec<String> {
    let normalized = source.replace("\r\n", "\n").replace('\r', "\n");

    let mut lines = Vec::new();
    // Track original source line per position so downstream tracked
    // passes can keep accurate violation coordinates. Per-line rules
    // preserve count; reshape rules break this map and fall back to
    // their own heuristics.
    let mut line_offset: Vec<usize> = Vec::new();
    for (idx, raw) in normalized.split('\n').enumerate() {
        let trimmed = raw.trim_end_matches([' ', '\t']);
        if trimmed.len() != raw.len() {
            violations.push(aver::diagnostics::model::FormatViolation {
                line: idx + 1,
                col: trimmed.len() + 1,
                rule: "trailing-whitespace",
                message: "trailing whitespace".to_string(),
                before: None,
                after: None,
            });
        }
        let (line, violation) = normalize_leading_indent_tracked(trimmed, idx + 1);
        if let Some(v) = violation {
            violations.push(v);
        }
        lines.push(line);
        line_offset.push(idx + 1);
    }

    let lines = normalize_effect_declaration_blocks_tracked(lines, violations, Some(&line_offset));
    let lines = normalize_function_header_effects_tracked(lines, violations, Some(&line_offset));
    let lines = normalize_module_intent_blocks_tracked(lines, violations, Some(&line_offset));
    let lines = normalize_module_effects_blocks_tracked(lines, violations, Some(&line_offset));
    normalize_inline_decision_fields_tracked(lines, violations, Some(&line_offset))
}

fn normalize_module_effects_blocks_tracked(
    lines: Vec<String>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
    line_offset: Option<&[usize]>,
) -> Vec<String> {
    let mut out = Vec::with_capacity(lines.len());
    let mut in_module_header = false;
    let mut i = 0usize;

    while i < lines.len() {
        let line = &lines[i];
        let trimmed = line.trim();
        let indent_len = line.chars().take_while(|c| *c == ' ').count();

        if indent_len == 0 && trimmed.starts_with("module ") {
            in_module_header = true;
            out.push(line.clone());
            i += 1;
            continue;
        }
        if in_module_header && indent_len == 0 && !trimmed.is_empty() && !trimmed.starts_with("//")
        {
            in_module_header = false;
        }

        if !(in_module_header && indent_len > 0 && trimmed.starts_with("effects ")) {
            out.push(line.clone());
            i += 1;
            continue;
        }

        // Found a module-level `effects [...]`. Span may be inline or
        // multi-line; collect until the matching `]`.
        let indent = " ".repeat(indent_len);
        let head = trimmed.trim_start_matches("effects").trim_start();
        if !head.starts_with('[') {
            out.push(line.clone());
            i += 1;
            continue;
        }

        let mut inner = String::new();
        let mut consumed = 1usize;
        let mut found_close = false;
        let first_open = &head[1..];

        // Single-line case: `effects [a, b, c]`.
        if let Some(before_close) = first_open.strip_suffix(']') {
            inner.push_str(before_close.trim());
            found_close = true;
        } else {
            // Multi-line: scan subsequent lines until the closing `]`.
            inner.push_str(first_open.trim());
            while i + consumed < lines.len() {
                let next = &lines[i + consumed];
                let next_trimmed = next.trim();
                if let Some(before_close) = next_trimmed.strip_suffix(']') {
                    if !inner.is_empty() && !before_close.trim().is_empty() {
                        inner.push(' ');
                    }
                    inner.push_str(before_close.trim());
                    consumed += 1;
                    found_close = true;
                    break;
                }
                if !inner.is_empty() && !next_trimmed.is_empty() {
                    inner.push(' ');
                }
                inner.push_str(next_trimmed);
                consumed += 1;
            }
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

        let original_block: Vec<String> = lines[i..i + consumed].to_vec();
        let rewritten_block = format_module_effects_declaration(&indent, &effects);
        if original_block != rewritten_block {
            let source_line = line_offset
                .and_then(|off| off.get(i))
                .copied()
                .unwrap_or(i + 1);
            let rule = {
                let mut sorted = effects.clone();
                sorted.sort();
                if effects != sorted {
                    "module-effects-unsorted"
                } else {
                    "module-effects-reshape"
                }
            };
            let message = match rule {
                "module-effects-unsorted" => {
                    "module effect list out of order; formatter sorts alphabetically".to_string()
                }
                _ => "module effect declaration reshaped to canonical form".to_string(),
            };
            violations.push(aver::diagnostics::model::FormatViolation {
                line: source_line,
                col: 1,
                rule,
                message,
                before: Some(original_block.join(" | ")),
                after: Some(rewritten_block.join(" | ")),
            });
        }
        out.extend(rewritten_block);
        i += consumed;
    }

    out
}

fn normalize_module_intent_blocks_tracked(
    lines: Vec<String>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
    line_offset: Option<&[usize]>,
) -> Vec<String> {
    let before = lines.clone();
    let after = normalize_module_intent_blocks_impl(lines);
    if before != after {
        // Find first differing input line and flag it.
        let diff_idx = before
            .iter()
            .zip(&after)
            .position(|(a, b)| a != b)
            .unwrap_or(0);
        let source_line = line_offset
            .and_then(|off| off.get(diff_idx))
            .copied()
            .unwrap_or(diff_idx + 1);
        violations.push(aver::diagnostics::model::FormatViolation {
            line: source_line,
            col: 1,
            rule: "module-intent-reshape",
            message: "module intent block reshaped to canonical multiline form".to_string(),
            before: None,
            after: None,
        });
    }
    after
}

fn normalize_module_intent_blocks_impl(lines: Vec<String>) -> Vec<String> {
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

/// Collapse internal blank-line runs to at most 2, strip leading/trailing
/// blanks. `block_start_line` is the 1-based source line of the block's
/// first line so violations point back at the original source.
fn normalize_internal_blank_runs_tracked(
    text: &str,
    block_start_line: usize,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
) -> String {
    let mut out = Vec::new();
    let mut blank_run = 0usize;
    let mut run_start_idx: Option<usize> = None;
    for (rel_idx, raw) in text.split('\n').enumerate() {
        if raw.is_empty() {
            if blank_run == 0 {
                run_start_idx = Some(rel_idx);
            }
            blank_run += 1;
            if blank_run <= 2 {
                out.push(String::new());
            }
        } else {
            if blank_run > 2
                && let Some(start) = run_start_idx
            {
                let line = block_start_line.saturating_add(start).max(1);
                violations.push(aver::diagnostics::model::FormatViolation {
                    line,
                    col: 1,
                    rule: "excess-blank",
                    message: format!(
                        "{} consecutive blank lines; formatter collapses to 2",
                        blank_run
                    ),
                    before: None,
                    after: None,
                });
            }
            blank_run = 0;
            run_start_idx = None;
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

fn normalize_inline_decision_fields_tracked(
    lines: Vec<String>,
    violations: &mut Vec<aver::diagnostics::model::FormatViolation>,
    line_offset: Option<&[usize]>,
) -> Vec<String> {
    let before = lines.clone();
    let after = normalize_inline_decision_fields_impl(lines);
    if before != after {
        let diff_idx = before
            .iter()
            .zip(&after)
            .position(|(a, b)| a != b)
            .unwrap_or(0);
        let source_line = line_offset
            .and_then(|off| off.get(diff_idx))
            .copied()
            .unwrap_or(diff_idx + 1);
        violations.push(aver::diagnostics::model::FormatViolation {
            line: source_line,
            col: 1,
            rule: "decision-inline",
            message: "decision fields should each live on their own line".to_string(),
            before: None,
            after: None,
        });
    }
    after
}

fn normalize_inline_decision_fields_impl(lines: Vec<String>) -> Vec<String> {
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

/// Format `source` and return the rewritten text plus a list of
/// [`FormatViolation`]s — one per rule that fired on a specific
/// location. Etap A: violations Vec is allocated but rules don't yet
/// populate it; callers must not claim precise line ranges.
/// Subsequent commits migrate each `normalize_*` rule to push to this
/// vec as they rewrite.
pub fn try_format_source(
    source: &str,
) -> Result<(String, Vec<aver::diagnostics::model::FormatViolation>), String> {
    let mut violations: Vec<aver::diagnostics::model::FormatViolation> = Vec::new();

    if !source.is_empty() && !source.ends_with('\n') {
        let last_line = source.lines().count().max(1);
        violations.push(aver::diagnostics::model::FormatViolation {
            line: last_line,
            col: source.lines().last().map(str::len).unwrap_or(0) + 1,
            rule: "missing-final-newline",
            message: "file must end with a single newline".to_string(),
            before: None,
            after: None,
        });
    }

    let lines = normalize_source_lines_tracked(source, &mut violations);
    let normalized = lines.join("\n");
    let ast_info = parse_ast_info_checked(&normalized)?;

    // 3) Split into top-level blocks and co-locate verify blocks under their functions.
    let blocks = split_top_level_blocks(&lines, Some(&ast_info));
    let reordered = reorder_verify_blocks_tracked(blocks, &mut violations);

    // 4) Rejoin with one blank line between top-level blocks.
    let mut non_empty_blocks = Vec::new();
    for block in reordered {
        let text =
            normalize_internal_blank_runs_tracked(&block.text, block.start_line, &mut violations);
        let text = text.trim_matches('\n').to_string();
        if !text.is_empty() {
            non_empty_blocks.push(text);
        }
    }

    if non_empty_blocks.is_empty() {
        return Ok(("\n".to_string(), violations));
    }
    let mut out = non_empty_blocks.join("\n\n");
    out.push('\n');
    Ok((out, violations))
}

#[cfg(test)]
pub fn format_source(source: &str) -> String {
    match try_format_source(source) {
        Ok((formatted, _violations)) => formatted,
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

    #[test]
    fn sorts_module_effects_inline() {
        let src = "module M\n    intent = \"t\"\n    effects [Time.now, Console.print]\n";
        let got = format_source(src);
        assert_eq!(
            got,
            "module M\n    intent = \"t\"\n    effects [Console.print, Time.now]\n"
        );
    }

    #[test]
    fn keeps_short_module_effects_inline() {
        let src = "module M\n    intent = \"t\"\n    effects [Console.print]\n";
        let got = format_source(src);
        assert_eq!(got, src);
    }

    #[test]
    fn expands_long_module_effects_to_multiline() {
        let src = "module M\n    intent = \"t\"\n    effects [Time.now, Args.get, Console.warn, Console.print, Disk.readText, Disk.writeText, Random.int, Random.float]\n";
        let got = format_source(src);
        assert_eq!(
            got,
            "module M\n    intent = \"t\"\n    effects [\n        Args.get,\n        Console.print, Console.warn,\n        Disk.readText, Disk.writeText,\n        Random.float, Random.int,\n        Time.now,\n    ]\n"
        );
    }

    #[test]
    fn collapses_short_multiline_module_effects_back_to_inline() {
        let src = "module M\n    intent = \"t\"\n    effects [\n        Console.print,\n        Time.now,\n    ]\n";
        let got = format_source(src);
        assert_eq!(
            got,
            "module M\n    intent = \"t\"\n    effects [Console.print, Time.now]\n"
        );
    }
}
