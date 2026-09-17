//! `aver effects` — the declared effect surface against the computed one.
//!
//! Three things the command does, all of them over the same computation:
//! report the difference, write the minimum into the source, and show a
//! reviewer which lists moved between a revision and the working tree without
//! any body moving with them.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process;

use colored::Colorize;

use aver::effect_surface::{ModuleSurface, ProgramSurface, SurfaceInput};

use super::commands::{load_report_program_with_cache, resolve_av_inputs};
use super::shared::resolve_module_root;

/// Rendered path of a module, relative to the module root where that is
/// shorter, so a report of the same program reads the same wherever the tree
/// happens to sit.
fn display_path(path: &str, module_root: &str) -> String {
    let root = std::fs::canonicalize(module_root).unwrap_or_else(|_| PathBuf::from(module_root));
    let full = std::fs::canonicalize(path).unwrap_or_else(|_| PathBuf::from(path));
    match full.strip_prefix(&root) {
        Ok(relative) if !relative.as_os_str().is_empty() => relative.to_string_lossy().to_string(),
        _ => path.to_string(),
    }
}

fn fail(message: String, json: bool, kind: &str) -> ! {
    if json {
        println!(
            "{}",
            serde_json::json!({
                "schemaVersion": 1,
                "kind": kind,
                "error": message,
            })
        );
    } else {
        eprintln!("{}", message.red());
    }
    process::exit(1);
}

/// A type error that is only the effect lists being out of date.
///
/// These are the three the command exists to close: the call-site violation
/// and its callback variant (`types::checker::flow`), and the module boundary
/// that is narrower than its functions
/// (`types::checker::check_module_effect_boundary`). Any other type error
/// means a name did not resolve, and an unresolved callee contributes no
/// effects, so the computed minimum would be too small to write into the
/// source.
fn is_stale_effect_list(message: &str) -> bool {
    (message.contains("which has effect '") && message.contains("does not declare it"))
        || (message.contains("passes callback '") && message.contains("does not declare it"))
        || (message.contains("declared `effects [")
            && message.contains("is not in the declared boundary"))
}

/// The program's effect surface, plus every type error that is not just a
/// stale effect list.
///
/// Every module of every input's program is read once and type-checked from
/// its own perspective, so call sites resolve the way that module's source
/// names them. A program whose lists have not caught up with a leaf does not
/// type-check, which is the state the command exists for, so type errors are
/// not fatal here: the signature map survives them.
fn load_surface(
    inputs: &[String],
    module_root: &str,
    json: bool,
) -> (ProgramSurface, Vec<(String, String)>) {
    let marked = aver::config::MarkedCapabilities::for_project_dir(Some(module_root));
    let mut cache = aver::source::ProgramLoadCache::default();
    let mut seen: BTreeMap<PathBuf, usize> = BTreeMap::new();
    let mut units: Vec<SurfaceInput> = Vec::new();
    let mut blocking: Vec<(String, String)> = Vec::new();

    for file in inputs {
        let program = match load_report_program_with_cache(file, module_root, &mut cache) {
            Ok(program) => program,
            Err(error) => fail(error, json, "effectSurfaceError"),
        };
        for module in program.report_units() {
            // A module read once already; this program only adds the name it
            // reaches that module by, which is how its own call sites spell it.
            if let Some(index) = seen.get(&aver::source::canonicalize_path(&module.path)) {
                let names = &mut units[*index].import_names;
                if !names.contains(&module.dep_name) {
                    names.push(module.dep_name.clone());
                }
                continue;
            }
            if let Some(fault) = &module.fault {
                fail(fault.to_string(), json, "effectSurfaceError");
            }
            let loaded = match program.loaded_dependencies_for(module) {
                Ok(loaded) => loaded,
                Err(error) => fail(error.to_string(), json, "effectSurfaceError"),
            };
            let mut transformed = module.items.clone();
            let user_program_len = transformed.len();
            let typecheck = aver::ir::pipeline::front_gate(
                &mut transformed,
                &aver::ir::TypecheckMode::WithLoaded(&loaded),
                user_program_len,
                &marked,
            );
            let path = module.path.to_string_lossy().to_string();
            for error in &typecheck.errors {
                if !is_stale_effect_list(&error.message) {
                    blocking.push((
                        format!("{}:{}", display_path(&path, module_root), error.line),
                        error.message.clone(),
                    ));
                }
            }
            seen.insert(aver::source::canonicalize_path(&module.path), units.len());
            units.push(SurfaceInput {
                path,
                import_names: vec![module.dep_name.clone()],
                items: module.items.clone(),
                fn_sigs: aver::effect_surface::compact(typecheck.fn_sigs),
            });
        }
    }

    (aver::effect_surface::compute(units), blocking)
}

fn plural(count: usize, one: &str, many: &str) -> String {
    if count == 1 {
        format!("{count} {one}")
    } else {
        format!("{count} {many}")
    }
}

fn join(entries: &[String]) -> String {
    entries.join(", ")
}

// ---------------------------------------------------------------------------
// The report
// ---------------------------------------------------------------------------

fn print_report(label: &str, surface: &ProgramSurface, module_root: &str) {
    println!("Effect surface: {}", label.cyan());
    println!();

    let mut functions_total = 0usize;
    let mut functions_differing = 0usize;
    let mut boundaries_differing = 0usize;

    for (index, module) in surface.modules.iter().enumerate() {
        let count = module.functions.len();
        let differing = module.differing_functions().count();
        functions_total += count;
        functions_differing += differing;
        if module.boundary.differs() {
            boundaries_differing += 1;
        }

        if index > 0 {
            println!();
        }
        println!(
            "{}  ({})",
            module.module.cyan(),
            display_path(&module.path, module_root)
        );

        if !module.differs() {
            println!(
                "  {}, every declared list is the computed minimum",
                plural(count, "function", "functions")
            );
            continue;
        }

        println!(
            "  {} of {} {} from the computed minimum; boundary {}",
            differing,
            plural(count, "function", "functions"),
            if differing == 1 { "differs" } else { "differ" },
            if module.boundary.differs() {
                "differs"
            } else {
                "matches"
            }
        );
        for function in module.differing_functions() {
            println!("  fn {}", function.name);
            if !function.missing.is_empty() {
                println!("    missing: {}", join(&function.missing));
            }
            if !function.unused.is_empty() {
                println!("    unused: {}", join(&function.unused));
            }
        }
        if module.boundary.differs() {
            println!("  effects [...]");
            if !module.boundary.missing.is_empty() {
                println!("    missing: {}", join(&module.boundary.missing));
            }
            if !module.boundary.unused.is_empty() {
                println!("    unused: {}", join(&module.boundary.unused));
            }
        }
    }

    println!();
    if functions_differing == 0 && boundaries_differing == 0 {
        println!(
            "{}, {}, every declared list is the computed minimum",
            plural(surface.modules.len(), "module", "modules"),
            plural(functions_total, "function", "functions")
        );
    } else {
        println!(
            "{}: {} of {} and {} of {} differ from the computed minimum",
            plural(surface.modules.len(), "module", "modules"),
            functions_differing,
            plural(functions_total, "function", "functions"),
            boundaries_differing,
            plural(
                surface.modules.len(),
                "module boundary",
                "module boundaries"
            )
        );
    }
    if surface.capped {
        eprintln!(
            "{}",
            "effect propagation did not settle; the report is the last round computed".yellow()
        );
    }
}

fn module_json(module: &ModuleSurface, module_root: &str) -> serde_json::Value {
    let functions: Vec<serde_json::Value> = module
        .differing_functions()
        .map(|function| {
            serde_json::json!({
                "function": function.name,
                "line": function.line,
                "declared": function.declared,
                "minimum": function.minimum,
                "resolved": function.resolved,
                "missing": function.missing,
                "unused": function.unused,
            })
        })
        .collect();
    serde_json::json!({
        "module": module.module,
        "importNames": module.import_names,
        "path": display_path(&module.path, module_root),
        "functionCount": module.functions.len(),
        "functionsDiffering": functions.len(),
        "functions": functions,
        "boundary": {
            "declared": module.boundary.declared,
            "minimum": module.boundary.minimum,
            "resolved": module.boundary.resolved,
            "missing": module.boundary.missing,
            "unused": module.boundary.unused,
        },
    })
}

fn print_report_json(label: &str, surface: &ProgramSurface, module_root: &str) {
    let value = serde_json::json!({
        "schemaVersion": 1,
        "kind": "effectSurface",
        "program": label,
        "settled": !surface.capped,
        "modules": surface
            .modules
            .iter()
            .map(|module| module_json(module, module_root))
            .collect::<Vec<_>>(),
    });
    println!(
        "{}",
        serde_json::to_string_pretty(&value).expect("effect surface JSON is serializable")
    );
}

// ---------------------------------------------------------------------------
// The rewrite
// ---------------------------------------------------------------------------

/// What a rewrite changed in one file.
struct WriteOutcome {
    functions: Vec<String>,
    boundary: bool,
}

impl WriteOutcome {
    fn touched(&self) -> bool {
        !self.functions.is_empty() || self.boundary
    }
}

/// A line that opens a new top-level item.
///
/// A `//` comment at column zero opens nothing: the formatter reads them the
/// same way in `is_top_level_start`, and a function whose body carries one is
/// still one function. Calling such a line top level would cut a function in
/// half, and both the rewriter and the reviewer's view walk from one of these
/// to the next.
fn is_top_level(line: &str) -> bool {
    !line.is_empty()
        && !line.starts_with(' ')
        && !line.starts_with('\t')
        && !line.trim_start().starts_with("//")
}

fn indent_of(line: &str) -> String {
    line.chars().take_while(|c| *c == ' ').collect()
}

/// One bracketed list as the source carries it.
struct Bracketed {
    /// The entries between the brackets, in source order.
    entries: Vec<String>,
    /// The `//` comments written on the list's own lines, in source order.
    /// The formatter leaves a line carrying one alone, so a rewrite puts them
    /// back instead of deleting what the author wrote.
    comments: Vec<String>,
    /// Index just past the closing `]`.
    end: usize,
}

/// One line of a bracketed list split into its list text and the comment
/// written after it.
///
/// An effect entry is a dotted name, so the first `//` on such a line always
/// starts a comment. Reading the line without this split makes a commented
/// `]` invisible, and then the list appears to run on until some later line
/// happens to end in a bracket.
fn split_comment(segment: &str) -> (&str, Option<&str>) {
    match segment.find("//") {
        Some(at) => (segment[..at].trim_end(), Some(segment[at..].trim_end())),
        None => (segment.trim_end(), None),
    }
}

/// The bracketed list `lines[start]` opens with `lead`. `None` when the list
/// never closes.
///
/// Effect declarations are written inline while they fit and across one line
/// per namespace group when they do not, and both the rewriter and the
/// reviewer's view have to read either shape, at either `! [` or `effects [`.
fn read_bracketed<S: AsRef<str>>(lines: &[S], start: usize, lead: &str) -> Option<Bracketed> {
    let mut inner = String::new();
    let mut comments: Vec<String> = Vec::new();
    let mut cursor = start;
    while cursor < lines.len() {
        let trimmed = lines[cursor].as_ref().trim();
        let segment = if cursor == start {
            trimmed.trim_start_matches(lead)
        } else {
            trimmed
        };
        cursor += 1;
        let (segment, comment) = split_comment(segment);
        if let Some(comment) = comment {
            comments.push(comment.to_string());
        }
        match segment.strip_suffix(']') {
            Some(before) => {
                inner.push(' ');
                inner.push_str(before.trim());
                return Some(Bracketed {
                    entries: split_entries(&inner),
                    comments,
                    end: cursor,
                });
            }
            None => {
                inner.push(' ');
                inner.push_str(segment);
            }
        }
    }
    None
}

/// The rendered list with the comments the old one carried put back on its
/// last line.
fn with_comments(mut rendered: Vec<String>, comments: &[String]) -> Vec<String> {
    if !comments.is_empty()
        && let Some(last) = rendered.last_mut()
    {
        last.push_str("  ");
        last.push_str(&comments.join("  "));
    }
    rendered
}

fn split_entries(inner: &str) -> Vec<String> {
    inner
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(ToString::to_string)
        .collect()
}

/// Index of the line that opens `name`'s `! [...]`, and the end of the region
/// the function occupies.
fn function_region(lines: &[String], name: &str) -> Option<(usize, usize)> {
    let header = lines.iter().position(|line| {
        is_top_level(line)
            && line
                .strip_prefix("fn ")
                .and_then(|rest| rest.split('(').next())
                .map(str::trim)
                == Some(name)
    })?;
    let end = lines
        .iter()
        .enumerate()
        .skip(header + 1)
        .find(|(_, line)| is_top_level(line))
        .map(|(index, _)| index)
        .unwrap_or(lines.len());
    Some((header, end))
}

/// The module header's `effects [` line, which is inside the `module` block
/// and nowhere else. Searching the whole file would find a line of a body
/// that happens to start the same way.
fn module_header_effects_line<S: AsRef<str>>(lines: &[S]) -> Option<usize> {
    let start = lines
        .iter()
        .position(|line| line.as_ref().starts_with("module "))?;
    let end = lines
        .iter()
        .enumerate()
        .skip(start + 1)
        .find(|(_, line)| is_top_level(line.as_ref()))
        .map(|(index, _)| index)
        .unwrap_or(lines.len());
    (start + 1..end).find(|index| lines[*index].as_ref().trim().starts_with("effects ["))
}

/// Where a function that declares no effects would have its list written: after
/// the `?` description, before the first line of the body.
fn insert_point(lines: &[String], header: usize, end: usize) -> usize {
    let mut index = header + 1;
    let mut in_description = false;
    while index < end {
        let trimmed = lines[index].trim();
        if trimmed.is_empty() {
            index += 1;
            continue;
        }
        if trimmed.starts_with('?') {
            in_description = true;
            index += 1;
            continue;
        }
        // A comment is not the body's first line, and stopping on one at column
        // zero would write the list at column zero with it.
        if trimmed.starts_with("//") {
            index += 1;
            continue;
        }
        // Continuation lines of a `?` description are bare strings. A bare
        // string that is not continuing one is the body's first expression.
        if in_description && trimmed.starts_with('"') {
            index += 1;
            continue;
        }
        break;
    }
    index
}

fn rewrite_source(source: &str, module: &ModuleSurface) -> Option<(String, WriteOutcome)> {
    let ends_with_newline = source.ends_with('\n');
    let mut lines: Vec<String> = source.lines().map(str::to_string).collect();
    let mut outcome = WriteOutcome {
        functions: Vec::new(),
        boundary: false,
    };

    // Functions last-first, so rewriting one does not move the line numbers of
    // the ones still to come. The filter is the one the report prints from: a
    // list that is already the minimum is left as its author spelled it, so
    // running the report to decide whether to run `--write` is honest.
    let mut functions: Vec<_> = module
        .functions
        .iter()
        .filter(|function| function.differs())
        .collect();
    functions.sort_by_key(|function| std::cmp::Reverse(function.line));

    for function in functions {
        let Some((header, end)) = function_region(&lines, &function.name) else {
            continue;
        };
        let opening = (header + 1..end).find(|index| lines[*index].trim().starts_with("! ["));
        let rendered = match opening {
            Some(open) => {
                let Some(list) = read_bracketed(&lines, open, "! [") else {
                    continue;
                };
                let indent = indent_of(&lines[open]);
                let rendered = with_comments(
                    crate::format::format_bracketed_effect_list(&indent, "! ", &function.resolved),
                    &list.comments,
                );
                if rendered == lines[open..list.end] {
                    continue;
                }
                lines.splice(open..list.end, rendered);
                true
            }
            None => {
                if function.resolved.is_empty() {
                    false
                } else {
                    let at = insert_point(&lines, header, end);
                    let indent = if at < lines.len() && !lines[at].trim().is_empty() {
                        indent_of(&lines[at])
                    } else {
                        "    ".to_string()
                    };
                    let rendered = crate::format::format_bracketed_effect_list(
                        &indent,
                        "! ",
                        &function.resolved,
                    );
                    lines.splice(at..at, rendered);
                    true
                }
            }
        };
        if rendered {
            outcome.functions.push(function.name.clone());
        }
    }
    outcome.functions.reverse();

    if module.boundary.differs()
        && let Some(open) = module_header_effects_line(&lines)
        && let Some(list) = read_bracketed(&lines, open, "effects [")
    {
        let indent = indent_of(&lines[open]);
        let rendered = with_comments(
            crate::format::format_bracketed_effect_list(
                &indent,
                "effects ",
                &module.boundary.resolved,
            ),
            &list.comments,
        );
        if rendered != lines[open..list.end] {
            lines.splice(open..list.end, rendered);
            outcome.boundary = true;
        }
    }

    if !outcome.touched() {
        return None;
    }
    let mut rewritten = lines.join("\n");
    if ends_with_newline {
        rewritten.push('\n');
    }
    Some((rewritten, outcome))
}

fn cmd_write(
    label: &str,
    surface: &ProgramSurface,
    module_root: &str,
    inputs: &[String],
    json: bool,
) {
    let walked: std::collections::BTreeSet<PathBuf> = inputs
        .iter()
        .map(|input| aver::source::canonicalize_path(Path::new(input)))
        .collect();
    let mut written: Vec<(String, WriteOutcome, bool)> = Vec::new();
    for module in &surface.modules {
        let source = match std::fs::read_to_string(&module.path) {
            Ok(source) => source,
            Err(error) => fail(
                format!("Cannot read '{}': {}", module.path, error),
                json,
                "effectSurfaceWriteError",
            ),
        };
        let Some((rewritten, outcome)) = rewrite_source(&source, module) else {
            continue;
        };
        if let Err(error) = std::fs::write(&module.path, rewritten) {
            fail(
                format!("Cannot write '{}': {}", module.path, error),
                json,
                "effectSurfaceWriteError",
            );
        }
        let dependency =
            !walked.contains(&aver::source::canonicalize_path(Path::new(&module.path)));
        written.push((display_path(&module.path, module_root), outcome, dependency));
    }

    if json {
        let value = serde_json::json!({
            "schemaVersion": 1,
            "kind": "effectSurfaceWrite",
            "program": label,
            "files": written
                .iter()
                .map(|(path, outcome, dependency)| serde_json::json!({
                    "path": path,
                    "functions": outcome.functions,
                    "boundary": outcome.boundary,
                    "reachedAsDependency": dependency,
                }))
                .collect::<Vec<_>>(),
        });
        println!(
            "{}",
            serde_json::to_string_pretty(&value).expect("effect write JSON is serializable")
        );
        return;
    }

    if written.is_empty() {
        println!(
            "Effect declarations already minimal: {} ({})",
            label,
            plural(surface.modules.len(), "module", "modules")
        );
        return;
    }

    println!("Rewrote effect declarations: {}", label.cyan());
    let mut functions = 0usize;
    let mut boundaries = 0usize;
    for (path, outcome, _) in &written {
        functions += outcome.functions.len();
        if outcome.boundary {
            boundaries += 1;
        }
        let mut parts = Vec::new();
        if !outcome.functions.is_empty() {
            parts.push(plural(outcome.functions.len(), "function", "functions"));
        }
        if outcome.boundary {
            parts.push("module boundary".to_string());
        }
        println!("  {}  {}", path, parts.join(", "));
    }
    println!();
    println!(
        "{}, {}, {}",
        plural(written.len(), "file", "files"),
        plural(functions, "function", "functions"),
        plural(boundaries, "module boundary", "module boundaries")
    );

    // A module the input did not name is one this walk arrived at through an
    // import. Its lists are the tree's, not this program's, and another
    // program that imports it was not read here.
    let dependencies: Vec<&str> = written
        .iter()
        .filter(|(_, _, dependency)| *dependency)
        .map(|(path, _, _)| path.as_str())
        .collect();
    if !dependencies.is_empty() {
        println!();
        println!(
            "Reached as a dependency, so a program outside this walk changes with it: {}",
            join(
                &dependencies
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
            )
        );
        println!("Run `aver check` over the module root to cover those programs.");
    }
}

// ---------------------------------------------------------------------------
// The reviewer's view
// ---------------------------------------------------------------------------

/// One function as the source text carries it: its declared entries, and
/// everything else it is made of.
struct SourceFn {
    effects: Vec<String>,
    rest: String,
}

/// Every top-level function of one revision of a file, with its effect
/// declaration held apart from the rest of its text.
///
/// Holding them apart is the whole point: two revisions of a function whose
/// `rest` is byte-identical and whose `effects` are not changed only because
/// something further down its call chain changed.
fn scan_functions(source: &str) -> BTreeMap<String, SourceFn> {
    let lines: Vec<&str> = source.lines().collect();
    let mut out = BTreeMap::new();
    let mut index = 0usize;
    while index < lines.len() {
        let line = lines[index];
        let Some(name) = line
            .strip_prefix("fn ")
            .filter(|_| is_top_level(line))
            .and_then(|rest| rest.split('(').next())
            .map(|name| name.trim().to_string())
        else {
            index += 1;
            continue;
        };
        let mut effects = None;
        let mut rest = format!("{line}\n");
        let mut cursor = index + 1;
        while cursor < lines.len() && !is_top_level(lines[cursor]) {
            if effects.is_none()
                && lines[cursor].trim().starts_with("! [")
                && let Some(list) = read_bracketed(&lines, cursor, "! [")
            {
                // A comment beside the list is the author's, not propagation's,
                // so it belongs to the text the two revisions are compared by.
                for comment in &list.comments {
                    rest.push_str(comment);
                    rest.push('\n');
                }
                effects = Some(list.entries);
                cursor = list.end;
                continue;
            }
            rest.push_str(lines[cursor]);
            rest.push('\n');
            cursor += 1;
        }
        out.insert(
            name,
            SourceFn {
                effects: effects.unwrap_or_default(),
                rest,
            },
        );
        index = cursor;
    }
    out
}

fn git_output(root: &Path, args: &[&str]) -> Result<Vec<u8>, String> {
    let output = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .map_err(|error| {
            format!("`aver effects --since` shells out to git, which did not run: {error}")
        })?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
    }
    Ok(output.stdout)
}

fn git_root(from: &Path) -> Result<PathBuf, String> {
    let start = if from.is_dir() {
        from.to_path_buf()
    } else {
        from.parent().unwrap_or(Path::new(".")).to_path_buf()
    };
    let out = git_output(&start, &["rev-parse", "--show-toplevel"])?;
    let text = String::from_utf8_lossy(&out).trim().to_string();
    if text.is_empty() {
        return Err(format!(
            "'{}' is not inside a git work tree",
            start.display()
        ));
    }
    Ok(PathBuf::from(text))
}

/// How one module's lists moved between the revision and the working tree.
struct SinceModule {
    module: String,
    path: String,
    propagation_only: Vec<String>,
    body_changed: Vec<String>,
    added: Vec<String>,
    removed: Vec<String>,
    boundary_changed: bool,
    absent_at_revision: bool,
}

fn compare_module(
    module: &ModuleSurface,
    old_source: Option<&str>,
    module_root: &str,
) -> SinceModule {
    let path = display_path(&module.path, module_root);
    let Some(old_source) = old_source else {
        return SinceModule {
            module: module.module.clone(),
            path,
            propagation_only: Vec::new(),
            body_changed: Vec::new(),
            added: Vec::new(),
            removed: Vec::new(),
            boundary_changed: false,
            absent_at_revision: true,
        };
    };
    let new_source = std::fs::read_to_string(&module.path).unwrap_or_default();
    let old = scan_functions(old_source);
    let new = scan_functions(&new_source);

    let mut propagation_only = Vec::new();
    let mut body_changed = Vec::new();
    let mut added = Vec::new();
    for (name, current) in &new {
        match old.get(name) {
            None => added.push(name.clone()),
            Some(previous) => {
                if previous.effects == current.effects {
                    continue;
                }
                if previous.rest == current.rest {
                    propagation_only.push(name.clone());
                } else {
                    body_changed.push(name.clone());
                }
            }
        }
    }
    let removed: Vec<String> = old
        .keys()
        .filter(|name| !new.contains_key(*name))
        .cloned()
        .collect();

    let boundary_changed = module_boundary(old_source) != module_boundary(&new_source);

    SinceModule {
        module: module.module.clone(),
        path,
        propagation_only,
        body_changed,
        added,
        removed,
        boundary_changed,
        absent_at_revision: false,
    }
}

fn module_boundary(source: &str) -> Vec<String> {
    let lines: Vec<&str> = source.lines().collect();
    let mut entries = module_header_effects_line(&lines)
        .and_then(|open| read_bracketed(&lines, open, "effects ["))
        .map(|list| list.entries)
        .unwrap_or_default();
    entries.sort();
    entries
}

fn cmd_since(label: &str, surface: &ProgramSurface, module_root: &str, rev: &str, json: bool) {
    let root = match git_root(Path::new(module_root)) {
        Ok(root) => root,
        Err(error) => fail(error, json, "effectSurfaceSinceError"),
    };

    let mut rows = Vec::new();
    for module in &surface.modules {
        let absolute =
            std::fs::canonicalize(&module.path).unwrap_or_else(|_| PathBuf::from(&module.path));
        let relative = absolute
            .strip_prefix(&root)
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| module.path.clone());
        let spec = format!("{rev}:{relative}");
        let old = match git_output(&root, &["show", &spec]) {
            Ok(bytes) => Some(String::from_utf8_lossy(&bytes).to_string()),
            Err(error) => {
                // A revision nobody has is a mistake worth stopping for; a file
                // that revision simply did not carry is a new module.
                if git_output(
                    &root,
                    &["rev-parse", "--verify", &format!("{rev}^{{commit}}")],
                )
                .is_err()
                {
                    fail(
                        format!("Cannot read revision '{rev}': {error}"),
                        json,
                        "effectSurfaceSinceError",
                    );
                }
                None
            }
        };
        rows.push(compare_module(module, old.as_deref(), module_root));
    }

    if json {
        let value = serde_json::json!({
            "schemaVersion": 1,
            "kind": "effectSurfaceSince",
            "program": label,
            "revision": rev,
            "modules": rows
                .iter()
                .map(|row| serde_json::json!({
                    "module": row.module,
                    "path": row.path,
                    "absentAtRevision": row.absent_at_revision,
                    "propagationOnly": row.propagation_only,
                    "bodyChanged": row.body_changed,
                    "addedFunctions": row.added,
                    "removedFunctions": row.removed,
                    "boundaryChanged": row.boundary_changed,
                }))
                .collect::<Vec<_>>(),
        });
        println!(
            "{}",
            serde_json::to_string_pretty(&value).expect("effect since JSON is serializable")
        );
        return;
    }

    println!("Effect surface since {}: {}", rev.cyan(), label.cyan());
    println!();
    let mut propagation_total = 0usize;
    let mut body_total = 0usize;
    let mut added_total = 0usize;
    let mut removed_total = 0usize;
    for (index, row) in rows.iter().enumerate() {
        if index > 0 {
            println!();
        }
        println!("{}  ({})", row.module.cyan(), row.path);
        if row.absent_at_revision {
            println!("  not in {rev}");
            continue;
        }
        propagation_total += row.propagation_only.len();
        body_total += row.body_changed.len();
        added_total += row.added.len();
        removed_total += row.removed.len();
        if row.propagation_only.is_empty()
            && row.body_changed.is_empty()
            && row.added.is_empty()
            && row.removed.is_empty()
            && !row.boundary_changed
        {
            println!("  no effect list changed");
            continue;
        }
        if !row.propagation_only.is_empty() || !row.body_changed.is_empty() {
            println!(
                "  {} changed: {} propagation only, {} with a body change",
                plural(
                    row.propagation_only.len() + row.body_changed.len(),
                    "list",
                    "lists"
                ),
                row.propagation_only.len(),
                row.body_changed.len()
            );
        }
        if !row.propagation_only.is_empty() {
            println!("  propagation only: {}", join(&row.propagation_only));
        }
        if !row.body_changed.is_empty() {
            println!("  body changed: {}", join(&row.body_changed));
        }
        // A function that was renamed took its list with it and is in neither
        // group. Leaving it out of the text view is the one change a reviewer
        // most needs named.
        if !row.added.is_empty() {
            println!("  added: {}", join(&row.added));
        }
        if !row.removed.is_empty() {
            println!("  removed: {}", join(&row.removed));
        }
        if row.boundary_changed {
            println!("  module boundary changed");
        }
    }
    println!();
    println!(
        "{}, {} changed: {} propagation only, {} with a body change",
        plural(rows.len(), "module", "modules"),
        plural(propagation_total + body_total, "list", "lists"),
        propagation_total,
        body_total
    );
    if added_total > 0 || removed_total > 0 {
        println!(
            "{} added, {} removed",
            plural(added_total, "function", "functions"),
            plural(removed_total, "function", "functions")
        );
    }
}

// ---------------------------------------------------------------------------

pub(super) fn cmd_effects(
    file: &str,
    module_root_override: Option<&str>,
    json: bool,
    write: bool,
    since: Option<&str>,
) {
    if write && since.is_some() {
        fail(
            "`--write` rewrites the working tree and `--since` reads a revision; run them separately"
                .to_string(),
            json,
            "effectSurfaceError",
        );
    }

    let module_root = resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(file) {
        Ok(inputs) => inputs,
        Err(error) => fail(error, json, "effectSurfaceError"),
    };
    let (surface, blocking) = load_surface(&inputs, &module_root, json);
    let label = display_path(file, &module_root);

    if write && !blocking.is_empty() {
        let shown: Vec<String> = blocking
            .iter()
            .take(5)
            .map(|(where_, message)| format!("  {where_}: {message}"))
            .collect();
        fail(
            format!(
                "`--write` needs every name to resolve, and {} did not:\n{}\n\
                 Fix those with `aver check` first; effect violations alone do not block the rewrite.",
                plural(blocking.len(), "one error", "errors"),
                shown.join("\n")
            ),
            json,
            "effectSurfaceWriteError",
        );
    }

    match (write, since) {
        (true, _) => cmd_write(&label, &surface, &module_root, &inputs, json),
        (false, Some(rev)) => cmd_since(&label, &surface, &module_root, rev, json),
        (false, None) => {
            if json {
                print_report_json(&label, &surface, &module_root);
            } else {
                print_report(&label, &surface, &module_root);
            }
        }
    }
}
