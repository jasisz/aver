//! Per-theorem error isolation for the `aver proof` Lean export.
//!
//! A law's proof is a `first | … | sorry` portfolio, so a strategy that does not
//! close normally lands on the caught `sorry`. Some failures escape that floor:
//! an arm that leaves goals behind (`unsolved goals` at the end of the theorem),
//! a tactic before the portfolio that fails (`rewrite` found no match), and
//! every runtime limit (a heartbeat timeout or the recursion depth), which Lean
//! never lets `first` catch. Each of those used to fail the whole `lake build`,
//! so one bad law hid every other law of the program from the audit.
//!
//! This pass puts each proof theorem of a module file behind
//! [`ISOLATION_GUARD`]. `#guard_msgs` drops the errors the theorem reports and
//! passes every other message through (the `declaration uses 'sorry'` warning
//! of a caught floor included). Lean's error recovery usually still adds the
//! theorem, with a synthetic `sorryAx` in place of the failed proof, so
//! everything that cites it elaborates and the axiom audit charges it as not
//! proven. A runtime limit hit while the declaration itself is being finished
//! (a heartbeat timeout in `whnf` at the `theorem` line, say) adds nothing at
//! all: the theorem is simply absent from the built environment. The check
//! reads both back (see [`isolation_check_source`]): a synthetic `sorryAx`, and
//! a guarded theorem the environment does not have. Either way a theorem whose
//! proof errored is reported as a failed law, never as proven.
//!
//! The bounded evidence stays outside the guard: a `_sample_N` or
//! `_checked_domain` theorem that fails is a counterexample to the law (or a
//! model the program disagrees with), and must keep failing the build.

/// The line the export puts in front of each isolated theorem.
pub const ISOLATION_GUARD: &str =
    "#guard_msgs (drop error, pass warning, pass info, pass trace) in";

/// The first token of [`ISOLATION_GUARD`], for scanners that slice the
/// emitted text into declarations and must treat the guard as a boundary.
pub const ISOLATION_GUARD_PREFIX: &str = "#guard_msgs";

/// The marker each isolated declaration whose proof failed prints in the
/// output of [`isolation_check_source`].
pub const ISOLATION_ERROR_MARKER: &str = "AVER_ISOLATED_ERROR ";
/// The line [`isolation_check_source`] prints once it has looked at every
/// module, so a check that stopped early is never read as "no errors".
pub const ISOLATION_DONE_MARKER: &str = "AVER_ISOLATION_DONE";

/// Put every proof theorem of `text` behind [`ISOLATION_GUARD`].
///
/// A theorem is guarded when it is declared at column 0 outside a `mutual`
/// block and is not bounded evidence (`_sample_N`, `_checked_domain`,
/// `_checked_domain_partN`). The guard goes in front of the lines that belong
/// to the same command (`set_option … in`, attribute lines, a doc comment) and
/// in front of the comment lines directly above them, so the law-class marker
/// stays next to its theorem.
pub fn isolate_proof_theorems(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let mut guard_before = vec![false; lines.len()];
    let mut in_mutual = false;
    for (index, line) in lines.iter().enumerate() {
        if *line == "mutual" {
            in_mutual = true;
            continue;
        }
        if in_mutual {
            if *line == "end" {
                in_mutual = false;
            }
            continue;
        }
        let Some(name) = theorem_name(line) else {
            continue;
        };
        if is_bounded_evidence(name) {
            continue;
        }
        let mut start = index;
        let mut cursor = index;
        while cursor > 0 {
            let above = lines[cursor - 1];
            if above.starts_with(ISOLATION_GUARD_PREFIX) {
                // Already guarded (a re-run of the pass).
                start = index + 1;
                break;
            }
            if above.starts_with("set_option ") && above.trim_end().ends_with(" in")
                || above.starts_with("@[")
                || above.starts_with("--")
            {
                cursor -= 1;
                start = cursor;
                continue;
            }
            if above.trim_end().ends_with("-/") {
                // A doc comment (or plain block comment) directly above.
                let Some(open) = (0..cursor).rev().find(|&i| lines[i].starts_with("/-")) else {
                    break;
                };
                cursor = open;
                start = cursor;
                continue;
            }
            break;
        }
        if start <= index {
            guard_before[start] = true;
        }
    }
    let mut out = String::with_capacity(text.len() + 64);
    for (index, line) in lines.iter().enumerate() {
        if guard_before[index] {
            out.push_str(ISOLATION_GUARD);
            out.push('\n');
        }
        out.push_str(line);
        out.push('\n');
    }
    if !text.ends_with('\n') {
        out.pop();
    }
    out
}

/// The qualified names of the theorems [`isolate_proof_theorems`] put behind
/// [`ISOLATION_GUARD`] in `text`: the first theorem after each guard line,
/// qualified by the `namespace` blocks open at that point. The isolation check
/// reports each one the built environment does not contain.
pub fn guarded_theorem_names(text: &str) -> Vec<String> {
    // One entry per open `namespace`/`section`: the name components it adds.
    let mut scopes: Vec<Vec<String>> = Vec::new();
    let mut in_mutual = false;
    let mut pending = false;
    let mut names = Vec::new();
    for line in text.lines() {
        if line == "mutual" {
            in_mutual = true;
            continue;
        }
        if in_mutual {
            if line == "end" {
                in_mutual = false;
            }
            continue;
        }
        if let Some(rest) = line.strip_prefix("namespace ") {
            scopes.push(rest.trim().split('.').map(str::to_string).collect());
            continue;
        }
        if line == "section"
            || line.starts_with("section ")
            || line == "noncomputable section"
            || line.starts_with("noncomputable section ")
        {
            scopes.push(Vec::new());
            continue;
        }
        if line == "end" || line.starts_with("end ") {
            scopes.pop();
            continue;
        }
        if line.starts_with(ISOLATION_GUARD_PREFIX) {
            pending = true;
            continue;
        }
        if !pending {
            continue;
        }
        if let Some(name) = theorem_name(line) {
            let mut parts: Vec<&str> = scopes.iter().flatten().map(String::as_str).collect();
            parts.push(name);
            names.push(parts.join("."));
            pending = false;
        }
    }
    names
}

/// The theorem name a column-0 declaration line opens, after the modifiers and
/// an inline attribute list the emitter uses.
fn theorem_name(line: &str) -> Option<&str> {
    let mut rest = line;
    if let Some(after) = rest.strip_prefix("@[") {
        let close = after.find(']')?;
        rest = after[close + 1..].trim_start();
    }
    for modifier in ["private ", "protected "] {
        if let Some(after) = rest.strip_prefix(modifier) {
            rest = after;
        }
    }
    let rest = rest.strip_prefix("theorem ")?;
    let name = rest.split_whitespace().next()?.trim_end_matches(':');
    (!name.is_empty()).then_some(name)
}

fn is_bounded_evidence(name: &str) -> bool {
    fn numbered(name: &str, marker: &str) -> bool {
        name.rfind(marker).is_some_and(|at| {
            let tail = &name[at + marker.len()..];
            !tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit())
        })
    }
    name.ends_with("_checked_domain")
        || numbered(name, "_checked_domain_part")
        || numbered(name, "_sample_")
}

/// Lean source of the isolation check: import every module of the export and
/// print one [`ISOLATION_ERROR_MARKER`] line per declaration of those modules
/// whose value or type carries a synthetic `sorryAx` (the term Lean puts in
/// place of a proof that failed to elaborate; an explicit `sorry` is not
/// synthetic), and one per name of `guarded` (see [`guarded_theorem_names`])
/// that no module declares. Ends with [`ISOLATION_DONE_MARKER`].
pub fn isolation_check_source(roots: &[String], guarded: &[String]) -> String {
    let mut src = String::from("import Lean.Elab.Command\n");
    for root in roots {
        src.push_str("import ");
        src.push_str(root);
        src.push('\n');
    }
    src.push_str(
        "\nopen Lean Elab Command in\n\
         #eval show CommandElabM Unit from do\n  \
           let env ← getEnv\n  \
           let synthetic (e : Expr) : Bool := (e.find? fun s =>\n      \
             s.isAppOf ``sorryAx && s.getAppNumArgs ≥ 2 && s.getArg! 1 == mkConst ``Bool.true).isSome\n  \
           let roots : Array Name := #[",
    );
    let names: Vec<String> = roots
        .iter()
        .map(|root| format!("`{}", lean_name_literal(root)))
        .collect();
    src.push_str(&names.join(", "));
    src.push_str("]\n  let guarded : Array Name := #[");
    let guarded: Vec<String> = guarded
        .iter()
        .map(|name| format!("`{}", lean_name_literal(name)))
        .collect();
    src.push_str(&guarded.join(", "));
    src.push_str(
        "]\n  \
           let mut declared : NameSet := {}\n  \
           for root in roots do\n    \
             let some idx := env.getModuleIdx? root | continue\n    \
             for name in env.header.moduleData[idx.toNat]!.constNames do\n      \
               declared := declared.insert ((privateToUserName? name).getD name)\n      \
               let some info := env.find? name | continue\n      \
               let value := (info.value? (allowOpaque := true)).getD (mkConst ``True)\n      \
               if synthetic value || synthetic info.type then\n        \
                 IO.println s!\"",
    );
    src.push_str(ISOLATION_ERROR_MARKER);
    src.push_str(
        "{name}\"\n  \
           for name in guarded do\n    \
             unless declared.contains name do\n      \
               IO.println s!\"",
    );
    src.push_str(ISOLATION_ERROR_MARKER);
    src.push_str("{name}\"\n  IO.println \"");
    src.push_str(ISOLATION_DONE_MARKER);
    src.push_str("\"\n");
    src
}

/// A dotted module name as a Lean name literal, each component escaped with
/// `«»` when it is not a plain identifier.
fn lean_name_literal(dotted: &str) -> String {
    dotted
        .split('.')
        .map(|part| {
            let plain = part
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
                && part
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '\'');
            if plain {
                part.to_string()
            } else {
                format!("«{part}»")
            }
        })
        .collect::<Vec<_>>()
        .join(".")
}

/// Parse the output of [`isolation_check_source`]: the declarations whose
/// proof failed, or `None` when the check did not reach its end.
pub fn parse_isolation_check(output: &str) -> Option<Vec<String>> {
    let mut names = Vec::new();
    let mut done = false;
    for line in output.lines() {
        let line = line.trim();
        if let Some(name) = line.strip_prefix(ISOLATION_ERROR_MARKER) {
            names.push(name.trim().to_string());
        } else if line == ISOLATION_DONE_MARKER {
            done = true;
        }
    }
    names.sort();
    names.dedup();
    done.then_some(names)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guards_law_theorems_but_not_bounded_evidence() {
        let text = "namespace M\n\
                    def f (x : Int) : Int := x\n\
                    -- verify law f.id\n\
                    -- aver:law-class f_law_id universal M.f.id\n\
                    theorem f_law_id : ∀ (x : Int), f x = x := by\n  \
                      intro x\n  first\n  | rfl\n  | sorry\n\
                    set_option synthInstance.maxSize 4096 in\n\
                    theorem f_law_id_checked_domain : f 0 = 0 := by native_decide\n\
                    theorem f_law_id_sample_1 : f 0 = 0 := by native_decide\n\
                    set_option maxHeartbeats 400000 in\n\
                    /-- a helper -/\n\
                    theorem helper : True := by trivial\n\
                    mutual\n\
                    theorem inner : True := by trivial\n\
                    end\n\
                    end M\n";
        let out = isolate_proof_theorems(text);
        let expected = "namespace M\n\
                    def f (x : Int) : Int := x\n\
                    #guard_msgs (drop error, pass warning, pass info, pass trace) in\n\
                    -- verify law f.id\n\
                    -- aver:law-class f_law_id universal M.f.id\n\
                    theorem f_law_id : ∀ (x : Int), f x = x := by\n  \
                      intro x\n  first\n  | rfl\n  | sorry\n\
                    set_option synthInstance.maxSize 4096 in\n\
                    theorem f_law_id_checked_domain : f 0 = 0 := by native_decide\n\
                    theorem f_law_id_sample_1 : f 0 = 0 := by native_decide\n\
                    #guard_msgs (drop error, pass warning, pass info, pass trace) in\n\
                    set_option maxHeartbeats 400000 in\n\
                    /-- a helper -/\n\
                    theorem helper : True := by trivial\n\
                    mutual\n\
                    theorem inner : True := by trivial\n\
                    end\n\
                    end M\n";
        assert_eq!(out, expected);
        // Idempotent: a second pass adds nothing.
        assert_eq!(isolate_proof_theorems(&out), out);
    }

    #[test]
    fn parses_the_check_output_and_requires_its_end() {
        let out =
            "AVER_ISOLATED_ERROR M.f_law_id\nAVER_ISOLATED_ERROR M.f_law_id\nAVER_ISOLATION_DONE\n";
        assert_eq!(
            parse_isolation_check(out),
            Some(vec!["M.f_law_id".to_string()])
        );
        assert_eq!(parse_isolation_check("AVER_ISOLATED_ERROR M.x\n"), None);
    }

    #[test]
    fn check_source_imports_every_root() {
        let src = isolation_check_source(
            &["Laws".to_string(), "Domain.Chainwork".to_string()],
            &["Laws.f_law_id".to_string()],
        );
        assert!(
            src.starts_with("import Lean.Elab.Command\nimport Laws\nimport Domain.Chainwork\n")
        );
        assert!(src.contains("#[`Laws, `Domain.Chainwork]"));
        assert!(src.contains("let guarded : Array Name := #[`Laws.f_law_id]"));
    }

    #[test]
    fn names_the_guarded_theorems_by_their_namespace() {
        let text = "namespace M\n\
                    theorem f_law_id_sample_1 : True := by trivial\n\
                    #guard_msgs (drop error, pass warning, pass info, pass trace) in\n\
                    -- aver:law-class f_law_id universal M.f.id\n\
                    theorem f_law_id : True := by trivial\n\
                    mutual\n\
                    theorem inner : True := by trivial\n\
                    end\n\
                    namespace Inner.Deep\n\
                    #guard_msgs (drop error, pass warning, pass info, pass trace) in\n\
                    set_option maxHeartbeats 400000 in\n\
                    private theorem helper : True := by trivial\n\
                    end Inner.Deep\n\
                    #guard_msgs (drop error, pass warning, pass info, pass trace) in\n\
                    @[simp] theorem g_law_x : True := by trivial\n\
                    end M\n";
        assert_eq!(
            guarded_theorem_names(text),
            vec![
                "M.f_law_id".to_string(),
                "M.Inner.Deep.helper".to_string(),
                "M.g_law_x".to_string(),
            ]
        );
    }
}
