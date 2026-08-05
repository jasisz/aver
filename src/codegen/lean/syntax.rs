//! Lean identifier spelling and reserved-token compatibility.
//!
//! Lean distinguishes identifier-shaped syntax tokens from ordinary
//! identifiers. Emitting an Aver function named `at`, `using`, or `exists`
//! verbatim therefore produces a parse error. The test below asks the pinned
//! Lean executable for its own token table, so a toolchain upgrade fails loudly
//! when this snapshot needs to grow.

/// Identifier-shaped syntax tokens registered by `import Lean` in Lean 4.32,
/// plus global declarations whose bare names are ambiguous in generated code.
const LEAN_RESERVED: &[&str] = &[
    "Prop",
    "Sort",
    "StateRefT",
    "Type",
    "abbrev",
    "add_decl_doc",
    "assert_not_exists",
    "assert_not_imported",
    "at",
    "attribute",
    "aux_def",
    "axiom",
    "bif",
    "binder_predicate",
    "break",
    "builtin_cbv_simproc",
    "builtin_cbv_simproc_decl",
    "builtin_dsimproc",
    "builtin_dsimproc_decl",
    "builtin_grind_propagator",
    "builtin_initialize",
    "builtin_simproc",
    "builtin_simproc_decl",
    "by",
    "by_elab",
    "calc",
    "catch",
    "cbv_eval",
    "cbv_simproc",
    "cbv_simproc_decl",
    "class",
    "coinductive",
    "coinductive_fixpoint",
    "continue",
    "dbg_trace",
    "declare_bitwise_int_theorems",
    "declare_bitwise_uint_theorems",
    "declare_command_config_elab",
    "declare_command_config_elab_legacy",
    "declare_config_elab",
    "declare_config_elab_legacy",
    "declare_core_config_elab",
    "declare_eval_bin",
    "declare_eval_bin_bitwise",
    "declare_eval_bin_bool_pred",
    "declare_int_theorems",
    "declare_simp_like_tactic",
    "declare_sint_simprocs",
    "declare_syntax_cat",
    "declare_term_config_elab",
    "declare_uint_simprocs",
    "declare_uint_theorems",
    "decreasing_by",
    "def",
    "def_eval_config_item",
    "deprecated_module",
    "deprecated_syntax",
    "deriving",
    "do",
    "docs_to_verso",
    "dsimproc",
    "dsimproc_decl",
    "elab",
    "elab_rules",
    "elab_stx_quot",
    "else",
    "end",
    "eval_prec",
    "eval_prio",
    "example",
    "exists",
    "export",
    "extends",
    "finally",
    "for",
    "forall",
    "from",
    "fun",
    "generalizing",
    "grind_annotated",
    "grind_pattern",
    "grind_propagator",
    "have",
    "haveI",
    "hiding",
    "idbg",
    "if",
    "import",
    "in",
    "include",
    "include_str",
    "inductive",
    "inductive_fixpoint",
    "inferInstanceAs",
    "infix",
    "infixl",
    "infixr",
    "init_grind_norm",
    "init_quot",
    "initialize",
    "instance",
    "leading_parser",
    "let",
    "letI",
    "let_delayed",
    "let_expr",
    "let_fun",
    "let_tmp",
    "local",
    "logNamedError",
    "logNamedErrorAt",
    "logNamedWarning",
    "logNamedWarningAt",
    "macro",
    "macro_rules",
    "match",
    "match_expr",
    "matches",
    "max_prec",
    "meta",
    "mod_cast",
    "mut",
    "mutual",
    "namespace",
    "nat_lit",
    "no_index",
    "nofun",
    "nomatch",
    "noncomputable",
    "nonrec",
    "norm_cast_add_elim",
    "notation",
    "omit",
    "opaque",
    "open",
    "partial",
    "partial_fixpoint",
    "postfix",
    "prefix",
    "private",
    "protected",
    "public",
    "recommended_spelling",
    "register_builtin_option",
    "register_error_explanation",
    "register_grind_attr",
    "register_label_attr",
    "register_linter_set",
    "register_option",
    "register_parser_alias",
    "register_simp_attr",
    "register_sym_dsimp",
    "register_sym_simp",
    "register_sym_simp_attr",
    "register_tactic_tag",
    "renaming",
    "repeat",
    "reprove",
    "return",
    "run_cmd",
    "run_elab",
    "run_meta",
    "scoped",
    "seal",
    "section",
    "set_library_suggestions",
    "set_option",
    "show",
    "show_panel_widgets",
    "show_term",
    "show_term_elab",
    "simproc",
    "simproc_decl",
    "sorry",
    "structure",
    "suffices",
    "syntax",
    "tactic_alt",
    "tactic_extension",
    "tactic_name",
    "tactic_tag",
    "termination_by",
    "test_extern",
    "then",
    "theorem",
    "throwError",
    "throwErrorAt",
    "throwNamedError",
    "throwNamedErrorAt",
    "trailing_parser",
    "try",
    "unif_hint",
    "universe",
    "unless",
    "unlock_limits",
    "unsafe",
    "unseal",
    "until",
    "using",
    "variable",
    "where",
    "while",
    "with",
    "with_annotate_term",
    "with_weak_namespace",
    "without_expected_type",
    // Global Lean declarations that make a bare user function ambiguous or
    // break simp/unfold references even though they are not syntax tokens.
    "and",
    "id",
    "insert",
    "max",
    "min",
    "not",
    "or",
    "priority",
    "toString",
    "xor",
];

pub(crate) fn aver_name_to_lean(name: &str) -> String {
    crate::codegen::common::escape_reserved_word(name, LEAN_RESERVED, "'")
}

/// Strip the trailing keyword guard for the `--explain` un-translator.
pub(crate) fn lean_name_to_aver(name: &str) -> String {
    if let Some(base) = name.strip_suffix('\'')
        && LEAN_RESERVED.contains(&base)
    {
        return base.to_string();
    }
    name.to_string()
}

#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::process::Command;

    use super::aver_name_to_lean;

    #[test]
    fn escapes_reported_keyword_regressions_and_global_collisions() {
        for name in [
            "at",
            "using",
            "exists",
            "sorry",
            "suffices",
            "variable",
            "universe",
            "notation",
            "attribute",
            "repeat",
            "termination_by",
            "max",
            "and",
        ] {
            assert_eq!(aver_name_to_lean(name), format!("{name}'"));
        }
        assert_eq!(aver_name_to_lean("value"), "value");
    }

    #[test]
    fn reserved_snapshot_covers_pinned_lean_token_table() {
        let mut probe = tempfile::Builder::new()
            .prefix("aver-lean-reserved-")
            .suffix(".lean")
            .tempfile()
            .expect("create Lean token probe");
        probe
            .write_all(
                br##"import Lean

open Lean Parser Elab Command

elab "#aver_reserved_tokens" : command => do
  let env <- getEnv
  let tokens := (getTokenTable env).values.qsort (fun a b => a < b)
  for token in tokens do
    if token != "_" && token.all (fun c => c.isAlphanum || c == '_') then
      logInfo m!"AVER_RESERVED_TOKEN:{token}"

#aver_reserved_tokens
"##,
            )
            .expect("write Lean token probe");

        let output = match Command::new("lean").arg(probe.path()).output() {
            Ok(output) => output,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return,
            Err(error) => panic!("run Lean token probe: {error}"),
        };
        assert!(
            output.status.success(),
            "Lean token probe failed:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );

        let transcript = format!(
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let tokens: Vec<&str> = transcript
            .lines()
            .filter_map(|line| {
                line.split_once("AVER_RESERVED_TOKEN:")
                    .map(|(_, token)| token)
            })
            .collect();
        assert!(
            !tokens.is_empty(),
            "Lean token probe returned no identifiers"
        );

        let missing: Vec<&str> = tokens
            .into_iter()
            .filter(|token| aver_name_to_lean(token) == *token)
            .collect();
        assert!(
            missing.is_empty(),
            "Lean added identifier-shaped reserved tokens; add them to LEAN_RESERVED: {missing:?}"
        );
    }
}
