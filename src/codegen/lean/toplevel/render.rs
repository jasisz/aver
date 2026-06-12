use super::expr::aver_name_to_lean;
use super::types::type_annotation_to_lean;
use crate::ast::*;

pub(super) fn emit_fn_param_names(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, _)| aver_name_to_lean(name))
        .collect::<Vec<_>>()
        .join(" ")
}

pub(super) fn indent_lines(block: &str, prefix: &str) -> Vec<String> {
    block
        .lines()
        .map(|line| format!("{prefix}{line}"))
        .collect()
}

/// Neutralize Lean block-comment delimiters inside doc text. A `/-` or `-/` in
/// the text would open/close a NESTED block comment inside the `/-- ... -/` doc
/// comment Lean wraps it in, leaving the comment unterminated and breaking the
/// whole file (`error: unterminated comment`). Splitting the 2-char token with a
/// space stops it tokenizing as a delimiter while reading identically in prose
/// (e.g. an Aver `?` doc mentioning `+2/-2` renders `+2/ -2`).
pub(crate) fn sanitize_doc(text: &str) -> String {
    text.replace("/-", "/ -").replace("-/", "- /")
}

pub(super) fn emit_doc_comment(desc: &Option<String>) -> Vec<String> {
    desc.as_ref()
        .map(|text| vec![format!("/-- {} -/", sanitize_doc(text))])
        .unwrap_or_default()
}

pub(super) fn ret_type_or_unit(fd: &FnDef) -> String {
    if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    }
}

pub(super) fn emit_fn_params(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, type_ann)| {
            let lean_type = type_annotation_to_lean(type_ann);
            let lean_name = aver_name_to_lean(name);
            format!("({} : {})", lean_name, lean_type)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn doc_comment_escapes_block_comment_delimiters() {
        // An Aver `?` doc mentioning `/-` or `-/` (e.g. "+2/-2") must not open or
        // close a NESTED block comment inside the `/-- ... -/` wrapper — that
        // leaves the comment unterminated and breaks the whole Lean file.
        let out = emit_doc_comment(&Some("delta +2/-2 ends with -/ token".to_string()));
        assert_eq!(out.len(), 1);
        let line = &out[0];
        let inner = line
            .strip_prefix("/-- ")
            .and_then(|s| s.strip_suffix(" -/"))
            .expect("doc comment keeps the /-- ... -/ wrapper");
        assert!(
            !inner.contains("/-"),
            "inner doc still opens a nested block comment: {line}"
        );
        assert!(
            !inner.contains("-/"),
            "inner doc still closes a nested block comment: {line}"
        );
    }
}
