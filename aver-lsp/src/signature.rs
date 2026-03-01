use tower_lsp_server::ls_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureInformation,
};

use crate::completion;
use crate::modules;

/// Provide signature help when the cursor is inside a function call.
pub fn signature_help(
    source: &str,
    line: usize,
    character: usize,
    base_dir: Option<&str>,
) -> Option<SignatureHelp> {
    let line_text = source.lines().nth(line)?;
    let before_cursor = if character <= line_text.len() {
        &line_text[..character]
    } else {
        line_text
    };

    // Walk backwards to find the function name and open paren
    let (fn_name, active_param) = find_call_context(before_cursor)?;

    // Try built-in namespace members first (e.g., "List.map")
    if let Some(dot_pos) = fn_name.find('.') {
        let namespace = &fn_name[..dot_pos];
        let member = &fn_name[dot_pos + 1..];
        let completions = completion::namespace_completions(namespace);
        if let Some(item) = completions.iter().find(|c| c.label == member) {
            let detail = item.detail.as_deref().unwrap_or("");
            return build_signature_help(&fn_name, detail, active_param);
        }
    }

    // Try user-defined functions
    let items = completion::parse_items(source);
    for item in &items {
        if let aver::ast::TopLevel::FnDef(fd) = item {
            if fd.name == fn_name {
                return build_sig_from_fndef(&fn_name, fd, active_param);
            }
        }
    }

    // Try cross-module functions (e.g., "Examples.Redis.set")
    if let (Some(last_dot), Some(base)) = (fn_name.rfind('.'), base_dir) {
        let module_name = &fn_name[..last_dot];
        let member = &fn_name[last_dot + 1..];
        let deps = modules::resolve_dependencies(source, base);
        for dep in &deps {
            let dep_short = dep.name.rsplit('.').next().unwrap_or(&dep.name);
            if dep.name == module_name || dep_short == module_name {
                for fd in modules::exported_fns(dep) {
                    if fd.name == member {
                        return build_sig_from_fndef(&fn_name, fd, active_param);
                    }
                }
            }
        }
    }

    None
}

/// Build signature help from an FnDef.
fn build_sig_from_fndef(
    display_name: &str,
    fd: &aver::ast::FnDef,
    active_param: u32,
) -> Option<SignatureHelp> {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(name, ty)| {
            if ty.is_empty() {
                name.clone()
            } else {
                format!("{}: {}", name, ty)
            }
        })
        .collect();
    let ret = if fd.return_type.is_empty() {
        "_"
    } else {
        &fd.return_type
    };
    let detail = format!("fn({}) -> {}", params.join(", "), ret);
    build_signature_help(display_name, &detail, active_param)
}

/// Extract function name and active parameter index from text before cursor.
fn find_call_context(before_cursor: &str) -> Option<(String, u32)> {
    let bytes = before_cursor.as_bytes();
    let mut depth = 0i32;
    let mut comma_count = 0u32;
    let mut paren_pos = None;

    // Scan backwards to find the matching open paren
    for i in (0..bytes.len()).rev() {
        match bytes[i] {
            b')' => depth += 1,
            b'(' => {
                if depth == 0 {
                    paren_pos = Some(i);
                    break;
                }
                depth -= 1;
            }
            b',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    let paren_pos = paren_pos?;

    // Extract the function name before the paren
    let before_paren = &before_cursor[..paren_pos];
    let fn_name = before_paren
        .trim_end()
        .rsplit(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .next()?;

    if fn_name.is_empty() {
        return None;
    }

    Some((fn_name.to_string(), comma_count))
}

/// Build SignatureHelp from a function name and its detail string like "fn(a: Int, b: Int) -> Int".
fn build_signature_help(fn_name: &str, detail: &str, active_param: u32) -> Option<SignatureHelp> {
    // Parse parameters from detail string: "fn(param1, param2) -> RetType"
    let params_str = detail.strip_prefix("fn(")?.split(") ->").next()?;

    let params: Vec<&str> = if params_str.is_empty() {
        vec![]
    } else {
        split_params(params_str)
    };

    let parameters: Vec<ParameterInformation> = params
        .iter()
        .map(|p| ParameterInformation {
            label: ParameterLabel::Simple(p.trim().to_string()),
            documentation: None,
        })
        .collect();

    let label = format!("{}({})", fn_name, params.join(", "));

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter: Some(active_param),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}

/// Split parameter string respecting nested generics like "List<a>" or "Fn(a) -> b".
fn split_params(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    let bytes = s.as_bytes();

    for i in 0..bytes.len() {
        match bytes[i] {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                result.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = s[start..].trim();
    if !last.is_empty() {
        result.push(last);
    }
    result
}
