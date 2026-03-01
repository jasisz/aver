use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind};

use crate::modules;

/// Namespace member info: (name, signature_label, detail).
struct Member {
    name: &'static str,
    detail: &'static str,
}

/// Return completion items for a given namespace prefix (e.g. "List", "Console").
pub fn namespace_completions(namespace: &str) -> Vec<CompletionItem> {
    let members: &[Member] = match namespace {
        "Int" => &[
            Member { name: "fromString", detail: "fn(String) -> Result<Int, String>" },
            Member { name: "fromFloat", detail: "fn(Float) -> Int" },
            Member { name: "toString", detail: "fn(Int) -> String" },
            Member { name: "abs", detail: "fn(Int) -> Int" },
            Member { name: "min", detail: "fn(Int, Int) -> Int" },
            Member { name: "max", detail: "fn(Int, Int) -> Int" },
            Member { name: "mod", detail: "fn(Int, Int) -> Result<Int, String>" },
            Member { name: "toFloat", detail: "fn(Int) -> Float" },
        ],
        "Float" => &[
            Member { name: "fromString", detail: "fn(String) -> Result<Float, String>" },
            Member { name: "fromInt", detail: "fn(Int) -> Float" },
            Member { name: "toString", detail: "fn(Float) -> String" },
            Member { name: "abs", detail: "fn(Float) -> Float" },
            Member { name: "floor", detail: "fn(Float) -> Int" },
            Member { name: "ceil", detail: "fn(Float) -> Int" },
            Member { name: "round", detail: "fn(Float) -> Int" },
            Member { name: "min", detail: "fn(Float, Float) -> Float" },
            Member { name: "max", detail: "fn(Float, Float) -> Float" },
        ],
        "String" => &[
            Member { name: "len", detail: "fn(String) -> Int" },
            Member { name: "byteLength", detail: "fn(String) -> Int" },
            Member { name: "startsWith", detail: "fn(String, String) -> Bool" },
            Member { name: "endsWith", detail: "fn(String, String) -> Bool" },
            Member { name: "contains", detail: "fn(String, String) -> Bool" },
            Member { name: "slice", detail: "fn(String, Int, Int) -> String" },
            Member { name: "trim", detail: "fn(String) -> String" },
            Member { name: "split", detail: "fn(String, String) -> List<String>" },
            Member { name: "replace", detail: "fn(String, String, String) -> String" },
            Member { name: "join", detail: "fn(List<String>, String) -> String" },
            Member { name: "charAt", detail: "fn(String, Int) -> Option<String>" },
            Member { name: "chars", detail: "fn(String) -> List<String>" },
            Member { name: "fromInt", detail: "fn(Int) -> String" },
            Member { name: "fromFloat", detail: "fn(Float) -> String" },
            Member { name: "fromBool", detail: "fn(Bool) -> String" },
            Member { name: "toLower", detail: "fn(String) -> String" },
            Member { name: "toUpper", detail: "fn(String) -> String" },
        ],
        "List" => &[
            Member { name: "len", detail: "fn(List<a>) -> Int" },
            Member { name: "get", detail: "fn(List<a>, Int) -> Option<a>" },
            Member { name: "push", detail: "fn(List<a>, a) -> List<a>" },
            Member { name: "head", detail: "fn(List<a>) -> Option<a>" },
            Member { name: "tail", detail: "fn(List<a>) -> Option<List<a>>" },
            Member { name: "map", detail: "fn(List<a>, Fn(a) -> b) -> List<b>" },
            Member { name: "filter", detail: "fn(List<a>, Fn(a) -> Bool) -> List<a>" },
            Member { name: "fold", detail: "fn(List<a>, b, Fn(b, a) -> b) -> b" },
            Member { name: "find", detail: "fn(List<a>, Fn(a) -> Bool) -> Option<a>" },
            Member { name: "any", detail: "fn(List<a>, Fn(a) -> Bool) -> Bool" },
            Member { name: "contains", detail: "fn(List<a>, a) -> Bool" },
        ],
        "Map" => &[
            Member { name: "empty", detail: "fn() -> Map<k, v>" },
            Member { name: "set", detail: "fn(Map<k, v>, k, v) -> Map<k, v>" },
            Member { name: "get", detail: "fn(Map<k, v>, k) -> Option<v>" },
            Member { name: "remove", detail: "fn(Map<k, v>, k) -> Map<k, v>" },
            Member { name: "has", detail: "fn(Map<k, v>, k) -> Bool" },
            Member { name: "keys", detail: "fn(Map<k, v>) -> List<k>" },
            Member { name: "values", detail: "fn(Map<k, v>) -> List<v>" },
            Member { name: "entries", detail: "fn(Map<k, v>) -> List<(k, v)>" },
            Member { name: "len", detail: "fn(Map<k, v>) -> Int" },
            Member { name: "fromList", detail: "fn(List<(k, v)>) -> Map<k, v>" },
        ],
        "Char" => &[
            Member { name: "toCode", detail: "fn(String) -> Int" },
            Member { name: "fromCode", detail: "fn(Int) -> Option<String>" },
        ],
        "Byte" => &[
            Member { name: "toHex", detail: "fn(Int) -> Result<String, String>" },
            Member { name: "fromHex", detail: "fn(String) -> Result<Int, String>" },
        ],
        "Result" => &[
            Member { name: "Ok", detail: "fn(a) -> Result<a, e>" },
            Member { name: "Err", detail: "fn(e) -> Result<a, e>" },
            Member { name: "withDefault", detail: "fn(Result<a, e>, a) -> a" },
        ],
        "Option" => &[
            Member { name: "Some", detail: "fn(a) -> Option<a>" },
            Member { name: "None", detail: "Option<a>" },
            Member { name: "withDefault", detail: "fn(Option<a>, a) -> a" },
            Member { name: "toResult", detail: "fn(Option<a>, e) -> Result<a, e>" },
        ],
        "Console" => &[
            Member { name: "print", detail: "fn(a) -> Unit ! [Console]" },
            Member { name: "error", detail: "fn(a) -> Unit ! [Console]" },
            Member { name: "warn", detail: "fn(a) -> Unit ! [Console]" },
            Member { name: "readLine", detail: "fn() -> Result<String, String> ! [Console]" },
        ],
        "Http" => &[
            Member { name: "get", detail: "fn(String) -> Result<HttpResponse, String> ! [Http]" },
            Member { name: "head", detail: "fn(String) -> Result<HttpResponse, String> ! [Http]" },
            Member { name: "delete", detail: "fn(String) -> Result<HttpResponse, String> ! [Http]" },
            Member { name: "post", detail: "fn(String, String, String, List<Header>) -> Result<HttpResponse, String> ! [Http]" },
            Member { name: "put", detail: "fn(String, String, String, List<Header>) -> Result<HttpResponse, String> ! [Http]" },
            Member { name: "patch", detail: "fn(String, String, String, List<Header>) -> Result<HttpResponse, String> ! [Http]" },
        ],
        "HttpServer" => &[
            Member { name: "listen", detail: "fn(Int, Fn(HttpRequest) -> HttpResponse) -> Unit ! [HttpServer]" },
            Member { name: "listenWith", detail: "fn(Int, context, Fn(context, HttpRequest) -> HttpResponse) -> Unit ! [HttpServer]" },
        ],
        "Disk" => &[
            Member { name: "readText", detail: "fn(String) -> Result<String, String> ! [Disk]" },
            Member { name: "writeText", detail: "fn(String, String) -> Result<Unit, String> ! [Disk]" },
            Member { name: "appendText", detail: "fn(String, String) -> Result<Unit, String> ! [Disk]" },
            Member { name: "exists", detail: "fn(String) -> Bool ! [Disk]" },
            Member { name: "delete", detail: "fn(String) -> Result<Unit, String> ! [Disk]" },
            Member { name: "deleteDir", detail: "fn(String) -> Result<Unit, String> ! [Disk]" },
            Member { name: "listDir", detail: "fn(String) -> Result<List<String>, String> ! [Disk]" },
            Member { name: "makeDir", detail: "fn(String) -> Result<Unit, String> ! [Disk]" },
        ],
        "Tcp" => &[
            Member { name: "send", detail: "fn(String, Int, String) -> Result<String, String> ! [Tcp]" },
            Member { name: "ping", detail: "fn(String, Int) -> Result<Unit, String> ! [Tcp]" },
            Member { name: "connect", detail: "fn(String, Int) -> Result<Tcp.Connection, String> ! [Tcp]" },
            Member { name: "writeLine", detail: "fn(Tcp.Connection, String) -> Result<Unit, String> ! [Tcp]" },
            Member { name: "readLine", detail: "fn(Tcp.Connection) -> Result<String, String> ! [Tcp]" },
            Member { name: "close", detail: "fn(Tcp.Connection) -> Result<Unit, String> ! [Tcp]" },
        ],
        _ => return Vec::new(),
    };

    members
        .iter()
        .map(|m| CompletionItem {
            label: m.name.to_string(),
            kind: Some(if m.name.chars().next().is_some_and(|c| c.is_uppercase()) {
                CompletionItemKind::CONSTRUCTOR
            } else {
                CompletionItemKind::FUNCTION
            }),
            detail: Some(m.detail.to_string()),
            ..Default::default()
        })
        .collect()
}

/// All known namespace names for top-level completion.
pub fn all_namespaces() -> Vec<CompletionItem> {
    let namespaces = [
        ("Int", "Integer operations"),
        ("Float", "Floating-point operations"),
        ("String", "String operations"),
        ("List", "List operations"),
        ("Map", "Map operations"),
        ("Char", "Character operations"),
        ("Byte", "Byte operations"),
        ("Result", "Result type constructors"),
        ("Option", "Option type constructors"),
        ("Console", "Console I/O"),
        ("Http", "HTTP client"),
        ("HttpServer", "HTTP server"),
        ("Disk", "File system operations"),
        ("Tcp", "TCP networking"),
    ];

    namespaces
        .iter()
        .map(|(name, detail)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(detail.to_string()),
            ..Default::default()
        })
        .collect()
}

/// Parse source into TopLevel items (best-effort, ignores errors).
pub fn parse_items(source: &str) -> Vec<aver::ast::TopLevel> {
    let mut lexer = aver::lexer::Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(_) => return Vec::new(),
    };
    let mut parser = aver::parser::Parser::new(tokens);
    match parser.parse() {
        Ok(i) => i,
        Err(_) => Vec::new(),
    }
}

/// Build completion items from user-defined function names found in AST.
pub fn user_fn_completions(source: &str) -> Vec<CompletionItem> {
    let items = parse_items(source);

    let mut completions = Vec::new();
    for item in &items {
        if let aver::ast::TopLevel::FnDef(fd) = item {
            let params: Vec<String> = fd
                .params
                .iter()
                .map(|(name, type_ann)| {
                    if type_ann.is_empty() {
                        name.clone()
                    } else {
                        format!("{}: {}", name, type_ann)
                    }
                })
                .collect();
            let ret = if fd.return_type.is_empty() {
                "_"
            } else {
                &fd.return_type
            };
            let detail = format!("fn({}) -> {}", params.join(", "), ret);

            completions.push(CompletionItem {
                label: fd.name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail),
                documentation: fd.desc.as_ref().map(|d| {
                    tower_lsp_server::ls_types::Documentation::String(d.clone())
                }),
                ..Default::default()
            });
        }
    }
    completions
}

/// Build completion items for user-defined type members.
/// When typing `Shape.`, suggests `Circle`, `Rect`, `Point`.
/// When typing `User.`, suggests `update` (record update) and field names.
pub fn user_type_completions(source: &str, type_name: &str) -> Vec<CompletionItem> {
    let items = parse_items(source);
    let mut completions = Vec::new();

    for item in &items {
        if let aver::ast::TopLevel::TypeDef(td) = item {
            match td {
                aver::ast::TypeDef::Sum {
                    name, variants, ..
                } if name == type_name => {
                    for v in variants {
                        let detail = if v.fields.is_empty() {
                            name.clone()
                        } else {
                            format!("fn({}) -> {}", v.fields.join(", "), name)
                        };
                        completions.push(CompletionItem {
                            label: v.name.clone(),
                            kind: Some(if v.fields.is_empty() {
                                CompletionItemKind::ENUM_MEMBER
                            } else {
                                CompletionItemKind::CONSTRUCTOR
                            }),
                            detail: Some(detail),
                            ..Default::default()
                        });
                    }
                }
                aver::ast::TypeDef::Product {
                    name, fields, ..
                } if name == type_name => {
                    // Record update function
                    completions.push(CompletionItem {
                        label: "update".to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format!("fn({}, ...) -> {}", name, name)),
                        ..Default::default()
                    });
                    // Field names
                    for (field_name, field_type) in fields {
                        completions.push(CompletionItem {
                            label: field_name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(field_type.clone()),
                            ..Default::default()
                        });
                    }
                }
                _ => {}
            }
        }
    }
    completions
}

/// Build completion items for functions exported from a dependent module.
/// When typing `ModuleName.`, suggests the module's exported functions.
pub fn module_completions(source: &str, module_name: &str, base_dir: &str) -> Vec<CompletionItem> {
    let deps = modules::resolve_dependencies(source, base_dir);

    for dep in &deps {
        // Match by last segment of module name (e.g., "Math" matches depends "Utils.Math")
        let dep_short = dep.name.rsplit('.').next().unwrap_or(&dep.name);
        if dep_short != module_name && dep.name != module_name {
            continue;
        }

        let mut completions = Vec::new();
        for fd in modules::exported_fns(dep) {
            let params: Vec<String> = fd
                .params
                .iter()
                .map(|(name, type_ann)| {
                    if type_ann.is_empty() {
                        name.clone()
                    } else {
                        format!("{}: {}", name, type_ann)
                    }
                })
                .collect();
            let ret = if fd.return_type.is_empty() {
                "_"
            } else {
                &fd.return_type
            };
            let detail = format!("fn({}) -> {}", params.join(", "), ret);

            completions.push(CompletionItem {
                label: fd.name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail),
                documentation: fd.desc.as_ref().map(|d| {
                    tower_lsp_server::ls_types::Documentation::String(d.clone())
                }),
                ..Default::default()
            });
        }

        // Also offer exported types from the module
        for td in modules::exported_types(dep) {
            let (name, detail) = match td {
                aver::ast::TypeDef::Sum { name, variants, .. } => {
                    let vs: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
                    (name.clone(), format!("type {} = {}", name, vs.join(" | ")))
                }
                aver::ast::TypeDef::Product { name, fields, .. } => {
                    let fs: Vec<String> =
                        fields.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                    (name.clone(), format!("record {}({})", name, fs.join(", ")))
                }
            };
            completions.push(CompletionItem {
                label: name,
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(detail),
                ..Default::default()
            });
        }

        return completions;
    }

    Vec::new()
}

/// Build completion items for module names from `depends` list.
/// Offered at top level so user can type `ModuleName.` and trigger member completion.
pub fn depends_module_completions(source: &str, base_dir: &str) -> Vec<CompletionItem> {
    let deps = modules::resolve_dependencies(source, base_dir);

    deps.iter()
        .map(|dep| {
            let short_name = dep.name.rsplit('.').next().unwrap_or(&dep.name);
            CompletionItem {
                label: short_name.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(format!("module {}", dep.name)),
                ..Default::default()
            }
        })
        .collect()
}
