//! Single source of truth for built-in record types that appear in
//! Aver's effect signatures and proof exports.
//!
//! Lean and Dafny both need declarations for these types in their
//! preludes. Previously each backend kept its own hard-coded literal
//! (a Lean `structure` block and a Dafny `datatype` block) and any
//! drift between them was caught only by integration tests. This
//! module declares each built-in record once, in a backend-neutral
//! shape, and each backend renders it on demand.
//!
//! The conditional-emit helpers (`needs_in_lean`, `needs_in_dafny`)
//! work the same way: scan the generated body for a backend-mangled
//! identifier and only include the prelude entry when it's actually
//! referenced.

/// A field in a built-in record. Backend-neutral type tags so Lean
/// and Dafny renderers can emit their own native form.
pub struct BuiltinField {
    pub name: &'static str,
    pub ty: BuiltinType,
}

/// Element types we currently need for built-in records. Add variants
/// as more shapes show up.
#[allow(dead_code)] // Bool/Float reserved for future record shapes; fields used by rendering paths.
pub enum BuiltinType {
    Int,
    Str,
    Bool,
    Float,
    /// `List<T>` / `seq<T>` of another built-in record.
    ListOf(&'static str),
    /// `Map<String, List<String>>` — used by `HttpResponse.headers`
    /// and `HttpRequest.headers`. Multi-value semantics match HTTP
    /// (RFC 9110: same-name fields, plus RFC 6265 Set-Cookie). Keys
    /// are case-insensitive by convention; runtime normalizes
    /// incoming names to lowercase, user code is expected to do the
    /// same on outgoing (mirrors Go `net/http.Header`).
    MapStrListStr,
}

impl BuiltinType {
    /// Convert to the Aver type-system representation (used by the
    /// WASM and proof backends for type inference of field accesses).
    #[allow(dead_code)] // consumed by `wasm` feature only
    pub fn as_aver_type(&self) -> crate::types::Type {
        use crate::types::Type;
        match self {
            BuiltinType::Int => Type::Int,
            BuiltinType::Str => Type::Str,
            BuiltinType::Bool => Type::Bool,
            BuiltinType::Float => Type::Float,
            BuiltinType::ListOf(name) => Type::List(Box::new(Type::Named((*name).to_string()))),
            BuiltinType::MapStrListStr => Type::Map(
                Box::new(Type::Str),
                Box::new(Type::List(Box::new(Type::Str))),
            ),
        }
    }
}

/// Description of a built-in record type. `aver_name` is the dotted
/// or undotted name as it appears in Aver source (`Terminal.Size`,
/// `HttpResponse`). Backends derive their identifier by replacing
/// dots with underscores.
pub struct BuiltinRecord {
    pub aver_name: &'static str,
    pub fields: &'static [BuiltinField],
    /// Other built-in records this one references (so the dependency
    /// can be emitted before this one — e.g. `Header` before
    /// `HttpResponse`).
    pub depends_on: &'static [&'static str],
    /// Maintainer-facing description; not consumed by codegen, but
    /// kept as documentation alongside the table.
    #[allow(dead_code)]
    pub doc: &'static str,
}

impl BuiltinRecord {
    /// The identifier the backends use (dots → underscores).
    pub fn backend_name(&self) -> String {
        self.aver_name.replace('.', "_")
    }

    /// Position of a field in the declaration order. Currently
    /// unused (WASM iterates `fields` directly), but kept available
    /// for future per-key WASM/Dafny offset lookups.
    #[allow(dead_code)]
    pub fn field_index(&self, name: &str) -> Option<u32> {
        self.fields
            .iter()
            .position(|f| f.name == name)
            .map(|i| i as u32)
    }

    /// Field type lookup (returns the backend-neutral `BuiltinType`).
    #[allow(dead_code)] // consumed by `wasm` feature only
    pub fn field_type(&self, name: &str) -> Option<&'static BuiltinType> {
        for f in self.fields {
            if f.name == name {
                return Some(&f.ty);
            }
        }
        None
    }
}

/// All built-in record types in declaration order.
pub const BUILTIN_RECORDS: &[BuiltinRecord] = &[
    BuiltinRecord {
        aver_name: "Header",
        fields: &[
            BuiltinField {
                name: "name",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "value",
                ty: BuiltinType::Str,
            },
        ],
        depends_on: &[],
        doc: "HTTP header pair shared by `HttpResponse` / `HttpRequest`.",
    },
    BuiltinRecord {
        aver_name: "HttpResponse",
        fields: &[
            BuiltinField {
                name: "status",
                ty: BuiltinType::Int,
            },
            BuiltinField {
                name: "body",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "headers",
                ty: BuiltinType::MapStrListStr,
            },
        ],
        depends_on: &[],
        doc: "Returned by classified `Http.get`/`.head`/`.delete`/`.post`/`.put`/`.patch`.",
    },
    BuiltinRecord {
        aver_name: "HttpRequest",
        fields: &[
            BuiltinField {
                name: "method",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "path",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "body",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "headers",
                ty: BuiltinType::MapStrListStr,
            },
        ],
        depends_on: &[],
        doc: "Server-side request record (used by lifted server-handler proofs).",
    },
    BuiltinRecord {
        aver_name: "Tcp.Connection",
        fields: &[
            BuiltinField {
                name: "id",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "host",
                ty: BuiltinType::Str,
            },
            BuiltinField {
                name: "port",
                ty: BuiltinType::Int,
            },
        ],
        depends_on: &[],
        doc: "Returned by `Tcp.connect`; consumed by the rest of `Tcp.*`.",
    },
    BuiltinRecord {
        aver_name: "Terminal.Size",
        fields: &[
            BuiltinField {
                name: "width",
                ty: BuiltinType::Int,
            },
            BuiltinField {
                name: "height",
                ty: BuiltinType::Int,
            },
        ],
        depends_on: &[],
        doc: "Returned by classified `Terminal.size` (snapshot effect).",
    },
];

/// Look up a built-in record by its Aver-source name.
pub fn find(aver_name: &str) -> Option<&'static BuiltinRecord> {
    BUILTIN_RECORDS.iter().find(|r| r.aver_name == aver_name)
}

/// Render the field type as Lean syntax.
fn lean_type(ty: &BuiltinType) -> String {
    match ty {
        BuiltinType::Int => "Int".to_string(),
        BuiltinType::Str => "String".to_string(),
        BuiltinType::Bool => "Bool".to_string(),
        BuiltinType::Float => "Float".to_string(),
        BuiltinType::ListOf(name) => format!("List {}", name.replace('.', "_")),
        // Lean has no first-class Map; mirror `type_to_lean` and use a
        // list of pairs as the Map representation.
        BuiltinType::MapStrListStr => "List (String × List String)".to_string(),
    }
}

/// Render the field type as Dafny syntax.
fn dafny_type(ty: &BuiltinType) -> String {
    match ty {
        BuiltinType::Int => "int".to_string(),
        BuiltinType::Str => "string".to_string(),
        BuiltinType::Bool => "bool".to_string(),
        BuiltinType::Float => "real".to_string(),
        BuiltinType::ListOf(name) => format!("seq<{}>", name.replace('.', "_")),
        BuiltinType::MapStrListStr => "map<string, seq<string>>".to_string(),
    }
}

/// Sanitize a field name for Dafny (Dafny reserves `method`, etc.,
/// so user-facing names with that token need a trailing underscore).
fn dafny_field(name: &str) -> &str {
    match name {
        "method" => "method_",
        other => other,
    }
}

/// Lean `structure` block for a built-in record.
pub fn render_lean(record: &BuiltinRecord) -> String {
    let mut s = format!("structure {} where\n", record.backend_name());
    for f in record.fields {
        s.push_str(&format!("  {} : {}\n", f.name, lean_type(&f.ty)));
    }
    s.push_str("  deriving Repr, BEq, Inhabited, DecidableEq");
    s
}

/// Dafny `datatype` block for a built-in record.
pub fn render_dafny(record: &BuiltinRecord) -> String {
    let bn = record.backend_name();
    let fields: Vec<String> = record
        .fields
        .iter()
        .map(|f| format!("{}: {}", dafny_field(f.name), dafny_type(&f.ty)))
        .collect();
    format!("datatype {} = {}({})", bn, bn, fields.join(", "))
}

/// Cheap textual scan: does the rendered backend body reference the
/// backend-mangled record name in a way that requires the prelude
/// declaration?
pub fn needs_in_body(body: &str, backend_name: &str) -> bool {
    let mut token = String::new();
    for ch in body.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            token.push(ch);
            continue;
        }
        if token == backend_name {
            return true;
        }
        token.clear();
    }
    token == backend_name
}

/// Shared decision: does the generated body include any Oracle-lifted
/// effectful function or verify-trace export, and therefore need the
/// proof-trust-assumption header at the top of the file?
///
/// Heuristic: the trust header documents what we promise about
/// classified effects, BranchPath addressing, schedule invariance,
/// and so on. None of that is meaningful for pure-math files that
/// never lift an effect. The presence of `BranchPath` in the body
/// is the strongest signal — Oracle's lifting always introduces it
/// as an explicit parameter on lifted functions and as a constant
/// in their theorem statements.
#[cfg(test)]
#[allow(dead_code)]
pub fn needs_trust_header(body: &str) -> bool {
    needs_in_body(body, "BranchPath")
}

/// Single shared decision: which built-in records does this generated
/// body need, in dependency-correct emission order?
///
/// Both Lean and Dafny backends call this and then render each entry
/// through `render_lean` / `render_dafny`. No backend reimplements the
/// "what to include" logic — the source of truth is `BUILTIN_RECORDS`
/// plus this function.
///
/// `force_all` requests every record regardless of body usage (used by
/// the test fixture that sanity-checks the prelude renders end-to-end).
pub fn needed_records(body: &str, force_all: bool) -> Vec<&'static BuiltinRecord> {
    let mut selected: Vec<&'static BuiltinRecord> = Vec::new();

    fn include_with_deps(record: &'static BuiltinRecord, out: &mut Vec<&'static BuiltinRecord>) {
        if out.iter().any(|r| r.aver_name == record.aver_name) {
            return;
        }
        for dep in record.depends_on {
            if let Some(d) = find(dep) {
                include_with_deps(d, out);
            }
        }
        out.push(record);
    }

    for record in BUILTIN_RECORDS {
        let directly_needed = needs_in_body(body, &record.backend_name());
        if force_all || directly_needed {
            include_with_deps(record, &mut selected);
        }
    }

    selected
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_renders_to_lean_structure() {
        let h = find("Header").unwrap();
        let lean = render_lean(h);
        assert!(lean.starts_with("structure Header where"));
        assert!(lean.contains("name : String"));
        assert!(lean.contains("value : String"));
        assert!(lean.contains("deriving Repr, BEq, Inhabited, DecidableEq"));
    }

    #[test]
    fn http_response_renders_to_dafny_datatype() {
        let r = find("HttpResponse").unwrap();
        let dafny = render_dafny(r);
        assert_eq!(
            dafny,
            "datatype HttpResponse = HttpResponse(status: int, body: string, headers: map<string, seq<string>>)"
        );
    }

    #[test]
    fn http_request_field_method_is_renamed_in_dafny() {
        let r = find("HttpRequest").unwrap();
        let dafny = render_dafny(r);
        assert!(dafny.contains("method_: string"));
    }

    #[test]
    fn terminal_size_dotted_name_maps_to_underscore() {
        let r = find("Terminal.Size").unwrap();
        assert_eq!(r.backend_name(), "Terminal_Size");
        let lean = render_lean(r);
        assert!(lean.starts_with("structure Terminal_Size where"));
        let dafny = render_dafny(r);
        assert_eq!(
            dafny,
            "datatype Terminal_Size = Terminal_Size(width: int, height: int)"
        );
    }

    #[test]
    fn tcp_connection_dotted_name_maps_to_underscore() {
        let r = find("Tcp.Connection").unwrap();
        assert_eq!(r.backend_name(), "Tcp_Connection");
    }

    #[test]
    fn needs_in_body_is_word_bounded() {
        assert!(needs_in_body("foo Header bar", "Header"));
        assert!(needs_in_body("Header", "Header"));
        assert!(!needs_in_body("HeaderX", "Header"));
        assert!(!needs_in_body("XHeader", "Header"));
    }

    #[test]
    fn dependencies_listed_for_http_records() {
        // Headers field is `Map<String, List<String>>` (built-in shape),
        // not `List<Header>`, so HTTP records have no Header dependency.
        assert!(find("HttpResponse").unwrap().depends_on.is_empty());
        assert!(find("HttpRequest").unwrap().depends_on.is_empty());
        assert!(find("Header").unwrap().depends_on.is_empty());
    }
}
