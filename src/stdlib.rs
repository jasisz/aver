//! Aver modules shipped with the compiler.
//!
//! Standard modules remain ordinary Aver source: they pass through the same
//! parser, type checker, VM compiler, and proof exporters as project modules.
//! Embedding only gives them an installation-independent home.

/// An Aver source module embedded in the compiler binary.
pub(crate) struct EmbeddedModule {
    pub(crate) virtual_path: &'static str,
    pub(crate) source: &'static str,
}

/// Resolve module names reserved by Aver's standard library.
///
/// Standard modules win over a same-named project file so a dependency cannot
/// silently change meaning with `--module-root`.
pub(crate) fn find(name: &str) -> Option<EmbeddedModule> {
    match name {
        "Bytes" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/bytes.av",
            source: include_str!("../stdlib/bytes.av"),
        }),
        "Crypto.Digest32" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/crypto/digest32.av",
            source: include_str!("../stdlib/crypto/digest32.av"),
        }),
        _ => None,
    }
}

/// Builtins whose signatures cross nominal record types owned by embedded
/// standard modules, paired with the modules those types live in.
///
/// Two consumers keep the builtin ↔ standard-module mapping in one place:
/// - `TypeChecker::canonicalize_source_typed_builtin_sigs` re-stamps these
///   signatures once the owning modules enter the symbol table;
/// - `implicit_stdlib_deps` lets compilation load the owning modules even
///   when a module never names them in `depends`, so every backend can emit
///   the nominal records the builtin's boundary references.
pub(crate) const SOURCE_TYPED_BUILTINS: &[(&str, &[&str])] = &[
    ("Crypto.sha256", &["Bytes", "Crypto.Digest32"]),
    ("Tcp.sendBytes", &["Bytes"]),
    ("Tcp.readBytes", &["Bytes"]),
    ("Tcp.writeBytes", &["Bytes"]),
];

/// Standard modules `items` implicitly depends on because a function body or
/// top-level statement calls a builtin whose signature crosses stdlib-owned
/// nominal types (e.g. `Crypto.sha256` produces a `Digest32` even when
/// `depends` never names `Crypto.Digest32`).
pub fn implicit_stdlib_deps(items: &[crate::ast::TopLevel]) -> Vec<String> {
    let mut callees = std::collections::HashSet::new();
    for item in items {
        match item {
            crate::ast::TopLevel::FnDef(fd) => {
                crate::call_graph::collect_callees_body(&fd.body, &mut callees);
            }
            crate::ast::TopLevel::Stmt(stmt) => {
                crate::call_graph::collect_callees_stmt(stmt, &mut callees);
            }
            _ => {}
        }
    }
    let mut deps: Vec<String> = Vec::new();
    for (builtin, modules) in SOURCE_TYPED_BUILTINS {
        if callees.contains(*builtin) {
            for module in *modules {
                if !deps.iter().any(|dep| dep == module) {
                    deps.push((*module).to_string());
                }
            }
        }
    }
    deps
}

#[cfg(test)]
mod tests {
    use super::implicit_stdlib_deps;

    fn parse(source: &str) -> Vec<crate::ast::TopLevel> {
        crate::source::parse_source(source).expect("parse test module")
    }

    #[test]
    fn sha256_call_implies_bytes_and_digest32() {
        let items = parse(
            "module Edge\n    intent = \"hash without naming Crypto.Digest32\"\n    depends [Bytes]\n    effects []\n\nfn hash(bytes: Bytes) -> String\n    ? \"Hash and discard.\"\n    digest = Crypto.sha256(bytes)\n    \"hashed\"\n",
        );
        assert_eq!(
            implicit_stdlib_deps(&items),
            vec!["Bytes", "Crypto.Digest32"]
        );
    }

    #[test]
    fn programs_without_source_typed_builtins_imply_nothing() {
        let items = parse(
            "module Plain\n    intent = \"no stdlib-typed builtins\"\n    depends []\n    effects []\n\nfn double(n: Int) -> Int\n    ? \"Double a number.\"\n    n * 2\n",
        );
        assert!(implicit_stdlib_deps(&items).is_empty());
    }
}
