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
        "Time" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/time.av",
            source: include_str!("../stdlib/capabilities/time.av"),
        }),
        "Random" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/random.av",
            source: include_str!("../stdlib/capabilities/random.av"),
        }),
        "Process" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/process.av",
            source: include_str!("../stdlib/capabilities/process.av"),
        }),
        "Disk" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/disk.av",
            source: include_str!("../stdlib/capabilities/disk.av"),
        }),
        "Tcp" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/tcp.av",
            source: include_str!("../stdlib/capabilities/tcp.av"),
        }),
        _ => None,
    }
}

/// Provider-backed standard capability modules. Operation identities and
/// semantics are derived from their embedded Aver contracts rather than
/// repeated in a Rust table.
pub(crate) const STANDARD_CAPABILITY_MODULES: &[&str] =
    &["Disk", "Process", "Random", "Tcp", "Time"];

/// Host-backed calls whose signatures cross nominal record types owned by
/// embedded standard modules, paired with the modules those types live in.
///
/// Two consumers keep the builtin ↔ standard-module mapping in one place:
/// - `TypeChecker::canonicalize_source_typed_builtin_sigs` re-stamps these
///   signatures once the owning modules enter the symbol table;
/// - `implicit_stdlib_deps` lets compilation load the owning modules even
///   when a module never names them in `depends`, so every backend can emit
///   the nominal records the builtin's boundary references.
pub(crate) const SOURCE_TYPED_BUILTINS: &[(&str, &[&str])] = &[
    ("Crypto.sha256", &["Bytes", "Crypto.Digest32"]),
    ("String.toUtf8", &["Bytes"]),
    ("String.fromUtf8", &["Bytes"]),
];

/// Standard modules `items` implicitly depends on because a type annotation,
/// record expression, or call crosses a stdlib-owned nominal type (e.g.
/// `Crypto.sha256` produces a `Digest32` even when `depends` never names
/// `Crypto.Digest32`).
///
/// Verify blocks count because backends compile them too: Rust codegen
/// emits verify cases into a `#[cfg(test)]` module, so a program whose only
/// `Crypto.sha256` call sits in a verify case still generates code that
/// references the `Digest32` record. Cases some backend later skips only
/// over-load a standard module, which is harmless.
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
            crate::ast::TopLevel::Verify(vb) => {
                for (left, right) in &vb.cases {
                    crate::call_graph::collect_callees_expr(left, &mut callees);
                    crate::call_graph::collect_callees_expr(right, &mut callees);
                }
                for givens in &vb.case_givens {
                    for (_, expr) in givens {
                        crate::call_graph::collect_callees_expr(expr, &mut callees);
                    }
                }
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
    for operation in standard_capability_registry_ref().operations() {
        if callees.contains(&operation.canonical_name)
            && !deps.iter().any(|dep| dep == &operation.module)
        {
            deps.push(operation.module.clone());
        }
    }
    for item in items {
        collect_standard_type_dependencies(item, &mut deps);
    }
    deps
}

fn collect_standard_type_dependencies(item: &crate::ast::TopLevel, deps: &mut Vec<String>) {
    use crate::ast::{CapabilityItem, Stmt, TopLevel, TypeDef};

    match item {
        TopLevel::FnDef(function) => {
            for (_, annotation) in &function.params {
                collect_standard_modules_from_annotation(annotation, deps);
            }
            collect_standard_modules_from_annotation(&function.return_type, deps);
            for statement in function.body.stmts() {
                match statement {
                    Stmt::Binding(_, annotation, expression) => {
                        if let Some(annotation) = annotation {
                            collect_standard_modules_from_annotation(annotation, deps);
                        }
                        collect_standard_modules_from_expr(expression, deps);
                    }
                    Stmt::Expr(expression) => {
                        collect_standard_modules_from_expr(expression, deps);
                    }
                }
            }
        }
        TopLevel::Stmt(statement) => match statement {
            Stmt::Binding(_, annotation, expression) => {
                if let Some(annotation) = annotation {
                    collect_standard_modules_from_annotation(annotation, deps);
                }
                collect_standard_modules_from_expr(expression, deps);
            }
            Stmt::Expr(expression) => collect_standard_modules_from_expr(expression, deps),
        },
        TopLevel::TypeDef(TypeDef::Product { fields, .. }) => {
            for (_, annotation) in fields {
                collect_standard_modules_from_annotation(annotation, deps);
            }
        }
        TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
            for variant in variants {
                for annotation in &variant.fields {
                    collect_standard_modules_from_annotation(annotation, deps);
                }
            }
        }
        TopLevel::Capability(CapabilityItem::Operation(operation)) => {
            for (_, annotation) in &operation.params {
                collect_standard_modules_from_annotation(annotation, deps);
            }
            collect_standard_modules_from_annotation(&operation.return_type, deps);
        }
        TopLevel::Verify(verify) => {
            for (left, right) in &verify.cases {
                collect_standard_modules_from_expr(left, deps);
                collect_standard_modules_from_expr(right, deps);
            }
            for givens in &verify.case_givens {
                for (_, expression) in givens {
                    collect_standard_modules_from_expr(expression, deps);
                }
            }
        }
        _ => {}
    }
}

fn collect_standard_modules_from_annotation(annotation: &str, deps: &mut Vec<String>) {
    let ty = crate::types::parse_type_str(annotation);
    collect_standard_modules_from_type(&ty, deps);
}

/// Record syntax carries a type reference even though it is not an ordinary
/// call in the call graph. Discover it before typechecking so an attempted
/// `Tcp.Connection(...)` forge loads the canonical capability contract and
/// reaches the resource-construction gate instead of looking like an unknown,
/// fieldless user record.
fn collect_standard_modules_from_expr(
    expression: &crate::ast::Spanned<crate::ast::Expr>,
    deps: &mut Vec<String>,
) {
    crate::call_graph::walk_expr(expression, &mut |node| match node {
        crate::ast::Expr::RecordCreate { type_name, .. }
        | crate::ast::Expr::RecordUpdate { type_name, .. } => {
            collect_standard_modules_from_type(&crate::types::Type::named(type_name.clone()), deps);
        }
        _ => {}
    });
}

fn collect_standard_modules_from_type(ty: &crate::types::Type, deps: &mut Vec<String>) {
    use crate::types::Type;

    match ty {
        Type::Named { name, .. } => {
            if let Some((module, _)) = name.split_once('.')
                && is_standard_capability(module)
                && !deps.iter().any(|dependency| dependency == module)
            {
                deps.push(module.to_string());
            }
        }
        Type::List(inner) | Type::Vector(inner) | Type::Option(inner) => {
            collect_standard_modules_from_type(inner, deps);
        }
        Type::Map(key, value) | Type::Result(key, value) => {
            collect_standard_modules_from_type(key, deps);
            collect_standard_modules_from_type(value, deps);
        }
        Type::Tuple(items) => {
            for item in items {
                collect_standard_modules_from_type(item, deps);
            }
        }
        Type::Fn(params, result, _) => {
            for param in params {
                collect_standard_modules_from_type(param, deps);
            }
            collect_standard_modules_from_type(result, deps);
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Invalid
        | Type::Var(_) => {}
    }
}

/// Parse the standard capability contracts shipped by the compiler.
///
/// They are globally reserved and automatically visible; callers do not need
/// a `depends [Time]`, `depends [Random]`, `depends [Process]`, or
/// `depends [Disk]` merely to use a built-in standard capability.
pub(crate) fn standard_capability_modules() -> Vec<crate::source::LoadedModule> {
    STANDARD_CAPABILITY_MODULES
        .iter()
        .map(|name| {
            let module = find(name).expect("standard capability source must be embedded");
            crate::source::LoadedModule {
                dep_name: (*name).to_string(),
                items: crate::source::parse_source(module.source)
                    .expect("embedded standard capability must parse"),
                path: std::path::PathBuf::from(module.virtual_path),
            }
        })
        .collect()
}

/// Canonical registry for compiler-shipped capabilities. Construction is
/// cheap enough for compiler setup and returns an owned clone, while the
/// parsed/validated value itself is cached once per process.
pub fn standard_capability_registry() -> crate::capability::CapabilityRegistry {
    standard_capability_registry_ref().clone()
}

pub(crate) fn standard_capability_registry_ref() -> &'static crate::capability::CapabilityRegistry {
    static REGISTRY: std::sync::OnceLock<crate::capability::CapabilityRegistry> =
        std::sync::OnceLock::new();
    REGISTRY.get_or_init(|| {
        let mut registry = crate::capability::CapabilityRegistry::default();
        for module in standard_capability_modules() {
            let (next, errors) =
                crate::capability::CapabilityRegistry::from_module(&module.dep_name, &module.items);
            assert!(
                errors.is_empty(),
                "embedded standard capability '{}' is invalid: {errors:?}",
                module.dep_name
            );
            registry.merge(next);
        }
        registry
    })
}

pub(crate) fn is_standard_capability(module: &str) -> bool {
    STANDARD_CAPABILITY_MODULES.contains(&module)
}

/// Source-module dependencies needed when a standard hostile profile is
/// lifted into the entry module as a synthetic verify function.
pub(crate) fn standard_capability_profile_dependencies(method: &str) -> Vec<String> {
    let Some(operation) = standard_capability_registry_ref().operation(method) else {
        return Vec::new();
    };
    let Some(module) = find(&operation.module) else {
        return Vec::new();
    };
    crate::source::parse_source(module.source)
        .ok()
        .and_then(|items| {
            items.into_iter().find_map(|item| match item {
                crate::ast::TopLevel::Module(module) => Some(module.depends),
                _ => None,
            })
        })
        .unwrap_or_default()
}

pub(crate) fn append_required_standard_capability_modules(
    entry_items: &[crate::ast::TopLevel],
    modules: &mut Vec<crate::source::LoadedModule>,
) {
    let mut required = std::collections::BTreeSet::new();
    for items in
        std::iter::once(entry_items).chain(modules.iter().map(|module| module.items.as_slice()))
    {
        for dependency in implicit_stdlib_deps(items) {
            if is_standard_capability(&dependency) {
                required.insert(dependency);
            }
        }
    }
    let mut pending = required.into_iter().collect::<Vec<_>>();
    let mut index = 0;
    while index < pending.len() {
        let name = pending[index].clone();
        index += 1;
        if !modules.iter().any(|loaded| loaded.dep_name == name) {
            let module = find(&name).expect("required standard capability must be embedded");
            modules.push(crate::source::LoadedModule {
                dep_name: name.clone(),
                items: crate::source::parse_source(module.source)
                    .expect("embedded standard capability must parse"),
                path: std::path::PathBuf::from(module.virtual_path),
            });
        }

        // Embedded capability contracts may themselves use a source-defined
        // standard boundary type. Pull that ordinary module into the same
        // closure even in callers (playground/tests) that supplied no module
        // root for `load_module_tree` to recurse through.
        let loaded = modules
            .iter()
            .find(|loaded| loaded.dep_name == name)
            .expect("just loaded or already present");
        let dependencies = loaded.items.iter().find_map(|item| match item {
            crate::ast::TopLevel::Module(module) => Some(module.depends.as_slice()),
            _ => None,
        });
        for dependency in dependencies.into_iter().flatten() {
            if find(dependency).is_some()
                && !modules.iter().any(|loaded| loaded.dep_name == *dependency)
                && !pending.iter().any(|queued| queued == dependency)
            {
                pending.push(dependency.clone());
            }
        }
    }
}

/// Hostile profile source declared by a standard capability operation.
/// Returns `(diagnostic_label, declared_fn_name, source_fn)` in declaration
/// order so the legacy verify injector can consume the canonical module model
/// without carrying a second hand-written semantic table.
pub(crate) fn standard_hostile_profiles(method: &str) -> Vec<(&'static str, String, String)> {
    type ProfileSource = (&'static str, String, String);
    static PROFILES: std::sync::OnceLock<std::collections::BTreeMap<String, Vec<ProfileSource>>> =
        std::sync::OnceLock::new();
    PROFILES
        .get_or_init(|| {
            let mut profiles = std::collections::BTreeMap::new();
            for operation in standard_capability_registry_ref().operations() {
                let Some(module) = find(&operation.module) else {
                    continue;
                };
                let entries = operation
                    .hostile
                    .iter()
                    .filter_map(|profile| {
                        let marker = format!("fn {profile}(");
                        let start = module.source.find(&marker)?;
                        let rest = &module.source[start..];
                        let end = rest[1..]
                            .find("\nfn ")
                            .map(|offset| offset + 1)
                            .unwrap_or(rest.len());
                        let source = format!("{}\n", rest[..end].trim_end());
                        let suffix = profile
                            .strip_prefix(&operation.name)
                            .unwrap_or(profile.as_str());
                        let label: &'static str =
                            Box::leak(camel_to_snake(suffix).into_boxed_str());
                        Some((label, profile.clone(), source))
                    })
                    .collect();
                profiles.insert(operation.canonical_name.clone(), entries);
            }
            profiles
        })
        .get(method)
        .cloned()
        .unwrap_or_default()
}

pub(crate) fn standard_hostile_profile_label(
    method: &str,
    qualified_profile: &str,
) -> Option<&'static str> {
    let module = method.split_once('.')?.0;
    let bare = qualified_profile
        .strip_prefix(module)
        .and_then(|rest| rest.strip_prefix('.'))
        .unwrap_or(qualified_profile);
    standard_hostile_profiles(method)
        .into_iter()
        .find_map(|(label, declared_name, _)| (declared_name == bare).then_some(label))
}

fn camel_to_snake(name: &str) -> String {
    let mut out = String::new();
    for (index, ch) in name.chars().enumerate() {
        if ch.is_uppercase() && index > 0 {
            out.push('_');
        }
        out.extend(ch.to_lowercase());
    }
    out
}

#[cfg(test)]
mod tests {
    use super::{
        find, implicit_stdlib_deps, standard_capability_registry, standard_hostile_profile_label,
        standard_hostile_profiles,
    };

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

    #[test]
    fn sha256_call_only_in_verify_case_implies_modules() {
        // Rust codegen emits verify cases into a #[cfg(test)] module, so a
        // sha256 call that appears ONLY inside a verify block still needs
        // the Bytes/Digest32 modules in the generated project.
        //
        // The two sides of the case sit on opposite sides of the literal
        // discharge boundary on purpose: `[double(0)]` has a computed
        // element so it keeps `Result` (hence the `?`), while `[0]` is an
        // all-literal in-range list and types as `Bytes` directly. The
        // implicit-dependency scan must reach both spellings.
        let items = parse(
            "module VerifyOnly\n    intent = \"sha256 only in a verify case\"\n    depends [Bytes]\n    effects []\n\nfn double(n: Int) -> Int\n    ? \"Double a number.\"\n    n * 2\n\nverify double\n    Crypto.sha256(Bytes.fromList([double(0)])?) => Crypto.sha256(Bytes.fromList([0]))\n",
        );
        assert_eq!(
            implicit_stdlib_deps(&items),
            vec!["Bytes", "Crypto.Digest32"]
        );
    }

    #[test]
    fn tcp_read_bytes_implies_bytes_without_any_depends() {
        // Tcp.readBytes RETURNS Bytes, so a program can hold Bytes values
        // without ever naming the module: read a frame, write it back.
        let items = parse(
            "module Relay\n    intent = \"pipe frames without naming Bytes\"\n    depends []\n    effects [Tcp]\n\nfn relay(conn: Tcp.Connection) -> Result<Unit, String>\n    ? \"Echo one 4-byte frame back to the peer.\"\n    ! [Tcp.readBytes, Tcp.writeBytes]\n    frame = Tcp.readBytes(conn, 4)?\n    Tcp.writeBytes(conn, frame)\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Tcp"]);
    }

    #[test]
    fn standard_resource_record_syntax_loads_its_capability_contract() {
        // The typechecker needs the Tcp contract even though this invalid
        // program has neither a Tcp annotation nor a Tcp operation call: only
        // then can it diagnose provider-owned resource fabrication precisely.
        let items = parse(
            "module Forge\n    intent = \"try to forge a provider resource\"\n    depends []\n    effects []\n\nfn fake() -> Unit\n    _ = Tcp.Connection(id = \"fake\", host = \"\", port = 0)\n    Unit\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Tcp"]);
    }

    #[test]
    fn disk_read_bytes_implies_the_contract_and_bytes_without_depends() {
        let items = parse(
            "module Reader\n    intent = \"read a binary file\"\n    depends []\n    effects [Disk.readBytes]\n\nfn read(path: String) -> Result<Bytes, String>\n    ? \"Read exact octets.\"\n    ! [Disk.readBytes]\n    Disk.readBytes(path)\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Disk"]);
    }

    #[test]
    fn standard_capability_calls_implicitly_load_reserved_contracts() {
        let items = parse(
            "module ClockUser\n    effects [Time.now]\n\nfn stamp() -> String\n    ! [Time.now]\n    Time.now()\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Time"]);
        let embedded = find("Time").expect("reserved Time module");
        assert_eq!(embedded.virtual_path, "<aver-stdlib>/capabilities/time.av");

        let items = parse(
            "module DiceUser\n    effects [Random.int]\n\nfn roll() -> Int\n    ! [Random.int]\n    Random.int(1, 6)\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Random"]);
        let embedded = find("Random").expect("reserved Random module");
        assert_eq!(
            embedded.virtual_path,
            "<aver-stdlib>/capabilities/random.av"
        );

        let items = parse(
            "module FileUser\n    effects [Disk.readText]\n\nfn read() -> Result<String, String>\n    ! [Disk.readText]\n    Disk.readText(\"data.txt\")\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Disk"]);
        let embedded = find("Disk").expect("reserved Disk module");
        assert_eq!(embedded.virtual_path, "<aver-stdlib>/capabilities/disk.av");

        let items = parse(
            "module Worker\n    effects [Process.stopRequested]\n\nfn stopping() -> Bool\n    ! [Process.stopRequested]\n    Process.stopRequested()\n",
        );
        assert_eq!(implicit_stdlib_deps(&items), vec!["Process"]);
        let embedded = find("Process").expect("reserved Process module");
        assert_eq!(
            embedded.virtual_path,
            "<aver-stdlib>/capabilities/process.av"
        );
    }

    #[test]
    fn standard_process_contract_model_and_profiles_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Process").expect("Process contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:5625844cde1e2704aeb154587181efb18ffe95ad2be047da00f8dd2b69874357"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:a005e95c1d4b21297cf1e365417651ec0feb6433c56233d32afc86983f387ab5"
        );
        assert_eq!(
            standard_hostile_profiles("Process.stopRequested")
                .into_iter()
                .map(|(label, _, _)| label)
                .collect::<Vec<_>>(),
            vec![
                "stop_never",
                "stop_immediately",
                "stop_after_one",
                "stop_after_three"
            ]
        );
        assert_eq!(
            standard_hostile_profile_label("Process.stopRequested", "Process.stopAfterThree"),
            Some("stop_after_three")
        );
    }

    #[test]
    fn standard_time_contract_and_model_hashes_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Time").expect("Time contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:e80d264b61f2808b4db4d765ded0d3db1a9a019c814d27686ef7e71bc4c208af"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:07ad032ad093e63f61e39f59f9452b4787936c18f97953a955c998e6593ac294"
        );
        assert_eq!(
            standard_hostile_profiles("Time.unixMs")
                .into_iter()
                .map(|(label, _, _)| label)
                .collect::<Vec<_>>(),
            vec![
                "normal",
                "frozen_zero",
                "saturated",
                "backward",
                "fast_forward"
            ]
        );
        assert_eq!(
            standard_hostile_profile_label("Time.unixMs", "Time.unixMsNormal"),
            Some("normal")
        );
    }

    #[test]
    fn standard_random_contract_model_and_profiles_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Random").expect("Random contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:5c23bcf6fe8a6515ea430de874828421cff538f89b3bc142d03f2e6cc014dec7"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:88b58ba022e2decf378a0c16597808b09ec6923c2d07977ae9ed28502ea5878b"
        );
        assert_eq!(
            standard_hostile_profiles("Random.int")
                .into_iter()
                .map(|(label, _, _)| label)
                .collect::<Vec<_>>(),
            vec!["midrange", "always_min", "always_max", "alternating"]
        );
        assert_eq!(
            standard_hostile_profile_label("Random.float", "Random.floatAlwaysOne"),
            Some("always_one")
        );
    }

    #[test]
    fn standard_disk_contract_model_and_profiles_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Disk").expect("Disk contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:21ba58983c2ba61c06153df36a9c205770994c36a61ae280c1f49da336e63e23"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:cf55979e264c3a26a246bb77663422011efb71e6ce2ba973ad4b267195f25570"
        );
        // Existing text-operation labels remain stable while the binary and
        // metadata profiles make short reads and zero-length files explicit.
        for (method, labels) in [
            ("Disk.readText", vec!["normal", "always_err", "empty_ok"]),
            ("Disk.writeText", vec!["normal_ok", "always_err"]),
            ("Disk.appendText", vec!["normal_ok", "always_err"]),
            ("Disk.readBytes", vec!["normal", "always_err", "empty_ok"]),
            ("Disk.readBytesAt", vec!["normal", "always_err", "short_ok"]),
            ("Disk.writeBytes", vec!["normal_ok", "always_err"]),
            ("Disk.appendBytes", vec!["normal_ok", "always_err"]),
            ("Disk.size", vec!["normal", "zero", "always_err"]),
            ("Disk.exists", vec!["normal", "never", "always"]),
            ("Disk.delete", vec!["normal_ok", "always_err"]),
            ("Disk.deleteDir", vec!["normal_ok", "always_err"]),
            ("Disk.listDir", vec!["normal", "empty", "always_err"]),
            ("Disk.makeDir", vec!["normal_ok", "always_err"]),
        ] {
            assert_eq!(
                standard_hostile_profiles(method)
                    .into_iter()
                    .map(|(label, _, _)| label)
                    .collect::<Vec<_>>(),
                labels,
                "hostile labels for {method}"
            );
        }
        assert_eq!(
            standard_hostile_profile_label("Disk.writeText", "Disk.writeTextAlwaysErr"),
            Some("always_err")
        );
    }

    #[test]
    fn standard_tcp_contract_model_and_profiles_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Tcp").expect("Tcp contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:2f32788e56fb4be7a05fa348315e52285e09c8e671ba02c05285105a68911af9"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:4d6a91832fe25919d9dcce68bb86397a72f959e24c4a685923abde7d3359a407"
        );
        for (method, labels) in [
            ("Tcp.send", vec!["normal_ok", "always_err"]),
            ("Tcp.sendBytes", vec!["normal_ok", "always_err"]),
            ("Tcp.ping", vec!["normal_ok", "always_err"]),
            ("Tcp.connect", vec!["normal_ok", "always_err"]),
            ("Tcp.beginConnect", vec!["normal_ok", "always_err"]),
            (
                "Tcp.dialled",
                vec!["connected", "still_pending", "refused"],
            ),
            ("Tcp.listen", vec!["normal_ok", "port_taken"]),
            (
                "Tcp.accept",
                vec!["nothing_pending", "once_then_nothing", "always_err"],
            ),
            ("Tcp.peerAddress", vec!["normal_ok", "always_err"]),
            (
                "Tcp.poll",
                vec!["none_ready", "everything_ready", "always_err"],
            ),
            ("Tcp.writeLine", vec!["normal_ok", "always_err"]),
            ("Tcp.writeBytes", vec!["normal_ok", "always_err"]),
            ("Tcp.readLine", vec!["normal_ok", "always_err"]),
            (
                "Tcp.readBytes",
                vec!["normal_ok", "short_read", "always_err"],
            ),
            ("Tcp.readSome", vec!["normal_ok", "eof", "always_err"]),
            ("Tcp.close", vec!["normal_ok", "always_err"]),
            ("Tcp.closeDial", vec!["normal_ok", "always_err"]),
            ("Tcp.closeListener", vec!["normal_ok", "always_err"]),
        ] {
            assert_eq!(
                standard_hostile_profiles(method)
                    .into_iter()
                    .map(|(label, _, _)| label)
                    .collect::<Vec<_>>(),
                labels,
                "hostile labels for {method}"
            );
        }
        assert_eq!(
            standard_hostile_profile_label("Tcp.connect", "Tcp.connectNormalOk"),
            Some("normal_ok")
        );
    }
}
