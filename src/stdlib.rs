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
        "Disk" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/capabilities/disk.av",
            source: include_str!("../stdlib/capabilities/disk.av"),
        }),
        _ => None,
    }
}

/// Provider-backed standard capability modules. Operation identities and
/// semantics are derived from their embedded Aver contracts rather than
/// repeated in a Rust table.
pub(crate) const STANDARD_CAPABILITY_MODULES: &[&str] = &["Disk", "Random", "Time"];

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

/// Standard modules `items` implicitly depends on because a function body,
/// top-level statement, or verify case calls a builtin whose signature
/// crosses stdlib-owned nominal types (e.g. `Crypto.sha256` produces a
/// `Digest32` even when `depends` never names `Crypto.Digest32`).
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
    deps
}

/// Parse the standard capability contracts shipped by the compiler.
///
/// They are globally reserved and automatically visible; callers do not need
/// a `depends [Time]`, `depends [Random]`, or `depends [Disk]` merely to use
/// a built-in standard capability.
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
    for name in required {
        if modules.iter().any(|loaded| loaded.dep_name == name) {
            continue;
        }
        let module = find(&name).expect("required standard capability must be embedded");
        modules.push(crate::source::LoadedModule {
            dep_name: name,
            items: crate::source::parse_source(module.source)
                .expect("embedded standard capability must parse"),
            path: std::path::PathBuf::from(module.virtual_path),
        });
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
        assert_eq!(implicit_stdlib_deps(&items), vec!["Bytes"]);
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
    }

    #[test]
    fn standard_time_contract_and_model_hashes_are_stable() {
        let registry = standard_capability_registry();
        let contract = registry.contract("Time").expect("Time contract");
        assert_eq!(
            contract.contract_hash,
            "sha256:c7bd82159c4e5922771531cbf583bf6ff74a85dbb5c2c362d1e3b156c5720a49"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:3b9239af56c4e89e527a53ce6fe4a470a42f84b203b10078c8633f39a6cec5f6"
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
            "sha256:d5d224fdf600e70776a570c5fb11781b4ca0e5260196860f477b355676c80197"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:100e21ef3da57eeed31149c82704a77f125811f3da90a376ab9484bd41fba9c4"
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
            "sha256:d134b487a92f2094eb6ad478bff0984c5a481577df07a7f993652e9bc1f9d537"
        );
        assert_eq!(
            contract.model_hash,
            "sha256:06f28c8acc428e9c55ecba571f96e1d23e2300a8ac307b8b6d6d0771fd18e604"
        );
        // The nineteen contract profiles carry the exact diagnostic labels
        // the handwritten Rust table used before the flip, so hostile-run
        // outputs do not move for existing programs.
        for (method, labels) in [
            ("Disk.readText", vec!["normal", "always_err", "empty_ok"]),
            ("Disk.writeText", vec!["normal_ok", "always_err"]),
            ("Disk.appendText", vec!["normal_ok", "always_err"]),
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
}
