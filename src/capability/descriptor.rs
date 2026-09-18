use super::validation::{collect_named_types, type_def_name};
use super::*;

pub(super) fn reachable_type_defs<'a>(
    operations: &[CapabilityOperation],
    defs: &BTreeMap<String, &'a TypeDef>,
) -> Vec<&'a TypeDef> {
    let mut pending = BTreeSet::new();
    for op in operations {
        for (_, ty) in &op.params {
            collect_named_types(ty, &mut pending);
        }
        collect_named_types(&op.return_type, &mut pending);
    }
    let mut seen = BTreeSet::new();
    while let Some(name) = pending.pop_first() {
        if !seen.insert(name.clone()) {
            continue;
        }
        let Some(td) = defs.get(&name) else { continue };
        match td {
            TypeDef::Sum { variants, .. } => {
                for variant in variants {
                    for field in &variant.fields {
                        if let Ok(ty) = crate::types::parse_type_str_strict(field) {
                            collect_named_types(&ty, &mut pending);
                        }
                    }
                }
            }
            TypeDef::Product { fields, .. } => {
                for (_, field) in fields {
                    if let Ok(ty) = crate::types::parse_type_str_strict(field) {
                        collect_named_types(&ty, &mut pending);
                    }
                }
            }
        }
    }
    seen.into_iter()
        .filter_map(|name| defs.get(&name).copied())
        .collect()
}

/// `imported` holds the dependency layouts a job kind's boundary named, keyed
/// by the canonical `Owner.Name` the descriptor prints. It is empty for every
/// other capability, and an empty table renders byte for byte the descriptor
/// this builder has always rendered.
pub(super) fn render_contract_descriptor(
    scope: &str,
    module_name: &str,
    operations: &[CapabilityOperation],
    resources: &[String],
    reachable_types: &[&TypeDef],
    imported: &BTreeMap<String, TypeDef>,
    dependencies: &DependencyTypes,
) -> Vec<u8> {
    let mut descriptor = CanonicalDescriptor::default();
    descriptor.field("avercap", "1");
    descriptor.field("kind", "contract");
    descriptor.field(
        "capability",
        &format!("{}::{module_name}", descriptor_path(scope)),
    );

    let mut reachable_names = BTreeSet::new();
    for operation in operations {
        for (_, ty) in &operation.params {
            collect_named_types(ty, &mut reachable_names);
        }
        collect_named_types(&operation.return_type, &mut reachable_names);
    }
    for td in reachable_types {
        reachable_names.insert(type_def_name(td).to_string());
    }
    let local_names: BTreeSet<String> = reachable_names
        .iter()
        .filter(|name| {
            resources.contains(name) || reachable_types.iter().any(|td| type_def_name(td) == *name)
        })
        .cloned()
        .collect();

    // The canonical name a dependency layout is printed and addressed under.
    // Empty for every capability but a job kind that named one.
    let imported_names: BTreeSet<String> = imported.keys().cloned().collect();

    // A dependency layout can be the only place the octet wire type is
    // reached, and the backends read that fact off this descriptor.
    let imported_bytes = imported.values().any(|type_def| {
        layout_field_sources(type_def).iter().any(|source| {
            let mut names = BTreeSet::new();
            if let Ok(ty) = crate::types::parse_type_str_strict(source) {
                collect_named_types(&ty, &mut names);
            }
            names.contains("Bytes")
        })
    });
    let uses_standard_bytes = reachable_names.contains("Bytes")
        || reachable_names.contains("Bytes.Bytes")
        || imported_bytes;
    if uses_standard_bytes && !local_names.contains("Bytes") {
        descriptor.field("type", "Aver::Bytes = octets");
    }

    let mut reachable_resources = resources
        .iter()
        .filter(|name| reachable_names.contains(name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    reachable_resources.sort();
    for name in reachable_resources {
        descriptor.field(
            "type",
            &format!("{}::{name} = resource", descriptor_path(scope)),
        );
    }

    let mut types = reachable_types.to_vec();
    types.sort_by_key(|td| type_def_name(td));
    for td in types {
        let path = format!("{}::{}", descriptor_path(scope), type_def_name(td));
        descriptor.field(
            "type",
            &layout_row(
                &path,
                scope,
                scope,
                td,
                &local_names,
                &imported_names,
                dependencies,
            ),
        );
    }
    // A dependency layout is printed under its owner's path, and its own
    // field names resolve in its owner's module. `imported` is a BTreeMap, so
    // the rows stay in canonical-name order and the hash stays deterministic.
    for (canonical, type_def) in imported {
        let owner = canonical.rsplit_once('.').map_or(scope, |(owner, _)| owner);
        descriptor.field(
            "type",
            &layout_row(
                &descriptor_path(canonical),
                scope,
                owner,
                type_def,
                &local_names,
                &imported_names,
                dependencies,
            ),
        );
    }
    let mut operations = operations.to_vec();
    operations.sort_by(|a, b| a.name.cmp(&b.name));
    for op in operations {
        let params = op
            .params
            .iter()
            .map(|(_, ty)| {
                canonical_type(
                    scope,
                    scope,
                    ty,
                    &local_names,
                    &imported_names,
                    dependencies,
                )
            })
            .collect::<Vec<_>>()
            .join(",");
        let ret = canonical_type(
            scope,
            scope,
            &op.return_type,
            &local_names,
            &imported_names,
            dependencies,
        );
        descriptor.field("op", &format!("{}({params}) -> {ret}", op.name));
    }
    descriptor.into_bytes()
}

/// The field source texts of one layout, in declaration order.
fn layout_field_sources(type_def: &TypeDef) -> Vec<&str> {
    match type_def {
        TypeDef::Product { fields, .. } => fields.iter().map(|(_, ty)| ty.as_str()).collect(),
        TypeDef::Sum { variants, .. } => variants
            .iter()
            .flat_map(|variant| variant.fields.iter().map(String::as_str))
            .collect(),
    }
}

/// One `type` row: the layout under `path`, with every field type written
/// canonically. `owner` is the module whose scope the layout's own bare field
/// names belong to, which is the capability itself for a local declaration
/// and the declaring module for a dependency layout.
#[allow(clippy::too_many_arguments)]
fn layout_row(
    path: &str,
    capability: &str,
    owner: &str,
    type_def: &TypeDef,
    local_names: &BTreeSet<String>,
    imported_names: &BTreeSet<String>,
    dependencies: &DependencyTypes,
) -> String {
    match type_def {
        TypeDef::Sum { variants, .. } => {
            let mut variants = variants.clone();
            variants.sort_by(|left, right| left.name.cmp(&right.name));
            let variants = variants
                .iter()
                .map(|variant| {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| {
                            canonical_type_text(
                                capability,
                                owner,
                                field,
                                local_names,
                                imported_names,
                                dependencies,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    if fields.is_empty() {
                        variant.name.clone()
                    } else {
                        format!("{}({fields})", variant.name)
                    }
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("{path} = sum{{{variants}}}")
        }
        TypeDef::Product { fields, .. } => {
            let mut fields = fields.clone();
            fields.sort_by(|left, right| left.0.cmp(&right.0));
            let fields = fields
                .iter()
                .map(|(field, ty)| {
                    format!(
                        "{field}:{}",
                        canonical_type_text(
                            capability,
                            owner,
                            ty,
                            local_names,
                            imported_names,
                            dependencies
                        )
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("{path} = record{{{fields}}}")
        }
    }
}

#[derive(Default)]
struct CanonicalDescriptor {
    bytes: Vec<u8>,
}

impl CanonicalDescriptor {
    fn field(&mut self, name: &str, value: &str) {
        self.bytes
            .extend_from_slice(&(name.len() as u64).to_be_bytes());
        self.bytes.extend_from_slice(name.as_bytes());
        self.bytes
            .extend_from_slice(&(value.len() as u64).to_be_bytes());
        self.bytes.extend_from_slice(value.as_bytes());
    }

    fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

fn canonical_type_text(
    capability: &str,
    owner: &str,
    source: &str,
    local_names: &BTreeSet<String>,
    imported_names: &BTreeSet<String>,
    dependencies: &DependencyTypes,
) -> String {
    crate::types::parse_type_str_strict(source)
        .map(|ty| {
            canonical_type(
                capability,
                owner,
                &ty,
                local_names,
                imported_names,
                dependencies,
            )
        })
        .unwrap_or_else(|_| source.split_whitespace().collect())
}

/// `capability` owns the contract; `owner` is the module a bare name in `ty`
/// belongs to, which differs from `capability` only inside a dependency
/// layout. A dependency type is printed under its own module's path, so a
/// capability-local `Tx` and a dependency's `Ledger.Tx` stay two identities
/// even when the bare name is the same.
fn canonical_type(
    capability: &str,
    owner: &str,
    ty: &Type,
    local_names: &BTreeSet<String>,
    imported_names: &BTreeSet<String>,
    dependencies: &DependencyTypes,
) -> String {
    let recur = |inner: &Type| {
        canonical_type(
            capability,
            owner,
            inner,
            local_names,
            imported_names,
            dependencies,
        )
    };
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Named { name, .. } => {
            // A module may write an imported type bare, so the name a
            // dependency layout wrote resolves through that module's own
            // `depends`. An imported identity is the only case this decides;
            // with nothing imported the fall-through below is what runs, and
            // it is what has always run.
            let canonical = dependencies
                .resolve(owner, name)
                .unwrap_or_else(|| format!("{owner}.{name}"));
            if imported_names.contains(&canonical) {
                return descriptor_path(&canonical);
            }
            let local = name.rsplit('.').next().unwrap_or(name);
            if local_names.contains(local) {
                format!("{}::{local}", descriptor_path(capability))
            } else if matches!(name.as_str(), "Bytes" | "Bytes.Bytes") {
                "Aver::Bytes".to_string()
            } else {
                descriptor_path(name)
            }
        }
        Type::Result(ok, err) => format!("Result<{},{}>", recur(ok), recur(err)),
        Type::Option(inner) => format!("Option<{}>", recur(inner)),
        Type::List(inner) => format!("List<{}>", recur(inner)),
        Type::Vector(inner) => format!("Vector<{}>", recur(inner)),
        Type::Map(key, value) => format!("Map<{},{}>", recur(key), recur(value)),
        Type::Tuple(items) => format!(
            "Tuple<{}>",
            items.iter().map(recur).collect::<Vec<_>>().join(",")
        ),
        Type::Fn(params, ret, effects) => format!(
            "Fn({})->{}![{}]",
            params.iter().map(recur).collect::<Vec<_>>().join(","),
            recur(ret),
            effects.join(",")
        ),
        Type::Var(name) => format!("Var<{name}>"),
        Type::Invalid => "Invalid".to_string(),
    }
}

fn descriptor_path(name: &str) -> String {
    name.replace('.', "::")
}

pub(super) fn render_model_descriptor(
    scope: &str,
    contract_hash: &str,
    operations: &[CapabilityOperation],
    items: &[TopLevel],
    errors: &mut Vec<CapabilityError>,
) -> Vec<u8> {
    let mut descriptor = CanonicalDescriptor::default();
    descriptor.field("avercap", "1");
    descriptor.field("kind", "model");
    descriptor.field("contract", contract_hash);
    let mut operations = operations.to_vec();
    operations.sort_by(|a, b| a.name.cmp(&b.name));
    let functions: BTreeMap<&str, &FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some((fd.name.as_str(), fd)),
            _ => None,
        })
        .collect();
    let mut profile_roots = BTreeSet::new();
    for op in operations {
        let mut hostile = op.hostile.clone();
        hostile.sort();
        let mut unmodelled = op.unmodelled.clone();
        unmodelled.sort();
        let mut model = format!(
            "{} oracle={} replay={} hostile=[{}]",
            op.name,
            op.oracle.map_or("none", OracleDimension::as_str),
            op.replay.map_or("none", ReplaySemantics::as_str),
            hostile.join(","),
        );
        if !unmodelled.is_empty() {
            model.push_str(&format!(" unmodelled=[{}]", unmodelled.join(",")));
        }
        descriptor.field("model", &model);
        profile_roots.extend(hostile);
    }

    let closure = function_closure(scope, &profile_roots, &functions, errors);
    for name in closure {
        let Some(fd) = functions.get(name.as_str()) else {
            continue;
        };
        let mut canonical = (*fd).clone();
        canonical.desc = None;
        canonical.resolution = None;
        match crate::ast::unparse::unparse(&[TopLevel::FnDef(canonical)]) {
            Ok(source) => descriptor.field("profile", &format!("{scope}.{name}\n{source}")),
            Err(error) => errors.push(CapabilityError::at(
                fd.line,
                format!("cannot canonicalise hostile profile '{name}': {error}"),
            )),
        }
    }
    descriptor.into_bytes()
}

pub(super) fn function_closure(
    scope: &str,
    roots: &BTreeSet<String>,
    functions: &BTreeMap<&str, &FnDef>,
    errors: &mut Vec<CapabilityError>,
) -> BTreeSet<String> {
    let mut pending = roots.clone();
    let mut seen = BTreeSet::new();
    while let Some(name) = pending.pop_first() {
        if !seen.insert(name.clone()) {
            continue;
        }
        let Some(fd) = functions.get(name.as_str()) else {
            errors.push(CapabilityError::at(
                1,
                format!(
                    "hostile profile '{name}' does not name a function in this capability module"
                ),
            ));
            continue;
        };
        for stmt in fd.body.stmts() {
            let expr = match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
            };
            crate::codegen::expr_walk::walk(expr, &mut |node| match &node.node {
                Expr::FnCall(callee, _) => {
                    if let Some(callee) = crate::ir::expr_to_dotted_name(&callee.node) {
                        let local = callee
                            .strip_prefix(scope)
                            .and_then(|name| name.strip_prefix('.'))
                            .unwrap_or(&callee);
                        if functions.contains_key(local) {
                            pending.insert(local.to_string());
                        }
                    }
                }
                Expr::TailCall(tail) => {
                    let local = tail
                        .target
                        .strip_prefix(scope)
                        .and_then(|name| name.strip_prefix('.'))
                        .unwrap_or(&tail.target);
                    if functions.contains_key(local) {
                        pending.insert(local.to_string());
                    }
                }
                // A profile may alias or pass a local helper before calling
                // it (`f = fail; f()`).  The resolved semantic closure must
                // bind that helper body too; looking only at syntactic call
                // callees would let an author hollow out the aliased helper
                // without moving model_hash.
                Expr::Ident(name) | Expr::Resolved { name, .. }
                    if functions.contains_key(name.as_str()) =>
                {
                    pending.insert(name.clone());
                }
                Expr::Attr(_, _) => {
                    if let Some(reference) = crate::ir::expr_to_dotted_name(&node.node) {
                        let local = reference
                            .strip_prefix(scope)
                            .and_then(|name| name.strip_prefix('.'))
                            .unwrap_or(&reference);
                        if functions.contains_key(local) {
                            pending.insert(local.to_string());
                        }
                    }
                }
                _ => {}
            });
        }
    }
    seen
}

pub(super) fn hash_descriptor(descriptor: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(descriptor);
    format!("sha256:{:x}", hasher.finalize())
}
