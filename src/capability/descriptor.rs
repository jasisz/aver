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

pub(super) fn render_contract_descriptor(
    scope: &str,
    module_name: &str,
    operations: &[CapabilityOperation],
    opaque: &[String],
    reachable_types: &[&TypeDef],
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
            opaque.contains(name) || reachable_types.iter().any(|td| type_def_name(td) == *name)
        })
        .cloned()
        .collect();

    let mut reachable_opaque = opaque
        .iter()
        .filter(|name| reachable_names.contains(name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    reachable_opaque.sort();
    for name in reachable_opaque {
        descriptor.field(
            "type",
            &format!("{}::{name} = opaque", descriptor_path(scope)),
        );
    }

    let mut types = reachable_types.to_vec();
    types.sort_by_key(|td| type_def_name(td));
    for td in types {
        match td {
            TypeDef::Sum { name, variants, .. } => {
                let mut variants = variants.clone();
                variants.sort_by(|left, right| left.name.cmp(&right.name));
                let variants = variants
                    .iter()
                    .map(|variant| {
                        let fields = variant
                            .fields
                            .iter()
                            .map(|field| canonical_type_text(scope, field, &local_names))
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
                descriptor.field(
                    "type",
                    &format!("{}::{name} = sum{{{variants}}}", descriptor_path(scope)),
                );
            }
            TypeDef::Product { name, fields, .. } => {
                let mut fields = fields.clone();
                fields.sort_by(|left, right| left.0.cmp(&right.0));
                let fields = fields
                    .iter()
                    .map(|(field, ty)| {
                        format!("{field}:{}", canonical_type_text(scope, ty, &local_names))
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                descriptor.field(
                    "type",
                    &format!("{}::{name} = record{{{fields}}}", descriptor_path(scope)),
                );
            }
        }
    }
    let mut operations = operations.to_vec();
    operations.sort_by(|a, b| a.name.cmp(&b.name));
    for op in operations {
        let params = op
            .params
            .iter()
            .map(|(_, ty)| canonical_type(scope, ty, &local_names))
            .collect::<Vec<_>>()
            .join(",");
        let ret = canonical_type(scope, &op.return_type, &local_names);
        descriptor.field("op", &format!("{}({params}) -> {ret}", op.name));
    }
    descriptor.into_bytes()
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

fn canonical_type_text(scope: &str, source: &str, local_names: &BTreeSet<String>) -> String {
    crate::types::parse_type_str_strict(source)
        .map(|ty| canonical_type(scope, &ty, local_names))
        .unwrap_or_else(|_| source.split_whitespace().collect())
}

fn canonical_type(scope: &str, ty: &Type, local_names: &BTreeSet<String>) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Named { name, .. } => {
            let local = name.rsplit('.').next().unwrap_or(name);
            if local_names.contains(local) {
                format!("{}::{local}", descriptor_path(scope))
            } else {
                descriptor_path(name)
            }
        }
        Type::Result(ok, err) => format!(
            "Result<{},{}>",
            canonical_type(scope, ok, local_names),
            canonical_type(scope, err, local_names)
        ),
        Type::Option(inner) => {
            format!("Option<{}>", canonical_type(scope, inner, local_names))
        }
        Type::List(inner) => format!("List<{}>", canonical_type(scope, inner, local_names)),
        Type::Vector(inner) => {
            format!("Vector<{}>", canonical_type(scope, inner, local_names))
        }
        Type::Map(key, value) => format!(
            "Map<{},{}>",
            canonical_type(scope, key, local_names),
            canonical_type(scope, value, local_names)
        ),
        Type::Tuple(items) => format!(
            "Tuple<{}>",
            items
                .iter()
                .map(|item| canonical_type(scope, item, local_names))
                .collect::<Vec<_>>()
                .join(",")
        ),
        Type::Fn(params, ret, effects) => format!(
            "Fn({})->{}![{}]",
            params
                .iter()
                .map(|param| canonical_type(scope, param, local_names))
                .collect::<Vec<_>>()
                .join(","),
            canonical_type(scope, ret, local_names),
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

fn function_closure(
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
