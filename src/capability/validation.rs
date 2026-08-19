use super::*;

pub(super) fn validate_hostile_profiles(
    operations: &[CapabilityOperation],
    items: &[TopLevel],
    errors: &mut Vec<CapabilityError>,
) {
    let functions: BTreeMap<&str, &FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some((fd.name.as_str(), fd)),
            _ => None,
        })
        .collect();

    for operation in operations {
        if matches!(operation.oracle, Some(OracleDimension::Output))
            && !operation.hostile.is_empty()
        {
            errors.push(CapabilityError::at(
                operation.line,
                format!(
                    "output-only operation '{}' cannot declare hostile return profiles",
                    operation.canonical_name
                ),
            ));
        }
        for profile in &operation.hostile {
            let Some(fd) = functions.get(profile.as_str()) else {
                // The model-closure builder emits the single missing-name
                // diagnostic with the same source identity.
                continue;
            };
            if !fd.effects.is_empty() {
                errors.push(CapabilityError::at(
                    fd.line,
                    format!(
                        "hostile profile '{}.{}' must be pure",
                        operation.module, profile
                    ),
                ));
            }
            let expected_params = operation
                .oracle_params()
                .into_iter()
                .map(|ty| canonicalize_type_names(ty, &operation.module))
                .collect::<Vec<_>>();
            let actual_params: Option<Vec<Type>> = fd
                .params
                .iter()
                .map(|(_, source)| {
                    crate::types::parse_type_str_strict(source)
                        .ok()
                        .map(|ty| canonicalize_type_names(ty, &operation.module))
                })
                .collect();
            let expected_return =
                canonicalize_type_names(operation.return_type.clone(), &operation.module);
            let actual_return = crate::types::parse_type_str_strict(&fd.return_type)
                .ok()
                .map(|ty| canonicalize_type_names(ty, &operation.module));
            if actual_params.as_ref() != Some(&expected_params)
                || actual_return.as_ref() != Some(&expected_return)
            {
                let expected = Type::Fn(expected_params, Box::new(expected_return), vec![]);
                errors.push(CapabilityError::at(
                    fd.line,
                    format!(
                        "hostile profile '{}.{}' must have oracle signature {}",
                        operation.module,
                        profile,
                        expected.display()
                    ),
                ));
            }
        }
    }
}

fn canonicalize_type_names(ty: Type, scope: &str) -> Type {
    match ty {
        Type::Named { id, name } => {
            if name.contains('.') || name == crate::types::branch_path::TYPE_NAME {
                Type::Named { id, name }
            } else {
                Type::Named {
                    id,
                    name: format!("{scope}.{name}"),
                }
            }
        }
        Type::Result(ok, err) => Type::Result(
            Box::new(canonicalize_type_names(*ok, scope)),
            Box::new(canonicalize_type_names(*err, scope)),
        ),
        Type::Option(inner) => Type::Option(Box::new(canonicalize_type_names(*inner, scope))),
        Type::List(inner) => Type::List(Box::new(canonicalize_type_names(*inner, scope))),
        Type::Vector(inner) => Type::Vector(Box::new(canonicalize_type_names(*inner, scope))),
        Type::Map(key, value) => Type::Map(
            Box::new(canonicalize_type_names(*key, scope)),
            Box::new(canonicalize_type_names(*value, scope)),
        ),
        Type::Tuple(items) => Type::Tuple(
            items
                .into_iter()
                .map(|item| canonicalize_type_names(item, scope))
                .collect(),
        ),
        Type::Fn(params, ret, effects) => Type::Fn(
            params
                .into_iter()
                .map(|param| canonicalize_type_names(param, scope))
                .collect(),
            Box::new(canonicalize_type_names(*ret, scope)),
            effects,
        ),
        other => other,
    }
}
pub(super) fn validate_operation_boundaries(
    scope: &str,
    operations: &mut [CapabilityOperation],
    opaque: &[String],
    resource_tainted: &BTreeSet<String>,
    errors: &mut Vec<CapabilityError>,
) {
    let opaque: BTreeSet<String> = opaque.iter().cloned().collect();
    let operation_names: BTreeSet<String> = operations.iter().map(|op| op.name.clone()).collect();

    fn minted_resource(
        scope: &str,
        ty: &Type,
        opaque: &BTreeSet<String>,
        tainted: &BTreeSet<String>,
    ) -> Result<Option<String>, String> {
        match ty {
            Type::Result(ok, err) => {
                if type_mentions_any_named(err, tainted) {
                    return Err(
                        "a capability resource may appear only in the success payload, never in Result.Err"
                            .to_string(),
                    );
                }
                minted_resource(scope, ok, opaque, tainted)
            }
            Type::Option(inner) => minted_resource(scope, inner, opaque, tainted),
            Type::Named { name, .. }
                if opaque.contains(name.rsplit('.').next().unwrap_or(name)) =>
            {
                let local = name.rsplit('.').next().unwrap_or(name);
                Ok(Some(format!("{scope}.{local}")))
            }
            Type::List(_)
            | Type::Vector(_)
            | Type::Map(_, _)
            | Type::Tuple(_)
            | Type::Named { .. }
                if type_mentions_any_named(ty, tainted) =>
            {
                Err(
                    "a capability resource may occur at most once and only directly through transparent Result/Option success wrappers"
                        .to_string(),
                )
            }
            _ => Ok(None),
        }
    }

    for operation in operations {
        match minted_resource(scope, &operation.return_type, &opaque, resource_tainted) {
            Ok(resource) => operation.minted_resource = resource,
            Err(reason) => errors.push(CapabilityError::at(
                operation.line,
                format!(
                    "operation '{}' is an invalid resource source: {reason}",
                    operation.canonical_name
                ),
            )),
        }

        let consumes_resource = operation
            .params
            .iter()
            .any(|(_, ty)| type_mentions_any_named(ty, resource_tainted));
        if consumes_resource
            && operation.is_effectful()
            && operation.replay != Some(ReplaySemantics::Recorded)
        {
            errors.push(CapabilityError::at(
                operation.line,
                format!(
                    "operation '{}' consumes a capability resource, so `replay = recorded` is required; a replayed token has no live provider counterpart",
                    operation.canonical_name
                ),
            ));
        }

        for related in &operation.unmodelled {
            if !operation_names.contains(related) {
                errors.push(CapabilityError::at(
                    operation.line,
                    format!(
                        "operation '{}' discloses unknown unmodelled operation '{}.{}'",
                        operation.canonical_name, scope, related
                    ),
                ));
            }
        }
    }
}

pub(super) fn type_def_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

pub(super) fn collect_named_types(ty: &Type, out: &mut BTreeSet<String>) {
    match ty {
        Type::Named { name, .. } => {
            out.insert(name.rsplit('.').next().unwrap_or(name).to_string());
        }
        Type::Result(ok, err) | Type::Map(ok, err) => {
            collect_named_types(ok, out);
            collect_named_types(err, out);
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            collect_named_types(inner, out)
        }
        Type::Tuple(items) | Type::Fn(items, _, _) => {
            for item in items {
                collect_named_types(item, out);
            }
            if let Type::Fn(_, ret, _) = ty {
                collect_named_types(ret, out);
            }
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => {}
    }
}

fn type_mentions_any_named(ty: &Type, names: &BTreeSet<String>) -> bool {
    match ty {
        Type::Named { name, .. } => names.contains(name.rsplit('.').next().unwrap_or(name)),
        Type::Result(left, right) | Type::Map(left, right) => {
            type_mentions_any_named(left, names) || type_mentions_any_named(right, names)
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_mentions_any_named(inner, names)
        }
        Type::Tuple(items) | Type::Fn(items, _, _) => {
            items
                .iter()
                .any(|item| type_mentions_any_named(item, names))
                || matches!(ty, Type::Fn(_, ret, _) if type_mentions_any_named(ret, names))
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => false,
    }
}

pub(super) fn resource_tainted_type_names(
    opaque: &[String],
    defs: &BTreeMap<String, &TypeDef>,
) -> BTreeSet<String> {
    let mut tainted: BTreeSet<String> = opaque.iter().cloned().collect();
    loop {
        let mut changed = false;
        for (name, td) in defs {
            if tainted.contains(name) {
                continue;
            }
            let fields: Vec<&str> = match td {
                TypeDef::Sum { variants, .. } => variants
                    .iter()
                    .flat_map(|variant| variant.fields.iter().map(String::as_str))
                    .collect(),
                TypeDef::Product { fields, .. } => {
                    fields.iter().map(|(_, ty)| ty.as_str()).collect()
                }
            };
            let contains_resource = fields.iter().any(|source| {
                crate::types::parse_type_str_strict(source)
                    .is_ok_and(|ty| type_mentions_any_named(&ty, &tainted))
            });
            if contains_resource {
                tainted.insert(name.clone());
                changed = true;
            }
        }
        if !changed {
            return tainted;
        }
    }
}

pub(super) fn validate_resource_map_keys(
    operations: &[CapabilityOperation],
    tainted: &BTreeSet<String>,
    errors: &mut Vec<CapabilityError>,
) {
    fn visit(
        ty: &Type,
        operation: &CapabilityOperation,
        tainted: &BTreeSet<String>,
        errors: &mut Vec<CapabilityError>,
    ) {
        match ty {
            Type::Map(key, value) => {
                if type_mentions_any_named(key, tainted) {
                    errors.push(CapabilityError::at(
                        operation.line,
                        format!(
                            "operation '{}' uses capability resource type '{}' as a Map key; provider token identity has no equality or hash semantics",
                            operation.canonical_name,
                            key.display()
                        ),
                    ));
                } else if !crate::types::map_key_has_ordering(key) {
                    errors.push(CapabilityError::at(
                        operation.line,
                        format!(
                            "operation '{}': {}",
                            operation.canonical_name,
                            crate::types::unordered_map_key_message(key)
                        ),
                    ));
                }
                visit(key, operation, tainted, errors);
                visit(value, operation, tainted, errors);
            }
            Type::Result(left, right) => {
                visit(left, operation, tainted, errors);
                visit(right, operation, tainted, errors);
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                visit(inner, operation, tainted, errors)
            }
            Type::Tuple(items) | Type::Fn(items, _, _) => {
                for item in items {
                    visit(item, operation, tainted, errors);
                }
                if let Type::Fn(_, ret, _) = ty {
                    visit(ret, operation, tainted, errors);
                }
            }
            Type::Named { .. }
            | Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
        }
    }

    for operation in operations {
        for (_, ty) in &operation.params {
            visit(ty, operation, tainted, errors);
        }
        visit(&operation.return_type, operation, tainted, errors);
    }
}

/// The v1 descriptor builder owns only the capability module's declarations.
/// Admitting a type from another module would print its name in an operation
/// row without hashing that type's layout, so a dependency could mutate the
/// provider ABI while contract_hash stayed fixed. Bare names are local only
/// when a represented or opaque declaration proves ownership; an imported bare
/// alias must not be silently qualified into the capability's own scope. Fail
/// closed until descriptors can bind cross-module identities transitively.
pub(super) fn validate_boundary_type_ownership(
    scope: &str,
    operations: &[CapabilityOperation],
    locally_declared: &BTreeSet<String>,
    errors: &mut Vec<CapabilityError>,
) {
    fn visit(
        scope: &str,
        operation: &CapabilityOperation,
        position: &str,
        ty: &Type,
        locally_declared: &BTreeSet<String>,
        seen: &mut BTreeSet<(String, String)>,
        errors: &mut Vec<CapabilityError>,
    ) {
        match ty {
            Type::Named { name, .. } => {
                let belongs_to_capability = match name.rsplit_once('.') {
                    Some((owner, _)) => owner == scope,
                    None => locally_declared.contains(name),
                };
                if !belongs_to_capability && seen.insert((position.to_string(), name.to_string())) {
                    errors.push(CapabilityError::at(
                        operation.line,
                        format!(
                            "operation '{}' {} uses cross-module boundary type '{}'; capability contract v1 requires named boundary types to be declared in the capability module so their layout is bound by contract_hash",
                            operation.canonical_name, position, name
                        ),
                    ));
                }
            }
            Type::Result(left, right) | Type::Map(left, right) => {
                visit(
                    scope,
                    operation,
                    position,
                    left,
                    locally_declared,
                    seen,
                    errors,
                );
                visit(
                    scope,
                    operation,
                    position,
                    right,
                    locally_declared,
                    seen,
                    errors,
                );
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => visit(
                scope,
                operation,
                position,
                inner,
                locally_declared,
                seen,
                errors,
            ),
            Type::Tuple(items) | Type::Fn(items, _, _) => {
                for item in items {
                    visit(
                        scope,
                        operation,
                        position,
                        item,
                        locally_declared,
                        seen,
                        errors,
                    );
                }
                if let Type::Fn(_, ret, _) = ty {
                    visit(
                        scope,
                        operation,
                        position,
                        ret,
                        locally_declared,
                        seen,
                        errors,
                    );
                }
            }
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
        }
    }

    for operation in operations {
        let mut seen = BTreeSet::new();
        for (index, (_, ty)) in operation.params.iter().enumerate() {
            visit(
                scope,
                operation,
                &format!("parameter {index}"),
                ty,
                locally_declared,
                &mut seen,
                errors,
            );
        }
        visit(
            scope,
            operation,
            "result",
            &operation.return_type,
            locally_declared,
            &mut seen,
            errors,
        );
    }
}
