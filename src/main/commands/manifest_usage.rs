//! A manifest and the generated loop are callers too. Count their function
//! references and the boundary types of those functions when checking exports.

use super::*;

pub(super) fn collect(
    units: &[&ReportUnit],
    module_root: &str,
    modules: &HashMap<String, ExposedModuleInfo>,
) -> HashMap<String, HashSet<String>> {
    let mut used = HashMap::new();
    let config = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .ok()
        .flatten();
    let targets: Vec<ImportTarget> = modules
        .values()
        .map(|info| ImportTarget {
            dep_path_parts: info.module_name.split('.').map(str::to_string).collect(),
            info: info.clone(),
        })
        .collect();
    let mut names = HashSet::new();
    if let Some(manifest) = config
        .as_ref()
        .and_then(|config| config.provider_manifest.as_ref())
    {
        for binding in &manifest.work_bindings {
            names.insert(binding.function.clone());
            names.extend([
                format!("{}.begin", binding.capability),
                format!("{}.take", binding.capability),
            ]);
        }
    }
    // An answer module says what it answers in its own header, and the
    // generated loop calls its `fresh`, one function per operation, and the
    // function a process declaration seats a family by.
    for (_, _, items) in units {
        let Some(module) = aver::visibility::module_decl(items) else {
            continue;
        };
        for seating in &module.seatings {
            names.insert(seating.by.clone());
        }
        if module.answers.is_empty() {
            continue;
        }
        names.insert(format!("{}.fresh", module.name));
        for capability in &module.answers {
            for (_, _, capability_items) in units {
                if aver::visibility::module_decl(capability_items)
                    .is_none_or(|declared| &declared.name != capability)
                {
                    continue;
                }
                for item in capability_items.iter() {
                    if let TopLevel::Capability(CapabilityItem::Operation(operation)) = item {
                        names.insert(format!("{}.{}", module.name, operation.name));
                        names.insert(format!("{capability}.{}", operation.name));
                    }
                }
            }
        }
    }
    let no_aliases = HashMap::new();
    for name in &names {
        mark_path_use(
            &name.split('.').map(str::to_string).collect::<Vec<_>>(),
            &targets,
            &no_aliases,
            &mut used,
        );
    }
    // A bound function makes the types in its signature used; those types
    // may themselves contain exported records or sums. Iterate to a fixed
    // point so nested boundary types are accounted for as well.
    loop {
        let before: usize = used.values().map(HashSet::len).sum();
        for (path, _, items) in units {
            let Some(module) = aver::visibility::module_decl(items) else {
                continue;
            };
            let path = canonical_path_key(path);
            for item in items {
                let mut annotations = Vec::new();
                match item {
                    TopLevel::FnDef(function)
                        if names.contains(&format!("{}.{}", module.name, function.name)) =>
                    {
                        annotations.extend(function.params.iter().map(|(_, ty)| ty.as_str()));
                        annotations.push(&function.return_type);
                    }
                    TopLevel::Capability(CapabilityItem::Operation(operation))
                        if names.contains(&format!("{}.{}", module.name, operation.name)) =>
                    {
                        annotations.extend(operation.params.iter().map(|(_, ty)| ty.as_str()));
                        annotations.push(&operation.return_type);
                    }
                    TopLevel::TypeDef(TypeDef::Product { name, fields, .. })
                        if used.get(&path).is_some_and(|used| used.contains(name)) =>
                    {
                        annotations.extend(fields.iter().map(|(_, ty)| ty.as_str()));
                    }
                    TopLevel::TypeDef(TypeDef::Sum { name, variants, .. })
                        if used.get(&path).is_some_and(|used| used.contains(name)) =>
                    {
                        annotations.extend(
                            variants
                                .iter()
                                .flat_map(|variant| variant.fields.iter().map(String::as_str)),
                        );
                    }
                    _ => {}
                }
                for annotation in annotations {
                    let ty = aver::capability::canonicalize_type_names(
                        parse_type_str(annotation),
                        &module.name,
                    );
                    mark_type_uses(&ty, &targets, &no_aliases, &mut used);
                }
            }
        }
        if used.values().map(HashSet::len).sum::<usize>() == before {
            break;
        }
    }
    used
}
