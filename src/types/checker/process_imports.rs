//! Retain a yielding library's source call shape until the importing process
//! is lowered. These signatures are never runtime functions.

use super::*;

impl TypeChecker {
    pub(super) fn integrate_process_protocols(
        &mut self,
        modules: &[crate::source::LoadedModule],
        visible: &[String],
    ) {
        for loaded in modules.iter().filter(|m| visible.contains(&m.dep_name)) {
            let Some(module) = Self::module_decl(&loaded.items) else {
                continue;
            };
            for export in &module.yield_protocols {
                let owner = &loaded.dep_name;
                let qualify = |name: &str| crate::visibility::qualified_name(owner, name);
                let resolve = |annotation: &str| {
                    self.canonicalize_named_in_module(
                        parse_type_str_strict(annotation).unwrap_or(Type::Invalid),
                        owner,
                    )
                };
                let annotation = |ty: &Type| {
                    ty.display_with(&|id| Some(self.symbol_table.type_entry(id).key.canonical()))
                };
                let mut protocol = export.protocol.clone();
                let params: Vec<Type> = protocol.params.iter().map(|(_, ty)| resolve(ty)).collect();
                let ret = resolve(&protocol.return_type);
                for ((_, written_type), ty) in protocol.params.iter_mut().zip(&params) {
                    *written_type = annotation(ty);
                }
                protocol.return_type = annotation(&ret);
                protocol.fn_name = qualify(&protocol.fn_name);
                protocol.start = qualify(&protocol.start);
                protocol.request = qualify(&protocol.request);
                protocol.outcome = qualify(&protocol.outcome);
                for nest in &mut protocol.nests {
                    // A nested imported helper already names its own module.
                    if !nest.callee.contains('.') {
                        nest.callee = qualify(&nest.callee);
                    }
                    nest.router = qualify(&nest.router);
                }
                if let Some(trace) = &mut protocol.trace {
                    for dependency in &mut trace.dependencies {
                        *dependency = qualify(dependency);
                    }
                    trace.input = qualify(&trace.input);
                    trace.query = qualify(&trace.query);
                    trace.event = qualify(&trace.event);
                    trace.result = qualify(&trace.result);
                    trace.source = qualify(&trace.source);
                    trace.drive = qualify(&trace.drive);
                    trace.protocol_from = qualify(&trace.protocol_from);
                    for segment in &mut trace.segments {
                        segment.function = qualify(&segment.function);
                        segment.observer = qualify(&segment.observer);
                        segment.result = qualify(&segment.result);
                        for (_, ty) in &mut segment.params {
                            *ty = annotation(&resolve(ty));
                        }
                    }
                    trace.cursor = trace.cursor.as_ref().map(|name| qualify(name));
                    trace.correspondence = trace.correspondence.as_ref().map(|name| qualify(name));
                    for operation in &mut trace.operations {
                        for ty in &mut operation.arg_types {
                            *ty = annotation(&resolve(ty));
                        }
                        if let Some(ty) = &mut operation.answer_type {
                            *ty = annotation(&resolve(ty));
                        }
                    }
                }
                for kind in &mut protocol.kinds {
                    kind.state = qualify(&kind.state);
                    kind.answer_fn = qualify(&kind.answer_fn);
                    for ty in &mut kind.arg_types {
                        *ty = annotation(&resolve(ty));
                    }
                    // A stop's live variables are written in the owner's names;
                    // an importer reads the layout to sample the stop.
                    for (_, live) in &mut kind.variants {
                        for ty in live.iter_mut() {
                            *ty = annotation(&resolve(ty));
                        }
                    }
                    if let Some(ty) = &mut kind.answer_type {
                        *ty = annotation(&resolve(ty));
                    }
                }
                self.extra_sigs.insert(
                    protocol.fn_name.clone(),
                    FnSig {
                        params,
                        ret,
                        effects: export.effects.clone(),
                    },
                );
                self.imported_processes
                    .insert(protocol.fn_name.clone(), protocol);
            }
        }
    }
}
