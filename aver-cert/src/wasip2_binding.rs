//! Binds the manifest-declared wasip2 core module to the component that runs it.
//!
//! The manifest declares the delivered component as `prefix ++ core ++ suffix`
//! by length, and the Lean wall checks the certificate against `core`. Lengths
//! alone do not say that `core` is the code the component runs: a component can
//! carry a second module (in a custom section, or as an extra module that is
//! never instantiated or never exported) and point the declaration at it. This
//! gate confirms the declaration; it never searches for the core. It accepts
//! only when:
//!
//! 1. the declared range is exactly the payload of one top-level core module
//!    section of the component (its position in the core-module index space is
//!    the declared module);
//! 2. the declared module is instantiated at least once;
//! 3. every function the component exports, directly or as a field of an
//!    exported instance, is a `canon lift` of a function exported by an
//!    instance of the declared module, and the lift's memory, realloc,
//!    post-return and callback options also come from such an instance;
//! 4. no instance of the declared module, and no item exported by one, is
//!    handed to another core instantiation, so other core modules (the
//!    `wit-component` shim and fixup) cannot reach its state;
//! 5. a nested component is only a re-export shim: it imports functions and
//!    types and exports those same imports, with no code, instances or
//!    canonical functions of its own.
//!
//! Anything outside these shapes is refused. The walk runs after
//! `wasmparser::Validator` has accepted the component, so every index it reads
//! is in bounds of a well-typed index space.

use wasmparser::{
    CanonicalFunction, CanonicalOption, ComponentAlias, ComponentExternalKind, ComponentInstance,
    ComponentTypeRef, ExternalKind, Instance, Parser, Payload,
};

/// Where a core instance comes from.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CoreInstance {
    /// `instantiate <declared module>`.
    Declared,
    /// Any other module's instance, or a `from_exports` bundle.
    Other,
}

/// What a nested component is. Only re-export shims are understood.
enum NestedComponent {
    /// Pairs of (export name, import name) for each re-exported function.
    Shim(Vec<(String, String)>),
    /// An imported or aliased component, whose behavior is unknown.
    Opaque,
}

/// A component instance. `Built` instances list their function fields with
/// whether each one is lifted from the declared module.
enum ComponentInstanceOrigin {
    Built {
        funcs: Vec<(String, bool)>,
        other_fields: bool,
    },
    Opaque,
}

impl ComponentInstanceOrigin {
    fn is_bound(&self) -> bool {
        match self {
            Self::Built {
                funcs,
                other_fields,
            } => !*other_fields && funcs.iter().all(|(_, bound)| *bound),
            Self::Opaque => false,
        }
    }
}

const CORE_SPACES: usize = 5;

fn core_space(kind: ExternalKind) -> usize {
    match kind {
        ExternalKind::Func | ExternalKind::FuncExact => 0,
        ExternalKind::Table => 1,
        ExternalKind::Memory => 2,
        ExternalKind::Global => 3,
        ExternalKind::Tag => 4,
    }
}

#[derive(Default)]
struct TopLevel {
    /// Core module index space: `true` at the declared module.
    core_modules: Vec<bool>,
    core_instances: Vec<CoreInstance>,
    /// Core item index spaces (func, table, memory, global, tag): `true` when
    /// the item is an export of a declared-module instance.
    core_items: [Vec<bool>; CORE_SPACES],
    /// Component function index space: `true` when lifted from the declared
    /// module.
    funcs: Vec<bool>,
    instances: Vec<ComponentInstanceOrigin>,
    components: Vec<NestedComponent>,
    declared_instantiations: usize,
    /// The first component export not lifted from the declared module. It is
    /// reported after the checks on the declaration itself.
    unbound_export: Option<String>,
}

fn at<'a, T>(items: &'a [T], index: u32, space: &str) -> Result<&'a T, String> {
    usize::try_from(index)
        .ok()
        .and_then(|index| items.get(index))
        .ok_or_else(|| format!("wasip2 component refers to {space} {index} out of range"))
}

/// Confirms that `core_range` (the manifest-declared embedded core module) is
/// the top-level core module every component export is lifted from.
pub(crate) fn confirm_declared_core_binding(
    component_bytes: &[u8],
    core_range: std::ops::Range<usize>,
) -> Result<(), String> {
    let mut top = TopLevel::default();
    let mut declared_seen = false;
    let mut payloads = Parser::new(0).parse_all(component_bytes);
    let mut first = true;
    while let Some(payload) = payloads.next() {
        let payload = payload.map_err(|error| format!("wasip2 component parse error: {error}"))?;
        if first {
            first = false;
            if !matches!(payload, Payload::Version { .. }) {
                return Err("wasip2 component does not start with a version header".to_string());
            }
            continue;
        }
        match payload {
            Payload::CustomSection(_)
            | Payload::ComponentTypeSection(_)
            | Payload::CoreTypeSection(_) => {}
            Payload::ModuleSection {
                unchecked_range, ..
            } => {
                let is_declared = unchecked_range == core_range;
                if is_declared {
                    declared_seen = true;
                }
                top.core_modules.push(is_declared);
                skip_nested_module(&mut payloads)?;
            }
            Payload::ComponentSection { .. } => {
                let shim = read_reexport_shim(&mut payloads)?;
                top.components.push(NestedComponent::Shim(shim));
            }
            Payload::InstanceSection(reader) => {
                for instance in reader {
                    let instance = instance.map_err(|e| e.to_string())?;
                    top.add_core_instance(instance)?;
                }
            }
            Payload::ComponentAliasSection(reader) => {
                for alias in reader {
                    let alias = alias.map_err(|e| e.to_string())?;
                    top.add_alias(alias)?;
                }
            }
            Payload::ComponentCanonicalSection(reader) => {
                for function in reader {
                    let function = function.map_err(|e| e.to_string())?;
                    top.add_canonical(function)?;
                }
            }
            Payload::ComponentImportSection(reader) => {
                for import in reader {
                    let import = import.map_err(|e| e.to_string())?;
                    top.add_import(import.ty)?;
                }
            }
            Payload::ComponentInstanceSection(reader) => {
                for instance in reader {
                    let instance = instance.map_err(|e| e.to_string())?;
                    top.add_component_instance(instance)?;
                }
            }
            Payload::ComponentExportSection(reader) => {
                for export in reader {
                    let export = export.map_err(|e| e.to_string())?;
                    top.add_export(export.name.0, export.kind, export.index)?;
                }
            }
            Payload::ComponentStartSection { .. } => {
                return Err(
                    "wasip2 component has a start function, which this checker does not admit"
                        .to_string(),
                );
            }
            Payload::End(_) => {
                if payloads.next().is_some() {
                    return Err("wasip2 component has bytes after its end".to_string());
                }
                break;
            }
            other => {
                return Err(format!(
                    "wasip2 component has an unexpected top-level section ({})",
                    section_name(&other)
                ));
            }
        }
    }
    if !declared_seen {
        return Err(format!(
            "declared embedded core module [{}, {}) is not a top-level core module section of the component",
            core_range.start, core_range.end
        ));
    }
    if top.declared_instantiations == 0 {
        return Err(
            "declared embedded core module is never instantiated by the component".to_string(),
        );
    }
    match top.unbound_export {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

impl TopLevel {
    fn add_core_instance(&mut self, instance: Instance<'_>) -> Result<(), String> {
        let origin = match instance {
            Instance::Instantiate { module_index, args } => {
                for arg in args.iter() {
                    if *at(&self.core_instances, arg.index, "core instance")?
                        == CoreInstance::Declared
                    {
                        return Err(format!(
                            "an instance of the declared embedded core module is passed as import `{}` to another core instantiation",
                            arg.name
                        ));
                    }
                }
                if *at(&self.core_modules, module_index, "core module")? {
                    self.declared_instantiations += 1;
                    CoreInstance::Declared
                } else {
                    CoreInstance::Other
                }
            }
            Instance::FromExports(exports) => {
                for export in exports.iter() {
                    let space = &self.core_items[core_space(export.kind)];
                    if *at(space, export.index, "core item")? {
                        return Err(format!(
                            "an export of the declared embedded core module is re-bundled as `{}` into another core instance",
                            export.name
                        ));
                    }
                }
                CoreInstance::Other
            }
        };
        self.core_instances.push(origin);
        Ok(())
    }

    fn add_alias(&mut self, alias: ComponentAlias<'_>) -> Result<(), String> {
        match alias {
            ComponentAlias::CoreInstanceExport {
                kind,
                instance_index,
                ..
            } => {
                let declared = *at(&self.core_instances, instance_index, "core instance")?
                    == CoreInstance::Declared;
                self.core_items[core_space(kind)].push(declared);
            }
            ComponentAlias::InstanceExport {
                kind,
                instance_index,
                name,
            } => {
                let instance = at(&self.instances, instance_index, "component instance")?;
                match kind {
                    ComponentExternalKind::Func => {
                        let bound = match instance {
                            ComponentInstanceOrigin::Built { funcs, .. } => funcs
                                .iter()
                                .find(|(field, _)| field == name)
                                .is_some_and(|(_, bound)| *bound),
                            ComponentInstanceOrigin::Opaque => false,
                        };
                        self.funcs.push(bound);
                    }
                    ComponentExternalKind::Instance => {
                        self.instances.push(ComponentInstanceOrigin::Opaque)
                    }
                    ComponentExternalKind::Component => {
                        self.components.push(NestedComponent::Opaque)
                    }
                    ComponentExternalKind::Module => self.core_modules.push(false),
                    ComponentExternalKind::Type => {}
                    ComponentExternalKind::Value => {
                        return Err(
                            "wasip2 component aliases a value, which this checker does not admit"
                                .to_string(),
                        );
                    }
                }
            }
            ComponentAlias::Outer { .. } => {
                return Err("wasip2 component has a top-level outer alias".to_string());
            }
        }
        Ok(())
    }

    fn add_canonical(&mut self, function: CanonicalFunction) -> Result<(), String> {
        match function {
            CanonicalFunction::Lift {
                core_func_index,
                options,
                ..
            } => {
                let mut bound = *at(&self.core_items[0], core_func_index, "core func")?;
                for option in options.iter() {
                    let (space, index) = match *option {
                        CanonicalOption::Memory(index) => (2, index),
                        CanonicalOption::Realloc(index)
                        | CanonicalOption::PostReturn(index)
                        | CanonicalOption::Callback(index) => (0, index),
                        _ => continue,
                    };
                    bound &= *at(&self.core_items[space], index, "core item")?;
                }
                self.funcs.push(bound);
            }
            // Every other canonical function defines a core function.
            _ => self.core_items[0].push(false),
        }
        Ok(())
    }

    fn add_import(&mut self, ty: ComponentTypeRef) -> Result<(), String> {
        match ty {
            ComponentTypeRef::Module(_) => self.core_modules.push(false),
            ComponentTypeRef::Func(_) => self.funcs.push(false),
            ComponentTypeRef::Instance(_) => self.instances.push(ComponentInstanceOrigin::Opaque),
            ComponentTypeRef::Component(_) => self.components.push(NestedComponent::Opaque),
            ComponentTypeRef::Type(_) => {}
            ComponentTypeRef::Value(_) => {
                return Err(
                    "wasip2 component imports a value, which this checker does not admit"
                        .to_string(),
                );
            }
        }
        Ok(())
    }

    fn add_component_instance(&mut self, instance: ComponentInstance<'_>) -> Result<(), String> {
        let origin = match instance {
            ComponentInstance::Instantiate {
                component_index,
                args,
            } => match at(&self.components, component_index, "component")? {
                NestedComponent::Opaque => ComponentInstanceOrigin::Opaque,
                NestedComponent::Shim(reexports) => {
                    let mut funcs = Vec::with_capacity(reexports.len());
                    for (export, import) in reexports {
                        let arg = args
                            .iter()
                            .find(|arg| {
                                arg.name == import && arg.kind == ComponentExternalKind::Func
                            })
                            .ok_or_else(|| {
                                format!("re-export shim import `{import}` is not given a function")
                            })?;
                        funcs.push((
                            export.clone(),
                            *at(&self.funcs, arg.index, "component func")?,
                        ));
                    }
                    ComponentInstanceOrigin::Built {
                        funcs,
                        other_fields: false,
                    }
                }
            },
            ComponentInstance::FromExports(exports) => {
                let mut funcs = Vec::new();
                let mut other_fields = false;
                for export in exports.iter() {
                    match export.kind {
                        ComponentExternalKind::Func => funcs.push((
                            export.name.0.to_string(),
                            *at(&self.funcs, export.index, "component func")?,
                        )),
                        ComponentExternalKind::Type => {}
                        ComponentExternalKind::Instance => {
                            other_fields |=
                                !at(&self.instances, export.index, "component instance")?
                                    .is_bound();
                        }
                        _ => other_fields = true,
                    }
                }
                ComponentInstanceOrigin::Built {
                    funcs,
                    other_fields,
                }
            }
        };
        self.instances.push(origin);
        Ok(())
    }

    fn add_export(
        &mut self,
        name: &str,
        kind: ComponentExternalKind,
        index: u32,
    ) -> Result<(), String> {
        // An export also defines a new index in its sort's index space.
        match kind {
            ComponentExternalKind::Func => {
                let bound = *at(&self.funcs, index, "component func")?;
                if !bound {
                    self.unbound_export.get_or_insert(format!(
                        "component export `{name}` is not lifted from the declared embedded core module"
                    ));
                }
                self.funcs.push(bound);
            }
            ComponentExternalKind::Instance => {
                let instance = at(&self.instances, index, "component instance")?;
                let exported = match instance {
                    ComponentInstanceOrigin::Built { funcs, .. } if instance.is_bound() => {
                        ComponentInstanceOrigin::Built {
                            funcs: funcs.clone(),
                            other_fields: false,
                        }
                    }
                    _ => {
                        self.unbound_export.get_or_insert(format!(
                            "component export `{name}` is an instance whose functions are not all lifted from the declared embedded core module"
                        ));
                        ComponentInstanceOrigin::Opaque
                    }
                };
                self.instances.push(exported);
            }
            ComponentExternalKind::Type => {}
            ComponentExternalKind::Module
            | ComponentExternalKind::Component
            | ComponentExternalKind::Value => {
                return Err(format!(
                    "component export `{name}` has kind {kind:?}, which this checker does not admit"
                ));
            }
        }
        Ok(())
    }
}

/// Skips the payloads of a nested core module up to and including its end.
fn skip_nested_module<'a>(
    payloads: &mut impl Iterator<Item = wasmparser::Result<Payload<'a>>>,
) -> Result<(), String> {
    for payload in payloads {
        let payload = payload.map_err(|error| format!("wasip2 component parse error: {error}"))?;
        if let Payload::End(_) = payload {
            return Ok(());
        }
    }
    Err("wasip2 component ends inside a core module".to_string())
}

/// Reads a nested component that must be a re-export shim: only types,
/// imports of functions and types, and exports of imported functions and of
/// types. Returns its (export name, import name) function pairs.
fn read_reexport_shim<'a>(
    payloads: &mut impl Iterator<Item = wasmparser::Result<Payload<'a>>>,
) -> Result<Vec<(String, String)>, String> {
    // The shim's function index space: the import name behind each index.
    let mut funcs: Vec<String> = Vec::new();
    let mut reexports = Vec::new();
    let refuse = |what: &str| {
        Err(format!(
            "nested component is not a re-export shim: it has {what}"
        ))
    };
    for payload in payloads {
        let payload = payload.map_err(|error| format!("wasip2 component parse error: {error}"))?;
        match payload {
            Payload::Version { .. }
            | Payload::CustomSection(_)
            | Payload::ComponentTypeSection(_) => {}
            Payload::ComponentImportSection(reader) => {
                for import in reader {
                    let import = import.map_err(|e| e.to_string())?;
                    match import.ty {
                        ComponentTypeRef::Func(_) => funcs.push(import.name.0.to_string()),
                        ComponentTypeRef::Type(_) => {}
                        other => return refuse(&format!("an import of kind {other:?}")),
                    }
                }
            }
            Payload::ComponentExportSection(reader) => {
                for export in reader {
                    let export = export.map_err(|e| e.to_string())?;
                    match export.kind {
                        ComponentExternalKind::Func => {
                            let import = at(&funcs, export.index, "shim func")?.clone();
                            reexports.push((export.name.0.to_string(), import.clone()));
                            funcs.push(import);
                        }
                        ComponentExternalKind::Type => {}
                        other => return refuse(&format!("an export of kind {other:?}")),
                    }
                }
            }
            Payload::End(_) => return Ok(reexports),
            other => return refuse(section_name(&other)),
        }
    }
    Err("wasip2 component ends inside a nested component".to_string())
}

/// A short name for a section the gate refuses.
fn section_name(payload: &Payload<'_>) -> &'static str {
    match payload {
        Payload::ModuleSection { .. } => "a core module",
        Payload::ComponentSection { .. } => "a nested component",
        Payload::InstanceSection(_) => "core instances",
        Payload::CoreTypeSection(_) => "core types",
        Payload::ComponentInstanceSection(_) => "component instances",
        Payload::ComponentAliasSection(_) => "aliases",
        Payload::ComponentCanonicalSection(_) => "canonical functions",
        Payload::ComponentStartSection { .. } => "a start function",
        _ => "a section outside the component grammar",
    }
}

#[cfg(test)]
mod tests {
    use super::confirm_declared_core_binding;
    use std::ops::Range;

    fn component(text: &str) -> Vec<u8> {
        let bytes = wat::parse_str(text).expect("test component parses");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("test component validates");
        bytes
    }

    /// The payload ranges of the top-level core module sections, in order.
    fn module_ranges(bytes: &[u8]) -> Vec<Range<usize>> {
        let mut ranges = Vec::new();
        let mut depth = 0usize;
        for payload in wasmparser::Parser::new(0).parse_all(bytes) {
            match payload.unwrap() {
                wasmparser::Payload::ModuleSection {
                    unchecked_range, ..
                } => {
                    if depth == 0 {
                        ranges.push(unchecked_range);
                    }
                    depth += 1;
                }
                wasmparser::Payload::ComponentSection { .. } => depth += 1,
                wasmparser::Payload::End(_) => depth = depth.saturating_sub(1),
                _ => {}
            }
        }
        ranges
    }

    fn refusal(bytes: &[u8], range: Range<usize>) -> String {
        confirm_declared_core_binding(bytes, range).expect_err("binding must be refused")
    }

    const DIRECT: &str = r#"(component
        (core module $m (func (export "f") (result i32) i32.const 0))
        (core instance $i (instantiate $m))
        (alias core export $i "f" (core func $f))
        (type $t (func (result bool)))
        (func $lf (type $t) (canon lift (core func $f)))
        (export "f" (func $lf)))"#;

    /// The `wit-component` shape: a helper module instantiated beside the
    /// main one, the world export lifted from the main module and handed out
    /// through a re-export shim component with imported and exported types.
    const SHIMMED: &str = r#"(component
        (core module $main
            (memory (export "memory") 1)
            (func (export "run") (result i32) i32.const 0)
            (func (export "cabi_post_run") (param i32)))
        (core module $helper (func (export "g")))
        (core instance $h (instantiate $helper))
        (core instance $main (instantiate $main))
        (alias core export $main "memory" (core memory $mem))
        (alias core export $main "cabi_post_run" (core func $post))
        (alias core export $main "run" (core func $run))
        (type $rt (result))
        (type $ft (func (result $rt)))
        (func $run (type $ft) (canon lift (core func $run) (memory $mem) (post-return $post)))
        (type $res (resource (rep i32)))
        (component $shim
            (import "import-type-res" (type (sub resource)))
            (type $rt (result))
            (type $ft (func (result $rt)))
            (import "import-func-run" (func (type $ft)))
            (export "res" (type 0))
            (export "run" (func 0)))
        (instance $si (instantiate $shim
            (with "import-func-run" (func $run))
            (with "import-type-res" (type $res))))
        (export "wasi:cli/run@0.2.4" (instance $si)))"#;

    #[test]
    fn accepts_a_direct_lift_of_the_declared_module() {
        let bytes = component(DIRECT);
        let ranges = module_ranges(&bytes);
        confirm_declared_core_binding(&bytes, ranges[0].clone()).unwrap();
    }

    #[test]
    fn accepts_the_wit_component_shim_shape() {
        let bytes = component(SHIMMED);
        let ranges = module_ranges(&bytes);
        confirm_declared_core_binding(&bytes, ranges[0].clone()).unwrap();
        // The helper module is instantiated but nothing is lifted from it.
        let helper = refusal(&bytes, ranges[1].clone());
        assert!(helper.contains("are not all lifted"), "{helper}");
    }

    #[test]
    fn refuses_a_core_hidden_in_a_custom_section() {
        let mut bytes = component(DIRECT);
        let decoy =
            wat::parse_str(r#"(module (func (export "f") (result i32) i32.const 1))"#).unwrap();
        let name = b"certified-decoy-core";
        let mut payload = vec![u8::try_from(name.len()).unwrap()];
        payload.extend_from_slice(name);
        let start = bytes.len() + 2 + payload.len();
        payload.extend_from_slice(&decoy);
        bytes.push(0);
        bytes.push(u8::try_from(payload.len()).unwrap());
        bytes.extend_from_slice(&payload);
        wasmparser::Validator::new().validate_all(&bytes).unwrap();
        let error = refusal(&bytes, start..start + decoy.len());
        assert!(
            error.contains("is not a top-level core module section"),
            "{error}"
        );
    }

    #[test]
    fn refuses_a_range_that_is_not_exactly_a_module_payload() {
        let bytes = component(DIRECT);
        let range = module_ranges(&bytes)[0].clone();
        let error = refusal(&bytes, range.start..range.end - 1);
        assert!(
            error.contains("is not a top-level core module section"),
            "{error}"
        );
    }

    #[test]
    fn refuses_an_extra_module_that_is_never_instantiated() {
        let bytes = component(
            r#"(component
            (core module $m (func (export "f") (result i32) i32.const 1))
            (core module $decoy (func (export "f") (result i32) i32.const 0))
            (core instance $i (instantiate $m))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[1].clone());
        assert!(error.contains("never instantiated"), "{error}");
    }

    #[test]
    fn refuses_an_instantiated_module_the_exports_do_not_come_from() {
        let bytes = component(
            r#"(component
            (core module $m (func (export "f") (result i32) i32.const 1))
            (core module $decoy (func (export "f") (result i32) i32.const 0))
            (core instance $d (instantiate $decoy))
            (core instance $i (instantiate $m))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[1].clone());
        assert!(
            error.contains("`f` is not lifted from the declared embedded core module"),
            "{error}"
        );
    }

    #[test]
    fn refuses_a_core_inside_a_nested_component() {
        let bytes = component(
            r#"(component
            (component $inner
                (core module $decoy (func (export "f") (result i32) i32.const 0)))
            (core module $m (func (export "f") (result i32) i32.const 1))
            (core instance $i (instantiate $m))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let inner = {
            let mut found = None;
            for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
                if let wasmparser::Payload::ModuleSection {
                    unchecked_range, ..
                } = payload.unwrap()
                {
                    found.get_or_insert(unchecked_range);
                }
            }
            found.unwrap()
        };
        let error = refusal(&bytes, inner);
        assert!(error.contains("is not a re-export shim"), "{error}");
    }

    #[test]
    fn refuses_a_shim_that_re_exports_another_module() {
        let bytes = component(
            &SHIMMED
                .replace(
                    "(with \"import-func-run\" (func $run))",
                    "(with \"import-func-run\" (func $other))",
                )
                .replace(
                    "(type $res (resource (rep i32)))",
                    "(type $res (resource (rep i32)))
             (core module $evil (func (export \"run\") (result i32) i32.const 1))
             (core instance $e (instantiate $evil))
             (alias core export $e \"run\" (core func $erun))
             (func $other (type $ft) (canon lift (core func $erun)))",
                ),
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("are not all lifted"), "{error}");
    }

    #[test]
    fn refuses_lift_options_from_another_module() {
        let bytes = component(
            r#"(component
            (core module $m
                (memory (export "memory") 1)
                (func (export "f") (result i32) i32.const 0))
            (core module $o (memory (export "memory") 1))
            (core instance $oi (instantiate $o))
            (core instance $i (instantiate $m))
            (alias core export $oi "memory" (core memory $omem))
            (alias core export $i "f" (core func $f))
            (type $t (func (result string)))
            (func $lf (type $t) (canon lift (core func $f) (memory $omem)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("is not lifted"), "{error}");
    }

    #[test]
    fn refuses_handing_the_declared_instance_to_another_module() {
        let bytes = component(
            r#"(component
            (core module $m
                (memory (export "memory") 1)
                (func (export "f") (result i32) i32.const 0))
            (core module $o (import "m" "memory" (memory 1)))
            (core instance $i (instantiate $m))
            (core instance $oi (instantiate $o (with "m" (instance $i))))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("passed as import `m`"), "{error}");
    }

    #[test]
    fn refuses_re_bundling_a_declared_export_for_another_module() {
        let bytes = component(
            r#"(component
            (core module $m
                (memory (export "memory") 1)
                (func (export "f") (result i32) i32.const 0))
            (core module $o (import "m" "memory" (memory 1)))
            (core instance $i (instantiate $m))
            (alias core export $i "memory" (core memory $mem))
            (core instance $bundle (export "memory" (memory $mem)))
            (core instance $oi (instantiate $o (with "m" (instance $bundle))))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("re-bundled as `memory`"), "{error}");
    }
}
