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
//! 2. the declared module is instantiated exactly once;
//! 3. every function the component exports is a `canon lift` of the core
//!    export of the same name of that one instance, under the `wit-component`
//!    naming: a top-level function export `f` lifts core export `f`, and field
//!    `f` of an exported instance `i` lifts core export `i#f`. A lift carries
//!    no canonical options, as Aver's lifts of its world entry points do, and
//!    a lower carries at most a memory, a realloc and the UTF-8 encoding. The
//!    gate returns the lifted core export names; the verifier refuses a
//!    certified export among them, since Aver never lifts one and the checker
//!    therefore knows no component signature to hold it to;
//! 4. no instance of the declared module, and no item exported by one, is
//!    handed to another core instantiation;
//! 5. every other core module the component instantiates is one of the two
//!    helper modules `wit-component` emits behind the declared module's
//!    imports, in exactly the shape it emits them, so no helper code runs at
//!    instantiation. The shim has no imports, one `funcref` table of N slots
//!    exported as `$imports`, and N functions, function `i` exported as `"i"`
//!    and doing nothing but pass its parameters to slot `i` through
//!    `call_indirect`. The fixup imports functions `"" "0"` to `"" "N-1"`
//!    and the N-slot table `"" "$imports"`, and has one active element
//!    segment writing import `i` to slot `i` and nothing else. A start
//!    function, a data segment, a memory, a global, a table initializer, any
//!    other function body or element segment, or an imported or aliased core
//!    module is refused;
//! 6. the helpers are wired by identity, as `wit-component` wires them: at
//!    most one shim instance, made with no arguments before the declared
//!    module; each import bundle `M` of the declared module hands it, per
//!    function `N`, the shim's next trampoline in slot order, a lower of the
//!    imported instance `M`'s function `N`, or a `resource.drop`; and one
//!    fixup instance, made after the declared module, given the shim's table
//!    and for each slot `i` the lower of exactly the import slot `i` was
//!    handed for. A shim without its fixup is refused;
//! 7. a nested component is only a re-export shim defined in the component:
//!    it imports functions and types and exports those same imports, with no
//!    code, instances or canonical functions of its own. An imported or
//!    aliased component is refused.
//!
//! Anything outside these shapes is refused. The walk runs after
//! `wasmparser::Validator` has accepted the component, so every index it reads
//! is in bounds of a well-typed index space.

use wasmparser::{
    CanonicalFunction, CanonicalOption, ComponentAlias, ComponentExternalKind, ComponentInstance,
    ComponentType, ComponentTypeRef, CompositeInnerType, Element, ElementItems, ElementKind,
    ExternalKind, FunctionBody, Instance, Operator, Parser, Payload, RefType, TableInit, TableType,
    TypeRef,
};

/// A core module in the component's core module index space.
#[derive(Clone)]
enum CoreModule {
    /// The manifest-declared module.
    Declared,
    /// The `wit-component` shim module, with this many slots.
    Shim(u32),
    /// The `wit-component` fixup module, filling a table of this many slots.
    Fixup(u32),
    /// Any other module. Instantiating it is refused for this reason.
    Other(String),
}

/// Where a core instance comes from.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CoreInstance {
    /// The one instance of the declared module.
    Declared,
    /// The one instance of the shim module.
    Shim,
    /// A `from_exports` bundle; its exports are `TopLevel::bundles[_]`.
    Bundle(usize),
    /// Any other instance.
    Plain,
}

/// A core item (function, table, memory, global or tag).
#[derive(Clone, PartialEq, Eq)]
enum CoreItem {
    /// The export of this name of the declared instance.
    Declared(String),
    /// The `$imports` table of the shim instance.
    ShimTable,
    /// The shim instance's trampoline for this slot.
    Trampoline(u32),
    /// A `canon lower` of this component function.
    Lowered(Lifted),
    /// A `canon resource.drop`.
    ResourceDrop,
    Plain,
}

/// A nested component. The only one admitted is a re-export shim defined in
/// the component itself: pairs of (export name, import name) for each
/// re-exported function. An imported or aliased component, whose code is
/// unknown, is refused.
type ReexportShim = Vec<(String, String)>;

/// Where a component function comes from.
#[derive(Clone, PartialEq, Eq)]
enum Lifted {
    /// A `canon lift` of the declared instance's core export of this name.
    Lift(String),
    /// Export `name` of the component instance imported as `instance`.
    Imported {
        instance: String,
        name: String,
    },
    Other,
}

/// A component instance. `Built` instances list their function fields with
/// the core export each one lifts.
enum ComponentInstanceOrigin {
    Built {
        funcs: Vec<(String, Lifted)>,
        other_fields: bool,
    },
    /// The component instance imported under this name.
    Imported(String),
    Opaque,
}

const CORE_SPACES: usize = 5;
const FUNC_SPACE: usize = 0;
const TABLE_SPACE: usize = 1;
const MEMORY_SPACE: usize = 2;

fn core_space(kind: ExternalKind) -> usize {
    match kind {
        ExternalKind::Func | ExternalKind::FuncExact => FUNC_SPACE,
        ExternalKind::Table => TABLE_SPACE,
        ExternalKind::Memory => MEMORY_SPACE,
        ExternalKind::Global => 3,
        ExternalKind::Tag => 4,
    }
}

#[derive(Default)]
struct TopLevel {
    core_modules: Vec<CoreModule>,
    core_instances: Vec<CoreInstance>,
    core_items: [Vec<CoreItem>; CORE_SPACES],
    funcs: Vec<Lifted>,
    instances: Vec<ComponentInstanceOrigin>,
    components: Vec<ReexportShim>,
    declared_instantiations: usize,
    /// The first component export not lifted from the declared module under
    /// its own name. It is reported after the checks on the declaration.
    unbound_export: Option<String>,
    /// The first instantiation of a module that is neither the declared one
    /// nor a helper. It is reported after the unbound export.
    helper_refusal: Option<String>,
    /// The declared instance's core exports that a `canon lift` exposes.
    lifted: Vec<String>,
    /// The exports of each `from_exports` core instance.
    bundles: Vec<Vec<(String, CoreItem)>>,
    /// The slot count of the shim instance, once instantiated.
    shim_slots: Option<u32>,
    /// For each shim slot in order, the declared module's import
    /// `(module, name)` it was handed for.
    wired: Vec<(String, String)>,
    fixup_instantiated: bool,
}

fn at<'a, T>(items: &'a [T], index: u32, space: &str) -> Result<&'a T, String> {
    usize::try_from(index)
        .ok()
        .and_then(|index| items.get(index))
        .ok_or_else(|| format!("wasip2 component refers to {space} {index} out of range"))
}

/// Confirms that `core_range` (the manifest-declared embedded core module) is
/// the top-level core module every component export is lifted from, and
/// returns the names of the declared module's core exports that the component
/// lifts. The caller refuses a certified export among them.
pub(crate) fn confirm_declared_core_binding(
    component_bytes: &[u8],
    core_range: std::ops::Range<usize>,
) -> Result<Vec<String>, String> {
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
            Payload::CustomSection(_) | Payload::CoreTypeSection(_) => {}
            Payload::ComponentTypeSection(reader) => {
                for ty in reader {
                    let ty = ty.map_err(|e| e.to_string())?;
                    if let ComponentType::Resource { dtor: Some(_), .. } = ty {
                        return Err(
                            "wasip2 component defines a resource with a destructor, which this checker does not admit"
                                .to_string(),
                        );
                    }
                }
            }
            Payload::ModuleSection {
                unchecked_range, ..
            } => {
                let shape = scan_nested_module(&mut payloads)?;
                top.core_modules.push(if unchecked_range == core_range {
                    declared_seen = true;
                    CoreModule::Declared
                } else {
                    shape
                });
            }
            Payload::ComponentSection { .. } => {
                let shim = read_reexport_shim(&mut payloads)?;
                top.components.push(shim);
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
                    top.add_import(import.name.0, import.ty)?;
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
    if let Some(error) = top.unbound_export.or(top.helper_refusal) {
        return Err(error);
    }
    if top.shim_slots.is_some() && !top.fixup_instantiated {
        return Err(
            "the wit-component shim module is instantiated but no fixup fills its table"
                .to_string(),
        );
    }
    Ok(top.lifted)
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
                            "an instance of the declared embedded core module is passed as import {:?} to another core instantiation",
                            arg.name
                        ));
                    }
                }
                match at(&self.core_modules, module_index, "core module")?.clone() {
                    CoreModule::Declared => {
                        self.declared_instantiations += 1;
                        if self.declared_instantiations > 1 {
                            return Err(
                                "declared embedded core module is instantiated more than once"
                                    .to_string(),
                            );
                        }
                        for arg in args.iter() {
                            self.wire_declared_import(arg.name, arg.index)?;
                        }
                        CoreInstance::Declared
                    }
                    CoreModule::Shim(slots) => {
                        if self.shim_slots.is_some() {
                            return Err(
                                "the wit-component shim module is instantiated more than once"
                                    .to_string(),
                            );
                        }
                        if self.declared_instantiations > 0 {
                            return Err(
                                "the wit-component shim module is instantiated after the declared embedded core module"
                                    .to_string(),
                            );
                        }
                        if !args.is_empty() {
                            return Err(
                                "the wit-component shim module is instantiated with arguments"
                                    .to_string(),
                            );
                        }
                        self.shim_slots = Some(slots);
                        CoreInstance::Shim
                    }
                    CoreModule::Fixup(slots) => {
                        self.check_fixup(slots, &args)?;
                        CoreInstance::Plain
                    }
                    CoreModule::Other(reason) => {
                        self.helper_refusal.get_or_insert(format!(
                            "core module {module_index} is instantiated, but it is neither the declared embedded core module nor a wit-component shim or fixup module: {reason}"
                        ));
                        CoreInstance::Plain
                    }
                }
            }
            Instance::FromExports(exports) => {
                for export in exports.iter() {
                    let space = &self.core_items[core_space(export.kind)];
                    if let CoreItem::Declared(_) = at(space, export.index, "core item")? {
                        return Err(format!(
                            "an export of the declared embedded core module is re-bundled as {:?} into another core instance",
                            export.name
                        ));
                    }
                }
                let mut bundle = Vec::with_capacity(exports.len());
                for export in exports.iter() {
                    let space = &self.core_items[core_space(export.kind)];
                    bundle.push((
                        export.name.to_string(),
                        at(space, export.index, "core item")?.clone(),
                    ));
                }
                self.bundles.push(bundle);
                CoreInstance::Bundle(self.bundles.len() - 1)
            }
        };
        self.core_instances.push(origin);
        Ok(())
    }

    /// The exports of the `from_exports` core instance `index`.
    fn bundle(&self, index: u32) -> Option<&[(String, CoreItem)]> {
        match at(&self.core_instances, index, "core instance").ok()? {
            CoreInstance::Bundle(bundle) => Some(&self.bundles[*bundle]),
            _ => None,
        }
    }

    /// The declared module's import instance `module` must be a bundle whose
    /// every function is wired as `wit-component` wires it: the next shim
    /// slot in order (the fixup later fills that slot with the lowered
    /// import), a `canon lower` of the component instance `module`'s function
    /// of the same name, or a `resource.drop` under `[resource-drop]...`.
    fn wire_declared_import(&mut self, module: &str, index: u32) -> Result<(), String> {
        let bundle = self.bundle(index).ok_or_else(|| {
            format!(
                "the declared embedded core module's import {module:?} is not a bundle of wired functions"
            )
        })?;
        let mut wired = Vec::new();
        for (name, item) in bundle {
            let next = u32::try_from(self.wired.len() + wired.len()).ok();
            let ok = match item {
                CoreItem::Trampoline(slot) => {
                    wired.push((module.to_string(), name.clone()));
                    Some(*slot) == next
                }
                CoreItem::Lowered(Lifted::Imported {
                    instance,
                    name: field,
                }) => instance == module && field == name,
                CoreItem::ResourceDrop => name.starts_with("[resource-drop]"),
                _ => false,
            };
            if !ok {
                return Err(format!(
                    "the declared embedded core module's import {module:?} {name:?} is not wired as wit-component wires it"
                ));
            }
        }
        self.wired.extend(wired);
        Ok(())
    }

    /// The fixup is instantiated once, after the declared module, with one
    /// bundle: the shim's `$imports` table, then for each slot `i` in order
    /// the `canon lower` of the component function the declared module
    /// imports through slot `i`. Every shim slot is so filled.
    fn check_fixup(
        &mut self,
        slots: u32,
        args: &[wasmparser::InstantiationArg<'_>],
    ) -> Result<(), String> {
        let refuse = |what: &str| {
            Err(format!(
                "the wit-component fixup module is not instantiated as wit-component instantiates it: {what}"
            ))
        };
        if self.fixup_instantiated {
            return refuse("it is instantiated more than once");
        }
        if self.declared_instantiations == 0 {
            return refuse("it is instantiated before the declared embedded core module");
        }
        if self.shim_slots != Some(slots) || self.wired.len() != slots as usize {
            return refuse(
                "its slots are not exactly the shim's slots the declared module imports through",
            );
        }
        let bundle = match args {
            [arg] if arg.name.is_empty() => self.bundle(arg.index),
            _ => None,
        };
        let Some((table, funcs)) = bundle.and_then(<[_]>::split_first) else {
            return refuse("it is not given one bundle of the shim table and lowered functions");
        };
        if *table != ("$imports".to_string(), CoreItem::ShimTable)
            || funcs.len() != self.wired.len()
        {
            return refuse("it is not given the shim's `$imports` table and one function per slot");
        }
        for (slot, ((name, item), (module, field))) in funcs.iter().zip(&self.wired).enumerate() {
            let expected = CoreItem::Lowered(Lifted::Imported {
                instance: module.clone(),
                name: field.clone(),
            });
            if *name != slot.to_string() || *item != expected {
                return refuse(&format!(
                    "slot {slot} is not given the lowered {module:?} {field:?} the declared module imports through it"
                ));
            }
        }
        self.fixup_instantiated = true;
        Ok(())
    }

    fn add_alias(&mut self, alias: ComponentAlias<'_>) -> Result<(), String> {
        match alias {
            ComponentAlias::CoreInstanceExport {
                kind,
                instance_index,
                name,
            } => {
                let item = match at(&self.core_instances, instance_index, "core instance")? {
                    CoreInstance::Declared => CoreItem::Declared(name.to_string()),
                    CoreInstance::Shim if kind == ExternalKind::Table && name == "$imports" => {
                        CoreItem::ShimTable
                    }
                    CoreInstance::Shim if kind == ExternalKind::Func => {
                        name.parse().map_or(CoreItem::Plain, CoreItem::Trampoline)
                    }
                    _ => CoreItem::Plain,
                };
                self.core_items[core_space(kind)].push(item);
            }
            ComponentAlias::InstanceExport {
                kind,
                instance_index,
                name,
            } => {
                let instance = at(&self.instances, instance_index, "component instance")?;
                match kind {
                    ComponentExternalKind::Func => {
                        let lifted = match instance {
                            ComponentInstanceOrigin::Built { funcs, .. } => funcs
                                .iter()
                                .find(|(field, _)| field == name)
                                .map_or(Lifted::Other, |(_, lifted)| lifted.clone()),
                            ComponentInstanceOrigin::Imported(instance) => Lifted::Imported {
                                instance: instance.clone(),
                                name: name.to_string(),
                            },
                            ComponentInstanceOrigin::Opaque => Lifted::Other,
                        };
                        self.funcs.push(lifted);
                    }
                    ComponentExternalKind::Instance | ComponentExternalKind::Component => {
                        return Err(format!(
                            "wasip2 component aliases {kind:?} `{name}` of a component instance, which this checker does not admit"
                        ));
                    }
                    ComponentExternalKind::Module => {
                        return Err(
                            "wasip2 component aliases a core module, which this checker does not admit"
                                .to_string(),
                        );
                    }
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

    /// Aver lowers with at most a memory and a realloc of the declared
    /// instance and the UTF-8 string encoding; any other option is refused.
    fn check_lower_options(&self, options: &[CanonicalOption]) -> Result<(), String> {
        for option in options {
            let (space, index) = match *option {
                CanonicalOption::Memory(index) => (MEMORY_SPACE, index),
                CanonicalOption::Realloc(index) => (FUNC_SPACE, index),
                CanonicalOption::UTF8 => continue,
                ref other => {
                    return Err(format!(
                        "wasip2 component lowers a function with canonical option {other:?}, which Aver does not emit"
                    ));
                }
            };
            if !matches!(
                at(&self.core_items[space], index, "core item")?,
                CoreItem::Declared(_)
            ) {
                return Err(format!(
                    "wasip2 component lowers a function with canonical option {option:?} from outside the declared embedded core module"
                ));
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
                // Aver lifts only its world entry points (`wasi:cli/run`'s
                // `run`, `wasi:http/incoming-handler`'s `handle`), whose
                // parameters and results are flat scalars and handles, with
                // no canonical options. A memory, realloc, post-return, string
                // encoding, async, callback, core-type or GC option changes how
                // values cross the boundary, so none is admitted.
                if let Some(option) = options.first() {
                    return Err(format!(
                        "wasip2 component lifts a function with canonical option {option:?}; Aver lifts with none"
                    ));
                }
                let lifted = match at(&self.core_items[FUNC_SPACE], core_func_index, "core func")? {
                    CoreItem::Declared(name) => {
                        self.lifted.push(name.clone());
                        Lifted::Lift(name.clone())
                    }
                    _ => Lifted::Other,
                };
                self.funcs.push(lifted);
            }
            CanonicalFunction::Lower {
                func_index,
                options,
            } => {
                self.check_lower_options(&options)?;
                let lowered = at(&self.funcs, func_index, "component func")?.clone();
                self.core_items[FUNC_SPACE].push(CoreItem::Lowered(lowered));
            }
            CanonicalFunction::ResourceDrop { .. } => {
                self.core_items[FUNC_SPACE].push(CoreItem::ResourceDrop)
            }
            _ => {
                return Err(
                    "wasip2 component uses a canonical built-in this checker does not admit"
                        .to_string(),
                );
            }
        }
        Ok(())
    }

    fn add_import(&mut self, name: &str, ty: ComponentTypeRef) -> Result<(), String> {
        match ty {
            ComponentTypeRef::Module(_) => {
                return Err(
                    "wasip2 component imports a core module, which this checker does not admit"
                        .to_string(),
                );
            }
            ComponentTypeRef::Func(_) => self.funcs.push(Lifted::Other),
            ComponentTypeRef::Instance(_) => self
                .instances
                .push(ComponentInstanceOrigin::Imported(name.to_string())),
            ComponentTypeRef::Component(_) => {
                return Err(
                    "wasip2 component imports a component, whose code this checker cannot see"
                        .to_string(),
                );
            }
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
            } => {
                let reexports = at(&self.components, component_index, "component")?;
                let mut funcs = Vec::with_capacity(reexports.len());
                for (export, import) in reexports {
                    let arg = args
                        .iter()
                        .find(|arg| arg.name == import && arg.kind == ComponentExternalKind::Func)
                        .ok_or_else(|| {
                            format!("re-export shim import `{import}` is not given a function")
                        })?;
                    funcs.push((
                        export.clone(),
                        at(&self.funcs, arg.index, "component func")?.clone(),
                    ));
                }
                ComponentInstanceOrigin::Built {
                    funcs,
                    other_fields: false,
                }
            }
            ComponentInstance::FromExports(exports) => {
                let mut funcs = Vec::new();
                let mut other_fields = false;
                for export in exports.iter() {
                    match export.kind {
                        ComponentExternalKind::Func => funcs.push((
                            export.name.0.to_string(),
                            at(&self.funcs, export.index, "component func")?.clone(),
                        )),
                        ComponentExternalKind::Type => {}
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
                let lifted = at(&self.funcs, index, "component func")?.clone();
                self.require_lift_of(name, name, &lifted);
                self.funcs.push(lifted);
            }
            ComponentExternalKind::Instance => {
                let exported = match at(&self.instances, index, "component instance")? {
                    ComponentInstanceOrigin::Built {
                        funcs,
                        other_fields: false,
                    } => Some(funcs.clone()),
                    _ => None,
                };
                match exported {
                    Some(funcs) => {
                        for (field, lifted) in &funcs {
                            self.require_lift_of(
                                &format!("{name}.{field}"),
                                &format!("{name}#{field}"),
                                lifted,
                            );
                        }
                        self.instances.push(ComponentInstanceOrigin::Built {
                            funcs,
                            other_fields: false,
                        });
                    }
                    None => {
                        self.unbound_export.get_or_insert(format!(
                            "component export `{name}` is an instance this checker cannot trace to the declared embedded core module"
                        ));
                        self.instances.push(ComponentInstanceOrigin::Opaque);
                    }
                }
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

    /// Records a refusal unless the exported function `shown` lifts exactly
    /// the declared instance's core export `core_name`.
    fn require_lift_of(&mut self, shown: &str, core_name: &str, lifted: &Lifted) {
        let error = match lifted {
            Lifted::Lift(lifted) if lifted == core_name => return,
            Lifted::Lift(lifted) => format!(
                "component export `{shown}` lifts core export {lifted:?} of the declared embedded core module; it must lift {core_name:?}"
            ),
            Lifted::Imported { .. } | Lifted::Other => format!(
                "component export `{shown}` is not lifted from the declared embedded core module"
            ),
        };
        self.unbound_export.get_or_insert(error);
    }
}

/// Reads the payloads of a nested core module up to and including its end,
/// and returns which helper shape it has, if any.
fn scan_nested_module<'a>(
    payloads: &mut impl Iterator<Item = wasmparser::Result<Payload<'a>>>,
) -> Result<CoreModule, String> {
    let mut scan = ModuleScan::default();
    for payload in payloads {
        let payload = payload.map_err(|error| format!("wasip2 component parse error: {error}"))?;
        if let Payload::End(_) = payload {
            return Ok(scan.shape());
        }
        scan.read(payload)
            .map_err(|error| format!("wasip2 component parse error: {error}"))?;
    }
    Err("wasip2 component ends inside a core module".to_string())
}

/// What a nested core module holds, as far as the helper shapes need.
#[derive(Default)]
struct ModuleScan {
    /// The parameter count of each type, `None` for a non-function type.
    params: Vec<Option<usize>>,
    imports: Vec<(String, String, TypeRef)>,
    /// The type of each defined function.
    funcs: Vec<u32>,
    tables: Vec<TableType>,
    exports: Vec<(String, ExternalKind, u32)>,
    /// Each element segment's functions, when it is active on table 0 at
    /// offset 0 and lists functions; `None` for any other segment.
    elements: Vec<Option<Vec<u32>>>,
    bodies: usize,
    /// How many bodies are the shim trampoline for their own slot.
    trampolines: usize,
    /// The first thing no helper module has.
    refused: Option<&'static str>,
}

impl ModuleScan {
    fn read(&mut self, payload: Payload<'_>) -> wasmparser::Result<()> {
        match payload {
            Payload::Version { .. } | Payload::CustomSection(_) => {}
            Payload::TypeSection(reader) => {
                for group in reader {
                    for ty in group?.types() {
                        self.params.push(match &ty.composite_type.inner {
                            CompositeInnerType::Func(func) => Some(func.params().len()),
                            _ => None,
                        });
                    }
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let import = import?;
                    self.imports.push((
                        import.module.to_string(),
                        import.name.to_string(),
                        import.ty,
                    ));
                }
            }
            Payload::FunctionSection(reader) => {
                for ty in reader {
                    self.funcs.push(ty?);
                }
            }
            Payload::TableSection(reader) => {
                for table in reader {
                    let table = table?;
                    if !matches!(table.init, TableInit::RefNull) {
                        self.refuse("a table initializer");
                    }
                    self.tables.push(table.ty);
                }
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export?;
                    self.exports
                        .push((export.name.to_string(), export.kind, export.index));
                }
            }
            Payload::ElementSection(reader) => {
                for element in reader {
                    self.elements.push(fixup_segment(element?)?);
                }
            }
            Payload::CodeSectionStart { .. } => {}
            Payload::CodeSectionEntry(body) => {
                if self.is_trampoline(&body, self.bodies)? {
                    self.trampolines += 1;
                }
                self.bodies += 1;
            }
            // What runs at instantiation is reported ahead of what merely
            // does not fit a helper's shape.
            Payload::StartSection { .. } => self.refused = Some("a start function"),
            Payload::DataSection(_) | Payload::DataCountSection { .. } => {
                if self.refused != Some("a start function") {
                    self.refused = Some("data segments");
                }
            }
            Payload::MemorySection(_) => self.refuse("a memory"),
            Payload::GlobalSection(_) => self.refuse("globals"),
            _ => self.refuse("a section no helper module has"),
        }
        Ok(())
    }

    fn refuse(&mut self, what: &'static str) {
        self.refused.get_or_insert(what);
    }

    fn shape(self) -> CoreModule {
        if let Some(what) = self.refused {
            CoreModule::Other(format!("it has {what}"))
        } else if self.is_shim() {
            CoreModule::Shim(u32::try_from(self.funcs.len()).unwrap_or(u32::MAX))
        } else if let Some(slots) = self.fixup_slots() {
            CoreModule::Fixup(slots)
        } else {
            CoreModule::Other("its imports, functions, tables, exports or element segments are not in either helper's shape".to_string())
        }
    }

    /// Body `slot` is `local.get 0 .. local.get (n-1); i32.const slot;
    /// call_indirect (type t) 0; end` with no locals, where `t` is the
    /// function's own type and `n` its parameter count.
    fn is_trampoline(&self, body: &FunctionBody<'_>, slot: usize) -> wasmparser::Result<bool> {
        let Some(&ty) = self.funcs.get(slot) else {
            return Ok(false);
        };
        let Some(&Some(params)) = usize::try_from(ty).ok().and_then(|ty| self.params.get(ty))
        else {
            return Ok(false);
        };
        if body.get_locals_reader()?.get_count() != 0 {
            return Ok(false);
        }
        let mut ops = body.get_operators_reader()?;
        for local in 0..params {
            if !matches!(ops.read()?, Operator::LocalGet { local_index } if usize::try_from(local_index) == Ok(local))
            {
                return Ok(false);
            }
        }
        Ok(
            matches!(ops.read()?, Operator::I32Const { value } if usize::try_from(value) == Ok(slot))
                && matches!(ops.read()?, Operator::CallIndirect { type_index, table_index: 0 } if type_index == ty)
                && matches!(ops.read()?, Operator::End)
                && ops.eof(),
        )
    }

    /// No imports and no element segments; one N-slot table; N trampolines,
    /// exported in order as `"0"` to `"N-1"`, then the table as `$imports`.
    fn is_shim(&self) -> bool {
        let slots = self.funcs.len();
        slots > 0
            && self.imports.is_empty()
            && self.elements.is_empty()
            && matches!(self.tables.as_slice(), [table] if slot_table(table, slots))
            && self.bodies == slots
            && self.trampolines == slots
            && self.exports.len() == slots + 1
            && self
                .exports
                .iter()
                .enumerate()
                .all(|(index, (name, kind, item))| {
                    if index == slots {
                        name == "$imports" && *kind == ExternalKind::Table && *item == 0
                    } else {
                        *name == index.to_string()
                            && *kind == ExternalKind::Func
                            && usize::try_from(*item) == Ok(index)
                    }
                })
    }

    /// Imports `"" "0"` to `"" "N-1"` (functions) then `"" "$imports"` (an
    /// N-slot table), and one segment writing imports `0..N` from slot 0;
    /// nothing else.
    fn fixup_slots(&self) -> Option<u32> {
        let ((table_module, table_name, table), funcs) = self.imports.split_last()?;
        let slots = funcs.len();
        let shape = slots > 0
            && self.funcs.is_empty()
            && self.bodies == 0
            && self.tables.is_empty()
            && self.exports.is_empty()
            && funcs.iter().enumerate().all(|(slot, (module, name, ty))| {
                module.is_empty() && *name == slot.to_string() && matches!(ty, TypeRef::Func(_))
            })
            && table_module.is_empty()
            && table_name == "$imports"
            && matches!(table, TypeRef::Table(table) if slot_table(table, slots))
            && matches!(self.elements.as_slice(), [Some(written)]
                if written.iter().enumerate().all(|(slot, func)| usize::try_from(*func) == Ok(slot))
                    && written.len() == slots);
        if shape {
            u32::try_from(slots).ok()
        } else {
            None
        }
    }
}

/// A 32-bit, unshared table of exactly `slots` nullable `funcref` slots.
fn slot_table(table: &TableType, slots: usize) -> bool {
    let slots = u64::try_from(slots).ok();
    table.element_type == RefType::FUNCREF
        && !table.table64
        && !table.shared
        && Some(table.initial) == slots
        && table.maximum == slots
}

/// The functions of an element segment that is active on table 0 at offset
/// `i32.const 0` and lists functions; `None` for any other segment.
fn fixup_segment(element: Element<'_>) -> wasmparser::Result<Option<Vec<u32>>> {
    let ElementKind::Active {
        table_index,
        offset_expr,
    } = element.kind
    else {
        return Ok(None);
    };
    if table_index.unwrap_or(0) != 0 {
        return Ok(None);
    }
    let mut offset = offset_expr.get_operators_reader();
    let at_zero = matches!(offset.read()?, Operator::I32Const { value: 0 })
        && matches!(offset.read()?, Operator::End)
        && offset.eof();
    match element.items {
        ElementItems::Functions(funcs) if at_zero => {
            funcs.into_iter().collect::<Result<_, _>>().map(Some)
        }
        _ => Ok(None),
    }
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

    /// The `wit-component` export shape: the world export lifted from the
    /// main module and handed out through a re-export shim component with
    /// imported and exported types.
    const SHIMMED: &str = r#"(component
        (core module $main
            (memory (export "memory") 1)
            (func (export "wasi:cli/run@0.2.4#run") (result i32) i32.const 0)
            (func (export "cabi_post_run") (param i32)))
        (core instance $main (instantiate $main))
        (alias core export $main "memory" (core memory $mem))
        (alias core export $main "cabi_post_run" (core func $post))
        (alias core export $main "wasi:cli/run@0.2.4#run" (core func $run))
        (type $rt (result))
        (type $ft (func (result $rt)))
        (func $run (type $ft) (canon lift (core func $run)))
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
    fn refuses_imported_and_aliased_components() {
        // Codex round 3: an imported (empty) component instantiated beside the
        // declared core runs code the checker cannot see.
        let imported = component(&DIRECT.replace(
            "(export \"f\" (func $lf)))",
            "(import \"c\" (component $c))
             (instance $ci (instantiate $c))
             (export \"f\" (func $lf)))",
        ));
        let error = refusal(&imported, module_ranges(&imported)[0].clone());
        assert!(error.contains("imports a component"), "{error}");
        let aliased = component(&DIRECT.replace(
            "(export \"f\" (func $lf)))",
            "(import \"i\" (instance $i (export \"c\" (component))))
             (alias export $i \"c\" (component $c))
             (instance $ci (instantiate $c))
             (export \"f\" (func $lf)))",
        ));
        let error = refusal(&aliased, module_ranges(&aliased)[0].clone());
        assert!(error.contains("aliases Component `c`"), "{error}");
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
        assert!(error.contains("is not lifted from the declared"), "{error}");
    }

    #[test]
    fn refuses_canonical_options_on_a_lift() {
        // Codex: a certified Bool core export exposed as `(u32) -> u32` is a
        // signature matter the verifier settles; the options are refused here,
        // from whichever module they come.
        let lift = |options: &str| {
            format!(
                r#"(component
                (core module $m
                    (memory (export "memory") 1)
                    (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) i32.const 0)
                    (func (export "cabi_post_f") (param i32))
                    (func (export "f") (result i32) i32.const 0))
                (core module $o (memory (export "memory") 1))
                (core instance $oi (instantiate $o))
                (core instance $i (instantiate $m))
                (alias core export $oi "memory" (core memory $omem))
                (alias core export $i "memory" (core memory $mem))
                (alias core export $i "cabi_realloc" (core func $realloc))
                (alias core export $i "cabi_post_f" (core func $post))
                (alias core export $i "f" (core func $f))
                (type $t (func (result string)))
                (func $lf (type $t) (canon lift (core func $f) {options}))
                (export "f" (func $lf)))"#
            )
        };
        for options in [
            "(memory $omem)",
            "(memory $mem)",
            "(memory $mem) (realloc $realloc) (post-return $post)",
            "(memory $mem) string-encoding=utf16",
            "(memory $mem) string-encoding=latin1+utf16",
        ] {
            let bytes = component(&lift(options));
            let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
            assert!(
                error.contains("lifts a function with canonical option")
                    && error.contains("Aver lifts with none"),
                "{options}: {error}"
            );
        }
    }

    #[test]
    fn refuses_a_lower_with_an_encoding_aver_does_not_emit() {
        for encoding in ["string-encoding=utf16", "string-encoding=latin1+utf16"] {
            let bytes = component(&patched("").replace(
                "(canon lower (func $get) (memory $mem) (realloc $realloc))",
                &format!("(canon lower (func $get) (memory $mem) (realloc $realloc) {encoding})"),
            ));
            let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
            assert!(
                error.contains("lowers a function with canonical option")
                    && error.contains("Aver does not emit"),
                "{encoding}: {error}"
            );
        }
    }

    #[test]
    fn reports_the_lifted_core_exports() {
        let bytes = component(SHIMMED);
        let lifted =
            confirm_declared_core_binding(&bytes, module_ranges(&bytes)[0].clone()).unwrap();
        assert_eq!(lifted, ["wasi:cli/run@0.2.4#run"]);
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
        assert!(error.contains("passed as import \"m\""), "{error}");
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
        assert!(error.contains("re-bundled as \"memory\""), "{error}");
    }

    #[test]
    fn prints_core_names_from_the_component_escaped() {
        let bytes = component(
            r#"(component
            (core module $m
                (memory (export "memory") 1)
                (func (export "f") (result i32) i32.const 0))
            (core module $o (import "m" "x\nCERTIFIED" (memory 1)))
            (core instance $i (instantiate $m))
            (alias core export $i "memory" (core memory $mem))
            (core instance $bundle (export "x\nCERTIFIED" (memory $mem)))
            (core instance $oi (instantiate $o (with "m" (instance $bundle))))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            !error.contains('\n') && error.contains(r#"re-bundled as "x\nCERTIFIED""#),
            "{error:?}"
        );
    }

    /// The `wit-component` import shape, as Aver emits it: the main module
    /// imports `host.get` and `host.put` through the shim's trampolines (slots
    /// 0 and 1) and `host.now` as a direct lower; after main is instantiated,
    /// the fixup fills each slot with the host function lowered into main's
    /// memory. `{after}` is spliced in after the fixup instantiation.
    fn patched(after: &str) -> String {
        format!(
            r#"(component
            (import "host" (instance $hi
                (export "get" (func (result string)))
                (export "put" (func (result string)))
                (export "now" (func (result u32)))
                (export "later" (func (result u32)))))
            (alias export $hi "get" (func $get))
            (alias export $hi "put" (func $put))
            (alias export $hi "now" (func $now))
            (core module $main
                (import "host" "get" (func (param i32)))
                (import "host" "now" (func (result i32)))
                (import "host" "put" (func (param i32)))
                (memory (export "memory") 1)
                (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) i32.const 0)
                (func (export "f") (result i32) i32.const 0))
            (core module $shim
                (type $t (func (param i32)))
                (table 2 2 funcref)
                (export "0" (func 0))
                (export "1" (func 1))
                (export "$imports" (table 0))
                (func (type $t) local.get 0 i32.const 0 call_indirect (type $t))
                (func (type $t) local.get 0 i32.const 1 call_indirect (type $t)))
            (core module $fixup
                (import "" "0" (func (param i32)))
                (import "" "1" (func (param i32)))
                (import "" "$imports" (table 2 2 funcref))
                (elem (i32.const 0) func 0 1))
            (core module $helper
                (import "" "0" (func (param i32)))
                (func $s i32.const 0 call 0)
                (start $s))
            (core instance $si (instantiate $shim))
            (alias core export $si "0" (core func $t0))
            (alias core export $si "1" (core func $t1))
            (core func $nowl (canon lower (func $now)))
            (core instance $hostbundle
                (export "get" (func $t0)) (export "now" (func $nowl)) (export "put" (func $t1)))
            (core instance $main (instantiate $main (with "host" (instance $hostbundle))))
            (alias core export $main "memory" (core memory $mem))
            (alias core export $main "cabi_realloc" (core func $realloc))
            (alias core export $si "$imports" (core table $table))
            (core func $getl (canon lower (func $get) (memory $mem) (realloc $realloc)))
            (core func $putl (canon lower (func $put) (memory $mem) (realloc $realloc)))
            (core instance $args
                (export "$imports" (table $table)) (export "0" (func $getl)) (export "1" (func $putl)))
            (core instance $fix (instantiate $fixup (with "" (instance $args))))
            {after}
            (alias core export $main "f" (core func $f))
            (type $ft (func (result bool)))
            (func $lf (type $ft) (canon lift (core func $f)))
            (export "f" (func $lf)))"#
        )
    }

    #[test]
    fn refuses_helper_wiring_wit_component_does_not_emit() {
        let base = patched("");
        let swap = |from: &str, to: &str| {
            assert!(base.contains(from), "{from}");
            base.replace(from, to)
        };
        let args = r#"(export "$imports" (table $table)) (export "0" (func $getl)) (export "1" (func $putl))"#;
        let host =
            r#"(export "get" (func $t0)) (export "now" (func $nowl)) (export "put" (func $t1))"#;
        for (label, text, reason) in [
            (
                "fixup slots swapped",
                swap(
                    args,
                    r#"(export "$imports" (table $table)) (export "0" (func $putl)) (export "1" (func $getl))"#,
                ),
                r#"slot 0 is not given the lowered "host" "get""#,
            ),
            (
                "fixup slot given a trampoline",
                swap(
                    args,
                    r#"(export "$imports" (table $table)) (export "0" (func $t0)) (export "1" (func $putl))"#,
                ),
                "slot 0 is not given",
            ),
            (
                "trampolines out of slot order",
                swap(
                    host,
                    r#"(export "get" (func $t1)) (export "now" (func $nowl)) (export "put" (func $t0))"#,
                ),
                r#"import "host" "get" is not wired"#,
            ),
            (
                "direct lower of another function",
                swap(
                    "(core func $nowl (canon lower (func $now)))",
                    "(alias export $hi \"later\" (func $later))
                     (core func $nowl (canon lower (func $later)))",
                ),
                r#"import "host" "now" is not wired"#,
            ),
            (
                "no fixup",
                swap(
                    "(core instance $fix (instantiate $fixup (with \"\" (instance $args))))",
                    "",
                ),
                "no fixup fills its table",
            ),
            (
                "second fixup",
                swap(
                    "(core instance $fix (instantiate $fixup (with \"\" (instance $args))))",
                    "(core instance $fix (instantiate $fixup (with \"\" (instance $args))))
                     (core instance $fix2 (instantiate $fixup (with \"\" (instance $args))))",
                ),
                "instantiated more than once",
            ),
            (
                "second shim",
                swap(
                    "(core instance $si (instantiate $shim))",
                    "(core instance $si (instantiate $shim)) (core instance $si2 (instantiate $shim))",
                ),
                "shim module is instantiated more than once",
            ),
        ] {
            let bytes = component(&text);
            let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
            assert!(error.contains(reason), "{label}: {error}");
        }
    }

    #[test]
    fn accepts_a_table_patched_with_lowered_imports() {
        let bytes = component(&patched(""));
        let ranges = module_ranges(&bytes);
        confirm_declared_core_binding(&bytes, ranges[0].clone()).unwrap();
        // Declaring the shim instead: its trampolines are handed out as
        // the main module's imports.
        let error = refusal(&bytes, ranges[1].clone());
        assert!(error.contains("re-bundled as \"get\""), "{error}");
    }

    const NOT_A_HELPER: &str =
        "is neither the declared embedded core module nor a wit-component shim or fixup module";

    #[test]
    fn refuses_a_lowered_import_handed_to_a_module_with_code() {
        // Codex: a helper whose start calls the lowered function writes the
        // host's string through main's realloc into main's memory.
        let bytes = component(&patched(
            r#"(core instance $evil (instantiate $helper (with "" (instance $args))))"#,
        ));
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains(NOT_A_HELPER) && error.contains("it has a start function"),
            "{error}"
        );
    }

    #[test]
    fn refuses_a_helper_with_an_active_data_segment() {
        // Codex: an out-of-bounds data segment traps at instantiation, so the
        // component never runs the certified core.
        let bytes = component(&patched(
            r#"(core module $data (memory 0) (data (i32.const 0) "x"))
               (core instance $d (instantiate $data))"#,
        ));
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains(NOT_A_HELPER) && error.contains("it has data segments"),
            "{error}"
        );
    }

    #[test]
    fn refuses_helper_modules_outside_the_wit_component_shapes() {
        let shim = "(func (type $t) local.get 0 i32.const 1 call_indirect (type $t)))";
        let fixup = "(elem (i32.const 0) func 0 1))";
        let altered = |from: &str, to: &str, module: &str| {
            let base = patched("");
            assert!(base.contains(module) && module.contains(from), "{from}");
            base.replace(module, &module.replace(from, to))
        };
        // A broken shim is not a shim: its instantiation is refused, and its
        // table and trampolines are not the shim's, so the declared module's
        // wiring is refused first.
        let shim_refusal = "is not wired as wit-component wires it";
        for (label, text, reason) in [
            (
                "shim body does more",
                altered(
                    "i32.const 1 call_indirect",
                    "i32.const 1 drop i32.const 1 call_indirect",
                    shim,
                ),
                shim_refusal,
            ),
            (
                "shim calls another slot",
                altered(
                    "i32.const 1 call_indirect",
                    "i32.const 0 call_indirect",
                    shim,
                ),
                shim_refusal,
            ),
            (
                "shim with a start",
                altered("(type $t)))", "(type $t)) (func $go) (start $go))", shim),
                shim_refusal,
            ),
            (
                "fixup at another offset",
                altered("(i32.const 0)", "(i32.const 1)", fixup).replace("func 0 1)", "func 0)"),
                NOT_A_HELPER,
            ),
            (
                "fixup writes slots out of order",
                altered("func 0 1", "func 1 0", fixup),
                NOT_A_HELPER,
            ),
            (
                "fixup with a second segment",
                altered(
                    "(elem (i32.const 0) func 0 1))",
                    "(elem (i32.const 0) func 0 1) (elem (i32.const 0) func 0))",
                    fixup,
                ),
                NOT_A_HELPER,
            ),
            (
                "fixup with code",
                altered(
                    "(elem (i32.const 0) func 0 1))",
                    "(elem (i32.const 0) func 0 1) (func))",
                    fixup,
                ),
                NOT_A_HELPER,
            ),
            (
                "fixup with data",
                altered(
                    "(elem (i32.const 0) func 0 1))",
                    "(elem (i32.const 0) func 0 1) (memory 1) (data (i32.const 0) \"x\"))",
                    fixup,
                ),
                "it has",
            ),
        ] {
            let bytes = component(&text);
            let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
            assert!(error.contains(reason), "{label}: {error}");
        }
    }

    #[test]
    fn refuses_a_broken_shim_instantiated_on_its_own() {
        let bytes = component(&DIRECT.replace(
            "(export \"f\" (func $lf)))",
            "(core module $shim
                (type $t (func))
                (table 1 1 funcref)
                (export \"0\" (func 0))
                (export \"$imports\" (table 0))
                (func (type $t) i32.const 0 call_indirect (type $t))
                (start 0))
             (core instance $si (instantiate $shim))
             (export \"f\" (func $lf)))",
        ));
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains(NOT_A_HELPER) && error.contains("it has a start function"),
            "{error}"
        );
    }

    #[test]
    fn refuses_an_imported_core_module() {
        let bytes = component(
            r#"(component
            (import "m" (core module $m (export "f" (func (result i32)))))
            (core module $d (func (export "f") (result i32) i32.const 0))
            (core instance $i (instantiate $d))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("imports a core module"), "{error}");
    }

    #[test]
    fn refuses_exporting_a_core_export_under_another_name() {
        // Codex: the certified `greet` returns false; an uncertified `other`
        // returns true and is exported as `greet`.
        let bytes = component(
            r#"(component
            (core module $m
                (func (export "greet") (result i32) i32.const 0)
                (func (export "other") (result i32) i32.const 1))
            (core instance $i (instantiate $m))
            (alias core export $i "other" (core func $other))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $other)))
            (export "greet" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains("lifts core export \"other\"") && error.contains("must lift \"greet\""),
            "{error}"
        );
    }

    #[test]
    fn refuses_an_instance_field_lifted_from_another_core_export() {
        let bytes = component(
            &SHIMMED
                .replace(
                    r#"(alias core export $main "wasi:cli/run@0.2.4#run" (core func $run))"#,
                    r#"(alias core export $main "cabi_realloc_other" (core func $run))"#,
                )
                .replace(
                    r#"(func (export "cabi_post_run") (param i32)))"#,
                    r#"(func (export "cabi_post_run") (param i32))
            (func (export "cabi_realloc_other") (result i32) i32.const 1))"#,
                ),
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains("lifts core export \"cabi_realloc_other\"")
                && error.contains("must lift \"wasi:cli/run@0.2.4#run\""),
            "{error}"
        );
    }

    #[test]
    fn refuses_a_second_instance_of_the_declared_module() {
        // Codex: lift from instance A, memory option from instance B.
        let bytes = component(
            r#"(component
            (core module $m
                (memory (export "memory") 1)
                (func (export "f") (result i32) i32.const 0))
            (core instance $a (instantiate $m))
            (core instance $b (instantiate $m))
            (alias core export $b "memory" (core memory $mem))
            (alias core export $a "f" (core func $f))
            (type $t (func (result string)))
            (func $lf (type $t) (canon lift (core func $f) (memory $mem)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("instantiated more than once"), "{error}");
    }

    #[test]
    fn refuses_a_resource_with_a_destructor() {
        let bytes = component(
            r#"(component
            (core module $m
                (func (export "dtor") (param i32))
                (func (export "f") (result i32) i32.const 0))
            (core instance $i (instantiate $m))
            (alias core export $i "dtor" (core func $dtor))
            (type $r (resource (rep i32) (dtor (func $dtor))))
            (alias core export $i "f" (core func $f))
            (type $t (func (result bool)))
            (func $lf (type $t) (canon lift (core func $f)))
            (export "f" (func $lf)))"#,
        );
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(error.contains("resource with a destructor"), "{error}");
    }

    #[test]
    fn refuses_an_adapter_module_that_wraps_the_declared_export() {
        // Red team round 2: the declared `answer` (false) runs, but a second
        // module imports it, negates it, and its result is exported.
        let adapter = |wiring: &str| {
            format!(
                r#"(component
                (core module $m (func (export "answer") (result i32) i32.const 0))
                (core module $adapter
                    (import "m" "answer" (func $inner (result i32)))
                    (func (export "answer") (result i32) call $inner i32.eqz))
                (core instance $i (instantiate $m))
                {wiring}
                (alias core export $a "answer" (core func $f))
                (type $t (func (result bool)))
                (func $lf (type $t) (canon lift (core func $f)))
                (export "answer" (func $lf)))"#
            )
        };
        let bundled = component(&adapter(
            r#"(alias core export $i "answer" (core func $inner))
               (core instance $b (export "answer" (func $inner)))
               (core instance $a (instantiate $adapter (with "m" (instance $b))))"#,
        ));
        let error = refusal(&bundled, module_ranges(&bundled)[0].clone());
        assert!(error.contains("re-bundled as \"answer\""), "{error}");
        let direct = component(&adapter(
            r#"(core instance $a (instantiate $adapter (with "m" (instance $i))))"#,
        ));
        let error = refusal(&direct, module_ranges(&direct)[0].clone());
        assert!(error.contains("passed as import \"m\""), "{error}");
    }
}
