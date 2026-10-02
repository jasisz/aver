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
//!    `call_indirect`; it is instantiated with no arguments. The fixup imports
//!    functions `"" "0"` to `"" "N-1"` and the N-slot table `"" "$imports"`,
//!    and has one active element segment writing import `i` to slot `i` and
//!    nothing else; it is instantiated with one bundle of a shim's `$imports`
//!    table and N `canon lower` functions under `"0"` to `"N-1"`. A start
//!    function, a data segment, a memory, a global, a table initializer, any
//!    other function body or element segment, or an imported or aliased core
//!    module is refused;
//! 6. a nested component is only a re-export shim: it imports functions and
//!    types and exports those same imports, with no code, instances or
//!    canonical functions of its own.
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
    /// The `wit-component` shim module.
    Shim,
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
    /// An instance of the shim module.
    Shim,
    /// A `from_exports` bundle in the shape the fixup module takes: a shim's
    /// `$imports` table and this many lowered functions.
    FixupArgs(u32),
    /// Any other instance.
    Plain,
}

/// A core item (function, table, memory, global or tag).
#[derive(Clone, PartialEq, Eq)]
enum CoreItem {
    /// The export of this name of the declared instance.
    Declared(String),
    /// The `$imports` table of a shim instance.
    ShimTable,
    /// A `canon lower` function.
    Lowered,
    Plain,
}

/// What a nested component is. Only re-export shims are understood.
enum NestedComponent {
    /// Pairs of (export name, import name) for each re-exported function.
    Shim(Vec<(String, String)>),
    /// An imported or aliased component, whose behavior is unknown.
    Opaque,
}

/// A component function: the declared instance's core export it lifts, if
/// it is such a lift.
type Lifted = Option<String>;

/// A component instance. `Built` instances list their function fields with
/// the core export each one lifts.
enum ComponentInstanceOrigin {
    Built {
        funcs: Vec<(String, Lifted)>,
        other_fields: bool,
    },
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
    components: Vec<NestedComponent>,
    declared_instantiations: usize,
    /// The first component export not lifted from the declared module under
    /// its own name. It is reported after the checks on the declaration.
    unbound_export: Option<String>,
    /// The first instantiation of a module that is neither the declared one
    /// nor a helper. It is reported after the unbound export.
    helper_refusal: Option<String>,
    /// The declared instance's core exports that a `canon lift` exposes.
    lifted: Vec<String>,
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
    match top.unbound_export.or(top.helper_refusal) {
        Some(error) => Err(error),
        None => Ok(top.lifted),
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
                match at(&self.core_modules, module_index, "core module")?.clone() {
                    CoreModule::Declared => {
                        self.declared_instantiations += 1;
                        if self.declared_instantiations > 1 {
                            return Err(
                                "declared embedded core module is instantiated more than once"
                                    .to_string(),
                            );
                        }
                        CoreInstance::Declared
                    }
                    CoreModule::Shim => {
                        if !args.is_empty() {
                            return Err(
                                "the wit-component shim module is instantiated with arguments"
                                    .to_string(),
                            );
                        }
                        CoreInstance::Shim
                    }
                    CoreModule::Fixup(slots) => {
                        let given = match &*args {
                            [arg] if arg.name.is_empty() => {
                                Some(*at(&self.core_instances, arg.index, "core instance")?)
                            }
                            _ => None,
                        };
                        if given != Some(CoreInstance::FixupArgs(slots)) {
                            return Err(format!(
                                "the wit-component fixup module is not instantiated with exactly one bundle of a shim's `$imports` table and {slots} lowered functions"
                            ));
                        }
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
                            "an export of the declared embedded core module is re-bundled as `{}` into another core instance",
                            export.name
                        ));
                    }
                }
                self.fixup_args(&exports)
                    .map_or(CoreInstance::Plain, CoreInstance::FixupArgs)
            }
        };
        self.core_instances.push(origin);
        Ok(())
    }

    /// The slot count, when `exports` is the bundle the fixup module takes:
    /// a shim's `$imports` table, then lowered functions `"0"`, `"1"`, ...
    fn fixup_args(&self, exports: &[wasmparser::Export<'_>]) -> Option<u32> {
        let item = |export: &wasmparser::Export<'_>| {
            self.core_items[core_space(export.kind)].get(usize::try_from(export.index).ok()?)
        };
        let (table, funcs) = exports.split_first()?;
        let shape = table.name == "$imports"
            && table.kind == ExternalKind::Table
            && item(table) == Some(&CoreItem::ShimTable)
            && !funcs.is_empty()
            && funcs.iter().enumerate().all(|(slot, func)| {
                func.name == slot.to_string()
                    && func.kind == ExternalKind::Func
                    && item(func) == Some(&CoreItem::Lowered)
            });
        if shape {
            u32::try_from(funcs.len()).ok()
        } else {
            None
        }
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
                    CoreInstance::Shim if kind == ExternalKind::Table => CoreItem::ShimTable,
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
                                .and_then(|(_, lifted)| lifted.clone()),
                            ComponentInstanceOrigin::Opaque => None,
                        };
                        self.funcs.push(lifted);
                    }
                    ComponentExternalKind::Instance => {
                        self.instances.push(ComponentInstanceOrigin::Opaque)
                    }
                    ComponentExternalKind::Component => {
                        self.components.push(NestedComponent::Opaque)
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
                    CoreItem::Declared(name) => Some(name.clone()),
                    _ => None,
                };
                if let Some(name) = &lifted {
                    self.lifted.push(name.clone());
                }
                self.funcs.push(lifted);
            }
            CanonicalFunction::Lower { options, .. } => {
                self.check_lower_options(&options)?;
                self.core_items[FUNC_SPACE].push(CoreItem::Lowered);
            }
            CanonicalFunction::ResourceNew { .. }
            | CanonicalFunction::ResourceDrop { .. }
            | CanonicalFunction::ResourceRep { .. } => {
                self.core_items[FUNC_SPACE].push(CoreItem::Plain)
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

    fn add_import(&mut self, ty: ComponentTypeRef) -> Result<(), String> {
        match ty {
            ComponentTypeRef::Module(_) => {
                return Err(
                    "wasip2 component imports a core module, which this checker does not admit"
                        .to_string(),
                );
            }
            ComponentTypeRef::Func(_) => self.funcs.push(None),
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
                            at(&self.funcs, arg.index, "component func")?.clone(),
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
            Some(lifted) if lifted == core_name => return,
            Some(lifted) => format!(
                "component export `{shown}` lifts core export `{lifted}` of the declared embedded core module; it must lift `{core_name}`"
            ),
            None => format!(
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
            CoreModule::Shim
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

    /// The `wit-component` shape: a helper module instantiated beside the
    /// main one, the world export lifted from the main module and handed out
    /// through a re-export shim component with imported and exported types.
    const SHIMMED: &str = r#"(component
        (core module $main
            (memory (export "memory") 1)
            (func (export "wasi:cli/run@0.2.4#run") (result i32) i32.const 0)
            (func (export "cabi_post_run") (param i32)))
        (core module $helper
            (type (func))
            (table 1 1 funcref)
            (export "0" (func 0))
            (export "$imports" (table 0))
            (func (type 0) i32.const 0 call_indirect (type 0)))
        (core instance $h (instantiate $helper))
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
        // The helper module is instantiated but nothing is lifted from it.
        let helper = refusal(&bytes, ranges[1].clone());
        assert!(
            helper.contains("is not lifted from the declared"),
            "{helper}"
        );
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
                "(canon lower (func $host) (memory $mem) (realloc $realloc))",
                &format!("(canon lower (func $host) (memory $mem) (realloc $realloc) {encoding})"),
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

    /// The `wit-component` import shape: main imports a host function through
    /// a shim module's table, and a fixup module (no functions, no start)
    /// fills that table with the host function lowered into main's memory.
    /// `{after}` is spliced in after the fixup instantiation.
    fn patched(after: &str) -> String {
        format!(
            r#"(component
            (import "host" (func $host (result string)))
            (core module $main
                (import "host" "get" (func (param i32)))
                (memory (export "memory") 1)
                (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) i32.const 0)
                (func (export "f") (result i32) i32.const 0))
            (core module $shim
                (type $t (func (param i32)))
                (table 1 1 funcref)
                (export "0" (func 0))
                (export "$imports" (table 0))
                (func (type $t) local.get 0 i32.const 0 call_indirect (type $t)))
            (core module $fixup
                (import "" "0" (func (param i32)))
                (import "" "$imports" (table 1 1 funcref))
                (elem (i32.const 0) func 0))
            (core module $helper
                (import "" "0" (func (param i32)))
                (func $s i32.const 0 call 0)
                (start $s))
            (core instance $si (instantiate $shim))
            (alias core export $si "0" (core func $indirect))
            (core instance $hostbundle (export "get" (func $indirect)))
            (core instance $main (instantiate $main (with "host" (instance $hostbundle))))
            (alias core export $main "memory" (core memory $mem))
            (alias core export $main "cabi_realloc" (core func $realloc))
            (alias core export $si "$imports" (core table $table))
            (core func $lowered (canon lower (func $host) (memory $mem) (realloc $realloc)))
            (core instance $args (export "$imports" (table $table)) (export "0" (func $lowered)))
            (core instance $fix (instantiate $fixup (with "" (instance $args))))
            {after}
            (alias core export $main "f" (core func $f))
            (type $ft (func (result bool)))
            (func $lf (type $ft) (canon lift (core func $f)))
            (export "f" (func $lf)))"#
        )
    }

    #[test]
    fn accepts_a_table_patched_with_lowered_imports() {
        let bytes = component(&patched(""));
        confirm_declared_core_binding(&bytes, module_ranges(&bytes)[0].clone()).unwrap();
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
        // SHIMMED instantiates a lone shim module; `patched` a shim and fixup.
        let shim = r#"(core module $helper
            (type (func))
            (table 1 1 funcref)
            (export "0" (func 0))
            (export "$imports" (table 0))
            (func (type 0) i32.const 0 call_indirect (type 0)))"#;
        let fixup = r#"(core module $fixup
                (import "" "0" (func (param i32)))
                (import "" "$imports" (table 1 1 funcref))
                (elem (i32.const 0) func 0))"#;
        let altered = |from: &str, to: &str, module: &str| {
            let base = if module == shim {
                SHIMMED.to_string()
            } else {
                patched("")
            };
            assert!(base.contains(module) && module.contains(from), "{from}");
            base.replace(module, &module.replace(from, to))
        };
        for (label, text, reason) in [
            (
                "shim body does more",
                altered(
                    "i32.const 0 call_indirect",
                    "i32.const 0 drop i32.const 0 call_indirect",
                    shim,
                ),
                NOT_A_HELPER,
            ),
            (
                "shim calls another slot",
                altered(
                    "i32.const 0 call_indirect",
                    "i32.const 1 call_indirect",
                    shim,
                )
                .replace("(table 1 1 funcref)", "(table 2 2 funcref)"),
                NOT_A_HELPER,
            ),
            (
                "shim with a start",
                altered(
                    "(export \"$imports\" (table 0))",
                    "(export \"$imports\" (table 0)) (start 1) (func)",
                    shim,
                ),
                "it has a start function",
            ),
            (
                "fixup at another offset",
                altered(
                    "(elem (i32.const 0) func 0)",
                    "(elem (i32.const 1) func 0)",
                    fixup,
                )
                .replace("(table 1 1 funcref)", "(table 2 2 funcref)"),
                NOT_A_HELPER,
            ),
            (
                "fixup with a second segment",
                altered(
                    "(elem (i32.const 0) func 0)",
                    "(elem (i32.const 0) func 0) (elem (i32.const 0) func 0)",
                    fixup,
                ),
                NOT_A_HELPER,
            ),
            (
                "fixup with code",
                altered(
                    "(elem (i32.const 0) func 0)",
                    "(elem (i32.const 0) func 0) (func)",
                    fixup,
                ),
                NOT_A_HELPER,
            ),
            (
                "fixup with data",
                altered(
                    "(elem (i32.const 0) func 0)",
                    "(elem (i32.const 0) func 0) (memory 1) (data (i32.const 0) \"x\")",
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
    fn refuses_a_fixup_that_writes_something_other_than_lowered_imports() {
        // The fixup's slot gets the shim's own trampoline instead of the
        // lowered import, and a fixup given no bundle at all.
        let bytes = component(&patched("").replace(
            r#"(core instance $args (export "$imports" (table $table)) (export "0" (func $lowered)))"#,
            r#"(core instance $args (export "$imports" (table $table)) (export "0" (func $indirect)))"#,
        ));
        let error = refusal(&bytes, module_ranges(&bytes)[0].clone());
        assert!(
            error.contains("fixup module is not instantiated with exactly"),
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
            error.contains("lifts core export `other`") && error.contains("must lift `greet`"),
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
            error.contains("lifts core export `cabi_realloc_other`")
                && error.contains("must lift `wasi:cli/run@0.2.4#run`"),
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
        assert!(error.contains("re-bundled as `answer`"), "{error}");
        let direct = component(&adapter(
            r#"(core instance $a (instantiate $adapter (with "m" (instance $i))))"#,
        ));
        let error = refusal(&direct, module_ranges(&direct)[0].clone());
        assert!(error.contains("passed as import `m`"), "{error}");
    }
}
