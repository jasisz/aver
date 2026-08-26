//! Contract-derived host-import plan for program-defined capabilities.
//!
//! This is ABI metadata, not a provider implementation. A selected complete
//! contract becomes one hashed import namespace and one function import per
//! operation. An external embedder supplies those functions directly; the
//! stock CLI can also adapt a configured Rust provider through the same ABI.

use std::collections::{BTreeSet, HashSet};

use crate::ast::Type;
use crate::capability::{CapabilityContract, CapabilityOperation, CapabilityRegistry};

#[derive(Debug, Clone, PartialEq)]
pub struct CapabilityWasmGcOperationPlan {
    pub operation: CapabilityOperation,
    pub import_name: String,
    /// Boundary types with capability-owned nominal names made canonical.
    /// The source contract may say `Token`; the linked ABI says
    /// `Vault.Token`, so an unrelated local `Token` can never become an
    /// `externref` by name collision.
    pub abi_params: Vec<Type>,
    pub abi_result: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CapabilityWasmGcInterfacePlan {
    pub capability: String,
    pub contract_hash: String,
    pub model_hash: String,
    pub interface_name: String,
    pub operations: Vec<CapabilityWasmGcOperationPlan>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct CapabilityWasmGcPlan {
    interfaces: Vec<CapabilityWasmGcInterfacePlan>,
    resource_types: BTreeSet<String>,
    named_boundary_types: BTreeSet<String>,
    force_bignum: bool,
}

impl CapabilityWasmGcPlan {
    /// Select only capabilities reached by the program. Selecting one
    /// operation retains the whole sorted contract so a host adapter is bound
    /// to one complete interface rather than a call-site-dependent fragment.
    pub fn build(
        registry: &CapabilityRegistry,
        required_operations: &BTreeSet<String>,
    ) -> Result<Self, String> {
        let mut required_capabilities = BTreeSet::new();
        for name in required_operations {
            let operation = registry.operation(name).ok_or_else(|| {
                format!("wasm-gc capability plan received unknown operation `{name}`")
            })?;
            required_capabilities.insert(operation.module.clone());
        }

        let mut interfaces = Vec::new();
        for capability in required_capabilities {
            let contract = registry
                .contract(&capability)
                .expect("required operation has an owning contract");
            if is_canonical_standard_capability(contract) {
                continue;
            }
            let mut operations = registry
                .operations()
                .filter(|operation| operation.module == capability)
                .map(|operation| {
                    let abi_params = operation
                        .params
                        .iter()
                        .map(|(_, ty)| qualify_boundary_type(ty, &capability, registry))
                        .collect();
                    let abi_result =
                        qualify_boundary_type(&operation.return_type, &capability, registry);
                    CapabilityWasmGcOperationPlan {
                        operation: operation.clone(),
                        import_name: format!(
                            "op-{}",
                            crate::codegen::wasip2::plan::encode_interface_identifier(
                                &operation.name
                            )
                        ),
                        abi_params,
                        abi_result,
                    }
                })
                .collect::<Vec<_>>();
            operations.sort_by(|left, right| {
                left.operation
                    .canonical_name
                    .cmp(&right.operation.canonical_name)
            });
            interfaces.push(CapabilityWasmGcInterfacePlan {
                capability: capability.clone(),
                contract_hash: contract.contract_hash.clone(),
                model_hash: contract.model_hash.clone(),
                interface_name: crate::codegen::wasip2::plan::capability_interface_name(contract),
                operations,
            });
        }

        let selected_capabilities = interfaces
            .iter()
            .map(|interface| interface.capability.as_str())
            .collect::<BTreeSet<_>>();
        let mut resource_types = BTreeSet::new();
        for resource in registry.resource_types() {
            let Some((owner, _)) = resource.rsplit_once('.') else {
                continue;
            };
            if !selected_capabilities.contains(owner) {
                continue;
            }
            resource_types.insert(resource.clone());
        }
        let mut named_boundary_types = BTreeSet::new();
        for interface in &interfaces {
            for operation in &interface.operations {
                for ty in &operation.abi_params {
                    collect_named_types(
                        ty,
                        registry,
                        &interface.capability,
                        &mut named_boundary_types,
                        &mut HashSet::new(),
                    );
                }
                collect_named_types(
                    &operation.abi_result,
                    registry,
                    &interface.capability,
                    &mut named_boundary_types,
                    &mut HashSet::new(),
                );
            }
        }
        let force_bignum = interfaces.iter().any(|interface| {
            interface.operations.iter().any(|operation| {
                operation.abi_params.iter().any(|ty| {
                    type_contains_int(ty, registry, &interface.capability, &mut HashSet::new())
                }) || type_contains_int(
                    &operation.abi_result,
                    registry,
                    &interface.capability,
                    &mut HashSet::new(),
                )
            })
        });

        Ok(Self {
            interfaces,
            resource_types,
            named_boundary_types,
            force_bignum,
        })
    }

    pub fn interfaces(&self) -> &[CapabilityWasmGcInterfacePlan] {
        &self.interfaces
    }

    pub fn resource_types(&self) -> &BTreeSet<String> {
        &self.resource_types
    }

    pub fn force_bignum(&self) -> bool {
        self.force_bignum
    }

    pub fn named_boundary_types(&self) -> &BTreeSet<String> {
        &self.named_boundary_types
    }

    pub fn boundary_type_strings(&self) -> Vec<String> {
        let mut types = self
            .interfaces
            .iter()
            .flat_map(|interface| &interface.operations)
            .flat_map(|operation| {
                operation
                    .abi_params
                    .iter()
                    .map(Type::display)
                    .chain(std::iter::once(operation.abi_result.display()))
                    // Linked source bodies retain the contract's local
                    // spelling (`Token`) while the external ABI uses the
                    // collision-proof canonical spelling (`Vault.Token`).
                    // Register both structural wrappers; the resource alias
                    // itself is installed only when the bare name is safe.
                    .chain(
                        operation
                            .operation
                            .params
                            .iter()
                            .map(|(_, ty)| ty.display()),
                    )
                    .chain(std::iter::once(operation.operation.return_type.display()))
            })
            .collect::<Vec<_>>();
        if self.force_bignum {
            // The JS host's full-ℤ bridge parses decimal through the ordinary
            // fail-closed `Int.fromString : Result<Int,String>` carrier.
            types.push("Result<Int,String>".to_string());
        }
        types
    }
}

fn qualify_boundary_type(ty: &Type, owner: &str, registry: &CapabilityRegistry) -> Type {
    match ty {
        Type::Result(ok, err) => Type::Result(
            Box::new(qualify_boundary_type(ok, owner, registry)),
            Box::new(qualify_boundary_type(err, owner, registry)),
        ),
        Type::Option(inner) => {
            Type::Option(Box::new(qualify_boundary_type(inner, owner, registry)))
        }
        Type::List(inner) => Type::List(Box::new(qualify_boundary_type(inner, owner, registry))),
        Type::Vector(inner) => {
            Type::Vector(Box::new(qualify_boundary_type(inner, owner, registry)))
        }
        Type::Map(key, value) => Type::Map(
            Box::new(qualify_boundary_type(key, owner, registry)),
            Box::new(qualify_boundary_type(value, owner, registry)),
        ),
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| qualify_boundary_type(item, owner, registry))
                .collect(),
        ),
        Type::Fn(params, result, effects) => Type::Fn(
            params
                .iter()
                .map(|param| qualify_boundary_type(param, owner, registry))
                .collect(),
            Box::new(qualify_boundary_type(result, owner, registry)),
            effects.clone(),
        ),
        // syntax-discovery-only: contract boundary annotations are parsed
        // before backend identity stamping; this walk qualifies source names.
        Type::Named { name, .. } => {
            let canonical = if name.contains('.') {
                name.clone()
            } else {
                format!("{owner}.{name}")
            };
            let owned = registry.boundary_type(&canonical).is_some()
                || registry
                    .resource_types()
                    .any(|resource| resource == &canonical);
            if owned {
                Type::named(canonical)
            } else {
                ty.clone()
            }
        }
        _ => ty.clone(),
    }
}

fn is_canonical_standard_capability(contract: &CapabilityContract) -> bool {
    if !crate::stdlib::is_standard_capability(&contract.module) {
        return false;
    }
    crate::stdlib::standard_capability_registry()
        .contract(&contract.module)
        .is_some_and(|standard| {
            contract.contract_hash == standard.contract_hash
                && contract.model_hash == standard.model_hash
        })
}

fn type_contains_int(
    ty: &Type,
    registry: &CapabilityRegistry,
    owner: &str,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Int => true,
        Type::Result(ok, err) | Type::Map(ok, err) => {
            type_contains_int(ok, registry, owner, visiting)
                || type_contains_int(err, registry, owner, visiting)
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_contains_int(inner, registry, owner, visiting)
        }
        Type::Tuple(items) => items
            .iter()
            .any(|item| type_contains_int(item, registry, owner, visiting)),
        Type::Fn(params, result, _) => {
            params
                .iter()
                .any(|param| type_contains_int(param, registry, owner, visiting))
                || type_contains_int(result, registry, owner, visiting)
        }
        // syntax-discovery-only: inspect the complete source contract shape to
        // decide whether its raw wasm-gc ABI needs the full-Int prelude.
        Type::Named { name, .. } => {
            if !visiting.insert(name.clone()) {
                return false;
            }
            let contains =
                boundary_definition(registry, owner, name).is_some_and(
                    |definition| match definition {
                        crate::ast::TypeDef::Product { fields, .. } => {
                            fields.iter().any(|(_, field)| {
                                type_contains_int(
                                    &crate::types::parse_type_str(field),
                                    registry,
                                    owner,
                                    visiting,
                                )
                            })
                        }
                        crate::ast::TypeDef::Sum { variants, .. } => {
                            variants.iter().any(|variant| {
                                variant.fields.iter().any(|field| {
                                    type_contains_int(
                                        &crate::types::parse_type_str(field),
                                        registry,
                                        owner,
                                        visiting,
                                    )
                                })
                            })
                        }
                    },
                );
            visiting.remove(name);
            contains
        }
        Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Var(_) | Type::Invalid => false,
    }
}

fn boundary_definition<'a>(
    registry: &'a CapabilityRegistry,
    owner: &str,
    name: &str,
) -> Option<&'a crate::ast::TypeDef> {
    registry
        .boundary_type(&format!("{owner}.{name}"))
        .or_else(|| registry.boundary_type(name))
        .or_else(|| {
            let mut matches = registry
                .boundary_types()
                .filter(|(canonical, _)| canonical.ends_with(&format!(".{name}")))
                .map(|(_, definition)| definition);
            let first = matches.next()?;
            matches.next().is_none().then_some(first)
        })
}

fn collect_named_types(
    ty: &Type,
    registry: &CapabilityRegistry,
    owner: &str,
    out: &mut BTreeSet<String>,
    visiting: &mut HashSet<String>,
) {
    match ty {
        Type::Result(left, right) | Type::Map(left, right) => {
            collect_named_types(left, registry, owner, out, visiting);
            collect_named_types(right, registry, owner, out, visiting);
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            collect_named_types(inner, registry, owner, out, visiting)
        }
        Type::Tuple(items) => {
            for item in items {
                collect_named_types(item, registry, owner, out, visiting);
            }
        }
        Type::Fn(params, result, _) => {
            for param in params {
                collect_named_types(param, registry, owner, out, visiting);
            }
            collect_named_types(result, registry, owner, out, visiting);
        }
        // syntax-discovery-only: collect nominal spellings from source-owned
        // contract definitions before the wasm-gc backend link stage.
        Type::Named { name, .. } if visiting.insert(name.clone()) => {
            out.insert(name.clone());
            if let Some(definition) = boundary_definition(registry, owner, name) {
                match definition {
                    crate::ast::TypeDef::Product {
                        name: canonical,
                        fields,
                        ..
                    } => {
                        out.insert(canonical.clone());
                        for (_, field) in fields {
                            collect_named_types(
                                &crate::types::parse_type_str(field),
                                registry,
                                owner,
                                out,
                                visiting,
                            );
                        }
                    }
                    crate::ast::TypeDef::Sum {
                        name: canonical,
                        variants,
                        ..
                    } => {
                        out.insert(canonical.clone());
                        for variant in variants {
                            for field in &variant.fields {
                                collect_named_types(
                                    &crate::types::parse_type_str(field),
                                    registry,
                                    owner,
                                    out,
                                    visiting,
                                );
                            }
                        }
                    }
                }
            }
            visiting.remove(name);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plan_keeps_the_complete_contract_and_hashes_the_namespace() {
        let items = crate::source::parse_source(
            "module Vault\n    kind = capability\n    semantics = pure\n    exposes [open, count]\n\nresource Handle\n\noperation open(name: String) -> Handle\n\noperation count(handle: Handle) -> Int\n",
        )
        .expect("parse capability");
        let (registry, errors) = CapabilityRegistry::from_module("Vault", &items);
        assert!(errors.is_empty(), "capability errors: {errors:?}");
        let required = ["Vault.open".to_string()].into_iter().collect();
        let plan = CapabilityWasmGcPlan::build(&registry, &required).expect("wasm-gc plan");
        let interface = &plan.interfaces()[0];
        assert!(interface.interface_name.starts_with("cap-n5661756c74-c"));
        assert_eq!(
            interface
                .operations
                .iter()
                .map(|operation| operation.operation.canonical_name.as_str())
                .collect::<Vec<_>>(),
            ["Vault.count", "Vault.open"]
        );
        assert!(plan.resource_types().contains("Vault.Handle"));
        assert_eq!(
            interface
                .operations
                .iter()
                .find(|operation| operation.operation.name == "count")
                .expect("count operation")
                .abi_params[0]
                .display(),
            "Vault.Handle"
        );
        assert!(plan.force_bignum());
    }
}
