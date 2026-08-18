//! Dynamic Component Model adapter for configured Rust providers.
//!
//! The component owns canonical-ABI lowering and lifting. This host-side seam
//! sees ordinary WIT values, converts the deliberately small supported subset
//! to `ProviderValue`, and invokes the same contract-checked native registry
//! used by the VM and generated Rust.

use aver::codegen::wasip2::{CapabilityWitOperationPlan, CapabilityWitPlan, CapabilityWitType};
use aver::provider::{ProviderRegistry, ProviderValue};
use wasmtime::component::{Linker, Val};

pub(super) fn install<T>(
    linker: &mut Linker<T>,
    plan: &CapabilityWitPlan,
    providers: &ProviderRegistry,
) -> wasmtime::Result<()> {
    for interface in plan.interfaces() {
        let instance_name = format!("aver:user/{}", interface.interface_name);
        let mut instance = linker.instance(&instance_name)?;
        for operation in &interface.operations {
            let operation = operation.clone();
            let capability = interface.capability.clone();
            let providers = providers.clone();
            let wit_name = operation.wit_name.clone();
            instance.func_new(&wit_name, move |_store, _ty, params, results| {
                let args = decode_args(&operation, params)?;
                let value = providers
                    .invoke_provider_values(&operation.canonical_name, &args)
                    .map_err(wasmtime::Error::msg)?;
                encode_result(&providers, &capability, &operation, value, results)
            })?;
        }
    }
    Ok(())
}

fn decode_args(
    operation: &CapabilityWitOperationPlan,
    params: &[Val],
) -> wasmtime::Result<Vec<ProviderValue>> {
    let mut values = Vec::with_capacity(operation.params.len());
    let mut incoming = params.iter();
    for parameter in &operation.params {
        let value = match parameter.ty {
            CapabilityWitType::Unit => ProviderValue::Unit,
            CapabilityWitType::Bool => match incoming.next() {
                Some(Val::Bool(value)) => ProviderValue::Bool(*value),
                other => return Err(argument_shape_error(operation, parameter.index, other)),
            },
            CapabilityWitType::F64 => match incoming.next() {
                Some(Val::Float64(value)) => ProviderValue::Float(*value),
                other => return Err(argument_shape_error(operation, parameter.index, other)),
            },
            CapabilityWitType::String => match incoming.next() {
                Some(Val::String(value)) => ProviderValue::String(value.clone()),
                other => return Err(argument_shape_error(operation, parameter.index, other)),
            },
        };
        values.push(value);
    }
    if incoming.next().is_some() {
        return Err(wasmtime::Error::msg(format!(
            "error[capability-provider-adapter-arguments]: component supplied too many WIT arguments for '{}'",
            operation.canonical_name
        )));
    }
    Ok(values)
}

fn argument_shape_error(
    operation: &CapabilityWitOperationPlan,
    index: usize,
    received: Option<&Val>,
) -> wasmtime::Error {
    wasmtime::Error::msg(format!(
        "error[capability-provider-adapter-arguments]: component supplied an invalid WIT value for '{}' parameter {}: expected {}, received {}",
        operation.canonical_name,
        index,
        operation.params[index].ty.wit_name().unwrap_or("unit"),
        received.map(component_shape).unwrap_or("<missing>")
    ))
}

fn encode_result(
    providers: &ProviderRegistry,
    capability: &str,
    operation: &CapabilityWitOperationPlan,
    value: ProviderValue,
    results: &mut [Val],
) -> wasmtime::Result<()> {
    let received = value.shape();
    let encoded = match (operation.result, value) {
        (CapabilityWitType::Unit, ProviderValue::Unit) => None,
        (CapabilityWitType::Bool, ProviderValue::Bool(value)) => Some(Val::Bool(value)),
        (CapabilityWitType::F64, ProviderValue::Float(value)) => Some(Val::Float64(value)),
        (CapabilityWitType::String, ProviderValue::String(value)) => Some(Val::String(value)),
        (expected, _) => {
            let provider = providers
                .binding(capability)
                .map(|binding| binding.provider_identity())
                .unwrap_or("<missing>");
            return Err(wasmtime::Error::msg(format!(
                "error[capability-provider-invalid-return]: provider '{provider}' returned an invalid value for '{}': expected {}, received {received}",
                operation.canonical_name,
                expected.wit_name().unwrap_or("Unit")
            )));
        }
    };

    match (encoded, results) {
        (None, []) => Ok(()),
        (Some(value), [slot]) => {
            *slot = value;
            Ok(())
        }
        (None, slots) => Err(result_slot_error(operation, 0, slots.len())),
        (Some(_), slots) => Err(result_slot_error(operation, 1, slots.len())),
    }
}

fn result_slot_error(
    operation: &CapabilityWitOperationPlan,
    expected: usize,
    received: usize,
) -> wasmtime::Error {
    wasmtime::Error::msg(format!(
        "error[capability-provider-adapter-result]: component allocated {received} result slot(s) for '{}', expected {expected}",
        operation.canonical_name
    ))
}

fn component_shape(value: &Val) -> &'static str {
    match value {
        Val::Bool(_) => "bool",
        Val::Float64(_) => "f64",
        Val::String(_) => "string",
        _ => "unsupported component value",
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use aver::capability::CapabilityRegistry;
    use aver::codegen::wasip2::CapabilityWitInterfacePlan;
    use aver::provider::{
        CapabilityProvider, ProviderBinding, ProviderContext, ProviderFault, ProviderValue,
    };

    use super::*;

    enum Failure {
        WrongShape,
        Fault,
        Panic,
    }

    struct FailingProvider(Failure);

    impl CapabilityProvider for FailingProvider {
        fn identity(&self) -> &str {
            "test.wit-failure@1"
        }

        fn fingerprint(&self) -> &str {
            "wit-failure-v1"
        }

        fn invoke(
            &self,
            _context: &ProviderContext,
            _args: &[ProviderValue],
        ) -> Result<ProviderValue, ProviderFault> {
            match self.0 {
                Failure::WrongShape => Ok(ProviderValue::String("private wrong value".into())),
                Failure::Fault => Err(ProviderFault::new("denied", "provider said no")),
                Failure::Panic => panic!("private provider panic"),
            }
        }
    }

    fn registry(failure: Failure) -> (ProviderRegistry, CapabilityWitOperationPlan) {
        let items = aver::source::parse_source(
            "module Probe\n    kind = capability\n    semantics = pure\n    exposes [read]\n\noperation read() -> Bool\n",
        )
        .unwrap();
        let (contracts, errors) = CapabilityRegistry::from_module("Probe", &items);
        assert!(errors.is_empty());
        let contract = contracts.contract("Probe").unwrap().clone();
        let operation = CapabilityWitInterfacePlan::build(&contracts, &contract)
            .unwrap()
            .operations
            .remove(0);
        let providers = ProviderRegistry::for_program_with_bindings(
            contracts,
            [ProviderBinding::new(
                "Probe",
                contract.contract_hash,
                ["Probe.read"],
                Arc::new(FailingProvider(failure)),
            )],
        )
        .unwrap();
        (providers, operation)
    }

    #[test]
    fn invalid_provider_return_is_attributed_before_wasmtime_type_checks_it() {
        let (providers, operation) = registry(Failure::WrongShape);
        let value = providers.invoke_provider_values("Probe.read", &[]).unwrap();
        let error = encode_result(
            &providers,
            "Probe",
            &operation,
            value,
            &mut [Val::Bool(false)],
        )
        .unwrap_err()
        .to_string();
        assert!(error.contains("error[capability-provider-invalid-return]"));
        assert!(error.contains("test.wit-failure@1"));
        assert!(error.contains("expected bool, received String"));
        assert!(!error.contains("private wrong value"));
    }

    #[test]
    fn provider_fault_and_panic_keep_the_native_boundary_diagnostics() {
        let (faulting, _) = registry(Failure::Fault);
        let fault = faulting
            .invoke_provider_values("Probe.read", &[])
            .unwrap_err();
        assert!(fault.contains("error[capability-provider-fault]"));
        assert!(fault.contains("denied: provider said no"));

        let (panicking, _) = registry(Failure::Panic);
        let panic = panicking
            .invoke_provider_values("Probe.read", &[])
            .unwrap_err();
        assert!(panic.contains("error[capability-provider-panic]"));
        assert!(panic.contains("private provider panic"));
    }
}
