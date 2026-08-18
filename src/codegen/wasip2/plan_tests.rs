use super::*;

fn registry(source: &str) -> CapabilityRegistry {
    registry_for("Echo", source)
}

fn registry_for(module: &str, source: &str) -> CapabilityRegistry {
    let items = crate::source::parse_source(source).expect("parse capability");
    let (registry, errors) = CapabilityRegistry::from_module(module, &items);
    assert!(errors.is_empty(), "capability errors: {errors:?}");
    registry
}

fn unsupported(source: &str) -> CapabilityWitUnsupported {
    let registry = registry(source);
    let contract = registry.contract("Echo").expect("Echo contract");
    CapabilityWitInterfacePlan::build(&registry, contract)
        .expect_err("contract must be outside the phase-3a subset")
}

const ECHO: &str = "\
module Echo
    kind = capability
    semantics = effectful
    exposes [echo, healthy, ratio, ping]

operation echo(value: String) -> String
    oracle = generative
    replay = recorded

operation healthy() -> Bool
    oracle = generative
    replay = recorded

operation ratio(value: Float) -> Float
    oracle = generative
    replay = recorded

operation ping(marker: Unit) -> Unit
    oracle = generative
    replay = recorded
";

#[test]
fn required_operation_selects_the_full_sorted_contract() {
    let registry = registry(ECHO);
    let required = ["Echo.echo".to_string()].into_iter().collect();
    let plan = CapabilityWitPlan::build(&registry, &required).expect("WIT plan");
    let interface = &plan.interfaces()[0];
    assert_eq!(interface.capability, "Echo");
    assert!(interface.interface_name.starts_with("cap-n4563686f-c"));
    assert_eq!(interface.interface_name.len(), "cap-n4563686f-c".len() + 64);
    assert_eq!(
        interface
            .operations
            .iter()
            .map(|operation| operation.canonical_name.as_str())
            .collect::<Vec<_>>(),
        ["Echo.echo", "Echo.healthy", "Echo.ping", "Echo.ratio"]
    );
    assert_eq!(interface.operations[0].wit_name, "op-n6563686f");
    assert_eq!(interface.operations[0].params[0].index, 0);
    assert_eq!(
        interface.operations[0].params[0].ty,
        CapabilityWitType::String
    );
    assert_eq!(interface.operations[2].result, CapabilityWitType::Unit);
}

#[test]
fn unused_contract_emits_no_interface() {
    let plan = CapabilityWitPlan::build(&registry(ECHO), &BTreeSet::new()).expect("WIT plan");
    assert!(plan.interfaces().is_empty());
}

#[test]
fn multiple_required_contracts_have_a_canonical_interface_order() {
    let mut registry = registry(ECHO);
    registry.merge(registry_for(
        "Probe",
        "\
module Probe
    kind = capability
    semantics = pure
    exposes [flip]

operation flip(value: Bool) -> Bool
",
    ));
    let required = ["Probe.flip".to_string(), "Echo.echo".to_string()]
        .into_iter()
        .collect();
    let plan = CapabilityWitPlan::build(&registry, &required).expect("WIT plan");
    assert_eq!(
        plan.interfaces()
            .iter()
            .map(|interface| interface.capability.as_str())
            .collect::<Vec<_>>(),
        ["Echo", "Probe"]
    );
}

#[test]
fn int_is_rejected_at_the_exact_type_path() {
    let registry = registry(
        "\
module Echo
    kind = capability
    semantics = pure
    exposes [read]

operation read(label: String) -> Int
",
    );
    let contract = registry.contract("Echo").expect("Echo contract");
    let error = CapabilityWitInterfacePlan::build(&registry, contract)
        .expect_err("Int must not narrow to s64");
    assert_eq!(error.operation, "Echo.read");
    assert_eq!(error.position, CapabilityWitTypePosition::Result);
    assert_eq!(error.aver_type, "Int");
}

#[test]
fn source_parameter_names_do_not_enter_the_plan() {
    let first = registry(ECHO);
    let renamed = registry(&ECHO.replace("value: String", "payload: String"));
    let first_contract = first.contract("Echo").expect("first contract");
    let renamed_contract = renamed.contract("Echo").expect("renamed contract");
    assert_eq!(first_contract.contract_hash, renamed_contract.contract_hash);
    assert_eq!(
        CapabilityWitInterfacePlan::build(&first, first_contract).expect("first plan"),
        CapabilityWitInterfacePlan::build(&renamed, renamed_contract).expect("renamed plan")
    );
}

#[test]
fn declaration_order_does_not_enter_the_plan() {
    let reordered = "\
module Echo
    kind = capability
    semantics = effectful
    exposes [ping, ratio, healthy, echo]

operation ping(marker: Unit) -> Unit
    replay = recorded
    oracle = generative

operation ratio(value: Float) -> Float
    replay = recorded
    oracle = generative

operation healthy() -> Bool
    replay = recorded
    oracle = generative

operation echo(value: String) -> String
    replay = recorded
    oracle = generative
";
    let first = registry(ECHO);
    let second = registry(reordered);
    assert_eq!(
        first.contract("Echo").expect("first").contract_hash,
        second.contract("Echo").expect("second").contract_hash
    );
    assert_eq!(
        CapabilityWitInterfacePlan::build(&first, first.contract("Echo").expect("first contract"))
            .expect("first plan"),
        CapabilityWitInterfacePlan::build(
            &second,
            second.contract("Echo").expect("second contract")
        )
        .expect("second plan")
    );
}

#[test]
fn contract_shape_changes_the_interface_identity() {
    let first = registry(ECHO);
    let changed = registry(&ECHO.replace("ratio(value: Float)", "ratio(value: Bool)"));
    let first =
        CapabilityWitInterfacePlan::build(&first, first.contract("Echo").expect("first contract"))
            .expect("first plan");
    let changed = CapabilityWitInterfacePlan::build(
        &changed,
        changed.contract("Echo").expect("changed contract"),
    )
    .expect("changed plan");
    assert_ne!(first.contract_hash, changed.contract_hash);
    assert_ne!(first.interface_name, changed.interface_name);
}

#[test]
fn model_changes_are_auditable_without_changing_transport_identity() {
    let first = registry(ECHO);
    let changed = registry(&ECHO.replace(
        "oracle = generative\n    replay = recorded",
        "oracle = generativeOutput\n    replay = recorded",
    ));
    let first =
        CapabilityWitInterfacePlan::build(&first, first.contract("Echo").expect("first contract"))
            .expect("first plan");
    let changed = CapabilityWitInterfacePlan::build(
        &changed,
        changed.contract("Echo").expect("changed contract"),
    )
    .expect("changed plan");
    assert_eq!(first.contract_hash, changed.contract_hash);
    assert_eq!(first.interface_name, changed.interface_name);
    assert_ne!(first.model_hash, changed.model_hash);
}

#[test]
fn identifier_encoding_is_injective_for_lookalike_source_names() {
    assert_ne!(
        encode_wit_identifier("A.B"),
        encode_wit_identifier("A_2e_B")
    );
    assert_ne!(
        encode_wit_identifier("fooBar"),
        encode_wit_identifier("foo-bar")
    );
}

#[test]
fn canonical_standard_capabilities_use_their_existing_wasi_bindings() {
    let registry = crate::stdlib::standard_capability_registry();
    let required = ["Random.int".to_string(), "Time.now".to_string()]
        .into_iter()
        .collect();
    let plan = CapabilityWitPlan::build(&registry, &required).expect("standard capability plan");
    assert!(plan.interfaces().is_empty());
}

#[test]
fn unsupported_types_report_the_exact_operation_and_type_path() {
    let cases = [
        (
            "\
module Echo
    kind = capability
    semantics = pure
    exposes [read]

operation read() -> Result<String, String>
",
            CapabilityWitTypePosition::Result,
            "Result<String, String>",
        ),
        (
            "\
module Echo
    kind = capability
    semantics = pure
    exposes [read]

operation read(label: String, values: List<String>) -> String
",
            CapabilityWitTypePosition::Parameter(1),
            "List<String>",
        ),
        (
            "\
module Echo
    kind = capability
    semantics = pure
    exposes [read]

record Reply
    value: String

operation read() -> Reply
",
            CapabilityWitTypePosition::Result,
            "Reply",
        ),
        (
            "\
module Echo
    kind = capability
    semantics = pure
    exposes [read]

opaque Token

operation read(token: Token) -> String
",
            CapabilityWitTypePosition::Parameter(0),
            "Token",
        ),
    ];

    for (source, position, aver_type) in cases {
        let error = unsupported(source);
        assert_eq!(error.capability, "Echo");
        assert_eq!(error.operation, "Echo.read");
        assert_eq!(error.position, position);
        assert_eq!(error.aver_type, aver_type);
    }
}
