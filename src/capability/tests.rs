use super::*;

fn descriptor_fields(bytes: &[u8]) -> Vec<(String, String)> {
    fn take_u64(bytes: &[u8], cursor: &mut usize) -> usize {
        let end = *cursor + 8;
        let raw: [u8; 8] = bytes[*cursor..end].try_into().expect("framed u64");
        *cursor = end;
        u64::from_be_bytes(raw) as usize
    }
    let mut cursor = 0usize;
    let mut fields = Vec::new();
    while cursor < bytes.len() {
        let name_len = take_u64(bytes, &mut cursor);
        let name = String::from_utf8(bytes[cursor..cursor + name_len].to_vec())
            .expect("descriptor field name");
        cursor += name_len;
        let value_len = take_u64(bytes, &mut cursor);
        let value = String::from_utf8(bytes[cursor..cursor + value_len].to_vec())
            .expect("descriptor field value");
        cursor += value_len;
        fields.push((name, value));
    }
    fields
}

fn registry(source: &str) -> CapabilityRegistry {
    let items = crate::source::parse_source(source).expect("parse capability fixture");
    let (registry, errors) = CapabilityRegistry::from_module("Entropy", &items);
    assert!(errors.is_empty(), "contract errors: {errors:?}");
    registry
}

fn hashes(source: &str) -> (String, String) {
    let registry = registry(source);
    let contract = registry.contracts().next().expect("one contract");
    (contract.contract_hash.clone(), contract.model_hash.clone())
}

fn error_messages(source: &str) -> Vec<String> {
    let items = crate::source::parse_source(source).expect("parse invalid contract fixture");
    let (_, errors) = CapabilityRegistry::from_module("Invalid", &items);
    errors.into_iter().map(|error| error.message).collect()
}

const BASE: &str = "\
module Entropy
    kind = capability
    semantics = effectful
    exposes [draw]

type Sample
    Sample(Int)

operation draw(limit: Int) -> Sample
    ? \"provider sample\"
    oracle = generative
    replay = recorded
    hostile = [small]

fn helper(limit: Int) -> Int
    limit - 1

fn small(path: BranchPath, call: Int, limit: Int) -> Sample
    Sample.Sample(helper(limit))

fn unrelated(x: Int) -> Int
    x
";

#[test]
fn model_only_changes_do_not_move_the_contract_hash() {
    let base = hashes(BASE);

    let description = hashes(&BASE.replace("provider sample", "different prose"));
    assert_eq!(base, description, "prose is outside both identities");

    let oracle = hashes(&BASE.replace("oracle = generative", "oracle = generativeOutput"));
    assert_eq!(base.0, oracle.0, "oracle metadata is not ABI");
    assert_ne!(base.1, oracle.1, "oracle metadata is proof trust");

    let hostile_body = hashes(&BASE.replace("limit - 1", "limit - 2"));
    assert_eq!(base.0, hostile_body.0, "hostile code is not ABI");
    assert_ne!(
        base.1, hostile_body.1,
        "model identity binds the transitive hostile helper closure"
    );

    let aliased = BASE.replace(
        "Sample.Sample(helper(limit))",
        "f = helper\n    Sample.Sample(f(limit))",
    );
    let aliased_base = hashes(&aliased);
    let aliased_body = hashes(&aliased.replace("limit - 1", "limit - 2"));
    assert_ne!(
        aliased_base.1, aliased_body.1,
        "function-value aliases must not escape the hostile semantic closure"
    );

    let qualified = BASE.replace(
        "Sample.Sample(helper(limit))",
        "Sample.Sample(Entropy.helper(limit))",
    );
    let qualified_base = hashes(&qualified);
    let qualified_body = hashes(&qualified.replace("limit - 1", "limit - 2"));
    assert_eq!(qualified_base.0, qualified_body.0);
    assert_ne!(
        qualified_base.1, qualified_body.1,
        "qualified local helper calls remain inside the hostile closure"
    );

    let unrelated = hashes(&BASE.replace(
        "fn unrelated(x: Int) -> Int\n    x",
        "fn unrelated(x: Int) -> Int\n    x + 1",
    ));
    assert_eq!(base, unrelated, "unreachable helpers are outside the model");
}

#[test]
fn boundary_changes_move_both_hashes() {
    let base = hashes(BASE);
    let changed = hashes(&BASE.replace("Sample(Int)", "Sample(Float)"));
    assert_ne!(base.0, changed.0);
    assert_ne!(base.1, changed.1);
}

#[test]
fn descriptor_order_is_source_order_independent() {
    let first = hashes(BASE);
    let reordered = hashes(&BASE.replace(
            "operation draw(limit: Int) -> Sample\n    ? \"provider sample\"\n    oracle = generative\n    replay = recorded\n    hostile = [small]",
            "operation draw(limit: Int) -> Sample\n    ? \"provider sample\"\n    hostile = [small]\n    replay = recorded\n    oracle = generative",
        ));
    assert_eq!(first, reordered);
}

#[test]
fn contract_descriptor_is_framed_sorted_and_positional() {
    let source = "\
module Entropy
    kind = capability
    semantics = pure

opaque Unused

record Reply
    z: Int
    a: String

type Outcome
    Zed
    Alpha(Int)

operation fetch(label: String, count: Int) -> Tuple<Reply, Outcome>
";
    let items = crate::source::parse_source(source).expect("parse descriptor fixture");
    let (registry, errors) = CapabilityRegistry::from_module("Entropy", &items);
    assert!(errors.is_empty(), "descriptor errors: {errors:?}");
    let contract = registry.contracts().next().expect("contract");
    let fields = descriptor_fields(&contract.contract_descriptor);
    assert_eq!(
        &fields[..3],
        &[
            ("avercap".to_string(), "1".to_string()),
            ("kind".to_string(), "contract".to_string()),
            ("capability".to_string(), "Entropy::Entropy".to_string()),
        ]
    );
    assert!(fields.iter().any(|field| {
        field
            == &(
                "type".to_string(),
                "Entropy::Reply = record{a:String,z:Int}".to_string(),
            )
    }));
    assert!(fields.iter().any(|field| {
        field
            == &(
                "type".to_string(),
                "Entropy::Outcome = sum{Alpha(Int),Zed}".to_string(),
            )
    }));
    assert!(fields.iter().any(|field| {
        field
            == &(
                "op".to_string(),
                "fetch(String,Int) -> Tuple<Entropy::Reply,Entropy::Outcome>".to_string(),
            )
    }));
    assert!(
        !fields.iter().any(|(_, value)| value.contains("Unused")),
        "unreachable opaque declarations are ordinary module internals"
    );

    let reordered = source
        .replace("    z: Int\n    a: String", "    a: String\n    z: Int")
        .replace("    Zed\n    Alpha(Int)", "    Alpha(Int)\n    Zed")
        .replace("label: String, count: Int", "text: String, amount: Int");
    assert_eq!(hashes(source), hashes(&reordered));
}

#[test]
fn semantic_classes_reject_unsound_attribute_combinations() {
    let pure = error_messages(
        "module Invalid\n    kind = capability\n    semantics = pure\n\noperation f() -> Int\n    oracle = generative\n    replay = recorded\n",
    );
    assert!(
        pure.iter()
            .any(|error| error.contains("pure capability operation"))
    );

    let missing = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation f() -> Int\n",
    );
    assert!(
        missing
            .iter()
            .any(|error| error.contains("must declare `oracle"))
    );
    assert!(
        missing
            .iter()
            .any(|error| error.contains("must declare `replay"))
    );

    let snapshot = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation f() -> Int\n    oracle = snapshot\n    replay = recorded\n",
    );
    assert!(
        snapshot
            .iter()
            .any(|error| error.contains("cannot claim `oracle = snapshot`"))
    );

    let incompatible = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation f() -> Int\n    oracle = generative\n    replay = reissued\n",
    );
    assert!(
        incompatible
            .iter()
            .any(|error| error.contains("incompatible"))
    );

    let output_value = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation f() -> Int\n    oracle = output\n    replay = suppressed\n",
    );
    assert!(
        output_value
            .iter()
            .any(|error| error.contains("must return Unit"))
    );

    let callback = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation subscribe(cb: Fn(String) -> Unit) -> Unit\n    oracle = output\n    replay = suppressed\n",
    );
    assert!(
        callback
            .iter()
            .any(|error| error.contains("must not call back"))
    );

    let unknown_disclosure = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\noperation f() -> Int\n    oracle = generative\n    replay = recorded\n    unmodelled = [missing]\n",
    );
    assert!(
        unknown_disclosure
            .iter()
            .any(|error| error.contains("unknown unmodelled operation"))
    );

    let external_boundary = error_messages(
        "module Invalid\n    kind = capability\n    semantics = pure\n\noperation f() -> Other.Error\n",
    );
    assert!(external_boundary.iter().any(|error| {
        error.contains("cross-module boundary type") && error.contains("contract_hash")
    }));

    let bare_external_boundary = error_messages(
        "module Invalid\n    kind = capability\n    semantics = pure\n    depends [Bytes]\n\noperation f(value: Bytes) -> Bytes\n",
    );
    assert_eq!(
        bare_external_boundary.len(),
        2,
        "{bare_external_boundary:?}"
    );
    assert!(bare_external_boundary.iter().any(|error| {
        error.contains("operation 'Invalid.f' parameter 0 uses cross-module boundary type 'Bytes'")
            && error.contains("contract_hash")
    }));
    assert!(bare_external_boundary.iter().any(|error| {
        error.contains("operation 'Invalid.f' result uses cross-module boundary type 'Bytes'")
            && error.contains("contract_hash")
    }));
    assert!(
        bare_external_boundary
            .iter()
            .all(|error| !error.contains("Invalid.Bytes")),
        "a bare imported type must not be misqualified as capability-owned: {bare_external_boundary:?}"
    );

    let repeated_in_one_position = error_messages(
        "module Invalid\n    kind = capability\n    semantics = pure\n    depends [Bytes]\n\noperation f(value: Tuple<Bytes, Bytes>) -> Int\n",
    );
    assert_eq!(
        repeated_in_one_position
            .iter()
            .filter(|error| error.contains("cross-module boundary type 'Bytes'"))
            .count(),
        1,
        "the same foreign type should be reported once per position: {repeated_in_one_position:?}"
    );
}

#[test]
fn capability_resources_and_transitive_wrappers_are_not_map_keys() {
    let errors = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nopaque Token\n\ntype Wrapper\n    Wrapped(Token)\n\noperation index(values: Map<Wrapper, Int>) -> Int\n    oracle = generative\n    replay = recorded\n",
    );
    assert!(
        errors.iter().any(|error| {
            error.contains("Map key") && error.contains("provider token identity")
        })
    );

    let nested_source = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nopaque Token\n\noperation mintMany() -> List<Token>\n    oracle = generative\n    replay = recorded\n",
    );
    assert!(
        nested_source
            .iter()
            .any(|error| error.contains("only directly through transparent Result/Option"))
    );

    let reissued_consumer = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nopaque Token\n\noperation flush(token: Token) -> Unit\n    oracle = output\n    replay = reissued\n",
    );
    assert!(
        reissued_consumer
            .iter()
            .any(|error| error.contains("replayed token has no live provider counterpart"))
    );
}

#[test]
fn resource_source_oracle_receives_one_unconstrained_fresh_token() {
    let source = "\
module Entropy
    kind = capability
    semantics = effectful

opaque Token

operation mint() -> Result<Token, String>
    oracle = generative
    replay = recorded
    hostile = [ok]

fn ok(path: BranchPath, call: Int, fresh: Token) -> Result<Token, String>
    Result.Ok(fresh)
";
    let registry = registry(source);
    let operation = registry.operation("Entropy.mint").expect("mint operation");
    assert_eq!(operation.minted_resource.as_deref(), Some("Entropy.Token"));
    assert_eq!(
        operation
            .oracle_params()
            .iter()
            .map(Type::display)
            .collect::<Vec<_>>(),
        vec!["BranchPath", "Int", "Entropy.Token"]
    );

    let missing_fresh = error_messages(&source.replace(", fresh: Token", ""));
    assert!(
        missing_fresh
            .iter()
            .any(|error| error.contains("must have oracle signature"))
    );
}

#[test]
fn profile_source_counts_separate_model_local_and_user_adversaries() {
    let mut registry = registry(BASE);
    let client = crate::source::parse_source(
            "module Client\n\nverify tick law profiles\n    given source: Entropy.draw = [small, userPeer]\n    tick() => 0\n",
        )
        .expect("parse client profile fixture");
    let (observations, errors) = CapabilityRegistry::from_module("Client", &client);
    assert!(errors.is_empty(), "client registry errors: {errors:?}");
    registry.merge(observations);
    assert_eq!(registry.profile_source_counts("Entropy.draw"), (1, 1));
}
