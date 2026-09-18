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

resource Unused

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
        "module Invalid\n    kind = capability\n    semantics = pure\n    depends [Other]\n\noperation f(value: Blob) -> Blob\n",
    );
    assert_eq!(
        bare_external_boundary.len(),
        2,
        "{bare_external_boundary:?}"
    );
    assert!(bare_external_boundary.iter().any(|error| {
        error.contains("operation 'Invalid.f' parameter 0 uses cross-module boundary type 'Blob'")
            && error.contains("contract_hash")
    }));
    assert!(bare_external_boundary.iter().any(|error| {
        error.contains("operation 'Invalid.f' result uses cross-module boundary type 'Blob'")
            && error.contains("contract_hash")
    }));
    assert!(
        bare_external_boundary
            .iter()
            .all(|error| !error.contains("Invalid.Blob")),
        "a bare imported type must not be misqualified as capability-owned: {bare_external_boundary:?}"
    );

    let repeated_in_one_position = error_messages(
        "module Invalid\n    kind = capability\n    semantics = pure\n    depends [Other]\n\noperation f(value: Tuple<Blob, Blob>) -> Int\n",
    );
    assert_eq!(
        repeated_in_one_position
            .iter()
            .filter(|error| error.contains("cross-module boundary type 'Blob'"))
            .count(),
        1,
        "the same foreign type should be reported once per position: {repeated_in_one_position:?}"
    );
}

#[test]
fn standard_bytes_is_a_canonical_capability_wire_type() {
    let source = "module Binary\n    kind = capability\n    semantics = pure\n    depends [Bytes]\n\noperation echo(value: Bytes) -> Bytes\n";
    let items = crate::source::parse_source(source).expect("parse Bytes capability");
    let (registry, errors) = CapabilityRegistry::from_module("Binary", &items);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(registry.uses_standard_bytes());

    let contract = registry.contract("Binary").expect("Binary contract");
    let descriptor = String::from_utf8_lossy(&contract.contract_descriptor);
    assert!(descriptor.contains("Aver::Bytes = octets"), "{descriptor}");
    assert!(
        descriptor.contains("echo(Aver::Bytes) -> Aver::Bytes"),
        "{descriptor}"
    );
}

#[test]
fn capability_owned_bytes_is_not_the_standard_wire_type() {
    let source = "module Binary\n    kind = capability\n    semantics = pure\n\nrecord Bytes\n    values: List<Int>\n\noperation echo(value: Bytes) -> Bytes\n";
    let items = crate::source::parse_source(source).expect("parse local Bytes capability");
    let (registry, errors) = CapabilityRegistry::from_module("Binary", &items);
    assert!(errors.is_empty(), "{errors:?}");
    assert!(!registry.uses_standard_bytes());

    let contract = registry.contract("Binary").expect("Binary contract");
    let descriptor = String::from_utf8_lossy(&contract.contract_descriptor);
    assert!(!descriptor.contains("Aver::Bytes = octets"), "{descriptor}");
    assert!(
        descriptor.contains("Binary::Bytes = record"),
        "{descriptor}"
    );
}

#[test]
fn capability_resources_and_transitive_wrappers_are_not_map_keys() {
    let errors = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nresource Token\n\ntype Wrapper\n    Wrapped(Token)\n\noperation index(values: Map<Wrapper, Int>) -> Int\n    oracle = generative\n    replay = recorded\n",
    );
    assert!(
        errors.iter().any(|error| {
            error.contains("Map key") && error.contains("provider token identity")
        })
    );

    let nested_source = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nresource Token\n\noperation mintMany() -> List<Token>\n    oracle = generative\n    replay = recorded\n",
    );
    assert!(
        nested_source
            .iter()
            .any(|error| error.contains("only directly through transparent Result/Option"))
    );

    let reissued_consumer = error_messages(
        "module Invalid\n    kind = capability\n    semantics = effectful\n\nresource Token\n\noperation flush(token: Token) -> Unit\n    oracle = output\n    replay = reissued\n",
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

resource Token

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

#[test]
fn embedded_reserved_capabilities_validate_and_expose_the_job_handle() {
    for module in crate::stdlib::RESERVED_CAPABILITY_MODULES {
        let embedded = crate::stdlib::find(module).expect("reserved capability is embedded");
        let items = crate::source::parse_source(embedded.source).expect("reserved source parses");
        let (registry, errors) = CapabilityRegistry::from_module(module, &items);
        assert!(errors.is_empty(), "{module} contract errors: {errors:?}");
        assert!(
            registry.contract(module).is_some(),
            "{module} has no contract"
        );
    }
    assert!(
        crate::stdlib::embedded_capability_resources().contains("Work.Job"),
        "Work.Job must be an embedded capability resource"
    );
}

#[test]
fn a_dependent_capability_may_name_an_embedded_capability_resource() {
    let source = "module Validation\n    kind = capability\n    semantics = effectful\n    depends [Work]\n\noperation begin(task: String) -> Result<Work.Job, String>\n    ? \"starts one validation job\"\n    oracle = generativeOutput\n    replay = recorded\n\noperation take(job: Work.Job) -> Result<Option<Int>, String>\n    ? \"collects one finished validation job\"\n    oracle = generativeOutput\n    replay = recorded\n";
    let items = crate::source::parse_source(source).expect("parse job kind");
    let (_, errors) = CapabilityRegistry::from_module("Validation", &items);
    assert!(errors.is_empty(), "job-kind contract errors: {errors:?}");
}

#[test]
fn an_undeclared_embedded_capability_resource_is_still_rejected() {
    let source = "module Validation\n    kind = capability\n    semantics = effectful\n\noperation begin(task: String) -> Result<Work.Job, String>\n    ? \"starts one validation job\"\n    oracle = generativeOutput\n    replay = recorded\n";
    let items = crate::source::parse_source(source).expect("parse job kind");
    let (_, errors) = CapabilityRegistry::from_module("Validation", &items);
    assert!(
        errors.iter().any(|error| error
            .message
            .contains("cross-module boundary type 'Work.Job'")),
        "expected a cross-module boundary error: {errors:?}"
    );
}

#[test]
fn nested_boundary_layouts_cannot_hide_foreign_types() {
    for outer in ["Envelope", "Invalid.Envelope"] {
        let source = format!(
            "module Invalid\n    kind = capability\n    semantics = pure\n    depends [Other]\n\nrecord Envelope\n    item: Option<Local>\n\ntype Local\n    Again(List<Envelope>)\n    Value(Other.Verdict)\n\noperation f(value: {outer}) -> {outer}\n"
        );
        let errors = error_messages(&source);
        for position in ["parameter 0", "result"] {
            let expected = format!("{position} uses cross-module boundary type 'Other.Verdict'");
            assert_eq!(
                errors
                    .iter()
                    .filter(|error| error.contains(&expected))
                    .count(),
                1,
                "{errors:?}"
            );
        }
        let local = source.replace("Other.Verdict", "Bool");
        assert!(error_messages(&local).is_empty());
    }
}

// ── A job kind may name the program's own data types ────────────────────
//
// The author's boundary: only a capability of Work shape, only plain data,
// only from a module it lists in `depends`, and the layout it names enters
// `contract_hash` exactly as a layout declared inside the job module does.

const LEDGER: &str = "\
module Ledger
    exposes [Request, Tx]

record Request
    source: String
    limit: Int

record Tx
    txid: String
    size: Int
";

const DECODE_JOB: &str = "\
module DecodeJob
    kind = capability
    semantics = effectful
    depends [Work, Ledger]
    exposes [begin, take]

operation begin(task: Ledger.Request) -> Result<Work.Job, String>
    ? \"starts one decode job\"
    oracle = generativeOutput
    replay = recorded

operation take(job: Work.Job) -> Result<Option<List<Ledger.Tx>>, String>
    ? \"collects one finished decode job\"
    oracle = generativeOutput
    replay = recorded
";

fn dependency_types(sources: &[(&str, &str)]) -> DependencyTypes {
    let mut table = DependencyTypes::default();
    for (module, source) in sources {
        let items = crate::source::parse_source(source).expect("parse dependency module");
        table.add_module(module, &items);
    }
    table
}

/// The same table a program's check builds, with the standard capability a
/// fixture names already in it — `check.rs` records every loaded module, and
/// the compiler's own modules load like any other.
fn dependency_types_with_standard(sources: &[(&str, &str)], standard: &[&str]) -> DependencyTypes {
    let mut table = dependency_types(sources);
    for module in standard {
        let embedded = crate::stdlib::find(module).expect("embedded module is present");
        let items = crate::source::parse_source(embedded.source).expect("embedded parses");
        table.add_module(module, &items);
    }
    table
}

fn job_kind_registry(source: &str, deps: &[(&str, &str)]) -> CapabilityRegistry {
    let items = crate::source::parse_source(source).expect("parse job kind fixture");
    let table = dependency_types(deps);
    let (registry, errors) =
        CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    assert!(errors.is_empty(), "job kind contract errors: {errors:?}");
    registry
}

#[test]
fn a_job_kind_may_name_a_plain_data_type_of_a_module_it_depends_on() {
    let registry = job_kind_registry(DECODE_JOB, &[("Ledger", LEDGER)]);
    let contract = registry.contract("DecodeJob").expect("job kind contract");
    let fields = descriptor_fields(&contract.contract_descriptor);
    let types: Vec<&str> = fields
        .iter()
        .filter(|(name, _)| name == "type")
        .map(|(_, value)| value.as_str())
        .collect();
    assert!(
        types.contains(&"Ledger::Request = record{limit:Int,source:String}"),
        "the task's dependency layout is not bound by contract_hash: {types:?}"
    );
    assert!(
        types.contains(&"Ledger::Tx = record{size:Int,txid:String}"),
        "the reply's dependency layout is not bound by contract_hash: {types:?}"
    );
    let ops: Vec<&str> = fields
        .iter()
        .filter(|(name, _)| name == "op")
        .map(|(_, value)| value.as_str())
        .collect();
    assert_eq!(
        ops,
        vec![
            "begin(Ledger::Request) -> Result<Work::Job,String>",
            "take(Work::Job) -> Result<Option<List<Ledger::Tx>>,String>",
        ],
        "operation rows do not name the dependency types canonically"
    );
    assert!(
        registry.boundary_type("Ledger.Tx").is_some(),
        "the dependency layout is not in the registry the runtimes read"
    );
}

#[test]
fn a_field_added_to_a_dependency_type_moves_the_job_kind_contract_hash() {
    let before = job_kind_registry(DECODE_JOB, &[("Ledger", LEDGER)]);
    let widened = LEDGER.replace("    size: Int\n", "    size: Int\n    fee: Int\n");
    let after = job_kind_registry(DECODE_JOB, &[("Ledger", widened.as_str())]);
    assert_ne!(
        before.contract("DecodeJob").expect("before").contract_hash,
        after.contract("DecodeJob").expect("after").contract_hash,
        "a field added to a named dependency type must invalidate old recordings"
    );
}

#[test]
fn a_job_kind_may_not_name_a_type_of_a_module_it_does_not_depend_on() {
    let source = DECODE_JOB.replace("depends [Work, Ledger]", "depends [Work]");
    let items = crate::source::parse_source(&source).expect("parse job kind fixture");
    let table = dependency_types(&[("Ledger", LEDGER)]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    assert!(
        errors.iter().any(|error| error
            .message
            .contains("cross-module boundary type 'Ledger.Request'")),
        "a dependency the module never declared must stay refused: {errors:?}"
    );
}

#[test]
fn an_ordinary_capability_may_not_name_a_type_of_a_module_it_depends_on() {
    let source = "\
module Ripemd
    kind = capability
    semantics = pure
    depends [Ledger]
    exposes [hash]

operation hash(tx: Ledger.Tx) -> Ledger.Tx
    ? \"hashes one transaction\"
";
    let items = crate::source::parse_source(source).expect("parse capability fixture");
    let table = dependency_types(&[("Ledger", LEDGER)]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("Ripemd", &items, &table);
    let messages: Vec<&str> = errors.iter().map(|error| error.message.as_str()).collect();
    for position in ["parameter 0", "result"] {
        assert!(
            messages.iter().any(|message| message.contains(&format!(
                "{position} uses cross-module boundary type 'Ledger.Tx'"
            ))),
            "a capability that is not a job kind must stay closed on its own types: {messages:?}"
        );
    }
}

#[test]
fn a_dependency_type_carrying_a_resource_is_refused_by_field_and_type() {
    let kept = "\
module Tending
    depends [Tcp]
    exposes [Kept]

record Kept
    sockets: Map<Int, Tcp.Socket>
    seen: Int
";
    let source = DECODE_JOB
        .replace("depends [Work, Ledger]", "depends [Work, Tending]")
        .replace("Ledger.Request", "Tending.Kept")
        .replace("List<Ledger.Tx>", "Int");
    let items = crate::source::parse_source(&source).expect("parse job kind fixture");
    let table = dependency_types_with_standard(&[("Tending", kept)], &["Tcp"]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    let messages: Vec<&str> = errors.iter().map(|error| error.message.as_str()).collect();
    assert!(
        messages.iter().any(|message| {
            message.contains("dependency type 'Tending.Kept'")
                && message.contains("field 'sockets'")
                && message.contains("has type 'Tcp.Socket'")
                && message.contains("holds capability resource 'Tcp.")
        }),
        "the refusal must name the field and the type: {messages:?}"
    );
}

#[test]
fn a_dependency_field_that_is_a_resource_is_refused_by_name() {
    let kept = "\
module Tending
    depends [Tcp]
    exposes [Kept]

record Kept
    live: Tcp.Connection
    seen: Int
";
    let source = DECODE_JOB
        .replace("depends [Work, Ledger]", "depends [Work, Tending]")
        .replace("Ledger.Request", "Tending.Kept")
        .replace("List<Ledger.Tx>", "Int");
    let items = crate::source::parse_source(&source).expect("parse job kind fixture");
    let table = dependency_types(&[("Tending", kept)]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    let messages: Vec<&str> = errors.iter().map(|error| error.message.as_str()).collect();
    assert!(
        messages.iter().any(|message| {
            message.contains("dependency type 'Tending.Kept'")
                && message.contains("field 'live'")
                && message.contains("carries capability resource 'Tcp.Connection'")
        }),
        "the refusal must name the field and the resource: {messages:?}"
    );
}

#[test]
fn a_dependency_variant_carrying_a_resource_is_refused_by_variant_and_type() {
    let held = "\
module Held
    depends [Work]
    exposes [Slot]

type Slot
    Empty
    Running(Work.Job)
";
    let source = DECODE_JOB
        .replace("depends [Work, Ledger]", "depends [Work, Held]")
        .replace("Ledger.Request", "Held.Slot")
        .replace("List<Ledger.Tx>", "Int");
    let items = crate::source::parse_source(&source).expect("parse job kind fixture");
    let table = dependency_types(&[("Held", held)]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    let messages: Vec<&str> = errors.iter().map(|error| error.message.as_str()).collect();
    assert!(
        messages.iter().any(|message| {
            message.contains("dependency type 'Held.Slot'")
                && message.contains("variant 'Running'")
                && message.contains("capability resource 'Work.Job'")
        }),
        "the refusal must name the variant and the type: {messages:?}"
    );
}

#[test]
fn a_dependency_type_the_program_does_not_declare_is_refused() {
    let items = crate::source::parse_source(DECODE_JOB).expect("parse job kind fixture");
    let table = dependency_types(&[(
        "Ledger",
        "module Ledger\n    exposes [Request]\n\nrecord Request\n    source: String\n    limit: Int\n",
    )]);
    let (_, errors) = CapabilityRegistry::from_module_in_program("DecodeJob", &items, &table);
    assert!(
        errors.iter().any(|error| error
            .message
            .contains("names dependency type 'Ledger.Tx', which module 'Ledger' does not declare")),
        "a name with no layout must not reach contract_hash: {errors:?}"
    );
}

#[test]
fn a_dependency_layout_nested_in_another_module_still_enters_the_hash() {
    let chain = "module Chain\n    exposes [Meta]\n\nrecord Meta\n    height: Int\n";
    let ledger = "\
module Ledger
    depends [Chain]
    exposes [Request, Tx]

record Request
    source: String
    limit: Int

record Tx
    txid: String
    size: Int
    meta: Chain.Meta
";
    let registry = job_kind_registry(DECODE_JOB, &[("Ledger", ledger), ("Chain", chain)]);
    let contract = registry.contract("DecodeJob").expect("job kind contract");
    let types: Vec<String> = descriptor_fields(&contract.contract_descriptor)
        .into_iter()
        .filter(|(name, _)| name == "type")
        .map(|(_, value)| value)
        .collect();
    assert!(
        types.contains(&"Chain::Meta = record{height:Int}".to_string()),
        "a layout reached through a dependency type must be hashed too: {types:?}"
    );
    assert!(
        types.contains(&"Ledger::Tx = record{meta:Chain::Meta,size:Int,txid:String}".to_string()),
        "the dependency layout must name its own module's types canonically: {types:?}"
    );
}

#[test]
fn a_job_kind_naming_no_dependency_type_keeps_its_contract_hash() {
    let source = "\
module Scorer
    kind = capability
    semantics = effectful
    depends [Work]
    exposes [begin, take, Task, Report]

record Task
    text: String
    weight: Int

record Report
    score: Int
    label: String

operation begin(task: Task) -> Result<Work.Job, String>
    ? \"starts one scoring job\"
    oracle = generativeOutput
    replay = recorded

operation take(job: Work.Job) -> Result<Option<Report>, String>
    ? \"collects one finished scoring job\"
    oracle = generativeOutput
    replay = recorded
";
    let items = crate::source::parse_source(source).expect("parse job kind fixture");
    let (closed, closed_errors) = CapabilityRegistry::from_module("Scorer", &items);
    assert!(closed_errors.is_empty(), "{closed_errors:?}");
    let table = dependency_types(&[("Ledger", LEDGER)]);
    let (open, open_errors) = CapabilityRegistry::from_module_in_program("Scorer", &items, &table);
    assert!(open_errors.is_empty(), "{open_errors:?}");
    assert_eq!(
        closed.contract("Scorer").expect("closed").contract_hash,
        open.contract("Scorer").expect("open").contract_hash,
        "a job kind that names no dependency type must keep the identity it already published"
    );
}
