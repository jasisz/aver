//! Fidelity of the honest (`normal_ok`) `Tcp.readBytes` hostile profile.
//!
//! Hostile verification runs on the VM, so the fidelity target is the
//! standard native Tcp provider (`aver_rt::provider::StandardTcpProvider`).
//! Its count handling, branch by branch:
//!
//! 1. count outside i64 (positive OR very negative) →
//!    `Err("Tcp.readBytes: count N exceeds the read limit")`
//! 2. count < 0 (within i64) → `Err("Tcp.readBytes: count N is negative")`
//! 3. count > 10485760 (within i64) →
//!    `Err("Tcp.readBytes: count N exceeds the 10485760 byte limit")`
//! 4. otherwise → `Ok` with exactly `count` bytes
//!
//! The stub used to collapse branches 1 and 3 into a generic "exceeds the
//! read limit" and sent very negative out-of-i64 counts down the "is
//! negative" branch. These tests call the real native provider with a typed
//! provider resource (count validation fires before any socket I/O) and the
//! actual stub body from `hostile_profiles_for`, compiled through the VM, and
//! require identical messages per branch.

use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::types::checker::hostile_effects::hostile_profiles_for;
use aver::value::Value;
use aver::vm;
use aver_rt::provider::{
    CapabilityProvider, ProviderContext, ProviderResource, ProviderValue, StandardTcpProvider,
};

/// 2^80 — comfortably outside i64 in both directions.
const BIG_COUNT: &str = "1208925819614629174706176";

fn fake_conn_value() -> Value {
    Value::Record {
        type_name: "Tcp.Connection".to_string(),
        fields: vec![
            ("id".to_string(), Value::Str("fidelity".to_string())),
            ("host".to_string(), Value::Str("127.0.0.1".to_string())),
            ("port".to_string(), Value::int(1)),
        ]
        .into(),
    }
}

fn fake_conn_resource() -> ProviderValue {
    ProviderValue::Resource(ProviderResource::new(aver_rt::TcpConnection::from_parts(
        "fidelity".to_string(),
        "127.0.0.1".to_string(),
        1,
    )))
}

/// Compile a VM holding the `normal_ok` stub body from
/// `hostile_profiles_for`, plus two helper fns that build out-of-i64
/// counts from source literals (so the test needs no bignum constructor
/// on the Rust side). The compiler-reserved stub name is changed to a
/// legal infix-`__` test name because this integration harness correctly
/// enters through the user-source parser; production hostile injection
/// uses the parser's private compiler-generated path. Type checking is
/// skipped on purpose — this harness pins runtime semantics, and the
/// stub's typecheck is already guarded in `hostile_effects::tests`.
fn stub_vm() -> (vm::VM, String) {
    let profile = hostile_profiles_for("Tcp.readBytes")
        .into_iter()
        .find(|p| p.name == "normal_ok")
        .expect("Tcp.readBytes must ship a normal_ok profile");
    let test_stub_name = profile.stub_fn_name.replacen("__hostile_", "hostile__", 1);
    let test_stub_body = profile
        .stub_body
        .replacen(&profile.stub_fn_name, &test_stub_name, 1);
    let src = format!(
        "module M\n    intent = \"t\"\n    depends [Bytes]\n    effects []\n\n{}\nfn bigPositive() -> Int\n    {BIG_COUNT}\n\nfn bigNegative() -> Int\n    0 - {BIG_COUNT}\n",
        test_stub_body
    );
    let mut lexer = aver::lexer::Lexer::new(&src);
    let tokens = lexer.tokenize().expect("lex stub program");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse stub program");
    aver::tco::transform_program(&mut items);
    let prepared_deps =
        aver::source::load_compile_deps(&items, env!("CARGO_MANIFEST_DIR")).expect("load Bytes");
    let dep_modules = prepared_deps.modules;
    aver::resolver::resolve_program(&mut items);
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let symbols = aver::ir::SymbolTable::build(&items, &dep_modules);
    let resolved = aver::ir::hir::resolve_program(&symbols, &items);
    let (code, globals) = vm::compile_program_with_modules(
        &resolved,
        &symbols,
        &mut arena,
        Some(env!("CARGO_MANIFEST_DIR")),
        "<stub-fidelity>",
        None,
    )
    .expect("VM compile of stub program");
    (vm::VM::new(code, globals, arena), test_stub_name)
}

/// Run the stub fn with a fabricated `BranchPath.Root` / connection and the
/// given count Value; return the stub's `Result` as a `Value`.
fn call_stub(machine: &mut vm::VM, stub_fn: &str, count: Value) -> Value {
    let path = Value::Variant {
        type_name: "BranchPath".to_string(),
        variant: "Root".to_string(),
        fields: Vec::new().into(),
    };
    let arg_values = [path, Value::int(0), fake_conn_value(), count];
    let mut args: Vec<NanValue> = Vec::with_capacity(arg_values.len());
    for value in &arg_values {
        args.push(NanValue::from_value(value, &mut machine.arena));
    }
    let out = machine
        .run_named_function(stub_fn, &args)
        .expect("stub run must not error");
    out.to_value(&machine.arena)
}

/// Build an out-of-i64 count by evaluating a helper fn in the same VM, so
/// the exact same `Int` value feeds both the stub and the real provider.
fn big_count(machine: &mut vm::VM, helper: &str) -> Value {
    let out = machine
        .run_named_function(helper, &[])
        .expect("bignum helper");
    out.to_value(&machine.arena)
}

fn real_read_bytes_error(count: Value) -> String {
    let Value::Int(count) = count else {
        panic!("test count must be an Int");
    };
    let context = ProviderContext {
        capability: "Tcp".to_string(),
        operation: "Tcp.readBytes".to_string(),
        contract_hash: "test-contract".to_string(),
        model_hash: "test-model".to_string(),
    };
    let result = StandardTcpProvider::default()
        .invoke(&context, &[fake_conn_resource(), ProviderValue::Int(count)])
        .expect("Tcp.readBytes provider boundary must accept the canonical arguments");
    let ProviderValue::ResultErr(message) = result else {
        panic!("count validation must return Result.Err, got {result:?}");
    };
    let ProviderValue::String(message) = *message else {
        panic!("Tcp.readBytes error must carry String");
    };
    message
}

fn err_msg(context: &str, value: Value) -> String {
    match value {
        Value::Err(inner) => match *inner {
            Value::Str(s) => s,
            other => panic!("{context}: expected Err(String), got Err({other:?})"),
        },
        other => panic!("{context}: expected Result.Err, got {other:?}"),
    }
}

#[test]
fn stub_matches_builtin_for_in_i64_negative_count() {
    let (mut machine, stub_fn) = stub_vm();
    let stub = err_msg("stub", call_stub(&mut machine, &stub_fn, Value::int(-1)));
    let real = real_read_bytes_error(Value::int(-1));
    assert_eq!(stub, real);
    assert_eq!(real, "Tcp.readBytes: count -1 is negative");
}

#[test]
fn stub_matches_builtin_for_in_i64_over_limit_count() {
    let (mut machine, stub_fn) = stub_vm();
    let stub = err_msg(
        "stub",
        call_stub(&mut machine, &stub_fn, Value::int(10485761)),
    );
    let real = real_read_bytes_error(Value::int(10485761));
    assert_eq!(stub, real);
    // The real path names the byte limit for ordinary over-limit counts —
    // pin the exact shape so a generic message can't sneak back in.
    assert_eq!(
        real,
        "Tcp.readBytes: count 10485761 exceeds the 10485760 byte limit"
    );
}

#[test]
fn stub_matches_builtin_for_out_of_i64_positive_count() {
    let (mut machine, stub_fn) = stub_vm();
    let count = big_count(&mut machine, "bigPositive");
    let stub = err_msg("stub", call_stub(&mut machine, &stub_fn, count.clone()));
    let real = real_read_bytes_error(count);
    assert_eq!(stub, real);
    assert_eq!(
        real,
        format!("Tcp.readBytes: count {BIG_COUNT} exceeds the read limit")
    );
}

#[test]
fn stub_matches_builtin_for_out_of_i64_negative_count() {
    let (mut machine, stub_fn) = stub_vm();
    let count = big_count(&mut machine, "bigNegative");
    let stub = err_msg("stub", call_stub(&mut machine, &stub_fn, count.clone()));
    let real = real_read_bytes_error(count);
    // The real provider checks i64-fit BEFORE sign, so a very negative
    // out-of-i64 count reports "exceeds the read limit", not "is negative".
    assert_eq!(stub, real);
    assert_eq!(
        real,
        format!("Tcp.readBytes: count -{BIG_COUNT} exceeds the read limit")
    );
}

#[test]
fn stub_read_is_exact_for_valid_counts() {
    let (mut machine, stub_fn) = stub_vm();
    for count in [0i64, 4, 255] {
        let result = call_stub(&mut machine, &stub_fn, Value::int(count));
        let Value::Ok(inner) = result else {
            panic!("count {count}: expected Result.Ok, got {result:?}");
        };
        let Value::Record { fields, .. } = *inner else {
            panic!("count {count}: expected a Bytes record");
        };
        let Some((_, Value::List(values))) = fields.iter().find(|(name, _)| name == "values")
        else {
            panic!("count {count}: malformed Bytes carrier");
        };
        assert_eq!(
            values.len(),
            usize::try_from(count).expect("non-negative"),
            "the honest profile must be read-exact: Ok carries exactly `count` bytes"
        );
    }
}
