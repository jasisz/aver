//! Execute frame-reuse cases whose semantics depend on fresh local state,
//! simultaneous argument assignment, or the implicit function label.

use super::*;

fn transformed(source: &str, parameters: &[u32]) -> Vec<u8> {
    let bytes = wat::parse_str(source).unwrap();
    let mut code = wasm_encoder::CodeSection::new();
    let mut index = 0;
    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        if let wasmparser::Payload::CodeSectionEntry(parsed) = payload.unwrap() {
            let mut reencoder = RoundtripReencoder;
            let mut function = reencoder.new_function_with_parsed_locals(&parsed).unwrap();
            let mut reader = parsed.get_operators_reader().unwrap();
            while !reader.eof() {
                function.instruction(&reencoder.parse_instruction(&mut reader).unwrap());
            }
            code.function(&lower(function, index, parameters[index as usize]).unwrap());
            index += 1;
        }
    }
    let mut module = wasm_encoder::Module::new();
    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        if let Some((id, range)) = payload.unwrap().as_section() {
            if id == 10 {
                module.section(&code);
            } else {
                module.section(&wasm_encoder::RawSection {
                    id,
                    data: &bytes[range],
                });
            }
        }
    }
    let bytes = module.finish();
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(&bytes)
        .unwrap();
    bytes
}

fn run(source: &str, parameters: &[u32]) -> i32 {
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true).wasm_tail_call(true);
    let engine = wasmtime::Engine::new(&config).unwrap();
    let module = wasmtime::Module::new(&engine, transformed(source, parameters)).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap()
        .call(&mut store, ())
        .unwrap()
}

#[test]
fn resets_numeric_and_nullable_reference_locals() {
    assert_eq!(
        run(
            r#"(module
        (type (struct (field i32)))
        (func (param i32 i32) (result i32) (local i32 (ref null 0))
            local.get 2 i32.eqz
            if
                local.get 1 local.set 2
            end
            local.get 3 ref.is_null
            if
                local.get 1 struct.new 0 local.set 3
            end
            local.get 0 i32.eqz
            if (result i32)
                local.get 2
                local.get 3 struct.get 0 0
                i32.add
            else
                local.get 0 i32.const 1 i32.sub
                local.get 1 i32.const 1 i32.add
                return_call 0
            end)
        (func (export "run") (result i32)
            i32.const 3 i32.const 10 call 0))"#,
            &[2, 0]
        ),
        26
    );
}

#[test]
fn assigns_arguments_simultaneously() {
    assert_eq!(
        run(
            r#"(module
        (func (param i32 i32 i32) (result i32)
            local.get 0 i32.eqz
            if (result i32)
                local.get 1 i32.const 10 i32.mul local.get 2 i32.add
            else
                local.get 0 i32.const 1 i32.sub
                local.get 2 local.get 1
                return_call 0
            end)
        (func (export "run") (result i32)
            i32.const 3 i32.const 1 i32.const 2 call 0))"#,
            &[3, 0]
        ),
        21
    );
}

#[test]
fn returns_multiple_results_without_a_new_block_type() {
    assert_eq!(
        run(
            r#"(module
        (func (param i32 i64) (result i64 i32)
            local.get 0 i32.eqz
            if (result i64 i32)
                local.get 1 local.get 0
            else
                local.get 0 i32.const 1 i32.sub
                local.get 1 i64.const 1 i64.add
                return_call 0
            end)
        (func (export "run") (result i32)
            i32.const 3 i64.const 40 call 0 drop i32.wrap_i64))"#,
            &[2, 0]
        ),
        43
    );
}

#[test]
fn a_unit_result_can_finish_or_continue_without_growing_the_stack() {
    assert_eq!(
        run(
            r#"(module
        (func (param i32)
            local.get 0 i32.eqz
            if
                return
            else
                local.get 0 i32.const 1 i32.sub return_call 0
            end)
        (func (export "run") (result i32)
            i32.const 20000 call 0 i32.const 1))"#,
            &[1, 0]
        ),
        1
    );
}

#[test]
fn preserves_branches_to_the_function_label() {
    for branch in ["br 1", "i32.const 0 br_table 1 1"] {
        let source = format!(
            r#"(module
            (func (param i32) (result i32)
                local.get 0 i32.eqz
                if
                    i32.const 77 {branch}
                end
                local.get 0 i32.const 1 i32.sub return_call 0)
            (func (export "run") (result i32) i32.const 3 call 0))"#
        );
        assert_eq!(run(&source, &[1, 0]), 77);
    }
}

#[test]
fn keeps_other_tail_calls_and_ordinary_self_calls() {
    assert_eq!(
        run(
            r#"(module
        (func (param i32) (result i32)
            local.get 0 i32.eqz
            if (result i32)
                return_call 1
            else
                local.get 0 i32.const 1 i32.sub return_call 0
            end)
        (func (result i32) i32.const 19)
        (func (export "run") (result i32) i32.const 3 call 0))"#,
            &[1, 0, 0]
        ),
        19
    );
    // MIR decides which fns are lowered (`MirFnRepr::self_tail_loop`); an
    // ordinary self call in a lowered fn stays a call.
    assert_eq!(
        run(
            r#"(module
        (func (param i32) (result i32)
            local.get 0 i32.eqz
            if (result i32)
                i32.const 5
            else
                local.get 0 i32.const 1 i32.sub call 0 i32.const 1 i32.add
            end)
        (func (export "run") (result i32) i32.const 3 call 0))"#,
            &[1, 0]
        ),
        8
    );
}
