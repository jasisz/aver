/// Builtin call emission for ExprEmitter.
///
/// Handles Aver builtin namespace calls: Console.print, List.*, Float.*, Int.*, etc.
use wasm_encoder::Instruction;

use crate::ast::{Expr, Spanned};
use crate::types::Type;

use super::super::types::{WasmType, aver_type_to_wasm};
use super::super::value;
use super::ExprEmitter;

impl<'a> ExprEmitter<'a> {
    /// Emit a builtin call (Console.print, List.len, Float.fromInt etc.)
    pub(super) fn emit_builtin_call(&mut self, name: &str, args: &[Spanned<Expr>]) {
        // Args are already on the stack
        match name {
            "Args.get" if args.is_empty() => {
                self.emit_args_get_import();
            }
            "Console.print" | "Console.error" | "Console.warn" => {
                let target = match name {
                    "Console.print" => "console_print",
                    "Console.error" => "console_error",
                    "Console.warn" => "console_warn",
                    _ => unreachable!(),
                };
                self.emit_console_print(args, target);
            }
            "Terminal.enableRawMode"
            | "Terminal.disableRawMode"
            | "Terminal.clear"
            | "Terminal.resetColor"
            | "Terminal.hideCursor"
            | "Terminal.showCursor"
            | "Terminal.flush"
                if args.is_empty() =>
            {
                let import_name = match name {
                    "Terminal.enableRawMode" => "terminal_enableRawMode",
                    "Terminal.disableRawMode" => "terminal_disableRawMode",
                    "Terminal.clear" => "terminal_clear",
                    "Terminal.resetColor" => "terminal_resetColor",
                    "Terminal.hideCursor" => "terminal_hideCursor",
                    "Terminal.showCursor" => "terminal_showCursor",
                    "Terminal.flush" => "terminal_flush",
                    _ => unreachable!(),
                };
                self.emit_host_unit_import(import_name);
            }
            "Terminal.moveTo" if args.len() == 2 => {
                self.emit_host_move_to_import("terminal_moveTo");
            }
            "Terminal.print" if args.len() == 1 => {
                let aver_type = self.aver_type_of(&args[0]).clone();
                let wasm_type = aver_type_to_wasm(&aver_type);
                self.emit_host_string_consumer_import(
                    "terminal_print",
                    wasm_type,
                    Some(&aver_type),
                    true,
                );
            }
            "Terminal.setColor" if args.len() == 1 => {
                let aver_type = self.aver_type_of(&args[0]).clone();
                let wasm_type = aver_type_to_wasm(&aver_type);
                self.emit_host_string_consumer_import(
                    "terminal_setColor",
                    wasm_type,
                    Some(&aver_type),
                    false,
                );
            }
            "Terminal.readKey" if args.is_empty() => {
                self.emit_host_option_string_import("terminal_readKey");
            }
            "Terminal.size" if args.is_empty() => {
                self.emit_terminal_size_import("terminal_size");
            }
            "List.prepend" if args.len() == 2 => {
                self.emit_list_prepend(args);
            }
            "List.len" if args.len() == 1 => {
                self.emit_list_len();
            }
            "List.take" if args.len() == 2 => {
                // args on stack: [list(i32), n(i64)]
                self.instructions.push(Instruction::I32WrapI64); // n: i64->i32
                self.instructions.push(Instruction::Call(self.rt.list_take));
            }
            "List.drop" if args.len() == 2 => {
                self.instructions.push(Instruction::I32WrapI64);
                self.instructions.push(Instruction::Call(self.rt.list_drop));
            }
            "List.concat" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_concat));
            }
            "List.reverse" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_reverse));
            }
            "List.contains" if args.len() == 2 => {
                // args on stack: [list(i32), val(?)]
                // list_contains expects (i32, i64) -- convert val to i64 if needed
                let val_type = self.wasm_type_of(&args[1]);
                if val_type == WasmType::I32 {
                    self.instructions.push(Instruction::I64ExtendI32S);
                }
                self.instructions
                    .push(Instruction::Call(self.rt.list_contains));
            }
            "List.zip" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.list_zip));
            }
            "Float.fromInt" if args.len() == 1 => {
                self.instructions.push(Instruction::F64ConvertI64S);
            }
            "Int.toFloat" if args.len() == 1 => {
                self.instructions.push(Instruction::F64ConvertI64S);
            }
            "Int.toString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            "Float.toString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            "Map.empty" if args.is_empty() => {
                // Empty map = empty association list = null ptr
                self.instructions.push(Instruction::I32Const(0));
            }
            "Map.get" if args.len() == 2 => {
                // args on stack: [map(i32), key(?)]
                // ABI: rt_map_get(map: i32, key: i64, kind: i32) -> i32
                let kind = self.emit_map_key_normalize(&args[1]);
                self.instructions.push(Instruction::I32Const(kind));
                self.instructions.push(Instruction::Call(self.rt.map_get));
            }
            "Map.set" if args.len() == 3 => {
                // args on stack: [map(i32), key(?), value(?)]
                // ABI: rt_map_set(map, key: i64, kind: i32, value: i64, value_ptr_flag: i32)
                let value_is_ptr = self.expr_is_heap_ptr_spanned(&args[2]);
                let val_type = self.wasm_type_of(&args[2]);
                let value_local = self.alloc_local(WasmType::I64);
                match val_type {
                    WasmType::I64 => {}
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::LocalSet(value_local));
                // Stack now: [map, key]. Normalize key to i64 + kind.
                let kind = self.emit_map_key_normalize(&args[1]);
                self.instructions.push(Instruction::I32Const(kind));
                self.instructions.push(Instruction::LocalGet(value_local));
                self.instructions
                    .push(Instruction::I32Const(if value_is_ptr { 1 } else { 0 }));
                // Owned-mutate fast path: when last-use analysis proves
                // the map argument is the sole observer, call the
                // _owned variant. The flat-hashtable runtime mutates
                // the bucket array in place — zero alloc on update,
                // zero alloc on insert (until load factor triggers a
                // resize). Mirrors the VM's `set_nv_owned` dispatch.
                // Owned-mutate fast path: when last-use analysis proves
                // the map argument is the sole observer, call the
                // _owned variant. The flat-hashtable runtime mutates
                // the bucket array in place — zero alloc on update,
                // amortized O(1) on insert (resize still allocates a
                // fresh table). Mirrors the VM's `set_nv_owned` dispatch.
                let target = if matches!(
                    &args[0].node,
                    Expr::Resolved { last_use, .. } if last_use.0
                ) {
                    self.rt.map_set_owned
                } else {
                    self.rt.map_set
                };
                self.instructions.push(Instruction::Call(target));
            }
            "Map.len" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.map_len));
            }
            "Map.has" if args.len() == 2 => {
                let kind = self.emit_map_key_normalize(&args[1]);
                self.instructions.push(Instruction::I32Const(kind));
                self.instructions.push(Instruction::Call(self.rt.map_has));
            }
            "Map.keys" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.map_keys));
            }
            "Map.entries" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.map_entries));
            }
            "Map.fromList" if args.len() == 1 => {
                // Fold the `List<(K, V)>` into a flat OBJ_MAP via the
                // runtime helper. The pre-0.14 association-list
                // representation used to make this an identity ("the
                // list IS a map"); the flat hashtable runtime needs
                // a real OBJ_MAP, so we materialize one.
                let (key_kind, value_ptr_flag) = self.map_from_list_kind_and_ptr_flag(&args[0]);
                self.instructions.push(Instruction::I32Const(key_kind));
                self.instructions
                    .push(Instruction::I32Const(value_ptr_flag));
                self.instructions
                    .push(Instruction::Call(self.rt.map_from_list));
            }
            "Option.withDefault" if args.len() == 2 => {
                // args: [option(i32), default]
                // Check if option == NONE_SENTINEL -> return default, else unwrap
                let opt_local = self.alloc_local(WasmType::I32);
                let result_type = self.wasm_type_of(&args[1]);
                let def_local = self.alloc_local(result_type);
                self.instructions.push(Instruction::LocalSet(def_local));
                self.instructions.push(Instruction::LocalSet(opt_local));
                // Check
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions
                    .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_else();
                // Unwrap
                self.instructions.push(Instruction::LocalGet(opt_local));
                match result_type {
                    WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
                    WasmType::F64 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_f64)),
                    WasmType::I32 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_i32)),
                }
                self.emit_end();
            }
            "Option.toResult" if args.len() == 2 => {
                // args: [option(i32), err_value]
                // Some(v) → Ok(v), None → Err(err_value)
                let opt_local = self.alloc_local(WasmType::I32);
                let inner_type = match self.aver_type_of(&args[0]) {
                    Type::Option(inner) => aver_type_to_wasm(inner),
                    _ => WasmType::I64,
                };
                let err_type = self.wasm_type_of(&args[1]);
                let err_is_ptr = self.expr_is_heap_ptr_spanned(&args[1]);
                let err_local = self.alloc_local(err_type);
                self.instructions.push(Instruction::LocalSet(err_local));
                self.instructions.push(Instruction::LocalSet(opt_local));
                // Check if None
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions
                    .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                // None → Err(err_value)
                self.instructions
                    .push(Instruction::I32Const(super::super::value::WRAP_ERR as i32));
                self.instructions.push(Instruction::LocalGet(err_local));
                match err_type {
                    WasmType::I64 => {
                        self.instructions.push(Instruction::I32Const(if err_is_ptr {
                            1
                        } else {
                            0
                        }));
                        self.instructions.push(Instruction::Call(self.rt.wrap));
                    }
                    WasmType::F64 => {
                        self.instructions.push(Instruction::Call(self.rt.wrap_f64));
                    }
                    WasmType::I32 => {
                        self.instructions.push(Instruction::I32Const(if err_is_ptr {
                            1
                        } else {
                            0
                        }));
                        self.instructions.push(Instruction::Call(self.rt.wrap_i32));
                    }
                }
                self.emit_else();
                // Some(v) → Ok(v): unwrap inner, re-wrap as Ok
                let inner_is_ptr = matches!(inner_type, WasmType::I32);
                self.instructions
                    .push(Instruction::I32Const(super::super::value::WRAP_OK as i32));
                self.instructions.push(Instruction::LocalGet(opt_local));
                match inner_type {
                    WasmType::I64 => {
                        self.instructions.push(Instruction::Call(self.rt.unwrap));
                        self.instructions
                            .push(Instruction::I32Const(if inner_is_ptr { 1 } else { 0 }));
                        self.instructions.push(Instruction::Call(self.rt.wrap));
                    }
                    WasmType::F64 => {
                        self.instructions
                            .push(Instruction::Call(self.rt.unwrap_f64));
                        self.instructions.push(Instruction::Call(self.rt.wrap_f64));
                    }
                    WasmType::I32 => {
                        self.instructions
                            .push(Instruction::Call(self.rt.unwrap_i32));
                        self.instructions
                            .push(Instruction::I32Const(if inner_is_ptr { 1 } else { 0 }));
                        self.instructions.push(Instruction::Call(self.rt.wrap_i32));
                    }
                }
                self.emit_end();
            }
            "Vector.fromList" if args.len() == 1 => {
                let elem_is_ptr = match self.aver_type_of(&args[0]) {
                    Type::List(elem) => self.is_heap_type(elem),
                    _ => false,
                };
                self.instructions
                    .push(Instruction::I32Const(if elem_is_ptr { 1 } else { 0 }));
                self.instructions
                    .push(Instruction::Call(self.rt.vec_from_list));
            }
            "Vector.get" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.vec_get));
            }
            "Vector.len" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.vec_len));
            }
            "Vector.set" if args.len() == 3 => {
                let val_type = self.wasm_type_of(&args[2]);
                match val_type {
                    WasmType::I64 => {}
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::Call(self.rt.vec_set));
            }
            "Vector.new" if args.len() == 2 => {
                let fill_type = self.wasm_type_of(&args[1]);
                match fill_type {
                    WasmType::I64 => {}
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::I32Const(
                    if self.expr_is_heap_ptr_spanned(&args[1]) {
                        1
                    } else {
                        0
                    },
                ));
                self.instructions.push(Instruction::Call(self.rt.vec_new));
            }
            "Vector.toList" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.vec_to_list));
            }
            "String.len" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.str_len));
            }
            "String.byteLength" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_byte_len));
            }
            "String.startsWith" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_starts_with));
            }
            "String.endsWith" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_ends_with));
            }
            "String.contains" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_contains));
            }
            "String.charAt" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_char_at));
            }
            "String.trim" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.str_trim));
            }
            "String.slice" if args.len() == 3 => {
                // args on stack: [str_ptr(i32), start(i64), end(i64)]
                // Convert i64 args to i32 for runtime function
                let end_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64); // end
                self.instructions.push(Instruction::LocalSet(end_local));
                self.instructions.push(Instruction::I32WrapI64); // start -> now TOS
                // Stack: [str_ptr, start_i32], need to push end_local
                let start_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(start_local));
                // Stack: [str_ptr]
                self.instructions.push(Instruction::LocalGet(start_local));
                self.instructions.push(Instruction::LocalGet(end_local));
                self.instructions.push(Instruction::Call(self.rt.str_slice));
            }
            "String.chars" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.str_chars));
            }
            "String.split" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.str_split));
            }
            "String.join" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.str_join));
            }
            // 0.15 Traversal — internal intrinsics for buffer-build
            // deforestation. NEVER emitted from user source; only the
            // codegen-synthesized buffered fn variants reach this
            // dispatch (Phase 2c.2 introduces the synthesis). Args are
            // already on the wasm stack: (buf_ptr i32, str_ptr i32) ->
            // i32 (possibly-relocated buf_ptr).
            "__buf_append" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.buffer_append_str));
            }
            // Sep-unless-first: append `sep` to `buf` only if `buf.len > 0`
            // (i.e. it already has at least one element; we want a
            // separator BEFORE the next element, not before the first).
            // Returns the possibly-grown buffer either way — the false
            // branch returns the input buf unchanged so the caller can
            // thread the result without conditional handling.
            "__buf_append_sep_unless_first" if args.len() == 2 => {
                // Stack at entry: [buf, sep] (sep on top, buf under).
                let sep = self.alloc_local(WasmType::I32);
                let buf = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(sep));
                self.instructions.push(Instruction::LocalSet(buf));
                // Compute len = (buf.header & 0xFFFFFFFF) — header is i64
                // at offset 0; OBJ_BUFFER stores byte_len in the low 32.
                self.instructions.push(Instruction::LocalGet(buf));
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                self.instructions.push(Instruction::I64And);
                self.instructions.push(Instruction::I32WrapI64);
                // If len > 0 (treated as non-zero), append sep; else
                // pass buf through.
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                // True branch: buf was non-empty, prepend sep before
                // the next element.
                self.instructions.push(Instruction::LocalGet(buf));
                self.instructions.push(Instruction::LocalGet(sep));
                self.instructions
                    .push(Instruction::Call(self.rt.buffer_append_str));
                self.emit_else();
                // False branch: empty buf, no separator yet.
                self.instructions.push(Instruction::LocalGet(buf));
                self.emit_end();
            }
            // Fusion-site wrappers: rt_buffer_new + rt_buffer_finalize
            // get the same builtin-dispatch path so the AST rewrite in
            // Phase 2c.3d can emit them as plain `Expr::FnCall(Ident,
            // args)` and stay inside normal expr lowering.
            //
            // The cap_hint arg comes from the rewriter as
            // `Expr::Literal(Literal::Int(8192))` which Aver's Int
            // type lowers to i64; rt_buffer_new wants i32, so wrap
            // before the call.
            "__buf_new" if args.len() == 1 => {
                self.instructions.push(Instruction::I32WrapI64);
                self.instructions
                    .push(Instruction::Call(self.rt.buffer_new));
            }
            "__buf_finalize" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.buffer_finalize));
            }
            "String.replace" if args.len() == 3 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_replace));
            }
            "String.toLower" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_to_lower));
            }
            "String.toUpper" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.str_to_upper));
            }
            "String.fromInt" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            "String.fromFloat" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            "String.fromBool" if args.len() == 1 => {
                let val = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(val));
                self.instructions.push(Instruction::LocalGet(val));
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                self.emit_string_literal("true");
                self.emit_else();
                self.emit_string_literal("false");
                self.emit_end();
            }
            "Int.mod" if args.len() == 2 => {
                // args: [a(i64), b(i64)] -> Result<Int, String>
                // Simplified: just return a % b wrapped in Ok
                let b_local = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b_local));
                self.instructions.push(Instruction::LocalGet(b_local));
                self.instructions.push(Instruction::I64RemS);
                // Wrap in Result.Ok
                let result = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(result));
                self.instructions
                    .push(Instruction::I32Const(value::WRAP_OK as i32));
                self.instructions.push(Instruction::LocalGet(result));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::Call(self.rt.wrap));
            }
            "Int.abs" if args.len() == 1 => {
                // if val < 0 then -val else val
                let v = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(v));
                self.instructions.push(Instruction::LocalGet(v));
                self.instructions.push(Instruction::I64Const(0));
                self.instructions.push(Instruction::I64LtS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::I64Const(0));
                self.instructions.push(Instruction::LocalGet(v));
                self.instructions.push(Instruction::I64Sub);
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(v));
                self.emit_end();
            }
            "Int.min" if args.len() == 2 => {
                let b = self.alloc_local(WasmType::I64);
                let a = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b));
                self.instructions.push(Instruction::LocalSet(a));
                self.instructions.push(Instruction::LocalGet(a));
                self.instructions.push(Instruction::LocalGet(b));
                self.instructions.push(Instruction::I64LeS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::LocalGet(a));
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(b));
                self.emit_end();
            }
            "Int.max" if args.len() == 2 => {
                let b = self.alloc_local(WasmType::I64);
                let a = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b));
                self.instructions.push(Instruction::LocalSet(a));
                self.instructions.push(Instruction::LocalGet(a));
                self.instructions.push(Instruction::LocalGet(b));
                self.instructions.push(Instruction::I64GeS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::LocalGet(a));
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(b));
                self.emit_end();
            }
            "Int.fromString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.int_from_str));
            }
            "Int.fromFloat" if args.len() == 1 => {
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.abs" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Abs);
            }
            "Float.floor" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Floor);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.ceil" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Ceil);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.round" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Nearest);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.sqrt" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Sqrt);
            }
            "Float.sin" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("math_sin") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `math_sin`");
                    self.instructions.push(Instruction::Drop);
                    self.emit_default_value(WasmType::F64);
                }
            }
            "Float.cos" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("math_cos") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `math_cos`");
                    self.instructions.push(Instruction::Drop);
                    self.emit_default_value(WasmType::F64);
                }
            }
            "Float.atan2" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_atan2") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `math_atan2`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                    self.emit_default_value(WasmType::F64);
                }
            }
            "Float.pow" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_pow") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `math_pow`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                    self.emit_default_value(WasmType::F64);
                }
            }
            "Float.min" if args.len() == 2 => {
                self.instructions.push(Instruction::F64Min);
            }
            "Float.max" if args.len() == 2 => {
                self.instructions.push(Instruction::F64Max);
            }
            "Float.pi" if args.is_empty() => {
                self.instructions
                    .push(Instruction::F64Const(std::f64::consts::PI));
            }
            "Float.toInt" if args.len() == 1 => {
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.fromString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.float_from_str));
            }
            "Bool.and" if args.len() == 2 => {
                self.instructions.push(Instruction::I32And);
            }
            "Bool.or" if args.len() == 2 => {
                self.instructions.push(Instruction::I32Or);
            }
            "Bool.not" if args.len() == 1 => {
                self.instructions.push(Instruction::I32Eqz);
            }
            "Byte.toHex" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.byte_to_hex));
            }
            "Byte.fromHex" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.byte_from_hex));
            }
            "Char.fromCode" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.char_from_code));
            }
            "Char.toCode" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.char_to_code));
            }
            "Random.int" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("random_int") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `random_int`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                    self.emit_default_value(WasmType::I64);
                }
            }
            "Random.float" if args.is_empty() => {
                if let Some(&idx) = self.host_import_indices.get("random_float") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `random_float`");
                    self.emit_default_value(WasmType::F64);
                }
            }
            "Http.get" if args.len() == 1 => {
                self.emit_http_send("GET", args, false);
            }
            "Http.head" if args.len() == 1 => {
                self.emit_http_send("HEAD", args, false);
            }
            "Http.delete" if args.len() == 1 => {
                self.emit_http_send("DELETE", args, false);
            }
            "Http.post" if args.len() == 4 => {
                self.emit_http_send("POST", args, true);
            }
            "Http.put" if args.len() == 4 => {
                self.emit_http_send("PUT", args, true);
            }
            "Http.patch" if args.len() == 4 => {
                self.emit_http_send("PATCH", args, true);
            }
            "Env.get" if args.len() == 1 => {
                // args on stack: [name(i32 OBJ_STRING ptr)]
                // Lower to:
                //   ptr+8, len → env_get → i32 raw_handle
                //   if raw_handle == -1: NONE_SENTINEL
                //   else: rt_wrap(WRAP_SOME, raw_handle as i64, ptr_flag=1)
                if let Some(&env_get_idx) = self.host_import_indices.get("env_get") {
                    let name_local = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::LocalSet(name_local));
                    self.instructions.push(Instruction::LocalGet(name_local));
                    self.instructions.push(Instruction::I32Const(8));
                    self.instructions.push(Instruction::I32Add); // name_ptr = OBJ_STRING + 8
                    self.instructions.push(Instruction::LocalGet(name_local));
                    self.instructions
                        .push(Instruction::I64Load(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                    self.instructions.push(Instruction::I64And);
                    self.instructions.push(Instruction::I32WrapI64); // name_len
                    self.instructions.push(Instruction::Call(env_get_idx));

                    // Branch on -1 sentinel.
                    let raw_local = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::LocalSet(raw_local));
                    self.instructions.push(Instruction::LocalGet(raw_local));
                    self.instructions
                        .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
                    self.instructions.push(Instruction::I32Eq);
                    self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                    self.instructions
                        .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
                    self.emit_else();
                    self.instructions
                        .push(Instruction::I32Const(super::super::value::WRAP_SOME as i32));
                    self.instructions.push(Instruction::LocalGet(raw_local));
                    self.instructions.push(Instruction::I64ExtendI32U);
                    self.instructions.push(Instruction::I32Const(1)); // value_ptr_flag
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                    self.emit_end();
                } else {
                    self.codegen_error("missing host import `env_get`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions
                        .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
                }
            }
            "Env.set" if args.len() == 2 => {
                // args on stack: [name(i32 OBJ_STRING), value(i32 OBJ_STRING)]
                // Lower to: env_set(name+8, name_len, value+8, value_len)
                if let Some(&env_set_idx) = self.host_import_indices.get("env_set") {
                    let value_local = self.alloc_local(WasmType::I32);
                    let name_local = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::LocalSet(value_local));
                    self.instructions.push(Instruction::LocalSet(name_local));

                    // name_ptr, name_len
                    self.instructions.push(Instruction::LocalGet(name_local));
                    self.instructions.push(Instruction::I32Const(8));
                    self.instructions.push(Instruction::I32Add);
                    self.instructions.push(Instruction::LocalGet(name_local));
                    self.instructions
                        .push(Instruction::I64Load(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                    self.instructions.push(Instruction::I64And);
                    self.instructions.push(Instruction::I32WrapI64);

                    // value_ptr, value_len
                    self.instructions.push(Instruction::LocalGet(value_local));
                    self.instructions.push(Instruction::I32Const(8));
                    self.instructions.push(Instruction::I32Add);
                    self.instructions.push(Instruction::LocalGet(value_local));
                    self.instructions
                        .push(Instruction::I64Load(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                    self.instructions.push(Instruction::I64And);
                    self.instructions.push(Instruction::I32WrapI64);

                    self.instructions.push(Instruction::Call(env_set_idx));
                } else {
                    self.codegen_error("missing host import `env_set`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0)); // Unit
            }
            "Console.readLine" if args.is_empty() => {
                self.emit_host_string_import("console_readLine");
                // Wrap string in Result.Ok — Console.readLine returns Result<String, String>
                // Value on stack is I32 (string object pointer).
                let tmp = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(tmp));
                self.instructions
                    .push(Instruction::I32Const(super::super::value::WRAP_OK as i32));
                self.instructions.push(Instruction::LocalGet(tmp));
                self.instructions.push(Instruction::I32Const(1)); // is_ptr=true
                self.instructions.push(Instruction::Call(self.rt.wrap_i32));
            }
            "Time.now" if args.is_empty() => {
                self.emit_host_string_import("time_now");
            }
            "Time.unixMs" if args.is_empty() => {
                if let Some(&idx) = self.host_import_indices.get("time_unixMs") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `time_unixMs`");
                    self.emit_default_value(WasmType::I64);
                }
            }
            "Time.sleep" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("time_sleep") {
                    self.instructions.push(Instruction::Call(idx));
                    self.instructions.push(Instruction::I32Const(0)); // Unit
                } else {
                    self.codegen_error("missing host import `time_sleep`");
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::I32Const(0));
                }
            }
            "Result.withDefault" if args.len() == 2 => {
                // Same as Option.withDefault
                let result_type = self.wasm_type_of(&args[1]);
                let def_local = self.alloc_local(result_type);
                let opt_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(def_local));
                self.instructions.push(Instruction::LocalSet(opt_local));
                // Check if err or none
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::I32GtS);
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                // Check tag: Ok (0) = unwrap, Err (1) = default
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions.push(Instruction::I32Eqz); // tag == 0 = Ok
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                self.instructions.push(Instruction::LocalGet(opt_local));
                match result_type {
                    WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
                    WasmType::F64 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_f64)),
                    WasmType::I32 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_i32)),
                }
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_end();
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_end();
            }
            _ => {
                // Unknown builtin → codegen error, push a placeholder value
                // so the wasm stack stays balanced. We can't know the real
                // return type without a stub registered for `name`, and
                // every known-good builtin has its own arm above. The real
                // fix is to add the missing arm; the placeholder is just
                // here so the rest of the body still compiles for diagnosis.
                self.codegen_error(format!("unknown builtin `{}` in WASM backend", name));
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.emit_default_value(WasmType::I64);
            }
        }
    }

    pub(super) fn emit_console_print(&mut self, args: &[Spanned<Expr>], target_import: &str) {
        // Always emit Aver-style host calls regardless of --adapter.
        // Under --bridge wasip1 the aver_to_wasi shim translates these
        // into wasi_snapshot_preview1 calls; the user.wasm bytes are
        // identical in both modes.
        //
        // `target_import` picks the destination stream — `console_print`
        // (stdout), `console_error` (stderr), or `console_warn` (stderr
        // routed through `console.warn` on JS hosts). When the target
        // is `console_print` we keep the old shape: value goes through
        // `print_value` (a single host crossing that formats + writes
        // in one shot), newline through `console_print`. For the
        // stderr targets we have to format-then-write separately —
        // value via `format_value` (returns ptr+len), then a
        // `console_<target>` write — so the value lands on the right
        // stream instead of always defaulting to stdout via print_value.
        let route_through_format = target_import != "console_print";
        if route_through_format {
            self.emit_console_print_via_format(args, target_import);
            return;
        }

        if let Some(&print_idx) = self.host_import_indices.get("print_value") {
            let arg_aver_type = self.aver_type_of(&args[0]);
            let wt = aver_type_to_wasm(arg_aver_type);

            let (tag, needs_convert) = match arg_aver_type {
                Type::Int => (0i32, false),
                Type::Float => (1, true), // f64 → reinterpret to i64
                Type::Bool => (2, false), // will extend i32 to i64
                Type::Str => (3, false),
                Type::Unit => (5, false),
                _ => match wt {
                    WasmType::I64 => (0, false),
                    WasmType::F64 => (1, true),
                    WasmType::I32 => (4, false), // heap pointer
                },
            };

            if tag == 5 {
                // Unit → don't print, just drop
                self.instructions.push(Instruction::Drop);
            } else {
                // Convert value to i64
                let val = self.alloc_local(WasmType::I64);
                match (wt, needs_convert) {
                    (WasmType::F64, true) => {
                        self.instructions.push(Instruction::I64ReinterpretF64);
                    }
                    (WasmType::I32, _) => {
                        self.instructions.push(Instruction::I64ExtendI32S);
                    }
                    _ => {} // already i64
                }
                self.instructions.push(Instruction::LocalSet(val));
                self.instructions.push(Instruction::I32Const(tag));
                self.instructions.push(Instruction::LocalGet(val));
                self.instructions.push(Instruction::Call(print_idx));
            }
        } else {
            // No host print — drop value
            self.instructions.push(Instruction::Drop);
        }
        // Newline: write '\n' to scratch + call aver/console_print(addr, 1).
        let console_print_idx = self
            .host_import_indices
            .get("console_print")
            .copied()
            .unwrap_or(0);
        self.instructions.push(Instruction::I32Const(
            super::super::runtime::NEWLINE_ADDR as i32,
        ));
        self.instructions.push(Instruction::I32Const(b'\n' as i32));
        self.instructions
            .push(Instruction::I32Store8(wasm_encoder::MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
        self.instructions.push(Instruction::I32Const(
            super::super::runtime::NEWLINE_ADDR as i32,
        ));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions.push(Instruction::Call(console_print_idx));
        self.instructions.push(Instruction::I32Const(0)); // Unit
    }

    /// Stderr-routed Console.error / Console.warn path: format the
    /// value to (ptr, len) via `format_value`, then write through
    /// the stderr-targeting host import. The legacy `print_value`
    /// path always writes to stdout in the WASI bridge — using it
    /// for `Console.error` would silently send errors to stdout,
    /// which a console-aware caller (CI runner, log shipper) cannot
    /// distinguish from `Console.print`. This path costs one extra
    /// host crossing per call versus the stdout path; the routing
    /// correctness wins.
    fn emit_console_print_via_format(&mut self, args: &[Spanned<Expr>], target_import: &str) {
        let target_idx = match self.host_import_indices.get(target_import).copied() {
            Some(idx) => idx,
            None => {
                // No host is satisfying this target — drop the value to
                // keep the stack typed; this matches the
                // print_value-less fallback in the stdout path.
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
                return;
            }
        };
        let format_idx = match self.host_import_indices.get("format_value").copied() {
            Some(idx) => idx,
            None => {
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
                return;
            }
        };

        let arg_aver_type = self.aver_type_of(&args[0]);
        let wt = aver_type_to_wasm(arg_aver_type);

        let (tag, needs_convert) = match arg_aver_type {
            Type::Int => (0i32, false),
            Type::Float => (1, true),
            Type::Bool => (2, false),
            Type::Str => (3, false),
            Type::Unit => (5, false),
            _ => match wt {
                WasmType::I64 => (0, false),
                WasmType::F64 => (1, true),
                WasmType::I32 => (4, false),
            },
        };

        if tag == 5 {
            // Unit — nothing to print, but still emit the trailing
            // newline so the stream stays line-oriented.
            self.instructions.push(Instruction::Drop);
        } else {
            let val_local = self.alloc_local(WasmType::I64);
            // Move the arg value into an i64 local matching format_value's
            // (i32, i64) -> (i32, i32) signature.
            match wt {
                WasmType::I64 => {}
                WasmType::F64 => {
                    if needs_convert {
                        self.instructions.push(Instruction::I64ReinterpretF64);
                    }
                }
                WasmType::I32 => {
                    if tag == 3 || tag == 4 {
                        self.instructions.push(Instruction::I64ExtendI32U);
                    } else {
                        self.instructions.push(Instruction::I64ExtendI32S);
                    }
                }
            }
            self.instructions.push(Instruction::LocalSet(val_local));

            // format_value(tag, val) — pushes (ptr, len) onto the stack.
            self.instructions.push(Instruction::I32Const(tag));
            self.instructions.push(Instruction::LocalGet(val_local));
            self.instructions.push(Instruction::Call(format_idx));

            // console_<target>(ptr, len) — drains the (ptr, len) pair.
            self.instructions.push(Instruction::Call(target_idx));
        }

        // Trailing newline through the same target stream.
        self.instructions.push(Instruction::I32Const(
            super::super::runtime::NEWLINE_ADDR as i32,
        ));
        self.instructions.push(Instruction::I32Const(b'\n' as i32));
        self.instructions
            .push(Instruction::I32Store8(wasm_encoder::MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
        self.instructions.push(Instruction::I32Const(
            super::super::runtime::NEWLINE_ADDR as i32,
        ));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions.push(Instruction::Call(target_idx));
        self.instructions.push(Instruction::I32Const(0)); // Unit
    }

    pub(super) fn emit_list_prepend(&mut self, args: &[Spanned<Expr>]) {
        let elem_type = self.wasm_type_of(&args[0]);
        let elem_is_ptr = self.expr_is_heap_ptr_spanned(&args[0]);
        match elem_type {
            WasmType::F64 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_cons_f64));
            }
            WasmType::I32 => {
                let tail_tmp = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(tail_tmp));
                self.instructions.push(Instruction::I64ExtendI32S);
                self.instructions.push(Instruction::LocalGet(tail_tmp));
                self.instructions
                    .push(Instruction::I32Const(if elem_is_ptr { 1 } else { 0 }));
                self.instructions.push(Instruction::Call(self.rt.list_cons));
            }
            _ => {
                self.instructions
                    .push(Instruction::I32Const(if elem_is_ptr { 1 } else { 0 }));
                self.instructions.push(Instruction::Call(self.rt.list_cons));
            }
        }
    }

    pub(super) fn emit_list_len(&mut self) {
        // List pointer on stack (i32). Count cons cells.
        let ptr = self.alloc_local(WasmType::I32);
        let count = self.alloc_local(WasmType::I64);
        self.instructions.push(Instruction::LocalSet(ptr));
        self.instructions.push(Instruction::I64Const(0));
        self.instructions.push(Instruction::LocalSet(count));
        // Loop
        self.instructions
            .push(Instruction::Block(wasm_encoder::BlockType::Empty));
        self.instructions
            .push(Instruction::Loop(wasm_encoder::BlockType::Empty));
        self.instructions.push(Instruction::LocalGet(ptr));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));
        // count++
        self.instructions.push(Instruction::LocalGet(count));
        self.instructions.push(Instruction::I64Const(1));
        self.instructions.push(Instruction::I64Add);
        self.instructions.push(Instruction::LocalSet(count));
        // ptr = tail (field[1] as i32)
        self.instructions.push(Instruction::LocalGet(ptr));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(ptr));
        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End); // loop
        self.instructions.push(Instruction::End); // block
        self.instructions.push(Instruction::LocalGet(count));
    }

    fn emit_string_from_ptr_len(&mut self, ptr_local: u32, len_local: u32) {
        let str_ptr = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::LocalGet(len_local));
        self.instructions.push(Instruction::I32Const(7));
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::I32Const(-8i32));
        self.instructions.push(Instruction::I32And);
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(str_ptr));
        self.instructions.push(Instruction::LocalGet(str_ptr));
        self.instructions.push(Instruction::I64Const(
            (value::OBJ_STRING << value::HDR_KIND_SHIFT) as i64,
        ));
        self.instructions.push(Instruction::LocalGet(len_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::I64Or);
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        self.instructions.push(Instruction::LocalGet(str_ptr));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions.push(Instruction::LocalGet(len_local));
        self.instructions.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        self.instructions.push(Instruction::LocalGet(str_ptr));
    }

    fn emit_host_string_import(&mut self, import_name: &str) {
        if let Some(&idx) = self.host_import_indices.get(import_name) {
            let ptr = self.alloc_local(WasmType::I32);
            let len = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::LocalSet(len));
            self.instructions.push(Instruction::LocalSet(ptr));
            self.emit_string_from_ptr_len(ptr, len);
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.emit_default_value(WasmType::I32);
        }
    }

    fn emit_host_unit_import(&mut self, import_name: &str) {
        if let Some(&idx) = self.host_import_indices.get(import_name) {
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::I32Const(0));
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.instructions.push(Instruction::I32Const(0));
        }
    }

    fn emit_host_move_to_import(&mut self, import_name: &str) {
        let y_local = self.alloc_local(WasmType::I32);
        let x_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(y_local));
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(x_local));
        if let Some(&idx) = self.host_import_indices.get(import_name) {
            self.instructions.push(Instruction::LocalGet(x_local));
            self.instructions.push(Instruction::LocalGet(y_local));
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::I32Const(0));
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.instructions.push(Instruction::I32Const(0));
        }
    }

    fn emit_host_string_consumer_import(
        &mut self,
        import_name: &str,
        wasm_type: WasmType,
        aver_type: Option<&Type>,
        format_if_needed: bool,
    ) {
        if format_if_needed || !matches!(aver_type, Some(Type::Str)) {
            self.emit_stack_value_to_str(wasm_type, aver_type);
        }

        let str_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::LocalSet(str_local));

        if let Some(&idx) = self.host_import_indices.get(import_name) {
            self.instructions.push(Instruction::LocalGet(str_local));
            self.instructions.push(Instruction::I32Const(8));
            self.instructions.push(Instruction::I32Add);
            self.instructions.push(Instruction::LocalGet(str_local));
            self.instructions
                .push(Instruction::Call(self.rt.str_byte_len));
            self.instructions.push(Instruction::I32WrapI64);
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::I32Const(0));
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.instructions.push(Instruction::I32Const(0));
        }
    }

    fn emit_host_option_string_import(&mut self, import_name: &str) {
        if let Some(&idx) = self.host_import_indices.get(import_name) {
            let ptr = self.alloc_local(WasmType::I32);
            let len = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::LocalSet(len));
            self.instructions.push(Instruction::LocalSet(ptr));
            self.instructions.push(Instruction::LocalGet(ptr));
            self.instructions
                .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
            self.instructions.push(Instruction::I32Eq);
            self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
            self.instructions
                .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
            self.emit_else();
            let str_local = self.alloc_local(WasmType::I32);
            self.emit_string_from_ptr_len(ptr, len);
            self.instructions.push(Instruction::LocalSet(str_local));
            self.instructions
                .push(Instruction::I32Const(value::WRAP_SOME as i32));
            self.instructions.push(Instruction::LocalGet(str_local));
            self.instructions.push(Instruction::I32Const(1));
            self.instructions.push(Instruction::Call(self.rt.wrap_i32));
            self.emit_end();
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.instructions
                .push(Instruction::I32Const(super::super::value::NONE_SENTINEL));
        }
    }

    fn emit_args_get_import(&mut self) {
        let Some(&len_idx) = self.host_import_indices.get("args_len") else {
            self.codegen_error("missing host import `args_len`");
            self.emit_default_value(WasmType::I32);
            return;
        };
        let Some(&get_idx) = self.host_import_indices.get("args_get") else {
            self.codegen_error("missing host import `args_get`");
            self.emit_default_value(WasmType::I32);
            return;
        };

        let count = self.alloc_local(WasmType::I32);
        let list = self.alloc_local(WasmType::I32);
        let ptr = self.alloc_local(WasmType::I32);
        let len = self.alloc_local(WasmType::I32);
        let str_local = self.alloc_local(WasmType::I32);

        self.instructions.push(Instruction::Call(len_idx));
        self.instructions.push(Instruction::LocalSet(count));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::LocalSet(list));

        self.instructions
            .push(Instruction::Block(wasm_encoder::BlockType::Empty));
        self.instructions
            .push(Instruction::Loop(wasm_encoder::BlockType::Empty));
        self.instructions.push(Instruction::LocalGet(count));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));

        self.instructions.push(Instruction::LocalGet(count));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions.push(Instruction::I32Sub);
        self.instructions.push(Instruction::LocalTee(count));
        self.instructions.push(Instruction::Call(get_idx));
        self.instructions.push(Instruction::LocalSet(len));
        self.instructions.push(Instruction::LocalSet(ptr));

        self.emit_string_from_ptr_len(ptr, len);
        self.instructions.push(Instruction::LocalSet(str_local));
        self.instructions.push(Instruction::LocalGet(str_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::LocalGet(list));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions.push(Instruction::Call(self.rt.list_cons));
        self.instructions.push(Instruction::LocalSet(list));
        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End);
        self.instructions.push(Instruction::End);
        self.instructions.push(Instruction::LocalGet(list));
    }

    fn emit_terminal_size_import(&mut self, import_name: &str) {
        if let Some(&idx) = self.host_import_indices.get(import_name) {
            let width = self.alloc_local(WasmType::I32);
            let height = self.alloc_local(WasmType::I32);
            let width_i64 = self.alloc_local(WasmType::I64);
            let height_i64 = self.alloc_local(WasmType::I64);
            self.instructions.push(Instruction::Call(idx));
            self.instructions.push(Instruction::LocalSet(height));
            self.instructions.push(Instruction::LocalSet(width));
            self.instructions.push(Instruction::LocalGet(width));
            self.instructions.push(Instruction::I64ExtendI32S);
            self.instructions.push(Instruction::LocalSet(width_i64));
            self.instructions.push(Instruction::LocalGet(height));
            self.instructions.push(Instruction::I64ExtendI32S);
            self.instructions.push(Instruction::LocalSet(height_i64));
            self.emit_record_from_locals(&[
                (width_i64, WasmType::I64),
                (height_i64, WasmType::I64),
            ]);
        } else {
            self.codegen_error(format!("missing host import `{}`", import_name));
            self.emit_default_value(WasmType::I32);
        }
    }

    fn emit_stack_value_to_str(&mut self, wt: WasmType, at: Option<&Type>) {
        if matches!(at, Some(Type::Str)) {
            return;
        }

        if let Some(&fmt_idx) = self.host_import_indices.get("format_value") {
            let tag = match at {
                Some(Type::Int) => 0i32,
                Some(Type::Float) => 1,
                Some(Type::Bool) => 2,
                _ => match wt {
                    WasmType::I64 => 0,
                    WasmType::F64 => 1,
                    WasmType::I32 => 4,
                },
            };
            match (wt, tag) {
                (WasmType::F64, _) => self.instructions.push(Instruction::I64ReinterpretF64),
                (WasmType::I32, _) => self.instructions.push(Instruction::I64ExtendI32S),
                _ => {}
            }
            let val = self.alloc_local(WasmType::I64);
            self.instructions.push(Instruction::LocalSet(val));
            self.instructions.push(Instruction::I32Const(tag));
            self.instructions.push(Instruction::LocalGet(val));
            self.instructions.push(Instruction::Call(fmt_idx));
            let len = self.alloc_local(WasmType::I32);
            let ptr = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(len));
            self.instructions.push(Instruction::LocalSet(ptr));
            self.emit_string_from_ptr_len(ptr, len);
        } else {
            match wt {
                WasmType::I64 => self
                    .instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj)),
                WasmType::F64 => self
                    .instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj)),
                WasmType::I32 => {
                    self.codegen_error(
                        "missing host import `format_value` for heap value formatting",
                    );
                    self.emit_default_value(WasmType::I32);
                }
            }
        }
    }

    pub(super) fn emit_value_to_str(&mut self, expr: &Spanned<Expr>) {
        let at = self.aver_type_of(expr).clone();
        let wt = aver_type_to_wasm(&at);
        self.emit_expr(expr);
        self.emit_stack_value_to_str(wt, Some(&at));
    }

    /// Top-of-stack holds a Map key — normalize it to the i64 lane the
    /// runtime expects and return the kind tag the runtime dispatches on:
    ///   0 = Int, 1 = Float, 2 = Bool, 3 = String, 4 = Heap (deep-eq).
    /// For pointer kinds we zero-extend the i32 ptr (high bits must
    /// stay zero so i32.wrap_i64 round-trips). For Int/Bool we
    /// sign-extend; for Float we reinterpret f64 bits.
    /// Static counterpart of `emit_map_key_normalize` for the Map
    /// literal / `Map.fromList` paths, where the key is buried inside
    /// a `List<(K, V)>` and we only have its declared element type.
    /// Returns `(key_kind, value_ptr_flag)` so the emitter can push
    /// them as constants ahead of the `rt_map_from_list` call.
    pub(super) fn map_from_list_kind_and_ptr_flag(
        &mut self,
        list_expr: &Spanned<Expr>,
    ) -> (i32, i32) {
        let (key_ty, value_ty) = match self.aver_type_of(list_expr) {
            Type::List(inner) => match inner.as_ref() {
                Type::Tuple(parts) if parts.len() == 2 => (Some(&parts[0]), Some(&parts[1])),
                _ => (None, None),
            },
            _ => (None, None),
        };
        let key_kind = self.kind_for_aver_type(key_ty);
        let value_ptr_flag = self.value_is_heap_aver_type(value_ty);
        (key_kind, value_ptr_flag)
    }

    pub(super) fn kind_for_aver_type(&self, ty: Option<&Type>) -> i32 {
        match ty {
            Some(Type::Int) => 0,
            Some(Type::Float) => 1,
            Some(Type::Bool) => 2,
            Some(Type::Str) => 3,
            // Heap (records, lists, etc.) — runtime falls back to deep-eq.
            _ => 4,
        }
    }

    pub(super) fn value_is_heap_aver_type(&self, ty: Option<&Type>) -> i32 {
        match ty {
            Some(Type::Int) | Some(Type::Float) | Some(Type::Bool) => 0,
            // Strings, lists, maps, vectors, records, variants, options,
            // results, tuples — all heap pointers in the WASM ABI.
            _ => 1,
        }
    }

    pub(super) fn emit_map_key_normalize(&mut self, key: &Spanned<Expr>) -> i32 {
        let at = self.aver_type_of(key);
        let wt = aver_type_to_wasm(at);
        let kind = match at {
            Type::Int => 0,
            Type::Float => 1,
            Type::Bool => 2,
            Type::Str => 3,
            _ => match wt {
                WasmType::I64 => 0,
                WasmType::F64 => 1,
                WasmType::I32 => 4,
            },
        };
        match wt {
            WasmType::I64 => {}
            WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
            WasmType::I32 => {
                if kind == 3 || kind == 4 {
                    self.instructions.push(Instruction::I64ExtendI32U);
                } else {
                    self.instructions.push(Instruction::I64ExtendI32S);
                }
            }
        }
        kind
    }
}
