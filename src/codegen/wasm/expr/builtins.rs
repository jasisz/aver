/// Builtin call emission for ExprEmitter.
///
/// Handles Aver builtin namespace calls: Console.print, List.*, Float.*, Int.*, etc.
use wasm_encoder::Instruction;

use crate::ast::{Expr, Spanned};
use crate::types::Type;

use super::super::types::WasmType;
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
                self.emit_console_print(args);
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
                let aver_type = self.infer_aver_type(&args[0].node);
                let wasm_type = self.infer_expr_type(&args[0].node);
                self.emit_host_string_consumer_import(
                    "terminal_print",
                    wasm_type,
                    aver_type.as_ref(),
                    true,
                );
            }
            "Terminal.setColor" if args.len() == 1 => {
                let aver_type = self.infer_aver_type(&args[0].node);
                let wasm_type = self.infer_expr_type(&args[0].node);
                self.emit_host_string_consumer_import(
                    "terminal_setColor",
                    wasm_type,
                    aver_type.as_ref(),
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
                let val_type = self.infer_expr_type(&args[1].node);
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
                // args: [map(i32), key(i32)]
                self.instructions.push(Instruction::Call(self.rt.map_get));
            }
            "Map.set" if args.len() == 3 => {
                // args: [map(i32), key(i32), value(?)]
                // value needs to be i64 for map_set
                let value_is_ptr = self.expr_is_heap_ptr(&args[2].node);
                let val_type = self.infer_expr_type(&args[2].node);
                match val_type {
                    WasmType::I64 => {} // already i64
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions
                    .push(Instruction::I32Const(if value_is_ptr { 1 } else { 0 }));
                self.instructions.push(Instruction::Call(self.rt.map_set));
            }
            "Map.len" if args.len() == 1 => {
                // Walk cons list, count entries. Map ptr is on stack.
                let map_local = self.alloc_local(WasmType::I32);
                let count_local = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(map_local));
                self.instructions.push(Instruction::I64Const(0));
                self.instructions.push(Instruction::LocalSet(count_local));
                self.instructions
                    .push(Instruction::Block(wasm_encoder::BlockType::Empty));
                self.block_depth += 1;
                self.instructions
                    .push(Instruction::Loop(wasm_encoder::BlockType::Empty));
                self.block_depth += 1;
                self.instructions.push(Instruction::LocalGet(map_local));
                self.instructions.push(Instruction::I32Eqz);
                self.instructions.push(Instruction::BrIf(1));
                // count += 1
                self.instructions.push(Instruction::LocalGet(count_local));
                self.instructions.push(Instruction::I64Const(1));
                self.instructions.push(Instruction::I64Add);
                self.instructions.push(Instruction::LocalSet(count_local));
                // map = tail (field 1)
                self.instructions.push(Instruction::LocalGet(map_local));
                self.instructions.push(Instruction::I32Const(1));
                self.instructions
                    .push(Instruction::Call(self.rt.obj_field_i32));
                self.instructions.push(Instruction::LocalSet(map_local));
                self.instructions.push(Instruction::Br(0));
                self.emit_end();
                self.emit_end();
                self.instructions.push(Instruction::LocalGet(count_local));
            }
            "Map.has" if args.len() == 2 => {
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
                // Identity -- list of tuples IS a map
            }
            "Option.withDefault" if args.len() == 2 => {
                // args: [option(i32), default]
                // Check if option == NONE_SENTINEL -> return default, else unwrap
                let opt_local = self.alloc_local(WasmType::I32);
                let result_type = self.infer_expr_type(&args[1].node);
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
            "Vector.fromList" if args.len() == 1 => {
                let elem_is_ptr = self
                    .infer_aver_type(&args[0].node)
                    .as_ref()
                    .and_then(|ty| match ty {
                        Type::List(elem) => Some(self.is_heap_type(elem)),
                        _ => None,
                    })
                    .unwrap_or(false);
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
                let val_type = self.infer_expr_type(&args[2].node);
                match val_type {
                    WasmType::I64 => {}
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::Call(self.rt.vec_set));
            }
            "Vector.new" if args.len() == 2 => {
                let fill_type = self.infer_expr_type(&args[1].node);
                match fill_type {
                    WasmType::I64 => {}
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::I32Const(
                    if self.expr_is_heap_ptr(&args[1].node) {
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
            "Console.readLine" if args.is_empty() => {
                self.emit_host_string_import("console_readLine");
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
                let result_type = self.infer_expr_type(&args[1].node);
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
                let ret_type = self.infer_call_return_type(
                    &crate::ast::Spanned {
                        node: crate::ast::Expr::Ident(name.to_string()),
                        line: 0,
                    },
                    args,
                );
                self.codegen_error(format!("unknown builtin `{}` in WASM backend", name));
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.emit_default_value(ret_type);
            }
        }
    }

    pub(super) fn emit_console_print(&mut self, args: &[Spanned<Expr>]) {
        // All printing delegated to host via print_value(tag, val)
        // tag: 0=Int, 1=Float, 2=Bool, 3=String, 4=Heap, 5=Unit
        if let Some(&print_idx) = self.host_import_indices.get("print_value") {
            let arg_aver_type = self.infer_aver_type(&args[0].node);
            let wt = self.infer_expr_type(&args[0].node);

            let (tag, needs_convert) = match &arg_aver_type {
                Some(Type::Int) => (0i32, false),
                Some(Type::Float) => (1, true), // f64 → reinterpret to i64
                Some(Type::Bool) => (2, false), // will extend i32 to i64
                Some(Type::Str) => (3, false),
                Some(Type::Unit) => (5, false),
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
        // Newline
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
        self.instructions
            .push(Instruction::Call(self.rt.fd_write_buf));
        self.instructions.push(Instruction::I32Const(0)); // Unit
    }

    pub(super) fn emit_list_prepend(&mut self, args: &[Spanned<Expr>]) {
        let elem_type = self.infer_expr_type(&args[0].node);
        let elem_is_ptr = self.expr_is_heap_ptr(&args[0].node);
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

    pub(super) fn emit_value_to_str(&mut self, expr: &Expr) {
        let wt = self.infer_expr_type(expr);
        let at = self.infer_aver_type(expr);
        self.emit_expr(expr);
        self.emit_stack_value_to_str(wt, at.as_ref());
    }
}
