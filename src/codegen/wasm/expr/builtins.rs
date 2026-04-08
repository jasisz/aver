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
            "Console.print" | "Console.error" | "Console.warn" => {
                self.emit_console_print(args);
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
                let val_type = self.infer_expr_type(&args[2].node);
                match val_type {
                    WasmType::I64 => {} // already i64
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::Call(self.rt.map_set));
            }
            "Map.has" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.map_has));
            }
            "Map.keys" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.map_keys));
            }
            "Map.entries" if args.len() == 1 => {
                // Map IS a list of tuples -- identity
            }
            "Map.fromList" if args.len() == 1 => {
                // Identity -- list of tuples IS a map
            }
            "Option.withDefault" if args.len() == 2 => {
                // args: [option(i32), default]
                // Check if option == NONE_SENTINEL -> return default, else unwrap
                let opt_local = self.alloc_local(WasmType::I32);
                let result_type = self.infer_expr_type(&args[1].node);
                self.instructions.push(Instruction::LocalSet(opt_local)); // save default
                // Wait -- args already on stack: [option, default]
                // Need to reorder. Actually let me save both.
                let def_local = self.alloc_local(result_type);
                // Stack has [option, default] -- save default first (TOS)
                // Actually emit_builtin_call receives args already on stack in order.
                // Stack: [arg0=option, arg1=default]
                // But WASM stack is LIFO. After emitting args left to right,
                // TOS = default, below = option.
                // Save default (TOS)
                self.instructions.push(Instruction::LocalSet(def_local));
                // Now TOS = option
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
                self.instructions.push(Instruction::Call(self.rt.vec_set));
            }
            "Vector.new" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.vec_new));
            }
            "Vector.toList" if args.len() == 1 => {
                // Convert vector back to list -- TODO full implementation
                // For now drop and return empty list
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
            }
            "String.len" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                self.instructions.push(Instruction::I64And);
            }
            "String.charAt" if args.len() == 2 => {
                // args: [str_ptr(i32), idx(i64)] -> returns String (single char)
                // Load byte at str_ptr + 8 + idx, create 1-char string
                let idx_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64); // idx -> i32
                self.instructions.push(Instruction::LocalSet(idx_local));
                let str_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(str_local));
                // Alloc 1-char string object: 8 header + 8 padded
                let ptr = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32Const(16));
                self.instructions.push(Instruction::Call(self.rt.alloc));
                self.instructions.push(Instruction::LocalSet(ptr));
                // Header: OBJ_STRING, len=1
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::I64Const(
                    (value::OBJ_STRING << value::HDR_KIND_SHIFT | 1) as i64,
                ));
                self.instructions
                    .push(Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                // Copy byte
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::LocalGet(str_local));
                self.instructions.push(Instruction::I32Const(8));
                self.instructions.push(Instruction::I32Add);
                self.instructions.push(Instruction::LocalGet(idx_local));
                self.instructions.push(Instruction::I32Add);
                self.instructions
                    .push(Instruction::I32Load8U(wasm_encoder::MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions
                    .push(Instruction::I32Store8(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
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
            "String.join" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.str_join));
            }
            "String.startsWith" | "String.endsWith" | "String.contains" | "String.replace"
            | "String.split" | "String.toUpper" | "String.toLower" | "String.byteLength"
                if !args.is_empty() =>
            {
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0));
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
                // bool i32 -> "true"/"false" string
                // Simplified: convert to int then to string
                self.instructions.push(Instruction::I64ExtendI32S);
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
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
                // Host import -- no native WASM instruction
                if let Some(&idx) = self.host_import_indices.get("math_sin") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    // Fallback: return 0.0
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(0.0));
                }
            }
            "Float.cos" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("math_cos") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(1.0));
                }
            }
            "Float.atan2" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_atan2") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(0.0));
                }
            }
            "Float.pow" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_pow") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
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
            "Bool.and" if args.len() == 2 => {
                self.instructions.push(Instruction::I32And);
            }
            "Bool.or" if args.len() == 2 => {
                self.instructions.push(Instruction::I32Or);
            }
            "Bool.not" if args.len() == 1 => {
                self.instructions.push(Instruction::I32Eqz);
            }
            "Char.fromCode" if args.len() == 1 => {
                // Int(i64) -> single-char String
                // Alloc 1-char string, store byte
                let code = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64);
                self.instructions.push(Instruction::LocalSet(code));
                let ptr = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32Const(16));
                self.instructions.push(Instruction::Call(self.rt.alloc));
                self.instructions.push(Instruction::LocalSet(ptr));
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::I64Const(
                    (value::OBJ_STRING << value::HDR_KIND_SHIFT | 1) as i64,
                ));
                self.instructions
                    .push(Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::LocalGet(code));
                self.instructions
                    .push(Instruction::I32Store8(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
            }
            "Char.toCode" if args.len() == 1 => {
                // String -> Int (first byte code)
                self.instructions
                    .push(Instruction::I32Load8U(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64ExtendI32U);
            }
            "Random.int" if args.len() == 2 => {
                // Stub: return min (first arg)
                self.instructions.push(Instruction::Drop); // drop max
                // min stays
            }
            "Console.readLine" if args.is_empty() => {
                // Stub: return empty string
                self.emit_string_literal("");
            }
            "Time.now" if args.is_empty() => {
                // Host import returns (ptr, len) — build string object
                if let Some(&idx) = self.host_import_indices.get("time_now") {
                    let ptr = self.alloc_local(WasmType::I32);
                    let len = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::Call(idx));
                    self.instructions.push(Instruction::LocalSet(len));
                    self.instructions.push(Instruction::LocalSet(ptr));
                    // Alloc string object: header + bytes
                    let str_ptr = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::I32Const(8));
                    self.instructions.push(Instruction::LocalGet(len));
                    self.instructions.push(Instruction::I32Const(7));
                    self.instructions.push(Instruction::I32Add);
                    self.instructions.push(Instruction::I32Const(-8i32));
                    self.instructions.push(Instruction::I32And);
                    self.instructions.push(Instruction::I32Add);
                    self.instructions.push(Instruction::Call(self.rt.alloc));
                    self.instructions.push(Instruction::LocalSet(str_ptr));
                    // Header
                    self.instructions.push(Instruction::LocalGet(str_ptr));
                    self.instructions.push(Instruction::I64Const(
                        (value::OBJ_STRING << value::HDR_KIND_SHIFT) as i64,
                    ));
                    self.instructions.push(Instruction::LocalGet(len));
                    self.instructions.push(Instruction::I64ExtendI32U);
                    self.instructions.push(Instruction::I64Or);
                    self.instructions
                        .push(Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    // Copy bytes from host ptr to string object
                    self.instructions.push(Instruction::LocalGet(str_ptr));
                    self.instructions.push(Instruction::I32Const(8));
                    self.instructions.push(Instruction::I32Add);
                    self.instructions.push(Instruction::LocalGet(ptr));
                    self.instructions.push(Instruction::LocalGet(len));
                    self.instructions.push(Instruction::MemoryCopy {
                        src_mem: 0,
                        dst_mem: 0,
                    });
                    self.instructions.push(Instruction::LocalGet(str_ptr));
                } else {
                    self.emit_string_literal("");
                }
            }
            "Time.unixMs" if args.is_empty() => {
                if let Some(&idx) = self.host_import_indices.get("time_unixMs") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::I64Const(0));
                }
            }
            "Time.sleep" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("time_sleep") {
                    self.instructions.push(Instruction::Call(idx));
                    self.instructions.push(Instruction::I32Const(0)); // Unit
                } else {
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
                // Unknown builtin -- drop args, return default for inferred type
                let ret_type = self.infer_call_return_type(&crate::ast::Spanned {
                    node: crate::ast::Expr::Ident(name.to_string()),
                    line: 0,
                });
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                match ret_type {
                    WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
                    WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
                    WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
                }
            }
        }
    }

    pub(super) fn emit_console_print(&mut self, args: &[Spanned<Expr>]) {
        let arg_aver_type = self.infer_aver_type(&args[0].node);

        match &arg_aver_type {
            Some(Type::Int) => {
                self.instructions.push(Instruction::Call(self.rt.print_i64));
            }
            Some(Type::Float) => {
                self.instructions.push(Instruction::Call(self.rt.print_f64));
            }
            Some(Type::Bool) => {
                self.instructions
                    .push(Instruction::Call(self.rt.print_bool));
            }
            Some(Type::Str) => {
                self.instructions
                    .push(Instruction::Call(self.rt.print_string));
            }
            Some(Type::Unit) => {
                self.instructions.push(Instruction::Drop);
            }
            _ => {
                let wt = self.infer_expr_type(&args[0].node);
                match wt {
                    WasmType::I64 => {
                        self.instructions.push(Instruction::Call(self.rt.print_i64));
                    }
                    WasmType::F64 => {
                        self.instructions.push(Instruction::Call(self.rt.print_f64));
                    }
                    WasmType::I32 => {
                        self.instructions
                            .push(Instruction::Call(self.rt.print_heap));
                    }
                }
            }
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
                self.instructions.push(Instruction::Call(self.rt.list_cons));
            }
            _ => {
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

    pub(super) fn emit_value_to_str(&mut self, expr: &Expr) {
        let wt = self.infer_expr_type(expr);
        let at = self.infer_aver_type(expr);
        self.emit_expr(expr);
        match at {
            Some(Type::Str) => {} // already a string pointer
            Some(Type::Int) => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            Some(Type::Float) => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            Some(Type::Bool) => {
                // Bool i32 → "true" or "false" string
                // Use if/else to pick the right static string
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                self.emit_string_literal("true");
                self.emit_else();
                self.emit_string_literal("false");
                self.emit_end();
            }
            Some(Type::List(_) | Type::Result(_, _) | Type::Option(_) | Type::Named(_)) => {
                // TODO: proper to_string for complex heap types
                // For now: convert pointer to number string (shows address)
                self.instructions.push(Instruction::I64ExtendI32S);
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            _ => match wt {
                WasmType::I64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.i64_to_str_obj));
                }
                WasmType::F64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.f64_to_str_obj));
                }
                WasmType::I32 => {} // assume string pointer
            },
        }
    }
}
