//! Admission follows definition-backed Dafny operations, not namespace names.
//! Every argument cone is checked before its operation is considered. Dynamic
//! partial builtins retain Result/Option, including their exact error branches.

use super::*;

fn result(ty: Type) -> Type {
    Type::Result(Box::new(ty), Box::new(Type::Str))
}

impl Checker<'_> {
    pub(super) fn defined_builtin(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
        env: &Env,
    ) -> Result<Type, String> {
        let values = args
            .iter()
            .map(|arg| self.expression(arg, env))
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(output) = super::super::arithmetic::division_result_type(name, args) {
            for value in &values {
                merge(value, &Type::Int)?;
            }
            return Ok(output);
        }
        Ok(match (name, values.as_slice()) {
            ("Bool.and" | "Bool.or", [Type::Bool, Type::Bool]) | ("Bool.not", [Type::Bool]) => {
                Type::Bool
            }
            ("Int.abs" | "Bits.not", [Type::Int])
            | (
                "Int.min" | "Int.max" | "Bits.and" | "Bits.or" | "Bits.xor",
                [Type::Int, Type::Int],
            ) => Type::Int,
            ("Bits.shiftLeft" | "Bits.shiftRight" | "Bits.low", [Type::Int, Type::Int]) => {
                // Use the same operation-specific syntactic predicates as HIR
                // and typechecking, including shiftRight's unbounded count.
                let discharged = if name == "Bits.shiftRight" {
                    crate::ast::is_literal_nonneg_shift_right_count(&args[1])
                } else {
                    crate::ast::is_literal_nonneg_int_count(&args[1])
                };
                if discharged {
                    Type::Int
                } else {
                    result(Type::Int)
                }
            }
            ("String.len", [Type::Str]) => Type::Int,
            ("String.fromInt", [Type::Int]) | ("String.fromBool", [Type::Bool]) => Type::Str,
            ("String.charAt", [Type::Str, Type::Int]) => Type::Option(Box::new(Type::Str)),
            ("String.chars", [Type::Str]) => Type::List(Box::new(Type::Str)),
            ("String.slice", [Type::Str, Type::Int, Type::Int]) => Type::Str,
            (
                "String.contains" | "String.startsWith" | "String.endsWith",
                [Type::Str, Type::Str],
            ) => Type::Bool,
            ("String.split", [Type::Str, Type::Str]) => Type::List(Box::new(Type::Str)),
            ("String.replace", [Type::Str, Type::Str, Type::Str])
            | ("String.trim", [Type::Str]) => Type::Str,
            ("String.join", [Type::List(inner), Type::Str]) => {
                merge(inner, &Type::Str)?;
                Type::Str
            }
            ("List.len", [Type::List(_)]) | ("Vector.len", [Type::Vector(_)]) => Type::Int,
            ("List.reverse", [list @ Type::List(_)])
            | ("List.take" | "List.drop", [list @ Type::List(_), Type::Int]) => list.clone(),
            ("List.concat", [left @ Type::List(_), right @ Type::List(_)]) => merge(left, right)?,
            ("List.prepend", [value, Type::List(inner)]) => {
                Type::List(Box::new(merge(value, inner)?))
            }
            ("List.contains", [Type::List(inner), value]) => {
                merge(inner, value)?;
                Type::Bool
            }
            ("List.zip", [Type::List(left), Type::List(right)]) => {
                Type::List(Box::new(Type::Tuple(vec![*left.clone(), *right.clone()])))
            }
            ("Vector.new", [Type::Int, fill]) => {
                let vector = Type::Vector(Box::new(fill.clone()));
                if crate::ast::is_literal_vector_size(&args[0]) {
                    vector
                } else {
                    result(vector)
                }
            }
            ("Vector.fromList", [Type::List(inner)]) => Type::Vector(inner.clone()),
            ("List.fromVector", [Type::Vector(inner)]) => Type::List(inner.clone()),
            ("Vector.get", [Type::Vector(inner), Type::Int]) => Type::Option(inner.clone()),
            ("Vector.set", [Type::Vector(inner), Type::Int, value]) => {
                Type::Option(Box::new(Type::Vector(Box::new(merge(inner, value)?))))
            }
            ("Map.get", [Type::Map(key, value), actual]) => {
                merge(key, actual)?;
                self.map_type(*key.clone(), *value.clone())?;
                Type::Option(value.clone())
            }
            ("Map.set", [Type::Map(key, value), actual_key, actual_value]) => {
                self.map_type(merge(key, actual_key)?, merge(value, actual_value)?)?
            }
            ("Map.has", [Type::Map(key, value), actual]) => {
                merge(key, actual)?;
                self.map_type(*key.clone(), *value.clone())?;
                Type::Bool
            }
            ("Map.remove", [Type::Map(key, value), actual]) => {
                self.map_type(merge(key, actual)?, *value.clone())?
            }
            ("Map.len", [Type::Map(key, value)]) => {
                self.map_type(*key.clone(), *value.clone())?;
                Type::Int
            }
            ("Map.fromList", [Type::List(inner)]) => match inner.as_ref() {
                Type::Tuple(pair) if pair.len() == 2 => {
                    self.map_type(pair[0].clone(), pair[1].clone())?
                }
                empty if *empty == hole() => self.map_type(hole(), hole())?,
                _ => return Err("Map.fromList requires key/value pairs".to_string()),
            },
            ("Result.withDefault", [Type::Result(ok, _), default]) => merge(ok, default)?,
            ("Option.withDefault", [Type::Option(inner), default]) => merge(inner, default)?,
            ("Result.fromOption", [Type::Option(inner), error]) => {
                Type::Result(inner.clone(), Box::new(error.clone()))
            }
            ("Int.toBigEndian" | "Int.toLittleEndian", [Type::Int, Type::Int]) => {
                let bytes = self.bytes_type()?;
                if crate::ast::is_literal_total_int_endian_call(&args[0], &args[1]) {
                    bytes
                } else {
                    result(bytes)
                }
            }
            ("Int.fromBigEndian" | "Int.fromLittleEndian", [bytes]) => {
                let expected = self.bytes_type()?;
                merge(bytes, &expected)?;
                Type::Int
            }
            // Map iteration uses bodyless MapEntries; String formatting,
            // parsing and most text utilities also need actual definitions.
            _ => return Err(format!("unsupported call {name}")),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::dafny::{reasons, tests::ctx_from_source};

    fn emit(source: &str) -> Result<String, String> {
        let ctx = ctx_from_source(source, "DefinedBuiltins");
        let blocks = reasons::local_blocks(&ctx);
        let block = blocks[0];
        let crate::ast::VerifyKind::Law(law) = &block.kind else {
            panic!("expected a law");
        };
        let empty = HashSet::new();
        let recursion = crate::codegen::dafny::toplevel::LawRecursion {
            opaque_fns: &empty,
            native_members: &empty,
            native_callers: &empty,
            termination_opaque: &empty,
        };
        reasons::emit(block, law, &ctx, &recursion)
    }

    #[test]
    fn literal_and_dynamic_counts_keep_their_distinct_result_types() {
        for (expression, result) in [
            ("Bits.shiftLeft(value, 1)", "Int"),
            ("Bits.shiftRight(value, 1)", "Int"),
            ("Bits.low(value, 1)", "Int"),
            ("Bits.shiftLeft(value, count)", "Result<Int, String>"),
            ("Bits.shiftRight(value, count)", "Result<Int, String>"),
            ("Bits.low(value, count)", "Result<Int, String>"),
            ("Vector.new(1, value)", "Vector<Int>"),
            ("Vector.new(count, value)", "Result<Vector<Int>, String>"),
        ] {
            let source = format!(
                "fn valueOf(value: Int, count: Int) -> {result}\n    {expression}\nverify valueOf law reflexive\n    given value: Int = [-1, 0, 3]\n    given count: Int = [-1, 0, 2]\n    because true\n    using []\n    valueOf(value, count) => valueOf(value, count)\n"
            );
            assert!(emit(&source).is_ok(), "{expression}: {:?}", emit(&source));
        }
    }

    #[test]
    fn map_iteration_and_hidden_opaque_helpers_stay_declined() {
        for (body, result, expected) in [
            ("    Map.keys(m)", "List<Int>", "Map.keys"),
            ("    Map.values(m)", "List<Int>", "Map.values"),
            ("    Map.entries(m)", "List<Tuple<Int, Int>>", "Map.entries"),
            (
                "    hidden = String.byteLength(\"unmodelled\")\n    Map.len(m)",
                "Int",
                "String.byteLength",
            ),
        ] {
            let source = format!(
                "fn inspect(m: Map<Int, Int>) -> {result}\n{body}\nverify inspect law reflexive\n    given m: Map<Int, Int> = [{{1 => 2}}]\n    because true\n    using []\n    inspect(m) => inspect(m)\n"
            );
            assert!(emit(&source).unwrap_err().contains(expected), "{expected}");
        }
    }

    #[test]
    fn unsupported_scalar_types_cannot_hide_inside_a_container() {
        let source = "fn inspect(m: Map<Int, Vector<Float>>) -> Int\n    Map.len(m)\nverify inspect law reflexive\n    given m: Map<Int, Vector<Float>> = [{1 => Vector.fromList([0.0])}]\n    because true\n    using []\n    inspect(m) => inspect(m)\n";
        assert!(emit(source).unwrap_err().contains("Float"));
    }
}
