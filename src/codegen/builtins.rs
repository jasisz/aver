/// Shared builtin recognition for all codegen backends.
///
/// Each backend matches on `Builtin` variants to emit target-specific code.
/// Adding a new variant here forces all backends to handle it (exhaustive match).
/// A recognized Aver builtin function or constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Builtin {
    // --- Constructors ---
    ResultOk,
    ResultErr,
    OptionSome,

    // --- Result/Option combinators ---
    ResultWithDefault,
    OptionWithDefault,
    OptionToResult,

    // --- Int ---
    IntAbs,
    IntToFloat,
    IntToString,
    IntFromString,
    IntParse,
    IntMin,
    IntMax,
    IntRem,
    IntMod,

    // --- Float ---
    FloatAbs,
    FloatSqrt,
    FloatPow,
    FloatRound,
    FloatFloor,
    FloatCeil,
    FloatToInt,
    FloatToString,
    FloatFromString,
    FloatParse,

    // --- String ---
    StringLen,
    StringConcat,
    StringCharAt,
    StringChars,
    StringSlice,
    StringContains,
    StringStartsWith,
    StringEndsWith,
    StringTrim,
    StringSplit,
    StringJoin,
    StringReplace,
    StringRepeat,
    StringIndexOf,
    StringToUpper,
    StringToLower,
    StringFromInt,
    StringFromFloat,
    StringFromBool,
    StringByteLength,

    // --- Char ---
    CharToCode,
    CharFromCode,

    // --- Byte ---
    ByteToHex,
    ByteFromHex,

    // --- List ---
    ListLen,
    ListGet,
    ListHead,
    ListTail,
    ListPrepend,
    ListPush,
    ListAppend,
    ListConcat,
    ListReverse,
    ListContains,
    ListFind,
    ListAny,
    ListZip,

    // --- Map ---
    MapEmpty,
    MapGet,
    MapSet,
    MapHas,
    MapRemove,
    MapKeys,
    MapValues,
    MapEntries,
    MapLen,
    MapFromList,
}

/// Try to recognize a dotted name as a known Aver builtin.
///
/// Returns `None` for user-defined functions and effectful services
/// (Console, Disk, Http, Tcp, etc.).
pub(crate) fn recognize_builtin(name: &str) -> Option<Builtin> {
    Some(match name {
        // Constructors
        "Result.Ok" => Builtin::ResultOk,
        "Result.Err" => Builtin::ResultErr,
        "Option.Some" => Builtin::OptionSome,

        // Result/Option combinators
        "Result.withDefault" => Builtin::ResultWithDefault,
        "Option.withDefault" => Builtin::OptionWithDefault,
        "Option.toResult" => Builtin::OptionToResult,

        // Int
        "Int.abs" => Builtin::IntAbs,
        "Int.toFloat" => Builtin::IntToFloat,
        "Int.toString" => Builtin::IntToString,
        "Int.fromString" => Builtin::IntFromString,
        "Int.parse" => Builtin::IntParse,
        "Int.min" => Builtin::IntMin,
        "Int.max" => Builtin::IntMax,
        "Int.rem" => Builtin::IntRem,
        "Int.mod" => Builtin::IntMod,

        // Float
        "Float.abs" => Builtin::FloatAbs,
        "Float.sqrt" => Builtin::FloatSqrt,
        "Float.pow" => Builtin::FloatPow,
        "Float.round" => Builtin::FloatRound,
        "Float.floor" => Builtin::FloatFloor,
        "Float.ceil" => Builtin::FloatCeil,
        "Float.toInt" => Builtin::FloatToInt,
        "Float.toString" => Builtin::FloatToString,
        "Float.fromString" => Builtin::FloatFromString,
        "Float.parse" => Builtin::FloatParse,

        // String
        "String.len" => Builtin::StringLen,
        "String.concat" => Builtin::StringConcat,
        "String.charAt" => Builtin::StringCharAt,
        "String.chars" => Builtin::StringChars,
        "String.slice" => Builtin::StringSlice,
        "String.contains" => Builtin::StringContains,
        "String.startsWith" => Builtin::StringStartsWith,
        "String.endsWith" => Builtin::StringEndsWith,
        "String.trim" => Builtin::StringTrim,
        "String.split" => Builtin::StringSplit,
        "String.join" => Builtin::StringJoin,
        "String.replace" => Builtin::StringReplace,
        "String.repeat" => Builtin::StringRepeat,
        "String.indexOf" => Builtin::StringIndexOf,
        "String.toUpper" => Builtin::StringToUpper,
        "String.toLower" => Builtin::StringToLower,
        "String.fromInt" => Builtin::StringFromInt,
        "String.fromFloat" => Builtin::StringFromFloat,
        "String.fromBool" => Builtin::StringFromBool,
        "String.byteLength" => Builtin::StringByteLength,

        // Char
        "Char.toCode" => Builtin::CharToCode,
        "Char.fromCode" => Builtin::CharFromCode,

        // Byte
        "Byte.toHex" => Builtin::ByteToHex,
        "Byte.fromHex" => Builtin::ByteFromHex,

        // List
        "List.len" => Builtin::ListLen,
        "List.get" => Builtin::ListGet,
        "List.head" => Builtin::ListHead,
        "List.tail" => Builtin::ListTail,
        "List.prepend" => Builtin::ListPrepend,
        "List.push" => Builtin::ListPush,
        "List.append" => Builtin::ListAppend,
        "List.concat" => Builtin::ListConcat,
        "List.reverse" => Builtin::ListReverse,
        "List.contains" => Builtin::ListContains,
        "List.find" => Builtin::ListFind,
        "List.any" => Builtin::ListAny,
        "List.zip" => Builtin::ListZip,

        // Map
        "Map.empty" => Builtin::MapEmpty,
        "Map.get" => Builtin::MapGet,
        "Map.set" => Builtin::MapSet,
        "Map.has" => Builtin::MapHas,
        "Map.remove" => Builtin::MapRemove,
        "Map.keys" => Builtin::MapKeys,
        "Map.values" => Builtin::MapValues,
        "Map.entries" => Builtin::MapEntries,
        "Map.len" => Builtin::MapLen,
        "Map.fromList" => Builtin::MapFromList,

        _ => return None,
    })
}
