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
    IntFromFloat,
    IntFromString,
    IntMin,
    IntMax,
    IntMod,
    IntDiv,

    // --- Bits (a bit-level view of Int, not a type) ---
    BitsAnd,
    BitsOr,
    BitsXor,
    BitsNot,
    BitsShiftLeft,
    BitsShiftRight,
    BitsLow,

    // --- Float ---
    FloatAbs,
    FloatSqrt,
    FloatPow,
    FloatRound,
    FloatFloor,
    FloatCeil,
    FloatFromInt,
    FloatFromString,
    FloatPi,
    FloatMin,
    FloatMax,
    FloatSin,
    FloatCos,
    FloatAtan2,

    // --- String ---
    StringLen,
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

    // --- Bool ---
    BoolOr,
    BoolAnd,
    BoolNot,

    // --- Char ---
    CharToCode,
    CharFromCode,

    // --- Crypto ---
    CryptoSha256,

    // --- List ---
    ListLen,
    ListHead,
    ListTail,
    ListPrepend,
    ListTake,
    ListDrop,
    ListConcat,
    ListReverse,
    ListContains,
    ListFind,
    ListAny,
    ListZip,

    // --- Vector ---
    VectorNew,
    VectorGet,
    VectorSet,
    VectorLen,
    VectorFromList,
    ListFromVector,

    // --- Map ---
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
        "Int.fromFloat" => Builtin::IntFromFloat,
        "Int.fromString" => Builtin::IntFromString,
        "Int.min" => Builtin::IntMin,
        "Int.max" => Builtin::IntMax,
        "Int.mod" => Builtin::IntMod,
        "Int.div" => Builtin::IntDiv,

        // Bits
        "Bits.and" => Builtin::BitsAnd,
        "Bits.or" => Builtin::BitsOr,
        "Bits.xor" => Builtin::BitsXor,
        "Bits.not" => Builtin::BitsNot,
        "Bits.shiftLeft" => Builtin::BitsShiftLeft,
        "Bits.shiftRight" => Builtin::BitsShiftRight,
        "Bits.low" => Builtin::BitsLow,

        // Float
        "Float.abs" => Builtin::FloatAbs,
        "Float.sqrt" => Builtin::FloatSqrt,
        "Float.pow" => Builtin::FloatPow,
        "Float.round" => Builtin::FloatRound,
        "Float.floor" => Builtin::FloatFloor,
        "Float.ceil" => Builtin::FloatCeil,
        "Float.fromInt" => Builtin::FloatFromInt,
        "Float.fromString" => Builtin::FloatFromString,
        "Float.pi" => Builtin::FloatPi,
        "Float.min" => Builtin::FloatMin,
        "Float.max" => Builtin::FloatMax,
        "Float.sin" => Builtin::FloatSin,
        "Float.cos" => Builtin::FloatCos,
        "Float.atan2" => Builtin::FloatAtan2,

        // String
        "String.len" => Builtin::StringLen,
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

        // Bool
        "Bool.or" => Builtin::BoolOr,
        "Bool.and" => Builtin::BoolAnd,
        "Bool.not" => Builtin::BoolNot,

        // Char
        "Char.toCode" => Builtin::CharToCode,
        "Char.fromCode" => Builtin::CharFromCode,

        // Crypto
        "Crypto.sha256" => Builtin::CryptoSha256,

        // List
        "List.len" => Builtin::ListLen,
        "List.head" => Builtin::ListHead,
        "List.tail" => Builtin::ListTail,
        "List.prepend" => Builtin::ListPrepend,
        "List.take" => Builtin::ListTake,
        "List.drop" => Builtin::ListDrop,
        "List.concat" => Builtin::ListConcat,
        "List.reverse" => Builtin::ListReverse,
        "List.contains" => Builtin::ListContains,
        "List.find" => Builtin::ListFind,
        "List.any" => Builtin::ListAny,
        "List.zip" => Builtin::ListZip,

        // Vector
        "Vector.new" => Builtin::VectorNew,
        "Vector.get" => Builtin::VectorGet,
        "Vector.set" => Builtin::VectorSet,
        "Vector.len" => Builtin::VectorLen,
        "Vector.fromList" => Builtin::VectorFromList,
        "List.fromVector" => Builtin::ListFromVector,

        // Map
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
