/// Shared builtin recognition for all codegen backends.
///
/// Each backend matches on `Builtin` variants to emit target-specific code.
/// Adding a new variant here forces all backends to handle it (exhaustive match).
///
/// The variant list and the dotted names it is recognised under are one
/// list, expanded into the enum, [`Builtin::name`], [`Builtin::ALL`] and
/// [`recognize_builtin`] together. A backend can only ever render a
/// `Builtin`, and a `Builtin` only ever comes out of `recognize_builtin`,
/// so this is the exact set of names a proof backend has to have a
/// declaration for — which is what
/// `dafny::tests::every_builtin_the_emitter_can_render_resolves_in_the_dafny_prelude`
/// walks. Keeping the two halves in one macro is what stops that guard
/// from being handed a shorter list than the emitter can actually produce.
macro_rules! codegen_builtins {
    ($($variant:ident => $name:literal,)+) => {
        /// A recognized Aver builtin function or constructor.
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub(crate) enum Builtin {
            $($variant,)+
        }

        #[cfg(test)]
        impl Builtin {
            /// The dotted Aver name this builtin is recognised under.
            pub(crate) const fn name(self) -> &'static str {
                match self {
                    $(Self::$variant => $name,)+
                }
            }

            /// Every builtin any backend can be asked to render.
            pub(crate) const ALL: &'static [Self] = &[
                $(Self::$variant,)+
            ];
        }

        /// Try to recognize a dotted name as a known Aver builtin.
        ///
        /// Returns `None` for user-defined functions and effectful services
        /// (Console, Disk, Http, Tcp, etc.).
        pub(crate) fn recognize_builtin(name: &str) -> Option<Builtin> {
            Some(match name {
                $($name => Builtin::$variant,)+
                _ => return None,
            })
        }
    };
}

codegen_builtins! {
    // --- Constructors ---
    ResultOk => "Result.Ok",
    ResultErr => "Result.Err",
    OptionSome => "Option.Some",

    // --- Result/Option combinators ---
    ResultWithDefault => "Result.withDefault",
    OptionWithDefault => "Option.withDefault",
    OptionToResult => "Option.toResult",

    // --- Int ---
    IntAbs => "Int.abs",
    IntFromFloat => "Int.fromFloat",
    IntFromString => "Int.fromString",
    IntMin => "Int.min",
    IntMax => "Int.max",
    IntMod => "Int.mod",
    IntDiv => "Int.div",

    // --- Bits (a bit-level view of Int, not a type) ---
    BitsAnd => "Bits.and",
    BitsOr => "Bits.or",
    BitsXor => "Bits.xor",
    BitsNot => "Bits.not",
    BitsShiftLeft => "Bits.shiftLeft",
    BitsShiftRight => "Bits.shiftRight",
    BitsLow => "Bits.low",

    // --- Float ---
    FloatAbs => "Float.abs",
    FloatSqrt => "Float.sqrt",
    FloatPow => "Float.pow",
    FloatRound => "Float.round",
    FloatFloor => "Float.floor",
    FloatCeil => "Float.ceil",
    FloatFromInt => "Float.fromInt",
    FloatFromString => "Float.fromString",
    FloatPi => "Float.pi",
    FloatMin => "Float.min",
    FloatMax => "Float.max",
    FloatSin => "Float.sin",
    FloatCos => "Float.cos",
    FloatAtan2 => "Float.atan2",

    // --- String ---
    StringLen => "String.len",
    StringCharAt => "String.charAt",
    StringChars => "String.chars",
    StringSlice => "String.slice",
    StringContains => "String.contains",
    StringStartsWith => "String.startsWith",
    StringEndsWith => "String.endsWith",
    StringTrim => "String.trim",
    StringSplit => "String.split",
    StringJoin => "String.join",
    StringReplace => "String.replace",
    StringToUpper => "String.toUpper",
    StringToLower => "String.toLower",
    StringFromInt => "String.fromInt",
    StringFromFloat => "String.fromFloat",
    StringFromBool => "String.fromBool",
    StringByteLength => "String.byteLength",

    // --- Bool ---
    BoolOr => "Bool.or",
    BoolAnd => "Bool.and",
    BoolNot => "Bool.not",

    // --- Char ---
    CharToCode => "Char.toCode",
    CharFromCode => "Char.fromCode",

    // --- Crypto ---
    CryptoSha256 => "Crypto.sha256",

    // --- List ---
    ListLen => "List.len",
    ListHead => "List.head",
    ListTail => "List.tail",
    ListPrepend => "List.prepend",
    ListTake => "List.take",
    ListDrop => "List.drop",
    ListConcat => "List.concat",
    ListReverse => "List.reverse",
    ListContains => "List.contains",
    ListFind => "List.find",
    ListAny => "List.any",
    ListZip => "List.zip",

    // --- Vector ---
    VectorNew => "Vector.new",
    VectorGet => "Vector.get",
    VectorSet => "Vector.set",
    VectorLen => "Vector.len",
    VectorFromList => "Vector.fromList",
    ListFromVector => "List.fromVector",

    // --- Map ---
    MapGet => "Map.get",
    MapSet => "Map.set",
    MapHas => "Map.has",
    MapRemove => "Map.remove",
    MapKeys => "Map.keys",
    MapValues => "Map.values",
    MapEntries => "Map.entries",
    MapLen => "Map.len",
    MapFromList => "Map.fromList",
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_builtin_round_trips_through_its_name() {
        for builtin in Builtin::ALL {
            assert_eq!(
                recognize_builtin(builtin.name()),
                Some(*builtin),
                "`{}` must recognise back to the variant it names",
                builtin.name()
            );
        }
    }
}
