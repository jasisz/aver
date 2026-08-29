use crate::nan_value::{Arena, NanValue};
use crate::types::{
    bits, bool, branch_path, crypto, float, int, list, map, option, result, string,
};
use crate::value::RuntimeError;

macro_rules! vm_builtins {
    ($($variant:ident => $name:literal,)+) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(u16)]
        pub(crate) enum VmBuiltin {
            $($variant,)+
        }

        impl VmBuiltin {
            pub(crate) const fn name(self) -> &'static str {
                match self {
                    $(Self::$variant => $name,)+
                }
            }

            pub(crate) const ALL: &'static [Self] = &[
                $(Self::$variant,)+
            ];
        }
    };
}

vm_builtins! {
    BoolOr => "Bool.or",
    BoolAnd => "Bool.and",
    BoolNot => "Bool.not",

    IntFromString => "Int.fromString",
    IntFromFloat => "Int.fromFloat",
    IntAbs => "Int.abs",
    IntMin => "Int.min",
    IntMax => "Int.max",
    IntMod => "Int.mod",
    IntDiv => "Int.div",
    IntToBigEndian => "Int.toBigEndian",
    IntToLittleEndian => "Int.toLittleEndian",
    IntFromBigEndian => "Int.fromBigEndian",
    IntFromLittleEndian => "Int.fromLittleEndian",

    BitsAnd => "Bits.and",
    BitsOr => "Bits.or",
    BitsXor => "Bits.xor",
    BitsNot => "Bits.not",
    BitsShiftLeft => "Bits.shiftLeft",
    BitsShiftRight => "Bits.shiftRight",
    BitsLow => "Bits.low",

    FloatFromString => "Float.fromString",
    FloatFromInt => "Float.fromInt",
    FloatAbs => "Float.abs",
    FloatFloor => "Float.floor",
    FloatCeil => "Float.ceil",
    FloatRound => "Float.round",
    FloatMin => "Float.min",
    FloatMax => "Float.max",
    FloatSin => "Float.sin",
    FloatCos => "Float.cos",
    FloatSqrt => "Float.sqrt",
    FloatPow => "Float.pow",
    FloatAtan2 => "Float.atan2",
    FloatPi => "Float.pi",

    StringLen => "String.len",
    StringByteLength => "String.byteLength",
    StringStartsWith => "String.startsWith",
    StringEndsWith => "String.endsWith",
    StringContains => "String.contains",
    StringSlice => "String.slice",
    StringTrim => "String.trim",
    StringSplit => "String.split",
    StringReplace => "String.replace",
    StringJoin => "String.join",
    StringCharAt => "String.charAt",
    StringChars => "String.chars",
    StringFromInt => "String.fromInt",
    StringFromFloat => "String.fromFloat",
    StringFromBool => "String.fromBool",
    StringToUtf8 => "String.toUtf8",
    StringFromUtf8 => "String.fromUtf8",
    StringToLower => "String.toLower",
    StringToUpper => "String.toUpper",

    ListLen => "List.len",
    ListPrepend => "List.prepend",
    ListTake => "List.take",
    ListDrop => "List.drop",
    ListConcat => "List.concat",
    ListReverse => "List.reverse",
    ListContains => "List.contains",
    ListZip => "List.zip",

    MapSet => "Map.set",
    MapGet => "Map.get",
    MapRemove => "Map.remove",
    MapHas => "Map.has",
    MapKeys => "Map.keys",
    MapValues => "Map.values",
    MapEntries => "Map.entries",
    MapLen => "Map.len",
    MapFromList => "Map.fromList",

    VectorNew => "Vector.new",
    VectorGet => "Vector.get",
    VectorSet => "Vector.set",
    VectorLen => "Vector.len",
    VectorFromList => "Vector.fromList",
    ListFromVector => "List.fromVector",

    OptionWithDefault => "Option.withDefault",
    ResultFromOption => "Result.fromOption",
    ResultWithDefault => "Result.withDefault",

    StringFirstCodePoint => "String.firstCodePoint",
    StringFromCodePoint => "String.fromCodePoint",
    CryptoSha256 => "Crypto.sha256",
    BranchPathChild => "BranchPath.child",
    BranchPathParse => "BranchPath.parse",
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum VmBuiltinParentThinClass {
    Heapless,
    Cheap,
    AllocHeavy,
}

impl VmBuiltin {
    pub(crate) const fn parent_thin_class(self) -> VmBuiltinParentThinClass {
        match self {
            Self::BoolOr
            | Self::BoolAnd
            | Self::BoolNot
            | Self::IntFromFloat
            | Self::IntAbs
            | Self::IntMin
            | Self::IntMax
            | Self::BitsAnd
            | Self::BitsOr
            | Self::BitsXor
            | Self::BitsNot
            | Self::FloatFromInt
            | Self::FloatAbs
            | Self::FloatFloor
            | Self::FloatCeil
            | Self::FloatRound
            | Self::FloatMin
            | Self::FloatMax
            | Self::FloatSin
            | Self::FloatCos
            | Self::FloatSqrt
            | Self::FloatPow
            | Self::FloatAtan2
            | Self::FloatPi
            | Self::StringLen
            | Self::StringByteLength
            | Self::StringStartsWith
            | Self::StringEndsWith
            | Self::StringContains
            | Self::ListContains
            | Self::MapLen
            | Self::MapHas
            | Self::VectorLen
            | Self::OptionWithDefault
            | Self::ResultWithDefault => VmBuiltinParentThinClass::Heapless,

            Self::MapGet | Self::VectorGet => VmBuiltinParentThinClass::Cheap,

            _ => VmBuiltinParentThinClass::AllocHeavy,
        }
    }

    pub(crate) fn effects(self) -> &'static [&'static str] {
        &[]
    }

    pub(crate) fn invoke_nv(
        self,
        args: &[NanValue],
        arena: &mut Arena,
        _cli_args: &[String],
        _silent_console: bool,
    ) -> Result<NanValue, RuntimeError> {
        let result = match self {
            Self::BoolOr | Self::BoolAnd | Self::BoolNot => bool::call_nv(self.name(), args, arena),

            Self::IntFromString
            | Self::IntFromFloat
            | Self::IntAbs
            | Self::IntMin
            | Self::IntMax
            | Self::IntMod
            | Self::IntDiv
            | Self::IntToBigEndian
            | Self::IntToLittleEndian
            | Self::IntFromBigEndian
            | Self::IntFromLittleEndian => int::call_nv(self.name(), args, arena),

            Self::FloatFromString
            | Self::FloatFromInt
            | Self::FloatAbs
            | Self::FloatFloor
            | Self::FloatCeil
            | Self::FloatRound
            | Self::FloatMin
            | Self::FloatMax
            | Self::FloatSin
            | Self::FloatCos
            | Self::FloatSqrt
            | Self::FloatPow
            | Self::FloatAtan2
            | Self::FloatPi => float::call_nv(self.name(), args, arena),

            Self::StringLen
            | Self::StringByteLength
            | Self::StringStartsWith
            | Self::StringEndsWith
            | Self::StringContains
            | Self::StringSlice
            | Self::StringTrim
            | Self::StringSplit
            | Self::StringReplace
            | Self::StringJoin
            | Self::StringCharAt
            | Self::StringChars
            | Self::StringFromInt
            | Self::StringFromFloat
            | Self::StringFromBool
            | Self::StringToUtf8
            | Self::StringFromUtf8
            | Self::StringToLower
            | Self::StringToUpper => string::call_nv(self.name(), args, arena),

            Self::StringFirstCodePoint | Self::StringFromCodePoint => {
                crate::types::code_point::call_nv(self.name(), args, arena)
            }

            Self::ListLen
            | Self::ListPrepend
            | Self::ListTake
            | Self::ListDrop
            | Self::ListConcat
            | Self::ListReverse
            | Self::ListContains
            | Self::ListZip => list::call_nv(self.name(), args, arena),

            Self::MapSet
            | Self::MapGet
            | Self::MapRemove
            | Self::MapHas
            | Self::MapKeys
            | Self::MapValues
            | Self::MapEntries
            | Self::MapLen
            | Self::MapFromList => map::call_nv(self.name(), args, arena),

            Self::VectorNew
            | Self::VectorGet
            | Self::VectorSet
            | Self::VectorLen
            | Self::VectorFromList
            | Self::ListFromVector => crate::types::vector::call_nv(self.name(), args, arena),

            Self::OptionWithDefault => option::call_nv(self.name(), args, arena),
            Self::ResultWithDefault | Self::ResultFromOption => {
                result::call_nv(self.name(), args, arena)
            }
            Self::CryptoSha256 => crypto::call_nv(self.name(), args, arena),
            Self::BitsAnd
            | Self::BitsOr
            | Self::BitsXor
            | Self::BitsNot
            | Self::BitsShiftLeft
            | Self::BitsShiftRight
            | Self::BitsLow => bits::call_nv(self.name(), args, arena),
            Self::BranchPathChild | Self::BranchPathParse => {
                branch_path::call_nv(self.name(), args, arena)
            }
        };

        result.expect("VmBuiltin list and call_nv dispatch are out of sync")
    }
}

#[cfg(test)]
mod tests {
    use super::VmBuiltin;

    #[test]
    fn every_classified_effect_is_visible_to_the_vm() {
        // `effects()` is what makes the VM record a call in the verify trace.
        // `Args.get` was missing its arm and fell through the `_ => &[]`
        // wildcard, so it read the real process arguments and then left no
        // trace entry — a `trace.length()` law counted one event where two had
        // fired, and the law stating the truth failed instead. `Env.get`, the
        // same dimension, had its arm and behaved correctly, which is what
        // made the omission invisible rather than obviously broken.
        //
        // Every effect the proof pipeline classifies has to be observable to
        // the VM, or the trace those proofs are checked against is missing
        // events.
        use crate::types::checker::effect_classification::classifications_for_proof_subset;

        let standard_capabilities = crate::stdlib::standard_capability_registry();
        for classification in classifications_for_proof_subset() {
            let method = classification.method;
            if let Some(builtin) = VmBuiltin::ALL.iter().find(|b| b.name() == method) {
                assert!(
                    builtin.effects().contains(&method),
                    "{method} is classified for proof but `VmBuiltin::effects()` does not report \
                     it, so calls to it never reach the verify trace"
                );
                continue;
            }
            let operation = standard_capabilities.operation(method).unwrap_or_else(|| {
                panic!("{method} is classified but has neither a VM builtin nor a standard capability operation")
            });
            assert!(
                operation.is_effectful(),
                "{method} is classified for proof but its standard capability contract marks it pure"
            );
        }
    }

    /// Source text of `effects()`, from its signature to the closing brace of
    /// its body. Panics if the signature moved, so the guard below can never
    /// pass by silently matching nothing.
    fn effects_fn_source() -> &'static str {
        const SOURCE: &str = include_str!("builtin.rs");
        const SIGNATURE: &str = "pub(crate) fn effects(self) -> &'static [&'static str] {";

        let start = SOURCE
            .find(SIGNATURE)
            .expect("`effects()` signature not found — update this guard to match the new one");
        let body = &SOURCE[start + SIGNATURE.len()..];
        let mut depth = 1usize;
        for (idx, ch) in body.char_indices() {
            match ch {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return &SOURCE[start..start + SIGNATURE.len() + idx + 1];
                    }
                }
                _ => {}
            }
        }
        panic!("`effects()` body has unbalanced braces");
    }

    #[test]
    fn effects_metadata_is_the_same_in_every_build() {
        // `effects()` answers "what does this builtin do", not "is the
        // implementation compiled into this binary". Those two questions have
        // different answers: `Http.get` is registered with the typechecker in
        // every build, but its networking implementation only ships behind a
        // feature. When the effect list was gated on that feature, a build
        // without it reported `Http.get` as pure — the runtime effect check
        // passed unconditionally, no verify-trace event was recorded, and
        // Record/Replay was bypassed, so a `trace` law over `Http.get`
        // counted zero events and passed while proving nothing.
        //
        // Build-dependent behaviour belongs in `invoke_nv`, which is where the
        // implementation actually lives. This is a source-shape check on
        // purpose: it holds for every feature combination at once, including
        // the ones no CI lane builds test code for.
        let source = effects_fn_source();
        assert!(
            !source.contains("#[cfg"),
            "`VmBuiltin::effects()` contains a `#[cfg` attribute. Effect metadata must be \
             identical in every build — gate the implementation in `invoke_nv` instead. \
             Offending body:\n{source}"
        );
    }

    #[test]
    fn builtin_names_are_unique() {
        let mut seen = std::collections::HashSet::new();
        for builtin in [
            VmBuiltin::StringReplace,
            VmBuiltin::ResultFromOption,
            VmBuiltin::MapFromList,
        ] {
            assert!(
                seen.insert(builtin.name()),
                "duplicate builtin name {}",
                builtin.name()
            );
        }
    }

    #[test]
    fn standard_capability_operations_never_reenter_the_builtin_table() {
        let builtin_names = VmBuiltin::ALL
            .iter()
            .map(|builtin| builtin.name())
            .collect::<std::collections::HashSet<_>>();
        for operation in crate::stdlib::standard_capability_registry_ref().operations() {
            assert!(
                !builtin_names.contains(operation.canonical_name.as_str()),
                "standard capability operation {} leaked back into VmBuiltin",
                operation.canonical_name
            );
        }
    }
}
