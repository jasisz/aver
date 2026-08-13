#[cfg(not(feature = "terminal"))]
use crate::nan_value::NanValueConvert;
use crate::nan_value::{Arena, NanValue};
#[cfg(feature = "runtime-net")]
use crate::services::http;
#[cfg(feature = "terminal")]
use crate::services::terminal;
use crate::services::{args, console, disk, env, random, tcp, time};
use crate::types::{
    bits, bool, branch_path, char, crypto, float, int, list, map, option, result, string,
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
    ArgsGet => "Args.get",

    ConsolePrint => "Console.print",
    ConsoleError => "Console.error",
    ConsoleWarn => "Console.warn",
    ConsoleReadLine => "Console.readLine",

    HttpGet => "Http.get",
    HttpHead => "Http.head",
    HttpDelete => "Http.delete",
    HttpPost => "Http.post",
    HttpPut => "Http.put",
    HttpPatch => "Http.patch",

    HttpServerListen => "HttpServer.listen",
    HttpServerListenWith => "HttpServer.listenWith",
    SelfHostRuntimeHttpServerListen => "SelfHostRuntime.httpServerListen",
    SelfHostRuntimeHttpServerListenWith => "SelfHostRuntime.httpServerListenWith",

    DiskReadText => "Disk.readText",
    DiskWriteText => "Disk.writeText",
    DiskAppendText => "Disk.appendText",
    DiskExists => "Disk.exists",
    DiskDelete => "Disk.delete",
    DiskDeleteDir => "Disk.deleteDir",
    DiskListDir => "Disk.listDir",
    DiskMakeDir => "Disk.makeDir",

    EnvGet => "Env.get",
    EnvSet => "Env.set",

    RandomInt => "Random.int",
    RandomFloat => "Random.float",

    TcpSend => "Tcp.send",
    TcpSendBytes => "Tcp.sendBytes",
    TcpPing => "Tcp.ping",
    TcpConnect => "Tcp.connect",
    TcpWriteLine => "Tcp.writeLine",
    TcpWriteBytes => "Tcp.writeBytes",
    TcpReadLine => "Tcp.readLine",
    TcpReadBytes => "Tcp.readBytes",
    TcpClose => "Tcp.close",

    TerminalEnableRawMode => "Terminal.enableRawMode",
    TerminalDisableRawMode => "Terminal.disableRawMode",
    TerminalClear => "Terminal.clear",
    TerminalMoveTo => "Terminal.moveTo",
    TerminalPrint => "Terminal.print",
    TerminalSetColor => "Terminal.setColor",
    TerminalResetColor => "Terminal.resetColor",
    TerminalReadKey => "Terminal.readKey",
    TerminalSize => "Terminal.size",
    TerminalHideCursor => "Terminal.hideCursor",
    TerminalShowCursor => "Terminal.showCursor",
    TerminalFlush => "Terminal.flush",

    TimeNow => "Time.now",
    TimeUnixMs => "Time.unixMs",
    TimeSleep => "Time.sleep",

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
    OptionToResult => "Option.toResult",
    ResultWithDefault => "Result.withDefault",

    CharToCode => "Char.toCode",
    CharFromCode => "Char.fromCode",
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
    pub(crate) const fn is_http_server(self) -> bool {
        matches!(
            self,
            Self::HttpServerListen
                | Self::HttpServerListenWith
                | Self::SelfHostRuntimeHttpServerListen
                | Self::SelfHostRuntimeHttpServerListenWith
        )
    }

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
            | Self::ResultWithDefault
            | Self::CharToCode => VmBuiltinParentThinClass::Heapless,

            Self::MapGet | Self::VectorGet => VmBuiltinParentThinClass::Cheap,

            _ => VmBuiltinParentThinClass::AllocHeavy,
        }
    }

    pub(crate) fn effects(self) -> &'static [&'static str] {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn | Self::ConsoleReadLine => {
                console::effects(self.name())
            }

            // Same reasoning as the Terminal arms below: the effect list is
            // structural metadata, identical whether or not the networking
            // implementation ships in this build. `Http.get` is registered
            // with the typechecker unconditionally, so gating it here made
            // the VM treat a network call as pure — no effect check, no
            // verify-trace entry, and Record/Replay silently bypassed.
            Self::HttpGet => &["Http.get"],
            Self::HttpHead => &["Http.head"],
            Self::HttpDelete => &["Http.delete"],
            Self::HttpPost => &["Http.post"],
            Self::HttpPut => &["Http.put"],
            Self::HttpPatch => &["Http.patch"],

            Self::HttpServerListen
            | Self::HttpServerListenWith
            | Self::SelfHostRuntimeHttpServerListen
            | Self::SelfHostRuntimeHttpServerListenWith => {
                crate::services::http_server::effects(self.name())
            }

            Self::DiskReadText
            | Self::DiskWriteText
            | Self::DiskAppendText
            | Self::DiskExists
            | Self::DiskDelete
            | Self::DiskDeleteDir
            | Self::DiskListDir
            | Self::DiskMakeDir => disk::effects(self.name()),

            Self::ArgsGet => args::effects(self.name()),

            Self::EnvGet | Self::EnvSet => env::effects(self.name()),
            Self::RandomInt | Self::RandomFloat => random::effects(self.name()),

            Self::TcpSend
            | Self::TcpSendBytes
            | Self::TcpPing
            | Self::TcpConnect
            | Self::TcpWriteLine
            | Self::TcpWriteBytes
            | Self::TcpReadLine
            | Self::TcpReadBytes
            | Self::TcpClose => tcp::effects(self.name()),

            // Effects list is structural metadata — same regardless of
            // whether the runtime impl ships (crossterm doesn't build
            // on wasm32). If we gated this, Replay mode in the
            // playground would mis-classify Terminal.* as non-effectful
            // and route them through invoke_nv → "not available".
            Self::TerminalEnableRawMode => &["Terminal.enableRawMode"],
            Self::TerminalDisableRawMode => &["Terminal.disableRawMode"],
            Self::TerminalClear => &["Terminal.clear"],
            Self::TerminalMoveTo => &["Terminal.moveTo"],
            Self::TerminalPrint => &["Terminal.print"],
            Self::TerminalSetColor => &["Terminal.setColor"],
            Self::TerminalResetColor => &["Terminal.resetColor"],
            Self::TerminalReadKey => &["Terminal.readKey"],
            Self::TerminalSize => &["Terminal.size"],
            Self::TerminalHideCursor => &["Terminal.hideCursor"],
            Self::TerminalShowCursor => &["Terminal.showCursor"],
            Self::TerminalFlush => &["Terminal.flush"],

            Self::TimeNow | Self::TimeUnixMs | Self::TimeSleep => time::effects(self.name()),

            _ => &[],
        }
    }

    pub(crate) fn invoke_nv(
        self,
        args: &[NanValue],
        arena: &mut Arena,
        cli_args: &[String],
        silent_console: bool,
    ) -> Result<NanValue, RuntimeError> {
        if silent_console
            && matches!(
                self,
                Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn
            )
        {
            return Ok(NanValue::UNIT);
        }

        let result = match self {
            Self::ArgsGet => crate::services::args::call_nv(self.name(), args, cli_args, arena),

            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn | Self::ConsoleReadLine => {
                console::call_nv(self.name(), args, arena)
            }

            #[cfg(feature = "runtime-net")]
            Self::HttpGet
            | Self::HttpHead
            | Self::HttpDelete
            | Self::HttpPost
            | Self::HttpPut
            | Self::HttpPatch => http::call_nv(self.name(), args, arena),
            #[cfg(not(feature = "runtime-net"))]
            Self::HttpGet
            | Self::HttpHead
            | Self::HttpDelete
            | Self::HttpPost
            | Self::HttpPut
            | Self::HttpPatch => Some(Err(RuntimeError::Error(format!(
                "{}: HTTP effects not available in this build",
                self.name()
            )))),

            Self::DiskReadText
            | Self::DiskWriteText
            | Self::DiskAppendText
            | Self::DiskExists
            | Self::DiskDelete
            | Self::DiskDeleteDir
            | Self::DiskListDir
            | Self::DiskMakeDir => disk::call_nv(self.name(), args, arena),

            Self::EnvGet | Self::EnvSet => env::call_nv(self.name(), args, arena),
            Self::RandomInt | Self::RandomFloat => random::call_nv(self.name(), args, arena),

            Self::TcpSend
            | Self::TcpSendBytes
            | Self::TcpPing
            | Self::TcpConnect
            | Self::TcpWriteLine
            | Self::TcpWriteBytes
            | Self::TcpReadLine
            | Self::TcpReadBytes
            | Self::TcpClose => tcp::call_nv(self.name(), args, arena),

            #[cfg(feature = "terminal")]
            Self::TerminalEnableRawMode
            | Self::TerminalDisableRawMode
            | Self::TerminalClear
            | Self::TerminalMoveTo
            | Self::TerminalPrint
            | Self::TerminalSetColor
            | Self::TerminalResetColor
            | Self::TerminalReadKey
            | Self::TerminalSize
            | Self::TerminalHideCursor
            | Self::TerminalShowCursor
            | Self::TerminalFlush => terminal::call_nv(self.name(), args, arena),
            // Stub Terminal runtime when `terminal` feature is off
            // (playground wasm32 build — crossterm doesn't compile).
            // Record mode still gets a value to log; Replay never
            // reaches this branch because effects() keeps Terminal.*
            // classified as effectful → replay_builtin short-circuits.
            #[cfg(not(feature = "terminal"))]
            Self::TerminalEnableRawMode
            | Self::TerminalDisableRawMode
            | Self::TerminalClear
            | Self::TerminalMoveTo
            | Self::TerminalPrint
            | Self::TerminalSetColor
            | Self::TerminalResetColor
            | Self::TerminalHideCursor
            | Self::TerminalShowCursor
            | Self::TerminalFlush => Some(Ok(NanValue::UNIT)),
            #[cfg(not(feature = "terminal"))]
            Self::TerminalReadKey => Some(Ok(NanValue::NONE)),
            #[cfg(not(feature = "terminal"))]
            Self::TerminalSize => {
                use crate::value::Value;
                use std::sync::Arc;
                let rec = Value::Record {
                    type_name: "Terminal.Size".to_string(),
                    fields: Arc::from(vec![
                        ("width".to_string(), Value::int(80)),
                        ("height".to_string(), Value::int(24)),
                    ]),
                };
                Some(Ok(NanValue::from_value(&rec, arena)))
            }

            Self::TimeNow | Self::TimeUnixMs | Self::TimeSleep => {
                time::call_nv(self.name(), args, arena)
            }

            Self::BoolOr | Self::BoolAnd | Self::BoolNot => bool::call_nv(self.name(), args, arena),

            Self::IntFromString
            | Self::IntFromFloat
            | Self::IntAbs
            | Self::IntMin
            | Self::IntMax
            | Self::IntMod
            | Self::IntDiv => int::call_nv(self.name(), args, arena),

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
            | Self::StringToLower
            | Self::StringToUpper => string::call_nv(self.name(), args, arena),

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

            Self::OptionWithDefault | Self::OptionToResult => {
                option::call_nv(self.name(), args, arena)
            }
            Self::ResultWithDefault => result::call_nv(self.name(), args, arena),
            Self::CharToCode | Self::CharFromCode => char::call_nv(self.name(), args, arena),
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

            Self::HttpServerListen
            | Self::HttpServerListenWith
            | Self::SelfHostRuntimeHttpServerListen
            | Self::SelfHostRuntimeHttpServerListenWith => None,
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

        for classification in classifications_for_proof_subset() {
            let method = classification.method;
            let builtin = VmBuiltin::ALL
                .iter()
                .find(|b| b.name() == method)
                .unwrap_or_else(|| panic!("{method} is classified but the VM has no builtin"));
            assert!(
                builtin.effects().contains(&method),
                "{method} is classified for proof but `VmBuiltin::effects()` does not report \
                 it, so calls to it never reach the verify trace"
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
            VmBuiltin::ConsolePrint,
            VmBuiltin::HttpServerListenWith,
            VmBuiltin::StringReplace,
            VmBuiltin::OptionToResult,
            VmBuiltin::MapFromList,
        ] {
            assert!(
                seen.insert(builtin.name()),
                "duplicate builtin name {}",
                builtin.name()
            );
        }
    }
}
