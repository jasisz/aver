use crate::nan_value::{Arena, NanValue};
#[cfg(feature = "terminal")]
use crate::services::terminal;
use crate::services::{console, disk, env, http, random, tcp, time};
use crate::types::{bool, byte, char, float, int, list, map, option, result, string};
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
    TcpPing => "Tcp.ping",
    TcpConnect => "Tcp.connect",
    TcpWriteLine => "Tcp.writeLine",
    TcpReadLine => "Tcp.readLine",
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
    IntToString => "Int.toString",
    IntAbs => "Int.abs",
    IntMin => "Int.min",
    IntMax => "Int.max",
    IntMod => "Int.mod",
    IntToFloat => "Int.toFloat",

    FloatFromString => "Float.fromString",
    FloatFromInt => "Float.fromInt",
    FloatToString => "Float.toString",
    FloatAbs => "Float.abs",
    FloatFloor => "Float.floor",
    FloatCeil => "Float.ceil",
    FloatRound => "Float.round",
    FloatMin => "Float.min",
    FloatMax => "Float.max",

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
    ListGet => "List.get",
    ListAppend => "List.append",
    ListPrepend => "List.prepend",
    ListConcat => "List.concat",
    ListReverse => "List.reverse",
    ListContains => "List.contains",
    ListZip => "List.zip",

    MapEmpty => "Map.empty",
    MapSet => "Map.set",
    MapGet => "Map.get",
    MapRemove => "Map.remove",
    MapHas => "Map.has",
    MapKeys => "Map.keys",
    MapValues => "Map.values",
    MapEntries => "Map.entries",
    MapLen => "Map.len",
    MapFromList => "Map.fromList",

    OptionWithDefault => "Option.withDefault",
    OptionToResult => "Option.toResult",
    ResultWithDefault => "Result.withDefault",

    CharToCode => "Char.toCode",
    CharFromCode => "Char.fromCode",
    ByteToHex => "Byte.toHex",
    ByteFromHex => "Byte.fromHex",
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum VmBuiltinParentThinClass {
    Heapless,
    Cheap,
    AllocHeavy,
}

impl VmBuiltin {
    pub(crate) const fn is_http_server(self) -> bool {
        matches!(self, Self::HttpServerListen | Self::HttpServerListenWith)
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
            | Self::IntToFloat
            | Self::FloatFromInt
            | Self::FloatAbs
            | Self::FloatFloor
            | Self::FloatCeil
            | Self::FloatRound
            | Self::FloatMin
            | Self::FloatMax
            | Self::StringLen
            | Self::StringByteLength
            | Self::StringStartsWith
            | Self::StringEndsWith
            | Self::StringContains
            | Self::ListContains
            | Self::MapEmpty
            | Self::MapLen
            | Self::MapHas
            | Self::OptionWithDefault
            | Self::ResultWithDefault
            | Self::CharToCode => VmBuiltinParentThinClass::Heapless,

            Self::ListGet | Self::MapGet => VmBuiltinParentThinClass::Cheap,

            _ => VmBuiltinParentThinClass::AllocHeavy,
        }
    }

    pub(crate) fn effects(self) -> &'static [&'static str] {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn | Self::ConsoleReadLine => {
                console::effects(self.name())
            }

            Self::HttpGet
            | Self::HttpHead
            | Self::HttpDelete
            | Self::HttpPost
            | Self::HttpPut
            | Self::HttpPatch => http::effects(self.name()),

            Self::HttpServerListen | Self::HttpServerListenWith => {
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

            Self::EnvGet | Self::EnvSet => env::effects(self.name()),
            Self::RandomInt | Self::RandomFloat => random::effects(self.name()),

            Self::TcpSend
            | Self::TcpPing
            | Self::TcpConnect
            | Self::TcpWriteLine
            | Self::TcpReadLine
            | Self::TcpClose => tcp::effects(self.name()),

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
            | Self::TerminalFlush => terminal::effects(self.name()),

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

            Self::HttpGet
            | Self::HttpHead
            | Self::HttpDelete
            | Self::HttpPost
            | Self::HttpPut
            | Self::HttpPatch => http::call_nv(self.name(), args, arena),

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
            | Self::TcpPing
            | Self::TcpConnect
            | Self::TcpWriteLine
            | Self::TcpReadLine
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

            Self::TimeNow | Self::TimeUnixMs | Self::TimeSleep => {
                time::call_nv(self.name(), args, arena)
            }

            Self::BoolOr | Self::BoolAnd | Self::BoolNot => bool::call_nv(self.name(), args, arena),

            Self::IntFromString
            | Self::IntFromFloat
            | Self::IntToString
            | Self::IntAbs
            | Self::IntMin
            | Self::IntMax
            | Self::IntMod
            | Self::IntToFloat => int::call_nv(self.name(), args, arena),

            Self::FloatFromString
            | Self::FloatFromInt
            | Self::FloatToString
            | Self::FloatAbs
            | Self::FloatFloor
            | Self::FloatCeil
            | Self::FloatRound
            | Self::FloatMin
            | Self::FloatMax => float::call_nv(self.name(), args, arena),

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
            | Self::ListGet
            | Self::ListAppend
            | Self::ListPrepend
            | Self::ListConcat
            | Self::ListReverse
            | Self::ListContains
            | Self::ListZip => list::call_nv(self.name(), args, arena),

            Self::MapEmpty
            | Self::MapSet
            | Self::MapGet
            | Self::MapRemove
            | Self::MapHas
            | Self::MapKeys
            | Self::MapValues
            | Self::MapEntries
            | Self::MapLen
            | Self::MapFromList => map::call_nv(self.name(), args, arena),

            Self::OptionWithDefault | Self::OptionToResult => {
                option::call_nv(self.name(), args, arena)
            }
            Self::ResultWithDefault => result::call_nv(self.name(), args, arena),
            Self::CharToCode | Self::CharFromCode => char::call_nv(self.name(), args, arena),
            Self::ByteToHex | Self::ByteFromHex => byte::call_nv(self.name(), args, arena),

            Self::HttpServerListen | Self::HttpServerListenWith => None,
        };

        result.expect("VmBuiltin list and call_nv dispatch are out of sync")
    }
}

#[cfg(test)]
mod tests {
    use super::VmBuiltin;

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
