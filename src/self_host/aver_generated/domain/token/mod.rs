#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    TkInt(aver_rt::AverInt),
    TkFloat(f64),
    TkStr(AverStr),
    TkIdent(AverStr),
    TkTrue,
    TkFalse,
    TkPlus,
    TkMinus,
    TkStar,
    TkSlash,
    TkEq,
    TkEqEq,
    TkNeq,
    TkLt,
    TkGt,
    TkLte,
    TkGte,
    TkLParen,
    TkRParen,
    TkComma,
    TkArrow,
    TkLBracket,
    TkRBracket,
    TkDot,
    TkDotDot,
    TkQuestion,
    TkNewline,
    TkInterpStart,
    TkInterpEnd,
    TkColon,
    TkBang,
    TkFatArrow,
    TkIndent,
    TkDedent,
    TkLBrace,
    TkRBrace,
    TkFn,
    TkMatch,
    TkEof,
}

impl aver_rt::AverDisplay for Token {
    fn aver_display(&self) -> String {
        match self {
            Token::TkInt(f0) => format!("TkInt({})", f0.aver_display_inner()),
            Token::TkFloat(f0) => format!("TkFloat({})", f0.aver_display_inner()),
            Token::TkStr(f0) => format!("TkStr({})", f0.aver_display_inner()),
            Token::TkIdent(f0) => format!("TkIdent({})", f0.aver_display_inner()),
            Token::TkTrue => "TkTrue".to_string(),
            Token::TkFalse => "TkFalse".to_string(),
            Token::TkPlus => "TkPlus".to_string(),
            Token::TkMinus => "TkMinus".to_string(),
            Token::TkStar => "TkStar".to_string(),
            Token::TkSlash => "TkSlash".to_string(),
            Token::TkEq => "TkEq".to_string(),
            Token::TkEqEq => "TkEqEq".to_string(),
            Token::TkNeq => "TkNeq".to_string(),
            Token::TkLt => "TkLt".to_string(),
            Token::TkGt => "TkGt".to_string(),
            Token::TkLte => "TkLte".to_string(),
            Token::TkGte => "TkGte".to_string(),
            Token::TkLParen => "TkLParen".to_string(),
            Token::TkRParen => "TkRParen".to_string(),
            Token::TkComma => "TkComma".to_string(),
            Token::TkArrow => "TkArrow".to_string(),
            Token::TkLBracket => "TkLBracket".to_string(),
            Token::TkRBracket => "TkRBracket".to_string(),
            Token::TkDot => "TkDot".to_string(),
            Token::TkDotDot => "TkDotDot".to_string(),
            Token::TkQuestion => "TkQuestion".to_string(),
            Token::TkNewline => "TkNewline".to_string(),
            Token::TkInterpStart => "TkInterpStart".to_string(),
            Token::TkInterpEnd => "TkInterpEnd".to_string(),
            Token::TkColon => "TkColon".to_string(),
            Token::TkBang => "TkBang".to_string(),
            Token::TkFatArrow => "TkFatArrow".to_string(),
            Token::TkIndent => "TkIndent".to_string(),
            Token::TkDedent => "TkDedent".to_string(),
            Token::TkLBrace => "TkLBrace".to_string(),
            Token::TkRBrace => "TkRBrace".to_string(),
            Token::TkFn => "TkFn".to_string(),
            Token::TkMatch => "TkMatch".to_string(),
            Token::TkEof => "TkEof".to_string(),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Token {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Token".to_string()),
        );
        match self {
            Token::TkInt(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkFloat(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkStr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkIdent(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkIdent".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkTrue => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkTrue".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkFalse => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkFalse".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkPlus => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkPlus".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkMinus => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkMinus".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkStar => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkStar".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkSlash => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkSlash".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkEq => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkEq".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkEqEq => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkEqEq".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkNeq => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkNeq".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkLt => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkLt".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkGt => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkGt".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkLte => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkLte".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkGte => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkGte".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkLParen => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkLParen".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkRParen => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkRParen".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkComma => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkComma".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkArrow => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkArrow".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkLBracket => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkLBracket".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkRBracket => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkRBracket".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkDot => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkDot".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkDotDot => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkDotDot".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkQuestion => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkQuestion".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkNewline => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkNewline".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkInterpStart => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkInterpStart".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkInterpEnd => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkInterpEnd".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkColon => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkColon".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkBang => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkBang".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkFatArrow => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkFatArrow".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkIndent => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkIndent".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkDedent => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkDedent".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkLBrace => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkLBrace".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkRBrace => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkRBrace".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkFn => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkFn".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkMatch => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkMatch".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Token::TkEof => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("TkEof".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Token" {
            return Err(format!(
                "$variant type mismatch: expected Token, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "TkInt" => Ok(Token::TkInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant TkInt missing field #{}", 0))?,
                )?,
            )),
            "TkFloat" => Ok(Token::TkFloat(<f64 as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant TkFloat missing field #{}", 0))?,
            )?)),
            "TkStr" => Ok(Token::TkStr(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant TkStr missing field #{}", 0))?,
            )?)),
            "TkIdent" => Ok(Token::TkIdent(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant TkIdent missing field #{}", 0))?,
            )?)),
            "TkTrue" => Ok(Token::TkTrue),
            "TkFalse" => Ok(Token::TkFalse),
            "TkPlus" => Ok(Token::TkPlus),
            "TkMinus" => Ok(Token::TkMinus),
            "TkStar" => Ok(Token::TkStar),
            "TkSlash" => Ok(Token::TkSlash),
            "TkEq" => Ok(Token::TkEq),
            "TkEqEq" => Ok(Token::TkEqEq),
            "TkNeq" => Ok(Token::TkNeq),
            "TkLt" => Ok(Token::TkLt),
            "TkGt" => Ok(Token::TkGt),
            "TkLte" => Ok(Token::TkLte),
            "TkGte" => Ok(Token::TkGte),
            "TkLParen" => Ok(Token::TkLParen),
            "TkRParen" => Ok(Token::TkRParen),
            "TkComma" => Ok(Token::TkComma),
            "TkArrow" => Ok(Token::TkArrow),
            "TkLBracket" => Ok(Token::TkLBracket),
            "TkRBracket" => Ok(Token::TkRBracket),
            "TkDot" => Ok(Token::TkDot),
            "TkDotDot" => Ok(Token::TkDotDot),
            "TkQuestion" => Ok(Token::TkQuestion),
            "TkNewline" => Ok(Token::TkNewline),
            "TkInterpStart" => Ok(Token::TkInterpStart),
            "TkInterpEnd" => Ok(Token::TkInterpEnd),
            "TkColon" => Ok(Token::TkColon),
            "TkBang" => Ok(Token::TkBang),
            "TkFatArrow" => Ok(Token::TkFatArrow),
            "TkIndent" => Ok(Token::TkIndent),
            "TkDedent" => Ok(Token::TkDedent),
            "TkLBrace" => Ok(Token::TkLBrace),
            "TkRBrace" => Ok(Token::TkRBrace),
            "TkFn" => Ok(Token::TkFn),
            "TkMatch" => Ok(Token::TkMatch),
            "TkEof" => Ok(Token::TkEof),
            _ => Err(format!("unknown variant '{}' for Token", variant_name)),
        }
    }
}

/// Debug representation of a token.
pub fn tokenRepr(t @ _: &Token) -> AverStr {
    crate::cancel_checkpoint();
    match t.clone() {
        crate::aver_generated::domain::token::Token::TkInt(n) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("Int("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        }),
        crate::aver_generated::domain::token::Token::TkFloat(f) => AverStr::from("Float"),
        crate::aver_generated::domain::token::Token::TkStr(s) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("Str("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(s))));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        }),
        crate::aver_generated::domain::token::Token::TkIdent(s) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("Ident("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(s))));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        }),
        crate::aver_generated::domain::token::Token::TkTrue => AverStr::from("True"),
        crate::aver_generated::domain::token::Token::TkFalse => AverStr::from("False"),
        crate::aver_generated::domain::token::Token::TkPlus => AverStr::from("Plus"),
        crate::aver_generated::domain::token::Token::TkMinus => AverStr::from("Minus"),
        crate::aver_generated::domain::token::Token::TkStar => AverStr::from("Star"),
        crate::aver_generated::domain::token::Token::TkSlash => AverStr::from("Slash"),
        crate::aver_generated::domain::token::Token::TkEq => AverStr::from("Eq"),
        crate::aver_generated::domain::token::Token::TkEqEq => AverStr::from("EqEq"),
        crate::aver_generated::domain::token::Token::TkNeq => AverStr::from("Neq"),
        crate::aver_generated::domain::token::Token::TkLt => AverStr::from("Lt"),
        crate::aver_generated::domain::token::Token::TkGt => AverStr::from("Gt"),
        crate::aver_generated::domain::token::Token::TkLte => AverStr::from("Lte"),
        crate::aver_generated::domain::token::Token::TkGte => AverStr::from("Gte"),
        crate::aver_generated::domain::token::Token::TkLParen => AverStr::from("LParen"),
        crate::aver_generated::domain::token::Token::TkRParen => AverStr::from("RParen"),
        crate::aver_generated::domain::token::Token::TkComma => AverStr::from("Comma"),
        crate::aver_generated::domain::token::Token::TkArrow => AverStr::from("Arrow"),
        crate::aver_generated::domain::token::Token::TkLBracket => AverStr::from("LBracket"),
        crate::aver_generated::domain::token::Token::TkRBracket => AverStr::from("RBracket"),
        crate::aver_generated::domain::token::Token::TkDot => AverStr::from("Dot"),
        crate::aver_generated::domain::token::Token::TkDotDot => AverStr::from("DotDot"),
        crate::aver_generated::domain::token::Token::TkQuestion => AverStr::from("Question"),
        crate::aver_generated::domain::token::Token::TkNewline => AverStr::from("Newline"),
        crate::aver_generated::domain::token::Token::TkInterpStart => AverStr::from("InterpStart"),
        crate::aver_generated::domain::token::Token::TkInterpEnd => AverStr::from("InterpEnd"),
        crate::aver_generated::domain::token::Token::TkColon => AverStr::from("Colon"),
        crate::aver_generated::domain::token::Token::TkBang => AverStr::from("Bang"),
        crate::aver_generated::domain::token::Token::TkFatArrow => AverStr::from("FatArrow"),
        crate::aver_generated::domain::token::Token::TkIndent => AverStr::from("Indent"),
        crate::aver_generated::domain::token::Token::TkDedent => AverStr::from("Dedent"),
        crate::aver_generated::domain::token::Token::TkLBrace => AverStr::from("LBrace"),
        crate::aver_generated::domain::token::Token::TkRBrace => AverStr::from("RBrace"),
        crate::aver_generated::domain::token::Token::TkFn => AverStr::from("Fn"),
        crate::aver_generated::domain::token::Token::TkMatch => AverStr::from("Match"),
        crate::aver_generated::domain::token::Token::TkEof => AverStr::from("Eof"),
    }
}
