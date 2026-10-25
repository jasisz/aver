#[allow(unused_imports)]
use crate::*;

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Val / Val__view / Val__pair {
        0 ValInt(Val__ValInt),
        1 ValFloat(Val__ValFloat),
        2 ValStr(Val__ValStr),
        3 ValBool(Val__ValBool),
        4 ValFnRef(Val__ValFnRef),
        5 ValList(Val__ValList),
        6 ValVector(Val__ValVector),
        7 ValOk(Val__ValOk),
        8 ValErr(Val__ValErr),
        9 ValSome(Val__ValSome),
        10 ValNone,
        11 ValTuple(Val__ValTuple),
        12 ValRecord(Val__ValRecord),
        13 ValTcpConnection(Val__ValTcpConnection),
        14 ValTcpDial(Val__ValTcpDial),
        15 ValTcpListener(Val__ValTcpListener),
        16 ValVariant(Val__ValVariant),
        17 ValMap(Val__ValMap),
        18 ValUnit,
    }
}

impl std::fmt::Debug for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValInt(pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Val__ValInt {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValFloat(pub f64);

impl aver_rt::thin::HandOver for Val__ValFloat {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValStr(pub AverStr);

impl aver_rt::thin::HandOver for Val__ValStr {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValBool(pub bool);

impl aver_rt::thin::HandOver for Val__ValBool {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValFnRef(pub AverStr);

impl aver_rt::thin::HandOver for Val__ValFnRef {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValList(pub aver_rt::AverList<Val>);

impl aver_rt::thin::HandOver for Val__ValList {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValVector(pub aver_rt::AverVector<Val>);

impl aver_rt::thin::HandOver for Val__ValVector {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValOk(pub Val);

impl aver_rt::WalkEq for Val__ValOk {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Val__ValOk {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValErr(pub Val);

impl aver_rt::WalkEq for Val__ValErr {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Val__ValErr {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValSome(pub Val);

impl aver_rt::WalkEq for Val__ValSome {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Val__ValSome {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTuple(pub aver_rt::AverList<Val>);

impl aver_rt::thin::HandOver for Val__ValTuple {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValRecord(pub AverStr, pub aver_rt::AverList<(AverStr, Val)>);

impl aver_rt::thin::HandOver for Val__ValRecord {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpConnection(pub crate::aver_generated::tcp::Connection);

impl aver_rt::thin::HandOver for Val__ValTcpConnection {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpDial(pub crate::aver_generated::tcp::Dial);

impl aver_rt::thin::HandOver for Val__ValTcpDial {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpListener(pub crate::aver_generated::tcp::Listener);

impl aver_rt::thin::HandOver for Val__ValTcpListener {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValVariant(
    pub aver_rt::AverInt,
    pub AverStr,
    pub aver_rt::AverList<Val>,
);

impl aver_rt::thin::HandOver for Val__ValVariant {}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValMap(pub aver_rt::AverMap<AverStr, (Val, Val)>);

impl aver_rt::thin::HandOver for Val__ValMap {}

impl aver_rt::WalkEq for Val {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        match Val::view_pair(self, other) {
            Some(Val__pair::ValInt(a0, b0)) => a0 == b0,
            Some(Val__pair::ValFloat(a0, b0)) => a0 == b0,
            Some(Val__pair::ValStr(a0, b0)) => a0 == b0,
            Some(Val__pair::ValBool(a0, b0)) => a0 == b0,
            Some(Val__pair::ValFnRef(a0, b0)) => a0 == b0,
            Some(Val__pair::ValList(a0, b0)) => a0 == b0,
            Some(Val__pair::ValVector(a0, b0)) => a0 == b0,
            Some(Val__pair::ValOk(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Val__pair::ValErr(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Val__pair::ValSome(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Val__pair::ValNone) => true,
            Some(Val__pair::ValTuple(a0, b0)) => a0 == b0,
            Some(Val__pair::ValRecord(a0, b0)) => a0 == b0,
            Some(Val__pair::ValTcpConnection(a0, b0)) => a0 == b0,
            Some(Val__pair::ValTcpDial(a0, b0)) => a0 == b0,
            Some(Val__pair::ValTcpListener(a0, b0)) => a0 == b0,
            Some(Val__pair::ValVariant(a0, b0)) => a0 == b0,
            Some(Val__pair::ValMap(a0, b0)) => a0 == b0,
            Some(Val__pair::ValUnit) => true,
            None => false,
        }
    }
}

impl PartialEq for Val {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl aver_rt::AverDisplay for Val {
    fn aver_display(&self) -> String {
        match self.view() {
            Val__view::ValInt(f) => format!("ValInt({})", (&f.0).aver_display_inner()),
            Val__view::ValFloat(f) => format!("ValFloat({})", (&f.0).aver_display_inner()),
            Val__view::ValStr(f) => format!("ValStr({})", (&f.0).aver_display_inner()),
            Val__view::ValBool(f) => format!("ValBool({})", (&f.0).aver_display_inner()),
            Val__view::ValFnRef(f) => format!("ValFnRef({})", (&f.0).aver_display_inner()),
            Val__view::ValList(f) => format!("ValList({})", (&f.0).aver_display_inner()),
            Val__view::ValVector(f) => format!("ValVector({})", (&f.0).aver_display_inner()),
            Val__view::ValOk(f) => format!("ValOk({})", (&f.0).aver_display_inner()),
            Val__view::ValErr(f) => format!("ValErr({})", (&f.0).aver_display_inner()),
            Val__view::ValSome(f) => format!("ValSome({})", (&f.0).aver_display_inner()),
            Val__view::ValNone => "ValNone".to_string(),
            Val__view::ValTuple(f) => format!("ValTuple({})", (&f.0).aver_display_inner()),
            Val__view::ValRecord(f) => format!(
                "ValRecord({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Val__view::ValTcpConnection(f) => {
                format!("ValTcpConnection({})", (&f.0).aver_display_inner())
            }
            Val__view::ValTcpDial(f) => format!("ValTcpDial({})", (&f.0).aver_display_inner()),
            Val__view::ValTcpListener(f) => {
                format!("ValTcpListener({})", (&f.0).aver_display_inner())
            }
            Val__view::ValVariant(f) => format!(
                "ValVariant({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Val__view::ValMap(f) => format!("ValMap({})", (&f.0).aver_display_inner()),
            Val__view::ValUnit => "ValUnit".to_string(),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Val {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Value.Val".to_string()),
        );
        match self.view() {
            Val__view::ValInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValFloat(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValStr(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValBool(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValBool".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValFnRef(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValFnRef".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValList(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValList".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValVector(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValVector".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValOk(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValOk".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValErr(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValErr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValSome(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValSome".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValNone => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValNone".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValTuple(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValTuple".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValRecord(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValRecord".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValTcpConnection(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValTcpConnection".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValTcpDial(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValTcpDial".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValTcpListener(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValTcpListener".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValVariant(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValVariant".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValMap(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValMap".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val__view::ValUnit => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValUnit".to_string()),
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
        if type_name != "Domain.Value.Val" && type_name != "Val" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Value.Val, got {}",
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
            "ValInt" => Ok(Val::ValInt(Val__ValInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValInt missing field #{}", 0))?,
                )?,
            ))),
            "ValFloat" => Ok(Val::ValFloat(Val__ValFloat(
                <f64 as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValFloat missing field #{}", 0))?,
                )?,
            ))),
            "ValStr" => Ok(Val::ValStr(Val__ValStr(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValStr missing field #{}", 0))?,
                )?,
            ))),
            "ValBool" => Ok(Val::ValBool(Val__ValBool(
                <bool as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValBool missing field #{}", 0))?,
                )?,
            ))),
            "ValFnRef" => Ok(Val::ValFnRef(Val__ValFnRef(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValFnRef missing field #{}", 0))?,
                )?,
            ))),
            "ValList" => Ok(Val::ValList(Val__ValList(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValList missing field #{}", 0))?,
                )?,
            ))),
            "ValVector" => Ok(Val::ValVector(Val__ValVector(
                <aver_rt::AverVector<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValVector missing field #{}", 0))?,
                )?,
            ))),
            "ValOk" => Ok(Val::ValOk(Val__ValOk(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValOk missing field #{}", 0))?,
                )?,
            ))),
            "ValErr" => Ok(Val::ValErr(Val__ValErr(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValErr missing field #{}", 0))?,
                )?,
            ))),
            "ValSome" => Ok(Val::ValSome(Val__ValSome(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValSome missing field #{}", 0))?,
                )?,
            ))),
            "ValNone" => Ok(Val::ValNone),
            "ValTuple" => Ok(Val::ValTuple(Val__ValTuple(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTuple missing field #{}", 0))?,
                )?,
            ))),
            "ValRecord" => Ok(Val::ValRecord(Val__ValRecord(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValRecord missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<(AverStr, Val)> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ValRecord missing field #{}", 1))?,
                )?,
            ))),
            "ValTcpConnection" => Ok(Val::ValTcpConnection(Val__ValTcpConnection(
                <crate::aver_generated::tcp::Connection as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTcpConnection missing field #{}", 0))?,
                )?,
            ))),
            "ValTcpDial" => Ok(Val::ValTcpDial(Val__ValTcpDial(
                <crate::aver_generated::tcp::Dial as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTcpDial missing field #{}", 0))?,
                )?,
            ))),
            "ValTcpListener" => Ok(Val::ValTcpListener(Val__ValTcpListener(
                <crate::aver_generated::tcp::Listener as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTcpListener missing field #{}", 0))?,
                )?,
            ))),
            "ValVariant" => Ok(Val::ValVariant(Val__ValVariant(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValVariant missing field #{}", 0))?,
                )?,
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ValVariant missing field #{}", 1))?,
                )?,
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant ValVariant missing field #{}", 2))?,
                )?,
            ))),
            "ValMap" => Ok(Val::ValMap(Val__ValMap(<aver_rt::AverMap<
                AverStr,
                (Val, Val),
            > as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValMap missing field #{}", 0))?,
            )?))),
            "ValUnit" => Ok(Val::ValUnit),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Value.Val",
                variant_name
            )),
        }
    }
}

/// Render a string key or nested string value with quotes.
pub fn quoteString(s @ _: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    ((::aver_rt::str_lit!("\"") + &s) + &::aver_rt::str_lit!("\""))
}

/// Build list repr recursively.
#[inline(always)]
pub fn valListRepr(
    items @ _: &aver_rt::AverList<Val>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    let mut __hold0: aver_rt::AverList<Val>;
    let mut items @ _: &aver_rt::AverList<Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("["));
                            __b
                        };
                        __b.push_display(&(acc));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("]"));
                    __b
                });
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if first {
                        {
                            let __tco0 = rest;
                            let __tco1 = crate::aver_generated::domain::value::valReprInner(v);
                            let __tco2 = false;
                            __hold0 = __tco0;
                            items = &__hold0;
                            acc = __tco1;
                            first = __tco2;
                            continue;
                        }
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco1 = ((acc + &::aver_rt::str_lit!(", "))
                                + &crate::aver_generated::domain::value::valReprInner(v));
                            let __tco2 = false;
                            __hold0 = __tco0;
                            items = &__hold0;
                            acc = __tco1;
                            first = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Display representation of a runtime value.
pub fn valRepr(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::display_str(&(n))
            }
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                aver_rt::display_str(&(f))
            }
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                s
            }
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let b = __b_fields.0.clone();
                aver_rt::bool_str(b)
            }
            crate::aver_generated::domain::value::Val__view::ValFnRef(__name_fields) => {
                let name = __name_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("<fn "));
                            __b
                        };
                        __b.push_display(&(name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::value::valListRepr(
                    items,
                    ::aver_rt::str_lit!(""),
                    true,
                )
            }
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = &__vec_fields.0;
                (::aver_rt::str_lit!("Vector")
                    + &{
                        let __arg16 @ _ = vec.to_list();
                        crate::aver_generated::domain::value::valListRepr(
                            &__arg16,
                            ::aver_rt::str_lit!(""),
                            true,
                        )
                    })
            }
            crate::aver_generated::domain::value::Val__view::ValOk(__inner_fields) => {
                let inner = &__inner_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("Result.Ok("));
                            __b
                        };
                        __b.push_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValErr(__inner_fields) => {
                let inner = &__inner_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("Result.Err("));
                            __b
                        };
                        __b.push_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValTuple(__items_fields) => {
                let items = &__items_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("("));
                            __b
                        };
                        __b.push_display(
                            &(crate::aver_generated::domain::value::valFieldsRepr(
                                items,
                                ::aver_rt::str_lit!(""),
                                true,
                            )),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValSome(__inner_fields) => {
                let inner = &__inner_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("Option.Some("));
                            __b
                        };
                        __b.push_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValNone => {
                ::aver_rt::str_lit!("Option.None")
            }
            crate::aver_generated::domain::value::Val__view::ValRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_display(&(name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("(...)"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValTcpConnection(_) => {
                ::aver_rt::str_lit!("Tcp.Connection(<resource>)")
            }
            crate::aver_generated::domain::value::Val__view::ValTcpDial(_) => {
                ::aver_rt::str_lit!("Tcp.Dial(<resource>)")
            }
            crate::aver_generated::domain::value::Val__view::ValTcpListener(_) => {
                ::aver_rt::str_lit!("Tcp.Listener(<resource>)")
            }
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                {
                    let __arg17 @ _ = crate::aver_generated::domain::value::mapPairs(m.clone());
                    crate::aver_generated::domain::value::valMapRepr(
                        &__arg17,
                        ::aver_rt::str_lit!(""),
                        true,
                    )
                }
            }
            crate::aver_generated::domain::value::Val__view::ValVariant(__fullName_fields) => {
                let fullName = __fullName_fields.1.clone();
                let fields = &__fullName_fields.2;
                crate::aver_generated::domain::value::valVariantReprTagged(fullName, fields)
            }
            crate::aver_generated::domain::value::Val__view::ValUnit => {
                ::aver_rt::str_lit!("()")
            }
        }
    }
}

/// Display representation for nested values. Strings are quoted inside containers and wrappers.
pub fn valReprInner(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                ((::aver_rt::str_lit!("\"") + &s) + &::aver_rt::str_lit!("\""))
            }
            crate::aver_generated::domain::value::Val__view::ValFnRef(__name_fields) => {
                let name = __name_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("<fn "));
                            __b
                        };
                        __b.push_display(&(name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValTuple(__items_fields) => {
                let items = &__items_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("("));
                            __b
                        };
                        __b.push_display(
                            &(crate::aver_generated::domain::value::valFieldsRepr(
                                items,
                                ::aver_rt::str_lit!(""),
                                true,
                            )),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = &__vec_fields.0;
                (::aver_rt::str_lit!("Vector")
                    + &{
                        let __arg6 @ _ = vec.to_list();
                        crate::aver_generated::domain::value::valListRepr(
                            &__arg6,
                            ::aver_rt::str_lit!(""),
                            true,
                        )
                    })
            }
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::value::valListRepr(
                    items,
                    ::aver_rt::str_lit!(""),
                    true,
                )
            }
            _ => crate::aver_generated::domain::value::valRepr(v),
        }
    }
}

/// Build a stable map repr close to the host interpreter output.
#[inline(always)]
pub fn valMapRepr(
    entries @ _: &aver_rt::AverList<(Val, Val)>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    let mut __hold0: aver_rt::AverList<(Val, Val)>;
    let mut entries @ _: &aver_rt::AverList<(Val, Val)> = entries;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = entries;
            if __list_subject.is_empty() {
                return aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("{"));
                            __b
                        };
                        __b.push_display(&(acc));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("}"));
                    __b
                });
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (k, v) = pair;
                        if first {
                            {
                                let __tco0 = rest;
                                let __tco1 =
                                    ((crate::aver_generated::domain::value::valReprInner(k)
                                        + &::aver_rt::str_lit!(": "))
                                        + &crate::aver_generated::domain::value::valReprInner(v));
                                let __tco2 = false;
                                __hold0 = __tco0;
                                entries = &__hold0;
                                acc = __tco1;
                                first = __tco2;
                                continue;
                            }
                        } else {
                            {
                                let __tco0 = rest;
                                let __tco1 = ((((acc + &::aver_rt::str_lit!(", "))
                                    + &crate::aver_generated::domain::value::valReprInner(k))
                                    + &::aver_rt::str_lit!(": "))
                                    + &crate::aver_generated::domain::value::valReprInner(v));
                                let __tco2 = false;
                                __hold0 = __tco0;
                                entries = &__hold0;
                                acc = __tco1;
                                first = __tco2;
                                continue;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The text a map files a key under: two keys of one type get the same text exactly when they are equal. A String, Int or Bool key is its own text. A key with parts writes each part with its length in front, so no part can pass for a separator, and a record takes its fields in name order.
pub fn mapKeyRepr(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::display_str(&(n))
            }
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                s
            }
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let b = __b_fields.0.clone();
                aver_rt::bool_str(b)
            }
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::value::partsKey(::aver_rt::str_lit!("["), items)
            }
            crate::aver_generated::domain::value::Val__view::ValTuple(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::value::partsKey(::aver_rt::str_lit!("("), items)
            }
            crate::aver_generated::domain::value::Val__view::ValSome(__inner_fields) => {
                let inner = &__inner_fields.0;
                {
                    let __arg12 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                    crate::aver_generated::domain::value::partsKey(
                        ::aver_rt::str_lit!("S"),
                        &__arg12,
                    )
                }
            }
            crate::aver_generated::domain::value::Val__view::ValOk(__inner_fields) => {
                let inner = &__inner_fields.0;
                {
                    let __arg13 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                    crate::aver_generated::domain::value::partsKey(
                        ::aver_rt::str_lit!("O"),
                        &__arg13,
                    )
                }
            }
            crate::aver_generated::domain::value::Val__view::ValErr(__inner_fields) => {
                let inner = &__inner_fields.0;
                {
                    let __arg14 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                    crate::aver_generated::domain::value::partsKey(
                        ::aver_rt::str_lit!("E"),
                        &__arg14,
                    )
                }
            }
            crate::aver_generated::domain::value::Val__view::ValRecord(__fields_fields) => {
                let fields = &__fields_fields.1;
                {
                    let __arg15 @ _ = {
                        let __map = &({
                            let mut __map = HashMap::new();
                            for (__key, __value) in fields.clone().iter().cloned() {
                                __map = __map.insert_owned(__key, __value);
                            }
                            __map
                        });
                        let mut es: Vec<_> = __map.iter().collect();
                        es.sort_by(|a, b| a.0.cmp(b.0));
                        aver_rt::AverList::from_vec(
                            es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>(),
                        )
                    };
                    crate::aver_generated::domain::value::partsKey(
                        ::aver_rt::str_lit!("R"),
                        &__arg15,
                    )
                }
            }
            crate::aver_generated::domain::value::Val__view::ValVariant(__fullName_fields) => {
                let fullName = __fullName_fields.1.clone();
                let fields = &__fullName_fields.2;
                {
                    let __arg16 @ _ = (fullName + &::aver_rt::str_lit!(":"));
                    crate::aver_generated::domain::value::partsKey(__arg16, fields)
                }
            }
            _ => crate::aver_generated::domain::value::valRepr(v),
        }
    }
}

/// A tag followed by each part's key text, each with its length in front.
#[inline(always)]
pub fn partsKey(mut tag @ _: AverStr, items @ _: &aver_rt::AverList<Val>) -> AverStr {
    let mut __hold1: aver_rt::AverList<Val>;
    let mut items @ _: &aver_rt::AverList<Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return tag;
            } else {
                if let (Some(item), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = (((tag
                            + &aver_rt::display_str(
                                &(aver_rt::AverInt::from_i64(
                                    crate::aver_generated::domain::value::mapKeyRepr(item)
                                        .chars()
                                        .count() as i64,
                                )),
                            ))
                            + &::aver_rt::str_lit!(":"))
                            + &crate::aver_generated::domain::value::mapKeyRepr(item));
                        let __tco1 = rest;
                        tag = __tco0;
                        __hold1 = __tco1;
                        items = &__hold1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A map's (key, value) pairs in key order, the order every backend iterates a map in. The text a String or Bool key is filed under already sorts that way; any other key is sorted.
#[inline(always)]
pub fn mapPairs(mut m @ _: aver_rt::AverMap<AverStr, (Val, Val)>) -> aver_rt::AverList<(Val, Val)> {
    crate::cancel_checkpoint();
    let pairs @ _ = {
        let __map = &(m);
        let mut es: Vec<_> = __map.iter().collect();
        es.sort_by(|a, b| a.0.cmp(b.0));
        aver_rt::AverList::from_vec(es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>())
    };
    {
        let __list_subject = pairs.clone();
        if __list_subject.is_empty() {
            pairs
        } else {
            if let Some((pair, _)) = aver_rt::list_uncons_cloned(&__list_subject) {
                {
                    let __t = pair;
                    match (__t.0.view(), &__t.1) {
                        (crate::aver_generated::domain::value::Val__view::ValStr(_), _) => pairs,
                        (crate::aver_generated::domain::value::Val__view::ValBool(_), _) => pairs,
                        _ => crate::aver_generated::domain::value::sortPairs(&pairs),
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Merge sort of (key, value) pairs by key.
#[inline(always)]
pub fn sortPairs(pairs @ _: &aver_rt::AverList<(Val, Val)>) -> aver_rt::AverList<(Val, Val)> {
    crate::cancel_checkpoint();
    {
        let __list_subject = pairs;
        if __list_subject.is_empty() {
            pairs.clone()
        } else {
            if let Some((_, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                {
                    let __list_subject = rest;
                    if __list_subject.is_empty() {
                        pairs.clone()
                    } else {
                        {
                            let __arg4 @ _ = {
                                let __arg2 @ _ = <aver_rt::AverList<(Val, Val)>>::empty();
                                {
                                    let __arg3 @ _ = <aver_rt::AverList<(Val, Val)>>::empty();
                                    crate::aver_generated::domain::value::splitPairs(
                                        pairs, __arg2, __arg3,
                                    )
                                }
                            };
                            crate::aver_generated::domain::value::sortHalves(&__arg4)
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Sort both halves and merge them.
pub fn sortHalves(
    halves @ _: &(aver_rt::AverList<(Val, Val)>, aver_rt::AverList<(Val, Val)>),
) -> aver_rt::AverList<(Val, Val)> {
    crate::cancel_checkpoint();
    {
        let (left, right) = halves;
        {
            let __arg3 @ _ = crate::aver_generated::domain::value::sortPairs(left);
            {
                let __arg4 @ _ = crate::aver_generated::domain::value::sortPairs(right);
                {
                    let __arg5 @ _ = aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    );
                    crate::aver_generated::domain::value::mergePairs__collected(
                        __arg3, __arg4, __arg5,
                    )
                }
            }
        }
    }
}

/// Deal pairs into two halves alternately.
#[inline(always)]
pub fn splitPairs(
    pairs @ _: &aver_rt::AverList<(Val, Val)>,
    mut left @ _: aver_rt::AverList<(Val, Val)>,
    mut right @ _: aver_rt::AverList<(Val, Val)>,
) -> (aver_rt::AverList<(Val, Val)>, aver_rt::AverList<(Val, Val)>) {
    let mut __hold0: aver_rt::AverList<(Val, Val)>;
    let mut pairs @ _: &aver_rt::AverList<(Val, Val)> = pairs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = pairs;
            if __list_subject.is_empty() {
                return (left, right);
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = right;
                        let __tco2 = aver_rt::AverList::prepend_owned(pair.clone(), left);
                        __hold0 = __tco0;
                        pairs = &__hold0;
                        left = __tco1;
                        right = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Merge two key-sorted lists of pairs.
#[inline(always)]
pub fn mergePairs(
    mut xs @ _: aver_rt::AverList<(Val, Val)>,
    mut ys @ _: aver_rt::AverList<(Val, Val)>,
    mut acc @ _: aver_rt::AverList<(Val, Val)>,
) -> aver_rt::AverList<(Val, Val)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(xs.clone(), [] => { return aver_rt::AverList::concat(&(acc).reverse_owned(), &ys); }, [x, xr] => { aver_list_match!(ys.clone(), [] => { return aver_rt::AverList::concat(&(acc).reverse_owned(), &xs); }, [y, yr] => { if crate::aver_generated::domain::value::pairBefore(&x, &y) { {
            let __tco0 = xr;
            let __tco2 = aver_rt::AverList::prepend_owned(x, acc);
            xs = __tco0;
            acc = __tco2;
            continue;
        } } else { {
            let __tco1 = yr;
            let __tco2 = aver_rt::AverList::prepend_owned(y, acc);
            ys = __tco1;
            acc = __tco2;
            continue;
        } } }) })
    }
}

/// Whether the first pair's key sorts before the second's.
pub fn pairBefore(x @ _: &(Val, Val), y @ _: &(Val, Val)) -> bool {
    crate::cancel_checkpoint();
    {
        let ((a, _), (b, _)) = (x.clone(), y.clone());
        (crate::aver_generated::domain::value::valCmp(a, b) < aver_rt::AverInt::from_i64(0))
    }
}

/// Order two map keys of one type, -1, 0 or 1: Int by value, String by codepoint, Bool false first, a list or tuple item by item, a record by its fields in name order, a variant by constructor name and then payload, None before Some, Ok before Err.
pub fn valCmp(mut a @ _: Val, mut b @ _: Val) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        {
            let __t = (a, b);
            match (__t.0.view(), __t.1.view()) {
                (
                    crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return aver_rt::AverInt::from_i64({
                        let __arg24 @ _ = (x < y);
                        {
                            let __arg25 @ _ = (x == y);
                            crate::aver_generated::domain::value::ordOf(__arg24, __arg25)
                        }
                    });
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValStr(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValStr(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return aver_rt::AverInt::from_i64({
                        let __arg26 @ _ = (x < y);
                        {
                            let __arg27 @ _ = (x == y);
                            crate::aver_generated::domain::value::ordOf(__arg26, __arg27)
                        }
                    });
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValBool(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValBool(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return aver_rt::AverInt::from_i64({
                        let __arg28 @ _ = (y && (x == false));
                        {
                            let __arg29 @ _ = (x == y);
                            crate::aver_generated::domain::value::ordOf(__arg28, __arg29)
                        }
                    });
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValList(__xs_fields),
                    crate::aver_generated::domain::value::Val__view::ValList(__ys_fields),
                ) => {
                    let xs = &__xs_fields.0;
                    let ys = &__ys_fields.0;
                    return crate::aver_generated::domain::value::valsCmp(xs, ys);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValTuple(__xs_fields),
                    crate::aver_generated::domain::value::Val__view::ValTuple(__ys_fields),
                ) => {
                    let xs = &__xs_fields.0;
                    let ys = &__ys_fields.0;
                    return crate::aver_generated::domain::value::valsCmp(xs, ys);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValRecord(__fs_fields),
                    crate::aver_generated::domain::value::Val__view::ValRecord(__gs_fields),
                ) => {
                    let fs = &__fs_fields.1;
                    let gs = &__gs_fields.1;
                    return {
                        let __arg30 @ _ = {
                            let __map = &({
                                let mut __map = HashMap::new();
                                for (__key, __value) in fs.clone().iter().cloned() {
                                    __map = __map.insert_owned(__key, __value);
                                }
                                __map
                            });
                            let mut es: Vec<_> = __map.iter().collect();
                            es.sort_by(|a, b| a.0.cmp(b.0));
                            aver_rt::AverList::from_vec(
                                es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>(),
                            )
                        };
                        {
                            let __arg31 @ _ = {
                                let __map = &({
                                    let mut __map = HashMap::new();
                                    for (__key, __value) in gs.clone().iter().cloned() {
                                        __map = __map.insert_owned(__key, __value);
                                    }
                                    __map
                                });
                                let mut es: Vec<_> = __map.iter().collect();
                                es.sort_by(|a, b| a.0.cmp(b.0));
                                aver_rt::AverList::from_vec(
                                    es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>(),
                                )
                            };
                            crate::aver_generated::domain::value::valsCmp(&__arg30, &__arg31)
                        }
                    };
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValVariant(__n_fields),
                    crate::aver_generated::domain::value::Val__view::ValVariant(__m_fields),
                ) => {
                    let n = __n_fields.1.clone();
                    let fs = &__n_fields.2;
                    let m = __m_fields.1.clone();
                    let gs = &__m_fields.2;
                    if (n == m) {
                        return crate::aver_generated::domain::value::valsCmp(fs, gs);
                    } else {
                        return aver_rt::AverInt::from_i64({
                            let __arg32 @ _ = (n < m);
                            crate::aver_generated::domain::value::ordOf(__arg32, false)
                        });
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValSome(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValSome(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        a = __tco0;
                        b = __tco1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValOk(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValOk(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        a = __tco0;
                        b = __tco1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValErr(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValErr(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        a = __tco0;
                        b = __tco1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValNone,
                    crate::aver_generated::domain::value::Val__view::ValSome(_),
                ) => {
                    return aver_rt::AverInt::from_i64(-1);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValSome(_),
                    crate::aver_generated::domain::value::Val__view::ValNone,
                ) => {
                    return aver_rt::AverInt::from_i64(1);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValOk(_),
                    crate::aver_generated::domain::value::Val__view::ValErr(_),
                ) => {
                    return aver_rt::AverInt::from_i64(-1);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValErr(_),
                    crate::aver_generated::domain::value::Val__view::ValOk(_),
                ) => {
                    return aver_rt::AverInt::from_i64(1);
                }
                _ => {
                    return aver_rt::AverInt::from_i64(0);
                }
            }
        }
    }
}

/// Order two lists of values item by item, a shorter prefix first.
#[inline(always)]
pub fn valsCmp(
    xs @ _: &aver_rt::AverList<Val>,
    ys @ _: &aver_rt::AverList<Val>,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverList<Val>;
    let mut xs @ _: &aver_rt::AverList<Val> = xs;
    let mut __hold1: aver_rt::AverList<Val>;
    let mut ys @ _: &aver_rt::AverList<Val> = ys;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                {
                    let __list_subject = ys;
                    if __list_subject.is_empty() {
                        return aver_rt::AverInt::from_i64(0);
                    } else {
                        return aver_rt::AverInt::from_i64(-1);
                    }
                }
            } else {
                if let (Some(x), Some(xr)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = ys;
                        if __list_subject.is_empty() {
                            return aver_rt::AverInt::from_i64(1);
                        } else {
                            if let (Some(y), Some(yr)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __int_match_subject =
                                        crate::aver_generated::domain::value::valCmp(
                                            x.clone(),
                                            y.clone(),
                                        );
                                    if __int_match_subject == aver_rt::AverInt::from_i64(0) {
                                        {
                                            let __tco0 = xr;
                                            let __tco1 = yr;
                                            __hold0 = __tco0;
                                            xs = &__hold0;
                                            __hold1 = __tco1;
                                            ys = &__hold1;
                                            continue;
                                        }
                                    } else {
                                        let c = __int_match_subject.clone();
                                        return c;
                                    }
                                }
                            } else {
                                panic!("Aver Rust codegen: non-exhaustive list match")
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// -1 when less, 0 when the same, 1 otherwise.
#[inline(always)]
pub fn ordOf(less @ _: bool, same @ _: bool) -> i64 {
    crate::cancel_checkpoint();
    if less {
        -1i64
    } else {
        if same { 0i64 } else { 1i64 }
    }
}

/// Display a variant value using its full name.
pub fn valVariantReprTagged(fullName @ _: AverStr, fields @ _: &aver_rt::AverList<Val>) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __list_subject = fields;
        if __list_subject.is_empty() {
            fullName
        } else {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                            );
                            __b.push_display(&(fullName));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!("("));
                        __b
                    };
                    __b.push_display(
                        &(crate::aver_generated::domain::value::valFieldsRepr(
                            fields,
                            ::aver_rt::str_lit!(""),
                            true,
                        )),
                    );
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
                __b
            })
        }
    }
}

/// Build comma-separated repr of variant fields.
#[inline(always)]
pub fn valFieldsRepr(
    items @ _: &aver_rt::AverList<Val>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    let mut __hold0: aver_rt::AverList<Val>;
    let mut items @ _: &aver_rt::AverList<Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if first {
                        {
                            let __tco0 = rest;
                            let __tco1 = crate::aver_generated::domain::value::valReprInner(v);
                            let __tco2 = false;
                            __hold0 = __tco0;
                            items = &__hold0;
                            acc = __tco1;
                            first = __tco2;
                            continue;
                        }
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco1 = ((acc + &::aver_rt::str_lit!(", "))
                                + &crate::aver_generated::domain::value::valReprInner(v));
                            let __tco2 = false;
                            __hold0 = __tco0;
                            items = &__hold0;
                            acc = __tco1;
                            first = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether two values are equal as == says: part by part, a Float by IEEE 754, so a NaN is not equal to itself and 0.0 is equal to -0.0.
pub fn valEq(a @ _: &Val, b @ _: &Val) -> bool {
    let mut __hold0: Val;
    let mut a @ _: &Val = a;
    let mut __hold1: Val;
    let mut b @ _: &Val = b;
    loop {
        crate::cancel_checkpoint();
        {
            let __t = (a.clone(), b.clone());
            match (__t.0.view(), __t.1.view()) {
                (
                    crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return (x == y);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValFloat(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValFloat(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return (x == y);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValStr(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValStr(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return (x == y);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValBool(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValBool(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return (x == y);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValFnRef(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValFnRef(__y_fields),
                ) => {
                    let x = __x_fields.0.clone();
                    let y = __y_fields.0.clone();
                    return (x == y);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValList(__xs_fields),
                    crate::aver_generated::domain::value::Val__view::ValList(__ys_fields),
                ) => {
                    let xs = &__xs_fields.0;
                    let ys = &__ys_fields.0;
                    return crate::aver_generated::domain::value::valsEq(xs, ys);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValTuple(__xs_fields),
                    crate::aver_generated::domain::value::Val__view::ValTuple(__ys_fields),
                ) => {
                    let xs = &__xs_fields.0;
                    let ys = &__ys_fields.0;
                    return crate::aver_generated::domain::value::valsEq(xs, ys);
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValVector(__xs_fields),
                    crate::aver_generated::domain::value::Val__view::ValVector(__ys_fields),
                ) => {
                    let xs = &__xs_fields.0;
                    let ys = &__ys_fields.0;
                    return {
                        let __arg34 @ _ = xs.to_list();
                        {
                            let __arg35 @ _ = ys.to_list();
                            crate::aver_generated::domain::value::valsEq(&__arg34, &__arg35)
                        }
                    };
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValOk(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValOk(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        __hold0 = __tco0;
                        a = &__hold0;
                        __hold1 = __tco1;
                        b = &__hold1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValErr(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValErr(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        __hold0 = __tco0;
                        a = &__hold0;
                        __hold1 = __tco1;
                        b = &__hold1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValSome(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValSome(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    {
                        let __tco0 = x.clone();
                        let __tco1 = y.clone();
                        __hold0 = __tco0;
                        a = &__hold0;
                        __hold1 = __tco1;
                        b = &__hold1;
                        continue;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValNone,
                    crate::aver_generated::domain::value::Val__view::ValNone,
                ) => {
                    return true;
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValUnit,
                    crate::aver_generated::domain::value::Val__view::ValUnit,
                ) => {
                    return true;
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValRecord(__n_fields),
                    crate::aver_generated::domain::value::Val__view::ValRecord(__m_fields),
                ) => {
                    let n = __n_fields.0.clone();
                    let fs = &__n_fields.1;
                    let m = __m_fields.0.clone();
                    let gs = &__m_fields.1;
                    if (n == m) {
                        return {
                            let __arg36 @ _ = {
                                let mut es: Vec<_> = {
                                    let mut __map = HashMap::new();
                                    for (__key, __value) in fs.clone().iter().cloned() {
                                        __map = __map.insert_owned(__key, __value);
                                    }
                                    __map
                                }
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone()))
                                .collect();
                                es.sort_by(|a, b| a.0.cmp(&b.0));
                                aver_rt::AverList::from_vec(es)
                            };
                            {
                                let __arg37 @ _ = {
                                    let mut es: Vec<_> = {
                                        let mut __map = HashMap::new();
                                        for (__key, __value) in gs.clone().iter().cloned() {
                                            __map = __map.insert_owned(__key, __value);
                                        }
                                        __map
                                    }
                                    .iter()
                                    .map(|(k, v)| (k.clone(), v.clone()))
                                    .collect();
                                    es.sort_by(|a, b| a.0.cmp(&b.0));
                                    aver_rt::AverList::from_vec(es)
                                };
                                crate::aver_generated::domain::value::entriesEq(&__arg36, &__arg37)
                            }
                        };
                    } else {
                        return false;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValVariant(__n_fields),
                    crate::aver_generated::domain::value::Val__view::ValVariant(__m_fields),
                ) => {
                    let n = __n_fields.1.clone();
                    let fs = &__n_fields.2;
                    let m = __m_fields.1.clone();
                    let gs = &__m_fields.2;
                    if (n == m) {
                        return crate::aver_generated::domain::value::valsEq(fs, gs);
                    } else {
                        return false;
                    }
                }
                (
                    crate::aver_generated::domain::value::Val__view::ValMap(__x_fields),
                    crate::aver_generated::domain::value::Val__view::ValMap(__y_fields),
                ) => {
                    let x = &__x_fields.0;
                    let y = &__y_fields.0;
                    return {
                        let __arg38 @ _ = {
                            let __map = &(x);
                            let mut es: Vec<_> = __map.iter().collect();
                            es.sort_by(|a, b| a.0.cmp(b.0));
                            aver_rt::AverList::from_vec(
                                es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>(),
                            )
                        };
                        {
                            let __arg39 @ _ = {
                                let __map = &(y);
                                let mut es: Vec<_> = __map.iter().collect();
                                es.sort_by(|a, b| a.0.cmp(b.0));
                                aver_rt::AverList::from_vec(
                                    es.into_iter().map(|(_, v)| v.clone()).collect::<Vec<_>>(),
                                )
                            };
                            crate::aver_generated::domain::value::pairsEq(&__arg38, &__arg39)
                        }
                    };
                }
                _ => {
                    return (crate::aver_generated::domain::value::valRepr(a)
                        == crate::aver_generated::domain::value::valRepr(b));
                }
            }
        }
    }
}

/// Whether two lists of values are equal item by item.
#[inline(always)]
pub fn valsEq(xs @ _: &aver_rt::AverList<Val>, ys @ _: &aver_rt::AverList<Val>) -> bool {
    let mut __hold0: aver_rt::AverList<Val>;
    let mut xs @ _: &aver_rt::AverList<Val> = xs;
    let mut __hold1: aver_rt::AverList<Val>;
    let mut ys @ _: &aver_rt::AverList<Val> = ys;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                {
                    let __list_subject = ys;
                    if __list_subject.is_empty() {
                        return true;
                    } else {
                        return false;
                    }
                }
            } else {
                if let (Some(x), Some(xr)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = ys;
                        if __list_subject.is_empty() {
                            return false;
                        } else {
                            if let (Some(y), Some(yr)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                if crate::aver_generated::domain::value::valEq(x, y) {
                                    {
                                        let __tco0 = xr;
                                        let __tco1 = yr;
                                        __hold0 = __tco0;
                                        xs = &__hold0;
                                        __hold1 = __tco1;
                                        ys = &__hold1;
                                        continue;
                                    }
                                } else {
                                    return false;
                                }
                            } else {
                                panic!("Aver Rust codegen: non-exhaustive list match")
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether two lists of named values are equal entry by entry.
#[inline(always)]
pub fn entriesEq(
    xs @ _: &aver_rt::AverList<(AverStr, Val)>,
    ys @ _: &aver_rt::AverList<(AverStr, Val)>,
) -> bool {
    let mut __hold0: aver_rt::AverList<(AverStr, Val)>;
    let mut xs @ _: &aver_rt::AverList<(AverStr, Val)> = xs;
    let mut __hold1: aver_rt::AverList<(AverStr, Val)>;
    let mut ys @ _: &aver_rt::AverList<(AverStr, Val)> = ys;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                {
                    let __list_subject = ys;
                    if __list_subject.is_empty() {
                        return true;
                    } else {
                        return false;
                    }
                }
            } else {
                if let (Some(x), Some(xr)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = ys;
                        if __list_subject.is_empty() {
                            return false;
                        } else {
                            if let (Some(y), Some(yr)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let ((k, v), (j, w)) = (x.clone(), y.clone());
                                    if (k == j) {
                                        if crate::aver_generated::domain::value::valEq(&v, &w) {
                                            {
                                                let __tco0 = xr;
                                                let __tco1 = yr;
                                                __hold0 = __tco0;
                                                xs = &__hold0;
                                                __hold1 = __tco1;
                                                ys = &__hold1;
                                                continue;
                                            }
                                        } else {
                                            return false;
                                        }
                                    } else {
                                        return false;
                                    }
                                }
                            } else {
                                panic!("Aver Rust codegen: non-exhaustive list match")
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether two lists of (key, value) pairs are equal pair by pair.
#[inline(always)]
pub fn pairsEq(
    xs @ _: &aver_rt::AverList<(Val, Val)>,
    ys @ _: &aver_rt::AverList<(Val, Val)>,
) -> bool {
    let mut __hold0: aver_rt::AverList<(Val, Val)>;
    let mut xs @ _: &aver_rt::AverList<(Val, Val)> = xs;
    let mut __hold1: aver_rt::AverList<(Val, Val)>;
    let mut ys @ _: &aver_rt::AverList<(Val, Val)> = ys;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                {
                    let __list_subject = ys;
                    if __list_subject.is_empty() {
                        return true;
                    } else {
                        return false;
                    }
                }
            } else {
                if let (Some(x), Some(xr)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = ys;
                        if __list_subject.is_empty() {
                            return false;
                        } else {
                            if let (Some(y), Some(yr)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let ((k, v), (j, w)) = (x.clone(), y.clone());
                                    if (crate::aver_generated::domain::value::valEq(&k, &j)
                                        && crate::aver_generated::domain::value::valEq(&v, &w))
                                    {
                                        {
                                            let __tco0 = xr;
                                            let __tco1 = yr;
                                            __hold0 = __tco0;
                                            xs = &__hold0;
                                            __hold1 = __tco1;
                                            ys = &__hold1;
                                            continue;
                                        }
                                    } else {
                                        return false;
                                    }
                                }
                            } else {
                                panic!("Aver Rust codegen: non-exhaustive list match")
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `mergePairs`. Appends to a builder where `mergePairs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn mergePairs__collected(
    mut xs @ _: aver_rt::AverList<(Val, Val)>,
    mut ys @ _: aver_rt::AverList<(Val, Val)>,
    mut acc @ _: aver_rt::AverList<(Val, Val)>,
) -> aver_rt::AverList<(Val, Val)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(xs.clone(), [] => { return aver_rt::AverList::concat(&aver_rt::list_builder_finalize(acc), &ys); }, [x, xr] => { aver_list_match!(ys.clone(), [] => { return aver_rt::AverList::concat(&aver_rt::list_builder_finalize(acc), &xs); }, [y, yr] => { if crate::aver_generated::domain::value::pairBefore(&x, &y) { {
            let __tco0 = xr;
            let __tco2 = aver_rt::list_builder_push(acc, x);
            xs = __tco0;
            acc = __tco2;
            continue;
        } } else { {
            let __tco1 = yr;
            let __tco2 = aver_rt::list_builder_push(acc, y);
            ys = __tco1;
            acc = __tco2;
            continue;
        } } }) })
    }
}
