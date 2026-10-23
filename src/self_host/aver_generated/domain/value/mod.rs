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
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
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
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
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
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
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
pub struct Val__ValMap(pub aver_rt::AverMap<AverStr, Val>);

impl aver_rt::thin::HandOver for Val__ValMap {}

impl aver_rt::WalkEq for Val {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
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
            "ValMap" => Ok(Val::ValMap(Val__ValMap(
                <aver_rt::AverMap<AverStr, Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValMap missing field #{}", 0))?,
                )?,
            ))),
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc))));
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
                (n.to_string()).into_aver()
            }
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                (f.to_string()).into_aver()
            }
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                s
            }
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let b = __b_fields.0.clone();
                (b.to_string()).into_aver()
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        )));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        )));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::domain::value::valFieldsRepr(
                                items,
                                ::aver_rt::str_lit!(""),
                                true,
                            )),
                        )));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::domain::value::valReprInner(inner)),
                        )));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
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
                    let __arg17 @ _ = {
                        let mut es: Vec<_> =
                            m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                        es.sort_by(|a, b| a.0.cmp(&b.0));
                        aver_rt::AverList::from_vec(es)
                    };
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::domain::value::valFieldsRepr(
                                items,
                                ::aver_rt::str_lit!(""),
                                true,
                            )),
                        )));
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
    entries @ _: &aver_rt::AverList<(AverStr, Val)>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    let mut __hold0: aver_rt::AverList<(AverStr, Val)>;
    let mut entries @ _: &aver_rt::AverList<(AverStr, Val)> = entries;
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
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("}"));
                    __b
                });
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (k, v) = pair.clone();
                        if first {
                            {
                                let __tco0 = rest;
                                let __tco1 =
                                    ((crate::aver_generated::domain::value::quoteString(k)
                                        + &::aver_rt::str_lit!(": "))
                                        + &crate::aver_generated::domain::value::valReprInner(&v));
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
                                    + &crate::aver_generated::domain::value::quoteString(k))
                                    + &::aver_rt::str_lit!(": "))
                                    + &crate::aver_generated::domain::value::valReprInner(&v));
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

/// Stable repr for map keys. Keeps common scalar keys off the general valRepr path.
pub fn mapKeyRepr(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__n_fields) => {
                let n = __n_fields.0.clone();
                (n.to_string()).into_aver()
            }
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                (f.to_string()).into_aver()
            }
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                s
            }
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let b = __b_fields.0.clone();
                (b.to_string()).into_aver()
            }
            _ => crate::aver_generated::domain::value::valRepr(v),
        }
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
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(fullName),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valFieldsRepr(
                            fields,
                            ::aver_rt::str_lit!(""),
                            true,
                        )),
                    )));
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
