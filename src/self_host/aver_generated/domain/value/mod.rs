#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug)]
pub enum Val {
    ValInt(std::sync::Arc<Val__ValInt>),
    ValFloat(std::sync::Arc<Val__ValFloat>),
    ValStr(std::sync::Arc<Val__ValStr>),
    ValBool(std::sync::Arc<Val__ValBool>),
    ValFnRef(std::sync::Arc<Val__ValFnRef>),
    ValList(std::sync::Arc<Val__ValList>),
    ValVector(std::sync::Arc<Val__ValVector>),
    ValOk(std::sync::Arc<Val__ValOk>),
    ValErr(std::sync::Arc<Val__ValErr>),
    ValSome(std::sync::Arc<Val__ValSome>),
    ValNone,
    ValTuple(std::sync::Arc<Val__ValTuple>),
    ValRecord(std::sync::Arc<Val__ValRecord>),
    ValTcpConnection(std::sync::Arc<Val__ValTcpConnection>),
    ValTcpDial(std::sync::Arc<Val__ValTcpDial>),
    ValTcpListener(std::sync::Arc<Val__ValTcpListener>),
    ValVariant(std::sync::Arc<Val__ValVariant>),
    ValMap(std::sync::Arc<Val__ValMap>),
    ValUnit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValInt(pub aver_rt::AverInt);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValFloat(pub f64);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValStr(pub AverStr);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValBool(pub bool);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValFnRef(pub AverStr);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValList(pub aver_rt::AverList<Val>);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValVector(pub aver_rt::AverVector<Val>);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValOk(pub Val);

impl aver_rt::WalkEq for Val__ValOk {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValErr(pub Val);

impl aver_rt::WalkEq for Val__ValErr {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValSome(pub Val);

impl aver_rt::WalkEq for Val__ValSome {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTuple(pub aver_rt::AverList<Val>);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValRecord(pub AverStr, pub aver_rt::AverList<(AverStr, Val)>);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpConnection(pub crate::aver_generated::tcp::Connection);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpDial(pub crate::aver_generated::tcp::Dial);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValTcpListener(pub crate::aver_generated::tcp::Listener);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValVariant(
    pub aver_rt::AverInt,
    pub AverStr,
    pub aver_rt::AverList<Val>,
);

#[derive(Clone, Debug, PartialEq)]
pub struct Val__ValMap(pub aver_rt::AverMap<AverStr, Val>);

impl aver_rt::WalkEq for Val {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
            && match (self, other) {
                (Val::ValInt(a0), Val::ValInt(b0)) => a0 == b0,
                (Val::ValFloat(a0), Val::ValFloat(b0)) => a0 == b0,
                (Val::ValStr(a0), Val::ValStr(b0)) => a0 == b0,
                (Val::ValBool(a0), Val::ValBool(b0)) => a0 == b0,
                (Val::ValFnRef(a0), Val::ValFnRef(b0)) => a0 == b0,
                (Val::ValList(a0), Val::ValList(b0)) => a0 == b0,
                (Val::ValVector(a0), Val::ValVector(b0)) => a0 == b0,
                (Val::ValOk(a0), Val::ValOk(b0)) => walk.shared(a0, b0, false),
                (Val::ValErr(a0), Val::ValErr(b0)) => walk.shared(a0, b0, false),
                (Val::ValSome(a0), Val::ValSome(b0)) => walk.shared(a0, b0, false),
                (Val::ValNone, Val::ValNone) => true,
                (Val::ValTuple(a0), Val::ValTuple(b0)) => a0 == b0,
                (Val::ValRecord(a0), Val::ValRecord(b0)) => a0 == b0,
                (Val::ValTcpConnection(a0), Val::ValTcpConnection(b0)) => a0 == b0,
                (Val::ValTcpDial(a0), Val::ValTcpDial(b0)) => a0 == b0,
                (Val::ValTcpListener(a0), Val::ValTcpListener(b0)) => a0 == b0,
                (Val::ValVariant(a0), Val::ValVariant(b0)) => a0 == b0,
                (Val::ValMap(a0), Val::ValMap(b0)) => a0 == b0,
                (Val::ValUnit, Val::ValUnit) => true,
                _ => false,
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
        match self {
            Val::ValInt(f) => format!("ValInt({})", (&f.0).aver_display_inner()),
            Val::ValFloat(f) => format!("ValFloat({})", (&f.0).aver_display_inner()),
            Val::ValStr(f) => format!("ValStr({})", (&f.0).aver_display_inner()),
            Val::ValBool(f) => format!("ValBool({})", (&f.0).aver_display_inner()),
            Val::ValFnRef(f) => format!("ValFnRef({})", (&f.0).aver_display_inner()),
            Val::ValList(f) => format!("ValList({})", (&f.0).aver_display_inner()),
            Val::ValVector(f) => format!("ValVector({})", (&f.0).aver_display_inner()),
            Val::ValOk(f) => format!("ValOk({})", (&f.0).aver_display_inner()),
            Val::ValErr(f) => format!("ValErr({})", (&f.0).aver_display_inner()),
            Val::ValSome(f) => format!("ValSome({})", (&f.0).aver_display_inner()),
            Val::ValNone => "ValNone".to_string(),
            Val::ValTuple(f) => format!("ValTuple({})", (&f.0).aver_display_inner()),
            Val::ValRecord(f) => format!(
                "ValRecord({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Val::ValTcpConnection(f) => {
                format!("ValTcpConnection({})", (&f.0).aver_display_inner())
            }
            Val::ValTcpDial(f) => format!("ValTcpDial({})", (&f.0).aver_display_inner()),
            Val::ValTcpListener(f) => format!("ValTcpListener({})", (&f.0).aver_display_inner()),
            Val::ValVariant(f) => format!(
                "ValVariant({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Val::ValMap(f) => format!("ValMap({})", (&f.0).aver_display_inner()),
            Val::ValUnit => "ValUnit".to_string(),
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
        match self {
            Val::ValInt(f) => {
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
            Val::ValFloat(f) => {
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
            Val::ValStr(f) => {
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
            Val::ValBool(f) => {
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
            Val::ValFnRef(f) => {
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
            Val::ValList(f) => {
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
            Val::ValVector(f) => {
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
            Val::ValOk(f) => {
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
            Val::ValErr(f) => {
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
            Val::ValSome(f) => {
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
            Val::ValNone => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValNone".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValTuple(f) => {
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
            Val::ValRecord(f) => {
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
            Val::ValTcpConnection(f) => {
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
            Val::ValTcpDial(f) => {
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
            Val::ValTcpListener(f) => {
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
            Val::ValVariant(f) => {
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
            Val::ValMap(f) => {
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
            Val::ValUnit => {
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
            "ValInt" => Ok(Val::ValInt(std::sync::Arc::new(Val__ValInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValInt missing field #{}", 0))?,
                )?,
            )))),
            "ValFloat" => Ok(Val::ValFloat(std::sync::Arc::new(Val__ValFloat(
                <f64 as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValFloat missing field #{}", 0))?,
                )?,
            )))),
            "ValStr" => Ok(Val::ValStr(std::sync::Arc::new(Val__ValStr(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValStr missing field #{}", 0))?,
                )?,
            )))),
            "ValBool" => Ok(Val::ValBool(std::sync::Arc::new(Val__ValBool(
                <bool as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValBool missing field #{}", 0))?,
                )?,
            )))),
            "ValFnRef" => Ok(Val::ValFnRef(std::sync::Arc::new(Val__ValFnRef(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValFnRef missing field #{}", 0))?,
                )?,
            )))),
            "ValList" => Ok(Val::ValList(std::sync::Arc::new(Val__ValList(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValList missing field #{}", 0))?,
                )?,
            )))),
            "ValVector" => Ok(Val::ValVector(std::sync::Arc::new(Val__ValVector(
                <aver_rt::AverVector<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValVector missing field #{}", 0))?,
                )?,
            )))),
            "ValOk" => Ok(Val::ValOk(std::sync::Arc::new(Val__ValOk(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValOk missing field #{}", 0))?,
                )?,
            )))),
            "ValErr" => Ok(Val::ValErr(std::sync::Arc::new(Val__ValErr(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValErr missing field #{}", 0))?,
                )?,
            )))),
            "ValSome" => Ok(Val::ValSome(std::sync::Arc::new(Val__ValSome(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValSome missing field #{}", 0))?,
                )?,
            )))),
            "ValNone" => Ok(Val::ValNone),
            "ValTuple" => Ok(Val::ValTuple(std::sync::Arc::new(Val__ValTuple(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTuple missing field #{}", 0))?,
                )?,
            )))),
            "ValRecord" => Ok(Val::ValRecord(std::sync::Arc::new(Val__ValRecord(
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
            )))),
            "ValTcpConnection" => Ok(Val::ValTcpConnection(std::sync::Arc::new(
                Val__ValTcpConnection(
                    <crate::aver_generated::tcp::Connection as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant ValTcpConnection missing field #{}", 0)
                        })?,
                    )?,
                ),
            ))),
            "ValTcpDial" => Ok(Val::ValTcpDial(std::sync::Arc::new(Val__ValTcpDial(
                <crate::aver_generated::tcp::Dial as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTcpDial missing field #{}", 0))?,
                )?,
            )))),
            "ValTcpListener" => Ok(Val::ValTcpListener(std::sync::Arc::new(
                Val__ValTcpListener(
                    <crate::aver_generated::tcp::Listener as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant ValTcpListener missing field #{}", 0)
                        })?,
                    )?,
                ),
            ))),
            "ValVariant" => Ok(Val::ValVariant(std::sync::Arc::new(Val__ValVariant(
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
            )))),
            "ValMap" => Ok(Val::ValMap(std::sync::Arc::new(Val__ValMap(
                <aver_rt::AverMap<AverStr, Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValMap missing field #{}", 0))?,
                )?,
            )))),
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
    ((AverStr::from("\"") + &s) + &AverStr::from("\""))
}

/// Build list repr recursively.
#[inline(always)]
pub fn valListRepr(
    mut items @ _: aver_rt::AverList<Val>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("[")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc)))); __b }; __b.push_str(&AverStr::from("]")); __b }); }, [v, rest] => { if first { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::value::valReprInner(&v);
            let __tco2 = false;
            items = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } else { {
            let __tco0 = rest;
            let __tco1 = ((acc + &AverStr::from(", ")) + &crate::aver_generated::domain::value::valReprInner(&v));
            let __tco2 = false;
            items = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } })
    }
}

/// Display representation of a runtime value.
pub fn valRepr(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v {
        crate::aver_generated::domain::value::Val::ValInt(__n_fields) => {
            let n = __n_fields.0.clone();
            (n.to_string()).into_aver()
        }
        crate::aver_generated::domain::value::Val::ValFloat(__f_fields) => {
            let f = __f_fields.0.clone();
            (f.to_string()).into_aver()
        }
        crate::aver_generated::domain::value::Val::ValStr(__s_fields) => {
            let s = __s_fields.0.clone();
            s
        }
        crate::aver_generated::domain::value::Val::ValBool(__b_fields) => {
            let b = __b_fields.0.clone();
            (b.to_string()).into_aver()
        }
        crate::aver_generated::domain::value::Val::ValFnRef(__name_fields) => {
            let name = __name_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("<fn "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValList(__items_fields) => {
            let items = __items_fields.0.clone();
            crate::aver_generated::domain::value::valListRepr(items, AverStr::from(""), true)
        }
        crate::aver_generated::domain::value::Val::ValVector(__vec_fields) => {
            let vec = __vec_fields.0.clone();
            (AverStr::from("Vector")
                + &crate::aver_generated::domain::value::valListRepr(
                    vec.to_list(),
                    AverStr::from(""),
                    true,
                ))
        }
        crate::aver_generated::domain::value::Val::ValOk(__inner_fields) => {
            let inner = __inner_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("Result.Ok("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValErr(__inner_fields) => {
            let inner = __inner_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("Result.Err("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValTuple(__items_fields) => {
            let items = __items_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valFieldsRepr(
                            items,
                            AverStr::from(""),
                            true,
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValSome(__inner_fields) => {
            let inner = __inner_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("Option.Some("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValNone => AverStr::from("Option.None"),
        crate::aver_generated::domain::value::Val::ValRecord(__name_fields) => {
            let name = __name_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                    __b
                };
                __b.push_str(&AverStr::from("(...)"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValTcpConnection(_) => {
            AverStr::from("Tcp.Connection(<resource>)")
        }
        crate::aver_generated::domain::value::Val::ValTcpDial(_) => {
            AverStr::from("Tcp.Dial(<resource>)")
        }
        crate::aver_generated::domain::value::Val::ValTcpListener(_) => {
            AverStr::from("Tcp.Listener(<resource>)")
        }
        crate::aver_generated::domain::value::Val::ValMap(__m_fields) => {
            let m = __m_fields.0.clone();
            crate::aver_generated::domain::value::valMapRepr(
                {
                    let mut es: Vec<_> = m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    es.sort_by(|a, b| a.0.cmp(&b.0));
                    aver_rt::AverList::from_vec(es)
                },
                AverStr::from(""),
                true,
            )
        }
        crate::aver_generated::domain::value::Val::ValVariant(__fullName_fields) => {
            let fullName = __fullName_fields.1.clone();
            let fields = __fullName_fields.2.clone();
            crate::aver_generated::domain::value::valVariantReprTagged(fullName, &fields)
        }
        crate::aver_generated::domain::value::Val::ValUnit => AverStr::from("()"),
    }
}

/// Display representation for nested values. Strings are quoted inside containers and wrappers.
pub fn valReprInner(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v {
        crate::aver_generated::domain::value::Val::ValStr(__s_fields) => {
            let s = __s_fields.0.clone();
            ((AverStr::from("\"") + &s) + &AverStr::from("\""))
        }
        crate::aver_generated::domain::value::Val::ValFnRef(__name_fields) => {
            let name = __name_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("<fn "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValTuple(__items_fields) => {
            let items = __items_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valFieldsRepr(
                            items,
                            AverStr::from(""),
                            true,
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::aver_generated::domain::value::Val::ValVector(__vec_fields) => {
            let vec = __vec_fields.0.clone();
            (AverStr::from("Vector")
                + &crate::aver_generated::domain::value::valListRepr(
                    vec.to_list(),
                    AverStr::from(""),
                    true,
                ))
        }
        crate::aver_generated::domain::value::Val::ValList(__items_fields) => {
            let items = __items_fields.0.clone();
            crate::aver_generated::domain::value::valListRepr(items, AverStr::from(""), true)
        }
        _ => crate::aver_generated::domain::value::valRepr(v),
    }
}

/// Build a stable map repr close to the host interpreter output.
#[inline(always)]
pub fn valMapRepr(
    mut entries @ _: aver_rt::AverList<(AverStr, Val)>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(entries, [] => { return aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("{")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc)))); __b }; __b.push_str(&AverStr::from("}")); __b }); }, [pair, rest] => { { let (k, v) = pair; if first { {
            let __tco0 = rest;
            let __tco1 = ((crate::aver_generated::domain::value::quoteString(k) + &AverStr::from(": ")) + &crate::aver_generated::domain::value::valReprInner(&v));
            let __tco2 = false;
            entries = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } else { {
            let __tco0 = rest;
            let __tco1 = ((((acc + &AverStr::from(", ")) + &crate::aver_generated::domain::value::quoteString(k)) + &AverStr::from(": ")) + &crate::aver_generated::domain::value::valReprInner(&v));
            let __tco2 = false;
            entries = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } } })
    }
}

/// Stable repr for map keys. Keeps common scalar keys off the general valRepr path.
pub fn mapKeyRepr(v @ _: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v {
        crate::aver_generated::domain::value::Val::ValInt(__n_fields) => {
            let n = __n_fields.0.clone();
            (n.to_string()).into_aver()
        }
        crate::aver_generated::domain::value::Val::ValFloat(__f_fields) => {
            let f = __f_fields.0.clone();
            (f.to_string()).into_aver()
        }
        crate::aver_generated::domain::value::Val::ValStr(__s_fields) => {
            let s = __s_fields.0.clone();
            s
        }
        crate::aver_generated::domain::value::Val::ValBool(__b_fields) => {
            let b = __b_fields.0.clone();
            (b.to_string()).into_aver()
        }
        _ => crate::aver_generated::domain::value::valRepr(v),
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
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valFieldsRepr(
                            fields.clone(),
                            AverStr::from(""),
                            true,
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// Build comma-separated repr of variant fields.
#[inline(always)]
pub fn valFieldsRepr(
    mut items @ _: aver_rt::AverList<Val>,
    mut acc @ _: AverStr,
    mut first @ _: bool,
) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return acc; }, [v, rest] => { if first { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::value::valReprInner(&v);
            let __tco2 = false;
            items = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } else { {
            let __tco0 = rest;
            let __tco1 = ((acc + &AverStr::from(", ")) + &crate::aver_generated::domain::value::valReprInner(&v));
            let __tco2 = false;
            items = __tco0;
            acc = __tco1;
            first = __tco2;
            continue;
        } } })
    }
}
