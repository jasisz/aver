#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    ValInt(aver_rt::AverInt),
    ValFloat(f64),
    ValStr(AverStr),
    ValBool(bool),
    ValFnRef(AverStr),
    ValList(aver_rt::AverList<Val>),
    ValVector(aver_rt::AverVector<Val>),
    ValOk(std::sync::Arc<Val>),
    ValErr(std::sync::Arc<Val>),
    ValSome(std::sync::Arc<Val>),
    ValNone,
    ValTuple(aver_rt::AverList<Val>),
    ValRecord(AverStr, aver_rt::AverList<(AverStr, Val)>),
    ValVariant(aver_rt::AverInt, AverStr, aver_rt::AverList<Val>),
    ValMap(aver_rt::AverMap<AverStr, Val>),
    ValUnit,
}

impl aver_rt::AverDisplay for Val {
    fn aver_display(&self) -> String {
        match self {
            Val::ValInt(f0) => format!("ValInt({})", f0.aver_display_inner()),
            Val::ValFloat(f0) => format!("ValFloat({})", f0.aver_display_inner()),
            Val::ValStr(f0) => format!("ValStr({})", f0.aver_display_inner()),
            Val::ValBool(f0) => format!("ValBool({})", f0.aver_display_inner()),
            Val::ValFnRef(f0) => format!("ValFnRef({})", f0.aver_display_inner()),
            Val::ValList(f0) => format!("ValList({})", f0.aver_display_inner()),
            Val::ValVector(f0) => format!("ValVector({})", f0.aver_display_inner()),
            Val::ValOk(f0) => format!("ValOk({})", f0.aver_display_inner()),
            Val::ValErr(f0) => format!("ValErr({})", f0.aver_display_inner()),
            Val::ValSome(f0) => format!("ValSome({})", f0.aver_display_inner()),
            Val::ValNone => "ValNone".to_string(),
            Val::ValTuple(f0) => format!("ValTuple({})", f0.aver_display_inner()),
            Val::ValRecord(f0, f1) => format!(
                "ValRecord({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Val::ValVariant(f0, f1, f2) => format!(
                "ValVariant({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Val::ValMap(f0) => format!("ValMap({})", f0.aver_display_inner()),
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
            serde_json::Value::String("Val".to_string()),
        );
        match self {
            Val::ValInt(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValFloat(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValStr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValBool(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValBool".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValFnRef(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValFnRef".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValList(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValList".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValVector(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValVector".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValOk(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValOk".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValErr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValErr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValSome(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValSome".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
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
            Val::ValTuple(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValTuple".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValRecord(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValRecord".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValVariant(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValVariant".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Val::ValMap(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ValMap".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
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
        if type_name != "Val" {
            return Err(format!(
                "$variant type mismatch: expected Val, got {}",
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
            "ValInt" => Ok(Val::ValInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValInt missing field #{}", 0))?,
                )?,
            )),
            "ValFloat" => Ok(Val::ValFloat(<f64 as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValFloat missing field #{}", 0))?,
            )?)),
            "ValStr" => Ok(Val::ValStr(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValStr missing field #{}", 0))?,
            )?)),
            "ValBool" => Ok(Val::ValBool(<bool as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValBool missing field #{}", 0))?,
            )?)),
            "ValFnRef" => Ok(Val::ValFnRef(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValFnRef missing field #{}", 0))?,
            )?)),
            "ValList" => Ok(Val::ValList(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValList missing field #{}", 0))?,
                )?,
            )),
            "ValVector" => Ok(Val::ValVector(
                <aver_rt::AverVector<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValVector missing field #{}", 0))?,
                )?,
            )),
            "ValOk" => Ok(Val::ValOk(
                <std::sync::Arc<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValOk missing field #{}", 0))?,
                )?,
            )),
            "ValErr" => Ok(Val::ValErr(
                <std::sync::Arc<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValErr missing field #{}", 0))?,
                )?,
            )),
            "ValSome" => Ok(Val::ValSome(
                <std::sync::Arc<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValSome missing field #{}", 0))?,
                )?,
            )),
            "ValNone" => Ok(Val::ValNone),
            "ValTuple" => Ok(Val::ValTuple(
                <aver_rt::AverList<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValTuple missing field #{}", 0))?,
                )?,
            )),
            "ValRecord" => Ok(Val::ValRecord(
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
            )),
            "ValVariant" => Ok(Val::ValVariant(
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
            )),
            "ValMap" => Ok(Val::ValMap(
                <aver_rt::AverMap<AverStr, Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ValMap missing field #{}", 0))?,
                )?,
            )),
            "ValUnit" => Ok(Val::ValUnit),
            _ => Err(format!("unknown variant '{}' for Val", variant_name)),
        }
    }
}

/// Render a string key or nested string value with quotes.
pub fn quoteString(s: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    ((AverStr::from("\"") + &s) + &AverStr::from("\""))
}

/// Build list repr recursively.
#[inline(always)]
pub fn valListRepr(
    mut items: aver_rt::AverList<Val>,
    mut acc: AverStr,
    mut first: bool,
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
pub fn valRepr(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValInt(n) => (n.to_string()).into_aver(),
        crate::aver_generated::domain::value::Val::ValFloat(f) => (f.to_string()).into_aver(),
        crate::aver_generated::domain::value::Val::ValStr(s) => s,
        crate::aver_generated::domain::value::Val::ValBool(b) => (b.to_string()).into_aver(),
        crate::aver_generated::domain::value::Val::ValFnRef(name) => aver_rt::AverStr::from({
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
        }),
        crate::aver_generated::domain::value::Val::ValList(items) => {
            crate::aver_generated::domain::value::valListRepr(items, AverStr::from(""), true)
        }
        crate::aver_generated::domain::value::Val::ValVector(vec) => {
            (AverStr::from("Vector")
                + &crate::aver_generated::domain::value::valListRepr(
                    vec.to_list(),
                    AverStr::from(""),
                    true,
                ))
        }
        crate::aver_generated::domain::value::Val::ValOk(inner) => {
            let inner = (*inner).clone();
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
        crate::aver_generated::domain::value::Val::ValErr(inner) => {
            let inner = (*inner).clone();
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
        crate::aver_generated::domain::value::Val::ValTuple(items) => aver_rt::AverStr::from({
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
        }),
        crate::aver_generated::domain::value::Val::ValSome(inner) => {
            let inner = (*inner).clone();
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
        crate::aver_generated::domain::value::Val::ValRecord(name, _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            };
            __b.push_str(&AverStr::from("(...)"));
            __b
        }),
        crate::aver_generated::domain::value::Val::ValMap(m) => {
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
        crate::aver_generated::domain::value::Val::ValVariant(_, fullName, fields) => {
            crate::aver_generated::domain::value::valVariantReprTagged(fullName, &fields)
        }
        crate::aver_generated::domain::value::Val::ValUnit => AverStr::from("()"),
    }
}

/// Display representation for nested values. Strings are quoted inside containers and wrappers.
pub fn valReprInner(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValStr(s) => {
            ((AverStr::from("\"") + &s) + &AverStr::from("\""))
        }
        crate::aver_generated::domain::value::Val::ValFnRef(name) => aver_rt::AverStr::from({
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
        }),
        crate::aver_generated::domain::value::Val::ValTuple(items) => aver_rt::AverStr::from({
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
        }),
        crate::aver_generated::domain::value::Val::ValVector(vec) => {
            (AverStr::from("Vector")
                + &crate::aver_generated::domain::value::valListRepr(
                    vec.to_list(),
                    AverStr::from(""),
                    true,
                ))
        }
        crate::aver_generated::domain::value::Val::ValList(items) => {
            crate::aver_generated::domain::value::valListRepr(items, AverStr::from(""), true)
        }
        _ => crate::aver_generated::domain::value::valRepr(v),
    }
}

/// Build a stable map repr close to the host interpreter output.
#[inline(always)]
pub fn valMapRepr(
    mut entries: aver_rt::AverList<(AverStr, Val)>,
    mut acc: AverStr,
    mut first: bool,
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
pub fn mapKeyRepr(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValInt(n) => (n.to_string()).into_aver(),
        crate::aver_generated::domain::value::Val::ValFloat(f) => (f.to_string()).into_aver(),
        crate::aver_generated::domain::value::Val::ValStr(s) => s,
        crate::aver_generated::domain::value::Val::ValBool(b) => (b.to_string()).into_aver(),
        _ => crate::aver_generated::domain::value::valRepr(v),
    }
}

/// Display a variant value using its full name.
pub fn valVariantReprTagged(fullName: AverStr, fields: &aver_rt::AverList<Val>) -> AverStr {
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
    mut items: aver_rt::AverList<Val>,
    mut acc: AverStr,
    mut first: bool,
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
