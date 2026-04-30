#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    ValInt(i64),
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
    ValVariant(i64, AverStr, aver_rt::AverList<Val>),
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
            "ValInt" => Ok(Val::ValInt(<i64 as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant ValInt missing field #{}", 0))?,
            )?)),
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
                <i64 as ReplayValue>::from_replay_json(
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
        return aver_list_match!(items, [] => { aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((18i64) as usize); __b.push_str(&AverStr::from("[")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc)))); __b }; __b.push_str(&AverStr::from("]")); __b }) }, [v, rest] => { if first { {
            items = rest;
            acc = valReprInner(&v);
            first = false;
            continue;
        } } else { {
            let __tmp1 = ((acc + &AverStr::from(", ")) + &valReprInner(&v));
            items = rest;
            acc = __tmp1;
            first = false;
            continue;
        } } });
    }
}

/// Display representation of a runtime value.
pub fn valRepr(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValInt(n) => (n.to_string()).into_aver(),
        Val::ValFloat(f) => (f.to_string()).into_aver(),
        Val::ValStr(s) => s,
        Val::ValBool(b) => (b.to_string()).into_aver(),
        Val::ValFnRef(name) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity((21i64) as usize);
                    __b.push_str(&AverStr::from("<fn "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            };
            __b.push_str(&AverStr::from(">"));
            __b
        }),
        Val::ValList(items) => valListRepr(items, AverStr::from(""), true),
        Val::ValVector(vec) => {
            (AverStr::from("Vector") + &valListRepr(vec.to_list(), AverStr::from(""), true))
        }
        Val::ValOk(inner) => {
            let inner = (*inner).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity((27i64) as usize);
                        __b.push_str(&AverStr::from("Result.Ok("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        Val::ValErr(inner) => {
            let inner = (*inner).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity((28i64) as usize);
                        __b.push_str(&AverStr::from("Result.Err("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        Val::ValTuple(items) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity((18i64) as usize);
                    __b.push_str(&AverStr::from("("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(valFieldsRepr(items, AverStr::from(""), true)),
                )));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        }),
        Val::ValSome(inner) => {
            let inner = (*inner).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity((29i64) as usize);
                        __b.push_str(&AverStr::from("Option.Some("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(valReprInner(&inner)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        Val::ValNone => AverStr::from("Option.None"),
        Val::ValRecord(name, _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((21i64) as usize);
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            };
            __b.push_str(&AverStr::from("(...)"));
            __b
        }),
        Val::ValMap(m) => valMapRepr(
            {
                let mut es: Vec<_> = m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                es.sort_by(|a, b| a.0.cmp(&b.0));
                aver_rt::AverList::from_vec(es)
            },
            AverStr::from(""),
            true,
        ),
        Val::ValVariant(_, fullName, fields) => valVariantReprTagged(fullName, &fields),
        Val::ValUnit => AverStr::from("()"),
    }
}

/// Display representation for nested values. Strings are quoted inside containers and wrappers.
pub fn valReprInner(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValStr(s) => ((AverStr::from("\"") + &s) + &AverStr::from("\"")),
        Val::ValFnRef(name) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity((21i64) as usize);
                    __b.push_str(&AverStr::from("<fn "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            };
            __b.push_str(&AverStr::from(">"));
            __b
        }),
        Val::ValTuple(items) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity((18i64) as usize);
                    __b.push_str(&AverStr::from("("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(valFieldsRepr(items, AverStr::from(""), true)),
                )));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        }),
        Val::ValVector(vec) => {
            (AverStr::from("Vector") + &valListRepr(vec.to_list(), AverStr::from(""), true))
        }
        Val::ValList(items) => valListRepr(items, AverStr::from(""), true),
        _ => valRepr(v),
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
        return aver_list_match!(entries, [] => { aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((18i64) as usize); __b.push_str(&AverStr::from("{")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(acc)))); __b }; __b.push_str(&AverStr::from("}")); __b }) }, [pair, rest] => { match pair {
            (k, v) => if first { {
            entries = rest;
            acc = ((quoteString(k) + &AverStr::from(": ")) + &valReprInner(&v));
            first = false;
            continue;
        } } else { {
            let __tmp1 = ((((acc + &AverStr::from(", ")) + &quoteString(k)) + &AverStr::from(": ")) + &valReprInner(&v));
            entries = rest;
            acc = __tmp1;
            first = false;
            continue;
        } }
        } });
    }
}

/// Stable repr for map keys. Keeps common scalar keys off the general valRepr path.
pub fn mapKeyRepr(v: &Val) -> AverStr {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValInt(n) => (n.to_string()).into_aver(),
        Val::ValFloat(f) => (f.to_string()).into_aver(),
        Val::ValStr(s) => s,
        Val::ValBool(b) => (b.to_string()).into_aver(),
        _ => valRepr(v),
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
                            let mut __b = aver_rt::Buffer::with_capacity((34i64) as usize);
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(fullName),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(valFieldsRepr(fields.clone(), AverStr::from(""), true)),
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
        return aver_list_match!(items, [] => acc, [v, rest] => { if first { {
            items = rest;
            acc = valReprInner(&v);
            first = false;
            continue;
        } } else { {
            let __tmp1 = ((acc + &AverStr::from(", ")) + &valReprInner(&v));
            items = rest;
            acc = __tmp1;
            first = false;
            continue;
        } } });
    }
}
