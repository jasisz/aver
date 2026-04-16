#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub struct FnStore {
    pub nameToId: aver_rt::AverMap<AverStr, i64>,
    pub byId: aver_rt::AverVector<FnDef>,
}

impl aver_rt::AverDisplay for FnStore {
    fn aver_display(&self) -> String {
        format!(
            "FnStore({})",
            vec![
                format!("nameToId: {}", self.nameToId.aver_display_inner()),
                format!("byId: {}", self.byId.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for FnStore {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert(
            "nameToId".to_string(),
            ReplayValue::to_replay_json(&self.nameToId),
        );
        fields.insert("byId".to_string(), ReplayValue::to_replay_json(&self.byId));
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("FnStore".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "FnStore" {
            return Err(format!(
                "$record type mismatch: expected FnStore, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            nameToId: <aver_rt::AverMap<AverStr, i64> as ReplayValue>::from_replay_json(
                fields
                    .get("nameToId")
                    .ok_or_else(|| "$record FnStore missing field 'nameToId'".to_string())?,
            )?,
            byId: <aver_rt::AverVector<FnDef> as ReplayValue>::from_replay_json(
                fields
                    .get("byId")
                    .ok_or_else(|| "$record FnStore missing field 'byId'".to_string())?,
            )?,
        })
    }
}

/// Look up a variable in the environment.
#[inline(always)]
pub fn lookupVar(env: &aver_rt::AverMap<AverStr, Val>, name: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match env.get(&name).cloned() {
        Some(v) => Ok(v),
        None => Err((AverStr::from("undefined variable: ") + &name)),
    }
}

/// Create an empty function store.
pub fn emptyFnStore() -> FnStore {
    crate::cancel_checkpoint();
    FnStore {
        nameToId: HashMap::new(),
        byId: aver_rt::AverVector::from_vec(aver_rt::AverList::empty().to_vec()),
    }
}

/// Look up a function id by name.
#[inline(always)]
pub fn lookupFnId(fns: &FnStore, name: AverStr) -> Result<i64, AverStr> {
    crate::cancel_checkpoint();
    match fns.nameToId.get(&name).cloned() {
        Some(id) => Ok(id),
        None => Err((AverStr::from("undefined function: ") + &name)),
    }
}

/// Look up a function definition by id.
#[inline(always)]
pub fn lookupFnById(fns: &FnStore, id: i64) -> Result<FnDef, AverStr> {
    crate::cancel_checkpoint();
    match fns.byId.get(id as usize).cloned() {
        Some(fd) => Ok(fd),
        None => Err(AverStr::from(format!(
            "undefined function id: {}",
            aver_rt::aver_display(&(id.to_string()).into_aver())
        ))),
    }
}

/// Look up a function definition by name without wrapping success in Result.
#[inline(always)]
pub fn lookupFnOption(fns: &FnStore, name: AverStr) -> Option<FnDef> {
    crate::cancel_checkpoint();
    match fns.nameToId.get(&name).cloned() {
        Some(id) => fns.byId.get(id as usize).cloned(),
        None => None,
    }
}

/// Look up a function definition by name through the function store.
#[inline(always)]
pub fn lookupFn(fns: &FnStore, name: AverStr) -> Result<FnDef, AverStr> {
    crate::cancel_checkpoint();
    match lookupFnOption(fns, name.clone()) {
        Some(fd) => Ok(fd),
        None => Err((AverStr::from("undefined function: ") + &name)),
    }
}

/// Build env map from parameter names and argument values.
#[inline(always)]
pub fn zipArgs(
    mut params: aver_rt::AverList<AverStr>,
    mut args: aver_rt::AverList<Val>,
    mut acc: aver_rt::AverMap<AverStr, Val>,
) -> aver_rt::AverMap<AverStr, Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(params, [] => acc, [p, ps] => { aver_list_match!(args, [] => acc, [a, as_] => { {
            let __tmp2 = acc.insert_owned(p, a);
            params = ps;
            args = as_;
            acc = __tmp2;
            continue;
        } }) });
    }
}

/// Add pattern match bindings to an env map.
#[inline(always)]
pub fn mergeBindings(
    mut bindings: aver_rt::AverList<(AverStr, Val)>,
    mut env: aver_rt::AverMap<AverStr, Val>,
) -> aver_rt::AverMap<AverStr, Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(bindings, [] => env, [pair, rest] => { match pair {
            (k, v) => {
            let __tmp1 = env.insert_owned(k, v);
            bindings = rest;
            env = __tmp1;
            continue;
        }
        } });
    }
}

/// Build a function store with a name->id index and id->FnDef table.
pub fn fnsToStore(fns: &aver_rt::AverList<FnDef>) -> FnStore {
    crate::cancel_checkpoint();
    let nameToId = fnsToIdMap(fns.clone(), HashMap::new(), 0i64);
    FnStore {
        nameToId: nameToId,
        byId: aver_rt::AverVector::from_vec(fns.to_vec()),
    }
}

/// Convert a list of FnDefs to a name->id map. Later defs shadow earlier ones.
#[inline(always)]
pub fn fnsToIdMap(
    mut fns: aver_rt::AverList<FnDef>,
    mut acc: aver_rt::AverMap<AverStr, i64>,
    mut idx: i64,
) -> aver_rt::AverMap<AverStr, i64> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc, [f, rest] => { {
            let __tmp1 = acc.insert_owned(f.name.clone(), idx);
            let __tmp2 = (idx + 1i64);
            fns = rest;
            acc = __tmp1;
            idx = __tmp2;
            continue;
        } });
    }
}
