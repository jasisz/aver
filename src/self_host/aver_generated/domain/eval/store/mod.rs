#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub struct FnStore {
    pub nameToId: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    pub byId: aver_rt::AverVector<crate::aver_generated::domain::ast::FnDef>,
    pub globals: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
}

impl aver_rt::AverDisplay for FnStore {
    fn aver_display(&self) -> String {
        format!(
            "FnStore({})",
            vec![
                format!("nameToId: {}", self.nameToId.aver_display_inner()),
                format!("byId: {}", self.byId.aver_display_inner()),
                format!("globals: {}", self.globals.aver_display_inner())
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
        fields.insert(
            "globals".to_string(),
            ReplayValue::to_replay_json(&self.globals),
        );
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
            nameToId:
                <aver_rt::AverMap<AverStr, aver_rt::AverInt> as ReplayValue>::from_replay_json(
                    fields
                        .get("nameToId")
                        .ok_or_else(|| "$record FnStore missing field 'nameToId'".to_string())?,
                )?,
            byId: <aver_rt::AverVector<FnDef> as ReplayValue>::from_replay_json(
                fields
                    .get("byId")
                    .ok_or_else(|| "$record FnStore missing field 'byId'".to_string())?,
            )?,
            globals: <aver_rt::AverMap<AverStr, Val> as ReplayValue>::from_replay_json(
                fields
                    .get("globals")
                    .ok_or_else(|| "$record FnStore missing field 'globals'".to_string())?,
            )?,
        })
    }
}

/// Look up a variable in the environment.
#[inline(always)]
pub fn lookupVar(
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    name @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match env.get(&name).cloned() {
        Some(v @ _) => Ok(v),
        None => Err((AverStr::from("undefined variable: ") + &name)),
    }
}

/// Create an empty function store.
pub fn emptyFnStore() -> FnStore {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::store::FnStore {
        nameToId: HashMap::new(),
        byId: aver_rt::AverVector::from_vec(aver_rt::AverList::empty().to_vec()),
        globals: HashMap::new(),
    }
}

/// Attach evaluated top-level bindings so function bodies can read them.
pub fn withGlobals(
    fns @ _: &FnStore,
    globals @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> FnStore {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::store::FnStore {
        nameToId: fns.nameToId.clone(),
        byId: fns.byId.clone(),
        globals: globals.clone(),
    }
}

/// Look up a top-level binding value by name.
#[inline(always)]
pub fn lookupGlobal(
    fns @ _: &FnStore,
    name @ _: AverStr,
) -> Option<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    fns.globals.get(&name).cloned()
}

/// Look up a function id by name.
#[inline(always)]
pub fn lookupFnId(fns @ _: &FnStore, name @ _: AverStr) -> Result<aver_rt::AverInt, AverStr> {
    crate::cancel_checkpoint();
    match fns.nameToId.get(&name).cloned() {
        Some(id @ _) => Ok(id),
        None => Err((AverStr::from("undefined function: ") + &name)),
    }
}

/// Look up a function definition by id.
#[inline(always)]
pub fn lookupFnById(
    fns @ _: &FnStore,
    id @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::ast::FnDef, AverStr> {
    crate::cancel_checkpoint();
    match (id).to_usize().and_then(|__i| fns.byId.get(__i).cloned()) {
        Some(fd @ _) => Ok(fd),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("undefined function id: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &((id.to_string()).into_aver()),
            )));
            __b
        })),
    }
}

/// Look up a function definition by name without wrapping success in Result.
#[inline(always)]
pub fn lookupFnOption(
    fns @ _: &FnStore,
    name @ _: AverStr,
) -> Option<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    match fns.nameToId.get(&name).cloned() {
        Some(id @ _) => (id).to_usize().and_then(|__i| fns.byId.get(__i).cloned()),
        None => None,
    }
}

/// Look up a function definition by name through the function store.
#[inline(always)]
pub fn lookupFn(
    fns @ _: &FnStore,
    name @ _: AverStr,
) -> Result<crate::aver_generated::domain::ast::FnDef, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) {
        Some(fd @ _) => Ok(fd),
        None => Err((AverStr::from("undefined function: ") + &name)),
    }
}

/// Build env map from parameter names and argument values.
#[inline(always)]
pub fn zipArgs(
    mut params @ _: aver_rt::AverList<AverStr>,
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(params, [] => { return acc; }, [p, ps] => { aver_list_match!(args, [] => { return acc; }, [a, as_] => { {
            let __tco0 = ps;
            let __tco1 = as_;
            let __tco2 = acc.insert_owned(p, a);
            params = __tco0;
            args = __tco1;
            acc = __tco2;
            continue;
        } }) })
    }
}

/// Add pattern match bindings to an env map.
#[inline(always)]
pub fn mergeBindings(
    mut bindings @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(bindings, [] => { return env; }, [pair, rest] => { { let (k, v) = pair; {
            let __tco0 = rest;
            let __tco1 = env.insert_owned(k, v);
            bindings = __tco0;
            env = __tco1;
            continue;
        } } })
    }
}

/// Build a function store with a name->id index and id->FnDef table.
pub fn fnsToStore(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> FnStore {
    crate::cancel_checkpoint();
    let nameToId @ _ = crate::aver_generated::domain::eval::store::fnsToIdMap(
        fns.clone(),
        HashMap::new(),
        aver_rt::AverInt::from_i64(0),
    );
    crate::aver_generated::domain::eval::store::FnStore {
        nameToId: nameToId,
        byId: aver_rt::AverVector::from_vec(fns.to_vec()),
        globals: HashMap::new(),
    }
}

/// Convert a list of FnDefs to a name->id map. Later defs shadow earlier ones.
#[inline(always)]
pub fn fnsToIdMap(
    mut fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut idx @ _: aver_rt::AverInt,
) -> aver_rt::AverMap<AverStr, aver_rt::AverInt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc; }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = acc.insert_owned(f.name.clone(), idx.clone());
            let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
            fns = __tco0;
            acc = __tco1;
            idx = __tco2;
            continue;
        } })
    }
}
