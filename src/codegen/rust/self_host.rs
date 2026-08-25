/// Self-host-only support emitted into generated Rust projects.
///
/// This is intentionally separate from `runtime_support`: it depends on the
/// generated self-host evaluator/value model and is not part of normal Rust
/// codegen output.
pub fn generate_self_host_support() -> String {
    format!("{}\n", SELF_HOST_SUPPORT)
}

const SELF_HOST_SUPPORT: &str = r#"
use std::cell::RefCell;

use crate::aver_generated::domain::ast::FnDef;
use crate::aver_generated::domain::eval::store::{FnStore, fnsToStore};

thread_local! {
    static SELF_HOST_FN_STORE: RefCell<Option<FnStore>> = const { RefCell::new(None) };
}

pub fn with_fn_store<T, F>(fns: FnStore, run: F) -> T
where
    F: FnOnce() -> T,
{
    let previous = SELF_HOST_FN_STORE.with(|cell| cell.replace(Some(fns)));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
    SELF_HOST_FN_STORE.with(|cell| {
        cell.replace(previous);
    });
    match result {
        Ok(value) => value,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

pub fn with_program_fn_store<T, F>(
    local_fns: crate::aver_rt::AverList<FnDef>,
    module_fns: crate::aver_rt::AverList<FnDef>,
    run: F,
) -> T
where
    F: FnOnce() -> T,
{
    let all_fns = crate::aver_rt::AverList::concat(&module_fns, &local_fns);
    let fns = fnsToStore(&all_fns);
    with_fn_store(fns, run)
}
"#;
