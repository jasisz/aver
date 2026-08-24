#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns, hidden_glob_reexports)]
// Aver Rust library emission — native provider host boundary
#[macro_use] extern crate aver_rt;
pub use ::aver_rt::AverMap as HashMap;
pub use ::aver_rt::AverStr;
pub use ::aver_rt::Buffer;
pub use ::aver_rt::ByteBuilder;

mod runtime_support;
pub use runtime_support::*;

mod replay_support;
pub use replay_support::*;

mod self_host_support;

pub mod provider_support;
pub use provider_support::{install_provider_bindings, install_provider_bindings_exact, preflight_required_providers};

pub mod aver_generated;
