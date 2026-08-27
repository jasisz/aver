#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::core::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::store::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Evaluate a complete program through the self-hosted evaluator.
pub fn evalProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalProgram(prog)
}

/// Evaluate a program with additional loaded module functions.
pub fn evalProgramWithFns(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    extraFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalProgramWithFns(prog, extraFns)
}

/// Stable facade for calling an already-resolved self-hosted function.
pub fn callResolved(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::callResolved(fd, args, fns)
}

pub mod common;

pub mod core;

pub mod fast;

pub mod ops;

pub mod records;

pub mod slots;

pub mod store;
