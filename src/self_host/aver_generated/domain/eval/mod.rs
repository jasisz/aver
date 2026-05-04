#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::store::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::core::*;

/// Evaluate a complete program through the self-hosted evaluator.
pub fn evalProgram(prog: &Program) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalProgram(prog)
}

/// Evaluate a program with additional loaded module functions.
pub fn evalProgramWithFns(prog: &Program, extraFns: &aver_rt::AverList<FnDef>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalProgramWithFns(prog, extraFns)
}

/// Stable facade for calling an already-resolved self-hosted function.
pub fn callResolved(fd: &FnDef, args: &aver_rt::AverList<Val>, fns: &FnStore) -> Result<Val, AverStr> {
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
