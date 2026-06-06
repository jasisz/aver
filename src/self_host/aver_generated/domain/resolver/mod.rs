#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::resolver::calls::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::resolver::core::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::resolver::fast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::resolver::rewrite::*;
#[allow(unused_imports)]
use crate::*;

/// Resolve all functions: slots, direct calls, then attach narrow fast-path tags.
pub fn resolveProgram(prog: &Program) -> Program {
    crate::cancel_checkpoint();
    let resolvedFns = crate::aver_generated::domain::resolver::core::resolveFns(
        prog.fns.clone(),
        aver_rt::AverList::empty(),
    );
    let fnMap = crate::aver_generated::domain::resolver::calls::buildFnMap(
        resolvedFns.clone(),
        HashMap::new(),
        0i64,
    );
    let calledFns = crate::aver_generated::domain::resolver::calls::resolveCallsInFns(
        resolvedFns,
        fnMap.clone(),
        aver_rt::AverList::empty(),
    );
    let annotatedFns = crate::aver_generated::domain::resolver::fast::annotateFastFns(
        calledFns,
        fnMap,
        aver_rt::AverList::empty(),
    );
    crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: crate::aver_generated::domain::resolver::rewrite::rewriteInternalFns(
            annotatedFns,
            aver_rt::AverList::empty(),
        ),
        stmts: crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmts(
            prog.stmts.clone(),
            aver_rt::AverList::empty(),
        ),
    }
}

pub mod calls;

pub mod core;

pub mod fast;

pub mod rewrite;
