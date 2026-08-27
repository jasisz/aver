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
pub fn resolveProgram(
    mut prog @ _: crate::aver_generated::domain::ast::Program,
) -> crate::aver_generated::domain::ast::Program {
    crate::cancel_checkpoint();
    let resolvedFns @ _ = crate::aver_generated::domain::resolver::core::resolveFns(
        prog.fns.clone(),
        aver_rt::AverList::empty(),
    );
    let fnMap @ _ = crate::aver_generated::domain::resolver::calls::buildFnMap(
        resolvedFns.clone(),
        HashMap::new(),
        aver_rt::AverInt::from_i64(0),
    );
    let calledFns @ _ = crate::aver_generated::domain::resolver::calls::resolveCallsInFns(
        resolvedFns,
        fnMap.clone(),
        aver_rt::AverList::empty(),
    );
    let annotatedFns @ _ = crate::aver_generated::domain::resolver::fast::annotateFastFns(
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
