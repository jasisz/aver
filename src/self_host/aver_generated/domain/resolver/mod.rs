#[allow(unused_imports)]
use crate::*;

/// Resolve all functions: slots, direct calls, then attach narrow fast-path tags.
pub fn resolveProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
) -> crate::aver_generated::domain::ast::Program {
    crate::cancel_checkpoint();
    let resolvedFns @ _ = {
        let __arg5 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
        crate::aver_generated::domain::resolver::core::resolveFns(&prog.fns, __arg5)
    };
    let fnMap @ _ = {
        let __arg6 @ _ = <aver_rt::AverMap<AverStr, aver_rt::AverInt>>::new();
        crate::aver_generated::domain::resolver::calls::buildFnMap(
            &resolvedFns,
            __arg6,
            aver_rt::AverInt::from_i64(0),
        )
    };
    let calledFns @ _ = {
        let __arg7 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
        crate::aver_generated::domain::resolver::calls::resolveCallsInFns(
            resolvedFns,
            &fnMap,
            __arg7,
        )
    };
    let annotatedFns @ _ = {
        let __arg8 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
        crate::aver_generated::domain::resolver::fast::annotateFastFns(calledFns, &fnMap, __arg8)
    };
    crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: {
            let __arg9 @ _ =
                <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalFns(
                annotatedFns,
                __arg9,
            )
        },
        stmts: {
            let __arg10 @ _ =
                <aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>>::empty();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmts(
                &prog.stmts,
                __arg10,
            )
        },
    }
}

pub mod calls;

pub mod core;

pub mod fast;

pub mod rewrite;
