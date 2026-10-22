#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

/// Read the goal, run the standard plan named plan on it, and write the answer the compiler reads back.
#[inline(always)]
pub fn run(plan @ _: AverStr, text @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::readGoal(text) {
        Err(why @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("goal\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        }),
        Ok(g @ _) => {
            let __arg4 @ _ = byName(plan, &g);
            crate::proof_std::aver_generated::kernel::lib::writeAnswer(&__arg4)
        }
    }
}

/// The standard plan a `by Kernel.Std.<plan>` line names; every plan Kernel.Std's `plans [...]` line lists has an arm.
pub fn byName(
    plan @ _: AverStr,
    g @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match &*plan.clone() {
        "cases" => crate::proof_std::aver_generated::kernel::std::cases(g),
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("Kernel.Std has no plan "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(plan))));
            __b
        })),
    }
}

/// One run of a plan's script (tactic) on the goal, with the compiler's answers so far: `proof`, `refused`, `ask` and the question to answer next, or `goal`.
#[inline(always)]
pub fn script(goal @ _: AverStr, tactic @ _: AverStr, answers @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::std::script(goal, tactic, answers)
}
