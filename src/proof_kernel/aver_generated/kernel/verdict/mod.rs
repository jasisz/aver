#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

/// Read one script and check it: the law it proves, or the refusal.
pub fn verdict(text @ _: AverStr) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let sexp @ _ = crate::proof_kernel::aver_generated::kernel::sexp::readAll(text)?;
    crate::proof_kernel::aver_generated::kernel::check::checkScript(
        &crate::proof_kernel::aver_generated::kernel::decode::script(&sexp)?,
    )
}
