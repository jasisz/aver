#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

/// The kernel's verdict on one step script.
#[inline(always)]
pub fn verdict(text @ _: AverStr) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::verdict::verdict(text)
}
